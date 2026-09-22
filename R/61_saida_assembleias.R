# 61_saida_assembleias.R — forma de saida de deputado estadual e distrital pela curadoria com fonte oficial (13/09/2026)
#
# Mesma regra do nivel federal (R/58). O primeiro ato definitivo nao revertido (renuncia, falecimento, cassacao,
# nao tomou posse, retotalizacao) encerra o mandato. Licenca e afastamento sao interregnos e vao para
# data/interregnos_assembleias.csv. O fim regular exige prova de que o deputado chegou ao fim da legislatura no mandato,
# que e a composicao final publicada pela casa ou o evento fim_regular registrado por fonte oficial.
#
# Entradas: ref/eventos_assembleias_fonte_oficial.csv e ref/composicao_final_assembleias_fonte_oficial.csv (R/61a), e as
# tabelas das casas que o R/10 ja integra, lidas aqui so para achar ato definitivo nomeado pela propria casa.
# Saidas: data/saida_assembleias.csv (uma linha por mandato resolvido pela curadoria), data/interregnos_assembleias.csv,
# output/verificacao/assembleias_casa_x_curadoria.csv (divergencias entre a casa e a curadoria).
# Execucao: cd ~/bocel && Rscript --vanilla R/61_saida_assembleias.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/61_saida_assembleias.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
HOJE <- as.IDate(trimws(readLines("output/data_referencia.txt", warn = FALSE)[1]))
DEF <- c("renuncia", "falecimento", "cassacao", "nao_tomou_posse", "retotalizacao")
TEMP <- c("licenca", "afastamento")

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA",
              select = c("id_mandato", "id_pessoa", "cd_cargo", "cargo", "sg_uf", "ano_eleicao", "mandato_inicio", "mandato_fim"))[cd_cargo %chin% c("7", "8")]
mand[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]

le_ref <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8") else NULL
ev <- le_ref("ref/eventos_assembleias_fonte_oficial.csv")
cf <- le_ref("ref/composicao_final_assembleias_fonte_oficial.csv")
if (is.null(ev)) ev <- data.table(id_mandato = character(), evento = character(), data_evento = character(), precisao_data = character(),
                                  data_evento_ate = character(), url = character(), tipo_fonte = character(), confianca = character(),
                                  trecho = character(), nome_fonte = character(), cargo_assumido = character())
if (is.null(cf)) cf <- data.table(id_mandato = character(), situacao = character(), url = character(), tipo_fonte = character(),
                                  confianca = character(), data_referencia = character())

## ---------------------------------------------------------------- ato definitivo nomeado pela propria casa
casa <- rbindlist(lapply(c("data/exercicio_assembleias.csv", "data/exercicio_assembleias_historico.csv",
                           "data/exercicio_assembleias_2.csv", "data/exercicio_assembleias_inventario.csv"), function(f) {
  if (!file.exists(f)) return(NULL)
  x <- fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
  x[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel, forma_casa = forma_saida, fim_casa = substr(data_fim_exercicio, 1, 10), tabela = basename(f))]
}), fill = TRUE)
casa <- merge(casa, mand[, .(id_mandato, mi, mf)], by = "id_mandato")
casa <- casa[forma_casa %chin% DEF & (is.na(fim_casa) | (as.IDate(fim_casa) >= mi - 60L & as.IDate(fim_casa) <= mf + 45L))]
casa_def <- casa[, .(forma_casa = forma_casa[1], fim_casa = fim_casa[1], tabela_casa = tabela[1]), by = id_mandato]

## ---------------------------------------------------------------- ato definitivo curado, com reversao
e <- merge(ev[!is.na(id_mandato)], mand[, .(id_mandato, mi, mf)], by = "id_mandato")
e[, d := as.IDate(data_evento)]
setorder(e, id_mandato, d, na.last = TRUE)
# ato definitivo revertido: ha retorno ou nova posse do mesmo deputado depois dele (liminar, anulacao)
e[, revertido := FALSE]
e[evento %chin% DEF, revertido := vapply(seq_len(.N), function(k) {
  dk <- d[k]; ids <- id_mandato[k]
  any(e$id_mandato == ids & e$evento %chin% c("retorno") & !is.na(e$d) & e$d > dk)
}, logical(1))]
def <- e[evento %chin% DEF & !revertido & !is.na(d)][, .SD[1], by = id_mandato]
res_def <- def[, .(id_mandato, forma_saida = evento, data_fim_efetiva = data_evento,
                   precisao_data_fim = fifelse(precisao_data %chin% "dia", "ato", "intervalo"),  # mes, ano ou intervalo: a data e o limite inferior
                   cobertura = "evento_curado", fonte = tipo_fonte, url_fonte = url, confianca, causa_original = substr(trecho, 1, 300))]

## ---------------------------------------------------------------- fim regular por composicao final ou evento
fr_cf <- cf[!is.na(id_mandato), .(url_fonte = url[1], fonte = tipo_fonte[1], confianca = if (any(confianca == "alta")) "alta" else "media",
                                  causa_original = paste("composicao final:", situacao[1])), by = id_mandato][, cobertura := "composicao_final_da_casa"]
fr_ev <- e[evento == "fim_regular", .(url_fonte = url[1], fonte = tipo_fonte[1], confianca = if (any(confianca == "alta")) "alta" else "media",
                                      causa_original = substr(trecho[1], 1, 300)), by = id_mandato][, cobertura := "evento_curado_fim_regular"]
fr <- rbind(fr_cf, fr_ev[!id_mandato %chin% fr_cf$id_mandato], fill = TRUE)
fr <- merge(fr, mand[, .(id_mandato, mandato_fim, mf)], by = "id_mandato")
fr <- fr[mf < HOJE & !id_mandato %chin% res_def$id_mandato]

## ---------------------------------------------------------------- divergencias entre a casa e a curadoria
div <- rbind(
  merge(res_def[, .(id_mandato, curadoria = forma_saida, data_curadoria = data_fim_efetiva)], casa_def, by = "id_mandato")[curadoria != forma_casa],
  merge(fr[, .(id_mandato, curadoria = "fim_regular", data_curadoria = mandato_fim)], casa_def, by = "id_mandato"), fill = TRUE)
fwrite(div, "output/verificacao/assembleias_casa_x_curadoria.csv", na = "NA", quote = TRUE)
# fim regular curado nao apaga ato definitivo nomeado pela casa; o caso fica na divergencia para a proxima rodada da curadoria
fr <- fr[!id_mandato %chin% casa_def$id_mandato]
res_fr <- fr[, .(id_mandato, forma_saida = "fim_regular", data_fim_efetiva = mandato_fim, precisao_data_fim = "convencional",
                 cobertura, fonte, url_fonte, confianca, causa_original)]

res <- rbind(res_def, res_fr, fill = TRUE)
res <- merge(mand[, .(id_mandato, cargo, sg_uf, ano_eleicao, mf)], res, by = "id_mandato")
res[, em_curso := mf >= HOJE & !forma_saida %chin% DEF][, mf := NULL]
setcolorder(res, c("id_mandato", "cargo", "sg_uf", "ano_eleicao", "forma_saida", "data_fim_efetiva", "precisao_data_fim", "em_curso",
                   "cobertura", "fonte", "url_fonte", "confianca", "causa_original"))
setorder(res, sg_uf, ano_eleicao, id_mandato)
stopifnot(!anyDuplicated(res$id_mandato))

## ---------------------------------------------------------------- interregnos
it <- e[evento %chin% TEMP]
if (nrow(it)) {
  ret <- e[evento == "retorno", .(id_mandato, d_ret = d)]
  it[, fim_fora := vapply(seq_len(.N), function(k) {
    r <- ret[id_mandato == it$id_mandato[k] & d_ret > it$d[k], d_ret]
    if (length(r)) as.character(min(r)) else NA_character_
  }, character(1))]
}
interregnos <- if (nrow(it)) merge(it[, .(id_mandato, tipo = evento, causa_original = substr(trecho, 1, 300), inicio_fora = data_evento, fim_fora,
                                           retorno_observado = !is.na(fim_fora), cargo_assumido, fonte = tipo_fonte, url_fonte = url)],
                                    mand[, .(id_mandato, id_pessoa, cargo, sg_uf, ano_eleicao)], by = "id_mandato") else
  data.table(id_mandato = character(), tipo = character(), inicio_fora = character(), fim_fora = character())

fwrite(res, "data/saida_assembleias.csv", na = "NA", quote = TRUE)
fwrite(interregnos, "data/interregnos_assembleias.csv", na = "NA", quote = TRUE)

enc <- mand[mf < HOJE]
cat("mandatos encerrados:", nrow(enc), "| resolvidos pela curadoria:", res[id_mandato %chin% enc$id_mandato, .N],
    "| divergencias casa x curadoria:", nrow(div), "\n")
print(res[, .N, by = .(forma_saida, cobertura)][order(-N)])
reg("sas_mandatos_resolvidos_pela_curadoria", nrow(res))
reg("sas_ato_definitivo_curado", nrow(res_def))
reg("sas_fim_regular_por_composicao_ou_evento", nrow(res_fr))
reg("sas_divergencias_casa_x_curadoria", nrow(div))
reg("sas_interregnos", nrow(interregnos))
