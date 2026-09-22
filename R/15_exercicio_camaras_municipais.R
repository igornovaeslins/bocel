# 15_exercicio_camaras_municipais.R — mandatos de vereador nas camaras municipais com SAPL
# Entrada:  data_raw/sapl_municipal/<uf>/<sg_ue>/{legislatura,parlamentar,mandato,tipoafastamento}.json
#           (python/fetch_sapl_municipal.py), data/mandatos.csv, data/pessoas.csv
# Saida:    data/exercicio_camaras_municipais.csv, data/exercicio_camaras_municipais_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/15_exercicio_camaras_municipais.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "data_referencia.R")); DATA_REF <- data_referencia(root)  # data fixa da versao, nao o dia da execucao (l. 444)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/15_exercicio_camaras_municipais.R"
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
d10 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

inv <- fread("data_raw/sapl_municipal/inventario_sapl_municipal.csv", colClasses = "character")
inst <- inv[responde_api == "TRUE"]
dirs <- file.path("data_raw/sapl_municipal", inst$uf, inst$sg_ue)
inst <- inst[file.exists(file.path(dirs, "_ok"))]
cat("instancias com cache:", nrow(inst), "\n")

ler <- function(d, rec) {
  f <- file.path(d, paste0(rec, ".json"))
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(fromJSON(f, simplifyVector = TRUE, flatten = TRUE), error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(NULL)
  as.data.table(x)
}
col <- function(dt, nm) if (!is.null(dt) && nm %in% names(dt)) as.character(dt[[nm]]) else rep(NA_character_, if (is.null(dt)) 0 else nrow(dt))

ex <- rbindlist(lapply(seq_len(nrow(inst)), function(i) {
  d <- file.path("data_raw/sapl_municipal", inst$uf[i], inst$sg_ue[i])
  leg <- ler(d, "legislatura"); par <- ler(d, "parlamentar"); man <- ler(d, "mandato"); taf <- ler(d, "tipoafastamento")
  if (is.null(leg) || is.null(par) || is.null(man)) return(NULL)
  L <- data.table(legislatura = as.character(leg$id), legislatura_numero = col(leg, "numero"),
                  legislatura_inicio = d10(col(leg, "data_inicio")), legislatura_fim = d10(col(leg, "data_fim")),
                  data_eleicao = d10(col(leg, "data_eleicao")))
  P <- data.table(parlamentar = as.character(par$id), nome_completo = col(par, "nome_completo"),
                  nome_parlamentar = col(par, "nome_parlamentar"), sexo = col(par, "sexo"))
  TA <- if (!is.null(taf)) data.table(tipo_afastamento = as.character(taf$id), tipo_afastamento_descricao = col(taf, "descricao")) else NULL
  M <- data.table(parlamentar = col(man, "parlamentar"), legislatura = col(man, "legislatura"),
                  data_inicio_mandato = d10(col(man, "data_inicio_mandato")), data_fim_mandato = d10(col(man, "data_fim_mandato")),
                  data_diploma = d10(col(man, "data_expedicao_diploma")), titular = toupper(col(man, "titular")) %in% c("TRUE", "T", "1"),
                  tipo_afastamento = col(man, "tipo_afastamento"), tipo_causa_fim = col(man, "tipo_causa_fim_mandato"),
                  votos = col(man, "votos_recebidos"), observacao = col(man, "observacao"), id_mandato_sapl = as.character(man$id))
  M <- merge(M, L, by = "legislatura", all.x = TRUE)
  M <- merge(M, P, by = "parlamentar", all.x = TRUE)
  if (!is.null(TA)) M <- merge(M, TA, by = "tipo_afastamento", all.x = TRUE) else M[, tipo_afastamento_descricao := NA_character_]
  M[, `:=`(sg_ue = inst$sg_ue[i], id_municipio_ibge = inst$id_municipio_ibge[i], uf = inst$uf[i], dominio = inst$dominio[i])]
  M
}), use.names = TRUE, fill = TRUE)
stopifnot(nrow(ex) > 0)

## eleicao de referencia: ano anterior ao inicio da legislatura quando cai no ciclo quadrienal municipal;
## senao o ano da data_eleicao quando este cai no ciclo; senao o ano anterior ao inicio da legislatura.
## (verifica_sapl_municipal.R, 28/ago/2026: a regra anterior preferia data_eleicao, mas em 125 legislaturas
## em que data_eleicao e legislatura_inicio discordam, o ano do inicio dos mandatos confirma legislatura_inicio
## em 120 e data_eleicao em 3; 811 linhas traziam data_eleicao no ano da posse e 85 linhas pareavam
## a eleicao errada. legislatura_inicio igual a data da eleicao, como em antonioolinto/PR, cai na 2a regra.)
CICLO <- seq(1996L, 2024L, 4L)
ex[, `:=`(ya = as.integer(substr(data_eleicao, 1, 4)), yb = as.integer(substr(legislatura_inicio, 1, 4)) - 1L)]
ex[, ano_eleicao_bocel := fcase(!is.na(yb) & yb %in% CICLO, yb,
                              !is.na(ya) & ya %in% CICLO, ya,
                              !is.na(yb), yb,
                              default = ya)]
registrar_numero("sapl_n_linhas_ano_eleicao_de_legislatura_inicio_diverge_data_eleicao", ex[!is.na(ya) & !is.na(yb) & ya != yb & ano_eleicao_bocel == yb, .N], script = script)
ex <- ex[!is.na(ano_eleicao_bocel) & ano_eleicao_bocel >= 1996L & ano_eleicao_bocel <= 2024L]
ex[, nome_fonte := fcoalesce(nome_completo, nome_parlamentar)]
ex[, nome_normalizado := norm(nome_fonte)]
ex[, nome_parl_norm := norm(nome_parlamentar)]

## forma de saida
ex[, desc := toupper(stri_trans_general(fcoalesce(tipo_afastamento_descricao, ""), "Latin-ASCII"))]
ex[, fim_leg := as.IDate(legislatura_fim)]
ex[, forma_saida := fcase(
  grepl("CASSA|PERDA", desc), "cassacao",
  grepl("RENUNC", desc), "renuncia",
  grepl("FALEC|MORTE|OBITO", desc), "falecimento",
  grepl("LICEN", desc), "licenca",
  grepl("AFAST", desc), "afastamento",
  !is.na(data_fim_mandato) & !is.na(fim_leg) & as.IDate(data_fim_mandato) >= fim_leg - 45L, "fim_regular",
  !is.na(data_fim_mandato) & !is.na(fim_leg) & as.IDate(data_fim_mandato) < fim_leg - 45L & titular == FALSE, "outro",
  !is.na(data_fim_mandato) & !is.na(fim_leg) & as.IDate(data_fim_mandato) < fim_leg - 45L, "outro",
  is.na(data_fim_mandato) & !is.na(fim_leg) & fim_leg < DATA_REF, "fim_regular",
  default = NA_character_)]

## pareamento com o BOCEL (vereador, mesmo municipio, mesma eleicao)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "13"]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano_eleicao)], pess[, .(id_pessoa, nome)], by = "id_pessoa")
mand[, nome_norm := norm(nome)]
# nome de urna do TSE para o fallback
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(arrow::read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO == "13"]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))], by = "id_mandato", all.x = TRUE)
mand[, ano_eleicao := as.integer(ano_eleicao)]

# (1) nome completo igual; (2) nome parlamentar igual ao nome de urna; (3) tokens do nome parlamentar
# (>= 2, todos >= 3 letras) contidos no nome civil, pessoa unica no municipio-eleicao
m1 <- merge(ex[, .(rid = .I, sg_ue, ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nome_normalizado"), by.y = c("sg_ue", "ano_eleicao", "nome_norm"))
m1 <- m1[, if (.N == 1) .SD, by = rid][, metodo := "nome_completo"]
# (verifica_sapl_municipal.R, 28/ago/2026: `.I` em i devolve tabela vazia em data.table 1.17, e a regra 2 nunca casava)
r2 <- ex[!(seq_len(nrow(ex)) %in% m1$rid)][, rid := which(!(seq_len(nrow(ex)) %in% m1$rid))]
m2 <- merge(r2[!is.na(nome_parl_norm), .(rid, sg_ue, ano_eleicao_bocel, nome_parl_norm)], mand[!is.na(nome_urna_norm), .(sg_ue, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nome_parl_norm"), by.y = c("sg_ue", "ano_eleicao", "nome_urna_norm"))
m2 <- m2[, if (.N == 1) .SD, by = rid][, metodo := "nome_parlamentar=nome_urna"]
feitos <- c(m1$rid, m2$rid)
r3 <- ex[!(seq_len(nrow(ex)) %in% feitos)][, rid := which(!(seq_len(nrow(ex)) %in% feitos))]
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
c3 <- merge(r3[, .(rid, sg_ue, ano_eleicao_bocel, nome_parl_norm, nome_normalizado)], mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c3)) {
  tr <- tok(fcoalesce(c3$nome_parl_norm, c3$nome_normalizado)); tn <- tok(c3$nome_norm)
  c3[, contido := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tr, tn)]
  m3 <- c3[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_nome_parlamentar"]
} else m3 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (nrow(d) && all(c("rid", "id_mandato", "id_pessoa", "metodo") %in% names(d))) d[, .(rid, id_mandato, id_pessoa, metodo)] else
  data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
# (4) tokens do nome parlamentar (apelido) contidos no NOME DE URNA do TSE, pessoa unica no municipio-eleicao
feitos4 <- c(sel(m1)$rid, sel(m2)$rid, sel(m3)$rid)
r4 <- ex[!(seq_len(nrow(ex)) %in% feitos4)][, rid := which(!(seq_len(nrow(ex)) %in% feitos4))]
c4 <- merge(r4[!is.na(nome_parl_norm), .(rid, sg_ue, ano_eleicao_bocel, nome_parl_norm)], mand[!is.na(nome_urna_norm), .(sg_ue, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c4)) {
  tr <- tok(c4$nome_parl_norm); tu <- tok(c4$nome_urna_norm)
  c4[, contido := mapply(function(a, b) length(a) >= 1 && any(nchar(a) >= 4) && all(a %in% b), tr, tu)]
  m4 <- c4[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_nome_parlamentar_no_nome_de_urna"]
} else m4 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
cat("pareamentos por regra:", nrow(m1), nrow(m2), nrow(m3), nrow(m4), "\n")
par <- rbindlist(list(sel(m1), sel(m2), sel(m3), sel(m4)), use.names = TRUE)
par <- par[!duplicated(rid)]
ex[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
ex[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# um mandato do BOCEL so pode receber uma linha de titular por instancia: em conflito, fica a com titular=TRUE e data mais antiga
setorder(ex, id_mandato_bocel, -titular, data_inicio_mandato, na.last = TRUE)
ex[!is.na(id_mandato_bocel) & titular == TRUE, dup := seq_len(.N) > 1, by = id_mandato_bocel]
ex[dup %in% TRUE, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = "descartado_duplicata")]

out <- ex[, .(sg_ue, id_municipio_ibge, uf, dominio, legislatura_numero, legislatura_inicio, legislatura_fim, ano_eleicao_bocel,
              nome_fonte, nome_parlamentar, nome_normalizado, sexo, titular, data_inicio_mandato, data_fim_mandato, data_diploma,
              tipo_afastamento = tipo_afastamento_descricao, votos, observacao, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento,
              id_mandato_sapl, url = paste0("https://", dominio, "/api/parlamentares/mandato/", id_mandato_sapl, "/"))]
setorder(out, uf, sg_ue, ano_eleicao_bocel, nome_normalizado)
fwrite(out, "data/exercicio_camaras_municipais.csv", na = "NA", quote = TRUE)

## cobertura
cob <- mand[, .(n_mandatos_bocel = .N), by = .(sg_ue, ano_eleicao)]
cob <- merge(cob, fread("data/municipios_tse_ibge.csv", colClasses = "character")[, .(sg_ue, uf = sg_uf)], by = "sg_ue")
cob <- cob[, .(n_mandatos_bocel = sum(n_mandatos_bocel), n_municipios_bocel = uniqueN(sg_ue)), by = .(uf, ano_eleicao)]
par_c <- out[!is.na(id_mandato_bocel), .(n_pareados = uniqueN(id_mandato_bocel), n_camaras_com_sapl = uniqueN(sg_ue)), by = .(uf, ano_eleicao = ano_eleicao_bocel)]
cob <- merge(cob, par_c, by = c("uf", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), `:=`(n_pareados = 0L, n_camaras_com_sapl = 0L)]
cob[, taxa := round(n_pareados / n_mandatos_bocel, 4)]
fwrite(cob[order(uf, ano_eleicao)], "data/exercicio_camaras_municipais_cobertura.csv", na = "NA")

registrar_numero("sapl_n_instancias_com_cache", nrow(inst), script = script)
registrar_numero("sapl_n_mandatos_coletados", nrow(out), script = script)
registrar_numero("sapl_n_mandatos_pareados_bocel", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("sapl_taxa_pareamento_vereadores_2000_2024", round(out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)] / mand[ano_eleicao >= 2000, .N], 4), script = script)
for (f in unique(na.omit(out$forma_saida))) registrar_numero(paste0("sapl_n_forma_saida_", f), out[forma_saida == f & !is.na(id_mandato_bocel), .N], script = script)
print(out[!is.na(id_mandato_bocel), .N, by = .(metodo_pareamento)]); print(out[!is.na(id_mandato_bocel), .N, by = forma_saida][order(-N)])
cat("15_exercicio_camaras_municipais: concluido —", nrow(out), "mandatos SAPL,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "pareados\n")
