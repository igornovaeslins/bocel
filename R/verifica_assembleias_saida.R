# verifica_assembleias_saida.R — verificacao independente da forma de saida de deputado estadual e distrital (13/09/2026)
# Confere a curadoria com fonte oficial (ref/), a camada do R/61 e o que chegou a data/mandatos.csv. Nao corrige nada.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_assembleias_saida.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/verifica_assembleias_saida.R"
passou <- character(); falhou <- character()
ok <- function(nome, cond, det = "") { if (isTRUE(cond)) passou <<- c(passou, nome) else falhou <<- c(falhou, paste(nome, det))
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", nome, if (nzchar(det) && !isTRUE(cond)) paste0(" [", substr(det, 1, 300), "]") else "", "\n") }
reg <- function(k, v) registrar_numero(k, v, script = script)
REF <- as.IDate(trimws(readLines("output/data_referencia.txt", warn = FALSE)[1]))
DEF <- c("renuncia", "falecimento", "cassacao", "nao_tomou_posse", "retotalizacao")

m <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("7", "8")]
m[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
le <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8") else NULL
ev <- le("ref/eventos_assembleias_fonte_oficial.csv")
cf <- le("ref/composicao_final_assembleias_fonte_oficial.csv")
sas <- le("data/saida_assembleias.csv")

## 1. curadoria: fonte, trecho e vocabulario
cur <- rbind(if (!is.null(ev)) ev[, .(url, trecho, tipo_fonte, confianca, id_mandato)], if (!is.null(cf)) cf[, .(url, trecho, tipo_fonte, confianca, id_mandato)])
if (!is.null(cur) && nrow(cur)) {
  ok("curadoria com URL, trecho de 15 ou mais caracteres e confianca alta ou media",
     all(grepl("^https?://", cur$url)) && all(nchar(cur$trecho) >= 15L) && all(cur$confianca %chin% c("alta", "media")))
  ok("curadoria sem Wikipedia como fonte", !any(grepl("wikipedia\\.org", cur$url)))
  ok("imprensa nunca com confianca alta", !any(cur$tipo_fonte == "imprensa" & cur$confianca == "alta"))
  ok("id_mandato da curadoria existe entre os deputados estaduais e distritais", all(is.na(cur$id_mandato) | cur$id_mandato %chin% m$id_mandato),
     paste(setdiff(na.omit(cur$id_mandato), m$id_mandato), collapse = ","))
}
if (!is.null(ev) && nrow(ev)) {
  e <- merge(ev[!is.na(id_mandato) & evento %chin% DEF], m[, .(id_mandato, mi, mf)], by = "id_mandato")
  fora <- e[as.IDate(data_evento) < mi - 60L | as.IDate(data_evento) > mf + 45L]
  ok("ato definitivo curado dentro da janela do mandato", nrow(fora) == 0L, paste(fora$id_mandato, collapse = ","))
}

## 2. a camada do R/61 chega intacta a mandatos.csv
if (!is.null(sas) && nrow(sas)) {
  cmp <- merge(sas[, .(id_mandato, f61 = forma_saida, d61 = data_fim_efetiva)], m[, .(id_mandato, forma_saida, data_fim_efetiva, fonte_forma_saida)], by = "id_mandato")
  ok("toda linha do R/61 esta em mandatos.csv", nrow(cmp) == nrow(sas))
  dif <- cmp[f61 != forma_saida | d61 != data_fim_efetiva | fonte_forma_saida != "fonte_oficial_curada"]
  ok("mandatos.csv reproduz forma, data e fonte da curadoria", nrow(dif) == 0L, paste(dif$id_mandato, collapse = ","))
  # ato definitivo curado: o primeiro nao revertido e o que vale
  if (!is.null(ev) && nrow(ev)) {
    ret <- ev[evento == "retorno" & !is.na(id_mandato), .(id_mandato, d_ret = as.IDate(data_evento))]
    d1 <- ev[evento %chin% DEF & !is.na(id_mandato), .(id_mandato, evento, d = as.IDate(data_evento))]
    d1 <- d1[!vapply(seq_len(nrow(d1)), function(k) any(ret$id_mandato == d1$id_mandato[k] & ret$d_ret > d1$d[k]), logical(1))]
    setorder(d1, id_mandato, d); d1 <- d1[, .SD[1], by = id_mandato]
    x <- merge(d1, m[, .(id_mandato, forma_saida, data_fim_efetiva)], by = "id_mandato")
    dx <- x[evento != forma_saida | as.character(d) != data_fim_efetiva]
    ok("primeiro ato definitivo nao revertido da curadoria e a forma do mandato", nrow(dx) == 0L, paste(dx$id_mandato, collapse = ","))
  }
  ok("fim regular curado com data igual ao fim convencional", sas[forma_saida == "fim_regular", all(data_fim_efetiva == m$mandato_fim[match(id_mandato, m$id_mandato)])])
}

## 3. cobertura (meta da fase 5)
enc <- m[mf < REF]
sem <- enc[forma_saida == "nao_observado"]
reg("vas_encerrados", nrow(enc))
reg("vas_encerrados_sem_forma", nrow(sem))
reg("vas_encerrados_com_licenca_ou_afastamento_como_saida", enc[forma_saida %chin% c("licenca", "afastamento"), .N])
reg("vas_encerrados_com_outro", enc[forma_saida == "outro", .N])
reg("vas_encerrados_por_curadoria", enc[fonte_forma_saida %chin% "fonte_oficial_curada", .N])
# lacuna que sobra depois da curadoria: sem forma em mandatos.csv e sem resolucao no R/61. Antes da integracao pelo R/10 e a
# projecao do buraco, e depois dela coincide com vas_encerrados_sem_forma
res61 <- if (!is.null(sas)) sas$id_mandato else character()
reg("vas_encerrados_sem_forma_fora_da_curadoria", enc[forma_saida == "nao_observado" & !id_mandato %chin% res61, .N])
reg("vas_encerrados_resolvidos_pela_curadoria", enc[id_mandato %chin% res61, .N])
por_uf <- enc[, .(encerrados = .N, sem_forma = sum(forma_saida == "nao_observado"), sem_forma_fora_da_curadoria = sum(forma_saida == "nao_observado" & !id_mandato %chin% res61),
                  resolvidos_pela_curadoria = sum(id_mandato %chin% res61), outro = sum(forma_saida == "outro"),
                  licenca_como_saida = sum(forma_saida %chin% c("licenca", "afastamento")), curadoria = sum(fonte_forma_saida %chin% "fonte_oficial_curada")),
              by = .(sg_uf)][order(-sem_forma)]
fwrite(por_uf, "output/verificacao/assembleias_saida_por_uf.csv")
ok("mandato de deputado estadual ou distrital encerrado tem forma de saida", nrow(sem) == 0L,
   sprintf("%d sem forma; por UF em output/verificacao/assembleias_saida_por_uf.csv", nrow(sem)))
# licenca e afastamento nao encerram mandato; so ficam como forma quando a fonte nao registra retorno nem composicao final
if (!is.null(cf) && nrow(cf)) {
  lic <- enc[forma_saida %chin% c("licenca", "afastamento") & id_mandato %chin% cf$id_mandato]
  ok("mandato na composicao final da casa nao termina em licenca ou afastamento", nrow(lic) == 0L, paste(lic$id_mandato, collapse = ","))
}

gravar_relatorio_verificacao(alvo = "assembleias: data/mandatos.csv (cd_cargo 7 e 8), data/saida_assembleias.csv, ref/eventos_assembleias_fonte_oficial.csv, ref/composicao_final_assembleias_fonte_oficial.csv",
                             script = script, passou = passou, falhou = falhou,
                             fora_de_cobertura = c("fidelidade do trecho ao bruto, conferida no R/61a sobre data_raw, que fica fora do deposito",
                                                   "completude do historico da casa quando o fim regular se apoia em historico completo (confianca media)"))
cat(sprintf("\nPASSOU: %d | FALHOU: %d\n", length(passou), length(falhou)))
