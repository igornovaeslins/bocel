# verifica_executivos.R — verificacao independente da forma de saida de presidente, vice-presidente, governador e
# vice-governador (13/09/2026). Nao corrige nada.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_executivos.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
source(file.path(root, "lib", "tipo_fonte.R"))
script <- "R/verifica_executivos.R"
passou <- character(); falhou <- character()
ok <- function(nome, cond, det = "") { if (isTRUE(cond)) passou <<- c(passou, nome) else falhou <<- c(falhou, paste(nome, det))
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", nome, if (nzchar(det) && !isTRUE(cond)) paste0(" [", det, "]") else "", "\n") }
reg <- function(k, v) registrar_numero(k, v, script = script)
REF <- as.IDate(trimws(readLines("output/data_referencia.txt", warn = FALSE)[1]))

m <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("1", "2", "3", "4")]
m[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
cur <- rbindlist(list(fread("ref/eventos_governos_fonte_oficial.csv", colClasses = "character", na.strings = c("", "NA")),
                      fread("ref/eventos_presidencia_fonte_oficial.csv", colClasses = "character", na.strings = c("", "NA"))), fill = TRUE)

## 1. cobertura
enc_sem <- m[mf < REF & forma_saida == "nao_observado"]
ok("mandato executivo encerrado tem forma de saida", nrow(enc_sem) == 0L, paste(enc_sem$id_mandato, collapse = ","))
reg("vexe_encerrados_sem_forma", nrow(enc_sem))
ok("seis presidentes e seis vices encerrados, dois em curso", m[cd_cargo %chin% c("1", "2") & forma_saida != "nao_observado", .N] == 12L &&
     m[cd_cargo %chin% c("1", "2") & forma_saida == "nao_observado", .N] == 2L)
ok("impeachment de 2016 registrado na presidencia e posse do vice na mesma data",
   m[cd_cargo == "1" & ano_eleicao == "2014", forma_saida == "impeachment" & data_fim_efetiva == "2016-08-31"] &&
   m[cd_cargo == "2" & ano_eleicao == "2014", forma_saida == "assumiu_titular" & data_fim_efetiva == "2016-08-31"])

## 2. nenhuma saida antecipada sem evento curado com fonte
we <- fread("data/wikipedia_estadual.csv", colClasses = "character", na.strings = "NA")[cargo %chin% c("GOVERNADOR", "VICE-GOVERNADOR") & condicao == "titular" & !is.na(id_mandato_bocel)]
w1 <- we[, .(fim_wp = suppressWarnings(max(as.IDate(fim), na.rm = TRUE))), by = .(id_mandato = id_mandato_bocel)][is.finite(fim_wp)]
ant <- merge(w1, m[, .(id_mandato, mf)], by = "id_mandato")[fim_wp < mf - 30L]
sem_ev <- ant[!id_mandato %chin% cur$id_mandato]
ok("toda saida antecipada pela lista tem evento curado com fonte", nrow(sem_ev) == 0L, paste(sem_ev$id_mandato, collapse = ","))
reg("vexe_saidas_antecipadas_pela_lista", nrow(ant))
ok("eventos curados: fonte com URL, confianca alta ou media e sem Wikipedia como fonte principal",
   all(grepl("^https?://", cur$fonte_1)) && all(cur$confianca %chin% c("alta", "media")) && !any(grepl("wikipedia.org", cur$fonte_1)))
cmp <- merge(cur[forma_saida != "em_curso", .(id_mandato, f = fifelse(forma_saida == "afastamento_temporario", "fim_regular", forma_saida), d = data_evento)],
             m[, .(id_mandato, forma_saida, data_fim_efetiva, mf)], by = "id_mandato")
cmp[f == "fim_regular" & forma_saida == "fim_regular", d := data_fim_efetiva]
ok("mandatos.csv reproduz os eventos curados (forma e data)", cmp[f != forma_saida | d != data_fim_efetiva, .N] == 0L,
   paste(cmp[f != forma_saida | d != data_fim_efetiva, id_mandato], collapse = ","))

## 3. coerencia entre titular e vice na mesma chapa
g <- m[cd_cargo %chin% c("1", "3"), .(ano_eleicao, unidade_posicao, cd_vice = fifelse(cd_cargo == "1", "2", "4"), f_tit = forma_saida, d_tit = as.IDate(data_fim_efetiva))]
v <- m[cd_cargo %chin% c("2", "4"), .(ano_eleicao, unidade_posicao, cd_vice = cd_cargo, f_vice = forma_saida, d_vice = as.IDate(data_fim_efetiva))]
ch <- merge(g, v, by = c("ano_eleicao", "unidade_posicao", "cd_vice"))
saida_def <- c("renuncia", "falecimento", "cassacao", "impeachment")
inc <- ch[f_tit %chin% saida_def & !f_vice %chin% c(saida_def, "assumiu_titular", "retotalizacao")]
ok("titular com saida definitiva: o vice assumiu, saiu antes ou caiu junto na cassacao", nrow(inc) == 0L,
   paste(inc[, paste(unidade_posicao, ano_eleicao, f_tit, f_vice)], collapse = "; "))
# a posse formal do vice pode vir dias depois da morte ou renuncia do titular (Deda morreu em 02/12/2013 e Jackson Barreto tomou posse em 10/12)
datas <- ch[f_tit %chin% saida_def & f_vice == "assumiu_titular" & (as.integer(d_vice) < as.integer(d_tit) | as.integer(d_vice) - as.integer(d_tit) > 15L)]
ok("vice assumiu na data da saida do titular ou ate 15 dias depois", nrow(datas) == 0L,
   paste(datas[, paste(unidade_posicao, ano_eleicao, d_tit, d_vice)], collapse = "; "))

## 4. rotulo de fonte do evento curado reproduz a classificacao de lib/tipo_fonte.R
sx <- fread("data/saida_executivos.csv", colClasses = "character", na.strings = "NA")
VOCAB_FONTE <- c("fonte_oficial_curada", "base_dhbb_curada", "noticia_orgao_publico_curada", "pista_nao_oficial", "wikipedia")
ok("rotulo de fonte de data/saida_executivos.csv no vocabulario esperado",
   all(sx[!is.na(fonte), fonte] %chin% VOCAB_FONTE),
   paste(setdiff(sx[!is.na(fonte), unique(fonte)], VOCAB_FONTE), collapse = ","))
## o vice derivado (via derivado_do_titular_curado) nao tem linha propria em cur, entao o merge abaixo (por id_mandato)
## so traz o mandato com evento curado proprio, que e o unico caso onde fonte vem direto de fonte_1/fonte_2
cev <- merge(sx[, .(id_mandato, fonte)], cur[, .(id_mandato, fonte_1, fonte_2)], by = "id_mandato")
cev[, esperado := rotulo_evento_curado(fonte_1, fonte_2)]
ok("rotulo do evento curado bate com lib/tipo_fonte.R aplicado a fonte_1/fonte_2",
   cev[fonte != esperado, .N] == 0L,
   paste(cev[fonte != esperado, id_mandato], collapse = ","))
reg("vexe_rotulo_pista_nao_oficial", sx[fonte %chin% "pista_nao_oficial", .N])

gravar_relatorio_verificacao(alvo = "executivos: data/mandatos.csv (cd_cargo 1 a 4), data/saida_executivos.csv, ref/eventos_governos_fonte_oficial.csv, ref/eventos_presidencia_fonte_oficial.csv",
                             script = script, passou = passou, falhou = falhou,
                             fora_de_cobertura = c("fim regular de vice-governador confirmado so pela lista da Wikipedia (confianca media)",
                                                   "interinos que nao eram o vice (presidente da Assembleia ou do Tribunal de Justica) na camada de ocupacao",
                                                   "admissibilidade de dominio de agencia publica de comunicacao servida fora de dominio .gov.br (ex.: ebc.com.br) na classificacao de lib/tipo_fonte.R"))
cat(sprintf("\nPASSOU: %d | FALHOU: %d\n", length(passou), length(falhou)))
