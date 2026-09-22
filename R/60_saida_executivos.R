# 60_saida_executivos.R — forma de saida dos mandatos de presidente, vice-presidente, governador e vice-governador
# (13/09/2026)
#
# Por que existe. A presuncao de fim regular pelo universo comparavel nao vale para cargo executivo, porque a varredura
# de casa legislativa nao enxerga renuncia de governador, e o banco marcava como fim regular presumido governadores
# que renunciaram para disputar outro cargo (Garotinho em 2002, Alckmin em 2006, Serra em 2010, Cabral em 2014) ou
# que sofreram impeachment (Witzel em 2021). Aqui cada mandato executivo recebe forma de saida por uma de duas vias:
#   1. evento curado com fonte oficial ou materia, com trecho literal conferido na pagina (ref/eventos_governos_fonte_
#      oficial.csv e ref/eventos_presidencia_fonte_oficial.csv), para toda saida antecipada, toda posse de vice como
#      titular, todo mandato sem par nas listas e toda a Presidencia;
#   2. para o mandato cumprido ate o fim sem evento curado, a cadeia da lista de governadores da Wikipedia com o
#      titular em exercicio ate o fim convencional, conferida com a data de fim do Wikidata quando existe; o grau de
#      confirmacao fica na coluna confianca.
# Mandato de 2022 sem saida registrada fica em curso.
#
# A coluna fonte do evento curado (via 1) nao e mais um rotulo unico: lib/tipo_fonte.R classifica fonte_1 e fonte_2 e
# devolve o melhor dos dois em fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada ou, quando as duas
# fontes sao imprensa comum ou Wikipedia, pista_nao_oficial (21/09/2026). O vice derivado da saida do titular (abaixo)
# herda o mesmo rotulo do titular.
#
# Entrada:  data/mandatos.csv (cd_cargo 1 a 4), data/wikipedia_estadual.csv, data/wikidata_mandatos.csv,
#           ref/eventos_governos_fonte_oficial.csv, ref/eventos_presidencia_fonte_oficial.csv
# Saida:    data/saida_executivos.csv (uma linha por mandato)
# Execucao: cd ~/bocel && Rscript --vanilla R/60_saida_executivos.R
set.seed(20260913)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
source(file.path(root, "lib", "tipo_fonte.R"))
script <- "R/60_saida_executivos.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
# data de referencia fixa do banco (a mesma do R/53), para a reconstrucao em outro dia nao mudar o que esta em curso
f_ref <- "output/data_referencia.txt"
HOJE <- as.IDate(if (file.exists(f_ref)) trimws(readLines(f_ref, warn = FALSE)[1]) else format(Sys.Date(), "%Y-%m-%d"))
FORMAS <- c("fim_regular", "renuncia", "falecimento", "cassacao", "impeachment", "assumiu_titular", "nao_tomou_posse",
            "afastamento_temporario", "em_curso")

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("1", "2", "3", "4")]
mand[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]

## ---------------------------------------------------------------- 1. eventos curados
cur <- rbindlist(lapply(c("ref/eventos_governos_fonte_oficial.csv", "ref/eventos_presidencia_fonte_oficial.csv"), function(f)
  if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA")) else NULL), use.names = TRUE, fill = TRUE)
stopifnot(!anyDuplicated(cur$id_mandato), all(cur$id_mandato %chin% mand$id_mandato), all(cur$forma_saida %chin% FORMAS),
          all(cur$precisao_data %chin% c("ato", "convencional", "intervalo", "")))
stopifnot(cur[forma_saida != "em_curso", all(grepl("^\\d{4}-\\d{2}-\\d{2}$", data_evento))])
# evento curado sem fonte que o sustente nao entra
cur <- cur[!is.na(fonte_1) & confianca %chin% c("alta", "media")]

## ---------------------------------------------------------------- 2. cadeia das listas para o mandato cumprido
we <- fread("data/wikipedia_estadual.csv", colClasses = "character", na.strings = "NA")[cargo %chin% c("GOVERNADOR", "VICE-GOVERNADOR")]
wd <- fread("data/wikidata_mandatos.csv", colClasses = "character", na.strings = "NA")[cargo %chin% c("GOVERNADOR", "VICE-GOVERNADOR", "PRESIDENTE", "VICE-PRESIDENTE")]
w1 <- we[!is.na(id_mandato_bocel) & condicao == "titular",
         .(fim_wp = suppressWarnings(max(as.IDate(fim), na.rm = TRUE)), url_wp = url_pagina[1]), by = .(id_mandato = id_mandato_bocel)]
d1 <- wd[!is.na(id_mandato_bocel), .(fim_wd = suppressWarnings(max(as.IDate(fim), na.rm = TRUE)), url_wd = url[1]), by = .(id_mandato = id_mandato_bocel)]

res <- merge(mand[, .(id_mandato, cd_cargo, cargo, sg_uf, ano_eleicao, mi, mf)], cur[, .(id_mandato, forma_c = forma_saida, data_c = data_evento,
             precisao_c = precisao_data, assumiu, fonte_1, fonte_2, confianca_c = confianca, observacao)], by = "id_mandato", all.x = TRUE)
res <- merge(merge(res, w1, by = "id_mandato", all.x = TRUE), d1, by = "id_mandato", all.x = TRUE)
res[is.infinite(fim_wp), fim_wp := NA]; res[is.infinite(fim_wd), fim_wd := NA]
perto <- function(a, b) !is.na(a) & abs(as.integer(a) - as.integer(b)) <= 31L
res[, via := fcase(!is.na(forma_c), "evento_curado",
                   mf < HOJE & perto(fim_wp, mf) & perto(fim_wd, mf), "listas_wikipedia_e_wikidata",
                   mf < HOJE & perto(fim_wp, mf) & is.na(fim_wd), "lista_wikipedia",
                   mf >= HOJE, "em_curso_sem_evento",
                   default = "sem_confirmacao")]
res[, `:=`(
  forma_saida = fcase(via == "evento_curado" & forma_c == "em_curso", NA_character_,
                      via == "evento_curado" & forma_c == "afastamento_temporario" & mf < HOJE, "fim_regular",
                      via == "evento_curado" & forma_c == "afastamento_temporario", NA_character_,
                      via == "evento_curado", forma_c,
                      via %chin% c("listas_wikipedia_e_wikidata", "lista_wikipedia"), "fim_regular",
                      default = NA_character_),
  data_fim_efetiva = fcase(via == "evento_curado" & !forma_c %chin% c("em_curso", "afastamento_temporario"), data_c,
                           via == "evento_curado" & forma_c == "afastamento_temporario" & mf < HOJE, as.character(mf),
                           via %chin% c("listas_wikipedia_e_wikidata", "lista_wikipedia"), as.character(mf),
                           default = NA_character_),
  precisao_data_fim = fcase(via == "evento_curado" & forma_c == "afastamento_temporario" & mf < HOJE, "convencional",
                           via == "evento_curado", precisao_c,
                           via %chin% c("listas_wikipedia_e_wikidata", "lista_wikipedia"), "convencional", default = NA_character_),
  confianca = fcase(via == "evento_curado", confianca_c, via == "listas_wikipedia_e_wikidata", "alta",
                    via == "lista_wikipedia", "media", default = NA_character_),
  # o rotulo do evento curado segue a melhor das duas fontes (fonte_1/fonte_2), pela classificacao de lib/tipo_fonte.R,
  # e nao mais um unico rotulo fixo para toda fonte oficial curada (21/09/2026)
  fonte = fcase(via == "evento_curado", rotulo_evento_curado(fonte_1, fonte_2),
                via %chin% c("listas_wikipedia_e_wikidata", "lista_wikipedia"), "wikipedia",
                default = NA_character_),
  url_fonte = fcase(via == "evento_curado", fonte_1, via %chin% c("listas_wikipedia_e_wikidata", "lista_wikipedia"), url_wp, default = NA_character_),
  em_curso = (via == "em_curso_sem_evento") | (via == "evento_curado" & (forma_c == "em_curso" | (forma_c == "afastamento_temporario" & mf >= HOJE))))]

## ---------------------------------------------------------------- vice sem evento proprio quando o titular sai
# Titular com renuncia, morte ou impeachment em evento curado e vice sem evento proprio: o vice assumiu o governo na data
# da saida do titular, com a mesma fonte (Joao Azevedo renunciou em 2026 e Lucas Ribeiro assumiu a Paraiba, e a lista da
# Wikipedia ainda nao trazia o fim da vice). Cassacao fica de fora, porque cai a chapa inteira.
tit <- res[cd_cargo %chin% c("1", "3") & via == "evento_curado" & forma_saida %chin% c("renuncia", "falecimento", "impeachment") &
             as.IDate(data_fim_efetiva) < mf,
           .(ano_eleicao, sg_uf, cd_vice = fifelse(cd_cargo == "1", "2", "4"), d_tit = data_fim_efetiva, p_tit = precisao_data_fim,
             u_tit = url_fonte, c_tit = confianca, f_tit = fonte)]
res <- merge(res, tit, by.x = c("ano_eleicao", "sg_uf", "cd_cargo"), by.y = c("ano_eleicao", "sg_uf", "cd_vice"), all.x = TRUE)
der <- res$cd_cargo %chin% c("2", "4") & res$via != "evento_curado" & !is.na(res$d_tit)
# o vice derivado herda o rotulo de fonte que o titular recebeu (nem sempre fonte_oficial_curada, desde 21/09/2026)
res[der, `:=`(forma_saida = "assumiu_titular", data_fim_efetiva = d_tit, precisao_data_fim = p_tit, confianca = c_tit,
              fonte = f_tit, url_fonte = u_tit, via = "derivado_do_titular_curado", em_curso = FALSE)]
res[, c("d_tit", "p_tit", "u_tit", "c_tit", "f_tit") := NULL]
reg("sexe_vices_derivados_do_titular_curado", sum(der))

## ---------------------------------------------------------------- conferencias e saida
stopifnot(!anyDuplicated(res$id_mandato), nrow(res) == nrow(mand))
stopifnot(res[!is.na(forma_saida) & forma_saida != "fim_regular", all(as.IDate(data_fim_efetiva) >= mi - 60L & as.IDate(data_fim_efetiva) <= mf + 45L)])
res[precisao_data_fim %chin% "", precisao_data_fim := NA_character_]
out <- res[, .(id_mandato, cargo, sg_uf, ano_eleicao, forma_saida, data_fim_efetiva, precisao_data_fim, em_curso, via, confianca, fonte, url_fonte,
               fonte_2, assumiu, observacao)]
setorder(out, cargo, sg_uf, ano_eleicao)
fwrite(out, "data/saida_executivos.csv", na = "NA", quote = TRUE)
fwrite(out[via == "sem_confirmacao"], "output/verificacao/executivos_sem_confirmacao.csv", na = "NA", quote = TRUE)
for (rt in c("fonte_oficial_curada", "base_dhbb_curada", "noticia_orgao_publico_curada", "pista_nao_oficial", "wikipedia")) {
  reg(sprintf("sexe_rotulo_%s", rt), out[fonte %chin% rt, .N])
}
for (cg in unique(out$cargo)) {
  k <- tolower(gsub("[^A-Za-z]+", "_", cg))
  r <- out[cargo == cg]
  reg(sprintf("sexe_%s_mandatos", k), nrow(r))
  reg(sprintf("sexe_%s_encerrados_com_forma", k), r[em_curso == FALSE & !is.na(forma_saida), .N])
  reg(sprintf("sexe_%s_em_curso", k), r[em_curso == TRUE, .N])
  reg(sprintf("sexe_%s_sem_confirmacao", k), r[via == "sem_confirmacao", .N])
  reg(sprintf("sexe_%s_evento_curado", k), r[via == "evento_curado", .N])
  reg(sprintf("sexe_%s_fim_regular_so_lista_wikipedia", k), r[via == "lista_wikipedia", .N])
}
cat("60_saida_executivos: concluido\n")
print(dcast(out[, .N, by = .(cargo, forma = fifelse(is.na(forma_saida), fifelse(em_curso, "em_curso", "sem_confirmacao"), forma_saida))], forma ~ cargo, fill = 0))
print(out[, .N, by = .(cargo, via, confianca)][order(cargo, via)])
