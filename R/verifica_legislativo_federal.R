# verifica_legislativo_federal.R — verificacao independente da forma de saida de senador e deputado federal (13/09/2026)
# Nao corrige nada. Reconta a partir das tabelas da casa (R/07), da biografia (R/59), das tabelas curadas (ref/) e da
# composicao atual da Camara, e confronta com data/mandatos.csv e com o registro de numeros.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_legislativo_federal.R
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R")); source(file.path(root, "lib", "asserts_rigor.R"))
script <- "R/verifica_legislativo_federal.R"
passou <- character(); falhou <- character()
ok <- function(nome, cond, det = "") { if (isTRUE(cond)) passou <<- c(passou, nome) else falhou <<- c(falhou, paste(nome, det))
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", nome, if (nzchar(det) && !isTRUE(cond)) paste0(" [", det, "]") else "", "\n") }
reg <- function(k, v) registrar_numero(k, v, script = script)
REF <- as.IDate(trimws(readLines("output/data_referencia.txt", warn = FALSE)[1]))

m <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("5", "6")]
m[, mf := as.IDate(mandato_fim)]
slf <- fread("data/saida_legislativo_federal.csv", colClasses = "character", na.strings = "NA")

## 1. cobertura: mandato encerrado tem forma, em curso nao tem
enc_sem <- m[mf < REF & forma_saida == "nao_observado"]
ok("mandato de senador e deputado federal encerrado tem forma de saida", nrow(enc_sem) == 0L, paste(nrow(enc_sem), "sem forma:", paste(head(enc_sem$id_mandato, 5), collapse = ",")))
reg("vlf_encerrados_sem_forma", nrow(enc_sem))
ok("nenhuma licenca ou afastamento como forma de saida do mandato", m[forma_saida %chin% c("licenca", "afastamento"), .N] == 0L,
   paste(m[forma_saida %chin% c("licenca", "afastamento"), .N]))
ok("toda forma federal vem das fontes autoritativas", all(m[forma_saida != "nao_observado", fonte_forma_saida] %chin% c("senado_api", "camara_api", "camara_biografia", "fonte_oficial_curada")),
   paste(unique(m[forma_saida != "nao_observado" & !fonte_forma_saida %chin% c("senado_api", "camara_api", "camara_biografia", "fonte_oficial_curada"), fonte_forma_saida]), collapse = ","))
ok("mandatos.csv reproduz saida_legislativo_federal.csv (forma, fim e precisao)",
   {x <- merge(m[, .(id_mandato, forma_saida, data_fim_efetiva, precisao_data_fim)], slf[cobertura %chin% c("historico_da_casa", "biografia_oficial_camara", "fonte_oficial_curada"),
      .(id_mandato, f2 = fifelse(is.na(forma_saida), "nao_observado", forma_saida), d2 = data_fim_efetiva, p2 = precisao_data_fim)], by = "id_mandato")
    x[f2 != forma_saida | !identical(is.na(d2), is.na(data_fim_efetiva)) | (!is.na(d2) & d2 != data_fim_efetiva) | (!is.na(p2) & p2 != precisao_data_fim), .N] == 0L})

## 2. reconta o ato definitivo pelo historico da casa, com regra propria
se <- fread("data/exercicio_senado.csv", colClasses = "character", na.strings = "NA")
ca <- fread("data/exercicio_camara.csv", colClasses = "character", na.strings = "NA")
ev <- rbindlist(list(
  se[!is.na(id_mandato), .(id_mandato, ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio),
     def = fcase(sigla_causa == "REN", "renuncia", sigla_causa == "FAL", "falecimento", sigla_causa %chin% c("PER", "CAS"), "cassacao", default = NA_character_))],
  ca[!is.na(id_mandato) & !situacao_final %chin% c("listado_sem_historico", "sem_registro_de_saida", "nao_pareado_camara"),
     .(id_mandato, ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio),
       def = fcase(grepl("definitivo - Renúncia", descricao_saida), "renuncia", grepl("Falecimento|dataFalecimento", descricao_saida), "falecimento",
                   grepl("Perda de Mandato", descricao_saida), "cassacao", grepl("Aposentadoria", descricao_saida), "aposentadoria", default = NA_character_))]))
ev <- merge(ev, m[, .(id_mandato, mi = as.IDate(mandato_inicio), mf)], by = "id_mandato")
ev <- ev[is.na(fim) | (fim >= mi - 60L & fim <= mf + 45L)]
defs <- ev[!is.na(def) & !is.na(fim)]
volta <- ev[!is.na(ini), .(id_mandato, ini)]
defs[, revertido := mapply(function(idm, f) volta[id_mandato == idm & ini > f + 1L, .N] > 0L, id_mandato, fim)]
rec <- defs[revertido == FALSE][order(id_mandato, fim)][, .SD[1], by = id_mandato]
cur <- fread("ref/eventos_legislativo_federal_fonte_oficial.csv", colClasses = "character", na.strings = c("", "NA"))
rec <- rec[!id_mandato %chin% cur$id_mandato]
cmp <- merge(rec[, .(id_mandato, def, fim = as.character(fim))], m[, .(id_mandato, forma_saida, data_fim_efetiva)], by = "id_mandato")
ok("ato definitivo recontado do historico = forma e data em mandatos.csv", cmp[def != forma_saida | fim != data_fim_efetiva, .N] == 0L,
   paste(cmp[def != forma_saida | fim != data_fim_efetiva, .N], "divergencias"))
reg("vlf_atos_definitivos_recontados", nrow(cmp))

## 3. tabelas curadas: fonte, confianca e mandato existente
ok("eventos curados: todo mandato existe, com URL de fonte e confianca alta ou media",
   all(cur$id_mandato %chin% m$id_mandato) && all(grepl("^https?://", cur$fonte_1)) && all(cur$confianca %chin% c("alta", "media")))
oc <- fread("ref/ocupantes_federais_fonte_oficial.csv", colClasses = "character", na.strings = c("", "NA"))
mand_all <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA", select = "id_mandato")
ok("ocupantes curados: toda cadeira nomeada existe em mandatos.csv", all(oc$id_mandato_cadeira %chin% mand_all$id_mandato),
   paste(setdiff(oc$id_mandato_cadeira, mand_all$id_mandato), collapse = ","))
ok("ocupantes curados: fonte com URL e confianca alta ou media", all(grepl("^https?://", oc$fonte_oficial)) && all(oc$confianca %chin% c("alta", "media")))
olf <- fread("data/ocupantes_legislativo_federal.csv", colClasses = "character", na.strings = "NA")
nao_ef <- olf[efetivado == "FALSE"]
ok("todo titular da casa sem mandato no TSE e nao efetivado tem cadeira curada", nao_ef[is.na(id_mandato_cadeira), .N] == 0L,
   paste(unique(nao_ef[is.na(id_mandato_cadeira), paste(casa, sg_uf, legislatura, nome_alt)]), collapse = "; "))

## 4. composicao atual da Camara: quem saiu sem evento no historico tem saida curada
f_comp <- "data_raw/camara/composicao_atual.json"
if (file.exists(f_comp)) {
  comp <- as.character(fromJSON(f_comp)$dados$id)
  ult <- ca[legislatura == "57"][order(id_deputado_camara, data_inicio_exercicio)][, .SD[.N], by = id_deputado_camara]
  fora <- ult[situacao_final == "Exercício" & !id_deputado_camara %chin% comp]
  sem_explicacao <- fora[!((!is.na(id_mandato) & id_mandato %chin% cur$id_mandato) | nome_parlamentar %chin% oc$nome_alt)]
  ok("deputado em exercicio no historico e fora da composicao atual tem saida curada", nrow(sem_explicacao) == 0L,
     paste(sem_explicacao$nome_parlamentar, collapse = ","))
  reg("vlf_saidas_silenciosas_na_camara", nrow(fora))
}

## 5. contagem de cadeiras
ok("senadores eleitos em 2018 = 54 (cadeira de Selma Arruda restaurada)", m[cd_cargo == "5" & ano_eleicao == "2018", .N] == 54L,
   paste(m[cd_cargo == "5" & ano_eleicao == "2018", .N]))
ok("deputados federais = 513 por eleicao", m[cd_cargo == "6", .N, by = ano_eleicao][, all(N == 513L)])

gravar_relatorio_verificacao(alvo = "legislativo federal: data/mandatos.csv (cd_cargo 5 e 6), data/saida_legislativo_federal.csv, ref/eventos_legislativo_federal_fonte_oficial.csv, ref/ocupantes_federais_fonte_oficial.csv",
                             script = script, passou = passou, falhou = falhou,
                             fora_de_cobertura = c("veracidade das materias de imprensa alem do trecho literal conferido", "atos de perda de mandato anteriores a 1999"))
cat(sprintf("\nPASSOU: %d | FALHOU: %d\n", length(passou), length(falhou)))
