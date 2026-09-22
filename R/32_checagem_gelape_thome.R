# 32_checagem_gelape_thome.R — checagem externa contra a base de replicacao de Gelape e Thome
# (Where Do City Councilors Go?, BPSR; Harvard Dataverse doi:10.7910/DVN/G6ID99, CC0).
# A base deles acompanha os vereadores eleitos em 2008 e classifica a trajetoria; a chave de
# pessoa e o CPF. Aqui comparo o universo e a trajetoria com a reconstrucao do BOCEL.
# Entrada: data_raw/bases_secundarias/gelape_thome/replication/data/*.rds, data/{mandatos,pessoas}.csv
# Saida:   output/verificacao/checagem_gelape_*.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/32_checagem_gelape_thome.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/32_checagem_gelape_thome.R"
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
base <- "data_raw/bases_secundarias/gelape_thome/replication/data"
gt <- as.data.table(readRDS(file.path(base, "tse_carreiras2008_comtempo.rds")))
gt[, cpf := gsub("\\D", "", NR_CPF_CANDIDATO)]
gt <- gt[nchar(cpf) == 11 & !grepl("^0+$", cpf)]
cat("linhas na base de Gelape e Thome:", nrow(gt), "| CPFs unicos:", uniqueN(gt$cpf), "\n")
print(gt[, .N, by = ANO_ELEICAO][order(ANO_ELEICAO)])

m <- fread("data/mandatos.csv", na.strings = "NA", colClasses = list(character = c("sg_ue", "unidade_posicao")))
p <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character")
p[, cpf := gsub("\\D", "", nr_cpf)]
m <- merge(m, p[, .(id_pessoa, cpf)], by = "id_pessoa", all.x = TRUE)
ver08 <- unique(m[cargo == "VEREADOR" & ano_eleicao == 2008 & !is.na(cpf) & nchar(cpf) == 11, .(cpf, id_pessoa, sg_uf, unidade_posicao)])
cat("vereadores eleitos em 2008 no BOCEL com CPF:", nrow(ver08), "de", m[cargo == "VEREADOR" & ano_eleicao == 2008, .N], "\n")

gt08 <- unique(gt[, .(cpf)])
inter <- intersect(gt08$cpf, ver08$cpf)
cat("CPFs em comum:", length(inter), "| so em Gelape e Thome:", length(setdiff(gt08$cpf, ver08$cpf)),
    "| so no BOCEL:", length(setdiff(ver08$cpf, gt08$cpf)), "\n")
reg("chk_gelape_cpfs_base_deles", nrow(gt08))
reg("chk_gelape_cpfs_bocel_2008", nrow(ver08))
reg("chk_gelape_cpfs_em_comum", length(inter))
reg("chk_gelape_pct_cobertura_deles", round(100 * length(inter) / nrow(gt08), 2))

## trajetoria: eles classificam em TIPOLOGIA; aqui reconstruo o mesmo do lado do BOCEL
# eleito vereador de novo em 2012, eleito prefeito ou vice em 2012 ou 2016, eleito estadual/federal
seg <- m[!is.na(cpf) & ano_eleicao %in% c(2012, 2016, 2020), .(cpf, ano_eleicao, cargo)]
bocel_traj <- ver08[, .(cpf)]
bocel_traj[, `:=`(
  reeleito_vereador_2012 = cpf %in% seg[ano_eleicao == 2012 & cargo == "VEREADOR", cpf],
  eleito_executivo_municipal = cpf %in% seg[cargo %in% c("PREFEITO", "VICE-PREFEITO"), cpf],
  eleito_estadual_ou_federal = cpf %in% seg[cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "SENADOR", "GOVERNADOR", "VICE-GOVERNADOR"), cpf])]
cmp <- merge(bocel_traj, gt[, .(cpf, TIPOLOGIA, PASSADO)], by = "cpf")
tb <- cmp[, .(n = .N,
              pct_reeleito_2012 = round(100 * mean(reeleito_vereador_2012), 1),
              pct_executivo = round(100 * mean(eleito_executivo_municipal), 1),
              pct_estadual_federal = round(100 * mean(eleito_estadual_ou_federal), 1)), by = TIPOLOGIA][order(-n)]
print(tb)
fwrite(tb, "output/verificacao/checagem_gelape_tipologia.csv")
reg("chk_gelape_pct_eleitos_estadual_federal", round(100 * cmp[, mean(eleito_estadual_ou_federal)], 2))
reg("chk_gelape_pct_reeleitos_vereador_2012", round(100 * cmp[, mean(reeleito_vereador_2012)], 2))
fwrite(data.table(chave = c("gt_linhas", "gt_cpfs", "bocel_2008_com_cpf", "em_comum"),
                  valor = c(nrow(gt), nrow(gt08), nrow(ver08), length(inter))),
       "output/verificacao/checagem_gelape_universo.csv")
cat("32_checagem_gelape_thome: concluido\n")
