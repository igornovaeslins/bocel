# sonda_chapa_vices.R — mede o buraco da chapa dos vices e a cobertura das rotas candidatas
#
# Ancilar: NAO entra em R/00_reconstruir.sh e nao grava nada em data/. Mede sobre
# data_raw/parquet/cand_<ANO>.parquet e sobre data/mandatos.csv, e registra os numeros da
# medicao de 06/09/2026.
#
# Entrada:  data_raw/parquet/cand_1998.parquet, cand_2000.parquet, data/mandatos.csv
# Saida:    output/verificacao/sonda_chapa_vices.csv + chaves chapa_ em output/numeros_assinatura.txt
# Execucao: Rscript --vanilla R/sonda_chapa_vices.R
set.seed(20260906)
suppressPackageStartupMessages({library(data.table); library(arrow)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
ESTE <- file.path(root, "R", "sonda_chapa_vices.R")
reg <- function(k, v) { registrar_numero(k, v, script = ESTE); v }
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)

# vazio do TSE: os rotulos de ausencia variam por ano no mesmo campo
nz <- function(v) fifelse(is.na(v) | v %in% c("", "-1", "-3", "-4", "#NE", "#NULO", "#NULO#"),
                          NA_character_, v)
ler_ano <- function(ano) {
  x <- setDT(read_parquet(sprintf("data_raw/parquet/cand_%d.parquet", ano)))
  x <- x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, `:=`(cd_cargo = as.integer(CD_CARGO), nr_turno = as.integer(NR_TURNO),
           sqcol = nz(SQ_COLIGACAO))]
  x[, ue := fcase(cd_cargo %in% 11:13, SG_UE, cd_cargo %in% 1:2, "BR", default = SG_UF)]
  x[]
}
ELEITO <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MEDIA", "ELEITO POR MÉDIA")

## ---------------------------------------------------------------- linha de base no banco
m <- fread("data/mandatos.csv", colClasses = "character",
           select = c("ano_eleicao", "cargo"))
base <- m[cargo %chin% c("PREFEITO","VICE-PREFEITO","GOVERNADOR","VICE-GOVERNADOR",
                         "PRESIDENTE","VICE-PRESIDENTE"), .N, by = .(ano_eleicao, cargo)]
setorder(base, ano_eleicao, cargo)
cat("=== linha de base: titular x vice no banco publicado ===\n"); print(base)
reg("chapa_base_vice_prefeito_2000", base[ano_eleicao == "2000" & cargo == "VICE-PREFEITO", N])
reg("chapa_base_prefeito_2000",      base[ano_eleicao == "2000" & cargo == "PREFEITO", N])
reg("chapa_base_vice_governador_1998", base[ano_eleicao == "1998" & cargo == "VICE-GOVERNADOR", N])

## ---------------------------------------------------------------- 2000, causa do buraco
x00 <- ler_ano(2000L)
pref <- x00[cd_cargo == 11L]; vice <- x00[cd_cargo == 12L]
cat("\n=== 2000: situacao do cadastro ===\n")
print(pref[, .N, by = DS_SITUACAO_CANDIDATURA][order(-N)])
print(vice[, .N, by = .(DS_SIT_TOT_TURNO)])
reg("chapa_2000_prefeito_linhas", nrow(pref))
reg("chapa_2000_prefeito_deferido", pref[DS_SITUACAO_CANDIDATURA == "DEFERIDO", .N])
reg("chapa_2000_vice_linhas", nrow(vice))
reg("chapa_2000_vice_sit_tot_nula", vice[is.na(nz(DS_SIT_TOT_TURNO)), .N])

el <- pref[toupper(DS_SIT_TOT_TURNO) %chin% ELEITO]
reg("chapa_2000_prefeito_eleito_linhas", nrow(el))
reg("chapa_2000_prefeito_eleito_turno1", el[nr_turno == 1L, .N])
reg("chapa_2000_prefeito_eleito_turno2", el[nr_turno == 2L, .N])
reg("chapa_2000_municipios_com_prefeito_eleito", uniqueN(el$ue))

cat("\n=== 2000: agremiacao do vice x presenca de SQ_COLIGACAO ===\n")
print(vice[, .N, by = .(TP_AGREMIACAO, tem_sq = !is.na(sqcol))])
reg("chapa_2000_vice_com_sq_coligacao", vice[!is.na(sqcol), .N])
reg("chapa_2000_vice_partido_isolado",  vice[is.na(sqcol), .N])

## ---------------------------------------------------------------- 2000, rotas
rota_a <- merge(vice[!is.na(sqcol)], unique(el[!is.na(sqcol), .(ue, sqcol)]), by = c("ue", "sqcol"))
rota_b <- merge(vice[is.na(sqcol)], unique(el[is.na(sqcol), .(ue, NR_PARTIDO)]), by = c("ue", "NR_PARTIDO"))
uni <- rbind(rota_a, rota_b, fill = TRUE)
cat("\n=== 2000: cobertura das rotas ===\n")
cat(sprintf("rota A (ue, SQ_COLIGACAO): %d vices em %d municipios\n", nrow(rota_a), uniqueN(rota_a$ue)))
cat(sprintf("rota B (ue, NR_PARTIDO):   %d vices em %d municipios\n", nrow(rota_b), uniqueN(rota_b$ue)))
cat(sprintf("uniao:                     %d vices em %d municipios (de %d com prefeito eleito)\n",
            nrow(uni), uniqueN(uni$ue), uniqueN(el$ue)))
reg("chapa_2000_rota_a_vices", nrow(rota_a)); reg("chapa_2000_rota_a_municipios", uniqueN(rota_a$ue))
reg("chapa_2000_rota_b_vices", nrow(rota_b)); reg("chapa_2000_rota_b_municipios", uniqueN(rota_b$ue))
reg("chapa_2000_uniao_vices", nrow(uni));     reg("chapa_2000_uniao_municipios", uniqueN(uni$ue))

dup <- uni[, .N, by = ue][N > 1]
reg("chapa_2000_municipios_com_mais_de_um_vice", nrow(dup))
reg("chapa_2000_casados_substituidos", uni[ST_SUBSTITUIDO == "S", .N])
cat(sprintf("\nmunicipios com mais de um vice casado: %d | entre os casados, %d substituidos\n",
            nrow(dup), uni[ST_SUBSTITUIDO == "S", .N]))
cat("\n=== o caso do numero divergente dentro da mesma chapa (municipio 00396) ===\n")
print(uni[ue == "00396", .(ue, NR_CANDIDATO, NM_CANDIDATO, SG_PARTIDO, sqcol,
                           ST_SUBSTITUIDO, DS_SITUACAO_CANDIDATURA)])

falta <- sort(setdiff(el$ue, uni$ue))
cat("\n=== municipios com prefeito eleito e nenhum vice casado ===\n"); print(falta)
reg("chapa_2000_municipios_sem_vice_por_nenhuma_rota", length(falta))

## ---------------------------------------------------------------- 1998
x98 <- ler_ano(1998L)
cat("\n=== 1998: SQ_COLIGACAO nos vices ===\n")
for (cg in c(4L, 2L)) {
  v <- x98[cd_cargo == cg]
  cat(sprintf("cargo %d: %d linhas, %d com SQ_COLIGACAO\n", cg, nrow(v), v[!is.na(sqcol), .N]))
  reg(sprintf("chapa_1998_cargo%d_linhas", cg), nrow(v))
  reg(sprintf("chapa_1998_cargo%d_com_sq_coligacao", cg), v[!is.na(sqcol), .N])
}
gov <- x98[cd_cargo == 3L & toupper(DS_SIT_TOT_TURNO) %chin% ELEITO]
vgov <- x98[cd_cargo == 4L]
rb98 <- merge(vgov, unique(gov[, .(ue, NR_PARTIDO)]), by = c("ue", "NR_PARTIDO"))
cat(sprintf("rota do partido em 1998 (vice-governador): %d linhas em %d estados, de %d eleitos\n",
            nrow(rb98), uniqueN(rb98$ue), uniqueN(gov$ue)))
reg("chapa_1998_rota_partido_linhas", nrow(rb98))
reg("chapa_1998_rota_partido_estados", uniqueN(rb98$ue))

## ---------------------------------------------------------------- tabela da sonda
res <- rbind(
  data.table(ano = 2000L, item = "vices casados pela coligacao", valor = nrow(rota_a)),
  data.table(ano = 2000L, item = "vices casados por partido isolado", valor = nrow(rota_b)),
  data.table(ano = 2000L, item = "municipios cobertos pela uniao das rotas", valor = uniqueN(uni$ue)),
  data.table(ano = 2000L, item = "municipios com prefeito eleito", valor = uniqueN(el$ue)),
  data.table(ano = 2000L, item = "municipios com mais de um vice casado", valor = nrow(dup)),
  data.table(ano = 2000L, item = "municipios sem vice por nenhuma rota", valor = length(falta)),
  data.table(ano = 1998L, item = "vice-governador com SQ_COLIGACAO", valor = vgov[!is.na(sqcol), .N]),
  data.table(ano = 1998L, item = "vice-governador alcancado pela rota do partido", valor = nrow(rb98)))
fwrite(res, "output/verificacao/sonda_chapa_vices.csv")
cat("\nsonda_chapa_vices: concluido |", nrow(res), "linhas em output/verificacao/sonda_chapa_vices.csv\n")
