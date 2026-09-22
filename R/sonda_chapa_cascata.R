# sonda_chapa_cascata.R — ancilar, fora de R/00_reconstruir.sh (12/09/2026)
# Mede a cascata de chapa do R/03 antes de fixa-la: o que cada rota acrescenta por eleicao e
# cargo, a situacao de origem do vice recuperado de 2004 em diante (onde a diferenca entre
# titulares e vices ja era compativel com vacancia real), os casos de mais de um vice por
# titular e os municipios de 2000 que nenhuma rota alcanca. Executa o R/03 ate a linha anterior
# a selecao dos eleitos e nao grava nada em data/.
suppressPackageStartupMessages({library(data.table)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
linhas <- readLines("R/03_build_banco.R")
fim <- grep("^eleitos <- cand_final\\[eleito == TRUE", linhas) - 1L
eval(parse(text = linhas[1:fim]))
vc <- cand_final[regra_chapa %in% c("sq_coligacao", "composicao_coligacao")]
cat("\n== recuperados pela cascata, por eleicao e cargo\n")
print(dcast(vc[, .N, by = .(ano_eleicao, cd_cargo, regra_chapa)], ano_eleicao + cd_cargo ~ regra_chapa, fill = 0))
cat("\n== situacao original do vice recuperado, 2002 em diante\n")
v2 <- vc[ano_eleicao >= 2002]
print(v2[, .N, by = .(DS_SIT_TOT_TURNO, DS_SITUACAO_CANDIDATURA, ST_SUBSTITUIDO, registro_ok)][order(-N)])
cat("\n== 1998 e 2000, situacao original\n")
print(vc[ano_eleicao <= 2000, .N, by = .(ano_eleicao, DS_SIT_TOT_TURNO, DS_SITUACAO_CANDIDATURA, ST_SUBSTITUIDO, registro_ok)][order(-N)])
cat("\n== vices de 1998 (cargos 2 e 4) eleitos no fim da cascata\n")
print(cand_final[ano_eleicao == 1998 & cd_cargo %in% c(2L, 4L) & eleito == TRUE,
                 .(ue_pos, cd_cargo, NM_CANDIDATO, SG_PARTIDO, NR_CANDIDATO, regra_chapa)][order(cd_cargo, ue_pos)])
cat("\n== titulares eleitos sem vice eleito, por eleicao e cargo, depois da cascata\n")
te <- cand_final[cd_cargo %in% vice_de & eleito == TRUE, .(ano_eleicao, ue_pos, cd_tit = cd_cargo)]
ve <- unique(cand_final[!is.na(cd_tit) & eleito == TRUE, .(ano_eleicao, ue_pos, cd_tit, tem = TRUE)])
sv <- merge(te, ve, by = c("ano_eleicao", "ue_pos", "cd_tit"), all.x = TRUE)[is.na(tem)]
print(sv[, .N, by = .(ano_eleicao, cd_tit)][order(ano_eleicao, cd_tit)])
cat("\n== mais de um vice eleito por titular (ano, unidade, cargo)\n")
mv <- cand_final[!is.na(cd_tit) & eleito == TRUE, .N, by = .(ano_eleicao, ue_pos, cd_tit)][N > 1]
print(mv[, .N, by = .(ano_eleicao, cd_tit)])
cat("\n== 2000 sem vice: o que existe no cadastro\n")
s0 <- sv[ano_eleicao == 2000 & cd_tit == 11L]
print(cand_final[ano_eleicao == 2000 & ue_pos %in% s0$ue_pos & cd_cargo %in% c(11L, 12L),
                 .(ue_pos, NM_UE, cd_cargo, NR_CANDIDATO, SG_PARTIDO, SQ_COLIGACAO, DS_COMPOSICAO_COLIGACAO, eleito, DS_SIT_TOT_TURNO, ST_SUBSTITUIDO, DS_SITUACAO_CANDIDATURA)][order(ue_pos, cd_cargo)])
saveRDS(list(vc = vc, sv = sv), file.path("logs", "sonda_chapa_cascata_1209.rds"))
