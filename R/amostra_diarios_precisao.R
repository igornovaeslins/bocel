# amostra_diarios_precisao.R — sorteia a amostra de leitura manual da frente Querido Diario.
# Sorteia 30 eventos de confianca alta (leitura integral do trecho) e 20 de confianca media; os 20
# primeiros 'alta' da amostra somados aos 20 'media' formam a amostra de 40 usada na medida de precisao
# por confianca. Saida: output/verificacao/diarios_amostra_50_sorteada.csv (julgada a mao ->
# output/verificacao/diarios_precisao_julgamentos.csv, lido por R/verifica_diarios.R).
# Execucao: cd ~/bocel && Rscript --vanilla R/amostra_diarios_precisao.R
# Uso: Rscript --vanilla R/amostra_diarios_precisao.R [rodada]
#   rodada = "v2" (default, semente 20260827, amostra lida sobre a regra anterior a corroboracao de mandato)
#            "v3" (semente 20260828, amostra de validacao fora da amostra de calibragem)
#            "v4" (semente 20260829, amostra final, sobre a regra com a excecao do ato que suspende a cassacao)
args <- commandArgs(trailingOnly = TRUE)
rodada <- if (length(args)) args[1] else "v2"
set.seed(fcase_seed <- switch(rodada, v4 = 20260829L, v3 = 20260828L, 20260827L))
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
ev <- fread("data/diarios_eventos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
ev <- merge(ev, pess[, .(id_pessoa_bocel = id_pessoa, nome)], by = "id_pessoa_bocel", all.x = TRUE)
alta <- ev[confianca == "alta"][sample(.N)][1:30]
media <- ev[confianca == "media" & evento_inferido != "NA"][sample(.N)][1:20]
am <- rbind(cbind(amostra = "30_alta", alta), cbind(amostra = "20_media", media))
am[, idx := .I]
am[, na_amostra_40 := (amostra == "30_alta" & seq_len(.N) <= 20) | amostra == "20_media", by = amostra]
out <- am[, .(idx, amostra, na_amostra_40, id_mandato_bocel, id_municipio_ibge, data_diario, confianca, evento_inferido,
              evento_ancorado, metodo_pareamento, motivo_rebaixamento, nome, url_diario, trecho)]
arq <- switch(rodada, v4 = "output/verificacao/diarios_amostra_50_sorteada_v4.csv",
                      v3 = "output/verificacao/diarios_amostra_50_sorteada_v3.csv",
                      "output/verificacao/diarios_amostra_50_sorteada.csv")
fwrite(out, arq, quote = TRUE, na = "NA")
cat("amostra sorteada:", nrow(out), "linhas;", out[na_amostra_40 == TRUE, .N], "na amostra de 40\n")
print(out[, .N, by = .(amostra, confianca, evento_inferido)][order(amostra, -N)])
