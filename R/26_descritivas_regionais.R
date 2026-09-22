# 26_descritivas_regionais.R — panorama descritivo do BOCEL com recorte regional
# Entrada: data/{mandatos,pessoas,posicoes_ano,filiacoes,sinais_tse_exercicio}.csv
# Saida:   output/descritivas/*.csv e docs/PANORAMA_REGIONAL.md
# Execucao: cd ~/bocel && Rscript --vanilla R/26_descritivas_regionais.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/26_descritivas_regionais.R"
dir.create("output/descritivas", showWarnings = FALSE, recursive = TRUE)
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
fmt <- function(x) trimws(format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE))
pc  <- function(a, b) sub(".", ",", sprintf("%.1f%%", 100 * a / b), fixed = TRUE)
tab <- function(d) paste(c(paste0("| ", paste(names(d), collapse = " | "), " |"),
                           paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
                           apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))), collapse = "\n")

REG <- c(AC="Norte", AM="Norte", AP="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
         AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste", PE="Nordeste",
         PI="Nordeste", RN="Nordeste", SE="Nordeste",
         DF="Centro-Oeste", GO="Centro-Oeste", MS="Centro-Oeste", MT="Centro-Oeste",
         ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
         PR="Sul", RS="Sul", SC="Sul")
ORD <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

m <- fread("data/mandatos.csv", na.strings = "NA")
p <- fread("data/pessoas.csv", na.strings = "NA")
m[, regiao := REG[sg_uf]]
m <- merge(m, p[, .(id_pessoa, genero, dt_nascimento)], by = "id_pessoa", all.x = TRUE)
m[, regiao := factor(regiao, levels = ORD)]
mr <- m[!is.na(regiao)]

## 1. volume por regiao e esfera
t1 <- dcast(mr[, .N, by = .(regiao, esfera)], regiao ~ esfera, value.var = "N", fill = 0)
t1[, total := municipal + estadual + federal]
t1 <- t1[order(match(regiao, ORD))]
fwrite(t1, "output/descritivas/mandatos_por_regiao_esfera.csv")

## 2. pessoas e mandatos por pessoa
pes <- mr[, .(regiao = regiao[1]), by = id_pessoa]
t2 <- merge(pes[, .(pessoas = .N), by = regiao], mr[, .(mandatos = .N), by = regiao], by = "regiao")
t2[, mandatos_por_pessoa := round(mandatos / pessoas, 2)]
t2 <- merge(t2, mr[, .(id_pessoa, regiao)][, .N, by = .(regiao, id_pessoa)][, .(pct_2mais = round(100 * mean(N >= 2), 1)), by = regiao], by = "regiao")
t2 <- t2[order(match(regiao, ORD))]
fwrite(t2, "output/descritivas/pessoas_por_regiao.csv")

## 3. genero por regiao e eleicao (todos os cargos)
g <- mr[genero %in% c("MASCULINO", "FEMININO")]
t3 <- dcast(g[, .N, by = .(regiao, ano_eleicao, genero)], regiao + ano_eleicao ~ genero, value.var = "N", fill = 0)
t3[, pct_fem := round(100 * FEMININO / (FEMININO + MASCULINO), 1)]
fwrite(t3[order(match(regiao, ORD), ano_eleicao)], "output/descritivas/genero_regiao_eleicao.csv")
t3b <- dcast(g[ano_eleicao %in% c(1998, 2004, 2012, 2020, 2024), .(pct = round(100 * mean(genero == "FEMININO"), 1)), by = .(regiao, ano_eleicao)],
             regiao ~ ano_eleicao, value.var = "pct")
t3b <- t3b[order(match(regiao, ORD))]
# vereadora e prefeita em 2024
t3c <- g[ano_eleicao == 2024 & cargo %in% c("VEREADOR", "PREFEITO"), .(pct_fem = round(100 * mean(genero == "FEMININO"), 1)), by = .(regiao, cargo)]
t3c <- dcast(t3c, regiao ~ cargo, value.var = "pct_fem")[order(match(regiao, ORD))]
fwrite(t3c, "output/descritivas/genero_2024_cargo_regiao.csv")

## 4. reeleicao imediata de prefeito (mesma pessoa em eleicoes consecutivas na mesma unidade)
pref <- m[cargo == "PREFEITO", .(id_pessoa, unidade_posicao, ano_eleicao, sg_uf, regiao = REG[sg_uf], sg_partido)]
setorder(pref, unidade_posicao, ano_eleicao)
pref[, ant := shift(id_pessoa), by = unidade_posicao]
pref[, ant_ano := shift(ano_eleicao), by = unidade_posicao]
pref[, ant_part := shift(sg_partido), by = unidade_posicao]
seq4 <- pref[!is.na(ant) & ano_eleicao - ant_ano == 4]
t4 <- seq4[, .(eleicoes = .N, reeleitos = sum(id_pessoa == ant), pct_reeleicao = round(100 * mean(id_pessoa == ant), 1),
               pct_mesmo_partido = round(100 * mean(sg_partido == ant_part, na.rm = TRUE), 1)), by = regiao]
t4 <- t4[order(match(regiao, ORD))]
fwrite(t4, "output/descritivas/reeleicao_prefeito_regiao.csv")
t4b <- seq4[, .(pct_reeleicao = round(100 * mean(id_pessoa == ant), 1)), by = .(regiao, ano_eleicao)]
fwrite(dcast(t4b, regiao ~ ano_eleicao, value.var = "pct_reeleicao")[order(match(regiao, ORD))], "output/descritivas/reeleicao_prefeito_ano.csv")

## 5. carreira: quem foi vereador antes de ser prefeito, deputado estadual ou federal
car <- m[, .(id_pessoa, cargo, ano_eleicao, regiao, cd_cargo)]
ver <- car[cargo == "VEREADOR", .(primeiro_ver = min(ano_eleicao)), by = id_pessoa]
alvo <- car[cargo %in% c("PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL"), .(id_pessoa, cargo, ano_eleicao, regiao)]
alvo <- merge(alvo, ver, by = "id_pessoa", all.x = TRUE)
alvo[, veio_de_vereador := !is.na(primeiro_ver) & primeiro_ver < ano_eleicao]
t5 <- alvo[!is.na(regiao), .(mandatos = .N, pct_ex_vereador = round(100 * mean(veio_de_vereador), 1)), by = .(regiao, cargo)]
t5 <- dcast(t5, regiao ~ cargo, value.var = "pct_ex_vereador")[order(match(regiao, ORD))]
fwrite(t5, "output/descritivas/carreira_ex_vereador.csv")

## 6. concentracao partidaria: numero efetivo de partidos entre prefeitos eleitos, por regiao e eleicao
nep <- function(x) { s <- table(x); p <- s / sum(s); round(1 / sum(p^2), 1) }
t6 <- m[cargo == "PREFEITO" & !is.na(regiao) & !is.na(sg_partido), .(nep = nep(sg_partido), partidos = uniqueN(sg_partido)), by = .(regiao, ano_eleicao)]
fwrite(dcast(t6, regiao ~ ano_eleicao, value.var = "nep")[order(match(regiao, ORD))], "output/descritivas/nep_prefeitos.csv")
# partido mais votado em prefeituras por regiao em 2024
t6b <- m[cargo == "PREFEITO" & ano_eleicao == 2024 & !is.na(regiao), .N, by = .(regiao, sg_partido)][order(regiao, -N)][, .SD[1:3], by = regiao]
fwrite(t6b, "output/descritivas/partidos_prefeitos_2024.csv")

## 7. idade ao tomar posse (1o de janeiro seguinte a eleicao)
m[, idade := as.integer(floor(as.numeric(as.IDate(mandato_inicio) - as.IDate(dt_nascimento)) / 365.25))]
t7 <- m[!is.na(idade) & idade >= 18 & idade <= 95 & !is.na(regiao),
        .(idade_mediana = as.numeric(median(idade)), pct_ate_35 = round(100 * mean(idade <= 35), 1)), by = .(regiao, cargo)]
t7 <- t7[cargo %in% c("VEREADOR", "PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL")]
fwrite(dcast(t7, regiao ~ cargo, value.var = "idade_mediana")[order(match(regiao, ORD))], "output/descritivas/idade_mediana_regiao.csv")

## 8. votacao mediana por cargo e regiao
t8 <- m[!is.na(votos_turno_decisivo) & votos_turno_decisivo > 0 & !is.na(regiao) &
        cargo %in% c("VEREADOR", "PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL"),
        .(mediana = as.integer(median(votos_turno_decisivo))), by = .(regiao, cargo)]
fwrite(dcast(t8, regiao ~ cargo, value.var = "mediana")[order(match(regiao, ORD))], "output/descritivas/votos_medianos.csv")

## 9. filiacao: numero de partidos distintos por pessoa (troca de legenda)
fil <- fread("data/filiacoes.csv", na.strings = "NA")
fp <- fil[, .(partidos = uniqueN(sigla_partido)), by = id_pessoa]
fp <- merge(fp, pes, by = "id_pessoa")
t9 <- fp[!is.na(regiao), .(pessoas = .N, media_partidos = round(mean(partidos), 2), pct_2mais = round(100 * mean(partidos >= 2), 1),
                           pct_3mais = round(100 * mean(partidos >= 3), 1)), by = regiao][order(match(regiao, ORD))]
fwrite(t9, "output/descritivas/filiacao_partidos_por_pessoa.csv")

## 10. camada de posse e saida por regiao
t10 <- mr[, .(mandatos = .N, saida_obs = sum(forma_saida != "nao_observado"),
              pct = round(100 * mean(forma_saida != "nao_observado"), 1)), by = regiao][order(match(regiao, ORD))]
fwrite(t10, "output/descritivas/saida_observada_regiao.csv")
t10b <- mr[forma_saida %in% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca"),
           .N, by = .(regiao, forma_saida)]
fwrite(dcast(t10b, regiao ~ forma_saida, value.var = "N", fill = 0)[order(match(regiao, ORD))], "output/descritivas/formas_saida_regiao.csv")

## 11. suplementares (perda de mandato inferida) por regiao
sup <- m[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar" & !is.na(regiao)]
t11 <- merge(sup[, .(pleitos = .N), by = regiao], m[cargo == "PREFEITO" & !is.na(regiao), .(prefeituras = uniqueN(unidade_posicao)), by = regiao], by = "regiao", all.y = TRUE)
t11[is.na(pleitos), pleitos := 0]
t11[, por_mil_prefeituras := round(1000 * pleitos / prefeituras, 1)]
fwrite(t11[order(match(regiao, ORD))], "output/descritivas/suplementares_regiao.csv")

## 12. troca de partido entre a eleicao e a candidatura seguinte
si <- fread("data/sinais_tse_exercicio.csv", na.strings = "NA")
si <- merge(si[candidatura_seguinte == TRUE & !is.na(mudou_partido), .(id_mandato, mudou_partido)],
            m[, .(id_mandato, regiao, cargo)], by = "id_mandato")
t12 <- si[!is.na(regiao), .(candidaturas = .N, pct_mudou = round(100 * mean(mudou_partido), 1)), by = regiao][order(match(regiao, ORD))]
fwrite(t12, "output/descritivas/troca_partido_regiao.csv")

## ---- numeros registrados
reg("desc_n_mandatos_total", nrow(m))
for (r in ORD) {
  k <- tolower(gsub("-", "_", iconv(r, to = "ASCII//TRANSLIT")))
  reg(paste0("desc_mandatos_", k), mr[regiao == r, .N])
  reg(paste0("desc_pct_feminino_2024_", k), round(100 * g[regiao == r & ano_eleicao == 2024, mean(genero == "FEMININO")], 2))
  reg(paste0("desc_pct_reeleicao_prefeito_", k), t4[regiao == r, pct_reeleicao])
  reg(paste0("desc_pct_saida_observada_", k), t10[regiao == r, pct])
}
reg("desc_pct_feminino_2024_brasil", round(100 * g[ano_eleicao == 2024, mean(genero == "FEMININO")], 2))
reg("desc_pct_feminino_1998_brasil", round(100 * g[ano_eleicao == 1998, mean(genero == "FEMININO")], 2))
reg("desc_pct_reeleicao_prefeito_brasil", round(100 * seq4[, mean(id_pessoa == ant)], 2))
reg("desc_pct_prefeitos_ex_vereadores_brasil", round(100 * alvo[cargo == "PREFEITO", mean(veio_de_vereador)], 2))
reg("desc_pessoas_com_2mais_mandatos", p[n_mandatos >= 2, .N])
reg("desc_pessoas_com_5mais_mandatos", p[n_mandatos >= 5, .N])
reg("desc_max_mandatos_uma_pessoa", p[, max(n_mandatos)])

## ---- documento
md <- c("# Panorama regional do BOCEL", "",
  sprintf("Gerado por `%s` a partir de `data/`. Regiões pelo IBGE, com o Distrito Federal no Centro-Oeste. Os números estão registrados em `output/numeros_assinatura.txt` (chaves `desc_*`) e as tabelas completas em `output/descritivas/`.", script), "",
  "## Volume por região e esfera", "", tab(t1[, lapply(.SD, as.character)]), "",
  "## Pessoas e recorrência", "", tab(t2[, lapply(.SD, as.character)]), "",
  "## Mulheres entre os eleitos (%, todos os cargos)", "", tab(t3b[, lapply(.SD, as.character)]), "",
  "## Mulheres em 2024, por cargo (%)", "", tab(t3c[, lapply(.SD, as.character)]), "",
  "## Reeleição imediata de prefeito", "", tab(t4[, lapply(.SD, as.character)]), "",
  "### Por eleição (%)", "", tab(fread("output/descritivas/reeleicao_prefeito_ano.csv")[, lapply(.SD, as.character)]), "",
  "## Quem já foi vereador antes (%)", "", tab(t5[, lapply(.SD, as.character)]), "",
  "## Número efetivo de partidos entre prefeitos eleitos", "", tab(fread("output/descritivas/nep_prefeitos.csv")[, lapply(.SD, as.character)]), "",
  "## Idade mediana na posse", "", tab(fread("output/descritivas/idade_mediana_regiao.csv")[, lapply(.SD, as.character)]), "",
  "## Votação mediana do eleito", "", tab(fread("output/descritivas/votos_medianos.csv")[, lapply(.SD, as.character)]), "",
  "## Partidos distintos no histórico de filiação", "", tab(t9[, lapply(.SD, as.character)]), "",
  "## Troca de partido entre o mandato e a candidatura seguinte", "", tab(t12[, lapply(.SD, as.character)]), "",
  "## Eleições suplementares por mil prefeituras", "", tab(t11[order(match(regiao, ORD))][, lapply(.SD, as.character)]), "",
  "## Cobertura da camada de posse e saída", "", tab(t10[, lapply(.SD, as.character)]), "",
  "## Formas de saída observadas (exceto fim regular)", "", tab(fread("output/descritivas/formas_saida_regiao.csv")[, lapply(.SD, as.character)]))
writeLines(md, "docs/PANORAMA_REGIONAL.md")
cat("docs/PANORAMA_REGIONAL.md gerado\n")

cat("26_descritivas_regionais: concluido\n")
print(t1); print(t3b); print(t4); print(t5); print(t9); print(t10)
