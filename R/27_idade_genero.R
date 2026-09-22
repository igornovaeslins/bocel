# 27_idade_genero.R — idade na posse cruzada com genero, por cargo, regiao e eleicao
# Entrada: data/mandatos.csv, data/pessoas.csv
# Saida:   output/descritivas/idade_genero_*.csv e docs/IDADE_GENERO.md
# Execucao: cd ~/bocel && Rscript --vanilla R/27_idade_genero.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/27_idade_genero.R"
dir.create("output/descritivas", showWarnings = FALSE, recursive = TRUE)
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
tab <- function(d) paste(c(paste0("| ", paste(names(d), collapse = " | "), " |"),
                           paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
                           apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))), collapse = "\n")
chr <- function(d) d[, lapply(.SD, as.character)]
REG <- c(AC="Norte", AM="Norte", AP="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
         AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste", PE="Nordeste",
         PI="Nordeste", RN="Nordeste", SE="Nordeste", DF="Centro-Oeste", GO="Centro-Oeste",
         MS="Centro-Oeste", MT="Centro-Oeste", ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
         PR="Sul", RS="Sul", SC="Sul")
ORD <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
CARGOS <- c("VEREADOR", "PREFEITO", "VICE-PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "SENADOR", "GOVERNADOR")

m <- fread("data/mandatos.csv", na.strings = "NA")
p <- fread("data/pessoas.csv", na.strings = "NA")
m <- merge(m, p[, .(id_pessoa, genero, dt_nascimento)], by = "id_pessoa", all.x = TRUE)
m[, regiao := factor(REG[sg_uf], levels = ORD)]
m[, idade := as.numeric(floor(as.numeric(as.IDate(mandato_inicio) - as.IDate(dt_nascimento)) / 365.25))]
d <- m[genero %in% c("MASCULINO", "FEMININO") & !is.na(idade) & idade >= 18 & idade <= 95]
d[, sexo := fifelse(genero == "FEMININO", "mulheres", "homens")]
reg("ig_n_mandatos_com_idade_e_genero", nrow(d))
reg("ig_pct_mandatos_sem_idade", round(100 * (1 - nrow(d) / m[genero %in% c("MASCULINO","FEMININO"), .N]), 2))

## 1. idade mediana e faixas por sexo e cargo
t1 <- d[cargo %in% CARGOS, .(n = .N, mediana = as.numeric(median(idade)), media = round(mean(idade), 1),
                             pct_ate_35 = round(100 * mean(idade <= 35), 1),
                             pct_60mais = round(100 * mean(idade >= 60), 1)), by = .(cargo, sexo)]
t1 <- t1[order(match(cargo, CARGOS), sexo)]
fwrite(t1, "output/descritivas/idade_genero_cargo.csv")
t1w <- dcast(t1, cargo ~ sexo, value.var = c("mediana", "pct_ate_35", "pct_60mais"))
t1w <- t1w[order(match(cargo, CARGOS))]
t1w[, dif_mediana := mediana_mulheres - mediana_homens]
fwrite(t1w, "output/descritivas/idade_genero_cargo_wide.csv")

## 2. idade de estreia (primeiro mandato da pessoa) por sexo e cargo
setorder(d, id_pessoa, mandato_inicio)
est <- d[, .SD[1], by = id_pessoa]
t2 <- est[cargo %in% CARGOS, .(pessoas = .N, estreia_mediana = as.numeric(median(idade)),
                               pct_estreia_ate_35 = round(100 * mean(idade <= 35), 1)), by = .(cargo, sexo)]
t2 <- dcast(t2[order(match(cargo, CARGOS))], cargo ~ sexo, value.var = c("pessoas", "estreia_mediana", "pct_estreia_ate_35"))
t2 <- t2[order(match(cargo, CARGOS))]
fwrite(t2, "output/descritivas/idade_genero_estreia.csv")

## 3. evolucao no tempo (todos os cargos)
t3 <- d[, .(mediana = as.numeric(median(idade)), pct_ate_35 = round(100 * mean(idade <= 35), 1)), by = .(ano_eleicao, sexo)]
t3w <- dcast(t3, ano_eleicao ~ sexo, value.var = "mediana")[order(ano_eleicao)]
t3w[, dif := mulheres - homens]
fwrite(t3w, "output/descritivas/idade_genero_ano.csv")
t3j <- dcast(t3, ano_eleicao ~ sexo, value.var = "pct_ate_35")[order(ano_eleicao)]
fwrite(t3j, "output/descritivas/idade_genero_ano_ate35.csv")

## 4. regiao x sexo (vereador e prefeito, os dois maiores)
t4 <- d[cargo %in% c("VEREADOR", "PREFEITO") & !is.na(regiao),
        .(mediana = as.numeric(median(idade))), by = .(regiao, cargo, sexo)]
t4 <- dcast(t4, regiao + cargo ~ sexo, value.var = "mediana")[order(cargo, match(regiao, ORD))]
t4[, dif := mulheres - homens]
fwrite(t4, "output/descritivas/idade_genero_regiao.csv")

## 5. composicao etaria: participacao feminina dentro de cada faixa
d[, faixa := cut(idade, breaks = c(17, 29, 39, 49, 59, 69, 95),
                 labels = c("18-29", "30-39", "40-49", "50-59", "60-69", "70+"), right = TRUE)]
t5 <- d[, .(mandatos = .N, pct_mulheres = round(100 * mean(sexo == "mulheres"), 1)), by = faixa][order(faixa)]
fwrite(t5, "output/descritivas/idade_genero_faixa.csv")
t5b <- dcast(d[ano_eleicao %in% c(2000, 2012, 2024), .(pct = round(100 * mean(sexo == "mulheres"), 1)), by = .(faixa, ano_eleicao)],
             faixa ~ ano_eleicao, value.var = "pct")[order(faixa)]
fwrite(t5b, "output/descritivas/idade_genero_faixa_ano.csv")

## 6. reeleicao imediata de prefeito por sexo e faixa
pref <- m[cargo == "PREFEITO" & genero %in% c("MASCULINO", "FEMININO")]
pref[, sexo := fifelse(genero == "FEMININO", "mulheres", "homens")]
setorder(pref, unidade_posicao, ano_eleicao)
pref[, `:=`(ant = shift(id_pessoa), ant_ano = shift(ano_eleicao)), by = unidade_posicao]
s4 <- pref[!is.na(ant) & ano_eleicao - ant_ano == 4]
t6 <- s4[, .(eleicoes = .N, pct_reeleicao = round(100 * mean(id_pessoa == ant), 1)), by = sexo]
fwrite(t6, "output/descritivas/idade_genero_reeleicao_prefeito.csv")

## 7. numero de mandatos por sexo (carreira)
car <- d[, .(mandatos = .N, sexo = sexo[1]), by = id_pessoa]
t7 <- car[, .(pessoas = .N, media_mandatos = round(mean(mandatos), 2),
              pct_2mais = round(100 * mean(mandatos >= 2), 1), pct_4mais = round(100 * mean(mandatos >= 4), 1)), by = sexo]
fwrite(t7, "output/descritivas/idade_genero_carreira.csv")

## ---- numeros
for (cg in c("VEREADOR", "PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL")) {
  k <- gsub(" ", "_", tolower(cg))
  reg(paste0("ig_mediana_", k, "_mulheres"), t1[cargo == cg & sexo == "mulheres", mediana])
  reg(paste0("ig_mediana_", k, "_homens"), t1[cargo == cg & sexo == "homens", mediana])
}
reg("ig_mediana_geral_mulheres", as.numeric(d[sexo == "mulheres", median(idade)]))
reg("ig_mediana_geral_homens", as.numeric(d[sexo == "homens", median(idade)]))
reg("ig_pct_ate35_mulheres", round(100 * d[sexo == "mulheres", mean(idade <= 35)], 2))
reg("ig_pct_ate35_homens", round(100 * d[sexo == "homens", mean(idade <= 35)], 2))
reg("ig_pct_60mais_mulheres", round(100 * d[sexo == "mulheres", mean(idade >= 60)], 2))
reg("ig_pct_60mais_homens", round(100 * d[sexo == "homens", mean(idade >= 60)], 2))
reg("ig_estreia_mediana_mulheres", as.numeric(est[sexo == "mulheres", median(idade)]))
reg("ig_estreia_mediana_homens", as.numeric(est[sexo == "homens", median(idade)]))
reg("ig_pct_mulheres_faixa_18_29", t5[faixa == "18-29", pct_mulheres])
reg("ig_pct_mulheres_faixa_70mais", t5[faixa == "70+", pct_mulheres])
reg("ig_reeleicao_prefeitas", t6[sexo == "mulheres", pct_reeleicao])
reg("ig_reeleicao_prefeitos", t6[sexo == "homens", pct_reeleicao])

md <- c("# Idade e gênero entre os eleitos (BOCEL)", "",
  sprintf("Gerado por `%s`. A idade é a que a pessoa tem no início convencional do mandato, e a base são os %s mandatos com data de nascimento e gênero no cadastro do TSE. Números registrados em `output/numeros_assinatura.txt` (chaves `ig_*`).", script, format(nrow(d), big.mark = ".", decimal.mark = ",")), "",
  "## Idade por cargo", "", tab(chr(t1w)), "",
  "## Idade de estreia (primeiro mandato da pessoa)", "", tab(chr(t2)), "",
  "## Idade mediana ao longo do tempo", "", tab(chr(t3w)), "",
  "## Menores de 36 anos, por eleição (%)", "", tab(chr(t3j)), "",
  "## Idade mediana por região (vereador e prefeito)", "", tab(chr(t4)), "",
  "## Participação feminina por faixa etária", "", tab(chr(t5)), "",
  "### Por eleição (%)", "", tab(chr(t5b)), "",
  "## Reeleição imediata de prefeito por sexo", "", tab(chr(t6)), "",
  "## Carreira", "", tab(chr(t7)))
writeLines(md, "docs/IDADE_GENERO.md")
cat("27_idade_genero: concluido\n")
print(t1w); print(t2); print(t3w); print(t4); print(t5); print(t5b); print(t6); print(t7)
