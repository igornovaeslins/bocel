# 19_levantamento_lacunas.R — levantamento do que falta na camada de posse, exercicio e forma de saida
# Mede, a partir de data/mandatos.csv, a cobertura por esfera, cargo, eleicao e UF, e grava
# output/verificacao/lacunas_por_cargo_eleicao.csv, lacunas_por_uf_cargo.csv e docs/LEVANTAMENTO_LACUNAS.md
# Execucao: cd ~/bocel && Rscript --vanilla R/19_levantamento_lacunas.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/19_levantamento_lacunas.R"
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
m[, `:=`(obs = forma_saida != "nao_observado", posse = !is.na(data_posse), exerc = !is.na(exercicio_confirmado),
         ano = as.integer(ano_eleicao))]
fmt <- function(x) trimws(format(x, big.mark = ".", decimal.mark = ","))
pct <- function(a, b) sub(".", ",", sprintf("%.1f%%", 100 * a / b), fixed = TRUE)

## 1. por cargo x eleicao (2002-2024 e anteriores separadas)
ce <- m[, .(mandatos = .N, saida_observada = sum(obs), posse = sum(posse), exercicio = sum(exerc)),
        by = .(esfera, cargo, ano_eleicao = ano)][order(esfera, cargo, ano_eleicao)]
ce[, `:=`(pct_saida = round(100 * saida_observada / mandatos, 1), pct_posse = round(100 * posse / mandatos, 1),
          pct_exercicio = round(100 * exercicio / mandatos, 1), faltam_saida = mandatos - saida_observada)]
fwrite(ce, "output/verificacao/lacunas_por_cargo_eleicao.csv")
## 2. por UF x cargo (2002-2024)
uc <- m[ano >= 2002, .(mandatos = .N, saida_observada = sum(obs), posse = sum(posse), exercicio = sum(exerc)),
        by = .(sg_uf, cargo)][order(sg_uf, cargo)]
uc[, `:=`(pct_saida = round(100 * saida_observada / mandatos, 1), faltam_saida = mandatos - saida_observada)]
fwrite(uc, "output/verificacao/lacunas_por_uf_cargo.csv")
## 3. por fonte
ft <- m[obs == TRUE, .N, by = .(esfera, fonte_forma_saida)][order(esfera, -N)]
fe <- m[exerc == TRUE, .N, by = .(esfera, fonte_exercicio)][order(esfera, -N)]

## 4. resumo por cargo (2002-2024)
rc <- m[ano >= 2002, .(mandatos = .N, saida_observada = sum(obs), posse = sum(posse), exercicio = sum(exerc)), by = .(esfera, cargo)][order(esfera, -mandatos)]
rc[, faltam_saida := mandatos - saida_observada]
for (i in seq_len(nrow(rc))) {
  k <- gsub("[^a-z]", "_", tolower(iconv(rc$cargo[i], to = "ASCII//TRANSLIT")))
  registrar_numero(paste0("lac_", k, "_mandatos_2002_2024"), rc$mandatos[i], script = script)
  registrar_numero(paste0("lac_", k, "_saida_observada_2002_2024"), rc$saida_observada[i], script = script)
  registrar_numero(paste0("lac_", k, "_faltam_saida_2002_2024"), rc$faltam_saida[i], script = script)
}
registrar_numero("lac_total_mandatos_2002_2024", m[ano >= 2002, .N], script = script)
registrar_numero("lac_total_saida_observada_2002_2024", m[ano >= 2002, sum(obs)], script = script)
registrar_numero("lac_total_faltam_saida_2002_2024", m[ano >= 2002, sum(!obs)], script = script)

## 5. municipios sem nenhuma fonte de saida (vereador e prefeito), 2002-2024
mun <- m[ano >= 2002 & esfera == "municipal", .(mandatos = .N, com_saida = sum(obs)), by = .(sg_uf, unidade_posicao)]
n_mun_sem <- mun[com_saida == 0, .N]; n_mun <- nrow(mun)
registrar_numero("lac_municipios_sem_nenhuma_saida_observada", n_mun_sem, script = script)
registrar_numero("lac_municipios_total", n_mun, script = script)
sapl <- if (file.exists("data_raw/sapl_municipal/inventario_sapl_municipal.csv")) fread("data_raw/sapl_municipal/inventario_sapl_municipal.csv", colClasses = "character") else NULL
n_sapl <- if (!is.null(sapl)) sapl[responde_api == "TRUE", .N] else NA
wp <- if (file.exists("data/wikipedia_prefeitos_cobertura.csv")) fread("data/wikipedia_prefeitos_cobertura.csv") else NULL

tab <- function(d, cols) paste(c(paste0("| ", paste(cols, collapse = " | "), " |"), paste0("|", paste(rep("---", length(cols)), collapse = "|"), "|"),
                                 apply(d[, ..cols], 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))), collapse = "\n")
rc_txt <- copy(rc)[, `:=`(mandatos = fmt(mandatos), saida_observada = paste0(fmt(saida_observada), " (", pct(saida_observada, as.numeric(mandatos)), ")"),
                          posse = fmt(posse), exercicio = fmt(exercicio), faltam_saida = fmt(faltam_saida))]
md <- c("# Levantamento do que falta na camada de posse, exercício e forma de saída (2002–2024)", "",
        sprintf("Gerado por `%s` em %s a partir de `data/mandatos.csv`. Todos os números estão registrados em `output/numeros_assinatura.txt` (chaves `lac_*`).", script, format(Sys.time(), "%Y-%m-%d %H:%M")), "",
        "## Por cargo", "", tab(rc_txt, c("esfera", "cargo", "mandatos", "saida_observada", "posse", "exercicio", "faltam_saida")), "",
        sprintf("Total 2002–2024: %s mandatos, %s com forma de saída observada (%s), %s sem.", fmt(m[ano >= 2002, .N]), fmt(m[ano >= 2002, sum(obs)]), pct(m[ano >= 2002, sum(obs)], m[ano >= 2002, .N]), fmt(m[ano >= 2002, sum(!obs)])), "",
        "## Por eleição (todos os cargos)", "",
        tab(copy(m[ano >= 2002, .(mandatos = .N, saida = sum(obs), posse = sum(posse), exercicio = sum(exerc)), by = .(ano_eleicao = ano)][order(ano_eleicao)])[
          , `:=`(mandatos = fmt(mandatos), saida = paste0(fmt(saida), " (", pct(saida, as.numeric(mandatos)), ")"), posse = fmt(posse), exercicio = fmt(exercicio))],
          c("ano_eleicao", "mandatos", "saida", "posse", "exercicio")), "",
        "## Fonte da forma de saída observada, por esfera", "", tab(copy(ft)[, N := fmt(N)], c("esfera", "fonte_forma_saida", "N")), "",
        "## Fonte da confirmação de exercício, por esfera", "", tab(copy(fe)[, N := fmt(N)], c("esfera", "fonte_exercicio", "N")), "",
        "## Municípios", "",
        sprintf("Dos %s municípios com mandatos municipais em 2002–2024, %s não têm nenhuma forma de saída observada para nenhum mandato. Câmaras com SAPL respondendo à API: %s. Municípios com lista de prefeitos na Wikipédia pareada: %s.",
                fmt(n_mun), fmt(n_mun_sem), if (is.na(n_sapl)) "não inventariado" else fmt(n_sapl),
                if (is.null(wp)) "não inventariado" else fmt(max(wp$n_municipios))), "",
        "## Por UF e cargo (2002–2024): os dez pares com mais mandatos sem saída observada", "",
        tab(copy(uc[order(-faltam_saida)][1:10])[, `:=`(mandatos = fmt(mandatos), saida_observada = fmt(saida_observada), faltam_saida = fmt(faltam_saida))], c("sg_uf", "cargo", "mandatos", "saida_observada", "pct_saida", "faltam_saida")), "",
        "Tabelas completas em `output/verificacao/lacunas_por_cargo_eleicao.csv` e `output/verificacao/lacunas_por_uf_cargo.csv`.")
writeLines(md, "docs/LEVANTAMENTO_LACUNAS.md")
print(rc); cat("19_levantamento_lacunas: concluido\n")
