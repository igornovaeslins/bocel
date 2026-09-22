#!/usr/bin/env Rscript
# Ancilar. Mede a concordancia entre fontes independentes sobre a forma de saida do mesmo
# mandato, o que e a evidencia empirica para a ordem de prioridade de R/10_integrar_exercicio.R.
# Reconstroi cada fonte como R/10 a le, sem aplicar a regra de prioridade, e cruza par a par.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente

mand <- fread("data/mandatos.csv", na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8")
ids <- mand$id_mandato
ler <- function(f) if (file.exists(f)) fread(f, na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8") else NULL
pega <- function(f, cid, cforma, fonte) {
  x <- ler(f); if (is.null(x) || !all(c(cid, cforma) %in% names(x))) return(NULL)
  y <- x[, .(id_mandato = get(cid), forma = get(cforma))]
  y <- y[!is.na(id_mandato) & id_mandato %in% ids & !is.na(forma) & forma != "nao_observado"]
  if (!nrow(y)) return(NULL)
  unique(y)[, .(forma = forma[1]), by = id_mandato][, fonte := fonte][]
}
fontes <- rbindlist(list(
  pega("data/exercicio_camaras_municipais.csv", "id_mandato_bocel", "forma_saida", "sapl_municipal"),
  pega("data/exercicio_camaras_sem_sapl.csv", "id_mandato_bocel", "forma_saida", "portal_camara"),
  pega("data/exercicio_camaras_sem_sapl_2.csv", "id_mandato_bocel", "forma_saida", "portal_camara"),
  pega("data/tce_gestores.csv", "id_mandato_bocel", "forma_saida", "tce"),
  pega("data/tce_gestores_b.csv", "id_mandato_bocel", "forma_saida", "tce"),
  pega("data/tce_gestores_c.csv", "id_mandato_bocel", "forma_saida", "tce"),
  pega("data/tce_gestores_d.csv", "id_mandato_bocel", "forma_saida", "tce"),
  pega("data/exercicio_assembleias.csv", "id_mandato_bocel", "forma_saida", "assembleia_api"),
  pega("data/exercicio_assembleias_historico.csv", "id_mandato_bocel", "forma_saida", "assembleia_historico"),
  pega("data/wikipedia_estadual.csv", "id_mandato_bocel", "forma_saida", "wikipedia"),
  pega("data/wikipedia_prefeitos.csv", "id_mandato_bocel", "forma_saida", "wikipedia"),
  pega("data/wikidata_mandatos.csv", "id_mandato_bocel", "forma_saida", "wikidata"),
  pega("data/exercicio_senado.csv", "id_mandato", "forma_saida", "senado_api"),
  pega("data/exercicio_camara.csv", "id_mandato", "forma_saida", "camara_api"),
  pega("data/diarios_mandatos_saida.csv", "id_mandato_bocel", "forma_saida", "diario_oficial")
), use.names = TRUE, fill = TRUE)
fontes <- unique(fontes[, .(forma = forma[1]), by = .(id_mandato, fonte)])
checa_unica(as.data.frame(fontes), c("id_mandato", "fonte"))

## pares de fontes que cobrem o mesmo mandato
pares <- merge(fontes, fontes, by = "id_mandato", allow.cartesian = TRUE)[fonte.x < fonte.y]
res <- pares[, .(mandatos = .N, concordam = sum(forma.x == forma.y)), by = .(fonte_a = fonte.x, fonte_b = fonte.y)]
res[, pct_concordancia := round(100 * concordam / mandatos, 1)][order(-mandatos)]
fwrite(res[order(-mandatos)], "output/verificacao/concordancia_entre_fontes.csv")

## matriz das discordancias mais frequentes
disc <- pares[forma.x != forma.y, .N, by = .(fonte_a = fonte.x, forma_a = forma.x, fonte_b = fonte.y, forma_b = forma.y)][order(-N)]
fwrite(disc, "output/verificacao/discordancias_forma_saida.csv")

n_mult <- fontes[, .N, by = id_mandato][N > 1, .N]
n_conc <- pares[, .(ok = all(forma.x == forma.y)), by = id_mandato][ok == TRUE, .N]
cat("mandatos cobertos por mais de uma fonte:", n_mult, "| unanimes:", n_conc,
    sprintf("(%.1f%%)", 100 * n_conc / n_mult), "\n")
print(head(res[order(-mandatos)], 12)); print(head(disc, 10))
registrar_numero("conc_mandatos_multifonte", n_mult)
registrar_numero("conc_mandatos_unanimes", n_conc)
registrar_numero("conc_pct_unanimes", round(100 * n_conc / n_mult, 1))
registrar_numero("conc_n_pares_de_fontes", nrow(res))
for (i in seq_len(nrow(res))) registrar_numero(paste0("conc_pct_", res$fonte_a[i], "_x_", res$fonte_b[i]), res$pct_concordancia[i])
cat("amostra_divergencia_fontes: concluido\n")
