#!/usr/bin/env Rscript
# Cobertura por unidade da federacao, camada a camada: nucleo, forma de saida por esfera e
# contribuicao de cada familia de fonte. Escrito em 29/08/2026 para responder onde o banco
# alcanca e onde nao alcanca, por UF, sem depender de memoria.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente

m <- fread("data/mandatos.csv", na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8")
obs <- function(x) !is.na(x) & !(x %in% c("nao_observado", ""))
m[, tem := obs(forma_saida)]
m[, esfera2 := fifelse(cd_cargo %in% c("1", "2"), "nacional",
                fifelse(cd_cargo %in% c("5", "6"), "federal",
                fifelse(cd_cargo %in% c("3", "4", "7", "8"), "estadual", "municipal")))]
UF27 <- sort(unique(m[sg_uf != "BR" & nchar(sg_uf) == 2]$sg_uf))
in_set(UF27, UF27, "UFs presentes")
registrar_numero("cuf_n_ufs_no_nucleo", length(UF27))

pv <- function(d) round(100 * sum(d$tem) / nrow(d), 1)
tab <- m[sg_uf %in% UF27, .(mandatos = .N, com_forma = sum(tem), pct = round(100 * mean(tem), 1)), by = sg_uf]
por_esf <- dcast(m[sg_uf %in% UF27, .(pct = round(100 * mean(tem), 1)), by = .(sg_uf, esfera2)],
                 sg_uf ~ esfera2, value.var = "pct")
tab <- merge(tab, por_esf, by = "sg_uf", all.x = TRUE)

## contribuicao por familia de fonte, por UF
fam <- c(camara_api = "parlamento nacional", senado_api = "parlamento nacional",
         assembleia_api = "assembleia", assembleia_historico = "assembleia",
         sapl_municipal = "camara municipal", portal_camara = "camara municipal",
         tce = "tribunal de contas", wikipedia = "enciclopedia", wikidata = "enciclopedia",
         wikidata_obito = "enciclopedia", diario_oficial = "diario oficial",
         tse_suplementar = "TSE suplementar", ibge_munic = "MUNIC", derivado_titular = "regra do vice",
         data_fim_efetiva = "fim efetivo")
m[tem == TRUE, familia := fam[fonte_forma_saida]]
ff <- dcast(m[tem == TRUE & sg_uf %in% UF27, .N, by = .(sg_uf, familia)], sg_uf ~ familia, value.var = "N", fill = 0L)
tab <- merge(tab, ff, by = "sg_uf", all.x = TRUE)
setorder(tab, -pct)
fwrite(tab, "output/descritivas/cobertura_por_uf.csv")

## UFs sem nenhuma linha em cada familia
vazias <- lapply(sort(unique(na.omit(fam))), function(f) {
  cols <- names(fam)[fam == f]
  ufs <- m[tem == TRUE & fonte_forma_saida %in% cols & sg_uf %in% UF27, unique(sg_uf)]
  setdiff(UF27, ufs)
})
names(vazias) <- sort(unique(na.omit(fam)))

## assembleias: quais casas entraram
asm <- m[esfera2 == "estadual" & cd_cargo %in% c("7", "8") & sg_uf %in% UF27,
         .(mandatos = .N, com_forma = sum(tem), pct = round(100 * mean(tem), 1)), by = sg_uf][order(pct)]
fwrite(asm, "output/descritivas/cobertura_assembleias_por_uf.csv")
## prefeitos e vereadores
pref <- m[cd_cargo == "11" & sg_uf %in% UF27, .(mandatos = .N, com_forma = sum(tem), pct = round(100 * mean(tem), 1)), by = sg_uf][order(pct)]
vere <- m[cd_cargo == "13" & sg_uf %in% UF27, .(mandatos = .N, com_forma = sum(tem), pct = round(100 * mean(tem), 1)), by = sg_uf][order(pct)]
fwrite(pref, "output/descritivas/cobertura_prefeitos_por_uf.csv")
fwrite(vere, "output/descritivas/cobertura_vereadores_por_uf.csv")

cat("\n== UFs no nucleo:", length(UF27), "|", paste(UF27, collapse = " "), "\n")
cat("\n== cobertura de forma de saida por UF (todas as esferas) ==\n"); print(tab[, .(sg_uf, mandatos, com_forma, pct)], nrows = 30)
cat("\n== deputado estadual e distrital ==\n"); print(asm, nrows = 30)
cat("\n== prefeito ==\n"); print(pref, nrows = 30)
cat("\n== vereador ==\n"); print(vere, nrows = 30)
cat("\n== UFs sem nenhuma saida vinda de cada familia de fonte ==\n")
for (f in names(vazias)) cat(sprintf("%-22s %2d UFs sem: %s\n", f, length(vazias[[f]]), paste(vazias[[f]], collapse = " ")))

for (i in seq_len(nrow(tab))) registrar_numero(paste0("cuf_pct_", tab$sg_uf[i]), tab$pct[i])
for (f in names(vazias)) registrar_numero(paste0("cuf_n_ufs_sem_", gsub("[^a-z]", "_", f)), length(vazias[[f]]))
registrar_numero("cuf_pct_min", min(tab$pct)); registrar_numero("cuf_pct_max", max(tab$pct))
registrar_numero("cuf_uf_pior", tab[which.min(pct)]$sg_uf); registrar_numero("cuf_uf_melhor", tab[which.max(pct)]$sg_uf)
cat("\n37_cobertura_por_uf: concluido\n")
