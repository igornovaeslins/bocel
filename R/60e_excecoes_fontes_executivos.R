#!/usr/bin/env Rscript
# 60e_excecoes_fontes_executivos.R -- gera em docs/EXCECOES_CONHECIDAS.csv uma linha para cada mandato executivo
# (presidente, vice-presidente, governador, vice-governador) cujo evento curado admissivel (ref/eventos_governos_
# fonte_oficial.csv e ref/eventos_presidencia_fonte_oficial.csv, com confianca alta ou media) so tem fonte_1 e
# fonte_2 de imprensa comum ou Wikipedia, sem fonte oficial, verbete do DHBB/CPDOC-FGV ou noticia de orgao publico
# localizada -- o mesmo calculo que R/60_saida_executivos.R faz para rotular esses mandatos como pista_nao_oficial
# (21/09/2026). O script e idempotente: nao duplica linha ja presente para o mesmo ano_eleicao+cargo+sg_uf.
#
# Entrada:  ref/eventos_governos_fonte_oficial.csv, ref/eventos_presidencia_fonte_oficial.csv, docs/EXCECOES_CONHECIDAS.csv
# Saida:    docs/EXCECOES_CONHECIDAS.csv (linhas novas acrescentadas ao final, arquivo CRLF preservado)
# Execucao: cd ~/bocel && Rscript --vanilla R/60e_excecoes_fontes_executivos.R
suppressPackageStartupMessages({ library(data.table) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
source(file.path(root, "lib", "tipo_fonte.R"))
script <- "R/60e_excecoes_fontes_executivos.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
le <- function(f) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")

ARQ <- "docs/EXCECOES_CONHECIDAS.csv"

cur <- rbindlist(list(le("ref/eventos_governos_fonte_oficial.csv"), le("ref/eventos_presidencia_fonte_oficial.csv")),
                  use.names = TRUE, fill = TRUE)
cur <- cur[!is.na(fonte_1) & confianca %chin% c("alta", "media")]
cur[, rotulo := rotulo_evento_curado(fonte_1, fonte_2)]
pista <- cur[rotulo %chin% "pista_nao_oficial"]
reg("fex_excecoes_pista_nao_oficial_total", nrow(pista))

pista[, `:=`(dom_1 = dominio_fonte(fonte_1), dom_2 = dominio_fonte(fonte_2))]
pista[, descricao := sprintf(
  "Sem fonte oficial, verbete do DHBB/CPDOC-FGV ou noticia de orgao publico localizada para a saida deste mandato; fonte_1 (%s) e fonte_2 (%s) registradas sao imprensa comum, mantidas como pista na ausencia de fonte melhor.",
  dom_1, ifelse(is.na(dom_2) | !nzchar(dom_2), "nao ha", dom_2))]

## ---------------------------------------------------------------- leitura byte-preservada do arquivo de excecoes
raw <- readBin(ARQ, "raw", file.info(ARQ)$size)
txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
termina_crlf <- endsWith(txt, "\r\n")
linhas <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
existentes <- le(ARQ) # so para checar duplicidade pela chave ano_eleicao+cargo+sg_uf
chave_existente <- existentes[, paste(ano_eleicao, cargo, sg_uf)]

campo <- function(x) {
  stopifnot(!grepl('"', x, fixed = TRUE)) # nenhum campo novo leva aspas embutidas
  if (grepl(",", x, fixed = TRUE)) paste0('"', x, '"') else x
}
montar_linha <- function(r) paste(vapply(r, campo, character(1)), collapse = ",")

novas_linhas <- character(0)
n_novas <- 0L
for (i in seq_len(nrow(pista))) {
  r <- pista[i]
  chave <- paste(r$ano_eleicao, r$cargo, r$sg_uf)
  if (chave %chin% chave_existente) next
  novas_linhas <- c(novas_linhas, montar_linha(list(r$ano_eleicao, r$cargo, r$sg_uf, "NA", r$descricao)))
  chave_existente <- c(chave_existente, chave)
  n_novas <- n_novas + 1L
}

linhas <- c(linhas, novas_linhas)
saida_txt <- paste(linhas, collapse = "\r\n")
if (termina_crlf) saida_txt <- paste0(saida_txt, "\r\n")
con <- file(ARQ, "wb"); on.exit(close(con), add = TRUE)
writeBin(charToRaw(enc2utf8(saida_txt)), con)

reg("fex_excecoes_linhas_acrescentadas", n_novas)
cat(sprintf("OK: %d linha(s) nova(s) de pista_nao_oficial acrescentada(s) a %s (de %d encontradas)\n", n_novas, ARQ, nrow(pista)))
