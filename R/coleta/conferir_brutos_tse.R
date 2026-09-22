#!/usr/bin/env Rscript
# conferir_brutos_tse.R -- confere os zips de dados abertos do TSE (consulta_cand e votacao) contra
# a tabela de proveniencia ref/fontes_brutas_tse.csv (url, arquivo, bytes, md5).
#
# O CDN de dados abertos do TSE recusa requisicao de R e de curl (so aceita navegador de verdade), de
# modo que o download e sempre manual, pelas URLs listadas em R/coleta/manifest_consulta_cand.txt e
# R/coleta/manifest_votacao.txt. Pela regra de 21/09/2026, baixar um arquivo pronto de fonte
# oficial nao exige script (so a URL basta); o script aqui nao baixa nada, so grava e depois confere
# o checksum do que ja esta em disco, para a reconstrucao abortar se o bruto baixado a mao divergir
# do que gerou o banco publicado.
#
# Uso:
#   Rscript --vanilla R/coleta/conferir_brutos_tse.R gerar   -- grava ref/fontes_brutas_tse.csv a partir
#                                                                do bruto atual em data_raw/consulta_cand
#                                                                e data_raw/votacao
#   Rscript --vanilla R/coleta/conferir_brutos_tse.R          -- confere o bruto atual contra a tabela
#                                                                 gravada; aborta (stop) na primeira
#                                                                 divergencia ou arquivo ausente
suppressPackageStartupMessages(library(tools))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source("lib/proveniencia.R")

script <- "R/coleta/conferir_brutos_tse.R"
TABELA <- "ref/fontes_brutas_tse.csv"

CONJUNTOS <- list(
  consulta_cand = list(manifest = "R/coleta/manifest_consulta_cand.txt", dir = "data_raw/consulta_cand"),
  votacao       = list(manifest = "R/coleta/manifest_votacao.txt",       dir = "data_raw/votacao")
)

ler_manifest <- function(f) {
  u <- readLines(f, warn = FALSE)
  u <- trimws(u)
  u[nzchar(u) & !startsWith(u, "#")]
}

# uma linha por arquivo esperado, com a url e o diretorio de destino de cada manifesto
mapa_esperado <- function() {
  partes <- lapply(names(CONJUNTOS), function(nm) {
    urls <- ler_manifest(CONJUNTOS[[nm]]$manifest)
    data.frame(conjunto = nm, url = urls, arquivo = basename(urls), dir = CONJUNTOS[[nm]]$dir,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, partes)
}

gerar <- function() {
  m <- mapa_esperado()
  linhas <- lapply(seq_len(nrow(m)), function(i) {
    caminho <- file.path(m$dir[i], m$arquivo[i])
    if (!file.exists(caminho)) {
      stop(sprintf("conferir_brutos_tse: %s nao esta em disco (esperado em %s); baixe pela URL do manifesto antes de gerar a tabela",
                   m$arquivo[i], caminho))
    }
    data.frame(conjunto = m$conjunto[i], url = m$url[i], arquivo = m$arquivo[i],
               bytes = as.character(file.info(caminho)$size),
               md5 = unname(md5sum(caminho)), stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, linhas)
  dir.create(dirname(TABELA), recursive = TRUE, showWarnings = FALSE)
  write.csv(tab, TABELA, row.names = FALSE)
  registrar_numero("tse_brutos_registrados", nrow(tab), script = script)
  cat(sprintf("gravado %s com %d arquivos (%d consulta_cand, %d votacao)\n",
              TABELA, nrow(tab), sum(tab$conjunto == "consulta_cand"), sum(tab$conjunto == "votacao")))
  invisible(tab)
}

conferir <- function() {
  if (!file.exists(TABELA)) {
    stop(sprintf("conferir_brutos_tse: %s nao existe; rode 'Rscript --vanilla %s gerar' com o bruto correto em disco antes",
                 TABELA, script))
  }
  tab <- read.csv(TABELA, colClasses = "character", stringsAsFactors = FALSE)
  m <- mapa_esperado()
  divergencias <- character()
  for (i in seq_len(nrow(tab))) {
    dir_esp <- m$dir[m$arquivo == tab$arquivo[i]][1]
    if (is.na(dir_esp)) {
      divergencias <- c(divergencias, sprintf("%s: nao consta em nenhum manifesto atual", tab$arquivo[i]))
      next
    }
    caminho <- file.path(dir_esp, tab$arquivo[i])
    if (!file.exists(caminho)) {
      divergencias <- c(divergencias, sprintf("%s: ausente (esperado em %s; baixe pela URL do manifesto)", tab$arquivo[i], caminho))
      next
    }
    bytes_atual <- as.character(file.info(caminho)$size)
    md5_atual <- unname(md5sum(caminho))
    if (bytes_atual != tab$bytes[i]) {
      divergencias <- c(divergencias, sprintf("%s: %s bytes, tabela espera %s", tab$arquivo[i], bytes_atual, tab$bytes[i]))
    }
    if (md5_atual != tab$md5[i]) {
      divergencias <- c(divergencias, sprintf("%s: md5 %s, tabela espera %s", tab$arquivo[i], md5_atual, tab$md5[i]))
    }
  }
  registrar_numero("tse_brutos_conferidos", nrow(tab), script = script)
  registrar_numero("tse_brutos_divergentes", length(divergencias), script = script)
  if (length(divergencias)) {
    stop(sprintf("conferir_brutos_tse: %d divergencia(s) contra %s:\n%s",
                 length(divergencias), TABELA, paste(divergencias, collapse = "\n")))
  }
  cat(sprintf("OK: %d arquivos do TSE conferidos contra %s, sem divergencia\n", nrow(tab), TABELA))
  invisible(TRUE)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) && identical(args[1], "gerar")) gerar() else conferir()
