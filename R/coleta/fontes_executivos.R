#!/usr/bin/env Rscript
# fontes_executivos.R -- baixa o bruto de cada fonte curada dos eventos de governador/vice e de Presidencia/vice, mais
# cada URL candidata em ref/candidatos_fontes_executivos.csv, para a conferencia mecanica do trecho contra o
# documento real (R/60a_conferir_fontes_executivos.R) nao depender de pagina lida por fora do R (21/09/2026).
#
# Entrada:  ref/eventos_governos_fonte_oficial.csv, ref/eventos_presidencia_fonte_oficial.csv (colunas fonte_1/fonte_2),
#           ref/candidatos_fontes_executivos.csv (colunas url/url_2, achados a conferir antes de entrar na curadoria)
# Saida:    data_raw/fontes_executivos/<hash>.<ext> (um arquivo por URL unica), data_raw/fontes_executivos/manifesto.csv
#           (url, arquivo, status, bytes, md5, data)
# Execucao: cd ~/bocel && Rscript --vanilla R/coleta/fontes_executivos.R
suppressPackageStartupMessages({ library(httr2); library(data.table); library(tools) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/coleta/fontes_executivos.R"
UA <- "BOCEL coleta academica"
DEST <- file.path(root, "data_raw", "fontes_executivos")
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
MANIFESTO <- file.path(DEST, "manifesto.csv")

le <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8") else NULL

## ---------------------------------------------------------------- lista de URLs a baixar
urls <- character()
for (f in c("ref/eventos_governos_fonte_oficial.csv", "ref/eventos_presidencia_fonte_oficial.csv")) {
  x <- le(f); if (is.null(x)) next
  urls <- c(urls, x$fonte_1, x$fonte_2)
}
cand <- le("ref/candidatos_fontes_executivos.csv")
if (!is.null(cand)) urls <- c(urls, cand$url, cand$url_2)
urls <- unique(trimws(urls))
urls <- urls[!is.na(urls) & nzchar(urls) & grepl("^https?://", urls)]
reg <- function(k, v) registrar_numero(k, v, script = script)
reg("fex_urls_distintas", length(urls))

## ---------------------------------------------------------------- nome de arquivo por hash da URL
extensao_de <- function(u) {
  caminho <- tolower(sub("[?#].*$", "", u))
  ext <- sub("^.*[.]", "", caminho)
  if (nchar(ext) >= 2 && nchar(ext) <= 5 && grepl("^[a-z0-9]+$", ext)) ext else "bin"
}
arquivo_de <- function(u) sprintf("%s.%s", md5sum_texto(u), extensao_de(u))
md5sum_texto <- function(s) { f <- tempfile(); on.exit(unlink(f)); writeLines(s, f, useBytes = TRUE); unname(tools::md5sum(f)) }

## ---------------------------------------------------------------- download com pausa curta e retentativa
baixar_um <- function(u) {
  destino <- file.path(DEST, arquivo_de(u))
  req <- httr2::request(u) |>
    httr2::req_user_agent(UA) |>
    httr2::req_timeout(45) |>
    httr2::req_throttle(rate = 3, realm = "fontes-executivos") |>
    httr2::req_retry(max_tries = 2, backoff = function(n) 2 * n) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, path = destino), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    return(data.table(url = u, arquivo = NA_character_, status = paste0("erro_rede:", conditionMessage(resp)),
                      bytes = NA_integer_, md5 = NA_character_, data = format(Sys.Date())))
  }
  st <- httr2::resp_status(resp)
  if (st != 200L || !file.exists(destino)) {
    unlink(destino)
    return(data.table(url = u, arquivo = NA_character_, status = paste0("http_", st),
                      bytes = NA_integer_, md5 = NA_character_, data = format(Sys.Date())))
  }
  data.table(url = u, arquivo = basename(destino), status = "ok", bytes = file.info(destino)$size,
             md5 = unname(tools::md5sum(destino)), data = format(Sys.Date()))
}

manifesto_antigo <- if (file.exists(MANIFESTO)) fread(MANIFESTO, colClasses = "character", na.strings = c("", "NA")) else NULL
linhas <- vector("list", length(urls))
for (i in seq_along(urls)) linhas[[i]] <- baixar_um(urls[i])
manifesto_novo <- rbindlist(linhas)

# uma linha por URL; a rodada mais recente substitui a anterior da mesma URL
if (!is.null(manifesto_antigo)) {
  manifesto_antigo <- manifesto_antigo[!url %chin% manifesto_novo$url]
  manifesto <- rbindlist(list(manifesto_antigo, manifesto_novo), use.names = TRUE, fill = TRUE)
} else {
  manifesto <- manifesto_novo
}
setorder(manifesto, url)
fwrite(manifesto, MANIFESTO, na = "NA", quote = TRUE)

reg("fex_urls_baixadas_ok", manifesto_novo[status == "ok", .N])
reg("fex_urls_falha_rede_ou_http", manifesto_novo[status != "ok", .N])
cat(sprintf("OK: %d URLs desta rodada, %d baixadas, %d com falha (rede ou HTTP); manifesto com %d linhas em %s\n",
            nrow(manifesto_novo), manifesto_novo[status == "ok", .N], manifesto_novo[status != "ok", .N],
            nrow(manifesto), MANIFESTO))
