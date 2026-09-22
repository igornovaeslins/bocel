#!/usr/bin/env Rscript
# camara.R -- coleta, na API de Dados Abertos da Camara dos Deputados, a lista de deputados
# por legislatura (51 a 57), o detalhe de cada deputado e o historico de situacao (posse,
# exercicio, licenca, afastamento, renuncia, falecimento, cassacao).
# Porta python/fetch_camara.py para R (decisao de 19/09/2026: coleta que vai ao ar
# na v1.0 tem que ser R, nao Python). Mesma fonte, mesmos caminhos relativos, mesmo formato
# de saida, para os produtores que ja leem esses brutos (R/07_exercicio_camara.R,
# R/verifica_camara.R) seguirem sem mudanca.
#
# Fonte:  https://dadosabertos.camara.leg.br/api/v2 (Camara dos Deputados, dados abertos)
#         https://dadosabertos.camara.leg.br/arquivos/deputados/csv/deputados.csv (arquivo em massa)
# Sem credencial. User-Agent "BOCEL coleta academica" em toda chamada.
#
# Saida (cache em JSON/CSV, sob o destino configurado -- ver DESTINO abaixo):
#   lista_leg_<N>.json          lista bruta da legislatura N (so idLegislatura + dados, sem links)
#   deputados/<id>.json          detalhe (GET /deputados/{id}), corpo bruto da resposta
#   historico/<id>.json          historico (GET /deputados/{id}/historico), corpo bruto da resposta
#   deputados_bulk.csv           arquivo em massa (dataFalecimento etc.), bytes brutos
#   manifest.json                ids por legislatura + falhas desta rodada + ids so no arquivo em massa
# Log: logs/fetch_camara.log (ou <destino>/_log_coleta_camara.log quando o destino nao e o padrao)
#
# Configuracao por variavel de ambiente:
#   BOCEL_ROOT             raiz do repositorio (padrao ~/bocel)
#   BOCEL_COLETA_DESTINO   pasta de saida (padrao <BOCEL_ROOT>/data_raw/camara)
#   BOCEL_COLETA_AMOSTRA   limite de deputados processados nesta rodada, os primeiros da lista
#                          de ids ordenada (padrao: todos). So limita deputados/historico; a
#                          lista por legislatura e o arquivo em massa sempre saem inteiros,
#                          porque servem para calcular o universo completo de ids.
#
# Uso:      cd ~/bocel && Rscript --vanilla R/coleta/camara.R [--refresh]
# Papel ancilar (coleta na API); a construcao do banco e em R/07_exercicio_camara.R.
#
# Nota de fidelidade ao original (python/fetch_camara.py): a paginacao (5 req/s, ate 6
# tentativas, espera 2^k s limitada a 60s) e reproduzida com httr2::req_throttle() e
# httr2::req_retry(); um 404 no detalhe ou no historico de um deputado NAO conta como falha
# no manifest e grava o marcador {"__erro__": 404} no arquivo (mesmo comportamento do
# original, replicado aqui de proposito, nao corrigido). O arquivo em massa nao passa pelo
# limitador (o original tambem nao limitava essa chamada). Diferente do original, que usa
# 4 threads simultaneas, esta versao roda sequencial: o limitador ja fixa o teto de 5 req/s
# e o repositorio nao tem um padrao de concorrencia em R para chamadas HTTP, entao nao
# valeria a pena introduzir uma dependencia nova so para isso. httr2::req_retry() nao expoe
# um gancho por tentativa, entao o log aqui registra o resultado final de cada URL, nao uma
# linha por tentativa como no original.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(data.table)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DESTINO_PADRAO <- file.path(root, "data_raw", "camara")
OUT <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = DESTINO_PADRAO)
LOGF <- if (identical(OUT, DESTINO_PADRAO)) {
  file.path(root, "logs", "fetch_camara.log")
} else {
  file.path(OUT, "_log_coleta_camara.log")
}
BASE <- "https://dadosabertos.camara.leg.br/api/v2"
BULK <- "https://dadosabertos.camara.leg.br/arquivos/deputados/csv/deputados.csv"
UA <- "BOCEL coleta academica"
LEGS <- 51:57
REFRESH <- "--refresh" %in% commandArgs(trailingOnly = TRUE)

amostra_env <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
AMOSTRA <- if (nzchar(amostra_env)) suppressWarnings(as.integer(amostra_env)) else Inf
if (nzchar(amostra_env) && is.na(AMOSTRA)) stop(sprintf("BOCEL_COLETA_AMOSTRA invalido: '%s'", amostra_env))
if (is.finite(AMOSTRA)) stopifnot(AMOSTRA >= 1)

dir.create(file.path(OUT, "deputados"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT, "historico"), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOGF), recursive = TRUE, showWarnings = FALSE)

logmsg <- function(msg) {
  linha <- sprintf("%s %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), msg)
  cat(linha, "\n")
  cat(linha, "\n", file = LOGF, append = TRUE)
}

## ------------------------------------------------------------ HTTP: throttle + retentativa

# GET com limite de 5 req/s (bucket compartilhado por toda a API da Camara nesta execucao) e
# ate 6 tentativas com espera 2^(tentativas-1) segundos, limitada a 60s -- mesmo esquema do
# limiter/get() do original. 404 devolve na hora, sem tentativa nova (o alvo nao existe).
get_json <- function(url, tries = 6) {
  req <- httr2::request(url) |>
    httr2::req_user_agent(UA) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(60) |>
    httr2::req_throttle(capacity = 5, fill_time_s = 1, realm = "camara-dadosabertos") |>
    httr2::req_retry(
      max_tries = tries,
      retry_on_failure = TRUE,
      is_transient = function(resp) !(httr2::resp_status(resp) %in% c(200L, 404L)),
      backoff = function(n) min(60, 2 ^ (n - 1))
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    logmsg(sprintf("EXC %s %s", class(resp)[1], url))
    return(NULL)
  }
  status <- httr2::resp_status(resp)
  if (status == 200L) return(list(status = 200L, erro = FALSE, body = httr2::resp_body_string(resp)))
  if (status == 404L) return(list(status = 404L, erro = TRUE, body = NULL))
  logmsg(sprintf("HTTP %d %s (apos %d tentativas)", status, url, tries))
  NULL
}

## ------------------------------------------------------------ lista por legislatura

fetch_lista <- function(leg) {
  dest <- file.path(OUT, sprintf("lista_leg_%d.json", leg))
  if (file.exists(dest) && !REFRESH) return(jsonlite::fromJSON(dest))
  url <- sprintf("%s/deputados?idLegislatura=%d&itens=100&ordem=ASC&ordenarPor=id&pagina=1", BASE, leg)
  paginas <- list()
  while (!is.null(url)) {
    res <- get_json(url)
    if (is.null(res) || isTRUE(res$erro)) stop(sprintf("lista da legislatura %d falhou em %s", leg, url))
    j <- jsonlite::fromJSON(res$body, simplifyVector = TRUE)
    paginas[[length(paginas) + 1]] <- j$dados
    prox <- if (is.data.frame(j$links) && nrow(j$links) > 0) j$links$href[j$links$rel == "next"] else character(0)
    url <- if (length(prox) > 0) prox[1] else NULL
  }
  dados <- data.table::rbindlist(paginas, use.names = TRUE, fill = TRUE)
  jsonlite::write_json(list(idLegislatura = leg, dados = dados), dest,
                        auto_unbox = TRUE, dataframe = "rows", na = "null")
  logmsg(sprintf("legislatura %d: %d deputados na lista", leg, nrow(dados)))
  list(idLegislatura = leg, dados = dados)
}

## ------------------------------------------------------------ detalhe + historico de um deputado

fetch_dep <- function(dep_id) {
  resultado <- list(id = dep_id, detalhe = "ok", historico = "ok")
  alvos <- list(deputados = "", historico = "/historico")
  for (kind in names(alvos)) {
    dest <- file.path(OUT, kind, sprintf("%d.json", dep_id))
    if (file.exists(dest) && !REFRESH) next
    campo <- if (kind == "deputados") "detalhe" else "historico"
    j <- get_json(sprintf("%s/deputados/%d%s", BASE, dep_id, alvos[[kind]]))
    if (is.null(j)) {
      resultado[[campo]] <- "falha"
      next
    }
    # 404 nao conta como falha no manifest -- mesmo comportamento do fetch_dep() original,
    # que so marca "falha" quando get() devolve None (esgotou as tentativas).
    if (isTRUE(j$erro)) cat('{"__erro__": 404}', file = dest) else cat(j$body, file = dest)
  }
  resultado
}

## ------------------------------------------------------------ arquivo em massa (sem limitador, como no original)

fetch_bulk <- function() {
  dest <- file.path(OUT, "deputados_bulk.csv")
  if (file.exists(dest) && !REFRESH) return(invisible(NULL))
  req <- httr2::request(BULK) |>
    httr2::req_user_agent(UA) |>
    httr2::req_headers(Accept = "*/*") |>
    httr2::req_timeout(300) |>
    httr2::req_error(is_error = function(resp) FALSE)
  resp <- tryCatch(httr2::req_perform(req, path = dest), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    logmsg(sprintf("arquivo em massa falhou: %s", conditionMessage(resp)))
    if (file.exists(dest)) file.remove(dest)
    return(invisible(NULL))
  }
  status <- httr2::resp_status(resp)
  if (status != 200L) {
    logmsg(sprintf("arquivo em massa falhou: HTTP %d", status))
    if (file.exists(dest)) file.remove(dest)
    return(invisible(NULL))
  }
  logmsg(sprintf("arquivo em massa deputados.csv: %d bytes", file.info(dest)$size))
  invisible(NULL)
}

## ------------------------------------------------------------ orquestracao

main <- function() {
  logmsg(sprintf("inicio fetch_camara (refresh=%s, amostra=%s, destino=%s)",
                  REFRESH, if (is.finite(AMOSTRA)) AMOSTRA else "todos", OUT))
  fetch_bulk()

  manifest <- list(legislaturas = list(), falhas = list())
  ids_all <- integer(0)
  for (leg in LEGS) {
    lst <- fetch_lista(leg)
    lid <- sort(unique(as.integer(lst$dados$id)))
    manifest$legislaturas[[as.character(leg)]] <- lid
    ids_all <- union(ids_all, lid)
  }

  # arquivo em massa: ids cuja faixa de legislaturas toca 51-57 mas que faltam nas listas
  bulk_path <- file.path(OUT, "deputados_bulk.csv")
  extra_bulk <- integer(0)
  if (file.exists(bulk_path)) {
    bulk <- data.table::fread(bulk_path, sep = ";", encoding = "UTF-8", colClasses = "character")
    a <- suppressWarnings(as.integer(bulk$idLegislaturaInicial)); a[is.na(a)] <- 0L
    b <- suppressWarnings(as.integer(bulk$idLegislaturaFinal)); b[is.na(b)] <- 0L
    i <- suppressWarnings(as.integer(sub(".*/", "", bulk$uri)))
    sel <- !is.na(i) & b >= LEGS[1] & a <= LEGS[length(LEGS)] & !(i %in% ids_all)
    extra_bulk <- sort(unique(i[sel]))
    ids_all <- union(ids_all, extra_bulk)
  }
  manifest$ids_so_arquivo_massa <- sort(extra_bulk)
  ids_all <- sort(ids_all)
  logmsg(sprintf("%d deputados unicos nas legislaturas %d-%d (%d so no arquivo em massa)",
                  length(ids_all), LEGS[1], LEGS[length(LEGS)], length(extra_bulk)))

  ids_alvo <- if (is.finite(AMOSTRA)) utils::head(ids_all, AMOSTRA) else ids_all
  done <- 0L
  for (dep_id in ids_alvo) {
    r <- fetch_dep(dep_id)
    if (r$detalhe != "ok" || r$historico != "ok") manifest$falhas[[length(manifest$falhas) + 1]] <- r
    done <- done + 1L
    if (done %% 250 == 0) logmsg(sprintf("%d/%d deputados coletados", done, length(ids_alvo)))
  }

  manifest$n_ids <- length(ids_all)
  manifest$coletado_em <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
  jsonlite::write_json(manifest, file.path(OUT, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  logmsg(sprintf("fim: %d ids no universo, %d tentados nesta rodada, %d falhas",
                  length(ids_all), length(ids_alvo), length(manifest$falhas)))
}

main()
