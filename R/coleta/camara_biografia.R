#!/usr/bin/env Rscript
# camara_biografia.R -- baixa a pagina de biografia oficial de cada deputado federal das legislaturas 51 a 57 (21/09/2026)
#
# A biografia da Camara, montada a partir do assentamento individual do deputado, traz em secoes padronizadas os
# mandatos com data de posse, as licencas com saida e reassuncao datadas, as renuncias, as suplencias e efetivacoes
# e a data de falecimento. A API de Dados Abertos entrega a legislatura 51 (1999-2003) sem eventos de exercicio, e a
# biografia e a fonte oficial que cobre esse periodo; nas legislaturas seguintes ela confere a API.
#
# Porte para R do antigo python/fetch_camara_biografia.py (21/09/2026): coleta primaria que entra na publicacao
# passa a ser R. Mesma fonte, mesmo formato de saida (html + csv de log), mesma logica de retentativa (3 tentativas,
# so aceita como sucesso status 200 com "Mandatos" no corpo) e mesmo espacamento entre pedidos; muda o motor http
# (httr2 no lugar de requests) e o user agent, agora "BOCEL coleta academica", sem credencial. req_retry() so espera
# antes de repetir (5s, 10s); o .py original ainda dormia 15s inuteis depois da terceira tentativa falhar, que aqui
# nao se repete por nao haver quarta tentativa a esperar.
#
# Fonte:   https://www.camara.leg.br/deputados/<id>/biografia (pagina html publica da Camara, sem autenticacao)
# Entrada: data_raw/camara/lista_leg_<n>.json (ids por legislatura, ja coletados)
# Saida:   <destino>/<id>.html e <destino>/_coleta.csv (id, status, bytes, data)
#          destino padrao: data_raw/camara/biografia sob BOCEL_ROOT
# Uso:     cd ~/bocel && Rscript --vanilla R/coleta/camara_biografia.R [--reverso]
#          --reverso percorre a lista de tras para frente, para um segundo processo dividir a coleta com o primeiro
#          BOCEL_COLETA_DESTINO=<caminho>   grava em outro destino em vez do data_raw/ padrao (usado no teste de porte)
#          BOCEL_COLETA_AMOSTRA=<n>         limita aos n primeiros ids da lista ordenada (usado no teste de porte)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

ROOT <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DEST <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = file.path(ROOT, "data_raw", "camara", "biografia"))
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
URL_TPL <- "https://www.camara.leg.br/deputados/%s/biografia"
UA <- "BOCEL coleta academica"

# ------------------------------------------------------------------ 1. ids: uniao das listas de legislatura 51-57
arqs_lista <- sort(Sys.glob(file.path(ROOT, "data_raw", "camara", "lista_leg_*.json")))
ids <- integer(0)
for (f in arqs_lista) {
  d <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  dados <- if (!is.null(d$dados)) d$dados else d
  if (is.data.frame(dados) && "id" %in% names(dados)) ids <- c(ids, as.integer(dados$id))
}
ids <- as.character(sort(unique(ids)))

args_cli <- commandArgs(trailingOnly = TRUE)
if ("--reverso" %in% args_cli) ids <- rev(ids)
cat(sprintf("deputados nas listas 51-57: %d\n", length(ids)))

amostra <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
if (nzchar(amostra)) {
  ids <- head(ids, as.integer(amostra))
  cat(sprintf("amostra limitada a %d id(s) por BOCEL_COLETA_AMOSTRA\n", length(ids)))
}

# ------------------------------------------------------------------ 2. log de coleta (append; cabecalho so se novo)
log_path <- file.path(DEST, "_coleta.csv")
novo <- !file.exists(log_path)
log_con <- file(log_path, open = "a", encoding = "UTF-8")
if (novo) writeLines("id,status,bytes,data", log_con)

# ------------------------------------------------------------------ 3. pedido http: throttle de 1 a cada 0.8s,
# ate 3 tentativas com espera de 5s e 10s entre elas, sucesso so com status 200 e "Mandatos" no corpo (o .py testava
# a mesma condicao em r.text antes de gravar). req_error(is_error=FALSE) devolve a resposta sempre, mesmo com status
# de erro, para o proprio loop decidir o que fazer -- como o requests do .py, que nunca lanca por status.
monta_pedido <- function(id) {
  httr2::request(sprintf(URL_TPL, id)) |>
    httr2::req_user_agent(UA) |>
    httr2::req_timeout(60) |>
    httr2::req_throttle(capacity = 1, fill_time_s = 0.8, realm = "camara_biografia") |>
    httr2::req_retry(
      max_tries = 3,
      retry_on_failure = TRUE,
      backoff = function(tentativas) 5 * tentativas,
      is_transient = function(resp) {
        corpo <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
        httr2::resp_status(resp) != 200L || !grepl("Mandatos", corpo, fixed = TRUE)
      }
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}

# ------------------------------------------------------------------ 4. baixa cada id que ainda nao esta em cache
feitos <- 0L
for (id in ids) {
  arq <- file.path(DEST, paste0(id, ".html"))
  if (file.exists(arq) && file.info(arq)$size > 5000) next

  status <- "erro"; tam <- 0L
  resp <- tryCatch(httr2::req_perform(monta_pedido(id)), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    status <- class(resp)[1]
  } else {
    status <- as.character(httr2::resp_status(resp))
    corpo <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    if (httr2::resp_status(resp) == 200L && grepl("Mandatos", corpo, fixed = TRUE)) {
      bruto <- httr2::resp_body_raw(resp)
      writeBin(bruto, arq)
      tam <- length(bruto)
    }
  }
  writeLines(sprintf("%s,%s,%d,%s", id, status, tam, format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), log_con)
  flush(log_con)
  feitos <- feitos + 1L
}
close(log_con)
cat(sprintf("baixadas nesta rodada: %d; em cache: %d\n", feitos, length(Sys.glob(file.path(DEST, "*.html")))))
