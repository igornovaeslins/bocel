#!/usr/bin/env Rscript
# senado.R -- coleta (ancilar) da API de Dados Abertos do Senado Federal: baixa e guarda em
# cache, sem transformar, tres familias de respostas JSON para todos os parlamentares (titulares
# e suplentes) que aparecem nas legislaturas 51 a 57 (eleicoes de 1998 a 2022):
#   lista_legislatura_<n>.json   /senador/lista/legislatura/<n>   (n = 51..57)
#   mandatos_<codigo>.json       /senador/<codigo>/mandatos
#   detalhe_<codigo>.json        /senador/<codigo>
# A construcao do banco e o pareamento ficam em R/07_exercicio_senado.R, que le esses brutos.
#
# Porte de python/fetch_senado.py para R em 21/09/2026 (regra do projeto: coleta primaria que
# entra na publicacao e em R). Mesmos caminhos e mesmo formato de saida do script antigo -- troca
# urllib por httr2 (com pausa e retentativa) e o par json.loads/json.dump por gravar direto o
# corpo da resposta, que ja e o JSON da API sem nenhuma transformacao. O corpo gravado tem outra
# formatacao de espacos que a reserializacao do Python produzia, mas a mesma estrutura, e quem le
# esses brutos depois (fromJSON, em R/07_exercicio_senado.R) nao diferencia uma coisa da outra.
# O script antigo baixava mandatos+detalhe com 6 downloads em paralelo (ThreadPoolExecutor) e sem
# pausa explicita entre requisicoes; aqui a coleta e sequencial e o req_throttle faz o papel de
# poupar o servidor no lugar do teto implicito da concorrencia.
#
# Fonte: API de Dados Abertos do Senado Federal, sem credencial.
#   https://legis.senado.leg.br/dadosabertos/senador/lista/legislatura/<legislatura>
#   https://legis.senado.leg.br/dadosabertos/senador/<codigo>/mandatos
#   https://legis.senado.leg.br/dadosabertos/senador/<codigo>
#
# Saida, em BOCEL_COLETA_DESTINO (padrao data_raw/senado sob BOCEL_ROOT):
#   lista_legislatura_<n>.json, mandatos_<codigo>.json, detalhe_<codigo>.json  (corpo cru da API)
#   _codigos.txt   codigos de parlamentar processados nesta rodada, um por linha, ordem numerica
#   _lacunas.csv   familia,chave,erro -- uma linha por download que falhou apos as retentativas
#
# Variaveis de ambiente:
#   BOCEL_ROOT             raiz do repositorio (padrao ~/bocel)
#   BOCEL_COLETA_DESTINO   pasta de saida (padrao data_raw/senado sob BOCEL_ROOT)
#   BOCEL_COLETA_AMOSTRA   limite de codigos de parlamentar, os primeiros da lista ordenada
#                          (padrao: todos). As 7 listas por legislatura sao sempre baixadas
#                          inteiras -- e delas que a amostra tira "os primeiros" codigos.
#
# Uso: Rscript --vanilla R/coleta/senado.R [--force]   (--force rebaixa mesmo o que ja esta em cache)
# Log: logs/coleta_senado.log
# Execucao: cd ~/bocel && Rscript --vanilla R/coleta/senado.R

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

ROOT <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DEST <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = file.path(ROOT, "data_raw", "senado"))
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)

amostra_env <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
AMOSTRA <- if (nzchar(amostra_env)) as.integer(amostra_env) else NA_integer_
if (nzchar(amostra_env) && (is.na(AMOSTRA) || AMOSTRA < 1L)) {
  stop("BOCEL_COLETA_AMOSTRA precisa ser um inteiro positivo, recebi '", amostra_env, "'")
}

FORCE <- "--force" %in% commandArgs(trailingOnly = TRUE)
BASE_URL <- "https://legis.senado.leg.br/dadosabertos"
LEGISLATURAS <- 51:57

LOG <- file.path(ROOT, "logs", "coleta_senado.log")
dir.create(dirname(LOG), recursive = TRUE, showWarnings = FALSE)
log_msg <- function(msg) {
  linha <- paste0(format(Sys.time(), "%Y-%m-%dT%H:%M:%S "), msg)
  cat(linha, "\n", sep = "")
  cat(linha, "\n", file = LOG, append = TRUE, sep = "")
}
log_msg(sprintf("senado.R -- destino %s%s", DEST,
                 if (!is.na(AMOSTRA)) sprintf(" (amostra %d)", AMOSTRA) else ""))

# ---------------------------------------------------------------- requisicao: pausa e retentativa
# no maximo 4 requisicoes por segundo ao mesmo servidor (req_throttle). Ate 4 tentativas com
# espera de 1,5s vezes o numero da tentativa (1,5s, 3s, 4,5s) em erro transitorio (req_retry) --
# a mesma contagem e a mesma progressao do script original em Python. Um erro HTTP 400 ou 404 nao
# e transitorio e desiste na hora, tambem como no script original (evita gastar 4 tentativas em
# codigo que a API garante que nao existe).
fazer_req <- function(caminho) {
  httr2::request(BASE_URL) |>
    httr2::req_url_path_append(caminho) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("BOCEL coleta academica") |>
    httr2::req_timeout(60) |>
    httr2::req_throttle(rate = 4) |>
    httr2::req_retry(
      max_tries = 4,
      retry_on_failure = TRUE,
      is_transient = function(resp) !(httr2::resp_status(resp) %in% c(400L, 404L)),
      backoff = function(tentativa) 1.5 * tentativa
    )
}

baixar_json <- function(caminho) {
  resp <- tryCatch(httr2::req_perform(fazer_req(caminho)), error = function(e) e)
  if (inherits(resp, "error")) {
    erro <- if (!is.null(resp$status)) sprintf("HTTP %d", resp$status) else conditionMessage(resp)
    return(list(corpo = NULL, erro = erro))
  }
  list(corpo = httr2::resp_body_raw(resp), erro = NULL)
}

esta_em_cache <- function(arq) !FORCE && file.exists(arq) && file.info(arq)$size > 0

as_lista <- function(x) if (is.null(x)) list() else if (!is.null(names(x))) list(x) else x
chr1 <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])

lacunas <- list()
registrar_lacuna <- function(familia, chave, erro) {
  log_msg(sprintf("LACUNA %s %s: %s", familia, chave, erro))
  lacunas[[length(lacunas) + 1L]] <<- data.frame(familia = familia, chave = as.character(chave),
                                                  erro = erro, stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------- 1. listas por legislatura (sempre inteiras)
codigos <- character(0)
for (n in LEGISLATURAS) {
  nome <- sprintf("lista_legislatura_%d.json", n)
  arq <- file.path(DEST, nome)
  if (!esta_em_cache(arq)) {
    r <- baixar_json(sprintf("/senador/lista/legislatura/%d", n))
    if (is.null(r$corpo)) { registrar_lacuna("lista", n, r$erro); next }
    writeBin(r$corpo, arq)
  }
  d <- jsonlite::fromJSON(arq, simplifyVector = FALSE)
  parl <- as_lista(d$ListaParlamentarLegislatura$Parlamentares$Parlamentar)
  cods <- vapply(parl, function(p) chr1(p$IdentificacaoParlamentar$CodigoParlamentar), character(1))
  cods <- unique(cods[!is.na(cods)])
  codigos <- union(codigos, cods)
  log_msg(sprintf("legislatura %d: %d parlamentares na lista (%d codigos)", n, length(parl), length(cods)))
}
codigos <- codigos[order(as.integer(codigos))]
log_msg(sprintf("codigos unicos nas legislaturas 51-57: %d", length(codigos)))

if (!is.na(AMOSTRA) && AMOSTRA < length(codigos)) {
  codigos <- codigos[seq_len(AMOSTRA)]
  log_msg(sprintf("amostra ativa (BOCEL_COLETA_AMOSTRA=%d): os %d primeiros codigos, em ordem numerica",
                   AMOSTRA, AMOSTRA))
}

# ---------------------------------------------------------------- 2. mandatos + detalhe por codigo
n_ok <- 0L
n_cache <- 0L
familias <- list(c("mandatos", "/senador/%s/mandatos"), c("detalhe", "/senador/%s"))
for (k in seq_along(codigos)) {
  cod <- codigos[k]
  for (fam in familias) {
    nome <- sprintf("%s_%s.json", fam[1], cod)
    arq <- file.path(DEST, nome)
    if (esta_em_cache(arq)) { n_cache <- n_cache + 1L; next }
    r <- baixar_json(sprintf(fam[2], cod))
    if (is.null(r$corpo)) { registrar_lacuna(fam[1], cod, r$erro); next }
    writeBin(r$corpo, arq)
    n_ok <- n_ok + 1L
  }
  if (k %% 100 == 0) log_msg(sprintf("progresso: %d/%d codigos", k, length(codigos)))
}

log_msg(sprintf("arquivos baixados agora: %d; do cache: %d; lacunas: %d", n_ok, n_cache, length(lacunas)))

# ---------------------------------------------------------------- 3. lacunas e lista de codigos processados
con <- file(file.path(DEST, "_lacunas.csv"), "w", encoding = "UTF-8")
writeLines("familia,chave,erro", con)
for (l in lacunas) writeLines(sprintf('%s,%s,"%s"', l$familia, l$chave, l$erro), con)
close(con)
writeLines(codigos, file.path(DEST, "_codigos.txt"))
log_msg("senado.R: concluido")
