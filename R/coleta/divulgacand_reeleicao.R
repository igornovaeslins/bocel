# divulgacand_reeleicao.R -- consulta a API publica DivulgaCandContas do TSE para ler
# ST_REELEICAO (situacao e partido da candidatura seguinte), nos anos em que o arquivo
# consulta_cand distribuido pelo TSE nao traz mais essa coluna (2014, 2018, 2020, 2022, 2024).
# Porte de python/fetch_divulgacand_reeleicao.py para R (decisao de 19/09/2026:
# coleta que entra na publicacao tem de ser R), 21/09/2026.
#
# Fonte:   https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/
#          {ano}/{unidade_eleitoral}/{id_eleicao}/candidato/{sq_candidato}  (API publica, sem chave)
# Entrada: data_raw/divulgacand/pedidos_reeleicao.csv (ano_eleicao, sg_ue, cd_cargo,
#          sq_candidato), gerado por R/14_sinais_tse_exercicio.R
# Saida:   <destino>/reeleicao_<ANO>.csv     (uma linha por candidatura consultada)
#          <destino>/respostas_<ANO>.jsonl   (cache bruto, append, retomavel entre rodadas)
#          <destino> = BOCEL_COLETA_DESTINO, ou data_raw/divulgacand sob BOCEL_ROOT quando
#          a variavel nao esta definida (mesmo caminho e mesmo formato do fetch em Python)
# Amostra: BOCEL_COLETA_AMOSTRA limita a quantos pedidos processar nesta rodada, tomando os
#          primeiros da lista ordenada por (ano_eleicao, sg_ue, cd_cargo, sq_candidato); sem a
#          variavel, processa todos os pedidos do arquivo de entrada (like o Python)
# Execucao: Rscript --vanilla R/coleta/divulgacand_reeleicao.R [caminho/pedidos.csv]
#           (a partir da raiz do repositorio; aceita o caminho do CSV de pedidos como
#           primeiro argumento posicional, para manter paridade com o script em Python)
#
# Nota sobre a fonte: o host devolve HTTP 403 a qualquer cliente que nao reproduza a
# assinatura TLS (JA3) de um navegador real -- inclusive ao curl de linha de comando sem
# nenhum cabecalho customizado, e ao pacote requests do Python sem impersonation. O fetch em
# Python contornava isso com curl_cffi(impersonate="chrome"), que troca a biblioteca TLS por
# uma copia do handshake do Chrome; nao ha equivalente disso em httr2/curl no R (o pacote curl
# do R usa a biblioteca TLS do sistema, sem controle sobre a ordem de cifras/extensoes do
# ClientHello). Este script usa httr2 do jeito correto -- User-Agent proprio, req_retry() e
# req_throttle() -- e quando a fonte bloqueia por TLS, a requisicao falha do mesmo jeito que
# falharia para qualquer outro cliente comum: ver output/verificacao/porte_divulgacand_reeleicao.csv
# para a evidencia item a item desta rodada de teste.
#
# Retentativa e pausa: o Python fazia 5 tentativas por item, com pausa crescente de
# 2 + 3*tentativa segundos entre elas, usando um pool de threads para paralelismo. Aqui a
# mesma politica de tentativa e pausa fica inteira dentro de req_retry()/req_throttle() do
# proprio httr2 (max_tries = 5, o mesmo backoff, e no maximo ~1 requisicao por segundo ao
# host depois de uma rajada inicial, no padrao ja usado em R/coleta/assembleias3/fetch_rj.R).
# A busca roda sequencial (nao em paralelo): req_perform_parallel() do httr2 documenta que,
# em paralelo, NAO respeita o limite de max_tries de req_retry() entre requisicoes da mesma
# fila -- arriscado contra um host que bloqueia toda requisicao, onde o limite de tentativas
# e o que garante que o script termina.
#
# Resultado que fica gravado quando uma consulta nao devolve 200 nem 400/404 depois de todas
# as tentativas (bloqueio por TLS, timeout, erro de rede): http = -1, igual ao Python -- nao o
# codigo real da ultima tentativa -- porque so esse valor marca o item como "sem resposta"
# para a proxima rodada tentar de novo (ver carregar_cache() abaixo e o uso em
# R/14_sinais_tse_exercicio.R / R/verifica_reeleicao_tse.R).

suppressPackageStartupMessages({
  library(data.table)
  library(httr2)
  library(jsonlite)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
dc_entrada <- file.path(root, "data_raw", "divulgacand")

args <- commandArgs(trailingOnly = TRUE)
PEDIDOS <- if (length(args) >= 1) args[[1]] else file.path(dc_entrada, "pedidos_reeleicao.csv")
if (!file.exists(PEDIDOS)) stop("arquivo de pedidos nao encontrado: ", PEDIDOS)

DESTINO <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = "")
if (!nzchar(DESTINO)) DESTINO <- dc_entrada
dir.create(DESTINO, recursive = TRUE, showWarnings = FALSE)

AMOSTRA <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")

# id da eleicao ordinaria por ano na API DivulgaCand (mesmo mapa do Python)
ID_ELEICAO <- c(`2014` = "680", `2016` = "2", `2018` = "2022802018",
                 `2020` = "2030402020", `2022` = "2040602022", `2024` = "2045202024")
# pleitos com id proprio na API: Macapa (06050) votou em dezembro de 2020 e consta como
# "Eleicoes Municipais 2020 - AP" (id 2032002020); com o id geral o host devolve corpo vazio
ID_ELEICAO_UE <- c(`2020_06050` = "2032002020")
BASE <- "https://divulgacandcontas.tse.jus.br/divulga/rest/v1/candidatura/buscar/%s/%s/%s/candidato/%s"

# ------------------------------------------------------------------ cache bruto (respostas_<ano>.jsonl)

# le o cache de uma rodada anterior; linha malformada e ignorada (mesma tolerancia do Python),
# e quando o sq_candidato se repete (reexecucao que acrescentou linha nova) a ULTIMA linha lida
# vale, porque e a que sobrescreve no dicionario do Python
carregar_cache <- function(destino, ano) {
  f <- file.path(destino, sprintf("respostas_%d.jsonl", ano))
  feito <- list()
  if (file.exists(f)) {
    linhas <- readLines(f, warn = FALSE, encoding = "UTF-8")
    for (ln in linhas) {
      if (!nzchar(trimws(ln))) next
      d <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(d) && !is.null(d$sq_candidato)) feito[[as.character(d$sq_candidato)]] <- d
    }
  }
  feito
}

# ------------------------------------------------------------------ requisicao

# molde da requisicao: User-Agent proprio (sem credencial), timeout de 40s como no Python,
# no maximo ~1 req/s sustentada ao host (rajada inicial de 60, o mesmo espirito do
# Sys.sleep(1) # no maximo uma requisicao por segundo ao mesmo servidor de fetch_rj.R), e
# repete ate 5 vezes com a mesma pausa crescente do Python (2, 5, 8, 11 segundos entre
# tentativas) enquanto a resposta nao for 200 com corpo valido nem 400/404 definitivo
requisicao_base <- function() {
  is_ok_definitivo <- function(resp) {
    st <- httr2::resp_status(resp)
    if (st %in% c(400L, 404L)) return(TRUE)
    if (st == 200L) {
      corpo <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      if (nzchar(trimws(corpo))) {
        ok <- !is.null(tryCatch(jsonlite::fromJSON(corpo, simplifyVector = TRUE), error = function(e) NULL))
        return(ok)
      }
      return(FALSE)
    }
    FALSE
  }
  httr2::request(BASE) |>
    httr2::req_user_agent("BOCEL coleta academica") |>
    httr2::req_timeout(40) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_throttle(capacity = 60, fill_time_s = 60, realm = "divulgacandcontas.tse.jus.br") |>
    httr2::req_retry(max_tries = 5, retry_on_failure = TRUE,
                      is_transient = function(resp) !is_ok_definitivo(resp),
                      backoff = function(tries) 2 + 3 * (tries - 1))
}

# consulta uma candidatura; devolve sempre os mesmos 11 campos do Python, na mesma ordem
consultar_um <- function(req_base, ano, ue, sq) {
  eid_chave <- paste(ano, ue, sep = "_")
  eid <- if (eid_chave %in% names(ID_ELEICAO_UE)) ID_ELEICAO_UE[[eid_chave]] else ID_ELEICAO[[as.character(ano)]]
  url <- sprintf(BASE, ano, ue, eid, sq)
  dt_consulta <- format(Sys.Date(), "%Y-%m-%d")
  vazio <- list(sq_candidato = sq, ano_eleicao = ano, sg_ue = ue, http = -1L,
                st_reeleicao = NA, descricao_situacao = NA_character_,
                sigla_partido = NA_character_, nome_completo = NA_character_,
                numero = NA_integer_, cargo = NA_character_, dt_consulta = dt_consulta)

  # suppressMessages() esconde a barra de progresso de retentativa do httr2 (cli), que nao
  # muda nada no resultado -- so polui o console em rodada longa
  resp <- tryCatch(suppressMessages(httr2::req_perform(httr2::req_url(req_base, url))),
                    error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) return(vazio)

  status <- httr2::resp_status(resp)
  if (status %in% c(400L, 404L)) {
    r <- vazio; r$http <- status
    return(r)
  }
  if (status == 200L) {
    corpo <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
    if (nzchar(trimws(corpo))) {
      d <- tryCatch(jsonlite::fromJSON(corpo, simplifyVector = TRUE), error = function(e) NULL)
      if (!is.null(d)) {
        return(list(
          sq_candidato = sq, ano_eleicao = ano, sg_ue = ue, http = 200L,
          st_reeleicao = if (is.null(d$st_REELEICAO)) NA else d$st_REELEICAO,
          descricao_situacao = if (is.null(d$descricaoSituacao)) NA_character_ else d$descricaoSituacao,
          sigla_partido = if (is.null(d$partido$sigla)) NA_character_ else d$partido$sigla,
          nome_completo = if (is.null(d$nomeCompleto)) NA_character_ else d$nomeCompleto,
          numero = if (is.null(d$numero)) NA_integer_ else as.integer(d$numero),
          cargo = if (is.null(d$cargo$nome)) NA_character_ else d$cargo$nome,
          dt_consulta = dt_consulta
        ))
      }
    }
  }
  # qualquer outro caso (bloqueio por TLS, erro 5xx persistente, timeout apos as tentativas)
  # cai aqui com http = -1, igual ao fallback do Python
  vazio
}

# ------------------------------------------------------------------ leitura dos pedidos

ped <- data.table::fread(PEDIDOS, colClasses = "character", encoding = "UTF-8")
ped <- ped[ano_eleicao %in% names(ID_ELEICAO)]

if (nzchar(AMOSTRA)) {
  n_amostra <- as.integer(AMOSTRA)
  data.table::setorder(ped, ano_eleicao, sg_ue, cd_cargo, sq_candidato)
  ped <- head(ped, n_amostra)
  cat("BOCEL_COLETA_AMOSTRA=", n_amostra, ": usando os ", nrow(ped),
      " primeiros pedidos da lista ordenada\n", sep = "")
}

# "ultima linha vence" por (ano_eleicao, sq_candidato), preservando a ordem original do
# arquivo de entrada -- o mesmo efeito do dict do Python (pedidos.setdefault(ano, {})[sq] = ue)
ped[, linha_original := .I]
data.table::setorder(ped, ano_eleicao, sq_candidato, linha_original)
ped_dedup <- ped[, .SD[.N], by = .(ano_eleicao, sq_candidato)]

campos_saida <- c("sq_candidato", "ano_eleicao", "sg_ue", "http", "st_reeleicao",
                   "descricao_situacao", "sigla_partido", "nome_completo", "numero",
                   "cargo", "dt_consulta")
req_base <- requisicao_base()

anos <- sort(unique(as.integer(ped_dedup$ano_eleicao)))
for (ano in anos) {
  tab_ano <- ped_dedup[ano_eleicao == as.character(ano)]
  feito <- carregar_cache(DESTINO, ano)
  http_previo <- vapply(tab_ano$sq_candidato, function(sq) {
    if (!is.null(feito[[sq]]) && !is.null(feito[[sq]]$http)) as.integer(feito[[sq]]$http) else -1L
  }, integer(1))
  falta <- tab_ano[http_previo == -1L]
  cat(ano, ": ", nrow(tab_ano), " pedidos, ", length(feito), " em cache, ",
      nrow(falta), " a consultar\n", sep = "")

  arq_jsonl <- file.path(DESTINO, sprintf("respostas_%d.jsonl", ano))
  con <- file(arq_jsonl, open = "at", encoding = "UTF-8")
  t0 <- Sys.time()
  n <- 0L
  if (nrow(falta) > 0) {
    for (i in seq_len(nrow(falta))) {
      sq <- falta$sq_candidato[i]; ue <- falta$sg_ue[i]
      d <- consultar_um(req_base, ano, ue, sq)
      cat(jsonlite::toJSON(d, auto_unbox = TRUE, na = "null"), "\n", file = con, sep = "")
      feito[[sq]] <- d
      n <- n + 1L
      if (n %% 500L == 0L) {
        flush(con)
        taxa <- n / as.numeric(difftime(Sys.time(), t0, units = "secs"))
        cat("  ", ano, ": ", n, "/", nrow(falta), " (", sprintf("%.1f", taxa), "/s)\n", sep = "")
      }
    }
  }
  close(con)

  linhas_saida <- lapply(sort(tab_ano$sq_candidato), function(sq) {
    if (is.null(feito[[sq]])) return(NULL)
    d <- feito[[sq]]
    setNames(lapply(campos_saida, function(campo) {
      v <- d[[campo]]
      if (is.null(v)) NA else v
    }), campos_saida)
  })
  linhas_saida <- linhas_saida[!vapply(linhas_saida, is.null, logical(1))]
  saida <- if (length(linhas_saida)) data.table::rbindlist(linhas_saida, fill = TRUE) else
    data.table::data.table(matrix(character(0), ncol = length(campos_saida),
                                   dimnames = list(NULL, campos_saida)))
  data.table::fwrite(saida, file.path(DESTINO, sprintf("reeleicao_%d.csv", ano)), na = "")

  ok <- sum(vapply(tab_ano$sq_candidato, function(sq) {
    !is.null(feito[[sq]]) && isTRUE(as.integer(feito[[sq]]$http) == 200L)
  }, logical(1)))
  cat(ano, ": concluido, ", ok, " respostas 200 de ", nrow(tab_ano), "\n", sep = "")
}
