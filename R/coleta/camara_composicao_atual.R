#!/usr/bin/env Rscript
# camara_composicao_atual.R -- guarda a composicao da Camara dos Deputados em exercicio na data
# da coleta.
#
# O historico de cada deputado na API de Dados Abertos nao registra a saida de quem perdeu a vaga
# por retotalizacao (Amapa em 2025, Alagoas e Ceara em 2026): o deputado continua com a situacao
# Exercicio no historico depois que outro tomou posse na vaga. A lista /deputados sem parametro de
# legislatura devolve so quem esta em exercicio hoje, e a comparacao das duas listas (feita em
# R/verifica_legislativo_federal.R) mostra quem saiu sem evento registrado.
#
# Porte de python/fetch_camara_composicao_atual.py para R em 21/09/2026 (regra do projeto: coleta
# primaria que entra na publicacao e em R). Mesma paginacao (100 itens por pagina, ordenado por
# nome ascendente) e o mesmo formato de saida do script anterior; troca requests por httr2
# (com pausa e retentativa) e json.dump por jsonlite.
#
# Fonte: API de Dados Abertos da Camara dos Deputados, endpoint /deputados sem parametro de
# legislatura (devolve quem esta em exercicio na data da chamada), sem credencial.
#   https://dadosabertos.camara.leg.br/api/v2/deputados
#
# Saida, em BOCEL_COLETA_DESTINO (padrao data_raw/camara sob BOCEL_ROOT):
#   composicao_atual_<AAAA-MM-DD>.json  lista completa da coleta do dia (data_coleta + dados)
#   composicao_atual.json               copia identica, sempre a coleta mais recente
#
# Variaveis de ambiente:
#   BOCEL_ROOT             raiz do repositorio (padrao ~/bocel)
#   BOCEL_COLETA_DESTINO   pasta de saida (padrao data_raw/camara sob BOCEL_ROOT)
#   BOCEL_COLETA_AMOSTRA   limite de itens, os primeiros da lista ordenada (padrao: todos)
#
# Execucao: cd ~/bocel && Rscript --vanilla R/coleta/camara_composicao_atual.R

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

ROOT <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DEST <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = file.path(ROOT, "data_raw", "camara"))
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)

amostra_env <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
AMOSTRA <- if (nzchar(amostra_env)) as.integer(amostra_env) else NA_integer_
if (nzchar(amostra_env) && (is.na(AMOSTRA) || AMOSTRA < 1L)) {
  stop("BOCEL_COLETA_AMOSTRA precisa ser um inteiro positivo, recebi '", amostra_env, "'")
}

URL <- "https://dadosabertos.camara.leg.br/api/v2/deputados"

# no maximo 2 requisicoes por segundo ao mesmo servidor (req_throttle) e ate 5 tentativas com
# espera exponencial em erro transitorio -- 429, 5xx -- antes de desistir (req_retry). Identificacao
# so pelo cabecalho, sem credencial.
req_base <- httr2::request(URL)
req_base <- httr2::req_user_agent(req_base, "BOCEL coleta academica")
req_base <- httr2::req_headers(req_base, Accept = "application/json")
req_base <- httr2::req_timeout(req_base, 90)
req_base <- httr2::req_throttle(req_base, rate = 2)
req_base <- httr2::req_retry(req_base, max_tries = 5, backoff = function(tentativa) 2^tentativa)

dados_paginas <- list()
pagina <- 1L
repeat {
  req <- httr2::req_url_query(req_base, pagina = pagina, itens = 100, ordem = "ASC", ordenarPor = "nome")
  resp <- httr2::req_perform(req)
  lote <- httr2::resp_body_json(resp, simplifyVector = TRUE)$dados
  n_lote <- if (is.data.frame(lote)) nrow(lote) else length(lote)
  if (n_lote > 0) dados_paginas[[length(dados_paginas) + 1L]] <- lote
  total_ate_agora <- sum(vapply(dados_paginas, function(d) if (is.data.frame(d)) nrow(d) else length(d), integer(1)))
  if (n_lote < 100 || (!is.na(AMOSTRA) && total_ate_agora >= AMOSTRA)) break
  pagina <- pagina + 1L
}
dados <- do.call(rbind, dados_paginas)
if (!is.na(AMOSTRA)) dados <- dados[seq_len(min(AMOSTRA, nrow(dados))), , drop = FALSE]

hoje <- format(Sys.Date(), "%Y-%m-%d")
arq <- file.path(DEST, sprintf("composicao_atual_%s.json", hoje))
jsonlite::write_json(list(data_coleta = hoje, dados = dados), arq, auto_unbox = TRUE)
invisible(file.copy(arq, file.path(DEST, "composicao_atual.json"), overwrite = TRUE))
cat(sprintf("deputados em exercicio em %s: %d\n", hoje, nrow(dados)))
