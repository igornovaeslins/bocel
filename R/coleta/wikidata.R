#!/usr/bin/env Rscript
# wikidata.R -- coleta no endpoint SPARQL do Wikidata os mandatos de presidente, vice-presidente,
# governador, vice-governador, deputado estadual/distrital e prefeito no Brasil (statements P39 e
# P6), com qualificadores de inicio, fim, sucessao, causa do fim, eleicao e partido.
#
# Estrategia (a mesma do script original): consultas "esqueleto" leves, uma por padrao de
# modelagem, que so trazem os pares (humano, statement, posicao, entidade); so depois vem os
# detalhes em lotes (VALUES) para statements, humanos, entidades e posicoes -- uma consulta unica
# com todos os OPTIONAL estoura o tempo do endpoint. Modelagem observada em amostra (28/08/2026),
# ver o comentario de cada padrao esqueleto, abaixo.
#
# Porte de python/fetch_wikidata.py para R em 21/09/2026 (regra do projeto: coleta primaria que
# entra na publicacao e em R). Troca requests por httr2 (pausa com req_throttle, retentativa com
# req_retry) e json.dump por jsonlite::write_json(auto_unbox = TRUE), que produz a mesma estrutura
# -- lista de objetos com so as chaves presentes em cada binding SPARQL, sem preencher nulo para
# OPTIONAL sem correspondencia -- que R/12_wikidata_mandatos.R ja le hoje (fromJSON, com colunas
# ausentes preenchidas na leitura). O QID de Presidente e de Vice-presidente do Brasil sai da
# mesma busca por wbsearchentities do script original, com o mesmo criterio de escolha (descricao
# que contem um dos termos esperados) e o mesmo QID de reserva -- mas AQUI com retentativa curta
# (o original tentava uma vez so). Achado no porte, em teste (21/09/2026): esse endpoint de busca
# devolveu 429 com frequencia, e sem retentativa a busca cai no QID de reserva do original
# ("Q1478178"), que hoje nao e mais o vice-presidente do Brasil (item sem rotulo, sem statement
# P39 nenhum), zerando o padrao vice_presidente_posicao inteiro (31 linhas na producao de
# 28-29/08/2026, 0 na primeira tentativa de porte sem retentativa). O QID correto e vivo,
# Q2978540, e o mesmo que a producao original usou (confirmado no campo ?pos gravado em
# data_raw/wikidata/esqueleto_vice_presidente_posicao.json) e o mesmo que a busca encontra aqui
# quando tem uma segunda chance.
#
# Fonte: endpoint SPARQL publico do Wikidata, sem credencial.
#   https://query.wikidata.org/sparql
#   https://www.wikidata.org/w/api.php  (wbsearchentities, so para o QID de Presidente/
#   Vice-presidente do Brasil)
#
# Saida, em BOCEL_COLETA_DESTINO (padrao data_raw/wikidata sob BOCEL_ROOT):
#   esqueleto_<padrao>.json   pares (humano, statement, posicao, entidade), um arquivo por padrao
#   humanos.json              rotulos, nome completo, nascimento, morte
#   statements.json           qualificadores dos statements
#   entidades.json            rotulo, codigo IBGE (P1585), ISO (P300), UF
#   posicoes.json             rotulo de cada QID de posicao/cargo
#   manifest.json             contagens por consulta
#
# Variaveis de ambiente:
#   BOCEL_ROOT             raiz do repositorio (padrao ~/bocel)
#   BOCEL_COLETA_DESTINO   pasta de saida (padrao data_raw/wikidata sob BOCEL_ROOT)
#   BOCEL_COLETA_AMOSTRA   limite de humanos (h), os primeiros em ordem alfabetica de QID, entre
#                          os que aparecem nos esqueletos (padrao: todos). Os 16 padroes esqueleto
#                          sao sempre consultados inteiros -- e deles que a amostra tira "os
#                          primeiros" humanos; so os statements, entidades e posicoes ligados a
#                          esses humanos entram nas consultas em lote que seguem.
#
# Uso:      cd ~/bocel && Rscript --vanilla R/coleta/wikidata.R [--refresh]
# Log:      logs/fetch_wikidata.log (ou <destino>/_log_coleta_wikidata.log quando o destino nao e
#           o padrao, para nao misturar log de teste com o log da coleta real)
# Papel ancilar (coleta na API); a construcao do banco e em R/12_wikidata_mandatos.R.

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DESTINO_PADRAO <- file.path(root, "data_raw", "wikidata")
DEST <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = DESTINO_PADRAO)
LOGF <- if (identical(DEST, DESTINO_PADRAO)) {
  file.path(root, "logs", "fetch_wikidata.log")
} else {
  file.path(DEST, "_log_coleta_wikidata.log")
}
dir.create(DEST, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(LOGF), recursive = TRUE, showWarnings = FALSE)

amostra_env <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
AMOSTRA <- if (nzchar(amostra_env)) as.integer(amostra_env) else Inf
if (is.finite(AMOSTRA) && (is.na(AMOSTRA) || AMOSTRA < 1L)) {
  stop("BOCEL_COLETA_AMOSTRA precisa ser um inteiro positivo, recebi '", amostra_env, "'")
}

REFRESH <- "--refresh" %in% commandArgs(trailingOnly = TRUE)
ENDPOINT <- "https://query.wikidata.org/sparql"
UA <- "BOCEL coleta academica"
LOTE <- 200L

log_msg <- function(msg) {
  linha <- sprintf("%s %s", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), msg)
  cat(linha, "\n", sep = "")
  cat(linha, "\n", file = LOGF, append = TRUE, sep = "")
}
log_msg(sprintf("wikidata.R -- destino %s%s", DEST,
                 if (is.finite(AMOSTRA)) sprintf(" (amostra %d)", AMOSTRA) else ""))

## ---------------------------------------------------------------- HTTP: throttle e retentativa
# no maximo 1 requisicao a cada 2s ao endpoint SPARQL (req_throttle) -- mais conservador que a
# pausa so entre lotes do script original (1s) e a pausa so entre esqueletos (2s), porque as duas
# familias de consulta aqui dividem o mesmo limitador. Ate 6 tentativas (mesmo teto do original)
# com espera de 10s vezes o numero da tentativa (10s, 20s, 30s, 40s, 50s) em erro transitorio --
# a mesma progressao do time.sleep(10 * (i + 1)) do original -- e HTTP 429 usa o Retry-After da
# resposta (ou 30s, tambem como no original) no lugar dessa progressao. Um HTTP 4xx que nao seja
# 429 (por exemplo 400, consulta malformada) nao e transitorio e desiste na hora, porque repetir
# a mesma consulta manda de novo o mesmo erro -- diferente do original, que tentava de novo
# mesmo nesse caso; adaptacao documentada aqui, nao um erro de porte.
# Achado no porte, em teste (21/09/2026): o callback "after" do req_retry precisa devolver um
# numero ou NA, nunca NULL -- a primeira versao devolvia NULL para todo erro transitorio que nao
# fosse 429 (por exemplo, um 5xx do endpoint), e o proprio httr2 interrompia a coleta com
# "must return a single number or NA, not NULL" no primeiro erro assim, mesmo tendo tentativas
# sobrando. Corrigido para devolver NA_real_ nesse caso, que faz o req_retry cair no backoff.
sparql_req <- function(query) {
  httr2::request(ENDPOINT) |>
    httr2::req_body_form(query = query, format = "json") |>
    httr2::req_user_agent(UA) |>
    httr2::req_timeout(300) |>
    httr2::req_throttle(rate = 0.5, realm = "wikidata-sparql") |>
    httr2::req_retry(
      max_tries = 6,
      retry_on_failure = TRUE,
      is_transient = function(resp) httr2::resp_status(resp) == 429L || httr2::resp_status(resp) >= 500L,
      after = function(resp) {
        if (httr2::resp_status(resp) != 429L) return(NA_real_)
        ra <- suppressWarnings(as.numeric(httr2::resp_header(resp, "Retry-After")))
        if (is.na(ra)) 30 else ra
      },
      backoff = function(tentativa) 10 * tentativa
    )
}

sparql <- function(query) {
  resp <- tryCatch(httr2::req_perform(sparql_req(query)), error = function(e) e)
  if (inherits(resp, "error") || inherits(resp, "condition")) {
    log_msg(sprintf("SPARQL falhou apos tentativas: %s", substr(conditionMessage(resp), 1, 200)))
    stop("SPARQL falhou apos tentativas", call. = FALSE)
  }
  corpo <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  lapply(corpo$results$bindings, function(b) lapply(b, function(v) v$value))
}

# QID pela API de busca (wbsearchentities), escolhendo a entidade cuja descricao contem o termo
# pedido. O original tentava uma vez so; aqui vai retentativa curta (ate 4 tentativas, 5s vezes a
# tentativa) porque esse endpoint devolveu 429 com frequencia em teste -- ver nota no cabecalho
qid_por_busca <- function(termo, deve_conter) {
  tryCatch({
    resp <- httr2::request("https://www.wikidata.org/w/api.php") |>
      httr2::req_url_query(action = "wbsearchentities", search = termo, language = "pt",
                           uselang = "pt", limit = 10, format = "json") |>
      httr2::req_user_agent(UA) |>
      httr2::req_timeout(30) |>
      httr2::req_throttle(rate = 0.5, realm = "wikidata-busca") |>
      httr2::req_retry(
        max_tries = 4,
        retry_on_failure = TRUE,
        is_transient = function(resp) httr2::resp_status(resp) == 429L || httr2::resp_status(resp) >= 500L,
        backoff = function(tentativa) 5 * tentativa
      ) |>
      httr2::req_perform()
    r <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    busca <- r$search
    if (is.null(busca)) busca <- list()
    for (it in busca) {
      d <- it$description
      d <- tolower(if (is.null(d)) "" else d)
      if (any(vapply(deve_conter, function(k) grepl(k, d, fixed = TRUE), logical(1)))) return(it$id)
    }
    if (length(busca)) busca[[1]]$id else NA_character_
  }, error = function(e) NA_character_)
}

QID_PRES <- qid_por_busca("Presidente do Brasil", c("chefe de estado", "head of state", "cargo", "posição"))
if (is.na(QID_PRES)) QID_PRES <- "Q5176750"
QID_VPRES <- qid_por_busca("Vice-presidente do Brasil", c("cargo", "posição", "vice", "office"))
if (is.na(QID_VPRES)) QID_VPRES <- "Q1478178"

## ---------------------------------------------------------------- consultas: esqueletos e lotes

ESQ <- "SELECT DISTINCT ?h ?s ?pos ?ent WHERE {"
ESQUELETOS <- list(
  presidente_posicao = paste0(ESQ, sprintf(
    " ?h p:P39 ?s . ?s ps:P39 wd:%s . ?h wdt:P31 wd:Q5 . BIND(wd:%s AS ?pos) BIND(wd:Q155 AS ?ent) }",
    QID_PRES, QID_PRES)),
  vice_presidente_posicao = paste0(ESQ, sprintf(
    " ?h p:P39 ?s . ?s ps:P39 wd:%s . ?h wdt:P31 wd:Q5 . BIND(wd:%s AS ?pos) BIND(wd:Q155 AS ?ent) }",
    QID_VPRES, QID_VPRES)),
  governador_posicao = paste0(ESQ,
    " ?pos wdt:P279* wd:Q25921469 . ?pos wdt:P1001 ?ent . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  governador_generico = paste0(ESQ,
    " ?h p:P39 ?s . ?s ps:P39 wd:Q25921469 . ?h wdt:P31 wd:Q5 . BIND(wd:Q25921469 AS ?pos) ",
    "OPTIONAL { ?s pq:P1001 ?e1 } OPTIONAL { ?s pq:P642 ?e2 } OPTIONAL { ?s pq:P768 ?e3 } BIND(COALESCE(?e1,?e2,?e3) AS ?ent) }"),
  governador_p6 = paste0(ESQ,
    " ?ent wdt:P31 wd:Q485258 . ?ent p:P6 ?s . ?s ps:P6 ?h . ?h wdt:P31 wd:Q5 . BIND(wd:Q25921469 AS ?pos) }"),
  # verificacao de 28/08/2026: 'governador(a) da Bahia' (Q53739339) e INSTANCIA (P31) de Q25921469,
  # nao subclasse; 'governador do Ceara' (Q53739345) e subclasse apenas de 'governador' (Q132050);
  # Blairo Maggi usa Q132050 direto.
  governador_instancia = paste0(ESQ,
    " ?pos wdt:P31 wd:Q25921469 . ?pos wdt:P1001 ?ent . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  governador_classe_generica = paste0(ESQ,
    " ?ent wdt:P31 wd:Q485258 . ?pos wdt:P1001 ?ent . ?pos wdt:P279 wd:Q132050 . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  # (qualificador de jurisdicao obrigatorio: sem ele, ?ent ficaria livre e faria produto
  # cartesiano com as 27 UFs)
  governador_generico_q132050 = paste0(ESQ,
    " ?h p:P39 ?s . ?s ps:P39 wd:Q132050 . ?h wdt:P31 wd:Q5 . BIND(wd:Q132050 AS ?pos) ",
    "?s pq:P1001|pq:P642|pq:P768|pq:P131 ?ent . ?ent wdt:P31 wd:Q485258 . }"),
  vice_governador_posicao = paste0(ESQ,
    " ?pos wdt:P279* wd:Q51334165 . ?pos wdt:P1001 ?ent . ?ent wdt:P17 wd:Q155 . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  vice_governador_generico = paste0(ESQ,
    " ?h p:P39 ?s . ?s ps:P39 wd:Q51334165 . ?h wdt:P31 wd:Q5 . ?h wdt:P27 wd:Q155 . BIND(wd:Q51334165 AS ?pos) ",
    "OPTIONAL { ?s pq:P1001 ?e1 } OPTIONAL { ?s pq:P642 ?e2 } OPTIONAL { ?s pq:P768 ?e3 } BIND(COALESCE(?e1,?e2,?e3) AS ?ent) }"),
  deputado_estadual_posicao = paste0(ESQ,
    " ?pos wdt:P279+ wd:Q10265290 . ?pos wdt:P1001 ?ent . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  deputado_estadual_generico = paste0(ESQ,
    " ?h p:P39 ?s . ?s ps:P39 wd:Q10265290 . ?h wdt:P31 wd:Q5 . BIND(wd:Q10265290 AS ?pos) ",
    "OPTIONAL { ?s pq:P1001 ?e1 } OPTIONAL { ?s pq:P768 ?e2 } OPTIONAL { ?s pq:P642 ?e3 } OPTIONAL { ?s pq:P131 ?e4 } BIND(COALESCE(?e1,?e2,?e3,?e4) AS ?ent) }"),
  prefeito_posicao = paste0(ESQ,
    " ?pos wdt:P279 wd:Q30185 . ?pos wdt:P1001 ?ent . ?ent wdt:P17 wd:Q155 . ?h p:P39 ?s . ?s ps:P39 ?pos . ?h wdt:P31 wd:Q5 . }"),
  prefeito_generico = paste0(ESQ,
    " ?h wdt:P27 wd:Q155 . ?h p:P39 ?s . ?s ps:P39 wd:Q30185 . ?h wdt:P31 wd:Q5 . BIND(wd:Q30185 AS ?pos) ",
    "OPTIONAL { ?s pq:P1001 ?e1 } OPTIONAL { ?s pq:P642 ?e2 } OPTIONAL { ?s pq:P131 ?e3 } OPTIONAL { ?s pq:P276 ?e4 } OPTIONAL { ?s pq:P768 ?e5 } BIND(COALESCE(?e1,?e2,?e3,?e4,?e5) AS ?ent) }"),
  prefeito_generico_ent_br = paste0(ESQ,
    " ?h p:P39 ?s . ?s ps:P39 wd:Q30185 . ?h wdt:P31 wd:Q5 . BIND(wd:Q30185 AS ?pos) ",
    "?s pq:P1001 ?ent . ?ent wdt:P17 wd:Q155 . }"),
  prefeito_p6 = paste0(ESQ,
    " ?ent wdt:P31 wd:Q3184121 . ?ent p:P6 ?s . ?s ps:P6 ?h . ?h wdt:P31 wd:Q5 . BIND(wd:Q30185 AS ?pos) }")
)

Q_HUM <- 'SELECT ?h ?rotulo ?rotulo_en ?nome_completo ?dt_nasc ?dt_morte ?sexoLabel WHERE {
  VALUES ?h { %s }
  OPTIONAL { ?h rdfs:label ?rotulo . FILTER(LANG(?rotulo) = "pt") }
  OPTIONAL { ?h rdfs:label ?rotulo_en . FILTER(LANG(?rotulo_en) = "en") }
  OPTIONAL { ?h wdt:P1477 ?nome_completo . }
  OPTIONAL { ?h wdt:P569 ?dt_nasc . }
  OPTIONAL { ?h wdt:P570 ?dt_morte . }
  OPTIONAL { ?h wdt:P21 ?sexo . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "pt,en". } }'

Q_STM <- 'SELECT ?s ?inicio ?fim ?substitui ?substituido_por ?causa_fim ?causa_fimLabel ?eleicao ?eleicaoLabel
  ?grupo ?grupoLabel ?partido ?partidoLabel ?serie WHERE {
  VALUES ?s { %s }
  OPTIONAL { ?s pq:P580 ?inicio . }
  OPTIONAL { ?s pq:P582 ?fim . }
  OPTIONAL { ?s pq:P1365 ?substitui . }
  OPTIONAL { ?s pq:P1366 ?substituido_por . }
  OPTIONAL { ?s pq:P1534 ?causa_fim . }
  OPTIONAL { ?s pq:P2715 ?eleicao . }
  OPTIONAL { ?s pq:P4100 ?grupo . }
  OPTIONAL { ?s pq:P102 ?partido . }
  OPTIONAL { ?s pq:P1545 ?serie . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "pt,en". } }'

Q_ENT <- 'SELECT ?ent ?rotulo ?ibge ?iso ?uf_iso ?tipoLabel WHERE {
  VALUES ?ent { %s }
  OPTIONAL { ?ent rdfs:label ?rotulo . FILTER(LANG(?rotulo) = "pt") }
  OPTIONAL { ?ent wdt:P1585 ?ibge . }
  OPTIONAL { ?ent wdt:P300 ?iso . }
  OPTIONAL { ?ent wdt:P131 ?uf . ?uf wdt:P300 ?uf_iso . }
  OPTIONAL { ?ent wdt:P31 ?tipo . }
  SERVICE wikibase:label { bd:serviceParam wikibase:language "pt,en". } }'

Q_POS <- 'SELECT ?pos ?rotulo WHERE { VALUES ?pos { %s }
  OPTIONAL { ?pos rdfs:label ?rotulo . FILTER(LANG(?rotulo) = "pt") } }'

CHAVES <- c(humanos = "h", statements = "s", entidades = "ent", posicoes = "pos")

# entity/statement URI -> forma curta wd:/wds: aceita em VALUES, ou <URI> completa quando nao e
# entidade nem statement do Wikidata (vetorizada, um QID por elemento de x)
uri <- function(x) {
  x <- gsub("http://www.wikidata.org/entity/statement/", "wds:", x, fixed = TRUE)
  x <- gsub("http://www.wikidata.org/entity/", "wd:", x, fixed = TRUE)
  ifelse(startsWith(x, "wd"), x, paste0("<", x, ">"))
}

escrever_json <- function(rows, path) {
  jsonlite::write_json(rows, path, auto_unbox = TRUE)
}

# consulta em lotes (VALUES) com cache incremental: reaproveita o que ja esta no arquivo e busca
# so os itens que faltam (mesmo comportamento do em_lotes original, inclusive o cache por chave
# em vez de por arquivo inteiro -- diferente do cache "tudo ou nada" dos esqueletos, abaixo)
em_lotes <- function(nome, template, uris, tamanho = LOTE) {
  path <- file.path(DEST, sprintf("%s.json", nome))
  chave <- CHAVES[[nome]]
  rows <- list()
  if (file.exists(path) && !REFRESH) {
    rows <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    existentes <- vapply(rows, function(r) as.character(r[[chave]]), character(1))
    faltam <- sort(setdiff(unique(uris), existentes))
    if (!length(faltam)) {
      log_msg(sprintf("%s: cache (%d linhas)", nome, length(rows)))
      return(rows)
    }
    log_msg(sprintf("%s: cache (%d linhas) + %d itens novos", nome, length(rows), length(faltam)))
    uris <- faltam
  } else {
    uris <- sort(unique(uris))
  }
  n_uris <- length(uris)
  if (n_uris > 0) {
    n_lotes <- ceiling(n_uris / tamanho)
    for (b in seq_len(n_lotes)) {
      ini <- (b - 1L) * tamanho + 1L
      fim <- min(b * tamanho, n_uris)
      lote <- uris[ini:fim]
      rows <- c(rows, sparql(sprintf(template, paste(uri(lote), collapse = " "))))
      if ((b - 1L) %% 10L == 0L) log_msg(sprintf("%s: %d/%d", nome, fim, n_uris))
    }
  }
  escrever_json(rows, path)
  log_msg(sprintf("%s: %d linhas", nome, length(rows)))
  rows
}

## ---------------------------------------------------------------------------------------- main

main <- function() {
  data_coleta <- format(Sys.Date(), "%Y-%m-%d")
  esq <- list()
  manifest_esq <- list()
  for (nome in names(ESQUELETOS)) {
    path <- file.path(DEST, sprintf("esqueleto_%s.json", nome))
    if (file.exists(path) && !REFRESH) {
      rows <- jsonlite::fromJSON(path, simplifyVector = FALSE)
      log_msg(sprintf("%s: cache (%d linhas)", nome, length(rows)))
    } else {
      rows <- sparql(ESQUELETOS[[nome]])
      escrever_json(rows, path)
      log_msg(sprintf("%s: %d linhas", nome, length(rows)))
    }
    manifest_esq[[nome]] <- list(
      linhas = length(rows),
      statements = length(unique(vapply(rows, function(r) r$s, character(1)))),
      humanos = length(unique(vapply(rows, function(r) r$h, character(1))))
    )
    rows <- lapply(rows, function(r) { r$padrao <- nome; r })
    esq <- c(esq, rows)
  }

  # amostra: os esqueletos acima saem sempre inteiros -- e deles que a amostra tira "os primeiros"
  # humanos, em ordem alfabetica de QID, antes das consultas em lote (mesmo desenho do universo
  # completo + amostra no detalhe usado em R/coleta/camara.R)
  hs_todos <- sort(unique(vapply(esq, function(r) r$h, character(1))))
  if (is.finite(AMOSTRA) && AMOSTRA < length(hs_todos)) {
    hs_alvo <- utils::head(hs_todos, AMOSTRA)
    log_msg(sprintf(
      "amostra ativa (BOCEL_COLETA_AMOSTRA=%d): os %d primeiros humanos (h), em ordem alfabetica de QID, entre os %d dos esqueletos completos",
      AMOSTRA, length(hs_alvo), length(hs_todos)))
    esq_uso <- Filter(function(r) r$h %in% hs_alvo, esq)
  } else {
    esq_uso <- esq
  }

  hum <- em_lotes("humanos", Q_HUM, vapply(esq_uso, function(r) r$h, character(1)))
  stm <- em_lotes("statements", Q_STM, vapply(esq_uso, function(r) r$s, character(1)))
  ent_uris <- unlist(lapply(esq_uso, function(r) r$ent), use.names = FALSE)
  ent <- em_lotes("entidades", Q_ENT, ent_uris)
  pos <- em_lotes("posicoes", Q_POS, vapply(esq_uso, function(r) r$pos, character(1)), tamanho = 300L)

  manifest <- list(
    data_coleta = data_coleta,
    esqueletos = manifest_esq,
    humanos = length(unique(vapply(hum, function(r) r$h, character(1)))),
    statements = length(unique(vapply(stm, function(r) r$s, character(1)))),
    entidades = length(unique(vapply(ent, function(r) r$ent, character(1)))),
    posicoes = length(unique(vapply(pos, function(r) r$pos, character(1))))
  )
  jsonlite::write_json(manifest, file.path(DEST, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
  log_msg(paste("concluido:", jsonlite::toJSON(manifest, auto_unbox = TRUE)))
}

main()
