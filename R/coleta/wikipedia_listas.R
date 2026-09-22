#!/usr/bin/env Rscript
# wikipedia_listas.R -- coleta na Wikipedia em portugues as listas de governadores,
# vice-governadores, deputados estaduais/distritais (por legislatura) e prefeitos (por
# municipio), via API MediaWiki (21/09/2026)
#
# Porte para R do antigo python/fetch_wikipedia_listas.py (regra do projeto: coleta primaria
# que entra na publicacao passa a ser R). Mesma fonte e mesma logica de descoberta (existencia
# de titulo para governador/vice, categoria da Wikipedia para deputado/prefeito), mesmos
# caminhos e mesmo conteudo de saida (html + json por pagina, csv de inventario), para os
# produtores que ja leem data_raw/wikipedia/ (R/16_wikipedia_estadual.R,
# R/17_wikipedia_prefeitos.R) seguirem sem mudanca. Muda o motor http (httr2 no lugar de
# requests) e o user agent, agora "BOCEL coleta academica", sem credencial; a pausa entre
# pedidos e o reenvio apos falha, que no .py eram sleeps manuais espalhados pelo codigo,
# viram aqui um unico throttle (req_throttle, ~1 pedido/segundo, todos os pedidos dividem o
# mesmo balde) e uma unica retentativa (req_retry, ate 4 tentativas, espera 3/6/9s, qualquer
# resposta != 200 conta como falha) -- mesma politica de um pedido por vez e desistir depois
# de tentar, com o mecanismo dedicado do httr2 no lugar do sleep manual.
#
# Diferenca deliberada: o .py grava o csv de inventario na ordem em que descobre cada pagina
# (uf por uf, governador/vice, depois deputado, depois prefeito); aqui o inventario sai
# ordenado por titulo, porque a variavel BOCEL_COLETA_AMOSTRA (abaixo) corta nos N primeiros
# dessa lista ordenada -- o corte tem que ser deterministico independente da ordem de
# descoberta por categoria, que a propria API da Wikipedia nao garante estavel entre rodadas.
# Nada le esse csv hoje (grep em R/*.R), entao a mudanca de ordem nao afeta produtor nenhum.
#
# Fonte:   https://pt.wikipedia.org/w/api.php (Wikimedia, texto sob CC BY-SA 4.0)
# Entrada: nenhuma (a lista de UFs e a fonte; os titulos de deputado/prefeito vem da
#          categoria correspondente de cada UF, resolvida na hora)
# Saida:   <destino>/<grupo>/<titulo_slug>.html (texto da pagina, prop=text da API) e .json
#          (metadado: uf, cargo, titulo, revid, arquivo, url) por pagina baixada;
#          <destino>/inventario_paginas.csv (uma linha por alvo: grupo, uf, cargo, titulo,
#          existe, revid) -- grupos "estadual" (governador, vice-governador, deputado
#          estadual/distrital), "prefeitos" (prefeito) e "geral" (as duas listas nacionais).
#          destino padrao: data_raw/wikipedia sob BOCEL_ROOT.
# Execucao: cd ~/bocel && Rscript --vanilla R/coleta/wikipedia_listas.R
#          BOCEL_COLETA_DESTINO=<caminho>  grava em outro destino em vez do data_raw/ padrao
#                                           (usado no teste de porte; muda tambem o log, que
#                                           vai para <destino>/_coleta.log em vez de
#                                           logs/wikipedia_listas.log sob BOCEL_ROOT)
#          BOCEL_COLETA_AMOSTRA=<n>        limita aos n primeiros alvos da lista ordenada por
#                                           titulo, e so baixa esses (usado no teste de porte)

suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(stringi)
})

ROOT    <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
DESTINO_ENV <- Sys.getenv("BOCEL_COLETA_DESTINO", unset = "")
DESTINO <- if (nzchar(DESTINO_ENV)) DESTINO_ENV else file.path(ROOT, "data_raw", "wikipedia")
dir.create(DESTINO, recursive = TRUE, showWarnings = FALSE)

API <- "https://pt.wikipedia.org/w/api.php"
UA  <- "BOCEL coleta academica"

LOG_PATH <- if (nzchar(DESTINO_ENV)) file.path(DESTINO, "_coleta.log") else file.path(ROOT, "logs", "wikipedia_listas.log")
dir.create(dirname(LOG_PATH), recursive = TRUE, showWarnings = FALSE)
log_msg <- function(m) { cat(m, "\n"); cat(m, "\n", file = LOG_PATH, append = TRUE) }

# uf -> nome por extenso e uf -> preposicao para montar o titulo da pagina (espelha os
# dicionarios UFS/PREP do .py; precisam ficar acentuados, sao titulo literal da Wikipedia)
UFS <- c(AC = "Acre", AL = "Alagoas", AP = "Amapá", AM = "Amazonas", BA = "Bahia",
         CE = "Ceará", DF = "Distrito Federal", ES = "Espírito Santo", GO = "Goiás",
         MA = "Maranhão", MT = "Mato Grosso", MS = "Mato Grosso do Sul", MG = "Minas Gerais",
         PA = "Pará", PB = "Paraíba", PR = "Paraná", PE = "Pernambuco",
         PI = "Piauí", RJ = "Rio de Janeiro", RN = "Rio Grande do Norte",
         RS = "Rio Grande do Sul", RO = "Rondônia", RR = "Roraima", SC = "Santa Catarina",
         SP = "São Paulo", SE = "Sergipe", TO = "Tocantins")
PREP <- c(AC = "do", AL = "de", AP = "do", AM = "do", BA = "da", CE = "do", DF = "do",
          ES = "do", GO = "de", MA = "do", MT = "de", MS = "de", MG = "de", PA = "do",
          PB = "da", PR = "do", PE = "de", PI = "do", RJ = "do", RN = "do", RS = "do",
          RO = "de", RR = "de", SC = "de", SP = "de", SE = "de", TO = "do")

# slug do titulo para nome de arquivo: decompoe unicode (NFKD, o que separa "1.ª" em "1.a" com
# acento combinante), remove os acentos combinantes, descarta o que ainda nao for ascii, troca
# run de nao-alfanumerico por "_", tira "_" do inicio/fim, corta em 150 -- espelha slug() do
# .py (unicodedata.normalize("NFKD", t).encode("ascii","ignore")); conferido byte a byte contra
# os 1225 nomes de arquivo ja existentes em data_raw/wikipedia (nenhuma divergencia)
fslug <- function(t) {
  t <- stri_trans_nfkd(t)
  t <- stri_replace_all_regex(t, "\\p{Mn}", "")
  t <- iconv(t, from = "UTF-8", to = "ASCII//IGNORE", sub = "")
  t <- gsub("[^A-Za-z0-9]+", "_", t)
  t <- gsub("^_+|_+$", "", t)
  substr(t, 1, 150)
}

# pedido-base da API: 1 por segundo (todos os pedidos dividem o mesmo balde, "wikipedia_listas"),
# ate 4 tentativas com espera de 3/6/9s, qualquer resposta != 200 ou falha de rede conta como
# transitoria e repete -- espelha get() do .py (tries=4, time.sleep(3*(t+1)), so aceita status
# 200). Devolve a lista json (parse ja feito) ou NULL quando as tentativas se esgotam, igual ao
# get() do .py devolver None.
api_get <- function(params) {
  params <- c(params, format = "json", formatversion = 2)
  req <- request(API) |>
    req_user_agent(UA) |>
    req_timeout(60) |>
    req_throttle(capacity = 1, fill_time_s = 1, realm = "wikipedia_listas") |>
    req_retry(max_tries = 4, retry_on_failure = TRUE,
              backoff = function(tentativas) 3 * tentativas,
              is_transient = function(resp) resp_status(resp) != 200L)
  req <- req_url_query(req, !!!params)
  resp <- tryCatch(req_perform(req), error = function(e) NULL)
  if (is.null(resp)) return(NULL)
  tryCatch(resp_body_json(resp, simplifyVector = FALSE), error = function(e) NULL)
}

# titulos (ns 0) de uma categoria, recursivo em subcategoria (ns 14) ate profundidade 2 --
# espelha membros() do .py
membros <- function(cat, profundidade = 0) {
  titulos <- character(0)
  cont <- list()
  repeat {
    params <- c(list(action = "query", list = "categorymembers", cmtitle = cat,
                     cmlimit = 500, cmtype = "page|subcat"), cont)
    j <- api_get(params)
    if (is.null(j)) break
    for (m in j$query$categorymembers) {
      if (m$ns == 0) {
        titulos <- c(titulos, m$title)
      } else if (m$ns == 14 && profundidade < 2) {
        titulos <- c(titulos, membros(m$title, profundidade + 1))
      }
    }
    if (!is.null(j$continue)) cont <- j$continue else break
  }
  titulos
}

# titulo final apos redirecionamento, ou NA se a pagina nao existe -- espelha existe() do .py
existe <- function(titulo) {
  j <- api_get(list(action = "query", titles = titulo, redirects = 1))
  if (is.null(j)) return(NA_character_)
  p <- j$query$pages[[1]]
  if (!is.null(p$missing)) return(NA_character_)
  p$title
}

# baixa o texto da pagina (prop=text|revid) e grava <grupo>/<slug>.html + .json; usa o cache em
# disco se o par ja existe (retomada); devolve a lista de metadado ou NULL se a coleta falhou --
# espelha baixar() do .py
baixar <- function(titulo, grupo, meta) {
  d <- file.path(DESTINO, grupo)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  base <- file.path(d, fslug(titulo))
  arq_json <- paste0(base, ".json")
  arq_html <- paste0(base, ".html")
  if (file.exists(arq_json)) return(fromJSON(arq_json, simplifyVector = TRUE))

  j <- api_get(list(action = "parse", page = titulo, prop = "text|revid", redirects = 1))
  if (is.null(j) || is.null(j$parse)) return(NULL)

  texto <- j$parse$text
  con <- file(arq_html, open = "wb")
  writeBin(charToRaw(enc2utf8(texto)), con)
  close(con)

  titulo_final <- j$parse$title
  info <- c(meta, list(
    titulo = titulo_final,
    revid = as.integer(j$parse$revid),
    arquivo = normalizePath(arq_html, mustWork = FALSE),
    url = paste0("https://pt.wikipedia.org/wiki/", gsub(" ", "_", titulo_final, fixed = TRUE))
  ))
  cat(toJSON(info, auto_unbox = TRUE), file = arq_json)
  info
}

# =============================================================================
# 1. descoberta: monta a lista de alvos inteira ANTES de baixar qualquer pagina, para o corte
# por BOCEL_COLETA_AMOSTRA valer sobre a lista ordenada por titulo, nao sobre a ordem de
# descoberta por UF/categoria
# =============================================================================
cat("descobrindo alvos (governador/vice por existencia de titulo, deputado/prefeito por categoria)...\n")
alvos <- list()

# 1a. governador e vice-governador: por uf, existe() resolve o titulo final (com redirecionamento)
for (uf in names(UFS)) {
  nome <- UFS[[uf]]
  for (cargo_pref in list(c("GOVERNADOR", "Lista de governadores"),
                          c("VICE-GOVERNADOR", "Lista de vice-governadores"))) {
    cargo <- cargo_pref[1]; pref <- cargo_pref[2]
    t <- sprintf("%s %s %s", pref, PREP[[uf]], nome)
    tt <- existe(t)
    if (is.na(tt) && uf == "DF") tt <- existe(sprintf("%s do Distrito Federal (Brasil)", pref))
    alvos[[length(alvos) + 1L]] <- data.frame(
      grupo = "estadual", uf = uf, cargo = cargo,
      titulo_tentativa = t, titulo_resolvido = tt, stringsAsFactors = FALSE)
  }
}

# 1b. deputado estadual/distrital: titulos vem da categoria por uf, com prefixo "lista de deputados"
for (uf in names(UFS)) {
  nome <- UFS[[uf]]
  catg <- sprintf("Categoria:Listas de deputados estaduais %s %s", PREP[[uf]], nome)
  if (uf == "DF") catg <- "Categoria:Listas de deputados distritais do Distrito Federal (Brasil)"
  tits <- membros(catg)
  tits <- tits[startsWith(tolower(tits), "lista de deputados")]
  cargo <- if (uf == "DF") "DEPUTADO DISTRITAL" else "DEPUTADO ESTADUAL"
  log_msg(sprintf("%s deputados: %d paginas na categoria", uf, length(tits)))
  if (length(tits) > 0) {
    alvos[[length(alvos) + 1L]] <- data.frame(
      grupo = "estadual", uf = uf, cargo = cargo,
      titulo_tentativa = tits, titulo_resolvido = tits, stringsAsFactors = FALSE)
  }
}

# 1c. prefeito: titulos vem da categoria por uf, contendo "prefeit"
for (uf in names(UFS)) {
  nome <- UFS[[uf]]
  catg <- sprintf("Categoria:Listas de prefeitos de municípios %s %s", PREP[[uf]], nome)
  if (uf == "ES") catg <- "Categoria:Listas de prefeitos de municípios do Espírito Santo (estado)"
  tits <- membros(catg)
  tits <- tits[grepl("prefeit", tits, ignore.case = TRUE)]
  log_msg(sprintf("%s prefeitos: %d paginas na categoria", uf, length(tits)))
  if (length(tits) > 0) {
    alvos[[length(alvos) + 1L]] <- data.frame(
      grupo = "prefeitos", uf = uf, cargo = "PREFEITO",
      titulo_tentativa = tits, titulo_resolvido = tits, stringsAsFactors = FALSE)
  }
}

# 1d. geral: as duas listas nacionais, sem checagem de existencia previa (o .py tambem baixa direto)
alvos[[length(alvos) + 1L]] <- data.frame(
  grupo = "geral", uf = "BR", cargo = "GERAL",
  titulo_tentativa = c("Lista de prefeitos das capitais do Brasil",
                       "Lista de governadores das unidades federativas do Brasil"),
  titulo_resolvido = c("Lista de prefeitos das capitais do Brasil",
                       "Lista de governadores das unidades federativas do Brasil"),
  stringsAsFactors = FALSE)

alvos <- do.call(rbind, alvos)
cat(sprintf("alvos descobertos: %d\n", nrow(alvos)))

# =============================================================================
# 2. ordena por titulo (radix: mesma ordem em qualquer maquina/locale) e corta em
# BOCEL_COLETA_AMOSTRA quando definida
# =============================================================================
chave_ordem <- ifelse(!is.na(alvos$titulo_resolvido), alvos$titulo_resolvido, alvos$titulo_tentativa)
alvos <- alvos[order(chave_ordem, method = "radix"), ]

amostra_env <- Sys.getenv("BOCEL_COLETA_AMOSTRA", unset = "")
if (nzchar(amostra_env)) {
  n <- as.integer(amostra_env)
  alvos <- head(alvos, n)
  log_msg(sprintf("amostra limitada aos %d primeiros alvos da lista ordenada por titulo (BOCEL_COLETA_AMOSTRA)", nrow(alvos)))
}

# =============================================================================
# 3. baixa cada alvo (ou le do cache em disco) e monta o inventario
# =============================================================================
inv <- vector("list", nrow(alvos))
for (i in seq_len(nrow(alvos))) {
  a <- alvos[i, ]
  info <- NULL
  if (!is.na(a$titulo_resolvido)) {
    info <- baixar(a$titulo_resolvido, a$grupo, list(uf = a$uf, cargo = a$cargo))
  }
  if (a$cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR")) {
    titulo_log <- if (!is.na(a$titulo_resolvido)) a$titulo_resolvido else a$titulo_tentativa
    log_msg(sprintf("%s %s: %s -> %s", a$uf, a$cargo, titulo_log, if (!is.null(info)) "ok" else "ausente"))
  }
  titulo_final <- if (!is.na(a$titulo_resolvido)) a$titulo_resolvido else a$titulo_tentativa
  inv[[i]] <- data.frame(
    grupo = a$grupo, uf = a$uf, cargo = a$cargo, titulo = titulo_final,
    existe = !is.null(info),
    revid = if (!is.null(info)) as.character(info$revid) else "",
    stringsAsFactors = FALSE)
}
inv <- do.call(rbind, inv)
write.csv(inv, file.path(DESTINO, "inventario_paginas.csv"), row.names = FALSE)

log_msg(sprintf("wikipedia_listas: concluido, %d paginas inventariadas, %d baixadas",
               nrow(inv), sum(inv$existe)))
