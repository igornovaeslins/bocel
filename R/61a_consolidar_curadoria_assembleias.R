# 61a_consolidar_curadoria_assembleias.R — consolida a curadoria da fase 5 (13/09/2026)
#
# As frentes de coleta por UF gravaram em data_raw/assembleias3/<UF>/ os eventos de saida e a composicao final de cada
# legislatura, com URL, trecho literal e bruto em disco. Este script confere cada linha contra o vocabulario, a janela do
# mandato e o proprio bruto (o trecho tem de estar no documento baixado), e grava as linhas aceitas nas tabelas
# versionadas ref/eventos_assembleias_fonte_oficial.csv e ref/composicao_final_assembleias_fonte_oficial.csv, que o R/61
# le. As linhas recusadas vao, com o motivo, para output/verificacao/curadoria_assembleias_recusadas.csv.
# Nao entra na cadeia de reconstrucao, porque o bruto das frentes fica fora do deposito. As tabelas de ref/ entram.
# Execucao: cd ~/bocel && Rscript --vanilla R/61a_consolidar_curadoria_assembleias.R
suppressPackageStartupMessages({ library(data.table); library(stringi); library(xml2) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/61a_consolidar_curadoria_assembleias.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

COL_EV <- c("uf", "ano_eleicao", "id_mandato", "nome_fonte", "evento", "data_evento", "precisao_data", "data_evento_ate",
            "cargo_assumido", "substituto_nome", "url", "trecho", "tipo_fonte", "arquivo_bruto", "confianca", "observacao")
COL_CF <- c("uf", "ano_eleicao", "data_referencia", "id_mandato", "nome_fonte", "situacao", "url", "trecho", "tipo_fonte",
            "arquivo_bruto", "confianca")
EVENTOS <- c("renuncia", "falecimento", "cassacao", "nao_tomou_posse", "retotalizacao", "licenca", "afastamento", "retorno",
             "posse_suplente", "efetivacao_suplente", "fim_regular")
TIPOS <- c("diario_oficial", "portal_casa", "memorial_casa", "wayback_portal_casa", "noticia_casa", "tribunal", "tce",
           "governo_estado", "imprensa")

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA",
              select = c("id_mandato", "cd_cargo", "sg_uf", "ano_eleicao", "mandato_inicio", "mandato_fim"))[cd_cargo %chin% c("7", "8")]

## texto normalizado do bruto, com cache para PDF (extraido por PyMuPDF)
# 13/09/2026: a hifenizacao de fim de linha dos diarios em PDF ("elei-\ncoes") e desfeita antes de normalizar
norm <- function(x) {
  x <- stri_replace_all_regex(x, "(\\p{L})-\\s*\\n\\s*(\\p{L})", "$1$2")
  trimws(stri_replace_all_regex(stri_trans_general(tolower(x), "Latin-ASCII"), "[^a-z0-9]+", " "))
}
cache_txt <- new.env()
# devolve o texto inteiro normalizado e os registros (objeto JSON, linha de CSV ou de texto, linha de tabela HTML)
texto_bruto <- function(uf, arq) {
  if (is.na(arq) || !nzchar(arq)) return(NULL)
  arq <- trimws(arq)
  cands <- unique(c(arq, file.path("data_raw/assembleias3", uf, arq), file.path("data_raw/assembleias3", uf, "brutos", basename(arq))))
  f <- cands[file.exists(cands) & !dir.exists(cands)][1]
  if (is.na(f)) return(NULL)
  if (!is.null(cache_txt[[f]])) return(cache_txt[[f]])
  ext <- tolower(tools::file_ext(f))
  bruto <- tryCatch({
    if (ext == "pdf") {
      out <- system2("python3", c("-c", shQuote("import sys,fitz; d=fitz.open(sys.argv[1]); print('\\n'.join(p.get_text() for p in d))"), shQuote(f)),
                     stdout = TRUE, stderr = FALSE)
      paste(out, collapse = "\n")
    } else {
      raw <- readBin(f, "raw", file.info(f)$size)
      s <- rawToChar(raw[raw != as.raw(0)]); Encoding(s) <- "UTF-8"
      if (!stri_enc_isutf8(s)) s <- stri_encode(s, from = "latin1", to = "UTF-8")
      if (ext == "json") s <- stri_unescape_unicode(s)
      s
    }
  }, error = function(e) NA_character_)
  if (is.na(bruto)) return(NULL)
  html <- ext %in% c("html", "htm", "xhtml") || grepl("<html|<body|<div|<table", substr(bruto, 1, 5000), ignore.case = TRUE)
  corrido <- if (html) { h <- tryCatch(read_html(bruto), error = function(e) NULL); if (!is.null(h)) paste(xml_text(h), bruto) else bruto } else bruto
  sep <- if (ext == "json") "\\}" else if (html) "</tr>|<br\\s*/?>|</p>|</li>" else "\\n"
  reg_ <- unlist(stri_split_regex(bruto, sep))
  if (html) reg_ <- stri_replace_all_regex(reg_, "<[^>]+>", " ")
  out <- list(txt = norm(corrido), registros = norm(reg_))
  assign(f, out, envir = cache_txt)
  out
}
# o trecho confere quando cada segmento dele (separado por reticencias ou por "||" entre brutos) com 15 ou mais caracteres
# esta, contiguo, no texto de um dos brutos, ou, para registro estruturado, tem todas as palavras dentro de um mesmo registro
confere <- function(uf, arq, trecho) {
  arqs <- trimws(unlist(strsplit(arq, ";")))
  bs <- Filter(Negate(is.null), lapply(arqs, function(a) texto_bruto(uf, a)))
  if (!length(bs)) return(NA)
  seg <- norm(unlist(strsplit(trecho, "\\|\\||\\[?\\.\\.\\.\\]?|\u2026|\\[\\s*\\]")))
  seg <- seg[nchar(seg) >= 15L]
  if (!length(seg)) return(NA)
  achou <- function(sg) any(vapply(bs, function(b) {
    if (grepl(sg, b$txt, fixed = TRUE)) return(TRUE)
    tk <- unique(strsplit(sg, " ", fixed = TRUE)[[1]])
    # so registro curto vale como unidade; pagina sem separador de registro nao vira saco de palavras
    cand <- b$registros[nchar(b$registros) <= 2000L & stri_detect_fixed(b$registros, tk[which.max(nchar(tk))])]
    any(vapply(cand, function(r) all(tk %chin% strsplit(r, " ", fixed = TRUE)[[1]]), logical(1)))
  }, logical(1)))
  all(vapply(seg, achou, logical(1)))
}

ler_frente <- function(arq, cols) {
  if (!file.exists(arq)) return(NULL)
  x <- fread(arq, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8", fill = TRUE)
  falta <- setdiff(cols, names(x))
  if (length(falta)) { cat("colunas faltando em", arq, ":", paste(falta, collapse = ","), "\n"); for (k in falta) set(x, j = k, value = NA_character_) }
  x <- x[, ..cols]
  x[, arquivo_frente := arq]
  x
}
ufs <- basename(list.dirs("data_raw/assembleias3", recursive = FALSE))
ev <- rbindlist(lapply(ufs, function(u) ler_frente(file.path("data_raw/assembleias3", u, "eventos.csv"), COL_EV)), fill = TRUE)
cf <- rbindlist(lapply(ufs, function(u) ler_frente(file.path("data_raw/assembleias3", u, "composicao_final.csv"), COL_CF)), fill = TRUE)
cat("linhas lidas: eventos", nrow(ev), "| composicao final", nrow(cf), "\n")

data_ok <- function(d) !is.na(d) & grepl("^\\d{4}-\\d{2}-\\d{2}$", d) & !is.na(suppressWarnings(as.IDate(d)))
checar_comum <- function(x) {
  x[, motivo := NA_character_]
  add <- function(cond, m) x[cond & is.na(motivo), motivo := m]
  add(!grepl("^https?://", x$url), "url_invalida")
  add(grepl("wikipedia\\.org", x$url), "wikipedia_como_fonte")
  add(is.na(x$trecho) | nchar(trimws(x$trecho)) < 15L, "trecho_vazio")
  add(!x$tipo_fonte %chin% TIPOS, "tipo_fonte_fora_do_vocabulario")
  add(!x$confianca %chin% c("alta", "media"), "confianca_fora_do_vocabulario")
  add(x$tipo_fonte %chin% "imprensa" & x$confianca %chin% "alta", "imprensa_com_confianca_alta")
  x[!is.na(id_mandato), `:=`(uf_m = mand$sg_uf[match(id_mandato, mand$id_mandato)], ano_m = mand$ano_eleicao[match(id_mandato, mand$id_mandato)])]
  add(!is.na(x$id_mandato) & is.na(x$uf_m), "id_mandato_inexistente_ou_de_outro_cargo")
  add(!is.na(x$id_mandato) & !is.na(x$uf_m) & (x$uf_m != x$uf | x$ano_m != x$ano_eleicao), "id_mandato_de_outra_uf_ou_eleicao")
  x[, trecho_no_bruto := mapply(confere, uf, arquivo_bruto, trecho)]
  add(x$trecho_no_bruto %in% FALSE, "trecho_nao_encontrado_no_bruto")
  add(is.na(x$trecho_no_bruto), "bruto_ausente_ou_ilegivel")
  x[, c("uf_m", "ano_m") := NULL]
  x
}
recusadas <- list()
if (nrow(ev)) {
  ev <- checar_comum(ev)
  ev[is.na(motivo) & !evento %chin% EVENTOS, motivo := "evento_fora_do_vocabulario"]
  ev[is.na(motivo) & !data_ok(data_evento) & evento != "fim_regular", motivo := "data_evento_invalida"]
  ev[is.na(motivo) & !precisao_data %chin% c("dia", "mes", "ano", "intervalo") & evento != "fim_regular", motivo := "precisao_fora_do_vocabulario"]
  ev[is.na(motivo) & precisao_data %chin% "intervalo" & !data_ok(data_evento_ate), motivo := "intervalo_sem_limite_superior"]
  ev[is.na(motivo) & is.na(id_mandato) & !evento %chin% c("posse_suplente", "efetivacao_suplente"), motivo := "evento_do_titular_sem_id_mandato"]
  jan <- mand[match(ev$id_mandato, id_mandato), .(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
  fora <- !is.na(ev$id_mandato) & data_ok(ev$data_evento) & (as.IDate(ev$data_evento) < jan$mi - 60L | as.IDate(ev$data_evento) > jan$mf + 45L)
  ev[is.na(motivo) & fora, motivo := "data_fora_da_janela_do_mandato"]
  recusadas$eventos <- ev[!is.na(motivo)]
  ev_ok <- unique(ev[is.na(motivo)], by = c("id_mandato", "nome_fonte", "evento", "data_evento", "url"))
} else ev_ok <- ev
if (nrow(cf)) {
  cf <- checar_comum(cf)
  cf[is.na(motivo) & !situacao %chin% c("em_exercicio", "licenciado", "concluiu_mandato"), motivo := "situacao_fora_do_vocabulario"]
  cf[is.na(motivo) & is.na(id_mandato), motivo := "composicao_sem_id_mandato"]
  cf[is.na(motivo) & !data_ok(data_referencia), motivo := "data_referencia_invalida"]
  jan <- mand[match(cf$id_mandato, id_mandato), .(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
  # a composicao prova o fim regular quando retrata a casa no ultimo ano do mandato
  cf[is.na(motivo) & (as.IDate(data_referencia) < jan$mf - 366L | as.IDate(data_referencia) > jan$mf + 45L), motivo := "composicao_fora_do_ultimo_ano"]
  recusadas$composicao <- cf[!is.na(motivo)]
  cf_ok <- unique(cf[is.na(motivo)], by = c("id_mandato", "url"))
} else cf_ok <- cf

rec <- rbindlist(recusadas, fill = TRUE, idcol = "tabela")
dir.create("output/verificacao", showWarnings = FALSE, recursive = TRUE)
fwrite(rec, "output/verificacao/curadoria_assembleias_recusadas.csv", na = "NA", quote = TRUE)
ev_ok[, motivo := NULL]; cf_ok[, motivo := NULL]
setorder(ev_ok, uf, ano_eleicao, id_mandato, data_evento, na.last = TRUE)
setorder(cf_ok, uf, ano_eleicao, id_mandato)
fwrite(ev_ok, "ref/eventos_assembleias_fonte_oficial.csv", na = "", quote = TRUE)
fwrite(cf_ok, "ref/composicao_final_assembleias_fonte_oficial.csv", na = "", quote = TRUE)

cat("aceitas: eventos", nrow(ev_ok), "| composicao", nrow(cf_ok), "| recusadas", nrow(rec), "\n")
if (nrow(rec)) print(rec[, .N, by = .(tabela, uf, motivo)][order(tabela, uf, -N)], nrows = 200)
reg("cas_eventos_aceitos", nrow(ev_ok))
reg("cas_composicao_final_aceita", nrow(cf_ok))
reg("cas_linhas_recusadas", nrow(rec))
