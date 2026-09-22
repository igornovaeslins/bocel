#!/usr/bin/env Rscript
# 60a_conferir_fontes_executivos.R -- confere, contra o bruto baixado por R/coleta/fontes_executivos.R, se o trecho
# citado por cada fonte aparece de fato no documento (21/09/2026). Duas frentes:
#   (a) as linhas ja curadas de ref/eventos_governos_fonte_oficial.csv e ref/eventos_presidencia_fonte_oficial.csv,
#       so para conferencia (nao decide entrada, que ja esta na tabela);
#   (b) ref/candidatos_fontes_executivos.csv, cuja confirmacao aqui e o que autoriza a integracao (R/60b).
#
# Entrada:  data_raw/fontes_executivos/manifesto.csv e os arquivos baixados, ref/eventos_governos_fonte_oficial.csv,
#           ref/eventos_presidencia_fonte_oficial.csv, ref/candidatos_fontes_executivos.csv
# Saida:    output/verificacao/fontes_executivos_conferencia.csv (uma linha por par url/trecho conferido)
# Execucao: cd ~/bocel && Rscript --vanilla R/60a_conferir_fontes_executivos.R
suppressPackageStartupMessages({ library(data.table); library(xml2); library(pdftools); library(stringi); library(tools) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/60a_conferir_fontes_executivos.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
le <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8") else NULL

DEST <- "data_raw/fontes_executivos"
manifesto <- le(file.path(DEST, "manifesto.csv"))
stopifnot(!is.null(manifesto))
# indice url -> linha por lista nomeada (nao por manifesto[manifesto$url == x], que o data.table resolve errado: a
# coluna "url" do proprio manifesto sombreia a variavel de mesmo nome dentro do "i" do "[.data.table]")
manifesto_idx <- setNames(seq_len(nrow(manifesto)), manifesto$url)

## ---------------------------------------------------------------- normalizacao e extracao de texto
norm <- function(s) {
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- tolower(s)
  s <- gsub("[[:space:]]+", " ", s)
  trimws(s)
}

# tipo do arquivo pelos primeiros bytes, nao pela extensao do nome (a URL de alguns diarios oficiais serve o PDF por
# um endpoint .aspx, sem extensao no caminho)
detectar_tipo <- function(caminho) {
  con <- file(caminho, "rb"); on.exit(close(con))
  cab <- readBin(con, "raw", 8)
  if (length(cab) >= 4 && rawToChar(cab[1:4]) == "%PDF") return("pdf")
  "html_ou_texto"
}

texto_cache <- new.env(parent = emptyenv())
extrair_texto <- function(arquivo) {
  if (is.na(arquivo)) return(NA_character_)
  if (exists(arquivo, envir = texto_cache)) return(get(arquivo, envir = texto_cache))
  caminho <- file.path(DEST, arquivo)
  txt <- if (!file.exists(caminho)) NA_character_ else {
    tryCatch({
      tipo <- detectar_tipo(caminho)
      if (tipo == "pdf") {
        paste(suppressMessages(pdftools::pdf_text(caminho)), collapse = " ")
      } else {
        doc <- xml2::read_html(caminho, encoding = "UTF-8")
        xml2::xml_text(doc)
      }
    }, error = function(e) NA_character_)
  }
  txt_norm <- if (is.na(txt)) NA_character_ else norm(txt)
  assign(arquivo, txt_norm, envir = texto_cache)
  txt_norm
}

# confere um par url/trecho: devolve exato, parcial (metade inicial do trecho normalizado aparece) ou nenhum;
# NA quando a URL nao baixou (sem como conferir)
conferir_par <- function(u_fonte, trecho) {
  if (is.na(u_fonte) || !nzchar(trimws(u_fonte)) || is.na(trecho) || !nzchar(trimws(trecho))) {
    return(list(status_download = NA_character_, arquivo = NA_character_, resultado = NA_character_))
  }
  i_m <- unname(manifesto_idx[u_fonte])
  if (is.na(i_m)) {
    return(list(status_download = "nao_baixado", arquivo = NA_character_, resultado = "sem_arquivo"))
  }
  m <- manifesto[i_m]
  if (is.na(m$status) || m$status != "ok" || is.na(m$arquivo)) {
    return(list(status_download = m$status, arquivo = NA_character_, resultado = "sem_arquivo"))
  }
  doc <- extrair_texto(m$arquivo)
  if (is.na(doc)) return(list(status_download = "ok", arquivo = m$arquivo, resultado = "sem_texto_extraido"))
  # o trecho curado costuma unir dois pedacos do documento com "...", que nunca aparecem como um bloco continuo;
  # confere cada pedaco (com pelo menos 15 caracteres depois de normalizado) como uma substring separada
  pedacos <- trimws(strsplit(trecho, "\\.\\.\\.")[[1]])
  pedacos <- pedacos[nchar(pedacos) > 0]
  pn <- norm(pedacos)
  pn <- pn[nchar(pn) >= 15]
  if (!length(pn)) return(list(status_download = "ok", arquivo = m$arquivo, resultado = "nenhum"))
  achado <- vapply(pn, function(p) grepl(p, doc, fixed = TRUE), logical(1))
  if (all(achado)) return(list(status_download = "ok", arquivo = m$arquivo, resultado = "exato"))
  if (any(achado)) return(list(status_download = "ok", arquivo = m$arquivo, resultado = "parcial"))
  list(status_download = "ok", arquivo = m$arquivo, resultado = "nenhum")
}

## ---------------------------------------------------------------- (a) linhas ja curadas, so informativo
linhas_saida <- list()
for (f in c("ref/eventos_governos_fonte_oficial.csv", "ref/eventos_presidencia_fonte_oficial.csv")) {
  x <- le(f); if (is.null(x)) next
  origem <- if (grepl("presidencia", f)) "curada_presidencia" else "curada_governos"
  for (i in seq_len(nrow(x))) {
    for (slot in 1:2) {
      url <- if (slot == 1) x$fonte_1[i] else x$fonte_2[i]
      trecho <- if (slot == 1) x$trecho_1[i] else x$trecho_2[i]
      if (is.na(url) || !nzchar(trimws(url))) next
      r <- conferir_par(url, trecho)
      linhas_saida[[length(linhas_saida) + 1L]] <- data.table(
        origem = origem, tabela = basename(f), id_mandato = x$id_mandato[i], slot = slot,
        url = url, trecho = trecho, arquivo = r$arquivo, status_download = r$status_download, resultado = r$resultado)
    }
  }
}

## ---------------------------------------------------------------- (b) candidatos: aqui o resultado e o portao de entrada
cand <- le("ref/candidatos_fontes_executivos.csv")
if (!is.null(cand)) {
  for (i in seq_len(nrow(cand))) {
    for (slot in 1:2) {
      url <- if (slot == 1) cand$url[i] else cand$url_2[i]
      trecho <- if (slot == 1) cand$trecho[i] else cand$trecho_2[i]
      if (is.na(url) || !nzchar(trimws(url))) next
      r <- conferir_par(url, trecho)
      linhas_saida[[length(linhas_saida) + 1L]] <- data.table(
        origem = "candidato", tabela = cand$tabela[i], id_mandato = cand$id_mandato[i], slot = slot,
        url = url, trecho = trecho, arquivo = r$arquivo, status_download = r$status_download, resultado = r$resultado)
    }
  }
}

saida <- rbindlist(linhas_saida, fill = TRUE)
fwrite(saida, "output/verificacao/fontes_executivos_conferencia.csv", na = "NA", quote = TRUE)

## ---------------------------------------------------------------- registro
for (org in unique(saida$origem)) {
  y <- saida[origem == org]
  k <- org
  reg(sprintf("fex_conf_%s_pares", k), nrow(y))
  reg(sprintf("fex_conf_%s_exato", k), y[resultado %chin% "exato", .N])
  reg(sprintf("fex_conf_%s_parcial", k), y[resultado %chin% "parcial", .N])
  reg(sprintf("fex_conf_%s_sem_confirmacao", k), y[resultado %chin% c("nenhum", "sem_texto_extraido"), .N])
  reg(sprintf("fex_conf_%s_sem_arquivo", k), y[resultado %chin% "sem_arquivo" | is.na(resultado), .N])
}
# candidato confirmado (para a integracao): pelo menos um dos dois pares (slot 1 ou 2) da mesma linha bateu exato
if (!is.null(cand)) {
  conf_cand <- saida[origem == "candidato", .(tem_exato = any(resultado %chin% "exato")), by = .(tabela, id_mandato)]
  reg("fex_candidatos_linhas_com_algum_slot", nrow(conf_cand))
  reg("fex_candidatos_confirmados_mecanicamente", conf_cand[tem_exato == TRUE, .N])
  reg("fex_candidatos_nao_confirmados", conf_cand[tem_exato == FALSE, .N])
  fwrite(conf_cand, "output/verificacao/candidatos_fontes_executivos_confirmados.csv", na = "NA", quote = TRUE)
}
print(saida[, .N, by = .(origem, resultado)][order(origem, resultado)])
