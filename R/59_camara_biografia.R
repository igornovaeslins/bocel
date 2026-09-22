# 59_camara_biografia.R — eventos de mandato na biografia oficial dos deputados federais (13/09/2026)
#
# A pagina de biografia da Camara (camara.leg.br/deputados/<id>/biografia), montada a partir do assentamento
# individual do deputado, registra em secoes padronizadas os mandatos com data de posse, as licencas, as renuncias,
# as perdas de mandato, as aposentadorias e afastamentos, as suplencias e a data de falecimento. A API de Dados
# Abertos entrega a legislatura 51 (1999-2003) sem eventos de exercicio, e a biografia e a fonte oficial desse
# periodo; nas legislaturas seguintes ela confere o que a API registra.
#
# Entrada:  data_raw/camara/biografia/<id>.html (R/coleta/camara_biografia.R)
# Saida:    data/camara_biografia_eventos.csv (um evento por linha, com o trecho da pagina)
#           data/camara_biografia_posses.csv (posse por deputado e legislatura)
# Execucao: cd ~/bocel && Rscript --vanilla R/59_camara_biografia.R
set.seed(20260913)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/59_camara_biografia.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

MESES <- c(janeiro = 1, fevereiro = 2, marco = 3, abril = 4, maio = 5, junho = 6, julho = 7, agosto = 8,
           setembro = 9, outubro = 10, novembro = 11, dezembro = 12)
sem_acento <- function(x) stri_trans_general(x, "Latin-ASCII")
# todas as datas por extenso de um trecho, na ordem em que aparecem ("1º a 15 de março de 2007" vira duas datas)
datas_extenso <- function(txt) {
  t <- tolower(sem_acento(txt))
  t <- gsub("1o\\b|1º", "1", t)
  t <- gsub("de(\\d{4})", "de \\1", t)
  out <- character()
  # intervalo "de 1 a 15 de marco de 2007"
  for (m in regmatches(t, gregexpr("\\b(\\d{1,2}) a (\\d{1,2}) de ([a-z]+) de (\\d{4})", t))[[1]]) {
    p <- regmatches(m, regexec("(\\d{1,2}) a (\\d{1,2}) de ([a-z]+) de (\\d{4})", m))[[1]]
    if (!is.na(MESES[p[4]])) out <- c(out, sprintf("%s-%02d-%02d", p[5], MESES[p[4]], as.integer(p[2])),
                                        sprintf("%s-%02d-%02d", p[5], MESES[p[4]], as.integer(p[3])))
    t <- sub(m, " ", t, fixed = TRUE)
  }
  for (m in regmatches(t, gregexpr("\\b(\\d{1,2}) (de )?([a-z]+) de (\\d{4})", t))[[1]]) {
    p <- regmatches(m, regexec("(\\d{1,2}) (de )?([a-z]+) de (\\d{4})", m))[[1]]
    if (!is.na(MESES[p[4]])) out <- c(out, sprintf("%s-%02d-%02d", p[5], MESES[p[4]], as.integer(p[2])))
    t <- sub(m, " ", t, fixed = TRUE)
  }
  # so mes e ano ("em maio de 2016"): data com precisao de mes, gravada como AAAA-MM
  for (m in regmatches(t, gregexpr("\\b(em|de|a|desde) ([a-z]+) de (\\d{4})", t))[[1]]) {
    p <- regmatches(m, regexec("(em|de|a|desde) ([a-z]+) de (\\d{4})", m))[[1]]
    if (!is.na(MESES[p[3]])) out <- c(out, sprintf("%s-%02d", p[4], MESES[p[3]]))
  }
  for (m in regmatches(t, gregexpr("\\b(\\d{1,2})/(\\d{1,2})/(\\d{4})", t))[[1]]) {
    p <- as.integer(strsplit(m, "/")[[1]]); out <- c(out, sprintf("%04d-%02d-%02d", p[3], p[2], p[1]))
  }
  out
}

arqs <- list.files("data_raw/camara/biografia", pattern = "^\\d+\\.html$", full.names = TRUE)
stopifnot(length(arqs) > 0)
SECOES <- c("Mandatos (na Câmara dos Deputados):", "Licenças:", "Renúncias:", "Perdas de Mandato:", "Afastamentos:",
            "Suplências e Efetivações:", "Data de falecimento:")
todas_secoes <- function(l) grepl(":$", l) & nchar(l) < 60

eventos <- list(); posses <- list()
for (f in arqs) {
  id <- sub("\\.html$", "", basename(f))
  h <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  h <- gsub("(?s)<script.*?</script>|<style.*?</style>", "", h, perl = TRUE)
  txt <- gsub("<[^>]+>", "\n", h)
  txt <- stri_replace_all_fixed(txt, c("&nbsp;", "&amp;", "&#39;", "&quot;"), c(" ", "&", "'", "\""), vectorize_all = FALSE)
  l <- trimws(strsplit(txt, "\n")[[1]]); l <- l[nzchar(l)]
  # o rodape da pagina (formulario de erro, Fale Conosco, endereco) fecha a ultima secao
  ini_bio <- which(l == "Mandatos (na Câmara dos Deputados):")[1]
  rod <- grep("comunicar erro", l, ignore.case = TRUE)
  rod <- rod[!is.na(ini_bio) & rod > ini_bio]
  if (length(rod)) {
    l[rod[1]] <- sub("(?i)\\s*comunicar erro.*$", "", l[rod[1]], perl = TRUE)
    l <- c(l[seq_len(rod[1])], "Rodapé:")
  }
  cab <- which(todas_secoes(l))
  bloco <- function(nome) {
    i <- which(l == nome); if (!length(i)) return(NA_character_)
    prox <- cab[cab > i[1]]; fim <- if (length(prox)) prox[1] - 1L else min(length(l), i[1] + 3L)
    paste(l[(i[1] + 1L):max(i[1] + 1L, fim)], collapse = " ")
  }
  url <- sprintf("https://www.camara.leg.br/deputados/%s/biografia", id)
  # posse por legislatura: "Deputado(a) Federal - 1999-2003, SP, PSDB, Dt. Posse: 01/02/1999"
  mb <- bloco("Mandatos (na Câmara dos Deputados):")
  if (!is.na(mb)) {
    for (m in regmatches(mb, gregexpr("Federal[^;]*?(\\d{4})-(\\d{4}),\\s*([A-Z]{2})[^;]*?Dt\\. Posse: (\\d{2}/\\d{2}/\\d{4})", mb))[[1]]) {
      p <- regmatches(m, regexec("(\\d{4})-(\\d{4}),\\s*([A-Z]{2}).*Dt\\. Posse: (\\d{2})/(\\d{2})/(\\d{4})", m))[[1]]
      posses[[length(posses) + 1L]] <- data.table(id_deputado_camara = id, leg_inicio = as.integer(p[2]), sg_uf = p[4],
                                                  data_posse = sprintf("%s-%s-%s", p[7], p[6], p[5]), url = url)
    }
  }
  # eventos: cada frase da secao, com a legislatura citada e as datas
  for (sec in c("Licenças:", "Renúncias:", "Perdas de Mandato:", "Afastamentos:", "Suplências e Efetivações:")) {
    b <- bloco(sec); if (is.na(b)) next
    frases <- trimws(unlist(strsplit(b, "(?<=\\.)\\s+(?=[A-Z0-9])", perl = TRUE)))
    frases <- frases[nchar(frases) > 10]
    leg_corrente <- NA_integer_
    for (fr in frases) {
      lg <- regmatches(fr, regexec("[Ll]egislatura (\\d{4})-(\\d{4})", fr))[[1]]
      if (length(lg)) leg_corrente <- as.integer(lg[2])
      ds <- datas_extenso(fr)
      tipo <- fcase(
        sec == "Renúncias:" & grepl("como Suplente", fr), "renuncia_suplente",
        sec == "Renúncias:", "renuncia",
        sec == "Perdas de Mandato:", "perda_mandato",
        grepl("aposentadoria", fr, ignore.case = TRUE), "aposentadoria",
        sec == "Afastamentos:", "afastamento",
        grepl("^Reassumiu|Reassumiu em", fr), "licenca_com_reassuncao",
        sec == "Licenças:", "licenca",
        grepl("efetivad", fr, ignore.case = TRUE), "efetivacao",
        sec == "Suplências e Efetivações:", "suplencia",
        default = "outro")
      eventos[[length(eventos) + 1L]] <- data.table(id_deputado_camara = id, secao = sub(":$", "", sec), leg_inicio = leg_corrente,
                                                    tipo = tipo, datas = paste(ds, collapse = ";"), trecho = substr(fr, 1, 400), url = url)
    }
  }
  df <- bloco("Data de falecimento:")
  if (!is.na(df) && grepl("^\\d{2}/\\d{2}/\\d{4}", df)) {
    p <- as.integer(strsplit(substr(df, 1, 10), "/")[[1]])
    eventos[[length(eventos) + 1L]] <- data.table(id_deputado_camara = id, secao = "Data de falecimento", leg_inicio = NA_integer_,
                                                  tipo = "falecimento", datas = sprintf("%04d-%02d-%02d", p[3], p[2], p[1]),
                                                  trecho = substr(df, 1, 40), url = url)
  }
}
ev <- rbindlist(eventos); ps <- unique(rbindlist(posses))
ev[, ano_eleicao := leg_inicio - 1L]; ps[, ano_eleicao := leg_inicio - 1L]
fwrite(ev, "data/camara_biografia_eventos.csv", na = "NA", quote = TRUE)
fwrite(ps, "data/camara_biografia_posses.csv", na = "NA", quote = TRUE)
reg("cbio_paginas_lidas", length(arqs))
reg("cbio_eventos", nrow(ev))
reg("cbio_eventos_sem_data", ev[!nzchar(datas) & tipo != "outro", .N])
reg("cbio_eventos_sem_legislatura", ev[is.na(leg_inicio) & tipo != "falecimento", .N])
reg("cbio_posses", nrow(ps))
cat("59_camara_biografia: concluido\n")
print(ev[, .N, by = .(secao, tipo)][order(secao, -N)])
print(ev[!nzchar(datas) & tipo != "outro", .(id_deputado_camara, secao, trecho)][1:10])
