# 17_wikipedia_prefeitos.R — prefeitos nas paginas 'Lista de prefeitos de <municipio>' da Wikipedia
# Entrada:  data_raw/wikipedia/prefeitos/*.html + .json (python/fetch_wikipedia_listas.py),
#           data/mandatos.csv, data/pessoas.csv, data/municipios_tse_ibge.csv
# Saida:    data/wikipedia_prefeitos.csv, data/wikipedia_prefeitos_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/17_wikipedia_prefeitos.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(rvest); library(xml2); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "data_referencia.R")); DATA_REF <- data_referencia(root)  # data fixa da versao, nao o dia da execucao (l. 444)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
# trecho literal da Wikipedia e CC BY-SA: publica-se citacao curta, truncada em 200 caracteres,
# com o credito na nota de cobertura; a classificacao de forma_saida usa o texto inteiro antes disso
# 29/08/2026: a citacao curta passou a ser uma JANELA em torno do termo que sustenta a
# classificacao, e nao os primeiros 200 caracteres. Truncar pelo comeco cortava a evidencia
# (verifica_wikipedia_estadual apontou 'suplente_efetivado sem assumiu/efetivado'), e a janela
# preserva ao mesmo tempo a brevidade exigida pela CC BY-SA e a auditabilidade do rotulo.
TERMOS_FORMA <- c(renuncia = "RENUNC", cassacao = "CASSAD|PERDA DO MANDATO|PERDEU O MANDATO|CASSA",
                  falecimento = "MORREU|FALECEU|MORTE|OBITO|FALEC", licenca = "LICENC",
                  afastamento = "AFASTAD|AFASTAMENTO", nao_tomou_posse = "NAO TOMOU POSSE|NAO ASSUMIU",
                  suplente_efetivado = "ASSUMIU|EFETIVAD", assumiu_titular = "ASSUMIU")
trunca_cit <- function(x, forma = NULL, n = 200) {
  x <- as.character(x)
  res <- x
  precisa <- !is.na(x) & nchar(x) > n
  if (!any(precisa)) return(res)
  xn <- toupper(iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = ""))
  for (i in which(precisa)) {
    ini <- 1L
    pat <- if (!is.null(forma) && !is.na(forma[i]) && forma[i] %in% names(TERMOS_FORMA)) TERMOS_FORMA[[forma[i]]] else NA_character_
    if (!is.na(pat)) {
      p <- regexpr(pat, xn[i])
      if (p > 0) ini <- max(1L, as.integer(p) - 60L)
    }
    trecho <- substr(x[i], ini, ini + n - 1L)
    res[i] <- paste0(if (ini > 1L) "..." else "", trecho, if (ini + n - 1L < nchar(x[i])) "..." else "")
  }
  res
}
script <- "R/17_wikipedia_prefeitos.R"
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
limpa <- function(x) { x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("\\s+", " ", x); trimws(x) }
limpa_nome <- function(x) { x <- limpa(x); x <- gsub("\\([^)]*\\)?", "", x); x <- gsub("\\s*[→–-]\\s*[A-Z]{2,}.*$", "", x); trimws(gsub("\\s+", " ", x)) }
MESES <- c(janeiro = 1, fevereiro = 2, marco = 3, abril = 4, maio = 5, junho = 6, julho = 7, agosto = 8, setembro = 9, outubro = 10, novembro = 11, dezembro = 12)
data_pt <- function(x) {
  x <- stri_trans_general(tolower(fcoalesce(x, "")), "Latin-ASCII")
  m <- regmatches(x, regexec("(\\d{1,2})\\.?[o°º]? de ([a-z]+) de (\\d{4})", x))
  vapply(seq_along(x), function(i) {
    mm <- m[[i]]
    if (length(mm) == 4 && !is.na(MESES[mm[3]])) return(sprintf("%s-%02d-%02d", mm[4], MESES[mm[3]], as.integer(mm[2])))
    if (grepl("\\d{4}-\\d{2}-\\d{2}", x[i])) return(regmatches(x[i], regexpr("\\d{4}-\\d{2}-\\d{2}", x[i])))
    if (grepl("\\d{2}/\\d{2}/\\d{4}", x[i])) { d <- regmatches(x[i], regexpr("\\d{2}/\\d{2}/\\d{4}", x[i])); return(paste(rev(strsplit(d, "/")[[1]]), collapse = "-")) }
    NA_character_
  }, character(1))
}

meta <- rbindlist(lapply(list.files("data_raw/wikipedia/prefeitos", pattern = "\\.json$", full.names = TRUE), function(f) as.data.table(fromJSON(f))), fill = TRUE)
stopifnot(nrow(meta) > 0)
# municipio da pagina: 'Lista de prefeitos de <Municipio>' (com desambiguacao '(UF)' as vezes)
# (verifica_wikipedia_prefeitos.R, 28/ago/2026: 11 paginas ficavam sem municipio por variantes do
#  titulo — 'Lista dos prefeitos', 'intendentes e prefeitos', 'prefeitos e vereadores', 'cidade de Sao Paulo')
meta[, municipio_wiki := sub("^Lista d(e|os) ([Pp]refeitos( e (vice-prefeitos|vereadores|intendentes))?|intendentes e prefeitos) (de|da|do|das|dos) ", "", titulo)]
meta[, municipio_wiki := sub("^cidade (de|do|da) ", "", municipio_wiki)]
meta[, uf_titulo := regmatches(municipio_wiki, regexpr("\\(([A-Z]{2})\\)", municipio_wiki))]
meta[, uf_titulo := gsub("[()]", "", uf_titulo)]
meta[, municipio_norm := norm(sub("\\s*\\(.*\\)$", "", municipio_wiki))]
tse_ibge <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
tse_ibge[, municipio_norm := norm(nome_ibge)]
# pareamento da pagina ao municipio: nome + UF da categoria (meta$uf); desambigua por '(UF)' no titulo
meta[, uf_ref := fifelse(nchar(fcoalesce(uf_titulo, "")) == 2, uf_titulo, uf)]
meta <- merge(meta, tse_ibge[, .(sg_ue, id_municipio_ibge, sg_uf, municipio_norm)], by.x = c("municipio_norm", "uf_ref"), by.y = c("municipio_norm", "sg_uf"), all.x = TRUE)
cat("paginas:", nrow(meta), "| com municipio pareado:", sum(!is.na(meta$sg_ue)), "\n")

parse_pref <- function(m) {
  h <- read_html(m$arquivo, encoding = "UTF-8")
  tabs <- html_elements(h, "table.wikitable")
  rbindlist(lapply(tabs, function(tb) {
    t <- tryCatch(as.data.table(html_table(tb, fill = TRUE, header = TRUE)), error = function(e) NULL)
    if (is.null(t) || nrow(t) == 0) return(NULL)
    hnorm <- function(v) { v <- stri_trans_general(tolower(as.character(v)), "Latin-ASCII"); v <- gsub("\\(.*?\\)", "", v); gsub("\\s+", " ", trimws(v)) }
    melhor <- function(idx) { if (!length(idx)) return(NA_integer_); sc <- vapply(idx, function(j) sum(grepl("[A-Za-z]{3,}", as.character(t[[j]]))), integer(1)); idx[which.max(sc)] }
    acha <- function(nml) list(
      nome = { c1 <- melhor(which(grepl("^prefeit", nml) & !grepl("vice", nml))); if (is.na(c1)) c1 <- melhor(which(grepl("^nome", nml) & !grepl("vice", nml))); if (is.na(c1)) c1 <- melhor(which(grepl("^(nome|prefeit|titular)", nml))); c1 },
      ini = which(grepl("inicio|posse", nml))[1], fim = which(grepl("termino|^fim|fim do|saida", nml))[1],
      per = which(grepl("^periodo|^mandato$|^mandato |^gestao|^ano", nml))[1], eleito = which(grepl("^eleito|^eleicao", nml))[1],
      part = which(grepl("partido", nml))[1], obs = which(grepl("observa|nota|motivo|ref", nml))[1], vice = which(grepl("^vice", nml))[1])
    nml <- hnorm(names(t)); cc <- acha(nml)
    if (is.na(cc$nome) || (is.na(cc$ini) && is.na(cc$per))) {
      for (r in seq_len(min(3, nrow(t)))) { cand <- hnorm(unlist(t[r])); cc2 <- acha(cand)
        if (!is.na(cc2$nome) && (!is.na(cc2$ini) || !is.na(cc2$per))) { t <- t[-seq_len(r)]; names(t) <- make.unique(cand); cc <- cc2; break } }
    }
    if (is.na(cc$nome) || (is.na(cc$ini) && is.na(cc$per)) || nrow(t) == 0) return(NULL)
    if (!is.na(cc$ini)) { ini_txt <- as.character(t[[cc$ini]]); fim_txt <- if (!is.na(cc$fim)) as.character(t[[cc$fim]]) else NA_character_ }
    else { per <- gsub("\\s+", " ", as.character(t[[cc$per]])); partes <- stri_split_regex(per, "\\s*(ate|até|—|–| - |a )\\s*")
           ini_txt <- vapply(partes, function(p) p[1], character(1)); fim_txt <- vapply(partes, function(p) if (length(p) > 1) p[2] else NA_character_, character(1)) }
    d <- data.table(nome_wiki = limpa_nome(as.character(t[[cc$nome]])), inicio_txt = ini_txt, fim_txt = fim_txt,
                    eleito_txt = if (!is.na(cc$eleito)) as.character(t[[cc$eleito]]) else NA_character_,
                    partido_wiki = if (!is.na(cc$part)) limpa(as.character(t[[cc$part]])) else NA_character_,
                    vice_wiki = if (!is.na(cc$vice)) limpa_nome(as.character(t[[cc$vice]])) else NA_character_,
                    observacao_original = if (!is.na(cc$obs)) limpa(as.character(t[[cc$obs]])) else NA_character_)
    d[, `:=`(inicio = data_pt(inicio_txt), fim = data_pt(fim_txt))]
    d[is.na(inicio) & grepl("(19|20)\\d{2}", inicio_txt), inicio := paste0(regmatches(inicio_txt, regexpr("(19|20)\\d{2}", inicio_txt)), "-01-01")]
    d[is.na(fim) & grepl("(19|20)\\d{2}", fim_txt), fim := paste0(regmatches(fim_txt, regexpr("(19|20)\\d{2}", fim_txt)), "-12-31")]
    d[nchar(nome_wiki) > 2 & !is.na(inicio) & substr(inicio, 1, 4) >= "1996"]
  }), fill = TRUE)
}
wp <- rbindlist(lapply(seq_len(nrow(meta)), function(i) {
  d <- tryCatch(parse_pref(meta[i]), error = function(e) NULL); if (is.null(d) || nrow(d) == 0) return(NULL)
  d[, `:=`(sg_ue = meta$sg_ue[i], id_municipio_ibge = meta$id_municipio_ibge[i], uf = meta$uf_ref[i], municipio = meta$municipio_wiki[i],
           url = meta$url[i], revisao = as.character(meta$revid[i]))]
}), fill = TRUE)
stopifnot(nrow(wp) > 0)
# data invalida (ex.: '2004-11-31') ou fim anterior ao inicio: o fim fica desconhecido
# (verifica_wikipedia_prefeitos.R, 28/ago/2026)
valida <- function(x) !is.na(x) & !is.na(suppressWarnings(as.IDate(x, format = "%Y-%m-%d")))
wp[!is.na(fim) & !valida(fim), fim := NA_character_]
wp <- wp[valida(inicio)]
wp[!is.na(fim) & fim < inicio, fim := NA_character_]
# celula com rowspan (dois partidos ou dois vices no mesmo mandato) sai duplicada de html_table:
# fica uma linha por pagina x nome x inicio, a mais preenchida (verifica_wikipedia_prefeitos.R, 28/ago/2026)
wp[, nome_normalizado := norm(nome_wiki)]
setorder(wp, url, nome_normalizado, inicio, -fim, -observacao_original, na.last = TRUE)
wp <- wp[!duplicated(wp[, .(url, nome_normalizado, inicio)])]
# linha que cobre mandatos consecutivos (reeleicao) vira uma linha por mandato de quatro anos
wp[, rid0 := .I]
longos <- wp[!is.na(inicio) & !is.na(fim) & substr(inicio, 6, 10) == "01-01" & as.integer(substr(fim, 1, 4)) - as.integer(substr(inicio, 1, 4)) >= 5L]
if (nrow(longos)) {
  partes <- rbindlist(lapply(seq_len(nrow(longos)), function(i) {
    a0 <- as.integer(substr(longos$inicio[i], 1, 4)); a1 <- as.integer(substr(longos$fim[i], 1, 4))
    ini_anos <- seq(a0, a1 - 1L, by = 4L); ini_anos <- ini_anos[ini_anos < a1]
    rbindlist(lapply(seq_along(ini_anos), function(k) {
      r <- copy(longos[i]); r[, inicio := sprintf("%d-01-01", ini_anos[k])]
      ult <- k == length(ini_anos)
      r[, fim := if (ult) longos$fim[i] else sprintf("%d-12-31", ini_anos[k] + 3L)]
      # observacao (saida) vale so para a ultima parte; ano de eleicao original so para a primeira
      if (!ult) r[, observacao_original := NA_character_]
      if (k > 1) r[, eleito_txt := NA_character_]
      r
    }))
  }))
  wp <- rbindlist(list(wp[!rid0 %in% longos$rid0], partes), use.names = TRUE, fill = TRUE)
}
wp[, rid0 := NULL]
wp[, obs_n := toupper(stri_trans_general(fcoalesce(observacao_original, ""), "Latin-ASCII"))]
# 'Vice-prefeito eleito no cargo de prefeito' e vice em exercicio, nao titular eleito: a exclusao por
# 'ELEITO' mandava 101 vices para o pareamento com o cargo de prefeito (verifica_wikipedia_prefeitos.R, 28/ago/2026)
wp[, condicao := fcase(grepl("^VICE", obs_n) | (grepl("VICE-PREFEIT", obs_n) & !grepl("^PREFEIT", obs_n) & grepl("NO CARGO|ASSUM|TITULAR|IMPEDIMENTO|VACANCIA|EMPOSSAD", obs_n)), "vice_em_exercicio",
                       grepl("INTERIN|EM EXERCICIO|SUBSTITU|PRESIDENTE DA CAMARA|ASSUMIU", obs_n) & !grepl("ELEITO|REELEITO", obs_n), "interino",
                       default = "eleito")]
wp[condicao == "eleito" & !is.na(inicio) & !substr(inicio, 6, 10) %in% c("01-01", "01-02") & !grepl("ELEITO|REELEITO", obs_n), condicao := "interino"]
# a causa citada na observacao pode ser a saida do ANTECESSOR ('assumiu apos a renuncia do titular'):
# nesse caso nao e a forma de saida desta linha (verifica_wikipedia_prefeitos.R, 28/ago/2026)
wp[, causa_antecessor := grepl("(APOS|DEPOIS D|COM A|DEVIDO|EM RAZAO|EM VIRTUDE|EM DECORRENCIA|NO IMPEDIMENTO|NA VAGA|SUBSTITUI|NO LUGAR|EM FUNCAO)[^.]{0,50}(RENUNC|MORTE|FALEC|OBITO|CASSA|AFAST|IMPEDIMENTO|LICEN|ASSASSIN)|(DO|DA) TITULAR|DO ENTAO|DO PREFEITO|DA PREFEITA", obs_n)]
wp[, forma_saida := fcase(
  !causa_antecessor & grepl("RENUNC", obs_n), "renuncia",
  !causa_antecessor & grepl("MORREU|MORTE|FALEC|OBITO|ASSASSIN", obs_n), "falecimento",
  !causa_antecessor & grepl("CASSA|IMPEACH|IMPEDIMENTO|PERDA DO MANDATO|AFASTADO DEFINITIV", obs_n), "cassacao",
  !causa_antecessor & grepl("AFAST", obs_n), "afastamento",
  !causa_antecessor & grepl("LICEN", obs_n), "licenca",
  !is.na(fim) & substr(fim, 6, 10) %in% c("12-31", "01-01"), "fim_regular",
  default = NA_character_)]
wp[, causa_antecessor := NULL]
# mandato em curso listado com o fim convencional futuro (ex.: 2025-2028) nao tem saida observada
# (verifica_wikipedia_prefeitos.R, 28/ago/2026: 41 linhas com fim em 2028/2029 e forma fim_regular)
wp[!is.na(fim) & fim > as.character(DATA_REF), `:=`(fim = NA_character_, forma_saida = fifelse(forma_saida %in% "fim_regular", NA_character_, forma_saida))]
wp[, ano_eleicao_bocel := as.integer(substr(inicio, 1, 4)) - 1L]
wp[!is.na(ano_eleicao_bocel) & ano_eleicao_bocel %% 4 != 0L, ano_eleicao_bocel := ano_eleicao_bocel - (ano_eleicao_bocel %% 4L)]
wp[, ano_eleito := suppressWarnings(as.integer(substr(regmatches(fcoalesce(eleito_txt, ""), regexpr("(19|20)\\d{2}", fcoalesce(eleito_txt, "")))[match(seq_len(.N), which(grepl("(19|20)\\d{2}", fcoalesce(eleito_txt, ""))))], 1, 4)))]
# o ano da coluna 'Eleicao' so vale quando precede o inicio em ate dois anos: em 28 paginas a coluna
# traz o ano da eleicao seguinte (Natal, Barueri, Diadema), o que pareava a linha ao mandato posterior
# (verifica_wikipedia_prefeitos.R, 28/ago/2026)
wp[!is.na(ano_eleito) & ano_eleito %% 4L == 0L & ano_eleito >= 1996L &
   (as.integer(substr(inicio, 1, 4)) - ano_eleito) %in% 0:2, ano_eleicao_bocel := ano_eleito]
wp <- wp[!is.na(ano_eleicao_bocel) & ano_eleicao_bocel >= 1996L & ano_eleicao_bocel <= 2024L]

## pareamento (prefeito eleito do BOCEL no mesmo municipio e eleicao)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cargo %in% c("PREFEITO", "VICE-PREFEITO")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, cargo, sg_ue = unidade_posicao, ano_eleicao = as.integer(ano_eleicao))], pess[, .(id_pessoa, nome, nome_urna_recente)], by = "id_pessoa")
mand[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente))]
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
wp[, rid := .I]
wp[, cargo_alvo := fifelse(condicao == "vice_em_exercicio", "VICE-PREFEITO", "PREFEITO")]
m1 <- merge(wp[!is.na(sg_ue), .(rid, sg_ue, cargo = cargo_alvo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_ue, cargo, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "cargo", "ano_eleicao", "nome_normalizado"), by.y = c("sg_ue", "cargo", "ano_eleicao", "nome_norm"))[, if (.N == 1) .SD, by = rid][, metodo := "nome_completo"]
r2 <- wp[!rid %in% m1$rid & !is.na(sg_ue)]
m2 <- merge(r2[, .(rid, sg_ue, cargo = cargo_alvo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[!is.na(urna_norm), .(sg_ue, cargo, ano_eleicao, urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "cargo", "ano_eleicao", "nome_normalizado"), by.y = c("sg_ue", "cargo", "ano_eleicao", "urna_norm"))[, if (.N == 1) .SD, by = rid][, metodo := "nome_urna"]
r3 <- wp[!rid %in% c(m1$rid, m2$rid) & !is.na(sg_ue)]
c3 <- merge(r3[, .(rid, sg_ue, cargo = cargo_alvo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_ue, cargo, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by = c("sg_ue", "cargo", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c3)) {
  tr <- tok(c3$nome_normalizado); tn <- tok(c3$nome_norm)
  c3[, contido := mapply(function(a, b) length(a) >= 1 && all(a %in% b), tr, tn)]
  m3 <- c3[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_no_nome_civil"]
} else m3 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
par <- rbindlist(list(m1[, .(rid, id_mandato, id_pessoa, metodo)], m2[, .(rid, id_mandato, id_pessoa, metodo)], m3[, .(rid, id_mandato, id_pessoa, metodo)]))[!duplicated(rid)]
wp[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
wp[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# interino nao e o titular do mandato
wp[condicao == "interino", id_mandato_bocel := NA_character_]

out <- wp[, .(sg_ue, id_municipio_ibge, uf, municipio, nome_wiki, nome_normalizado, partido_wiki, vice_wiki, inicio, fim, observacao_original,
              forma_saida, condicao, ano_eleicao_bocel, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url, revisao)]
setorder(out, uf, municipio, inicio, na.last = TRUE)
out <- unique(out)
out[, observacao_original := trunca_cit(observacao_original, forma_saida)]
fwrite(out, "data/wikipedia_prefeitos.csv", na = "NA", quote = TRUE)
cob <- merge(mand[cargo == "PREFEITO", .(n_bocel = .N), by = .(ano_eleicao)],
             # denominador e o cargo de prefeito: o numerador exclui o mandato de vice pareado (verifica_wikipedia_prefeitos.R, 28/ago/2026)
             out[!is.na(id_mandato_bocel) & condicao != "vice_em_exercicio", .(n_pareados = uniqueN(id_mandato_bocel), n_municipios = uniqueN(sg_ue)), by = .(ano_eleicao = ano_eleicao_bocel)], by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_pareados), `:=`(n_pareados = 0L, n_municipios = 0L)][, taxa := round(n_pareados / n_bocel, 4)]
fwrite(cob[order(ano_eleicao)], "data/wikipedia_prefeitos_cobertura.csv", na = "NA")
registrar_numero("wp_pref_n_paginas", nrow(meta), script = script)
registrar_numero("wp_pref_n_paginas_com_municipio", sum(!is.na(meta$sg_ue)), script = script)
registrar_numero("wp_pref_n_linhas", nrow(out), script = script)
registrar_numero("wp_pref_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("wp_pref_n_com_forma_saida", out[!is.na(id_mandato_bocel) & !is.na(forma_saida), uniqueN(id_mandato_bocel)], script = script)
print(out[!is.na(id_mandato_bocel), .N, by = forma_saida][order(-N)]); print(cob)
cat("17_wikipedia_prefeitos: concluido —", nrow(out), "linhas,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
