# 16_wikipedia_estadual.R — governadores, vice-governadores e deputados estaduais/distritais nas
# listas da Wikipedia em portugues (tabelas de mandato com inicio/fim/observacoes; secoes de
# mortes, cassacoes, renuncias, licencas e suplentes nas listas por legislatura).
# Entrada:  data_raw/wikipedia/estadual/*.html + .json (R/coleta/wikipedia_listas.R), data/mandatos.csv, data/pessoas.csv
# Saida:    data/wikipedia_estadual.csv, data/wikipedia_estadual_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/16_wikipedia_estadual.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(rvest); library(xml2); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
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
script <- "R/16_wikipedia_estadual.R"
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
limpa <- function(x) { x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("\\s+", " ", x); trimws(x) }
# nome sem o que a Wikipedia cola entre parenteses (anos de vida, partido, seta de troca de partido)
limpa_nome <- function(x) { x <- limpa(x); x <- gsub("\\([^)]*\\)?", "", x); x <- gsub("\\s*[→–-]\\s*[A-Z]{2,}.*$", "", x); trimws(gsub("\\s+", " ", x)) }
MESES <- c(janeiro = 1, fevereiro = 2, marco = 3, abril = 4, maio = 5, junho = 6, julho = 7, agosto = 8, setembro = 9, outubro = 10, novembro = 11, dezembro = 12)
data_pt <- function(x) {
  # '7 de julho de 2019', '1.º de maio de 2018', '2019-02-01', '01/02/2019', '2019'
  x <- stri_trans_general(tolower(fcoalesce(x, "")), "Latin-ASCII")
  m <- regmatches(x, regexec("(\\d{1,2})\\.?[o°º]? de ([a-z]+) de (\\d{4})", x))
  out <- vapply(seq_along(x), function(i) {
    mm <- m[[i]]
    if (length(mm) == 4 && !is.na(MESES[mm[3]])) return(sprintf("%s-%02d-%02d", mm[4], MESES[mm[3]], as.integer(mm[2])))
    if (grepl("\\d{4}-\\d{2}-\\d{2}", x[i])) return(regmatches(x[i], regexpr("\\d{4}-\\d{2}-\\d{2}", x[i])))
    if (grepl("\\d{2}/\\d{2}/\\d{4}", x[i])) { d <- regmatches(x[i], regexpr("\\d{2}/\\d{2}/\\d{4}", x[i])); return(paste(rev(strsplit(d, "/")[[1]]), collapse = "-")) }
    NA_character_
  }, character(1))
  out
}
ano_de <- function(x) { y <- regmatches(x, regexpr("(19|20)\\d{2}", x)); ifelse(length(y), y, NA) }

meta <- rbindlist(lapply(list.files("data_raw/wikipedia/estadual", pattern = "\\.json$", full.names = TRUE), function(f) as.data.table(fromJSON(f))), fill = TRUE)
stopifnot(nrow(meta) > 0)

## ------------------------------------------------ governadores e vices (tabelas de mandato)
parse_exec <- function(m) {
  h <- read_html(m$arquivo, encoding = "UTF-8")
  tabs <- html_elements(h, "table.wikitable")
  rbindlist(lapply(tabs, function(tb) {
    t <- tryCatch(as.data.table(html_table(tb, fill = TRUE, header = TRUE)), error = function(e) NULL)
    if (is.null(t) || nrow(t) == 0) return(NULL)
    hnorm <- function(v) { v <- stri_trans_general(tolower(as.character(v)), "Latin-ASCII"); v <- gsub("\\(.*?\\)", "", v); gsub("\\s+", " ", trimws(v)) }
    nm <- names(t); nml <- hnorm(nm)
    # entre colunas candidatas ao nome (a Wikipedia repete o cabecalho quando ha faixa de cor),
    # fica a que tem mais celulas com letras
    melhor <- function(idx) {
      if (!length(idx)) return(NA_integer_)
      sc <- vapply(idx, function(j) sum(grepl("[A-Za-z]{3,}", as.character(t[[j]]))), integer(1))
      idx[which.max(sc)]
    }
    acha_cols <- function(nml) {
      # 13 listas de vices trazem o nome sob o cabecalho 'Lista de vice-governadores de <UF>' (verificacao 28/ago)
      pref <- if (m$cargo == "VICE-GOVERNADOR") "^(vice|lista de vice)" else "^(governador|presidente|interventor|titular)"
      c_nome <- melhor(which(grepl(pref, nml) & !grepl("^nº|^n\\.º|^no ", nml)))
      if (is.na(c_nome)) c_nome <- melhor(which(grepl("^nome", nml) & !grepl("vice", nml)))
      if (is.na(c_nome) && m$cargo != "VICE-GOVERNADOR") c_nome <- melhor(which(grepl("^(nome|governador|presidente|interventor|prefeito)", nml) & !grepl("vice", nml)))
      if (is.na(c_nome)) c_nome <- melhor(which(grepl("^(nome|vice)", nml)))
      # duas colunas 'Periodo' (AP): a primeira e o inicio, a segunda o fim (verificacao 28/ago: o fim ficava NA
      # e o mandato parecia em curso)
      per2 <- which(grepl("^periodo|^mandato$|^mandato ", nml))
      ini_c <- which(grepl("inicio|posse", nml))[1]; fim_c <- which(grepl("termino|^fim|fim do|saida", nml))[1]
      if (is.na(ini_c) && length(per2) >= 2) { ini_c <- per2[1]; fim_c <- per2[2]; per2 <- integer() }
      list(nome = c_nome, ini = ini_c, fim = fim_c,
           per = per2[1], eleito = which(grepl("^eleito|^eleicao", nml))[1],
           part = which(grepl("partido", nml))[1], obs = which(grepl("observa|nota|motivo|ref", nml))[1])
    }
    cc <- acha_cols(nml)
    # cabecalho de secao no lugar dos nomes de coluna (SP): procura a linha de cabecalho nas 3 primeiras
    if (is.na(cc$nome) || (is.na(cc$ini) && is.na(cc$per))) {
      for (r in seq_len(min(3, nrow(t)))) {
        cand <- hnorm(unlist(t[r]))
        cc2 <- acha_cols(cand)
        if (!is.na(cc2$nome) && (!is.na(cc2$ini) || !is.na(cc2$per))) { t <- t[-seq_len(r)]; names(t) <- make.unique(cand); nml <- cand; cc <- cc2; break }
      }
    }
    if (is.na(cc$nome) || (is.na(cc$ini) && is.na(cc$per)) || nrow(t) == 0) return(NULL)
    c_nome <- cc$nome; c_part <- cc$part; c_obs <- cc$obs
    if (!is.na(cc$ini)) { ini_txt <- as.character(t[[cc$ini]]); fim_txt <- if (!is.na(cc$fim)) as.character(t[[cc$fim]]) else NA_character_ }
    else {  # coluna unica 'Periodo': 'A ate B', 'A — B', 'A - B'
      per <- gsub("\\s+", " ", as.character(t[[cc$per]]))
      partes <- stri_split_regex(per, "\\s*(ate|até|—|–| - )\\s*")
      ini_txt <- vapply(partes, function(p) p[1], character(1)); fim_txt <- vapply(partes, function(p) if (length(p) > 1) p[2] else NA_character_, character(1))
    }
    d <- data.table(nome_wiki = limpa_nome(as.character(t[[c_nome]])), inicio_txt = ini_txt,
                    fim_txt = fim_txt,
                    eleito_txt = if (!is.na(cc$eleito)) as.character(t[[cc$eleito]]) else NA_character_,
                    partido_wiki = if (!is.na(c_part)) limpa(as.character(t[[c_part]])) else NA_character_,
                    observacao_original = if (!is.na(c_obs)) limpa(as.character(t[[c_obs]])) else NA_character_)
    d[, `:=`(inicio = data_pt(inicio_txt), fim = data_pt(fim_txt))]
    d[is.na(inicio) & grepl("(19|20)\\d{2}", inicio_txt), inicio := paste0(regmatches(inicio_txt, regexpr("(19|20)\\d{2}", inicio_txt)), "-01-01")]
    d[is.na(fim) & grepl("(19|20)\\d{2}", fim_txt), fim := paste0(regmatches(fim_txt, regexpr("(19|20)\\d{2}", fim_txt)), "-12-31")]
    # 'Cargo vago', 'Nenhum', 'Vago' marcam vacancia, nao pessoa (verificacao 28/ago)
    d <- d[!grepl("^(CARGO )?VAGO|^NENHUM|^VACANTE", norm(nome_wiki))]
    d[nchar(nome_wiki) > 2 & !grepl("^(19|20)\\d{2}", nome_wiki) & !is.na(inicio) & substr(inicio, 1, 4) >= "1994"]
  }), fill = TRUE)
}
exec <- rbindlist(lapply(which(meta$cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR")), function(i) {
  d <- parse_exec(meta[i]); if (is.null(d) || nrow(d) == 0) return(NULL)
  d[, `:=`(uf = meta$uf[i], cargo = meta$cargo[i], legislatura = NA_character_, condicao = "titular", url_pagina = meta$url[i], revisao = as.character(meta$revid[i]))]
}), fill = TRUE)
fwrite(exec, "output/verificacao/wikipedia_exec_bruto.csv", na = "NA")
if (nrow(exec)) {
  # rowspan (nome com mais de uma linha de partido ou de eleicao) repete a mesma pessoa com o mesmo
  # periodo; fica a primeira ocorrencia (verificacao 28/ago: 27 mandatos pareados a 2-3 linhas identicas)
  exec <- exec[!duplicated(exec[, .(uf, cargo, nome_wiki, inicio, fim)])]
  exec[, obs_n := toupper(stri_trans_general(fcoalesce(observacao_original, ""), "Latin-ASCII"))]
  exec[, forma_saida := fcase(
    grepl("RENUNC", obs_n), "renuncia",
    grepl("MORREU|MORTE|FALEC|OBITO|ASSASSIN", obs_n), "falecimento",
    grepl("CASSA|IMPEACH|IMPEDIMENTO|PERDA DO MANDATO|AFASTADO DEFINITIV", obs_n), "cassacao",
    grepl("INTERINO|EM EXERCICIO|SUBSTITU", obs_n), "outro",
    grepl("AFAST", obs_n), "afastamento",
    grepl("LICEN", obs_n), "licenca",
    !is.na(fim) & substr(fim, 6, 10) %in% c("12-31", "01-01"), "fim_regular",
    default = NA_character_)]
}

## ------------------------------------------------ deputados (tabelas por secao)
parse_dep <- function(m) {
  h <- read_html(m$arquivo, encoding = "UTF-8")
  # alem de wikitable, as listas usam 'prettytable' (SC 17a, suplentes de PE 17a) e tabela sem classe (RS 50a, 51a)
  # (verificacao 28/ago); tabelas de navegacao (navbox, toccolours) ficam de fora
  nodes <- html_elements(h, "h2, h3, h4, table.wikitable, table.prettytable, table:not([class])")
  sec <- ""; out <- list()
  for (n in nodes) {
    tg <- xml_name(n)
    if (tg %in% c("h2", "h3", "h4")) { sec <- limpa(html_text2(n)); next }
    if (grepl("REFER|LIGAC|VER TAMBEM", toupper(stri_trans_general(sec, "Latin-ASCII")))) next
    t <- tryCatch(as.data.table(html_table(n, fill = TRUE, header = TRUE)), error = function(e) NULL)
    if (is.null(t) || nrow(t) == 0) next
    nm <- names(t); nml <- stri_trans_general(tolower(nm), "Latin-ASCII")
    if (any(grepl("^lider|bancada", nml)) || nml[1] == "cargo") next
    # entre colunas candidatas ao nome ('Nome|Nome' = foto + nome, AM 20a), fica a com mais celulas com letras;
    # 'Candidato(a)' e o cabecalho da lista por votacao (DF 9a) (verificacao 28/ago)
    melhor_nome <- function(idx) { if (!length(idx)) return(NA_integer_); sc <- vapply(idx, function(j) sum(grepl("[A-Za-z]{3,}", as.character(t[[j]]))), integer(1)); idx[which.max(sc)] }
    c_nome <- melhor_nome(which(grepl("^(nome|deputad|suplente|parlamentar|candidat)", nml)))
    if (is.na(c_nome)) { if (nrow(t) > 1 && any(grepl("deputad|nome", stri_trans_general(tolower(as.character(t[1])), "Latin-ASCII")))) { names(t) <- as.character(t[1]); t <- t[-1]; nm <- names(t); nml <- stri_trans_general(tolower(nm), "Latin-ASCII"); c_nome <- melhor_nome(which(grepl("^(nome|deputad|suplente|candidat)", nml))) } }
    if (is.na(c_nome)) next
    c_part <- which(grepl("partido", nml))[1]; c_obs <- which(grepl("nota|observa|motivo|situa|data", nml))
    d <- data.table(nome_wiki = limpa_nome(as.character(t[[c_nome]])), partido_wiki = if (!is.na(c_part)) limpa(as.character(t[[c_part]])) else NA_character_,
                    observacao_original = if (length(c_obs)) limpa(apply(t[, ..c_obs], 1, function(r) paste(na.omit(r), collapse = " | "))) else NA_character_,
                    secao = sec)
    out[[length(out) + 1]] <- d[nchar(nome_wiki) > 2]
  }
  rbindlist(out, fill = TRUE)
}
deps <- rbindlist(lapply(which(meta$cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL")), function(i) {
  d <- parse_dep(meta[i]); if (is.null(d) || nrow(d) == 0) return(NULL)
  leg <- regmatches(meta$titulo[i], regexpr("\\d+", meta$titulo[i]))
  d[, `:=`(uf = meta$uf[i], cargo = meta$cargo[i], legislatura = if (length(leg)) leg else NA_character_, url_pagina = meta$url[i], revisao = as.character(meta$revid[i]))]
}), fill = TRUE)
if (nrow(deps)) {
  deps[, sec_n := toupper(stri_trans_general(secao, "Latin-ASCII"))]
  deps[, obs_n := toupper(stri_trans_general(fcoalesce(observacao_original, ""), "Latin-ASCII"))]
  deps[, condicao := fifelse(grepl("SUPLENTE", sec_n) | grepl("^SUPLENTE|ASSUMIU", obs_n), "suplente", "titular")]
  deps[, forma_saida := fcase(
    grepl("MORTE|FALEC|OBITO", sec_n) | grepl("MORREU|FALECEU|MORTE|OBITO", obs_n), "falecimento",
    grepl("CASSA", sec_n) | grepl("CASSAD|PERDA DO MANDATO|PERDEU O MANDATO", obs_n), "cassacao",
    grepl("RENUNC", sec_n) | grepl("RENUNCIOU|RENUNCIA", obs_n), "renuncia",
    grepl("LICEN", sec_n) | grepl("LICENC", obs_n), "licenca",
    grepl("AFAST", sec_n) | grepl("AFASTOU|AFASTAD", obs_n), "afastamento",
    grepl("NAO TOMOU POSSE|NAO ASSUMIU", obs_n), "nao_tomou_posse",
    grepl("ASSUMIU|EFETIVAD", obs_n) & condicao == "suplente", "suplente_efetivado",
    default = NA_character_)]
  deps[, fim := data_pt(observacao_original)]
  deps[, inicio := NA_character_]
}

if (!"eleito_txt" %in% names(exec)) exec[, eleito_txt := NA_character_]
# uma linha da Wikipedia que atravessa uma posse regular (1/jan de 1999, 2003, ...) cobre mais de um
# mandato (reeleicao; vice que assumiu e depois foi eleito; mandato em curso sem fim) e vira uma linha
# por mandato, cortada em cada 1/jan de posse; a forma de saida fica so na ultima parte
# (verificacao 28/ago: antes o corte exigia inicio em 1/jan e fim conhecido, e deixava sem par
# Pezao 2014, Alckmin 2002, Carlesse 2018, Portela 2002, Ottomar 2006, Anchieta 2010, Marcos Rocha 2022)
POSSES <- sprintf("%d-01-01", seq(1995L, 2031L, by = 4L))
if (nrow(exec)) {
  exec[, rid0 := .I]
  exec[, fim_aberto := is.na(fim)]
  exec[, fim_ref := fifelse(fim_aberto, "2026-12-31", fim)]
  cortes <- lapply(seq_len(nrow(exec)), function(i) { if (is.na(exec$inicio[i])) return(character()); POSSES[POSSES > exec$inicio[i] & POSSES < exec$fim_ref[i]] })
  longos <- which(lengths(cortes) > 0)
  if (length(longos)) {
    partes <- rbindlist(lapply(longos, function(i) {
      bordas <- c(exec$inicio[i], cortes[[i]], exec$fim_ref[i]); n <- length(bordas) - 1L
      rbindlist(lapply(seq_len(n), function(k) {
        r <- copy(exec[i]); r[, `:=`(inicio = bordas[k], fim = bordas[k + 1L])]
        if (k < n) r[, forma_saida := "fim_regular"]
        if (k == n && exec$fim_aberto[i]) r[, fim := NA_character_]
        r[, parte := k]
        r
      }))
    }))
    exec[, parte := 1L]
    exec <- rbindlist(list(exec[!rid0 %in% longos], partes), use.names = TRUE, fill = TRUE)
  } else exec[, parte := 1L]
  exec[, `:=`(fim_aberto = NULL, fim_ref = NULL)]
  # substituto: quem entra no governo fora da posse regular (vice, presidente da Assembleia ou do
  # Tribunal, vencedor de pleito suplementar) nao e o titular do mandato ordinario do BOCEL; a parte
  # que comeca em 1/jan de posse e titular. Excecao: retorno do proprio titular apos afastamento
  # (Ibaneis, DF 2023), reconhecido pela linha anterior da mesma pessoa iniciada na posse regular.
  # (verificacao 28/ago: a regra anterior isentava quem tinha 'eleito' na observacao, o que deixava
  # como titular o vice 'eleito no cargo de titular' e o eleito em pleito suplementar)
  exec[, posse_regular := !is.na(inicio) & substr(inicio, 6, 10) %in% c("01-01", "01-02", "03-15")]
  exec[, nome_n := norm(nome_wiki)]
  exec[cargo == "GOVERNADOR", tem_posse := any(posse_regular), by = .(uf, nome_n, ciclo = (as.integer(substr(fcoalesce(inicio, "0000"), 1, 4)) - 3L) %/% 4L)]
  exec[cargo == "GOVERNADOR" & !posse_regular & !(tem_posse %in% TRUE), condicao := "substituto"]
  # a data domina a observacao: parte que termina na posse regular seguinte encerrou o mandato
  # (a nota 'vice-governador renunciou' na linha de Helder Barbalho 2021-2023 marcava renuncia do
  # governador; verificacao 28/ago)
  exec[!is.na(fim) & fim %in% POSSES, forma_saida := "fim_regular"]
  exec[, `:=`(rid0 = NULL, parte = NULL, posse_regular = NULL, nome_n = NULL, tem_posse = NULL)]
}
wk <- rbindlist(list(exec[, .(uf, cargo, legislatura, nome_wiki, partido_wiki, condicao, inicio, fim, observacao_original, forma_saida, url_pagina, revisao, eleito_txt)],
                     deps[, .(uf, cargo, legislatura, nome_wiki, partido_wiki, condicao, inicio, fim, observacao_original, forma_saida, url_pagina, revisao)]), fill = TRUE)
wk[, nome_normalizado := norm(nome_wiki)]

## ------------------------------------------------ eleicao de referencia
# governadores/vices: eleicao = ano do inicio - 1 quando inicio em 1/jan (posse regular); deputados: pela legislatura
# (numeracao varia por UF): usa a eleicao do BOCEL cujo periodo 1/fev(ano+1)..31/jan(ano+5) contem a data do evento,
# senao a ordem das legislaturas na categoria (a mais recente = 2022)
wk[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR") & !is.na(inicio), ano_eleicao_bocel := as.integer(substr(inicio, 1, 4)) - 1L]
wk[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR") & !is.na(ano_eleicao_bocel) & ano_eleicao_bocel %% 4 != 2L, ano_eleicao_bocel := ano_eleicao_bocel - ((ano_eleicao_bocel - 2L) %% 4L)]
# a coluna 'Eleito' da tabela NAO prevalece: com rowspan ela repete a linha da pessoa com cada ano
# (1998 e 2002 para o mesmo periodo 1999-2007) e atribuia a parte 1999-2003 a eleicao de 2002
# (verificacao 28/ago); o ano vem do inicio de cada parte cortada em 1/jan de posse
# deputados: o numero da legislatura vira ano de eleicao pelo ano de posse que a propria pagina
# declara (titulo ou primeiro paragrafo: '2019–2023'); o deslocamento numero->ano e constante por UF
# e vale a moda, para as paginas sem ano no texto. (verificacao 28/ago: a regra 'a mais recente = 2022'
# falhava em RO, SE, RN, cuja categoria termina na legislatura 2019-2023, e em PA, que termina em 2015-2019)
anos_pag <- rbindlist(lapply(which(meta$cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL")), function(i) {
  leg <- suppressWarnings(as.integer(stri_extract_first_regex(meta$titulo[i], "\\d+")))
  h <- read_html(meta$arquivo[i], encoding = "UTF-8")
  tx <- paste(meta$titulo[i], paste(head(html_text2(html_elements(h, "p")), 2), collapse = " "))
  # ancoras, na ordem: 'YYYY–YYYY' (posse e fim), 'assumiram em YYYY' (posse), 'eleitos/eleicao de YYYY'
  par <- stri_match_first_regex(tx, "((?:19|20)\\d{2})\\s*[–—-]\\s*((?:19|20)\\d{2})")
  posse <- suppressWarnings(as.integer(par[, 2])); fim <- suppressWarnings(as.integer(par[, 3]))
  if (is.na(posse) || is.na(fim) || fim - posse != 4L || posse %% 4L != 3L) posse <- NA_integer_
  ano <- if (!is.na(posse)) posse - 1L else NA_integer_
  if (is.na(ano)) { a <- suppressWarnings(as.integer(stri_match_first_regex(tx, "assumiram em ((?:19|20)\\d{2})")[, 2])); if (!is.na(a) && a %% 4L == 3L) ano <- a - 1L }
  if (is.na(ano)) { a <- suppressWarnings(as.integer(stri_match_first_regex(tx, "elei[cç][aã]o[^.]{0,40}?((?:19|20)\\d{2})|eleitos[^.]{0,40}?((?:19|20)\\d{2})")[, 2:3])); a <- a[!is.na(a)][1]; if (length(a) && !is.na(a) && a %% 4L == 2L) ano <- a }
  data.table(uf = meta$uf[i], legislatura = leg, ano_pag = ano)
}))[!is.na(legislatura)]
anos_pag[, desloc := ano_pag - 4L * legislatura]
desloc_uf <- anos_pag[!is.na(desloc), .(desloc = as.integer(names(which.max(table(desloc)))), n_ancoras = .N), by = uf]
legs <- merge(unique(anos_pag[, .(uf, legislatura)]), desloc_uf, by = "uf", all.x = TRUE)
# sem ancora em nenhuma pagina da UF: a mais recente = 2022 (avisado)
sem <- legs[is.na(desloc), unique(uf)]
if (length(sem)) { cat("wikipedia_estadual: UF sem ano no texto das paginas, legislatura mais recente = 2022:", sem, "\n"); legs[is.na(desloc), desloc := 2022L - 4L * max(legislatura), by = uf] }
legs[, ano_eleicao_bocel := 4L * legislatura + desloc]
print(legs[, .(n_paginas = .N, desloc = desloc[1], n_ancoras = n_ancoras[1], ano_max = max(ano_eleicao_bocel)), by = uf][order(uf)], nrows = 30)
fwrite(merge(anos_pag, legs[, .(uf, legislatura, ano_eleicao_bocel)], by = c("uf", "legislatura")), "output/verificacao/wikipedia_legislatura_ano.csv", na = "NA")
stopifnot(legs[, max(ano_eleicao_bocel), by = uf][, all(V1 <= 2022L)])
wk <- merge(wk, legs[, .(uf, legislatura = as.character(legislatura), ano_leg = ano_eleicao_bocel)], by = c("uf", "legislatura"), all.x = TRUE)
wk[is.na(ano_eleicao_bocel), ano_eleicao_bocel := ano_leg]; wk[, ano_leg := NULL]
wk <- wk[!is.na(ano_eleicao_bocel) & ano_eleicao_bocel >= 1998L & ano_eleicao_bocel <= 2022L]

## ------------------------------------------------ pareamento com o BOCEL
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR", "DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, cargo, sg_uf, ano_eleicao = as.integer(ano_eleicao))], pess[, .(id_pessoa, nome, nome_urna_recente)], by = "id_pessoa")
mand[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente))]
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
wk[, rid := .I]
m1 <- merge(wk[, .(rid, sg_uf = uf, cargo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_uf, cargo, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_uf", "cargo", "ano_eleicao", "nome_normalizado"), by.y = c("sg_uf", "cargo", "ano_eleicao", "nome_norm"))
m1 <- m1[, if (.N == 1) .SD, by = rid][, metodo := "nome_completo"]
r2 <- wk[!rid %in% m1$rid]
m2 <- merge(r2[, .(rid, sg_uf = uf, cargo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_uf", "cargo", "ano_eleicao", "nome_normalizado"), by.y = c("sg_uf", "cargo", "ano_eleicao", "urna_norm"))
m2 <- m2[, if (.N == 1) .SD, by = rid][, metodo := "nome_urna"]
r3 <- wk[!rid %in% c(m1$rid, m2$rid)]
c3 <- merge(r3[, .(rid, sg_uf = uf, cargo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_uf, cargo, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by = c("sg_uf", "cargo", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c3)) {
  tr <- tok(c3$nome_normalizado); tn <- tok(c3$nome_norm)
  c3[, contido := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tr, tn)]
  m3 <- c3[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_no_nome_civil"]
} else m3 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
# (4) tokens do nome da Wikipedia contidos no NOME DE URNA (apelidos: Pezao, Ratinho Junior, Anchieta), pessoa unica
r4 <- wk[!rid %in% c(m1$rid, m2$rid, m3$rid)]
c4 <- merge(r4[, .(rid, sg_uf = uf, cargo, ano_eleicao = ano_eleicao_bocel, nome_normalizado)], mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, urna_norm, id_mandato, id_pessoa)],
            by = c("sg_uf", "cargo", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c4)) {
  tr <- tok(c4$nome_normalizado); tu <- tok(c4$urna_norm)
  c4[, contido := mapply(function(a, b) length(a) >= 1 && any(nchar(a) >= 4) && all(a %in% b), tr, tu)]
  m4 <- c4[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_no_nome_de_urna"]
} else m4 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
par <- rbindlist(list(m1[, .(rid, id_mandato, id_pessoa, metodo)], m2[, .(rid, id_mandato, id_pessoa, metodo)], m3[, .(rid, id_mandato, id_pessoa, metodo)], m4[, .(rid, id_mandato, id_pessoa, metodo)]))[!duplicated(rid)]
# suplente so pareia por nome exato: o BOCEL so tem titulares, e a regra de tokens levava o suplente
# 'Guilherme Sampaio' (CE 2022) ao mandato de Guilherme Sampaio Landim (verificacao 28/ago)
par <- par[!(rid %in% wk[condicao == "suplente", rid] & metodo %in% c("tokens_no_nome_civil", "tokens_no_nome_de_urna"))]
wk[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
wk[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# substituto (vice em exercicio) nao e o titular do mandato de governador: nao pareia ao mandato de governador
wk[condicao == "substituto", `:=`(id_mandato_bocel = NA_character_)]

out <- wk[, .(uf, cargo, legislatura, ano_eleicao_bocel, nome_wiki, nome_normalizado, partido_wiki, condicao, inicio, fim, observacao_original,
              forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url_pagina, revisao)]
for (cc in c("partido_wiki", "observacao_original", "legislatura")) out[get(cc) == "" | get(cc) == "—" | get(cc) == "-", (cc) := NA_character_]
# chave url x nome x inicio: a mesma pessoa aparece na tabela principal e na de renuncias/licencas/suplentes;
# fica a linha que traz forma de saida, depois a com observacao, depois a de suplente (verificacao 28/ago:
# 125 linhas repetiam a chave e 72 mandatos ficavam pareados a duas linhas iguais)
setorder(out, uf, cargo, ano_eleicao_bocel, nome_normalizado, -forma_saida, -observacao_original, -condicao, na.last = TRUE)
out <- out[!duplicated(out[, .(url_pagina, nome_normalizado, inicio = fcoalesce(inicio, ""))])]
# a mesma pessoa com nome de urna numa tabela e nome civil em outra da mesma pagina (PB e PE, 17a legislatura)
# pareava duas linhas ao mesmo mandato: fica uma por mandato e inicio, na mesma prioridade
out <- out[is.na(id_mandato_bocel) | !duplicated(out[, .(id_mandato_bocel, inicio = fcoalesce(inicio, ""))]) ]
setorder(out, uf, cargo, ano_eleicao_bocel, nome_normalizado)
out[, observacao_original := trunca_cit(observacao_original, forma_saida)]
fwrite(out, "data/wikipedia_estadual.csv", na = "NA", quote = TRUE)
cob <- merge(mand[, .(n_bocel = .N), by = .(uf = sg_uf, cargo, ano_eleicao)],
             out[, .(n_wiki = .N, n_pareados = uniqueN(na.omit(id_mandato_bocel))), by = .(uf, cargo, ano_eleicao = ano_eleicao_bocel)],
             by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_wiki), `:=`(n_wiki = 0L, n_pareados = 0L)][, taxa := round(n_pareados / n_bocel, 4)]
fwrite(cob[order(uf, cargo, ano_eleicao)], "data/wikipedia_estadual_cobertura.csv", na = "NA")

registrar_numero("wp_est_n_paginas", nrow(meta), script = script)
registrar_numero("wp_est_n_linhas", nrow(out), script = script)
for (cg in unique(out$cargo)) {
  k <- gsub("[^a-z]", "_", tolower(cg))
  registrar_numero(paste0("wp_est_", k, "_pareados"), out[cargo == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
  registrar_numero(paste0("wp_est_", k, "_com_forma_saida"), out[cargo == cg & !is.na(id_mandato_bocel) & !is.na(forma_saida), uniqueN(id_mandato_bocel)], script = script)
}
print(out[!is.na(id_mandato_bocel), .N, by = .(cargo, forma_saida)][order(cargo, -N)])
cat("16_wikipedia_estadual: concluido —", nrow(out), "linhas,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
