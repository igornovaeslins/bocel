# 00_funcoes_PR.R — leitura do expediente (cabecalho) do Diario da Assembleia do Parana
#   e transformacao em painel data x deputado. Usado por 01_exercicio_PR.R e verifica_PR.R.
suppressPackageStartupMessages({library(data.table); library(stringi)})

asc   <- function(x) stri_trans_general(x, "Latin-ASCII")
limpa <- function(x) trimws(stri_replace_all_regex(x, "\\s+", " "))
norm_nome <- function(x) {                       # mesma normalizacao de R/13_exercicio_assembleias.R
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}

# o DIOE imprime a sigla do partido em 5 camadas sobrepostas; linhas repetidas sao artefato
dedup_linhas <- function(t) {
  L <- unlist(stri_split_fixed(t, "\n")); k <- trimws(L)
  manter <- c(TRUE, k[-1] != k[-length(k)] | !nzchar(k[-1]))
  paste(L[manter], collapse = "\n")
}

# bloco da relacao nominal por partido. Duas eras de diagramacao:
#   1999-2021  "Representacao Partidaria" seguida da relacao por partido;
#   2022-2026  "Representacao Partidaria" e "Liderancas" como titulos vizinhos, a Mesa Executiva
#              em caixa alta antes e a relacao por partido depois.
# Em vez de adivinhar a era, colhe todo candidato e fica com o que ancora mais siglas de partido.
bloco_rep <- function(t) {
  if (is.na(t) || !nzchar(t)) return(NA_character_)
  t <- dedup_linhas(t); ta <- asc(t); cand <- character(0)
  for (chave in c("Representacao Partidaria", "Liderancas", "Representacao partidaria")) {
    p <- stri_locate_first_fixed(ta, chave); if (is.na(p[1])) next
    resto <- substr(t, p[2] + 1, nchar(t)); restoa <- substr(ta, p[2] + 1, nchar(ta))
    fim <- nchar(resto)
    for (m in c("DIARIO DA ASSEMB", "Representacao Partidaria", "Edicao n",
                "Mesa Diretora", "Diretor Geral", "Curitiba,", "SUMARIO", "Sumario")) {
      q <- stri_locate_first_fixed(restoa, m)
      if (!is.na(q[1]) && q[1] > 1 && q[1] - 1 < fim) fim <- q[1] - 1
    }
    cand <- c(cand, substr(resto, 1, fim))
  }
  cand <- cand[nchar(cand) >= 200]
  if (!length(cand)) return(NA_character_)
  # ancora: sigla de partido colada a dois-pontos, hifen ou numero de bancada
  anc <- vapply(cand, function(z) {
    za <- toupper(asc(z))
    sum(vapply(SIGLAS, function(s)
      stri_count_regex(za, paste0("\\b", stri_replace_all_fixed(s, " ", "\\s"), "\\s*[:\u2013\u2014-]")),
      integer(1)))
  }, integer(1))
  cand[order(-anc, -nchar(cand))][1]
}

SIGLAS <- c("PFL","PTB","PMDB","PPB","PSDB","PT","PDT","PL","PSB","PSC","PP","PPS","PSD","PV","PRB",
 "PSL","PMN","PEN","DEM","PSDC","PTC","PRP","PHS","PROS","PSOL","PRTB","PTN","PODE","PODEMOS",
 "PATRIOTA","PATRI","REPUBLICANOS","SOLIDARIEDADE","SD","NOVO","AVANTE","CIDADANIA","REDE","PCDOB",
 "PC DO B","UNIAO","MDB","PRD","PPL","PMB","DC","PTDOB","PT DO B","PSTU","PCB","PSN","PST","PGT",
 "PAN","PRN","PR","UB","AGIR","UNIAO BRASIL","SEM PARTIDO","BLOCO","LIDERANCA","LIDERANCAS",
 "GOVERNO","OPOSICAO","PARANA","ALEP")
DESCARTE <- paste0("^(LIDER|DEPUTAD|MESA|PRESIDENTE|SECRETARI|VICE|DIRETOR|ANO |PAGINA|CURITIBA|",
                   "EDICAO|SESSAO|LEGISLATURA|DIARIO|ASSEMB|BLOCO|SEM PARTIDO|SUPLENTE|LICENCIAD|",
                   "ANAIS|SUPLEMENTO|ORDINARIA|EXTRAORDINARIA|PROJETO|REQUERIMENTO|OFICIO|",
                   "PARLAMENTAR|PALACIO|PLENARIO|PARTIDO )")
# dia da semana e mes so descartam quando sao a palavra inteira: MARCOS ISFER e nome de gente,
# e um descarte por prefixo apagava o deputado junto com a data do cabecalho
MES_DIA <- paste0("^(SEGUNDA|TERCA|QUARTA|QUINTA|SEXTA|SABADO|DOMINGO|JANEIRO|FEVEREIRO|MARCO|",
                  "ABRIL|MAIO|JUNHO|JULHO|AGOSTO|SETEMBRO|OUTUBRO|NOVEMBRO|DEZEMBRO)( DE | |$)")
CARGOS <- c("LIDER","LIDERANCA","LIDERANCAS","OPOSICAO","GOVERNO","MAIORIA","MINORIA","BLOCO",
            "PRESIDENTE","SECRETARIO","SECRETARIA","VICE")
# cabecalho e data que o extrator de PDF derrama dentro do bloco nominal
LIXO <- paste0("( FEIRA)|( DE ANO )|(^EM DE )|( N CURITIBA)|(^DE )|( ANO X)|(^EM )|",
               "( E )|",                                # dois nomes colados pelo extrator
               "( D[AEO]S?$)|( NA$)|( NO$)|( LIDER$)|( COM$)|",   # particula solta no fim
               "(^D[AEO]S? )|(^NA )|(^NO )|(^E )")      # particula solta no comeco
# vocabulo de tres letras ou mais sem vogal nao e nome de gente: sobra de sigla (PMRB, CDN, PPMDB)
sem_vogal <- function(nn) {
  v <- unlist(stri_split_fixed(nn, " "))
  any(nchar(v) >= 3L & !stri_detect_regex(v, "[AEIOU]")) ||
    (length(v) == 1L && !stri_detect_regex(v, "[AEIOU]"))
}

# devolve uma linha por parlamentar citado no expediente, com partido e anotacao entre parenteses
nomes_bloco <- function(bloco) {
  vazio <- data.table(nome = character(), nome_norm = character(),
                      partido = character(), anotacao = character())
  b <- bloco
  b <- stri_replace_all_regex(b, "\u00AD", "")
  b <- stri_replace_all_regex(b, "(?<=[a-z\u00e0-\u00fc])-\\s*\n\\s*(?=[a-z\u00e0-\u00fc])", "")
  b <- stri_replace_all_regex(b, "\\s*\n\\s*", " ")
  b <- stri_replace_all_regex(b, "[\u201C\u201D\u2018\u2019\"]", "")
  b <- limpa(b)
  for (k in 1:4) b <- stri_replace_all_regex(b, "\\b([A-Z\u00C0-\u00DA]{2,14})(\\s+\\1\\b)+", "$1")
  b <- stri_replace_all_regex(b, "\\b(Dr|Dra|Prof|Profa|Sr|Sra|Cel|Ten|Sgt|Del|Pr|Pe)\\.", "$1\u00A7")
  b <- stri_replace_all_regex(b, "\\bJr\\.", "Jr\u00A7")
  b <- stri_replace_all_regex(b, "\\bJ\u00FAnior\\.", "J\u00FAnior\u00A7")
  b <- stri_replace_all_regex(b, "\\bFilho\\.", "Filho\u00A7")
  b <- stri_replace_all_regex(b, "(?<=\\b[A-Z\u00C0-\u00DA])\\.", "\u00A7")
  pecas <- unlist(stri_split_regex(b, "[;,\\.]|\\s?[-\u2013\u2014]\\s|\\s[-\u2013\u2014]\\s?|:"))
  pecas <- limpa(stri_replace_all_fixed(pecas, "\u00A7", "."))
  pecas <- pecas[nzchar(pecas)]
  res <- vector("list", length(pecas)); partido_corrente <- NA_character_
  for (i in seq_along(pecas)) {
    p <- limpa(stri_replace_all_regex(pecas[i], "^\\d{1,2}\\s*", ""))
    tok <- stri_match_first_regex(asc(toupper(p)), "^([A-Z]{2,14})\\b")
    if (!is.na(tok[1, 1]) && tok[1, 2] %in% SIGLAS) {
      partido_corrente <- tok[1, 2]
      p <- limpa(substr(p, nchar(tok[1, 2]) + 1, nchar(p)))
      p <- limpa(stri_replace_all_regex(p, "^[-\u2013\u2014:]?\\s*\\d{0,2}\\s*[-\u2013\u2014:]?\\s*", ""))
    }
    if (!nzchar(p)) next
    # o layout de 2014 e 2023 cola a sigla ou o cargo de lideranca no fim do nome
    for (k in 1:3) {
      ult <- stri_match_first_regex(asc(toupper(p)), "\\b([A-Z]{2,14})\\s*$")
      if (!is.na(ult[1, 1]) && (ult[1, 2] %in% SIGLAS || ult[1, 2] %in% CARGOS))
        p <- limpa(substr(p, 1, nchar(p) - nchar(ult[1, 1]))) else break
    }
    p <- limpa(stri_replace_all_regex(p, "(?i)\\s*(l[ií]der\\s+d[ao]\\s+\\w+|lideran[cç]as?)\\s*$", ""))
    if (!nzchar(p)) next
    anot <- NA_character_
    m <- stri_match_first_regex(p, "\\(([^)]*)\\)")
    if (!is.na(m[1, 1])) { anot <- limpa(m[1, 2]); p <- limpa(stri_replace_all_regex(p, "\\([^)]*\\)", "")) }
    nn <- norm_nome(p)
    # nome parlamentar curto e comum no PR (Galo, Goura, Litro, Brasil); o corte em cinco
    # letras apagava deputado de verdade, e o filtro de lixo abaixo ja segura o cabecalho
    if (nn %in% SIGLAS || !nzchar(nn) || nchar(nn) < 4) next
    if (grepl(DESCARTE, nn)) next
    if (grepl(MES_DIA, nn)) next
    if (grepl(LIXO, nn)) next
    if (sem_vogal(nn)) next
    if (nchar(nn) > 45 || stri_count_fixed(nn, " ") > 5) next   # bloco da Mesa vazado no nominal
    res[[i]] <- list(nome = p, nome_norm = nn, partido = partido_corrente, anotacao = anot)
  }
  res <- res[!vapply(res, is.null, logical(1))]
  if (!length(res)) return(vazio)
  unique(rbindlist(res), by = "nome_norm")
}

# legislaturas da ALEP na janela do BOCEL (posse em 1 de fevereiro do ano seguinte a eleicao)
LEGS <- data.table(legislatura = as.character(14:20), ano_eleicao = seq(1998L, 2022L, 4L))
LEGS[, `:=`(leg_inicio = as.IDate(sprintf("%d-02-01", ano_eleicao + 1L)),
            leg_fim    = as.IDate(sprintf("%d-01-31", ano_eleicao + 5L)))]

# Le do cache o texto normalizado dos documentos pedidos (ou de todos, se doc_ids for NULL).
# corpus_normalizado.jsonl guarda o inteiro teor do acervo da ALEP e das materias do DIOE;
# cabecalhos.jsonl guarda a pagina 1 de cada edicao, que e onde mora a Representacao Partidaria.
# E a referencia contra a qual verifica_PR.R confere que causa_original existe mesmo na fonte.
ler_corpus <- function(raw, doc_ids = NULL, com_p1 = TRUE) {
  stopifnot(requireNamespace("jsonlite", quietly = TRUE))
  out <- list()
  f <- file.path(raw, "corpus_normalizado.jsonl")
  if (file.exists(f)) {
    L <- readLines(f, warn = FALSE)
    ids <- stri_match_first_regex(substr(L, 1, 160), '"doc_id"\\s*:\\s*"([^"]+)"')[, 2]
    keep <- if (is.null(doc_ids)) rep(TRUE, length(L)) else ids %in% doc_ids
    keep[is.na(ids)] <- FALSE
    if (any(keep)) out[[1]] <- rbindlist(lapply(which(keep), function(k) {
      o <- jsonlite::fromJSON(L[k])
      data.table(doc_id = o$doc_id, texto = o$texto)
    }))
    rm(L)
  }
  if (com_p1) {
    f2 <- file.path(raw, "cabecalhos.jsonl")
    if (file.exists(f2)) {
      L2 <- readLines(f2, warn = FALSE)
      a2 <- stri_match_first_regex(substr(L2, 1, 200), '"arquivo"\\s*:\\s*"([^"]+)"')[, 2]
      k2 <- if (is.null(doc_ids)) rep(TRUE, length(L2)) else a2 %in% doc_ids
      k2[is.na(a2)] <- FALSE
      if (any(k2)) out[[length(out) + 1L]] <- rbindlist(lapply(which(k2), function(k) {
        o <- jsonlite::fromJSON(L2[k])
        data.table(doc_id = o$arquivo,
                   texto = if (is.null(o$texto_p1)) "" else limpa(o$texto_p1))
      }))
    }
  }
  out <- out[lengths(out) > 0]
  if (!length(out)) return(data.table(doc_id = character(), texto = character()))
  x <- rbindlist(out)
  x[, .(texto = paste(unique(texto), collapse = " \u00b6 ")), by = doc_id]
}

# A ata de posse traz a data por extenso ("Aos vinte e cinco dias do mes de maio de dois mil e
# nove"), e ela e a data do ato, nao a da publicacao. Converte para IDate; devolve NA se a
# formula nao aparecer ou se algum vocabulo estiver fora do dicionario.
.UNI <- c(zero=0, um=1, uma=1, hum=1, huma=1, primeiro=1, segundo=2, terceiro=3, quarto=4,
          quinto=5, sexto=6, setimo=7, oitavo=8, nono=9, decimo=10,
          dois=2, duas=2, tres=3, quatro=4, cinco=5, seis=6, sete=7, oito=8, nove=9, dez=10,
          onze=11, doze=12, treze=13, quatorze=14, catorze=14, quinze=15, dezesseis=16,
          dezessete=17, dezoito=18, dezenove=19)
.DEZ <- c(vinte=20, trinta=30, quarenta=40, cinquenta=50, sessenta=60, setenta=70,
          oitenta=80, noventa=90)
.CEM <- c(cem=100, cento=100, duzentos=200, trezentos=300, quatrocentos=400, quinhentos=500,
          seiscentos=600, setecentos=700, oitocentos=800, novecentos=900)
.MESES <- c(janeiro=1, fevereiro=2, marco=3, abril=4, maio=5, junho=6, julho=7, agosto=8,
            setembro=9, outubro=10, novembro=11, dezembro=12)

num_extenso <- function(s) {
  w <- unlist(strsplit(tolower(asc(s)), "[^a-z]+"))
  w <- w[nzchar(w) & w != "e"]
  if (!length(w)) return(NA_integer_)
  tot <- 0; cur <- 0
  for (t in w) {
    if (t %in% names(.UNI)) cur <- cur + .UNI[[t]]
    else if (t %in% names(.DEZ)) cur <- cur + .DEZ[[t]]
    else if (t %in% names(.CEM)) cur <- cur + .CEM[[t]]
    else if (t == "mil") { cur <- if (cur == 0) 1000 else cur * 1000; tot <- tot + cur; cur <- 0 }
    else return(NA_integer_)
  }
  as.integer(tot + cur)
}

# a data que interessa e a do ato mais proximo do trecho citado, e a janela pode conter mais de
# uma ata; por isso fica com a ULTIMA formula antes do ponto de ancoragem
data_por_extenso <- function(txt) {
  m <- stri_match_last_regex(
    txt, paste0("(?i)\\bA[oe]s?\\s+([\\p{L} ]{2,40}?)\\s+dias?\\s+do\\s+m[\u00eae]s\\s+de\\s+(\\p{L}+)",
                "\\s+d[eo]\\s+(?:ano\\s+de\\s+)?([\\p{L} ]{2,60}?)\\s*[,.;]"))
  if (is.na(m[1, 1])) return(as.IDate(NA))
  d <- num_extenso(m[1, 2]); a <- num_extenso(m[1, 4])
  mes <- .MESES[tolower(asc(m[1, 3]))]
  if (is.na(d) || is.na(a) || is.na(mes) || d < 1 || d > 31 || a < 1990 || a > 2030) return(as.IDate(NA))
  suppressWarnings(as.IDate(sprintf("%04d-%02d-%02d", a, mes, d)))
}

# "falecido em 30 de agosto de 1999": dia e ano em algarismo, mes por nome
data_apos <- function(txt, gatilho) {
  m <- stri_match_first_regex(
    txt, paste0("(?i)", gatilho, "\\s+(\\d{1,2})\\s+de\\s+(\\p{L}+)\\s+de\\s+(\\d{4})"))
  if (is.na(m[1, 1])) {
    m2 <- stri_match_first_regex(txt, paste0("(?i)", gatilho, "\\s+(\\d{2})[/.](\\d{2})[/.](\\d{2,4})"))
    if (is.na(m2[1, 1])) return(as.IDate(NA))
    a <- as.integer(m2[1, 4]); if (a < 100) a <- a + if (a > 50) 1900L else 2000L
    return(suppressWarnings(as.IDate(sprintf("%04d-%02d-%02d", a, as.integer(m2[1, 3]), as.integer(m2[1, 2])))))
  }
  mes <- .MESES[tolower(asc(m[1, 3]))]
  if (is.na(mes)) return(as.IDate(NA))
  suppressWarnings(as.IDate(sprintf("%04d-%02d-%02d", as.integer(m[1, 4]), mes, as.integer(m[1, 2]))))
}
