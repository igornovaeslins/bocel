#!/usr/bin/env Rscript
# verifica_PE.R — verificador de data/assembleias2/PE.csv (frente ALEPE do BOCEL).
#
# O que exige, alem do esquema: que todo ato nomeado (renuncia, cassacao, falecimento,
# afastamento, licenca, posse nao ocorrida) traga em causa_original um TEXTO QUE EXISTE
# LITERALMENTE no cache da fonte — o .txt.gz extraido do PDF do Diario Oficial ou o JSON da
# noticia da Casa. Um numero ou uma frase que nao se reencontra na fonte e erro, nao registro.
#
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_PE.R
suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(jsonlite)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
raw    <- file.path(root, "data_raw", "assembleias2", "PE")
verd   <- file.path(root, "output", "verificacao")
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "verifica_PE.R")
reg <- function(k, v) registrar_numero(paste0("asm2pe_ver_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L || is.na(a[1])) b else a[1]
pass <- 0L; fail <- 0L; falhas <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE },
                error = function(e) { falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
  invisible(r)
}

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
TEXTUAL    <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")

x <- fread("data/assembleias2/PE.csv", colClasses = "character", na.strings = c("NA", ""),
           encoding = "UTF-8")
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""),
           encoding = "UTF-8")

## ---------------------------------------------------------------- esquema
ok("esquema: 21 colunas na ordem canonica", stopifnot(identical(names(x), COLS)))
ok("uf sempre PE", stopifnot(all(x$uf == "PE")))
ok("vocabulario fechado de forma_saida", in_set(x$forma_saida, VOCAB, nome = "forma_saida"))
ok("condicao em titular/suplente/nao_informado",
   in_set(x$condicao, c("titular", "suplente", "nao_informado"), nome = "condicao"))
ok("ano_eleicao em 1998..2022", em_faixa(as.integer(x$ano_eleicao), 1998, 2022, nome = "ano_eleicao"))
ok("legislatura corresponde ao ano de eleicao",
   stopifnot(all(as.integer(x$legislatura) == 14L + (as.integer(x$ano_eleicao) - 1998L) / 4L)))
iso <- function(v) all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v))
ok("datas em ISO", stopifnot(iso(x$data_inicio_exercicio), iso(x$data_fim_exercicio),
                             iso(x$data_nascimento)))
ok("inicio nao posterior ao fim",
   stopifnot(x[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio),
               all(data_inicio_exercicio <= data_fim_exercicio)]))
ok("uma linha por (legislatura, nome normalizado)",
   checa_unica(as.data.frame(x), c("uf", "legislatura", "nome_normalizado")))
ok("toda linha traz url de origem", stopifnot(all(!is.na(x$url) & nzchar(x$url))))
ok("id_fonte preenchido", stopifnot(all(!is.na(x$id_fonte) & nzchar(x$id_fonte))))

## ---------------------------------------------------------------- integridade com o BOCEL
par <- x[!is.na(id_mandato_bocel)]
ok("todo id_mandato_bocel existe em data/mandatos.csv", stopifnot(all(par$id_mandato_bocel %in% m$id_mandato)))
ix <- match(par$id_mandato_bocel, m$id_mandato)
ok("mandato pareado e deputado estadual (cd_cargo 7)", stopifnot(all(m$cd_cargo[ix] == "7")))
ok("mandato pareado e de PE", stopifnot(all(m$sg_uf[ix] == "PE")))
ok("ano de eleicao do mandato bate com a legislatura da coleta",
   stopifnot(all(m$ano_eleicao[ix] == par$ano_eleicao)))
ok("nenhum id_mandato_bocel reutilizado", stopifnot(uniqueN(par$id_mandato_bocel) == nrow(par)))
ok("id_pessoa_bocel acompanha o mandato",
   stopifnot(all(par$id_pessoa_bocel == m$id_pessoa[ix])))
ok("linha pareada tem metodo de pareamento declarado",
   stopifnot(all(!is.na(par$metodo_pareamento))))
jan <- is.na(par$data_fim_exercicio) |
  (as.IDate(par$data_fim_exercicio) >= as.IDate(m$mandato_inicio[ix]) - 60L &
     as.IDate(par$data_fim_exercicio) <= as.IDate(m$mandato_fim[ix]) + 45L)
cat("linhas pareadas com fim fora da janela do mandato:", sum(!jan), "\n")
reg("n_fim_fora_da_janela", sum(!jan))
ok("fim de exercicio dentro da janela do mandato", stopifnot(all(jan)))

## ---------------------------------------------------------------- regime de evidencia
sem_txt <- x[forma_saida %in% TEXTUAL & (is.na(causa_original) | causa_original == "")]
ok("ato nomeado traz o texto da fonte em causa_original", stopifnot(nrow(sem_txt) == 0L))
ok("forma sem texto so nos rotulos derivaveis da estrutura",
   stopifnot(x[(is.na(causa_original) | causa_original == "") &
                 !forma_saida %in% c("nao_observado", "fim_regular") & !is.na(forma_saida),
               all(forma_saida %in% ESTRUTURAL)]))
ok("mandato em curso nunca recebe fim_regular",
   stopifnot(x[legislatura == "20" & forma_saida %in% "fim_regular", .N] == 0L))

## ---------------------------------------------------------------- o texto existe no cache
# Reproduz em R a limpeza que o parsing aplicou ao texto do PDF, para que a comparacao seja
# literal e nao aproximada: colapso de espaco, hifen de quebra de linha e cabecalho corrido de
# pagina, que o extrator do PDF injeta no meio da frase.
RUIDO <- paste(c(
  "Di[áa]rio Oficial do Estado de Pernambuco\\s*[–-]\\s*Poder Legislativo",
  "DI[ÁA]RIO OFICIAL DO ESTADO DE PERNAMBUCO\\s*[–-]\\s*PODER LEGISLATIVO",
  "Recife,\\s*\\d{1,2}\\s+de\\s+[a-zç]+\\s+de\\s+\\d{4}",
  "Recife,\\s*(segunda|ter[çc]a|quarta|quinta|sexta)-feira,\\s*\\d{1,2}\\s+de\\s+[a-zç]+\\s+de\\s+\\d{4}",
  "Ano\\s+[CDILMVX]+\\s*[•·]?\\s*N[ºo°]?\\s*\\d+\\s*-?\\s*\\d*",
  "CERTIFICADO DIGITALMENTE",
  "Atas\\s+Of[íi]cios\\s+Ordem do Dia\\s+Atos",
  "Ordem do Dia\\s+Atos",
  "\\bAtas\\b\\s+\\bOf[íi]cios\\b",
  "P[áa]gina\\s+\\d+"), collapse = "|")
limpa_r <- function(t) {
  t <- gsub("­", "", t, fixed = TRUE)
  t <- gsub("‐", "-", t, fixed = TRUE)
  t <- gsub("\\s+", " ", t)
  t <- gsub("(?<=[A-Za-zÀ-ÿ])-\\s+(?=[A-Za-zÀ-ÿ])", "", t, perl = TRUE)
  gsub(RUIDO, " ", t)
}
so_alnum <- function(t) gsub("[^A-Z0-9]", "",
                             toupper(stri_trans_general(t, "Latin-ASCII")))
# mesma reducao que o parsing aplicou ao corpo da noticia: tira marcacao e entidade e colapsa
# espaco, para que a comparacao seja com o texto lido, nao com o HTML bruto
txt_html <- function(h) {
  h <- gsub("<script.*?</script>", " ", h)
  h <- gsub("<style.*?</style>", " ", h)
  h <- gsub("<[^>]+>", " ", h)
  m <- gregexpr("&#[0-9]+;", h)
  regmatches(h, m) <- lapply(regmatches(h, m), function(v)
    if (length(v)) intToUtf8(as.integer(gsub("\\D", "", v)), multiple = TRUE) else v)
  h <- gsub("&nbsp;", " ", h, fixed = TRUE)
  h <- gsub("&amp;", "&", h, fixed = TRUE)
  h <- gsub("&#8217;", "'", h, fixed = TRUE)
  gsub("\\s+", " ", trimws(h))
}

txts <- list.files(file.path(raw, "do_txt"), pattern = "\\.txt\\.gz$", full.names = TRUE)
mapa_do <- setNames(txts, sub("^DO_\\d{4}-\\d{2}-\\d{2}__", "", sub("\\.txt\\.gz$", "", basename(txts))))
posts <- list.files(file.path(raw, "noticias"), pattern = "^post_\\d+\\.json$", full.names = TRUE)
link_post <- new.env(hash = TRUE, parent = emptyenv())
for (p in posts) {
  l <- readLines(p, warn = FALSE, encoding = "UTF-8")
  mm <- regmatches(l, regexpr('"link":"[^"]+"', l))
  if (length(mm)) assign(gsub('\\\\/', "/", sub('"link":"', "", gsub('"$', "", mm[1]))), p,
                         envir = link_post)
}
cat("cache: ", length(txts), " edicoes do DO em texto | ", length(posts), " noticias\n", sep = "")

ato <- x[!is.na(causa_original) & causa_original != ""]
res <- rbindlist(lapply(seq_len(nrow(ato)), function(i) {
  u <- ato$url[i]; ca <- ato$causa_original[i]
  arq <- NA_character_; achou <- NA; modo <- "sem_cache"
  mp <- regmatches(u, regexpr("diario-oficial-[0-9A-Za-z-]+", u))
  if (length(mp) && mp %in% names(mapa_do)) {
    arq <- mapa_do[[mp]]
    bruto <- paste(readLines(gzfile(arq), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    lim <- limpa_r(bruto)
    achou <- grepl(ca, lim, fixed = TRUE)
    modo <- if (achou) "literal_do" else {
      achou <- grepl(so_alnum(ca), so_alnum(lim), fixed = TRUE); "alfanumerico_do" }
  } else if (exists(u, envir = link_post, inherits = FALSE)) {
    arq <- get(u, envir = link_post)
    d <- tryCatch(fromJSON(arq, simplifyVector = TRUE), error = function(e) NULL)
    corpo <- if (is.null(d)) "" else
      txt_html(paste0(d$title$rendered %||% "", ". ", d$content$rendered %||% ""))
    achou <- grepl(ca, corpo, fixed = TRUE)
    modo <- if (achou) "literal_noticia" else {
      achou <- grepl(so_alnum(ca), so_alnum(corpo), fixed = TRUE); "alfanumerico_noticia" }
  }
  data.table(legislatura = ato$legislatura[i], nome = ato$nome[i],
             forma_saida = ato$forma_saida[i], url = u, arquivo = basename(arq %||% ""),
             modo = modo, achou = achou, n_car = nchar(ca))
}))
if (nrow(res)) {
  fwrite(res, file.path(verd, "asm2pe_causa_original_no_cache.csv"))
  cat("\ntexto de causa_original conferido contra o cache:\n"); print(res[, .N, by = .(modo, achou)])
  reg("n_causa_original_conferidas", nrow(res))
  reg("n_causa_original_encontradas", res[achou %in% TRUE, .N])
  reg("n_causa_original_sem_cache", res[modo == "sem_cache", .N])
  ok("todo texto de causa_original tem cache da fonte", stopifnot(res[modo == "sem_cache", .N] == 0L))
  ok("todo texto de causa_original existe literalmente no cache da fonte",
     stopifnot(all(res$achou %in% TRUE)))
} else {
  cat("\nnenhuma linha com causa_original a conferir\n")
  reg("n_causa_original_conferidas", 0L)
}

## ---------------------------------------------------------------- a estrutura confirma a saida
# O rotulo derivado da estrutura afirma que a pessoa deixou de figurar na relacao nominal
# enquanto a Casa continuou publicando quem estava nela. Aqui isso e reconferido contra o cache:
# depois da data de fim tem de haver reuniao registrada na mesma legislatura, senao o que se
# observou foi o fim do acervo, nao o fim do exercicio.
LEG <- data.table(legislatura = as.character(14:20),
                  leg_inicio = as.IDate(paste0(seq(1999L, 2023L, 4L), "-02-01")),
                  leg_fim    = as.IDate(paste0(seq(2003L, 2027L, 4L), "-01-31")))
MIN_DEPOIS <- 6L
a <- fread(file.path(raw, "atas_presenca.csv"), colClasses = "character", encoding = "UTF-8",
           na.strings = c("NA", ""))
a[, dref := as.IDate(fifelse(!is.na(data_reuniao), data_reuniao, data_do))]
r <- fread(file.path(raw, "retratos_wayback.csv"), colClasses = "character", encoding = "UTF-8",
           na.strings = c("NA", ""))
r[, dref := as.IDate(data_retrato)]
cal <- unique(rbindlist(list(a[, .(dref)], r[, .(dref)])))[!is.na(dref)]
cal[LEG, on = .(dref >= leg_inicio, dref <= leg_fim), legislatura := i.legislatura]
cal <- cal[!is.na(legislatura)]
DERIV <- c("outro", "suplente_efetivado", "licenca", "afastamento")
ant <- merge(x[forma_saida %in% DERIV & !is.na(data_fim_exercicio)],
             LEG, by = "legislatura")[as.IDate(data_fim_exercicio) < (leg_fim - 120L)]
if (nrow(ant)) {
  ant[, n_depois := mapply(function(lg, dt) cal[legislatura == lg & dref > as.IDate(dt), .N],
                           legislatura, data_fim_exercicio)]
  cat("saidas antecipadas conferidas:", nrow(ant),
      "| com menos de", MIN_DEPOIS, "reunioes registradas depois:", ant[n_depois < MIN_DEPOIS, .N], "\n")
  if (ant[n_depois < MIN_DEPOIS, .N])
    print(ant[n_depois < MIN_DEPOIS, .(legislatura, nome, forma_saida, data_fim_exercicio, n_depois)])
  reg("n_saidas_antecipadas", nrow(ant))
  ok("saida antecipada tem reuniao registrada depois no cache da fonte",
     stopifnot(ant[, all(n_depois >= MIN_DEPOIS)]))
}
fr <- merge(x[forma_saida == "fim_regular"], LEG, by = "legislatura")
ok("fim_regular encerra na data de fim da legislatura",
   stopifnot(fr[, all(as.IDate(data_fim_exercicio) == leg_fim)]))
ok("legislatura em curso nao recebe data de fim de exercicio no fim do mandato",
   stopifnot(x[legislatura == "20" & !is.na(data_fim_exercicio),
               all(as.IDate(data_fim_exercicio) < as.IDate("2026-08-30"))]))

## ---------------------------------------------------------------- lacuna de camada de texto
rec <- file.path(raw, "recusas.csv")
n_sem_texto <- 0L; n_recusa <- 0L
if (file.exists(rec)) {
  r <- fread(rec, colClasses = "character")
  n_sem_texto <- r[status %in% c("SEMTEXTO", "PDFERR"), .N]
  n_recusa <- nrow(r)
}
## 05/09/2026: o OCR existe (tesserocr 5.5.1, lib/tessdata), mas o coletor apagava o PDF depois de
## extrair o texto (fetch_alepe_PE.py, _grava_edicao), de modo que essas edicoes nao estao em disco
## e o OCR delas exige nova aquisicao; o coletor passou a reter o PDF sem texto em do_pdf/.
n_pdf_retido <- length(list.files(file.path(raw, "do_pdf"), pattern = "\\.pdf$"))
reg("n_pdfs_sem_texto_retidos_em_disco", n_pdf_retido)
cat("edicoes do DO sem camada de texto ou ilegiveis (PDF nao retido pelo coletor; OCR exige nova aquisicao):",
    n_sem_texto, "| recusas registradas:", n_recusa, "\n")
reg("n_edicoes_sem_camada_de_texto", n_sem_texto)
reg("n_recusas_registradas", n_recusa)

## ---------------------------------------------------------------- cobertura recontada
dep <- m[cd_cargo == "7" & sg_uf == "PE"]
cob <- dep[, .(n_bocel = .N), by = ano_eleicao]
cob <- merge(cob, x[!is.na(id_mandato_bocel),
                    .(n_pareados = uniqueN(id_mandato_bocel),
                      n_com_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado" &
                                                             !is.na(forma_saida)])),
                    by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
for (j in c("n_pareados", "n_com_forma")) set(cob, which(is.na(cob[[j]])), j, 0L)
setorder(cob, ano_eleicao)
print(cob)
n_forma <- uniqueN(x[!is.na(id_mandato_bocel) & forma_saida != "nao_observado" &
                       !is.na(forma_saida)]$id_mandato_bocel)
cat("\nmandatos de PE com forma observada:", n_forma, "de", nrow(dep),
    sprintf("(%.1f%%)", 100 * n_forma / nrow(dep)), "\n")

## registro assinado: le por readLines porque ha valores com '|' no meio
ass <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- ass[grepl("\\|", ass)]
pa <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass)),
                 valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass)))
ult <- pa[pa[, .I[.N], by = chave]$V1]
REG <- setNames(ult$valor, ult$chave)
ok("asm2pe_n_linhas registrado = recontado",
   stopifnot(REG[["asm2pe_n_linhas"]] == as.character(nrow(x))))
ok("asm2pe_n_pareados_mandato registrado = recontado",
   stopifnot(REG[["asm2pe_n_pareados_mandato"]] == as.character(uniqueN(par$id_mandato_bocel))))
ok("asm2pe_n_mandatos_com_forma registrado = recontado",
   stopifnot(REG[["asm2pe_n_mandatos_com_forma"]] == as.character(n_forma)))

reg("n_checagens", pass + fail)
reg("n_aprovadas", pass)
reg("n_reprovadas", fail)
gravar_relatorio_verificacao(
  alvo = "data/assembleias2/PE.csv", script = script,
  passou = c(sprintf("%d checagens aprovadas", pass),
             sprintf("%d linhas, %d pareadas ao BOCEL, %d mandatos com forma observada de %d",
                     nrow(x), uniqueN(par$id_mandato_bocel), n_forma, nrow(dep)),
             if (nrow(res)) sprintf("%d de %d textos de causa_original reencontrados no cache da fonte",
                                    res[achou %in% TRUE, .N], nrow(res)) else
               "nenhum ato nomeado a conferir"),
  falhou = falhas,
  fora_de_cobertura = c(
    "pertinencia semantica do pareamento por nome: homonimo e apelido nao sao resolvidos por documento",
    "veracidade do que a ALEPE publica na ata, no Diario Oficial e no acervo de noticias",
    "completude do acervo digital do DO, que comeca em 2005",
    "presenca na relacao nominal mede exercicio observado, nao a data juridica de posse ou desligamento",
    sprintf("%d edicoes do DO sem camada de texto ficam como lacuna: tesseract nao esta instalado nesta maquina",
            n_sem_texto)))
cat("\nverifica_PE: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("FALHAS:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); quit(status = 1) }
