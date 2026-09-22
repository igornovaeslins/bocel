#!/usr/bin/env Rscript
# verifica_MA.R — conferencia independente de data/assembleias2/MA.csv.
#
# O que a checagem cobre:
#   (a) esquema, vocabulario fechado, datas em ISO e integridade referencial contra mandatos.csv;
#   (b) que o texto gravado em causa_original existe LITERALMENTE no cache da fonte (o .txt
#       extraido do PDF do Diario), comparado com espaco em branco normalizado dos dois lados,
#       porque o PDF quebra linha no meio da frase;
#   (c) que a saida antecipada se SUSTENTA NA SERIE: quem recebeu forma de saida antes do fim do
#       mandato tem de aparecer em exercicio na data que se declara como fim, e tem de estar
#       ausente das relacoes completas posteriores, refeitas do zero a partir do cache;
#   (d) que fim_regular tem evidencia de permanencia, e nao mera ausencia de saida detectada;
#   (e) que nenhuma licenca marcada como saida tem a pessoa de volta ao exercicio depois.
#
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_MA.R
suppressPackageStartupMessages({library(data.table); library(stringi)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))
script <- "R/assembleias2/verifica_MA.R"
verd <- "output/verificacao"
DIA <- "data_raw/assembleias2/MA/diarios"
UF <- "MA"; CARGO <- "7"; CADEIRAS <- 42L; HOJE <- as.IDate("2026-08-30")
LEG_ANO <- c("14" = 1998L, "15" = 2002L, "16" = 2006L, "17" = 2010L,
             "18" = 2014L, "19" = 2018L, "20" = 2022L)
LEG_FIM <- setNames(as.IDate(c("2003-01-31", "2007-01-31", "2011-01-31", "2015-01-31",
                               "2019-01-31", "2023-01-31", "2027-01-31")), names(LEG_ANO))
reg <- function(k, v) registrar_numero(paste0("asm2ma_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")
nz <- function(x) !is.na(x) & nzchar(x)
esp <- function(x) gsub("\\s+", " ", trimws(x))
nn <- function(x) {
  x <- stri_trans_general(toupper(ifelse(is.na(x), "", x)), "Latin-ASCII")
  x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x))
}

pass <- 0L; fail <- 0L; falhas <- character()
ok <- function(nome, cond) {
  r <- isTRUE(tryCatch(all(cond), error = function(e) FALSE))
  if (r) pass <<- pass + 1L else { fail <<- fail + 1L; falhas <<- c(falhas, nome) }
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
  r
}

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")

x <- fread("data/assembleias2/MA.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")

cat("\n== esquema e vocabulario\n")
ok("21 colunas na ordem canonica", identical(names(x), COLS))
ok("uf sempre MA", all(x$uf == UF))
ok("forma_saida no vocabulario fechado", all(x$forma_saida %in% VOCAB))
ok("condicao em titular/suplente/nao_informado", all(x$condicao %in% c("titular", "suplente", "nao_informado")))
ok("ano_eleicao nos sete pleitos do BOCEL", all(as.integer(x$ano_eleicao) %in% LEG_ANO))
ok("legislatura consistente com o ano de eleicao",
   all(LEG_ANO[x$legislatura] == as.integer(x$ano_eleicao)))
iso <- function(v) is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v)
ok("datas em ISO", all(iso(x$data_inicio_exercicio)) && all(iso(x$data_fim_exercicio)) &&
     all(iso(x$data_nascimento)))
ok("inicio nao posterior ao fim",
   x[nz(data_inicio_exercicio) & nz(data_fim_exercicio), all(data_inicio_exercicio <= data_fim_exercicio)])
ok("toda linha traz url de origem", all(nz(x$url)))
ok("uma linha por pessoa e legislatura", !anyDuplicated(x[, .(ano_eleicao, nome_normalizado)]))

cat("\n== integridade referencial com o BOCEL\n")
par <- x[nz(id_mandato_bocel)]
ok("todo id_mandato_bocel existe em mandatos.csv", all(par$id_mandato_bocel %in% m$id_mandato))
i <- match(par$id_mandato_bocel, m$id_mandato)
ok("mandato pareado e deputado estadual do MA", all(m$cd_cargo[i] == CARGO & m$sg_uf[i] == UF))
ok("ano de eleicao do mandato bate com o da coleta", all(m$ano_eleicao[i] == par$ano_eleicao))
ok("nenhum mandato pareado duas vezes", !anyDuplicated(par$id_mandato_bocel))
jan <- is.na(par$data_fim_exercicio) |
  (as.IDate(par$data_fim_exercicio) >= as.IDate(m$mandato_inicio[i]) - 60L &
     as.IDate(par$data_fim_exercicio) <= as.IDate(m$mandato_fim[i]) + 45L)
ok("data de fim dentro da janela do mandato", all(jan))
ok("linha de suplente nao carrega mandato do BOCEL", x[condicao == "suplente", all(!nz(id_mandato_bocel))])

cat("\n== regime de evidencia\n")
ok("ato nomeado sempre traz o texto da fonte", x[forma_saida %in% TEXTUAL, all(nz(causa_original))])
ok("forma sem texto so nos rotulos derivaveis da estrutura",
   x[!nz(causa_original) & !forma_saida %in% c("nao_observado", "fim_regular"),
     all(forma_saida %in% ESTRUTURAL)])
ok("mandato em curso (2022) nunca recebe fim_regular",
   x[ano_eleicao == "2022", all(forma_saida != "fim_regular")])

## ---------------------------------------------------------------- serie refeita do zero
cat("\n== serie de exercicio refeita a partir do cache\n")
rel <- fread(file.path(DIA, "relacao_nominal.csv"), colClasses = "character",
             na.strings = c("NA", ""), encoding = "UTF-8")
rel[, data := as.IDate(data)]
rel[, arq_pdf := sub("\\.txt$", "", arquivo)]
wbf <- "data_raw/assembleias2/MA/wayback/relacao_wayback.csv"
if (file.exists(wbf)) {
  wb <- fread(wbf, colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
  wb[, `:=`(data = as.IDate(data), arq_pdf = arquivo)]
  rel <- rbind(rel, wb, use.names = TRUE, fill = TRUE)
}
rel <- rel[!is.na(data)]
rel[, nome_norm := nn(nome)]
LEG_INI <- setNames(as.IDate(c("1999-02-01", "2003-02-01", "2007-02-01", "2011-02-01",
                               "2015-02-01", "2019-02-01", "2023-02-01")), names(LEG_ANO))
rel[, leg := NA_character_]
for (k in names(LEG_INI)) rel[data >= LEG_INI[[k]] & data <= LEG_FIM[[k]], leg := k]
rel <- rel[!is.na(leg)]
## grafias que a montagem tratou como a mesma pessoa (o censo conta PESSOAS, nao linhas)
vg0 <- if (file.exists(file.path(verd, "asm2ma_variantes_agrupadas.csv")))
  fread(file.path(verd, "asm2ma_variantes_agrupadas.csv"), colClasses = "character") else
    data.table(leg = character(), variante = character(), canonico = character())
rel <- merge(rel, vg0[, .(leg, nome_norm = variante, canonico)], by = c("leg", "nome_norm"), all.x = TRUE)
rel[, nome_canon := fifelse(is.na(canonico), nome_norm, canonico)]
censo <- rel[situacao == "exercicio", .(n = uniqueN(nome_canon)), by = .(leg, data)][n == CADEIRAS]
cat("datas com relacao completa de ", CADEIRAS, " nomes: ", nrow(censo), "\n", sep = "")
cob <- rel[, .(cob_ini = min(data), cob_fim = max(data)), by = leg]

## grafias que a montagem tratou como a mesma pessoa
vg <- vg0
grafias <- function(leg_i, nome_norm) {
  v <- unique(c(nome_norm, vg[leg == leg_i & canonico == nome_norm]$variante,
                vg[leg == leg_i & variante == nome_norm]$canonico))
  g <- vg[leg == leg_i & canonico %in% v]$variante
  unique(c(v, g))
}
presenca <- function(leg_i, nome_norm, dts) {
  gs <- grafias(leg_i, nome_norm)
  rel[leg == leg_i & situacao == "exercicio" & nome_norm %in% gs & data %in% dts, .N] > 0L
}

cat("\n-- saida antecipada conferida contra a serie\n")
SAIDA <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "outro")
ant <- x[forma_saida %in% SAIDA]
lin <- rbindlist(lapply(seq_len(nrow(ant)), function(k) {
  r <- ant[k]; l <- r$legislatura
  dts <- censo[leg == l]$data
  fim <- if (nz(r$data_fim_exercicio)) as.IDate(r$data_fim_exercicio) else NA
  gs <- grafias(l, r$nome_normalizado)
  visto_no_fim <- if (is.na(fim)) NA else
    rel[leg == l & situacao == "exercicio" & nome_norm %in% gs & data == fim, .N] > 0L
  depois <- if (is.na(fim)) dts else dts[dts > fim]
  ausente_depois <- if (!length(depois)) 0L else
    sum(!vapply(depois, function(d) presenca(l, r$nome_normalizado, d), TRUE))
  data.table(legislatura = l, nome = r$nome, forma_saida = r$forma_saida,
             data_fim = r$data_fim_exercicio, censos_depois = length(depois),
             ausente_em = ausente_depois, visto_na_data_de_fim = visto_no_fim)
}))
fwrite(lin, file.path(verd, "asm2ma_saida_sustentada_na_serie.csv"))
cat("linhas com saida antecipada: ", nrow(lin), "\n", sep = "")
if (nrow(lin)) {
  ok("quem tem data de fim aparece em exercicio nessa data",
     lin[!is.na(visto_na_data_de_fim), all(visto_na_data_de_fim)])
  ok("quem saiu esta ausente de TODAS as relacoes completas posteriores",
     lin[censos_depois > 0L, all(ausente_em == censos_depois)])
  ok("toda saida antecipada com data de fim tem ao menos tres relacoes completas depois dela",
     lin[!is.na(data_fim), all(censos_depois >= 3L)])
  ok("saida sem data de fim so onde ha ao menos duas relacoes completas na legislatura",
     lin[is.na(data_fim), all(censos_depois >= 2L)])
  # na legislatura em curso a serie esta aberta: ausencia curta nao e saida
  cor <- names(LEG_FIM)[LEG_FIM >= HOJE]
  lin[, em_curso := legislatura %in% as.integer(cor) | as.character(legislatura) %in% cor]
  lin[, dias_ate_fim_da_serie := as.integer(cob[match(as.character(legislatura), leg)]$cob_fim -
                                              as.IDate(data_fim))]
  ok("na legislatura em curso, a saida exige 90 dias de ausencia ate o fim da serie",
     lin[em_curso == TRUE & !is.na(data_fim), all(dias_ate_fim_da_serie >= 90L)])
} else {
  cat("(sem saida antecipada nesta versao)\n")
}

cat("\n-- fim_regular conferido contra a serie\n")
# A prova de permanencia e posicional, e nao em dias: a ultima sessao ordinaria e em dezembro e
# as edicoes de janeiro sao administrativas, entao quem ficou ate o fim aparece semanas antes do
# ultimo Diario. Exige-se presenca em uma das cinco ultimas relacoes COMPLETAS da legislatura.
ULT_K <- 5L
fr <- x[forma_saida == "fim_regular"]
lin2 <- rbindlist(lapply(seq_len(nrow(fr)), function(k) {
  r <- fr[k]; l <- r$legislatura
  cf <- cob[leg == l]$cob_fim
  u <- tail(sort(censo[leg == l]$data), ULT_K)
  gs <- grafias(l, r$nome_normalizado)
  ult <- suppressWarnings(rel[leg == l & situacao == "exercicio" & nome_norm %in% gs, max(data)])
  data.table(legislatura = l, nome = r$nome, cob_fim = cf, ultima_presenca = ult,
             nas_ultimas_completas = rel[leg == l & situacao == "exercicio" & nome_norm %in% gs &
                                           data %in% u, .N] > 0L,
             dias_serie_ate_fim_da_legislatura = as.integer(LEG_FIM[[l]] - cf))
}))
fwrite(lin2, file.path(verd, "asm2ma_fim_regular_sustentado.csv"))
if (nrow(lin2)) {
  ok("fim_regular so onde a serie alcanca o fim da legislatura",
     lin2[, all(dias_serie_ate_fim_da_legislatura <= 60L)])
  ok("fim_regular exige presenca numa das cinco ultimas relacoes completas",
     lin2[, all(nas_ultimas_completas)])
}

cat("\n-- licenca marcada como saida nao tem volta ao exercicio\n")
lc <- x[forma_saida %in% c("licenca", "afastamento")]
volta <- vapply(seq_len(nrow(lc)), function(k) {
  r <- lc[k]; l <- r$legislatura
  gs <- grafias(l, r$nome_normalizado)
  d_lic <- rel[leg == l & situacao == "licenciado" & nome_norm %in% gs, suppressWarnings(max(data))]
  if (!is.finite(d_lic)) return(FALSE)
  rel[leg == l & situacao == "exercicio" & nome_norm %in% gs & data > d_lic, .N] > 0L
}, TRUE)
cat("licencas/afastamentos: ", nrow(lc), " | com volta ao exercicio depois: ", sum(volta), "\n", sep = "")
ok("nenhuma licenca de saida tem a pessoa de volta ao exercicio", !any(volta))

## ---------------------------------------------------------------- texto literal no cache
cat("\n== causa_original conferida contra o texto em cache\n")
cz <- x[nz(causa_original)]
lit <- rbindlist(lapply(seq_len(nrow(cz)), function(k) {
  r <- cz[k]
  cand <- c(file.path(DIA, "capa_ordenada", paste0(r$id_fonte, ".txt")),
            file.path(DIA, "txt", paste0(r$id_fonte, ".txt")),
            file.path("data_raw/assembleias2/MA/wayback", r$id_fonte))
  cand <- cand[file.exists(cand)]
  if (!length(cand))
    return(data.table(nome = r$nome, id_fonte = r$id_fonte, achou_arquivo = 0L, literal = 0L, onde = NA_character_))
  alvo_txt <- esp(r$causa_original)
  achou <- NA_character_
  for (f in cand) {
    t <- esp(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = " "))
    if (grepl(alvo_txt, t, fixed = TRUE)) { achou <- basename(dirname(f)); break }
  }
  data.table(nome = r$nome, id_fonte = r$id_fonte, achou_arquivo = 1L,
             literal = as.integer(!is.na(achou)), onde = achou)
}))
fwrite(lit, file.path(verd, "asm2ma_causa_literal.csv"))
cat("linhas com causa_original: ", nrow(cz), " | arquivo em cache: ", sum(lit$achou_arquivo),
    " | texto literal: ", sum(lit$literal), "\n", sep = "")
ok("todo texto de causa_original vem de um arquivo em cache", nrow(lit) == 0L || all(lit$achou_arquivo == 1L))
ok("todo texto de causa_original aparece literalmente no cache", nrow(lit) == 0L || all(lit$literal == 1L))

## ---------------------------------------------------------------- numeros
cat("\n== numeros\n")
obs <- x[forma_saida != "nao_observado" & nz(id_mandato_bocel)]
dep_n <- m[cd_cargo == CARGO & sg_uf == UF, .N]
reg("v_n_linhas", nrow(x))
reg("v_n_pareadas", nrow(par))
reg("v_n_mandatos_com_forma", uniqueN(obs$id_mandato_bocel))
reg("v_taxa_forma_observada", sprintf("%d/%d=%.4f", uniqueN(obs$id_mandato_bocel), dep_n,
                                      uniqueN(obs$id_mandato_bocel) / dep_n))
reg("v_n_saida_antecipada", nrow(lin))
reg("v_n_causa_literal_ok", if (nrow(lit)) sum(lit$literal) else 0L)
reg("v_n_datas_censo_completo", nrow(censo))
reg("v_n_checagens", pass + fail)
reg("v_n_aprovadas", pass)
reg("v_n_reprovadas", fail)

gravar_relatorio_verificacao(
  alvo = "data/assembleias2/MA.csv", script = script,
  passou = c(sprintf("%d de %d checagens aprovadas", pass, pass + fail),
             sprintf("%d mandatos de %d com forma de saida observada", uniqueN(obs$id_mandato_bocel), dep_n),
             sprintf("%d datas de Diario com a relacao nominal completa (%d nomes)", nrow(censo), CADEIRAS),
             "causa_original conferida literalmente contra o texto extraido do PDF em cache",
             "saida antecipada conferida contra a serie refeita do zero"),
  fora_de_cobertura = c(
    "veracidade do que a Casa publica no expediente do Diario (a camada confere a copia, nao o fato)",
    "causa da saida quando o Diario nao publica ato: a linha fica 'outro', e nao renuncia presumida",
    "lacuna de fonte de 02/1999 a 04/2001, de 04/2002 a 02/2004 e de 12/2006 a 02/2007: o acervo do Diario comeca em 12/04/2004 e do portal antigo so restaram quatro capturas (04/2001, 03/2002, 03/2004 e 11/2005), de modo que a 14a legislatura tem apenas duas datas e a 15a comeca 13 meses depois da posse",
    "edicoes sem camada de texto no PDF (contadas em data_raw/assembleias2/MA/diarios/sem_texto.csv); nao ha Tesseract neste ambiente",
    "homonimia no pareamento por nome, onde a fonte nao publica identificador da candidatura"))

cat("\nverifica_MA: PASSOU ", pass, " | FALHOU ", fail, "\n", sep = "")
if (fail) { cat("FALHAS:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); quit(status = 1) }
