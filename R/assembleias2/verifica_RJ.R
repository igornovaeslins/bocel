#!/usr/bin/env Rscript
# verifica_RJ.R — verificador proprio da coleta da ALERJ (data/assembleias2/RJ.csv).
# Re-executa as contas a partir do arquivo gravado e do cache bruto, sem confiar em nenhum numero
# narrado: cada checagem recomputa o que afirma. Relatorio em output/verificacao/.
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_RJ.R
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/assembleias2/verifica_RJ.R"
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)

pass <- 0L; fail <- 0L; falhas <- character(); linhas_rel <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE },
                error = function(e) { falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) pass <<- pass + 1L else fail <<- fail + 1L
  linhas_rel <<- c(linhas_rel, paste0(if (r) "PASS  " else "FALHA ", nome))
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
}

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")
LEG_ANO <- c(`7` = 1998L, `8` = 2002L, `9` = 2006L, `10` = 2010L, `11` = 2014L, `12` = 2018L, `13` = 2022L)
RAW <- "data_raw/assembleias2/RJ"; PAR <- file.path(RAW, "parsed")

x <- fread("data/assembleias2/RJ.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
mrj <- m[cd_cargo == "7" & sg_uf == "RJ"]
bib <- fread(file.path(PAR, "biblioteca_composicao.csv"), colClasses = "character", na.strings = "")
qs  <- fread(file.path(PAR, "quemsao_composicao.csv"),   colClasses = "character", na.strings = "")

## ------------------------------------------------------------------ esquema
ok("21 colunas na ordem canonica", stopifnot(identical(names(x), COLS)))
ok("uf sempre RJ", stopifnot(all(x$uf == "RJ")))
ok("vocabulario fechado de forma_saida", in_set(x$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida"))
ok("condicao em titular/suplente/nao_informado",
   in_set(x$condicao, c("titular", "suplente", "nao_informado"), permitir_na = FALSE, nome = "condicao"))
ok("legislatura de 7 a 13", in_set(x$legislatura, as.character(7:13), permitir_na = FALSE, nome = "legislatura"))
ok("ano_eleicao em 1998..2022", em_faixa(as.integer(x$ano_eleicao), 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao"))
ok("ano_eleicao e a legislatura sao coerentes",
   stopifnot(all(as.integer(x$ano_eleicao) == LEG_ANO[x$legislatura])))
ok("uma linha por legislatura x nome", checa_unica(as.data.frame(x), c("legislatura", "nome_normalizado")))
iso <- function(v) is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v)
ok("datas em ISO", stopifnot(all(iso(x$data_inicio_exercicio)), all(iso(x$data_fim_exercicio)),
                             all(iso(x$data_nascimento))))
ok("inicio nao posterior ao fim",
   stopifnot(x[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio),
               all(data_inicio_exercicio <= data_fim_exercicio)]))
ok("toda linha traz url de origem", stopifnot(all(!is.na(x$url) & nzchar(x$url))))
ok("url aponta para dominio da ALERJ", stopifnot(all(grepl("alerj\\.rj\\.gov\\.br", x$url))))
ok("sexo_fonte so M ou F", in_set(x$sexo_fonte, c("M", "F"), permitir_na = TRUE, nome = "sexo_fonte"))

## ------------------------------------------------------------------ evidencia
sem_txt <- x[forma_saida %in% TEXTUAL & (is.na(causa_original) | causa_original == "")]
ok("ato nomeado traz o texto da fonte em causa_original", stopifnot(nrow(sem_txt) == 0L))
ok("forma sem texto so nos rotulos derivaveis da estrutura",
   stopifnot(x[(is.na(causa_original) | causa_original == "") &
               !forma_saida %in% c("nao_observado", "fim_regular"), all(forma_saida %in% ESTRUTURAL)]))
# o texto gravado tem de existir de fato no cache bruto da Casa (noticia ou rotulo estrutural)
nt <- fread(file.path(PAR, "noticias_eventos.csv"), colClasses = "character", na.strings = "")
ftx <- file.path(PAR, "noticias_texto.csv")
ntx <- if (file.exists(ftx)) fread(ftx, colClasses = "character", na.strings = "") else nt[0]
norm_nome <- function(v) {
  v <- stri_trans_general(toupper(fcoalesce(as.character(v), "")), "Latin-ASCII")
  v <- gsub("&[A-Z]+;", " ", v); v <- gsub("[^A-Z ]", " ", v); gsub(" +", " ", trimws(v))
}
corpus <- c(norm_nome(nt$titulo), if (nrow(ntx)) norm_nome(paste(ntx$titulo, ntx$texto)) else character())
alvo_txt <- x[forma_saida %in% c("renuncia", "cassacao", "falecimento", "licenca") & !is.na(causa_original)]
achou <- vapply(alvo_txt$causa_original, function(cc) {
  cc <- norm_nome(cc); any(stri_detect_fixed(corpus, cc))
}, logical(1))
cat("textos de ato conferidos contra o cache de noticias:", sum(achou), "de", length(achou), "\n")
registrar_numero("vrj_textos_de_ato_conferidos", sprintf("%d/%d", sum(achou), length(achou)), script = script)
ok("todo texto de ato existe literalmente no cache de noticias da Casa", stopifnot(all(achou)))

## ------------------------------------------------------------------ lastro nas fontes brutas
# (norm_nome ja definido acima)
bibn <- unique(data.table(legislatura = as.character(as.integer(bib$legislatura)), n = norm_nome(bib$nome_fonte)))
qsn  <- unique(data.table(legislatura = as.character(as.integer(qs$legislatura)),  n = norm_nome(qs$nome_fonte)))
fontes_brutas <- unique(rbind(bibn, qsn))
# toda linha nasce de um nome que esta no cache bruto (por si ou por uma grafia ligada)
x[, tem_lastro := nome_normalizado %in% fontes_brutas[legislatura == .BY$legislatura]$n, by = legislatura]
# a linha que nasce so da noticia (eleito que a Casa nunca listou) tem o lastro no texto da noticia
x[fonte == "alerj_noticias" & tem_lastro == FALSE,
  tem_lastro := stri_detect_regex(norm_nome(causa_original),
                                  paste0("(^| )", gsub(" ", "[^A-Z]+", nome_normalizado), "( |$)"))]
cat("linhas sem lastro direto no cache bruto:", x[tem_lastro == FALSE, .N], "\n")
ok("toda linha vem de um nome presente no cache bruto da Casa", stopifnot(x[, all(tem_lastro)]))
# a contagem por legislatura nao pode passar a da fonte bruta (fora as linhas que nascem da
# noticia, que por definicao nao estao no quadro de composicao)
cnt <- merge(x[fonte != "alerj_noticias", .(n_saida = .N), by = legislatura],
             fontes_brutas[, .(n_bruto = .N), by = legislatura], by = "legislatura")
print(cnt[order(as.integer(legislatura))])
ok("nenhuma legislatura tem mais nomes do que a fonte bruta",
   stopifnot(nrow(cnt) == uniqueN(x$legislatura), cnt[, all(n_saida <= n_bruto)]))

## ------------------------------------------------------------------ BOCEL
par <- x[!is.na(id_mandato_bocel)]
ok("id_mandato_bocel existe em mandatos.csv", stopifnot(all(par$id_mandato_bocel %in% m$id_mandato)))
idx <- match(par$id_mandato_bocel, m$id_mandato)
ok("mandato pareado e deputado estadual do RJ",
   stopifnot(all(m$cd_cargo[idx] == "7"), all(m$sg_uf[idx] == "RJ")))
ok("ano do mandato bate com o ano da linha", stopifnot(all(m$ano_eleicao[idx] == par$ano_eleicao)))
ok("nenhum mandato do BOCEL aparece em duas linhas", stopifnot(!any(duplicated(par$id_mandato_bocel))))
fora <- par[!is.na(data_fim_exercicio)][
  as.IDate(data_fim_exercicio) < as.IDate(m$mandato_inicio[match(id_mandato_bocel, m$id_mandato)]) - 60L |
  as.IDate(data_fim_exercicio) > as.IDate(m$mandato_fim[match(id_mandato_bocel, m$id_mandato)]) + 45L]
ok("data de fim dentro da janela do mandato", stopifnot(nrow(fora) == 0L))
ok("id_pessoa_bocel coerente com o mandato",
   stopifnot(par[!is.na(id_pessoa_bocel), all(id_pessoa_bocel == m$id_pessoa[match(id_mandato_bocel, m$id_mandato)])]))
ok("linha pareada sempre tem metodo de pareamento", stopifnot(all(!is.na(par$metodo_pareamento))))
ok("linha nao pareada nao tem metodo", stopifnot(all(is.na(x[is.na(id_mandato_bocel)]$metodo_pareamento))))

## ------------------------------------------------------------------ coerencia interna
ok("suplente nunca recebe fim_regular", stopifnot(x[condicao == "suplente", !any(forma_saida == "fim_regular")]))
ok("fim_regular so em legislatura ja encerrada",
   stopifnot(x[forma_saida == "fim_regular", all(legislatura != "13")]))
ok("fim_regular traz a data de termino publicada pela Casa",
   stopifnot(x[forma_saida == "fim_regular", all(!is.na(data_fim_exercicio))]))
term <- unique(bib[, .(legislatura = as.character(as.integer(legislatura)), termino_legislatura)])
ok("a data de termino gravada e a do PDF da Biblioteca",
   stopifnot(x[forma_saida == "fim_regular",
               all(data_fim_exercicio == term$termino_legislatura[match(legislatura, term$legislatura)])]))
ok("13a legislatura nao tem fim_regular nem termino",
   stopifnot(x[legislatura == "13", all(is.na(data_fim_exercicio) | forma_saida != "fim_regular")]))

## ------------------------------------------------------------------ cobertura recontada
cob <- merge(mrj[, .(n_bocel = .N), by = .(ano_eleicao)],
             x[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel))), by = ano_eleicao], by = "ano_eleicao")
cob <- merge(cob, par[forma_saida != "nao_observado", .(com_forma = uniqueN(id_mandato_bocel)), by = ano_eleicao],
             by = "ano_eleicao", all.x = TRUE)
cob[is.na(com_forma), com_forma := 0L]
setorder(cob, ano_eleicao)
print(cob)
cg <- fread("output/verificacao/assembleias2_RJ_cobertura.csv")
ok("cobertura gravada pelo build = cobertura recontada",
   stopifnot(identical(as.integer(cob$com_forma), as.integer(cg[order(ano_eleicao)]$n_mandato_com_forma)),
             identical(as.integer(cob$pareadas), as.integer(cg[order(ano_eleicao)]$n_pareados))))

## registro assinado (le por readLines: ha valores com '|' no meio)
ass <- readLines("output/numeros_assinatura.txt", warn = FALSE); ass <- ass[grepl("\\|", ass)]
pa <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass)),
                 valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass)))
ult <- pa[pa[, .I[.N], by = chave]$V1]; REG <- setNames(ult$valor, ult$chave)
ok("asm2rj_linhas registrado = recontado", stopifnot(REG[["asm2rj_linhas"]] == as.character(nrow(x))))
ok("asm2rj_pareadas registrado = recontado", stopifnot(REG[["asm2rj_pareadas"]] == as.character(nrow(par))))
ok("asm2rj_mandatos_com_forma_saida registrado = recontado",
   stopifnot(REG[["asm2rj_mandatos_com_forma_saida"]] ==
             as.character(uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel))))

registrar_numero("vrj_n_checagens", pass + fail, script = script)
registrar_numero("vrj_n_aprovadas", pass, script = script)
registrar_numero("vrj_n_reprovadas", fail, script = script)
registrar_numero("vrj_n_linhas", nrow(x), script = script)
registrar_numero("vrj_n_mandatos_com_forma", uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel), script = script)
rel <- c(paste0("verifica_RJ — ", format(Sys.time())), "",
         linhas_rel, "",
         paste0("linhas: ", nrow(x), " | pareadas: ", nrow(par),
                " | mandatos do BOCEL com forma observada: ",
                uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel), " de ", nrow(mrj)),
         "", "cobertura por eleicao:",
         capture.output(print(cob)), "",
         "forma_saida das linhas pareadas:",
         capture.output(print(par[, .N, by = .(condicao, forma_saida)][order(condicao, -N)])),
         "", if (fail) c("FALHAS:", paste0(" - ", falhas)) else "sem falhas")
writeLines(rel, "output/verificacao/assembleias2_RJ_relatorio.txt")
cat("\nverifica_RJ: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("FALHAS:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); quit(status = 1) }
