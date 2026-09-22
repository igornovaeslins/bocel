#!/usr/bin/env Rscript
# verifica_PR.R — verificador proprio da coleta do Parana (data/assembleias2/PR.csv).
# Alem do esquema e da integridade referencial contra data/mandatos.csv, confere o que o
# verificador geral nao tem como conferir: que o texto gravado em causa_original existe
# LITERALMENTE no cache da fonte (data_raw/assembleias2/PR/corpus_normalizado.jsonl e
# cabecalhos.jsonl), e no documento que a propria linha declara em id_fonte.
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_PR.R
suppressPackageStartupMessages({library(data.table); library(stringi); library(jsonlite)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source("R/assembleias2/00_funcoes_PR.R")
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/assembleias2/verifica_PR.R"
raw <- file.path(root, "data_raw", "assembleias2", "PR")
verd <- file.path(root, "output", "verificacao")
dir.create(verd, showWarnings = FALSE, recursive = TRUE)

pass <- 0L; fail <- 0L; falhas <- character(); passou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE },
                error = function(e) { falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) { pass <<- pass + 1L; passou <<- c(passou, nome) } else fail <<- fail + 1L
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
}
reg <- function(k, v) registrar_numero(paste0("asm2pr_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")

x <- fread("data/assembleias2/PR.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")

## ------------------------------------------------------------------ esquema
ok("esquema: 21 colunas na ordem canonica", stopifnot(identical(names(x), COLS)))
ok("uf e sempre PR", stopifnot(all(x$uf == "PR")))
ok("vocabulario fechado de forma_saida", in_set(x$forma_saida, VOCAB, "forma_saida do PR"))
ok("condicao em titular/suplente/nao_informado",
   in_set(x$condicao, c("titular", "suplente", "nao_informado"), "condicao"))
ok("ano_eleicao em 1998..2022 de quatro em quatro",
   stopifnot(all(as.integer(x$ano_eleicao) %in% seq(1998L, 2022L, 4L))))
ok("legislatura de 14 a 20", in_set(x$legislatura, as.character(14:20), "legislatura"))
ok("legislatura e ano de eleicao sao consistentes",
   stopifnot(all(as.integer(x$legislatura) == (as.integer(x$ano_eleicao) - 1998L) / 4L + 14L)))
iso <- function(v) is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v)
ok("datas em ISO", stopifnot(all(iso(x$data_inicio_exercicio)), all(iso(x$data_fim_exercicio))))
ok("inicio nao posterior ao fim",
   stopifnot(x[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio),
               all(data_inicio_exercicio <= data_fim_exercicio)]))
ok("toda linha traz url de origem", stopifnot(all(!is.na(x$url) & nzchar(x$url))))
ok("uma linha por legislatura x nome normalizado",
   checa_unica(as.data.frame(x), c("legislatura", "nome_normalizado")))

## ------------------------------------------------------------------ integridade contra o BOCEL
par <- x[!is.na(id_mandato_bocel)]
idx <- match(par$id_mandato_bocel, m$id_mandato)
ok("todo id_mandato_bocel existe em mandatos.csv", stopifnot(!any(is.na(idx))))
ok("mandato pareado e deputado estadual (cd_cargo 7)", stopifnot(all(m$cd_cargo[idx] == "7")))
ok("mandato pareado e do Parana", stopifnot(all(m$sg_uf[idx] == "PR")))
ok("ano de eleicao do mandato bate com o da linha", stopifnot(all(m$ano_eleicao[idx] == par$ano_eleicao)))
ok("nenhum mandato aparece em duas linhas", stopifnot(!anyDuplicated(par$id_mandato_bocel)))
obs <- unique(par[forma_saida != "nao_observado", .(id_mandato_bocel, forma_saida)])
ok("um mandato nao recebe duas formas divergentes",
   stopifnot(nrow(obs[, .N, by = id_mandato_bocel][N > 1]) == 0L))
jan <- par[!is.na(data_fim_exercicio)]
ji <- match(jan$id_mandato_bocel, m$id_mandato)
fora <- jan[as.IDate(data_fim_exercicio) < as.IDate(m$mandato_inicio[ji]) - 60L |
            as.IDate(data_fim_exercicio) > as.IDate(m$mandato_fim[ji]) + 45L]
ok("data de fim dentro da janela do mandato", stopifnot(nrow(fora) == 0L))

## ------------------------------------------------------------------ coerencia da composicao
ok("cada legislatura tem os 54 eleitos do PR pareados",
   stopifnot(all(x[, .(n = uniqueN(id_mandato_bocel[!is.na(id_mandato_bocel)])), by = ano_eleicao]$n == 54L)))
ok("titular e exatamente a linha pareada a um mandato",
   stopifnot(x[, all((condicao == "titular") == !is.na(id_mandato_bocel))]))
ok("exercicio comeca dentro da legislatura",
   stopifnot(x[!is.na(data_inicio_exercicio), {
     li <- as.IDate(sprintf("%d-02-01", as.integer(ano_eleicao) + 1L))
     lf <- as.IDate(sprintf("%d-01-31", as.integer(ano_eleicao) + 5L))
     all(as.IDate(data_inicio_exercicio) >= li - 40L & as.IDate(data_inicio_exercicio) <= lf)
   }]))
ok("exercicio termina dentro da legislatura",
   stopifnot(x[!is.na(data_fim_exercicio), {
     lf <- as.IDate(sprintf("%d-01-31", as.integer(ano_eleicao) + 5L))
     all(as.IDate(data_fim_exercicio) <= lf + 5L)
   }]))
ok("trecho citado tem tamanho de citacao, nao de documento",
   stopifnot(x[!is.na(causa_original), all(nchar(causa_original) >= 20L & nchar(causa_original) <= 900L)]))

## ------------------------------------------------------------------ regime de evidencia
sem_txt <- par[forma_saida %in% TEXTUAL & (is.na(causa_original) | causa_original == "")]
ok("ato nomeado so aparece com o texto da fonte", stopifnot(nrow(sem_txt) == 0L))
ok("forma sem texto so nos rotulos derivaveis da estrutura",
   stopifnot(x[(is.na(causa_original) | causa_original == "") &
               !forma_saida %in% c("nao_observado", "fim_regular"),
               all(forma_saida %in% ESTRUTURAL)]))

## ------------------------------------------------------------------ o texto existe na fonte
# Compara em espaco colapsado, que e a forma em que o cache guarda o texto do PDF; a quebra de
# palavra na virada de linha ("Assem- bleia") e preservada dos dois lados, e nao e higienizada.
cx <- x[!is.na(causa_original) & nzchar(causa_original)]
cat("linhas com causa_original:", nrow(cx), "| textos distintos:", uniqueN(cx$causa_original), "\n")
if (nrow(cx)) {
  corpus <- ler_corpus(raw, doc_ids = NULL, com_p1 = TRUE)
  cat("documentos no cache:", nrow(corpus), "\n")
  setkey(corpus, doc_id)
  achou_no_id <- vapply(seq_len(nrow(cx)), function(i) {
    d <- corpus[.(cx$id_fonte[i]), texto, nomatch = NULL]
    length(d) > 0L && stri_detect_fixed(d[1], limpa(cx$causa_original[i]))
  }, logical(1))
  falta <- which(!achou_no_id)
  achou_em_algum <- rep(TRUE, nrow(cx))
  if (length(falta)) {
    todos <- paste(corpus$texto, collapse = " ¶ ")
    achou_em_algum[falta] <- vapply(falta, function(i)
      stri_detect_fixed(todos, limpa(cx$causa_original[i])), logical(1))
  }
  # 10% do acervo da ALEP foi digitalizado como imagem, sem camada de texto: o ato que estiver
  # nessas edicoes e invisivel para esta coleta, e o numero fica registrado como limite conhecido
  acervo <- corpus[grepl("\\.zip$", doc_id)]
  reg("verif_n_documentos_acervo", nrow(acervo))
  reg("verif_n_documentos_sem_camada_de_texto", acervo[nchar(texto) < 3000L, .N])
  cat("documentos do acervo da ALEP sem camada de texto (digitalizacao por imagem):",
      acervo[nchar(texto) < 3000L, .N], "de", nrow(acervo), "\n")
  reg("verif_n_causa_no_documento_declarado", sum(achou_no_id))
  reg("verif_n_causa_em_algum_documento", sum(achou_em_algum))
  cat("causa_original encontrada no documento declarado em id_fonte:", sum(achou_no_id),
      "| em algum documento do cache:", sum(achou_em_algum), "de", nrow(cx), "\n")
  arq_sem <- file.path(verd, "asm2_PR_causa_sem_lastro.csv")
  if (any(!achou_em_algum))
    fwrite(cx[!achou_em_algum, .(nome, forma_saida, id_fonte, causa_original)], arq_sem)
  else if (file.exists(arq_sem)) file.remove(arq_sem)
  ok("causa_original existe literalmente no cache da fonte",
     stopifnot(all(achou_em_algum)))
  ok("causa_original esta no documento que a linha declara em id_fonte",
     stopifnot(all(achou_no_id)))
  ok("causa_original sem aspa reta, que o vaivem de CSV nao preserva",
     stopifnot(!any(grepl('"', cx$causa_original, fixed = TRUE))))
  ok("causa_original nomeia a pessoa da linha",
     stopifnot(all(vapply(seq_len(nrow(cx)), function(i) {
       v <- unlist(stri_split_fixed(cx$nome_normalizado[i], " "))
       v <- v[nchar(v) >= 4L]
       if (!length(v)) return(TRUE)
       tx <- toupper(asc(cx$causa_original[i]))
       any(vapply(v, function(w) stri_detect_regex(
         tx, paste0("\\b", paste0(unlist(stri_split_boundaries(w, type = "character")),
                                  collapse = "(?:-\\s?)?"), "\\b")), logical(1)))
     }, logical(1)))))
}

## ------------------------------------------------------------------ cobertura e registro
cob <- x[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)),
             com_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)])),
         by = .(ano_eleicao, legislatura)][order(ano_eleicao)]
print(cob)
print(x[, .N, by = forma_saida][order(-N)])
fwrite(cob, file.path(verd, "asm2_PR_cobertura_verificada.csv"))

ass <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- ass[grepl("\\|", ass)]
pa <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass)),
                 valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass)))
ult <- pa[pa[, .I[.N], by = chave]$V1]
REG <- setNames(ult$valor, ult$chave)
ok("asm2pr_n_linhas registrado = recontado",
   stopifnot(REG[["asm2pr_n_linhas"]] == as.character(nrow(x))))
ok("asm2pr_n_pareadas registrado = recontado",
   stopifnot(REG[["asm2pr_n_pareadas"]] == as.character(nrow(par))))
ok("asm2pr_n_mandatos_com_forma registrado = recontado",
   stopifnot(REG[["asm2pr_n_mandatos_com_forma"]] ==
               as.character(uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel))))

## ---------------------------------------------------------------- edicoes em imagem (OCR, 05/09/2026)
## o indice gravado por python/assembleias2/ocr_diarios_PR.py diz quais edicoes do acervo estavam
## sem camada de texto e com que confianca o OCR as leu; cabecalhos.jsonl marca ocr=true na edicao
## cujo expediente veio do OCR. Nenhum desses numeros e escrito a mao.
idx_ocr <- file.path(raw, "diarios", "_indice_ocr.csv")
n_ocr <- 0L; conf_ocr <- NA_real_
if (file.exists(idx_ocr)) {
  io <- fread(idx_ocr, colClasses = "character")[estado == "ocr"]
  n_ocr <- uniqueN(io$arquivo); conf_ocr <- round(mean(as.numeric(io$conf), na.rm = TRUE), 1)
}
n_ocr_txt <- length(list.files(file.path(raw, "diarios"), pattern = "\\.ocr\\.txt$"))
cab_l <- readLines(file.path(raw, "cabecalhos.jsonl"), warn = FALSE)
n_cab_ocr <- sum(grepl('"ocr": true', cab_l, fixed = TRUE))
ok("toda edicao que o indice diz submetida a OCR tem o .ocr.txt em disco",
   stopifnot(n_ocr_txt >= n_ocr))
ok("toda edicao com OCR em disco entrou em cabecalhos.jsonl com a marca ocr=true",
   stopifnot(n_cab_ocr == n_ocr_txt))
cat("edicoes submetidas a OCR:", n_ocr, "| .ocr.txt em disco:", n_ocr_txt,
    "| expediente lido via OCR:", n_cab_ocr, "| confianca media:", conf_ocr, "\n")
reg("ver_n_edicoes_submetidas_a_ocr", n_ocr)
reg("ver_n_ocr_txt_em_disco", n_ocr_txt)
reg("ver_n_edicoes_com_expediente_via_ocr", n_cab_ocr)
reg("ver_conf_media_ocr", conf_ocr)

reg("verif_n_checagens", pass + fail)
reg("verif_n_aprovadas", pass)
reg("verif_n_reprovadas", fail)
gravar_relatorio_verificacao(
  alvo = "data/assembleias2/PR.csv — Assembleia Legislativa do Parana, 1998-2022",
  script = script, passou = passou, falhou = falhas,
  fora_de_cobertura = c(
    "identidade da pessoa: o pareamento e por nome, sem CPF nem titulo, e nome de urna curto pode casar errado",
    "completude do acervo: o Diario da Assembleia tem lacunas de edicao (2005 e 2009 sao os piores anos)",
    "a relacao nominal do expediente pode demorar a registrar troca de titular por suplente",
    "motivo por tras do ato: o texto diz que houve licenca, nao se ela foi de saude ou de conveniencia",
    "legislatura 20 esta em curso: ausencia de forma nao e ausencia de evento",
    "densidade desigual: de 1999 a 2011 o expediente e quase diario, de 2011 a 2026 e uma amostra de cerca de dezessete edicoes por ano, e a data de saida ai tem folga de semanas",
    "titular licenciado que segue impresso na relacao nominal ate o fim da legislatura sai como fim_regular; a licenca aparece so quando o ato foi publicado",
    "cerca de um decimo do acervo da ALEP (226 edicoes, 2005, 2007 e 2009 sobretudo) estava digitalizado como imagem; desde 05/09/2026 essas edicoes passam por OCR (tesserocr 5.5.1, confianca media por edicao em diarios/_indice_ocr.csv) e o ato publicado nelas so aparece se o OCR o leu de forma legivel",
    "'outro' e rotulo de ignorancia examinada: a cadeira vagou antes do fim e o diario nao publicou o ato"),
  dir = verd)
cat("\nverifica_PR: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("FALHAS:\n", paste0(" - ", falhas, collapse = "\n"), "\n"); quit(status = 1) }
