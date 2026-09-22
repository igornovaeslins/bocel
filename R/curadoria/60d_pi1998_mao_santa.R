#!/usr/bin/env Rscript
# 60d_pi1998_mao_santa.R -- substitui em ref/eventos_governos_fonte_oficial.csv a linha do governo do Piaui em 1998,
# hoje presa ao id_mandato do 2o colocado (Hugo Napoleao do Rego Neto), pela linha do titular eleito (Francisco de
# Assis de Moraes Souza, Mao Santa), cujo mandato foi cassado pelo TSE (21/09/2026). O cadastro do TSE foi reescrito
# depois da cassacao e passou a gravar o 2o colocado como eleito; ref/correcoes_eleitos_fonte_oficial.csv e o bloco
# correspondente de R/03_build_banco.R desfazem essa reescrita antes de qualquer mandato ser derivado, e esta linha
# acompanha a troca do id_mandato resultante.
#
# Nao localizei o numero do acordao do TSE nem o registro oficial (ata da ALEPI ou Diario Oficial do Piaui) da posse
# de Hugo Napoleao; por isso fonte_1 e o verbete do DHBB/CPDOC-FGV (a melhor fonte disponivel, ainda que nao seja
# ato oficial) e fonte_2 e a Wikipedia, como pista para a data do evento, que a observacao da linha declara.
#
# Entrada:  ref/eventos_governos_fonte_oficial.csv
# Saida:    ref/eventos_governos_fonte_oficial.csv (so a linha do governo do Piaui em 1998)
# Execucao: cd ~/bocel && Rscript --vanilla R/curadoria/60d_pi1998_mao_santa.R
# Rodou uma vez em 21/09/2026, e ref/eventos_governos_fonte_oficial.csv ja traz o resultado. Fica fora da cadeia de
# reconstrucao porque edita a tabela curada (e aborta se rodar de novo), e vai no pacote como registro da edicao.
suppressPackageStartupMessages({ library(data.table) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/curadoria/60d_pi1998_mao_santa.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

ARQ <- "ref/eventos_governos_fonte_oficial.csv"
ID_ANTIGO <- "M1998_PI_3_25_180001803000251"
ID_NOVO <- "M1998_PI_3_15_180001803000151"

g <- fread(ARQ, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
idx_dado <- which(g$id_mandato == ID_ANTIGO)
stopifnot(length(idx_dado) == 1L, !ID_NOVO %chin% g$id_mandato)

campo <- function(x) {
  if (is.na(x) || !nzchar(x)) return('""')
  stopifnot(!grepl('"', x, fixed = TRUE)) # os campos desta linha nao levam aspas embutidas
  paste0('"', x, '"')
}
COLS <- c("id_mandato", "cargo", "sg_uf", "ano_eleicao", "nome_tse", "forma_saida", "data_evento", "precisao_data",
          "assumiu", "fonte_1", "trecho_1", "fonte_2", "trecho_2", "confianca", "observacao")
stopifnot(identical(names(g), COLS))

nova <- list(
  id_mandato = ID_NOVO, cargo = "GOVERNADOR", sg_uf = "PI", ano_eleicao = "1998",
  nome_tse = "FRANCISCO DE ASSIS DE MORAES SOUZA", forma_saida = "cassacao",
  data_evento = "2001-11-06", precisao_data = "ato",
  assumiu = paste0("Kleber Eulálio (presidente da ALEPI, interino, de 09/11 a 19/11/2001) e depois Hugo ",
                   "Napoleão do Rego Neto (2º colocado em 1998, reempossado por decisão do TSE em ",
                   "19/11/2001, após renunciar ao mandato de senador)"),
  fonte_1 = "https://www18.fgv.br/cpdoc/acervo/dicionarios/verbete-biografico/hugo-napoleao-do-rego-neto",
  trecho_1 = paste0("Com a cassação do mandato de governador de Mão Santa em 2001 e a decisão TSE ",
                     "de empossar no cargo o segundo colocado no segundo turno, Hugo Napoleão renunciou ",
                     "ao mandato de senador e foi empossado no governo do Piauí"),
  fonte_2 = "https://pt.wikipedia.org/wiki/M%C3%A3o_Santa",
  trecho_2 = paste0("Após julgamento de 7 votos a zero Tribunal Superior Eleitoral cassou seu mandato em 6 ",
                     "de novembro de 2001"),
  confianca = "media",
  observacao = paste0("Não localizei o acórdão do TSE (número, data de julgamento e de ",
                       "publicação) nem o registro oficial da posse de Hugo Napoleão (ata da ALEPI ",
                       "ou Diário Oficial do Piauí de novembro de 2001); a data de 06/11/2001 vem da ",
                       "Wikipédia, como pista, na ausência desses dois documentos.")
)
nova <- nova[COLS]

raw <- readBin(ARQ, "raw", file.info(ARQ)$size)
txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
termina_crlf <- endsWith(txt, "\r\n")
linhas <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
stopifnot(length(linhas) == nrow(g) + 1L) # cabecalho + uma linha por mandato

linhas[idx_dado + 1L] <- paste(vapply(nova, campo, character(1)), collapse = ",")

saida_txt <- paste(linhas, collapse = "\r\n")
if (termina_crlf) saida_txt <- paste0(saida_txt, "\r\n")
con <- file(ARQ, "wb"); on.exit(close(con), add = TRUE)
writeBin(charToRaw(enc2utf8(saida_txt)), con)

reg("fex_pi1998_linha_substituida", 1L)
cat(sprintf("OK: %s substituido por %s em %s\n", ID_ANTIGO, ID_NOVO, ARQ))
