#!/usr/bin/env Rscript
# 60b_integrar_fontes_executivos.R -- integra em ref/eventos_governos_fonte_oficial.csv os achados de
# ref/candidatos_fontes_executivos.csv que R/60a_conferir_fontes_executivos.R confirmou contra o documento baixado
# (21/09/2026). So entra achado com tem_exato=TRUE (trecho citado presente no arquivo real) e cuja fonte, pela
# classificacao de lib/tipo_fonte.R, e oficial, base_dhbb ou noticia_orgao_publico -- nunca imprensa ou Wikipedia
# (regra A1). A fonte confirmada vira fonte_1/trecho_1; a fonte de imprensa que estava em fonte_1 desce
# para fonte_2/trecho_2, como pista (nenhuma das 52 linhas do recorte tinha fonte oficial ja presente em fonte_1
# ou fonte_2, conferido antes de rodar). Achado sem confirmacao mecanica, ou confirmado mas ainda de imprensa,
# fica de fora e vai para output/verificacao/fontes_executivos_pendencias.csv.
#
# O arquivo curado usa fim de linha CRLF; a edicao e feita linha a linha sobre o texto bruto (nao por
# fread()+fwrite() da tabela inteira), porque a volta fread->fwrite nao preserva byte a byte os poucos campos
# do arquivo com aspas duplicadas (citacao dentro de citacao), o que mudaria linhas fora do escopo desta integracao.
#
# Entrada:  ref/candidatos_fontes_executivos.csv, output/verificacao/fontes_executivos_conferencia.csv,
#           output/verificacao/candidatos_fontes_executivos_confirmados.csv, ref/eventos_governos_fonte_oficial.csv
# Saida:    ref/eventos_governos_fonte_oficial.csv (editado nas linhas confirmadas e admissiveis),
#           output/verificacao/fontes_executivos_pendencias.csv (achados que nao entraram, com o motivo)
# Execucao: cd ~/bocel && Rscript --vanilla R/curadoria/60b_integrar_fontes_executivos.R
# Rodou uma vez em 21/09/2026, e ref/eventos_governos_fonte_oficial.csv ja traz o resultado. Fica fora da cadeia de
# reconstrucao porque edita a tabela curada, e vai no pacote como registro da edicao.
suppressPackageStartupMessages({ library(data.table) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
source(file.path(root, "lib", "tipo_fonte.R"))
script <- "R/curadoria/60b_integrar_fontes_executivos.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
le <- function(f) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")

ARQ <- "ref/eventos_governos_fonte_oficial.csv"

conf <- le("output/verificacao/fontes_executivos_conferencia.csv")
confc <- le("output/verificacao/candidatos_fontes_executivos_confirmados.csv")
cand <- le("ref/candidatos_fontes_executivos.csv")
g <- le(ARQ)

## ---------------------------------------------------------------- confirmados e sua admissibilidade
ok_ids <- confc[tem_exato %chin% "TRUE", id_mandato]
achados <- conf[origem %chin% "candidato" & id_mandato %chin% ok_ids & resultado %chin% "exato"]
setorder(achados, id_mandato, slot)
achados_um <- achados[, .SD[1L], by = id_mandato] # um achado por id_mandato (o primeiro slot que bateu exato)
achados_um[, tipo := tipo_fonte_vec(url)]

admissiveis <- achados_um[tipo %chin% c("oficial", "base_dhbb", "noticia_orgao_publico")]
nao_admissiveis <- achados_um[!tipo %chin% c("oficial", "base_dhbb", "noticia_orgao_publico")]

reg("fex_integra_candidatos_confirmados", nrow(achados_um))
reg("fex_integra_admissiveis", nrow(admissiveis))
reg("fex_integra_nao_admissiveis_tipo", nrow(nao_admissiveis))
reg("fex_integra_nao_confirmados", confc[tem_exato %chin% "FALSE", .N])

DESC_TIPO <- c(oficial = "pagina oficial do orgao", base_dhbb = "verbete do DHBB/CPDOC-FGV",
               noticia_orgao_publico = "noticia publicada por orgao publico")

## ---------------------------------------------------------------- leitura byte-preservada do arquivo curado
raw <- readBin(ARQ, "raw", file.info(ARQ)$size)
txt <- rawToChar(raw); Encoding(txt) <- "UTF-8"
termina_crlf <- endsWith(txt, "\r\n")
linhas <- strsplit(txt, "\r\n", fixed = TRUE)[[1]]
stopifnot(length(linhas) == nrow(g) + 1L) # cabecalho + uma linha por mandato

campo <- function(x) {
  if (is.na(x) || !nzchar(x)) return('""')
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}
COLS <- c("id_mandato", "cargo", "sg_uf", "ano_eleicao", "nome_tse", "forma_saida", "data_evento", "precisao_data",
          "assumiu", "fonte_1", "trecho_1", "fonte_2", "trecho_2", "confianca", "observacao")
stopifnot(identical(names(g), COLS))
montar_linha <- function(r) paste(vapply(r, campo, character(1)), collapse = ",")

n_atualizadas <- 0L
for (i in seq_len(nrow(admissiveis))) {
  id <- admissiveis$id_mandato[i]
  url_conf <- admissiveis$url[i]
  slot_conf <- admissiveis$slot[i]
  tipo_conf <- admissiveis$tipo[i]

  crow <- cand[id_mandato %chin% id]
  stopifnot(nrow(crow) == 1L)
  trecho_conf <- if (slot_conf %chin% "1") crow$trecho[1] else crow$trecho_2[1]
  stopifnot(!is.na(trecho_conf), nzchar(trecho_conf))

  idx_dado <- which(g$id_mandato == id)
  stopifnot(length(idx_dado) == 1L)
  gr <- g[idx_dado]

  # nenhuma das duas fontes antigas pode ter aspas: garante que descer fonte_1 para fonte_2 nao corrompe o CSV
  stopifnot(!grepl('"', gr$fonte_1, fixed = TRUE), !grepl('"', gr$trecho_1, fixed = TRUE))

  data_conf <- crow$data_evento[1]
  obs_extra <- sprintf(
    "Fonte_1 atualizada em 21/09/2026 para %s, apos conferir o trecho no documento baixado; a fonte de imprensa que estava em fonte_1 passa a fonte_2, como pista.",
    DESC_TIPO[[tipo_conf]])
  nova_data <- gr$data_evento
  if (!is.na(data_conf) && (is.na(gr$data_evento) || data_conf != gr$data_evento)) {
    obs_extra <- paste0(obs_extra, sprintf(" A data de evento foi corrigida de %s para %s, conforme a fonte confirmada.",
                                            gr$data_evento, data_conf))
    nova_data <- data_conf
  }
  obs_velha <- if (is.na(gr$observacao) || !nzchar(gr$observacao)) "" else trimws(gr$observacao)
  if (nzchar(obs_velha) && !grepl("[.!?]$", obs_velha)) obs_velha <- paste0(obs_velha, ".")
  nova_obs <- trimws(paste(obs_velha, obs_extra))

  nova <- copy(gr)
  nova[, `:=`(fonte_2 = fonte_1, trecho_2 = trecho_1, fonte_1 = url_conf, trecho_1 = trecho_conf,
              data_evento = nova_data, observacao = nova_obs)]

  linhas[idx_dado + 1L] <- montar_linha(as.list(nova[, ..COLS]))
  n_atualizadas <- n_atualizadas + 1L
}
stopifnot(n_atualizadas == nrow(admissiveis))

saida_txt <- paste(linhas, collapse = "\r\n")
if (termina_crlf) saida_txt <- paste0(saida_txt, "\r\n")
con <- file(ARQ, "wb"); on.exit(close(con), add = TRUE)
writeBin(charToRaw(enc2utf8(saida_txt)), con)

## ---------------------------------------------------------------- pendencias (o que nao entrou, e por que)
pend <- rbindlist(list(
  if (nrow(nao_admissiveis)) nao_admissiveis[, .(id_mandato, url, motivo = paste0(
    "achado confirmado no documento, mas a fonte e do tipo '", tipo, "' (nao admissivel para fonte_1)"))],
  if (confc[tem_exato %chin% "FALSE", .N]) confc[tem_exato %chin% "FALSE", .(id_mandato, url = NA_character_,
    motivo = "achado nao confirmado mecanicamente contra o documento baixado")]
), fill = TRUE)
fwrite(pend, "output/verificacao/fontes_executivos_pendencias.csv", na = "NA", quote = TRUE)
reg("fex_integra_linhas_atualizadas", n_atualizadas)
reg("fex_integra_pendencias_total", nrow(pend))
cat(sprintf("OK: %d linhas atualizadas em %s; %d pendencias em output/verificacao/fontes_executivos_pendencias.csv\n",
            n_atualizadas, ARQ, nrow(pend)))
