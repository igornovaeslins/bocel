# 18_datajud_cassacoes.R — processos eleitorais de cassacao (AIJE, AIME, RCED, representacoes)
# nos 27 TREs e no TSE, via DataJud/CNJ (API publica). A API nao expoe as partes, por isso a
# saida e por processo (com municipio do orgao julgador e cargo do assunto) e um sinal agregado
# por unidade x eleicao; nao ha pareamento a mandato individual.
# Entrada:  data_raw/datajud/<tribunal>.jsonl (python/fetch_datajud_eleitoral.py)
# Saida:    data/datajud_processos_cassacao.csv, data/datajud_sinal_unidade_eleicao.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/18_datajud_cassacoes.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/18_datajud_cassacoes.R"

files <- list.files("data_raw/datajud", pattern = "\\.jsonl$", full.names = TRUE)
stopifnot(length(files) > 0)
ler <- function(f) {
  l <- readLines(f, warn = FALSE); if (!length(l)) return(NULL)
  x <- rbindlist(lapply(l, function(s) {
    j <- fromJSON(s, simplifyVector = FALSE)
    mv <- j$movimentos
    nomes_mov <- vapply(mv, function(m) toupper(stringi::stri_trans_general(paste(m$nome, paste(unlist(m$complementos), collapse = " ")), "Latin-ASCII")), character(1))
    datas_mov <- vapply(mv, function(m) substr(if (is.null(m$dataHora)) "" else m$dataHora, 1, 10), character(1))
    ind <- grepl("CASSA|PERDA D[OE] MANDATO|PROCEDEN", nomes_mov) & !grepl("IMPROCEDEN|NAO PROVIMENTO|NEGADO", nomes_mov)
    data.table(
      tribunal = toupper(j$tribunal %||% NA_character_), grau = j$grau %||% NA_character_,
      atualizacao = j$dataHoraUltimaAtualizacao %||% NA_character_,
      numero_processo = j$numeroProcesso %||% NA_character_, classe = j$classe %||% NA_character_,
      assuntos = paste(unlist(j$assuntos), collapse = "; "),
      cargo_assunto = { a <- unlist(j$assuntos); a <- a[grepl("^Cargo - ", a)]; if (length(a)) paste(sub("^Cargo - ", "", a), collapse = "; ") else NA_character_ },
      data_ajuizamento = { d <- j$dataAjuizamento %||% ""; if (nchar(d) >= 8) sprintf("%s-%s-%s", substr(d, 1, 4), substr(d, 5, 6), substr(d, 7, 8)) else NA_character_ },
      orgao_julgador = j$orgaoJulgador %||% NA_character_,
      id_municipio_ibge = as.character(j$codigoMunicipioIBGE %||% NA_character_),
      n_movimentos = length(mv),
      ultimo_movimento = if (length(mv)) mv[[length(mv)]]$nome else NA_character_,
      data_ultimo_movimento = if (length(mv)) datas_mov[length(mv)] else NA_character_,
      indicio_cassacao = any(ind),
      data_indicio = if (any(ind)) max(datas_mov[ind]) else NA_character_,
      movimento_indicio = if (any(ind)) { k <- which(ind); nomes_mov[k[order(datas_mov[k], decreasing = TRUE)][1]] } else NA_character_
    )
  }), use.names = TRUE, fill = TRUE)
  x
}
`%||%` <- function(a, b) if (is.null(a)) b else a
proc <- rbindlist(lapply(files, ler), use.names = TRUE, fill = TRUE)
# correcao 2026-08-28 (verifica_datajud): o indice do DataJud guarda versoes repetidas do mesmo processo
# (mesmo tribunal, numero e grau, dataHoraUltimaAtualizacao distinta; 537 casos, 105 com movimentos a mais
# na versao recente). O unique() em ordem de arquivo ficava com a versao MAIS ANTIGA. Mantem-se o grau que
# aparece primeiro (como antes) e, dentro dele, a versao mais recente.
proc[, ordem := .I]
proc[, grau_1 := grau[1], by = .(tribunal, numero_processo)]
proc <- proc[grau == grau_1]
setorder(proc, tribunal, numero_processo, -atualizacao, ordem, na.last = TRUE)
proc <- unique(proc, by = c("tribunal", "numero_processo"))
proc[, c("ordem", "grau_1", "atualizacao") := NULL]
proc[, ano_ajuizamento := as.integer(substr(data_ajuizamento, 1, 4))]
proc[, uf := fifelse(tribunal == "TSE", "BR", sub("^TRE-", "", tribunal))]
# municipio TSE via correspondencia IBGE
tse_ibge <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
proc <- merge(proc, tse_ibge[, .(id_municipio_ibge, sg_ue)], by = "id_municipio_ibge", all.x = TRUE)
# Distrito Federal nao tem eleicao municipal e fica fora de municipios_tse_ibge; o TSE usa 97012 para Brasilia
proc[id_municipio_ibge == "5300108" & is.na(sg_ue), sg_ue := "97012"]

# sinal agregado por unidade (municipio do orgao julgador) x eleicao de referencia (a ultima
# eleicao municipal anterior ao ajuizamento) x cargo do assunto
# correcao 2026-08-29 (verifica_datajud): 93% dos processos relevantes nao trazem 'Cargo - ...' nos assuntos
# (cargo_assunto NA) e a regra antiga os jogava no calendario das eleicoes gerais; como 16.861 deles sao de
# zona eleitoral ajuizados em ano de eleicao municipal (2020, 2024), o sinal ficava atribuido a 2018 e 2022.
# Sem cargo, a eleicao de referencia passa a ser a ultima eleicao (de qualquer tipo) ate o ano do ajuizamento,
# como declara o livro de codigos; com cargo, mantem-se o calendario municipal ou geral do cargo.
proc[, cargo_mun := grepl("PREFEITO|VEREADOR", toupper(cargo_assunto))]
proc[, eleicao_ref := fcase(cargo_mun, ano_ajuizamento - (ano_ajuizamento %% 4L),
                            is.na(cargo_assunto), ano_ajuizamento - (ano_ajuizamento %% 2L),
                            default = ano_ajuizamento - ((ano_ajuizamento - 2L) %% 4L))]
proc[, cargo_mun := NULL]
# o sinal de cassacao considera AIJE, AIME, RCED e representacoes cujo assunto fala em cassacao,
# captacao ilicita de sufragio ou perda de mandato (a classe 'Representacao' generica e sobretudo propaganda)
proc[, assuntos_n := toupper(stringi::stri_trans_general(assuntos, "Latin-ASCII"))]
proc[, classe_n := toupper(stringi::stri_trans_general(classe, "Latin-ASCII"))]
proc[, relevante_cassacao := grepl("INVESTIGACAO JUDICIAL|IMPUGNACAO DE MANDATO|EXPEDICAO DE DIPLOMA", classe_n) |
                             grepl("CASSA|CAPTACAO ILICITA|PERDA D[OE] MANDATO|ABUSO", assuntos_n)]
registrar_numero("dj_n_relevantes_cassacao", proc[relevante_cassacao == TRUE, .N], script = script)
# correcao 2026-08-28 (verifica_datajud): a classe vem do DataJud como 'Recurso contra Expedicao de Diploma'
# (contra em minuscula), e a comparacao literal com 'Contra' zerava n_rced em todo o sinal; as tres
# contagens passam a usar a classe normalizada (classe_n), que ja alimenta relevante_cassacao.
sinal <- proc[relevante_cassacao == TRUE & (!is.na(sg_ue) | uf != "BR"), .(n_processos = .N, n_aije = sum(grepl("INVESTIGACAO JUDICIAL", classe_n)),
                                              n_aime = sum(grepl("IMPUGNACAO DE MANDATO", classe_n)),
                                              n_rced = sum(grepl("EXPEDICAO DE DIPLOMA", classe_n)),
                                              n_com_indicio_cassacao = sum(indicio_cassacao),
                                              primeira_data_indicio = { d <- data_indicio[indicio_cassacao]; if (length(d)) min(d) else NA_character_ }),
              by = .(uf, sg_ue, id_municipio_ibge, cargo_assunto, eleicao_ref)][order(uf, sg_ue, eleicao_ref)]
fwrite(sinal, "data/datajud_sinal_unidade_eleicao.csv", na = "NA", quote = TRUE)
fwrite(proc[, .(tribunal, grau, numero_processo, classe, assuntos, cargo_assunto, relevante_cassacao, data_ajuizamento, ano_ajuizamento, uf,
                orgao_julgador, id_municipio_ibge, sg_ue, n_movimentos, ultimo_movimento, data_ultimo_movimento,
                indicio_cassacao, data_indicio, movimento_indicio)],
       "data/datajud_processos_cassacao.csv", na = "NA", quote = TRUE)

registrar_numero("dj_n_processos", nrow(proc), script = script)
registrar_numero("dj_n_tribunais", uniqueN(proc$tribunal), script = script)
registrar_numero("dj_n_com_indicio_cassacao", proc[indicio_cassacao == TRUE, .N], script = script)
registrar_numero("dj_n_com_municipio", proc[!is.na(sg_ue), .N], script = script)
for (cl in unique(proc$classe)) registrar_numero(paste0("dj_n_", gsub("[^a-z]", "_", tolower(iconv(cl, to = "ASCII//TRANSLIT")))), proc[classe == cl, .N], script = script)
print(proc[, .N, by = .(classe)][order(-N)]); print(proc[, .N, by = .(indicio_cassacao)])
cat("18_datajud_cassacoes: concluido —", nrow(proc), "processos,", nrow(sinal), "linhas de sinal\n")
