# 02_ingest_votacao.R — agrega votacao_candidato_munzona por candidato (SQ_CANDIDATO x turno)
# Entrada:  data_raw/votacao/votacao_candidato_munzona_<ANO>.zip
# Saida:    data_raw/parquet/votos_<ANO>.parquet (votos totais por candidatura e turno)
# Execucao: Rscript --vanilla R/02_ingest_votacao.R
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
raw  <- file.path(root, "data_raw", "votacao")
outd <- file.path(root, "data_raw", "parquet")
dir.create(outd, recursive = TRUE, showWarnings = FALSE)

anos <- sort(as.integer(gsub("\\D", "", basename(
  list.files(raw, pattern = "^votacao_candidato_munzona_\\d{4}\\.zip$")))))
stopifnot(length(anos) > 0)

for (ano in anos) {
  dest <- file.path(outd, sprintf("votos_%d.parquet", ano))
  if (file.exists(dest)) { cat("SKIP", ano, "\n"); next }
  zipf <- file.path(raw, sprintf("votacao_candidato_munzona_%d.zip", ano))
  tmp  <- file.path(tempdir(), sprintf("vot_%d", ano))
  dir.create(tmp, showWarnings = FALSE)
  unzip(zipf, exdir = tmp)
  # os zips trazem UF, _BR (presidente) e _BRASIL (concatenacao), com cobertura que varia
  # por ano (1998: DF e presidente so no BRASIL). Le-se tudo, removem-se linhas repetidas
  # no nivel municipio-zona e so entao se agrega.
  csvs <- list.files(tmp, pattern = "_[A-Z]+\\.csv$", full.names = TRUE)
  # chave composta: SQ_CANDIDATO so e unico nacionalmente a partir de 2010;
  # antes ele reinicia por unidade eleitoral
  # NM_TIPO_ELEICAO entra na chave porque em 2004 e 2008 o SQ de pleitos suplementares
  # colide com o de ordinarios na mesma unidade
  chave <- c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO", "SG_UF", "SG_UE", "CD_CARGO",
             "NR_CANDIDATO", "SQ_CANDIDATO")
  linhas <- unique(rbindlist(lapply(csvs, function(f) {
    hdr <- names(fread(f, sep = ";", encoding = "Latin-1", nrows = 0))
    sel <- intersect(c(chave, "CD_MUNICIPIO", "NR_ZONA", "QT_VOTOS_NOMINAIS",
                       "QT_VOTOS_NOMINAIS_VALIDOS", "DS_SIT_TOT_TURNO"), hdr)
    x <- fread(f, sep = ";", encoding = "Latin-1", quote = '"', select = sel,
               colClasses = "character", showProgress = FALSE)
    if (!"QT_VOTOS_NOMINAIS_VALIDOS" %in% names(x)) x[, QT_VOTOS_NOMINAIS_VALIDOS := NA_character_]
    x
  }), use.names = TRUE))
  # em 1998 e 2000 QT_VOTOS_NOMINAIS vem zerado e o voto esta em QT_VOTOS_NOMINAIS_VALIDOS
  linhas[, v := as.numeric(QT_VOTOS_NOMINAIS)]
  linhas[, vv := suppressWarnings(as.numeric(QT_VOTOS_NOMINAIS_VALIDOS))]
  linhas[(is.na(v) | v == 0) & !is.na(vv) & vv > 0, v := vv]
  # situacao de totalizacao vem dos arquivos de votacao (preenchida em todos os anos)
  agg <- linhas[, .(votos = sum(v, na.rm = TRUE),
                    sit_tot_vot = names(which.max(table(DS_SIT_TOT_TURNO)))),
                by = chave]
  rm(linhas); gc()
  write_parquet(agg, dest)
  cat("OK", ano, nrow(agg), "candidaturas-turno\n")
  unlink(tmp, recursive = TRUE)
}
cat("02_ingest_votacao: concluido para anos:", paste(anos, collapse = " "), "\n")
