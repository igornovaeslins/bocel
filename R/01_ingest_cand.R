# 01_ingest_cand.R — le consulta_cand (TSE, layout unificado 71 col), empilha e salva parquet
# Entrada:  data_raw/consulta_cand/consulta_cand_<ANO>.zip
# Saida:    data_raw/parquet/cand_<ANO>.parquet (todas as candidaturas, colunas selecionadas)
# Execucao: Rscript --vanilla R/01_ingest_cand.R
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
raw  <- file.path(root, "data_raw", "consulta_cand")
outd <- file.path(root, "data_raw", "parquet")
dir.create(outd, recursive = TRUE, showWarnings = FALSE)

keep <- c(
  "ANO_ELEICAO", "NR_TURNO", "NM_TIPO_ELEICAO", "TP_ABRANGENCIA", "SG_UF", "SG_UE", "NM_UE",
  "CD_CARGO", "DS_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO",
  "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_CPF_CANDIDATO",
  "NR_TITULO_ELEITORAL_CANDIDATO", "DT_NASCIMENTO", "CD_GENERO", "DS_GENERO",
  "CD_SITUACAO_CANDIDATURA", "DS_SITUACAO_CANDIDATURA",
  "TP_AGREMIACAO", "NR_PARTIDO", "SG_PARTIDO", "NM_PARTIDO",
  "SQ_COLIGACAO", "NM_COLIGACAO", "DS_COMPOSICAO_COLIGACAO",
  "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO", "ST_REELEICAO",
  "SQ_ORDEM_SUPLENCIA", "DT_ELEICAO", "ST_SUBSTITUIDO", "DS_SITUACAO_CANDIDATO_PLEITO"
)

anos <- sort(as.integer(gsub("\\D", "", basename(
  list.files(raw, pattern = "^consulta_cand_\\d{4}\\.zip$")))))
stopifnot(length(anos) > 0)

for (ano in anos) {
  dest <- file.path(outd, sprintf("cand_%d.parquet", ano))
  if (file.exists(dest)) { cat("SKIP", ano, "\n"); next }
  zipf <- file.path(raw, sprintf("consulta_cand_%d.zip", ano))
  tmp  <- file.path(tempdir(), sprintf("cand_%d", ano))
  dir.create(tmp, showWarnings = FALSE)
  unzip(zipf, exdir = tmp)
  # os zips trazem UF, _BR (presidente) e _BRASIL (concatenacao), com cobertura que varia
  # por ano (1998: DF e presidente so no BRASIL). Le-se tudo e removem-se linhas repetidas.
  csvs <- list.files(tmp, pattern = "^consulta_cand_\\d{4}_[A-Z]+\\.csv$", full.names = TRUE)
  stopifnot(length(csvs) >= 26)
  dt <- unique(rbindlist(lapply(csvs, function(f) {
    x <- fread(f, sep = ";", encoding = "Latin-1", quote = '"',
               colClasses = "character", showProgress = FALSE)
    for (k in setdiff(keep, names(x))) x[, (k) := NA_character_]
    x[, ..keep]
  }), use.names = TRUE))
  write_parquet(dt, dest)
  cat("OK", ano, nrow(dt), "linhas\n")
  unlink(tmp, recursive = TRUE)
}
cat("01_ingest_cand: concluido para anos:", paste(anos, collapse = " "), "\n")
