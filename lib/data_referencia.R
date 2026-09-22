# data_referencia.R — data de referencia da versao do BOCEL (19/09/2026)
#
# Mandato com fim posterior a esta data esta em curso, e pleito com data posterior ainda nao ocorreu. A data e fixa e
# gravada em output/data_referencia.txt (escrita pelo R/53 na primeira execucao), para que a reconstrucao em outro dia de
# o mesmo resultado (pendencia da data de referencia). BOCEL_DATA_REFERENCIA no ambiente sobrepoe
# o arquivo. Sem nenhum dos dois o produtor aborta, porque cair no dia da execucao e justamente o que se quer evitar.
data_referencia <- function(root = Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))) {
  d <- Sys.getenv("BOCEL_DATA_REFERENCIA", unset = "")
  if (!nzchar(d)) {
    f <- file.path(root, "output", "data_referencia.txt")
    if (!file.exists(f)) stop("output/data_referencia.txt ausente: fixe a data de referencia da versao antes de rodar a cadeia")
    d <- trimws(readLines(f, warn = FALSE)[1])
  }
  stopifnot(grepl("^\\d{4}-\\d{2}-\\d{2}$", d))
  data.table::as.IDate(d)
}
