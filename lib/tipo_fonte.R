# tipo_fonte.R — tipo de fonte de uma URL curada, no vocabulario da saida (21/09/2026)
#
# Mesma regra de dominio e caminho do classificador de R/auditoria_fontes_e_linguagem.R (dominio de governo, casa
# legislativa, tribunal ou ministerio publico e oficial; dentro dele, caminho de agencia ou noticia e noticia de
# orgao publico; o DHBB da FGV e base academica; Wikipedia e Wikidata sao pista; o resto e imprensa), com o nome de
# cada tipo no vocabulario que a saida dos executivos usa (regra A1: oficial > base_dhbb >
# noticia_orgao_publico > pista, e noticia de imprensa comum e Wikipedia nunca viram fonte, so pista).
#
# Uso: source("lib/tipo_fonte.R")
#   tipo_fonte(u)              -> um dos 5 rotulos, para uma URL
#   tipo_fonte_vec(x)          -> vetorizada
#   melhor_tipo_fonte(u1, u2)  -> o melhor dos dois (fonte_1 e fonte_2 de uma linha curada), pela ordem de admissibilidade
#   rotulo_evento_curado(u1, u2) -> fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada ou pista_nao_oficial
orig_fonte <- function(u) sub("^https?://web[.]archive[.]org/web/[0-9a-z_*]+/", "", u)
dominio_fonte <- function(u) tolower(sub("^www[0-9]*[.]", "", sub("^(https?://)?([^/:?#]+).*$", "\\2", orig_fonte(u))))
OFICIAL_FONTE <- "(^|[.])(gov|leg|jus|mp|def|tc)[.]br$|^(camara|senado|planalto|in)[.]"
# Portal oficial servido por fornecedor fora do dominio publico (transparencia da ALMT na Eloweb), conferido caso a caso.
OFICIAL_TERCEIRO_FONTE <- "^almt[.]eloweb[.]net$"
NOTICIA_PATH_FONTE <- "(noticia|agencia|comunicacao|imprensa/|/news|materia)"

tipo_fonte <- function(u) {
  if (is.na(u) || !nzchar(trimws(u))) return(NA_character_)
  d <- dominio_fonte(u); caminho <- tolower(orig_fonte(u))
  if (grepl("wikipedia[.]org$|wikidata[.]org$", d)) return("wikipedia")
  if (grepl("(^|[.])fgv[.]br$", d)) return("base_dhbb")
  if (grepl(OFICIAL_FONTE, d) || grepl(OFICIAL_TERCEIRO_FONTE, d)) {
    return(if (grepl(NOTICIA_PATH_FONTE, caminho) && !grepl("diario|doe|dou|publicacao", caminho)) "noticia_orgao_publico" else "oficial")
  }
  "imprensa"
}
tipo_fonte_vec <- function(x) vapply(x, tipo_fonte, character(1), USE.NAMES = FALSE)

ORDEM_TIPO_FONTE <- c("oficial", "base_dhbb", "noticia_orgao_publico", "wikipedia", "imprensa")
melhor_tipo_fonte <- function(u1, u2 = NA_character_) {
  t1 <- tipo_fonte_vec(u1); t2 <- tipo_fonte_vec(u2)
  ORDEM_TIPO_FONTE[pmin(match(t1, ORDEM_TIPO_FONTE, nomatch = 99L), match(t2, ORDEM_TIPO_FONTE, nomatch = 99L))]
}

rotulo_evento_curado <- function(u1, u2 = NA_character_) {
  m <- melhor_tipo_fonte(u1, u2)
  out <- rep(NA_character_, length(m))
  out[m %in% "oficial"] <- "fonte_oficial_curada"
  out[m %in% "base_dhbb"] <- "base_dhbb_curada"
  out[m %in% "noticia_orgao_publico"] <- "noticia_orgao_publico_curada"
  out[m %in% c("wikipedia", "imprensa")] <- "pista_nao_oficial"
  out
}
