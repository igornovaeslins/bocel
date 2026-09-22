# congela_referencia_ids.R — ancilar, roda uma vez por versao publicada (12/09/2026)
#
# O R/03 numerava id_pessoa pela posicao da chave canonica na ordem alfabetica de todas as chaves,
# e o R/40 numerava os suplentes a partir do maior id do nucleo. Qualquer pessoa nova no universo
# desloca o numero de todas as que vem depois dela. A rodada de 07/09/2026 com a chapa dos vices
# acrescentou 3.471 mandatos e renumerou 482.264 dos 482.687 mandatos em comum, o que desalinha
# todas as tabelas de fonte que guardam id_pessoa.
#
# A referencia congela o id de cada candidatura no ultimo estado verificado. O R/03 e o R/40 passam
# a herdar o id da referencia e so numeram pessoa nova, a partir do maior id ja usado. O id e
# guardado sem prefixo, como inteiro, para que a troca de prefixo nao exija nova referencia.
#
# Fonte do estado verificado de 05/09/2026: data/ocupacoes.csv (linhas de titular, uma por
# mandato, com o id do nucleo) e data/suplentes_identidade.csv (candidaturas de suplente, com o id
# que o R/40 atribuiu em 30/08/2026 e que o R/41 e o R/42 usaram).
# Saida: ref/ids_pessoa_referencia.parquet (chave_cand, id_num, origem)
suppressPackageStartupMessages({library(data.table); library(arrow)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
ESTE <- file.path(root, "R", "congela_referencia_ids.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
num <- function(x) as.integer(sub("^[A-Z]+", "", x))

oc <- fread("data/ocupacoes.csv", select = c("id_mandato", "id_pessoa", "tipo_ocupante"), colClasses = "character")
nuc <- unique(oc[tipo_ocupante == "titular", .(chave_cand = sub("^M", "", id_mandato), id_pessoa)])
stopifnot(uniqueN(nuc$chave_cand) == nrow(nuc))
su <- fread("data/suplentes_identidade.csv", select = c("chave_cand", "id_pessoa"), colClasses = "character")
stopifnot(uniqueN(su$chave_cand) == nrow(su))
# a mesma candidatura nao pode estar nas duas origens com ids diferentes
x <- merge(nuc, su, by = "chave_cand", suffixes = c("_n", "_s"))
stopifnot(x[id_pessoa_n != id_pessoa_s, .N] == 0L)
ref <- rbindlist(list(nuc[, .(chave_cand, id_num = num(id_pessoa), origem = "nucleo")],
                      su[!chave_cand %in% nuc$chave_cand, .(chave_cand, id_num = num(id_pessoa), origem = "suplente")]))
stopifnot(!anyNA(ref$id_num), uniqueN(ref$chave_cand) == nrow(ref))
# id do nucleo e id so de suplente sao espacos disjuntos na referencia
ids_nuc <- unique(ref[origem == "nucleo", id_num])
stopifnot(ref[origem == "suplente" & id_num %in% ids_nuc & !chave_cand %in% su[id_pessoa %in% nuc$id_pessoa, chave_cand], .N] == 0L)
write_parquet(ref, "ref/ids_pessoa_referencia.parquet", compression = "zstd")
reg("ref_ids_candidaturas_nucleo", ref[origem == "nucleo", .N])
reg("ref_ids_candidaturas_suplente", ref[origem == "suplente", .N])
reg("ref_ids_pessoas_nucleo", length(ids_nuc))
reg("ref_ids_pessoas_total", uniqueN(ref$id_num))
reg("ref_ids_maior_id", max(ref$id_num))
cat("referencia:", nrow(ref), "candidaturas,", uniqueN(ref$id_num), "pessoas, maior id", max(ref$id_num), "\n")
