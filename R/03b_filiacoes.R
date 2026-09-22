# 03b_filiacoes.R — tabela de filiacoes partidarias das pessoas do banco
# Entrada:  data/pessoas.csv + data_raw/filiacao/filiacao_atual.parquet, filiacao_antiga.parquet
# Saida:    data/filiacoes.csv|parquet|rds (id_pessoa x partido x data_filiacao)
# Execucao: cd ~/bocel && Rscript --vanilla R/03b_filiacoes.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
atual  <- setDT(read_parquet("data_raw/filiacao/filiacao_atual.parquet"))
antiga <- setDT(read_parquet("data_raw/filiacao/filiacao_antiga.parquet"))

chr <- function(x) { x <- as.character(x); fifelse(is.na(x) | x %in% c("None", "NaT", ""), NA_character_, x) }
dt  <- function(x) { x <- chr(x); fifelse(is.na(x), NA_character_, substr(x, 1, 10)) }

a <- atual[, .(
  titulo = chr(titulo), fonte = "lista_atual",
  sigla_partido = chr(sigla_partido), sg_uf = chr(sigla_uf),
  cd_municipio_tse = chr(id_municipio_tse),
  data_filiacao = dt(data_filiacao), situacao_registro = toupper(chr(situacao_registro)),
  data_desfiliacao = dt(data_desfiliacao), data_cancelamento = dt(data_cancelamento),
  motivo = toupper(chr(fcoalesce(chr(motivo_desfiliacao), chr(motivo_cancelamento)))),
  data_referencia = dt(data_extracao)
)]
b <- antiga[, .(
  titulo = chr(titulo), fonte = "lista_antiga",
  sigla_partido = chr(sigla_partido), sg_uf = chr(sigla_uf),
  cd_municipio_tse = chr(id_municipio_tse),
  data_filiacao = dt(data_filiacao), situacao_registro = toupper(chr(situacao_registro)),
  data_desfiliacao = dt(data_desfiliacao), data_cancelamento = dt(data_cancelamento),
  motivo = toupper(chr(motivo_cancelamento)),
  data_referencia = dt(data_processamento)
)]
fil <- unique(rbindlist(list(a, b), use.names = TRUE))
# 21/09/2026: a separacao de pessoas fundidas por documento (R/03) deixa o titulo digitado errado no
# cadastro do TSE em mais de um id_pessoa, e a lista de filiados nao traz nome para desempatar. A
# filiacao desse titulo fica fora do banco, em vez de ir para as duas pessoas
tit_pess <- pess[!is.na(nr_titulo_eleitoral), .(titulo = nr_titulo_eleitoral, id_pessoa)]
tit_rep <- tit_pess[, .N, by = titulo][N > 1L, titulo]
source("lib/proveniencia.R")
registrar_numero("fil_titulos_repetidos_fora_da_ponte", length(tit_rep), script = "R/03b_filiacoes.R")
registrar_numero("fil_registros_de_titulo_repetido_fora_do_banco", fil[titulo %chin% tit_rep, .N], script = "R/03b_filiacoes.R")
fil <- merge(fil, tit_pess[!titulo %chin% tit_rep], by = "titulo", all.x = FALSE)
stopifnot(nrow(fil) > 0)
# mesma filiacao (pessoa, partido, data) presente nas duas listas: fica a atual
setorder(fil, id_pessoa, sigla_partido, data_filiacao, fonte)
fil <- fil[!duplicated(fil[, .(id_pessoa, sigla_partido, sg_uf, data_filiacao)])]
fil[, titulo := NULL]
setcolorder(fil, c("id_pessoa", "sigla_partido", "sg_uf", "cd_municipio_tse", "data_filiacao",
                   "situacao_registro", "data_desfiliacao", "data_cancelamento", "motivo",
                   "fonte", "data_referencia"))

fwrite(fil, "data/filiacoes.csv", na = "NA", quote = TRUE)
write_parquet(fil, "data/filiacoes.parquet")
saveRDS(fil, "data/filiacoes.rds")
cat("filiacoes:", nrow(fil), "registros |", uniqueN(fil$id_pessoa), "pessoas de", nrow(pess), "\n")
