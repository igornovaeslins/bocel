# 57_tce_ac.R — quem respondeu pelas contas anuais de cada prefeitura e camara do Acre, por exercicio,
# lido dos acordaos do TCE-AC (python/fetch_tce_ac.py).
#
# Por que. O TCE-AC nao publica cadastro de gestores, mas julga a prestacao de contas anual de cada
# unidade e o acordao nomeia o responsavel e o exercicio. Isso da, por municipio e ano, o prefeito
# (ou o presidente da camara) que respondeu pelo exercicio, o que confirma exercicio e aponta troca
# de gestor quando o nome nao e o do eleito. A fonte nao da data de posse, de saida nem causa, e
# por isso forma_saida sai sempre nao_observado; data_inicio e data_fim delimitam o exercicio.
#
# Pareamento. Municipio pelo nome contra data/municipios_tse_ibge.csv (com aliases); eleicao pelo
# exercicio (mandato municipal 2001-2004 <- eleicao 2000, etc.); pessoa pelo nome normalizado contra
# os mandatos daquele municipio, cargo e eleicao, primeiro exato, depois Jaro-Winkler >= 0,92 com
# candidato unico. O papel (prefeito, ex-prefeito, presidente) vem do inicio do extrato do relatorio.
#
# Entrada: data_raw/tce_ac/indice_acordaos.csv, data_raw/tce_ac/detalhe/*.json,
#          data/mandatos.csv, data/pessoas.csv, data/municipios_tse_ibge.csv
# Saida:   data/tce_gestores_e.csv (mesmo esquema de tce_gestores_b.csv, fonte tce_ac_jurisprudencia)
#          data/tce_gestores_e_cobertura.csv
# Execucao: Rscript --vanilla R/57_tce_ac.R
set.seed(20260905)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(stringdist) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
ESTE <- "R/57_tce_ac.R"
reg <- function(k, v) registrar_numero(paste0("tceac_", k), v, script = ESTE)
norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }

## ---------------------------------------------------------------- 1. indice e detalhes
idx <- fread("data_raw/tce_ac/indice_acordaos.csv", colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8")
idx <- idx[tipo == "contas" & orgao %in% c("prefeitura", "camara")]
reg("acordaos_contas", nrow(idx))
fs <- list.files("data_raw/tce_ac/detalhe", pattern = "\\.json$", full.names = TRUE)
reg("detalhes_em_disco", length(fs))
EXERC <- "exerc[ií]cio(?:\\s+financeiro)?(?:\\s+e\\s+or[cç]ament[aá]rio)?\\s+(?:de\\s+)?(\\d{4})"
`%||%` <- function(a, b) if (is.null(a)) b else a
dbr <- function(x) { x <- trimws(x); fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", x), paste0(substr(x, 7, 10), "-", substr(x, 4, 5), "-", substr(x, 1, 2)), NA_character_) }
ler_det <- function(f) {
  d <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  if (!is.null(d$data)) d <- d$data          # a API ao vivo embrulha o registro em 'data'
  resp <- d$responsaveis
  if (is.character(resp)) resp <- tryCatch(fromJSON(resp, simplifyVector = FALSE), error = function(e) list())
  nomes <- unique(unlist(lapply(resp, function(r) r$nome_responsavel)))
  nomes <- nomes[!is.na(nomes) & nchar(nomes) >= 5]
  if (!length(nomes)) return(NULL)
  extr <- paste(substr(as.character(d$extrato_completo_relatorio %||% ""), 1, 800),
                as.character(d$objeto %||% ""), as.character(d$ementa %||% ""))
  ex <- regmatches(extr, regexpr(EXERC, extr, ignore.case = TRUE, perl = TRUE))
  ex <- if (length(ex)) sub(".*?(\\d{4})$", "\\1", ex) else NA_character_
  # papel e periodo de cada nome, lidos do extrato: "Sr. X (01/01/2020 a 27/07/2020)", "Sr. X, ex-Prefeito"
  info <- lapply(nomes, function(n) {
    n1 <- strsplit(n, " ")[[1]]; n1 <- paste(n1[1:min(2, length(n1))], collapse = " ")
    m <- regmatches(extr, regexpr(paste0(gsub("([.()])", "\\\\\\1", n1), "[^;]{0,160}"), extr, ignore.case = TRUE, perl = TRUE))
    m <- if (length(m)) m else ""
    per <- regmatches(m, regexpr("\\((\\d{2}/\\d{2}/\\d{4})\\s*a\\s*(\\d{2}/\\d{2}/\\d{4})\\)", m, perl = TRUE))
    ini <- fim <- NA_character_
    if (length(per)) { dd <- regmatches(per, gregexpr("\\d{2}/\\d{2}/\\d{4}", per))[[1]]; ini <- dbr(dd[1]); fim <- dbr(dd[2]) }
    papel <- if (grepl("ex-Prefeit|ex-Presidente|ex-Gestor", m, ignore.case = TRUE)) "ex_gestor" else
             if (grepl("Presidente", m, ignore.case = TRUE)) "presidente" else
             if (grepl("Prefeit", m, ignore.case = TRUE)) "prefeito" else NA_character_
    list(papel = papel, periodo_inicio = ini, periodo_fim = fim)
  })
  data.table(arquivo_pdf = as.character(d$arquivo_pdf %||% sub("\\.json$", "", basename(f))), nome = nomes,
             papel = vapply(info, `[[`, character(1), "papel"),
             periodo_inicio = vapply(info, `[[`, character(1), "periodo_inicio"),
             periodo_fim = vapply(info, `[[`, character(1), "periodo_fim"),
             exercicio_detalhe = ex, n_responsaveis = length(nomes))
}
det <- rbindlist(lapply(fs, ler_det), use.names = TRUE)
cat("acordaos de contas:", nrow(idx), "| detalhes lidos:", length(fs), "| responsaveis nomeados:", nrow(det), "\n")
idx[, chave_pdf := gsub("[^A-Za-z0-9._-]", "_", arquivo_pdf)]
det[, chave_pdf := sub("\\.json$", "", basename(gsub("[^A-Za-z0-9._-]", "_", arquivo_pdf)))]
x <- merge(idx, det[, -"arquivo_pdf"], by = "chave_pdf", all.x = FALSE)
x[, exercicio := fifelse(!is.na(exercicio) & exercicio != "", exercicio, exercicio_detalhe)]
x <- x[!is.na(exercicio) & as.integer(exercicio) >= 1997L & as.integer(exercicio) <= 2026L]
reg("linhas_nome_exercicio", nrow(x))
reg("linhas_com_periodo_no_extrato", x[!is.na(periodo_inicio), .N])

## ---------------------------------------------------------------- 2. municipio e eleicao
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")[sg_uf == "AC"]
mun[, nome_norm := norm(nome_ibge)]
ALIAS <- c("MANUEL URBANO" = "MANOEL URBANO", "BRASILEIA" = "BRASILEIA", "ASSIS BRASIL" = "ASSIS BRASIL")
x[, mun_norm := norm(sub("\\s*[-/]\\s*(AC|Acre)\\s*$", "", municipio_texto, ignore.case = TRUE))]
x[mun_norm %in% names(ALIAS), mun_norm := ALIAS[mun_norm]]
x[mun, on = .(mun_norm = nome_norm), `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge)]
sem_mun <- x[is.na(sg_ue), unique(municipio_texto)]
if (length(sem_mun)) cat("municipios nao resolvidos:", paste(sem_mun, collapse = "; "), "\n")
reg("municipios_nao_resolvidos", length(sem_mun))
x <- x[!is.na(sg_ue)]
x[, ano_eleicao := as.character(((as.integer(exercicio) - 1L) %/% 4L) * 4L)]
x[, cargo_bocel := fifelse(orgao == "prefeitura", "PREFEITO", "VEREADOR")]
x[, cd_cargo := fifelse(orgao == "prefeitura", "11", "13")]

## ---------------------------------------------------------------- 3. pessoa
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""),
           select = c("id_mandato", "id_pessoa", "sg_uf", "sg_ue", "cd_cargo", "ano_eleicao", "mandato_inicio", "mandato_fim"))[sg_uf == "AC"]
p <- fread("data/pessoas.csv", colClasses = "character", select = c("id_pessoa", "nome", "nome_urna_recente"))
m <- merge(m, p, by = "id_pessoa", all.x = TRUE)
m[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente))]
x[, nome_norm := norm(nome)]
x[, `:=`(id_pessoa_bocel = NA_character_, id_mandato_bocel = NA_character_, metodo_pareamento = NA_character_)]
for (i in seq_len(nrow(x))) {
  cand <- m[sg_ue == x$sg_ue[i] & cd_cargo == x$cd_cargo[i] & ano_eleicao == x$ano_eleicao[i]]
  if (!nrow(cand)) next
  hit <- cand[nome_norm == x$nome_norm[i] | urna_norm == x$nome_norm[i]]
  met <- "nome_exato"
  if (nrow(hit) != 1L) {
    d <- stringdist(x$nome_norm[i], cand$nome_norm, method = "jw", p = 0.1)
    ok <- which(d <= 0.08)
    if (length(ok) == 1L) { hit <- cand[ok]; met <- "jaro_winkler_0.92" } else next
  }
  set(x, i, "id_pessoa_bocel", hit$id_pessoa); set(x, i, "id_mandato_bocel", hit$id_mandato); set(x, i, "metodo_pareamento", met)
}
reg("linhas_pareadas", x[!is.na(id_mandato_bocel), .N])
reg("mandatos_pareados", x[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
reg("prefeitos_pareados", x[!is.na(id_mandato_bocel) & cd_cargo == "11", uniqueN(id_mandato_bocel)])
# quem respondeu pelo exercicio sem ser o eleito: sinal de troca de gestor, sem data
# so conta como outro responsavel quem o extrato chama de prefeito ou ex-prefeito, ou o unico nome do
# acordao; secretarios e contadores corresponsaveis ficam de fora
x[, outro_responsavel := is.na(id_mandato_bocel) & cd_cargo == "11" & (papel %in% c("prefeito", "ex_gestor") | n_responsaveis == 1L)]
reg("prefeitura_exercicios_com_outro_responsavel", x[outro_responsavel == TRUE, uniqueN(paste(sg_ue, exercicio))])

## ---------------------------------------------------------------- 4. saida no esquema dos TCEs
out <- x[, .(uf = "AC", tribunal = "TCE-AC", unidade_gestora = entidade_fiscalizada,
             tipo_unidade = orgao, id_municipio_ibge, sg_ue, nome, cpf = NA_character_,
             cargo_fonte = fifelse(is.na(papel), fifelse(orgao == "prefeitura", "responsavel pelas contas da prefeitura", "responsavel pelas contas da camara"), papel),
             cargo_bocel, data_inicio = fifelse(!is.na(periodo_inicio), periodo_inicio, paste0(exercicio, "-01-01")),
             data_fim = fifelse(!is.na(periodo_fim), periodo_fim, paste0(exercicio, "-12-31")),
             situacao_fonte = paste0("contas anuais julgadas: ", resultado, " (acordao ", numero_acordao, ", ", data_sessao, ")"),
             forma_saida = "nao_observado", id_pessoa_bocel, id_mandato_bocel, metodo_pareamento,
             url = paste0("https://jurisprudencia.tceac.tc.br/api/v1/acordao/jurisprudencia/pdf/", arquivo_pdf),
             fonte = "tce_ac_jurisprudencia", nome_municipio_fonte = municipio_texto, exercicio, ano_eleicao)]
setorder(out, sg_ue, tipo_unidade, exercicio, nome)
out <- unique(out, by = c("sg_ue", "tipo_unidade", "exercicio", "nome"))
fwrite(out, "data/tce_gestores_e.csv", na = "NA")
cob <- out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)), municipios = uniqueN(sg_ue)), by = .(tipo_unidade, ano_eleicao)][order(tipo_unidade, ano_eleicao)]
fwrite(cob, "data/tce_gestores_e_cobertura.csv")
print(cob)
reg("linhas_saida", nrow(out))
cat("57_tce_ac: concluido |", nrow(out), "linhas |", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
