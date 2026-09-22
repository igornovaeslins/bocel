# build_SP.R — ALESP (SP): posse, fim de exercicio, condicao e forma de saida dos
#   deputados estaduais das legislaturas 14a a 20a (eleicoes de 1998 a 2022),
#   com pareamento ao Banco de Ocupacao de Cargos Eletivos.
#
# Entrada:  data_raw/assembleias2/SP/legislaturas.json          (limites de cada legislatura)
#           data_raw/assembleias2/SP/legislaturas_14_20.json    (composicao por legislatura)
#           data_raw/assembleias2/SP/mandatos.json              (mandato: dtInicio, dtTermino, situacao)
#           data_raw/assembleias2/SP/afastamentos.json          (licencas com tipo e periodo)
#           data_raw/assembleias2/SP/detalhes.json              (filiacoes partidarias por periodo)
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/SP.csv (21 colunas do esquema do banco)
#           output/verificacao/asm2_SP_*.csv
#           output/numeros_assinatura.txt (prefixo asm2sp_)
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/build_SP.R
set.seed(20260829)
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(stringi); library(jsonlite)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
raw  <- file.path(root, "data_raw", "assembleias2", "SP")
outd <- file.path(root, "data", "assembleias2")
verd <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "build_SP.R")
logf   <- file.path(root, "logs", "asm2_build_SP.log")
sink(logf, split = TRUE)
cat("build_SP.R (ALESP) —", format(Sys.time()), "\n")

HOJE <- as.IDate("2026-08-29")
ANOS <- seq(1998L, 2022L, 4L)
UF   <- "SP"
FONTE <- "alesp_api_legis_portal"
API  <- "https://legis-api-portal-prd.al.sp.gov.br"
reg  <- function(k, v) registrar_numero(paste0("asm2sp_", k), v, script = script)

# ------------------------------------------------------------------ normalizacao (identica a R/13)
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
# normalizacao extra (so nas regras marcadas sp_*): remove titulo/profissao no inicio do nome
TITULOS <- c("DR","DRA","DOUTOR","DOUTORA","PROF","PROFA","PROFESSOR","PROFESSORA","PASTOR","PASTORA",
             "DELEGADO","DELEGADA","CORONEL","CEL","MAJOR","CAPITAO","CAP","TENENTE","SARGENTO","SGT",
             "SOLDADO","POLICIAL","AGENTE","FEDERAL","VEREADOR","VEREADORA","DEPUTADO","DEPUTADA",
             "GENERAL","ADVOGADO","ENFERMEIRA","ENFERMEIRO","MEDICO","MEDICA","PADRE","BISPO",
             "SENHOR","SR","SRA","JORNALISTA","APOSTOLO","MISSIONARIA","MISSIONARIO")
sem_titulo <- function(x) {
  y <- norm_nome(x)
  repeat {
    z <- sub(paste0("^(", paste(TITULOS, collapse = "|"), ") "), "", y)
    if (identical(z, y)) break
    y <- z
  }
  y[y == ""] <- NA_character_
  y
}
prim_ult <- function(x) {
  y <- sem_titulo(x)
  vapply(strsplit(y, " ", fixed = TRUE), function(v) {
    v <- v[nzchar(v)]
    if (length(v) < 2) NA_character_ else paste(v[1], v[length(v)])
  }, character(1))
}

## ------------------------------------------------------------------ 1. legislaturas
legs <- as.data.table(fromJSON(file.path(raw, "legislaturas.json"), simplifyVector = TRUE))
legs <- legs[nuLegislatura %in% 14:20,
             .(legislatura = as.integer(nuLegislatura),
               leg_inicio = as.IDate(dtInicio), leg_fim = as.IDate(dtFim))]
legs[, ano_eleicao := 1998L + (legislatura - 14L) * 4L]
setorder(legs, legislatura); print(legs)
stopifnot(nrow(legs) == 7L, all(legs$ano_eleicao %in% ANOS))

## ------------------------------------------------------------------ 2. composicao por legislatura
comp_raw <- fromJSON(file.path(raw, "legislaturas_14_20.json"), simplifyVector = FALSE)
comp <- rbindlist(lapply(names(comp_raw), function(L) {
  rbindlist(lapply(comp_raw[[L]], function(p) data.table(
    legislatura = as.integer(L),
    id_fonte    = as.character(p$nuMatricula),
    id_parlamentar = as.integer(p$idParlamentar),
    nome        = p$txNomeParlamentar,
    partido_lista = if (is.null(p$txPartido)) NA_character_ else p$txPartido)))
}))
cat("composicao ALESP 14a-20a:", nrow(comp), "linhas |", uniqueN(comp$id_fonte), "pessoas\n")
checa_unica(as.data.frame(comp), c("legislatura", "id_fonte"))

## ------------------------------------------------------------------ 3. mandatos (posse, fim, situacao)
man_raw <- fromJSON(file.path(raw, "mandatos.json"), simplifyVector = FALSE)
dt10 <- function(v) if (is.null(v) || is.na(v[1])) as.IDate(NA) else as.IDate(substr(v[1], 1, 10))
txt1 <- function(v) if (is.null(v) || is.na(v[1])) NA_character_ else as.character(v[1])
int1 <- function(v) if (is.null(v) || is.na(v[1])) NA_integer_ else as.integer(v[1])
man <- rbindlist(lapply(man_raw, function(m) data.table(
  id_mandato_alesp = int1(m$idMandato),
  legislatura = int1(m$legislatura$nuLegislatura),
  id_fonte    = txt1(m$parlamentar$nuMatricula),
  id_parlamentar = int1(m$parlamentar$idParlamentar),
  dt_inicio   = dt10(m$dtInicio),
  dt_termino  = dt10(m$dtTermino),
  situacao    = txt1(m$situacao$txSituacao),
  observacao  = txt1(m$txObservacao))))
man <- unique(man)
man <- man[legislatura %in% 14:20]
cat("mandatos ALESP 14a-20a:", nrow(man), "\n")
cat("situacoes de mandato:\n"); print(man[, .N, by = situacao][order(-N)])

# mais de um registro de mandato para a mesma pessoa na mesma legislatura e legitimo (dois periodos)
dup_leg <- man[, .N, by = .(legislatura, id_fonte)][N > 1]
cat("pessoas com mais de um registro de mandato na mesma legislatura:", nrow(dup_leg), "\n")
checa_unica(as.data.frame(man), "id_mandato_alesp")

## ------------------------------------------------------------------ 4. afastamentos (licencas)
af_raw <- fromJSON(file.path(raw, "afastamentos.json"), simplifyVector = FALSE)
af <- if (length(af_raw)) rbindlist(lapply(af_raw, function(a) data.table(
  id_afast = int1(a$idParlamentarAfastamto),
  id_fonte = txt1(a$parlamentar$nuMatricula),
  dt_ini   = dt10(a$dtIni),
  dt_fim   = dt10(a$dtFim),
  tipo     = txt1(a$tipo$txDescricao),
  justificativa = txt1(a$txJustificativa)))) else
  data.table(id_afast = integer(), id_fonte = character(), dt_ini = as.IDate(character()),
             dt_fim = as.IDate(character()), tipo = character(), justificativa = character())
af <- unique(af)
cat("afastamentos (licencas) coletados:", nrow(af), "\n")
if (nrow(af)) print(af[, .N, by = tipo][order(-N)])

## ------------------------------------------------------------------ 5. partido vigente na legislatura
fil <- data.table(id_fonte = character(), partido = character(),
                  dt_ini = as.IDate(character()), dt_fim = as.IDate(character()))
fdet <- file.path(raw, "detalhes.json")
if (file.exists(fdet)) {
  det <- fromJSON(fdet, simplifyVector = FALSE)
  fil <- rbindlist(lapply(det, function(d) {
    lf <- d$listaFiliacoes
    if (is.null(lf) || !length(lf)) return(NULL)
    rbindlist(lapply(lf, function(f) data.table(
      id_fonte = txt1(d$nuMatricula),
      partido  = txt1(f$partido$txSigla),
      dt_ini   = dt10(f$dtInicio),
      dt_fim   = dt10(f$dtFim))))
  }), use.names = TRUE, fill = TRUE)
  fil <- unique(fil[!is.na(partido)])
}
cat("filiacoes partidarias:", nrow(fil), "\n")

## ------------------------------------------------------------------ 6. base: composicao x mandato
ex <- merge(comp, legs, by = "legislatura", all.x = TRUE)
ex <- merge(ex, man[, .(legislatura, id_fonte, id_mandato_alesp, dt_inicio, dt_termino,
                        situacao, observacao)],
            by = c("legislatura", "id_fonte"), all.x = TRUE, allow.cartesian = TRUE)
cat("linhas apos juntar mandato:", nrow(ex), "| sem registro de mandato:",
    ex[is.na(id_mandato_alesp), .N], "\n")

# partido: filiacao que cobre o inicio do exercicio (ou o inicio da legislatura)
ex[, dt_ref := fifelse(is.na(dt_inicio), leg_inicio, dt_inicio)]
if (nrow(fil)) {
  fj <- fil[ex[, .(id_fonte, legislatura, id_mandato_alesp, dt_ref)], on = "id_fonte",
            allow.cartesian = TRUE]
  fj <- fj[!is.na(dt_ini) & dt_ini <= dt_ref & (is.na(dt_fim) | dt_fim >= dt_ref)]
  setorder(fj, id_fonte, legislatura, -dt_ini)
  fj <- fj[, .SD[1], by = .(id_fonte, legislatura, id_mandato_alesp)]
  ex <- merge(ex, fj[, .(id_fonte, legislatura, id_mandato_alesp, partido_fil = partido)],
              by = c("id_fonte", "legislatura", "id_mandato_alesp"), all.x = TRUE)
} else ex[, partido_fil := NA_character_]
ex[, partido := fifelse(!is.na(partido_fil), partido_fil, partido_lista)]
cat("linhas com partido:", ex[!is.na(partido), .N], "de", nrow(ex), "\n")

## ------------------------------------------------------------------ 7. pareamento ao BOCEL
bocel_m <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("sq_candidato", "nr_candidato")))
bocel_p <- fread(file.path(root, "data", "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bocel_m[cd_cargo == 7L & sg_uf == UF,
             .(id_mandato, id_pessoa, ano_eleicao, sg_uf, cd_cargo, sq_candidato, nr_candidato)]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome)], by = "id_pessoa")
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
urna <- rbindlist(lapply(list.files(file.path(root, "data_raw", "parquet"),
                                    pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE), function(f) {
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                            "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_TIPO_ELEICAO")))
  x <- x[as.integer(CD_CARGO) == 7L & SG_UF == UF & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sg_uf = SG_UF, cd_cargo = as.integer(CD_CARGO),
        nr_candidato = as.character(NR_CANDIDATO), sq_candidato = as.character(SQ_CANDIDATO),
        nome_urna = NM_URNA_CANDIDATO)]
}))
urna <- urna[!duplicated(urna[, .(ano_eleicao, sg_uf, cd_cargo, nr_candidato, sq_candidato)])]
dep <- merge(dep, urna, by = c("ano_eleicao", "sg_uf", "cd_cargo", "nr_candidato", "sq_candidato"),
             all.x = TRUE)
dep[, `:=`(nome_urna_norm = norm_nome(nome_urna),
           nome_bocel_st = sem_titulo(nome_bocel), nome_urna_st = sem_titulo(nome_urna),
           nome_bocel_pu = prim_ult(nome_bocel),  nome_urna_pu = prim_ult(nome_urna))]
cat("eleitos BOCEL SP cd_cargo 7:", nrow(dep), "| com nome de urna:", dep[!is.na(nome_urna), .N], "\n")
stopifnot(nrow(dep) == 658L)

ex[, `:=`(nome_normalizado = norm_nome(nome), nome_completo = NA_character_)]
ex[nome_normalizado == "", nome_normalizado := NA_character_]
ex[, uf := UF]
# o pareamento e feito no nivel pessoa x legislatura e depois propagado aos registros de mandato
# (uma pessoa pode ter dois periodos de exercicio na mesma legislatura, e o mandato do TSE e um so)
exp <- unique(ex[, .(uf, legislatura, ano_eleicao, id_fonte, nome, nome_normalizado)])
exp[, `:=`(nome_completo_norm = NA_character_, nome_st = sem_titulo(nome), nome_pu = prim_ult(nome))]
exp[, rid := .I]
exp[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, metodo_pareamento = NA_character_)]

# regra de pareamento identica a R/13_exercicio_assembleias.R (unicidade exigida nos dois lados)
parear <- function(ex, dep, col_ex, col_dep, metodo) {
  a <- ex[is.na(id_mandato) & !is.na(get(col_ex)), .(rid, sg_uf = uf, ano_eleicao, chave = get(col_ex))]
  b <- dep[!is.na(get(col_dep)), .(id_mandato, id_pessoa, sg_uf, ano_eleicao, chave = get(col_dep))]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  a <- a[, if (.N == 1L) .SD, by = .(sg_uf, ano_eleicao, chave)]
  b <- b[, if (.N == 1L) .SD, by = .(sg_uf, ano_eleicao, chave)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  m <- merge(a, b, by = c("sg_uf", "ano_eleicao", "chave"))
  m <- m[!id_mandato %in% ex$id_mandato]
  ex[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, metodo_pareamento = metodo)]
  cat(sprintf("  %-32s +%d\n", metodo, nrow(m)))
  invisible(ex)
}
cat("pareamento por regra (ordem de R/13 primeiro, extras da UF depois):\n")
parear(exp, dep, "nome_completo_norm", "nome_bocel_norm",  "nome_completo_x_nome_bocel")
parear(exp, dep, "nome_normalizado",   "nome_urna_norm", "nome_parlamentar_x_urna")
parear(exp, dep, "nome_normalizado",   "nome_bocel_norm",  "nome_parlamentar_x_nome_bocel")
parear(exp, dep, "nome_completo_norm", "nome_urna_norm", "nome_completo_x_urna")
# extras desta UF: o nome parlamentar da ALESP carrega titulo/profissao que o nome de urna nem sempre traz
parear(exp, dep, "nome_st", "nome_urna_st", "sp_sem_titulo_x_urna")
parear(exp, dep, "nome_st", "nome_bocel_st",  "sp_sem_titulo_x_nome_bocel")
parear(exp, dep, "nome_pu", "nome_urna_pu", "sp_primeiro_ultimo_x_urna")
parear(exp, dep, "nome_pu", "nome_bocel_pu",  "sp_primeiro_ultimo_x_nome_bocel")

# extras baseados em alias: o nome de urna do TSE traz variantes ("CAMARINHA / ABELARDO
# CAMARINHA", "ROQUE BARBIERE - ROQUINHO") e a ALESP usa o nome parlamentar corrente.
STOPW <- c("DE","DA","DO","DAS","DOS","E","DI","DEL","VAN","VON")
toks <- function(x) lapply(strsplit(x, " ", fixed = TRUE), function(v) {
  v <- v[nzchar(v)]; v <- v[nchar(v) > 1L]; unique(setdiff(v, STOPW))
})
partes_alias <- function(s) {
  ps <- unlist(strsplit(s, "[/-]"))
  unique(c(norm_nome(ps), sem_titulo(ps)))
}
alias <- rbindlist(lapply(seq_len(nrow(dep)), function(k) {
  a <- unique(na.omit(c(dep$nome_urna_norm[k], dep$nome_bocel_norm[k],
                        dep$nome_urna_st[k], dep$nome_bocel_st[k],
                        if (!is.na(dep$nome_urna[k])) partes_alias(dep$nome_urna[k]),
                        if (!is.na(dep$nome_bocel[k]))  partes_alias(dep$nome_bocel[k]))))
  a <- a[nzchar(a) & nchar(a) >= 4L]
  if (!length(a)) return(NULL)
  data.table(id_mandato = dep$id_mandato[k], id_pessoa = dep$id_pessoa[k],
             ano_eleicao = dep$ano_eleicao[k], sg_uf = UF, chave = a)
}))
alias <- unique(alias)
parear_alias <- function(ex, alias, col_ex, metodo) {
  a <- ex[is.na(id_mandato) & !is.na(get(col_ex)), .(rid, sg_uf = uf, ano_eleicao, chave = get(col_ex))]
  if (!nrow(a)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  a <- a[, if (.N == 1L) .SD, by = .(sg_uf, ano_eleicao, chave)]
  b <- alias[, if (uniqueN(id_mandato) == 1L) .SD[1], by = .(sg_uf, ano_eleicao, chave)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  m <- merge(a, b, by = c("sg_uf", "ano_eleicao", "chave"))
  m <- m[!id_mandato %in% ex$id_mandato]
  if (nrow(m)) m <- m[, if (.N == 1L) .SD, by = id_mandato]
  if (nrow(m)) ex[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                                      metodo_pareamento = metodo)]
  cat(sprintf("  %-32s +%d\n", metodo, nrow(m)))
  invisible(ex)
}
parear_alias(exp, alias, "nome_normalizado", "sp_alias_urna_ou_civil")
parear_alias(exp, alias, "nome_st",          "sp_alias_sem_titulo")

# containment de tokens nos dois sentidos, exigindo unicidade dos dois lados
parear_tokens <- function(ex, dep, metodo) {
  a <- ex[is.na(id_mandato) & !is.na(nome_st), .(rid, ano_eleicao, chave = nome_st)]
  b <- dep[!id_mandato %in% ex$id_mandato, .(id_mandato, id_pessoa, ano_eleicao,
                                             urna = nome_urna_st, civil = nome_bocel_st)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  cand <- b[a, on = "ano_eleicao", allow.cartesian = TRUE]
  ta <- toks(cand$chave); tu <- toks(cand$urna); tc <- toks(cand$civil)
  contido <- function(p, q) {
    length(p) > 0L && length(q) > 0L && all(p %in% q) &&
      (length(p) >= 2L || (length(p) == 1L && nchar(p[1]) >= 5L))
  }
  keep <- mapply(function(A, U, C) contido(A, U) || contido(U, A) ||
                                    contido(A, C) || contido(C, A), ta, tu, tc)
  cand <- cand[which(keep)]
  if (!nrow(cand)) { cat(sprintf("  %-32s +0\n", metodo)); return(invisible(ex)) }
  cand <- cand[, if (uniqueN(id_mandato) == 1L) .SD[1], by = rid]
  cand <- cand[, if (uniqueN(rid) == 1L) .SD[1], by = id_mandato]
  ex[cand, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                            metodo_pareamento = metodo)]
  cat(sprintf("  %-32s +%d\n", metodo, nrow(cand)))
  invisible(ex)
}
parear_tokens(exp, dep, "sp_tokens_contidos")

# pessoa sem mandato pareado: nome unico entre pessoas com mandato na UF (regra de R/13)
pess_uf <- merge(bocel_m[sg_uf == UF, .(id_pessoa, sg_uf)],
                 bocel_p[, .(id_pessoa, nome_norm = norm_nome(nome))], by = "id_pessoa")
pess_uf <- unique(pess_uf[, .(id_pessoa, sg_uf, nome_norm)])
pess_uf <- pess_uf[, if (uniqueN(id_pessoa) == 1L) .SD[1], by = .(sg_uf, nome_norm)]
np <- exp[is.na(id_pessoa) & !is.na(nome_normalizado)]
np <- merge(np[, .(rid, sg_uf = uf, nome_norm = nome_normalizado)], pess_uf,
            by = c("sg_uf", "nome_norm"))
np <- np[, if (.N == 1L) .SD, by = rid]
exp[np, on = "rid", `:=`(id_pessoa = i.id_pessoa, metodo_pareamento = "pessoa_nome_parlamentar_uf")]
cat(sprintf("  %-32s +%d\n", "pessoa_nome_parlamentar_uf", nrow(np)))

# propaga o pareamento (pessoa x legislatura) para os registros de mandato
ex <- merge(ex, exp[, .(legislatura, id_fonte, id_mandato, id_pessoa, metodo_pareamento)],
            by = c("legislatura", "id_fonte"), all.x = TRUE)
ex[, rid := .I]
cat("linhas pareadas a um mandato do BOCEL:", ex[!is.na(id_mandato), .N], "de", nrow(ex), "\n")

## ------------------------------------------------------------------ 8. condicao (titular / suplente)
# titular: pareado a um mandato de eleito do TSE (unica evidencia direta de titularidade)
# suplente: nao pareado e com entrada em exercicio DEPOIS do inicio da legislatura
# nao_informado: nao pareado e com entrada no primeiro dia, ou sem data — a contagem de
#   entradas no primeiro dia excede as 94 cadeiras em algumas legislaturas, entao a data de
#   entrada sozinha nao prova titularidade.
ex[, condicao := fcase(
  !is.na(id_mandato), "titular",
  !is.na(dt_inicio) & dt_inicio >  leg_inicio, "suplente",
  default = "nao_informado")]
cat("entradas no primeiro dia da legislatura:", ex[!is.na(dt_inicio) & dt_inicio <= leg_inicio, .N],
    "| cadeiras por legislatura: 94\n")
cat("condicao:\n"); print(ex[, .N, by = condicao][order(-N)])

## ------------------------------------------------------------------ 9. forma de saida
# vocabulario fechado do banco
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
# A distribuicao do intervalo entre o fim do mandato e o fim da legislatura e bimodal e sem
# ruido de escrituracao: a maioria fecha exatamente no fim da legislatura e o resto tem intervalo
# grande (o pico secundario e 42 dias, saida em 31 de janeiro de quem assumiu mandato federal).
# Por isso a comparacao e exata, sem tolerancia arbitraria.
TOL <- 0L    # tolerancia em dias entre o fim do mandato e o fim da legislatura
JAN <- 3L    # janela em dias para casar o fim do mandato com o inicio de um afastamento

# afastamento da propria pessoa que comeca na saida (explica a saida antecipada)
ex[, `:=`(af_tipo = NA_character_, af_just = NA_character_)]
if (nrow(af)) {
  cand <- af[ex[!is.na(dt_termino), .(rid, id_fonte, dt_termino)], on = "id_fonte",
             allow.cartesian = TRUE]
  cand <- cand[!is.na(dt_ini) & abs(as.integer(dt_ini - dt_termino)) <= JAN]
  setorder(cand, rid, dt_ini)
  cand <- cand[, .SD[1], by = rid]
  ex[cand, on = "rid", `:=`(af_tipo = i.tipo, af_just = i.justificativa)]
}
cat("saidas explicadas por afastamento da propria pessoa:", ex[!is.na(af_tipo), .N], "\n")

ex[, `:=`(em_curso    = !is.na(dt_termino) & dt_termino > HOJE,
          antecipado  = !is.na(dt_termino) & !is.na(leg_fim) &
                        as.integer(leg_fim - dt_termino) > TOL)]
sit <- function(x) toupper(stri_trans_general(ifelse(is.na(x), "", x), "Latin-ASCII"))
ex[, forma_saida := fcase(
  is.na(dt_termino),                                        "nao_observado",
  grepl("FALEC|MORTE", sit(situacao)),                      "falecimento",
  grepl("CASSA|PERDA DE MANDATO", sit(situacao)),           "cassacao",
  grepl("RENUNC", sit(situacao)),                           "renuncia",
  em_curso == TRUE,                                          NA_character_,
  antecipado == TRUE & grepl("LICEN", sit(af_tipo)),        "licenca",
  antecipado == TRUE & !is.na(af_tipo),                     "afastamento",
  antecipado == TRUE,                                       "outro",
  default = "fim_regular")]
# causa_original: texto da fonte, sem traducao
ex[, causa_original := {
  v <- ifelse(is.na(situacao), "", situacao)
  v <- ifelse(!is.na(observacao), paste0(v, " | ", observacao), v)
  v <- ifelse(!is.na(af_tipo), paste0(v, " | afastamento: ", af_tipo), v)
  v <- ifelse(!is.na(af_just), paste0(v, " (", af_just, ")"), v)
  v <- trimws(v); ifelse(v == "", NA_character_, v)
}]
ex[, gap_fim := as.integer(leg_fim - dt_termino)]
gapd <- ex[!is.na(gap_fim), .N, by = gap_fim][order(gap_fim)]
fwrite(gapd, file.path(verd, "asm2_SP_gap_fim_legislatura.csv"))
cat("intervalo (dias) entre fim do mandato e fim da legislatura — 12 primeiros:\n"); print(head(gapd, 12))
cat("forma_saida:\n"); print(ex[, .N, by = forma_saida][order(-N)])
cat("forma_saida x condicao:\n"); print(dcast(ex[, .N, by = .(condicao, forma_saida)],
                                              condicao ~ forma_saida, value.var = "N", fill = 0))

## ------------------------------------------------------------------ 10. saida no esquema do banco
ex[, url := fifelse(!is.na(id_mandato_alesp),
                    sprintf("%s/presencaPlenario/mandatosParlamentar/%d", API, id_parlamentar),
                    sprintf("%s/parlamentar-portal/?filtroLegislatura=%d", API, legislatura))]
setorder(ex, ano_eleicao, condicao, nome_normalizado, dt_inicio, na.last = TRUE)
saida <- ex[, .(uf, fonte = FONTE, legislatura = as.character(legislatura), ano_eleicao,
                nome, nome_normalizado, nome_completo, data_nascimento = NA_character_,
                partido, condicao,
                data_inicio_exercicio = as.character(dt_inicio),
                data_fim_exercicio    = as.character(dt_termino),
                causa_original, forma_saida,
                id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato, metodo_pareamento,
                url, id_fonte, votos_fonte = NA_character_, sexo_fonte = NA_character_)]

## ------------------------------------------------------------------ 11. asserts
in_set(saida$forma_saida, VOCAB, permitir_na = TRUE, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente", "nao_informado"), permitir_na = FALSE, nome = "condicao")
in_set(saida$uf, "SP", permitir_na = FALSE, nome = "uf")
in_set(saida$legislatura, as.character(14:20), permitir_na = FALSE, nome = "legislatura")
em_faixa(saida$ano_eleicao, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
em_faixa(as.integer(format(as.IDate(saida$data_inicio_exercicio), "%Y")), 1999, 2026,
         permitir_na = TRUE, nome = "ano_inicio_exercicio")
em_faixa(as.integer(format(as.IDate(saida$data_fim_exercicio), "%Y")), 1999, 2027,
         permitir_na = TRUE, nome = "ano_fim_exercicio")
# granularidade: uma linha por legislatura x matricula x registro de mandato da ALESP
checa_unica(as.data.frame(cbind(saida[, .(legislatura, id_fonte)],
                                idm = ex$id_mandato_alesp)),
            c("legislatura", "id_fonte", "idm"))
# um mandato do BOCEL nao pode ser atribuido a duas pessoas distintas da fonte
dupm <- saida[!is.na(id_mandato_bocel), .(n = uniqueN(id_fonte)), by = id_mandato_bocel][n > 1]
stopifnot(nrow(dupm) == 0L)
# todo id_mandato_bocel existe em mandatos.csv com cd_cargo 7 e sg_uf SP e ano de eleicao coerente
chk <- merge(saida[!is.na(id_mandato_bocel), .(id_mandato_bocel, ano_eleicao)],
             dep[, .(id_mandato_bocel = id_mandato, ano_bocel = ano_eleicao)], by = "id_mandato_bocel")
stopifnot(nrow(chk) == saida[!is.na(id_mandato_bocel), .N], all(chk$ano_eleicao == chk$ano_bocel))
# datas coerentes: inicio <= fim
stopifnot(saida[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio) &
                as.IDate(data_inicio_exercicio) > as.IDate(data_fim_exercicio), .N] == 0L)
COLS <- c("uf","fonte","legislatura","ano_eleicao","nome","nome_normalizado","nome_completo",
          "data_nascimento","partido","condicao","data_inicio_exercicio","data_fim_exercicio",
          "causa_original","forma_saida","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento",
          "url","id_fonte","votos_fonte","sexo_fonte")
stopifnot(identical(names(saida), COLS))

fwrite(saida, file.path(outd, "SP.csv"), sep = ",", na = "NA", quote = TRUE)
registrar_fonte(file.path(outd, "SP.csv"), "ALESP — API legis-api-portal-prd (portal da propria casa)",
                url = paste0(API, "/presencaPlenario/mandatosParlamentar/{idParlamentar}"))

## ------------------------------------------------------------------ 12. cobertura e numeros
cob <- dep[, .(n_bocel = .N), by = ano_eleicao]
cob <- merge(cob, saida[!is.na(id_mandato_bocel), .(n_pareados = uniqueN(id_mandato_bocel)), by = ano_eleicao],
             by = "ano_eleicao", all.x = TRUE)
cob <- merge(cob, saida[, .(n_fonte = .N,
                            n_titular = sum(condicao == "titular"),
                            n_suplente = sum(condicao == "suplente"),
                            n_com_inicio = sum(!is.na(data_inicio_exercicio)),
                            n_com_fim = sum(!is.na(data_fim_exercicio)),
                            n_com_forma = sum(!is.na(forma_saida) & forma_saida != "nao_observado")),
                        by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
setorder(cob, ano_eleicao)
fwrite(cob, file.path(verd, "asm2_SP_cobertura_ano.csv"))
cat("\ncobertura por ano de eleicao:\n"); print(cob)

fwrite(saida[, .N, by = .(forma_saida, condicao)][order(-N)],
       file.path(verd, "asm2_SP_forma_saida.csv"))
fwrite(saida[is.na(id_mandato_bocel), .(ano_eleicao, nome, condicao, data_inicio_exercicio,
                                      data_fim_exercicio, id_fonte)][order(ano_eleicao, nome)],
       file.path(verd, "asm2_SP_nao_pareados.csv"))
fwrite(dep[!id_mandato %in% saida$id_mandato_bocel,
           .(ano_eleicao, id_mandato, nome_bocel, nome_urna)][order(ano_eleicao, nome_bocel)],
       file.path(verd, "asm2_SP_bocel_sem_fonte.csv"))

reg("n_linhas", nrow(saida))
reg("n_pessoas_fonte", uniqueN(saida$id_fonte))
reg("n_legislaturas", uniqueN(saida$legislatura))
reg("n_mandatos_bocel_sp_cargo7", nrow(dep))
reg("n_pareadas", saida[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_bocel_pareados", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]))
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f",
    uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]), nrow(dep),
    uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]) / nrow(dep)))
reg("n_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("n_com_data_fim", saida[!is.na(data_fim_exercicio), .N])
reg("n_com_forma_saida_observada", saida[!is.na(forma_saida) & forma_saida != "nao_observado", .N])
reg("n_com_causa_original", saida[!is.na(causa_original), .N])
reg("n_titular", saida[condicao == "titular", .N])
reg("n_suplente", saida[condicao == "suplente", .N])
reg("n_condicao_nao_informado", saida[condicao == "nao_informado", .N])
reg("n_entrada_primeiro_dia_legislatura", ex[!is.na(dt_inicio) & dt_inicio <= leg_inicio, .N])
reg("n_gap_zero_fim_legislatura", ex[!is.na(gap_fim) & gap_fim == 0L, .N])
reg("n_gap_positivo_saida_antecipada", ex[!is.na(gap_fim) & gap_fim > 0L, .N])
reg("n_gap_42_dias_saida_31jan", ex[!is.na(gap_fim) & gap_fim == 42L, .N])
reg("n_afastamentos_coletados", nrow(af))
reg("n_saidas_explicadas_por_afastamento", ex[!is.na(af_tipo), .N])
for (v in sort(unique(na.omit(saida$forma_saida))))
  reg(paste0("n_forma_saida_", v), saida[forma_saida == v, .N])
for (m in sort(unique(na.omit(saida$metodo_pareamento))))
  reg(paste0("n_pareadas_metodo_", m), saida[metodo_pareamento == m, .N])
for (i in seq_len(nrow(cob)))
  reg(sprintf("taxa_pareamento_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
reg("n_partido_preenchido", saida[!is.na(partido), .N])

cat("\nOK — data/assembleias2/SP.csv com", nrow(saida), "linhas\n")
sink()
