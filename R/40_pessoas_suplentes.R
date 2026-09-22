# 40_pessoas_suplentes.R — identidade de pessoa estendida aos suplentes
#
# Decisao de 30/08/2026 (pendencia 5):
# "suplente e eleito", os suplentes precisam aparecer sem aumentar o N de cadeiras.
# Escolha de escopo confirmada em 30/08: a deduplicacao roda sobre eleitos e suplentes
# juntos, de modo que todo suplente ganha id_pessoa estavel no MESMO espaco de
# identificador do nucleo; o cadastro principal continua com quem ocupou cadeira e os
# suplentes nunca convocados saem em data/pessoas_suplentes.csv.
#
# Restricao dura: os id_pessoa ja verificados do nucleo NAO mudam, porque as tabelas de
# exercicio, as descritivas e os numeros registrados dependem deles. Componente que
# contem eleito herda o id do eleito; componente so de suplentes recebe id novo,
# numerado deterministicamente pela chave canonica a partir do maximo do nucleo.
#
# Entrada: data_raw/parquet/cand_<ANO>.parquet, data/mandatos.csv
# Saida:   data/pessoas_suplentes.csv|parquet, data/suplentes_identidade.csv (mapa
#          candidatura -> id_pessoa), output/verificacao/pontes_via_suplente.csv
# Execucao: Rscript --vanilla R/40_pessoas_suplentes.R
set.seed(20260830)
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(stringi)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
ESTE <- file.path(root, "R", "40_pessoas_suplentes.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
`%||%` <- function(a, b) if (length(a) == 0 || is.na(a[1])) b else a[1]

## ------------------------------------------------------- normalizacao (identica a R/03)
ne <- function(x) fifelse(x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue)
  fcase(cd_cargo %in% 11:13, sg_ue, cd_cargo %in% 1:2, "BR", default = sg_uf)

COLS <- c("ANO_ELEICAO","NM_TIPO_ELEICAO","CD_CARGO","DS_CARGO","SG_UF","SG_UE","NM_UE",
          "SQ_CANDIDATO","NR_CANDIDATO","NM_CANDIDATO","NM_URNA_CANDIDATO",
          "NR_CPF_CANDIDATO","NR_TITULO_ELEITORAL_CANDIDATO","DT_NASCIMENTO","DS_GENERO",
          "DS_SIT_TOT_TURNO","NR_PARTIDO","SG_PARTIDO","TP_AGREMIACAO","NM_COLIGACAO",
          "DS_COMPOSICAO_COLIGACAO","SQ_COLIGACAO","SQ_ORDEM_SUPLENCIA","NR_TURNO",
          "DS_SITUACAO_CANDIDATURA","DS_SITUACAO_CANDIDATO_PLEITO","ST_SUBSTITUIDO")

fs <- list.files(file.path(root,"data_raw","parquet"), pattern="^cand_\\d{4}\\.parquet$", full.names=TRUE)
stopifnot(length(fs) == 14)
cand <- rbindlist(lapply(fs, function(f) {
  x <- setDT(read_parquet(f, col_select = COLS))
  x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
}), use.names = TRUE)

cand[, `:=`(
  ano_eleicao = as.integer(ANO_ELEICAO), cd_cargo = as.integer(CD_CARGO),
  nr_turno = as.integer(NR_TURNO),
  titulo = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)),
  cpf    = ne(num_only(NR_CPF_CANDIDATO)),
  nome   = ne(NM_CANDIDATO), nome_urna = ne(NM_URNA_CANDIDATO),
  dt_nasc = ne(DT_NASCIMENTO), genero = ne(DS_GENERO),
  sit_tot = toupper(ne(DS_SIT_TOT_TURNO))
)]
cand[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
cand[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
cand[, titulo := fifelse(is.na(titulo), NA_character_, formatC(titulo, width = 12, flag = "0"))]
cand[, nome_norm := stri_trans_general(toupper(nome), "Latin-ASCII")]
cand[, nome_norm := gsub("[^A-Z ]", "", nome_norm)]
cand[, nome_norm := gsub(" +", " ", trimws(nome_norm))]
cand[, dt_nasc_iso := {
  d <- as.IDate(dt_nasc, format = "%d/%m/%Y")
  fifelse(is.na(d) | d < as.IDate("1890-01-01") | d > as.IDate("2010-01-01"),
          NA_character_, format(d, "%Y-%m-%d"))
}]
cand[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cand[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
# uma linha por candidatura (turno decisivo), como em R/03
setorder(cand, chave_cand, -nr_turno)
cand <- cand[, .SD[1], by = chave_cand]

## ------------------------------------------------------- universos
mand <- fread(file.path(root,"data","mandatos.csv"),
              select = c("id_mandato","id_pessoa","cd_cargo","ano_eleicao"), colClasses = "character")
mand[, chave_cand := sub("^M", "", id_mandato)]
n_mand <- nrow(mand)

# eleitos: as candidaturas que geraram mandato, com o id_pessoa ja atribuido pelo nucleo
elei <- merge(cand, mand[, .(chave_cand, id_pessoa_nucleo = id_pessoa)], by = "chave_cand")
stopifnot(nrow(elei) == n_mand)

# Suplentes de duas naturezas. Nas proporcionais, quem o TSE marca como SUPLENTE (dep
# federal, estadual, distrital e vereador). No Senado, os cargos 9 e 10 sao os dois
# suplentes da chapa, que o TSE nao marca como eleitos, do mesmo modo que nao marca o vice
# do executivo; entram todos aqui, e R/41 fica com o filtro de chapa eleita.
CARGOS_PROP <- c(6L, 7L, 8L, 13L)
sup <- cand[(grepl("SUPLENTE", sit_tot) & cd_cargo %in% CARGOS_PROP) | cd_cargo %in% c(9L, 10L)]
sup[, id_pessoa_nucleo := NA_character_]
reg("sup_candidaturas_suplente", nrow(sup))
reg("sup_candidaturas_eleitas", nrow(elei))

# registro sem nome, titulo nem CPF nao identifica pessoa (mesma regra do nucleo)
n_sup_sem_id <- sup[is.na(nome) & is.na(titulo) & is.na(cpf), .N]
sup <- sup[!(is.na(nome) & is.na(titulo) & is.na(cpf))]
reg("sup_sem_identidade_excluidos", n_sup_sem_id)

reg_all <- rbindlist(list(
  elei[, .(chave_cand, titulo, cpf, nome, nome_norm, nome_urna, dt_nasc_iso, genero,
           ano_eleicao, cd_cargo, sg_uf = SG_UF, sg_ue = SG_UE, sg_partido = SG_PARTIDO,
           id_pessoa_nucleo, tipo = "eleito")],
  sup[,  .(chave_cand, titulo, cpf, nome, nome_norm, nome_urna, dt_nasc_iso, genero,
           ano_eleicao, cd_cargo, sg_uf = SG_UF, sg_ue = SG_UE, sg_partido = SG_PARTIDO,
           id_pessoa_nucleo, tipo = "suplente")]
), use.names = TRUE)

## ------------------------------------------------------- componentes (label propagation)
# Mesmas tres regras do nucleo: mesmo titulo, ou mesmo CPF, ou mesmo (nome_norm, nascimento).
# Label propagation em data.table no lugar da union-find recursiva de R/03, porque o
# universo passa de 481 mil para mais de 2 milhoes de registros; o resultado (componentes
# conexos) e o mesmo, e R/verifica_ocupacoes.R confere a equivalencia no nucleo.
reg_all[, rid := .I]
reg_all[, k_nom := fifelse(!is.na(nome_norm) & !is.na(dt_nasc_iso),
                           paste(nome_norm, dt_nasc_iso), NA_character_)]
reg_all[, lab := rid]
propagar <- function(col) {
  reg_all[!is.na(get(col)), lab_min := min(lab), by = col]
  n <- reg_all[!is.na(lab_min) & lab_min < lab, .N]
  reg_all[!is.na(lab_min) & lab_min < lab, lab := lab_min]
  reg_all[, lab_min := NULL]
  n
}
it <- 0L
repeat {
  it <- it + 1L
  mudou <- propagar("titulo") + propagar("cpf") + propagar("k_nom")
  cat("  iteracao", it, "- rotulos alterados:", mudou, "\n")
  if (mudou == 0L || it > 40L) break
}
stopifnot(it <= 40L)
reg("sup_iteracoes_componentes", it)

## ------------------------------------------------------- atribuicao de id
# Regra: o eleito conserva sempre o id que o nucleo publicou, sem excecao. O suplente
# recebe o id do eleito com quem compartilha uma chave forte, na ordem titulo, CPF,
# nome com nascimento. So quando nenhuma chave direta resolve o componente decide, e
# apenas se o componente tiver um unico id do nucleo.
#
# Por que nao deixar o componente mandar: 65 componentes reunem mais de um id do nucleo,
# e a inspecao mostra que a ponte vem de registro do TSE com dado cruzado (o titulo de
# uma pessoa ao lado do CPF de outra, nomes e nascimentos distintos), e nao de identidade
# real. Fundir esses ids alteraria pessoas ja publicadas com base em erro de cadastro, de
# modo que os casos ficam em output/verificacao/pontes_via_suplente.csv para exame.
comp <- reg_all[tipo == "eleito", .(ids = uniqueN(id_pessoa_nucleo),
                                    id_nucleo = sort(unique(id_pessoa_nucleo))[1]), by = lab]
pontes <- comp[ids > 1]
reg("sup_pontes_novas_entre_ids_do_nucleo", nrow(pontes))
if (nrow(pontes)) {
  det <- reg_all[lab %in% pontes$lab & tipo == "eleito",
                 .(lab, chave_cand, id_pessoa_nucleo, nome, titulo, cpf, dt_nasc_iso, ano_eleicao)]
  dir.create(file.path(root,"output","verificacao"), showWarnings = FALSE, recursive = TRUE)
  fwrite(det[order(lab)], file.path(root,"output","verificacao","pontes_via_suplente.csv"))
  reg("sup_pontes_novas_pessoas_envolvidas", uniqueN(det$id_pessoa_nucleo))
}
reg_all <- merge(reg_all, comp[, .(lab, id_nucleo, ids_no_componente = ids)], by = "lab", all.x = TRUE)

# mapas de chave forte -> id, so onde a chave leva a uma unica pessoa do nucleo
mapa_chave <- function(col) {
  m <- reg_all[tipo == "eleito" & !is.na(get(col)),
               .(n_ids = uniqueN(id_pessoa_nucleo), id = id_pessoa_nucleo[1]), by = c(col)]
  setnames(m, col, "chave")
  m[n_ids == 1, .(chave, id)]
}
m_tit <- mapa_chave("titulo"); m_cpf <- mapa_chave("cpf"); m_nom <- mapa_chave("k_nom")
reg_all[m_tit, id_tit := i.id, on = .(titulo = chave)]
reg_all[m_cpf, id_cpf := i.id, on = .(cpf = chave)]
reg_all[m_nom, id_nom := i.id, on = .(k_nom = chave)]
reg_all[, id_direto := fcase(!is.na(id_tit), id_tit, !is.na(id_cpf), id_cpf,
                             !is.na(id_nom), id_nom, default = NA_character_)]
reg_all[, regra_id := fcase(
  tipo == "eleito", "nucleo",
  !is.na(id_tit), "titulo", !is.na(id_cpf), "cpf", !is.na(id_nom), "nome_nascimento",
  !is.na(id_nucleo) & ids_no_componente == 1L, "componente",
  !is.na(id_nucleo), "componente_ambiguo", default = "novo")]
reg("sup_pessoa_resolvida_por_titulo", reg_all[regra_id == "titulo", .N])
reg("sup_pessoa_resolvida_por_cpf", reg_all[regra_id == "cpf", .N])
reg("sup_pessoa_resolvida_por_nome_nascimento", reg_all[regra_id == "nome_nascimento", .N])
reg("sup_pessoa_resolvida_por_componente", reg_all[regra_id == "componente", .N])
reg("sup_pessoa_em_componente_ambiguo", reg_all[regra_id == "componente_ambiguo", .N])

# componente so de suplentes: id novo, numerado pela chave canonica (mesma regra de R/03)
novos <- reg_all[regra_id == "novo"]
canon <- novos[, {
  t <- sort(titulo[!is.na(titulo)]); c <- sort(cpf[!is.na(cpf)])
  n <- sort(k_nom[!is.na(k_nom)])
  key <- if (length(t)) paste0("TIT:", t[1]) else
         if (length(c)) paste0("CPF:", c[1]) else
         if (length(n)) paste0("NOM:", n[1]) else paste0("RID:", rid[1])
  .(chave_canonica = key)
}, by = lab]
pess_nuc <- fread(file.path(root,"data","pessoas.csv"), select = "id_pessoa", colClasses = "character")
# 12/09/2026: a componente so de suplentes herda o id da referencia congelada
# (ref/ids_pessoa_referencia.parquet) quando suas candidaturas levam a um unico id de suplente; so a
# componente sem candidatura na referencia recebe numero novo, a partir do maior id ja usado. Antes o
# numero partia do maior id do nucleo e seguia a ordem da chave canonica, de modo que cada pessoa
# nova no nucleo deslocava todos os suplentes.
PREFIXO_ID <- "BOCEL"
ids_nuc_num <- as.integer(sub("^[A-Z]+", "", pess_nuc$id_pessoa))
f_ref <- file.path(root, "ref", "ids_pessoa_referencia.parquet")
if (file.exists(f_ref)) {
  ref <- setDT(read_parquet(f_ref))
  hs <- merge(novos[, .(lab, chave_cand)], ref[origem == "suplente", .(chave_cand, id_num)], by = "chave_cand")
  pl <- hs[, .(n_ids = uniqueN(id_num), id_num = min(id_num)), by = lab]
  reg("sup_ids_componente_com_mais_de_um_id_de_referencia", pl[n_ids > 1L, .N])
  stopifnot(pl[n_ids > 1L, .N] == 0L)
  reg("sup_ids_referencia_tomados_pelo_nucleo", pl[id_num %in% ids_nuc_num, .N])
  pl <- pl[!id_num %in% ids_nuc_num]
  stopifnot(!anyDuplicated(pl$id_num))
  canon <- merge(canon, pl[, .(lab, id_num)], by = "lab", all.x = TRUE)
  maxi <- max(c(ids_nuc_num, ref$id_num))
} else {
  canon[, id_num := NA_integer_]
  maxi <- max(ids_nuc_num)
}
setorder(canon, chave_canonica)
reg("sup_ids_herdados_da_referencia", canon[!is.na(id_num), .N])
reg("sup_ids_novos", canon[is.na(id_num), .N])
canon[is.na(id_num), id_num := maxi + seq_len(.N)]
stopifnot(!anyDuplicated(canon$id_num), !any(canon$id_num %in% ids_nuc_num))
canon[, id_pessoa_novo := paste0(PREFIXO_ID, formatC(id_num, width = 7, flag = "0"))]
canon[, id_num := NULL]
reg_all <- merge(reg_all, canon[, .(lab, id_pessoa_novo, chave_canonica)], by = "lab", all.x = TRUE)

reg_all[, id_pessoa := fcase(
  tipo == "eleito", id_pessoa_nucleo,
  regra_id %in% c("titulo","cpf","nome_nascimento"), id_direto,
  regra_id %in% c("componente","componente_ambiguo"), id_nucleo,
  default = id_pessoa_novo)]
stopifnot(!any(is.na(reg_all$id_pessoa)))
# o id publicado do nucleo nao muda, e este assert e a guarda disso
conf <- reg_all[tipo == "eleito" & id_pessoa != id_pessoa_nucleo, .N]
stopifnot(conf == 0L)
stopifnot(uniqueN(reg_all[tipo == "eleito", .(chave_cand, id_pessoa)]) == n_mand)
reg("sup_ids_nucleo_preservados", n_mand)

## ------------------------------------------------------- cadastro dos suplentes
# pessoa de suplente que NUNCA foi eleita (as demais ja estao em pessoas.csv)
so_sup <- reg_all[regra_id == "novo"]
setorder(so_sup, id_pessoa, -ano_eleicao)
pess_sup <- so_sup[, .(
  nome = nome[1], nome_urna_recente = nome_urna[1],
  dt_nascimento = na.omit(dt_nasc_iso)[1] %||% NA_character_,
  genero = na.omit(genero)[1] %||% NA_character_,
  nr_titulo_eleitoral = na.omit(titulo)[1] %||% NA_character_,
  nr_cpf = na.omit(cpf)[1] %||% NA_character_,
  n_candidaturas_suplente = .N,
  primeiro_ano_suplente = min(ano_eleicao), ultimo_ano_suplente = max(ano_eleicao),
  chave_dedup = fcase(any(!is.na(titulo)), "titulo", any(!is.na(cpf)), "cpf",
                      default = "nome_nascimento"),
  condicao_no_banco = "suplente_nao_eleito"
), by = id_pessoa]

fwrite(pess_sup, file.path(root,"data","pessoas_suplentes.csv"), quote = TRUE, na = "NA")
write_parquet(pess_sup, file.path(root,"data","pessoas_suplentes.parquet"))
# mapa candidatura -> pessoa, insumo de R/41 e R/42
mapa <- reg_all[tipo == "suplente", .(chave_cand, id_pessoa, ano_eleicao, cd_cargo,
                                      sg_uf, sg_ue, sg_partido, regra_id,
                                      pessoa_tambem_eleita = regra_id != "novo")]
fwrite(mapa, file.path(root,"data","suplentes_identidade.csv"), quote = TRUE, na = "NA")

reg("sup_pessoas_suplentes_nunca_eleitas", nrow(pess_sup))
reg("sup_pessoas_suplentes_tambem_eleitas", uniqueN(mapa[pessoa_tambem_eleita == TRUE]$id_pessoa))
reg("sup_pessoas_novas_no_espaco_de_id", uniqueN(reg_all[regra_id == "novo"]$id_pessoa))
reg("sup_pessoas_total_universo", uniqueN(reg_all$id_pessoa))
reg("sup_candidaturas_mapeadas", nrow(mapa))

cat("\n40_pessoas_suplentes: concluido\n")
cat("  candidaturas-suplente:", nrow(mapa), "\n")
cat("  pessoas novas (suplente nunca eleito):", nrow(pess_sup), "\n")
cat("  pessoas do universo:", uniqueN(reg_all$id_pessoa), "\n")
cat("  pontes novas entre ids do nucleo:", nrow(pontes), "\n")
