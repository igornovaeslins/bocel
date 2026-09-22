# 41_lista_suplencia.R — a fila de suplencia de cada lista partidaria
#
# Decisao da pendencia 5: "precisamos dos mandatos de TODOS os suplentes".
# Esta tabela registra todos os suplentes das eleicoes proporcionais ordinarias, com a
# posicao de cada um na fila da propria lista, que e o que determina quem e convocado
# quando a cadeira vaga. A ocupacao efetiva fica em R/42; aqui esta o universo.
#
# A ordem: o TSE so publica SQ_ORDEM_SUPLENCIA em 2016. Nos demais anos ela e derivada da
# votacao nominal dentro da lista, que e a regra legal (Codigo Eleitoral, art. 112). A
# unidade da lista sai de SQ_COLIGACAO, que o TSE atribui tanto a coligacao quanto ao
# partido isolado, de modo que a mudanca legal de 2020 (fim da coligacao proporcional) e a
# federacao de 2022 entram pelo proprio dado, sem regra escrita a mao. A derivada e
# conferida contra 2016, e a taxa de acerto fica registrada.
#
# Entrada: data_raw/parquet/cand_<ANO>.parquet, votos_<ANO>.parquet, data/suplentes_identidade.csv
# Saida:   data/lista_suplencia.csv|parquet
# Execucao: Rscript --vanilla R/41_lista_suplencia.R
set.seed(20260830)
suppressPackageStartupMessages({library(data.table); library(arrow); library(stringi)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root,"lib","proveniencia.R"))
ESTE <- file.path(root,"R","41_lista_suplencia.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)

ne <- function(x) fifelse(x %in% c("#NE","#NULO","#NULO#","-1","-3","-4",""), NA_character_, x)
unidade_posicao <- function(cd, uf, ue) fcase(cd %in% 11:13, ue, cd %in% 1:2, "BR", default = uf)
CARGOS_PROP <- c(6L,7L,8L,13L)
mand_cargos <- CARGOS_PROP

COLS <- c("ANO_ELEICAO","NM_TIPO_ELEICAO","CD_CARGO","DS_CARGO","SG_UF","SG_UE","NM_UE",
          "SQ_CANDIDATO","NR_CANDIDATO","NM_CANDIDATO","NM_URNA_CANDIDATO","DS_SIT_TOT_TURNO",
          "NR_PARTIDO","SG_PARTIDO","TP_AGREMIACAO","NM_COLIGACAO","DS_COMPOSICAO_COLIGACAO",
          "SQ_COLIGACAO","SQ_ORDEM_SUPLENCIA","NR_TURNO","DT_ELEICAO","DT_NASCIMENTO",
          "DS_SITUACAO_CANDIDATURA","DS_SITUACAO_CANDIDATO_PLEITO")
fs <- list.files(file.path(root,"data_raw","parquet"), pattern="^cand_\\d{4}\\.parquet$", full.names=TRUE)
cand <- rbindlist(lapply(fs, function(f) {
  x <- setDT(read_parquet(f, col_select = COLS))
  x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)) & as.integer(CD_CARGO) %in% CARGOS_PROP]
}), use.names = TRUE)
cand[, `:=`(ano_eleicao = as.integer(ANO_ELEICAO), cd_cargo = as.integer(CD_CARGO),
            nr_turno = as.integer(NR_TURNO), sit_tot = toupper(ne(DS_SIT_TOT_TURNO)))]
cand[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cand[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep="_")]
setorder(cand, chave_cand, -nr_turno)
cand <- cand[, .SD[1], by = chave_cand]

## votos nominais (proporcional tem turno unico)
vf <- list.files(file.path(root,"data_raw","parquet"), pattern="^votos_\\d{4}\\.parquet$", full.names=TRUE)
votos <- rbindlist(lapply(vf, function(f) {
  x <- setDT(read_parquet(f))
  if ("NM_TIPO_ELEICAO" %in% names(x)) x <- x[!grepl("SUPLEMENTAR|EXTRAORDIN", toupper(NM_TIPO_ELEICAO))]
  x <- x[as.integer(CD_CARGO) %in% CARGOS_PROP]
  x[, `:=`(ano_eleicao = as.integer(ANO_ELEICAO), cd_cargo = as.integer(CD_CARGO))]
  x[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
  x[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep="_")]
  x[, .(votos = sum(votos)), by = chave_cand]
}), use.names = TRUE)
votos <- votos[, .(votos = sum(votos)), by = chave_cand]
cand <- merge(cand, votos, by = "chave_cand", all.x = TRUE)

## ---------------------------------------------------------------- a lista partidaria
# SQ_COLIGACAO identifica a lista que concorreu (coligacao, federacao ou partido isolado).
# Onde o TSE nao preenche, a lista e o proprio partido.
cand[, sq_col := ne(as.character(SQ_COLIGACAO))]
cand[, id_lista := paste(ano_eleicao, ue_pos, cd_cargo,
                         fifelse(is.na(sq_col), paste0("P", NR_PARTIDO), sq_col), sep="_")]
n_lista_multipartido <- cand[, .(np = uniqueN(SG_PARTIDO)), by = .(ano_eleicao, id_lista)][
  np > 1, .N, by = ano_eleicao][order(ano_eleicao)]
cat("listas com mais de um partido, por ano:\n"); print(n_lista_multipartido)
for (i in seq_len(nrow(n_lista_multipartido)))
  reg(paste0("lsu_listas_multipartido_", n_lista_multipartido$ano_eleicao[i]), n_lista_multipartido$N[i])

sup <- cand[grepl("SUPLENTE", sit_tot)]
sup[, votos := fifelse(is.na(votos), 0, as.numeric(votos))]
# ordem derivada: votacao nominal decrescente entre os suplentes da lista, com empate
# resolvido pelo mais idoso, que e a regra do Codigo Eleitoral (art. 110). O desempate por
# idade importa mais do que parece, porque 49 mil suplentes tem zero voto nominal e o TSE
# ainda assim lhes atribui posicao na fila: sem ele o acerto contra 2016 fica em 93,97%,
# com ele em 99,99%.
sup[, dt_nasc := as.IDate(ne(DT_NASCIMENTO), format = "%d/%m/%Y")]
setorder(sup, id_lista, -votos, dt_nasc, SQ_CANDIDATO, na.last = TRUE)
sup[, ordem_suplencia_derivada := seq_len(.N), by = id_lista]
sup[, empate_na_ordem := .N > 1, by = .(id_lista, votos, dt_nasc)]
reg("lsu_suplentes_com_empate_de_votos_e_nascimento", sup[empate_na_ordem == TRUE, .N])
reg("lsu_suplentes_sem_votos_na_fonte", sup[votos == 0, .N])

## ---------------------------------------------------------------- validacao contra 2016
sup[, ordem_tse := suppressWarnings(as.integer(ne(as.character(SQ_ORDEM_SUPLENCIA))))]
val <- sup[!is.na(ordem_tse)]
if (nrow(val)) {
  acerto <- val[ordem_tse == ordem_suplencia_derivada, .N] / nrow(val)
  cat(sprintf("validacao da ordem derivada contra o TSE: %d registros, acerto %.4f\n",
              nrow(val), acerto))
  reg("lsu_validacao_n", nrow(val))
  reg("lsu_validacao_acerto_pct", round(100 * acerto, 2))
  reg("lsu_validacao_acerto_primeiro_da_fila",
      round(100 * val[ordem_tse == 1, mean(ordem_suplencia_derivada == 1)], 2))
  fwrite(val[ordem_tse != ordem_suplencia_derivada,
             .(chave_cand, ano_eleicao, cd_cargo, SG_UF, SG_UE, SG_PARTIDO, id_lista,
               votos, ordem_tse, ordem_suplencia_derivada)],
         file.path(root,"output","verificacao","lsu_ordem_divergente.csv"))
}



## ---------------------------------------------------------------- cadeiras de cada lista
# A suplencia pertence a lista, e nao ao candidato: quem assume vem da mesma coligacao,
# federacao ou partido que ganhou a cadeira. Esta tabela liga cada cadeira ja publicada em
# mandatos.csv a sua lista, e e o que permite a R/42 saber quais cadeiras um suplente pode
# ocupar.
mand_prop <- fread(file.path(root,"data","mandatos.csv"),
                   select = c("id_mandato","id_pessoa","cd_cargo","ano_eleicao",
                              "unidade_posicao","sg_partido","forma_saida","data_fim_efetiva",
                              "data_posse","mandato_inicio","mandato_fim"))
mand_prop[, chave_cand := sub("^M", "", id_mandato)]
ml <- merge(mand_prop, cand[, .(chave_cand, id_lista, sq_col, NR_PARTIDO)], by = "chave_cand")
fwrite(ml, file.path(root,"data","mandatos_lista.csv"), quote = TRUE, na = "NA")
reg("lsu_cadeiras_com_lista", nrow(ml))
reg("lsu_cadeiras_proporcionais_no_banco", mand_prop[cd_cargo %in% CARGOS_PROP, .N])

## ---------------------------------------------------------------- suplentes de senador
# A chapa do Senado elege titular e dois suplentes (cargos 9 e 10). O TSE nao marca esses
# dois como eleitos, do mesmo modo que nao marca o vice do executivo, e por isso a condicao
# vem da chapa: o suplente esta eleito quando o titular da mesma chapa foi eleito. A ordem
# da fila vem do proprio cargo, e nao da votacao, porque a chapa ja a fixa no registro.
COLS_S <- COLS
cs <- rbindlist(lapply(fs, function(f) {
  x <- setDT(read_parquet(f, col_select = COLS_S))
  x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)) & as.integer(CD_CARGO) %in% c(5L, 9L, 10L)]
}), use.names = TRUE)
cs[, `:=`(ano_eleicao = as.integer(ANO_ELEICAO), cd_cargo = as.integer(CD_CARGO),
          nr_turno = as.integer(NR_TURNO), sit_tot = toupper(ne(DS_SIT_TOT_TURNO)))]
cs[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cs[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep="_")]
setorder(cs, chave_cand, -nr_turno)
cs <- cs[, .SD[1], by = chave_cand]
sit_eleito <- c("ELEITO","ELEITO POR QP","ELEITO POR MEDIA","ELEITO POR MÉDIA","MÉDIA","MEDIA")
# a chapa eleita, identificada pelo mandato de senador ja publicado no banco
mand_sen <- fread(file.path(root,"data","mandatos.csv"),
                  select = c("id_mandato","id_pessoa","cd_cargo","ano_eleicao",
                             "unidade_posicao","nr_candidato","sg_partido"))
mand_sen <- mand_sen[cd_cargo == 5L]
mand_sen[, nr_candidato := as.character(nr_candidato)]
chapa <- unique(mand_sen[, .(ano_eleicao, ue_pos = unidade_posicao, nr_tit = nr_candidato,
                             id_mandato_titular = id_mandato, id_pessoa_titular = id_pessoa,
                             sg_partido_titular = sg_partido)])
sup_sen <- cs[cd_cargo %in% c(9L, 10L)]
sup_sen[, nr_tit := as.character(NR_CANDIDATO)]
sen1 <- merge(sup_sen, chapa, by = c("ano_eleicao","ue_pos","nr_tit"))
# 1998 e 2000: o numero do suplente pode vir do titular seguido de digito, como no vice
falt <- sup_sen[!chave_cand %in% sen1$chave_cand]
falt[, nr_tit := sub("[12]$", "", as.character(NR_CANDIDATO))]
sen2 <- merge(falt, chapa, by = c("ano_eleicao","ue_pos","nr_tit"))
sen <- rbindlist(list(sen1, sen2), use.names = TRUE)
sen <- sen[!duplicated(chave_cand)]
# a chapa as vezes traz mais de um registro no mesmo posto de suplente, quando houve
# substituicao de candidato; fica o de registro deferido e sequencial mais alto, como em R/03
sen[, def := !grepl("INDEFERIDO|INAPTO|CASSAD|CANCELAD",
                    toupper(paste(DS_SITUACAO_CANDIDATURA, DS_SITUACAO_CANDIDATO_PLEITO)))]

sen[, ordem_cargo := fifelse(cd_cargo == 9L, 1L, 2L)]
setorder(sen, ano_eleicao, ue_pos, nr_tit, ordem_cargo, -def, -SQ_CANDIDATO)
sen <- sen[!duplicated(sen[, .(ano_eleicao, ue_pos, nr_tit, ordem_cargo)])]
reg("lsu_senado_suplentes_de_chapa_eleita", nrow(sen))
reg("lsu_senado_suplentes_sem_chapa", sup_sen[!chave_cand %in% sen$chave_cand, .N])
sen[, id_lista := paste(ano_eleicao, ue_pos, "5", nr_tit, sep = "_")]
setorder(sen, id_lista, ordem_cargo)
# a fila e sequencial na chapa observada: onde o TSE so registra o segundo suplente, ele
# ocupa a primeira posicao da fila, e cargo_registro guarda o posto formal
sen[, `:=`(ordem_suplencia_derivada = seq_len(.N),
           empate_na_ordem = FALSE, ordem_tse = NA_integer_, votos = NA_real_), by = id_lista]

## ---------------------------------------------------------------- montagem
sup <- rbindlist(list(sup, sen), use.names = TRUE, fill = TRUE)
ident <- fread(file.path(root,"data","suplentes_identidade.csv"),
               select = c("chave_cand","id_pessoa","pessoa_tambem_eleita"), colClasses = c(chave_cand="character"))
out <- merge(sup, ident, by = "chave_cand", all.x = TRUE)
lista <- out[, .(
  id_suplencia = paste0("S", chave_cand), chave_cand, id_pessoa,
  ano_eleicao,
  # o suplente de senador concorre nos cargos 9 e 10 do TSE, mas a cadeira que ele pode vir
  # a ocupar e a de senador; o cargo do registro fica preservado ao lado
  cd_cargo_registro = cd_cargo, cargo_registro = toupper(DS_CARGO),
  cd_cargo = fifelse(cd_cargo %in% c(9L, 10L), 5L, cd_cargo),
  cargo = fifelse(cd_cargo %in% c(9L, 10L), "SENADOR", toupper(DS_CARGO)),
  sg_uf = SG_UF, sg_ue = SG_UE, nm_ue = NM_UE, unidade_posicao = ue_pos,
  id_lista, nr_partido = NR_PARTIDO, sg_partido = SG_PARTIDO,
  tp_agremiacao = TP_AGREMIACAO, nm_coligacao = ne(NM_COLIGACAO),
  composicao_coligacao = ne(DS_COMPOSICAO_COLIGACAO),
  nome = ne(NM_CANDIDATO), nome_urna = ne(NM_URNA_CANDIDATO),
  nr_candidato = NR_CANDIDATO, sq_candidato = SQ_CANDIDATO,
  votos_nominais = votos,
  ordem_suplencia = ordem_suplencia_derivada,
  ordem_suplencia_tse = ordem_tse,
  fonte_ordem = fcase(cd_cargo %in% c(9L,10L), "chapa_senado",
                      !is.na(ordem_tse), "tse_e_derivada",
                      default = "derivada_votacao"),
  empate_na_ordem, pessoa_tambem_eleita,
  # o suplente de senador ja nasce ligado a cadeira da propria chapa;
  # nas proporcionais a cadeira so se sabe quando ha convocacao (R/42)
  id_mandato_cadeira = if ("id_mandato_titular" %in% names(out)) id_mandato_titular else NA_character_
)]
setorder(lista, ano_eleicao, unidade_posicao, cd_cargo, id_lista, ordem_suplencia)
fwrite(lista, file.path(root,"data","lista_suplencia.csv"), quote = TRUE, na = "NA")
write_parquet(lista, file.path(root,"data","lista_suplencia.parquet"))
reg("lsu_linhas", nrow(lista))
reg("lsu_listas", uniqueN(lista$id_lista))
reg("lsu_pessoas", uniqueN(lista$id_pessoa))
reg("lsu_sem_id_pessoa", lista[is.na(id_pessoa), .N])
cat("\n41_lista_suplencia: concluido |", nrow(lista), "suplentes em", uniqueN(lista$id_lista), "listas\n")
