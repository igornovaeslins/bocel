# 08_suplementares.R — eleicoes suplementares (municipal, estadual e federal) inferidas do TSE
# Entrada:  data_raw/parquet/cand_<ANO>.parquet + votos_<ANO>.parquet, data/mandatos.csv, data/pessoas.csv
# Saida:    data/eleicoes_suplementares.csv            (uma linha por pleito suplementar majoritario)
#           data/eleicoes_suplementares_vereador.csv   (uma linha por eleito em pleito suplementar proporcional)
#           data/mandatos_forma_saida_suplementar.csv  (uma linha por mandato ordinario afetado)
#           output/suplementares_contagens.csv, output/numeros_assinatura.txt (registrar_numero)
# Execucao: cd ~/bocel && Rscript --vanilla R/08_suplementares.R
# Nao altera mandatos.csv, pessoas.csv nem posicoes_ano.csv.
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
pq   <- file.path(root, "data_raw", "parquet")
outd <- file.path(root, "data")
script <- "R/08_suplementares.R"
source(file.path(root, "lib", "data_referencia.R"))
data_arquivo <- data_referencia(root)   # pleitos com DT_ELEICAO posterior a esta data (fixa da versao) ainda nao ocorreram

## ---------------------------------------------------------------- funcoes (mesmas convencoes do 03)
ne <- function(x) fifelse(x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue) {
  fcase(cd_cargo %in% 11:13, sg_ue,
        cd_cargo %in% 1:2, "BR",
        default = sg_uf)
}
registro_valido <- function(sit_cand, sit_pleito) {
  a <- toupper(fifelse(is.na(sit_cand), "", sit_cand))
  b <- toupper(fifelse(is.na(sit_pleito), "", sit_pleito))
  !grepl("INAPTO|INDEFERIDO|CASSAD|CANCELAD|RENUNCIA|FALECIDO", a) &
    !grepl("^INDEFERIDO|NEGADO|CASSAD|CANCELAD", b)
}
sit_eleito <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MEDIA", "ELEITO POR MÉDIA", "MÉDIA", "MEDIA")
esfera_map <- c(`1` = "federal", `2` = "federal", `3` = "estadual", `4` = "estadual",
                `5` = "federal", `6` = "federal", `7` = "estadual", `8` = "estadual",
                `9` = "federal", `10` = "federal",
                `11` = "municipal", `12` = "municipal", `13` = "municipal")

## ---------------------------------------------------------------- carga: candidaturas suplementares
cand_files <- list.files(pq, pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE)
stopifnot(length(cand_files) > 0)
tipos <- list()
cand <- rbindlist(lapply(cand_files, function(f) {
  x <- setDT(read_parquet(f))
  tipos[[basename(f)]] <<- x[, .N, by = NM_TIPO_ELEICAO][, arquivo := basename(f)]
  x[, ordinaria := grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x
}), use.names = TRUE)
tipos <- rbindlist(tipos)
fwrite(tipos, "output/suplementares_tipos_eleicao_por_arquivo.csv")
# tudo o que nao e ordinaria e suplementar (confirmado no inventario dos arquivos)
stopifnot(all(grepl("ORDIN|SUPLEMENTAR", toupper(unique(cand$NM_TIPO_ELEICAO)))))

cand[, `:=`(
  ano_eleicao = as.integer(ANO_ELEICAO),
  nr_turno    = as.integer(NR_TURNO),
  cd_cargo    = as.integer(CD_CARGO),
  ds_cargo    = toupper(DS_CARGO),
  titulo      = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)),
  cpf         = ne(num_only(NR_CPF_CANDIDATO)),
  nome        = ne(NM_CANDIDATO),
  sit_tot     = toupper(ne(DS_SIT_TOT_TURNO))
)]
cand[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
cand[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
cand[, titulo := fifelse(is.na(titulo), NA_character_, formatC(titulo, width = 12, flag = "0"))]
cand[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cand[cd_cargo %in% 1:2, SG_UF := "BR"]
cand[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
cand[, dt_ele := as.IDate(DT_ELEICAO, format = "%d/%m/%Y")]

# chaves de candidatura das ordinarias (para detectar colisao de SQ antes de 2010)
chaves_ord <- unique(cand[ordinaria == TRUE, .(chave_cand, nr_turno)])
sup <- cand[ordinaria == FALSE]
rm(cand); invisible(gc())
n_cand_sup <- nrow(sup)
stopifnot(n_cand_sup > 0)
sup[, colide_ordinaria := chave_cand %in% chaves_ord$chave_cand]

## ---------------------------------------------------------------- votos
voto_files <- list.files(pq, pattern = "^votos_\\d{4}\\.parquet$", full.names = TRUE)
votos <- rbindlist(lapply(voto_files, read_parquet))
# a partir da correcao do 02, votos_<ANO> traz NM_TIPO_ELEICAO na chave: ficam so os
# pleitos suplementares e a colisao de SQ com ordinarias deixa de contaminar os votos
votos_separam_tipo <- "NM_TIPO_ELEICAO" %in% names(votos)
votos[, `:=`(ano_eleicao = as.integer(ANO_ELEICAO), nr_turno = as.integer(NR_TURNO),
             cd_cargo = as.integer(CD_CARGO))]
votos[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
votos[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
# rotulos na votacao: 'Eleicao Suplementar' (2008) e 'Eleicao Extraordinaria' (2016+); parte dos
# pleitos suplementares vem rotulada como ordinaria, e entra se a chave nao e de candidatura ordinaria
if (votos_separam_tipo)
  votos <- votos[grepl("SUPLEMENTAR|EXTRAORDIN", toupper(NM_TIPO_ELEICAO)) |
                 !chave_cand %in% chaves_ord$chave_cand]
votos <- votos[chave_cand %in% sup$chave_cand,
               .(votos = sum(votos), sit_tot_vot = toupper(names(which.max(table(sit_tot_vot))))),
               by = .(chave_cand, nr_turno)]
sup <- merge(sup, votos, by = c("chave_cand", "nr_turno"), all.x = TRUE)
# O TSE rotula na votacao com NR_TURNO = 2 o pleito suplementar de 30/10/2022 (mesmo dia do 2o
# turno da eleicao geral), enquanto o cadastro o traz como turno 1 (verificado no zip bruto de
# 2020: mesmo SQ, mesmo nome, DT_ELEICAO 30/10/2022, ELEITO). Quando a candidatura nao tem linha
# de votos em nenhum turno do cadastro e a votacao traz um unico turno para a chave, usa-se esse
# turno (verificador cetico, 28/ago/2026: 8 vencedores ficavam sem votos por esse descompasso)
sup[, n_turnos_cad := .N, by = chave_cand]
sem_voto <- sup[is.na(votos) & n_turnos_cad == 1L, chave_cand]
alt <- votos[chave_cand %in% sem_voto][, n_t := .N, by = chave_cand][n_t == 1L,
             .(chave_cand, votos_alt = votos, sit_alt = sit_tot_vot, turno_votacao = nr_turno)]
sup <- merge(sup, alt, by = "chave_cand", all.x = TRUE)
n_votos_turno_divergente <- sup[is.na(votos) & !is.na(votos_alt), .N]
sup[is.na(votos) & !is.na(votos_alt), `:=`(votos = votos_alt, sit_tot_vot = sit_alt)]
sup[, c("n_turnos_cad", "votos_alt", "sit_alt", "turno_votacao") := NULL]
sup[, sit_tot_vot := fifelse(grepl("^#NULO", sit_tot_vot), NA_character_, sit_tot_vot)]
sup[, sit_tot := fifelse(grepl("^#NULO", sit_tot), NA_character_, sit_tot)]
sup[, fonte_situacao := fifelse(is.na(sit_tot), NA_character_, "cadastro")]
sup[is.na(sit_tot) & !is.na(sit_tot_vot), `:=`(sit_tot = sit_tot_vot, fonte_situacao = "votacao")]
# quando a chave colide com uma candidatura ordinaria do mesmo ano (SQ reiniciava por
# unidade ate 2008), o agregado de votos do 02 soma as duas e nao serve
if (!votos_separam_tipo) sup[colide_ordinaria == TRUE, votos := NA_real_]
sup[, registro_ok := registro_valido(DS_SITUACAO_CANDIDATURA, DS_SITUACAO_CANDIDATO_PLEITO)]
sup[, eleito := sit_tot %in% sit_eleito]

## ---------------------------------------------------------------- pleitos
# Um pleito = (ano do arquivo, unidade da posicao, cargo, data do 1o turno). O 2o turno
# (governador 2017 AM e 2018 TO) e ligado ao pleito pelo SQ_CANDIDATO, que se mantem.
titulares <- c(1L, 3L, 5L, 11L)
sup[, cargo_pleito := fcase(cd_cargo %in% c(2L), 1L, cd_cargo %in% c(4L), 3L,
                            cd_cargo %in% c(9L, 10L), 5L, cd_cargo %in% c(12L), 11L,
                            default = cd_cargo)]
t1 <- sup[nr_turno == 1L, .(ano_eleicao, ue_pos, cargo_pleito, SQ_CANDIDATO, dt_t1 = dt_ele)]
sup <- merge(sup, unique(t1), by = c("ano_eleicao", "ue_pos", "cargo_pleito", "SQ_CANDIDATO"), all.x = TRUE)
# candidatura so com linha de 2o turno (nao ocorre; salvaguarda): usa a propria data
sup[is.na(dt_t1), dt_t1 := dt_ele]
sup[, id_pleito := paste0("S", ano_eleicao, "_", ue_pos, "_", cargo_pleito, "_", format(dt_t1, "%Y%m%d"))]

## ---------------------------------------------------------------- vencedor (majoritarios)
maj <- sup[cd_cargo %in% titulares]
# turno decisivo por candidatura = maior turno presente; por pleito = maior turno
setorder(maj, id_pleito, SQ_CANDIDATO, -nr_turno)
maj_c <- maj[, .SD[1], by = .(id_pleito, SQ_CANDIDATO)]
maj_c[, turno_max := max(nr_turno), by = id_pleito]
maj_c[, tem_eleito := any(eleito), by = id_pleito]
maj_c[, todos_sem_sit := all(is.na(sit_tot)), by = id_pleito]
maj_c[, realizado := dt_t1 <= data_arquivo]
# imputacao pelo mais votado so quando nenhuma candidatura traz situacao (cadastro ou votacao);
# quando o TSE marca explicitamente NAO ELEITO em todas, o pleito fica sem vencedor e vira lacuna
maj_c[tem_eleito == FALSE & todos_sem_sit == TRUE & nr_turno == turno_max & !is.na(votos) & realizado,
      imput := registro_ok & votos == max(votos) & votos > 0, by = id_pleito]
maj_c[imput %in% TRUE, `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO POR VOTOS)",
                            fonte_situacao = "imputacao_votos")]
n_imput_votos <- maj_c[imput %in% TRUE, .N]
# candidato substituido nao e o vencedor
maj_c[ST_SUBSTITUIDO %in% "S" & eleito == TRUE, eleito := FALSE]
setorder(maj_c, id_pleito, -eleito, -votos, -registro_ok, -SQ_CANDIDATO, na.last = TRUE)
venc <- maj_c[eleito == TRUE, .SD[1], by = id_pleito]
# mesma posicao e numero com mais de uma candidatura eleita (substituicao de candidato,
# ex.: SC 82953 em 2007): fica a nao substituida, mais votada, deferida e de SQ mais alto,
# como no 03; so entao se testa a unicidade do vencedor por pleito
el_num <- maj_c[eleito == TRUE, .N, by = .(id_pleito, NR_CANDIDATO)]
n_dup_posicao_numero <- el_num[N > 1, sum(N - 1L)]
maj_c[eleito == TRUE, dup_num := duplicated(paste(id_pleito, NR_CANDIDATO))]
maj_c[dup_num %in% TRUE, eleito := FALSE]
dup2 <- maj_c[eleito == TRUE][, n_el := .N, by = id_pleito][n_el > 1]
fwrite(dup2[, .(id_pleito, NR_CANDIDATO, SQ_CANDIDATO, nome, sit_tot, fonte_situacao, votos,
                DS_SITUACAO_CANDIDATURA, ST_SUBSTITUIDO)], "output/suplementares_pleitos_2_eleitos.csv")
n_pleitos_com_2_eleitos <- uniqueN(dup2$id_pleito)

pleitos <- maj_c[, .(
  ano_arquivo = ano_eleicao[1], unidade_posicao = ue_pos[1], sg_uf = SG_UF[1],
  sg_ue = SG_UE[1], nm_ue = NM_UE[1],
  cd_cargo = cd_cargo[1], cargo = ds_cargo[1], esfera = esfera_map[as.character(cd_cargo[1])],
  dt_eleicao_suplementar = format(dt_t1[1], "%Y-%m-%d"),
  dt_turno_decisivo = format(max(dt_ele), "%Y-%m-%d"),
  nr_turno = max(nr_turno), n_candidatos = .N,
  realizado_ate_data_do_arquivo = realizado[1],
  chave_colide_com_ordinaria = any(colide_ordinaria)
), by = id_pleito]
# chave_colide_com_ordinaria e do PLEITO (alguma candidatura colide); votos_vencedor so fica NA
# quando a chave do proprio vencedor colide (vencedor_chave_colide_com_ordinaria)
pleitos <- merge(pleitos, venc[, .(id_pleito, vencedor_nome = nome, vencedor_titulo = titulo,
                                   vencedor_cpf = cpf, votos_vencedor = votos,
                                   vencedor_chave_colide_com_ordinaria = colide_ordinaria,
                                   sq_candidato = SQ_CANDIDATO, nr_candidato = NR_CANDIDATO,
                                   sg_partido_vencedor = SG_PARTIDO,
                                   situacao_totalizacao = sit_tot, fonte_situacao)],
                 by = "id_pleito", all.x = TRUE)
pleitos[, status_vencedor := fcase(
  !is.na(vencedor_nome), "vencedor_identificado",
  realizado_ate_data_do_arquivo == FALSE, "pleito_posterior_a_data_do_arquivo",
  default = "sem_vencedor_marcado_no_tse")]

# vice da chapa vencedora: mesmo pleito, mesmo numero de candidato, cargo de vice
vices <- sup[cd_cargo %in% c(2L, 4L, 12L),
             .(id_pleito, nr_candidato = NR_CANDIDATO, vice_nome = nome, vice_titulo = titulo,
               vice_cpf = cpf, vice_substituido = ST_SUBSTITUIDO %in% "S", vice_sq = SQ_CANDIDATO)]
setorder(vices, id_pleito, nr_candidato, vice_substituido, -vice_sq)
vices <- vices[, .SD[1], by = .(id_pleito, nr_candidato)][, c("vice_substituido", "vice_sq") := NULL]
pleitos <- merge(pleitos, vices, by = c("id_pleito", "nr_candidato"), all.x = TRUE)

## ---------------------------------------------------------------- id_pessoa via pessoas.csv
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
# 21/09/2026: a separacao de pessoas fundidas por documento (R/03) deixa o titulo ou o CPF digitado
# errado no cadastro do TSE em mais de um id_pessoa. Esse documento deixa de servir de ponte para a
# candidatura suplementar, que segue pelo outro documento, e documento repetido em pessoa que a
# separacao nao partiu interrompe o script
sep_fusao <- fread("output/verificacao/fusao_documentos_separacao.csv", colClasses = "character",
                   na.strings = c("", "NA"))
tit_amb <- pess[!is.na(nr_titulo_eleitoral), .(n = .N, fora_sep = sum(!id_pessoa %chin% sep_fusao$id_pessoa_esperado)),
                by = nr_titulo_eleitoral][n > 1L]
cpf_amb <- pess[!is.na(nr_cpf), .(n = .N, fora_sep = sum(!id_pessoa %chin% sep_fusao$id_pessoa_esperado)),
                by = nr_cpf][n > 1L]
stopifnot(sum(tit_amb$fora_sep) == 0L, sum(cpf_amb$fora_sep) == 0L)
by_tit <- pess[!is.na(nr_titulo_eleitoral) & !nr_titulo_eleitoral %chin% tit_amb$nr_titulo_eleitoral,
               .(nr_titulo_eleitoral, id_tit = id_pessoa)]
by_cpf <- pess[!is.na(nr_cpf) & !nr_cpf %chin% cpf_amb$nr_cpf, .(nr_cpf, id_cpf = id_pessoa)]
checa_unica(as.data.frame(by_tit), "nr_titulo_eleitoral")
checa_unica(as.data.frame(by_cpf), "nr_cpf")
liga_pessoa <- function(dt, col_tit, col_cpf, col_out) {
  dt <- merge(dt, by_tit, by.x = col_tit, by.y = "nr_titulo_eleitoral", all.x = TRUE, sort = FALSE)
  dt <- merge(dt, by_cpf, by.x = col_cpf, by.y = "nr_cpf", all.x = TRUE, sort = FALSE)
  dt[, (col_out) := fifelse(!is.na(id_tit), id_tit, id_cpf)]
  dt[, c("id_tit", "id_cpf") := NULL]
  dt
}
pleitos <- liga_pessoa(pleitos, "vencedor_titulo", "vencedor_cpf", "vencedor_id_pessoa")
pleitos <- liga_pessoa(pleitos, "vice_titulo", "vice_cpf", "vice_id_pessoa")

## ---------------------------------------------------------------- mandato ordinario afetado
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
mand[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo),
            ini = as.IDate(mandato_inicio), fim = as.IDate(mandato_fim))]
# o pleito suplementar substitui o resultado da eleicao ordinaria do mesmo ano de arquivo
# (a candidatura suplementar e registrada sob ANO_ELEICAO da ordinaria que ela repoe);
# para senador, que tem dois mandatos vigentes na mesma UF, e o ano do arquivo que desambigua
afet <- mand[cd_cargo %in% c(1L, 3L, 11L),
             .(id_mandato_ordinario_afetado = id_mandato, ano_arquivo = ano_eleicao,
               unidade_posicao, cd_cargo, ocupante_ordinario_id_pessoa = id_pessoa,
               ocupante_ordinario_nome_urna = NA_character_, ini, fim)]
checa_unica(as.data.frame(afet), c("ano_arquivo", "unidade_posicao", "cd_cargo"))
pleitos <- merge(pleitos, afet, by = c("ano_arquivo", "unidade_posicao", "cd_cargo"), all.x = TRUE)
# senador: nas eleicoes de duas vagas por UF (2002, 2010, 2018) o TSE nao diz qual cadeira
# o suplementar repoe; o mandato afetado fica NA e os candidatos vao para uma coluna propria
sen <- mand[cd_cargo == 5L, .(mandatos_ordinarios_candidatos = paste(sort(id_mandato), collapse = ";"),
                              n_cadeiras = .N, ini = min(ini), fim = max(fim)),
            by = .(ano_arquivo = ano_eleicao, unidade_posicao, cd_cargo)]
pleitos <- merge(pleitos, sen[, .(ano_arquivo, unidade_posicao, cd_cargo, mandatos_ordinarios_candidatos,
                                  ini_sen = ini, fim_sen = fim)],
                 by = c("ano_arquivo", "unidade_posicao", "cd_cargo"), all.x = TRUE)
pleitos[cd_cargo == 5L, `:=`(ini = ini_sen, fim = fim_sen)]
pleitos[, c("ini_sen", "fim_sen") := NULL]
pleitos[, dt_dentro_mandato_ordinario := fifelse(
  is.na(ini), NA, as.IDate(dt_eleicao_suplementar) >= ini & as.IDate(dt_eleicao_suplementar) <= fim)]
# pleito realizado entre a eleicao ordinaria e a posse (nov-dez do ano do arquivo): a eleicao
# foi renovada antes de o mandato ordinario comecar, e o eleito ordinario do banco nao tomou posse
pleitos[, momento := fcase(is.na(ini), NA_character_,
                           as.IDate(dt_eleicao_suplementar) < ini, "antes_da_posse",
                           as.IDate(dt_eleicao_suplementar) > fim, "apos_o_fim_do_mandato",
                           default = "durante_o_mandato")]
pleitos[, vencedor_e_o_ocupante_ordinario := fifelse(
  is.na(vencedor_id_pessoa) | is.na(ocupante_ordinario_id_pessoa), NA,
  vencedor_id_pessoa == ocupante_ordinario_id_pessoa)]
pleitos[, c("ini", "fim", "ocupante_ordinario_nome_urna") := NULL]
pleitos[, fonte := "tse_consulta_cand+votacao"]
setorder(pleitos, ano_arquivo, cd_cargo, sg_uf, unidade_posicao, dt_eleicao_suplementar)
setcolorder(pleitos, c("id_pleito", "ano_arquivo", "unidade_posicao", "sg_uf", "sg_ue", "nm_ue",
                       "cd_cargo", "cargo", "esfera", "dt_eleicao_suplementar", "dt_turno_decisivo",
                       "nr_turno", "n_candidatos", "status_vencedor",
                       "vencedor_nome", "vencedor_titulo", "vencedor_cpf", "vencedor_id_pessoa",
                       "votos_vencedor", "sq_candidato", "nr_candidato", "sg_partido_vencedor",
                       "situacao_totalizacao", "fonte_situacao",
                       "vice_nome", "vice_titulo", "vice_cpf", "vice_id_pessoa",
                       "id_mandato_ordinario_afetado", "ocupante_ordinario_id_pessoa",
                       "dt_dentro_mandato_ordinario", "momento", "vencedor_e_o_ocupante_ordinario",
                       "mandatos_ordinarios_candidatos",
                       "realizado_ate_data_do_arquivo", "chave_colide_com_ordinaria",
                       "vencedor_chave_colide_com_ordinaria", "fonte"))
checa_unica(as.data.frame(pleitos), "id_pleito")

## ---------------------------------------------------------------- vereador (proporcional)
ver <- sup[cd_cargo == 13L]
ver_pleitos <- ver[, .(n_candidatos = .N, n_eleitos = sum(eleito),
                       realizado = dt_t1[1] <= data_arquivo), by = id_pleito]
ver_el <- ver[eleito == TRUE, .(
  id_pleito, ano_arquivo = ano_eleicao, unidade_posicao = ue_pos, sg_uf = SG_UF, sg_ue = SG_UE,
  nm_ue = NM_UE, cd_cargo, cargo = ds_cargo, esfera = "municipal",
  dt_eleicao_suplementar = format(dt_t1, "%Y-%m-%d"), nr_turno,
  eleito_nome = nome, eleito_titulo = titulo, eleito_cpf = cpf, votos = votos,
  sq_candidato = SQ_CANDIDATO, nr_candidato = NR_CANDIDATO, sg_partido = SG_PARTIDO,
  situacao_totalizacao = sit_tot, fonte_situacao, fonte = "tse_consulta_cand+votacao")]
ver_el <- liga_pessoa(ver_el, "eleito_titulo", "eleito_cpf", "eleito_id_pessoa")
# mandatos ordinarios de vereador da mesma camara (ano do arquivo, unidade): afetados em bloco
ver_afet <- mand[cd_cargo == 13L, .(n_mandatos_ordinarios_vereador_na_unidade = .N),
                 by = .(ano_arquivo = ano_eleicao, unidade_posicao)]
ver_el <- merge(ver_el, ver_afet, by = c("ano_arquivo", "unidade_posicao"), all.x = TRUE)
# o eleito no suplementar ja tinha mandato ordinario na mesma camara?
ver_ord <- mand[cd_cargo == 13L, .(ano_arquivo = ano_eleicao, unidade_posicao, eleito_id_pessoa = id_pessoa,
                                   id_mandato_ordinario_mesma_pessoa = id_mandato)]
ver_el <- merge(ver_el, ver_ord, by = c("ano_arquivo", "unidade_posicao", "eleito_id_pessoa"), all.x = TRUE)
setorder(ver_el, ano_arquivo, sg_uf, unidade_posicao, dt_eleicao_suplementar, -votos, na.last = TRUE)
setcolorder(ver_el, c("id_pleito", "ano_arquivo", "unidade_posicao", "sg_uf", "sg_ue", "nm_ue",
                      "cd_cargo", "cargo", "esfera", "dt_eleicao_suplementar", "nr_turno"))

## ---------------------------------------------------------------- forma de saida do mandato ordinario
# um mandato ordinario pode ter mais de um pleito suplementar: ou o vencedor do primeiro tambem
# foi cassado, ou o primeiro pleito foi adiado e o TSE manteve as candidaturas na data original
# sem marcar eleito (ex.: Campestre e Espera Feliz-MG, 11/04/2021 -> 13/06/2021). A data de fim
# inferida e a do PRIMEIRO pleito realizado (a vacancia ja existia); o sucessor vem do PRIMEIRO
# pleito com vencedor identificado, e n_pleitos registra os demais
fs_base <- pleitos[!is.na(id_mandato_ordinario_afetado) & realizado_ate_data_do_arquivo == TRUE]
setorder(fs_base, id_mandato_ordinario_afetado, dt_eleicao_suplementar)
fs_base[, tem_venc := status_vencedor == "vencedor_identificado"]
forma_saida <- fs_base[, {
  iv <- if (any(tem_venc)) which(tem_venc)[1] else 1L
  .(forma_saida = fifelse(momento[1] == "antes_da_posse",
                          "resultado_ordinario_substituido_por_eleicao_suplementar_antes_da_posse",
                          "perda_do_mandato_inferida_por_eleicao_suplementar"),
    momento = momento[1],
    data_fim_inferida = dt_eleicao_suplementar[1],
    id_pleito_suplementar = id_pleito[1],
    sucessor_via_suplementar = vencedor_id_pessoa[iv],
    sucessor_nome = vencedor_nome[iv],
    sucessor_titulo = vencedor_titulo[iv],
    via_sucessao = "eleicao_suplementar",
    id_pleito_sucessor = id_pleito[iv],
    dt_pleito_sucessor = dt_eleicao_suplementar[iv],
    status_vencedor = status_vencedor[iv],
    n_pleitos_suplementares = .N,
    vencedor_e_o_ocupante_ordinario = vencedor_e_o_ocupante_ordinario[iv],
    fonte = "tse_consulta_cand+votacao")
}, by = id_mandato_ordinario_afetado]
n_sucessor_de_pleito_posterior <- forma_saida[id_pleito_sucessor != id_pleito_suplementar, .N]
checa_unica(as.data.frame(forma_saida), "id_mandato_ordinario_afetado")
stopifnot(all(forma_saida$id_mandato_ordinario_afetado %in% mand$id_mandato))

## ---------------------------------------------------------------- salvar
salvar <- function(dt, nome) {
  fwrite(dt, file.path(outd, paste0(nome, ".csv")), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
  cat(nome, ":", nrow(dt), "linhas x", ncol(dt), "colunas\n")
}
salvar(pleitos, "eleicoes_suplementares")
salvar(ver_el, "eleicoes_suplementares_vereador")
salvar(forma_saida, "mandatos_forma_saida_suplementar")

## ---------------------------------------------------------------- numeros registrados
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")
cont <- pleitos[, .N, by = .(ano_arquivo, cd_cargo, cargo)][order(ano_arquivo, cd_cargo)]
fwrite(cont, "output/suplementares_contagens.csv")
for (i in seq_len(nrow(cont)))
  reg(sprintf("sup_n_pleitos_%d_%s", cont$ano_arquivo[i], tolower(gsub("[^A-Za-z]", "", cont$cargo[i]))), cont$N[i])
cont_ver <- ver_pleitos[, .(ano = as.integer(substr(id_pleito, 2, 5)))][, .N, by = ano][order(ano)]
for (i in seq_len(nrow(cont_ver))) reg(sprintf("sup_n_pleitos_%d_vereador", cont_ver$ano[i]), cont_ver$N[i])
reg("sup_n_candidaturas_suplementares", n_cand_sup)
reg("sup_cargos_presentes", paste(sort(unique(sup$cd_cargo)), collapse = ";"))
reg("sup_anos_arquivo_com_suplementar", paste(sort(unique(sup$ano_eleicao)), collapse = ";"))
reg("sup_n_pleitos_majoritarios", nrow(pleitos))
reg("sup_n_pleitos_por_cargo", paste(pleitos[, .N, by = cargo][order(cargo), paste0(cargo, "=", N)], collapse = ";"))
reg("sup_n_pleitos_realizados", pleitos[realizado_ate_data_do_arquivo == TRUE, .N])
reg("sup_n_pleitos_posteriores_a_data", pleitos[realizado_ate_data_do_arquivo == FALSE, .N])
reg("sup_n_pleitos_com_vencedor", pleitos[status_vencedor == "vencedor_identificado", .N])
reg("sup_n_pleitos_sem_vencedor_marcado", pleitos[status_vencedor == "sem_vencedor_marcado_no_tse", .N])
reg("sup_n_pleitos_2_turnos", pleitos[nr_turno == 2L, .N])
reg("sup_n_vencedores_imputados_por_votos", n_imput_votos)
reg("sup_n_pleitos_com_2_eleitos_marcados", n_pleitos_com_2_eleitos)
reg("sup_n_duplicatas_posicao_numero_removidas", n_dup_posicao_numero)
reg("sup_n_pleitos_chave_colide_ordinaria", pleitos[chave_colide_com_ordinaria == TRUE, .N])
reg("sup_n_vencedores_com_votos", pleitos[!is.na(votos_vencedor), .N])
reg("sup_n_candidaturas_votos_ligados_por_turno_divergente", n_votos_turno_divergente)
reg("sup_n_vencedores_sem_votos", pleitos[status_vencedor == "vencedor_identificado" & is.na(votos_vencedor), .N])
reg("sup_n_mandatos_afetados", nrow(forma_saida))
reg("sup_n_mandatos_afetados_mais_de_um_pleito", forma_saida[n_pleitos_suplementares > 1, .N])
reg("sup_n_mandatos_afetados_sucessor_de_pleito_posterior", n_sucessor_de_pleito_posterior)
reg("sup_n_vencedores_chave_colide_ordinaria", pleitos[vencedor_chave_colide_com_ordinaria %in% TRUE, .N])
reg("sup_n_pleitos_sem_mandato_ordinario_no_bocel", pleitos[is.na(id_mandato_ordinario_afetado) & cd_cargo != 5L, .N])
reg("sup_n_pleitos_senador_mandato_ambiguo", pleitos[cd_cargo == 5L & is.na(id_mandato_ordinario_afetado), .N])
reg("sup_n_pleitos_data_fora_do_mandato_ordinario", pleitos[dt_dentro_mandato_ordinario %in% FALSE, .N])
reg("sup_n_pleitos_antes_da_posse", pleitos[momento %in% "antes_da_posse", .N])
reg("sup_n_mandatos_afetados_forma_saida_perda", forma_saida[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar", .N])
reg("sup_n_mandatos_afetados_forma_saida_antes_posse", forma_saida[momento == "antes_da_posse", .N])
reg("sup_n_vencedores_ja_no_bocel", pleitos[!is.na(vencedor_id_pessoa), .N])
reg("sup_n_vencedores_sem_id_pessoa", pleitos[status_vencedor == "vencedor_identificado" & is.na(vencedor_id_pessoa), .N])
reg("sup_n_vencedor_e_ocupante_ordinario", pleitos[vencedor_e_o_ocupante_ordinario %in% TRUE, .N])
reg("sup_n_vices_identificados", pleitos[!is.na(vice_nome), .N])
reg("sup_n_vices_ja_no_bocel", pleitos[!is.na(vice_id_pessoa), .N])
reg("sup_n_sucessores_via_suplementar_no_bocel", forma_saida[!is.na(sucessor_via_suplementar), .N])
reg("sup_n_pleitos_vereador", nrow(ver_pleitos))
reg("sup_n_eleitos_vereador", nrow(ver_el))
reg("sup_n_eleitos_vereador_ja_no_bocel", ver_el[!is.na(eleito_id_pessoa), .N])
reg("sup_n_eleitos_vereador_com_mandato_ordinario_mesma_camara", ver_el[!is.na(id_mandato_ordinario_mesma_pessoa), .N])
reg("sup_data_de_referencia_do_arquivo", format(data_arquivo))
reg("sup_titulos_fora_da_ponte_por_separacao_de_fusao", nrow(tit_amb))
reg("sup_cpfs_fora_da_ponte_por_separacao_de_fusao", nrow(cpf_amb))

## ---------------------------------------------------------------- verificacao
passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ expr; TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  r
}
ok("pleitos: id_pleito unico", checa_unica(as.data.frame(pleitos), "id_pleito"))
ok("pleitos: cargos majoritarios apenas", in_set(pleitos$cd_cargo, c(1L, 3L, 5L, 11L), nome = "cd_cargo"))
ok("pleitos: ano_arquivo em faixa", em_faixa(pleitos$ano_arquivo, 1998, 2024, nome = "ano_arquivo"))
ok("pleitos: data suplementar > ano do arquivo",
   stopifnot(all(as.integer(substr(pleitos$dt_eleicao_suplementar, 1, 4)) >= pleitos$ano_arquivo)))
ok("pleitos: vencedor unico por pleito", stopifnot(n_pleitos_com_2_eleitos == 0))
ok("pleitos: todo vencedor_id_pessoa existe em pessoas",
   stopifnot(all(na.omit(pleitos$vencedor_id_pessoa) %in% pess$id_pessoa)))
ok("pleitos: todo id_mandato afetado existe em mandatos",
   stopifnot(all(na.omit(pleitos$id_mandato_ordinario_afetado) %in% mand$id_mandato)))
ok("pleitos realizados: data ate o fim do mandato ordinario",
   stopifnot(all(pleitos[realizado_ate_data_do_arquivo == TRUE & !is.na(id_mandato_ordinario_afetado),
                         momento != "apos_o_fim_do_mandato"])))
ok("pleitos: pleito futuro nao tem vencedor",
   stopifnot(pleitos[realizado_ate_data_do_arquivo == FALSE & !is.na(vencedor_nome), .N] == 0))
ok("forma_saida: id_mandato unico", checa_unica(as.data.frame(forma_saida), "id_mandato_ordinario_afetado"))
ok("forma_saida: mandato com algum pleito vencido tem sucessor_nome",
   stopifnot(forma_saida[status_vencedor == "vencedor_identificado" & is.na(sucessor_nome), .N] == 0,
             all(forma_saida[n_pleitos_suplementares > 1 & is.na(sucessor_nome), id_mandato_ordinario_afetado] %in%
                 fs_base[, .(v = any(tem_venc)), by = id_mandato_ordinario_afetado][v == FALSE, id_mandato_ordinario_afetado])))
ok("pleitos: votos_vencedor NA sempre que a chave do vencedor colide",
   stopifnot(votos_separam_tipo || all(is.na(pleitos[vencedor_chave_colide_com_ordinaria %in% TRUE, votos_vencedor]))))
ok("forma_saida: data_fim_inferida ISO",
   stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", forma_saida$data_fim_inferida))))
ok("vereador: eleitos com situacao de eleito",
   in_set(ver_el$situacao_totalizacao, sit_eleito, nome = "situacao_totalizacao"))
ok("votos do vencedor em faixa", em_faixa(pleitos$votos_vencedor, 1, 5e6, nome = "votos_vencedor"))
gravar_relatorio_verificacao(
  alvo = "data/eleicoes_suplementares.csv;data/eleicoes_suplementares_vereador.csv;data/mandatos_forma_saida_suplementar.csv",
  script = script, passou = passou, falhou = falhou,
  fora_de_cobertura = c(
    "a causa da perda do mandato (cassacao, anulacao, morte) nao esta no TSE: a forma de saida e inferida da existencia do pleito",
    "o TSE nao informa quem ocupou o cargo entre a saida do titular e a posse do vencedor do suplementar",
    "pleitos suplementares sem vencedor marcado e pleitos posteriores a data do arquivo ficam sem sucessor",
    "posse efetiva do vencedor do suplementar nao observada"))
if (length(falhou)) stop("08_suplementares: verificacao reprovada em ", length(falhou), " itens")
cat("\n08_suplementares: concluido. pleitos majoritarios:", nrow(pleitos),
    "| mandatos afetados:", nrow(forma_saida), "| eleitos vereador:", nrow(ver_el), "\n")
