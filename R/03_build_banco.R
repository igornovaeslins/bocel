# 03_build_banco.R — constroi o banco: pessoas, mandatos, painel pessoa x cargo x ano, sucessao
# Entrada:  data_raw/parquet/cand_<ANO>.parquet + votos_<ANO>.parquet
# Saida:    data/mandatos.csv|parquet|rds, data/posicoes_ano.csv|parquet|rds, data/pessoas.csv|parquet|rds
# Execucao: Rscript --vanilla R/03_build_banco.R
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
pq   <- file.path(root, "data_raw", "parquet")
outd <- file.path(root, "data")
dir.create(outd, showWarnings = FALSE)
`%||%` <- function(a, b) if (length(a) == 0 || is.na(a[1])) b else a[1]

## ---------------------------------------------------------------- carga
cand_files <- list.files(pq, pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE)
stopifnot(length(cand_files) > 0)
cand <- rbindlist(lapply(cand_files, function(f) {
  x <- setDT(read_parquet(f))
  # somente eleicoes ordinarias na v1 (suplementares declaradas na nota de cobertura)
  x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
}), use.names = TRUE)

voto_files <- list.files(pq, pattern = "^votos_\\d{4}\\.parquet$", full.names = TRUE)
votos <- if (length(voto_files)) rbindlist(lapply(voto_files, read_parquet)) else NULL
# somente votacao de eleicoes ordinarias (a chave de votos_<ANO> traz NM_TIPO_ELEICAO)
# (rotulos na votacao: 'Eleicao Ordinaria', 'Eleicao Suplementar', 'Eleicao Extraordinaria')
if (!is.null(votos) && "NM_TIPO_ELEICAO" %in% names(votos))
  votos <- votos[!grepl("SUPLEMENTAR|EXTRAORDIN", toupper(NM_TIPO_ELEICAO))]

## ---------------------------------------------------------------- limpeza
ne <- function(x) fifelse(x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)

cand[, `:=`(
  ano_eleicao = as.integer(ANO_ELEICAO),
  nr_turno    = as.integer(NR_TURNO),
  cd_cargo    = as.integer(CD_CARGO),
  ds_cargo    = toupper(DS_CARGO),
  titulo      = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)),
  cpf         = ne(num_only(NR_CPF_CANDIDATO)),
  nome        = ne(NM_CANDIDATO),
  nome_urna   = ne(NM_URNA_CANDIDATO),
  dt_nasc     = ne(DT_NASCIMENTO),
  genero      = ne(DS_GENERO),
  sit_tot     = toupper(ne(DS_SIT_TOT_TURNO))
)]
cand[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
cand[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
cand[, titulo := fifelse(is.na(titulo), NA_character_,
                         formatC(titulo, width = 12, flag = "0"))]
cand[, nome_norm := stri_trans_general(toupper(nome), "Latin-ASCII")]
cand[, nome_norm := gsub("[^A-Z ]", "", nome_norm)]
cand[, nome_norm := gsub(" +", " ", trimws(nome_norm))]
cand[, dt_nasc_iso := {
  d <- as.IDate(dt_nasc, format = "%d/%m/%Y")
  fifelse(is.na(d) | d < as.IDate("1890-01-01") | d > as.IDate("2010-01-01"),
          NA_character_, format(d, "%Y-%m-%d"))
}]

## ---------------------------------------------------------------- unidade da posicao
# codigos TSE: 1 pres, 2 vice-pres, 3 gov, 4 vice-gov, 5 senador, 6 dep fed,
# 7 dep est, 8 dep distrital, 9/10 suplentes de senador, 11 pref, 12 vice-pref, 13 vereador
# A unidade em que a posicao e disputada: municipio (SG_UE) para cargos municipais,
# UF para estaduais e federais, BR para presidente. Nos arquivos de votacao dos cargos
# gerais o SG_UE e o municipio de apuracao, por isso a chave usa a unidade da posicao.
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue) {
  fcase(cd_cargo %in% 11:13, sg_ue,
        cd_cargo %in% 1:2, "BR",
        default = sg_uf)
}
cand[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cand[cd_cargo %in% 1:2, SG_UF := "BR"]
# chave da candidatura: SQ_CANDIDATO so e unico nacionalmente a partir de 2010;
# antes reinicia por unidade eleitoral, por isso a chave e composta
cand[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]

## ---------------------------------------------------------------- votos por candidatura-turno
if (!is.null(votos)) {
  votos[, `:=`(ano_eleicao = as.integer(ANO_ELEICAO), nr_turno = as.integer(NR_TURNO),
               cd_cargo = as.integer(CD_CARGO))]
  votos[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
  votos[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
  votos <- votos[, .(votos = sum(votos),
                     sit_tot_vot = toupper(names(which.max(table(sit_tot_vot))))),
                 by = .(chave_cand, nr_turno)]
}

## ---------------------------------------------------------------- eleitos
sit_eleito <- c("ELEITO", "ELEITO POR QP", "ELEITO POR MEDIA", "ELEITO POR MÉDIA", "MÉDIA", "MEDIA")
cargos_posicao <- c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L)
cand[, sit_tot := fifelse(grepl("^#NULO", sit_tot), NA_character_, sit_tot)]
cand[, fonte_situacao := fifelse(is.na(sit_tot), NA_character_, "cadastro")]
# situacao de totalizacao dos arquivos de votacao (preenchida na maior parte dos anos antigos)
if (!is.null(votos)) {
  cand <- merge(cand, votos[, .(chave_cand, nr_turno, sit_tot_vot, votos_turno = votos)],
                by = c("chave_cand", "nr_turno"), all.x = TRUE)
  cand[, sit_tot_vot := fifelse(grepl("^#NULO", sit_tot_vot), NA_character_, sit_tot_vot)]
  cand[is.na(sit_tot) & !is.na(sit_tot_vot), `:=`(sit_tot = sit_tot_vot, fonte_situacao = "votacao")]
} else {
  cand[, votos_turno := NA_real_]
}
# por candidatura, ficar com a linha do turno decisivo (max turno presente no cadastro)
setorder(cand, chave_cand, -nr_turno)
cand_final <- cand[, .SD[1], by = chave_cand]
cand_final[, eleito := sit_tot %in% sit_eleito]
# votos e turno decisivo vem da votacao, que traz o 2o turno mesmo quando o cadastro
# so registra o 1o (presidente 2006)
if (!is.null(votos)) {
  vt <- votos[, .(nr_turno_vot = max(nr_turno), votos_dec = votos[which.max(nr_turno)],
                  votos_1 = sum(votos[nr_turno == 1])), by = chave_cand]
  cand_final <- merge(cand_final, vt, by = "chave_cand", all.x = TRUE)
  cand_final[!is.na(nr_turno_vot) & nr_turno_vot > nr_turno, nr_turno := nr_turno_vot]
  cand_final[!is.na(votos_dec), votos_turno := votos_dec]
  cand_final[, votos_t1 := votos_1]
  cand_final[, c("nr_turno_vot", "votos_dec", "votos_1") := NULL]
} else {
  cand_final[, votos_t1 := NA_real_]
}

# majoritarios sem vencedor marcado (ex.: presidente 2006, em que o TSE deixou #NULO
# no turno decisivo): eleito e o mais votado no turno decisivo da unidade, desde que
# o registro da candidatura esteja deferido (registro negado nao gera posse)
majo <- c(1L, 3L, 11L)
# registro valido: nao INAPTO/INDEFERIDO/CASSADO/CANCELADO (rotulos variam por ano:
# APTO/INAPTO ate 2008, DEFERIDO/INDEFERIDO depois, #NE em 2024)
registro_valido <- function(sit_cand, sit_pleito) {
  a <- toupper(fifelse(is.na(sit_cand), "", sit_cand))
  b <- toupper(fifelse(is.na(sit_pleito), "", sit_pleito))
  # 13/09/2026: o cadastro grava "RENÚNCIA" com acento, e a expressao sem acento deixava o registro renunciado passar
  # como valido; na posicao com numero repetido o desempate caia no sequencial mais alto (Arlindo Mello, ES 1998, com
  # registro de renuncia, ficou com a cadeira de Jose Carlos Elias, que tomou posse)
  !grepl("INAPTO|INDEFERID|CASSAD|CANCELAD|REN[UÚ]NCIA|FALECID", a) &
    !grepl("^INDEFERIDO|NEGADO|CASSAD|CANCELAD", b)
}
cand_final[, registro_ok := registro_valido(DS_SITUACAO_CANDIDATURA, DS_SITUACAO_CANDIDATO_PLEITO)]
cand_final[cd_cargo %in% majo, turno_max_ue := max(nr_turno), by = .(ano_eleicao, ue_pos, cd_cargo)]
cand_final[cd_cargo %in% majo, tem_eleito := any(eleito), by = .(ano_eleicao, ue_pos, cd_cargo)]
cand_final[cd_cargo %in% majo & tem_eleito == FALSE & nr_turno == turno_max_ue & !is.na(votos_turno),
           imput := registro_ok & votos_turno == max(votos_turno) & votos_turno > 0,
           by = .(ano_eleicao, ue_pos, cd_cargo)]
cand_final[imput %in% TRUE, `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO POR VOTOS)",
                                 fonte_situacao = "imputacao_votos")]
n_imput_votos <- cand_final[imput %in% TRUE, .N]

# 13/09/2026: eleito diplomado e empossado cuja situacao o TSE regravou depois da cassacao do registro, deixando a vaga
# sem eleito no arquivo de candidaturas (Selma Arruda, senadora por MT em 2018). A correcao vem de
# ref/correcoes_eleitos_fonte_oficial.csv, uma linha por candidatura com a fonte oficial que registra a posse, e a
# forma de saida do mandato (a cassacao) segue da fonte da casa na integracao.
corr_f <- file.path(root, "ref", "correcoes_eleitos_fonte_oficial.csv")
n_corrigidos_fonte_oficial <- 0L
n_rebaixados_fonte_oficial <- 0L
# 21/09/2026: mapa da candidatura rebaixada (abaixo) para a candidatura vencedora que a substitui, usado mais
# adiante para fechar a cadeira quando a candidatura ausente do nucleo da referencia saiu por essa via, e nao
# pelo dedup por (ano, unidade, cargo, numero de urna) que a heranca de id normalmente cobre.
mapa_desloc_fonte_oficial <- data.table(chave_cand_desloc = character(0), chave_cand_novo = character(0))
if (file.exists(corr_f)) {
  corr <- fread(corr_f, colClasses = "character")
  corr[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo))]
  alvo_corr <- cand_final[corr, on = .(ano_eleicao, cd_cargo, SQ_CANDIDATO = sq_candidato), which = TRUE, nomatch = 0L]
  stopifnot(length(alvo_corr) == nrow(corr))
  cand_final[alvo_corr, `:=`(eleito = TRUE, sit_tot = corr$situacao_corrigida, fonte_situacao = "fonte_oficial_casa")]
  n_corrigidos_fonte_oficial <- length(alvo_corr)

  # 21/09/2026: quando a correcao troca quem e o eleito porque o proprio cadastro do TSE foi
  # reescrito depois de uma cassacao (Mao Santa/Hugo Napoleao, PI 1998), a candidatura que o
  # cadastro marcava eleita tem que ser rebaixada, ou as duas sobrevivem ao dedup por numero de
  # candidato (abaixo) e duplicam a cadeira, quebrando a cadeia de sucessao e o vinculo de
  # cadeira unica da unidade no R/42.
  if ("sq_candidato_substituido" %in% names(corr)) {
    desloc <- corr[!is.na(sq_candidato_substituido) & nzchar(sq_candidato_substituido)]
    if (nrow(desloc)) {
      alvo_desl <- cand_final[desloc, on = .(ano_eleicao, cd_cargo, SQ_CANDIDATO = sq_candidato_substituido),
                              which = TRUE, nomatch = 0L]
      stopifnot(length(alvo_desl) == nrow(desloc), all(cand_final$eleito[alvo_desl]))
      cand_final[alvo_desl, `:=`(eleito = FALSE,
                                 sit_tot = "NAO ELEITO (SUBSTITUIDO POR RETOTALIZACAO OFICIAL)",
                                 fonte_situacao = "fonte_oficial_casa")]
      n_rebaixados_fonte_oficial <- length(alvo_desl)
      alvo_novo_desl <- cand_final[desloc, on = .(ano_eleicao, cd_cargo, SQ_CANDIDATO = sq_candidato),
                                   which = TRUE, nomatch = 0L]
      stopifnot(length(alvo_novo_desl) == nrow(desloc))
      mapa_desloc_fonte_oficial <- data.table(chave_cand_desloc = cand_final$chave_cand[alvo_desl],
                                              chave_cand_novo   = cand_final$chave_cand[alvo_novo_desl])
    }
  }
}

# vices nao tem linha de votacao: herdam a situacao do titular da chapa
# (mesmo ano, mesma unidade, mesmo numero de candidato)
# Em 1998 e 2000 o numero do vice e o do titular seguido de "1" (14 -> 141);
# a partir de 2002 vice e titular compartilham o numero.
vice_de <- c(`2` = 1L, `4` = 3L, `12` = 11L)
# situacao bruta do cadastro, antes de qualquer imputacao (o titular imputado por votos tem sit_tot
# reescrito, e a comparacao com o vice precisa do que o TSE gravou)
cand_final[, sit_bruta := toupper(ne(DS_SIT_TOT_TURNO))]
cand_final[grepl("^#NULO", sit_bruta), sit_bruta := NA_character_]
tit <- unique(cand_final[cd_cargo %in% vice_de & eleito == TRUE,
                         .(ano_eleicao, ue_pos, NR_CANDIDATO, cd_tit = cd_cargo, eleito_tit = TRUE,
                           sit_bruta_tit = sit_bruta, fonte_tit = fonte_situacao)])
tit <- unique(rbindlist(list(tit, copy(tit)[, NR_CANDIDATO := paste0(NR_CANDIDATO, "1")])))
cand_final[, cd_tit := vice_de[as.character(cd_cargo)]]
cand_final <- merge(cand_final, tit, by = c("ano_eleicao", "ue_pos", "NR_CANDIDATO", "cd_tit"),
                    all.x = TRUE)
cand_final[!is.na(cd_tit) & !eleito & eleito_tit %in% TRUE & is.na(sit_tot),
           `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO DO TITULAR)",
                fonte_situacao = "imputacao_titular")]
cand_final[fonte_situacao %in% "imputacao_titular", regra_chapa := "numero_do_titular"]

# 12/09/2026: vice do titular imputado por votos. Quando o TSE nao marca o prefeito como eleito e o
# banco o imputa pelo mais votado com registro valido (bloco acima), o cadastro grava no vice da
# mesma chapa exatamente a situacao do titular ("NAO ELEITO" em 2016, 2020 e 2024, "RENUNCIA/
# FALECIMENTO COM SUBSTITUICAO" em 2004), e a heranca pelo numero, que exige situacao vazia, nao o
# alcancava. A medicao da sonda R/sonda_chapa_cascata.R achou 149 vices nessa condicao, todos com o
# numero do titular. A regra so vale quando a situacao do vice repete a do titular imputado, com
# registro valido e sem substituicao, e nao alcanca o vice cuja situacao propria difere da do
# titular que o TSE marcou eleito (70 casos em 2004 e 2008, de renuncia, substituicao e
# indeferimento), que continua fora do banco.
vice_registro_ok <- function(sit_cand, subst) {
  !grepl("INDEFERID|CASSA|RENUN|RENÚN|CANCELAD|FALECID|INAPTO|NEGAD", toupper(fifelse(is.na(sit_cand), "", sit_cand))) &
    !(subst %in% "S")
}
cand_final[!is.na(cd_tit) & !eleito & eleito_tit %in% TRUE & fonte_tit %in% "imputacao_votos" &
             !is.na(sit_tot) & sit_bruta == sit_bruta_tit &
             vice_registro_ok(DS_SITUACAO_CANDIDATURA, ST_SUBSTITUIDO),
           `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO DO TITULAR)",
                fonte_situacao = "imputacao_titular", regra_chapa = "numero_titular_imputado")]
n_chapa_titular_imputado <- cand_final[regra_chapa %in% "numero_titular_imputado", .N]

# 06/09/2026: a herança pelo número não fecha a chapa em 1998 e 2000, quando o vice recebe o
# número do titular seguido de "1" e o casamento falha em metade dos casos. A medição: dos 15.692
# vices de 2000 no cadastro, nenhum tem número igual ao de um prefeito da mesma unidade e só 7.889
# têm o número do prefeito seguido de "1", o que deixava 2.274 vice-prefeitos eleitos contra 5.555
# prefeitos. A chapa se identifica melhor pela coligação, que é a mesma do titular dentro da
# unidade. As rotas entram em cascata, cada uma só sobre o que a anterior não resolveu, e nenhuma
# desfaz o que a herança pelo número já estabeleceu; a coluna regra_chapa declara o caminho.
# Validação em todas as eleições: a cascata reencontra 97,5% dos vice-prefeitos que o banco já
# tinha, 98,8% dos vice-governadores e 6 de 6 vice-presidentes, e diverge do titular já gravado em
# 31 municípios, que ficam com a herança pelo número por ser a regra mais antiga e conferida.
util_col <- function(x) !is.na(x) & !as.character(x) %in% c("", "-1", "-3", "NA", "#NULO#", "#NE")
tit_eleito <- cand_final[cd_cargo %in% vice_de & eleito == TRUE,
                         .(ano_eleicao, ue_pos, cd_tit = cd_cargo, chave_tit = chave_cand,
                           sq_col_tit = SQ_COLIGACAO, comp_tit = DS_COMPOSICAO_COLIGACAO)]
# titular que ainda não tem vice eleito na mesma unidade
ja_tem <- cand_final[!is.na(cd_tit) & eleito == TRUE, .(ano_eleicao, ue_pos, cd_tit, achou = TRUE)]
tit_sem <- merge(tit_eleito, unique(ja_tem), by = c("ano_eleicao", "ue_pos", "cd_tit"), all.x = TRUE)
tit_sem <- tit_sem[is.na(achou)]
n_chapa_coligacao <- 0L; n_chapa_composicao <- 0L
if (nrow(tit_sem)) {
  # 12/09/2026: candidato a vice so entra na cascata com situacao vazia no cadastro (a regra existe
  # para 1998 e 2000, em que o TSE deixou #NULO em todos os vices), registro valido e sem
  # substituicao. Sem essa guarda a cascata imputava 219 vices de 2004 a 2024 por cima de situacao
  # explicita do TSE.
  vic <- cand_final[!is.na(cd_tit) & eleito == FALSE & is.na(sit_tot) &
                      vice_registro_ok(DS_SITUACAO_CANDIDATURA, ST_SUBSTITUIDO),
                    .(ano_eleicao, ue_pos, cd_tit, chave_cand,
                      sq_col = SQ_COLIGACAO, comp = DS_COMPOSICAO_COLIGACAO)]
  fecha <- function(alvo, chave_v, chave_t) {
    if (!nrow(alvo)) return(NULL)
    v <- vic[util_col(get(chave_v))]
    if (!nrow(v)) return(NULL)
    j <- merge(alvo[util_col(get(chave_t)), .(ano_eleicao, ue_pos, cd_tit, chave_tit, k = get(chave_t))],
               v[, .(ano_eleicao, ue_pos, cd_tit, chave_cand, k = get(chave_v))],
               by = c("ano_eleicao", "ue_pos", "cd_tit", "k"), allow.cartesian = TRUE)
    if (!nrow(j)) return(NULL)
    # só fecha a chapa quando a correspondência é de um para um nos dois sentidos
    j[, `:=`(n_v = .N), by = .(ano_eleicao, ue_pos, chave_tit)]
    j[, `:=`(n_t = .N), by = .(ano_eleicao, ue_pos, chave_cand)]
    j[n_v == 1L & n_t == 1L]
  }
  r1 <- fecha(tit_sem, "sq_col", "sq_col_tit")
  if (!is.null(r1) && nrow(r1)) {
    cand_final[chave_cand %in% r1$chave_cand,
               `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO DO TITULAR)",
                    fonte_situacao = "imputacao_titular", regra_chapa = "sq_coligacao")]
    n_chapa_coligacao <- nrow(r1)
    tit_sem <- tit_sem[!chave_tit %in% r1$chave_tit]
    vic <- vic[!chave_cand %in% r1$chave_cand]
  }
  r2 <- fecha(tit_sem, "comp", "comp_tit")
  if (!is.null(r2) && nrow(r2)) {
    cand_final[chave_cand %in% r2$chave_cand,
               `:=`(eleito = TRUE, sit_tot = "ELEITO (IMPUTADO DO TITULAR)",
                    fonte_situacao = "imputacao_titular", regra_chapa = "composicao_coligacao")]
    n_chapa_composicao <- nrow(r2)
  }
}
cat("chapa fechada pelo titular imputado:", n_chapa_titular_imputado,
    "| por coligacao:", n_chapa_coligacao, "| por composicao:", n_chapa_composicao, "\n")
# 12/09/2026: cadeira de titular eleito que termina sem vice eleito, e quantas delas tem no cadastro
# um vice com o numero do titular que ficou de fora pela situacao propria ou pelo registro
te_fim <- cand_final[cd_cargo %in% vice_de & eleito == TRUE,
                     .(ano_eleicao, ue_pos, cd_tit = cd_cargo, NR_CANDIDATO = as.character(NR_CANDIDATO))]
ve_fim <- unique(cand_final[!is.na(cd_tit) & eleito == TRUE, .(ano_eleicao, ue_pos, cd_tit)])
sem_vice <- unique(te_fim[!ve_fim, on = c("ano_eleicao", "ue_pos", "cd_tit")])
vice_fora <- unique(cand_final[!is.na(cd_tit) & eleito == FALSE,
                               .(ano_eleicao, ue_pos, cd_tit, NR_CANDIDATO = as.character(NR_CANDIDATO))])
sem_vice_num <- unique(rbindlist(list(sem_vice, copy(sem_vice)[, NR_CANDIDATO := paste0(NR_CANDIDATO, "1")])))
n_sem_vice <- nrow(sem_vice)
n_sem_vice_2004 <- sem_vice[ano_eleicao >= 2004, .N]
n_sem_vice_com_candidato <- uniqueN(sem_vice_num[vice_fora, on = c("ano_eleicao", "ue_pos", "cd_tit", "NR_CANDIDATO"),
                                                 nomatch = 0L], by = c("ano_eleicao", "ue_pos", "cd_tit"))
eleitos <- cand_final[eleito == TRUE & cd_cargo %in% cargos_posicao]
# registro sem nome, titulo nem CPF nao identifica pessoa (vices de 1998 no cadastro
# do TSE): sai do banco e entra na nota de cobertura
sem_id <- eleitos[is.na(nome) & is.na(titulo) & is.na(cpf)]
n_sem_identidade <- nrow(sem_id)
eleitos <- eleitos[!(is.na(nome) & is.na(titulo) & is.na(cpf))]

# uma pessoa por posicao-numero: candidato substituido (ST_SUBSTITUIDO = S) sai;
# entre os que restam, fica o mais votado, depois o de registro deferido, depois o
# de SQ mais alto (o substituto recebe sequencial posterior)
eleitos[, def := registro_ok]
eleitos[, subst := ST_SUBSTITUIDO %in% "S"]
setorder(eleitos, ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, subst, -votos_turno, -def, -SQ_CANDIDATO,
         na.last = TRUE)
n_antes <- nrow(eleitos)
eleitos <- eleitos[!duplicated(eleitos[, .(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO)])]
n_dup_posicao <- n_antes - nrow(eleitos)
# posicao que ficou com registro invalido havendo outro candidato da mesma posicao com registro valido: deve ser zero
pos_def <- cand_final[eleito == TRUE & cd_cargo %in% cargos_posicao & registro_ok == TRUE, .(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO)]
n_posicao_invalida_com_valida <- eleitos[def == FALSE][unique(pos_def), on = c("ano_eleicao", "ue_pos", "cd_cargo", "NR_CANDIDATO"), nomatch = 0L, .N]

## ---------------------------------------------------------------- dedup pessoa (union-find)
# Regra (documentada no livro de codigos):
#  1) mesma pessoa se mesmo NR_TITULO_ELEITORAL (12 dig, valido)
#  2) ou mesmo CPF (11 dig, valido)
#  3) ou mesmo (nome_norm, dt_nasc_iso) quando ambos presentes
uf_parent <- new.env(hash = TRUE)
uf_find <- function(k) {
  p <- uf_parent[[k]]
  if (is.null(p) || p == k) return(k)
  r <- uf_find(p)
  uf_parent[[k]] <- r
  r
}
uf_union <- function(a, b) {
  ra <- uf_find(a); rb <- uf_find(b)
  if (ra != rb) uf_parent[[rb]] <- ra
  invisible(NULL)
}
eleitos[, rid := paste0("r", .I)]
link_by <- function(key_vec) {
  ok <- !is.na(key_vec)
  sp <- split(eleitos$rid[ok], key_vec[ok])
  for (g in sp) if (length(g) > 1) for (i in 2:length(g)) uf_union(g[1], g[i])
}
link_by(eleitos$titulo)
link_by(eleitos$cpf)
link_by(fifelse(!is.na(eleitos$nome_norm) & !is.na(eleitos$dt_nasc_iso),
                paste(eleitos$nome_norm, eleitos$dt_nasc_iso), NA_character_))
eleitos[, comp := vapply(rid, uf_find, character(1))]
eleitos[, comp_raiz := comp]

## ---------------------------------------------------------------- separar pessoas fundidas por documento (21/09/2026)
# O fecho acima une por titulo, por CPF e por nome+nascimento sem olhar para dentro do grupo formado.
# Quando o titulo de uma candidatura casa com o de outra mas os CPFs das duas sao validos (digito
# verificador correto), distintos, e o nome e o nascimento tambem diferem entre as duas, as duas
# candidaturas sao de pessoas diferentes e o titulo, erro de digitacao cruzado no cadastro do TSE, nao
# pode ser a unica ponte entre elas; o mesmo vale na direcao inversa, quando o CPF une dois titulos
# validos e distintos. Quando nome ou nascimento coincidem entre os dois lados (mesmo nome com
# nascimento diferente, nome diferente com mesmo nascimento, ou os dois iguais com documento
# reemitido) a leitura fica ambigua e a fusao permanece, com a marca em dedup_suspeita_fusao de
# pessoas.csv para quem quiser filtrar. O metodo (bloco sem titulo por CPF valido e nome+nascimento,
# bloco sem CPF por titulo valido e nome+nascimento, leitura pela sobreposicao de nome e nascimento
# entre blocos) repete o de R/auditoria_fusao_documentos.R, que audita o banco inteiro depois de
# pronto e tem de encontrar zero pessoas na leitura duas_pessoas_nome_e_nascimento_diferentes aqui.
source(file.path(root, "lib", "proveniencia.R"))
reg03 <- function(k, v) registrar_numero(k, v, script = file.path(root, "R", "03_build_banco.R"))
cpf_valido_pes <- function(d) {
  ok <- !is.na(d) & nchar(d) == 11L & !grepl("^(\\d)\\1{10}$", d)
  if (!any(ok)) return(ok)
  x <- matrix(as.integer(unlist(strsplit(d[ok], ""))), ncol = 11L, byrow = TRUE)
  r1 <- (x[, 1:9] %*% (10:2)) %% 11; v1 <- ifelse(r1 < 2, 0, 11 - r1)
  r2 <- (x[, 1:10] %*% (11:2)) %% 11; v2 <- ifelse(r2 < 2, 0, 11 - r2)
  ok[ok] <- as.vector(v1 == x[, 10] & v2 == x[, 11])
  ok
}
# titulo de 12 digitos: 8 de sequencia, 2 da UF de emissao, 2 verificadores; SP (01) e MG (02) trocam resto 0 por 1
titulo_valido_pes <- function(d) {
  ok <- !is.na(d) & nchar(d) == 12L & !grepl("^0+$", d)
  if (!any(ok)) return(ok)
  x <- matrix(as.integer(unlist(strsplit(d[ok], ""))), ncol = 12L, byrow = TRUE)
  uf <- x[, 9] * 10 + x[, 10]
  r1 <- as.vector((x[, 1:8] %*% (2:9)) %% 11)
  v1 <- ifelse(r1 == 10, 0, ifelse(r1 == 0 & uf %in% 1:2, 1, r1))
  r2 <- (x[, 9] * 7 + x[, 10] * 8 + v1 * 9) %% 11
  v2 <- ifelse(r2 == 10, 0, ifelse(r2 == 0 & uf %in% 1:2, 1, r2))
  ok[ok] <- v1 == x[, 11] & v2 == x[, 12] & uf >= 1 & uf <= 28
  ok
}
# 21/09/2026: documento por candidatura para a deteccao de fusao, com a mesma consulta ao
# parquet bruto de R/auditoria_fusao_documentos.R (open_dataset + filter + collect na ordem
# de chegada do arquivo), e nao a linha que cand_final escolhe pelo turno decisivo
# (setorder por -nr_turno, acima). Quando uma chapa e substituida depois do registro a
# mesma chave de candidatura tem documento de duas pessoas e nenhuma das duas linhas tem
# situacao eleita marcada (Rio Negrinho, Sao Joao d'Alianca e Itapeva em 2004, descritos no
# cabecalho da auditoria); o desempate cai na ordem de chegada do parquet, que so repete de
# forma confiavel vindo da mesma consulta Arrow da auditoria, nao da tabela cand ja lida por
# read_parquet() e reordenada em memoria. A linha que o desempate por turno decisivo
# descarta pode ser exatamente a que revelaria a fusao la na frente, porque a candidatura
# vizinha da mesma pessoa ja chega identica a ela por titulo, CPF e nome+nascimento.
norm_nome_fusao <- function(x) stri_trim_both(gsub(" +", " ", gsub("[^A-Z ]", " ", toupper(stri_trans_general(x, "Latin-ASCII")))))
docf <- rbindlist(lapply(sort(unique(eleitos$ano_eleicao)), function(a) {
  sq <- unique(eleitos[ano_eleicao == a, SQ_CANDIDATO])
  x <- as.data.table(open_dataset(file.path(pq, sprintf("cand_%s.parquet", a))) |>
                       dplyr::filter(SQ_CANDIDATO %in% sq) |>
                       dplyr::select(SG_UF, SG_UE, CD_CARGO, NR_CANDIDATO, SQ_CANDIDATO, NM_CANDIDATO,
                                     NR_CPF_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO, DT_NASCIMENTO,
                                     DS_SIT_TOT_TURNO) |> dplyr::collect())
  x <- unique(x[, lapply(.SD, as.character)])
  x[, ano_eleicao := a]
}))
docf[, `:=`(cd_cargo = as.integer(CD_CARGO),
            nome = norm_nome_fusao(NM_CANDIDATO), cpf = gsub("[^0-9]", "", NR_CPF_CANDIDATO),
            titulo = gsub("[^0-9]", "", NR_TITULO_ELEITORAL_CANDIDATO),
            nasc = fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", trimws(DT_NASCIMENTO)), trimws(DT_NASCIMENTO), NA_character_),
            elt_doc = toupper(trimws(DS_SIT_TOT_TURNO)) %chin% sit_eleito, ord_doc = .I)]
docf[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
docf[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
docf[nchar(titulo) %in% 9:11, titulo := formatC(as.numeric(titulo), width = 12, flag = "0", format = "f", digits = 0)]
docf[, `:=`(nome = fifelse(nzchar(nome), nome, NA_character_), cpf = fifelse(nzchar(cpf), cpf, NA_character_),
            titulo = fifelse(nzchar(titulo), titulo, NA_character_))]
docf <- unique(docf[, .(chave_cand, nome, cpf, titulo, nasc, elt_doc, ord_doc)])
setorder(docf, chave_cand, -elt_doc, ord_doc)
docf <- docf[, .SD[1], by = chave_cand, .SDcols = c("nome", "cpf", "titulo", "nasc")]
setnames(docf, c("nome", "cpf", "titulo", "nasc"), c("nome_fusao", "cpf_fusao", "titulo_fusao", "nasc_fusao"))
eleitos[docf, `:=`(nome_fusao = i.nome_fusao, nasc_fusao = i.nasc_fusao,
                    titulo_fusao = i.titulo_fusao, cpf_fusao = i.cpf_fusao), on = "chave_cand"]
# candidatura sem documento nenhum na consulta bruta (nao deveria existir, ja que toda
# chave de eleitos vem do proprio cand_final que leu o mesmo parquet): usa o documento do
# turno decisivo como reserva, para nao perder a candidatura do fecho de fusao
eleitos[is.na(nome_fusao) & is.na(titulo_fusao) & is.na(cpf_fusao),
        `:=`(nome_fusao = nome_norm, nasc_fusao = dt_nasc_iso, titulo_fusao = titulo, cpf_fusao = cpf)]
reg03("fusao_candidaturas_sem_documento_bruto_na_consulta_arrow",
      eleitos[is.na(nome_fusao) & is.na(nome_norm), .N])
reg03("fusao_candidaturas_com_documento_diferente_do_turno_decisivo",
      eleitos[!is.na(nome_fusao) & (nome_fusao != nome_norm |
                                     (!is.na(nasc_fusao) & !is.na(dt_nasc_iso) & nasc_fusao != dt_nasc_iso) |
                                     (!is.na(titulo_fusao) & !is.na(titulo) & titulo_fusao != titulo) |
                                     (!is.na(cpf_fusao) & !is.na(cpf) & cpf_fusao != cpf)), .N])
blocos_fusao <- function(d, chaves) {
  d <- copy(d)[, lab := .I]
  repeat {
    antes <- d$lab
    for (k in chaves) d[!is.na(get(k)), lab := min(lab), by = c("comp", k)]
    if (identical(antes, d$lab)) break
  }
  d$lab
}
resumo_ponte_fusao <- function(d, bloco, doc_col, rotulo) {
  b <- d[, .(docs = list(unique(na.omit(get(doc_col))))), by = c("comp", bloco)]
  b[, tem := lengths(docs) > 0L]
  p <- b[tem == TRUE, {
    todos <- unlist(docs)
    .(n_blocos = .N, sem_sobreposicao = !anyDuplicated(todos))
  }, by = comp][n_blocos > 1L & sem_sobreposicao == TRUE]
  if (nrow(p)) p[, ponte := rotulo]
  p
}
passo_fusao <- function() {
  cd <- eleitos[, .(rid, comp, nome = nome_fusao, cpf = cpf_fusao, titulo = titulo_fusao, nasc = nasc_fusao)]
  cd[, `:=`(cpf_v = fifelse(cpf_valido_pes(cpf), cpf, NA_character_),
            tit_v = fifelse(titulo_valido_pes(titulo), titulo, NA_character_),
            knom = fifelse(!is.na(nome) & nzchar(nome) & !is.na(nasc), paste(nome, nasc), NA_character_))]
  cd[, bloco_sem_titulo := blocos_fusao(cd, c("cpf_v", "knom"))]
  cd[, bloco_sem_cpf := blocos_fusao(cd, c("tit_v", "knom"))]
  pt <- resumo_ponte_fusao(cd, "bloco_sem_titulo", "cpf_v", "titulo_une_cpfs_distintos")
  pc <- resumo_ponte_fusao(cd, "bloco_sem_cpf", "tit_v", "cpf_une_titulos_distintos")
  sus <- rbindlist(list(pt, pc), fill = TRUE)
  if (!nrow(sus)) return(list(cd = cd, sus = sus))
  bl <- rbindlist(list(
    cd[comp %in% pt$comp, .(comp, ponte = "titulo_une_cpfs_distintos", bloco = bloco_sem_titulo, nome, nasc)],
    cd[comp %in% pc$comp, .(comp, ponte = "cpf_une_titulos_distintos", bloco = bloco_sem_cpf, nome, nasc)]))
  cmp <- bl[, {
    nb <- split(nome, bloco); db <- split(nasc, bloco)
    pares <- combn(length(nb), 2L, simplify = FALSE)
    nome_comum <- vapply(pares, function(p) length(intersect(na.omit(nb[[p[1]]]), na.omit(nb[[p[2]]]))) > 0L, TRUE)
    nasc_comum <- vapply(pares, function(p) length(intersect(na.omit(db[[p[1]]]), na.omit(db[[p[2]]]))) > 0L, TRUE)
    .(algum_par_sem_nome_comum = any(!nome_comum), algum_par_sem_nasc_comum = any(!nasc_comum))
  }, by = .(comp, ponte)]
  sus <- merge(sus, cmp, by = c("comp", "ponte"))
  sus[, leitura := fcase(algum_par_sem_nome_comum & algum_par_sem_nasc_comum, "duas_pessoas_nome_e_nascimento_diferentes",
                         algum_par_sem_nome_comum, "nome_diferente_mesmo_nascimento",
                         algum_par_sem_nasc_comum, "mesmo_nome_nascimento_diferente",
                         default = "mesmo_nome_e_nascimento_documento_reemitido")]
  list(cd = cd, sus = sus)
}
for (iter_fusao in 1:5) {
  passo <- passo_fusao()
  alvo <- passo$sus[leitura == "duas_pessoas_nome_e_nascimento_diferentes"]
  if (!nrow(alvo)) break
  cd <- passo$cd
  at <- alvo[ponte == "titulo_une_cpfs_distintos"]
  if (nrow(at)) {
    aux <- cd[comp %in% at$comp, .(rid, novo = paste0(comp, "_T", bloco_sem_titulo))]
    eleitos[aux, comp := i.novo, on = "rid"]
  }
  ac <- alvo[ponte == "cpf_une_titulos_distintos" & !comp %in% at$comp]
  if (nrow(ac)) {
    aux <- cd[comp %in% ac$comp, .(rid, novo = paste0(comp, "_C", bloco_sem_cpf))]
    eleitos[aux, comp := i.novo, on = "rid"]
  }
}
passo_final <- passo_fusao()
stopifnot(nrow(passo_final$sus[leitura == "duas_pessoas_nome_e_nascimento_diferentes"]) == 0L)
amb_fusao <- passo_final$sus[leitura != "duas_pessoas_nome_e_nascimento_diferentes", .(leitura = leitura[1]), by = comp]
eleitos[, leitura_fusao := NA_character_]
if (nrow(amb_fusao)) eleitos[amb_fusao, leitura_fusao := i.leitura, on = "comp"]
n_fusao_componentes_separadas <- eleitos[, .(n_final = uniqueN(comp)), by = comp_raiz][n_final > 1L, .N]
reg03("fusao_componentes_separadas", n_fusao_componentes_separadas)
reg03("fusao_pessoas_marcadas_suspeita_ambigua", uniqueN(eleitos[!is.na(leitura_fusao), comp]))
cat("separacao de pessoas fundidas por documento:", n_fusao_componentes_separadas,
    "componentes originais separadas |", uniqueN(eleitos[!is.na(leitura_fusao), comp]), "componentes ambiguas marcadas\n")

# 12/09/2026: ponte nova entre pessoas da referencia congelada. Candidatura que entra no banco pode
# trazer o titulo de uma pessoa ao lado do CPF e do nascimento de outra (erro cruzado do cadastro do
# TSE), e a uniao transitiva fundiria duas pessoas ja verificadas. A regra segue a do R/40 para os
# suplentes, que nao funde ids por ponte de cadastro: cada candidatura da referencia fica com o seu
# id, e a candidatura nova vai para a pessoa com quem compartilha mais chaves (titulo, CPF, nome
# com nascimento). Empate, ou nenhuma chave, deixa a candidatura como pessoa propria. O caso
# medido e o vice-prefeito de 2000 em 44130, com o titulo de Jose Milton Nunes e o CPF, o nome e o
# nascimento de Jose Ricardo de Melo. Auditoria em output/verificacao/pontes_nucleo_referencia.csv.
source(file.path(root, "lib", "proveniencia.R"))
reg03 <- function(k, v) registrar_numero(k, v, script = file.path(root, "R", "03_build_banco.R"))
f_ref <- file.path(root, "ref", "ids_pessoa_referencia.parquet")
ref <- if (file.exists(f_ref)) setDT(read_parquet(f_ref)) else NULL
n_pontes_ref <- 0L
if (!is.null(ref)) {
  eleitos[ref[origem == "nucleo"], id_ref := i.id_num, on = "chave_cand"]
  eleitos[, k_nom := fifelse(!is.na(nome_norm) & !is.na(dt_nasc_iso), paste(nome_norm, dt_nasc_iso), NA_character_)]
  fus <- eleitos[!is.na(id_ref), .(n = uniqueN(id_ref)), by = comp][n > 1L]
  n_pontes_ref <- nrow(fus)
  if (n_pontes_ref) {
    aud <- list()
    for (cc in fus$comp) {
      s <- eleitos[comp == cc]
      refs <- s[!is.na(id_ref)]
      for (r in s[is.na(id_ref), rid]) {
        x <- s[rid == r]
        sc <- refs[, .(pontos = sum(any(!is.na(x$titulo) & titulo %in% x$titulo),
                                    any(!is.na(x$cpf) & cpf %in% x$cpf),
                                    any(!is.na(x$k_nom) & k_nom %in% x$k_nom))), by = id_ref]
        melhor <- sc[pontos == max(pontos) & pontos > 0L]
        destino <- if (nrow(melhor) == 1L) melhor$id_ref else NA_integer_
        aud[[length(aud) + 1L]] <- data.table(componente = cc, chave_cand = x$chave_cand, nome = x$nome,
                                              titulo = x$titulo, cpf = x$cpf, dt_nasc_iso = x$dt_nasc_iso,
                                              pontos_por_id = paste(sprintf("%d:%d", sc$id_ref, sc$pontos), collapse = ";"),
                                              id_destino = destino)
        eleitos[rid == r, comp := if (is.na(destino)) paste0(cc, "_", r) else paste0(cc, "_", destino)]
      }
      eleitos[comp == cc & !is.na(id_ref), comp := paste0(cc, "_", id_ref)]
    }
    aud <- rbindlist(aud)
    fwrite(aud, file.path(root, "output", "verificacao", "pontes_nucleo_referencia.csv"))
    reg03("ids_pontes_nucleo_referencia_candidaturas", nrow(aud))
  }
  reg03("ids_pontes_nucleo_referencia_componentes", n_pontes_ref)
  eleitos[, c("id_ref", "k_nom") := NULL]
}

# id estavel: chave canonica da componente (menor titulo; senao menor cpf; senao nome+nasc)
canon <- eleitos[, {
  t <- sort(titulo[!is.na(titulo)])
  c <- sort(cpf[!is.na(cpf)])
  n <- sort(paste(nome_norm, dt_nasc_iso)[!is.na(nome_norm) & !is.na(dt_nasc_iso)])
  key <- if (length(t)) paste0("TIT:", t[1]) else
         if (length(c)) paste0("CPF:", c[1]) else
         if (length(n)) paste0("NOM:", n[1]) else paste0("RID:", rid[1])
  .(chave_canonica = key)
}, by = comp]
# 12/09/2026: o numero do id vem da referencia congelada do ultimo estado verificado
# (ref/ids_pessoa_referencia.parquet, gerada por R/congela_referencia_ids.R). Antes o numero era a
# posicao da chave canonica na ordem de todas as chaves, e qualquer pessoa nova deslocava as
# seguintes (a rodada de 07/09 com os vices renumerou 482.264 mandatos). A componente herda o id
# da referencia quando suas candidaturas levam a um unico id; so a componente sem candidatura na
# referencia recebe numero novo, a partir do maior id ja usado (nucleo e suplentes), na ordem da
# chave canonica. Sem a referencia (primeira construcao), vale a numeracao pela ordem das chaves.
PREFIXO_ID <- "BOCEL"
if (!is.null(ref)) {
  hit <- merge(eleitos[, .(comp, chave_cand)], ref[, .(chave_cand, id_num, origem)], by = "chave_cand")
  por_comp <- hit[, .(n_ids = uniqueN(id_num), id_num = min(id_num)), by = comp]
  # componente que junta duas pessoas da referencia: fusao nova, que nao se decide aqui
  fusoes <- por_comp[n_ids > 1L]
  if (nrow(fusoes)) {
    fwrite(merge(hit[comp %in% fusoes$comp], eleitos[, .(chave_cand, nome, titulo, cpf, dt_nasc_iso)], by = "chave_cand"),
           file.path(root, "output", "verificacao", "ids_fusao_contra_referencia.csv"))
  }
  stopifnot(nrow(fusoes) == 0L)
  # pessoa da referencia partida em duas componentes: a separacao de pessoas fundidas por documento,
  # acima, faz isso de proposito quando o titulo ou o CPF que ligava duas candidaturas da referencia
  # era ponte indevida entre gente diferente. O id antigo fica com a componente de mais candidaturas
  # (empate: ano_eleicao mais antigo, depois menor sq_candidato) e a outra recebe id novo junto com as
  # demais componentes sem id na referencia, mais abaixo. Fora dessa separacao deliberada nenhuma
  # pessoa da referencia deveria se partir, e o id_num que sobra sem par continua proibido de duplicar.
  chaves_fusao_separadas <- character(0)
  dup_id <- por_comp[, .N, by = id_num][N > 1L, id_num]
  if (length(dup_id)) {
    partida <- por_comp[id_num %in% dup_id]
    crit <- merge(eleitos[comp %in% partida$comp, .(comp, chave_cand, ano_eleicao)], partida[, .(comp, id_num)], by = "comp")
    crit <- merge(crit, cand_final[, .(chave_cand, sq_min = as.numeric(SQ_CANDIDATO))], by = "chave_cand")
    crit <- crit[, .(n_cand = .N, ano_min = min(ano_eleicao), sq_min = min(sq_min)), by = .(comp, id_num)]
    setorder(crit, id_num, -n_cand, ano_min, sq_min)
    fica_com_ref <- crit[, .SD[1L], by = id_num]$comp
    perde_ref <- setdiff(partida$comp, fica_com_ref)
    por_comp[comp %in% perde_ref, id_num := NA_integer_]
    chaves_fusao_separadas <- eleitos[comp %in% perde_ref, chave_cand]
  }
  reg03("fusao_candidaturas_com_id_de_referencia_trocado", length(chaves_fusao_separadas))
  stopifnot(!anyDuplicated(por_comp[!is.na(id_num), id_num]))
  canon <- merge(canon, por_comp[, .(comp, id_num)], by = "comp", all.x = TRUE)

  # Candidatura da referencia deslocada na deduplicacao da posicao (12/09/2026). A heranca do vice
  # do titular imputado trouxe, em 8 cadeiras de vice-prefeito de 2016 a 2024, o vice substituto
  # com registro apto, que na deduplicacao por (eleicao, unidade, cargo, numero) passa na frente do
  # vice que o banco trazia, com registro inapto e substituido em 7 delas. A cadeira continua uma
  # so e ocupada, e a guarda abaixo reprova se alguma candidatura da referencia sumir sem outra na
  # mesma posicao. Quando a candidatura que fica e da mesma pessoa (mesmo titulo, CPF ou nome com
  # nascimento), ela herda o id da que saiu. Auditoria em output/verificacao/ids_candidatura_deslocada.csv.
  aus <- ref[origem == "nucleo" & !chave_cand %in% eleitos$chave_cand, .(chave_cand, id_ant = id_num)]
  n_herda_deslocada <- 0L
  if (nrow(aus)) {
    kn <- function(n, d) fifelse(!is.na(n) & !is.na(d), paste(n, d), NA_character_)
    da <- cand_final[chave_cand %in% aus$chave_cand,
                     .(chave_cand, ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, titulo, cpf,
                       k_nom = kn(nome_norm, dt_nasc_iso), nome_saiu = nome,
                       situacao_candidatura_saiu = DS_SITUACAO_CANDIDATURA, substituido_saiu = ST_SUBSTITUIDO)]
    da <- merge(da, aus, by = "chave_cand")
    stopifnot(nrow(da) == nrow(aus))
    w <- eleitos[, .(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, chave_cand_ficou = chave_cand, comp,
                     titulo_n = titulo, cpf_n = cpf, k_nom_n = kn(nome_norm, dt_nasc_iso),
                     nome_ficou = nome, regra_chapa_ficou = regra_chapa)]
    da <- merge(da, w, by = c("ano_eleicao", "ue_pos", "cd_cargo", "NR_CANDIDATO"), all.x = TRUE)
    # 21/09/2026: quando a candidatura ausente do nucleo saiu porque a correcao de fonte oficial a rebaixou (ex.:
    # Hugo Napoleao, PI 1998), quem ocupa a cadeira agora e a candidatura vencedora da correcao, de numero de urna
    # diferente do rebaixado, e a heranca acima (por ano, unidade, cargo e numero) nao a encontra. Fecha a cadeira
    # com o mapa construido junto com o rebaixamento, sem herdar id (mesma_pessoa fica FALSE de proposito: sao
    # pessoas diferentes, e a candidatura vencedora recebe id proprio pelo caminho normal, abaixo).
    if (nrow(mapa_desloc_fonte_oficial)) {
      da[mapa_desloc_fonte_oficial, on = .(chave_cand = chave_cand_desloc),
         chave_cand_ficou := fifelse(is.na(chave_cand_ficou), i.chave_cand_novo, chave_cand_ficou)]
    }
    # a posicao continua ocupada por uma unica candidatura
    stopifnot(!anyNA(da$chave_cand_ficou), !anyDuplicated(da$chave_cand))
    da[, mesma_pessoa := (!is.na(titulo) & titulo %in% titulo_n) | (!is.na(cpf) & cpf %in% cpf_n) |
                         (!is.na(k_nom) & k_nom %in% k_nom_n)]
    da <- merge(da, canon[, .(comp, id_comp = id_num)], by = "comp")
    herda <- da[mesma_pessoa == TRUE & is.na(id_comp)]
    stopifnot(!anyDuplicated(herda$comp), !any(herda$id_ant %in% canon$id_num))
    if (nrow(herda)) canon[herda, on = "comp", id_num := i.id_ant]
    n_herda_deslocada <- nrow(herda)
  }
  reg03("ids_nucleo_referencia_deslocadas_na_posicao", nrow(aus))
  reg03("ids_nucleo_referencia_deslocadas_mesma_pessoa", n_herda_deslocada)

  novos <- canon[is.na(id_num)]
  setorder(novos, chave_canonica)
  novos[, id_num := max(ref$id_num) + seq_len(.N)]
  canon <- rbindlist(list(canon[!is.na(id_num)], novos), use.names = TRUE)
  stopifnot(!anyDuplicated(canon$id_num))
  if (nrow(aus)) {
    da <- merge(da, canon[, .(comp, id_final = id_num)], by = "comp")
    fwrite(da[, .(chave_cand_saiu = chave_cand, nome_saiu, situacao_candidatura_saiu, substituido_saiu, id_ant,
                  chave_cand_ficou, nome_ficou, regra_chapa_ficou, mesma_pessoa, id_final)][order(chave_cand_saiu)],
           file.path(root, "output", "verificacao", "ids_candidatura_deslocada.csv"))
  }
  # toda candidatura do nucleo da referencia continua no banco com o mesmo id, salvo as deslocadas
  conf <- merge(ref[origem == "nucleo", .(chave_cand, id_ref = id_num)],
                merge(eleitos[, .(chave_cand, comp)], canon[, .(comp, id_num)], by = "comp"),
                by = "chave_cand", all.x = TRUE)
  # divergencia de id contra a referencia so e admitida na candidatura que a separacao de pessoas
  # fundidas por documento (acima) deliberadamente levou para outro id; qualquer outra e reprovada
  n_diverge_fusao <- conf[!is.na(id_num) & id_num != id_ref & chave_cand %in% chaves_fusao_separadas, .N]
  n_diverge_inesperado <- conf[!is.na(id_num) & id_num != id_ref & !chave_cand %in% chaves_fusao_separadas, .N]
  reg03("ids_nucleo_referencia_ausentes_no_banco", conf[is.na(id_num), .N])
  reg03("ids_nucleo_referencia_com_id_diferente", conf[!is.na(id_num) & id_num != id_ref, .N])
  reg03("ids_nucleo_referencia_com_id_diferente_por_separacao_de_fusao", n_diverge_fusao)
  stopifnot(conf[is.na(id_num), .N] == nrow(aus), n_diverge_inesperado == 0L)
  # a componente do nucleo so tem candidaturas eleitas, que na referencia sao todas de origem
  # nucleo; a pessoa nova que ja fora suplente recebe id novo aqui, e o R/40 leva as candidaturas
  # de suplente dela para esse id (o id antigo, so de suplente, deixa de ser usado)
  reg03("ids_pessoas_herdadas_do_nucleo", uniqueN(hit$id_num))
  reg03("ids_pessoas_novas", nrow(novos))
  reg03("eleitos_corrigidos_por_fonte_oficial", n_corrigidos_fonte_oficial)
  reg03("eleitos_rebaixados_por_fonte_oficial", n_rebaixados_fonte_oficial)
  reg03("posicao_com_registro_invalido_havendo_valido", n_posicao_invalida_com_valida)
  reg03("chapa_vices_titular_imputado", n_chapa_titular_imputado)
  reg03("chapa_vices_sq_coligacao", n_chapa_coligacao)
  reg03("chapa_vices_composicao_coligacao", n_chapa_composicao)
  reg03("chapa_titulares_eleitos_sem_vice", n_sem_vice)
  reg03("chapa_titulares_eleitos_sem_vice_2004_2024", n_sem_vice_2004)
  reg03("chapa_titulares_sem_vice_com_vice_de_mesmo_numero_fora", n_sem_vice_com_candidato)
} else {
  canon[, id_num := as.integer(factor(chave_canonica, levels = sort(unique(chave_canonica))))]
}
canon[, id_pessoa := paste0(PREFIXO_ID, formatC(id_num, width = 7, flag = "0"))]
canon[, id_num := NULL]
eleitos <- merge(eleitos, canon, by = "comp")

# 21/09/2026: registro da separacao de pessoas fundidas por documento, para a auditoria (R/09) aceitar
# como excecao conhecida a componente original que a separacao partiu em mais de um id_pessoa, e para
# conferir, candidatura por candidatura, que cada lado ficou exatamente com o id esperado.
grupos_fusao_afetados <- eleitos[, .(n_final = uniqueN(comp)), by = comp_raiz][n_final > 1L, comp_raiz]
fusao_separacao <- eleitos[comp_raiz %in% grupos_fusao_afetados,
                           .(chave_cand, comp_raiz, id_pessoa_esperado = id_pessoa, leitura_fusao)]
fwrite(fusao_separacao[order(comp_raiz, chave_cand)],
       file.path(root, "output", "verificacao", "fusao_documentos_separacao.csv"))
reg03("fusao_documentos_separacao_candidaturas", nrow(fusao_separacao))
reg03("fusao_documentos_separacao_componentes_originais", length(grupos_fusao_afetados))
eleitos[, comp_raiz := NULL]

## ---------------------------------------------------------------- mandatos
esfera_map <- c(`1` = "federal", `2` = "federal", `3` = "estadual", `4` = "estadual",
                `5` = "federal", `6` = "federal", `7` = "estadual", `8` = "estadual",
                `11` = "municipal", `12` = "municipal", `13` = "municipal")
dur <- function(cd) fifelse(cd == 5L, 8L, 4L)
mand <- eleitos[, .(
  id_mandato = paste0("M", chave_cand),
  id_pessoa, sq_candidato = SQ_CANDIDATO, nr_candidato = NR_CANDIDATO, ano_eleicao,
  nr_turno_decisivo = nr_turno, dt_eleicao = DT_ELEICAO,
  cd_cargo, cargo = ds_cargo,
  esfera = esfera_map[as.character(cd_cargo)],
  sg_uf = SG_UF, sg_ue = SG_UE, nm_ue = NM_UE, unidade_posicao = ue_pos,
  nr_partido = NR_PARTIDO, sg_partido = SG_PARTIDO,
  tp_agremiacao = TP_AGREMIACAO, nm_coligacao = ne(NM_COLIGACAO),
  composicao_coligacao = ne(DS_COMPOSICAO_COLIGACAO),
  regra_chapa = regra_chapa,   # como a chapa do vice foi fechada (06/09/2026); NA no titular
  reeleicao_declarada = ne(ST_REELEICAO),
  situacao_totalizacao = sit_tot, fonte_situacao,
  votos_t1, votos_turno_decisivo = votos_turno,
  # convencao: legislativo federal e estadual (senador, deputados) toma posse em 1o de
  # fevereiro e a legislatura termina em 31 de janeiro; executivos e cargos municipais,
  # 1o de janeiro a 31 de dezembro
  mandato_inicio = fifelse(cd_cargo %in% c(5L, 6L, 7L, 8L),
                           sprintf("%d-02-01", ano_eleicao + 1L), sprintf("%d-01-01", ano_eleicao + 1L)),
  mandato_fim    = fifelse(cd_cargo %in% c(5L, 6L, 7L, 8L),
                           sprintf("%d-01-31", ano_eleicao + dur(cd_cargo) + 1L),
                           sprintf("%d-12-31", ano_eleicao + dur(cd_cargo))),
  forma_saida = "nao_observado"
)]

## ---------------------------------------------------------------- sucessao (executivos)
exec <- c(1L, 3L, 11L)  # presidente, governador, prefeito
setorder(mand, cd_cargo, unidade_posicao, ano_eleicao)
mand[cd_cargo %in% exec,
     `:=`(antecessor_id = shift(id_pessoa, 1L),
          sucessor_id   = shift(id_pessoa, -1L),
          antecessor_ano = shift(ano_eleicao, 1L),
          sucessor_ano   = shift(ano_eleicao, -1L)),
     by = .(cd_cargo, unidade_posicao)]
# antecessor/sucessor validos apenas se eleicao imediatamente adjacente (4 anos)
mand[cd_cargo %in% exec & !is.na(antecessor_ano) & ano_eleicao - antecessor_ano != 4L,
     antecessor_id := NA_character_]
mand[cd_cargo %in% exec & !is.na(sucessor_ano) & sucessor_ano - ano_eleicao != 4L,
     sucessor_id := NA_character_]
mand[, c("antecessor_ano", "sucessor_ano") := NULL]
mand[cd_cargo %in% exec, via_sucessao := "nova_eleicao"]
mand[!cd_cargo %in% exec, `:=`(antecessor_id = NA_character_,
                               sucessor_id = NA_character_,
                               via_sucessao = NA_character_)]
mand[cd_cargo %in% exec, reeleito_mesma_pessoa :=
       fifelse(is.na(antecessor_id), NA, antecessor_id == id_pessoa)]

## ---------------------------------------------------------------- pessoas
setorder(eleitos, id_pessoa, -ano_eleicao)
pess <- eleitos[, .(
  nome = nome[1], nome_urna_recente = nome_urna[1],
  dt_nascimento = na.omit(dt_nasc_iso)[1] %||% NA_character_,
  genero = na.omit(genero)[1] %||% NA_character_,
  nr_titulo_eleitoral = na.omit(titulo)[1] %||% NA_character_,
  nr_cpf = na.omit(cpf)[1] %||% NA_character_,
  n_mandatos = .N,
  primeiro_ano_eleito = min(ano_eleicao),
  ultimo_ano_eleito = max(ano_eleicao),
  chave_dedup = fcase(any(!is.na(titulo)), "titulo",
                      any(!is.na(cpf)), "cpf",
                      default = "nome_nascimento"),
  # 21/09/2026: leitura da fusao de documentos (R/auditoria_fusao_documentos.R) quando a candidatura
  # ficou numa componente ambigua (mesmo nome ou mesmo nascimento entre os dois lados da ponte, ou os
  # dois iguais com documento reemitido) e a fusao continuou de proposito; NA para quem nao entrou nessa leitura
  dedup_suspeita_fusao = na.omit(leitura_fusao)[1] %||% NA_character_
), by = id_pessoa]

## ---------------------------------------------------------------- painel ano
pos <- mand[, {
  anos <- (ano_eleicao + 1L):(ano_eleicao + dur(cd_cargo))
  .(ano = anos)
}, by = .(id_mandato, id_pessoa, cd_cargo, cargo, esfera, sg_uf, sg_ue, nm_ue,
          ano_eleicao, sg_partido, nr_partido)]

## ---------------------------------------------------------------- salvar
salvar <- function(dt, nome) {
  fwrite(dt, file.path(outd, paste0(nome, ".csv")), sep = ",", na = "NA",
         quote = TRUE, bom = FALSE)
  write_parquet(dt, file.path(outd, paste0(nome, ".parquet")))
  saveRDS(dt, file.path(outd, paste0(nome, ".rds")))
  cat(nome, ":", nrow(dt), "linhas x", ncol(dt), "colunas\n")
}
setcolorder(mand, c("id_mandato", "id_pessoa"))
salvar(mand, "mandatos")
salvar(pos, "posicoes_ano")
salvar(pess, "pessoas")
fwrite(data.table(chave = c("n_imputados_por_votos", "n_duplicatas_posicao_removidas",
                            "n_eleitos_sem_identidade_excluidos"),
                  valor = c(n_imput_votos, n_dup_posicao, n_sem_identidade)),
       file.path(root, "output", "construcao_contagens.csv"))

cat("\n03_build_banco: concluido.\n")
cat("pessoas:", nrow(pess), "| mandatos:", nrow(mand), "| posicoes-ano:", nrow(pos),
    "| imputados por votos:", n_imput_votos, "| duplicatas de posicao removidas:", n_dup_posicao, "\n")
