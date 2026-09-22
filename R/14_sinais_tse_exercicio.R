# 14_sinais_tse_exercicio.R — sinais do proprio TSE sobre exercicio do mandato
# Para cada mandato do BOCEL procura a candidatura da mesma pessoa na eleicao
# imediatamente posterior (mesmo cargo, mesma unidade) e le ST_REELEICAO:
# 'S' = titular em exercicio no registro de candidatura (15/ago do ano eleitoral).
# Registra tambem a mudanca de partido entre o mandato e a candidatura seguinte
# e a situacao da candidatura seguinte (renuncia, falecimento, cassacao, ...).
#
# Entrada:  data/mandatos.csv, data_raw/parquet/cand_<ANO>.parquet,
#           data_raw/divulgacand/reeleicao_<ANO>.csv (cache da API DivulgaCand,
#           gerado por R/coleta/divulgacand_reeleicao.R, para os anos em que
#           o arquivo consulta_cand distribuido pelo TSE nao traz ST_REELEICAO)
# Saida:    data/sinais_tse_exercicio.csv
#           data_raw/divulgacand/pedidos_reeleicao.csv (lista de SQ a consultar na API)
#           output/sinais_tse_exercicio_por_cargo_ano.csv
# Execucao: Rscript --vanilla R/14_sinais_tse_exercicio.R   (a partir da raiz do repo)
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
pq   <- file.path(root, "data_raw", "parquet")
dc   <- file.path(root, "data_raw", "divulgacand")
dir.create(dc, showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE)
dir.create("output", showWarnings = FALSE)
script <- "R/14_sinais_tse_exercicio.R"
logf <- file("logs/14_sinais_tse_exercicio.log", open = "wt")
sink(logf, split = TRUE)
cat("14_sinais_tse_exercicio | inicio", format(Sys.time()), "\n")
reg <- function(chave, valor) registrar_numero(chave, valor, script = script,
                                              out = "output/numeros_assinatura.txt")

## ---------------------------------------------------------------- candidaturas (mesma limpeza do 03)
ne <- function(x) fifelse(is.na(x) | x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""),
                          NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue) {
  fcase(cd_cargo %in% 11:13, sg_ue, cd_cargo %in% 1:2, "BR", default = sg_uf)
}
cols <- c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO", "SG_UF", "SG_UE", "CD_CARGO",
          "SQ_CANDIDATO", "NR_CANDIDATO", "NM_CANDIDATO", "NR_CPF_CANDIDATO",
          "NR_TITULO_ELEITORAL_CANDIDATO", "DT_NASCIMENTO", "SG_PARTIDO", "NR_PARTIDO",
          "ST_REELEICAO", "DS_SITUACAO_CANDIDATURA", "DS_SITUACAO_CANDIDATO_PLEITO",
          "DS_SIT_TOT_TURNO", "ST_SUBSTITUIDO")
cand_files <- list.files(pq, pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE)
cand <- rbindlist(lapply(cand_files, function(f) {
  x <- setDT(read_parquet(f))
  falta <- setdiff(cols, names(x))
  for (v in falta) x[, (v) := NA_character_]
  x <- x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)), ..cols]
  x
}), use.names = TRUE)
cand[, `:=`(
  ano_eleicao = as.integer(ANO_ELEICAO),
  nr_turno    = as.integer(NR_TURNO),
  cd_cargo    = as.integer(CD_CARGO),
  titulo      = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)),
  cpf         = ne(num_only(NR_CPF_CANDIDATO)),
  nome        = ne(NM_CANDIDATO),
  dt_nasc     = ne(DT_NASCIMENTO),
  st_reel_arq = ne(ST_REELEICAO),
  sit_cand    = toupper(ne(DS_SITUACAO_CANDIDATURA)),
  sit_pleito  = toupper(ne(DS_SITUACAO_CANDIDATO_PLEITO)),
  sit_tot     = toupper(ne(DS_SIT_TOT_TURNO))
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
cand[, nome_nasc := fifelse(!is.na(nome_norm) & !is.na(dt_nasc_iso),
                            paste(nome_norm, dt_nasc_iso), NA_character_)]
cand[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
cand[cd_cargo %in% 1:2, SG_UF := "BR"]
cand[, chave_cand := paste(ano_eleicao, ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
# uma linha por candidatura: turno decisivo (situacao de totalizacao do ultimo turno)
setorder(cand, chave_cand, -nr_turno)
cand <- cand[, .SD[1], by = chave_cand]

## ---------------------------------------------------------------- preenchimento de ST_REELEICAO por ano
fill_arq <- cand[, .(n = .N, n_preenchido = sum(!is.na(st_reel_arq)),
                     n_S = sum(st_reel_arq %in% "S")), by = ano_eleicao][order(ano_eleicao)]
fill_arq[, fonte_arquivo := fifelse(n_preenchido > 0, "consulta_cand", "ausente")]
cat("\nST_REELEICAO no arquivo consulta_cand por ano:\n"); print(fill_arq)
anos_vazios <- fill_arq[n_preenchido == 0, ano_eleicao]
reg("sinais_anos_st_reeleicao_vazio_consulta_cand", paste(anos_vazios, collapse = ";"))
reg("sinais_anos_st_reeleicao_preenchido_consulta_cand",
    paste(fill_arq[n_preenchido > 0, ano_eleicao], collapse = ";"))

## ---------------------------------------------------------------- cache da API DivulgaCand (anos sem ST_REELEICAO no arquivo)
api_files <- list.files(dc, pattern = "^reeleicao_\\d{4}\\.csv$", full.names = TRUE)
api <- if (length(api_files)) {
  a <- rbindlist(lapply(api_files, fread, colClasses = "character"), use.names = TRUE, fill = TRUE)
  a <- a[!is.na(sq_candidato) & sq_candidato != ""]
  a[, ano_eleicao := as.integer(ano_eleicao)]
  a[, st_reel_api := fcase(st_reeleicao %in% c("True", "TRUE", "true", "S"), "S",
                           st_reeleicao %in% c("False", "FALSE", "false", "N"), "N",
                           default = NA_character_)]
  unique(a[, .(ano_eleicao, SQ_CANDIDATO = sq_candidato, st_reel_api,
               sit_api = toupper(ne(descricao_situacao)), partido_api = ne(sigla_partido))],
         by = c("ano_eleicao", "SQ_CANDIDATO"))
} else data.table(ano_eleicao = integer(), SQ_CANDIDATO = character(), st_reel_api = character(),
                  sit_api = character(), partido_api = character())
cat("\ncache DivulgaCand:", nrow(api), "candidaturas em", length(api_files), "arquivos\n")
# SQ_CANDIDATO e unico nacionalmente a partir de 2010; o cache so cobre anos >= 2014
cand <- merge(cand, api, by = c("ano_eleicao", "SQ_CANDIDATO"), all.x = TRUE)
cand[, st_reeleicao := fifelse(!is.na(st_reel_arq), st_reel_arq, st_reel_api)]
cand[, fonte_st_reeleicao := fcase(!is.na(st_reel_arq), "consulta_cand",
                                   !is.na(st_reel_api), "divulgacand_api",
                                   default = NA_character_)]
cand[, situacao := fcase(!is.na(sit_pleito), sit_pleito,
                         !is.na(sit_api), sit_api,
                         !is.na(sit_cand) & !sit_cand %in% c("APTO", "INAPTO", "CADASTRADO"), sit_cand,
                         default = sit_cand)]

## ---------------------------------------------------------------- id_pessoa das candidaturas (regra do 03)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
mand[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo))]
mand[, chave_cand := sub("^M", "", id_mandato)]
# chaves de identidade de cada pessoa = chaves das candidaturas eleitas que compoem a pessoa
elei <- merge(mand[, .(id_pessoa, chave_cand)], cand[, .(chave_cand, titulo, cpf, nome_nasc)],
              by = "chave_cand", all.x = TRUE)
n_sem_par <- elei[!chave_cand %in% cand$chave_cand, .N]
n_sem_chave <- elei[is.na(titulo) & is.na(cpf) & is.na(nome_nasc), .N]
cat("mandatos sem candidatura pareada no parquet:", n_sem_par,
    "| mandatos cuja candidatura nao tem titulo, cpf nem nome+nascimento:", n_sem_chave, "\n")
if (n_sem_par > 0) print(mand[chave_cand %in% elei[!chave_cand %in% cand$chave_cand, chave_cand],
                              .N, by = .(ano_eleicao, cd_cargo)])
if (n_sem_chave > 0) print(merge(elei[is.na(titulo) & is.na(cpf) & is.na(nome_nasc), .(chave_cand)],
                                 mand[, .(chave_cand, ano_eleicao, cd_cargo)], by = "chave_cand")[, .N, by = .(ano_eleicao, cd_cargo)])
reg("sinais_n_mandatos_sem_chave_identidade", n_sem_chave)
reg("sinais_n_mandatos_sem_par_parquet", n_sem_par)
k_tit <- unique(elei[!is.na(titulo), .(titulo, id_pessoa)])
k_cpf <- unique(elei[!is.na(cpf), .(cpf, id_pessoa)])
k_nom <- unique(elei[!is.na(nome_nasc), .(nome_nasc, id_pessoa)])
# uma chave nunca aponta para duas pessoas (o 03 funde por fecho transitivo), salvo a ponte de cadastro
# auditada no R/03 (12/09/2026, output/verificacao/pontes_nucleo_referencia.csv), em que o titulo de uma
# pessoa aparece ao lado do CPF e do nascimento de outra. A chave ambigua sai do mapa e a candidatura cai
# na chave seguinte; a guarda reprova qualquer ambiguidade fora das candidaturas de ponte.
f_pontes <- file.path(root, "output", "verificacao", "pontes_nucleo_referencia.csv")
cc_ponte <- if (file.exists(f_pontes)) fread(f_pontes, colClasses = "character")$chave_cand else character()
# 21/09/2026: a separacao de pessoas fundidas por documento (R/03, output/verificacao/fusao_documentos_separacao.csv)
# deixa o titulo ou o CPF digitado errado no cadastro do TSE em mais de um id_pessoa. A chave ambigua so passa
# quando vem de candidatura de ponte ou quando todas as pessoas que ela liga sairam dessa separacao
sep_ids <- fread(file.path(root, "output", "verificacao", "fusao_documentos_separacao.csv"),
                 colClasses = "character", na.strings = c("", "NA"))$id_pessoa_esperado
amb_de <- function(k, col) {
  a <- k[, .(N = .N, so_separadas = all(id_pessoa %chin% sep_ids)), by = col][N > 1L]
  a[, ponte := get(col) %chin% elei[chave_cand %in% cc_ponte, get(col)]]
  stopifnot(all(a$ponte | a$so_separadas))
  a
}
a_tit <- amb_de(k_tit, "titulo"); a_cpf <- amb_de(k_cpf, "cpf"); a_nom <- amb_de(k_nom, "nome_nasc")
amb_tit <- a_tit$titulo; amb_cpf <- a_cpf$cpf; amb_nom <- a_nom$nome_nasc
reg("sinais_chaves_ambiguas_por_ponte", sum(a_tit$ponte) + sum(a_cpf$ponte) + sum(a_nom$ponte))
reg("sinais_chaves_ambiguas_por_separacao_de_fusao", sum(!a_tit$ponte) + sum(!a_cpf$ponte) + sum(!a_nom$ponte))
k_tit <- k_tit[!titulo %in% amb_tit]; k_cpf <- k_cpf[!cpf %in% amb_cpf]; k_nom <- k_nom[!nome_nasc %in% amb_nom]
stopifnot(!anyDuplicated(k_tit$titulo), !anyDuplicated(k_cpf$cpf), !anyDuplicated(k_nom$nome_nasc))
cand[k_tit, id_pessoa := i.id_pessoa, on = "titulo"]
cand[is.na(id_pessoa), id_pessoa := k_cpf[.SD, id_pessoa, on = "cpf"]]
cand[is.na(id_pessoa), id_pessoa := k_nom[.SD, id_pessoa, on = "nome_nasc"]]
cat("\ncandidaturas com id_pessoa do BOCEL:", cand[!is.na(id_pessoa), .N], "de", nrow(cand), "\n")

## ---------------------------------------------------------------- candidatura seguinte
dur <- function(cd) fifelse(cd == 5L, 8L, 4L)
mand[, ano_seguinte := ano_eleicao + dur(cd_cargo)]
janela_max <- max(cand$ano_eleicao)
prox <- cand[!is.na(id_pessoa), .(id_pessoa, ano_seguinte = ano_eleicao, cd_cargo_seg = cd_cargo,
                                  ue_seg = ue_pos, st_reeleicao, fonte_st_reeleicao,
                                  partido_seg = SG_PARTIDO, situacao_seg = situacao,
                                  sit_tot_seg = sit_tot, sq_seg = SQ_CANDIDATO,
                                  subst = ST_SUBSTITUIDO %in% "S")]
# candidatura no mesmo cargo e unidade (preferida); entre repetidas, a nao substituida
setorder(prox, id_pessoa, ano_seguinte, subst, -sq_seg)
pm <- merge(mand[, .(id_mandato, id_pessoa, ano_seguinte, cd_cargo, unidade_posicao)],
            prox, by.x = c("id_pessoa", "ano_seguinte", "cd_cargo", "unidade_posicao"),
            by.y = c("id_pessoa", "ano_seguinte", "cd_cargo_seg", "ue_seg"), all.x = FALSE)
pm <- pm[!duplicated(id_mandato)]
pm[, mesmo := TRUE]
# qualquer candidatura da pessoa no ano seguinte (para quem nao repetiu cargo/unidade)
po <- merge(mand[!id_mandato %in% pm$id_mandato, .(id_mandato, id_pessoa, ano_seguinte)],
            prox, by = c("id_pessoa", "ano_seguinte"))
po <- po[!duplicated(id_mandato)]
po[, mesmo := FALSE]
seg <- rbindlist(list(
  pm[, .(id_mandato, mesmo, st_reeleicao, fonte_st_reeleicao, partido_seg, situacao_seg,
         sit_tot_seg, sq_seg, cd_cargo_seg = cd_cargo, ue_seg = unidade_posicao)],
  po[, .(id_mandato, mesmo, st_reeleicao, fonte_st_reeleicao, partido_seg, situacao_seg,
         sit_tot_seg, sq_seg, cd_cargo_seg, ue_seg)]), use.names = TRUE)

out <- merge(mand[, .(id_mandato, id_pessoa, ano_eleicao, cd_cargo, cargo, esfera,
                      unidade_posicao, ano_seguinte, partido_mandato = sg_partido)],
             seg, by = "id_mandato", all.x = TRUE)
out[, candidatura_seguinte := fifelse(ano_seguinte > janela_max, NA, !is.na(sq_seg))]
out[, mesmo_cargo_mesma_unidade := fifelse(candidatura_seguinte %in% TRUE, mesmo, NA)]
out[, exercicio_confirmado_em := fifelse(mesmo_cargo_mesma_unidade %in% TRUE & st_reeleicao %in% "S",
                                         sprintf("%d-08-15", ano_seguinte), NA_character_)]
out[, mudou_partido := fifelse(candidatura_seguinte %in% TRUE & !is.na(partido_seg) & !is.na(partido_mandato),
                               partido_seg != partido_mandato, NA)]
sinais <- out[, .(id_mandato, id_pessoa, ano_eleicao, cd_cargo, cargo, esfera, unidade_posicao,
                  ano_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, ano_seguinte, NA_integer_),
                  candidatura_seguinte, mesmo_cargo_mesma_unidade,
                  cd_cargo_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, cd_cargo_seg, NA_integer_),
                  unidade_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, ue_seg, NA_character_),
                  sq_candidatura_seguinte = sq_seg,
                  st_reeleicao = fifelse(candidatura_seguinte %in% TRUE, st_reeleicao, NA_character_),
                  fonte_st_reeleicao = fifelse(candidatura_seguinte %in% TRUE, fonte_st_reeleicao, NA_character_),
                  exercicio_confirmado_em,
                  partido_mandato,
                  partido_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, partido_seg, NA_character_),
                  mudou_partido,
                  situacao_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, situacao_seg, NA_character_),
                  resultado_candidatura_seguinte = fifelse(candidatura_seguinte %in% TRUE, sit_tot_seg, NA_character_))]
setorder(sinais, ano_eleicao, cd_cargo, unidade_posicao, id_mandato)
stopifnot(nrow(sinais) == nrow(mand), !anyDuplicated(sinais$id_mandato))
fwrite(sinais, "data/sinais_tse_exercicio.csv", sep = ",", na = "NA", quote = TRUE)
cat("\ndata/sinais_tse_exercicio.csv:", nrow(sinais), "linhas x", ncol(sinais), "colunas\n")

## ---------------------------------------------------------------- pedidos para a API (anos sem ST_REELEICAO no arquivo)
ped <- sinais[candidatura_seguinte %in% TRUE & mesmo_cargo_mesma_unidade %in% TRUE &
                ano_candidatura_seguinte %in% anos_vazios & ano_candidatura_seguinte >= 2014,
              .(ano_eleicao = ano_candidatura_seguinte, sg_ue = unidade_candidatura_seguinte,
                cd_cargo = cd_cargo_candidatura_seguinte, sq_candidato = sq_candidatura_seguinte)]
ped <- unique(ped)
fwrite(ped, file.path(dc, "pedidos_reeleicao.csv"))
# sem resposta = ausente do cache OU presente so com erro de rede (http -1, st_reel_api NA);
# o fetch em Python refaz os erros de rede a cada execucao
ped_falta <- ped[!api[!is.na(st_reel_api)], on = c("ano_eleicao", "sq_candidato" = "SQ_CANDIDATO")]
cat("pedidos a API DivulgaCand:", nrow(ped), "| ainda sem resposta no cache:", nrow(ped_falta), "\n")
reg("sinais_n_pedidos_api_divulgacand", nrow(ped))
reg("sinais_n_pedidos_api_sem_resposta", nrow(ped_falta))

## ---------------------------------------------------------------- numeros
reg("sinais_n_mandatos", nrow(sinais))
reg("sinais_n_mandatos_janela_seguinte", sinais[!is.na(candidatura_seguinte), .N])
reg("sinais_n_candidatura_seguinte", sinais[candidatura_seguinte %in% TRUE, .N])
reg("sinais_n_mesmo_cargo_unidade", sinais[mesmo_cargo_mesma_unidade %in% TRUE, .N])
reg("sinais_n_mesmo_cargo_st_reeleicao_lido", sinais[mesmo_cargo_mesma_unidade %in% TRUE & !is.na(st_reeleicao), .N])
reg("sinais_n_exercicio_confirmado", sinais[!is.na(exercicio_confirmado_em), .N])
reg("sinais_n_mesmo_cargo_st_reeleicao_N", sinais[mesmo_cargo_mesma_unidade %in% TRUE & st_reeleicao %in% "N", .N])
reg("sinais_n_st_reeleicao_fonte_api", sinais[fonte_st_reeleicao %in% "divulgacand_api", .N])
reg("sinais_n_mudou_partido", sinais[mudou_partido %in% TRUE, .N])

tab <- sinais[!is.na(candidatura_seguinte),
              .(n_mandatos = .N,
                n_candidatura_seguinte = sum(candidatura_seguinte %in% TRUE),
                n_mesmo_cargo_unidade = sum(mesmo_cargo_mesma_unidade %in% TRUE),
                n_st_reeleicao_lido = sum(mesmo_cargo_mesma_unidade %in% TRUE & !is.na(st_reeleicao)),
                n_exercicio_confirmado = sum(!is.na(exercicio_confirmado_em)),
                n_mudou_partido = sum(mudou_partido %in% TRUE)),
              by = .(ano_eleicao, cd_cargo, cargo)][order(cd_cargo, ano_eleicao)]
tab[, prop_exercicio_confirmado := round(n_exercicio_confirmado / n_mandatos, 4)]
tab[, prop_confirmado_entre_lidos := round(fifelse(n_st_reeleicao_lido > 0,
                                                   n_exercicio_confirmado / n_st_reeleicao_lido, NA_real_), 4)]
fwrite(tab, "output/sinais_tse_exercicio_por_cargo_ano.csv", na = "NA")
cat("\nproporcao de mandatos com exercicio confirmado por cargo e ano:\n"); print(tab)
for (i in seq_len(nrow(tab))) {
  reg(sprintf("sinais_prop_exercicio_confirmado_c%d_%d", tab$cd_cargo[i], tab$ano_eleicao[i]),
      tab$prop_exercicio_confirmado[i])
}
tabc <- sinais[, .(n_mudou_partido = sum(mudou_partido %in% TRUE),
                   n_comparavel = sum(!is.na(mudou_partido))), by = .(cd_cargo, cargo)][order(cd_cargo)]
cat("\nmudancas de partido por cargo:\n"); print(tabc)
for (i in seq_len(nrow(tabc))) {
  reg(sprintf("sinais_n_mudou_partido_c%d", tabc$cd_cargo[i]), tabc$n_mudou_partido[i])
  reg(sprintf("sinais_n_partido_comparavel_c%d", tabc$cd_cargo[i]), tabc$n_comparavel[i])
}
# coerencia com a coluna reeleicao_declarada do proprio mandato (mesma origem, mandato de destino)
chk <- merge(sinais[mesmo_cargo_mesma_unidade %in% TRUE & !is.na(st_reeleicao) &
                      fonte_st_reeleicao == "consulta_cand",
                    .(id_pessoa, ano_eleicao = ano_candidatura_seguinte, cd_cargo, unidade_posicao, st_reeleicao)],
             mand[, .(id_pessoa, ano_eleicao, cd_cargo, unidade_posicao, reeleicao_declarada)],
             by = c("id_pessoa", "ano_eleicao", "cd_cargo", "unidade_posicao"))
reg("sinais_check_n_reeleitos_comparaveis", nrow(chk))
reg("sinais_check_n_divergentes_reeleicao_declarada", chk[st_reeleicao != reeleicao_declarada, .N])
if (chk[st_reeleicao != reeleicao_declarada, .N] > 0) {
  cat("\ndivergencias entre st_reeleicao lido e reeleicao_declarada do mandato de destino:\n")
  print(chk[st_reeleicao != reeleicao_declarada][, .N, by = .(ano_eleicao, cd_cargo, st_reeleicao, reeleicao_declarada)])
}
# preenchimento efetivo de ST_REELEICAO por ano e cargo (n de 'S'), para declarar em que
# anos o campo vem vazio ou preenchido so com 'N'
fill_ac <- cand[cd_cargo %in% c(1:8, 11:13),
                .(n_candidaturas = .N, n_st_preenchido = sum(!is.na(st_reeleicao)),
                  n_S = sum(st_reeleicao %in% "S"), n_N = sum(st_reeleicao %in% "N"),
                  n_fonte_api = sum(fonte_st_reeleicao %in% "divulgacand_api")),
                by = .(ano_eleicao, cd_cargo)][order(ano_eleicao, cd_cargo)]
fwrite(fill_ac, "output/sinais_st_reeleicao_preenchimento_ano_cargo.csv", na = "NA")
cat("\nST_REELEICAO por ano e cargo (n de S):\n"); print(fill_ac, nrows = 200)
ano_s <- cand[, .(n_S = sum(st_reeleicao %in% "S")), by = ano_eleicao][order(ano_eleicao)]
for (i in seq_len(nrow(ano_s))) reg(sprintf("sinais_n_st_reeleicao_S_%d", ano_s$ano_eleicao[i]), ano_s$n_S[i])

cat("\n14_sinais_tse_exercicio | fim", format(Sys.time()), "\n")
sink()
close(logf)
