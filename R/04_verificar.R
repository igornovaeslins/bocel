# 04_verificar.R — verificacao: asserts, contagens de referencia TSE, registro de numeros
# Execucao: cd ~/bocel && Rscript --vanilla R/04_verificar.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
mand <- fread("data/mandatos.csv", colClasses = "character")
pos  <- fread("data/posicoes_ano.csv", colClasses = "character")
pess <- fread("data/pessoas.csv", colClasses = "character")

passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ expr; TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  r
}

## ------------------------------------------------ chaves e granularidade
ok("mandatos: id_mandato unico", checa_unica(as.data.frame(mand),"id_mandato"))
ok("pessoas: id_pessoa unico", checa_unica(as.data.frame(pess),"id_pessoa"))
ok("painel: id_mandato x ano unico", checa_unica(as.data.frame(pos),c("id_mandato", "ano")))
ok("mandatos: todo id_pessoa existe em pessoas",
   stopifnot(all(mand$id_pessoa %in% pess$id_pessoa)))
ok("painel: todo id_mandato existe em mandatos",
   stopifnot(all(pos$id_mandato %in% mand$id_mandato)))

## ------------------------------------------------ dominio
ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA",
         "PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO","BR")
ok("mandatos: SG_UF no conjunto canonico", in_set(mand$sg_uf, ufs, nome = "sg_uf"))
ok("mandatos: esfera canonica",
   in_set(mand$esfera, c("federal", "estadual", "municipal"), nome = "esfera"))
ok("mandatos: ano_eleicao em faixa",
   em_faixa(as.integer(mand$ano_eleicao), 1998, 2024, nome = "ano_eleicao"))
ok("painel: ano em faixa",
   em_faixa(as.integer(pos$ano), 1999, 2032, nome = "ano"))
ok("mandatos: votos em faixa plausivel",
   em_faixa(as.numeric(mand$votos_turno_decisivo), 0, 70e6,
            nome = "votos_turno_decisivo"))

## ------------------------------------------------ contagens de referencia TSE
# cadeiras esperadas por cargo e ano (referencias institucionais publicas)
mand[, ano_i := as.integer(ano_eleicao)]
cont <- mand[, .N, by = .(ano_i, cargo)]
ref_check <- function(nome, cargo_pat, anos, lo, hi) {
  n <- cont[grepl(cargo_pat, toupper(cargo)) & ano_i %in% anos, sum(N)] / length(anos)
  ok(nome, stopifnot(n >= lo, n <= hi))
}
anos_gerais <- intersect(unique(mand$ano_i), seq(1998, 2022, 4))
anos_munic  <- intersect(unique(mand$ano_i), seq(2000, 2024, 4))
if (length(anos_gerais)) {
  ref_check("presidentes: 1 por eleicao geral", "^PRESIDENTE", anos_gerais, 1, 1)
  ref_check("governadores: 27 por eleicao geral", "^GOVERNADOR", anos_gerais, 27, 27)
  ref_check("dep federais: 513 por eleicao geral", "FEDERAL", anos_gerais, 513, 513)
  ref_check("dep estaduais+distritais: 1059", "ESTADUAL|DISTRITAL", anos_gerais, 1059, 1059)
}
if (length(anos_munic)) {
  ref_check("prefeitos: ~5500 por eleicao municipal", "^PREFEITO", anos_munic, 5400, 5600)
  ref_check("vereadores: 50k-60k por eleicao municipal", "VEREADOR", anos_munic, 50000, 60000)
}
# senadores alternam 1/3 e 2/3; excecoes documentadas em docs/EXCECOES_CONHECIDAS.csv
exc <- fread("docs/EXCECOES_CONHECIDAS.csv")
for (a in anos_gerais) {
  esperado <- if (a %in% c(1998, 2006, 2014, 2022)) 27L else 54L
  esperado <- esperado + exc[ano_eleicao == a & cargo == "SENADOR", sum(delta_esperado)]
  n_sen <- cont[grepl("^SENADOR", toupper(cargo)) & ano_i == a, sum(N)]
  ok(sprintf("senadores %d: %d cadeiras", a, esperado),
     stopifnot(n_sen == esperado))
}
# cada posicao executiva tem no maximo um ocupante por eleicao
ok("executivos: uma pessoa por unidade-cargo-eleicao",
   checa_unica(as.data.frame(mand[cargo %in% c("PREFEITO", "GOVERNADOR", "PRESIDENTE", "VICE-PREFEITO", "VICE-GOVERNADOR", "VICE-PRESIDENTE")]),
               c("ano_eleicao", "unidade_posicao", "cd_cargo")))
# vices: um por titular eleito
n_pref <- cont[cargo == "PREFEITO", sum(N)]; n_vpref <- cont[cargo == "VICE-PREFEITO", sum(N)]
ok("vice-prefeitos <= prefeitos (total)", stopifnot(n_vpref <= n_pref))

## ------------------------------------------------ taxa de pareamento (spec §4)
# vices nao tem linha de votacao propria (o voto e da chapa): ficam fora da taxa
pareamento <- mand[!grepl("^VICE", toupper(cargo)), .(
  n_mandatos = .N,
  com_votos = sum(!is.na(votos_turno_decisivo) & votos_turno_decisivo != "NA"),
  com_titulo = sum(!is.na(id_pessoa))
), by = .(ano_eleicao, esfera)][order(ano_eleicao, esfera)]
pareamento[, taxa_votos := round(com_votos / n_mandatos, 4)]
fwrite(pareamento, "output/verificacao/taxa_pareamento.csv")
ok("pareamento votos >= 97% em toda esfera-ano",
   stopifnot(pareamento[, all(taxa_votos >= 0.97)]))

## ------------------------------------------------ sucessao executiva
exec <- mand[cargo %in% c("PREFEITO", "GOVERNADOR", "PRESIDENTE")]
suc <- exec[!is.na(sucessor_id) & sucessor_id != "NA"]
ok("sucessao: sucessor_id sempre existe em pessoas",
   stopifnot(all(suc$sucessor_id %in% pess$id_pessoa)))
# simetria: se A tem sucessor B na posicao P, B tem antecessor A em P
setkey(exec, cd_cargo, sg_ue, ano_eleicao)
exec[, ante_do_sucessor := shift(antecessor_id, -1L), by = .(cd_cargo, sg_ue)]
sim <- exec[!is.na(sucessor_id) & sucessor_id != "NA" &
            !is.na(ante_do_sucessor) & ante_do_sucessor != "NA"]
ok("sucessao: simetria antecessor/sucessor",
   stopifnot(sim[, all(id_pessoa == ante_do_sucessor)]))

## ------------------------------------------------ posse, exercicio e forma de saida (se integrados)
for (f in c("data/exercicio_camaras_municipais.csv", "data/wikipedia_estadual.csv", "data/wikipedia_prefeitos.csv",
            "data/exercicio_camaras_sem_sapl.csv", "data/diarios_mandatos_saida.csv", "data/exercicio_assembleias_historico.csv",
            "data/exercicio_camaras_sem_sapl_2.csv", "data/tce_gestores.csv", "data/tce_gestores_b.csv",
            "data/tce_gestores_c.csv", "data/tce_gestores_d.csv", "data/exercicio_camaras_sem_sapl_2.csv")) {
  if (file.exists(f)) {
    x <- fread(f, colClasses = "character", na.strings = "NA")
    idc <- intersect(c("id_mandato_bocel", "id_mandato"), names(x))
    if (length(idc)) ok(sprintf("%s: id_mandato pareado existe em mandatos", basename(f)),
                        stopifnot(all(na.omit(x[[idc[1]]]) %in% mand$id_mandato)))
  }
}
VOCAB_SAIDA <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
                 "nao_tomou_posse", "suplente_efetivado", "perda_do_mandato_inferida_por_eleicao_suplementar",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
                 "substituicao_inferida_munic", "assumiu_titular", "aposentadoria", "impeachment", "retotalizacao", "outro", "nao_observado")
ok("mandatos: forma_saida no vocabulario fechado",
   in_set(mand$forma_saida, VOCAB_SAIDA, permitir_na = FALSE, nome = "forma_saida"))

# 06/09/2026: as colunas que R/53 grava passam a ser verificadas aqui, porque R/53 passou a rodar
# ANTES deste verificador e a versao que vai ao deposito e a que ele produz.
if ("motivo_sem_saida" %in% names(mand)) {
  VOCAB_MOTIVO <- c("nao_se_aplica", "em_curso", "fim_regular_presumido", "fonte_ausente")
  ok("mandatos: motivo_sem_saida no vocabulario fechado",
     in_set(mand$motivo_sem_saida, VOCAB_MOTIVO, permitir_na = FALSE, nome = "motivo_sem_saida"))
  ok("mandatos: motivo 'nao_se_aplica' se e somente se a saida foi observada",
     mand[(forma_saida != "nao_observado") != (motivo_sem_saida == "nao_se_aplica"), .N] == 0)
  ok("mandatos: 'fim_regular_presumido' so dentro do universo comparavel",
     mand[motivo_sem_saida == "fim_regular_presumido" & universo_comparavel != TRUE, .N] == 0)
  ok("mandatos: universo_comparavel logico sem vazio",
     all(as.character(mand$universo_comparavel) %in% c("TRUE", "FALSE")))
  ok("mandatos: chave_tse unica", uniqueN(mand$chave_tse) == nrow(mand))
}
# a chapa do vice: o numero de vices nao pode ficar muito abaixo do de titulares (06/09/2026)
for (par in list(c("12", "11"), c("4", "3"), c("2", "1"))) {
  cmp <- merge(mand[cd_cargo == par[1], .(n_vice = .N), by = ano_eleicao],
               mand[cd_cargo == par[2], .(n_tit = .N), by = ano_eleicao], by = "ano_eleicao")
  if (nrow(cmp)) {
    cmp[, falta := n_tit - n_vice]
    nm <- c(`12` = "vice_prefeito", `4` = "vice_governador", `2` = "vice_presidente")[[par[1]]]
    registrar_numero(sprintf("bocel_chapa_%s_maior_lacuna", nm), max(cmp$falta), script = "R/04_verificar.R")
    ok(sprintf("chapa: %s alcanca ao menos 95%% dos titulares em toda eleicao", nm),
       cmp[n_vice < 0.95 * n_tit, .N] == 0)
  }
}
if ("data_posse" %in% names(mand)) {
  ok("mandatos: data_posse em faixa (ano da eleicao+1 ate fim do mandato)",
     stopifnot(mand[!is.na(data_posse) & data_posse != "NA",
                    all(substr(data_posse, 1, 4) >= ano_eleicao & data_posse <= mandato_fim)]))
  ok("mandatos: data_fim_efetiva nao anterior a data_posse",
     stopifnot(mand[!is.na(data_posse) & !is.na(data_fim_efetiva) & data_posse != "NA" & data_fim_efetiva != "NA",
                    all(data_fim_efetiva >= data_posse)]))
  ok("mandatos: forma_saida observada tem fonte",
     stopifnot(mand[forma_saida != "nao_observado", all(!is.na(fonte_forma_saida) & fonte_forma_saida != "NA")]))
  ok("mandatos: saida antecipada tem data_fim_efetiva anterior ao fim convencional",
     stopifnot(mand[forma_saida %in% c("renuncia", "falecimento", "cassacao", "perda_do_mandato_inferida_por_eleicao_suplementar") &
                    !is.na(data_fim_efetiva) & data_fim_efetiva != "NA", all(data_fim_efetiva <= mandato_fim)]))
}
for (f in c("data/exercicio_camara.csv", "data/exercicio_senado.csv", "data/exercicio_assembleias.csv",
            "data/wikidata_mandatos.csv", "data/munic_prefeitos.csv", "data/eleicoes_suplementares.csv",
            "data/sinais_tse_exercicio.csv", "data/auditoria_homonimos.csv")) {
  if (file.exists(f)) {
    x <- fread(f, colClasses = "character", na.strings = "NA")
    idc <- intersect(c("id_mandato_bocel", "id_mandato", "id_mandato_ordinario_afetado"), names(x))
    if (length(idc)) ok(sprintf("%s: id_mandato pareado existe em mandatos", basename(f)),
                        stopifnot(all(na.omit(x[[idc[1]]]) %in% mand$id_mandato)))
    idp <- intersect(c("id_pessoa_bocel", "id_pessoa"), names(x))
    if (length(idp)) ok(sprintf("%s: id_pessoa pareado existe em pessoas", basename(f)),
                        stopifnot(all(na.omit(x[[idp[1]]]) %in% pess$id_pessoa)))
    if ("forma_saida" %in% names(x)) ok(sprintf("%s: forma_saida no vocabulario", basename(f)),
                                        in_set(x$forma_saida, VOCAB_SAIDA, nome = basename(f)))
  }
}

## ------------------------------------------------ filiacoes (se existir)
tem_fil <- file.exists("data/filiacoes.csv")
if (tem_fil) {
  fil <- fread("data/filiacoes.csv", colClasses = "character", na.strings = "NA")
  ok("filiacoes: id_pessoa existe em pessoas",
     stopifnot(all(fil$id_pessoa %in% pess$id_pessoa)))
  ok("filiacoes: chave pessoa x partido x uf x data unica",
     checa_unica(as.data.frame(fil),c("id_pessoa", "sigla_partido", "sg_uf", "data_filiacao")))
  ok("filiacoes: data_filiacao em faixa",
     em_faixa(as.integer(substr(fil$data_filiacao, 1, 4)), 1900, 2026, nome = "ano_filiacao"))
  ok("filiacoes: cobertura >= 70% das pessoas com titulo",
     stopifnot(uniqueN(fil$id_pessoa) / sum(!is.na(pess$nr_titulo_eleitoral)) >= 0.70))
}

## ------------------------------------------------ registrar numeros
script <- "R/04_verificar.R"
if (tem_fil) {
  registrar_numero("bocel_n_filiacoes", nrow(fil), script = script)
  registrar_numero("bocel_pessoas_com_filiacao", uniqueN(fil$id_pessoa), script = script)
  registrar_numero("bocel_pct_pessoas_com_filiacao",
                   round(uniqueN(fil$id_pessoa) / nrow(pess), 4), script = script)
}
registrar_numero("bocel_n_pessoas", nrow(pess), script = script)
registrar_numero("bocel_n_mandatos", nrow(mand), script = script)
registrar_numero("bocel_n_posicoes_ano", nrow(pos), script = script)
registrar_numero("bocel_anos_cobertos",
                 paste(sort(unique(mand$ano_eleicao)), collapse = ";"),
                 script = script)
registrar_numero("bocel_pct_dedup_por_titulo",
                 round(mean(pess$chave_dedup == "titulo"), 4), script = script)
cc <- fread("output/construcao_contagens.csv")
for (i in seq_len(nrow(cc))) registrar_numero(paste0("bocel_", cc$chave[i]), cc$valor[i], script = script)
fs <- mand[, .N, by = fonte_situacao][order(-N)]
for (i in seq_len(nrow(fs))) registrar_numero(paste0("bocel_fonte_situacao_", fs$fonte_situacao[i]), fs$N[i], script = script)

fora <- c(
  "posse e exercicio nos mandatos sem fonte institucional (municipal e estadual fora das Assembleias com SAPL): convencao de datas",
  "forma de saida nos mandatos em que nenhuma fonte a registra (nao_observado) e causa da vacancia nos pleitos suplementares",
  "exatidao das datas e causas informadas pelas fontes (Camara, Senado, Assembleias, Wikidata, MUNIC)",
  "validade substantiva do pareamento por nome (homonimos) e das pontes nome+nascimento na deduplicacao"
)
rel <- gravar_relatorio_verificacao(
  alvo = "bocel v1.0",
  script = script, passou = passou, falhou = falhou,
  fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\n")
cat("PASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) { cat("Falhas:\n"); cat(paste("-", falhou), sep = "\n"); quit(status = 1) }
