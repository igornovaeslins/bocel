# 64_exportar_v1.R — filtra data/ para o recorte da v1.0 e grava as tabelas resultantes em data_v1/.
# Decisao de 21/09/2026 (item 1, opcao A): a v1.0 sai so com
# presidencia e vice, governador e vice, senador (com suplentes), deputado federal, a camada de ocupacao
# com suplencia e o historico de filiacao partidaria das pessoas do recorte. Assembleias (deputado estadual
# e distrital) e municipios (prefeito, vice-prefeito, vereador) ficam fora e vao para a v1.5/v2.0.
# A tabela ref/ids_pessoa_referencia.parquet nao entra aqui: ela ancora o id_pessoa entre TODAS as versoes
# do banco e por isso fica inteira e sem filtro, servida direto de ref/.
# Execucao: cd ~/bocel && Rscript --vanilla R/64_exportar_v1.R
suppressPackageStartupMessages({ library(data.table); library(arrow) })

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source("lib/asserts_rigor.R")
source("lib/proveniencia.R")
source("lib/data_referencia.R")

script <- "R/64_exportar_v1.R"
dir.create("data_v1", showWarnings = FALSE)

REF <- data_referencia()  # 12/09/2026: data de referencia da reconstrucao, nunca Sys.Date()

# cd_cargo de titular no recorte da v1.0 (presidente/vice, governador/vice, senador, deputado federal)
CARGOS_RECORTE <- c("1", "2", "3", "4", "5", "6")
# codigo de registro do 1o/2o suplente de senador; so aparece em suplentes_identidade.csv, nunca em
# mandatos.csv (suplente nao ganha mandato proprio)
CARGOS_SUPLENCIA_SENADO <- c("9", "10")

le <- function(nome) fread(file.path("data", nome), colClasses = "character", na.strings = "NA", encoding = "UTF-8")
# grava CSV sempre; grava tambem parquet quando a tabela tinha par .csv/.parquet no deposit.py anterior
# (as 7 tabelas de nucleo/ocupacao mais lidas por quem reconstroi em R), para nao perder essa paridade na v1.0
grava <- function(dt, nome, parquet = FALSE) {
  fwrite(dt, file.path("data_v1", nome), na = "NA")
  if (parquet) write_parquet(dt, file.path("data_v1", sub("\\.csv$", ".parquet", nome)))
  invisible(dt)
}

## --------------------------------------------------------------------------------------------- nucleo
mand <- le("mandatos.csv")
pos  <- le("posicoes_ano.csv")
pess <- le("pessoas.csv")

mand_v1 <- mand[cd_cargo %in% CARGOS_RECORTE]
pos_v1  <- pos[cd_cargo %in% CARGOS_RECORTE]
in_set(mand_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "mand_v1$cd_cargo")
in_set(pos_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "pos_v1$cd_cargo")
checa_unica(as.data.frame(mand_v1), "id_mandato")

IDS_MANDATO <- unique(mand_v1$id_mandato)

## --------------------------------------------------------------------------------- ocupacao e suplencia
ocu  <- le("ocupacoes.csv")
lsu  <- le("lista_suplencia.csv")
psup <- le("pessoas_suplentes.csv")
si   <- le("suplentes_identidade.csv")
ml   <- le("mandatos_lista.csv")

ocu_v1 <- ocu[cd_cargo %in% CARGOS_RECORTE]
lsu_v1 <- lsu[cd_cargo %in% CARGOS_RECORTE]                                  # cd_cargo = cadeira alvo, nao a de registro
si_v1  <- si[cd_cargo %in% c(CARGOS_RECORTE, CARGOS_SUPLENCIA_SENADO)]       # aqui cd_cargo e o de registro (inclui 9/10)
ml_v1  <- ml[cd_cargo %in% CARGOS_RECORTE]                                   # so tem cargos de lista: sobra deputado federal
in_set(ocu_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "ocu_v1$cd_cargo")
in_set(lsu_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "lsu_v1$cd_cargo")
in_set(si_v1$cd_cargo, c(CARGOS_RECORTE, CARGOS_SUPLENCIA_SENADO), permitir_na = FALSE, nome = "si_v1$cd_cargo")
in_set(ml_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "ml_v1$cd_cargo")
checa_unica(as.data.frame(ocu_v1), "id_ocupacao")

## ---------------------------------------------------------------- pessoas e universo de id_pessoa do recorte
# Pendencia 17, opcao A (decisao de Igor em 24/09, docs/PENDENCIAS_JULGAMENTO_IGOR.md): o cadastro de pessoas
# do recorte passava so por quem tem mandato ou posicao_ano (eleito), deixando de fora quem so ocupa a cadeira
# sem nunca ter sido o titular eleito (vice que assumiu, suplente efetivado, retotalizacao, substituicao por
# decisao) — 269 pessoas, 534 das 6287 ocupacoes do recorte, achado por R/verifica_exportacao_v1.R desde 22/09.
# IDS_ELEITOS e o universo antigo; IDS_OCUPANTE_SEM_MANDATO e definido pela MESMA regra que o verificador usa
# para achar orfao (ocupante identificado que nao resolve nem em pessoas.csv nem em pessoas_suplentes.csv), de
# modo que o universo exportado nasce sem o defeito, em vez de o verificador so flagrar depois.
IDS_ELEITOS <- unique(c(mand_v1$id_pessoa, pos_v1$id_pessoa))
pess_eleitos <- pess[id_pessoa %in% IDS_ELEITOS]

IDS_SUPLENTE <- unique(c(ocu_v1$id_pessoa, lsu_v1$id_pessoa, si_v1$id_pessoa))
psup_v1 <- psup[id_pessoa %in% IDS_SUPLENTE]

ocu_id_v1 <- ocu_v1[!is.na(id_pessoa) & id_pessoa != ""]
IDS_OCUPANTE_SEM_MANDATO <- setdiff(unique(ocu_id_v1$id_pessoa), union(pess_eleitos$id_pessoa, psup_v1$id_pessoa))

IDS_PESSOA <- unique(c(IDS_ELEITOS, IDS_OCUPANTE_SEM_MANDATO))
pess_v1 <- pess[id_pessoa %in% IDS_PESSOA]
checa_unica(as.data.frame(pess_v1), "id_pessoa")

# n_mandatos, primeiro_ano_eleito e ultimo_ano_eleito em pessoas.csv sao contados no banco inteiro, nunca
# recalculados para o recorte (a mesma semantica ja valia para os 2.155 da v1.0). Para quem entra como
# ocupante_sem_mandato_no_recorte os tres campos vem preenchidos, com n_mandatos >= 1: IDS_OCUPANTE_SEM_MANDATO
# exclui quem consta de pessoas_suplentes.csv, e pessoas_suplentes.csv reune quem nunca foi eleito em nenhuma
# esfera do banco. Sobram em pessoas.csv, portanto, os ocupantes eleitos noutra esfera fora do recorte (ex.:
# Assembleia), com o mandato fora do recorte. Os ocupantes que nunca foram eleitos (suplente_efetivado puro,
# suplente de senador, retotalizacao) resolvem so em pessoas_suplentes.csv. Correcao de 29/09/2026: o
# comentario anterior dizia que os campos "podem vir zerados", caso que nao ocorre (achado DADOS-02); a condicao
# abaixo marca quem e quem no recorte, e os registros mais adiante contam cada um dos dois grupos.
pess_v1[, condicao_no_recorte := fifelse(id_pessoa %in% IDS_ELEITOS, "eleito_no_recorte", "ocupante_sem_mandato_no_recorte")]

falta_pessoa <- setdiff(IDS_ELEITOS, pess_eleitos$id_pessoa)
if (length(falta_pessoa)) {
  stop(sprintf("R/64: %d id_pessoa de mandatos/posicoes_ano do recorte sem linha em pessoas exportada (ex.: %s)",
               length(falta_pessoa), paste(utils::head(falta_pessoa, 5), collapse = ", ")))
}
falta_ocupante <- setdiff(IDS_OCUPANTE_SEM_MANDATO, pess$id_pessoa)
if (length(falta_ocupante)) {
  warning(sprintf("R/64: %d ocupante do recorte sem linha em data/pessoas.csv, fica de fora do cadastro (nomeavel so se tiver linha em lista_suplencia.csv): %s",
                   length(falta_ocupante), paste(utils::head(falta_ocupante, 5), collapse = ", ")))
}

## ------------------------------------------------------------------------------------- DEF-01: exerceu_no_ano
# Auditoria de 24/09 (DEF-01): posicoes_ano.csv credita o titular eleito o intervalo mecanico inteiro
# (ano_eleicao+1 ate ano_eleicao+dur), sem ajuste para quem saiu antes do fim formal — README dava a entender
# que dava para "descobrir quem exerceu quando o titular renunciou/foi cassado" a partir dessa tabela, o que so
# vale de fato para ocupacoes.csv. Decisao de Igor (pendencia 18): entra uma coluna de exercicio por ano, e as
# linhas ficam todas (18092), sem remover nenhuma.
# Fonte da data efetiva: a ocupacao TITULAR do proprio mandato (ocu_v1, tipo_ocupante=="titular"), que ja
# reflete curadoria que mandatos.csv nao tem (ex.: OCU-07, Walter Pinheiro) — sempre existe uma por mandato do
# recorte (checado abaixo) e so falta data_fim para quem esta em_curso.
# nunca_exerceu (mandato inteiro fica FALSE, nao so o que vem depois de uma saida): forma_saida=="nao_tomou_posse"
# e o registro explicito de nunca ter tomado posse; oc_fim < oc_ini pega o caso adicional em que a saida
# efetiva (por fonte de saida, ex.: renuncia) aconteceu ANTES da propria data de inicio do mandato — Jean
# Wyllys (M2018_RJ_6_5005) e o caso PI 2022 renunciaram antes de tomar posse, mesmo com forma_saida="renuncia"
# em vez de "nao_tomou_posse". Isso NAO e o mesmo desenho ingenuo de DEF-02 (que trata todo mandato com
# data_posse desconhecida como nunca exercido so por faltar a data): dos 21 mandatos do recorte com
# data_posse=NA (pendencia 12), 4 tem forma_saida de saida em pleno exercicio (falecimento/cassacao/
# assumiu_titular) com data_fim_efetiva muito depois do inicio do mandato, ou seja, exerceram de verdade so
# sem data de posse capturada — para esses a ocupacao titular fornece oc_ini por convencao (igual a
# mandato_inicio) e o desenho abaixo credita exercicio normalmente ate a saida real.
tit_v1 <- ocu_v1[tipo_ocupante == "titular", .(id_mandato, oc_ini = as.IDate(data_inicio), oc_fim = as.IDate(data_fim))]
checa_unica(as.data.frame(tit_v1), "id_mandato")
faltam_titular <- setdiff(mand_v1$id_mandato, tit_v1$id_mandato)
if (length(faltam_titular)) {
  stop(sprintf("R/64: %d mandato do recorte sem ocupacao titular correspondente (ex.: %s)",
               length(faltam_titular), paste(utils::head(faltam_titular, 5), collapse = ", ")))
}
mand_ex <- merge(mand_v1[, .(id_mandato, forma_saida)], tit_v1, by = "id_mandato")
mand_ex[, nunca_exerceu := forma_saida == "nao_tomou_posse" | (!is.na(oc_fim) & oc_fim < oc_ini)]

n_pos_v1_antes <- nrow(pos_v1)
pos_v1[mand_ex, on = "id_mandato", `:=`(.oc_ini = i.oc_ini, .oc_fim = i.oc_fim, .nunca_exerceu = i.nunca_exerceu)]
pos_v1[, .ano_int := as.integer(ano)]
pos_v1[, exerceu_no_ano := fcase(
  .nunca_exerceu, FALSE,
  .ano_int < year(.oc_ini), FALSE,
  !is.na(.oc_fim) & .ano_int > year(.oc_fim), FALSE,
  !is.na(.oc_fim), TRUE,
  is.na(.oc_fim) & .ano_int <= year(REF), TRUE,        # em_curso, ano ja alcancado pela data de referencia
  is.na(.oc_fim) & .ano_int > year(REF), NA            # em_curso, ano futuro: ainda nao da para saber
)]
pos_v1[, c(".oc_ini", ".oc_fim", ".nunca_exerceu", ".ano_int") := NULL]
stopifnot(nrow(pos_v1) == n_pos_v1_antes)             # o join por atualizacao nao pode multiplicar linha
in_set(pos_v1$exerceu_no_ano, c(TRUE, FALSE), permitir_na = TRUE, nome = "pos_v1$exerceu_no_ano")

## ------------------------------------------------------------------------ filiacao (pessoas do recorte)
fil <- le("filiacoes.csv")
fil_v1 <- fil[id_pessoa %in% IDS_PESSOA]

## ------------------------------------------------------------------------------------------------ DOC-01
# Auditoria de 24/09 (DOC-01): filiacoes.csv pode ter a mesma tripla (id_pessoa, sigla_partido, data_filiacao)
# duas vezes quando ela vem tanto de fonte="lista_antiga" quanto de fonte="lista_atual" do TSE, com sg_uf e
# cd_municipio_tse diferentes entre as duas — artefato de fusao de fonte, nao corrupcao (a LINHA inteira e
# unica). Decisao de Igor: nao deduplicar, so documentar (LIVRO_DE_CODIGOS.md, fora deste script).
dup_fil <- fil_v1[, .N, by = .(id_pessoa, sigla_partido, data_filiacao)][N > 1]

## ------------------------------------------------------------------------------------------------- pendencia 12
# Auditoria de 24/09 (P12, "padrao Jean Wyllys"): mandatos do recorte com forma_saida preenchida mas
# data_posse=NA. Igor ja decidiu o caso do Jean (mantido, forma_saida="renuncia" preservada da fonte); sem
# regra geral, so registro do tamanho do conjunto para o livro de codigos.
p12_v1 <- mand_v1[!is.na(forma_saida) & forma_saida != "" & is.na(data_posse)]

## --------------------------------------------------------------------- dedup/auditoria (pessoas do recorte)
au  <- le("auditoria_homonimos.csv")
pfd <- le("pessoas_flags_dedup.csv")
au_v1  <- au[id_pessoa %in% IDS_PESSOA]
pfd_v1 <- pfd[id_pessoa %in% IDS_PESSOA]

## ------------------------------------------------------- fontes de posse, exercicio e saida do recorte
se   <- le("saida_executivos.csv")
slf  <- le("saida_legislativo_federal.csv")
ilf  <- le("interregnos_legislativo_federal.csv")
mfss <- le("mandatos_forma_saida_suplementar.csv")
es   <- le("eleicoes_suplementares.csv")
olf  <- le("ocupantes_legislativo_federal.csv")

se_v1   <- se[id_mandato %in% IDS_MANDATO]
slf_v1  <- slf[id_mandato %in% IDS_MANDATO]
ilf_v1  <- ilf[id_mandato %in% IDS_MANDATO]
mfss_v1 <- mfss[id_mandato_ordinario_afetado %in% IDS_MANDATO]
es_v1   <- es[cd_cargo %in% CARGOS_RECORTE]                                  # so sobram governador e senador
olf_v1  <- olf[cd_cargo %in% CARGOS_RECORTE]                                 # so tem camara e senado (5,6)
in_set(es_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "es_v1$cd_cargo")
in_set(olf_v1$cd_cargo, CARGOS_RECORTE, permitir_na = FALSE, nome = "olf_v1$cd_cargo")

# exercicio_camara.csv, exercicio_senado.csv, camara_biografia_eventos.csv e camara_biografia_posses.csv sao
# exclusivas da Camara e do Senado (cd_cargo 5 e 6, ja dentro do recorte): entram inteiras, sem filtro
exercicio_camara <- le("exercicio_camara.csv")
exercicio_senado <- le("exercicio_senado.csv")
cbio_eventos <- le("camara_biografia_eventos.csv")
cbio_posses  <- le("camara_biografia_posses.csv")

## ------------------------------------------------------- todo mandato encerrado do recorte tem saida
# 100% fechado e a meta declarada da v1.0 (decisao de 21/09/2026). O proprio R/10 ja classifica
# a ausencia de saida em motivo_sem_saida (nao_se_aplica = saida observada; em_curso = mandato_fim posterior
# a data de referencia, legitimo; fim_regular_presumido/fonte_ausente = lacuna genuina). No recorte da v1.0
# so podem sobrar nao_se_aplica e em_curso; qualquer outra coisa e um mandato encerrado sem forma de saida.
in_set(mand_v1$motivo_sem_saida, c("nao_se_aplica", "em_curso"), permitir_na = FALSE,
       nome = "mand_v1$motivo_sem_saida (recorte declarado 100% fechado)")

## -------------------------------------------------------------------------------------------------- grava
grava(mand_v1, "mandatos.csv", parquet = TRUE); grava(pos_v1, "posicoes_ano.csv", parquet = TRUE)
grava(pess_v1, "pessoas.csv", parquet = TRUE)
grava(fil_v1, "filiacoes.csv", parquet = TRUE)
grava(ocu_v1, "ocupacoes.csv", parquet = TRUE); grava(lsu_v1, "lista_suplencia.csv", parquet = TRUE)
grava(psup_v1, "pessoas_suplentes.csv", parquet = TRUE)
grava(si_v1, "suplentes_identidade.csv"); grava(ml_v1, "mandatos_lista.csv")
grava(se_v1, "saida_executivos.csv"); grava(slf_v1, "saida_legislativo_federal.csv")
grava(ilf_v1, "interregnos_legislativo_federal.csv"); grava(mfss_v1, "mandatos_forma_saida_suplementar.csv")
grava(es_v1, "eleicoes_suplementares.csv"); grava(olf_v1, "ocupantes_legislativo_federal.csv")
grava(exercicio_camara, "exercicio_camara.csv"); grava(exercicio_senado, "exercicio_senado.csv")
grava(cbio_eventos, "camara_biografia_eventos.csv"); grava(cbio_posses, "camara_biografia_posses.csv")
grava(au_v1, "auditoria_homonimos.csv"); grava(pfd_v1, "pessoas_flags_dedup.csv")

## --------------------------------------------------------------------------------- registro dos numeros
registrar_numero("v1_n_mandatos", nrow(mand_v1), script = script)
registrar_numero("v1_n_pessoas", nrow(pess_v1), script = script)
registrar_numero("v1_n_posicoes_ano", nrow(pos_v1), script = script)
registrar_numero("v1_n_ocupacoes", nrow(ocu_v1), script = script)
registrar_numero("v1_n_filiacoes", nrow(fil_v1), script = script)
n_pf <- uniqueN(fil_v1$id_pessoa)
registrar_numero("v1_pessoas_com_filiacao", n_pf, script = script)
registrar_numero("v1_pct_pessoas_com_filiacao", round(n_pf / nrow(pess_v1), 4), script = script)
registrar_numero("v1_n_lista_suplencia", nrow(lsu_v1), script = script)
registrar_numero("v1_n_pessoas_suplentes", nrow(psup_v1), script = script)
registrar_numero("v1_n_suplentes_identidade", nrow(si_v1), script = script)
registrar_numero("v1_n_mandatos_lista", nrow(ml_v1), script = script)
registrar_numero("v1_n_saida_executivos", nrow(se_v1), script = script)
registrar_numero("v1_n_saida_legislativo_federal", nrow(slf_v1), script = script)
registrar_numero("v1_n_interregnos_legislativo_federal", nrow(ilf_v1), script = script)
registrar_numero("v1_n_mandatos_forma_saida_suplementar", nrow(mfss_v1), script = script)
registrar_numero("v1_n_eleicoes_suplementares", nrow(es_v1), script = script)
registrar_numero("v1_n_ocupantes_legislativo_federal", nrow(olf_v1), script = script)
registrar_numero("v1_n_exercicio_camara", nrow(exercicio_camara), script = script)
registrar_numero("v1_n_exercicio_senado", nrow(exercicio_senado), script = script)
registrar_numero("v1_n_camara_biografia_eventos", nrow(cbio_eventos), script = script)
registrar_numero("v1_n_camara_biografia_posses", nrow(cbio_posses), script = script)
registrar_numero("v1_n_auditoria_homonimos", nrow(au_v1), script = script)
registrar_numero("v1_n_pessoas_flags_dedup", nrow(pfd_v1), script = script)

por_cargo <- mand_v1[, .N, by = cargo]
chave_cargo <- c(PRESIDENTE = "v1_n_mandatos_presidente", "VICE-PRESIDENTE" = "v1_n_mandatos_vice_presidente",
                 GOVERNADOR = "v1_n_mandatos_governador", "VICE-GOVERNADOR" = "v1_n_mandatos_vice_governador",
                 SENADOR = "v1_n_mandatos_senador", "DEPUTADO FEDERAL" = "v1_n_mandatos_deputado_federal")
for (cg in names(chave_cargo)) {
  registrar_numero(chave_cargo[[cg]], if (cg %in% por_cargo$cargo) por_cargo[cargo == cg, N] else 0L, script = script)
}
registrar_numero("v1_mandatos_com_saida_observada", mand_v1[motivo_sem_saida == "nao_se_aplica", .N], script = script)
registrar_numero("v1_mandatos_em_curso", mand_v1[motivo_sem_saida == "em_curso", .N], script = script)

# pendencia 17 (opcao A): pessoas por condicao no recorte
registrar_numero("v1_pessoas_eleitas_no_recorte", pess_v1[condicao_no_recorte == "eleito_no_recorte", .N], script = script)
registrar_numero("v1_pessoas_ocupantes_sem_mandato_no_recorte",
                 pess_v1[condicao_no_recorte == "ocupante_sem_mandato_no_recorte", .N], script = script)
registrar_numero("v1_ocupacoes_de_ocupante_sem_mandato",
                 ocu_id_v1[id_pessoa %in% IDS_OCUPANTE_SEM_MANDATO, .N], script = script)

# 29/09/2026 (DADOS-02, COB-03): o identificador de todo ocupante resolve em pessoas.csv OU em pessoas_suplentes.csv.
# Os ocupantes identificados que nao sao eleitos no recorte se dividem entre os dois arquivos, e as contagens abaixo
# saem dos mesmos vetores que definiram o universo; o n_mandatos >= 1 das pessoas ocupante_sem_mandato e conferido
# aqui e registrado, porque o livro de codigos passa a afirma-lo
IDS_OCUPANTE_NAO_ELEITO <- setdiff(unique(ocu_id_v1$id_pessoa), IDS_ELEITOS)
IDS_OCUPANTE_SO_PSUP    <- setdiff(IDS_OCUPANTE_NAO_ELEITO, IDS_OCUPANTE_SEM_MANDATO)
stopifnot(all(IDS_OCUPANTE_SO_PSUP %in% psup_v1$id_pessoa))
registrar_numero("v1_ocupantes_identificados", uniqueN(ocu_id_v1$id_pessoa), script = script)
registrar_numero("v1_ocupantes_identificados_nao_eleitos_no_recorte", length(IDS_OCUPANTE_NAO_ELEITO), script = script)
registrar_numero("v1_ocupantes_so_em_pessoas_suplentes", length(IDS_OCUPANTE_SO_PSUP), script = script)
registrar_numero("v1_ocupacoes_de_ocupante_so_em_pessoas_suplentes",
                 ocu_id_v1[id_pessoa %in% IDS_OCUPANTE_SO_PSUP, .N], script = script)
psm <- pess_v1[condicao_no_recorte == "ocupante_sem_mandato_no_recorte"]
registrar_numero("v1_ocupante_sem_mandato_com_n_mandatos_menor_que_1",
                 psm[is.na(suppressWarnings(as.integer(n_mandatos))) | suppressWarnings(as.integer(n_mandatos)) < 1L, .N], script = script)
registrar_numero("v1_ocupante_sem_mandato_com_primeiro_ano_eleito_na",
                 psm[is.na(primeiro_ano_eleito) | primeiro_ano_eleito == "", .N], script = script)

# DEF-01: exerceu_no_ano em posicoes_ano.csv, contagem por valor (linhas continuam 18092, nenhuma removida)
registrar_numero("v1_exerceu_no_ano_true", pos_v1[exerceu_no_ano == TRUE, .N], script = script)
registrar_numero("v1_exerceu_no_ano_false", pos_v1[exerceu_no_ano == FALSE, .N], script = script)
registrar_numero("v1_exerceu_no_ano_na", pos_v1[is.na(exerceu_no_ano), .N], script = script)
registrar_numero("v1_mandatos_nunca_exerceu", mand_ex[nunca_exerceu == TRUE, .N], script = script)
registrar_numero("v1_mandatos_com_alguma_linha_nao_exercida",
                 uniqueN(pos_v1[exerceu_no_ano == FALSE, id_mandato]), script = script)

# DOC-01: filiacao duplicada por fusao lista_antiga/lista_atual, documentada e nao deduplicada
registrar_numero("v1_doc01_triplas_filiacao_duplicadas", nrow(dup_fil), script = script)
registrar_numero("v1_doc01_linhas_filiacao_duplicadas", sum(dup_fil$N), script = script)
# 29/09/2026 (achado COB-06): a decisao de 24/09 falava em 17 triplas, contadas na copia da v1.0, que so tinha as
# pessoas eleitas. A v1.1 acrescenta os ocupantes sem mandato, e o total de 20 se reparte em 17 (eleitas) e 3 (ocupantes)
registrar_numero("v1_doc01_triplas_filiacao_duplicadas_pessoas_eleitas", dup_fil[id_pessoa %in% IDS_ELEITOS, .N], script = script)
registrar_numero("v1_doc01_triplas_filiacao_duplicadas_ocupantes_sem_mandato", dup_fil[!id_pessoa %in% IDS_ELEITOS, .N], script = script)

# pendencia 12: mandatos do recorte com forma_saida preenchida e data_posse desconhecida
registrar_numero("v1_p12_mandatos_sem_data_posse", nrow(p12_v1), script = script)

cat(sprintf("R/64: data_v1/ gravado - %d mandatos, %d pessoas (%d eleitos, %d ocupantes sem mandato), %d posicoes_ano, %d ocupacoes, %d filiacoes\n",
            nrow(mand_v1), nrow(pess_v1), pess_v1[condicao_no_recorte == "eleito_no_recorte", .N],
            pess_v1[condicao_no_recorte == "ocupante_sem_mandato_no_recorte", .N],
            nrow(pos_v1), nrow(ocu_v1), nrow(fil_v1)))
