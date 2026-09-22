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
IDS_PESSOA <- unique(c(mand_v1$id_pessoa, pos_v1$id_pessoa))
pess_v1 <- pess[id_pessoa %in% IDS_PESSOA]
checa_unica(as.data.frame(pess_v1), "id_pessoa")
falta_pessoa <- setdiff(IDS_PESSOA, pess_v1$id_pessoa)
if (length(falta_pessoa)) {
  stop(sprintf("R/64: %d id_pessoa de mandatos/posicoes_ano do recorte sem linha em pessoas exportada (ex.: %s)",
               length(falta_pessoa), paste(utils::head(falta_pessoa, 5), collapse = ", ")))
}

IDS_SUPLENTE <- unique(c(ocu_v1$id_pessoa, lsu_v1$id_pessoa, si_v1$id_pessoa))
psup_v1 <- psup[id_pessoa %in% IDS_SUPLENTE]

## ------------------------------------------------------------------------ filiacao (pessoas do recorte)
fil <- le("filiacoes.csv")
fil_v1 <- fil[id_pessoa %in% IDS_PESSOA]

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

cat(sprintf("R/64: data_v1/ gravado - %d mandatos, %d pessoas, %d posicoes_ano, %d ocupacoes, %d filiacoes\n",
            nrow(mand_v1), nrow(pess_v1), nrow(pos_v1), nrow(ocu_v1), nrow(fil_v1)))
