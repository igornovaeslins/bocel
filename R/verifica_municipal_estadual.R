# verifica_municipal_estadual.R — recontagem consolidada das frentes municipal e estadual (SAPL
# municipal, Wikipedia estadual, Wikipedia prefeitos, DataJud) e da integracao em data/mandatos.csv.
#
# Historico. Escrito em 29/08/2026 para o relatorio RELATORIO_VERIFICACAO_MUNICIPAL_ESTADUAL.md, com a
# data de referencia fixa em 29/08; em 05/09/2026 generalizado: a data de referencia e a da execucao,
# toda checagem (inclusive as dos asserts de rigor, que antes abortavam o script) e acumulada em
# passou/falhou, o script abre log e grava o relatorio JSON padrao de verificacao. As chaves vme_* sao
# regravadas a cada execucao e a leitura correta e sempre a ultima linha de cada chave.
# Entrada: data/mandatos.csv, data/exercicio_camaras_municipais.csv, data/wikipedia_estadual.csv,
#          data/wikipedia_prefeitos.csv, data/datajud_*.csv, docs/LIVRO_DE_CODIGOS.md, docs/NOTA_DE_COBERTURA.md
# Saida:   logs/verifica_municipal_estadual.log, output/verificacao/relatorio_verificacao_<ts>.json, chaves vme_*
# Execucao: Rscript --vanilla R/verifica_municipal_estadual.R  (a partir da raiz do repositorio)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_municipal_estadual.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- "logs/verifica_municipal_estadual.log"; sink(logf, split = TRUE)
cat("verifica_municipal_estadual.R —", format(Sys.time()), "\n")
HOJE <- format(Sys.Date())   # data de referencia: a da execucao (era "2026-08-29" fixo ate 05/09/2026)
reg <- function(k, v) registrar_numero(paste0("vme_", k), v, script = script)
ok <- character(); falha <- character()
chk <- function(nome, cond) { if (isTRUE(cond)) ok <<- c(ok, nome) else falha <<- c(falha, nome); cat(if (isTRUE(cond)) "OK:" else "FALHA:", nome, "\n"); invisible(cond) }
# assert de rigor que aborta vira checagem registrada
chk_assert <- function(nome, expr) chk(nome, tryCatch({ force(expr); TRUE }, error = function(e) { cat("  ", conditionMessage(e), "\n"); FALSE }))

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("", "NA"))
sapl <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character", na.strings = c("", "NA"))
wpe  <- fread("data/wikipedia_estadual.csv", colClasses = "character", na.strings = c("", "NA"))
wpp  <- fread("data/wikipedia_prefeitos.csv", colClasses = "character", na.strings = c("", "NA"))
dj   <- fread("data/datajud_sinal_unidade_eleicao.csv", colClasses = "character", na.strings = c("", "NA"))
djp  <- fread("data/datajud_processos_cassacao.csv", colClasses = "character", na.strings = c("", "NA"),
              select = c("tribunal", "numero_processo"))

# chaves declaradas no livro
chk_assert("mandatos: id_mandato unico", checa_unica(as.data.frame(mand), "id_mandato"))
chk_assert("sapl: chave dominio x id_mandato_sapl unica", checa_unica(as.data.frame(sapl), c("dominio", "id_mandato_sapl")))
chk_assert("wpp: chave url x nome_normalizado x inicio unica", checa_unica(as.data.frame(wpp), c("url", "nome_normalizado", "inicio")))
chk_assert("dj: chave uf x sg_ue x cargo_assunto x eleicao_ref unica", checa_unica(as.data.frame(dj), c("uf", "sg_ue", "cargo_assunto", "eleicao_ref")))
chk_assert("djp: chave tribunal x numero_processo unica", checa_unica(as.data.frame(djp), c("tribunal", "numero_processo")))
voc_fs <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
            "nao_tomou_posse", "outro", "nao_observado", "substituicao_inferida_munic", "aposentadoria", "impeachment", "retotalizacao",
            "suplente_efetivado", "perda_por_suplementar")
chk_assert("sapl forma_saida no vocabulario", in_set(sapl$forma_saida, voc_fs, nome = "sapl forma_saida"))
chk_assert("wpe forma_saida no vocabulario", in_set(wpe$forma_saida, voc_fs, nome = "wpe forma_saida"))
chk_assert("wpp forma_saida no vocabulario", in_set(wpp$forma_saida, voc_fs, nome = "wpp forma_saida"))
chk_assert("sapl ano_eleicao_bocel em 1996..2024", em_faixa(as.integer(sapl$ano_eleicao_bocel), 1996, 2024, nome = "sapl ano_eleicao_bocel"))
chk_assert("wpe ano_eleicao_bocel em 1994..2022", em_faixa(as.integer(wpe$ano_eleicao_bocel), 1994, 2022, nome = "wpe ano_eleicao_bocel"))
chk_assert("dj eleicao_ref em 2002..2026", em_faixa(as.integer(dj$eleicao_ref), 2002, 2026, nome = "dj eleicao_ref"))

# pareados -> mandatos: many-to-one sem perda
par_ok <- function(x, nome) {
  p <- unique(x[!is.na(id_mandato_bocel), .(id_mandato_bocel)])
  j <- tryCatch(join_seguro(as.data.frame(p), as.data.frame(mand[, .(id_mandato_bocel = id_mandato, cd_cargo, ano_eleicao)]),
                            by = "id_mandato_bocel", cardinalidade = "one-to-one"),
                error = function(e) { cat("  ", conditionMessage(e), "\n"); p[0] })
  chk(paste(nome, "todo id_mandato_bocel existe em mandatos"), nrow(j) == nrow(p))
  nrow(p)
}
n_sapl_par <- par_ok(sapl, "sapl"); n_wpe_par <- par_ok(wpe, "wpe"); n_wpp_par <- par_ok(wpp, "wpp")
chk("sapl pareados sao vereador (13)", all(mand[id_mandato %in% sapl$id_mandato_bocel, cd_cargo] == "13"))
chk("wpp pareados sao prefeito/vice (11/12)", all(mand[id_mandato %in% wpp$id_mandato_bocel, cd_cargo] %in% c("11", "12")))
chk("wpe pareados sao 3/4/7/8", all(mand[id_mandato %in% wpe$id_mandato_bocel, cd_cargo] %in% c("3", "4", "7", "8")))

reg("sapl_n_linhas", nrow(sapl)); reg("sapl_n_pareados", n_sapl_par)
reg("sapl_taxa_2000_2024", round(n_sapl_par / mand[cd_cargo == "13" & ano_eleicao >= "2000", .N], 4))
reg("sapl_n_ano_fora_ciclo", sapl[(as.integer(ano_eleicao_bocel) %% 4) != 0, .N])
reg("sapl_n_fim_antes_inicio", sapl[!is.na(data_fim_mandato) & !is.na(data_inicio_mandato) & data_fim_mandato < data_inicio_mandato, .N])
regra <- sapl[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(metodo_pareamento)]
for (i in seq_len(nrow(regra))) reg(paste0("sapl_regra_", gsub("[^a-z0-9]", "_", regra$metodo_pareamento[i])), regra$N[i])
reg("sapl_n_titular_false_pareado", sapl[titular %in% c("FALSE", "False", "false") & !is.na(id_mandato_bocel), .N])
reg("sapl_n_tipo_afastamento_sem_forma_mapeada",
    sapl[!is.na(tipo_afastamento) & forma_saida %in% c("fim_regular", "outro"), .N])

reg("wpe_n_linhas", nrow(wpe)); reg("wpe_n_pareados", n_wpe_par)
for (cg in unique(wpe$cargo)) reg(paste0("wpe_pareados_", gsub("[^a-z]", "_", tolower(cg))),
                                  wpe[cargo == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
reg("wpe_n_substituto_pareado", wpe[condicao == "substituto" & !is.na(id_mandato_bocel), .N])
reg("wpe_n_gov_bocel_sem_par", mand[cd_cargo == "3" & !(id_mandato %in% wpe$id_mandato_bocel), .N])
reg("wpe_n_vice_bocel_sem_par", mand[cd_cargo == "4" & !(id_mandato %in% wpe$id_mandato_bocel), .N])

reg("wpp_n_linhas", nrow(wpp)); reg("wpp_n_pareados", n_wpp_par)
reg("wpp_n_pareados_prefeito", mand[id_mandato %in% wpp$id_mandato_bocel & cd_cargo == "11", .N])
reg("wpp_n_pareados_vice", mand[id_mandato %in% wpp$id_mandato_bocel & cd_cargo == "12", .N])
reg("wpp_n_com_forma", wpp[!is.na(forma_saida) & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
reg("wpp_n_paginas", uniqueN(wpp$url))
reg("wpp_n_fim_futuro", wpp[!is.na(fim) & fim > HOJE, .N])

reg("dj_n_sinal_linhas", nrow(dj)); reg("dj_n_processos", nrow(djp))
reg("dj_n_sinal_eleicao_ref_impar", dj[as.integer(eleicao_ref) %% 2 == 1, .N])
for (a in sort(unique(dj$eleicao_ref))) reg(paste0("dj_sinal_", a), dj[eleicao_ref == a, .N])
reg("dj_n_sinal_sg_ue_NA", dj[is.na(sg_ue), .N])
chk("dj eleicao_ref sempre par", dj[as.integer(eleicao_ref) %% 2 == 1, .N] == 0)

# integracao em mandatos.csv
fte <- mand[!is.na(fonte_forma_saida), .N, by = .(esfera, fonte_forma_saida)]
for (i in seq_len(nrow(fte))) reg(paste0("fonte_", fte$esfera[i], "_", fte$fonte_forma_saida[i]), fte$N[i])
fs <- mand[esfera == "municipal", .N, by = forma_saida]
for (i in seq_len(nrow(fs))) reg(paste0("forma_municipal_", fs$forma_saida[i]), fs$N[i])
reg("mandatos_forma_observada", mand[forma_saida != "nao_observado", .N])
reg("mandatos_com_data_posse", mand[!is.na(data_posse), .N])
reg("mandatos_em_curso_com_fim_regular",
    mand[mandato_fim > HOJE & forma_saida == "fim_regular", .N])
reg("mandatos_fim_efetiva_futura", mand[!is.na(data_fim_efetiva) & data_fim_efetiva > HOJE, .N])
reg("munic_antecipada_igual_convencional",
    mand[forma_saida == "substituicao_inferida_munic" & data_fim_efetiva == mandato_fim, .N])
chk("nenhum mandato em curso com fim_regular", mand[mandato_fim > HOJE & forma_saida == "fim_regular", .N] == 0)
chk("nenhuma data_fim_efetiva futura", mand[!is.na(data_fim_efetiva) & data_fim_efetiva > HOJE, .N] == 0)
chk("wikipedia prefeitos integrados <= pareados",
    mand[esfera == "municipal" & fonte_forma_saida == "wikipedia", .N] <= n_wpp_par)
chk("sapl integrados <= pareados", mand[fonte_forma_saida == "sapl_municipal", .N] <= n_sapl_par)

# docs citam os totais de linhas correntes
livro <- readLines("docs/LIVRO_DE_CODIGOS.md"); nota <- readLines("docs/NOTA_DE_COBERTURA.md")
fmt <- function(n) format(n, big.mark = ".", decimal.mark = ",")
chk("livro cita n linhas sapl", any(grepl(paste0("exercicio_camaras_municipais.csv.*", fmt(nrow(sapl)), " linhas"), livro)))
chk("livro cita n linhas wikipedia_estadual", any(grepl(paste0("wikipedia_estadual.csv.*", fmt(nrow(wpe)), " linhas"), livro)))
chk("livro cita n linhas wikipedia_prefeitos", any(grepl(paste0("wikipedia_prefeitos.csv.*", fmt(nrow(wpp)), " linhas"), livro)))
chk("livro cita n linhas datajud_sinal", any(grepl(paste0("datajud_sinal_unidade_eleicao.csv.*", fmt(nrow(dj)), " linhas"), livro)))
chk("nota cita sapl pareados", any(grepl(fmt(n_sapl_par), nota)))
chk("nota cita wikipedia prefeitos pareados", any(grepl(fmt(n_wpp_par), nota)))
n_sapl_int <- mand[fonte_forma_saida == "sapl_municipal", .N]
chk("nota cita sapl_municipal integrados", any(grepl(paste0("sapl_municipal.*", fmt(n_sapl_int)), nota)))
n_wpp_int <- mand[esfera == "municipal" & fonte_forma_saida == "wikipedia", .N]
chk("nota cita wikipedia municipal integrados", any(grepl(paste0("municipal \\| wikipedia.*", fmt(n_wpp_int)), nota)))
# fontes de R/10 nao mais novas que mandatos.csv
fontes <- c("data/exercicio_camaras_municipais.csv", "data/wikipedia_estadual.csv", "data/wikipedia_prefeitos.csv")
chk("fontes 15-17 anteriores a mandatos.csv", all(file.mtime(fontes) < file.mtime("data/mandatos.csv")))
chk("docs posteriores a mandatos.csv", all(file.mtime(c("docs/LIVRO_DE_CODIGOS.md", "docs/NOTA_DE_COBERTURA.md")) > file.mtime("data/mandatos.csv")))

reg("n_checks_passaram", length(ok)); reg("n_checks_falharam", length(falha))
gravar_relatorio_verificacao(alvo = "data/mandatos.csv + data/exercicio_camaras_municipais.csv + data/wikipedia_estadual.csv + data/wikipedia_prefeitos.csv + data/datajud_sinal_unidade_eleicao.csv",
                             script = script, passou = ok, falhou = falha,
                             fora_de_cobertura = c("veracidade das datas e causas de cada fonte: fica com o verificador da frente (verifica_sapl_municipal, verifica_wikipedia_estadual, verifica_wikipedia_prefeitos, verifica_datajud)",
                                                   "a data de referencia dos checks de 'futuro' e a da execucao; um mandato em curso so e futuro em relacao a ela"))
cat("\nPASSOU:", length(ok), "| FALHOU:", length(falha), "\n"); if (length(falha)) cat("- ", falha, sep = "\n- ")
if (length(falha)) { sink(); quit(status = 1) }
sink()
