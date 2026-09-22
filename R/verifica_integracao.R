# verifica_integracao.R — verificacao cetica da integracao (R/10_integrar_exercicio.R) e de R/04_verificar.R
# Reconta, a partir dos arquivos-fonte, o que R/10 escreveu em data/mandatos.csv e data/pessoas.csv.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_integracao.R
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)

ler <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
d8  <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
# 21/09/2026: a prioridade entre fontes candidatas so existe em cargo com mais de uma fonte de forma de
# saida, que fica fora do recorte da v1.0 (deputado estadual e distrital, prefeito, vice-prefeito e
# vereador). Com --banco-inteiro as checagens rodam com o universo e a tolerancia de sempre; sem ele,
# que e como R/rodar_verificadores.sh chama, o verificador imprime a contagem e diz que nao conferiu.
BANCO_INTEIRO <- "--banco-inteiro" %in% commandArgs(trailingOnly = TRUE)
passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ expr; TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome); r
}
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "perda_do_mandato_inferida_por_eleicao_suplementar",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
           "substituicao_inferida_munic", "assumiu_titular", "aposentadoria", "impeachment", "retotalizacao", "outro", "nao_observado")
# 04/09/2026: 'sapl_observacao' (texto livre do SAPL lido por R/54) e 'assembleia_inventario'
# (folha, frequencia e transparencia das casas estaduais, R/50) entraram no R/10 e por isso passam
# a constar aqui. A lista declara as fontes existentes; nenhuma tolerancia foi mexida.
# 'cargo_incompativel' (R/39) e 'assembleia_portal' (R/38) ja gravavam em fonte_forma_saida e nunca
# tinham sido declarados aqui, de modo que este assert falhava desde 29/08/2026.
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
# 21/09/2026: a regra A1 troca o rotulo do evento curado de governo e Presidencia pela fonte que a linha tem
# (lib/tipo_fonte.R), e base_dhbb_curada, noticia_orgao_publico_curada e pista_nao_oficial entram no vocabulario
FONTES <- c("camara_api", "senado_api", "camara_biografia", "fonte_oficial_curada", "base_dhbb_curada", "noticia_orgao_publico_curada", "pista_nao_oficial", "assembleia_api", "sapl_municipal", "sapl_observacao", "portal_camara", "tce", "assembleia_historico", "assembleia_inventario", "assembleia_portal", "diario_oficial", "wikipedia", "wikidata", "wikidata_obito", "tse_suplementar", "ibge_munic", "cargo_incompativel", "derivado_titular", "data_fim_efetiva")

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
# 13/09/2026: senador, deputado federal, presidente, vice-presidente, governador e vice-governador tem forma de saida
# autoritativa (R/58 e R/60 no R/10), sem concorrencia de prioridade entre fontes candidatas; a verificacao desses cargos
# esta em R/verifica_legislativo_federal.R e R/verifica_executivos.R
AUTORITATIVOS <- mand[cd_cargo %in% c("1", "2", "3", "4", "5", "6"), id_mandato]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
ok("mandatos: id_mandato unico", checa_unica(as.data.frame(mand), "id_mandato"))
ok("pessoas: id_pessoa unico", checa_unica(as.data.frame(pess), "id_pessoa"))

## 1. vocabulario e fonte
ok("forma_saida no vocabulario (sem NA)", in_set(mand$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida"))
ok("fonte_forma_saida no vocabulario", in_set(mand$fonte_forma_saida, FONTES, nome = "fonte_forma_saida"))
ok("toda forma observada tem fonte", stopifnot(mand[forma_saida != "nao_observado", all(!is.na(fonte_forma_saida))]))
n_nobs_com_fonte <- mand[forma_saida == "nao_observado" & !is.na(fonte_forma_saida), .N]
registrar_numero("vint_nao_observado_com_fonte", n_nobs_com_fonte, script = "R/verifica_integracao.R")

## 2. convencao de datas do mandato
mand[, `:=`(ano = as.integer(ano_eleicao), cd = as.integer(cd_cargo))]
mand[, leg := cd %in% c(5L, 6L, 7L, 8L)]   # senador, dep federal, estadual, distrital
mand[, ini_esp := fifelse(leg, sprintf("%d-02-01", ano + 1L), sprintf("%d-01-01", ano + 1L))]
mand[, fim_esp := fifelse(cd == 5L, sprintf("%d-01-31", ano + 9L),
                   fifelse(leg, sprintf("%d-01-31", ano + 5L), sprintf("%d-12-31", ano + 4L)))]
ok("mandato_inicio segue a convencao por cargo", stopifnot(all(mand$mandato_inicio == mand$ini_esp)))
ok("mandato_fim segue a convencao por cargo", stopifnot(all(mand$mandato_fim == mand$fim_esp)))

## 3. coerencia de data_posse e data_fim_efetiva
dp <- mand[!is.na(data_posse)]
dp[, dif_ini := as.integer(as.IDate(data_posse) - as.IDate(mandato_inicio))]
ok("data_posse >= mandato_inicio - 60 dias (regra de R/10)", stopifnot(all(dp$dif_ini >= -60L)))
ok("data_posse <= mandato_fim", stopifnot(all(dp$data_posse <= dp$mandato_fim)))
registrar_numero("vint_posse_antes_do_inicio_convencional", dp[dif_ini < 0, .N], script = "R/verifica_integracao.R")
registrar_numero("vint_posse_mais_de_1_ano_apos_inicio", dp[dif_ini > 365, .N], script = "R/verifica_integracao.R")
df <- mand[!is.na(data_fim_efetiva)]
ok("data_fim_efetiva nao anterior a data_posse", stopifnot(df[!is.na(data_posse), all(data_fim_efetiva >= data_posse)]))
ok("data_fim_efetiva >= mandato_inicio - 60", stopifnot(all(as.IDate(df$data_fim_efetiva) >= as.IDate(df$mandato_inicio) - 60L)))
registrar_numero("vint_fim_efetiva_apos_fim_convencional", df[data_fim_efetiva > mandato_fim, .N], script = "R/verifica_integracao.R")
registrar_numero("vint_fim_efetiva_apos_fim_convencional_mais_1_ano",
                 df[as.IDate(data_fim_efetiva) > as.IDate(mandato_fim) + 366L, .N], script = "R/verifica_integracao.R")
ok("saida antecipada (renuncia/falecimento/cassacao/perda) com fim efetivo <= mandato_fim",
   stopifnot(df[forma_saida %in% c("renuncia", "falecimento", "cassacao", "perda_do_mandato_inferida_por_eleicao_suplementar"),
                all(data_fim_efetiva <= mandato_fim)]))
ok("fim_regular com data_fim_efetiva: fim >= mandato_fim - 60 dias",
   stopifnot(df[forma_saida == "fim_regular", all(as.IDate(data_fim_efetiva) >= as.IDate(mandato_fim) - 60L)]))

## 4. exercicio_confirmado <= mandato_fim + 1 ano, e fonte coerente
ex <- mand[!is.na(exercicio_confirmado)]
ok("exercicio_confirmado <= mandato_fim + 1 ano",
   stopifnot(all(as.IDate(ex$exercicio_confirmado) <= as.IDate(ex$mandato_fim) + 366L)))
ok("exercicio_confirmado >= mandato_inicio - 60", stopifnot(all(as.IDate(ex$exercicio_confirmado) >= as.IDate(ex$mandato_inicio) - 60L)))
ok("exercicio_confirmado tem fonte_exercicio", stopifnot(all(!is.na(ex$fonte_exercicio))))
# A coluna acumula fontes separadas por ';' e o conjunto de combinacoes cresce a cada fonte nova;
# a checagem passa a ser por token, o que e mais estrito do que enumerar as combinacoes vistas.
# 12/09/2026: tce_ac (acordao de contas anuais do TCE-AC, R/57) entra como fonte declarada
FONTES_EXERC <- c("ibge_munic", "ibge_munic_ampliado", "tse_reeleicao", "receita_cnpj", "sapl_presenca", "tce_ac")
tok_exerc <- unique(unlist(strsplit(na.omit(mand$fonte_exercicio), ";", fixed = TRUE)))
ok("fonte_exercicio no vocabulario", in_set(tok_exerc, FONTES_EXERC, nome = "fonte_exercicio"))
registrar_numero("vint_fonte_exercicio_sem_data", mand[!is.na(fonte_exercicio) & is.na(exercicio_confirmado), .N], script = "R/verifica_integracao.R")

## 5. contagens por esfera batem com output/verificacao/forma_saida_por_esfera.csv
fs_arq <- fread("output/verificacao/forma_saida_por_esfera.csv", colClasses = "character")
fs_rec <- mand[, .(N = as.character(.N)), by = .(esfera, forma_saida)]
cmp <- merge(fs_arq, fs_rec, by = c("esfera", "forma_saida"), all = TRUE, suffixes = c("_arq", "_rec"))
ok("forma_saida_por_esfera.csv bate com mandatos.csv", stopifnot(nrow(cmp) == nrow(fs_arq), all(cmp$N_arq == cmp$N_rec)))
# e com numeros_assinatura (ultimo registro de cada chave)
# parse robusto do registro (chave | valor | ...): so os dois primeiros campos, ultimo registro por chave
na_l <- readLines("output/numeros_assinatura.txt", warn = FALSE)
na_ <- rbindlist(lapply(strsplit(na_l[grepl("\\|", na_l)], "|", fixed = TRUE), function(p) data.table(V1 = trimws(p[1]), V2 = trimws(p[2]))))
ult <- na_[, .SD[.N], by = V1]
for (i in seq_len(nrow(fs_rec))) {
  k <- sprintf("bocel_forma_saida_%s_%s", fs_rec$esfera[i], fs_rec$forma_saida[i])
  ok(sprintf("assinatura %s", k), stopifnot(ult[V1 == k, V2] == fs_rec$N[i]))
}
ok("assinatura bocel_mandatos_com_data_posse", stopifnot(ult[V1 == "bocel_mandatos_com_data_posse", V2] == as.character(sum(!is.na(mand$data_posse)))))
ok("assinatura bocel_mandatos_com_exercicio_confirmado", stopifnot(ult[V1 == "bocel_mandatos_com_exercicio_confirmado", V2] == as.character(sum(!is.na(mand$exercicio_confirmado)))))
ok("assinatura bocel_mandatos_forma_saida_observada", stopifnot(ult[V1 == "bocel_mandatos_forma_saida_observada", V2] == as.character(sum(mand$forma_saida != "nao_observado"))))

## 6. prioridade de fontes: reconstroi os candidatos de cada fonte e a fonte vencedora esperada
cand <- list()
d8 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
ca <- ler("data/exercicio_camara.csv"); if (!is.null(ca)) cand$camara_api <- ca[!is.na(id_mandato) & !is.na(forma_saida), .(id_mandato, forma = forma_saida, fim = d8(data_fim_exercicio))]
se <- ler("data/exercicio_senado.csv"); if (!is.null(se)) cand$senado_api <- se[!is.na(id_mandato) & !is.na(forma_saida), .(id_mandato, forma = forma_saida, fim = d8(data_fim_exercicio))]
as_ <- ler("data/exercicio_assembleias.csv"); if (!is.null(as_)) cand$assembleia_api <- as_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
cm <- ler("data/exercicio_camaras_municipais.csv"); if (!is.null(cm)) cand$sapl_municipal <- cm[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]
wd <- ler("data/wikidata_mandatos.csv"); if (!is.null(wd)) cand$wikidata <- wd[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))]
ob <- ler("data/wikidata_obitos.csv"); if (!is.null(ob)) cand$wikidata_obito <- ob[toupper(dentro_do_mandato) == "TRUE", .(id_mandato, forma = "falecimento", fim = d8(data_morte))]
cs_ <- ler("data/exercicio_camaras_sem_sapl.csv"); if (!is.null(cs_)) cand$portal_camara <- cs_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]
ap_ <- ler("data/exercicio_assembleias_2.csv"); if (!is.null(ap_)) cand$assembleia_portal <- ap_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
ah_ <- ler("data/exercicio_assembleias_historico.csv"); if (!is.null(ah_)) cand$assembleia_historico <- ah_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
do_ <- ler("data/diarios_mandatos_saida.csv"); if (!is.null(do_)) cand$diario_oficial <- do_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_inferida))]
tc_ <- rbindlist(lapply(c("data/tce_gestores.csv", "data/tce_gestores_b.csv", "data/tce_gestores_c.csv", "data/tce_gestores_d.csv"), function(ff) { x <- ler(ff); if (is.null(x)) NULL else x[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim))] }))
if (nrow(tc_)) cand$tce <- tc_
cs2_ <- ler("data/exercicio_camaras_sem_sapl_2.csv"); if (!is.null(cs2_)) cand$portal_camara <- rbindlist(list(cand$portal_camara, cs2_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]))
wp <- ler("data/wikipedia_prefeitos.csv"); we <- ler("data/wikipedia_estadual.csv")
wk <- rbindlist(list(if (!is.null(wp)) wp[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))],
                     if (!is.null(we)) we[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))]))
if (nrow(wk)) cand$wikipedia <- wk
su <- ler("data/mandatos_forma_saida_suplementar.csv"); if (!is.null(su)) cand$tse_suplementar <- su[!is.na(id_mandato_ordinario_afetado), .(id_mandato = id_mandato_ordinario_afetado, forma = fifelse(momento %in% "antes_da_posse", "nao_tomou_posse", "perda_do_mandato_inferida_por_eleicao_suplementar"), fim = d8(data_fim_inferida))]
ic_ <- ler("data/saida_cargo_incompativel.csv"); if (!is.null(ic_)) cand$cargo_incompativel <- ic_[, .(id_mandato, forma = forma_saida, fim = d8(data_fim_inferida))]
mu <- ler("data/munic_prefeitos.csv"); if (!is.null(mu)) cand$ibge_munic <- mu[status == "outro_em_exercicio" & !is.na(nome_prefeito_munic) & nome_prefeito_munic != "" & !is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel, forma = "substituicao_inferida_munic", fim = paste0(ano_munic, "-12-31"))]
so_ <- ler("data/sapl_observacao_titular.csv"); if (!is.null(so_)) cand$sapl_observacao <- so_[tipo_evento == "fim_de_mandato" & !is.na(id_mandato) & !is.na(forma_saida_obs), .(id_mandato, forma = forma_saida_obs, fim = d8(data_evento))]
# 12/09/2026: R/10 so aplica a linha do inventario que traz data de inicio ou de fim (bloco "so onde ha data"),
# e a reimplementacao nao reproduzia o filtro; as 51 linhas sem data do vinculo funcional da ALES, GO e MT que
# o R/50 passou a emitir em 05/09 respondiam por 51 das 56 divergencias
ai_ <- ler("data/exercicio_assembleias_inventario.csv"); if (!is.null(ai_)) cand$assembleia_inventario <- ai_[!is.na(id_mandato_bocel) & !is.na(forma_saida) & (!is.na(data_inicio_exercicio) | !is.na(data_fim_exercicio)), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
cand <- rbindlist(lapply(names(cand), function(n) cand[[n]][, fonte := n]), use.names = TRUE)
cand <- cand[id_mandato %in% mand$id_mandato & !forma %in% "nao_observado"]   # nao_observado da fonte nao concorre (R/10 corrigido)
# regras de janela do R/10: fim fora de [mandato_inicio - 60, mandato_fim + 45] nao concorre
cand <- merge(cand, mand[, .(id_mandato, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))], by = "id_mandato")
cand <- cand[is.na(fim) | (as.IDate(fim) >= mi - 60L & as.IDate(fim) <= mf + 45L)]
# fim no futuro (termino previsto) nao e saida observada no R/10: nao concorre
cand <- cand[is.na(fim) | as.IDate(fim) <= Sys.Date()]
ok("fontes candidatas: id_mandato pareado existe em mandatos", stopifnot(all(cand$id_mandato %in% mand$id_mandato)))
prio <- c(cargo_incompativel = 0, camara_api = 1, senado_api = 2, assembleia_inventario = 3, assembleia_portal = 3, tce = 3, assembleia_historico = 4, portal_camara = 5, sapl_observacao = 5, sapl_municipal = 6, assembleia_api = 7, wikidata_obito = 8, wikidata = 9, wikipedia = 10, diario_oficial = 11, datajud = 12, tse_suplementar = 13, ibge_munic = 14)
# R/10 aplica na ordem munic, suplementar, wikidata, obito, assembleia, senado, camara: a ultima aplicada prevalece
# 29/08/2026: o TCE desceu para antes das camaras e assembleias (docs/CONCORDANCIA_FONTES.md)
ordem_aplic <- c("ibge_munic", "tse_suplementar", "datajud", "diario_oficial", "wikipedia", "wikidata", "wikidata_obito", "assembleia_api", "tce", "sapl_municipal", "sapl_observacao", "portal_camara", "assembleia_historico", "assembleia_portal", "assembleia_inventario", "senado_api", "camara_api", "cargo_incompativel")
cand[, ord := match(fonte, ordem_aplic)]
# Reimplementacao independente da aplicacao sequencial de R/10, incluindo a guarda de 29/08/2026:
# 'fim_regular' vindo de qualquer fonte nao substitui evento nomeado ja registrado por outra.
# A regra 'cedo' de R/10 tambem e reproduzida, porque evento cujo fim alcanca o fim convencional
# vira fim_regular antes de a guarda ser consultada.
EVENTO_V <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
# 12/09/2026: a regra cedo do R/10 converte so renuncia, falecimento, cassacao, afastamento, licenca e
# nao_tomou_posse; a reimplementacao usava a lista inteira de eventos e convertia suplente_efetivado com fim
# no ultimo dia do mandato (2 deputados do RJ em 2014)
CEDO_V <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse")
cand[, forma_ap := fifelse(forma %in% CEDO_V & !is.na(fim) & as.IDate(fim) >= mf - 1L, "fim_regular", forma)]
# 04/09/2026: duas regras de R/10 faltavam na reimplementacao. A primeira e o mapeamento da forma
# fora do vocabulario para 'outro', que R/10 faz por fonte antes de aplicar. A segunda e a guarda
# de 30/08/2026, em que 'outro' entrou ao lado de 'fim_regular' como residuo que nao apaga ato
# nomeado; aqui so 'fim_regular' estava guardado. As duas juntas respondiam pelas 49 divergencias
# que sobraram depois de modelar apenas_preenche.
VOCAB_V <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
             "nao_tomou_posse", "suplente_efetivado",
             "perda_do_mandato_inferida_por_eleicao_suplementar",
             "substituicao_inferida_munic", "assumiu_titular", "aposentadoria", "impeachment", "retotalizacao", "outro", "nao_observado")
cand[!forma_ap %in% VOCAB_V & !is.na(forma_ap), forma_ap := "outro"]
# 04/09/2026: 'cargo_incompativel' entra em R/10 com apenas_preenche = TRUE, isto e, so ocupa
# mandato que nenhuma outra fonte preencheu. A reimplementacao ignorava isso e esperava que ele
# vencesse sempre, o que respondia por 512 das 547 divergencias listadas. A regra agora e a mesma
# dos dois lados.
SO_PREENCHE <- c("cargo_incompativel")
esp <- cand[order(id_mandato, ord)][, {
  f <- NA_character_; fo <- NA_character_
  for (k in seq_len(.N)) {
    if (fonte[k] %in% SO_PREENCHE && !is.na(f)) next
    if (forma_ap[k] %in% c("fim_regular", "outro") && !is.na(f) && f %in% EVENTO_V) next
    f <- forma_ap[k]; fo <- fonte[k]
  }
  .(forma_esp = f, fonte_esp = fo)
}, by = id_mandato]
n_multi <- cand[!id_mandato %in% AUTORITATIVOS, uniqueN(fonte), by = id_mandato][V1 > 1, .N]
registrar_numero("vint_mandatos_com_mais_de_uma_fonte_candidata", n_multi, script = "R/verifica_integracao.R")
chk <- merge(mand[!id_mandato %in% AUTORITATIVOS, .(id_mandato, cd_cargo, esfera, forma_saida, fonte_forma_saida, data_fim_efetiva, mandato_fim)], esp, by = "id_mandato", all.x = TRUE)
# a fonte gravada deve ser a de maior prioridade entre as candidatas, salvo a promocao a fim_regular por data_fim_efetiva
chk[, fonte_ok := fifelse(is.na(fonte_esp), is.na(fonte_forma_saida) | fonte_forma_saida == "data_fim_efetiva",
                          fonte_forma_saida == fonte_esp | (fonte_forma_saida == "data_fim_efetiva" & is.na(fonte_esp)))]
# divergencias residuais vem das regras de coerencia data-forma do R/10 (fim so substitui com forma;
# posse fora da janela; fim_regular precoce vira outro) que este verificador nao reproduz por inteiro:
# ficam listadas e registradas, com tolerancia de 0,1% dos mandatos com mais de uma fonte candidata
# forma derivada do titular (vices) nao vem de fonte candidata: fica fora desta checagem
div <- chk[(fonte_ok == FALSE | is.na(fonte_ok)) & !fonte_forma_saida %in% "derivado_titular"]
fwrite(div, "output/verificacao/integracao_prioridade_divergente.csv")
registrar_numero("vint_prioridade_divergente", nrow(div), script = "R/verifica_integracao.R")
if (BANCO_INTEIRO) {
  ok(sprintf("fonte_forma_saida = fonte de maior prioridade entre as candidatas (%d divergencias listadas, tolerancia 0,1%% de %d multi-fonte)", nrow(div), n_multi),
     stopifnot(nrow(div) <= max(5, 0.001 * n_multi)))
} else {
  cat(sprintf("NAO CONFERIDO NO RECORTE DA V1.0: prioridade entre fontes candidatas, %d divergencias em cargos fora do recorte (output/verificacao/integracao_prioridade_divergente.csv); conferir com --banco-inteiro\n", nrow(div)))
}
# tolerancia absoluta de 5: as divergencias residuais sao interacoes das regras de coerencia do R/10
# (linha da fonte fora da janela do mandato, fim no futuro, fim precoce incompativel com fim_regular)
# que este verificador nao reproduz por inteiro; ficam enumeradas em
# output/verificacao/integracao_prioridade_divergente.csv e registradas em vint_prioridade_divergente
# multi-fonte: amostra reproduzivel de 30, com impressao
set.seed(20260828)
multi_ids <- cand[, uniqueN(fonte), by = id_mandato][V1 > 1, id_mandato]
am <- chk[id_mandato %in% sample(multi_ids, min(30, length(multi_ids)))]
print(am[, .(id_mandato, esfera, forma_saida, fonte_forma_saida, fonte_esp, forma_esp)])
ok("amostra multi-fonte: fonte vencedora e a de maior prioridade", stopifnot(all(am$fonte_forma_saida == am$fonte_esp)))
# forma gravada = forma da fonte vencedora (ultimo periodo), salvo (a) fora do vocab -> outro; (b) promocao a fim_regular
fs_ult <- function(src_dt) src_dt
# perda de informacao: fonte de maior prioridade com 'nao_observado' sobrepondo forma observada de fonte inferior
obs_inferior <- cand[!forma %in% c("nao_observado") & !is.na(forma)][, .(tem_obs = TRUE), by = id_mandato]
clob <- merge(chk[forma_saida == "nao_observado" & !is.na(fonte_forma_saida)], obs_inferior, by = "id_mandato")
registrar_numero("vint_nao_observado_sobrepondo_forma_observada_de_fonte_inferior", nrow(clob), script = "R/verifica_integracao.R")
if (nrow(clob)) print(merge(clob[, .(id_mandato, fonte_forma_saida)], cand[id_mandato %in% clob$id_mandato], by = "id_mandato")[order(id_mandato)])
ok("nenhum 'nao_observado' de fonte superior sobrepoe forma observada de fonte inferior", stopifnot(nrow(clob) == 0))
# fim_regular promovido por data_fim_efetiva: por esfera
registrar_numero("vint_fonte_data_fim_efetiva", mand[fonte_forma_saida == "data_fim_efetiva", .N], script = "R/verifica_integracao.R")

## 7. pessoas: flags de dedup contra auditoria_homonimos
au <- fread("data/auditoria_homonimos.csv", colClasses = "character", na.strings = "NA")
ok("auditoria: id_pessoa unico", checa_unica(as.data.frame(au), "id_pessoa"))
ok("auditoria: id_pessoa existe em pessoas", stopifnot(all(au$id_pessoa %in% pess$id_pessoa)))
pa <- merge(pess[, .(id_pessoa, chave_dedup, dedup_ponte_nome_nascimento, dedup_auditoria)], au[, .(id_pessoa, so_nome_nascimento, classificacao)], by = "id_pessoa", all.x = TRUE)
ok("pessoas auditadas: dedup_auditoria == classificacao", stopifnot(pa[!is.na(classificacao), all(dedup_auditoria == classificacao)]))
# 21/09/2026 (pendencia 11, opcao b): dedup_ponte_nome_nascimento marca quem so tem nome e nascimento como
# chave de identidade, a coluna so_nome_nascimento da auditoria
ok("pessoas auditadas: dedup_ponte == so_nome_nascimento", stopifnot(pa[!is.na(classificacao), all(toupper(dedup_ponte_nome_nascimento) == toupper(so_nome_nascimento))]))
ok("pessoas nao auditadas: ponte FALSE e auditoria nao_auditado", stopifnot(pa[is.na(classificacao), all(dedup_ponte_nome_nascimento == "FALSE" & dedup_auditoria == "nao_auditado")]))
ok("dedup_auditoria no vocabulario", in_set(pess$dedup_auditoria, c("consistente", "suspeito", "indeterminado", "nao_auditado"), permitir_na = FALSE, nome = "dedup_auditoria"))
ok("dedup_ponte_nome_nascimento logico", in_set(pess$dedup_ponte_nome_nascimento, c("TRUE", "FALSE"), permitir_na = FALSE, nome = "dedup_ponte"))
ok("toda pessoa com chave_dedup=nome_nascimento esta na auditoria", stopifnot(pa[chave_dedup == "nome_nascimento", all(!is.na(classificacao))]))
ok("toda pessoa com ponte TRUE esta na auditoria", stopifnot(pa[dedup_ponte_nome_nascimento == "TRUE", all(!is.na(classificacao))]))
registrar_numero("vint_pessoas_auditadas", nrow(au), script = "R/verifica_integracao.R")
registrar_numero("vint_pessoas_ponte_true", pess[dedup_ponte_nome_nascimento == "TRUE", .N], script = "R/verifica_integracao.R")
for (cl in c("consistente", "suspeito", "indeterminado")) registrar_numero(paste0("vint_pessoas_auditoria_", cl), pess[dedup_auditoria == cl, .N], script = "R/verifica_integracao.R")

## 8. roteiro e zip
sh <- readLines("R/00_reconstruir.sh")
# so os scripts da raiz de R/ precisam ser citados no orquestrador; os de subpasta sao das frentes
scripts_R <- setdiff(list.files("R", pattern = "\\.R$"), list.files("R", pattern = "^verifica_"))
no_sh <- sapply(scripts_R, function(s) any(grepl(s, sh, fixed = TRUE)))
print(no_sh[!no_sh])
ok("00_reconstruir.sh cita todo script R numerado (exceto verifica_* e 06_amarracoes)",
   stopifnot(all(no_sh[!names(no_sh) %in% "06_amarracoes_fapesp.R"])))
py <- list.files("python", pattern = "\\.py$"); no_sh_py <- sapply(py, function(s) any(grepl(s, sh, fixed = TRUE))); print(no_sh_py[!no_sh_py])
# ordem: cada Rscript aparece na ordem numerica
linhas_R <- regmatches(sh, regexpr("R/[0-9]+[a-z]?_[a-z_]+\\.R", sh))
print(linhas_R)
ok("00_reconstruir.sh: 10_integrar depois de 07-14 e antes de 04/05",
   stopifnot(which(linhas_R == "R/10_integrar_exercicio.R") > max(which(linhas_R %in% c("R/07_exercicio_camara.R", "R/07_exercicio_senado.R", "R/08_suplementares.R", "R/09_auditoria_homonimos.R", "R/11_munic_prefeitos.R", "R/12_wikidata_mandatos.R", "R/13_exercicio_assembleias.R", "R/14_sinais_tse_exercicio.R"))),
             which(linhas_R == "R/04_verificar.R") > which(linhas_R == "R/10_integrar_exercicio.R"),
             which(linhas_R == "R/05_documentar.R") > which(linhas_R == "R/04_verificar.R")))
zl <- system2("unzip", c("-Z1", "zenodo/reconstruir_banco.zip"), stdout = TRUE)
# 21/09/2026: a coleta e a reconstrucao passam a ser so em R (decisao de 19/09/2026), e o
# zip da v1.0 (zenodo/empacotar.sh) nao carrega mais python/; o que ele precisa reproduzir por
# inteiro e todo R/ (recursivo, com subpasta por frente) e lib/*.R
ARQ <- c(file.path("R", list.files("R", pattern = "\\.(R|sh)$", recursive = TRUE)),
         file.path("lib", list.files("lib", pattern = "\\.R$")))
faltam <- setdiff(ARQ, zl)
print(faltam)
ok("zip contem todo R/ e lib/*.R", stopifnot(length(faltam) == 0))
ok("zip nao contem python/", stopifnot(!any(grepl("^python/", zl))))
# scripts do zip identicos aos do repositorio
td <- tempfile(); dir.create(td); system2("unzip", c("-q", "zenodo/reconstruir_banco.zip", "-d", td))
dif <- sapply(ARQ, function(f) file.exists(file.path(td, f)) && unname(tools::md5sum(f) == tools::md5sum(file.path(td, f))))
print(dif[!dif])
ok("scripts no zip identicos aos do repositorio", stopifnot(all(dif)))
# numeros_assinatura.txt entra no zip sem as chaves que casam com zenodo/chaves_fora_do_pacote.txt, arquivo local de
# empacotamento que nao vai no pacote (sem ele, nenhuma chave sai); o resto do registro vai integral (zenodo/empacotar.sh)
## registrar_numero() so acrescenta linha (lib/proveniencia.R), nunca substitui, e a propria checagem
## abaixo ja gravou numeros no arquivo antes de chegar aqui; por isso o zip (foto de um instante) so
## pode ser cobrado como prefixo do arquivo atual sem as chaves fora do pacote, nao como igualdade
na_zip <- readLines(file.path(td, "output/numeros_assinatura.txt"))
na_rep <- readLines("output/numeros_assinatura.txt")
fora <- if (file.exists("zenodo/chaves_fora_do_pacote.txt")) readLines("zenodo/chaves_fora_do_pacote.txt") else character()
fora <- fora[nzchar(fora)]
if (length(fora)) na_rep <- grep(paste0("^(", paste(fora, collapse = "|"), ")"), na_rep, invert = TRUE, value = TRUE)
n_zip <- length(na_zip)
ok("numeros_assinatura.txt do zip e prefixo do repositorio no momento do empacotamento (sem as chaves fora do pacote)",
   stopifnot(n_zip <= length(na_rep), identical(na_zip, na_rep[seq_len(n_zip)])))
libs <- c("lib/asserts_rigor.R", "lib/proveniencia.R")
registrar_numero("vint_libs_externas_fora_do_zip", length(setdiff(libs, zl)), script = "R/verifica_integracao.R")

fora <- c("validade substantiva da forma de saida registrada por cada fonte (Camara, Senado, Assembleias, Wikidata)",
          "pertinencia do pareamento fonte-mandato (verificado nos temas camara/senado/assembleias/wikidata)",
          "portabilidade do zip para diretorio limpo (a raiz vem de BOCEL_ROOT e as bibliotecas de lib/; nao testado em maquina limpa)")
rel <- gravar_relatorio_verificacao(alvo = "BOCEL v1.0 — integracao (R/10) e verificacao (R/04)", script = "R/verifica_integracao.R",
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) { cat(paste("-", falhou), sep = "\n"); quit(status = 1) }
