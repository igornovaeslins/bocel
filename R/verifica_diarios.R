# verifica_diarios.R — verificacao cetica independente da frente Querido Diario (R/21_diarios_oficiais.R):
# data/diarios_eventos.csv, data/diarios_mandatos_saida.csv, data/diarios_cobertura_bocel.csv, data/diarios_taxa_uf_ano.csv.
# Confere chave unica, vocabulario, datas dentro da janela do mandato, pareamento (nome no trecho), recontagem dos numeros
# registrados por R/21 em output/numeros_assinatura.txt, precisao da inferencia por confianca lida a mao em amostras
# lidas a mao em 29/08/2026, uma por rodada de aperto da regra:
#   diarios_precisao_julgamentos_antes.csv    30 'alta' antes de qualquer ancora            12/30 = 0,400
#   diarios_precisao_julgamentos_regra_v1.csv 20 'alta' + 20 'media' com a ancora nome-cargo 14/20 = 0,700
#   diarios_precisao_julgamentos_regra_v2.csv 30 'alta' + 20 'media' com o contexto do ato    23/30 = 0,767
#   diarios_precisao_julgamentos_regra_v3.csv 30 'alta' com corroboracao de mandato           28/30 = 0,933
#   diarios_precisao_julgamentos.csv          30 'alta' + 20 'media' na regra final (limiar de 0,9)
# e cobertura de cidades contra /cities, inclusive a varredura dos municipios que o /cities marca como nivel 0.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_diarios.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_diarios.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", showWarnings = FALSE, recursive = TRUE)
sink("logs/verifica_diarios.log", split = TRUE)
cat("verifica_diarios.R —", format(Sys.time()), "\n")
problemas <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); cat("PROBLEMA:", msg, "\n") }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }

ev <- fread("data/diarios_eventos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
sa <- fread("data/diarios_mandatos_saida.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cb <- fread("data/diarios_cobertura_bocel.csv", colClasses = "character", na.strings = "NA")
tx <- fread("data/diarios_taxa_uf_ano.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data_raw/querido_diario/cobertura_cidades.csv", colClasses = "character", na.strings = c("", "NA"))
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")

## 1. colunas, vocabulario, chaves, faixas
cols <- c("id_municipio_ibge", "sg_ue", "sg_uf", "data_diario", "termo", "evento_inferido", "trecho", "id_pessoa_bocel", "id_mandato_bocel",
          "cd_cargo_bocel", "ano_eleicao_bocel", "metodo_pareamento", "confianca", "motivo_rebaixamento", "evento_ancorado", "url_diario")
faltam <- setdiff(cols, names(ev)); if (length(faltam)) prob(paste("colunas ausentes em diarios_eventos:", paste(faltam, collapse = ",")))
VOC_EV <- c("NA", "renuncia", "afastamento", "licenca", "cassacao", "falecimento", "suplente_efetivado", "nao_tomou_posse")
VOC_BOCEL <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
in_set(ev$evento_inferido, VOC_EV, permitir_na = FALSE, nome = "evento_inferido")
in_set(ev$evento_ancorado, setdiff(VOC_EV, c("NA", "suplente_efetivado", "nao_tomou_posse")), nome = "evento_ancorado")
in_set(ev$confianca, c("alta", "media", "baixa"), nome = "confianca")
in_set(ev$metodo_pareamento, c("nome_completo_no_trecho", "nome_de_urna_unico_no_trecho", "tokens_primeiro_e_ultimo_nome_unico"), nome = "metodo_pareamento")
in_set(ev$cd_cargo_bocel, c("11", "12", "13"), nome = "cd_cargo_bocel")
in_set(sa$forma_saida, setdiff(VOC_EV, "NA"), permitir_na = FALSE, nome = "forma_saida")
in_set(sa$forma_saida_bocel_atual, VOC_BOCEL, permitir_na = FALSE, nome = "forma_saida_bocel_atual")
in_set(sa$confianca, "alta", permitir_na = FALSE, nome = "confianca_saida")
checa_unica(as.data.frame(sa), "id_mandato_bocel")
checa_unica(as.data.frame(cob), "id_municipio_ibge")
checa_unica(as.data.frame(cb), "cd_cargo")
checa_unica(as.data.frame(tx), c("sg_uf", "ano_eleicao"))
checa_unica(as.data.frame(ev), c("id_municipio_ibge", "data_diario", "url_diario", "trecho", "id_mandato_bocel"))
em_faixa(as.integer(substr(ev$data_diario, 1, 4)), 2005, 2026, permitir_na = FALSE, nome = "ano_diario")
em_faixa(as.numeric(tx$taxa_nomeados), 0, 1, permitir_na = FALSE, nome = "taxa_nomeados")
em_faixa(as.numeric(tx$taxa_saida_alta), 0, 1, permitir_na = FALSE, nome = "taxa_saida_alta")
if (any(is.na(ev$id_mandato_bocel) & ev$evento_inferido == "NA")) prob("linha de evento sem evento e sem pareamento")
if (!all(ev$id_municipio_ibge %in% cob[!is.na(total_edicoes) & total_edicoes != "0", id_municipio_ibge])) prob("evento em municipio sem cobertura registrada")

## 2. pareamento: mandato existe, mesmo municipio, cargo municipal, data do diario na janela [inicio, fim + 60]
par <- ev[!is.na(id_mandato_bocel)]
mj <- as.data.frame(mand[, .(id_mandato_bocel = id_mandato, id_pessoa_m = id_pessoa, sg_ue_m = unidade_posicao, cd_cargo_m = cd_cargo, mandato_inicio, mandato_fim, ano_m = ano_eleicao)])
m <- as.data.table(join_seguro(as.data.frame(par[, .(id_mandato_bocel, id_pessoa_bocel, id_municipio_ibge, data_diario, cd_cargo_bocel, ano_eleicao_bocel)]), mj,
                               by = "id_mandato_bocel", cardinalidade = "many-to-one", tipo = "inner", unmatched = "error"))
if (nrow(m) != nrow(par)) prob("id_mandato_bocel pareado inexistente em mandatos.csv")
if (any(m$id_pessoa_bocel != m$id_pessoa_m)) prob("id_pessoa_bocel difere da pessoa do mandato")
if (any(m$cd_cargo_bocel != m$cd_cargo_m | m$ano_eleicao_bocel != m$ano_m)) prob("cargo/ano do evento difere do mandato")
m <- merge(m, mun[, .(sg_ue_m = sg_ue, ibge_m = id_municipio_ibge)], by = "sg_ue_m", all.x = TRUE)
if (any(is.na(m$ibge_m)) || any(m$id_municipio_ibge != m$ibge_m)) prob("pareamento cruza municipios")
fora <- m[as.IDate(data_diario) < as.IDate(mandato_inicio) | as.IDate(data_diario) > as.IDate(mandato_fim) + 60L, .N]
if (fora > 0) prob(sprintf("%d pareamentos com data do diario fora da janela do mandato", fora))
if (!all(sa$id_mandato_bocel %in% mand$id_mandato)) prob("id_mandato_bocel de saida inexistente em mandatos.csv")
sm <- merge(sa[, .(id_mandato_bocel, data_fim_inferida)], mand[, .(id_mandato_bocel = id_mandato, mandato_inicio, mandato_fim)], by = "id_mandato_bocel")
if (sm[as.IDate(data_fim_inferida) < as.IDate(mandato_inicio) | as.IDate(data_fim_inferida) > as.IDate(mandato_fim) + 60L, .N] > 0) prob("data_fim_inferida fora da janela do mandato")
# a linha de saida e o evento 'alta' mais antigo do mandato, com o mesmo evento
alt <- ev[confianca == "alta" & !is.na(id_mandato_bocel) & evento_inferido != "NA"][order(id_mandato_bocel, data_diario)][!duplicated(id_mandato_bocel)]
if (nrow(alt) != nrow(sa)) prob(sprintf("saida tem %d linhas; eventos alta mais antigos por mandato: %d", nrow(sa), nrow(alt)))
chk <- merge(sa[, .(id_mandato_bocel, forma_saida, data_fim_inferida)], alt[, .(id_mandato_bocel, evento_inferido, data_diario)], by = "id_mandato_bocel")
if (nrow(chk) != nrow(sa) || any(chk$forma_saida != chk$evento_inferido | chk$data_fim_inferida != chk$data_diario)) prob("saida nao coincide com o evento alta mais antigo")
# confianca alta exige nome completo, evento ancorado igual ao evento da linha, sem motivo de rebaixamento
a <- ev[confianca == "alta"]
if (any(a$metodo_pareamento != "nome_completo_no_trecho" | a$evento_inferido == "NA" | is.na(a$evento_ancorado) | a$evento_ancorado != a$evento_inferido | !is.na(a$motivo_rebaixamento)))
  prob("confianca alta sem nome completo, sem ancora ou com motivo de rebaixamento")
if (any(ev$confianca %in% "media" & is.na(ev$id_mandato_bocel)) || any(ev$confianca %in% "baixa" & !is.na(ev$id_mandato_bocel))) prob("confianca media/baixa incoerente com pareamento")
# o nome civil consta de fato no trecho normalizado (alta) e o nome (civil ou urna) no trecho para todo pareamento por nome completo
a <- merge(a, pess[, .(id_pessoa_bocel = id_pessoa, nome)], by = "id_pessoa_bocel")
a[, ok := stri_detect_fixed(paste0(" ", norm(gsub("<<|>>", "", trecho)), " "), paste0(" ", norm(nome), " "))]
if (!all(a$ok)) prob(sprintf("%d linhas alta cujo nome civil nao consta do trecho", sum(!a$ok)))
nc <- merge(ev[metodo_pareamento %in% "nome_completo_no_trecho"], pess[, .(id_pessoa_bocel = id_pessoa, nome)], by = "id_pessoa_bocel")
nc[, ok := stri_detect_fixed(paste0(" ", norm(gsub("<<|>>", "", trecho)), " "), paste0(" ", norm(nome), " "))]
if (!all(nc$ok)) prob(sprintf("%d pareamentos por nome completo sem o nome no trecho", sum(!nc$ok)))
# alta so com termo e cargo no trecho (regra apertada): 'ex-' nao precede o cargo ligado ao nome (amostra integral por regex simples)
tp <- norm(gsub("<<|>>", "", a$trecho))
if (!all(grepl("RENUNC|LICENC|AFAST|CASSA|PERDA DO MANDATO|FALEC|OBITO", tp) & grepl("PREFEIT|VEREADOR|EDIL", tp))) prob("linha alta sem termo de saida ou sem cargo no trecho")

## 3. recontagem dos numeros registrados por R/21 (ultimo valor de cada chave)
# o registro e um append de varias frentes e ha linha com "|" dentro do valor (registro de string por outro script),
# entao a leitura e por linha e nao por fread com numero fixo de colunas
lin <- readLines("output/numeros_assinatura.txt", warn = FALSE)
lin <- grep("^qd_[A-Za-z0-9_]+ \\|", lin, value = TRUE)
reg <- data.table(chave = trimws(sub(" \\|.*$", "", lin)), valor = trimws(sapply(strsplit(lin, "|", fixed = TRUE), `[`, 2)))
reg <- reg[, .SD[.N], by = chave]
ult <- function(k) { v <- reg[chave == k, valor]; if (length(v)) as.numeric(v) else NA_real_ }
rec <- list(
  qd_n_eventos_linhas = nrow(ev),
  qd_n_eventos_com_evento_inferido = ev[evento_inferido != "NA", .N],
  qd_n_eventos_pareados_bocel = ev[!is.na(id_mandato_bocel), .N],
  qd_n_mandatos_bocel_nomeados_em_excerto = ev[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  qd_n_pareamentos_com_ancora_nome_cargo_termo = ev[!is.na(evento_ancorado), .N],
  qd_n_mandatos_com_saida_inferida_alta = nrow(sa),
  qd_n_mandatos_saida_inferida_novos_para_bocel = sa[forma_saida_bocel_atual == "nao_observado", .N],
  qd_n_mandatos_saida_inferida_concorda_bocel = sa[forma_saida_bocel_atual != "nao_observado" & forma_saida_bocel_atual == forma_saida, .N],
  qd_n_mandatos_saida_inferida_discorda_bocel = sa[forma_saida_bocel_atual != "nao_observado" & forma_saida_bocel_atual != forma_saida, .N],
  qd_n_municipios_com_edicoes = cob[!is.na(total_edicoes) & total_edicoes != "0", .N],
  qd_n_municipios_lista_cities = nrow(cob),
  qd_n_municipios_com_excertos_em_cache = length(grep("^\\d{7}$", list.dirs("data_raw/querido_diario", recursive = FALSE, full.names = FALSE))),
  qd_taxa_saida_alta_em_cidades_com_cache = round(sum(as.integer(tx$n_mandatos_saida_alta)) / sum(as.integer(tx$n_mandatos)), 4))
for (k in names(rec)) {
  r <- ult(k); registrar_numero(paste0("verif_", k), rec[[k]], script = script)
  if (is.na(r) || abs(r - rec[[k]]) > 1e-9) prob(sprintf("recontagem de %s: registrado %s, recontado %s", k, r, rec[[k]]))
}
tabc <- ev[evento_inferido != "NA", .N, by = .(evento_inferido, confianca)]
for (i in seq_len(nrow(tabc))) { k <- sprintf("qd_n_eventos_%s_%s", tabc$evento_inferido[i], tabc$confianca[i]); r <- ult(k)
  if (is.na(r) || r != tabc$N[i]) prob(sprintf("recontagem de %s: registrado %s, recontado %s", k, r, tabc$N[i])) }
for (i in seq_len(nrow(cb))) for (v in c("mandatos_2002_2024", "em_cidade_coberta", "com_janela_coberta")) {
  k <- sprintf("qd_bocel_%s_%s", cb$cd_cargo[i], v); r <- ult(k)
  x <- as.numeric(cb[[c(mandatos_2002_2024 = "n_mandatos", em_cidade_coberta = "n_em_cidade_coberta", com_janela_coberta = "n_com_janela_coberta")[v]]][i])
  if (is.na(r) || r != x) prob(sprintf("recontagem de %s: registrado %s, recontado %s", k, r, x)) }
# cobertura de cidades: todo municipio de nivel 3 no /cities tem edicoes e cache; nenhum com edicoes fica sem cache completo
dirs <- grep("^\\d{7}$", list.dirs("data_raw/querido_diario", recursive = FALSE, full.names = FALSE), value = TRUE)
com_ed <- cob[!is.na(total_edicoes) & total_edicoes != "0", id_municipio_ibge]
if (!setequal(dirs, com_ed)) prob("cache de excertos nao coincide com os municipios com edicoes")
if (!all(file.exists(file.path("data_raw/querido_diario", dirs, "_ok")))) prob("municipio com cache incompleto (sem _ok)")
if (cob[nivel == "3" & (is.na(total_edicoes) | total_edicoes == "0"), .N] > 0) prob("municipio de nivel 3 sem edicoes apuradas")
if (cob[nivel != "0" & is.na(total_edicoes), .N] > 0) prob("municipio de nivel != 0 sem apuracao de edicoes")
# expansao de 29/08/2026: o /cities marca nivel 0 tambem em municipios que ja tem edicao publicada; python/fetch_qd_expansao.py
# varreu por lotes de 50 territory_ids todos os municipios sem apuracao. Exige varredura completa e cache dos achados.
if (cob[is.na(total_edicoes), .N] > 0) prob(sprintf("%d municipios do /cities ainda sem apuracao de edicoes", cob[is.na(total_edicoes), .N]))
if (!file.exists("data_raw/querido_diario/varredura_nivel0.csv")) prob("varredura de nivel 0 ausente") else {
  vr <- fread("data_raw/querido_diario/varredura_nivel0.csv", colClasses = "character")
  checa_unica(as.data.frame(vr), "id_municipio_ibge")
  in_set(vr$resultado, c("sem_edicao", "com_edicao", "indeterminado"), permitir_na = FALSE, nome = "resultado_varredura")
  if (vr[resultado == "indeterminado", .N] > 0) prob(sprintf("%d municipios ficaram indeterminados na varredura", vr[resultado == "indeterminado", .N]))
  if (!setequal(vr$id_municipio_ibge, cob[nivel == "0", id_municipio_ibge])) prob("varredura nao cobre exatamente os municipios de nivel 0")
  if (!all(vr[resultado == "com_edicao", id_municipio_ibge] %in% dirs)) prob("municipio de nivel 0 com edicao ficou sem cache de excertos")
  registrar_numero("verif_qd_n_municipios_nivel0_varridos", nrow(vr), script = script)
  registrar_numero("verif_qd_n_municipios_nivel0_com_edicao", vr[resultado == "com_edicao", .N], script = script)
}
registrar_numero("verif_qd_n_municipios_nivel3", cob[nivel == "3", .N], script = script)
registrar_numero("verif_qd_n_municipios_nivel1_com_edicoes", cob[nivel == "1" & !is.na(total_edicoes) & total_edicoes != "0", .N], script = script)
# consultas truncadas remanescentes no cache
trunc <- sum(unlist(lapply(dirs, function(d) sapply(list.files(file.path("data_raw/querido_diario", d), pattern = "\\.json$", full.names = TRUE),
  function(f) isTRUE(tryCatch(fromJSON(f, simplifyVector = FALSE)$truncado, error = function(e) NA))))))
registrar_numero("verif_qd_n_consultas_truncadas_remanescentes", trunc, script = script)

## 4. precisao lida a mao (amostras sorteadas com set.seed(20260827) a partir dos arquivos de saida; julgamento por leitura do trecho)
prec <- function(f, rot) {
  if (!file.exists(f)) { prob(paste("amostra de precisao ausente:", f)); return(invisible(NULL)) }
  j <- fread(f, colClasses = "character", na.strings = "NA", encoding = "UTF-8")
  in_set(j$correto, c("0", "1"), permitir_na = FALSE, nome = paste0("correto_", rot))
  for (cf in unique(j$confianca)) {
    x <- j[confianca == cf]; p <- round(mean(as.integer(x$correto)), 4)
    cat(sprintf("precisao %s %s: %d/%d = %.3f\n", rot, cf, sum(as.integer(x$correto)), nrow(x), p))
    registrar_numero(sprintf("verif_qd_precisao_%s_%s", rot, cf), p, script = script)
    registrar_numero(sprintf("verif_qd_precisao_%s_%s_n", rot, cf), nrow(x), script = script)
    if ("na_amostra_40" %in% names(j)) {
      x40 <- x[na_amostra_40 %in% c("TRUE", "true")]
      if (nrow(x40)) {
        cat(sprintf("precisao %s %s (amostra de 40): %d/%d = %.3f\n", rot, cf, sum(as.integer(x40$correto)), nrow(x40), mean(as.integer(x40$correto))))
        registrar_numero(sprintf("verif_qd_precisao_amostra40_%s_%s", rot, cf), round(mean(as.integer(x40$correto)), 4), script = script)
        registrar_numero(sprintf("verif_qd_precisao_amostra40_%s_%s_n", rot, cf), nrow(x40), script = script)
      }
    }
  }
  j
}
ja <- prec("output/verificacao/diarios_precisao_julgamentos_antes.csv", "antes_regra_apertada")
j1 <- prec("output/verificacao/diarios_precisao_julgamentos_regra_v1.csv", "regra_apertada_v1")
j2 <- prec("output/verificacao/diarios_precisao_julgamentos_regra_v2.csv", "regra_apertada_v2")
j3 <- prec("output/verificacao/diarios_precisao_julgamentos_regra_v3.csv", "regra_apertada_v3")
jd <- prec("output/verificacao/diarios_precisao_julgamentos.csv", "depois_regra_apertada")
if (!is.null(jd)) {
  # a amostra julgada tem de corresponder a linhas existentes no arquivo atual (mesmo mandato, data e evento)
  chave <- merge(jd[, .(id_mandato_bocel, data_diario, evento_inferido, confianca)], ev[, .(id_mandato_bocel, data_diario, evento_inferido, confianca, ok = TRUE)],
                 by = c("id_mandato_bocel", "data_diario", "evento_inferido", "confianca"), all.x = TRUE)
  if (any(is.na(chave$ok))) prob("amostra julgada nao corresponde ao arquivo atual de eventos")
  pa <- jd[confianca == "alta", mean(as.integer(correto))]
  if (is.na(pa) || pa < 0.9) prob(sprintf("precisao de 'alta' na amostra julgada = %.3f < 0,9", pa))
}

veredito <- if (length(problemas)) "reprovado" else "verificado"
gravar_relatorio_verificacao(alvo = "data/diarios_mandatos_saida.csv", script = script,
  passou = if (length(problemas)) character() else c("vocabulario fechado", "chaves unicas", "id_mandato existe e e do mesmo municipio/cargo/pessoa",
    "data do diario e data_fim_inferida na janela [inicio, fim+60]", "saida = evento alta mais antigo", "alta = nome completo + ancora nome-cargo-termo sem rebaixamento",
    "nome civil consta do trecho", "recontagem dos numeros de R/21", "cache de excertos cobre todos os municipios com edicoes do /cities", "precisao 'alta' >= 0,9 na amostra julgada"),
  falhou = problemas,
  fora_de_cobertura = c("pertinencia semantica do trecho fora das amostras lidas (homonimo, ato revogado, mocao rejeitada)",
                        "licenca/afastamento sao temporarios: data_fim_inferida e a data do diario, nao do retorno",
                        "diarios anteriores a 2013 quase inexistentes na base",
                        "municipio sem diario publicado na base do Querido Diario nao entra: a varredura de 29/08/2026 apurou os 5.570 do /cities e so 527 tem edicao"))
cat("veredito:", veredito, "\n"); if (length(problemas)) print(problemas)
sink()
if (length(problemas)) quit(status = 1)
