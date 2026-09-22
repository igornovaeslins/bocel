# verifica_wikipedia_prefeitos.R — verificacao cetica independente de data/wikipedia_prefeitos.csv e
# data/wikipedia_prefeitos_cobertura.csv (frente R/17_wikipedia_prefeitos.R) e da integracao em
# data/mandatos.csv (fonte_forma_saida = 'wikipedia', cargos 11 e 12). Reconta a partir dos arquivos,
# aplica os asserts de rigor e sorteia as amostras conferidas a mao (30 pares; 20 saidas antecipadas).
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_wikipedia_prefeitos.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_wikipedia_prefeitos.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", showWarnings = FALSE)
logf <- "logs/verifica_wikipedia_prefeitos.log"; sink(logf, split = TRUE)
cat("verifica_wikipedia_prefeitos.R —", format(Sys.time()), "\n")
problemas <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); cat("PROBLEMA:", msg, "\n") }
# 05/09/2026: o verificador so acumulava problemas e nao gravava relatorio_verificacao_*.json; cada secao
# que termina sem problema novo entra em `passou`, e o relatorio JSON e gravado antes do sink().
passou <- character(); .n_prob <- 0L
secao <- function(nome) { if (length(problemas) == .n_prob) passou <<- c(passou, nome); .n_prob <<- length(problemas) }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }

## 1. arquivos e chaves --------------------------------------------------------------------------
wp <- fread("data/wikipedia_prefeitos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cob <- fread("data/wikipedia_prefeitos_cobertura.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
ti <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
cols <- c("sg_ue","id_municipio_ibge","uf","municipio","nome_wiki","nome_normalizado","partido_wiki","vice_wiki","inicio","fim",
          "observacao_original","forma_saida","condicao","ano_eleicao_bocel","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento","url","revisao")
faltam <- setdiff(cols, names(wp)); if (length(faltam)) prob(paste("colunas ausentes:", paste(faltam, collapse = ",")))
cat("linhas:", nrow(wp), "\n")
checa_unica(as.data.frame(wp), c("url", "nome_normalizado", "inicio"))
checa_unica(as.data.frame(cob), "ano_eleicao")
vocab <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca")
in_set(wp$forma_saida, vocab, nome = "forma_saida")
in_set(wp$condicao, c("eleito","interino","vice_em_exercicio"), permitir_na = FALSE, nome = "condicao")
in_set(wp$metodo_pareamento, c("nome_completo","nome_urna","tokens_no_nome_civil"), nome = "metodo_pareamento")
ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
in_set(wp$uf, ufs, permitir_na = FALSE, nome = "uf")
em_faixa(as.integer(substr(wp$inicio, 1, 4)), 1996, 2026, permitir_na = FALSE, nome = "ano_inicio")
em_faixa(as.integer(substr(wp$fim, 1, 4)), 1996, 2027, nome = "ano_fim")
em_faixa(as.integer(wp$ano_eleicao_bocel), 1996, 2024, permitir_na = FALSE, nome = "ano_eleicao_bocel")
if (any(as.integer(wp$ano_eleicao_bocel) %% 4L != 0L)) prob("ano_eleicao_bocel fora do ciclo municipal")
d_ok <- function(x) is.na(x) | !is.na(suppressWarnings(as.IDate(x, format = "%Y-%m-%d")))
if (!all(d_ok(wp$inicio)) || !all(d_ok(wp$fim))) prob("data invalida em inicio/fim")
if (wp[!is.na(fim) & fim < inicio, .N] > 0) prob(sprintf("%d linhas com fim < inicio", wp[!is.na(fim) & fim < inicio, .N]))
if (wp[is.na(id_mandato_bocel) != is.na(metodo_pareamento) & condicao != "interino", .N] > 0) prob("metodo_pareamento inconsistente com id_mandato_bocel")
if (wp[!is.na(id_mandato_bocel) & is.na(id_pessoa_bocel), .N] > 0) prob("mandato pareado sem pessoa")
if (wp[!is.na(sg_ue) & is.na(id_municipio_ibge), .N] > 0) prob("sg_ue sem id_municipio_ibge")

secao("1. arquivos e chaves")
## 2. pagina -> municipio (recontado dos .json) --------------------------------------------------
meta <- rbindlist(lapply(list.files("data_raw/wikipedia/prefeitos", pattern = "\\.json$", full.names = TRUE), function(f) as.data.table(fromJSON(f))), fill = TRUE)
n_pag <- nrow(meta)
# pareamento pagina -> municipio refeito aqui com a mesma regra do script (titulo -> nome + UF da categoria)
meta[, mw := sub("^Lista d(e|os) ([Pp]refeitos( e (vice-prefeitos|vereadores|intendentes))?|intendentes e prefeitos) (de|da|do|das|dos) ", "", titulo)]
meta[, mw := sub("^cidade (de|do|da) ", "", mw)]
meta[, uf_tit := gsub("[()]", "", regmatches(mw, regexpr("\\(([A-Z]{2})\\)", mw)))]
meta[, mn := norm(sub("\\s*\\(.*\\)$", "", mw))]
meta[, uf_ref := fifelse(nchar(fcoalesce(uf_tit, "")) == 2, uf_tit, uf)]
ti[, mn := norm(nome_ibge)]
meta <- merge(meta, ti[, .(mn, uf_ref = sg_uf, sg_ue_v = sg_ue)], by = c("mn", "uf_ref"), all.x = TRUE)
n_pag_com <- meta[!is.na(sg_ue_v), .N]
cat("paginas baixadas:", n_pag, "| com municipio (recontado):", n_pag_com, "| urls no csv com municipio:", uniqueN(wp[!is.na(sg_ue), url]), "| urls no csv sem municipio:", uniqueN(wp[is.na(sg_ue), url]), "\n")
nao_par <- meta[is.na(sg_ue_v), url]
cat("paginas nao pareadas a municipio (", length(nao_par), "):\n", sep = ""); print(meta[url %in% nao_par, .(titulo, uf)], nrow = 60)
fwrite(meta[url %in% nao_par, .(titulo, uf, url)], "output/verificacao/wikipedia_prefeitos_paginas_nao_pareadas.csv")
sem_linha <- meta[!is.na(sg_ue_v) & !url %in% wp$url]
cat("paginas com municipio mas sem linha extraida (", nrow(sem_linha), "): tabela sem cabecalho reconhecido ou sem mandato desde 1996\n", sep = ""); print(sem_linha[, .(titulo, uf)], nrow = 40)
fwrite(sem_linha[, .(titulo, uf, url)], "output/verificacao/wikipedia_prefeitos_paginas_sem_linha.csv")
# o csv concorda com a recontagem
jj <- merge(unique(wp[, .(url, sg_ue)]), meta[, .(url, sg_ue_v)], by = "url")
if (jj[!identical(sg_ue, sg_ue_v) & !(is.na(sg_ue) & is.na(sg_ue_v)) & (is.na(sg_ue) | is.na(sg_ue_v) | sg_ue != sg_ue_v), .N] > 0) prob("sg_ue do csv diverge do pareamento recontado")
# UF do csv = UF da categoria de coleta; sg_ue existe na tabela TSE-IBGE e tem a mesma UF
j <- merge(unique(wp[!is.na(sg_ue), .(url, sg_ue, uf, id_municipio_ibge)]), ti[, .(sg_ue, sg_uf, id_ibge = id_municipio_ibge)], by = "sg_ue", all.x = TRUE)
if (j[is.na(sg_uf), .N] > 0) prob("sg_ue nao consta de municipios_tse_ibge")
if (j[uf != sg_uf, .N] > 0) prob(sprintf("%d paginas com UF divergente da tabela TSE", j[uf != sg_uf, .N]))
if (j[id_municipio_ibge != id_ibge, .N] > 0) prob("id_municipio_ibge divergente")
if (j[, .N, by = sg_ue][N > 1, .N] > 0) prob("mais de uma pagina para o mesmo municipio")
cat("UF da categoria x UF do titulo (homonimos entre UFs):\n")
if (meta[!is.na(uf_tit) & uf_tit != uf, .N] > 0) prob("UF do titulo diverge da UF da categoria")
hom <- ti[, .N, by = mn][N > 1, mn]
wp[, mn := norm(sub("\\s*\\(.*\\)$", "", municipio))]
cat("paginas pareadas cujo nome existe em mais de uma UF:", uniqueN(wp[!is.na(sg_ue) & mn %in% hom, url]), "(todas resolvidas pela UF da categoria, assert acima)\n")

secao("2. pagina -> municipio (recontado dos .json)")
## 3. pessoa -> mandato ---------------------------------------------------------------------------
par <- wp[!is.na(id_mandato_bocel)]
jm <- as.data.table(join_seguro(as.data.frame(par[, .(id_mandato_bocel, sg_ue, ano_eleicao_bocel, condicao, id_pessoa_bocel, nome_normalizado, metodo_pareamento, inicio, fim, forma_saida, municipio, nome_wiki, url, observacao_original)]),
                  as.data.frame(mand[, .(id_mandato_bocel = id_mandato, cd_cargo, cargo, unidade_posicao, ano_eleicao, id_pessoa_m = id_pessoa, mandato_inicio, mandato_fim, forma_saida_m = forma_saida, fonte_forma_saida, data_posse, data_fim_efetiva)]),
                  by = "id_mandato_bocel", cardinalidade = "many-to-one", tipo = "inner"))
in_set(jm$cd_cargo, c("11", "12"), permitir_na = FALSE, nome = "cd_cargo dos pareados")
if (jm[sg_ue != unidade_posicao, .N] > 0) prob("pareado a mandato de outro municipio")
if (jm[ano_eleicao != ano_eleicao_bocel, .N] > 0) prob("pareado a mandato de outra eleicao")
if (jm[id_pessoa_bocel != id_pessoa_m, .N] > 0) prob("id_pessoa_bocel diverge da pessoa do mandato")
if (jm[condicao == "vice_em_exercicio" & cd_cargo != "12", .N] > 0) prob("vice_em_exercicio pareado a cargo diferente de 12")
if (jm[condicao == "eleito" & cd_cargo != "11", .N] > 0) prob("eleito pareado a cargo diferente de 11")
if (wp[condicao == "interino" & !is.na(id_mandato_bocel), .N] > 0) prob("interino pareado a mandato")
cat("condicao x pareado:\n"); print(wp[, .N, by = .(condicao, pareado = !is.na(id_mandato_bocel))])
# nome pareado: o nome civil ou de urna do BOCEL contem os tokens do nome da Wikipedia (recheque independente)
jp <- merge(jm, pess[, .(id_pessoa_bocel = id_pessoa, nome, nome_urna_recente)], by = "id_pessoa_bocel")
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
tw <- tok(jp$nome_normalizado); tn <- tok(norm(jp$nome)); tu <- tok(norm(fcoalesce(jp$nome_urna_recente, "")))
jp[, bate := mapply(function(a, b, c) length(a) >= 1 && (all(a %in% b) || all(a %in% c)), tw, tn, tu)]
cat("pares em que todos os tokens do nome da Wikipedia estao no nome civil ou de urna:", sum(jp$bate), "de", nrow(jp), "\n")
if (mean(jp$bate) < 0.95) prob("menos de 95% dos pares com tokens contidos")
cat("pares sem tokens contidos (amostra):\n"); print(jp[bate == FALSE, .(municipio, nome_wiki, nome, nome_urna_recente, metodo_pareamento)][1:20])
# ano de inicio coerente com a eleicao pareada
jm[, iy := as.integer(substr(inicio, 1, 4)) - as.integer(ano_eleicao)]
cat("inicio - ano_eleicao (pareados):\n"); print(table(jm$iy))
if (jm[condicao == "eleito" & !iy %in% 0:4, .N] > 0) prob(sprintf("%d pareados eleitos com inicio fora da janela do mandato", jm[condicao == "eleito" & !iy %in% 0:4, .N]))
# amostra de 30 pares (10 conferidos ao vivo na Wikipedia pelo verificador)
am30 <- jp[sample(.N, 30), .(municipio, uf = substr(id_mandato_bocel, 1, 5), nome_wiki, nome, nome_urna_recente, ano_eleicao, cargo, condicao, metodo_pareamento, inicio, fim, forma_saida, url)]
fwrite(am30, "output/verificacao/wikipedia_prefeitos_amostra30_pares.csv"); print(am30[, .(municipio, nome_wiki, nome, ano_eleicao, cargo, condicao, inicio, fim)], nrow = 30)

secao("3. pessoa -> mandato")
## 4. divisao de linhas plurianuais -----------------------------------------------------------------
wp[, span := as.integer(substr(fim, 1, 4)) - as.integer(substr(inicio, 1, 4))]
cat("distribuicao fim - inicio (anos):\n"); print(table(wp$span, useNA = "ifany"))
long01 <- wp[!is.na(span) & span >= 5 & substr(inicio, 6, 10) == "01-01"]
if (nrow(long01) > 0) prob(sprintf("%d linhas iniciadas em 1/jan cobrindo 5+ anos nao divididas", nrow(long01)))
cat("linhas de 5+ anos com inicio fora de 1/jan (nao divididas, por regra):", wp[!is.na(span) & span >= 5, .N], "\n")
print(wp[!is.na(span) & span >= 5, .(municipio, nome_wiki, inicio, fim, condicao, pareado = !is.na(id_mandato_bocel))])
partes <- wp[substr(inicio, 6, 10) == "01-01" & substr(fim, 6, 10) == "12-31" & span == 3]
cat("linhas de exatamente um mandato (1/jan a 31/dez, 4 anos):", nrow(partes), "\n")

secao("4. divisao de linhas plurianuais")
## 5. forma de saida coerente com a observacao -------------------------------------------------------
o <- toupper(stri_trans_general(fcoalesce(wp$observacao_original, ""), "Latin-ASCII"))
if (wp[forma_saida == "fim_regular" & !substr(fim, 6, 10) %in% c("12-31", "01-01"), .N] > 0) prob("fim_regular com fim fora de 31/dez ou 1/jan")
cat("saida antecipada com fim em 31/dez (causa deve ser propria):\n")
print(wp[forma_saida %in% c("renuncia","cassacao","falecimento") & substr(fim, 6, 10) == "12-31", .(municipio, nome_wiki, inicio, fim, forma_saida, obs = substr(observacao_original, 1, 90))], nrow = 40)
ant <- grepl("(APOS|DEPOIS D|COM A|DEVIDO|EM RAZAO|EM VIRTUDE|EM DECORRENCIA|NO IMPEDIMENTO|NA VAGA|SUBSTITUI|NO LUGAR|EM FUNCAO)[^.]{0,50}(RENUNC|MORTE|FALEC|OBITO|CASSA|AFAST|IMPEDIMENTO|LICEN|ASSASSIN)|(DO|DA) TITULAR|DO ENTAO|DO PREFEITO|DA PREFEITA", o)
n_ant <- wp[ant & forma_saida %in% c("renuncia","cassacao","falecimento","afastamento","licenca"), .N]
if (n_ant > 0) prob(sprintf("%d linhas com causa do antecessor atribuida como saida propria", n_ant))
cat("condicao 'eleito' com observacao iniciada por VICE:", wp[grepl("^VICE", o) & condicao == "eleito", .N], "\n")
sa <- wp[forma_saida %in% c("renuncia","cassacao","falecimento")]
am20 <- sa[sample(.N, 20), .(municipio, uf, nome_wiki, inicio, fim, forma_saida, condicao, pareado = !is.na(id_mandato_bocel), observacao_original, url)]
fwrite(am20, "output/verificacao/wikipedia_prefeitos_amostra20_saidas.csv")
print(am20[, .(municipio, nome_wiki, inicio, fim, forma_saida, obs = substr(observacao_original, 1, 100))], nrow = 20)
am20[, coerente := (forma_saida == "renuncia" & grepl("RENUNC", toupper(stri_trans_general(observacao_original, "Latin-ASCII")))) |
                   (forma_saida == "cassacao" & grepl("CASSA|IMPEACH|IMPEDIMENTO|PERDA", toupper(stri_trans_general(observacao_original, "Latin-ASCII")))) |
                   (forma_saida == "falecimento" & grepl("MORR|MORTE|FALEC|OBITO|ASSASSIN", toupper(stri_trans_general(observacao_original, "Latin-ASCII"))))]
cat("amostra de 20 saidas antecipadas coerentes com a observacao:", sum(am20$coerente), "de 20\n")
if (sum(am20$coerente) < 20) prob("saida antecipada incoerente com a observacao na amostra")

secao("5. forma de saida coerente com a observacao")
## 6. cobertura recontada --------------------------------------------------------------------------
rec <- merge(mand[cargo == "PREFEITO", .(n_bocel = .N), by = .(ano_eleicao)],
             wp[!is.na(id_mandato_bocel) & condicao != "vice_em_exercicio", .(n_pareados = uniqueN(id_mandato_bocel), n_municipios = uniqueN(sg_ue)), by = .(ano_eleicao = ano_eleicao_bocel)], by = "ano_eleicao", all.x = TRUE)
rec[is.na(n_pareados), `:=`(n_pareados = 0L, n_municipios = 0L)][, taxa := round(n_pareados / n_bocel, 4)]
cmp <- merge(rec, cob[, .(ano_eleicao, n_bocel_c = as.integer(n_bocel), n_par_c = as.integer(n_pareados), n_mun_c = as.integer(n_municipios), taxa_c = as.numeric(taxa))], by = "ano_eleicao")
print(cmp)
if (cmp[n_bocel != n_bocel_c | n_pareados != n_par_c | n_municipios != n_mun_c | abs(taxa - taxa_c) > 1e-4, .N] > 0) prob("cobertura publicada diverge da recontagem")
if (cmp[n_pareados > n_bocel, .N] > 0) prob("pareados maior que o denominador")

secao("6. cobertura recontada")
## 7. integracao em mandatos.csv ---------------------------------------------------------------------
wm <- mand[fonte_forma_saida == "wikipedia" & cd_cargo %in% c("11", "12")]
cat("mandatos (cargo 11/12) com fonte_forma_saida = wikipedia:", nrow(wm), "\n")
if (!all(wm$id_mandato %in% wp$id_mandato_bocel)) prob("mandato com fonte wikipedia ausente do csv da Wikipedia")
in_set(wm$forma_saida, c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","outro"), permitir_na = FALSE, nome = "forma_saida integrada")
cat("forma_saida integrada por cargo:\n"); print(wm[, .N, by = .(cargo, forma_saida)][order(cargo, -N)])
# concordancia da forma do csv com a integrada (ultima linha por mandato, so quando o fim cabe na janela)
ult <- wp[!is.na(id_mandato_bocel) & !is.na(forma_saida)][order(id_mandato_bocel, inicio)][, .SD[.N], by = id_mandato_bocel]
ci <- merge(ult[, .(id_mandato_bocel, forma_wp = forma_saida, fim)], wm[, .(id_mandato_bocel = id_mandato, forma_saida, mandato_fim)], by = "id_mandato_bocel")
cat("concordancia forma csv x integrada:\n"); print(ci[, .N, by = .(igual = forma_wp == forma_saida, forma_wp, forma_saida)][order(-N)])
if (ci[forma_wp != forma_saida & forma_saida != "outro" & fim < mandato_fim, .N] > 0) prob("forma integrada diverge do csv sem fim posterior ao convencional")
if (wm[!is.na(data_fim_efetiva) & !is.na(data_posse) & data_fim_efetiva < data_posse, .N] > 0) prob("fim efetivo anterior a posse")
if (wm[forma_saida %in% c("renuncia","falecimento","cassacao","afastamento","licenca") & data_fim_efetiva >= mandato_fim, .N] > 0) prob("saida antecipada com fim efetivo no fim convencional")
# vice: posse do mandato de vice nao vem da data em que assumiu a prefeitura
vv <- merge(wp[condicao == "vice_em_exercicio" & !is.na(id_mandato_bocel), .(id_mandato_bocel, inicio)], mand[, .(id_mandato_bocel = id_mandato, data_posse)], by = "id_mandato_bocel")
if (vv[!is.na(data_posse) & data_posse == inicio & substr(inicio, 6, 10) != "01-01", .N] > 0) prob("posse do vice igual a data em que assumiu a prefeitura")

secao("7. integracao em mandatos.csv")
## 8. registro --------------------------------------------------------------------------------------
registrar_numero("verif_wp_pref_n_linhas", nrow(wp), script = script)
registrar_numero("verif_wp_pref_n_paginas", n_pag, script = script)
registrar_numero("verif_wp_pref_n_paginas_com_municipio", n_pag_com, script = script)
registrar_numero("verif_wp_pref_n_paginas_nao_pareadas", length(nao_par), script = script)
registrar_numero("verif_wp_pref_n_mandatos_pareados", uniqueN(par$id_mandato_bocel), script = script)
registrar_numero("verif_wp_pref_n_mandatos_pareados_prefeito", uniqueN(par[condicao != "vice_em_exercicio", id_mandato_bocel]), script = script)
registrar_numero("verif_wp_pref_n_mandatos_pareados_vice", uniqueN(par[condicao == "vice_em_exercicio", id_mandato_bocel]), script = script)
registrar_numero("verif_wp_pref_n_com_forma_saida", par[!is.na(forma_saida), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("verif_wp_pref_n_interino", wp[condicao == "interino", .N], script = script)
registrar_numero("verif_wp_pref_n_integrados_mandatos", nrow(wm), script = script)
registrar_numero("verif_wp_pref_n_integrados_prefeito", wm[cd_cargo == "11", .N], script = script)
registrar_numero("verif_wp_pref_n_integrados_vice", wm[cd_cargo == "12", .N], script = script)
registrar_numero("verif_wp_pref_pares_tokens_contidos", sum(jp$bate), script = script)
registrar_numero("verif_wp_pref_amostra20_coerentes", sum(am20$coerente), script = script)
for (i in seq_len(nrow(rec))) registrar_numero(sprintf("verif_wp_pref_pareados_%s", rec$ano_eleicao[i]), rec$n_pareados[i], script = script)
cat("\nPROBLEMAS:", length(problemas), "\n"); if (length(problemas)) print(problemas)
writeLines(c(sprintf("# verifica_wikipedia_prefeitos — %s", format(Sys.time())), sprintf("linhas: %d | paginas: %d | com municipio: %d | mandatos pareados: %d | integrados: %d", nrow(wp), n_pag, n_pag_com, uniqueN(par$id_mandato_bocel), nrow(wm)),
             "problemas:", if (length(problemas)) paste0("- ", problemas) else "- nenhum"), "output/verificacao/verifica_wikipedia_prefeitos.md")
fora <- c("veracidade das datas e causas da Wikipedia alem das amostras conferidas a mao (30 pares; 20 saidas antecipadas)",
          "homonimos entre nome de urna e nome da Wikipedia no mesmo municipio e eleicao (pareamento por nome)")
gravar_relatorio_verificacao(alvo = "data/wikipedia_prefeitos.csv + data/wikipedia_prefeitos_cobertura.csv", script = script,
                             passou = passou, falhou = problemas, fora_de_cobertura = fora)
sink()
if (length(problemas)) stop("verifica_wikipedia_prefeitos: ", length(problemas), " problema(s)")
cat("verifica_wikipedia_prefeitos: OK\n")
