# verifica_wikipedia_estadual.R — verificacao cetica independente de data/wikipedia_estadual.csv e
# data/wikipedia_estadual_cobertura.csv (frente Wikipedia estadual: python/fetch_wikipedia_listas.py
# -> R/16_wikipedia_estadual.R). Reconta a partir dos arquivos de saida, aplica os asserts de rigor e
# procura erros silenciosos: mandato pareado a mais de uma linha, substituto pareado a mandato, parte
# de linha plurianual fora de 4 anos, ano de eleicao do deputado incoerente com o ano que a pagina
# declara, forma de saida incoerente com a data, condicao/cargo fora do vocabulario.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_wikipedia_estadual.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_wikipedia_estadual.R"
logf <- "logs/verifica_wikipedia_estadual.log"; sink(logf, split = TRUE)
cat("verifica_wikipedia_estadual.R —", format(Sys.time()), "\n")
problemas <- character(); passou <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); cat("PROBLEMA:", msg, "\n") }
ok <- function(msg) { passou <<- c(passou, msg); cat("OK:", msg, "\n") }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("\\[[^]]*\\]", "", x); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])

## 1. arquivos, colunas, chave -----------------------------------------------------------------
f_w <- "data/wikipedia_estadual.csv"; f_c <- "data/wikipedia_estadual_cobertura.csv"
stopifnot(file.exists(f_w), file.exists(f_c))
w <- fread(f_w, na.strings = "NA", colClasses = "character", encoding = "UTF-8")
cb <- fread(f_c, na.strings = "NA", encoding = "UTF-8")
mand <- fread("data/mandatos.csv", na.strings = "NA", colClasses = "character")
pess <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character")
cols <- c("uf","cargo","legislatura","ano_eleicao_bocel","nome_wiki","nome_normalizado","partido_wiki","condicao","inicio","fim",
          "observacao_original","forma_saida","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento","url_pagina","revisao")
faltam <- setdiff(cols, names(w)); if (length(faltam)) prob(paste("colunas ausentes:", paste(faltam, collapse = ","))) else ok("17 colunas presentes")
n_vazio <- sum(w == "", na.rm = TRUE); if (n_vazio > 0) prob(sprintf("%d celulas vazias em vez de NA", n_vazio)) else ok("sem celula vazia")
cat("linhas:", nrow(w), " cobertura:", nrow(cb), "\n")
# chave declarada no livro de codigos: url_pagina x nome_normalizado x inicio (deputados sem inicio: url x nome)
chave <- w[, .(url_pagina, nome_normalizado, inicio = fcoalesce(inicio, ""), cargo)]
dup <- chave[duplicated(chave) | duplicated(chave, fromLast = TRUE)]
cat("linhas com chave url x nome x inicio repetida:", nrow(dup), "\n")
if (nrow(dup)) { print(head(merge(unique(dup[, .(url_pagina, nome_normalizado)]), w, by = c("url_pagina", "nome_normalizado"))[, .(uf, cargo, ano_eleicao_bocel, nome_wiki, condicao, partido_wiki, forma_saida)], 12))
  prob(sprintf("%d linhas repetem a chave url x nome x inicio (mesma pessoa em duas tabelas da pagina)", nrow(dup))) } else ok("chave url x nome x inicio unica")
checa_unica(as.data.frame(cb), c("uf", "cargo", "ano_eleicao")); ok("cobertura unica por uf x cargo x eleicao")

## 2. vocabularios e faixas ---------------------------------------------------------------------
vocab <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","nao_tomou_posse","suplente_efetivado","outro")
in_set(w$forma_saida, vocab, nome = "forma_saida"); ok("forma_saida no vocabulario fechado")
in_set(w$cargo, c("GOVERNADOR","VICE-GOVERNADOR","DEPUTADO ESTADUAL","DEPUTADO DISTRITAL"), permitir_na = FALSE, nome = "cargo")
in_set(w$condicao, c("titular","suplente","substituto"), permitir_na = FALSE, nome = "condicao")
in_set(w$metodo_pareamento, c("nome_completo","nome_urna","tokens_no_nome_civil","tokens_no_nome_de_urna"), nome = "metodo_pareamento")
ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
in_set(w$uf, ufs, permitir_na = FALSE, nome = "uf")
em_faixa(as.integer(w$ano_eleicao_bocel), 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao_bocel")
em_faixa(as.integer(substr(w$inicio, 1, 4)), 1995, 2026, nome = "ano_inicio")
em_faixa(as.integer(substr(w$fim, 1, 4)), 1995, 2027, nome = "ano_fim")
em_faixa(cb$taxa, 0, 1, permitir_na = FALSE, nome = "taxa")
ok("vocabularios e faixas")
if (w[!is.na(inicio) & !is.na(fim) & fim < inicio, .N]) prob("fim < inicio") else ok("fim >= inicio")
if (w[cargo == "DEPUTADO DISTRITAL" & uf != "DF", .N] + w[cargo == "DEPUTADO ESTADUAL" & uf == "DF", .N]) prob("cargo distrital fora do DF") else ok("distrital so no DF")
if (any(!grepl("^\\d+$", w$revisao))) prob("revisao nao numerica")
if (any(!startsWith(w$url_pagina, "https://pt.wikipedia.org/wiki/"))) prob("url fora de pt.wikipedia.org")
if (any(is.na(w$id_pessoa_bocel) != is.na(w$metodo_pareamento))) prob("metodo_pareamento inconsistente com id_pessoa_bocel")
if (any(!is.na(w$id_mandato_bocel) & is.na(w$id_pessoa_bocel))) prob("mandato sem pessoa")

## 3. pareamento: mandato existe, pessoa, cargo, UF e eleicao conferem; regras recontadas ------------
pm <- merge(mand[, .(id_mandato, id_pessoa, cargo, sg_uf, ano_eleicao)], pess[, .(id_pessoa, nome, nome_urna_recente)], by = "id_pessoa")
pm[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente))]
p <- w[!is.na(id_mandato_bocel)]
m <- as.data.table(join_seguro(as.data.frame(p), as.data.frame(pm[, .(id_mandato_bocel = id_mandato, id_pessoa_m = id_pessoa, cargo_m = cargo, uf_m = sg_uf, ano_m = ano_eleicao, nome_norm, urna_norm)]),
                               by = "id_mandato_bocel", cardinalidade = "many-to-one", tipo = "inner"))
if (nrow(m) != nrow(p)) prob("id_mandato_bocel inexistente em mandatos.csv") else ok("todo id_mandato_bocel existe")
if (any(m$id_pessoa_bocel != m$id_pessoa_m)) prob("mandato de outra pessoa") else ok("pessoa do mandato = pessoa pareada")
if (any(m$cargo != m$cargo_m)) prob("cargo difere") else ok("cargo confere")
if (any(m$uf != m$uf_m)) prob("UF difere") else ok("UF confere")
if (any(m$ano_eleicao_bocel != m$ano_m)) prob("ano de eleicao difere do mandato") else ok("eleicao confere")
# reconta as regras
m[, regra_ok := fcase(metodo_pareamento == "nome_completo", nome_normalizado == nome_norm,
                      metodo_pareamento == "nome_urna", nome_normalizado == urna_norm,
                      metodo_pareamento == "tokens_no_nome_civil", mapply(function(a, b) length(a) >= 2 && all(a %in% b), tok(nome_normalizado), tok(nome_norm)),
                      metodo_pareamento == "tokens_no_nome_de_urna", mapply(function(a, b) length(a) >= 1 && any(nchar(a) >= 4) && all(a %in% b), tok(nome_normalizado), tok(urna_norm)),
                      default = FALSE)]
cat("regras de pareamento recontadas: falhas =", m[regra_ok == FALSE, .N], "\n")
if (m[regra_ok == FALSE, .N]) prob(sprintf("%d pares nao satisfazem a regra declarada", m[regra_ok == FALSE, .N])) else ok("as 4 regras de pareamento recontam")
print(m[, .N, by = .(cargo, metodo_pareamento)][order(cargo, -N)])
# um mandato deve ter no maximo uma linha por periodo distinto; linhas com o mesmo inicio para o mesmo mandato = duplicata
dm <- p[, .N, by = .(id_mandato_bocel, inicio = fcoalesce(inicio, ""))][N > 1]
cat("mandatos com linhas duplicadas (mesmo inicio):", nrow(dm), "\n")
if (nrow(dm)) prob(sprintf("%d mandatos pareados a linhas repetidas com o mesmo inicio", nrow(dm))) else ok("nenhum mandato pareado a linha repetida")
multi <- p[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR"), .N, by = id_mandato_bocel][N > 1]
cat("mandatos executivos com mais de uma linha (periodos distintos):", nrow(multi), "\n"); print(p[id_mandato_bocel %in% multi$id_mandato_bocel, .(uf, ano_eleicao_bocel, nome_wiki, inicio, fim, forma_saida)])

## 4. governadores e vices: totais no BOCEL, substitutos, cortes de 4 anos, coerencia da forma ------------
ex <- w[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR")]
n_gov <- mand[cargo == "GOVERNADOR", .N]; n_vice <- mand[cargo == "VICE-GOVERNADOR", .N]
cat("mandatos BOCEL: governador", n_gov, " vice", n_vice, "\n")
# 12/09/2026: vices 173 -> 189 com a chapa de 1998 fechada no R/03 (16 vice-governadores); constante trocada
# junto com as demais constantes de N alteradas pela correcao dos vices
if (n_gov != 189L || n_vice != 189L) prob(sprintf("BOCEL tem %d governadores e %d vices (esperado 189 e 189)", n_gov, n_vice)) else ok("189 governadores e 189 vices no BOCEL")
gp <- ex[cargo == "GOVERNADOR" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]; vp <- ex[cargo == "VICE-GOVERNADOR" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]
cat("pareados: governador", gp, "/", n_gov, " vice", vp, "/", n_vice, "\n")
np <- pm[cargo %in% c("GOVERNADOR", "VICE-GOVERNADOR") & !id_mandato %in% w$id_mandato_bocel, .(cargo, sg_uf, ano_eleicao, nome, nome_urna_recente)]
cat("mandatos executivos do BOCEL sem par na Wikipedia:\n"); print(np)
if (ex[condicao == "substituto" & !is.na(id_mandato_bocel), .N]) prob("substituto pareado a mandato") else ok("nenhum substituto pareado a mandato")
if (ex[condicao == "substituto" & cargo != "GOVERNADOR", .N]) prob("substituto fora de governador")
POSSES <- sprintf("%d-01-01", seq(1995L, 2031L, by = 4L))
# substituto = inicio fora da posse regular; titular pareado com inicio fora de posse regular so quando ha retorno (mesma pessoa com linha na posse)
sub_bad <- ex[condicao == "substituto" & substr(inicio, 6, 10) %in% c("01-01", "01-02", "03-15")]
if (nrow(sub_bad)) prob(sprintf("%d substitutos com inicio em data de posse regular", nrow(sub_bad))) else ok("substitutos comecam fora da posse regular")
tit_fora <- ex[cargo == "GOVERNADOR" & condicao == "titular" & !is.na(id_mandato_bocel) & !substr(inicio, 6, 10) %in% c("01-01", "01-02", "03-15")]
tit_fora[, retorno := mapply(function(u, n, a) ex[uf == u & nome_normalizado == n & ano_eleicao_bocel == a & substr(inicio, 6, 10) == "01-01", .N] > 0, uf, nome_normalizado, ano_eleicao_bocel)]
cat("titulares pareados com inicio fora da posse (devem ser retorno apos afastamento):\n"); print(tit_fora[, .(uf, ano_eleicao_bocel, nome_wiki, inicio, fim, retorno)])
if (tit_fora[retorno == FALSE, .N]) prob("titular pareado com inicio fora da posse e sem linha anterior na posse") else ok("titular fora da posse so em retorno")
# nenhuma linha atravessa uma posse regular (corte plurianual)
atravessa <- ex[!is.na(inicio), any(POSSES > inicio & POSSES < fcoalesce(fim, "2026-12-31")), by = .I][V1 == TRUE]
if (nrow(atravessa)) prob(sprintf("%d linhas atravessam uma posse regular sem corte", nrow(atravessa))) else ok("nenhuma linha executiva atravessa posse regular (corte em partes de ate 4 anos)")
dur <- ex[!is.na(inicio) & !is.na(fim), as.numeric(as.IDate(fim) - as.IDate(inicio))]
em_faixa(dur, 0, 4 * 366, permitir_na = FALSE, nome = "duracao_dias_exec"); ok("partes executivas com ate 4 anos")
# ano de eleicao do executivo = ano anterior ao inicio da parte, arredondado ao ciclo
ex[, ano_esp := { a <- as.integer(substr(inicio, 1, 4)) - 1L; a - ((a - 2L) %% 4L) }]
if (ex[!is.na(inicio) & ano_esp != as.integer(ano_eleicao_bocel), .N]) prob("ano de eleicao executivo nao deriva do inicio") else ok("ano de eleicao executivo deriva do inicio")
# coerencia forma x data: fim em posse regular => fim_regular; renuncia/cassacao/falecimento => observacao com a palavra
if (ex[fim %in% POSSES & forma_saida != "fim_regular", .N]) prob("fim em posse regular sem fim_regular") else ok("fim em posse regular => fim_regular")
ex[, obs_n := toupper(stri_trans_general(fcoalesce(observacao_original, ""), "Latin-ASCII"))]
inc <- ex[(forma_saida == "renuncia" & !grepl("RENUNC", obs_n)) | (forma_saida == "falecimento" & !grepl("MORR|MORTE|FALEC|OBITO|ASSASSIN", obs_n)) |
          (forma_saida == "cassacao" & !grepl("CASSA|IMPEACH|IMPEDIMENTO|PERDA DO MANDATO|AFASTADO DEFINITIV", obs_n))]
if (nrow(inc)) prob(sprintf("%d linhas executivas com forma_saida sem a palavra na observacao", nrow(inc))) else ok("forma_saida executiva sustentada pela observacao")
dp <- w[cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL")]
dp[, obs_n := toupper(stri_trans_general(fcoalesce(observacao_original, ""), "Latin-ASCII"))]
# deputados: a forma vem da secao ou da observacao; sem a secao no arquivo, checa que ao menos a observacao ou a pagina traz a palavra
inc_d <- dp[forma_saida %in% c("renuncia", "cassacao", "falecimento") & obs_n == ""]
cat("deputados com renuncia/cassacao/falecimento sem observacao (forma vinda so da secao da pagina):", nrow(inc_d), "\n")
if (dp[condicao == "suplente" & forma_saida == "suplente_efetivado" & !grepl("ASSUMIU|EFETIVAD", obs_n), .N]) prob("suplente_efetivado sem 'assumiu/efetivado'") else ok("suplente_efetivado sustentado")

## 5. deputados: numero da legislatura -> eleicao por UF, coerente com o ano que a pagina declara ------
la <- fread("output/verificacao/wikipedia_legislatura_ano.csv", na.strings = "NA")
la_ok <- la[!is.na(ano_pag)]
cat("paginas de deputados com ano declarado no texto:", nrow(la_ok), "/", nrow(la), "\n")
if (la_ok[ano_pag != ano_eleicao_bocel, .N]) { print(la_ok[ano_pag != ano_eleicao_bocel]); prob(sprintf("%d paginas com ano atribuido diferente do declarado", la_ok[ano_pag != ano_eleicao_bocel, .N])) } else ok("ano atribuido = ano declarado em todas as paginas com ancora")
mx <- la[, .(ano_max = max(ano_eleicao_bocel)), by = uf]
cat("eleicao mais recente coberta por UF:\n"); print(dcast(mx, . ~ uf, value.var = "ano_max"))
if (any(mx$ano_max > 2022L)) prob("legislatura mais recente alem de 2022") else ok("legislatura mais recente <= 2022 em toda UF")
sem_anc <- la[, .(n = sum(!is.na(ano_pag))), by = uf][n == 0, uf]
cat("UF sem ancora (regra 'mais recente = 2022'):", sem_anc, "\n")
# deputados por pagina: contagem compativel com o BOCEL (ate 3x o numero de cadeiras, por suplentes)
dp_pag <- merge(dp[, .(n_wiki = .N), by = .(uf, ano_eleicao = as.integer(ano_eleicao_bocel))], cb[grepl("DEPUT", cargo), .(uf, ano_eleicao, n_bocel)], by = c("uf", "ano_eleicao"))
if (dp_pag[n_wiki > 3 * n_bocel, .N]) prob("pagina de deputados com mais de 3x as cadeiras") else ok("linhas por pagina <= 3x cadeiras")
# secoes de mortes/cassacoes/renuncias/suplentes: paginas com a secao e nenhuma linha correspondente
meta <- rbindlist(lapply(list.files("data_raw/wikipedia/estadual", pattern = "[.]json$", full.names = TRUE), function(f) as.data.table(fromJSON(f))), fill = TRUE)
cat("paginas baixadas:", nrow(meta), "\n")
if (nrow(meta) != uniqueN(meta$url)) prob("url repetida no cache") else ok("cache sem url repetida")
if (!all(w$url_pagina %in% meta$url)) prob("url na saida fora do cache")
dep_meta <- merge(meta[grepl("DEPUT", cargo)][, legislatura := suppressWarnings(as.integer(stri_extract_first_regex(titulo, "[0-9]+")))], la[, .(uf, legislatura, ano_eleicao_bocel)], by = c("uf", "legislatura"))
sem_linha <- dep_meta[ano_eleicao_bocel >= 1998L & !url %in% w$url_pagina, .(uf, ano_eleicao_bocel, titulo)]
cat("paginas de deputados 1998+ sem nenhuma linha lida:", nrow(sem_linha), "\n"); print(sem_linha)

## 6. cobertura recontada --------------------------------------------------------------------------
cb2 <- merge(pm[cargo %in% unique(w$cargo), .(n_bocel = .N), by = .(uf = sg_uf, cargo, ano_eleicao = as.integer(ano_eleicao))],
             w[, .(n_wiki = .N, n_pareados = uniqueN(na.omit(id_mandato_bocel))), by = .(uf, cargo, ano_eleicao = as.integer(ano_eleicao_bocel))], by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE)
cb2[is.na(n_wiki), `:=`(n_wiki = 0L, n_pareados = 0L)]
cmp <- merge(cb2, cb, by = c("uf", "cargo", "ano_eleicao"), suffixes = c("_rec", "_arq"))
if (nrow(cmp) != nrow(cb) || cmp[n_bocel_rec != n_bocel_arq | n_wiki_rec != n_wiki_arq | n_pareados_rec != n_pareados_arq, .N]) prob("cobertura nao reconta") else ok("cobertura reconta a partir de wikipedia_estadual.csv e mandatos.csv")
if (cb[n_pareados > n_bocel, .N]) prob("pareados > mandatos") else ok("pareados <= mandatos")
print(cb[, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados), taxa = round(sum(n_pareados) / sum(n_bocel), 3)), by = cargo])

## 7. numeros registrados ---------------------------------------------------------------------------
registrar_numero("vwp_est_n_linhas", nrow(w), script = script)
registrar_numero("vwp_est_n_paginas", nrow(meta), script = script)
registrar_numero("vwp_est_governador_pareados", gp, script = script)
registrar_numero("vwp_est_vice_governador_pareados", vp, script = script)
registrar_numero("vwp_est_deputado_estadual_pareados", w[cargo == "DEPUTADO ESTADUAL" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("vwp_est_deputado_distrital_pareados", w[cargo == "DEPUTADO DISTRITAL" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("vwp_est_substitutos", ex[condicao == "substituto", .N], script = script)
registrar_numero("vwp_est_bocel_exec_sem_par", nrow(np), script = script)
registrar_numero("vwp_est_paginas_dep_sem_linha", nrow(sem_linha), script = script)
registrar_numero("vwp_est_problemas", length(problemas), script = script)
fora <- c("veracidade do conteudo da Wikipedia (data, causa) — conferida por amostra ao vivo, nao integralmente",
          "homonimos entre nome de urna e nome da Wikipedia na mesma UF/eleicao (pareamento e por nome)",
          "paginas de deputados sem tabela (listas em prosa: RJ 9a legislatura) e listas de vices com grafia divergente do TSE")
# 05/09/2026: alvo vetorial gerava dois objetos JSON concatenados no mesmo arquivo (sprintf vetorizado em
# proveniencia.R); os 12 relatorios de 28-29/08 falhavam em json.load. Um unico alvo textual "a + b".
gravar_relatorio_verificacao(alvo = paste(f_w, f_c, sep = " + "), script = script, passou = passou, falhou = problemas, fora_de_cobertura = fora)
cat("\nverifica_wikipedia_estadual:", length(passou), "checks ok;", length(problemas), "problemas\n")
if (length(problemas)) { cat(paste("-", problemas), sep = "\n"); quit(status = 1) }
