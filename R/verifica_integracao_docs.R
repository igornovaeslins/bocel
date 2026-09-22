# verifica_integracao_docs.R — verificador cetico do tema 'integracao_docs' (28/08/2026)
# Confere, apos as frentes 15-18: (1) vocabulario de forma_saida e fonte_forma_saida contra o LIVRO_DE_CODIGOS;
# (2) coerencia de data_posse/data_fim_efetiva com mandato_inicio/mandato_fim; (3) prioridade de fontes em
# mandatos.csv, reconstruida dos arquivos de origem, com amostra de 20 mandatos multi-fonte impressa;
# (4) LIVRO cobre todas as colunas dos CSV listados em FILES de zenodo/deposit.py; (5) NOTA declara as lacunas;
# (6) zip (todo R/ e lib/*.R, sem python/) e roteiro 00_reconstruir.sh (decisao de 19/09/2026: coleta
# e reconstrucao passam a ser so em R).
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_integracao_docs.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_integracao_docs.R"
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
ler <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
d8  <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
# 13/09/2026: senador, deputado federal, presidente, vice-presidente, governador e vice-governador tem forma de saida
# autoritativa (R/58 e R/60 no R/10), sem concorrencia de prioridade entre fontes candidatas; a verificacao desses cargos
# esta em R/verifica_legislativo_federal.R e R/verifica_executivos.R
AUTORITATIVOS <- mand[cd_cargo %in% c("1", "2", "3", "4", "5", "6"), id_mandato]
ok("mandatos: id_mandato unico", checa_unica(as.data.frame(mand), "id_mandato"))

## 1. vocabularios lidos do LIVRO (nao de uma lista fixa no verificador)
lc <- readLines("docs/LIVRO_DE_CODIGOS.md", encoding = "UTF-8")
vocab_do_livro <- function(var) {
  l <- grep(sprintf("^\\| `%s` \\|", var), lc, value = TRUE)[1]
  dentro <- regmatches(l, regexpr("\\([^()]*\\)", l))            # primeiro parentese da descricao
  toks <- trimws(strsplit(gsub("^\\(|\\)$", "", dentro), "[,;]")[[1]])
  unique(sub(" .*$", "", toks))                                   # primeira palavra de cada item
}
VOCAB  <- vocab_do_livro("forma_saida")
FONTES <- vocab_do_livro("fonte_forma_saida")
cat("VOCAB (livro):", VOCAB, "\nFONTES (livro):", FONTES, "\n")
# 13/09/2026: 16 termos com aposentadoria, impeachment e retotalizacao
ok("livro: vocabulario de forma_saida tem 16 termos", stopifnot(length(VOCAB) == 16))
ok("forma_saida dentro do vocabulario do livro (sem NA)", in_set(mand$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida"))
ok("fonte_forma_saida dentro do vocabulario do livro", in_set(mand$fonte_forma_saida, FONTES, nome = "fonte_forma_saida"))
ok("forma observada <=> fonte preenchida", stopifnot(all((mand$forma_saida != "nao_observado") == !is.na(mand$fonte_forma_saida))))
usadas <- sort(unique(na.omit(mand$fonte_forma_saida)))
registrar_numero("vid_fontes_forma_saida_usadas", paste(usadas, collapse = ";"), script = script)
registrar_numero("vid_fontes_do_livro_sem_uso", length(setdiff(FONTES, usadas)), script = script)
fe_vals <- unique(unlist(strsplit(na.omit(mand$fonte_exercicio), ";")))
# 04/09/2026: sapl_presenca (lista de presenca, R/55) e ibge_munic_ampliado (R/49) entraram no R/10
ok("fonte_exercicio dentro do vocabulario do livro", in_set(fe_vals, c("ibge_munic", "ibge_munic_ampliado", "tse_reeleicao", "receita_cnpj", "sapl_presenca", "tce_ac"), permitir_na = FALSE, nome = "fonte_exercicio"))  # tce_ac: 12/09/2026

## 2. datas coerentes com mandato_inicio / mandato_fim
mi <- as.IDate(mand$mandato_inicio); mf <- as.IDate(mand$mandato_fim)
ok("mandato_inicio < mandato_fim em todo mandato", stopifnot(all(mi < mf)))
dp <- !is.na(mand$data_posse)
ok("data_posse >= mandato_inicio - 60 dias", stopifnot(all(as.IDate(mand$data_posse[dp]) >= mi[dp] - 60L)))
ok("data_posse <= mandato_fim", stopifnot(all(as.IDate(mand$data_posse[dp]) <= mf[dp])))
df <- !is.na(mand$data_fim_efetiva)
ok("data_fim_efetiva >= mandato_inicio - 60 dias", stopifnot(all(as.IDate(mand$data_fim_efetiva[df]) >= mi[df] - 60L)))
ok("data_fim_efetiva <= mandato_fim + 45 dias", stopifnot(all(as.IDate(mand$data_fim_efetiva[df]) <= mf[df] + 45L)))
ok("data_fim_efetiva >= data_posse", stopifnot(all(mand$data_fim_efetiva[dp & df] >= mand$data_posse[dp & df])))
ant <- mand$forma_saida %in% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
                               "perda_do_mandato_inferida_por_eleicao_suplementar", "substituicao_inferida_munic", "aposentadoria", "impeachment", "retotalizacao") & df
ok("saida antecipada: data_fim_efetiva <= mandato_fim", stopifnot(all(as.IDate(mand$data_fim_efetiva[ant]) <= mf[ant])))
# saida antecipada com fim IGUAL ao fim convencional: a MUNIC 2004 usa 31/12 do ano da edicao como data de referencia
# (convencao de R/10), o que coincide com o fim do mandato 2001-2004; os demais casos sao contradicao da fonte, listados
igual <- mand[ant & data_fim_efetiva == mandato_fim]
registrar_numero("vid_antecipada_fim_igual_convencional_munic", igual[forma_saida == "substituicao_inferida_munic", .N], script = script)
registrar_numero("vid_antecipada_fim_igual_convencional_outras_fontes", igual[forma_saida != "substituicao_inferida_munic", .N], script = script)
if (nrow(igual[forma_saida != "substituicao_inferida_munic"])) print(igual[forma_saida != "substituicao_inferida_munic", .(id_mandato, forma_saida, fonte_forma_saida, data_fim_efetiva, mandato_fim)])
fwrite(igual[, .(id_mandato, esfera, forma_saida, fonte_forma_saida, data_posse, data_fim_efetiva, mandato_fim)], "output/verificacao/integracao_docs_antecipada_fim_igual_convencional.csv")
# mandatos em curso (mandato_fim posterior a hoje) com fim_regular: a data de fim vem da legislatura projetada pela fonte,
# nao de saida observada — contado e listado por fonte para decisao do autor (a nota diz que na Camara ficam sem forma)
curso <- mand[mandato_fim > as.character(Sys.Date()) & forma_saida == "fim_regular"]
registrar_numero("vid_mandatos_em_curso_com_fim_regular", nrow(curso), script = script)
print(curso[, .N, by = .(esfera, ano_eleicao, fonte_forma_saida)][order(-N)])
fr <- mand$forma_saida == "fim_regular" & df
ok("fim_regular: data_fim_efetiva >= mandato_fim - 60 dias", stopifnot(all(as.IDate(mand$data_fim_efetiva[fr]) >= mf[fr] - 60L)))
ok("fonte data_fim_efetiva so com forma fim_regular", stopifnot(all(mand$forma_saida[mand$fonte_forma_saida %in% "data_fim_efetiva"] == "fim_regular")))
ok("data_posse e data_fim_efetiva em ISO", stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", c(mand$data_posse[dp], mand$data_fim_efetiva[df])))))
ex <- !is.na(mand$exercicio_confirmado)
ok("exercicio_confirmado em [mandato_inicio - 60, mandato_fim + 366]",
   stopifnot(all(as.IDate(mand$exercicio_confirmado[ex]) >= mi[ex] - 60L), all(as.IDate(mand$exercicio_confirmado[ex]) <= mf[ex] + 366L)))
registrar_numero("vid_n_data_posse", sum(dp), script = script)
registrar_numero("vid_n_data_fim_efetiva", sum(df), script = script)
registrar_numero("vid_n_forma_observada", sum(mand$forma_saida != "nao_observado"), script = script)

## 3. prioridade das fontes, reconstruida dos arquivos de origem
cand <- list()
ca <- ler("data/exercicio_camara.csv"); cand$camara_api <- ca[!is.na(id_mandato) & !is.na(forma_saida), .(id_mandato, forma = forma_saida, fim = d8(data_fim_exercicio))]
se <- ler("data/exercicio_senado.csv"); cand$senado_api <- se[!is.na(id_mandato) & !is.na(forma_saida), .(id_mandato, forma = forma_saida, fim = d8(data_fim_exercicio))]
as_ <- ler("data/exercicio_assembleias.csv"); cand$assembleia_api <- as_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
cm <- ler("data/exercicio_camaras_municipais.csv"); cand$sapl_municipal <- cm[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]
wd <- ler("data/wikidata_mandatos.csv"); cand$wikidata <- wd[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))]
ob <- ler("data/wikidata_obitos.csv"); cand$wikidata_obito <- ob[toupper(dentro_do_mandato) == "TRUE", .(id_mandato, forma = "falecimento", fim = d8(data_morte))]
tc_ <- rbindlist(lapply(c("data/tce_gestores.csv", "data/tce_gestores_b.csv", "data/tce_gestores_c.csv", "data/tce_gestores_d.csv"), function(ff) { x <- ler(ff); if (is.null(x)) NULL else x[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim))] }))
if (nrow(tc_)) cand$tce <- tc_
cs_ <- ler("data/exercicio_camaras_sem_sapl.csv"); if (!is.null(cs_)) cand$portal_camara <- cs_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]
# 29/08/2026: a segunda rodada de coletores de camaras entra na mesma fonte 'portal_camara' e faltava aqui
# 04/09/2026: duas fontes que entraram no R/10 e faltavam aqui
so_ <- ler("data/sapl_observacao_titular.csv")
if (!is.null(so_)) cand$sapl_observacao <- so_[tipo_evento == "fim_de_mandato" & !is.na(id_mandato) & !is.na(forma_saida_obs), .(id_mandato, forma = forma_saida_obs, fim = d8(data_evento))]
ai_ <- ler("data/exercicio_assembleias_inventario.csv")
# 12/09/2026: mesmo filtro do R/10, que so aplica a linha do inventario com data de inicio ou de fim
if (!is.null(ai_)) cand$assembleia_inventario <- ai_[!is.na(id_mandato_bocel) & !is.na(forma_saida) & (!is.na(data_inicio_exercicio) | !is.na(data_fim_exercicio)), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
ic_ <- ler("data/saida_cargo_incompativel.csv")
if (!is.null(ic_)) cand$cargo_incompativel <- ic_[, .(id_mandato, forma = forma_saida, fim = d8(data_fim_inferida))]
ap_ <- ler("data/exercicio_assembleias_2.csv")
if (!is.null(ap_)) cand$assembleia_portal <- ap_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
cs2_ <- ler("data/exercicio_camaras_sem_sapl_2.csv")
if (!is.null(cs2_)) cand$portal_camara <- rbindlist(list(cand$portal_camara,
  cs2_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_mandato))]))
ah_ <- ler("data/exercicio_assembleias_historico.csv"); if (!is.null(ah_)) cand$assembleia_historico <- ah_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_exercicio))]
do_ <- ler("data/diarios_mandatos_saida.csv"); if (!is.null(do_)) cand$diario_oficial <- do_[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(data_fim_inferida))]
wp <- ler("data/wikipedia_prefeitos.csv"); we <- ler("data/wikipedia_estadual.csv")
cand$wikipedia <- rbindlist(list(wp[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))],
                                 we[!is.na(id_mandato_bocel) & !is.na(forma_saida), .(id_mandato = id_mandato_bocel, forma = forma_saida, fim = d8(fim))]))
su <- ler("data/mandatos_forma_saida_suplementar.csv")
cand$tse_suplementar <- su[!is.na(id_mandato_ordinario_afetado), .(id_mandato = id_mandato_ordinario_afetado,
  forma = fifelse(momento %in% "antes_da_posse", "nao_tomou_posse", "perda_do_mandato_inferida_por_eleicao_suplementar"), fim = d8(data_fim_inferida))]
mu <- ler("data/munic_prefeitos.csv")
cand$ibge_munic <- mu[status == "outro_em_exercicio" & !is.na(nome_prefeito_munic) & !is.na(id_mandato_bocel),
                      .(id_mandato = id_mandato_bocel, forma = "substituicao_inferida_munic", fim = paste0(ano_munic, "-12-31"))]
cand <- rbindlist(lapply(names(cand), function(n) cand[[n]][, fonte := n]), use.names = TRUE)
cand <- cand[id_mandato %in% mand$id_mandato & !forma %in% "nao_observado"]
ok("fontes candidatas: todo id_mandato pareado existe em mandatos", stopifnot(all(cand$id_mandato %in% mand$id_mandato)))
# regra de janela de R/10: fim fora de [mandato_inicio - 60, mandato_fim + 45] nao concorre
cand <- merge(cand, data.table(id_mandato = mand$id_mandato, mi = mi, mf = mf), by = "id_mandato")
cand <- cand[is.na(fim) | (as.IDate(fim) >= mi - 60L & as.IDate(fim) <= mf + 45L)]
# fim no futuro (termino previsto) nao e saida observada no R/10
cand <- cand[is.na(fim) | as.IDate(fim) <= Sys.Date()]
# niveis exigidos pela tarefa: institucional > wikipedia/wikidata > tse_suplementar > ibge_munic
nivel <- c(camara_api = 1, senado_api = 1, assembleia_api = 1, sapl_municipal = 1, sapl_observacao = 1, portal_camara = 1, assembleia_historico = 1, assembleia_portal = 1, assembleia_inventario = 1, tce = 1, cargo_incompativel = 1,
           wikipedia = 2, wikidata = 2, wikidata_obito = 2, diario_oficial = 2, tse_suplementar = 3, ibge_munic = 4)
cand[, nv := nivel[fonte]]
# 04/09/2026: cargo_incompativel entra no R/10 em modo que so preenche lacuna (apenas_preenche), e por
# isso nao concorre ao melhor nivel quando ha outra fonte candidata; sem esta regra o verificador
# esperava que ele vencesse e listava 19 mandatos da MUNIC como nivel inferior.
SO_PREENCHE_V <- c("cargo_incompativel")
cand[, nv_conc := fifelse(fonte %in% SO_PREENCHE_V & .N > uniqueN(fonte[fonte %in% SO_PREENCHE_V]), NA_real_, nv), by = id_mandato]
esp <- cand[, .(nivel_esp = if (all(is.na(nv_conc))) min(nv) else min(nv_conc, na.rm = TRUE), fontes = paste(sort(unique(fonte)), collapse = ";"),
                formas = paste(sprintf("%s=%s", fonte, forma), collapse = ";"), n_fontes = uniqueN(fonte)), by = id_mandato]
# a regra do vice (derivado_titular) nao vem de fonte candidata: fica fora das checagens de prioridade
chk <- merge(mand[!fonte_forma_saida %in% "derivado_titular" & !id_mandato %in% AUTORITATIVOS, .(id_mandato, cd_cargo, esfera, forma_saida, fonte_forma_saida, data_fim_efetiva, mandato_fim)], esp, by = "id_mandato")
chk[, nivel_grav := nivel[fonte_forma_saida]]
# gravada = data_fim_efetiva so quando promocao a fim_regular (nivel nao se aplica)
# 29/08/2026: a guarda de R/10 permite que fonte de nivel inferior prevaleca quando ela nomeia o
# evento e a de nivel superior traz apenas fim_regular. O nivel deixa de ser exigido nesse caso, e a
# checagem passa a exigir que a forma gravada seja um evento nomeado que alguma fonte candidata
# declarou (docs/CONCORDANCIA_FONTES.md).
EVENTO_V <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
chk[, evento_declarado := forma_saida %in% EVENTO_V &
      mapply(function(f, fo) grepl(sprintf("=%s", f), fo, fixed = TRUE), forma_saida, formas)]
chk[, nivel_ok := fonte_forma_saida %in% "data_fim_efetiva" |
      (!is.na(nivel_grav) & nivel_grav <= nivel_esp) |
      (!is.na(nivel_grav) & evento_declarado)]
multi <- chk[n_fontes > 1]
registrar_numero("vid_mandatos_com_mais_de_uma_fonte_candidata", nrow(multi), script = script)
registrar_numero("vid_mandatos_multi_fonte_nivel_gravado_inferior_ao_esperado", multi[nivel_ok == FALSE, .N], script = script)
# diagnostico (04/09/2026): os casos em que a gravada fica abaixo do melhor nivel vao para arquivo, para leitura
fwrite(multi[nivel_ok == FALSE], "output/verificacao/integracao_docs_nivel_inferior.csv")
if (BANCO_INTEIRO) {
  ok("prioridade: nivel da fonte gravada nunca inferior ao melhor nivel candidato (todos os multi-fonte)", stopifnot(all(multi$nivel_ok)))
  # derivado_titular ja saiu de chk; sobra o caso em que a unica fonte candidata perdeu a forma por
  # regra de coerencia de R/10 e o mandato ficou com o fim efetivo, sem fonte de forma
  ok("prioridade: mandato com uma unica fonte candidata recebeu essa fonte (ou promocao a fim_regular)",
     stopifnot(chk[n_fontes == 1, all(fonte_forma_saida == fontes | fonte_forma_saida %in% "data_fim_efetiva" | is.na(fonte_forma_saida))]))
  ok("forma observada sem nenhuma fonte candidata: so por data_fim_efetiva",
     stopifnot(mand[forma_saida != "nao_observado" & !id_mandato %in% cand$id_mandato & !id_mandato %in% AUTORITATIVOS, all(fonte_forma_saida %in% c("data_fim_efetiva", "derivado_titular"))]))
} else {
  cat(sprintf("NAO CONFERIDO NO RECORTE DA V1.0: prioridade entre fontes candidatas, %d mandatos multi-fonte com nivel inferior em cargos fora do recorte (output/verificacao/integracao_docs_nivel_inferior.csv); conferir com --banco-inteiro\n", multi[nivel_ok == FALSE, .N]))
}
# amostra de 20 multi-fonte, cruzada com os arquivos de origem (as formas de cada fonte vao impressas)
am <- multi[sample(.N, min(20, .N))]
cat("\n== Amostra de 20 mandatos com mais de uma fonte candidata ==\n")
print(am[, .(id_mandato, esfera, forma_saida, fonte_forma_saida, formas)], nrows = 25)
fwrite(am, "output/verificacao/integracao_docs_amostra_prioridade.csv")
ok("amostra de 20: fonte gravada e do melhor nivel candidato", stopifnot(all(am$nivel_ok)))
ok("amostra de 20: forma gravada e a forma declarada pela fonte gravada, ou fim_regular por promocao, ou 'outro' por saneamento",
   stopifnot(all(mapply(function(f, fo, formas) {
     fo %in% c("fim_regular", "outro") || grepl(sprintf("%s=%s", f, fo), formas, fixed = TRUE)
   }, am$fonte_forma_saida, am$forma_saida, am$formas))))
# pares de niveis efetivamente disputados (evidencia de que a prioridade foi exercida)
pares <- multi[, .N, by = .(fontes, fonte_forma_saida)][order(-N)]
cat("\n== Combinacoes de fontes candidatas x fonte gravada ==\n"); print(pares, nrows = 60)
fwrite(pares, "output/verificacao/integracao_docs_pares_prioridade.csv")
for (f in c("sapl_municipal", "wikipedia")) registrar_numero(sprintf("vid_multi_fonte_gravada_%s", f), multi[fonte_forma_saida == f, .N], script = script)

## 4. LIVRO cobre todas as colunas dos CSV listados em FILES de zenodo/deposit.py
dep <- readLines("zenodo/deposit.py")
ini <- grep("^FILES = \\[", dep); fim_ <- ini + which(grepl("^\\]", dep[ini:length(dep)]))[1] - 1
files <- gsub('"', "", regmatches(paste(dep[ini:fim_], collapse = " "), gregexpr('"[^"]+"', paste(dep[ini:fim_], collapse = " ")))[[1]])
# 21/09/2026: FILES de zenodo/deposit.py aponta para data_v1/ (recorte da v1.0), nao mais para data/
# (banco completo); o regex ficava em "data/" e nao casava nenhum arquivo, o que fazia os tres
# checks abaixo passarem vazios (0 CSVs varridos) sem checar nada
csvs <- grep("^data_v1/.*\\.csv$", files, value = TRUE)
ok("deposit.py: todo arquivo de FILES existe", stopifnot(all(file.exists(files[!grepl("camaras_sem_sapl|diarios_|assembleias_historico|tce_gestores", files)]))))
sec_idx <- grep("^## [a-z0-9_]+\\.csv", lc)
livro_cols <- rbindlist(lapply(seq_along(sec_idx), function(i) {
  a <- sec_idx[i]; b <- if (i < length(sec_idx)) sec_idx[i + 1] - 1 else length(lc)
  rows <- grep("^\\| `", lc[a:b], value = TRUE)
  data.table(arquivo = regmatches(lc[a], regexpr("[a-z0-9_]+\\.csv", lc[a])), variavel = gsub("`| ", "", sapply(strsplit(rows, "\\|"), `[`, 2)))
}))
falta <- list()
for (f in csvs) {
  if (!file.exists(f)) next
  cols <- names(fread(f, nrows = 0))
  dl <- livro_cols[arquivo == basename(f), variavel]
  if (!identical(cols, dl)) falta[[f]] <- data.table(arquivo = basename(f), sem_livro = paste(setdiff(cols, dl), collapse = ";"), sem_dado = paste(setdiff(dl, cols), collapse = ";"),
                                                    ordem_igual = setequal(cols, dl))
}
falta <- rbindlist(falta); if (nrow(falta)) print(falta)
ok(sprintf("livro cobre todas as colunas dos %d CSV de FILES (conjunto e ordem)", length(csvs)), stopifnot(nrow(falta) == 0))
registrar_numero("vid_csvs_em_files", length(csvs), script = script)
registrar_numero("vid_colunas_documentadas", nrow(livro_cols[arquivo %in% basename(csvs)]), script = script)
ok("README lista todo CSV de FILES", { rd <- paste(readLines("docs/README.md"), collapse = "\n"); stopifnot(all(sapply(basename(csvs[file.exists(csvs)]), function(b) grepl(b, rd, fixed = TRUE)))) })

## 5. NOTA declara as lacunas
nota <- paste(readLines("docs/NOTA_DE_COBERTURA.md"), collapse = "\n")
lac <- c(camara_leg51 = "legislatura 51")
# 21/09/2026: assembleias_sem_fonte, munic_sem_nome, datajud_sem_partes, cnpj_descartado,
# wikipedia_so_municipios_com_lista e sapl_so_camaras_com_instancia saem da lista porque a nota da
# v1.0 documenta so o recorte federal (presidencia/vice, governador/vice, senador, deputado federal);
# assembleias e municipio ficam para a v1.5 e a v2.0, e nao ha lacuna dessas esferas para declarar
# nesta versao (decisao de 21/09/2026). vices_1998_2000 sai porque a
# alegacao que verificava ("o TSE omite a linha do vice") era factualmente errada: no cadastro de 1998
# o TSE grava a linha do vice-governador para as 27 UFs, so deixa a situacao de eleicao em branco, e
# isso ja fica coberto por fonte_situacao == imputacao_titular, que a nota cita a parte (112 mandatos
# herdados do titular da chapa). MT 2018 (Selma Arruda) foi resolvida por
# ref/correcoes_eleitos_fonte_oficial.csv e vira fonte_situacao == fonte_oficial_casa, tambem citado
# a parte na nota, sem precisar nomear o caso.
for (k in names(lac)) ok(sprintf("nota declara: %s", k), stopifnot(grepl(lac[[k]], nota, ignore.case = FALSE)))
# numeros da nota e do livro batem com os dados, no recorte da v1.0: NOTA_DE_COBERTURA.md e
# LIVRO_DE_CODIGOS.md sao gerados por R/05_documentar.R a partir de data_v1/ (recorte), nao de
# data/ (banco completo, que ainda traz assembleias e municipio fora do recorte); mand, usado nas
# secoes 1-3 acima para checar a integracao do R/10 no banco inteiro, nao serve para comparar com
# esses dois documentos
mand_v1 <- fread("data_v1/mandatos.csv", colClasses = "character", na.strings = "NA")
fmt <- function(n) format(n, big.mark = ".", decimal.mark = ",", trim = TRUE)
ft <- mand_v1[forma_saida != "nao_observado", .N, by = .(esfera, fonte_forma_saida)]
for (i in seq_len(nrow(ft))) ok(sprintf("nota: linha da tabela de fontes %s/%s = %s", ft$esfera[i], ft$fonte_forma_saida[i], ft$N[i]),
   stopifnot(grepl(sprintf("\\| %s \\| %s \\| *%s \\|", ft$esfera[i], ft$fonte_forma_saida[i], fmt(ft$N[i])), nota)))
fs <- mand_v1[, .N, by = .(esfera, forma_saida)]
for (i in seq_len(nrow(fs))) ok(sprintf("nota: linha da tabela de formas %s/%s = %s", fs$esfera[i], fs$forma_saida[i], fs$N[i]),
   stopifnot(grepl(sprintf("\\| %s \\| %s \\| *%s \\|", fs$esfera[i], fs$forma_saida[i], fmt(fs$N[i])), nota)))
n_lin_livro <- as.integer(gsub("\\.", "", regmatches(lc[grep("^## mandatos.csv", lc)], regexpr("[0-9.]+(?= linhas)", lc[grep("^## mandatos.csv", lc)], perl = TRUE))))
ok("livro: n de linhas de mandatos.csv", stopifnot(n_lin_livro == nrow(mand_v1)))
p_livro <- function(var) as.numeric(sub("%", "", trimws(strsplit(grep(sprintf("^\\| `%s` \\|", var), lc, value = TRUE)[1], "\\|")[[1]][5])))
ok("livro: preenchimento de fonte_forma_saida", stopifnot(sprintf("%.1f", 100 * mean(!is.na(mand_v1$fonte_forma_saida))) == sprintf("%.1f", p_livro("fonte_forma_saida"))))
ok("livro: preenchimento de data_posse", stopifnot(sprintf("%.1f", 100 * mean(!is.na(mand_v1$data_posse))) == sprintf("%.1f", p_livro("data_posse"))))

## 6. zip e roteiro
zl <- system2("unzip", c("-Z1", "zenodo/reconstruir_banco.zip"), stdout = TRUE)
# 21/09/2026: a coleta e a reconstrucao passam a ser so em R (decisao de 19/09/2026), e o
# zip da v1.0 (zenodo/empacotar.sh) nao carrega mais python/; o que ele precisa reproduzir por
# inteiro e todo R/ (recursivo, com subpasta por frente) e lib/*.R
todos <- c(file.path("R", list.files("R", pattern = "\\.(R|sh)$", recursive = TRUE)), file.path("lib", list.files("lib", pattern = "\\.R$")))
ok("zip contem todo R/ e lib/*.R", stopifnot(all(todos %in% zl)))
ok("zip nao contem python/", stopifnot(!any(grepl("^python/", zl))))
td <- tempfile(); dir.create(td); system2("unzip", c("-q", "zenodo/reconstruir_banco.zip", "-d", td))
ident <- sapply(todos, function(f) file.exists(file.path(td, f)) && unname(tools::md5sum(f) == tools::md5sum(file.path(td, f))))
print(ident[!ident])
ok("scripts do zip identicos aos do repositorio", stopifnot(all(ident)))
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
sh <- readLines("R/00_reconstruir.sh")
pos_ <- function(p) { w <- grep(p, sh, fixed = TRUE); stopifnot(length(w) >= 1); w[1] }
# 21/09/2026: R/15..R/18 cobrem assembleias, municipios, wikipedia estadual/prefeitos e datajud, que
# ficam fora do recorte da v1.0 (v1.5); o roteiro so os roda quando o bruto correspondente esta em
# disco, com aviso de adiamento (a coleta em Python do datajud ainda nao foi portada para R)
ok("00_reconstruir.sh: R/15 (fora do recorte da v1.0) so roda se o bruto sapl_municipal estiver em disco",
   stopifnot(grepl("[ -d data_raw/sapl_municipal ] && Rscript --vanilla R/15_exercicio_camaras_municipais.R || echo", sh[pos_("R/15_")], fixed = TRUE)))
ok("00_reconstruir.sh: R/16 e R/17 (fora do recorte da v1.0) so rodam se o bruto da Wikipedia estadual/prefeitos estiver em disco",
   stopifnot(grepl("[ -d data_raw/wikipedia/estadual ] && Rscript --vanilla R/16_wikipedia_estadual.R || echo", sh[pos_("R/16_")], fixed = TRUE),
             grepl("[ -d data_raw/wikipedia/prefeitos ] && Rscript --vanilla R/17_wikipedia_prefeitos.R || echo", sh[pos_("R/17_")], fixed = TRUE)))
ok("00_reconstruir.sh: R/18 (fora do recorte da v1.0) so roda se o bruto do DataJud estiver em disco",
   stopifnot(grepl("[ -d data_raw/datajud ] && Rscript --vanilla R/18_datajud_cassacoes.R || echo", sh[pos_("R/18_")], fixed = TRUE)))
ok("00_reconstruir.sh: ordem 03 < 07..14 < 15 < 16/17 < 18 < 10_integrar < 04 < 05",
   stopifnot(pos_("R/03_build_banco.R") < pos_("R/07_exercicio_camara.R"), pos_("R/14_sinais") < pos_("R/15_"), pos_("R/15_") < pos_("R/16_"),
             pos_("R/16_") < pos_("R/18_"), pos_("R/18_") < pos_("R/10_integrar_exercicio.R"),
             pos_("R/10_integrar_exercicio.R") < pos_("R/04_verificar.R"), pos_("R/04_verificar.R") < pos_("R/05_documentar.R")))
ok("R/10 le as saidas de R/15..R/18 (camaras municipais, wikipedia, e nao o datajud sem partes)",
   { r10 <- paste(readLines("R/10_integrar_exercicio.R"), collapse = "\n")
     stopifnot(grepl("exercicio_camaras_municipais.csv", r10), grepl("wikipedia_prefeitos.csv", r10), grepl("wikipedia_estadual.csv", r10)) })
procs <- grep("Rscript --vanilla R/1[5-8]_", system2("ps", c("-axo", "command"), stdout = TRUE), value = TRUE)
procs <- procs[!grepl("verifica_integracao_docs", procs)]; print(procs)
ok("sem processo Rscript de R/15..R/18 em execucao", stopifnot(length(procs) == 0))
# fontes lidas por R/10 nao podem ser mais novas que mandatos.csv (senao a integracao esta desatualizada)
fontes_r10 <- c("data/exercicio_camara.csv", "data/exercicio_senado.csv", "data/mandatos_forma_saida_suplementar.csv", "data/munic_prefeitos.csv",
                "data/wikidata_mandatos.csv", "data/wikidata_obitos.csv", "data/exercicio_assembleias.csv", "data/sinais_tse_exercicio.csv",
                "data/exercicio_camaras_municipais.csv", "data/wikipedia_prefeitos.csv", "data/wikipedia_estadual.csv", "data/pessoas_flags_dedup.csv")
mt <- file.mtime(fontes_r10); names(mt) <- fontes_r10; print(mt[mt > file.mtime("data/mandatos.csv")])
ok("mandatos.csv mais novo que toda fonte lida por R/10", stopifnot(all(mt <= file.mtime("data/mandatos.csv"))))
ok("docs mais novos que mandatos.csv", stopifnot(all(file.mtime(c("docs/NOTA_DE_COBERTURA.md", "docs/LIVRO_DE_CODIGOS.md", "docs/README.md")) >= file.mtime("data/mandatos.csv"))))

fora <- c("validade substantiva da forma de saida e das datas informadas por cada fonte (SAPL, Wikipedia, Wikidata, APIs)",
          "pertinencia do pareamento fonte-mandato por nome (verificado nos temas de cada frente)",
          "pertinencia semantica das descricoes do livro de codigos e da prosa da nota (conferida pelo autor)",
          "portabilidade do zip para diretorio limpo (nao executado aqui)")
rel <- gravar_relatorio_verificacao(alvo = "BOCEL v1.0 — integracao (R/10) e documentacao apos R/15..R/18", script = script,
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) { cat(paste("-", falhou), sep = "\n"); quit(status = 1) }
