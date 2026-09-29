# verifica_exportacao_v1.R — verificacao cetica e independente de data_v1/ (recorte da v1.0, gerado por
# R/64_exportar_v1.R). Reconstroi o filtro do zero a partir de data/, sem chamar R/64, para nao herdar um erro
# do produtor; reconta data_v1/*.csv contra essa reconstrucao e contra as chaves v1_* em numeros_assinatura.txt;
# confere que todo arquivo que zenodo/deposit.py promete existe.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_exportacao_v1.R
set.seed(20260921)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source("lib/asserts_rigor.R")
source("lib/proveniencia.R")
script <- "R/verifica_exportacao_v1.R"

passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ expr; TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  r
}
fmt <- function(n) format(n, big.mark = ".", decimal.mark = ",", trim = TRUE)

CARGOS_RECORTE <- c("1", "2", "3", "4", "5", "6")
CARGOS_SUPLENCIA_SENADO <- c("9", "10")

le_data   <- function(nome) fread(file.path("data", nome), colClasses = "character", na.strings = "NA", encoding = "UTF-8")
le_data_v1 <- function(nome) fread(file.path("data_v1", nome), colClasses = "character", na.strings = "NA", encoding = "UTF-8")

## ------------------------------------------------------------- 1. reconstrucao independente do filtro
mand   <- le_data("mandatos.csv");   mand_esp   <- mand[cd_cargo %in% CARGOS_RECORTE]
pos    <- le_data("posicoes_ano.csv"); pos_esp  <- pos[cd_cargo %in% CARGOS_RECORTE]
pess   <- le_data("pessoas.csv")
ocu    <- le_data("ocupacoes.csv");  ocu_esp    <- ocu[cd_cargo %in% CARGOS_RECORTE]
lsu    <- le_data("lista_suplencia.csv"); lsu_esp <- lsu[cd_cargo %in% CARGOS_RECORTE]
si     <- le_data("suplentes_identidade.csv"); si_esp <- si[cd_cargo %in% c(CARGOS_RECORTE, CARGOS_SUPLENCIA_SENADO)]
psup   <- le_data("pessoas_suplentes.csv")
fil    <- le_data("filiacoes.csv")

ids_eleitos_esp <- unique(c(mand_esp$id_pessoa, pos_esp$id_pessoa))
pess_eleitos_esp <- pess[id_pessoa %in% ids_eleitos_esp]

# Pendencia 17, opcao A: reconstrucao independente do universo alargado, com codigo proprio (nao chama R/64).
# ocupante_sem_mandato e quem R/64 tambem classificaria assim: identificado em ocupacoes.csv, mas que nem o
# universo eleito nem pessoas_suplentes.csv (pelo escopo de suplencia do recorte) resolvem.
ids_suplente_esp <- unique(c(ocu_esp$id_pessoa, lsu_esp$id_pessoa, si_esp$id_pessoa))
psup_esp <- psup[id_pessoa %in% ids_suplente_esp]
ocu_id_esp <- ocu_esp[!is.na(id_pessoa) & id_pessoa != ""]
ids_ocupante_sem_mandato_esp <- setdiff(unique(ocu_id_esp$id_pessoa), union(pess_eleitos_esp$id_pessoa, psup_esp$id_pessoa))

ids_pessoa_esp <- unique(c(ids_eleitos_esp, ids_ocupante_sem_mandato_esp))
pess_esp <- pess[id_pessoa %in% ids_pessoa_esp]
fil_esp  <- fil[id_pessoa %in% ids_pessoa_esp]

## ------------------------------------------------------------- 2. data_v1/ existe e bate com a reconstrucao
falta_arq <- setdiff(c("mandatos.csv", "posicoes_ano.csv", "pessoas.csv", "filiacoes.csv", "ocupacoes.csv",
                       "lista_suplencia.csv", "pessoas_suplentes.csv", "suplentes_identidade.csv", "mandatos_lista.csv",
                       "saida_executivos.csv", "saida_legislativo_federal.csv", "interregnos_legislativo_federal.csv",
                       "mandatos_forma_saida_suplementar.csv", "eleicoes_suplementares.csv",
                       "ocupantes_legislativo_federal.csv", "exercicio_camara.csv", "exercicio_senado.csv",
                       "camara_biografia_eventos.csv", "camara_biografia_posses.csv", "auditoria_homonimos.csv",
                       "pessoas_flags_dedup.csv"),
                     list.files("data_v1"))
ok("data_v1/: todos os arquivos esperados existem", stopifnot(length(falta_arq) == 0))

mv1 <- le_data_v1("mandatos.csv"); pv1 <- le_data_v1("posicoes_ano.csv"); pev1 <- le_data_v1("pessoas.csv")
ov1 <- le_data_v1("ocupacoes.csv"); lv1 <- le_data_v1("lista_suplencia.csv"); fv1 <- le_data_v1("filiacoes.csv")

ok("mandatos.csv: id_mandato de data_v1 == id_mandato da reconstrucao independente (mesmo conjunto)",
   stopifnot(setequal(mv1$id_mandato, mand_esp$id_mandato)))
ok("posicoes_ano.csv: (id_mandato, ano) de data_v1 == reconstrucao independente",
   stopifnot(setequal(paste(pv1$id_mandato, pv1$ano), paste(pos_esp$id_mandato, pos_esp$ano))))
ok("pessoas.csv: id_pessoa de data_v1 == reconstrucao independente (union de eleitos + ocupantes sem mandato)",
   stopifnot(setequal(pev1$id_pessoa, pess_esp$id_pessoa)))
ok("ocupacoes.csv: id_ocupacao de data_v1 == reconstrucao independente",
   stopifnot(setequal(ov1$id_ocupacao, ocu_esp$id_ocupacao)))
ok("lista_suplencia.csv: linhas de data_v1 == reconstrucao independente (contagem)",
   stopifnot(nrow(lv1) == nrow(lsu_esp)))
ok("filiacoes.csv: id_pessoa de data_v1 subconjunto do universo do recorte (nenhuma pessoa fora)",
   stopifnot(all(unique(fv1$id_pessoa) %in% ids_pessoa_esp)))
ok("filiacoes.csv: nenhuma pessoa do recorte com filiacao em data/ ficou de fora de data_v1",
   stopifnot(setequal(fv1$id_pessoa, fil_esp$id_pessoa)))

## ------------------------------------------------------------- 3. nenhuma linha fora do recorte (redundante
## com o assert do R/64, mas recontado aqui com codigo proprio, sem tocar em R/64)
ok("mandatos.csv: nenhum cd_cargo fora de {1..6}", stopifnot(all(mv1$cd_cargo %in% CARGOS_RECORTE)))
ok("posicoes_ano.csv: nenhum cd_cargo fora de {1..6}", stopifnot(all(pv1$cd_cargo %in% CARGOS_RECORTE)))
ok("ocupacoes.csv: nenhum cd_cargo fora de {1..6}", stopifnot(all(ov1$cd_cargo %in% CARGOS_RECORTE)))
ok("lista_suplencia.csv: nenhum cd_cargo fora de {1..6}", stopifnot(all(lv1$cd_cargo %in% CARGOS_RECORTE)))
si1 <- le_data_v1("suplentes_identidade.csv")
ok("suplentes_identidade.csv: nenhum cd_cargo fora de {1..6,9,10}",
   stopifnot(all(si1$cd_cargo %in% c(CARGOS_RECORTE, CARGOS_SUPLENCIA_SENADO))))

## ------------------------------------------------------------- 4. chaves unicas
ok("mandatos.csv: id_mandato unico", checa_unica(as.data.frame(mv1), "id_mandato"))
ok("pessoas.csv: id_pessoa unico", checa_unica(as.data.frame(pev1), "id_pessoa"))
ok("ocupacoes.csv: id_ocupacao unico", checa_unica(as.data.frame(ov1), "id_ocupacao"))

## ------------------------------------------------------------- 5. todo id_pessoa exportado existe em pessoas
falta1 <- setdiff(unique(c(mv1$id_pessoa, pv1$id_pessoa)), pev1$id_pessoa)
ok("todo id_pessoa de mandatos/posicoes_ano existe em pessoas.csv exportado", stopifnot(length(falta1) == 0))

## ------------------------------------------------------------- 5b. todo ocupante identificado resolve numa
## tabela de pessoa do proprio deposito. A ocupacao e a contribuicao central do banco, e quem le o deposito so
## nomeia o ocupante pelas tabelas depositadas; id_pessoa que nao resolve em pessoas.csv nem em
## pessoas_suplentes.csv deixa a linha de ocupacao sem cadastro. Ocupacao sem id_pessoa (vice que assumiu,
## interino) e outra coisa, ja declarada em NOTA_DE_COBERTURA.md, e entra so como numero registrado.
sup1 <- le_data_v1("pessoas_suplentes.csv")
ocu_id <- ov1[!is.na(id_pessoa) & id_pessoa != ""]
orfaos <- setdiff(unique(ocu_id$id_pessoa), union(pev1$id_pessoa, sup1$id_pessoa))
ocu_orfas <- ocu_id[id_pessoa %in% orfaos]
sem_nome <- setdiff(orfaos, lv1$id_pessoa)
cat("\nocupantes identificados:", fmt(uniqueN(ocu_id$id_pessoa)),
    "| fora do cadastro:", fmt(length(orfaos)),
    "| ocupacoes afetadas:", fmt(nrow(ocu_orfas)),
    "| sem nome em qualquer tabela:", fmt(length(sem_nome)), "\n")
if (length(orfaos)) {
  fwrite(ocu_orfas[, .(id_ocupacao, id_mandato, id_pessoa, tipo_ocupante, cargo, sg_uf, ano_eleicao,
                       nome_em_lista_suplencia = !(id_pessoa %in% sem_nome))],
         "output/verificacao/exportacao_v1_ocupantes_fora_do_cadastro.csv")
}
ok("todo id_pessoa de ocupacoes.csv existe em pessoas.csv ou pessoas_suplentes.csv exportados",
   stopifnot(length(orfaos) == 0))
ok("todo ocupante identificado tem nome em alguma tabela do deposito",
   stopifnot(length(sem_nome) == 0))

## ------------------------------------------------------------- 5c. pendencia 17: dominio e coerencia de condicao_no_recorte
## dominio: so os dois rotulos da decisao de Igor; coerencia: cada rotulo tem que bater exatamente com a
## reconstrucao independente da secao 1 (eleito_no_recorte == ids_eleitos_esp, o outro == ids_ocupante_sem_mandato_esp)
ok("pessoas.csv: condicao_no_recorte so tem os dois valores da decisao (eleito_no_recorte, ocupante_sem_mandato_no_recorte)",
   in_set(pev1$condicao_no_recorte, c("eleito_no_recorte", "ocupante_sem_mandato_no_recorte"),
          permitir_na = FALSE, nome = "pev1$condicao_no_recorte"))
ok("pessoas.csv: quem esta marcado eleito_no_recorte e exatamente quem tem mandato/posicao_ano no recorte",
   stopifnot(setequal(pev1[condicao_no_recorte == "eleito_no_recorte", id_pessoa], ids_eleitos_esp)))
ok("pessoas.csv: quem esta marcado ocupante_sem_mandato_no_recorte e exatamente o orfao de ocupacoes.csv da secao 1",
   stopifnot(setequal(pev1[condicao_no_recorte == "ocupante_sem_mandato_no_recorte", id_pessoa],
                       ids_ocupante_sem_mandato_esp)))

## ------------------------------------------------------------- 5d. DEF-01: dominio e coerencia de exerceu_no_ano
## dominio: so TRUE/FALSE/NA (le_data_v1 traz como texto, por isso os literais em aspas); coerencia (sem
## recalcular data de ocupacao titular, que e o proprio objeto do produtor): todo mandato com forma_saida
## explicito de "nao_tomou_posse" tem TODAS as suas linhas de posicoes_ano marcadas FALSE, e NA so pode
## aparecer em mandato ainda em curso (motivo_sem_saida == "em_curso"), nunca em mandato encerrado
ok("posicoes_ano.csv: exerceu_no_ano so tem TRUE/FALSE/NA",
   in_set(pv1$exerceu_no_ano, c("TRUE", "FALSE"), permitir_na = TRUE, nome = "pv1$exerceu_no_ano"))
mand_nao_tomou_posse <- mv1[forma_saida == "nao_tomou_posse", id_mandato]
ok("posicoes_ano.csv: mandato com forma_saida = nao_tomou_posse tem todas as linhas exerceu_no_ano = FALSE",
   stopifnot(all(pv1[id_mandato %in% mand_nao_tomou_posse, exerceu_no_ano] == "FALSE")))
ids_na_exercicio <- unique(pv1[is.na(exerceu_no_ano), id_mandato])
ok("posicoes_ano.csv: exerceu_no_ano so vem NA em mandato em_curso, nunca em mandato encerrado",
   stopifnot(length(ids_na_exercicio) == 0 || all(mv1[id_mandato %in% ids_na_exercicio, motivo_sem_saida] == "em_curso")))

# 29/09/2026 (achado F9): as tres checagens acima leem o proprio pv1 e so a do nao_tomou_posse tinha conteudo
# semantico. Aqui exerceu_no_ano e recalculado do zero, sem chamar R/64 nem reusar as colunas do produtor, a partir da
# ocupacao TITULAR de data/ocupacoes.csv (reconstrucao independente da secao 1) e da forma_saida de data/mandatos.csv, com o
# ano lido como intervalo fechado [ano de data_inicio, ano de data_fim], e o ano da data de referencia como teto do mandato
# em curso. A cobertura para nisto: se a propria ocupacao titular estiver errada, o recalculo repete o erro.
source("lib/data_referencia.R")
ano_ref <- as.integer(format(data_referencia(), "%Y"))
tit_esp <- ocu_esp[tipo_ocupante == "titular", .(id_mandato, ini = as.IDate(data_inicio), fim = as.IDate(data_fim))]
tit_esp <- merge(tit_esp, mand_esp[, .(id_mandato, fs = forma_saida)], by = "id_mandato")
tit_esp[, nunca := fs == "nao_tomou_posse" | (!is.na(fim) & fim < ini)]
tit_esp[, `:=`(a0 = as.integer(format(ini, "%Y")), a1 = as.integer(format(fim, "%Y")))]
ex_rec <- merge(pv1[, .(id_mandato, ano = as.integer(ano), obs = exerceu_no_ano)], tit_esp, by = "id_mandato", all.x = TRUE)
ex_rec[, esp := {
  dentro_fechado <- !is.na(a1) & ano >= a0 & ano <= a1
  dentro_em_curso <- is.na(a1) & ano >= a0 & ano <= ano_ref
  futuro_em_curso <- is.na(a1) & ano >= a0 & ano > ano_ref
  fifelse(nunca, "FALSE", fifelse(dentro_fechado | dentro_em_curso, "TRUE", fifelse(futuro_em_curso, NA_character_, "FALSE")))
}]
ok("posicoes_ano.csv: exerceu_no_ano bate com o recalculo independente a partir da ocupacao titular (todas as linhas)",
   { stopifnot(nrow(ex_rec) == nrow(pv1), !anyNA(ex_rec$ini),
               identical(ex_rec$obs, ex_rec$esp) || all(is.na(ex_rec$obs) == is.na(ex_rec$esp) & (is.na(ex_rec$obs) | ex_rec$obs == ex_rec$esp))) })
registrar_numero("verif_exerceu_no_ano_recalculo_independente_divergentes", ex_rec[is.na(obs) != is.na(esp) | (!is.na(obs) & !is.na(esp) & obs != esp), .N],
               script = script, out = "output/numeros_assinatura.txt")
registrar_numero("verif_exerceu_no_ano_recalculo_independente_linhas", nrow(ex_rec), script = script, out = "output/numeros_assinatura.txt")

## ------------------------------------------------------------- 5e. OCU-06: camara_biografia_eventos.csv sem linha duplicada
cbev1 <- le_data_v1("camara_biografia_eventos.csv")
ok("camara_biografia_eventos.csv: nenhuma linha duplicada", stopifnot(!anyDuplicated(cbev1)))

## ------------------------------------------------------------- 6. todo mandato encerrado tem forma de saida
## motivo_sem_saida so pode ser nao_se_aplica (saida observada) ou em_curso (mandato ainda nao terminou); qualquer
## outro valor e um mandato encerrado sem forma de saida, o que contradiz o recorte declarado 100% fechado
ok("todo mandato do recorte com motivo_sem_saida em {nao_se_aplica, em_curso}",
   stopifnot(all(mv1$motivo_sem_saida %in% c("nao_se_aplica", "em_curso"))))
n_fechado_sem_forma <- mv1[motivo_sem_saida == "nao_se_aplica" & (is.na(forma_saida) | forma_saida %in% c("", "nao_observado")), .N]
ok("nenhum mandato com motivo_sem_saida = nao_se_aplica e forma_saida vazia/nao_observada",
   stopifnot(n_fechado_sem_forma == 0))

## ------------------------------------------------------------- 7. csv e parquet identicos, tabela a tabela
core <- c("mandatos", "posicoes_ano", "pessoas", "filiacoes", "ocupacoes", "lista_suplencia", "pessoas_suplentes")
for (nm in core) {
  fcsv <- file.path("data_v1", paste0(nm, ".csv")); fpq <- file.path("data_v1", paste0(nm, ".parquet"))
  ok(sprintf("%s: parquet existe e bate em linhas/colunas com o csv", nm), {
    stopifnot(file.exists(fcsv), file.exists(fpq))
    dcsv <- fread(fcsv, colClasses = "character", na.strings = "NA", encoding = "UTF-8")
    dpq <- as.data.table(read_parquet(fpq))
    stopifnot(nrow(dcsv) == nrow(dpq), identical(names(dcsv), names(dpq)))
  })
}

## ------------------------------------------------------------- 8. recontagem contra output/numeros_assinatura.txt
ass <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, strip.white = TRUE, colClasses = "character")
setnames(ass, c("chave", "valor", "ep", "data", "md5", "out"))
ult <- ass[, .(valor = valor[.N]), by = chave]
sig <- function(k) { v <- ult[chave == k, valor]; if (length(v)) v else NA_character_ }

rec <- list(
  v1_n_mandatos = nrow(mv1), v1_n_pessoas = nrow(pev1), v1_n_posicoes_ano = nrow(pv1),
  v1_n_ocupacoes = nrow(ov1), v1_n_filiacoes = nrow(fv1),
  v1_pessoas_com_filiacao = uniqueN(fv1$id_pessoa),
  v1_n_lista_suplencia = nrow(lv1),
  v1_mandatos_com_saida_observada = mv1[motivo_sem_saida == "nao_se_aplica", .N],
  v1_mandatos_em_curso = mv1[motivo_sem_saida == "em_curso", .N]
)
por_cargo <- mv1[, .N, by = cargo]
chave_cargo <- c(PRESIDENTE = "v1_n_mandatos_presidente", "VICE-PRESIDENTE" = "v1_n_mandatos_vice_presidente",
                 GOVERNADOR = "v1_n_mandatos_governador", "VICE-GOVERNADOR" = "v1_n_mandatos_vice_governador",
                 SENADOR = "v1_n_mandatos_senador", "DEPUTADO FEDERAL" = "v1_n_mandatos_deputado_federal")
for (cg in names(chave_cargo)) rec[[chave_cargo[[cg]]]] <- if (cg %in% por_cargo$cargo) por_cargo[cargo == cg, N] else 0L

# pendencia 17 (opcao A): recontado a partir da reconstrucao independente da secao 1, nao de pev1
rec$v1_pessoas_eleitas_no_recorte <- length(ids_eleitos_esp)
rec$v1_pessoas_ocupantes_sem_mandato_no_recorte <- length(ids_ocupante_sem_mandato_esp)
rec$v1_ocupacoes_de_ocupante_sem_mandato <- ocu_id_esp[id_pessoa %in% ids_ocupante_sem_mandato_esp, .N]

# DEF-01: recontado a partir de pv1 (data_v1/posicoes_ano.csv), ja lido acima
rec$v1_exerceu_no_ano_true  <- pv1[exerceu_no_ano == "TRUE", .N]
rec$v1_exerceu_no_ano_false <- pv1[exerceu_no_ano == "FALSE", .N]
rec$v1_exerceu_no_ano_na    <- pv1[is.na(exerceu_no_ano), .N]
# mandato nunca_exerceu recontado sem reabrir ocupacoes.csv: mandato cujas linhas de posicoes_ano sao
# todas FALSE (nenhuma TRUE nem NA) so acontece quando o mandato nunca foi exercido em ano nenhum
rec$v1_mandatos_nunca_exerceu <- pv1[, .(todas_false = all(exerceu_no_ano == "FALSE")), by = id_mandato][todas_false == TRUE, .N]
rec$v1_mandatos_com_alguma_linha_nao_exercida <- uniqueN(pv1[exerceu_no_ano == "FALSE", id_mandato])

# DOC-01: recontado a partir de fv1 (data_v1/filiacoes.csv), com o mesmo criterio de R/64 mas codigo proprio
dup_fil_esp <- fv1[, .N, by = .(id_pessoa, sigla_partido, data_filiacao)][N > 1]
rec$v1_doc01_triplas_filiacao_duplicadas <- nrow(dup_fil_esp)
rec$v1_doc01_linhas_filiacao_duplicadas <- sum(dup_fil_esp$N)
# 29/09/2026 (COB-06): a repartilha 17 + 3 e recontada pela condicao gravada em pessoas.csv, sem os conjuntos do R/64
rec$v1_doc01_triplas_filiacao_duplicadas_pessoas_eleitas <- dup_fil_esp[id_pessoa %in% pev1[condicao_no_recorte == "eleito_no_recorte", id_pessoa], .N]
rec$v1_doc01_triplas_filiacao_duplicadas_ocupantes_sem_mandato <- dup_fil_esp[id_pessoa %in% pev1[condicao_no_recorte == "ocupante_sem_mandato_no_recorte", id_pessoa], .N]

# pendencia 12: recontado a partir de mv1 (data_v1/mandatos.csv)
rec$v1_p12_mandatos_sem_data_posse <- mv1[!is.na(forma_saida) & forma_saida != "" & is.na(data_posse), .N]

comp <- rbindlist(lapply(names(rec), function(k) data.table(chave = k, recontado = as.character(rec[[k]]), registrado = sig(k))))
comp[, bate := recontado == registrado]
cat("\n== recontagem independente x registro (v1_*) ==\n"); print(comp, nrows = 50)
fwrite(comp, "output/verificacao/exportacao_v1_recontagem.csv")
ok("chaves v1_* nucleo: recontagem independente bate com o registro", stopifnot(all(!is.na(comp$registrado) & comp$bate)))

## ------------------------------------------------------------- 9. todo arquivo de zenodo/deposit.py existe
dep <- readLines("zenodo/deposit.py")
ini <- grep("^FILES = \\[", dep); fim <- ini + which(grepl("^\\]", dep[ini:length(dep)]))[1] - 1
files <- regmatches(paste(dep[ini:fim], collapse = " "), gregexpr('"[^"]+"', paste(dep[ini:fim], collapse = " ")))[[1]]
files <- gsub('"', "", files)
ausentes <- files[!file.exists(files)]
cat("\nFILES do deposit.py:", length(files), "; ausentes:", length(ausentes), "\n")
if (length(ausentes)) print(ausentes)
ok("todo arquivo listado em zenodo/deposit.py FILES existe em disco", stopifnot(length(ausentes) == 0))
ok("FILES do deposit.py aponta so para data_v1/ (nao data/) nas tabelas de nucleo/ocupacao/fonte",
   stopifnot(!any(grepl("^data/", files))))

## ------------------------------------------------------------- 5c. vocabulario fechado das colunas de data e de
## vinculo de ocupacoes.csv (v1.1, 29/09/2026). Recontado aqui com codigo proprio, sem tocar em R/42 nem em R/64.
## Os conjuntos abaixo sao os valores definidos no livro de codigos; valor novo na coluna sem definicao reprova.
VOC_ORIGEM_INI <- c("fonte", "convencao", "biografia_camara", "sinal_efetivacao_api", "curadoria_ocu07")
VOC_ORIGEM_FIM <- c("fonte", "biografia_camara", "convencao_fim_legislatura", "curadoria_biografia_camara")
VOC_VINCULO    <- c("eleicao", "chapa_senado", "cadeira_unica_da_unidade", "fonte_da_casa", "lista_vaga_unica",
                    "lista_data_estrita", "lista_vaga_inicial", "lista_sucessao_vaga", "casa_legislatura")
ok("ocupacoes.csv: coluna origem_data_fim existe (v1.1)", stopifnot("origem_data_fim" %in% names(ov1)))
ok("ocupacoes.csv: origem_data_inicio so assume valores do vocabulario da v1.1 (ou NA)",
   stopifnot(all(is.na(ov1$origem_data_inicio) | ov1$origem_data_inicio == "" | ov1$origem_data_inicio %in% VOC_ORIGEM_INI)))
ok("ocupacoes.csv: origem_data_fim so assume valores do vocabulario da v1.1 (ou NA)",
   stopifnot(all(is.na(ov1$origem_data_fim) | ov1$origem_data_fim == "" | ov1$origem_data_fim %in% VOC_ORIGEM_FIM)))
ok("ocupacoes.csv: vinculo_cadeira so assume valores do vocabulario da v1.1 e nunca fica vazio",
   stopifnot(!anyNA(ov1$vinculo_cadeira), all(ov1$vinculo_cadeira %in% VOC_VINCULO)))
ok("ocupacoes.csv: origem_data_fim tem valor se e somente se data_fim tem valor",
   stopifnot(identical(is.na(ov1$origem_data_fim) | ov1$origem_data_fim == "",
                       is.na(ov1$data_fim) | ov1$data_fim == "")))

## ------------------------------------------------------------- registro e relatorio
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
reg("verif_exportacao_v1_n_arquivos_data_v1", length(list.files("data_v1")))
reg("v1_ocupacoes_sem_id_pessoa", ov1[is.na(id_pessoa) | id_pessoa == "", .N])
reg("v1_ocupantes_fora_do_cadastro", length(orfaos))
reg("v1_ocupacoes_fora_do_cadastro", nrow(ocu_orfas))
reg("v1_ocupantes_sem_nome_no_deposito", length(sem_nome))
reg("verif_exportacao_v1_n_problemas", length(falhou))

fora <- c("pertinencia da fonte citada em cada mandato (fica com verifica_integracao.R)",
          "cobertura da camada de posse/exercicio para Assembleias e municipios (fora do recorte da v1.0)")
rel <- gravar_relatorio_verificacao(alvo = "data_v1/ — exportacao do recorte da v1.0", script = script,
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) quit(status = 1)
