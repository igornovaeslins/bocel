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
fil    <- le_data("filiacoes.csv")

ids_pessoa_esp <- unique(c(mand_esp$id_pessoa, pos_esp$id_pessoa))
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
ok("pessoas.csv: id_pessoa de data_v1 == reconstrucao independente (union de mandatos+posicoes_ano)",
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

## ------------------------------------------------------------- registro e relatorio
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
reg("verif_exportacao_v1_n_arquivos_data_v1", length(list.files("data_v1")))
reg("verif_exportacao_v1_n_problemas", length(falhou))

fora <- c("pertinencia da fonte citada em cada mandato (fica com verifica_integracao.R)",
          "cobertura da camada de posse/exercicio para Assembleias e municipios (fora do recorte da v1.0)")
rel <- gravar_relatorio_verificacao(alvo = "data_v1/ — exportacao do recorte da v1.0", script = script,
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) quit(status = 1)
