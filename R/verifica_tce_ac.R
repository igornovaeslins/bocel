# verifica_tce_ac.R — verificacao cetica de R/57_tce_ac.R (data/tce_gestores_e.csv).
#
# Cobre: esquema identico ao das outras tabelas de TCE, unicidade por unidade x exercicio x nome,
# datas dentro do exercicio, pareamento apontando para mandato do mesmo municipio, cargo e eleicao,
# recontagem dos numeros registrados com chave tceac_*, e uma checagem INDEPENDENTE: o nome pareado
# por Jaro-Winkler tem de compartilhar ao menos dois tokens com o nome do eleito.
# FORA DE COBERTURA: se o responsavel nomeado no acordao e de fato quem exerceu o cargo (a Casa pode
# responsabilizar o sucessor pelas contas do antecessor); erro de digitacao no proprio acordao.
# Execucao: Rscript --vanilla R/verifica_tce_ac.R
set.seed(20260905)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "asserts_rigor.R")); source(file.path(root, "lib", "proveniencia.R"))
script <- "R/verifica_tce_ac.R"
passou <- character(); falhou <- character(); fora <- character()
ok <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg); cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n") }
norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }

o <- fread("data/tce_gestores_e.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
b <- fread("data/tce_gestores_b.csv", colClasses = "character", nrows = 1)
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), select = c("id_mandato", "id_pessoa", "sg_uf", "sg_ue", "cd_cargo", "cargo", "ano_eleicao"))
p <- fread("data/pessoas.csv", colClasses = "character", select = c("id_pessoa", "nome"))
cat("linhas:", nrow(o), "\n")
ok(identical(names(o), names(b)), "esquema identico ao de tce_gestores_b.csv")
ok(all(o$uf == "AC") && all(o$tribunal == "TCE-AC") && all(o$fonte == "tce_ac_jurisprudencia"), "uf, tribunal e fonte fixos")
ok(uniqueN(o[, paste(sg_ue, tipo_unidade, exercicio, nome)]) == nrow(o), "uma linha por unidade, exercicio e nome")
ok(all(o$forma_saida == "nao_observado"), "a fonte nao afirma forma de saida")
ok(all(o$tipo_unidade %in% c("prefeitura", "camara")) && all(o$cargo_bocel %in% c("PREFEITO", "VEREADOR")), "tipo de unidade e cargo em vocabulario fechado")
ok(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", o$data_inicio)) && all(grepl("^\\d{4}-\\d{2}-\\d{2}$", o$data_fim)), "datas em ISO")
ok(all(substr(o$data_inicio, 1, 4) == o$exercicio | as.integer(substr(o$data_inicio, 1, 4)) >= as.integer(o$exercicio) - 1L), "inicio do periodo no exercicio (ou no ano anterior, quando o extrato da o periodo)")
ok(all(o$data_fim >= o$data_inicio), "fim nao antecede inicio")
ok(all(as.integer(o$ano_eleicao) == ((as.integer(o$exercicio) - 1L) %/% 4L) * 4L), "eleicao derivada do exercicio pela regra do mandato municipal")
ok(all(o$sg_ue %in% m[sg_uf == "AC", unique(sg_ue)]), "todo municipio existe no banco como municipio do Acre")
pa <- o[!is.na(id_mandato_bocel)]
chk <- merge(pa, m[, .(id_mandato_bocel = id_mandato, sg_ue_m = sg_ue, cd_m = cd_cargo, ano_m = ano_eleicao, id_pessoa_m = id_pessoa)], by = "id_mandato_bocel")
ok(nrow(chk) == nrow(pa), "todo mandato pareado existe em mandatos.csv")
ok(all(chk$sg_ue == chk$sg_ue_m), "mandato pareado e do mesmo municipio")
ok(all(chk$ano_eleicao == chk$ano_m), "mandato pareado e da mesma eleicao")
ok(all((chk$cargo_bocel == "PREFEITO") == (chk$cd_m == "11")), "cargo pareado bate com a unidade (prefeitura -> prefeito, camara -> vereador)")
ok(all(chk$id_pessoa_bocel == chk$id_pessoa_m), "id_pessoa e o do mandato pareado")
# checagem independente: tokens em comum no pareamento por Jaro-Winkler
jw <- merge(pa[metodo_pareamento == "jaro_winkler_0.92"], p, by.x = "id_pessoa_bocel", by.y = "id_pessoa")
if (nrow(jw)) {
  # 05/09/2026: a regra de dois tokens em comum reprovava um par correto de nome com dois tokens e
  # grafia trocada num deles (Hidelbrando/Hildebrando Amorim). A checagem passa a exigir dois tokens
  # em comum, ou, quando o nome tem so dois tokens, o sobrenome igual e o primeiro nome a distancia
  # de edicao de ate dois caracteres.
  tk <- function(x) strsplit(norm(x), " ")[[1]]
  sust <- mapply(function(a, b) {
    ta <- tk(a); tb <- tk(b); comum <- length(intersect(ta, tb))
    comum >= 2 || (min(length(ta), length(tb)) <= 2 && tail(ta, 1) == tail(tb, 1) && adist(ta[1], tb[1]) <= 2)
  }, jw$nome.x, jw$nome.y)
  registrar_numero("tceac_verif_jw_sem_sustento", sum(!sust), script = script)
  ok(all(sust), "todo pareamento por Jaro-Winkler tem dois tokens em comum, ou sobrenome igual e primeiro nome a ate dois caracteres")
} else fora <- c(fora, "sem pareamento por Jaro-Winkler nesta execucao")
# recontagem dos numeros registrados pelo construtor
ult <- function(k) { l <- grep(paste0("^", k, " \\|"), readLines("output/numeros_assinatura.txt"), value = TRUE); if (!length(l)) NA else as.numeric(trimws(strsplit(tail(l, 1), "\\|")[[1]][2])) }
ok(ult("tceac_linhas_saida") == nrow(o), "tceac_linhas_saida bate com a tabela")
ok(ult("tceac_mandatos_pareados") == uniqueN(pa$id_mandato_bocel), "tceac_mandatos_pareados bate com a tabela")
registrar_numero("tceac_verif_linhas", nrow(o), script = script)
registrar_numero("tceac_verif_pareadas", nrow(pa), script = script)
registrar_numero("tceac_verif_prefeitos_pareados", pa[cargo_bocel == "PREFEITO", uniqueN(id_mandato_bocel)], script = script)
fora <- c(fora, "se o responsavel nomeado no acordao e quem de fato exerceu o cargo", "erro de digitacao no acordao")
cat("\nPASS:", length(passou), "| FAIL:", length(falhou), "\n"); if (length(falhou)) cat(paste0("  - ", falhou, collapse = "\n"), "\n")
f <- gravar_relatorio_verificacao("data/tce_gestores_e.csv", script, passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("relatorio:", f, "\n"); if (length(falhou)) quit(status = 1)
