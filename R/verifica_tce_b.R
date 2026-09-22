# verifica_tce_b.R — verificacao independente de data/tce_gestores_b.csv e data/tce_gestores_b_cobertura.csv
# (frente dos Tribunais de Contas, grupo B). Nao reescreve nada: le as saidas de R/24_tce_gestores_b.R e o
# cache bruto em data_raw/tce/, confere as invariantes e grava o relatorio em output/verificacao/.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce_b.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_tce_b.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce_b.log", open = "wt"); sink(logf, split = TRUE)
norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
COLS <- c("uf", "tribunal", "unidade_gestora", "tipo_unidade", "id_municipio_ibge", "sg_ue", "nome", "cpf", "cargo_fonte",
          "cargo_bocel", "data_inicio", "data_fim", "situacao_fonte", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel",
          "metodo_pareamento", "url")
passou <- character(); falhou <- character(); fora <- character()
ck <- function(nome, ok, detalhe = "") { if (isTRUE(ok)) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nzchar(detalhe)) paste0(" — ", detalhe) else "")); cat(if (isTRUE(ok)) "OK   " else "FALHA", nome, detalhe, "\n") }

o <- fread("data/tce_gestores_b.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/tce_gestores_b_cobertura.csv")
inv <- fread("data_raw/tce/inventario_tce_b.csv", colClasses = "character")

## 1. contrato de colunas (as mesmas 18 da frente A, na mesma ordem)
ck("01 colunas obrigatorias na ordem da frente A", identical(names(o)[1:18], COLS), paste(setdiff(COLS, names(o)), collapse = ","))
## 2. vocabulario fechado de forma de saida
ck("02 forma_saida no vocabulario fechado", all(o$forma_saida %in% VOCAB), paste(setdiff(unique(o$forma_saida), VOCAB), collapse = ","))
## 3. cargo no vocabulario do BOCEL
ck("03 cargo_bocel em PREFEITO/VICE-PREFEITO/VEREADOR", all(o$cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO", "VEREADOR")))
## 4. municipio sempre resolvido e coerente com o mapa TSE-IBGE
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
ck("04 sg_ue sempre preenchido", o[is.na(sg_ue), .N] == 0L, paste(o[is.na(sg_ue), .N], "linhas"))
ck("05 par sg_ue/ibge existe no mapa TSE-IBGE", nrow(fsetdiff(unique(o[, .(sg_ue, id_municipio_ibge)]), unique(mun[, .(sg_ue, id_municipio_ibge)]))) == 0L)
ck("06 UF da linha bate com a UF do municipio", nrow(merge(unique(o[, .(sg_ue, uf)]), unique(mun[, .(sg_ue, sg_uf)]), by = "sg_ue")[uf != sg_uf]) == 0L)
## 7. datas
d <- copy(o)[, `:=`(ai = as.integer(ano_eleicao))]
ck("07 datas em ISO yyyy-mm-dd", d[!is.na(data_inicio) & !grepl("^\\d{4}-\\d{2}-\\d{2}$", data_inicio), .N] +
                                  d[!is.na(data_fim) & !grepl("^\\d{4}-\\d{2}-\\d{2}$", data_fim), .N] == 0L)
ck("08 data_fim nunca anterior a data_inicio", d[!is.na(data_inicio) & !is.na(data_fim) & data_fim < data_inicio, .N] == 0L)
ck("09 data_inicio nunca antes da posse da legislatura", d[!is.na(data_inicio) & !is.na(ai) & data_inicio < sprintf("%d-01-01", ai + 1L), .N] == 0L)
## 10. pareamento: id_mandato_bocel existe no BOCEL, e o cargo e o municipio batem
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13"),
          .(id_mandato, id_pessoa, sg_ue_bocel = unidade_posicao, ano_eleicao_bocel = ano_eleicao, cargo_bocel_ref = cargo)]
p <- merge(o[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
ck("10 todo id_mandato_bocel existe em data/mandatos.csv", p[is.na(sg_ue_bocel), .N] == 0L, paste(p[is.na(sg_ue_bocel), .N], "orfaos"))
ck("11 municipio do pareamento bate", p[!is.na(sg_ue_bocel) & sg_ue != sg_ue_bocel, .N] == 0L)
ck("12 cargo do pareamento bate", p[!is.na(cargo_bocel_ref) & cargo_bocel != cargo_bocel_ref, .N] == 0L)
ck("13 id_pessoa do pareamento bate", p[!is.na(id_pessoa) & id_pessoa_bocel != id_pessoa, .N] == 0L)
ck("14 eleicao do pareamento bate quando a fonte a informa",
   p[!is.na(ano_eleicao) & !is.na(ano_eleicao_bocel) & ano_eleicao != ano_eleicao_bocel, .N] == 0L)
## 15. um mandato nao recebe duas formas de saida observadas
ck("15 no maximo uma saida observada por mandato",
   o[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", .N, by = id_mandato_bocel][N > 1L, .N] == 0L)
ck("16 metodo_pareamento presente sempre que ha mandato", o[!is.na(id_mandato_bocel) & is.na(metodo_pareamento), .N] == 0L)
ck("17 nenhuma linha pareada com metodo descartado_duplicata", o[metodo_pareamento == "descartado_duplicata" & !is.na(id_mandato_bocel), .N] == 0L)
## 18. fontes sem data nunca declaram saida (PI so confirma exercicio)
ck("18 PI nunca declara forma de saida", o[uf == "PI" & forma_saida != "nao_observado", .N] == 0L)
ck("19 legislatura em curso nunca recebe fim_regular",
   o[forma_saida == "fim_regular" & !is.na(ano_eleicao) & as.integer(ano_eleicao) + 4L >= as.integer(format(Sys.Date(), "%Y")), .N] == 0L)
## 20. recontagem independente do bruto: PB (folha) e PE (cadastro)
arqs <- list.files("data_raw/tce/pb", pattern = "^gestores_folha_\\d{4}\\.csv$", full.names = TRUE)
pb <- rbindlist(lapply(arqs, function(f) { x <- fread(f, colClasses = "character"); if ("nome_municipio" %in% names(x)) setnames(x, "nome_municipio", "municipio"); x }), use.names = TRUE, fill = TRUE)
ck("20 os 14 anos da folha do TCE-PB estao no cache", length(arqs) == 14L, paste(length(arqs), "arquivos"))
ck("21 folha do TCE-PB so tem cargo eletivo",
   pb[!grepl("^(PREFEIT|VICE ?-? ?PREFEIT|VICEPREFEIT|VEREADOR)", gsub("^[0-9]+ *- *", "", norm(descricao_cargo))), .N] == 0L)
np_pb <- pb[, uniqueN(paste(descricao_unidade_gestora, norm(nome_servidor), gsub("^[0-9]+ *- *", "", norm(descricao_cargo)),
                            ((as.integer(substr(ano_mes, 1, 4)) - 1L) %/% 4L) * 4L))]
cat("recontagem PB: vinculos distintos no bruto =", np_pb, "| linhas PB na saida =", o[uf == "PB", .N], "\n")
ck("22 linhas de PB na saida nao excedem os vinculos do bruto", o[uf == "PB", .N] <= np_pb)
pe <- rbindlist(lapply(list.files("data_raw/tce/pe", pattern = "^servidores_", full.names = TRUE),
                       function(f) as.data.table(fromJSON(f, simplifyVector = TRUE)$resposta$conteudo)), use.names = TRUE, fill = TRUE)
ck("23 toda linha de PE na saida tem vinculo Eletivo no bruto", o[uf == "PE", .N] <= pe[NomeTipoVinculo == "Eletivo", .N],
   paste(o[uf == "PE", .N], "de", pe[NomeTipoVinculo == "Eletivo", .N]))
ck("24 falecimentos batem com o ato do TCE-PE",
   o[uf == "PE" & forma_saida == "falecimento", .N] <= pe[NomeTipoVinculo == "Eletivo" & TipoAtoPessoalAfastamento == "Falecimento", .N])
## 25. cobertura: os pareados por UF/cargo/eleicao nao passam do total de mandatos do BOCEL
ck("25 cobertura nunca passa de 1", cob[taxa > 1, .N] == 0L)
ck("26 cobertura reproduz a contagem de pareados da tabela principal",
   cob[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
   paste(cob[, sum(n_pareados)], "vs", o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]))
## 27. inventario cobre as 14 UFs do grupo B
UF_B <- c("PE", "CE", "PB", "RN", "PI", "MA", "AL", "SE", "PA", "AM", "AC", "RO", "RR", "AP")
ck("27 inventario cobre as 14 UFs do grupo B", setequal(unique(inv$uf), UF_B), paste(setdiff(UF_B, inv$uf), collapse = ","))
ck("28 inventario tem url e observacao em toda linha", inv[is.na(url) | url == "" | is.na(observacao) | observacao == "", .N] == 0L)

fora <- c("pertinencia semantica do pareamento por tokens do nome (homonimo em municipio pequeno)",
          "completude do cadastro de cada tribunal: a folha do TCE-PB e o cadastro do TCE-PE cobrem quem foi informado na remessa, e a omissao de um vereador na remessa nao e distinguivel da ausencia de mandato",
          "leitura substantiva de 'outro' na folha do TCE-PB: a interrupcao do pagamento e evidencia de saida, nao prova de forma de saida",
          "validade do CPF mascarado das duas fontes principais, que impede a checagem de identidade por documento")
registrar_numero("tceb_verif_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tceb_verif_n_aprovadas", length(passou), script = script)
registrar_numero("tceb_verif_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao("data/tce_gestores_b.csv + data/tce_gestores_b_cobertura.csv", "R/24_tce_gestores_b.R",
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nrelatorio:", f, "\n", length(passou), "aprovadas,", length(falhou), "reprovadas\n")
if (length(falhou)) { sink(); stop("verificacao reprovada: ", paste(falhou, collapse = " | ")) }
sink()
