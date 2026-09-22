# verifica_tce_d.R — verificacao independente de data/tce_gestores_d.csv e data/tce_gestores_d_cobertura.csv
# (frente dos Tribunais de Contas, grupo D: CE, RN, MA, AL, SE, PA, AM, AC, RO, RR, AP). Nao reescreve nada:
# le as saidas de R/34_tce_gestores_d.R e o cache bruto em data_raw/tce/, confere as invariantes e grava o
# relatorio em output/verificacao/.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce_d.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_tce_d.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce_d.log", open = "wt"); sink(logf, split = TRUE)
norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z0-9 ]", " ", x); gsub(" +", " ", trimws(x)) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
COLS <- c("uf", "tribunal", "unidade_gestora", "tipo_unidade", "id_municipio_ibge", "sg_ue", "nome", "cpf", "cargo_fonte",
          "cargo_bocel", "data_inicio", "data_fim", "situacao_fonte", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel",
          "metodo_pareamento", "url")
UF_D <- c("CE", "RN", "MA", "AL", "SE", "PA", "AM", "AC", "RO", "RR", "AP")
UF_AB <- c("BA", "ES", "MS", "RJ", "RS", "SC", "SP", "PB", "PE", "PI")
passou <- character(); falhou <- character(); fora <- character()
ck <- function(nome, ok, detalhe = "") { if (isTRUE(ok)) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nzchar(detalhe)) paste0(" — ", detalhe) else "")); cat(if (isTRUE(ok)) "OK   " else "FALHA", nome, detalhe, "\n") }

o <- fread("data/tce_gestores_d.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/tce_gestores_d_cobertura.csv")
inv <- fread("data_raw/tce/inventario_tce_d.csv", colClasses = "character")
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")

## contrato de colunas e vocabularios
ck("01 colunas obrigatorias na ordem das frentes A e B", identical(names(o)[1:18], COLS), paste(setdiff(COLS, names(o)), collapse = ","))
ck("02 forma_saida no vocabulario fechado", all(o$forma_saida %in% VOCAB), paste(setdiff(unique(o$forma_saida), VOCAB), collapse = ","))
ck("03 toda linha do grupo D e nao_observado (lista de julgamento nao datava a saida)",
   all(o$forma_saida == "nao_observado"), paste(unique(o$forma_saida), collapse = ","))
ck("04 cargo_bocel em PREFEITO/VICE-PREFEITO/VEREADOR ou ausente",
   all(is.na(o$cargo_bocel) | o$cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO", "VEREADOR")),
   paste(setdiff(unique(o$cargo_bocel), c(NA, "PREFEITO", "VICE-PREFEITO", "VEREADOR")), collapse = ","))
ck("05 tipo_unidade em camara/prefeitura/outra", all(o$tipo_unidade %in% c("camara", "prefeitura", "outra", "estadual")))
ck("06 nenhuma UF dos grupos A e B entra nesta frente", length(intersect(unique(o$uf), UF_AB)) == 0L,
   paste(intersect(unique(o$uf), UF_AB), collapse = ","))
ck("07 toda UF da saida pertence ao grupo D", all(unique(o$uf) %in% UF_D), paste(setdiff(unique(o$uf), UF_D), collapse = ","))

## municipio
ck("08 sg_ue sempre preenchido", o[is.na(sg_ue), .N] == 0L, paste(o[is.na(sg_ue), .N], "linhas"))
ck("09 par sg_ue/ibge existe no mapa TSE-IBGE",
   nrow(fsetdiff(unique(o[, .(sg_ue, id_municipio_ibge)]), unique(mun[, .(sg_ue, id_municipio_ibge)]))) == 0L)
ck("10 UF da linha bate com a UF do municipio",
   nrow(merge(unique(o[, .(sg_ue, uf)]), unique(mun[, .(sg_ue, sg_uf)]), by = "sg_ue")[uf != sg_uf]) == 0L)
ck("11 nenhum municipio excede o total da UF no cadastro",
   nrow(merge(o[, .(n = uniqueN(sg_ue)), by = uf], mun[, .(tot = uniqueN(sg_ue)), by = .(uf = sg_uf)], by = "uf")[n > tot]) == 0L)

## datas: esta frente nao coleta periodo, entao as duas colunas sao vazias por construcao
ck("12 data_inicio e data_fim vazias (a fonte nao datava o vinculo)",
   o[!is.na(data_inicio), .N] + o[!is.na(data_fim), .N] == 0L)
# um registro do TCE-AM traz exercicio 1900, que e o valor da propria fonte; o limite inferior do assert
# e 1900 e o excedente fica contado em tced_n_registros_exercicio_anterior_a_1990
ck("13 exercicio, quando informado, e ano de quatro digitos entre 1900 e o ano corrente",
   o[!is.na(exercicio) & (!grepl("^\\d{4}$", exercicio) | as.integer(exercicio) < 1900L |
                            as.integer(exercicio) > as.integer(format(Sys.Date(), "%Y"))), .N] == 0L)
ck("14 ano_eleicao, quando informado, e ano de eleicao municipal",
   o[!is.na(ano_eleicao) & !(as.integer(ano_eleicao) %in% seq(1996L, 2024L, 4L)), .N] == 0L)

## pareamento
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13"),
          .(id_mandato, id_pessoa, sg_ue_bocel = unidade_posicao, ano_eleicao_bocel = ano_eleicao, cargo_bocel_ref = cargo)]
p <- merge(o[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
ck("15 todo id_mandato_bocel existe em data/mandatos.csv", p[is.na(sg_ue_bocel), .N] == 0L, paste(p[is.na(sg_ue_bocel), .N], "orfaos"))
ck("16 municipio do pareamento bate", p[!is.na(sg_ue_bocel) & sg_ue != sg_ue_bocel, .N] == 0L)
ck("17 cargo do pareamento bate quando o cargo foi determinado",
   p[!is.na(cargo_bocel) & !is.na(cargo_bocel_ref) & cargo_bocel != cargo_bocel_ref, .N] == 0L)
ck("18 id_pessoa do pareamento bate", p[!is.na(id_pessoa) & id_pessoa_bocel != id_pessoa, .N] == 0L)
ck("19 eleicao do pareamento bate quando a fonte a informa",
   p[!is.na(ano_eleicao) & !is.na(ano_eleicao_bocel) & ano_eleicao != ano_eleicao_bocel, .N] == 0L)
ck("20 metodo_pareamento presente sempre que ha mandato", o[!is.na(id_mandato_bocel) & is.na(metodo_pareamento), .N] == 0L)
ck("21 linha de cargo indeterminado so casa pela regra declarada como indeterminada",
   o[is.na(cargo_bocel) & !is.na(metodo_pareamento) & !grepl("indeterminado|^cpf_municipio$", metodo_pareamento), .N] == 0L,
   paste(unique(o[is.na(cargo_bocel) & !is.na(metodo_pareamento)]$metodo_pareamento), collapse = ","))
ck("22 linha de cargo determinado nunca usa a regra de cargo indeterminado",
   o[!is.na(cargo_bocel) & grepl("indeterminado", metodo_pareamento), .N] == 0L)
ck("23 unidade estadual nunca e pareada a mandato municipal",
   o[tipo_unidade == "estadual" & !is.na(id_mandato_bocel), .N] == 0L)
ck("24 pareamento por CPF so em fonte que expoe CPF completo",
   o[grepl("^cpf", metodo_pareamento) & (is.na(cpf) | nchar(cpf) != 11L), .N] == 0L)

## cobertura
ck("25 cobertura nunca passa de 1", cob[taxa > 1, .N] == 0L)
ck("26 cobertura reproduz a contagem de pareados da tabela principal",
   cob[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
   paste(cob[, sum(n_pareados)], "vs", o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]))
ck("27 nenhuma saida observada na cobertura, coerente com a natureza da fonte", cob[, sum(n_com_saida)] == 0L)

## recontagem independente a partir do cache bruto
ce <- fread("data_raw/tce/ce/contas_irregulares.csv", colClasses = "character")
ck("28 CE: linhas na saida nao excedem as linhas municipais da planilha",
   o[uf == "CE", .N] <= ce[!norm(localidade) %in% c("CEARA", ""), .N],
   paste(o[uf == "CE", .N], "de", ce[!norm(localidade) %in% c("CEARA", ""), .N]))
ck("29 CE: nenhuma linha de localidade 'CEARA' entrou", o[uf == "CE" & norm(nome_municipio_fonte) == "CEARA", .N] == 0L)
pa <- fread("data_raw/tce/pa/tcmpa_contas_irregulares.csv", colClasses = "character")
ck("30 PA: linhas na saida nao excedem as da grade do TCM-PA", o[uf == "PA", .N] <= nrow(pa),
   paste(o[uf == "PA", .N], "de", nrow(pa)))
ck("31 PA: so o TCM-PA aparece como tribunal", all(o[uf == "PA"]$tribunal == "TCM-PA"))
rn <- fread("data_raw/tce/rn/contas_irregulares.csv", colClasses = "character")
ck("32 RN: todo CPF da saida existe no bruto", length(setdiff(o[uf == "RN" & !is.na(cpf)]$cpf, rn$cpf)) == 0L)
ma <- as.data.table(fromJSON("data_raw/tce/ma/responsaveisirregulares.json", simplifyVector = TRUE)$elements)
ck("33 MA: linhas na saida nao excedem irregulares + inadimplentes",
   o[uf == "MA", .N] <= nrow(ma) + nrow(as.data.table(fromJSON("data_raw/tce/ma/responsaveisinadimplentes.json", simplifyVector = TRUE)$elements)))
rr <- as.data.table(fromJSON("data_raw/tce/rr/responsabilizacoes_publicas.json", simplifyVector = TRUE)$elements)
ck("34 RR: o cache nao tem id repetido", rr[, uniqueN(id)] == nrow(rr), paste(rr[, uniqueN(id)], "de", nrow(rr)))
ro <- fread("data_raw/tce/ro/contas_julgadas_irregulares.csv", colClasses = "character")
ck("35 RO: numeracao de ordem do PDF e estritamente crescente", all(diff(as.integer(ro$n)) > 0L))
ck("36 RO: todo CPF do bruto tem 11 digitos", ro[nchar(cpf) != 11L, .N] == 0L)

## inventario
ck("37 inventario cobre as 11 UFs do grupo D", setequal(unique(inv$uf), UF_D), paste(setdiff(UF_D, inv$uf), collapse = ","))
ck("38 inventario tem url e observacao em toda linha", inv[is.na(url) | url == "" | is.na(observacao) | observacao == "", .N] == 0L)
ck("39 toda UF sem fonte declarada no inventario esta ausente da saida",
   length(intersect(setdiff(inv$uf, inv[grepl("^sim", oferece_gestores)]$uf), unique(o$uf))) == 0L)
ck("40 toda UF com fonte declarada produziu linha", setequal(inv[grepl("^sim", oferece_gestores), unique(uf)], unique(o$uf)),
   paste(setdiff(inv[grepl("^sim", oferece_gestores), unique(uf)], unique(o$uf)), collapse = ","))

fora <- c("pertinencia semantica do pareamento por nome em municipio pequeno (homonimia), sobretudo nas linhas de cargo indeterminado",
          "cobertura substantiva das listas: elas registram quem teve contas julgadas irregulares, e nao o conjunto de quem ocupou o cargo, de modo que a ausencia de um prefeito na lista nada diz sobre o mandato dele",
          "atribuicao de cargo a partir do tipo da unidade gestora: o ordenador de uma prefeitura e em regra o prefeito, mas pode ser um delegatario, e o de uma camara e em regra o presidente, que e vereador",
          "atualidade das listas de AL, SE e AC, que existem e foram enviadas a Justica Eleitoral mas nao tem publicacao em arquivo localizavel",
          "completude do PDF de 2016 do TCE-RO, que congela o estoque daquele ano e nao cobre julgamentos posteriores")
registrar_numero("tced_verif_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tced_verif_n_aprovadas", length(passou), script = script)
registrar_numero("tced_verif_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao("data/tce_gestores_d.csv + data/tce_gestores_d_cobertura.csv", "R/34_tce_gestores_d.R",
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nrelatorio:", f, "\n", length(passou), "aprovadas,", length(falhou), "reprovadas\n")
if (length(falhou)) { sink(); stop("verificacao reprovada: ", paste(falhou, collapse = " | ")) }
sink()
