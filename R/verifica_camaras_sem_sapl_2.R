# verifica_camaras_sem_sapl_2.R — verificacao da SEGUNDA rodada de coletores nas camaras sem SAPL.
# Re-executa as contagens de R/25 a partir dos arquivos gravados e confere as invariantes; nao corrige nada.
# Entrada:  data/exercicio_camaras_sem_sapl_2.csv, data/exercicio_camaras_sem_sapl_2_cobertura.csv,
#           data/exercicio_camaras_sem_sapl.csv, data/mandatos.csv, data/pessoas.csv,
#           data_raw/camaras_sem_sapl_2/inventario_familias.csv
# Saida:    output/verificacao/camaras_sem_sapl_2_*.csv, relatorio_verificacao_*.json
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_camaras_sem_sapl_2.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
if (!file.exists("data/exercicio_camaras_sem_sapl_2.csv")) { cat("exercicio_camaras_sem_sapl_2.csv nao existe (frente interrompida): verificacao adiada\n"); quit(status = 0) }
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_camaras_sem_sapl_2.R"
logf <- file("logs/verifica_camaras_sem_sapl_2.log", open = "wt"); sink(logf, split = TRUE)
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
passou <- character(); falhou <- character(); fora <- character()
chk <- function(nome, ok) { if (isTRUE(ok)) passou <<- c(passou, nome) else falhou <<- c(falhou, nome); cat(ifelse(isTRUE(ok), "  ok  ", " FALHA "), nome, "\n") }

r2 <- fread("data/exercicio_camaras_sem_sapl_2.csv", colClasses = "character", na.strings = "NA")
r1 <- fread("data/exercicio_camaras_sem_sapl.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/exercicio_camaras_sem_sapl_2_cobertura.csv")
inv <- fread("data_raw/camaras_sem_sapl_2/inventario_familias.csv", colClasses = "character")

chk("1. colunas identicas as da rodada 1", identical(names(r1), names(r2)))
chk("2. base nao vazia", nrow(r2) > 0)
chk("3. forma_saida no vocabulario fechado", all(r2$forma_saida %in% VOCAB))
chk("4. datas em ISO ou NA", all(is.na(r2$data_inicio_mandato) | grepl("^\\d{4}-\\d{2}-\\d{2}$", r2$data_inicio_mandato)) &&
                              all(is.na(r2$data_fim_mandato) | grepl("^\\d{4}-\\d{2}-\\d{2}$", r2$data_fim_mandato)) &&
                              all(is.na(r2$legislatura_inicio) | grepl("^\\d{4}-\\d{2}-\\d{2}$", r2$legislatura_inicio)))
chk("5. legislatura_inicio <= legislatura_fim", r2[!is.na(legislatura_inicio) & !is.na(legislatura_fim) & legislatura_inicio > legislatura_fim, .N] == 0)
chk("6. nenhum nome_fonte vazio", r2[is.na(nome_fonte) | nchar(trimws(nome_fonte)) < 3, .N] == 0)
rod1 <- inv[coletado_rodada1 == "TRUE", unique(sg_ue)]
chk("7. nenhum municipio da rodada 1 reaparece", r2[sg_ue %in% rod1, .N] == 0)
chk("8. id_mandato_bocel sem repeticao", r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)] == r2[!is.na(id_mandato_bocel), .N])
chk("9. ano_eleicao_bocel no ciclo 1996-2024", all(as.integer(r2$ano_eleicao_bocel) %in% seq(1996, 2024, 4)))
chk("10. so_legislatura_atual implica eleicao de 2024", r2[so_legislatura_atual == "TRUE" & ano_eleicao_bocel != "2024", .N] == 0)

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "13", .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano_eleicao)]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")[, .(id_pessoa, nome)]
par <- merge(r2[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE, suffixes = c("", "_bocel"))
chk("11. todo id_mandato_bocel existe no BOCEL como vereador", par[is.na(ano_eleicao), .N] == 0)
chk("12. municipio do par bate com o do BOCEL", par[sg_ue != sg_ue_bocel, .N] == 0)
chk("13. eleicao do par bate com a do BOCEL", par[ano_eleicao_bocel != ano_eleicao, .N] == 0)
chk("14. id_pessoa do par bate com o do mandato", par[id_pessoa_bocel != id_pessoa, .N] == 0)
par <- merge(par, pess, by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
chk("15. metodo nome_completo tem nome civil identico", par[metodo_pareamento == "nome_completo" & nome_normalizado != norm(nome), .N] == 0)
tok_in <- function(a, b) mapply(function(x, y) { x <- x[nchar(x) >= 3]; length(x) >= 1 && all(x %in% y) },
                                strsplit(a, " "), strsplit(b, " "))
sub3 <- par[metodo_pareamento == "tokens_nome_fonte_no_nome_civil"]
chk("16. metodo por tokens no nome civil de fato contem os tokens",
    nrow(sub3) == 0 || all(tok_in(norm(fcoalesce(sub3$nome_parlamentar, sub3$nome_fonte)), norm(sub3$nome))))

## o registro tem linhas de texto livre com "|" no meio (chaves de vocabulario), que truncam o fread por
## contagem de campos: le linha a linha e fica com os dois primeiros campos, sem descartar o resto do arquivo
lin <- readLines("output/numeros_assinatura.txt", warn = FALSE)
lin <- lin[grepl("\\|", lin)]
reg <- data.table(chave = trimws(sub("\\|.*$", "", lin)),
                  valor = trimws(sapply(strsplit(lin, "|", fixed = TRUE), function(x) if (length(x) >= 2) x[2] else NA_character_)))
ult <- function(k) { v <- reg[chave == k, valor]; if (length(v)) as.numeric(v[length(v)]) else NA_real_ }
chk("17. csem2_n_linhas_coletadas bate com a recontagem", ult("csem2_n_linhas_coletadas") == nrow(r2))
chk("18. csem2_n_camaras_coletadas bate com a recontagem", ult("csem2_n_camaras_coletadas") == r2[, uniqueN(sg_ue)])
chk("19. csem2_n_mandatos_pareados bate com a recontagem", ult("csem2_n_mandatos_pareados") == r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
chk("20. cobertura soma o mesmo total de pareados", sum(cob$n_pareados) == r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
chk("21. taxa da cobertura = pareados / mandatos do BOCEL", all(abs(cob$taxa - round(cob$n_pareados / cob$n_mandatos_bocel, 4)) < 1e-9))
chk("22. nenhuma taxa acima de 1", all(cob$taxa <= 1))
sob <- r2[!is.na(id_mandato_bocel) & id_mandato_bocel %in% r1[!is.na(id_mandato_bocel), id_mandato_bocel], .N]
chk("23. rodada 2 nao repete mandato ja pareado na rodada 1", sob == 0)

## amostras para inspecao manual (pertinencia do pareamento fica fora de cobertura)
set.seed(20260827)
amo <- par[sample(.N, min(40, .N))][, .(uf, sg_ue, ano_eleicao_bocel, sistema, nome_fonte, nome_parlamentar, nome_bocel = nome,
                                        metodo_pareamento, forma_saida, tipo_afastamento, id_mandato_bocel, url)]
fwrite(amo, "output/verificacao/camaras_sem_sapl_2_amostra_40_pares.csv")
nao <- r2[is.na(id_mandato_bocel)][sample(.N, min(40, .N))][, .(uf, sg_ue, ano_eleicao_bocel, sistema, nome_fonte, url)]
fwrite(nao, "output/verificacao/camaras_sem_sapl_2_amostra_40_nao_pareadas.csv")
fwrite(r2[, .(camaras = uniqueN(sg_ue), linhas = .N, pareadas = sum(!is.na(id_mandato_bocel))), by = sistema][order(-pareadas)],
       "output/verificacao/camaras_sem_sapl_2_por_sistema.csv")

fora <- c("pertinencia semantica do pareamento por tokens (apelido x nome civil)",
          "completude das listas publicadas pelos portais (a fonte pode omitir legislaturas ou suplentes)",
          "veracidade da situacao declarada pelo portal (motivo de substituicao nao e ato oficial)",
          "ruido residual da extracao generica: texto de pagina que passa no filtro de nome e nao e parlamentar",
          "nomes cujo sobrenome e 'Camara' sao descartados pelo filtro de ruido")
cat("\npassou:", length(passou), " falhou:", length(falhou), "\n")
f <- gravar_relatorio_verificacao("data/exercicio_camaras_sem_sapl_2.csv", "R/25_exercicio_camaras_sem_sapl_2.R",
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("relatorio:", f, "\n")
registrar_numero("csem2_verificacao_n_checagens_aprovadas", length(passou), script = script)
registrar_numero("csem2_verificacao_n_checagens_reprovadas", length(falhou), script = script)
if (length(falhou)) quit(status = 1)
sink()
