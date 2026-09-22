# verifica_camaras_sem_sapl.R — verificacao deterministica de data/exercicio_camaras_sem_sapl.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_camaras_sem_sapl.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
if (!file.exists("data/exercicio_camaras_sem_sapl.csv")) { cat("exercicio_camaras_sem_sapl.csv ainda nao existe: verificacao adiada\n"); quit(status = 0) }
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/20_exercicio_camaras_sem_sapl.R"
passou <- character(); falhou <- character()
chk <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg); invisible(cond) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")

ex <- fread("data/exercicio_camaras_sem_sapl.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/exercicio_camaras_sem_sapl_cobertura.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
sapl <- fread("data_raw/sapl_municipal/inventario_sapl_municipal.csv", colClasses = "character")
cols <- c("sg_ue", "id_municipio_ibge", "uf", "dominio", "legislatura_numero", "legislatura_inicio", "legislatura_fim", "ano_eleicao_bocel",
          "nome_fonte", "nome_parlamentar", "nome_normalizado", "titular", "data_inicio_mandato", "data_fim_mandato", "tipo_afastamento",
          "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento", "sistema", "so_legislatura_atual", "url")
chk(all(cols %in% names(ex)), "colunas esperadas presentes")
chk(nrow(ex) > 0, "arquivo nao vazio")
chk(all(ex$forma_saida %in% VOCAB), "forma_saida no vocabulario fechado")
chk(!any(ex$sg_ue %in% sapl[responde_api == "TRUE", sg_ue]), "nenhum municipio ja coberto pelo SAPL (nao repetir)")
p <- ex[!is.na(id_mandato_bocel)]
chk(all(p$id_mandato_bocel %in% mand$id_mandato), "todo id_mandato_bocel existe em mandatos.csv")
chk(!any(duplicated(p$id_mandato_bocel)), "id_mandato_bocel unico por linha")
m <- merge(p[, .(id_mandato_bocel, sg_ue, ano_eleicao_bocel)], mand[, .(id_mandato, unidade_posicao, ano_eleicao, cd_cargo)], by.x = "id_mandato_bocel", by.y = "id_mandato")
chk(all(m$cd_cargo == "13"), "pareados sao vereadores (cd_cargo 13)")
chk(all(m$sg_ue == m$unidade_posicao & m$ano_eleicao_bocel == m$ano_eleicao), "pareado no mesmo municipio e mesma eleicao")
chk(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", na.omit(ex$data_inicio_mandato))), "data_inicio_mandato ISO")
chk(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", na.omit(ex$data_fim_mandato))), "data_fim_mandato ISO")
chk(all(as.integer(ex$ano_eleicao_bocel) %in% seq(1996L, 2024L, 4L)), "ano_eleicao_bocel no ciclo municipal")
chk(all(p$metodo_pareamento %in% c("nome_completo", "nome_fonte=nome_urna", "tokens_nome_fonte_no_nome_civil", "tokens_nome_fonte_no_nome_de_urna")), "metodo_pareamento registrado")
chk(all(as.numeric(cob$taxa) >= 0 & as.numeric(cob$taxa) <= 1, na.rm = TRUE), "taxa de cobertura em [0,1]")
chk(all(as.integer(cob$n_pareados) <= as.integer(cob$n_mandatos_bocel)), "pareados <= mandatos por UF-ano")
num <- grep("^csem_n_mandatos_pareados \\|", readLines("output/numeros_assinatura.txt", warn = FALSE), value = TRUE)
num <- if (length(num)) as.integer(trimws(strsplit(num[length(num)], "\\|")[[1]][2])) else NA_integer_
chk(!is.na(num) && num == uniqueN(p$id_mandato_bocel), "csem_n_mandatos_pareados registrado bate com o arquivo")
cat("passou:", length(passou), "falhou:", length(falhou), "\n"); if (length(falhou)) print(falhou)
gravar_relatorio_verificacao("data/exercicio_camaras_sem_sapl.csv", script, passou = passou, falhou = falhou,
  fora_de_cobertura = c("pertinencia do pareamento por tokens (homonimos no mesmo municipio-eleicao)",
                        "completude das listas dos portais (sistemas que so publicam a legislatura atual: so_legislatura_atual)",
                        "erro de digitacao de datas e situacoes nos portais das camaras"))
if (length(falhou)) quit(status = 1)
