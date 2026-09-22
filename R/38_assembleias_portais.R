#!/usr/bin/env Rscript
# Junta as coletas por UF de data/assembleias2/<UF>.csv numa tabela unica, que R/10 consome como
# a fonte 'assembleia_portal'. Escrito em 29/08/2026, quando a medida por UF mostrou que 17 das 27
# casas nao tinham nenhum registro proprio de saida no banco (docs/CONCORDANCIA_FONTES.md e
# output/descritivas/cobertura_por_uf.csv). Uma frente por UF escreveu seu proprio arquivo, e este
# script apenas concatena, valida o esquema comum e mede a cobertura.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")

arqs <- list.files("data/assembleias2", pattern = "^[A-Z]{2}\\.csv$", full.names = TRUE)
if (!length(arqs)) { cat("38_assembleias_portais: nenhuma coleta em data/assembleias2, nada a fazer\n"); quit(save = "no") }

le <- function(f) {
  x <- fread(f, colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
  faltando <- setdiff(COLS, names(x))
  if (length(faltando)) { cat("PULADO", basename(f), "sem colunas:", paste(faltando, collapse = ","), "\n"); return(NULL) }
  x[, ..COLS]
}
out <- rbindlist(lapply(arqs, le), use.names = TRUE, fill = FALSE)
if (!nrow(out)) { cat("38_assembleias_portais: nenhuma linha valida\n"); quit(save = "no") }

out[forma_saida %in% "" | is.na(forma_saida), forma_saida := "nao_observado"]
in_set(out$forma_saida, VOCAB, "forma_saida da coleta por portal")
d <- function(x) fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", substr(x, 1, 10)), substr(x, 1, 10), NA_character_)
out[, `:=`(data_inicio_exercicio = d(data_inicio_exercicio), data_fim_exercicio = d(data_fim_exercicio))]
em_faixa(as.integer(out$ano_eleicao), 1998, 2024, "ano_eleicao da coleta por portal")

## nao competir com o que ja existe: linha cujo mandato ja tem registro da propria casa em
## exercicio_assembleias.csv ou no historico entra assim mesmo, e a prioridade fica com R/10
mand <- fread("data/mandatos.csv", na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8")
out[!id_mandato_bocel %in% mand$id_mandato, id_mandato_bocel := NA_character_]
checa_unica(as.data.frame(unique(out[!is.na(id_mandato_bocel) & forma_saida != "nao_observado",
            .(id_mandato_bocel, forma_saida, data_fim_exercicio)])), c("id_mandato_bocel"))

setorder(out, uf, ano_eleicao, nome_normalizado)
fwrite(out, "data/exercicio_assembleias_2.csv", na = "NA")

obs <- out[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)]
cob <- out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)),
               com_forma = sum(forma_saida != "nao_observado" & !is.na(id_mandato_bocel))), by = uf][order(-com_forma)]
fwrite(cob, "data/exercicio_assembleias_2_cobertura.csv")
print(cob, nrows = 30)

registrar_numero("asm2_n_ufs", uniqueN(out$uf))
registrar_numero("asm2_n_linhas", nrow(out))
registrar_numero("asm2_n_pareadas", out[!is.na(id_mandato_bocel), .N])
registrar_numero("asm2_n_mandatos_com_forma", uniqueN(obs$id_mandato_bocel))
for (i in seq_len(nrow(cob))) registrar_numero(paste0("asm2_com_forma_", cob$uf[i]), cob$com_forma[i])
cat("\n38_assembleias_portais: concluido |", uniqueN(out$uf), "UFs |", nrow(out), "linhas |",
    uniqueN(obs$id_mandato_bocel), "mandatos com forma\n")
