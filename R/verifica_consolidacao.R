# verifica_consolidacao.R — recontagem dos totais de data/mandatos.csv (mandatos, formas de saida por
# esfera, fontes da forma de saida, exercicio confirmado, substituicao inferida pela MUNIC) contra o
# ULTIMO registro de cada chave em output/numeros_assinatura.txt e contra
# output/verificacao/forma_saida_por_esfera.csv.
#
# Historico. Escrito em 28/08/2026 como recontagem datada para o relatorio de verificacao consolidado;
# em 05/09/2026 generalizado: le sempre o registro corrente (nao guarda numero de 28/08), acumula cada
# checagem em passou/falhou em vez de abortar na primeira, abre log e grava o relatorio JSON padrao da
# verificacao, para que a execucao deixe evidencia datada dentro de output/verificacao. As chaves vcons_*
# sao regravadas a cada execucao e a leitura correta e sempre a ultima linha de cada chave.
# Entrada: data/mandatos.csv, output/numeros_assinatura.txt, output/verificacao/forma_saida_por_esfera.csv
# Saida:   logs/verifica_consolidacao.log, output/verificacao/relatorio_verificacao_<ts>.json, chaves vcons_*
# Execucao: Rscript --vanilla R/verifica_consolidacao.R  (a partir da raiz do repositorio)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_consolidacao.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- "logs/verifica_consolidacao.log"; sink(logf, split = TRUE)
cat("verifica_consolidacao.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(k, v, script = script)
passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { cat("FALHA:", nome, "—", conditionMessage(e), "\n"); FALSE })
  if (r) { passou <<- c(passou, nome); cat("OK:", nome, "\n") } else falhou <<- c(falhou, nome)
  invisible(r)
}

mand <- fread("data/mandatos.csv", colClasses = "character")
ok("id_mandato unico", checa_unica(as.data.frame(mand), "id_mandato"))
ok("esfera em federal/estadual/municipal", in_set(mand$esfera, c("federal", "estadual", "municipal"), nome = "esfera"))
ok("ano_eleicao em 1998..2024", em_faixa(as.integer(mand$ano_eleicao), 1998, 2024, nome = "ano_eleicao"))

# ultimo registro por chave (le por linha: ha valores com '|' no meio)
ass_l <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass_l <- ass_l[grepl("|", ass_l, fixed = TRUE)]
ass <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass_l)), valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass_l)))
ass <- ass[, .SD[.N], by = chave]
ult <- function(k) { v <- ass[chave == k, valor]; if (!length(v)) NA_character_ else v }

fs <- mand[, .N, by = .(esfera, forma_saida)]
fte <- mand[!is.na(fonte_forma_saida) & fonte_forma_saida != "", .N, by = .(esfera, fonte_forma_saida)]
obs_est <- mand[esfera == "estadual" & forma_saida != "nao_observado", .N]
obs_tot <- mand[forma_saida != "nao_observado", .N]
n_exerc <- mand[!is.na(exercicio_confirmado) & exercicio_confirmado != "", .N]
n_munic <- mand[forma_saida == "substituicao_inferida_munic", .N]
n_munic_sem_data <- mand[forma_saida == "substituicao_inferida_munic" &
                         (is.na(data_fim_efetiva) | data_fim_efetiva == ""), .N]
n_ibge_sem_exerc <- mand[grepl("ibge_munic", fonte_exercicio) &
                         (is.na(exercicio_confirmado) | exercicio_confirmado == ""), .N]
n_naoobs_com_fonte <- mand[forma_saida == "nao_observado" &
                           !is.na(fonte_forma_saida) & fonte_forma_saida != "", .N]

reg("vcons_n_mandatos", nrow(mand))
reg("vcons_forma_saida_observada_total", obs_tot)
reg("vcons_forma_saida_observada_estadual", obs_est)
reg("vcons_n_exercicio_confirmado", n_exerc)
reg("vcons_n_substituicao_inferida_munic", n_munic)
reg("vcons_n_substituicao_munic_sem_data_fim", n_munic_sem_data)
reg("vcons_n_ibge_munic_sem_exercicio_confirmado", n_ibge_sem_exerc)
reg("vcons_n_nao_observado_com_fonte", n_naoobs_com_fonte)
for (i in seq_len(nrow(fs))) reg(sprintf("vcons_forma_saida_%s_%s", fs$esfera[i], fs$forma_saida[i]), fs$N[i])
for (i in seq_len(nrow(fte))) reg(sprintf("vcons_fonte_%s_%s", fte$esfera[i], fte$fonte_forma_saida[i]), fte$N[i])

ok("substituicao inferida pela MUNIC sempre com data_fim_efetiva", stopifnot(n_munic_sem_data == 0))
ok("fonte ibge_munic sempre com exercicio_confirmado", stopifnot(n_ibge_sem_exerc == 0))
ok("nao_observado nunca com fonte de forma de saida", stopifnot(n_naoobs_com_fonte == 0))
# comparacao com o registro valido (ultimo valor de cada chave)
comp <- list(
  c("bocel_n_mandatos", nrow(mand)),
  c("bocel_mandatos_com_exercicio_confirmado", n_exerc))
for (cc in comp) {
  v <- ult(cc[1]); cat(sprintf("%s: registro=%s recontado=%s\n", cc[1], v, cc[2]))
  ok(sprintf("%s registrado = recontado", cc[1]), stopifnot(is.na(v) || as.numeric(v) == as.numeric(cc[2])))
}
# forma_saida_por_esfera.csv coincide com a recontagem
ref <- fread("output/verificacao/forma_saida_por_esfera.csv")
m <- merge(ref, fs, by = c("esfera", "forma_saida"), all = TRUE)
ok("forma_saida_por_esfera.csv coincide com mandatos.csv", stopifnot(nrow(m) == nrow(fs), all(m$N.x == m$N.y)))
cat("forma_saida_por_esfera.csv comparado com mandatos.csv em", nrow(m), "celulas\n")
print(fs[order(esfera, -N)]); print(fte[order(esfera, -N)])
cat("exercicio_confirmado:", n_exerc, "| observadas:", obs_tot, "| estadual observadas:", obs_est, "\n")

reg("vcons_n_checks_passaram", length(passou)); reg("vcons_n_checks_falharam", length(falhou))
gravar_relatorio_verificacao(alvo = "data/mandatos.csv", script = script, passou = passou, falhou = falhou,
                             fora_de_cobertura = c("veracidade da forma de saida e da data de cada fonte: fica com o verificador de cada frente",
                                                   "esta recontagem le o registro corrente; o valor de uma chave vcons_* so vale na ultima linha dela"))
cat("\nverifica_consolidacao:", length(passou), "checks ok;", length(falhou), "falharam\n")
if (length(falhou)) { cat(paste("-", falhou), sep = "\n"); sink(); quit(status = 1) }
sink()
