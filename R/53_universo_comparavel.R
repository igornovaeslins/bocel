# 53_universo_comparavel.R — marca onde a forma de saida pode ser comparada
#
# Escrito em 31/08/2026. O problema e que a forma de saida tem
# 88,7% de cobertura no federal e 10,8% no municipal, e quem cruzar isso conclui que deputado
# federal renuncia mais do que vereador, quando o que varia e o portal existir. A medicao mostrou
# que a desigualdade nao desaparece com mais coleta, porque 2.140 das 3.374 camaras alcancadas so
# publicam de 2020 em diante e a mediana de eleicoes cobertas por casa e uma.
#
# A solucao nao e esconder a variavel, e sim delimitar onde ela vale. Uma unidade-eleicao entra no
# universo comparavel quando ha fonte INSTITUCIONAL cobrindo aquela casa naquele periodo, isto e,
# quando alguem de fato olhou. Dentro dele a ausencia de saida observada significa que a pessoa
# terminou o mandato, e o denominador e honesto. Fora dele a ausencia significa que ninguem olhou.
#
# Fonte institucional e o registro da propria casa ou do orgao de controle (API da Camara e do
# Senado, SAPL, portal da camara, portais e memoriais das assembleias, tribunais de contas).
# Enciclopedia, Wikidata, diario oficial, MUNIC, suplementar e deducao do proprio banco confirmam
# eventos, mas nao estabelecem que a casa foi varrida, e por isso nao abrem universo.
#
# Entrada: data/mandatos.csv
# Saida:   data/mandatos.csv (colunas universo_comparavel e motivo_sem_saida), data/cobertura_saida.csv,
#          output/data_referencia.txt (data fixa da reconstrucao)
set.seed(20260831)
suppressPackageStartupMessages({library(data.table); library(arrow)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root,"lib","proveniencia.R"))
ESTE <- file.path(root,"R","53_universo_comparavel.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)

# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
INSTITUCIONAL <- c("camara_api","senado_api","camara_biografia","fonte_oficial_curada","assembleia_api","assembleia_portal",
                   "assembleia_historico","sapl_municipal","portal_camara","tce")
m <- fread("data/mandatos.csv")
m[, inst := fonte_forma_saida %in% INSTITUCIONAL]
# a unidade da varredura e a casa: o municipio nos cargos municipais, a UF nos estaduais, e a
# propria casa nacional no federal
m[, unidade_varredura := fcase(esfera == "municipal", paste0("M", sg_ue),
                               esfera == "estadual", paste0("E", sg_uf),
                               default = paste0("F", cd_cargo))]
m[, universo_comparavel := any(inst), by = .(unidade_varredura, ano_eleicao)]
m[, c("inst", "unidade_varredura") := NULL]   # auxiliares, nao entram na tabela publicada

reg("unc_mandatos_no_universo_comparavel", m[universo_comparavel == TRUE, .N])
reg("unc_pct_no_universo_comparavel", round(100 * mean(m$universo_comparavel), 1))
for (e in c("federal","estadual","municipal")) {
  reg(paste0("unc_universo_", e), m[esfera == e & universo_comparavel == TRUE, .N])
  reg(paste0("unc_pct_universo_", e), round(100 * m[esfera == e, mean(universo_comparavel)], 1))
  reg(paste0("unc_saida_no_universo_", e),
      round(100 * m[esfera == e & universo_comparavel == TRUE, mean(forma_saida != "nao_observado")], 1))
}
cat("=== cobertura da forma de saida DENTRO e FORA do universo comparavel ===\n")
print(m[, .(mandatos = .N,
            no_universo = sum(universo_comparavel),
            pct_universo = round(100 * mean(universo_comparavel), 1),
            saida_no_universo = round(100 * mean(forma_saida[universo_comparavel] != "nao_observado"), 1),
            saida_geral = round(100 * mean(forma_saida != "nao_observado"), 1)),
        by = esfera][order(-mandatos)])

## ---------------------------------------------------------- tabela de cobertura publicada
cob <- m[, .(cadeiras = .N,
             no_universo_comparavel = sum(universo_comparavel),
             com_forma_saida = sum(forma_saida != "nao_observado"),
             com_forma_saida_no_universo = sum(forma_saida != "nao_observado" & universo_comparavel),
             com_data_posse = sum(!is.na(data_posse)),
             com_exercicio = sum(!is.na(exercicio_confirmado))),
         by = .(esfera, cargo, sg_uf, ano_eleicao)]
cob[, `:=`(pct_universo = round(100 * no_universo_comparavel / cadeiras, 1),
           pct_saida = round(100 * com_forma_saida / cadeiras, 1),
           pct_saida_no_universo = fifelse(no_universo_comparavel > 0,
                                           round(100 * com_forma_saida_no_universo / no_universo_comparavel, 1),
                                           NA_real_))]
setorder(cob, esfera, cargo, sg_uf, ano_eleicao)
fwrite(cob, "data/cobertura_saida.csv", quote = TRUE, na = "NA")
reg("unc_linhas_tabela_cobertura", nrow(cob))

## ---------------------------------------------------------- chave de juncao com outras bases
# O SQ_CANDIDATO do TSE so e unico nacionalmente a partir de 2010 (em 2000 ha 66.976 repeticoes em
# 68.067 linhas). Quem juntar por ele nos anos antigos cola registro errado, e por isso o banco
# entrega a chave composta pronta, que e unica em 100% das linhas em toda a serie.
# ---------------------------------------------------------------- motivo da ausencia de saida
# 06/09/2026. A coluna forma_saida usava 'nao_observado' para duas coisas opostas.
# Dentro do universo comparavel a ausencia significa que a pessoa terminou o mandato, porque a casa
# foi varrida; fora dele significa que ninguem olhou. Quem contasse a categoria somava as duas.
# A coluna nova e derivada, sem fonte nem julgamento novo, e sai de universo_comparavel, mandato_fim
# e forma_saida. A data de referencia e fixa e gravada, para a reconstrucao em outro dia nao mudar
# o resultado (o mandato em curso hoje nao pode virar encerrado amanha).
DATA_REFERENCIA <- Sys.getenv("BOCEL_DATA_REFERENCIA", unset = "")
if (!nzchar(DATA_REFERENCIA)) {
  f_ref <- "output/data_referencia.txt"
  DATA_REFERENCIA <- if (file.exists(f_ref)) trimws(readLines(f_ref, warn = FALSE)[1]) else format(Sys.Date(), "%Y-%m-%d")
  if (!file.exists(f_ref)) writeLines(DATA_REFERENCIA, f_ref)
}
stopifnot(grepl("^\\d{4}-\\d{2}-\\d{2}$", DATA_REFERENCIA))
reg("unc_data_referencia", DATA_REFERENCIA)
m[, motivo_sem_saida := fcase(
  forma_saida != "nao_observado", "nao_se_aplica",
  mandato_fim > DATA_REFERENCIA, "em_curso",
  universo_comparavel == TRUE, "fim_regular_presumido",
  default = "fonte_ausente")]
stopifnot(all(m$motivo_sem_saida %in% c("nao_se_aplica", "em_curso", "fim_regular_presumido", "fonte_ausente")))
stopifnot(m[(forma_saida != "nao_observado") != (motivo_sem_saida == "nao_se_aplica"), .N] == 0)
stopifnot(m[motivo_sem_saida == "fim_regular_presumido" & universo_comparavel != TRUE, .N] == 0)
for (k in c("nao_se_aplica", "em_curso", "fim_regular_presumido", "fonte_ausente"))
  reg(paste0("unc_motivo_", k), m[motivo_sem_saida == k, .N])
print(m[, .N, by = motivo_sem_saida][order(-N)])

m[, chave_tse := paste(ano_eleicao, cd_cargo, sg_ue, nr_candidato, sep = "_")]
stopifnot(uniqueN(m$chave_tse) == nrow(m))
reg("unc_chave_tse_unica", uniqueN(m$chave_tse))

salvar <- function(dt, nome) {
  fwrite(dt, file.path("data", paste0(nome, ".csv")), na = "NA", quote = TRUE)
  write_parquet(dt, file.path("data", paste0(nome, ".parquet")))
  saveRDS(dt, file.path("data", paste0(nome, ".rds")))
}
salvar(m, "mandatos")
cat("\n53_universo_comparavel: concluido |", m[universo_comparavel == TRUE, .N], "mandatos no universo |",
    nrow(cob), "linhas de cobertura\n")
