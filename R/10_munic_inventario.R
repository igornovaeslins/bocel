# 10_munic_inventario.R — levantamento das variaveis do bloco "prefeito(a) em exercicio" na MUNIC/IBGE
# Entrada:  data_raw/munic/<ano>/ (baixado por python/fetch_munic.py) + data_raw/munic/instrumentos/
# Saida:    data_raw/munic/inventario_variaveis_munic.csv (uma linha por edicao da MUNIC)
#           data_raw/munic/inventario_abas_munic.csv (todas as abas de cada base, para auditoria)
# Execucao: Rscript --vanilla R/10_munic_inventario.R
# Regra: le o dicionario de cada base e localiza, por edicao, as colunas de nome, sexo, idade,
# escolaridade, partido e "estava em exercicio no ano anterior". O que nao esta na base e lacuna.
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringi)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
mdir <- file.path(root, "data_raw", "munic")
logf <- file.path(root, "logs", "10_munic_inventario.log")
sink(logf, split = TRUE)
cat("10_munic_inventario:", format(Sys.time()), "\n")

bases <- c(
  `2004` = "2004/Base 2004.xls", `2005` = "2005/Base 2005.xls", `2006` = "2006/Base 2006.xls",
  `2008` = "2008/Base 2008 corrigida em 26 12..xls", `2009` = "2009/base.xls",
  `2011` = "2011/base_MUNIC_xls_2011.xls", `2012` = "2012/base.xls", `2013` = "2013/base.xls",
  `2014` = "2014/munic2014.xls", `2015` = "2015/Base_MUNIC_2015.xls", `2017` = "2017/Base MUNIC 2017.xls",
  `2018` = "2018/Base_MUNIC_2018_xlsx_20201103.xlsx", `2019` = "2019/Base_MUNIC_2019_20210817.xlsx",
  `2020` = "2020/Base_MUNIC_2020.xlsx", `2021` = "2021/Base_MUNIC_2021_20240425.xlsx",
  `2023` = "2023/Base_MUNIC_2023.xlsx", `2024` = "2024/Base_MUNIC_2024_20251107.xlsx")
for (b in bases) stopifnot(file.exists(file.path(mdir, b)))

norm <- function(x) stri_trans_general(tolower(x), "Latin-ASCII")

## ---------------------------------------------------------------- dicionarios
ler_dic <- function(f) {
  d <- as.data.table(suppressMessages(read_excel(f, sheet = "Dicionário", col_names = FALSE)))
  txt <- apply(d, 1, function(r) paste(na.omit(as.character(r)), collapse = " | "))
  data.table(linha = seq_along(txt), texto = txt)
}
abas <- rbindlist(lapply(names(bases), function(y) {
  data.table(ano_munic = as.integer(y), aba = excel_sheets(file.path(mdir, bases[y])))
}))
fwrite(abas, file.path(mdir, "inventario_abas_munic.csv"))

# localiza o bloco do prefeito: a aba dedicada (Estrutura politica / Informacoes [atual] prefeito)
# e, no dicionario, as linhas do bloco; variaveis "Sexo/Idade" fora do bloco sao de gestores setoriais
inv <- rbindlist(lapply(names(bases), function(y) {
  f <- file.path(mdir, bases[y])
  sh <- excel_sheets(f)
  aba <- sh[grepl("prefeito|estrutura pol", norm(sh))]
  dic <- ler_dic(f)
  dic[, t := norm(texto)]
  # janela do bloco: da linha "informacoes sobre o atual prefeito" / "estrutura politica" ate a proxima secao numerada
  ini <- dic[grepl("^1\\. ?informacoes sobre o atual prefeito|^1\\.estrutura politica|^1\\.informacoes sobre o atual prefeito", t) &
             !grepl("titulo link", t), linha]
  # o cabecalho aparece no sumario e de novo no corpo: usa a ultima ocorrencia (corpo)
  ini <- if (length(ini)) ini[length(ini)] else integer(0)
  bloco <- if (length(ini)) {
    fim <- dic[linha > ini & grepl("^2\\.", t), linha]
    dic[linha > ini & linha < (if (length(fim)) fim[1] else ini + 12)]
  } else dic[(grepl("prefeito\\(a\\)|prefeito nome", t) & grepl("sexo|idade|escolaridade|partido|nome|exercicio|cor/raca", t)) |
             grepl("^(sexo|idade|escolaridade) \\| mpeg", t)]
  pega <- function(rx) { v <- bloco[grepl(rx, t), texto]; if (length(v)) paste(v, collapse = " ;; ") else NA_character_ }
  cod <- function(rx) { v <- bloco[grepl(rx, t), texto]; if (!length(v)) return(NA_character_)
    m <- regmatches(v, regexpr("(A[0-9]+|M[Pp][Ee][Gg][0-9]+)", v)); if (length(m)) paste(m, collapse = ";") else NA_character_ }
  data.table(
    ano_munic = as.integer(y), arquivo = bases[y],
    aba_prefeito = if (length(aba)) aba[1] else NA_character_,
    tem_bloco_prefeito = length(aba) > 0,
    var_nome = cod("prefeito nome|nome do\\(a\\) prefeito"),
    var_exercicio_ano_anterior = cod("estava em exercicio"),
    var_sexo = cod("^sexo( do\\(a\\) prefeito\\(a\\))? \\|"),
    var_idade = cod("^idade( do\\(a\\) prefeito\\(a\\))? \\|"),
    var_escolaridade = cod("^escolaridade"),
    var_partido_eleito = cod("partido pelo qual"),
    var_partido_atual = cod("partido atual|prefeito partido atual"),
    var_cor_raca = cod("cor/raca"),
    rotulos_bloco = paste(bloco$texto, collapse = " ;; ")
  )
}))
inv[, `:=`(tem_nome = !is.na(var_nome), tem_sexo = !is.na(var_sexo), tem_idade = !is.na(var_idade),
           tem_partido = !is.na(var_partido_eleito) | !is.na(var_partido_atual))]

## ---------------------------------------------------------------- edicoes sem base de microdados
# 1999: sem pasta no FTP (HTTP 404 em 2026-08-28; ver data_raw/munic/manifest_munic.csv).
# 2001 e 2002: o FTP traz apenas tabelas agregadas (sexo, grupo de idade, instrucao, partido do
# prefeito por UF e classe de populacao), sem microdado por municipio.
sem_base <- data.table(
  ano_munic = c(1999L, 2001L, 2002L),
  arquivo = c(NA_character_, "2001/Tabelas_2001.zip (tabela02-04.xls, agregadas)", "2002/Tabelas_2002.zip (Tab001-003.xls, agregadas)"),
  aba_prefeito = NA_character_, tem_bloco_prefeito = c(NA, TRUE, TRUE),
  var_nome = NA_character_, var_exercicio_ano_anterior = NA_character_, var_sexo = NA_character_,
  var_idade = NA_character_, var_escolaridade = NA_character_, var_partido_eleito = NA_character_,
  var_partido_atual = NA_character_, var_cor_raca = NA_character_,
  rotulos_bloco = c("base nao disponivel no FTP do IBGE (404) nem na API de downloads (500)",
                    "tabelas agregadas: sexo e grupos de idade do prefeito (tab. 2), nivel de instrucao (tab. 3), reeleicao e partido atual (tab. 4); sem microdado",
                    "tabelas agregadas: sexo e grupos de idade do prefeito (tab. 1), nivel de instrucao (tab. 2), exercicio em 2001 e partido atual (tab. 3); sem microdado"),
  tem_nome = FALSE, tem_sexo = FALSE, tem_idade = FALSE, tem_partido = FALSE)
inv <- rbindlist(list(sem_base, inv), use.names = TRUE)
inv[, microdado_disponivel := !ano_munic %in% c(1999L, 2001L, 2002L)]

## ---------------------------------------------------------------- data de referencia
# As publicacoes da MUNIC informam que "os dados coletados referiram-se, de maneira geral, a data
# da entrevista" e o periodo de coleta; lido de munic2011/2012/2013.pdf (data_raw/munic/instrumentos).
# Para as demais edicoes, a base nao traz a data e a publicacao nao foi localizada no FTP:
# fica registrado o ano da pesquisa como referencia (lacuna de mes).
ref <- data.table(
  ano_munic = c(2011L, 2012L, 2013L),
  periodo_coleta = c("julho a dezembro de 2011", "maio a dezembro de 2012", "marco a novembro de 2013"),
  fonte_periodo = c("munic2011.pdf, Notas tecnicas", "munic2012.pdf, Notas tecnicas", "munic2013.pdf, Notas tecnicas"))
inv <- merge(inv, ref, by = "ano_munic", all.x = TRUE)
inv[is.na(periodo_coleta), `:=`(periodo_coleta = "nao localizado (dado refere-se a data da entrevista, no ano da pesquisa)",
                                fonte_periodo = "lacuna")]
inv[, data_referencia := ifelse(ano_munic == 2013L, "2013-03/2013-11",
                          ifelse(ano_munic == 2012L, "2012-05/2012-12",
                          ifelse(ano_munic == 2011L, "2011-07/2011-12", paste0(ano_munic, " (ano da entrevista)"))))]
inv[, eleicao_de_origem_bocel := ((ano_munic - 1L) %/% 4L) * 4L]  # mandato vigente: eleito na municipal anterior
setorder(inv, ano_munic)
setcolorder(inv, c("ano_munic", "microdado_disponivel", "tem_bloco_prefeito", "tem_nome", "tem_sexo", "tem_idade",
                   "tem_partido", "data_referencia", "periodo_coleta", "eleicao_de_origem_bocel"))
fwrite(inv, file.path(mdir, "inventario_variaveis_munic.csv"), na = "NA")
print(inv[, .(ano_munic, microdado_disponivel, tem_bloco_prefeito, tem_nome, tem_sexo, tem_idade, tem_partido,
              var_nome, var_sexo, var_idade, var_partido_eleito, var_partido_atual)])
cat("\n10_munic_inventario: concluido;", nrow(inv), "edicoes inventariadas\n")
sink()
