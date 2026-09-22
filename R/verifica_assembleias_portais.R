#!/usr/bin/env Rscript
# Verificador da tabela unica de assembleias por portal (data/exercicio_assembleias_2.csv) e da
# sua integracao em data/mandatos.csv pela fonte 'assembleia_portal'. Escrito em 29/08/2026, junto
# com a coleta das 17 casas que nao tinham registro proprio de saida no banco.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente
script <- "R/verifica_assembleias_portais.R"
# 05/09/2026: o verificador nunca deixou evidencia de execucao (sem log, sem relatorio JSON); passa a abrir
# logs/verifica_assembleias_portais.log, a guardar o nome de cada checagem aprovada e a gravar o relatorio padrao.
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- "logs/verifica_assembleias_portais.log"; sink(logf, split = TRUE)
cat("verifica_assembleias_portais.R —", format(Sys.time()), "\n")

pass <- 0L; fail <- 0L; falhas <- character(); aprovadas <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) { pass <<- pass + 1L; aprovadas <<- c(aprovadas, nome) } else fail <<- fail + 1L
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
}

F2 <- "data/exercicio_assembleias_2.csv"
if (!file.exists(F2)) { cat("verifica_assembleias_portais: ", F2, " nao existe, nada a verificar\n", sep = ""); quit(save = "no") }

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")

x <- fread(F2, colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")

ok("esquema: 21 colunas na ordem canonica", stopifnot(identical(names(x), COLS)))
ok("vocabulario fechado de forma_saida", in_set(x$forma_saida, VOCAB, "forma_saida"))
ok("ano_eleicao em 1998..2024", em_faixa(as.integer(x$ano_eleicao), 1998, 2024, "ano_eleicao"))
ok("uf com duas letras", stopifnot(all(grepl("^[A-Z]{2}$", x$uf))))
ok("condicao em titular/suplente/nao_informado", stopifnot(all(is.na(x$condicao) | x$condicao %in% c("titular", "suplente", "nao_informado"))))
iso <- function(v) is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v)
ok("datas em ISO", stopifnot(all(iso(x$data_inicio_exercicio)), all(iso(x$data_fim_exercicio))))
ok("inicio nao posterior ao fim", stopifnot(x[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio), all(data_inicio_exercicio <= data_fim_exercicio)]))

## integridade referencial
par <- x[!is.na(id_mandato_bocel)]
ok("todo id_mandato_bocel existe em mandatos.csv", stopifnot(all(par$id_mandato_bocel %in% m$id_mandato)))
idx <- match(par$id_mandato_bocel, m$id_mandato)
ok("mandato pareado e deputado estadual ou distrital", stopifnot(all(m$cd_cargo[idx] %in% c("7", "8"))))
ok("UF do mandato bate com a UF da coleta", stopifnot(all(m$sg_uf[idx] == par$uf)))
ok("ano de eleicao do mandato bate com o da coleta", stopifnot(all(m$ano_eleicao[idx] == par$ano_eleicao)))
jan_ok <- is.na(par$data_fim_exercicio) |
  (as.IDate(par$data_fim_exercicio) >= as.IDate(m$mandato_inicio[idx]) - 60L &
   as.IDate(par$data_fim_exercicio) <= as.IDate(m$mandato_fim[idx]) + 45L)
cat("linhas pareadas com fim fora da janela do mandato:", sum(!jan_ok), "\n")
registrar_numero("vasp_n_fim_fora_da_janela", sum(!jan_ok), script = script)

## granularidade: um mandato nao pode receber duas formas observadas diferentes
obs <- unique(par[forma_saida != "nao_observado", .(id_mandato_bocel, forma_saida)])
dup <- obs[, .N, by = id_mandato_bocel][N > 1]
if (nrow(dup)) print(par[id_mandato_bocel %in% dup$id_mandato_bocel, .(uf, id_mandato_bocel, nome, forma_saida, data_fim_exercicio, url)][order(id_mandato_bocel)])
ok("um mandato nao recebe duas formas observadas divergentes", stopifnot(nrow(dup) == 0L))

## Evidencia exigida por tipo de forma. Renuncia, cassacao, falecimento, afastamento, licenca e
## posse nao ocorrida sao afirmacoes sobre um ato, e por isso so valem com o texto da fonte em
## causa_original. Ja 'suplente_efetivado', 'assumiu_titular' e 'outro' podem ser derivados da
## ESTRUTURA da tabela da casa, quando o quadro da legislatura mostra o suplente ocupando um
## periodo ou o titular encerrando antes do fim, e nesses casos nao ha frase a citar. A checagem
## separa os dois regimes em vez de exigir texto para tudo (29/08/2026).
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")
sem_txt <- par[forma_saida %in% TEXTUAL & (is.na(causa_original) | causa_original == "")]
der <- par[forma_saida %in% ESTRUTURAL & (is.na(causa_original) | causa_original == "")]
cat("formas textuais sem texto de origem:", nrow(sem_txt), "| formas derivadas da estrutura:", nrow(der), "\n")
if (nrow(der)) print(der[, .N, by = .(uf, fonte, forma_saida)][order(-N)])
registrar_numero("vasp_n_forma_nomeada_sem_texto", nrow(sem_txt), script = script)
registrar_numero("vasp_n_forma_derivada_da_estrutura", nrow(der), script = script)
ok("ato nomeado (renuncia, cassacao, falecimento, afastamento, licenca, posse nao ocorrida) traz o texto da fonte",
   stopifnot(nrow(sem_txt) == 0L))
ok("forma sem texto de origem so aparece nos rotulos derivaveis da estrutura da tabela",
   stopifnot(par[(is.na(causa_original) | causa_original == "") & !forma_saida %in% c("nao_observado", "fim_regular"),
                 all(forma_saida %in% ESTRUTURAL)]))

## url de origem
ok("toda linha traz url de origem", stopifnot(all(!is.na(x$url) & nzchar(x$url))))

## cobertura recontada
cob <- x[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)),
             com_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)])), by = uf][order(-com_forma)]
print(cob, nrows = 30)
cf <- fread("data/exercicio_assembleias_2_cobertura.csv")
ok("cobertura gravada = cobertura recontada", stopifnot(identical(as.integer(cob[order(uf)]$com_forma), as.integer(cf[order(uf)]$com_forma))))

## registro assinado: le por readLines porque ha valores com '|' no meio
ass <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- ass[grepl("\\|", ass)]
pa <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass)), valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass)))
ult <- pa[pa[, .I[.N], by = chave]$V1]
REG <- setNames(ult$valor, ult$chave)
ok("asm2_n_linhas registrado = recontado", stopifnot(REG[["asm2_n_linhas"]] == as.character(nrow(x))))
ok("asm2_n_mandatos_com_forma registrado = recontado",
   stopifnot(REG[["asm2_n_mandatos_com_forma"]] == as.character(uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel))))

## integracao: todo mandato com fonte assembleia_portal tem lastro na tabela
mp <- m[fonte_forma_saida == "assembleia_portal"]
cat("mandatos com fonte assembleia_portal em mandatos.csv:", nrow(mp), "\n")
registrar_numero("vasp_n_mandatos_fonte_assembleia_portal", nrow(mp), script = script)
ok("todo mandato com fonte assembleia_portal tem linha na tabela",
   stopifnot(all(mp$id_mandato %in% par[forma_saida != "nao_observado"]$id_mandato_bocel)))
ok("forma gravada e a forma da tabela, ou saneada para outro/fim_regular",
   stopifnot(nrow(merge(mp[, .(id_mandato, forma_saida)], obs, by.x = "id_mandato", by.y = "id_mandato_bocel")[
     forma_saida.x != forma_saida.y & !forma_saida.x %in% c("outro", "fim_regular")]) == 0L))

registrar_numero("vasp_n_checagens", pass + fail, script = script)
registrar_numero("vasp_n_aprovadas", pass, script = script)
registrar_numero("vasp_n_reprovadas", fail, script = script)
gravar_relatorio_verificacao(alvo = F2, script = script, passou = aprovadas, falhou = falhas,
                             fora_de_cobertura = c("veracidade das datas e causas publicadas pelos portais das casas (conferida por amostra nos scripts por casa em R/assembleias2, nao aqui)",
                                                   "homonimos no pareamento por nome dentro da mesma UF e eleicao"))
cat("\nverifica_assembleias_portais: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("FALHAS:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); sink(); quit(status = 1) }
sink()
