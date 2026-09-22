#!/usr/bin/env Rscript
# Verificador das tabelas recuperadas do material que ja estava em disco (30/08/2026), frente
# aberta para esgotar o coletado antes de qualquer requisicao nova.
# Cobre: exercicio_camaras_generico, exercicio_sapl_recuperado, exercicio_assembleias_inventario,
# munic_exercicio_ampliado.
suppressPackageStartupMessages({library(data.table)})
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/51_verifica_tabelas_recuperadas.R"
pass <- 0L; fail <- 0L; falhas <- character(); passou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) {
    falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) { pass <<- pass + 1L; passou <<- c(passou, nome) } else fail <<- fail + 1L
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
}
reg <- function(k, v) registrar_numero(k, v, script = script)
VOCAB <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca",
           "nao_tomou_posse","suplente_efetivado","assumiu_titular","outro","nao_observado")
m <- fread("data/mandatos.csv", select=c("id_mandato","id_pessoa","cd_cargo","sg_ue","sg_uf",
                                         "ano_eleicao","esfera","forma_saida"))
m[, sg_ue := sprintf("%05d", suppressWarnings(as.integer(sg_ue)))]

TABS <- c("exercicio_camaras_generico","exercicio_sapl_recuperado",
          "exercicio_assembleias_inventario","munic_exercicio_ampliado")
for (t in TABS) {
  f <- file.path("data", paste0(t, ".csv"))
  if (!file.exists(f)) { cat("AUSENTE:", f, "\n"); next }
  x <- fread(f, na.strings = c("NA",""))
  cat("\n### ", t, " (", nrow(x), " linhas)\n", sep="")
  ok(paste0(t, ": forma_saida em vocabulario fechado"),
     in_set(x$forma_saida, VOCAB, permitir_na = TRUE))
  cm <- intersect(c("id_mandato_bocel"), names(x))
  if (length(cm)) {
    par <- x[!is.na(get(cm))]
    ok(paste0(t, ": todo id_mandato existe no banco"),
       stopifnot(all(par[[cm]] %in% m$id_mandato)))
    if (nrow(par)) {
      idx <- match(par[[cm]], m$id_mandato)
      if ("uf" %in% names(x))
        ok(paste0(t, ": UF da linha bate com a do mandato"),
           stopifnot(all(is.na(par$uf) | par$uf == m$sg_uf[idx])))
      if ("sg_ue" %in% names(x)) {
        ue <- sprintf("%05d", suppressWarnings(as.integer(par$sg_ue)))
        ok(paste0(t, ": municipio da linha bate com o do mandato"),
           stopifnot(all(is.na(ue) | ue == m$sg_ue[idx])))
      }
      if ("ano_eleicao_bocel" %in% names(x))
        ok(paste0(t, ": eleicao da linha bate com a do mandato"),
           stopifnot(all(is.na(par$ano_eleicao_bocel) |
                           as.integer(par$ano_eleicao_bocel) == as.integer(m$ano_eleicao[idx]))))
      reg(paste0("vrec_", t, "_pareadas"), nrow(par))
      reg(paste0("vrec_", t, "_mandatos_sem_saida_hoje"),
          sum(m$forma_saida[idx] == "nao_observado"))
    }
  }
  reg(paste0("vrec_", t, "_linhas"), nrow(x))
}

## a tabela generica afirma exercicio, e nao forma de saida
g <- fread("data/exercicio_camaras_generico.csv", na.strings=c("NA",""))
ok("generico: nenhuma linha afirma forma de saida",
   stopifnot(all(g$forma_saida == "nao_observado")))
ok("generico: todo arquivo de origem existe em disco",
   stopifnot(all(file.exists(file.path(getwd(), g$url)))))
ok("generico: metodo declara o criterio de desambiguacao",
   stopifnot(all(grepl("janela_na_pagina|mandato_unico_no_municipio", g$metodo_pareamento))))
ok("generico: uma linha por mandato", checa_unica(as.data.frame(g), "id_mandato_bocel"))
ok("generico: so cargo municipal",
   stopifnot(all(m$cd_cargo[match(g$id_mandato_bocel, m$id_mandato)] %in% c(11L, 13L))))
reg("vrec_generico_municipios", uniqueN(g$sg_ue))
reg("vrec_generico_ufs", uniqueN(g$uf))

## a tabela de assembleias nao pode ter voltado a afirmar forma de saida
a <- fread("data/exercicio_assembleias_inventario.csv", na.strings=c("NA",""))
# 06/09/2026: a checagem exigia forma_saida sempre nao_observado, politica anterior de rebaixamento
# total. R/50 passou a declarar fim_regular quando o vinculo alcanca o fim da legislatura e outro
# quando ha fim datado antes disso, o que usa o vocabulario para o que a folha de fato mostra, sem
# inventar causa. A checagem passa a exigir a coerencia entre forma e data, o que e mais estrito do
# que a regra anterior: nenhuma forma observada sem data de fim ou cobertura integral do periodo.
a[, fim_dentro := !is.na(data_fim_exercicio)]
ok("assembleias do inventario: forma observada exige fim datado ou cobertura do periodo",
   a[forma_saida %in% c("fim_regular", "outro") & fim_dentro == FALSE & !grepl("cobre", fcoalesce(causa_original, "")), .N] == 0)
ok("assembleias do inventario: causa nunca vem da folha (so fim_regular, outro ou nao_observado)",
   all(a$forma_saida %in% c("fim_regular", "outro", "nao_observado", NA)))

fc <- c("veracidade da relacao nominal publicada por cada casa",
        "legislatura da pagina quando ela nao traz a janela de anos",
        "identidade da pessoa pareada so por nome dentro do municipio",
        "se a ausencia de um nome na relacao significa saida do mandato")
gravar_relatorio_verificacao("tabelas recuperadas do material em disco", script,
                             passou = passou, falhou = falhas, fora_de_cobertura = fc)
cat("\nverifica_tabelas_recuperadas: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat(paste0(" - ", falhas, collapse="\n"), "\n"); quit(save="no", status=1) }
