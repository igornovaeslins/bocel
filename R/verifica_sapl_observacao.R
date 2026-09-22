# verifica_sapl_observacao.R — verificacao cetica de R/54_sapl_observacao.R
#
# O que esta verificacao cobre: cardinalidade e unicidade das tabelas, coerencia interna entre
# causa, tipo de evento e forma de saida, plausibilidade das datas contra a legislatura e contra
# o periodo do mandato, e a validade do pareamento do titular nomeado no texto. Cobre ainda uma
# checagem INDEPENDENTE, que compara a causa lida no texto livre com o campo tipificado
# tipo_afastamento que a propria Casa preenche, nas linhas em que os dois existem.
#
# FORA DE COBERTURA: se o texto da Casa diz a verdade sobre o que aconteceu, e se a lista de
# suplencia da eleicao corresponde a quem de fato foi convocado. Isso e leitura de fonte, e fica
# com o julgamento do autor, apoiado na amostra sorteada por R/amostra_sapl_observacao_precisao.R.
#
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_sapl_observacao.R
set.seed(20260903)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_sapl_observacao.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
passou <- character(); falhou <- character(); fora <- character()
ok <- function(cond, msg) {
  if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg)
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n")
}
norm <- function(x) {
  y <- stri_trans_general(toupper(x), "Latin-ASCII")
  y <- gsub("[^A-Z ]", " ", y); gsub(" +", " ", trimws(y))
}

ev  <- fread("data/sapl_observacao_eventos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
tit <- fread("data/sapl_observacao_titular.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
ex  <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
man <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
nao <- fread("output/verificacao/sapl_observacao_nao_lidas.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cat("eventos:", nrow(ev), "| mandatos de titular:", nrow(tit), "| nao lidas:", nrow(nao), "\n\n")

## ---- 1. conservacao: nada foi inventado nem perdido
ex[, obs := trimws(fifelse(is.na(observacao), "", observacao))]
ex[, obs := fifelse(toupper(obs) %in% c("NONE","NULL","-","0"), "", obs)]
ex[, chave_sapl := paste0(dominio, "#", id_mandato_sapl)]
n_obs <- ex[obs != "", .N]
ok(nrow(ev) + nrow(nao) <= n_obs, "eventos lidos mais nao lidos nao excedem as observacoes existentes")
ok(all(ev$chave_sapl %in% ex$chave_sapl), "todo evento vem de uma linha do SAPL coletado")
ok(uniqueN(ev$chave_sapl) == nrow(ev), "um evento por registro de mandato do SAPL")
try(checa_unica(tit, "id_mandato"), silent = TRUE)
ok(uniqueN(tit$id_mandato) == nrow(tit), "sapl_observacao_titular tem um registro por mandato do BOCEL")

## ---- 2. o mandato apontado existe e e de vereador
m13 <- man[cd_cargo == "13"]
ok(all(tit$id_mandato %in% m13$id_mandato), "todo mandato de titular apontado existe em mandatos.csv como vereador")
ev_t <- ev[!is.na(id_mandato_titular)]
ok(all(ev_t$id_mandato_titular %in% m13$id_mandato), "todo titular nomeado pareado existe como mandato de vereador")
ok(ev[!is.na(id_mandato_titular) & !is.na(id_mandato_bocel) & id_mandato_titular == id_mandato_bocel, .N] == 0,
   "nenhum registro aponta a si proprio como titular substituido")

## ---- 3. coerencia entre causa, tipo de evento e forma de saida
IRREV <- c("falecimento", "cassacao", "renuncia")
ok(ev[causa %in% IRREV & tipo_evento != "fim_de_mandato", .N] == 0,
   "falecimento, cassacao e renuncia sempre entram como fim de mandato")
ok(ev[tipo_evento == "interregno_temporario" & !is.na(forma_saida_causa), .N] == 0,
   "interregno temporario nao produz forma de saida")
ok(ev[tipo_evento == "fim_de_mandato" & is.na(forma_saida_causa), .N] == 0,
   "todo fim de mandato tem forma de saida preenchida")
ok(all(na.omit(unique(ev$forma_saida_causa)) %in%
         c("falecimento","cassacao","renuncia","licenca","afastamento")),
   "forma de saida usa apenas o vocabulario do banco")
ok(ev[tipo_evento == "interregno_temporario" & is.na(retorno_evidenciado), .N] == 0,
   "todo interregno temporario declara a evidencia do retorno")
ok(tit[tipo_evento == "fim_de_mandato" & is.na(forma_saida_obs), .N] == 0,
   "todo fim de mandato na tabela do titular tem forma de saida")

## ---- 4. as datas
ev[, `:=`(d_ev = as.IDate(data_evento), d_ret = as.IDate(data_retorno),
          li = as.IDate(legislatura_inicio), lf = as.IDate(legislatura_fim))]
ok(ev[!is.na(d_ev) & !is.na(li) & (d_ev < li - 60L | d_ev > lf + 60L), .N] == 0,
   "toda data de evento cai dentro da legislatura declarada, com folga de 60 dias")
ok(ev[!is.na(d_ret) & !is.na(d_ev) & d_ret <= d_ev, .N] == 0,
   "a data de retorno e sempre posterior a data do evento")
ok(ev[!is.na(d_ev) & (year(d_ev) < 1988 | year(d_ev) > 2030), .N] == 0, "nenhuma data fora de 1988 a 2030")
tit[, d_ev := as.IDate(data_evento)]
tit <- merge(tit, m13[, .(id_mandato, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))],
             by = "id_mandato", all.x = TRUE)
fora_janela <- tit[!is.na(d_ev) & !is.na(mi) & (d_ev < mi - 60L | d_ev > mf + 60L), .N]
cat("datas de evento fora da janela do mandato do BOCEL:", fora_janela, "\n")
reg("vobs_datas_fora_da_janela_do_mandato", fora_janela)
ok(fora_janela == 0, "a data do evento cai dentro do mandato do BOCEL a que foi atribuida")

## ---- 5. checagem INDEPENDENTE: texto livre contra o campo tipificado da Casa
# O campo tipo_afastamento e preenchido pela Casa num menu fechado, sem relacao com o texto que
# ela digita na observacao. Onde os dois existem, a concordancia mede a leitura do texto por
# fora dela mesma.
cmp <- merge(ev[, .(chave_sapl, causa, tipo_evento)],
             ex[!is.na(tipo_afastamento) & tipo_afastamento != "",
                .(chave_sapl, tipo_afastamento)], by = "chave_sapl")
if (nrow(cmp)) {
  cmp[, TA := norm(tipo_afastamento)]
  cmp[, causa_do_campo := fcase(
    grepl("CASSA|PERDA", TA), "cassacao",
    grepl("RENUNC", TA), "renuncia",
    grepl("FALEC|OBITO|MORTE", TA), "falecimento",
    grepl("SECRETARI|EXECUTIVO|CARGO NO", TA), "licenca_cargo_executivo",
    grepl("SAUDE|MEDICA", TA), "licenca_saude",
    grepl("MATERNIDADE|GESTANTE", TA), "licenca_maternidade",
    grepl("PARTICULAR", TA), "licenca_particular",
    grepl("MISSAO", TA), "missao_oficial",
    grepl("SUSPENSAO|JUDICIAL|PRISAO", TA), "afastamento_judicial",
    grepl("LICEN", TA), "licenca_sem_causa",
    default = NA_character_)]
  ct <- cmp[!is.na(causa_do_campo) & !is.na(causa)]
  conc <- ct[causa == causa_do_campo, .N]
  # concordancia grosseira: mesma familia (licenca contra licenca, saida contra saida)
  fam <- function(x) fcase(x %in% IRREV, "fim", grepl("^licenca|missao", x), "licenca",
                           grepl("^afastamento", x), "afastamento", default = "outro")
  conc_fam <- ct[fam(causa) == fam(causa_do_campo), .N]
  cat("comparaveis com o campo tipificado:", nrow(ct),
      "| causa exata igual:", conc, sprintf("(%.1f%%)", 100 * conc / max(nrow(ct), 1)),
      "| mesma familia:", conc_fam, sprintf("(%.1f%%)", 100 * conc_fam / max(nrow(ct), 1)), "\n")
  print(ct[causa != causa_do_campo, .N, by = .(causa, causa_do_campo)][order(-N)])
  reg("vobs_comparaveis_campo_tipificado", nrow(ct))
  reg("vobs_concordancia_exata", conc)
  reg("vobs_concordancia_familia", conc_fam)
  reg("vobs_taxa_concordancia_familia", round(conc_fam / max(nrow(ct), 1), 4))
  ok(nrow(ct) >= 30, "ha comparaveis suficientes para medir concordancia")
  ok(conc_fam / max(nrow(ct), 1) >= 0.80,
     "concordancia de familia com o campo tipificado da Casa é de ao menos 80%")
} else {
  fora <- c(fora, "sem linhas comparaveis com o campo tipificado")
}

## ---- 6. o pareamento do titular nomeado
# Um titular pareado tem de ser de mandato do MESMO municipio e da MESMA eleicao do registro.
chk <- merge(ev_t[, .(chave_sapl, sg_ue, ano_eleicao, id_mandato_titular, regra_titular)],
             m13[, .(id_mandato_titular = id_mandato, sg_ue_m = unidade_posicao, ano_m = ano_eleicao)],
             by = "id_mandato_titular")
ok(chk[sg_ue != sg_ue_m, .N] == 0, "o titular pareado e do mesmo municipio do registro")
ok(chk[ano_eleicao != ano_m, .N] == 0, "o titular pareado e da mesma eleicao do registro")
reg("vobs_titulares_pareados", nrow(ev_t))
print(ev_t[, .N, by = regra_titular][order(-N)])

## ---- 7. contagem por confianca e por tipo, para o registro
for (cf in c("alta","media","baixa")) reg(paste0("vobs_eventos_", cf), ev[confianca == cf, .N])
for (te in unique(na.omit(ev$tipo_evento))) reg(paste0("vobs_tipo_", te), ev[tipo_evento == te, .N])
reg("vobs_eventos", nrow(ev)); reg("vobs_titular_mandatos", nrow(tit))
reg("vobs_interregno_com_as_duas_pontas",
    tit[tipo_evento == "interregno_temporario" & !is.na(data_evento) & !is.na(data_retorno), .N])

## ---- 8. veredito
fora <- c(fora,
  "se o texto da Casa descreve corretamente o que aconteceu",
  "se o suplente convocado corresponde a ordem da fila da lista partidaria",
  "erro de medida no cadastro do SAPL, que e alimentado a mao pela Casa")
cat("\nPASS:", length(passou), "| FAIL:", length(falhou), "| FORA DE COBERTURA:", length(fora), "\n")
if (length(falhou)) { cat("\nfalhas:\n"); cat(paste0("  - ", falhou, collapse = "\n"), "\n") }
f <- gravar_relatorio_verificacao("R/54_sapl_observacao.R", script,
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("relatorio:", f, "\n")
if (length(falhou)) quit(status = 1)
