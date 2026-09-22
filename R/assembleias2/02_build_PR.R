#!/usr/bin/env Rscript
# 02_build_PR.R — monta data/assembleias2/PR.csv: posse, exercicio e forma de saida dos
#   deputados estaduais do Parana, 1998-2022, a partir do Diario da Assembleia.
#
# Entrada (tudo em cache, nenhuma requisicao):
#   data_raw/assembleias2/PR/painel_expediente.rds   painel data x deputado (R/assembleias2/01)
#   data_raw/assembleias2/PR/atos_candidatos.jsonl   trechos de ato (python/assembleias2/atos_PR.py)
#   data/exercicio_assembleias.csv (uf == PR)        relacao por legislatura do portal da ALEP
#   data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:
#   data/assembleias2/PR.csv (21 colunas)
#   output/verificacao/asm2_PR_*.csv, output/numeros_assinatura.txt (prefixo asm2pr_)
# Execucao:
#   cd ~/bocel && Rscript --vanilla R/assembleias2/01_exercicio_PR.R &&
#   Rscript --vanilla R/assembleias2/02_build_PR.R
set.seed(20260830)
suppressPackageStartupMessages({
  library(data.table); library(stringi); library(arrow); library(jsonlite)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source("R/assembleias2/00_funcoes_PR.R")
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

raw  <- file.path(root, "data_raw", "assembleias2", "PR")
outd <- file.path(root, "data", "assembleias2")
verd <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "02_build_PR.R")
sink(file.path(root, "logs", "asm2_PR_build.log"), split = TRUE)
cat("02_build_PR.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(paste0("asm2pr_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
# prioridade quando duas evidencias caem no mesmo mandato: o ato nomeado vence a derivacao
PRIOR <- c(falecimento = 1, cassacao = 2, renuncia = 3, nao_tomou_posse = 4, afastamento = 5,
           licenca = 6, suplente_efetivado = 7, assumiu_titular = 8, outro = 9,
           fim_regular = 10, nao_observado = 11)
URL_ACERVO <- "https://transparencia.assembleia.pr.leg.br/plenario/diarios-da-assembleia"

## ---------------------------------------------------------------- 1. painel do expediente
P <- readRDS(file.path(raw, "painel_expediente.rds"))
pan <- P$pan; per <- copy(P$per); grade <- P$grade; blocos <- P$blocos
setnames(per, "canonico", "nome_norm")
per[, ano_eleicao := as.integer(ano_eleicao)]
# quem esta na relacao nominal nas primeiras seis semanas da legislatura tomou posse na abertura
per[, `:=`(abertura = primeira <= leg_inicio + 45L,
           ate_o_fim = ultima >= leg_fim - 45L)]
# periodo em que o expediente marcou "licenciado" ao lado do nome
# a marca no expediente muda de forma ao longo do tempo: "(licenciado)", "(em licenca)"
lic <- pan[grepl("LICEN", toupper(asc(anotacao))),
           .(lic_ini = min(data), lic_fim = max(data), lic_n = .N,
             lic_arq = arquivo[which.min(data)]), by = .(legislatura, canonico)]
setnames(lic, "canonico", "nome_norm")
per <- merge(per, lic, by = c("legislatura", "nome_norm"), all.x = TRUE)
cat("pessoa x legislatura no painel:", nrow(per), "| com marca de licenciado:", per[!is.na(lic_ini), .N], "\n")
print(per[, .(.N, abertura = sum(abertura), ate_o_fim = sum(ate_o_fim),
              licenciado = sum(!is.na(lic_ini))), by = legislatura][order(legislatura)])

## ---------------------------------------------------------------- 2. portal da ALEP (ja coletado)
port <- fread("data/exercicio_assembleias.csv", na.strings = "NA", encoding = "UTF-8",
              colClasses = "character")[uf == "PR"]
port <- port[, .(legislatura, ano_eleicao = as.integer(ano_eleicao), nome_portal = nome,
                 nome_norm = nome_normalizado, id_fonte_portal = id_fonte, url_portal = url)]
port <- unique(port, by = c("legislatura", "nome_norm"))
cat("relacao do portal da ALEP:", nrow(port), "linhas\n")

## ---------------------------------------------------------------- 3. BOCEL: mandatos do PR
bm <- fread("data/mandatos.csv", na.strings = "NA", encoding = "UTF-8",
            colClasses = list(character = c("sq_candidato", "nr_candidato")))
bp <- fread("data/pessoas.csv", na.strings = "NA", encoding = "UTF-8",
            colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bm[cd_cargo == 7L & sg_uf == "PR",
          .(id_mandato, id_pessoa, ano_eleicao, sq_candidato, nr_candidato,
            mandato_inicio = as.IDate(mandato_inicio), mandato_fim = as.IDate(mandato_fim))]
dep <- merge(dep, bp[, .(id_pessoa, nome_bocel = nome, dt_nascimento = as.character(dt_nascimento),
                         sexo_bocel = as.character(genero))], by = "id_pessoa", all.x = TRUE)
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
urna <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE),
  function(f) {
    x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                              "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_TIPO_ELEICAO")))
    x <- x[SG_UF == "PR" & as.integer(CD_CARGO) == 7L & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
    x[, .(ano_eleicao = as.integer(ANO_ELEICAO), nr_candidato = as.character(NR_CANDIDATO),
          sq_candidato = as.character(SQ_CANDIDATO), nome_urna = NM_URNA_CANDIDATO)]
  }))
urna <- unique(urna, by = c("ano_eleicao", "nr_candidato", "sq_candidato"))
dep <- merge(dep, urna, by = c("ano_eleicao", "nr_candidato", "sq_candidato"), all.x = TRUE)
dep[, nome_urna_norm := norm_nome(nome_urna)]
dep <- merge(dep, LEGS[, .(legislatura, ano_eleicao)], by = "ano_eleicao")
cat("mandatos de deputado estadual do PR no BOCEL:", nrow(dep),
    "| com nome de urna:", dep[!is.na(nome_urna), .N], "\n")

## ---------------------------------------------------------------- 4. casamento de nomes
# O nome que o expediente imprime nao e o nome civil nem sempre e o nome de urna: a urna do PR
# traz sobrenome solto ("KIELSE", "PLAUTO", "LITRO") onde o expediente traz nome e sobrenome.
# Em vez de uma regra so, gera varias CHAVES por registro e casa chave a chave, sempre exigindo
# que a chave seja unica dos dois lados dentro da legislatura.
toks <- function(x) stri_split_fixed(x, " ")
primeiro_ultimo <- function(x) vapply(toks(x), function(v)
  if (length(v) < 2L) NA_character_ else paste(v[1L], v[length(v)]), character(1))
ultimo <- function(x, minc = 5L) vapply(toks(x), function(v) {
  u <- v[length(v)]; if (is.na(u) || nchar(u) < minc) NA_character_ else u }, character(1))
primeiro <- function(x, minc = 5L) vapply(toks(x), function(v) {
  u <- v[1L]; if (is.na(u) || nchar(u) < minc) NA_character_ else u }, character(1))

conjunto <- function(x) vapply(toks(x), function(v) {
  v <- sort(unique(v[nchar(v) >= 3L])); if (!length(v)) NA_character_ else paste(v, collapse = " ")
}, character(1))
chaves_de <- function(nomes, id) {
  n <- norm_nome(nomes)
  rbindlist(list(
    data.table(id = id, tipo = "exato",    chave = n),
    data.table(id = id, tipo = "conjunto", chave = conjunto(n)),
    data.table(id = id, tipo = "prim_ult", chave = primeiro_ultimo(n)),
    data.table(id = id, tipo = "ultimo",   chave = ultimo(n, 5L)),
    data.table(id = id, tipo = "primeiro", chave = primeiro(n, 5L))
  ))[!is.na(chave) & nzchar(chave)]
}
# continencia: todos os vocabulos de um nome estao no outro, com pelo menos dois em comum
casar_contido <- function(na, ida, nb, idb) {
  if (!length(na) || !length(nb)) return(data.table(id_a = character(), id_b = character()))
  ta <- toks(na); tb <- toks(nb)
  res <- data.table(id_a = character(), id_b = character())
  for (i in seq_along(na)) {
    hit <- which(vapply(seq_along(nb), function(j) {
      a <- ta[[i]]; b <- tb[[j]]
      length(intersect(a, b)) >= 2L && (all(a %in% b) || all(b %in% a))
    }, logical(1)))
    if (length(hit) != 1L) next
    j <- hit
    volta <- which(vapply(seq_along(na), function(k) {
      a <- ta[[k]]; b <- tb[[j]]
      length(intersect(a, b)) >= 2L && (all(a %in% b) || all(b %in% a))
    }, logical(1)))
    if (length(volta) != 1L) next
    res <- rbind(res, data.table(id_a = as.character(ida[i]), id_b = as.character(idb[j])))
  }
  res
}
# ultimo recurso: similaridade de Jaro-Winkler, aceita so o par mutuamente mais proximo
casar_fuzzy <- function(na, ida, nb, idb, corte = 0.90) {
  if (!length(na) || !length(nb)) return(data.table(id_a = character(), id_b = character()))
  M <- outer(seq_along(na), seq_along(nb),
             Vectorize(function(i, j) 1 - stringdist::stringdist(na[i], nb[j], method = "jw", p = 0.1)))
  res <- data.table(id_a = character(), id_b = character())
  for (i in seq_along(na)) {
    j <- which.max(M[i, ]); if (M[i, j] < corte) next
    if (which.max(M[, j]) != i) next
    if (sum(M[i, ] >= corte) > 1L || sum(M[, j] >= corte) > 1L) next
    res <- rbind(res, data.table(id_a = as.character(ida[i]), id_b = as.character(idb[j])))
  }
  res
}
# dois nomes de duas ou mais palavras so podem ser a mesma pessoa se o primeiro vocabulo
# coincidir, ou se um estiver contido no outro: "PLAUTO MIRO GUIMARAES" e "JONAS GUIMARAES"
# compartilham o sobrenome e nao sao a mesma pessoa
compat_nome <- function(x, y) {
  a <- unlist(stri_split_fixed(x, " ")); b <- unlist(stri_split_fixed(y, " "))
  if (length(a) == 1L || length(b) == 1L) return(TRUE)
  a[1L] == b[1L] || all(a %in% b) || all(b %in% a) || length(intersect(a, b)) >= 2L
}
filtra_compat <- function(m, na, ia, nb, ib) {
  if (!nrow(m)) return(m)
  xa <- na[match(m$id_a, as.character(ia))]; xb <- nb[match(m$id_b, as.character(ib))]
  m[mapply(compat_nome, xa, xb)]
}
# casa dois conjuntos de chaves exigindo unicidade dos dois lados; devolve pares aceitos
casar <- function(ka, kb, tipos_a, tipos_b, ja_a = character(), ja_b = character()) {
  a <- ka[tipo %in% tipos_a & !id %in% ja_a, .(id, chave)]
  b <- kb[tipo %in% tipos_b & !id %in% ja_b, .(id, chave)]
  a <- unique(a); b <- unique(b)
  a <- a[, if (uniqueN(id) == 1L) .SD[1L], by = chave]
  b <- b[, if (uniqueN(id) == 1L) .SD[1L], by = chave]
  m <- merge(a, b, by = "chave", suffixes = c("_a", "_b"))
  if (!nrow(m)) return(data.table(id_a = character(), id_b = character()))
  m <- unique(m[, .(id_a = as.character(id_a), id_b = as.character(id_b))])
  m <- m[, if (.N == 1L) .SD, by = id_a]
  if (!nrow(m)) return(data.table(id_a = character(), id_b = character()))
  m <- m[, if (.N == 1L) .SD, by = id_b]
  if (!nrow(m)) return(data.table(id_a = character(), id_b = character()))
  m[, .(id_a, id_b)]
}

## ---------------------------------------------------------------- 5. roster: painel + portal
ros <- per[, .(legislatura, ano_eleicao, nome_norm, nome = nome_diario, partido, partidos,
               leg_inicio, leg_fim, primeira, ultima, prim_data, ult_data, n_edicoes,
               abertura, ate_o_fim, lic_ini, lic_fim, lic_n, lic_arq)]
ros[, `:=`(no_painel = TRUE, rid = .I,
           nome_portal = NA_character_, id_fonte_portal = NA_character_, url_portal = NA_character_)]
# o portal e a segunda relacao da Casa: o mesmo deputado aparece com outra grafia, e so o que
# nao casa com ninguem do painel vira linha nova
port[, pid := .I]
sobra_port <- copy(port)
for (L in sort(unique(ros$legislatura))) {
  rL <- ros[legislatura == L]; pL <- sobra_port[legislatura == L]
  if (!nrow(rL) || !nrow(pL)) next
  kr <- chaves_de(rL$nome_norm, rL$rid); kp <- chaves_de(pL$nome_norm, pL$pid)
  usados_r <- character(); usados_p <- character()
  for (tp in list(c("exato"), c("conjunto"), c("prim_ult"), c("ultimo"), c("primeiro"))) {
    m <- casar(kp, kr, tp, tp, usados_p, usados_r)
    m <- filtra_compat(m, pL$nome_norm, pL$pid, rL$nome_norm, rL$rid)
    if (!nrow(m)) next
    ros[match(m$id_b, rid), `:=`(
      nome_portal     = pL$nome_portal[match(m$id_a, pL$pid)],
      id_fonte_portal = pL$id_fonte_portal[match(m$id_a, pL$pid)],
      url_portal      = pL$url_portal[match(m$id_a, pL$pid)])]
    usados_r <- c(usados_r, m$id_b); usados_p <- c(usados_p, m$id_a)
  }
  # continencia de vocabulos: "Ana Julia" no portal e "Ana Julia Ribeiro" no expediente
  livre_r <- rL[!as.character(rid) %in% usados_r]; livre_p <- pL[!as.character(pid) %in% usados_p]
  mc <- casar_contido(livre_p$nome_norm, livre_p$pid, livre_r$nome_norm, livre_r$rid)
  if (nrow(mc)) {
    ros[match(as.integer(mc$id_b), rid), `:=`(
      nome_portal     = pL$nome_portal[match(as.integer(mc$id_a), pL$pid)],
      id_fonte_portal = pL$id_fonte_portal[match(as.integer(mc$id_a), pL$pid)],
      url_portal      = pL$url_portal[match(as.integer(mc$id_a), pL$pid)])]
    usados_p <- c(usados_p, mc$id_a); usados_r <- c(usados_r, mc$id_b)
  }
  livre_r <- rL[!as.character(rid) %in% usados_r]; livre_p <- pL[!as.character(pid) %in% usados_p]
  mf <- casar_fuzzy(livre_p$nome_norm, livre_p$pid, livre_r$nome_norm, livre_r$rid, 0.91)
  if (nrow(mf)) {
    ros[match(as.integer(mf$id_b), rid), `:=`(
      nome_portal     = pL$nome_portal[match(as.integer(mf$id_a), pL$pid)],
      id_fonte_portal = pL$id_fonte_portal[match(as.integer(mf$id_a), pL$pid)],
      url_portal      = pL$url_portal[match(as.integer(mf$id_a), pL$pid)])]
    usados_p <- c(usados_p, mf$id_a)
  }
  sobra_port <- sobra_port[!(pid %in% usados_p & legislatura == L)]
}
cat("linhas do portal absorvidas pelo painel:", nrow(port) - nrow(sobra_port),
    "| linhas do portal sem par no painel:", nrow(sobra_port), "\n")
extra <- sobra_port[, .(legislatura, ano_eleicao, nome_norm, nome = nome_portal,
                        nome_portal, id_fonte_portal, url_portal)]
extra[LEGS, on = "legislatura", `:=`(leg_inicio = i.leg_inicio, leg_fim = i.leg_fim)]
extra[, `:=`(no_painel = FALSE, abertura = FALSE, ate_o_fim = FALSE)]
ros <- rbindlist(list(ros, extra), use.names = TRUE, fill = TRUE)
ros[, rid := .I]
# Sobra do extrator: grafia que aparece em pouquissimas edicoes de uma legislatura densa. No
# acervo da ALEP sao 150 a 220 edicoes por ano, e um suplente que de fato tomou posse aparece
# em dezenas; presenca abaixo de 1% das edicoes e ruido de leitura, nao parlamentar.
n_ed_leg <- grade[, .(ed_leg = uniqueN(arquivo)), by = legislatura]
ros <- merge(ros, n_ed_leg, by = "legislatura", all.x = TRUE)
ros[, piso := pmax(2L, ceiling(0.01 * fifelse(is.na(ed_leg), 0L, ed_leg)))]
ruido <- ros[no_painel == TRUE & n_edicoes < piso]
cat("grafias de baixissima presenca descartadas do roster:", nrow(ruido), "\n")
if (nrow(ruido)) print(ruido[, .(legislatura, nome_norm, n_edicoes, piso)])
ros <- ros[!(no_painel == TRUE & n_edicoes < piso)]
ros[, rid := .I]
cat("roster (painel + portal):", nrow(ros), "linhas\n")

## ---------------------------------------------------------------- 6. pareamento ao BOCEL
ros[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, metodo = NA_character_)]
kd_urna <- chaves_de(dep$nome_urna, dep$id_mandato)[, fonte := "urna"]
kd_bocel  <- chaves_de(dep$nome_bocel,  dep$id_mandato)[, fonte := "bocel"]
kd <- rbindlist(list(kd_urna, kd_bocel))
kd <- merge(kd, dep[, .(id = id_mandato, ano_eleicao)], by = "id")
kr_all <- chaves_de(ros$nome_norm, as.character(ros$rid))
kr_all <- merge(kr_all, ros[, .(id = as.character(rid), ano_eleicao)], by = "id")
REGRAS <- list(
  list(m = "nome_parlamentar_x_urna",       ta = "exato",    tb = "exato",    f = "urna"),
  list(m = "nome_parlamentar_x_nome_bocel",   ta = "exato",    tb = "exato",    f = "bocel"),
  list(m = "primeiro_ultimo_x_urna",        ta = "prim_ult", tb = "prim_ult", f = "urna"),
  list(m = "primeiro_ultimo_x_nome_bocel",    ta = "prim_ult", tb = "prim_ult", f = "bocel"),
  list(m = "ultimo_vocabulo_x_urna",        ta = "ultimo",   tb = "ultimo",   f = "urna"),
  list(m = "ultimo_vocabulo_x_nome_bocel",    ta = "ultimo",   tb = "ultimo",   f = "bocel"),
  list(m = "nome_parlamentar_x_urna_curta", ta = "ultimo",   tb = "exato",    f = "urna"),
  list(m = "primeiro_vocabulo_x_urna",      ta = "primeiro", tb = "exato",    f = "urna"),
  list(m = "primeiro_vocabulo",             ta = "primeiro", tb = "primeiro", f = "bocel"),
  list(m = "conjunto_vocabulos_x_urna",      ta = "conjunto", tb = "conjunto", f = "urna"),
  list(m = "conjunto_vocabulos_x_nome_bocel",  ta = "conjunto", tb = "conjunto", f = "bocel")
)
cat("pareamento por regra:\n")
for (ano in sort(unique(ros$ano_eleicao))) {
  ka <- kr_all[ano_eleicao == ano]; kbu <- kd[ano_eleicao == ano]
  for (rg in REGRAS) {
    kb <- kbu[fonte == rg$f]
    ja_a <- as.character(ros[ano_eleicao == ano & !is.na(id_mandato), rid])
    ja_b <- ros[!is.na(id_mandato), id_mandato]
    m <- casar(ka, kb, rg$ta, rg$tb, ja_a, ja_b)
    nb <- if (rg$f == "urna") dep$nome_urna_norm else dep$nome_bocel_norm
    m <- filtra_compat(m, ros$nome_norm, ros$rid, nb, dep$id_mandato)
    if (!nrow(m)) next
    ros[match(as.integer(m$id_a), rid), `:=`(id_mandato = m$id_b, metodo = rg$m)]
  }
}
# passe por continencia de vocabulos antes do fuzzy
for (ano in sort(unique(ros$ano_eleicao))) {
  lr <- ros[ano_eleicao == ano & is.na(id_mandato)]
  ld <- dep[ano_eleicao == ano & !id_mandato %in% ros$id_mandato]
  if (!nrow(lr) || !nrow(ld)) next
  for (lado in c("nome_urna_norm", "nome_bocel_norm")) {
    ld2 <- ld[!id_mandato %in% ros$id_mandato]
    if (!nrow(ld2)) break
    mc <- casar_contido(lr$nome_norm, lr$rid, ld2[[lado]], ld2$id_mandato)
    if (!nrow(mc)) next
    ros[match(as.integer(mc$id_a), rid), `:=`(id_mandato = mc$id_b, metodo = paste0("continencia_", lado))]
    lr <- lr[!as.character(rid) %in% mc$id_a]
  }
}
# passe final por similaridade, dentro do ano, entre o que sobrou dos dois lados
for (ano in sort(unique(ros$ano_eleicao))) {
  lr <- ros[ano_eleicao == ano & is.na(id_mandato)]
  ld <- dep[ano_eleicao == ano & !id_mandato %in% ros$id_mandato]
  if (!nrow(lr) || !nrow(ld)) next
  for (lado in c("nome_urna_norm", "nome_bocel_norm")) {
    ld2 <- ld[!id_mandato %in% ros$id_mandato]
    if (!nrow(ld2)) break
    mf <- casar_fuzzy(lr$nome_norm, lr$rid, ld2[[lado]], ld2$id_mandato, 0.90)
    if (!nrow(mf)) next
    ros[match(as.integer(mf$id_a), rid), `:=`(id_mandato = mf$id_b, metodo = paste0("similaridade_", lado))]
    lr <- lr[!as.character(rid) %in% mf$id_a]
  }
}
ros[dep, on = c(id_mandato = "id_mandato"), id_pessoa := i.id_pessoa]
for (mm in sort(unique(na.omit(ros$metodo))))
  cat(sprintf("  %-32s +%d\n", mm, ros[metodo == mm, .N]))
cat("pareados:", ros[!is.na(id_mandato), .N], "de", nrow(ros), "linhas |",
    uniqueN(na.omit(ros$id_mandato)), "mandatos de", nrow(dep), "\n")
print(ros[, .(linhas = .N, pareadas = sum(!is.na(id_mandato)),
              mandatos_bocel = 54L), by = ano_eleicao][order(ano_eleicao)])

## ---------------------------------------------------------------- 7. condicao
# titular: e um dos eleitos do BOCEL, ou consta da relacao nominal ja na abertura da legislatura.
# suplente: entra depois da abertura e nao e eleito.
ros[, abertura := !is.na(primeira) & !is.na(prim_data) & primeira <= prim_data + 10L]
# os 54 eleitos de cada legislatura estao todos pareados; logo, quem esta na relacao nominal e
# nao e eleito so pode ser suplente convocado
ros[, condicao := fifelse(!is.na(id_mandato), "titular",
                   fifelse(no_painel == TRUE, "suplente", "nao_informado"))]
print(ros[, .N, by = .(ano_eleicao, condicao)][order(ano_eleicao, condicao)])

## ---------------------------------------------------------------- 8. presenca na abertura e no fim
# a densidade do expediente muda muito (diaria no acervo da ALEP, quinzenal no DIOE); em vez de
# uma tolerancia em dias, usa as tres primeiras e as tres ultimas edicoes de cada legislatura
setorder(grade, legislatura, data)
# so entram na referencia de abertura e de fechamento as edicoes cuja relacao nominal veio
# inteira; a de 1 de fevereiro de 2011, por exemplo, saiu truncada em 41 nomes
cheias <- pan[, .(nomes = .N), by = .(legislatura, data, arquivo)][nomes >= 50L]
setorder(cheias, legislatura, data)
prim3 <- cheias[, .(arquivo = head(arquivo, 3L)), by = legislatura]
ult3  <- cheias[, .(arquivo = tail(arquivo, 3L)), by = legislatura]
pres_ini <- unique(pan[prim3, on = .(legislatura, arquivo), nomatch = 0L][, .(legislatura, canonico)])[, ini := TRUE]
pres_fim <- unique(pan[ult3,  on = .(legislatura, arquivo), nomatch = 0L][, .(legislatura, canonico)])[, fim := TRUE]
ros[pres_ini, on = c("legislatura", nome_norm = "canonico"), desde_inicio := i.ini]
ros[pres_fim, on = c("legislatura", nome_norm = "canonico"), ate_fim := i.fim]
ros[is.na(desde_inicio), desde_inicio := FALSE]; ros[is.na(ate_fim), ate_fim := FALSE]
ENCERRADA <- LEGS[leg_fim < as.IDate("2026-08-30"), legislatura]
ros[, leg_encerrada := legislatura %in% ENCERRADA]
print(ros[, .(.N, desde_inicio = sum(desde_inicio), ate_fim = sum(ate_fim)),
          by = .(legislatura, condicao)][order(legislatura, condicao)])

## ---------------------------------------------------------------- 9. atos: quem, quando, o que
# JSONL, e nao CSV: o trecho tem de sobreviver caractere a caractere para poder ser conferido
# contra a fonte, e o escape de aspas do CSV nao aguenta o vaivem
atos <- rbindlist(lapply(readLines(file.path(raw, "atos_candidatos.jsonl"), warn = FALSE),
                         function(l) as.data.table(jsonlite::fromJSON(l))))
atos[, data := as.IDate(data)]
atos[LEGS, on = .(data >= leg_inicio, data <= leg_fim), `:=`(legislatura = i.legislatura)]
atos <- atos[!is.na(legislatura)]
atos[, aid := .I]
atos[, tx := toupper(asc(trecho))]
cat("trechos de ato dentro das legislaturas 14-20:", nrow(atos), "\n")
print(atos[, .N, by = .(padrao)][order(-N)])

# padroes de busca de cada pessoa do roster: nome do expediente, do portal, de urna e civil
# o PDF do diario quebra a palavra na virada de linha ("Anto- nio Carlos Baratter"), e o texto
# normalizado guarda o hifen; o padrao de busca tolera essa quebra entre quaisquer duas letras
flex <- function(tok) paste0(unlist(stri_split_boundaries(tok, type = "character")),
                             collapse = "(?:-\\s?)?")
padrao_nome <- function(nn) {
  v <- unlist(stri_split_fixed(nn, " ")); v <- v[nchar(v) >= 2L]
  if (!length(v)) return(NA_character_)
  if (length(v) == 1L) return(if (nchar(v) >= 7L) paste0("\\b", flex(v), "\\b") else NA_character_)
  if (nchar(v[length(v)]) < 4L) return(NA_character_)
  paste0("\\b", flex(v[1L]), "\\b[A-Z .'\u00b4-]{0,45}\\b", flex(v[length(v)]), "\\b")
}
alvos <- rbindlist(list(
  ros[, .(rid, legislatura, n = nome_norm)],
  ros[!is.na(nome_portal), .(rid, legislatura, n = norm_nome(nome_portal))],
  ros[!is.na(id_mandato)][dep, on = "id_mandato", nomatch = 0L][, .(rid, legislatura, n = nome_bocel_norm)],
  ros[!is.na(id_mandato)][dep, on = "id_mandato", nomatch = 0L][, .(rid, legislatura, n = nome_urna_norm)]
))
alvos <- unique(alvos[!is.na(n) & nzchar(n)])
# a fonte as vezes encurta o nome ("Carta de Renuncia Cesar Silvestri I" para Cesar Silvestri
# Filho); alem do primeiro-com-ultimo, tenta os dois primeiros vocabulos
dois_primeiros <- function(nn) {
  v <- unlist(stri_split_fixed(nn, " "))
  if (length(v) < 3L || nchar(v[2L]) < 4L) return(NA_character_)
  paste(v[1L], v[2L])
}
alvos <- rbindlist(list(alvos, alvos[, .(rid, legislatura, n = vapply(n, dois_primeiros, character(1)))]))
alvos <- unique(alvos[!is.na(n) & nzchar(n)])
alvos[, rx := vapply(n, padrao_nome, character(1))]
alvos <- alvos[!is.na(rx)]
cat("padroes de busca de nome:", nrow(alvos), "para", uniqueN(alvos$rid), "pessoas\n")

# localiza cada pessoa em cada trecho da sua legislatura
acha <- function(txs, aids, rxs, rids) {
  out <- vector("list", length(rxs))
  for (k in seq_along(rxs)) {
    p <- stri_locate_first_regex(txs, rxs[k])
    ok <- !is.na(p[, 1])
    if (any(ok)) out[[k]] <- data.table(aid = aids[ok], rid = rids[k],
                                        pos = p[ok, 1], fim = p[ok, 2])
  }
  rbindlist(out)
}
ocor <- rbindlist(lapply(sort(unique(atos$legislatura)), function(L) {
  a <- atos[legislatura == L]; b <- alvos[legislatura == L]
  if (!nrow(a) || !nrow(b)) return(NULL)
  acha(a$tx, a$aid, b$rx, b$rid)
}))
ocor <- ocor[order(aid, rid, pos)][, .SD[1L], by = .(aid, rid)]
cat("ocorrencias nome x trecho:", nrow(ocor), "\n")

# marcador da vaga dentro do trecho, que separa quem entra de quem sai
MARC <- paste0("VAGA DEIXADA|NA VAGA|EM VAGA|EM VIRTUDE D[AO] (LICENCA|FALECIMENTO|RENUNCIA)|",
               "VAGA ABERTA|VAGA DECORRENTE|QUE SUBSTITUI O DEPUTAD|SUBSTITUINDO O DEPUTAD|",
               "EM SUBSTITUICAO AO DEPUTAD")
atos[, pos_marc := stri_locate_first_regex(tx, MARC)[, 1]]
atos[, pos_gat := {
  g <- stri_locate_first_regex(tx, "ASSUM|CONVOC|RENUNC|LICEN|FALECIMENTO|CASSA|POSSE|REASSUM")
  fifelse(is.na(g[, 1]), 1L, g[, 1])
}]

# O trecho citado precisa atravessar CSV, fread e a tabela unica sem perder um caractere. A
# aspa reta e o unico caractere que nao aguenta esse vaivem (fread nao desfaz o escape ""), e o
# diario a usa para abrir o compromisso de posse. Em vez de higienizar o texto, o recorte para
# na aspa mais proxima dos dois lados da ancora: continua sendo trecho literal da fonte.
# o nome civil sai do texto do PDF com a quebra de linha dentro da palavra; aqui, ao contrario
# de causa_original, o campo e derivado e nao citacao, entao a emenda e legitima
descola <- function(x) limpa(stri_replace_all_regex(x, "(?<=\\p{L})-\\s+(?=\\p{L})", ""))

recorta <- function(tx, anc, esq, dir) {
  ini <- max(1L, anc - esq); fim <- min(nchar(tx), anc + dir)
  seg <- substr(tx, ini, fim); a <- anc - ini + 1L
  p <- stri_locate_all_fixed(seg, "\"")[[1]][, 1]; p <- p[!is.na(p)]
  if (length(p)) {
    e <- p[p < a]
    if (length(e)) { seg <- substr(seg, max(e) + 1L, nchar(seg)); a <- a - max(e) }
    q <- stri_locate_all_fixed(seg, "\"")[[1]][, 1]; q <- q[!is.na(q)]; q <- q[q >= a]
    if (length(q)) seg <- substr(seg, 1L, min(q) - 1L)
  }
  limpa(seg)
}

# Segue a convencao de R/13_exercicio_assembleias.R: eleicao para outro cargo e RENUNCIA
# (o parlamentar abre mao do mandato), investidura em cargo do Executivo e AFASTAMENTO
# (o mandato fica suspenso e o suplente assume no lugar).
classifica_causa <- function(seg) {
  s <- toupper(asc(seg))
  fifelse(grepl("FALECIMENTO|FALECEU|OBITO", s), "falecimento",
   fifelse(grepl("CASSA|PERDA DO MANDATO|PERDA DE MANDATO", s), "cassacao",
    fifelse(grepl(paste0("RENUNC|ELEIT[OA] (PARA )?(O CARGO DE )?(PREFEIT|VICE|GOVERNADOR|",
                         "SENADOR|DEPUTAD[OA] FEDERAL)|ASSUMIR A PREFEITURA|",
                         "TOMAR POSSE COMO PREFEIT"), s), "renuncia",
     fifelse(grepl("SECRETARI|MINISTR|NOMEAD|CARGO DE|INVESTIDURA|ASSUMIR A SECRETARIA", s), "afastamento",
      fifelse(grepl("LICEN", s), "licenca", NA_character_)))))
}

## ---- 9a. ata de posse: nomeia o suplente que entra e o titular que sai, com a causa
ap <- atos[padrao %in% c("ata_posse", "vaga_de", "assume_cadeira", "substituicao") & !is.na(pos_marc)]
ev_posse <- rbindlist(lapply(ap$aid, function(A) {
  a <- ap[aid == A]; o <- ocor[aid == A]
  if (!nrow(o)) return(NULL)
  # o presidente da Casa aparece antes do empossado ("o Senhor Presidente Fulano anunciou");
  # quem entra e o nome mais proximo da vaga que nao venha logo depois da palavra presidente
  cand <- o[pos < a$pos_marc][order(-pos)]
  if (nrow(cand)) {
    antes <- substr(a$tx, pmax(1L, cand$pos - 26L), cand$pos - 1L)
    depois <- substr(a$tx, cand$pos, cand$pos + 90L)
    cand <- cand[!grepl("PRESIDENTE\\s*$|PRESIDENCIA\\s*$|\\($", antes) &
                 !grepl("PRESIDENTE D[AO]\\s+ASSEMB|,?\\s*PRESIDENTE\\s+D", depois)]
  }
  ent <- if (nrow(cand)) cand[1] else data.table(rid = NA_integer_, pos = NA_integer_)
  # a vaga tem de ser de DEPUTADO: a mesma formula ("na vaga deixada pela renuncia do Sr. X")
  # aparece em sessao que elege conselheiro do Tribunal de Contas, e ali o nome mais proximo
  # depois do marcador e o presidente da sessao, nao quem deixou a cadeira
  cs <- o[pos > a$pos_marc][order(pos)]
  if (nrow(cs)) {
    # o PDF quebra a propria palavra "Deputado" na virada de linha ("Dep- utado")
    ctx <- stri_replace_all_regex(substr(a$tx, a$pos_marc, cs$pos - 1L), "-\\s", "")
    marcado <- substr(a$tx, pmax(1L, cs$pos - 2L), cs$pos - 1L)
    cs <- cs[grepl("DEPUTAD|PARLAMENTAR", ctx) & !grepl("\\(", marcado)]
  }
  sai <- if (nrow(cs)) cs[1] else data.table(rid = NA_integer_, pos = NA_integer_)
  # o ato tem de ser de cadeira de deputado; a mesma formula abre vaga no Tribunal de Contas
  txd <- stri_replace_all_regex(a$tx, "-\\s", "")
  if (!grepl(paste0("CADEIRA DE DEPUTAD|POSSE DE DEPUTAD|VAGA DE DEPUTAD|ASSUMIR A CADEIRA|",
                    "CADEIRA DE DEPU|POSSE AO DEPUTAD|SUBSTITUI O DEPUTAD|SUBSTITUICAO AO DEPUTAD"), txd))
    return(NULL)
  causa <- classifica_causa(substr(a$trecho, a$pos_marc, a$pos_marc + 320L))
  r <- list()
  jan <- recorta(a$trecho, a$pos_marc, 300L, 330L)
  d_ato <- data_por_extenso(substr(a$trecho, max(1L, a$pos_marc - 600L), a$pos_marc + 80L))
  if (!is.na(ent$rid)) r[[1]] <- data.table(aid = A, rid = ent$rid, papel = "entra",
                                            forma = "suplente_efetivado", causa_txt = jan,
                                            data_efeito = d_ato,
                                            nome_civil = descola(substr(a$trecho, ent$pos, ent$fim)))
  if (!is.na(sai$rid)) r[[2]] <- data.table(aid = A, rid = sai$rid, papel = "sai",
                                            forma = fifelse(is.na(causa), "outro", causa),
                                            causa_txt = jan, data_efeito = d_ato,
                                            nome_civil = NA_character_)
  rbindlist(r, use.names = TRUE, fill = TRUE)
}))
cat("eventos vindos da ata de posse:", nrow(ev_posse), "\n")

## ---- 9b. atos nomeados avulsos. O trecho quase sempre traz mais de um nome (o autor do
## requerimento, o presidente que assina, o orador). So conta o nome que ocupa a POSICAO do
## sujeito do ato, isto e, logo depois da expressao, sem outro qualificativo no meio.
RUIDO_SLOT <- paste0("FEDERAL|SENADOR|VEREADOR|MINISTR|GOVERNADOR|PREFEIT|EX-|\\bEX \\b|DESEMBARGADOR|",
                     "PAPA|PRESIDENTE DA REPUBLICA|SALA DAS SESSOES|\\(A|JUSTIFICATIVA|REQUERIMENTO|",
                     "ASSINAD|SUBSCREVE|AUTORIA")
avulso <- function(padroes, gatilho, forma, folga = 55L) {
  a <- atos[padrao %in% padroes]
  if (!nrow(a)) return(data.table())
  g <- stri_locate_first_regex(a$tx, gatilho)
  a <- a[!is.na(g[, 1])]; g <- g[!is.na(g[, 1]), , drop = FALSE]
  if (!nrow(a)) return(data.table())
  a[, `:=`(g_ini = g[, 1], g_fim = g[, 2])]
  o <- merge(ocor, a[, .(aid, g_ini, g_fim, tx, trecho)], by = "aid")
  o <- o[pos > g_fim & pos <= g_fim + folga]
  if (!nrow(o)) return(data.table())
  o[, meio := substr(tx, g_fim + 1L, pos - 1L)]
  o <- o[!grepl(RUIDO_SLOT, meio)]
  o <- o[, .SD[which.min(pos)], by = aid]                    # o mais proximo do gatilho
  if (!nrow(o)) return(data.table())
  o[, .(aid, rid, papel = "sai", forma = forma,
        causa_txt = mapply(recorta, trecho, g_ini, 120L, pos - g_ini + 160L))]
}
# o ato de pensao a viuva nomeia o parlamentar ANTES da expressao ("ex-Deputado Estadual
# Anibal Khury, falecido em 30 de agosto de 1999"), e por isso a busca aqui e para tras
avulso_antes <- function(padroes, gatilho, forma, folga = 90L) {
  a <- atos[padrao %in% padroes]
  if (!nrow(a)) return(data.table())
  g <- stri_locate_first_regex(a$tx, gatilho)
  a <- a[!is.na(g[, 1])]; g <- g[!is.na(g[, 1]), , drop = FALSE]
  if (!nrow(a)) return(data.table())
  a[, `:=`(g_ini = g[, 1], g_fim = g[, 2])]
  o <- merge(ocor, a[, .(aid, g_ini, g_fim, tx, trecho)], by = "aid")
  o <- o[pos < g_ini & pos >= g_ini - folga]
  if (!nrow(o)) return(data.table())
  o[, meio := substr(tx, pos, g_ini - 1L)]
  o <- o[!grepl("FEDERAL|SENADOR|VEREADOR|PREFEIT|GOVERNADOR|\\(A|;|:", meio)]
  o <- o[, .SD[which.max(pos)], by = aid]
  if (!nrow(o)) return(data.table())
  o[, .(aid, rid, papel = "sai", forma = forma,
        causa_txt = mapply(recorta, trecho, pos, 90L, g_fim - pos + 60L))]
}
ev_av <- rbindlist(list(
  avulso("falecimento", "FALECIMENTO D[EOA]S?", "falecimento"),
  avulso("cassacao", "CASSA[CR]\\w*\\s+(?:D?O?\\s*)?MANDATO(?:\\s+EXERCIDO)?(?:\\s+POR)?|PERDA D[EO] MANDATO D[EO]",
         "cassacao"),
  avulso(c("renuncia", "escritura_renuncia"),
         "REN[UU]NCIA (?:AO|DO|DE) MANDATO (?:DE )?(?:DEPUTADO )?(?:ESTADUAL )?D?[EO]?|REN[UU]NCIA D[EO]",
         "renuncia"),
  avulso("licenca", "LICEN[CQ]\\w*\\s+(?:D[OA]S?\\s+)?(?:PARA\\s+)?(?:EXERC[IY]CIO\\s+D[EO]\\s+)?MANDATO D[EO]",
         "licenca"),
  avulso("secretaria", "PARA EXERCER O CARGO DE SECRET\\w+ D[EO]|NOMEAD[OA] SECRET\\w+ D[EO]", "afastamento"),
  avulso_antes("falecido_em", "FALECID[OA] EM", "falecimento", 90L),
  avulso_antes("pedido_licenca", "SOLICITANDO LICEN|REQUER\\w* LICEN|PEDIDO DE LICEN", "licenca", 120L),
  avulso_antes("marca_licenca", "\\((?:EM )?LICEN[CQ]\\w*[^)]{0,20}\\)|\\(LICENCIAD[OA]\\)", "licenca", 42L)
))
cat("eventos vindos de ato avulso:", nrow(ev_av), "\n")

## ---- 9c. materia do DIOE cujo TITULO ja nomeia o ato e o parlamentar
# De 2011 em diante o inteiro teor esta no DIOE, e o sumario da edicao rotula cada materia
# ("Carta de Renuncia Augustinho Zucchi", "ATA DE POSSE DEP CRISTINA SILVESTRI"). O titulo e
# indice da propria Casa: quando ele nomeia uma pessoa so do roster, resolve a atribuicao que
# a busca dentro do texto deixaria ambigua. O texto citado continua saindo do corpo da materia.
classifica_titulo <- function(t) {
  s <- toupper(asc(t))
  fifelse(grepl("REN[UU]NC", s), "renuncia",
   fifelse(grepl("FALEC|OBITO|IN MEMORIAM", s), "falecimento",
    fifelse(grepl("CASSA|PERDA DE MANDATO", s), "cassacao",
     fifelse(grepl("\\bPOSSE\\b", s), "suplente_efetivado",
      fifelse(grepl("LICEN|LIBERA[CÇ][AÃ]O MANDATO", s), "licenca", NA_character_)))))
}
CHAVE_FORMA <- c(renuncia = "REN[UU]NC", falecimento = "FALEC|OBITO",
                 cassacao = "CASSA|PERDA D[EO] MANDATO", suplente_efetivado = "POSSE|ASSUM|COMPROMISSO",
                 licenca = "LICEN")
ev_tit <- data.table()
fidx <- file.path(raw, "dioe_indice_materias.csv")
if (file.exists(fidx)) {
  mi <- fread(fidx, encoding = "UTF-8", colClasses = "character")
  mi[, data := as.IDate(data)]
  mi[LEGS, on = .(data >= leg_inicio, data <= leg_fim), legislatura := i.legislatura]
  mi <- mi[!is.na(legislatura)]
  mi[, forma_tit := classifica_titulo(titulo)]
  em_cache <- list.files(file.path(raw, "dioe_materias"))
  mi <- mi[!is.na(forma_tit) & doc_id %in% em_cache]
  cat("materias do DIOE com titulo de ato e conteudo em cache:", nrow(mi), "\n")
  if (nrow(mi)) {
    mi[, linha := .I]
    mi[, tt := toupper(asc(titulo))]
    achou <- rbindlist(lapply(sort(unique(mi$legislatura)), function(L) {
      a <- mi[legislatura == L]; b <- alvos[legislatura == L]
      if (!nrow(a) || !nrow(b)) return(NULL)
      acha(a$tt, a$linha, b$rx, b$rid)[, .(linha = aid, rid, pos)]
    }))
    if (nrow(achou)) {
      achou <- achou[, .(pos = min(pos)), by = .(linha, rid)]
      achou <- achou[, if (uniqueN(rid) == 1L) .SD[1L], by = linha]  # titulo ambiguo nao entra
      achou <- merge(achou, mi[, .(linha, doc_id, forma_tit, data_mat = data, url_mat = url)],
                     by = "linha")
      # o texto citado sai do CORPO da materia, ancorado no nome da pessoa
      txts <- ler_corpus(raw, doc_ids = unique(achou$doc_id), com_p1 = FALSE)
      achou <- merge(achou, txts, by = "doc_id")
      rx_de <- alvos[, .(rx = list(rx)), by = rid]
      achou <- merge(achou, rx_de, by = "rid")
      achou[, pos_nome := {
        v <- integer(.N)
        for (k in seq_len(.N)) {
          p <- NA_integer_
          for (r in rx[[k]]) {
            q <- stri_locate_first_regex(toupper(asc(texto[k])), r)[1, 1]
            if (!is.na(q)) { p <- q; break }
          }
          v[k] <- p
        }
        v
      }]
      cat("materias tituladas com o nome tambem no corpo:", achou[!is.na(pos_nome), .N],
          "de", nrow(achou), "\n")
      cand <- copy(achou)
      # quando o corpo do ato nao repete o nome, cita-se o titulo, que e o texto que a Casa
      # publica no sumario da edicao e esta no cache como documento proprio
      cand[, `:=`(doc_sum = paste0("sumario_", as.character(data_mat), "_",
                                   stri_match_first_regex(doc_id, "^\\d{4}-\\d{2}-\\d{2}_(\\d+)_")[, 2]),
                  url_sum = paste0("https://dioe.pr.gov.br/portal/visualizacoes/view_html_diario/",
                                   stri_match_first_regex(doc_id, "^\\d{4}-\\d{2}-\\d{2}_(\\d+)_")[, 2]))]
      cand[, titulo_lit := mi$titulo[match(linha, mi$linha)]]
      cand[, titulo_lit := limpa(stri_replace_first_regex(titulo_lit, "^#\\S+\\s*-\\s*", ""))]
      if (nrow(cand)) {
        cand[, causa_txt := fifelse(!is.na(pos_nome),
                                    mapply(recorta, texto, fifelse(is.na(pos_nome), 1L, pos_nome), 260L, 340L),
                                    titulo_lit)]
        cand[is.na(pos_nome), `:=`(doc_id = doc_sum, url_mat = url_sum)]
        cand <- cand[nchar(causa_txt) >= 12L]
        ev_tit <- cand[, .(aid = NA_integer_, rid,
                           papel = fifelse(forma_tit == "suplente_efetivado", "entra", "sai"),
                           forma = forma_tit, causa_txt, doc_id, origem = "dioe_materia",
                           data_ato = data_mat, url = url_mat, padrao = "materia_titulada")]
      }
    }
  }
  cat("eventos vindos do titulo da materia:", nrow(ev_tit), "\n")
}

ev <- rbindlist(list(ev_posse, ev_av), use.names = TRUE, fill = TRUE)
ev <- merge(ev, atos[, .(aid, data_ato = data, doc_id, origem, url, padrao)], by = "aid")
if (nrow(ev_tit)) ev <- rbindlist(list(ev, ev_tit), use.names = TRUE, fill = TRUE)
ev <- merge(ev, ros[, .(rid, legislatura, nome_norm, primeira, ultima, condicao, id_mandato,
                        leg_inicio, leg_fim)], by = "rid")
# o ato so vale para quem estava em exercicio: a data tem de cair perto da janela observada
ev[, plausivel := is.na(primeira) |
     (data_ato >= primeira - 120L & data_ato <= ultima + 120L)]
cat("eventos com data compativel com a janela de exercicio:", ev[plausivel == TRUE, .N],
    "de", nrow(ev), "\n")
ev <- ev[plausivel == TRUE]
print(ev[, .N, by = .(forma, padrao)][order(forma, -N)])

saveRDS(list(atos = atos, ocor = ocor, ev = ev), file.path(raw, "eventos_PR.rds"))

## ---------------------------------------------------------------- 10. url de cada documento
idx <- fread(file.path(raw, "indice_diarios_alep.csv"), encoding = "UTF-8")
idx <- idx[grepl("^\\d{2}/\\d{2}/\\d{4}$", data)]
idx[, iso := paste(substr(data, 7, 10), substr(data, 4, 5), substr(data, 1, 2), sep = "-")]
# o coletor gerou o nome do arquivo trocando por hifen tudo que nao fosse [A-Za-z0-9] SEM
# rebaixar o acento antes ("Diario 129" virou "di-rio-129"); o indice reproduz as duas formas
sluga <- function(x, folder = TRUE) {
  z <- if (folder) asc(x) else x
  y <- tolower(stri_replace_all_regex(z, "[^A-Za-z0-9]+", "-"))
  stri_replace_all_regex(y, "^-|-$", "")
}
idx <- rbindlist(list(copy(idx)[, doc_id := paste0(iso, "_", sluga(nome, TRUE), ext)],
                      copy(idx)[, doc_id := paste0(iso, "_", sluga(nome, FALSE), ext)]))
idx <- unique(idx, by = "doc_id")
url_de <- function(doc) {
  u <- idx$url[match(doc, idx$doc_id)]
  e <- stri_match_first_regex(doc, "^exec_(\\d+)_(\\d+)$")
  ex <- !is.na(e[, 1])
  u[ex] <- paste0("https://dioe.pr.gov.br/portal/edicoes/download/", e[ex, 2], "/", e[ex, 3])
  d <- stri_match_first_regex(doc, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)\\.pdf$")
  dioe <- !is.na(d[, 1])
  u[dioe] <- paste0("https://dioe.pr.gov.br/portal/edicoes/download/", d[dioe, 3], "/1")
  m <- stri_match_first_regex(doc, "^(\\d{4}-\\d{2}-\\d{2})_(\\d+)_(\\d+)\\.html$")
  mat <- !is.na(m[, 1])
  u[mat] <- paste0("https://dioe.pr.gov.br/apifront/portal/edicoes/publicacoes_ver_conteudo/",
                   m[mat, 4], "/", m[mat, 3])
  fifelse(is.na(u), URL_ACERVO, u)
}
# arquivo da primeira e da ultima edicao em que a pessoa aparece
prim_arq <- pan[order(data), .(arq_prim = arquivo[1L], arq_ult = arquivo[.N]),
                by = .(legislatura, canonico)]
ros[prim_arq, on = c("legislatura", nome_norm = "canonico"),
    `:=`(arq_prim = i.arq_prim, arq_ult = i.arq_ult)]

## ---------------------------------------------------------------- 11. uma forma por linha
ev[, causa_txt := limpa(causa_txt)]
ev <- unique(ev, by = c("rid", "forma", "papel", "causa_txt"))
TERMINAL <- c("falecimento", "cassacao", "renuncia", "afastamento", "licenca")
# o ato so explica a SAIDA se a pessoa de fato parou de aparecer por volta dele
# comunicacao de retorno as funcoes parlamentares: prova que a licenca nao encerrou o mandato
volta <- avulso_antes("reassume", "REASSUM\\w+", "reassume", 130L)
if (nrow(volta)) {
  volta <- merge(volta, atos[, .(aid, data_volta = data)], by = "aid")
  volta <- volta[, .(volta_ult = max(data_volta)), by = rid]
} else volta <- data.table(rid = integer(), volta_ult = as.IDate(character()))
ev[volta, on = "rid", volta_ult := i.volta_ult]
cat("pessoas com comunicacao de retorno:", nrow(volta), "\n")
# 'outro' tambem encerra: a ata de posse que nao diz a causa ainda assim prova que a cadeira
# vagou, e o texto entra como lastro mesmo sendo rotulo derivavel da estrutura
ev[, encerra := papel == "sai" & forma %in% c(TERMINAL, "outro") &
     !is.na(ultima) & data_ato >= ultima - 60L & data_ato <= ultima + 200L &
     (is.na(volta_ult) | volta_ult <= data_ato | forma %in% c("falecimento", "cassacao"))]
# o ato de pensao diz a data do obito, que e a data real de fim do mandato
ev[padrao == "falecido_em", data_efeito := vapply(causa_txt, function(z)
  as.numeric(data_apos(z, "FALECID[OA] EM")), numeric(1))]
ev[, data_efeito := as.IDate(data_efeito)]
ev[, prio := PRIOR[forma]]
if (!"data_efeito" %in% names(ev)) ev[, data_efeito := as.IDate(NA)]
ev[, data_efeito := fifelse(!is.na(data_efeito) & data_efeito >= leg_inicio &
                              data_efeito <= leg_fim & data_efeito <= data_ato + 30L,
                            data_efeito, as.IDate(NA))]
cat("eventos com data do ato por extenso:", ev[!is.na(data_efeito), .N], "\n")
ev_sai <- ev[encerra == TRUE][order(rid, prio, -data_ato)][, .SD[1L], by = rid]
# quem entrou duas vezes na mesma legislatura tem duas atas; vale a do inicio do exercicio
ev_ent <- ev[papel == "entra"][, dist := abs(as.integer(data_ato - primeira))]
ev_ent <- ev_ent[order(rid, dist, data_ato)][, .SD[1L], by = rid]
cat("linhas com ato terminal:", nrow(ev_sai), "| com ato de posse de suplente:", nrow(ev_ent), "\n")

ros[, `:=`(forma_saida = NA_character_, causa_original = NA_character_,
           fonte_evento = NA_character_, url_evento = NA_character_, doc_evento = NA_character_)]
# ato de saida so encerra o mandato se a pessoa de fato parou de aparecer na relacao nominal;
# discurso sobre cassacao em curso, ou licenca de que se voltou, nao encerram nada
ev_sai <- ev_sai[rid %in% ros[ate_fim == FALSE, rid]]
cat("atos terminais confirmados pela saida da relacao nominal:", nrow(ev_sai), "\n")
ros[ev_sai, on = "rid", `:=`(forma_saida = i.forma, causa_original = i.causa_txt,
                             fonte_evento = paste0(fifelse(i.origem == "dioe_materia",
                                                           "dioe_alep_", "alep_diario_"), i.padrao),
                             url_evento = url_de(i.doc_id), doc_evento = i.doc_id,
                             data_evento = fifelse(is.na(i.data_efeito), i.data_ato, i.data_efeito))]
# suplente com ata de posse: a forma ja e suplente_efetivado, mas o texto entra como lastro
ros[ev_ent, on = "rid", `:=`(causa_posse = i.causa_txt, doc_posse = i.doc_id,
                             padrao_posse = i.padrao, data_posse = i.data_ato,
                             origem_posse = i.origem,
                             nome_civil = i.nome_civil, data_posse_ato = i.data_efeito)]

# derivacao estrutural onde o ato nao aparece
# quem so aparecia na relacao do portal mas tem ato de posse no diario e suplente comprovado
ros[condicao == "nao_informado" & !is.na(causa_posse), condicao := "suplente"]
ros[is.na(forma_saida) & condicao == "suplente", `:=`(forma_saida = "suplente_efetivado")]
ros[is.na(forma_saida) & condicao == "titular" & no_painel == TRUE & ate_fim == TRUE &
      leg_encerrada == TRUE, forma_saida := "fim_regular"]
ros[is.na(forma_saida) & condicao == "titular" & no_painel == TRUE & ate_fim == FALSE,
    forma_saida := "outro"]
ros[is.na(forma_saida) & no_painel == FALSE, forma_saida := "nao_observado"]
ros[is.na(forma_saida) & condicao == "titular" & leg_encerrada == FALSE, forma_saida := "nao_observado"]
ros[is.na(forma_saida), forma_saida := "nao_observado"]
# suplente que entrou com ata de posse ganha o texto; sem ata, e derivacao da estrutura
ros[condicao == "suplente" & !is.na(causa_posse) & is.na(causa_original),
    `:=`(causa_original = causa_posse, doc_evento = doc_posse,
         url_evento = url_de(doc_posse),
         fonte_evento = paste0(fifelse(origem_posse == "dioe_materia",
                                       "dioe_alep_", "alep_diario_"), padrao_posse))]
in_set(ros$forma_saida, VOCAB, "forma_saida do PR")
print(ros[, .N, by = .(condicao, forma_saida)][order(condicao, -N)])

## ---------------------------------------------------------------- 12. datas de exercicio
ros[, data_inicio := as.IDate(NA)]
ros[condicao == "titular" & desde_inicio == TRUE, data_inicio := leg_inicio]
ros[is.na(data_inicio) & !is.na(data_posse_ato), data_inicio := data_posse_ato]
ros[is.na(data_inicio) & no_painel == TRUE, data_inicio := primeira]
# a ata de posse do suplente tambem data a saida do titular que ele substituiu
ros[condicao == "suplente" & !is.na(data_posse_ato) & !is.na(primeira) & data_posse_ato < primeira,
    data_inicio := data_posse_ato]
ros[, data_fim := as.IDate(NA)]
ros[no_painel == TRUE & ate_fim == TRUE & leg_encerrada == TRUE, data_fim := leg_fim]
ros[no_painel == TRUE & is.na(data_fim) & ate_fim == FALSE, data_fim := ultima]
# ato terminal com data propria manda na data de fim
ros[!is.na(data_evento) & forma_saida %in% c(TERMINAL, "outro") & !is.na(data_fim) &
      data_evento >= data_inicio & data_evento <= leg_fim, data_fim := data_evento]
ros[!is.na(data_inicio) & !is.na(data_fim) & data_fim < data_inicio, data_fim := data_inicio]

## ---------------------------------------------------------------- 13. as 21 colunas
fonte_de <- function(base, portal, evento) {
  f <- base
  f <- fifelse(!is.na(portal) & base != "alep_portal_legislatura",
               paste0(f, "+alep_portal_legislatura"), f)
  fifelse(!is.na(evento) & evento != f, paste0(f, "+", evento), f)
}
out <- ros[, .(
  uf = "PR",
  fonte = fonte_de(fifelse(no_painel == TRUE, "alep_diario_expediente", "alep_portal_legislatura"),
                   nome_portal, fonte_evento),
  legislatura = as.character(legislatura),
  ano_eleicao = as.character(ano_eleicao),
  nome,
  nome_normalizado = nome_norm,
  nome_completo = fifelse(!is.na(nome_civil) & nchar(nome_civil) > nchar(nome), nome_civil,
                          NA_character_),
  data_nascimento = NA_character_,
  partido,
  condicao,
  data_inicio_exercicio = as.character(data_inicio),
  data_fim_exercicio = as.character(data_fim),
  causa_original,
  forma_saida,
  id_pessoa_bocel = id_pessoa,
  id_mandato_bocel = id_mandato,
  metodo_pareamento = metodo,
  url = fifelse(!is.na(url_evento), url_evento,
         fifelse(!is.na(arq_prim), url_de(arq_prim),
          fifelse(!is.na(url_portal), url_portal, URL_ACERVO))),
  id_fonte = fifelse(!is.na(doc_evento), doc_evento,
              fifelse(!is.na(arq_prim), arq_prim, id_fonte_portal)),
  votos_fonte = NA_character_,
  sexo_fonte = NA_character_
)]
setorder(out, ano_eleicao, nome_normalizado)
fwrite(out, file.path(outd, "PR.csv"), na = "NA", quote = TRUE)
cat("\ndata/assembleias2/PR.csv:", nrow(out), "linhas\n")
print(out[, .N, by = forma_saida][order(-N)])

## ---------------------------------------------------------------- 13a. saidas sem ato
# lista de quem deixou a cadeira antes do fim sem que o Diario da Assembleia publicasse o ato;
# e a entrada de python/assembleias2/busca_executivo_PR.py, que procura o decreto no Diario do
# Executivo (a unica parte do DIOE que tem busca por texto)
sem_ato <- ros[forma_saida == "outro" & !is.na(ultima),
               .(nome = nome, data_ini = as.character(pmax(ultima - 60L, leg_inicio)),
                 data_fim = as.character(pmin(ultima + 150L, leg_fim)))]
fwrite(sem_ato, file.path(raw, "nomes_sem_ato.csv"))
cat("saidas sem ato publicado no Diario da Assembleia:", nrow(sem_ato), "\n")

## ---------------------------------------------------------------- 13b. janelas de transicao
# De 2011 em diante o inteiro teor mora no DIOE, materia a materia, e baixar as 3.400 atas de
# sessao seria desproporcional. Estas sao as janelas em que a relacao nominal muda (alguem entra
# ou alguem sai fora da virada de legislatura): e nelas que a ata explica a troca.
tr <- ros[no_painel == TRUE & as.integer(legislatura) >= 17L]
datas <- unique(c(tr[desde_inicio == FALSE & !is.na(primeira), primeira],
                  tr[ate_fim == FALSE & !is.na(ultima), ultima]))
janelas <- sort(unique(paste0(as.character(datas - 45L), "..", as.character(datas + 45L))))
writeLines(janelas, file.path(raw, "janelas_sessao.txt"))
cat("janelas de transicao para busca de ata (2011-2026):", length(janelas), "\n")

## ---------------------------------------------------------------- 14. numeros assinados
reg("n_linhas", nrow(out))
reg("n_pareadas", out[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_bocel", nrow(dep))
reg("n_mandatos_com_forma", uniqueN(out[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)]$id_mandato_bocel))
reg("n_linhas_com_causa_original", out[!is.na(causa_original), .N])
reg("n_edicoes_expediente", uniqueN(pan$arquivo))
reg("n_obs_deputado_edicao", nrow(pan))
reg("n_trechos_de_ato", nrow(atos))
reg("n_eventos_atribuidos", nrow(ev))
for (f in sort(unique(out$forma_saida))) reg(paste0("forma_", f), out[forma_saida == f, .N])
for (a in sort(unique(out$ano_eleicao))) {
  reg(paste0("linhas_", a), out[ano_eleicao == a, .N])
  reg(paste0("com_forma_", a),
      uniqueN(out[ano_eleicao == a & forma_saida != "nao_observado" & !is.na(id_mandato_bocel)]$id_mandato_bocel))
}
cob <- out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)),
               com_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)])),
           by = ano_eleicao][order(ano_eleicao)]
print(cob)
fwrite(cob, file.path(verd, "asm2_PR_cobertura_legislatura.csv"))
fwrite(out[, .(ano_eleicao, nome, condicao, forma_saida, data_inicio_exercicio, data_fim_exercicio,
               id_mandato_bocel, metodo_pareamento, id_fonte)],
       file.path(verd, "asm2_PR_linhas.csv"))
fwrite(ev[, .(rid, legislatura, nome_norm, data_ato, forma, papel, padrao, doc_id)],
       file.path(verd, "asm2_PR_eventos.csv"))

saveRDS(list(ros = ros, dep = dep, per = per, pan = pan, grade = grade),
        file.path(raw, "roster_PR.rds"))
cat("\n02_build_PR concluido —", format(Sys.time()), "\n")
sink()
