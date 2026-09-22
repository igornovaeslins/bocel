#!/usr/bin/env Rscript
# monta_MA.R — exercicio e forma de saida dos deputados estaduais do Maranhao (ALEMA),
# a partir da relacao nominal que abre cada edicao do Diario da Assembleia.
#
# Fonte e metodo. A capa de toda edicao do Diario da Assembleia traz o expediente com a relacao
# nominal dos deputados EM EXERCICIO, agrupada por bloco, e, a parte, os LICENCIADOS. Como o
# Diario sai a cada sessao, essa relacao e uma serie quase diaria da composicao da Casa: a
# entrada de um nome data a posse do suplente e a saida de um nome data o fim do exercicio do
# titular. Em toda edicao conferida a relacao soma exatamente 42, o tamanho da bancada, de modo
# que ela e um censo da Casa e nao uma lista de presenca da sessao. O ato publicado no corpo do
# Diario, quando existe, da a causa, e vai literalmente para causa_original.
# O acervo do Diario comeca em 12/04/2004. A 14a legislatura (1999-2002) so tem a relacao
# capturada pelo Internet Archive no portal antigo (www.al.ma.gov.br/deputados.htm).
#
# Entrada:  data_raw/assembleias2/MA/diarios/{relacao_nominal,capas,atos,indice_*}.csv
#           data_raw/assembleias2/MA/wayback/relacao_wayback.csv
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/MA.csv
#           output/verificacao/asm2ma_*.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/monta_MA.R
set.seed(20260830)
suppressPackageStartupMessages({library(data.table); library(arrow); library(stringi)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))
script <- "R/assembleias2/monta_MA.R"
verd <- "output/verificacao"; dir.create(verd, showWarnings = FALSE, recursive = TRUE)
dir.create("data/assembleias2", showWarnings = FALSE, recursive = TRUE)
reg <- function(k, v) registrar_numero(paste0("asm2ma_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")
UF <- "MA"; CARGO <- "7"; HOJE <- as.IDate("2026-08-30"); CADEIRAS <- 42L
DIA <- "data_raw/assembleias2/MA/diarios"
LEG_ANO <- c("14" = 1998L, "15" = 2002L, "16" = 2006L, "17" = 2010L,
             "18" = 2014L, "19" = 2018L, "20" = 2022L)
LEG_INI <- setNames(as.IDate(c("1999-02-01", "2003-02-01", "2007-02-01", "2011-02-01",
                               "2015-02-01", "2019-02-01", "2023-02-01")), names(LEG_ANO))
LEG_FIM <- setNames(as.IDate(c("2003-01-31", "2007-01-31", "2011-01-31", "2015-01-31",
                               "2019-01-31", "2023-01-31", "2027-01-31")), names(LEG_ANO))

nn <- function(x) {
  x <- stri_trans_general(toupper(ifelse(is.na(x), "", x)), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
nz <- function(x) !is.na(x) & nzchar(x)
moda <- function(x) { x <- x[nz(x)]; if (!length(x)) NA_character_ else names(sort(table(x), decreasing = TRUE))[1] }

## ------------------------------------------------------------------ 1. serie de exercicio
rel <- fread(file.path(DIA, "relacao_nominal.csv"), colClasses = "character",
             na.strings = c("NA", ""), encoding = "UTF-8")
cap <- fread(file.path(DIA, "capas.csv"), colClasses = "character",
             na.strings = c("NA", ""), encoding = "UTF-8")
idx <- rbindlist(lapply(c("indice_antigo.csv", "indice_novo.csv"), function(f)
  fread(file.path(DIA, f), colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")),
  use.names = TRUE, fill = TRUE)
idx <- unique(idx[, .(arquivo, url)])[!duplicated(arquivo)]
rel[, arq_pdf := sub("\\.txt$", "", arquivo)]
rel <- merge(rel, idx, by.x = "arq_pdf", by.y = "arquivo", all.x = TRUE)
rel[, data := as.IDate(data)]
rel[, fonte := "diario_assembleia_capa"]

janela <- function(d) {
  out <- rep(NA_character_, length(d))
  for (k in names(LEG_INI)) out[!is.na(d) & d >= LEG_INI[[k]] & d <= LEG_FIM[[k]]] <- k
  out
}
rel[, leg_data := janela(data)]
rel[, leg := legislatura]
rel[is.na(leg) | !leg %in% names(LEG_ANO) | leg != leg_data, leg := leg_data]

wbf <- "data_raw/assembleias2/MA/wayback/relacao_wayback.csv"
if (file.exists(wbf)) {
  wb <- fread(wbf, colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
  wb[, data := as.IDate(data)]
  wb[, `:=`(leg = janela(data), fonte = "wayback_portal_antigo", arq_pdf = arquivo)]
  rel <- rbind(rel, wb, use.names = TRUE, fill = TRUE)
}
rel <- rel[!is.na(leg) & !is.na(data)]
rel[, nome_normalizado := nn(nome)]
rel <- rel[nzchar(nome_normalizado) & nchar(nome_normalizado) >= 3]
rel[, ano_eleicao := LEG_ANO[leg]]
cat("serie: ", nrow(rel), " observacoes nome x edicao | ", uniqueN(rel$arq_pdf),
    " edicoes | ", uniqueN(rel$data), " datas\n", sep = "")

## uma linha por (data, nome): em exercicio em qualquer edicao do dia = em exercicio
rel[, prio := fifelse(situacao == "exercicio", 1L, 2L)]
serie <- rel[order(data, nome_normalizado, prio)][, .SD[1], by = .(data, leg, nome_normalizado)]

datas <- unique(serie[, .(leg, data)])[order(leg, data)]
cob <- datas[, .(n_datas = .N, cob_ini = min(data), cob_fim = max(data)), by = leg]
cob[, `:=`(leg_ini = LEG_INI[leg], leg_fim = LEG_FIM[leg])]
cob[, `:=`(atraso_ini = as.integer(cob_ini - leg_ini), falta_fim = as.integer(leg_fim - cob_fim))]
setorder(cob, leg); print(cob)

## ------------------------------------------------------------------ 2. universo TSE / BOCEL
man <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
pes <- fread("data/pessoas.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
dep <- man[cd_cargo == CARGO & sg_uf == UF, .(id_mandato, id_pessoa, ano_eleicao = as.integer(ano_eleicao),
                                              sq_candidato, sg_partido, votos_turno_decisivo,
                                              mandato_inicio = as.IDate(mandato_inicio),
                                              mandato_fim = as.IDate(mandato_fim))]
dep <- merge(dep, pes[, .(id_pessoa, nome_bocel = nome, nome_urna_bocel = nome_urna_recente,
                          dt_nascimento)], by = "id_pessoa", all.x = TRUE)
cand <- rbindlist(lapply(sort(unique(dep$ano_eleicao)), function(a) {
  x <- setDT(read_parquet(sprintf("data_raw/parquet/cand_%d.parquet", a),
                          col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "SQ_CANDIDATO",
                                         "NM_URNA_CANDIDATO", "NM_CANDIDATO", "NM_TIPO_ELEICAO",
                                         "DT_NASCIMENTO", "DS_GENERO")))
  x <- x[SG_UF == UF & CD_CARGO == CARGO & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  unique(x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sq_candidato = as.character(SQ_CANDIDATO),
               nome_urna = NM_URNA_CANDIDATO, nome_civil = NM_CANDIDATO,
               dt_nasc_tse = DT_NASCIMENTO, genero = DS_GENERO)])
}))
cand <- cand[!duplicated(cand[, .(ano_eleicao, sq_candidato)])]
dep <- merge(dep, cand[, .(ano_eleicao, sq_candidato, nome_urna, nome_civil, dt_nasc_tse)],
             by = c("ano_eleicao", "sq_candidato"), all.x = TRUE)
dep[, leg := names(LEG_ANO)[match(ano_eleicao, LEG_ANO)]]
cat("eleitos BOCEL MA cd_cargo 7: ", nrow(dep), " | candidatos TSE no universo: ", nrow(cand), "\n", sep = "")

## ------------------------------------------------------------------ 3. pareamento por nome
# Cascata em ordem de forca, cada regra exigindo unicidade nos dois lados dentro do ano de
# eleicao. Passo A contra os ELEITOS (o nome vira titular); passo B contra os NAO eleitos do
# mesmo pleito (vira suplente, com sq_candidato). O que sobra fica suplente sem identificacao.
# Grafia divergente e regra: a Casa escreve REZENDE onde o TSE escreve RESENDE, e as capturas do
# portal antigo chegam sem os acentos, dai as regras por conteudo de tokens e por distancia.
tok <- function(x) strsplit(x, " ", fixed = TRUE)
nomes <- unique(serie[, .(ano_eleicao, leg, nome_normalizado)])

## Variantes graficas do mesmo nome, dentro da legislatura. As capturas do portal antigo chegam
## sem as letras acentuadas ("JOS JORGE LEITE SOARES" por "JOSE JORGE LEITE SOARES") e a Casa
## oscila entre REZENDE e RESENDE. Se as duas grafias entram separadas na cascata, elas quebram a
## exigencia de unicidade e NENHUMA e pareada. Por isso o agrupamento vem antes: cada grupo entra
## uma vez, com a grafia mais longa como representante, e o resultado volta para todas as formas.
## Duas grafias sao a mesma pessoa quando (a) o primeiro token coincide ou difere de uma letra,
## a contagem de tokens difere de no maximo um e a distancia total nao passa de tres; ou (b) o
## nome curto e o longo sem o titulo da frente ("CLEIDE COUTINHO" e "DR CLEIDE COUTINHO").
## Sem a condicao sobre o primeiro token, "HELIO SOARES" e "ADELMO SOARES" — dois deputados
## distintos da 19a legislatura — cairiam no mesmo grupo, a tres edicoes de distancia.
mesma_pessoa <- function(a, b, maxd = 3L) {
  d <- as.integer(adist(a, b))
  if (d > maxd) return(FALSE)
  ta <- tok(a)[[1]]; tb <- tok(b)[[1]]
  curto <- if (length(ta) <= length(tb)) ta else tb
  longo <- if (length(ta) <= length(tb)) tb else ta
  k <- length(longo) - length(curto)
  if (k >= 1L && identical(curto, longo[(k + 1L):length(longo)])) return(TRUE)
  if (abs(length(ta) - length(tb)) > 1L) return(FALSE)
  as.integer(adist(ta[1], tb[1])) <= 1L
}

agrupa <- function(nomes) {
  nomes[, nome_grupo := nome_normalizado]
  regs <- list()
  for (l in unique(nomes$leg)) {
    v <- nomes[leg == l]$nome_normalizado
    v <- v[order(-nchar(v), v)]
    chefe <- character(0)
    mapa <- setNames(v, v)
    for (a in v) {
      if (!length(chefe)) { chefe <- a; next }
      cand <- which(vapply(chefe, function(h) mesma_pessoa(a, h), TRUE))
      d <- as.integer(adist(a, chefe))
      j <- if (length(cand) == 1L) cand else
        if (length(cand) > 1L) cand[which.min(d[cand])][if (sum(d[cand] == min(d[cand])) == 1L) 1L else 0L] else integer(0)
      ok <- length(j) == 1L
      if (ok) { mapa[[a]] <- chefe[j]
        regs[[length(regs) + 1L]] <- data.table(leg = l, variante = a, canonico = chefe[j],
                                                distancia = min(d))
      } else chefe <- c(chefe, a)
    }
    nomes[leg == l, nome_grupo := unname(mapa[nome_normalizado])]
  }
  if (length(regs)) fwrite(rbindlist(regs), file.path(verd, "asm2ma_variantes_agrupadas.csv"))
  cat("variantes graficas agrupadas: ", length(regs), "\n", sep = "")
  nomes
}
nomes <- agrupa(nomes)
alvo <- unique(nomes[, .(ano_eleicao, leg, nome_normalizado = nome_grupo)])
alvo[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, sq_cand = NA_character_,
            metodo = NA_character_, nome_completo = NA_character_, dt_nasc = NA_character_,
            partido_tse = NA_character_)]
prim_ult <- function(x) vapply(tok(x), function(v)
  if (length(v) >= 2) paste(v[1], v[length(v)]) else NA_character_, "")
contido <- function(a, b) mapply(function(x, y) length(x) >= 1 && all(x %in% y), tok(a), tok(b))

aplica <- function(alvo, base, col_alvo, col_base, met, so_eleitos = TRUE) {
  liv <- alvo[is.na(metodo)]
  if (!nrow(liv)) return(alvo)
  a <- liv[, .(ano_eleicao, leg, nome_normalizado, chave = get(col_alvo))]
  a <- a[nz(chave)][, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- base[nz(get(col_base)), .(ano_eleicao, chave = get(col_base), id_mandato, id_pessoa,
                                 sq_candidato, nome_civil, dt_nasc, sg_partido)]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  if (so_eleitos) m <- m[!id_mandato %in% alvo$id_mandato]
  if (!nrow(m)) { cat(sprintf("  %-38s +0\n", met)); return(alvo) }
  alvo[m, on = .(ano_eleicao, leg, nome_normalizado),
       `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, sq_cand = i.sq_candidato,
            metodo = met, nome_completo = i.nome_civil, dt_nasc = i.dt_nasc, partido_tse = i.sg_partido)]
  cat(sprintf("  %-38s +%d\n", met, nrow(m)))
  alvo
}
casa_linha <- function(alvo, base, col_base, met, modo, maxd = 2L, min_tok = 1L) {
  liv <- alvo[is.na(metodo)]
  if (!nrow(liv)) return(alvo)
  res <- rbindlist(lapply(seq_len(nrow(liv)), function(i) {
    a <- liv$nome_normalizado[i]; an <- liv$ano_eleicao[i]
    b <- base[ano_eleicao == an & !id_mandato %in% alvo$id_mandato & nz(get(col_base))]
    if (!nrow(b)) return(NULL)
    if (modo == "contido") {
      if (length(tok(a)[[1]]) < min_tok) return(NULL)
      j <- which(contido(rep(a, nrow(b)), b[[col_base]]))
    } else if (modo == "contem") {
      # o caminho inverso: o nome de urna ('CAMILO FIGUEIREDO') dentro do nome civil que a
      # fonte publica ('CAMILO LELIS CARNEIRO FIGUEIREDO')
      j <- which(vapply(tok(b[[col_base]]), function(y)
        length(y) >= min_tok && all(y %in% tok(a)[[1]]), TRUE))
    } else {
      d <- as.integer(adist(a, b[[col_base]]))
      j <- which(d <= maxd & d == min(d))
    }
    if (length(j) != 1L) return(NULL)
    data.table(ano_eleicao = an, leg = liv$leg[i], nome_normalizado = a,
               id_mandato = b$id_mandato[j], id_pessoa = b$id_pessoa[j],
               sq_candidato = b$sq_candidato[j], nome_civil = b$nome_civil[j],
               dt_nasc = b$dt_nasc[j], sg_partido = b$sg_partido[j])
  }))
  if (!length(res) || !nrow(res)) { cat(sprintf("  %-38s +0\n", met)); return(alvo) }
  res <- res[, if (.N == 1L) .SD, by = .(ano_eleicao, id_mandato)]
  if (!nrow(res)) { cat(sprintf("  %-38s +0\n", met)); return(alvo) }
  res <- res[, if (.N == 1L) .SD, by = .(ano_eleicao, nome_normalizado)]
  if (!nrow(res)) { cat(sprintf("  %-38s +0\n", met)); return(alvo) }
  alvo[res, on = .(ano_eleicao, leg, nome_normalizado),
       `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, sq_cand = i.sq_candidato,
            metodo = met, nome_completo = i.nome_civil, dt_nasc = i.dt_nasc, partido_tse = i.sg_partido)]
  cat(sprintf("  %-38s +%d\n", met, nrow(res)))
  alvo
}

el <- copy(dep)
el[, `:=`(k_urna = nn(nome_urna), k_bocel = nn(nome_bocel), k_civil = nn(nome_civil),
          k_urna_rec = nn(nome_urna_bocel),
          dt_nasc = fifelse(nz(dt_nascimento), dt_nascimento, NA_character_))]
el[, `:=`(k_pu_urna = prim_ult(k_urna), k_pu_bocel = prim_ult(k_bocel))]
nao <- cand[!sq_candidato %in% dep$sq_candidato]
nao[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, sg_partido = NA_character_,
           k_urna = nn(nome_urna), k_civil = nn(nome_civil),
           dt_nasc = fifelse(nz(dt_nasc_tse), dt_nasc_tse, NA_character_))]
nao[, `:=`(k_bocel = k_civil, k_pu_urna = prim_ult(nn(nome_urna)), k_pu_bocel = prim_ult(nn(nome_civil)))]
alvo[, `:=`(k = nome_normalizado, k_pu = prim_ult(nome_normalizado))]

cat("pareamento contra os eleitos (titular):\n")
for (rr in list(c("k", "k_urna", "nome_parlamentar_x_urna"),
                c("k", "k_bocel", "nome_parlamentar_x_nome_bocel"),
                c("k", "k_civil", "nome_parlamentar_x_nome_civil_tse"),
                c("k", "k_urna_rec", "nome_parlamentar_x_urna_recente"),
                c("k_pu", "k_pu_urna", "primeiro_ultimo_x_urna"),
                c("k_pu", "k_pu_bocel", "primeiro_ultimo_x_nome_bocel")))
  alvo <- aplica(alvo, el, rr[1], rr[2], rr[3])
alvo <- casa_linha(alvo, el, "k_urna", "tokens_contidos_na_urna", "contido")
alvo <- casa_linha(alvo, el, "k_civil", "tokens_contidos_no_nome_civil", "contido", min_tok = 2L)
alvo <- casa_linha(alvo, el, "k_bocel", "tokens_contidos_no_nome_bocel", "contido", min_tok = 2L)
alvo <- casa_linha(alvo, el, "k_urna", "urna_contida_no_nome_da_fonte", "contem", min_tok = 2L)
alvo <- casa_linha(alvo, el, "k_bocel", "nome_bocel_contido_no_nome_da_fonte", "contem", min_tok = 3L)
alvo <- casa_linha(alvo, el, "k_civil", "nome_civil_contido_no_nome_da_fonte", "contem", min_tok = 3L)
alvo <- casa_linha(alvo, el, "k_urna", "urna_com_grafia_divergente", "adist")
alvo <- casa_linha(alvo, el, "k_bocel", "nome_bocel_com_grafia_divergente", "adist")
alvo <- casa_linha(alvo, el, "k_civil", "nome_civil_com_grafia_divergente", "adist")
cat("pareamento contra os nao eleitos do mesmo pleito (suplente):\n")
for (rr in list(c("k", "k_urna", "suplente_nome_parlamentar_x_urna"),
                c("k", "k_civil", "suplente_nome_parlamentar_x_nome_civil"),
                c("k_pu", "k_pu_urna", "suplente_primeiro_ultimo_x_urna")))
  alvo <- aplica(alvo, nao, rr[1], rr[2], rr[3], so_eleitos = FALSE)
## Variante grafica dentro da mesma legislatura. As capturas do portal antigo chegam sem as
## letras acentuadas ("STNIO" por "STENIO") e a propria Casa oscila entre REZENDE e RESENDE, de
## modo que a mesma pessoa aparece sob duas grafias e o mandato, ja tomado pela primeira, fica
## fora do alcance da segunda. Esta passagem absorve a variante quando ela esta a no maximo tres
## edicoes de distancia de um nome ja pareado da mesma legislatura, o casamento e unico, e as duas
## grafias compartilham o primeiro ou o ultimo token.
absorve_variantes <- function(alvo) {
  n <- 0L
  for (l in unique(alvo$leg)) {
    pareados <- alvo[leg == l & !is.na(id_mandato)]
    livres <- alvo[leg == l & is.na(id_mandato)]
    if (!nrow(pareados) || !nrow(livres)) next
    for (i in seq_len(nrow(livres))) {
      a <- livres$nome_normalizado[i]
      cand <- which(vapply(pareados$nome_normalizado, function(h) mesma_pessoa(a, h), TRUE))
      if (length(cand) != 1L) next
      j <- cand
      alvo[leg == l & nome_normalizado == a,
           `:=`(id_mandato = pareados$id_mandato[j], id_pessoa = pareados$id_pessoa[j],
                sq_cand = pareados$sq_cand[j], metodo = "variante_grafica_absorvida",
                nome_completo = pareados$nome_completo[j], dt_nasc = pareados$dt_nasc[j],
                partido_tse = pareados$partido_tse[j])]
      n <- n + 1L
    }
  }
  cat(sprintf("  %-38s +%d\n", "variante_grafica_absorvida", n))
  alvo
}
alvo <- absorve_variantes(alvo)

alvo <- merge(nomes[, .(ano_eleicao, leg, nome_normalizado, nome_grupo)],
              alvo, by.x = c("ano_eleicao", "leg", "nome_grupo"),
              by.y = c("ano_eleicao", "leg", "nome_normalizado"), all.x = TRUE)
alvo[, condicao := fifelse(!is.na(id_mandato), "titular", "suplente")]
cat("nomes distintos na serie: ", nrow(alvo), " | titulares pareados: ",
    alvo[!is.na(id_mandato), .N], " | suplentes: ", alvo[is.na(id_mandato), .N], "\n", sep = "")
fwrite(alvo[order(ano_eleicao, nome_normalizado)], file.path(verd, "asm2ma_pareamento.csv"), na = "NA")

## ------------------------------------------------------------------ 4. chave de pessoa
# A grafia do nome oscila entre edicoes (REZENDE/RESENDE) e as capturas do portal antigo chegam
# sem acento. Por isso a identidade da pessoa dentro da legislatura e o mandato ou a candidatura
# a que o nome foi pareado, e nao o nome; o nome so serve de chave quando nao houve pareamento.
serie <- merge(serie, alvo[, .(ano_eleicao, leg, nome_normalizado, nome_grupo, id_mandato,
                               id_pessoa, sq_cand, metodo, nome_completo, dt_nasc, condicao)],
               by = c("ano_eleicao", "leg", "nome_normalizado"), all.x = TRUE)
# quando nao ha pareamento, a chave e o NOME DO GRUPO, e nao a grafia bruta: sem isso as duas
# grafias de um suplente nao pareado contam como duas pessoas e a relacao nunca fecha em 42
serie[is.na(nome_grupo), nome_grupo := nome_normalizado]
serie[, chave := fifelse(!is.na(id_mandato), id_mandato,
                  fifelse(!is.na(sq_cand), paste0("SQ_", sq_cand),
                          paste0("NM_", leg, "_", nome_grupo)))]

## censo: so entra como prova de ausencia a data em que a relacao soma as 42 cadeiras
# o censo conta PESSOAS, e nao linhas: a mesma pessoa aparece na bancada e na Mesa, e a grafia
# oscila entre as duas ("Josimar de Maranhaozinho" e "Josimar Maranhaozinho"), de modo que a
# contagem tem de ser sobre a chave pareada, que ja absorveu a variante
censo <- serie[situacao == "exercicio", .(n_ex = uniqueN(chave)), by = .(leg, data)]
censo <- merge(unique(serie[, .(leg, data)]), censo, by = c("leg", "data"), all.x = TRUE)
censo[is.na(n_ex), n_ex := 0L]
censo[, completo := n_ex == CADEIRAS]
cat("datas com relacao completa (", CADEIRAS, " nomes): ", censo[completo == TRUE, .N], " de ",
    nrow(censo), "\n", sep = "")
fwrite(censo[order(leg, data)], file.path(verd, "asm2ma_censo_por_data.csv"))
datas_cpl <- split(censo[completo == TRUE]$data, censo[completo == TRUE]$leg)

ex <- serie[situacao == "exercicio"]
li <- serie[situacao == "licenciado"]
sp <- ex[, .(prim_ex = min(data), ult_ex = max(data), n_ex = .N), by = .(leg, chave)]
spl <- li[, .(prim_lic = min(data), ult_lic = max(data), n_lic = .N), by = .(leg, chave)]
sp <- merge(sp, spl, by = c("leg", "chave"), all = TRUE)
ident <- serie[, .(ano_eleicao = ano_eleicao[1], nome = moda(nome), nome_normalizado = moda(nome_normalizado),
                   id_mandato = id_mandato[1], id_pessoa = id_pessoa[1], sq_cand = sq_cand[1],
                   metodo = metodo[1], nome_completo = nome_completo[1], dt_nasc = dt_nasc[1],
                   condicao = condicao[1], sexo_fonte = moda(sexo_fonte),
                   fonte = paste(sort(unique(fonte)), collapse = "+")), by = .(leg, chave)]
sp <- merge(sp, ident, by = c("leg", "chave"), all.x = TRUE)
part <- ex[nz(partido), .(partido = partido[which.max(data)]), by = .(leg, chave)]
sp <- merge(sp, part, by = c("leg", "chave"), all.x = TRUE)
sp <- merge(sp, cob[, .(leg, cob_ini, cob_fim, n_datas)], by = "leg", all.x = TRUE)
sp <- merge(sp, dep[, .(id_mandato, mandato_inicio, mandato_fim)], by = "id_mandato", all.x = TRUE)

## A prova de permanencia e de saida vem das datas em que a relacao esta COMPLETA (soma as 42
## cadeiras). Contar dias ate o fim da serie enganaria: a ultima sessao ordinaria da legislatura
## e em dezembro, e as edicoes de janeiro sao administrativas, de modo que quem ficou ate o fim
## aparece 45 dias antes do ultimo Diario. Por isso a regra e posicional: quem esta em uma das
## cinco ultimas relacoes completas ficou; quem falta em tres ou mais relacoes completas depois
## da sua ultima presenca saiu.
ULT_K <- 5L; MIN_AUSENCIAS <- 3L
leg_corrente <- names(LEG_FIM)[LEG_FIM >= HOJE]
ultimas_cpl <- lapply(datas_cpl, function(v) utils::tail(sort(v), ULT_K))
presentes_em <- ex[, .(leg, chave, data)]
setkey(presentes_em, leg, chave)
sp[, n_censos_depois := mapply(function(l, d)
  if (is.na(d) || is.null(datas_cpl[[l]])) 0L else sum(datas_cpl[[l]] > d), leg, ult_ex)]
sp[, nas_ultimas := mapply(function(l, ch) {
  u <- ultimas_cpl[[l]]
  if (is.null(u) || !length(u)) return(NA)
  nrow(presentes_em[.(l, ch)][data %in% u]) > 0L
}, leg, chave)]
sp[, dias_ate_fim_cob := as.integer(cob_fim - ult_ex)]
sp[, serie_alcanca_fim := !is.na(cob_fim) & as.integer(LEG_FIM[leg] - cob_fim) <= 60L]
sp[, saiu_antes := !is.na(ult_ex) & n_censos_depois >= MIN_AUSENCIAS &
     (is.na(nas_ultimas) | nas_ultimas == FALSE)]
## Na legislatura em curso a serie ainda esta aberta, e uma ausencia de poucas semanas nao
## distingue saida de licenca curta. Exige-se ao menos 90 dias entre a ultima presenca e o fim
## da serie antes de tratar a ausencia como saida.
sp[leg %in% leg_corrente & (is.na(dias_ate_fim_cob) | dias_ate_fim_cob < 90L), saiu_antes := FALSE]
sp[, lic_depois := !is.na(ult_lic) & !is.na(ult_ex) & ult_lic > ult_ex]

## trecho literal do expediente que registra a licenca, e url das edicoes de referencia
lic_txt <- li[order(data)][, .SD[.N], by = .(leg, chave)][
  , .(leg, chave, trecho_lic = trecho, arq_lic = arq_pdf, url_lic = url, obs_lic = obs)]
ult_arq <- ex[order(data)][, .SD[.N], by = .(leg, chave)][
  , .(leg, chave, arq_ult = arq_pdf, url_ult = url)]
sp <- merge(sp, lic_txt, by = c("leg", "chave"), all.x = TRUE)
sp <- merge(sp, ult_arq, by = c("leg", "chave"), all.x = TRUE)

## ------------------------------------------------------------------ 5. atos citados no Diario
atos <- fread(file.path(DIA, "atos.csv"), colClasses = "character", na.strings = c("NA", ""),
              encoding = "UTF-8")
if (nrow(atos)) {
  atos[, data := as.IDate(data)]
  atos[, arq_pdf := sub("\\.txt$", "", arquivo)]
  atos <- merge(atos, idx, by.x = "arq_pdf", by.y = "arquivo", all.x = TRUE)
  atos[, frase_norm := nn(frase)]
  if (!"nome_ato_normalizado" %in% names(atos)) atos[, nome_ato_normalizado := NA_character_]
} else {
  atos <- data.table(arq_pdf = character(), data = as.IDate(character()), tipo = character(),
                     frase = character(), url = character(), frase_norm = character(),
                     nome_ato_normalizado = character())
}
FORMA_ATO <- c(renuncia = "renuncia", falecimento = "falecimento", cassacao = "cassacao",
               afastamento = "afastamento", licenca = "licenca")
## O ato so vale para a pessoa que a PROPRIA FORMULA designa (o grupo <nome> capturado na
## extracao), e nao para qualquer nome que apareca na frase. A frase "em virtude da vaga aberta
## devido ao falecimento do Deputado Jose Gentil, o Deputado Edivaldo Holanda passa a condicao de
## titular" nomeia dois deputados, e o casamento pela frase inteira daria o falecimento ao errado.
casa_ato <- function(grafias, ref, antes = 45L, depois = 120L) {
  if (is.na(ref) || !nrow(atos)) return(NULL)
  a <- atos[data >= ref - antes & data <= ref + depois & tipo %in% names(FORMA_ATO) &
              nz(nome_ato_normalizado)]
  if (!nrow(a)) return(NULL)
  hit <- a[nome_ato_normalizado %in% grafias]
  if (!nrow(hit)) return(NULL)
  ordem <- c("falecimento", "cassacao", "renuncia", "afastamento", "licenca")
  hit[, ord := match(tipo, ordem)]
  setorder(hit, ord, data)
  hit[1]
}
## grafias da pessoa: a canonica mais as variantes que o agrupamento absorveu
graf_leg <- nomes[, .(leg, nome_grupo, nome_normalizado)]
grafias_de <- function(l, nm) {
  g <- graf_leg[leg == l & nome_normalizado == nm]$nome_grupo
  if (!length(g)) g <- nm
  unique(c(nm, g, graf_leg[leg == l & nome_grupo %in% g]$nome_normalizado))
}

## ------------------------------------------------------------------ 6. forma de saida
sp[, `:=`(forma_saida = NA_character_, causa_original = NA_character_, url_evid = NA_character_,
          id_fonte = NA_character_)]
marca_lic <- function(i) {
  obs <- nn(sp$obs_lic[i])
  txt <- nn(sp$trecho_lic[i])
  if (grepl("SECRETARI|MINISTR|PREFEIT|CARGO", paste(obs, txt))) "afastamento" else "licenca"
}
for (i in seq_len(nrow(sp))) {
  l <- sp$leg[i]
  em_curso <- l %in% leg_corrente
  if (is.na(sp$ult_ex[i])) {
    # so aparece na relacao de licenciados, nunca em exercicio na janela coberta
    if (!is.na(sp$ult_lic[i]) && nz(sp$trecho_lic[i])) {
      sp$forma_saida[i] <- marca_lic(i)
      sp$causa_original[i] <- sp$trecho_lic[i]
      sp$url_evid[i] <- sp$url_lic[i]
      sp$id_fonte[i] <- sp$arq_lic[i]
    } else sp$forma_saida[i] <- "nao_observado"
    next
  }
  if (!sp$saiu_antes[i]) {
    if (sp$condicao[i] %in% "suplente") { sp$forma_saida[i] <- "suplente_efetivado"
      sp$url_evid[i] <- sp$url_ult[i]; sp$id_fonte[i] <- sp$arq_ult[i]; next }
    # fim_regular exige ver a pessoa numa das ultimas relacoes completas da legislatura, e a
    # serie tem de alcancar o fim dela; mandato em curso nunca recebe fim_regular
    permaneceu <- !em_curso && isTRUE(sp$serie_alcanca_fim[i]) && isTRUE(sp$nas_ultimas[i])
    sp$forma_saida[i] <- if (permaneceu) "fim_regular" else "nao_observado"
    if (permaneceu) { sp$url_evid[i] <- sp$url_ult[i]; sp$id_fonte[i] <- sp$arq_ult[i] }
    next
  }
  ato <- casa_ato(grafias_de(l, sp$nome_normalizado[i]), sp$ult_ex[i])
  if (!is.null(ato)) {
    sp$forma_saida[i] <- FORMA_ATO[[ato$tipo]]
    sp$causa_original[i] <- ato$frase
    sp$url_evid[i] <- ato$url
    sp$id_fonte[i] <- ato$arq_pdf
  } else if (isTRUE(sp$lic_depois[i]) && nz(sp$trecho_lic[i])) {
    sp$forma_saida[i] <- marca_lic(i)
    sp$causa_original[i] <- sp$trecho_lic[i]
    sp$url_evid[i] <- sp$url_lic[i]
    sp$id_fonte[i] <- sp$arq_lic[i]
  } else {
    sp$forma_saida[i] <- if (sp$condicao[i] %in% "suplente") "suplente_efetivado" else "outro"
    sp$url_evid[i] <- sp$url_ult[i]
    sp$id_fonte[i] <- sp$arq_ult[i]
  }
}
## licenca so e saida quando a serie nao mostra a pessoa de volta ao exercicio
volta <- sp[forma_saida %in% c("licenca", "afastamento") & !is.na(ult_ex) & !is.na(ult_lic) &
              ult_ex > ult_lic]
if (nrow(volta)) sp[volta, on = .(leg, chave), forma_saida := "nao_observado"]

## ------------------------------------------------------------------ 7. datas de exercicio
sp[, ini := as.character(prim_ex)]
sp[condicao == "titular" & !is.na(prim_ex) & !is.na(cob_ini) & prim_ex <= cob_ini + 15L &
     !is.na(mandato_inicio) & as.integer(cob_ini - mandato_inicio) <= 45L,
   ini := as.character(pmin(mandato_inicio, prim_ex))]
sp[, fim := as.character(ult_ex)]
sp[forma_saida == "fim_regular", fim := as.character(LEG_FIM[leg])]
sp[forma_saida == "suplente_efetivado" & !saiu_antes & !leg %in% leg_corrente &
     serie_alcanca_fim & nas_ultimas %in% TRUE, fim := as.character(LEG_FIM[leg])]
sp[forma_saida == "nao_observado", fim := NA_character_]

## ------------------------------------------------------------------ 8. eleitos ausentes da serie
# Um titular que nao aparece em NENHUMA relacao completa da sua legislatura nao esta na Casa.
# Quando ha censo completo (42 nomes) e ele nao esta la, a saida esta observada pela estrutura,
# ainda que sem ato: o rotulo honesto e 'outro'. Sem censo completo na legislatura, nao ha o que
# afirmar, e a linha fica nao_observado.
falta <- dep[!id_mandato %in% sp$id_mandato]
falta <- merge(falta, cob[, .(leg, cob_ini, cob_fim)], by = "leg", all.x = TRUE)
falta[, n_censos := vapply(leg, function(l) if (is.null(datas_cpl[[l]])) 0L else length(datas_cpl[[l]]), 1L)]
falta[, forma_saida := fifelse(n_censos >= 2L, "outro", "nao_observado")]
falta[, url_evid := fifelse(n_censos >= 2L,
                            "https://www.al.ma.leg.br/sitealema/busca-diarios/",
                            "https://www.al.ma.leg.br/sitealema/busca-diarios/")]
cat("eleitos sem nenhuma aparicao na serie: ", nrow(falta), " (com censo completo: ",
    falta[n_censos >= 2L, .N], ")\n", sep = "")

## ------------------------------------------------------------------ 9. saida no esquema comum
iso <- function(x) fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", x),
                           sub("^(\\d{2})/(\\d{2})/(\\d{4})$", "\\3-\\2-\\1", x),
                           fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_))
out <- sp[, .(uf = UF, fonte, legislatura = leg, ano_eleicao = as.character(LEG_ANO[leg]),
              nome, nome_normalizado, nome_completo, data_nascimento = iso(dt_nasc),
              partido, condicao, data_inicio_exercicio = ini, data_fim_exercicio = fim,
              causa_original, forma_saida, id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato,
              metodo_pareamento = metodo, url = fifelse(nz(url_evid), url_evid, url_ult),
              id_fonte, votos_fonte = NA_character_, sexo_fonte)]
out2 <- falta[, .(uf = UF, fonte = "ausente_da_relacao_nominal", legislatura = leg,
                  ano_eleicao = as.character(ano_eleicao),
                  nome = fifelse(nz(nome_urna), nome_urna, nome_bocel),
                  nome_normalizado = nn(fifelse(nz(nome_urna), nome_urna, nome_bocel)),
                  nome_completo = fifelse(nz(nome_civil), nome_civil, nome_bocel),
                  data_nascimento = iso(dt_nascimento), partido = sg_partido, condicao = "titular",
                  data_inicio_exercicio = NA_character_, data_fim_exercicio = NA_character_,
                  causa_original = NA_character_, forma_saida, id_pessoa_bocel = id_pessoa,
                  id_mandato_bocel = id_mandato, metodo_pareamento = NA_character_, url = url_evid,
                  id_fonte = NA_character_, votos_fonte = NA_character_, sexo_fonte = NA_character_)]
out <- rbind(out, out2, use.names = TRUE)

VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
out[is.na(forma_saida) | forma_saida == "", forma_saida := "nao_observado"]
out[nz(data_inicio_exercicio) & nz(data_fim_exercicio) & data_inicio_exercicio > data_fim_exercicio,
    data_fim_exercicio := data_inicio_exercicio]
in_set(out$forma_saida, VOCAB, nome = "forma_saida MA")
in_set(out$condicao, c("titular", "suplente", "nao_informado"), nome = "condicao MA")
em_faixa(as.integer(out$ano_eleicao), 1998, 2024, nome = "ano_eleicao MA")
stopifnot(all(nz(out$url)))
checa_unica(as.data.frame(out), c("ano_eleicao", "nome_normalizado"))
stopifnot(out[!is.na(id_mandato_bocel), !anyDuplicated(id_mandato_bocel)])
setorder(out, ano_eleicao, condicao, nome_normalizado)
fwrite(out, "data/assembleias2/MA.csv", na = "NA", quote = TRUE)

## ------------------------------------------------------------------ 10. numeros assinados
obs <- out[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)]
reg("n_edicoes_em_cache", uniqueN(rel[fonte == "diario_assembleia_capa"]$arq_pdf))
reg("n_datas_na_serie", uniqueN(rel$data))
reg("n_observacoes_nome_edicao", nrow(rel))
reg("n_datas_censo_completo", censo[completo == TRUE, .N])
reg("n_linhas", nrow(out))
reg("n_pareadas", out[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_bocel", nrow(dep))
reg("n_mandatos_com_forma", uniqueN(obs$id_mandato_bocel))
reg("taxa_pareamento", sprintf("%d/%d=%.4f", out[!is.na(id_mandato_bocel), .N], nrow(dep),
                               out[!is.na(id_mandato_bocel), .N] / nrow(dep)))
reg("taxa_forma_observada", sprintf("%d/%d=%.4f", uniqueN(obs$id_mandato_bocel), nrow(dep),
                                    uniqueN(obs$id_mandato_bocel) / nrow(dep)))
fs <- out[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fs))) reg(paste0("n_forma_", fs$forma_saida[i]), fs$N[i])
mp <- out[!is.na(metodo_pareamento), .N, by = metodo_pareamento][order(-N)]
for (i in seq_len(nrow(mp))) reg(paste0("n_metodo_", mp$metodo_pareamento[i]), mp$N[i])
pl <- out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)),
              com_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado" & !is.na(id_mandato_bocel)])),
          by = .(ano_eleicao, legislatura)][order(ano_eleicao)]
for (i in seq_len(nrow(pl))) reg(paste0("com_forma_leg_", pl$legislatura[i]), pl$com_forma[i])
fwrite(pl, file.path(verd, "asm2ma_cobertura_legislatura.csv"))
fwrite(cob, file.path(verd, "asm2ma_cobertura_serie.csv"))
cat("\nforma de saida:\n"); print(fs)
cat("\ncobertura por legislatura:\n"); print(pl)
cat("\nmonta_MA: ", nrow(out), " linhas | ", out[!is.na(id_mandato_bocel), .N], " pareadas | ",
    uniqueN(obs$id_mandato_bocel), " mandatos com forma observada\n", sep = "")
