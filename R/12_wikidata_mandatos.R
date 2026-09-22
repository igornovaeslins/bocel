# 12_wikidata_mandatos.R — mandatos de governador, vice-governador, deputado estadual/distrital
# e prefeito no Wikidata (P39/P6 com qualificadores), pareados ao BOCEL.
# Entrada:  data_raw/wikidata/{esqueleto_*.json, humanos.json, statements.json, entidades.json, posicoes.json}
#           (R/coleta/wikidata.R), data/pessoas.csv, data/mandatos.csv, data/municipios_tse_ibge.csv
# Saida:    data/wikidata_mandatos.csv, data/wikidata_obitos.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/12_wikidata_mandatos.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
wd <- "data_raw/wikidata"
qid <- function(u) sub(".*/", "", u)
d10 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", "", x); gsub(" +", " ", trimws(x)) }

## ---------------------------------------------------------------- carga
esq <- rbindlist(lapply(list.files(wd, pattern = "^esqueleto_.*\\.json$", full.names = TRUE), function(f) {
  x <- fromJSON(f); if (length(x) == 0) return(NULL)
  x <- as.data.table(x); x[, esqueleto := sub("^esqueleto_(.*)\\.json$", "\\1", basename(f))]
  for (k in c("pos", "ent")) if (!k %in% names(x)) x[, (k) := NA_character_]
  x[, .(h = qid(h), s, pos = qid(pos), ent = qid(ent), esqueleto)]
}), use.names = TRUE)
esq <- unique(esq)
hum <- as.data.table(fromJSON(file.path(wd, "humanos.json")))
for (k in c("rotulo", "rotulo_en", "nome_completo", "dt_nasc", "dt_morte", "sexoLabel")) if (!k %in% names(hum)) hum[, (k) := NA_character_]
hum <- hum[, .(h = qid(h), rotulo = fcoalesce(rotulo, rotulo_en), nome_completo,
               dt_nasc = d10(dt_nasc), dt_morte = d10(dt_morte), sexo = sexoLabel)]
hum <- hum[!duplicated(h)]
stm <- as.data.table(fromJSON(file.path(wd, "statements.json")))
for (k in c("inicio", "fim", "substitui", "substituido_por", "causa_fimLabel", "partidoLabel", "eleicaoLabel"))
  if (!k %in% names(stm)) stm[, (k) := NA_character_]
stm <- stm[, .(s, inicio = d10(inicio), fim = d10(fim), substitui = qid(substitui),
               substituido_por = qid(substituido_por), causa_fim = causa_fimLabel,
               partido_wd = partidoLabel, eleicao_wd = eleicaoLabel)][!duplicated(s)]
ent <- as.data.table(fromJSON(file.path(wd, "entidades.json")))
for (k in c("ibge", "iso", "uf_iso", "rotulo", "tipoLabel")) if (!k %in% names(ent)) ent[, (k) := NA_character_]
ent <- ent[, .(ent = qid(ent), ent_rotulo = rotulo, ibge = ibge,
               uf = sub("^BR-", "", fcoalesce(iso, uf_iso)), tipo_ent = tipoLabel)][!duplicated(ent)]
posl <- as.data.table(fromJSON(file.path(wd, "posicoes.json")))[, .(pos = qid(pos), pos_rotulo = rotulo)][!duplicated(pos)]

w <- merge(esq, hum, by = "h", all.x = TRUE)
w <- merge(w, stm, by = "s", all.x = TRUE)
w <- merge(w, ent, by = "ent", all.x = TRUE)
w <- merge(w, posl, by = "pos", all.x = TRUE)
w[, cargo_wd := fcase(grepl("^vice_presidente", esqueleto), "VICE-PRESIDENTE",
                      grepl("^presidente", esqueleto), "PRESIDENTE",
                      grepl("^vice_governador", esqueleto), "VICE-GOVERNADOR",
                      grepl("^governador", esqueleto), "GOVERNADOR",
                      grepl("^deputado_estadual", esqueleto), "DEPUTADO ESTADUAL",
                      grepl("^prefeito", esqueleto), "PREFEITO", default = NA_character_)]
# UF da posicao quando o rotulo da posicao a traz (ex.: 'governador do Ceara') e a entidade nao
ufs <- fread(text = "uf,nome\nAC,Acre\nAL,Alagoas\nAP,Amapa\nAM,Amazonas\nBA,Bahia\nCE,Ceara\nDF,Distrito Federal\nES,Espirito Santo\nGO,Goias\nMA,Maranhao\nMT,Mato Grosso\nMS,Mato Grosso do Sul\nMG,Minas Gerais\nPA,Para\nPB,Paraiba\nPR,Parana\nPE,Pernambuco\nPI,Piaui\nRJ,Rio de Janeiro\nRN,Rio Grande do Norte\nRS,Rio Grande do Sul\nRO,Rondonia\nRR,Roraima\nSC,Santa Catarina\nSP,Sao Paulo\nSE,Sergipe\nTO,Tocantins")
w[is.na(uf) & !is.na(pos_rotulo), uf := {
  r <- norm(pos_rotulo)
  vapply(r, function(z) { m <- ufs$uf[vapply(ufs$nome, function(n) grepl(paste0("\\b", norm(n), "\\b"), z), logical(1))]
                          if (length(m) == 1) m else NA_character_ }, character(1))
}]
w[cargo_wd == "DEPUTADO ESTADUAL" & uf == "DF", cargo_wd := "DEPUTADO DISTRITAL"]
w[cargo_wd %in% c("PRESIDENTE", "VICE-PRESIDENTE"), uf := "BR"]
# um statement pode vir de dois esqueletos (prefeito_generico e prefeito_generico_ent_br) ou de
# duas entidades (posicao generica cujo P1001 e a classe 'unidade federativa'); fica uma linha por
# statement, a que traz UF ou codigo IBGE (verificacao de 28/08/2026: 109 statements duplicados)
# posicoes 'vice-governador de X' sao subclasse de 'governador' (Q132050) e entram tambem no
# esqueleto de governador; o rotulo da posicao decide o cargo
w[!is.na(pos_rotulo) & grepl("^VICE", norm(pos_rotulo)) & cargo_wd == "GOVERNADOR", cargo_wd := "VICE-GOVERNADOR"]
w <- w[order(s, is.na(uf), is.na(ibge))][!duplicated(s)]

## ---------------------------------------------------------------- statements plurianuais
# um statement que cobre mandatos consecutivos (reeleicao; inicio em 1/jan e fim >= 5 anos depois)
# vira uma linha por mandato de quatro anos (executivos) ; a causa do fim fica so na ultima parte
w[, s_orig := s]
longos <- w[cargo_wd %in% c("PRESIDENTE", "VICE-PRESIDENTE", "GOVERNADOR", "VICE-GOVERNADOR", "PREFEITO") &
            !is.na(inicio) & !is.na(fim) & substr(inicio, 6, 10) == "01-01" &
            as.integer(substr(fim, 1, 4)) - as.integer(substr(inicio, 1, 4)) >= 5L]
if (nrow(longos)) {
  partes <- rbindlist(lapply(seq_len(nrow(longos)), function(i) {
    a0 <- as.integer(substr(longos$inicio[i], 1, 4)); a1 <- as.integer(substr(longos$fim[i], 1, 4))
    ini_anos <- seq(a0, a1 - 1L, by = 4L); ini_anos <- ini_anos[ini_anos < a1]
    rbindlist(lapply(seq_along(ini_anos), function(k) {
      r <- copy(longos[i]); r[, `:=`(inicio = sprintf("%d-01-01", ini_anos[k]), s = paste0(s_orig, "#", k))]
      ult <- k == length(ini_anos)
      r[, fim := if (ult) longos$fim[i] else sprintf("%d-12-31", ini_anos[k] + 3L)]
      if (!ult) r[, causa_fim := NA_character_]
      r
    }))
  }))
  w <- rbindlist(list(w[!s %in% longos$s], partes), use.names = TRUE, fill = TRUE)
}

## ---------------------------------------------------------------- pessoas do BOCEL
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
mand <- mand[cargo %in% c("PRESIDENTE", "VICE-PRESIDENTE", "GOVERNADOR", "VICE-GOVERNADOR", "DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL", "PREFEITO")]
tse_ibge <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
pess[, nome_norm := norm(nome)]
w[, nome_norm := norm(fcoalesce(nome_completo, rotulo))]
w[, rotulo_norm := norm(rotulo)]

# (1) nome completo + nascimento; (2) rotulo + nascimento; (3) nome + UF + ano da posicao (sem nascimento)
p1 <- pess[!is.na(dt_nascimento), .(id_pessoa, nome_norm, dt_nasc = dt_nascimento)]
m1 <- merge(w[!is.na(dt_nasc)], p1, by = c("nome_norm", "dt_nasc"), all.x = FALSE)[, tipo_pareamento := "nome_completo+nascimento"]
rest <- w[!s %in% m1$s]
m2 <- merge(rest[!is.na(dt_nasc)], p1, by.x = c("rotulo_norm", "dt_nasc"), by.y = c("nome_norm", "dt_nasc"))[, tipo_pareamento := "rotulo+nascimento"]
rest <- rest[!s %in% m2$s]
# (2b): mesma data de nascimento e todos os tokens do rotulo (>= 3 letras) contidos no nome civil
# (o Wikidata usa o nome publico, ex. 'Lula'; o TSE, o nome civil completo), pessoa unica
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
r2b <- rest[!is.na(dt_nasc) & !is.na(rotulo_norm)]
c2b <- merge(r2b[, .(s, dt_nasc, rotulo_norm)], p1, by = "dt_nasc", allow.cartesian = TRUE)
if (nrow(c2b)) {
  tr <- tok(c2b$rotulo_norm); tn <- tok(c2b$nome_norm)
  c2b[, contido := mapply(function(a, b) length(a) > 0 && all(a %in% b), tr, tn)]
  c2b <- c2b[contido == TRUE][, if (uniqueN(id_pessoa) == 1) .SD[1], by = s]
  m2b <- merge(rest, c2b[, .(s, id_pessoa)], by = "s")[, tipo_pareamento := "tokens_rotulo+nascimento"]
} else m2b <- rest[0][, `:=`(id_pessoa = character(), tipo_pareamento = character())]
rest <- rest[!s %in% m2b$s]
# (2c): mesma data de nascimento, ao menos um token do rotulo (>= 3 letras) contido no nome civil,
# e pessoa unica com mandato do mesmo cargo na mesma UF (rotulo com apelido: 'Binho Marques',
# 'Tiao Viana', 'Ratinho Junior'; verificacao de 28/08/2026)
r2c <- rest[!is.na(dt_nasc) & !is.na(rotulo_norm) & !is.na(uf)]
cand2c <- unique(merge(mand[, .(id_pessoa, cargo, sg_uf)], p1, by = "id_pessoa"))
c2c <- merge(r2c[, .(s, dt_nasc, rotulo_norm, cargo_wd, uf)], cand2c,
             by.x = c("dt_nasc", "cargo_wd", "uf"), by.y = c("dt_nasc", "cargo", "sg_uf"), allow.cartesian = TRUE)
if (nrow(c2c)) {
  tr <- tok(c2c$rotulo_norm); tn <- tok(c2c$nome_norm)
  c2c[, contido := mapply(function(a, b) length(a) > 0 && any(a %in% b), tr, tn)]
  c2c <- c2c[contido == TRUE][, if (uniqueN(id_pessoa) == 1) .SD[1], by = s]
  m2c <- merge(rest, c2c[, .(s, id_pessoa)], by = "s")[, tipo_pareamento := "token_rotulo+nascimento+uf+cargo"]
} else m2c <- rest[0][, `:=`(id_pessoa = character(), tipo_pareamento = character())]
rest <- rest[!s %in% m2c$s]
# (3): nome unico entre as pessoas com mandato do mesmo cargo na mesma UF, com inicio dentro da janela
cand3 <- merge(mand[, .(id_pessoa, cargo, sg_uf, ano_eleicao = as.integer(ano_eleicao), unidade_posicao)],
               pess[, .(id_pessoa, nome_norm)], by = "id_pessoa")
r3 <- rest[!is.na(uf) & !is.na(inicio)]
r3[, ano_ini := as.integer(substr(inicio, 1, 4))]
m3 <- merge(r3, cand3, by.x = c("nome_norm", "cargo_wd", "uf"), by.y = c("nome_norm", "cargo", "sg_uf"), allow.cartesian = TRUE)
m3 <- m3[ano_ini >= ano_eleicao & ano_ini <= ano_eleicao + 4L]
m3 <- m3[, if (uniqueN(id_pessoa) == 1) .SD[1], by = s][, tipo_pareamento := "nome+uf+cargo+ano"]
m3[, c("ano_eleicao", "unidade_posicao", "ano_ini") := NULL]
rest <- rest[!s %in% m3$s]
# (3b): sem nascimento, tokens do rotulo contidos no nome civil de pessoa unica com mandato do
# mesmo cargo na mesma UF (ou municipio, quando a entidade traz IBGE) e inicio dentro da janela
r3b <- rest[!is.na(inicio) & !is.na(rotulo_norm) & (!is.na(uf) | !is.na(ibge))]
r3b[, ano_ini := as.integer(substr(inicio, 1, 4))]
cand3b <- merge(mand[, .(id_pessoa, cargo, sg_uf, sg_ue, ano_eleicao = as.integer(ano_eleicao))],
                pess[, .(id_pessoa, nome_norm)], by = "id_pessoa")
cand3b <- merge(cand3b, tse_ibge[, .(sg_ue, ibge6 = substr(id_municipio_ibge, 1, 6))], by = "sg_ue", all.x = TRUE)
c3b <- merge(r3b[, .(s, rotulo_norm, cargo_wd, uf, ibge6 = substr(ibge, 1, 6), ano_ini)],
             cand3b, by.x = "cargo_wd", by.y = "cargo", allow.cartesian = TRUE)
c3b <- c3b[ano_ini >= ano_eleicao & ano_ini <= ano_eleicao + 4L]
c3b <- c3b[(is.na(uf) | uf == sg_uf) & (is.na(ibge6.x) | is.na(ibge6.y) | ibge6.x == ibge6.y)]
if (nrow(c3b)) {
  tr <- tok(c3b$rotulo_norm); tn <- tok(c3b$nome_norm)
  c3b[, contido := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tr, tn)]
  c3b <- c3b[contido == TRUE][, if (uniqueN(id_pessoa) == 1) .SD[1], by = s]
  m3b <- merge(rest, c3b[, .(s, id_pessoa)], by = "s")[, tipo_pareamento := "tokens_rotulo+uf+cargo+ano"]
} else m3b <- rest[0][, `:=`(id_pessoa = character(), tipo_pareamento = character())]
pareado <- rbindlist(list(m1, m2, m2b, m2c, m3, m3b), use.names = TRUE, fill = TRUE)
pareado <- pareado[!duplicated(s)]
w <- merge(w, pareado[, .(s, id_pessoa, tipo_pareamento)], by = "s", all.x = TRUE)

## ---------------------------------------------------------------- mandato do BOCEL
# mandato da mesma pessoa, mesmo cargo, cuja janela convencional contem o inicio (ou o fim) do
# statement; para prefeito, a unidade tem de bater quando a entidade traz o codigo IBGE
mand[, `:=`(ini_c = as.IDate(mandato_inicio), fim_c = as.IDate(mandato_fim))]
mand <- merge(mand, tse_ibge[, .(sg_ue, id_municipio_ibge)], by = "sg_ue", all.x = TRUE)
w[, id_mandato_bocel := NA_character_]
cand_m <- merge(w[!is.na(id_pessoa), .(s, id_pessoa, cargo_wd, ibge, uf, inicio, fim)],
                mand[, .(id_pessoa, id_mandato, cargo, sg_uf, id_municipio_ibge, ini_c, fim_c, ano_eleicao)],
                by = "id_pessoa", allow.cartesian = TRUE)
cand_m <- cand_m[cargo == cargo_wd | (cargo_wd == "DEPUTADO ESTADUAL" & cargo == "DEPUTADO DISTRITAL")]
# o mandato do BOCEL e o que contem o FIM do statement (a saida pertence a ele); sem fim, o inicio.
# O Wikidata registra com frequencia como fim a data da posse do sucessor (2003-01-01 para o
# mandato 1999-2002; 1999-01-31 para a legislatura 1995-1998), por isso a referencia e a vespera
# do fim, e a vespera tem de cair DENTRO da janela convencional: com a tolerancia anterior de 60
# dias antes do inicio, statements da legislatura anterior (fim 1999-01-31) eram atribuidos ao
# mandato de 1998 (verificacao de 28/08/2026: 128 statements comecavam mais de 60 dias antes do
# mandato atribuido). Sem fim, o inicio pode anteceder em ate 60 dias a janela (posse antecipada).
cand_m[, ref := fifelse(!is.na(fim), as.IDate(fim) - 1L, as.IDate(inicio))]
cand_m[, ini_d := as.IDate(inicio)]
# cauda da legislatura anterior: statement que comecou antes deste mandato e terminou ate 60 dias
# depois do inicio dele (ex.: 2007-02-01..2011-02-28 para o mandato 2011-2015) nao e deste mandato
cand_m[, cauda_anterior := !is.na(ini_d) & ini_d < ini_c - 60L & ref <= ini_c + 60L]
# statement que comecou com este mandato e terminou ate 60 dias depois do fim convencional e deste mandato
cand_m[, comeca_junto := !is.na(ini_d) & ini_d >= ini_c - 60L & ini_d <= ini_c + 60L]
cand_m <- cand_m[!is.na(ref) & (
  (is.na(fim) & ref >= ini_c - 60L & ref <= fim_c) |
  (!is.na(fim) & ref >= ini_c & ref <= fim_c & !cauda_anterior) |
  (!is.na(fim) & comeca_junto & ref > fim_c & ref <= fim_c + 60L))]
cand_m <- cand_m[is.na(ibge) | is.na(id_municipio_ibge) | substr(ibge, 1, 6) == substr(id_municipio_ibge, 1, 6)]
cand_m <- cand_m[is.na(uf) | uf == sg_uf]
setorder(cand_m, s, ano_eleicao)
cand_m <- cand_m[, .SD[1], by = s]
w[cand_m, id_mandato_bocel := i.id_mandato, on = "s"]

## ---------------------------------------------------------------- forma de saida
w[, causa_norm := norm(causa_fim)]
w[, forma_saida := fcase(
  is.na(causa_fim) & !is.na(fim) & !is.na(id_mandato_bocel) &
    as.IDate(fim) >= as.IDate(mand$mandato_fim[match(id_mandato_bocel, mand$id_mandato)]) - 45L, "fim_regular",
  grepl("RENUNCIA", causa_norm), "renuncia",
  grepl("MORTE|FALECIMENTO|OBITO|DEATH", causa_norm), "falecimento",
  grepl("IMPEACHMENT|CASSAC|IMPEDIMENTO|PERDA", causa_norm), "cassacao",
  grepl("FIM DE MANDATO|TERMINO|TERM ", causa_norm), "fim_regular",
  grepl("AFAST|LICEN", causa_norm), "afastamento",
  !is.na(causa_fim), "outro",
  default = NA_character_)]
# obito dentro da janela do mandato pareado
ob <- w[!is.na(id_pessoa) & !is.na(dt_morte), .(id_pessoa, dt_morte = dt_morte[1]), by = h][, .(id_pessoa, dt_morte)]
ob <- unique(ob)
ob <- merge(ob, fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[, .(id_pessoa, id_mandato, mandato_inicio, mandato_fim, cargo)],
            by = "id_pessoa", allow.cartesian = TRUE)
ob[, dentro_do_mandato := dt_morte >= mandato_inicio & dt_morte <= mandato_fim]
obitos <- ob[dentro_do_mandato == TRUE, .(id_pessoa, id_mandato, cargo, data_morte = dt_morte, dentro_do_mandato, fonte = "wikidata")]
# a morte dentro da janela convencional nao e morte no exercicio quando o proprio Wikidata registra
# que a pessoa deixou o cargo antes (Eduardo Campos renunciou em 2014-04-04 e morreu em 2014-08-13;
# verificacao de 28/08/2026)
saiu_antes <- w[!is.na(id_mandato_bocel) & !is.na(fim), .(id_mandato = id_mandato_bocel, fim_wd = as.IDate(fim))]
saiu_antes <- saiu_antes[, .(fim_wd = max(fim_wd)), by = id_mandato]
obitos <- merge(obitos, saiu_antes, by = "id_mandato", all.x = TRUE)
n_saiu_antes <- obitos[!is.na(fim_wd) & fim_wd < as.IDate(data_morte) - 1L, .N]
obitos <- obitos[is.na(fim_wd) | fim_wd >= as.IDate(data_morte) - 1L][, fim_wd := NULL]
setcolorder(obitos, c("id_pessoa", "id_mandato"))
w[obitos, `:=`(forma_saida = fifelse(is.na(forma_saida) | forma_saida %in% c("outro", "fim_regular"), "falecimento", forma_saida),
               fim = fifelse(is.na(fim), i.data_morte, fim)), on = c(id_mandato_bocel = "id_mandato")]

out <- w[, .(qid = h, statement = qid(s), nome_wikidata = rotulo, nome_completo, dt_nascimento = dt_nasc, dt_morte,
             cargo = cargo_wd, posicao_wd = pos_rotulo, entidade = ent_rotulo, ibge_entidade = ibge, uf,
             inicio, fim, causa_fim_original = causa_fim, forma_saida, partido_wd, eleicao_wd,
             substitui_qid = substitui, substituido_por_qid = substituido_por,
             id_pessoa_bocel = id_pessoa, id_mandato_bocel, tipo_pareamento,
             url = paste0("https://www.wikidata.org/wiki/", h))]
setorder(out, cargo, uf, inicio, na.last = TRUE)
fwrite(out, "data/wikidata_mandatos.csv", na = "NA", quote = TRUE)
fwrite(obitos, "data/wikidata_obitos.csv", na = "NA", quote = TRUE)

## ---------------------------------------------------------------- registro
script <- "R/12_wikidata_mandatos.R"
# mandatos distintos: varios statements podem apontar para o mesmo mandato (verificacao de 28/08/2026)
cob <- out[, .(n_statements = .N, n_pareados_pessoa = sum(!is.na(id_pessoa_bocel)),
               n_pareados_mandato = uniqueN(id_mandato_bocel[!is.na(id_mandato_bocel)])), by = cargo]
tot <- mand[, .(n_bocel = .N), by = cargo]
cob <- merge(cob, tot, by = "cargo", all.x = TRUE)[, cobertura_mandatos := round(n_pareados_mandato / n_bocel, 4)]
print(cob)
fwrite(cob, "output/verificacao/wikidata_cobertura_por_cargo.csv")
for (i in seq_len(nrow(cob))) {
  registrar_numero(sprintf("wd_%s_statements", gsub(" |-", "_", tolower(cob$cargo[i]))), cob$n_statements[i], script = script)
  registrar_numero(sprintf("wd_%s_mandatos_pareados", gsub(" |-", "_", tolower(cob$cargo[i]))), cob$n_pareados_mandato[i], script = script)
}
registrar_numero("wd_n_obitos_em_mandato", nrow(obitos), script = script)
registrar_numero("wd_n_obitos_excluidos_saida_anterior", n_saiu_antes, script = script)
registrar_numero("wd_n_forma_saida_observada", out[!is.na(forma_saida) & !is.na(id_mandato_bocel), .N], script = script)
cat("12_wikidata_mandatos: concluido —", nrow(out), "statements,", nrow(obitos), "obitos em mandato\n")
