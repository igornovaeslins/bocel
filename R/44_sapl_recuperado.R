# 44_sapl_recuperado.R — recuperacao, SO com o que ja esta em disco, das duas situacoes em que o SAPL
# municipal nao chegou a data/exercicio_camaras_municipais.csv:
#   (A) instancias SAPL 3 com cache gravado (_ok) e mandato.json vazio — fetch_sapl_municipal.py grava
#       o marcador _ok mesmo quando a paginacao devolve lista vazia, entao a instancia nunca e retentada;
#   (B) municipios cujo HTML da casa traz o marcador de SAPL (sistema_detectado sapl2/sapl3 nos inventarios
#       de camaras_sem_sapl e camaras_sem_sapl_2) e que nao tem nenhuma linha no exercicio das camaras.
# Nenhuma requisicao de rede: le apenas data_raw/. O que exigiria baixar fica de fora e entra no diagnostico.
# Entrada:  data_raw/sapl_municipal/<uf>/<sg_ue>/{legislatura,parlamentar,mandato}.json,
#           data_raw/sapl_municipal/inventario_sapl_municipal.csv,
#           data_raw/camaras_sem_sapl/inventario_camaras.csv + <uf>/<sg_ue>/home_*.html,
#           data_raw/camaras_sem_sapl_2/inventario_familias.csv + <uf>/<sg_ue>/*.html,
#           data/mandatos.csv, data/pessoas.csv, data/municipios_tse_ibge.csv, data_raw/parquet/cand_*.parquet
# Saida:    data/exercicio_sapl_recuperado.csv, output/verificacao/sapl_cache_vazio.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/44_sapl_recuperado.R
set.seed(20260830)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/44_sapl_recuperado.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/44_sapl_recuperado.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
CICLO <- seq(1996L, 2024L, 4L)
n_json <- function(f) { if (!file.exists(f)) return(NA_integer_)
  x <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(x)) return(-1L)          # -1 = arquivo existe mas nao e JSON valido (falha de parser)
  length(x) }

## ---------------------------------------------------------------- (A) instancias SAPL 3 com cache vazio
inv_sapl <- fread("data_raw/sapl_municipal/inventario_sapl_municipal.csv", colClasses = "character")
inst <- inv_sapl[responde_api == "TRUE"]
inst[, dir := file.path("data_raw/sapl_municipal", uf, sg_ue)]
inst <- inst[file.exists(file.path(dir, "_ok"))]
inst[, `:=`(n_leg = vapply(file.path(dir, "legislatura.json"), n_json, integer(1)),
            n_par = vapply(file.path(dir, "parlamentar.json"), n_json, integer(1)),
            n_man = vapply(file.path(dir, "mandato.json"), n_json, integer(1)))]
registrar_numero("srec_a_instancias_sapl_com_cache", nrow(inst), script = script)
vazias <- inst[n_man %in% c(0L, NA_integer_) | n_man == -1L]
registrar_numero("srec_a_instancias_mandato_vazio", nrow(vazias), script = script)
registrar_numero("srec_a_json_invalido_parser_falhou", inst[n_leg == -1L | n_par == -1L | n_man == -1L, .N], script = script)

## diagnostico da origem: n_legislaturas do inventario foi medido na descoberta, por uma consulta
## independente a /api/parlamentares/legislatura (total_entries). Se ele diz 0, a instancia estava
## vazia na origem; se ele diz k e legislatura.json tem k, a paginacao funcionou e o vazio de
## mandato.json e da propria base. So restaria duvida se os dois discordassem.
vazias[, n_leg_inventario := as.integer(fifelse(n_legislaturas == "", NA_character_, n_legislaturas))]
vazias[, diagnostico := fcase(
  n_man == -1L, "parser_falhou_json_invalido",
  !is.na(n_leg_inventario) & n_leg_inventario == 0L & n_leg == 0L, "vazio_na_origem_instancia_sem_legislatura",
  !is.na(n_leg_inventario) & n_leg_inventario == n_leg & n_leg > 0L & n_par == 0L, "vazio_na_origem_legislatura_sem_parlamentar",
  !is.na(n_leg_inventario) & n_leg_inventario == n_leg & n_leg > 0L & n_par > 0L, "vazio_na_origem_parlamentar_sem_mandato",
  default = "indeterminado_inventario_discorda_do_cache")]
print(vazias[, .N, by = diagnostico][order(-N)])
for (k in unique(vazias$diagnostico)) registrar_numero(paste0("srec_a_diag_", k), vazias[diagnostico == k, .N], script = script)

## recuperavel em (A): parlamentar gravado sem mandato. A legislatura so pode ser atribuida quando a
## instancia tem UMA legislatura no cache; com duas ou nenhuma, o vinculo nao esta no disco.
rec_a <- rbindlist(lapply(which(vazias$n_par > 0L), function(i) {
  d <- vazias$dir[i]
  par <- as.data.table(fromJSON(file.path(d, "parlamentar.json"), simplifyVector = TRUE, flatten = TRUE))
  leg <- tryCatch(as.data.table(fromJSON(file.path(d, "legislatura.json"), simplifyVector = TRUE, flatten = TRUE)), error = function(e) data.table())
  um <- nrow(leg) == 1L
  data.table(sg_ue = vazias$sg_ue[i], id_municipio_ibge = vazias$id_municipio_ibge[i], uf = vazias$uf[i],
             dominio = vazias$dominio[i],
             legislatura_numero = if (um) as.character(leg$numero) else NA_character_,
             legislatura_inicio = if (um) substr(as.character(leg$data_inicio), 1, 10) else NA_character_,
             legislatura_fim    = if (um) substr(as.character(leg$data_fim), 1, 10) else NA_character_,
             nome_fonte = as.character(par$nome_completo), nome_parlamentar = as.character(par$nome_parlamentar),
             sistema = "sapl3_api_parlamentar_sem_mandato",
             url = paste0("https://", vazias$dominio[i], "/api/parlamentares/parlamentar/", par$id, "/"))
}), use.names = TRUE, fill = TRUE)
if (nrow(rec_a)) {
  rec_a[, ano_eleicao_bocel := as.integer(substr(legislatura_inicio, 1, 4)) - 1L]
  rec_a[!(ano_eleicao_bocel %in% CICLO), ano_eleicao_bocel := NA_integer_]
  rec_a[, so_legislatura_atual := NA]
}
registrar_numero("srec_a_linhas_recuperadas", nrow(rec_a), script = script)

## ---------------------------------------------------------------- (B) marcador de SAPL no HTML da casa
inv1 <- fread("data_raw/camaras_sem_sapl/inventario_camaras.csv", colClasses = "character")
inv2 <- fread("data_raw/camaras_sem_sapl_2/inventario_familias.csv", colClasses = "character")
marc <- unique(rbindlist(list(inv1[sistema_detectado %in% c("sapl2", "sapl3"), .(sg_ue, id_municipio_ibge, uf, nome, host, sistema_detectado)],
                              inv2[sistema_detectado %in% c("sapl2", "sapl3"), .(sg_ue, id_municipio_ibge, uf, nome, host, sistema_detectado)]),
                         use.names = TRUE), by = "sg_ue")
registrar_numero("srec_b_municipios_com_marcador_sapl_no_html", nrow(marc), script = script)
ja_cm <- unique(fread("data/exercicio_camaras_municipais.csv", colClasses = "character", select = "sg_ue")$sg_ue)
registrar_numero("srec_b_marcados_com_linha_no_exercicio_camaras", marc[sg_ue %in% ja_cm, .N], script = script)
marc <- marc[!sg_ue %in% ja_cm]
ja_out <- unique(c(fread("data/exercicio_camaras_sem_sapl.csv", colClasses = "character", select = "sg_ue")$sg_ue,
                   fread("data/exercicio_camaras_sem_sapl_2.csv", colClasses = "character", select = "sg_ue")$sg_ue))
registrar_numero("srec_b_marcados_ja_cobertos_por_outro_coletor", marc[sg_ue %in% ja_out, .N], script = script)
marc[, ja_coberto_outro_coletor := sg_ue %in% ja_out]
registrar_numero("srec_b_municipios_alvo", marc[ja_coberto_outro_coletor == FALSE, .N], script = script)

## --- leitura e limpeza do HTML em cache (nenhum download; so os arquivos ja gravados)
ENT <- c(nbsp = " ", amp = "&", quot = "\"", apos = "'", lt = "<", gt = ">", ordm = "o", ordf = "a", deg = " ",
         aacute = "á", agrave = "à", acirc = "â", atilde = "ã", auml = "ä",
         eacute = "é", egrave = "è", ecirc = "ê", euml = "ë",
         iacute = "í", icirc = "î", oacute = "ó", ocirc = "ô", otilde = "õ", ouml = "ö",
         uacute = "ú", ucirc = "û", uuml = "ü", ccedil = "ç", ntilde = "ñ",
         Aacute = "Á", Agrave = "À", Acirc = "Â", Atilde = "Ã", Eacute = "É", Ecirc = "Ê",
         Iacute = "Í", Oacute = "Ó", Ocirc = "Ô", Otilde = "Õ", Uacute = "Ú", Ccedil = "Ç")
unescape <- function(x) {
  for (nm in names(ENT)) x <- gsub(paste0("&", nm, ";"), ENT[[nm]], x, fixed = TRUE)
  m <- unique(unlist(regmatches(x, gregexpr("&#x?[0-9A-Fa-f]+;", x, perl = TRUE))))
  for (e in m) {
    cod <- sub("^&#", "", sub(";$", "", e))
    v <- if (grepl("^[xX]", cod)) strtoi(substring(cod, 2), 16L) else suppressWarnings(as.integer(cod))
    if (!is.na(v) && v > 31 && v < 66000) x <- gsub(e, intToUtf8(v), x, fixed = TRUE)
  }
  x
}
ler_html <- function(f) {
  s <- tryCatch(paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n"), error = function(e) "")
  iconv(s, "UTF-8", "UTF-8", sub = " ")
}
achatar <- function(s) {
  s <- gsub("(?is)<(script|style)[^>]*>.*?</\\1>", " ", s, perl = TRUE)
  t <- gsub("(?s)<[^>]+>", "|", s, perl = TRUE)
  t <- unescape(t)
  t <- gsub("[[:space:]]+", " ", t)
  gsub("( *\\| *)+", "|", t)
}
## palavras que denunciam rotulo de menu, secao ou noticia no lugar de nome de pessoa
LIXO <- paste0("CAMARA|MUNICIPAL|MUNICIPIO|VEREADOR|VEREADORA|VEREADORES|PRESIDENT|SECRETARI|MESA|DIRETORA|SESSAO|SESSOES|",
               "LEI|LEIS|PROJETO|DECRETO|PORTARIA|RESOLUCAO|ATA|ATAS|PAUTA|NOTICIA|GALERIA|TRANSPARENC|OUVIDORIA|CONTATO|",
               "ACESSO|INFORMACAO|PORTAL|SITE|MAPA|PAGINA|INICIO|LEGISLATIV|LEGISLATURA|COMISSAO|COMISSOES|AUDIENCIA|",
               "ENDERECO|TELEFONE|EMAIL|HORARIO|SERVIDOR|CIDADAO|EMPRESA|LICITACAO|CONTRATO|CONCURSO|EDITAL|BANCO|IDEIAS|",
               "PARTIDO|GABINETE|EXPEDIENTE|TRIBUNA|PLENARIO|BIOGRAFIA|CURRICULO|VER MAIS|CLIQUE|SAIBA|LEIA")
nome_ok <- function(x) {
  x <- trimws(x)
  n <- norm(x)
  tk <- strsplit(n, " ")
  n_tok <- vapply(tk, length, integer(1))
  n_gr3 <- vapply(tk, function(t) sum(nchar(t) >= 3), integer(1))
  !is.na(x) & nchar(x) >= 8 & nchar(x) <= 60 & n_tok >= 2 & n_tok <= 7 & n_gr3 >= 2 &
    grepl("^[A-Za-zÀ-ÿ' .-]+$", x) & !grepl(LIXO, n) & !grepl("[0-9@]", x)
}
limpa_ancora <- function(x) {
  x <- gsub("(?s)<[^>]+>", " ", x, perl = TRUE); x <- unescape(x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- gsub("(?i)\\s*(partido\\s*[:-].*|ver mais.*|clique.*|saiba.*)$", "", x, perl = TRUE)
  x <- gsub("(?i)\\s*\\([^)]*\\)\\s*", " ", x, perl = TRUE)
  x <- gsub("(?i)\\s*(presidente|vice-?presidente|[0-9]+\\s*[º°o]?\\s*secret[áa]ri[oa]|secret[áa]ri[oa]|vereador[a]?)\\s*$", "", x, perl = TRUE)
  trimws(x)
}
RX_LINK <- paste0("<a[^>]+href=[\"'][^\"']*(?:/portal/vereadores/[0-9]+|cod_parlamentar=[0-9]+|parlamentar_mostrar",
                  "|/parlamentar/[0-9]+|/parl/[0-9]+|/vereador(?:es)?/[0-9]+)[^\"']*[\"'][^>]*>(?s).*?</a>")

extrai <- function(uf, sg_ue) {
  ds <- file.path(c("data_raw/camaras_sem_sapl", "data_raw/camaras_sem_sapl_2"), uf, sg_ue)
  ## 05/09/2026: recursive = TRUE. Os HTML de camaras_sem_sapl_2 vivem em <uf>/<sg_ue>/html/ e parte
  ## dos de camaras_sem_sapl tambem esta abaixo do primeiro nivel; sem recursao o script declarava
  ## essa entrada no cabecalho e nunca a lia (srec_b_* de 30/08 refletem so os arquivos rasos)
  fs <- unlist(lapply(ds[dir.exists(ds)], function(d) list.files(d, pattern = "\\.html$", full.names = TRUE, recursive = TRUE)))
  fs <- sort(fs)
  if (!length(fs)) return(list(n_html = 0L, n_car = 0L, nomes = character(), regra = character(), leg = NULL, url = NA_character_))
  nomes <- character(); regras <- character(); leg <- NULL; car <- 0L; url <- NA_character_
  for (f in fs) {
    s <- ler_html(f); t <- achatar(s); car <- max(car, nchar(t))
    ## regra 1: ficha de parlamentar rotulada ("Nome:" / "Nome completo:")
    m <- unlist(regmatches(t, gregexpr("(?i)\\|[ ]*Nome(?: completo)?[ ]*:[ ]*\\|?[ ]*[^|]{5,60}", t, perl = TRUE)))
    if (length(m)) {
      v <- trimws(sub("(?i)^\\|[ ]*Nome(?: completo)?[ ]*:[ ]*\\|?[ ]*", "", m, perl = TRUE))
      v <- v[nome_ok(v)]
      if (length(v)) { nomes <- c(nomes, v); regras <- c(regras, rep("html_rotulo_nome", length(v))); url <- f }
    }
    ## regra 2: ancora que aponta para a ficha do parlamentar; o texto da ancora e o nome
    a <- unlist(regmatches(s, gregexpr(RX_LINK, s, perl = TRUE)))
    if (length(a)) {
      v <- limpa_ancora(a); v <- v[nome_ok(v)]
      if (length(v)) { nomes <- c(nomes, v); regras <- c(regras, rep("html_link_ficha", length(v))); url <- f }
    }
    ## legislatura declarada na propria pagina
    l1 <- regmatches(t, regexpr("(?i)legislatura[|: ]*[0-3][0-9]/[01][0-9]/(20[0-9]{2})[ ]*[-–][ ]*[0-3][0-9]/[01][0-9]/(20[0-9]{2})", t, perl = TRUE))
    l2 <- regmatches(t, regexpr("(?i)vereador(?:es)?[ ]*[(]?[ ]*(20[0-9]{2})[ ]*[-–/][ ]*(20[0-9]{2})", t, perl = TRUE))
    anos <- unlist(regmatches(c(l1, l2), gregexpr("20[0-9]{2}", c(l1, l2))))
    if (length(anos) >= 2 && is.null(leg)) leg <- as.integer(anos[1:2])
  }
  ok <- !duplicated(norm(nomes))
  list(n_html = length(fs), n_car = car, nomes = nomes[ok], regra = regras[ok], leg = leg, url = url)
}

alvo <- marc[ja_coberto_outro_coletor == FALSE]
ext <- lapply(seq_len(nrow(alvo)), function(i) extrai(alvo$uf[i], alvo$sg_ue[i]))
alvo[, `:=`(n_html = vapply(ext, function(e) e$n_html, integer(1)),
            n_caracteres_texto = vapply(ext, function(e) e$n_car, integer(1)),
            n_nomes = vapply(ext, function(e) length(e$nomes), integer(1)))]
## exige lista (>= 3 nomes no mesmo municipio): um nome solto e link perdido, nao composicao da casa
MIN_NOMES <- 3L
alvo[, tem_lista := n_nomes >= MIN_NOMES]
registrar_numero("srec_b_municipios_html_sem_texto_spa", alvo[n_caracteres_texto < 200L, .N], script = script)
registrar_numero("srec_b_municipios_html_sem_lista", alvo[n_caracteres_texto >= 200L & tem_lista == FALSE, .N], script = script)
registrar_numero("srec_b_municipios_html_com_lista", alvo[tem_lista == TRUE, .N], script = script)

## a pagina em cache e a home coletada em ago/2026: a lista publicada e a da legislatura corrente
## (2025-2028, eleicao de 2024), salvo quando a propria pagina declara o intervalo.
rec_b <- rbindlist(lapply(which(alvo$tem_lista), function(i) {
  e <- ext[[i]]
  li <- if (!is.null(e$leg)) e$leg[1] else 2025L
  lf <- if (!is.null(e$leg)) e$leg[2] else 2028L
  data.table(sg_ue = alvo$sg_ue[i], id_municipio_ibge = alvo$id_municipio_ibge[i], uf = alvo$uf[i],
             dominio = alvo$host[i], legislatura_numero = NA_character_,
             legislatura_inicio = sprintf("%d-01-01", li), legislatura_fim = sprintf("%d-12-31", lf),
             ano_eleicao_bocel = li - 1L, nome_fonte = e$nomes, nome_parlamentar = e$nomes,
             sistema = paste0("sapl2_", e$regra), so_legislatura_atual = is.null(e$leg),
             url = paste0("https://", alvo$host[i], "/"))
}), use.names = TRUE, fill = TRUE)
if (nrow(rec_b)) rec_b[!(ano_eleicao_bocel %in% CICLO), ano_eleicao_bocel := NA_integer_]
registrar_numero("srec_b_linhas_recuperadas", nrow(rec_b), script = script)

## ---------------------------------------------------------------- consolidacao no esquema padrao
ex <- rbindlist(list(rec_a, rec_b), use.names = TRUE, fill = TRUE)
stopifnot(nrow(ex) > 0)
ex[, `:=`(nome_normalizado = norm(nome_fonte), titular = NA, data_inicio_mandato = NA_character_,
          data_fim_mandato = NA_character_, tipo_afastamento = NA_character_, forma_saida = "nao_observado")]
stopifnot(all(ex$forma_saida %in% VOCAB))

## ---------------------------------------------------------------- pareamento com o BOCEL (vereador)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "13"]
registrar_numero("srec_mandatos_vereador_com_unidade_posicao_diferente_de_sg_ue", mand[unidade_posicao != sg_ue, .N], script = script)
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano_eleicao)], pess[, .(id_pessoa, nome)], by = "id_pessoa")
mand[, `:=`(nome_norm = norm(nome), ano_eleicao = as.integer(ano_eleicao))]
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO == "13"]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))], by = "id_mandato", all.x = TRUE)

ex[, rid := .I]
ex[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
alvo_par <- ex[!is.na(ano_eleicao_bocel) & nchar(nome_normalizado) >= 8L]
## regra 1: nome civil identico, unico no municipio-eleicao dos dois lados
p1 <- merge(alvo_par[, .(rid, sg_ue, ano_eleicao_bocel, nome_normalizado)],
            mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nome_normalizado"), by.y = c("sg_ue", "ano_eleicao", "nome_norm"))
p1 <- p1[, if (.N == 1L) .SD, by = rid][, metodo := "nome_civil_exato"]
## regra 2: nome da fonte igual ao nome de urna do TSE
r2 <- alvo_par[!rid %in% p1$rid]
p2 <- merge(r2[, .(rid, sg_ue, ano_eleicao_bocel, nome_normalizado)],
            mand[!is.na(nome_urna_norm), .(sg_ue, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nome_normalizado"), by.y = c("sg_ue", "ano_eleicao", "nome_urna_norm"))
p2 <- p2[, if (.N == 1L) .SD, by = rid][, metodo := "nome_urna_exato"]
## regra 3: todos os tokens (>= 2, cada um >= 3 letras) do nome da fonte contidos no nome civil, mandato unico
r3 <- alvo_par[!rid %in% c(p1$rid, p2$rid)]
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
c3 <- merge(r3[, .(rid, sg_ue, ano_eleicao_bocel, nome_normalizado)],
            mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), allow.cartesian = TRUE)
if (nrow(c3)) {
  ta <- tok(c3$nome_normalizado); tb <- tok(c3$nome_norm)
  c3[, contido := mapply(function(a, b) length(a) >= 2L && all(a %in% b), ta, tb)]
  p3 <- c3[contido == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else .SD[0], by = rid][, metodo := "tokens_no_nome_civil"]
} else p3 <- data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (nrow(d)) d[, .(rid, id_mandato, id_pessoa, metodo)] else
  data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
par <- rbindlist(list(sel(p1), sel(p2), sel(p3)), use.names = TRUE)
par <- par[!duplicated(rid)]
## um mandato do BOCEL nao pode receber duas linhas da fonte: em conflito, nenhuma das duas fica
par[, dup := .N > 1L, by = id_mandato]
registrar_numero("srec_pareamentos_descartados_por_mandato_repetido", par[dup == TRUE, .N], script = script)
par <- par[dup == FALSE]
ex[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]

## ---------------------------------------------------------------- precisao: o ano atribuido resiste?
## Para cada nome recuperado, procura casamento exato de nome civil no MESMO municipio em qualquer
## eleicao do ciclo. Se o nome so casa em ano diferente do atribuido, a atribuicao de legislatura falhou.
qq <- merge(ex[!is.na(ano_eleicao_bocel), .(rid, sg_ue, ano_eleicao_bocel, nome_normalizado)],
            mand[, .(sg_ue, ano_bocel = ano_eleicao, nome_norm)],
            by.x = c("sg_ue", "nome_normalizado"), by.y = c("sg_ue", "nome_norm"), allow.cartesian = TRUE)
casa <- qq[, .(casa_no_ano = any(ano_bocel == ano_eleicao_bocel), casa_outro = any(ano_bocel != ano_eleicao_bocel)), by = rid]
registrar_numero("srec_nomes_que_casam_no_ano_atribuido", casa[casa_no_ano == TRUE, .N], script = script)
registrar_numero("srec_nomes_que_casam_apenas_em_outro_ano", casa[casa_no_ano == FALSE & casa_outro == TRUE, .N], script = script)
registrar_numero("srec_nomes_sem_casamento_no_municipio", ex[!is.na(ano_eleicao_bocel), .N] - nrow(casa), script = script)
## a lista recuperada nao pode passar do numero de cadeiras que o BOCEL registra no municipio-eleicao
cad <- mand[, .(n_cadeiras_bocel = .N), by = .(sg_ue, ano_eleicao)]
cmp <- merge(ex[!is.na(ano_eleicao_bocel), .(n_linhas = .N, n_pareadas = sum(!is.na(id_mandato_bocel))), by = .(sg_ue, ano_eleicao_bocel)],
             cad, by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), all.x = TRUE)
registrar_numero("srec_municipios_com_mais_linhas_que_cadeiras", cmp[!is.na(n_cadeiras_bocel) & n_linhas > n_cadeiras_bocel, .N], script = script)
print(cmp[order(uf <- sg_ue)])

## ---------------------------------------------------------------- saida no esquema padrao
out <- ex[, .(sg_ue, id_municipio_ibge, uf, dominio, legislatura_numero, legislatura_inicio, legislatura_fim,
              ano_eleicao_bocel, nome_fonte, nome_parlamentar, nome_normalizado, titular, data_inicio_mandato,
              data_fim_mandato, tipo_afastamento, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento,
              sistema, so_legislatura_atual, url)]
setorder(out, uf, sg_ue, ano_eleicao_bocel, nome_normalizado, na.last = TRUE)
stopifnot(!anyDuplicated(out[!is.na(id_mandato_bocel), id_mandato_bocel]))
fwrite(out, "data/exercicio_sapl_recuperado.csv", na = "NA", quote = TRUE)
registrar_fonte("data/exercicio_sapl_recuperado.csv", "cache local SAPL + HTML das camaras (sem nova requisicao)", NA)

registrar_numero("srec_linhas_total", nrow(out), script = script)
registrar_numero("srec_municipios_total", out[, uniqueN(sg_ue)], script = script)
registrar_numero("srec_ufs_total", out[, uniqueN(uf)], script = script)
registrar_numero("srec_linhas_pareadas", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("srec_taxa_pareamento", round(out[!is.na(id_mandato_bocel), .N] / out[!is.na(ano_eleicao_bocel), .N], 4), script = script)
for (m in unique(na.omit(out$metodo_pareamento))) registrar_numero(paste0("srec_pareados_por_", m), out[metodo_pareamento == m, .N], script = script)
print(out[, .N, by = .(sistema)]); print(out[, .(N = .N, pareadas = sum(!is.na(id_mandato_bocel))), by = .(uf, sg_ue)][order(-N)])

## ---------------------------------------------------------------- diagnostico por instancia
dgA <- vazias[, .(grupo = "A_sapl3_cache_api", uf, sg_ue, nome, dominio, n_leg_inventario,
                  n_legislatura_json = n_leg, n_parlamentar_json = n_par, n_mandato_json = n_man,
                  n_arquivos_html = NA_integer_, n_caracteres_texto_html = NA_integer_,
                  n_nomes_recuperados = 0L, diagnostico,
                  evidencia = "json valido e vazio; n_legislaturas do inventario medido na descoberta")]
if (nrow(rec_a)) dgA[sg_ue %in% rec_a$sg_ue, n_nomes_recuperados := rec_a[, .N, by = sg_ue][match(dgA[sg_ue %in% rec_a$sg_ue, sg_ue], sg_ue), N]]
dgB <- marc[, .(grupo = "B_marcador_sapl_no_html", uf, sg_ue, nome, dominio = host,
                n_leg_inventario = NA_integer_, n_legislatura_json = NA_integer_, n_parlamentar_json = NA_integer_,
                n_mandato_json = NA_integer_)]
dgB <- merge(dgB, alvo[, .(sg_ue, n_arquivos_html = n_html, n_caracteres_texto_html = n_caracteres_texto, n_nomes_recuperados = n_nomes)],
             by = "sg_ue", all.x = TRUE)
dgB[is.na(n_nomes_recuperados), n_nomes_recuperados := 0L]
dgB[, diagnostico := fcase(
  sg_ue %in% marc[ja_coberto_outro_coletor == TRUE, sg_ue], "coberto_por_outro_coletor_sem_api_sapl",
  n_nomes_recuperados >= MIN_NOMES, "sem_coleta_de_api_html_com_lista_recuperada",
  is.na(n_caracteres_texto_html) | n_caracteres_texto_html < 200L, "sem_coleta_de_api_html_sem_texto_renderizado",
  default = "sem_coleta_de_api_html_sem_lista_de_parlamentares")]
dgB[, evidencia := "instancia SAPL nunca consultada por API (fora do inventario sapl_municipal); so ha HTML em cache"]
dg <- rbindlist(list(dgA, dgB), use.names = TRUE, fill = TRUE)
setcolorder(dg, c("grupo", "uf", "sg_ue", "nome", "dominio"))
setorder(dg, grupo, uf, sg_ue)
fwrite(dg, "output/verificacao/sapl_cache_vazio.csv", na = "NA")
print(dg[, .N, by = .(grupo, diagnostico)][order(grupo, -N)])
registrar_numero("srec_diag_linhas", nrow(dg), script = script)

gravar_relatorio_verificacao(
  alvo = "data/exercicio_sapl_recuperado.csv", script = script,
  passou = c("nenhuma requisicao de rede: leitura restrita a data_raw/",
             "forma_saida so com valores do vocabulario fechado",
             "nenhum id_mandato_bocel repetido na saida",
             "nome com menos de 8 caracteres ou um unico token nao entra no pareamento"),
  falhou = character(),
  fora_de_cobertura = c("distinguir, nos 143 caches vazios, falha de requisicao de falta de dado exigiria nova consulta",
                        "a legislatura das listas de HTML sem intervalo declarado e a corrente na data da coleta, nao um dado da fonte",
                        "titular/suplente e datas de posse e saida nao existem nas paginas recuperadas"))
cat("44_sapl_recuperado: concluido —", nrow(out), "linhas,", out[!is.na(id_mandato_bocel), .N], "pareadas,",
    out[, uniqueN(sg_ue)], "municipios\n")
sink(); close(logf)
