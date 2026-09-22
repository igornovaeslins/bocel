# 22_exercicio_assembleias_historico.R — deputados estaduais por legislatura nas fontes estruturadas
# alternativas das Assembleias cujo portal principal so lista a legislatura atual (frente 'assembleias
# sem historico': AP, CE, ES, MA, MG, MS, MT, PA, PE, RN, RS, SC, SE e DF).
# Fontes parseadas (levantamento e coleta em python/fetch_assembleias_historico.py, 29/ago/2026):
#   AP  ALAP  listas por legislatura (V-IX) + pagina do deputado (nome civil, aniversario, legislaturas)
#   MG  ALMG  'Legislaturas anteriores' (14a-19a) e legislatura atual (20a) por situacao ao fim da
#             legislatura (em exercicio, exerceram, renunciaram, afastados, perderam o mandato) + perfil
#             (nome completo, nascimento)
#   MT  ALMT  'Membros Parlamentares' por legislatura (nome parlamentar, nome civil, partido, condicao)
#   SC  ALESC Memoria Politica: eleitos e suplentes convocados por legislatura (prosa)
#   SE  ALESE aleselegis: parlamentares por legislatura + perfil (nome civil, legislaturas)
# Entrada:  data_raw/assembleias_sem_historico/<UF>/*.html, data/mandatos.csv, data/pessoas.csv, data/wikipedia_estadual.csv
# Saida:    data/exercicio_assembleias_historico.csv (colunas de data/exercicio_assembleias.csv),
#           data/exercicio_assembleias_historico_cobertura.csv,
#           data_raw/assembleias_sem_historico/inventario_fontes.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/22_exercicio_assembleias_historico.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(rvest); library(xml2); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/22_exercicio_assembleias_historico.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/22_exercicio_assembleias_historico.log", open = "wt"); sink(logf, split = TRUE)
RAW <- "data_raw/assembleias_sem_historico"
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
norm <- function(x) { x <- stri_trans_general(toupper(fcoalesce(x, "")), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
limpa <- function(x) { x <- gsub("&nbsp;|\u00a0", " ", x); trimws(gsub("\\s+", " ", x)) }
texto <- function(arq) { h <- read_html(arq, encoding = "UTF-8"); xml_remove(html_elements(h, "script, style")); limpa(html_text2(h)) }
d_br <- function(x) { m <- stri_match_first_regex(fcoalesce(x, ""), "(\\d{2})/(\\d{2})/(\\d{4})"); ifelse(is.na(m[, 1]), NA_character_, sprintf("%s-%s-%s", m[, 4], m[, 3], m[, 2])) }
# legislatura -> ano da eleicao: a numeracao e continua (4 anos) em MG, MT, SC e SE (15a = eleicao de 2002);
# na ALAP a V Legislatura e 2007-2011 (eleicao de 2006)
ano_leg <- function(uf, leg) { leg <- as.integer(leg); fifelse(uf == "AP", 4L * leg + 1986L, 4L * leg + 1942L) }

## ------------------------------------------------ inventario das fontes (levantamento 29/ago/2026)
inv <- rbindlist(list(
  list("AP", "ALAP", "Portal: Parlamentares por legislatura (pagina.php?pg=exibir_legislatura&idlegislatura=N) + pagina do deputado (nome civil, aniversario, legislaturas, situacao)", "https://www.al.ap.gov.br/pagina.php?pg=exibir_legislatura", "html", "VI (2011-2015) a IX (2023-2027), so os deputados com pagina ativa (25); I-V listadas sem membros", FALSE, TRUE, "listas por legislatura incompletas: so aparecem deputados com pagina ativa no portal; sem datas de posse/saida; nome civil e nascimento no perfil"),
  list("CE", "ALECE", "PDF 'Deputados na Historia' (MALCE): lista alfabetica de deputados provinciais e estaduais 1835-2015, sem legislatura", "https://www.al.ce.gov.br/userfiles/files/deputadosnahistoria.pdf", "pdf", "nenhuma (lista alfabetica sem legislatura)", FALSE, FALSE, "24 paginas de nomes sem legislatura, sem datas; nao pareavel por mandato. Atas por legislatura (index.php/atas/legislaturas) em PDF de sessao, nao estruturadas"),
  list("DF", "CLDF", "Portal: 'Legislaturas anteriores' com deputados e suplentes por legislatura (ja coletado em 13_exercicio_assembleias.R)", "https://www.cl.df.gov.br/legislaturas-anteriores", "html", "1a (1991) a 8a (2019-2022) + atual", FALSE, TRUE, "ja coletada (cldf_legislaturas_anteriores); sem datas de posse/saida; dadosabertos.cl.df.gov.br sem resposta TLS"),
  list("ES", "ALES", "Portal /Deputado/Lista (aplicacao JS, so a legislatura atual); dadosabertos.al.es.gov.br nao resolve", "https://www.al.es.gov.br/Deputado/Lista", "html", "atual", FALSE, FALSE, "sem lista historica; pagina do deputado sem legislaturas"),
  list("MA", "ALEMA", "Portal sitealema/deputados (so atual); Memorial sem lista por legislatura", "https://www.al.ma.leg.br/sitealema/deputados/", "html", "atual", FALSE, FALSE, "sem lista historica"),
  list("MG", "ALMG", "Portal: 'Legislaturas anteriores' (14a-19a) por situacao ao fim da legislatura + legislatura atual por situacao; perfil com nome completo e nascimento; PDFs so para 1a-13a", "https://www.almg.gov.br/a-assembleia/deputados/legislaturas-anteriores/", "html", "14a (1999-2003) a 20a (2023-2027)", FALSE, TRUE, "situacao ao fim da legislatura (em exercicio, exerceram, renunciaram, afastados, perderam) sem data do evento; API v2 /legislaturas/{id}/deputados retorna 500"),
  list("MS", "ALEMS", "Portal al.ms.gov.br: rotas /deputados, /Deputados, /Parlamentares, /ex-deputados retornam 404; sem lista estruturada", "https://al.ms.gov.br/", "html", "nenhuma", FALSE, FALSE, "sem lista por legislatura"),
  list("MT", "ALMT", "Portal: 'Membros Parlamentares' por legislatura (accordion com frames /parlamento/legislatura/<id>/membros)", "https://www.al.mt.gov.br/parlamento/membros-parlamentares", "html", "1a (1947) a 20a (2023-2027)", FALSE, TRUE, "nome parlamentar, nome civil, partido e condicao (titular/suplente); perfil sem datas de mandato"),
  list("PA", "ALEPA", "Portal /Institucional/Deputados (DevExpress, so a 61a legislatura); Memorial Legislativo sem lista", "https://www.alepa.pa.gov.br/Institucional/Deputados", "html", "atual", FALSE, FALSE, "parametro legislatura ignorado"),
  list("PE", "ALEPE", "Portal /parlamentares (so atual); dadosabertos.alepe.pe.gov.br/api/v1/parlamentares so em exercicio; /legislaturas-anteriores 404", "https://www.alepe.pe.gov.br/parlamentares/", "html", "atual", FALSE, FALSE, "sem lista historica"),
  list("RN", "ALRN", "Portal /deputados (so atual); memorial.al.rn.leg.br sem lista de parlamentares", "https://www.al.rn.leg.br/deputados", "html", "atual", FALSE, FALSE, "sem lista historica"),
  list("RS", "ALRS", "Portal ww4.al.rs.gov.br/deputados (56a legislatura); acervomemorial.al.rs.gov.br (AtoM) sem base de deputados; ws.al.rs.gov.br nao resolve", "https://ww4.al.rs.gov.br/deputados", "html", "atual", FALSE, FALSE, "sem lista historica estruturada"),
  list("SC", "ALESC", "Memoria Politica de Santa Catarina: pagina por legislatura com eleitos e suplentes convocados (prosa) e biografias", "https://memoriapolitica.alesc.sc.gov.br/legislativo/deputado-estadual/legislaturas", "html", "1a (1947) a 20a (2023-2027)", FALSE, TRUE, "nomes em prosa (eleitos; suplentes convocados), partido a partir da 19a; sem datas"),
  list("SE", "ALESE", "aleselegis (SPL): parlamentares por legislatura (parlamentares.aspx?leg=N) + perfil com nome civil e legislaturas exercidas", "https://aleselegis.al.se.leg.br/spl/parlamentares.aspx", "html", "14a (1999-2003) a 20a (2023-2027)", FALSE, TRUE, "sem datas de posse/saida individuais; a lista separa ativos de 'deputados que nao encerraram o mandato' (sem a causa)")
))
setnames(inv, c("uf", "casa", "fonte", "url", "tipo", "legislaturas_cobertas", "tem_datas", "viavel", "observacao"))
fwrite(inv, file.path(RAW, "inventario_fontes.csv"), na = "NA", quote = TRUE)

## ------------------------------------------------ parsers
vazio <- function() data.table(uf = character(), fonte = character(), legislatura = character(), nome = character(), nome_completo = character(),
                               data_nascimento = character(), partido = character(), condicao = character(), causa_original = character(),
                               forma_saida = character(), url = character(), id_fonte = character())

# --- MT: tabela Nome do Parlamentar | Partido | Condicao; 'Nome parlamentar (Nome civil)'
parse_mt <- function() {
  fs <- list.files(file.path(RAW, "MT"), pattern = "^legislatura_\\d+\\.html$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    leg <- stri_extract_first_regex(basename(f), "\\d+")
    h <- read_html(f, encoding = "UTF-8")
    tr <- html_elements(h, "table tbody tr")
    if (!length(tr)) return(NULL)
    rbindlist(lapply(tr, function(r) {
      td <- html_elements(r, "td"); if (length(td) < 3) return(NULL)
      a <- html_element(td[1], "a"); nm <- limpa(html_text2(a)); id <- html_attr(a, "data-mandato-id")
      civ <- stri_match_first_regex(nm, "\\(([^)]+)\\)\\s*$")[, 2]
      urna <- trimws(sub("\\s*\\([^)]*\\)\\s*$", "", nm))
      cond <- tolower(limpa(html_text2(td[3])))
      data.table(uf = "MT", fonte = "almt_membros_legislatura", legislatura = leg, nome = urna, nome_completo = fcoalesce(civ, urna),
                 data_nascimento = NA_character_, partido = limpa(html_text2(td[2])),
                 condicao = fifelse(grepl("suplente", cond), "suplente", "titular"), causa_original = cond,
                 forma_saida = NA_character_, url = "https://www.al.mt.gov.br/parlamento/membros-parlamentares", id_fonte = id)
    }))
  }), fill = TRUE)
}

# --- SC: prosa 'Foram eleitos ... : A; B (PARTIDO); ...' e 'Suplentes convocado(a)s: ...'
parse_sc <- function() {
  fs <- list.files(file.path(RAW, "SC"), pattern = "^legislatura_\\d+_\\d+\\.html$", full.names = TRUE)
  # a Memoria Politica numera duas series: ids 95-109 sao as legislaturas de 1891-1930 (a '14a' de id 109 e de 1930);
  # so a serie pos-1947 (ids >= 110) corresponde a numeracao continua usada aqui (verificador, 29/ago/2026)
  fs <- fs[as.integer(sub("^legislatura_\\d+_(\\d+)\\.html$", "\\1", basename(fs))) >= 110L]
  rbindlist(lapply(fs, function(f) {
    leg <- stri_extract_first_regex(basename(f), "\\d+")
    h <- read_html(f, encoding = "UTF-8")
    ps <- limpa(html_text2(html_elements(h, "p")))
    corta <- function(txt) {
      txt <- sub("^.*?:\\s*", "", txt)               # tira o rotulo ate o primeiro ':'
      txt <- sub("\\.\\s*(Os tr[eê]s|Suplentes|Mesa Diretora|A elei|As elei|Dos \\d+|Nesta).*$", "", txt)
      txt <- sub("\\.\\s*$", "", txt)
      it <- trimws(unlist(strsplit(txt, ";|,(?=\\s+[A-ZÁÉÍÓÚÂÊÔÃÕÇ][^,;]*(\\(|;|$))", perl = TRUE)))
      it <- sub("^e\\s+", "", it); it <- it[nchar(it) > 2 & !grepl("^https?://", it)]
      part <- stri_match_first_regex(it, "\\(([^)]+)\\)\\s*$")[, 2]
      nome <- trimws(sub("\\s*\\([^)]*\\)\\s*$", "", it))
      # 'Antonio Carlos Vieira - Vieirao', 'Renato Jardel Gurtinski – Renato Pike': nome civil - apelido
      apel <- stri_match_first_regex(nome, "^(.*?)\\s+[-–]\\s+(.+)$")
      data.table(nome = fifelse(is.na(apel[, 1]), nome, apel[, 3]), nome_completo = fifelse(is.na(apel[, 1]), nome, apel[, 2]), partido = part)
    }
    el <- ps[grepl("^Foram eleitos", ps)][1]
    su <- ps[grepl("^(Foram )?Suplentes? convocad", ps)]
    out <- list()
    if (!is.na(el)) out[[1]] <- corta(el)[, condicao := "titular"]
    if (length(su)) out[[2]] <- rbindlist(lapply(su, function(s) corta(sub("^.*?convocad[^:]*:\\s*(Suplentes convocados/convocadas:\\s*)?", "X:", s))))[, condicao := "suplente"]
    d <- rbindlist(out, fill = TRUE); if (!nrow(d)) return(NULL)
    d[, `:=`(uf = "SC", fonte = "alesc_memoria_politica", legislatura = leg, data_nascimento = NA_character_, causa_original = condicao,
             forma_saida = NA_character_, url = paste0("https://memoriapolitica.alesc.sc.gov.br/legislativo/deputado-estadual/legislaturas/", sub("legislatura_\\d+_(\\d+)\\.html", "\\1", basename(f)), "-", leg, "a_Legislatura"), id_fonte = NA_character_)]
    d[!duplicated(d[, .(nome, condicao)])]
  }), fill = TRUE)
}

# --- SE: lista por legislatura (fieldset com legenda de situacao) + perfil (nome civil)
parse_se <- function() {
  fs <- list.files(file.path(RAW, "SE"), pattern = "^legislatura_\\d+\\.html$", full.names = TRUE)
  lst <- rbindlist(lapply(fs, function(f) {
    leg <- stri_extract_first_regex(basename(f), "\\d+")
    h <- read_html(f, encoding = "UTF-8")
    fsets <- html_elements(h, "#ContentPlaceHolder1_parlamentares_lista .custom-fieldset")
    if (!length(fsets)) fsets <- html_elements(h, ".custom-fieldset")
    rbindlist(lapply(fsets, function(fsx) {
      legenda <- limpa(html_text2(html_element(fsx, ".custom-legend")))
      as_ <- html_elements(fsx, "a.kt-widget__username")
      if (!length(as_)) return(NULL)
      id <- stri_extract_first_regex(html_attr(as_, "href"), "\\d+")
      nm <- limpa(html_text2(as_))
      part <- vapply(as_, function(a) { s <- xml_find_first(a, "following-sibling::span"); if (inherits(s, "xml_missing")) NA_character_ else limpa(gsub("[()]", "", html_text2(s))) }, character(1))
      data.table(legislatura = leg, id_fonte = id, nome = nm, partido = part, situacao_lista = legenda)
    }))
  }), fill = TRUE)
  if (!nrow(lst)) return(vazio())
  perf <- rbindlist(lapply(unique(lst$id_fonte), function(i) {
    f <- file.path(RAW, "SE", paste0("parlamentar_", i, ".html")); if (!file.exists(f)) return(data.table(id_fonte = i))
    tx <- texto(f)
    civ <- stri_match_first_regex(tx, "Nome civil:\\s*(.*?)\\s+(Telefone|Celular|E-mail|Gabinete)")[, 2]
    legs <- unique(stri_extract_all_regex(tx, "\\d+ª Legislatura \\d{2}/\\d{2}/\\d{4} a \\d{2}/\\d{2}/\\d{4}")[[1]])
    data.table(id_fonte = i, nome_completo = civ, legislaturas_perfil = paste(stri_extract_first_regex(legs, "\\d+"), collapse = ";"))
  }), fill = TRUE)
  d <- merge(lst, perf, by = "id_fonte", all.x = TRUE)
  # legenda da lista: 'Ativo' (encerrou a legislatura) vs 'Deputados que nao encerraram o mandato' (causa nao informada)
  d[, sit := tolower(fcoalesce(situacao_lista, ""))]
  d[, `:=`(uf = "SE", fonte = "alese_legis_legislatura", nome_completo = fcoalesce(nome_completo, nome), data_nascimento = NA_character_,
           condicao = "titular", causa_original = fifelse(sit == "ativo", "ativo (lista da legislatura)", sit),
           forma_saida = fcase(grepl("nao encerraram|não encerraram", sit), "outro", sit == "ativo" & as.integer(legislatura) < 20L, "fim_regular", default = NA_character_),
           url = paste0("https://aleselegis.al.se.leg.br/spl/parlamentar.aspx?id=", id_fonte))]
  d[, .(uf, fonte, legislatura, nome, nome_completo, data_nascimento, partido, condicao, causa_original, forma_saida, url, id_fonte)]
}

# --- AP: lista por legislatura (tooltip 'Nome Completo: ... Partido: ...') + perfil (Nome Civil, Aniversario, Situacao)
parse_ap <- function() {
  fs <- list.files(file.path(RAW, "AP"), pattern = "^legislatura_\\d+\\.html$", full.names = TRUE)
  lst <- rbindlist(lapply(fs, function(f) {
    leg <- stri_extract_first_regex(basename(f), "\\d+")
    raw <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    m <- stri_match_all_regex(raw, "iddeputado=(\\d+)[^>]*>(.*?)</a>", opts_regex = list(dotall = TRUE))[[1]]
    if (!nrow(m) || is.na(m[1, 1])) return(NULL)
    tx <- limpa(gsub("<[^>]+>", " ", m[, 3]))
    d <- data.table(legislatura = leg, id_fonte = m[, 2], bloco = tx)
    d <- d[grepl("Nome Completo", bloco)]
    d[, nome := limpa(stri_match_first_regex(bloco, "Dep\\.?\\s*(.*?)\\s*Nome Completo")[, 2])]
    d[, nome_completo := limpa(stri_match_first_regex(bloco, "Nome Completo:\\s*(.*?)\\s*(Partido:|Profiss)")[, 2])]
    d[, partido := limpa(stri_match_first_regex(bloco, "Partido:\\s*(.*?)\\s*(Profiss|$)")[, 2])]
    d[!duplicated(id_fonte), .(legislatura, id_fonte, nome, nome_completo, partido)]
  }), fill = TRUE)
  if (!nrow(lst)) return(vazio())
  perf <- rbindlist(lapply(unique(lst$id_fonte), function(i) {
    f <- file.path(RAW, "AP", paste0("parlamentar_", i, ".html")); if (!file.exists(f)) return(data.table(id_fonte = i))
    tx <- texto(f)
    data.table(id_fonte = i,
               nome_civil = limpa(stri_match_first_regex(tx, "Nome Civil:\\s*(.*?)\\s+(E-mail|Anivers|Profiss|Partido)")[, 2]),
               data_nascimento = d_br(stri_match_first_regex(tx, "Anivers[áa]rio:\\s*(\\d{2}/\\d{2}/\\d{4})")[, 2]),
               situacao_perfil = limpa(stri_match_first_regex(tx, "Situa[çc][ãa]o:\\s*(\\w+)")[, 2]))
  }), fill = TRUE)
  d <- merge(lst, perf, by = "id_fonte", all.x = TRUE)
  d[, `:=`(uf = "AP", fonte = "alap_legislatura", nome_completo = fcoalesce(nome_civil, nome_completo, nome), condicao = "titular",
           causa_original = tolower(fcoalesce(situacao_perfil, "")), forma_saida = NA_character_,
           url = paste0("https://www.al.ap.gov.br/pagina.php?pg=exibir_parlamentar&iddeputado=", id_fonte))]
  d[, .(uf, fonte, legislatura, nome, nome_completo, data_nascimento, partido, condicao, causa_original, forma_saida, url, id_fonte)]
}

# --- MG: listas por legislatura x situacao ao fim da legislatura + perfil (nome completo, nascimento)
SIT_MG <- c("1" = "em exercicio ao fim da legislatura", "2" = "exerceu o mandato (nao em exercicio ao fim)", "3" = "renunciou", "4" = "afastado", "8" = "perdeu o mandato")
parse_mg <- function() {
  fs <- list.files(file.path(RAW, "MG"), pattern = "^lista_leg\\d+_sit\\d+\\.html$", full.names = TRUE)
  lst <- rbindlist(lapply(fs, function(f) {
    leg <- stri_match_first_regex(basename(f), "leg(\\d+)_sit(\\d+)")
    h <- read_html(f, encoding = "UTF-8")
    as_ <- html_elements(h, "a.h4[href*='/deputados/']")
    if (!length(as_)) return(NULL)
    href <- html_attr(as_, "href")
    part <- vapply(as_, function(a) { b <- xml_find_first(a, "following-sibling::div//span[contains(@class,'almg-css_partido')]"); if (inherits(b, "xml_missing")) NA_character_ else limpa(html_text2(b)) }, character(1))
    data.table(legislatura = leg[, 2], cod_sit = leg[, 3], id_fonte = stri_extract_first_regex(href, "/(\\d+)(\\?|$)") |> stri_extract_first_regex("\\d+"),
               nome = limpa(html_text2(as_)), partido = part, href = sub("\\?.*$", "", href))
  }), fill = TRUE)
  if (!nrow(lst)) return(vazio())
  perf <- rbindlist(lapply(split(lst, by = c("id_fonte", "legislatura"), keep.by = TRUE), function(x) {
    i <- x$id_fonte[1]; l <- x$legislatura[1]
    f <- file.path(RAW, "MG", paste0("perfil_", i, "_leg", l, ".html")); if (!file.exists(f)) return(data.table(id_fonte = i, legislatura = l))
    tx <- texto(f)
    data.table(id_fonte = i, legislatura = l,
               nome_completo = limpa(stri_match_first_regex(tx, "Nome Completo\\s+(.*?)\\s+(Data de Nascimento|Naturalidade|Atividade|Principais)")[, 2]),
               data_nascimento = d_br(stri_match_first_regex(tx, "Data de Nascimento\\s+(\\d{2}/\\d{2}/\\d{4})")[, 2]),
               situacao_perfil = limpa(stri_match_first_regex(tx, "Situa[çc][ãa]o (ao fim da \\d+ª legislatura|atual)\\s+(.*?)\\s+Biografia")[, 3]))
  }), fill = TRUE)
  d <- merge(lst, perf, by = c("id_fonte", "legislatura"), all.x = TRUE)
  # a mesma pessoa em mais de uma lista da legislatura: prevalece o evento (renuncia, perda, afastamento) sobre 'em exercicio' e 'exerceu'
  d[, prio := match(cod_sit, c("3", "8", "4", "1", "2"))]
  setorder(d, legislatura, id_fonte, prio)
  d <- d[!duplicated(d[, .(legislatura, id_fonte)])]
  d[, situacao_perfil := limpa(sub("\\s*Veja as informa.*$", "", fcoalesce(situacao_perfil, "")))]
  # perfil 'Termino de exercicio de suplencia' / 'Suplente ...': a pessoa exerceu como suplente, nao e o titular do BOCEL
  d[, cond := fifelse(grepl("supl[eê]ncia|suplente", situacao_perfil, ignore.case = TRUE), "suplente", "titular")]
  d[, `:=`(uf = "MG", fonte = "almg_legislaturas_anteriores", nome_completo = fcoalesce(nome_completo, nome), condicao = cond,
           causa_original = paste0(SIT_MG[cod_sit], fifelse(is.na(situacao_perfil) | situacao_perfil == "", "", paste0(" | perfil: ", situacao_perfil))),
           forma_saida = fcase(cond == "suplente", NA_character_,
                               grepl("^Fale", situacao_perfil), "falecimento",
                               grepl("^Licen", situacao_perfil), "licenca",
                               cod_sit == "1" & legislatura == "20", NA_character_,   # legislatura em curso: sem saida
                               cod_sit == "1", "fim_regular", cod_sit == "3", "renuncia", cod_sit == "8", "cassacao", cod_sit == "4", "afastamento", cod_sit == "2", "outro", default = NA_character_),
           url = paste0("https://www.almg.gov.br", href, "?legislatura=", legislatura))]
  d[, .(uf, fonte, legislatura, nome, nome_completo, data_nascimento, partido, condicao, causa_original, forma_saida, url, id_fonte)]
}

fontes <- rbindlist(list(parse_ap(), parse_mg(), parse_mt(), parse_sc(), parse_se()), fill = TRUE)
stopifnot(nrow(fontes) > 0)
fontes[, ano_eleicao := ano_leg(uf, legislatura)]
fontes[, cargo := "DEPUTADO ESTADUAL"]
fontes <- fontes[ano_eleicao >= 1998L & ano_eleicao <= 2022L]
fontes[, nome_normalizado := norm(nome)]
fontes[, nome_completo_norm := norm(nome_completo)]
cat("linhas por UF e legislatura:\n"); print(dcast(fontes[, .N, by = .(uf, ano_eleicao)], uf ~ ano_eleicao, value.var = "N", fill = 0L))

## ------------------------------------------------ pareamento com o BOCEL (cargo 7; DF nao tem fonte nova)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("7", "8") & sg_uf %in% unique(fontes$uf)]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, cargo, sg_uf, ano_eleicao = as.integer(ano_eleicao))], pess[, .(id_pessoa, nome, nome_urna_recente, dt_nascimento)], by = "id_pessoa")
mand[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente))]
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
fontes[, rid := .I]
chave <- c("sg_uf", "cargo", "ano_eleicao")
unico <- function(m, metodo) m[, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := metodo]
# (1) nome completo da fonte = nome civil do BOCEL
m1 <- unico(merge(fontes[, .(rid, sg_uf = uf, cargo, ano_eleicao, k = nome_completo_norm)], mand[, .(sg_uf, cargo, ano_eleicao, k = nome_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_completo_civil")
feito <- m1$rid
# (2) nome parlamentar = nome de urna
m2 <- unico(merge(fontes[!rid %in% feito, .(rid, sg_uf = uf, cargo, ano_eleicao, k = nome_normalizado)], mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, k = urna_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_urna")
feito <- c(feito, m2$rid)
# (3) nome parlamentar = nome civil
m3 <- unico(merge(fontes[!rid %in% feito, .(rid, sg_uf = uf, cargo, ano_eleicao, k = nome_normalizado)], mand[, .(sg_uf, cargo, ano_eleicao, k = nome_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_parlamentar_civil")
feito <- c(feito, m3$rid)
# (4) nome completo + data de nascimento (AP, MG), pessoa unica na UF/eleicao
m4 <- unico(merge(fontes[!rid %in% feito & !is.na(data_nascimento), .(rid, sg_uf = uf, cargo, ano_eleicao, data_nascimento)], mand[!is.na(dt_nascimento), .(sg_uf, cargo, ano_eleicao, data_nascimento = dt_nascimento, id_mandato, id_pessoa)], by = c(chave, "data_nascimento")), "data_nascimento_unica")
feito <- c(feito, m4$rid)
# (5) tokens do nome completo da fonte contidos no nome civil (>= 2 tokens), mandato unico
contido <- function(cand, a, b, min_tok = 2L, metodo) {
  if (!nrow(cand)) return(data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character()))
  ta <- tok(cand[[a]]); tb <- tok(cand[[b]])
  cand[, ok := mapply(function(x, y) length(x) >= min_tok && all(x %in% y), ta, tb)]
  unico(cand[ok == TRUE], metodo)
}
c5 <- merge(fontes[!rid %in% feito, .(rid, sg_uf = uf, cargo, ano_eleicao, a = nome_completo_norm)], mand[, .(sg_uf, cargo, ano_eleicao, b = nome_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
m5 <- contido(c5, "a", "b", 2L, "tokens_nome_completo_no_civil")
feito <- c(feito, m5$rid)
# (6) tokens do nome parlamentar contidos no nome civil (>= 2 tokens)
c6 <- merge(fontes[!rid %in% feito, .(rid, sg_uf = uf, cargo, ano_eleicao, a = nome_normalizado)], mand[, .(sg_uf, cargo, ano_eleicao, b = nome_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
m6 <- contido(c6, "a", "b", 2L, "tokens_no_nome_civil")
feito <- c(feito, m6$rid)
# (7) tokens do nome parlamentar contidos no nome de urna (apelidos), pessoa unica
c7 <- merge(fontes[!rid %in% feito, .(rid, sg_uf = uf, cargo, ano_eleicao, a = nome_normalizado)], mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, b = urna_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
if (nrow(c7)) { ta <- tok(c7$a); tb <- tok(c7$b); c7[, ok := mapply(function(x, y) length(x) >= 1 && any(nchar(x) >= 4) && all(x %in% y), ta, tb)]; m7 <- unico(c7[ok == TRUE], "tokens_no_nome_de_urna") } else m7 <- m6[0]
par <- rbindlist(list(m1, m2, m3, m4, m5, m6, m7), fill = TRUE)[, .(rid, id_mandato, id_pessoa, metodo)][!duplicated(rid)]
# suplente so pareia por nome exato ou nascimento: o BOCEL so tem titulares
par <- par[!(rid %in% fontes[condicao == "suplente", rid] & grepl("^tokens", metodo))]
# um mandato do BOCEL recebe uma linha por fonte; se a mesma fonte pareia duas linhas ao mesmo mandato, fica a que traz forma de saida
fontes[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
fontes[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
setorder(fontes, uf, ano_eleicao, -forma_saida, condicao, nome_normalizado, na.last = TRUE)
dup <- fontes[!is.na(id_mandato_bocel) & duplicated(fontes[, .(uf, id_mandato_bocel)])]
cat("linhas repetidas por mandato (desfeitas):", nrow(dup), "\n")
fontes[!is.na(id_mandato_bocel) & duplicated(fontes[, .(uf, id_mandato_bocel)]), `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = paste0(metodo_pareamento, "_repetido"))]
# checagem de coerencia: pareado por nome tem de coincidir no nascimento quando a fonte a traz
chk <- merge(fontes[!is.na(id_mandato_bocel) & !is.na(data_nascimento), .(rid, data_nascimento, id_pessoa_bocel)], pess[, .(id_pessoa_bocel = id_pessoa, dt_nascimento)], by = "id_pessoa_bocel")
conflito <- chk[!is.na(dt_nascimento) & dt_nascimento != data_nascimento]
cat("pareados com nascimento divergente (desfeitos):", nrow(conflito), "\n")
if (nrow(conflito)) fontes[rid %in% conflito$rid, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = paste0(metodo_pareamento, "_nascimento_divergente"))]

## ------------------------------------------------ saida nas colunas de exercicio_assembleias.csv
fontes[is.na(forma_saida) | forma_saida == "", forma_saida := "nao_observado"]
fontes[condicao == "suplente" & forma_saida == "nao_observado", forma_saida := NA_character_]  # suplente convocado nao encerra o mandato do titular
stopifnot(all(is.na(fontes$forma_saida) | fontes$forma_saida %in% VOCAB))
out <- fontes[, .(uf, fonte, legislatura, ano_eleicao, nome, nome_normalizado, nome_completo, data_nascimento, partido, condicao,
                  data_inicio_exercicio = NA_character_, data_fim_exercicio = NA_character_, causa_original, forma_saida,
                  id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url, id_fonte, votos_fonte = NA_character_, sexo_fonte = NA_character_)]
setorder(out, uf, ano_eleicao, condicao, nome_normalizado)
fwrite(out, "data/exercicio_assembleias_historico.csv", na = "NA", quote = TRUE)

## ------------------------------------------------ cobertura por UF e legislatura (contra o BOCEL e contra a Wikipedia ja coletada)
we <- fread("data/wikipedia_estadual.csv", colClasses = "character", na.strings = "NA")[cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL") & !is.na(id_mandato_bocel), .(uf, ano_eleicao = as.integer(ano_eleicao_bocel), id_mandato_bocel, fs_wiki = forma_saida)]
mand_all <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("7", "8"), .(id_mandato, uf = sg_uf, ano_eleicao = as.integer(ano_eleicao), forma_saida_bocel = forma_saida)]
cob <- merge(mand_all[uf %in% inv$uf, .(n_bocel = .N, n_bocel_sem_saida = sum(forma_saida_bocel == "nao_observado")), by = .(uf, ano_eleicao)],
             out[condicao == "titular", .(n_fonte = .N, n_pareados = uniqueN(na.omit(id_mandato_bocel)), n_pareados_com_saida = uniqueN(na.omit(id_mandato_bocel[forma_saida != "nao_observado"]))), by = .(uf, ano_eleicao)],
             by = c("uf", "ano_eleicao"), all.x = TRUE)
cob <- merge(cob, we[, .(n_wiki_pareados = uniqueN(id_mandato_bocel)), by = .(uf, ano_eleicao)], by = c("uf", "ano_eleicao"), all.x = TRUE)
novos <- merge(out[condicao == "titular" & !is.na(id_mandato_bocel), .(uf, ano_eleicao, id_mandato_bocel)], we[, .(uf, ano_eleicao, id_mandato_bocel, wiki = TRUE)], by = c("uf", "ano_eleicao", "id_mandato_bocel"), all.x = TRUE)[is.na(wiki), .(n_novos_vs_wiki = .N), by = .(uf, ano_eleicao)]
cob <- merge(cob, novos, by = c("uf", "ano_eleicao"), all.x = TRUE)
for (cc in c("n_fonte", "n_pareados", "n_pareados_com_saida", "n_wiki_pareados", "n_novos_vs_wiki")) cob[is.na(get(cc)), (cc) := 0L]
cob[, taxa := round(n_pareados / n_bocel, 4)]
cob[, fonte := inv$fonte[match(uf, inv$uf)]][, viavel := inv$viavel[match(uf, inv$uf)]]
setcolorder(cob, c("uf", "ano_eleicao", "n_bocel", "n_bocel_sem_saida", "n_fonte", "n_pareados", "taxa", "n_pareados_com_saida", "n_wiki_pareados", "n_novos_vs_wiki", "viavel"))
fwrite(cob[order(uf, ano_eleicao)], "data/exercicio_assembleias_historico_cobertura.csv", na = "NA", quote = TRUE)
print(cob[n_fonte > 0, .(uf, ano_eleicao, n_bocel, n_fonte, n_pareados, taxa, n_pareados_com_saida, n_novos_vs_wiki)], nrows = 60)
print(out[!is.na(id_mandato_bocel), .N, by = .(uf, metodo_pareamento)][order(uf, -N)], nrows = 60)
print(out[!is.na(id_mandato_bocel) & condicao == "titular", .N, by = .(uf, forma_saida)][order(uf, -N)], nrows = 60)

## ------------------------------------------------ registro
n_minutas <- length(list.files("docs/LAI_ASSEMBLEIAS", pattern = "^[A-Z]{2}_pedido_LAI\\.md$"))
registrar_numero("ash_n_casas", nrow(inv), script = script)
registrar_numero("ash_n_com_fonte_estruturada", sum(inv$viavel), script = script)
registrar_numero("ash_n_com_fonte_nova_coletada", uniqueN(out$uf), script = script)
registrar_numero("ash_n_linhas_fonte", nrow(out), script = script)
registrar_numero("ash_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("ash_n_mandatos_pareados_com_saida", out[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)], script = script)
registrar_numero("ash_n_mandatos_novos_vs_wikipedia", sum(cob$n_novos_vs_wiki), script = script)
for (u in sort(unique(out$uf))) {
  registrar_numero(paste0("ash_", tolower(u), "_n_mandatos_pareados"), out[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
  registrar_numero(paste0("ash_", tolower(u), "_taxa_pareamento"), round(out[uf == u & condicao == "titular" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)] / cob[uf == u, sum(n_bocel)], 4), script = script)
}
registrar_numero("ash_n_minutas_lai", n_minutas, script = script)
## ------------------------------------------------ verificacao
passou <- character(); falhou <- character()
chk_ <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg) }
chk_(all(out$forma_saida[!is.na(out$forma_saida)] %in% VOCAB), "forma_saida no vocabulario fechado")
chk_(!any(duplicated(out[!is.na(id_mandato_bocel), id_mandato_bocel])), "um mandato do BOCEL recebe no maximo uma linha")
chk_(all(out[!is.na(id_mandato_bocel), id_mandato_bocel] %in% mand$id_mandato), "id_mandato_bocel existe em mandatos.csv (cargo 7/8)")
chk_(all(out$ano_eleicao %in% seq(1998L, 2022L, 4L)), "ano_eleicao em 1998..2022 (eleicoes estaduais)")
chk_(all(cob$taxa <= 1), "taxa de pareamento <= 1 por UF/eleicao")
chk_(all(out[condicao == "suplente" & !is.na(id_mandato_bocel), !grepl("^tokens", metodo_pareamento)]), "suplente so pareado por nome exato ou nascimento")
chk_(all(out[uf == "MG" & ano_eleicao == 2022L & !is.na(id_mandato_bocel), forma_saida] != "fim_regular"), "legislatura em curso sem fim_regular (MG 2022)")
chk_(nrow(out[!is.na(id_mandato_bocel) & !is.na(data_nascimento)]) == 0 || nrow(conflito) == 0 || !any(out[rid %in% conflito$rid, !is.na(id_mandato_bocel)]), "pareado por nome nao diverge no nascimento")
chk_(identical(names(out), names(fread("data/exercicio_assembleias.csv", nrows = 1))), "colunas identicas a exercicio_assembleias.csv")
rel <- gravar_relatorio_verificacao("data/exercicio_assembleias_historico.csv", script, passou = passou, falhou = falhou,
  fora_de_cobertura = c("datas de posse/saida nao existem nas fontes (so situacao ao fim da legislatura em MG e SE)",
                        "pertinencia do pareamento por tokens depende de unicidade do nome na UF/eleicao (amostra em output/verificacao/ash_amostra_tokens.csv)",
                        "listas da ALAP so trazem deputados com pagina ativa; ALESC Memoria Politica e prosa editada"))
cat("relatorio de verificacao:", rel, "| passou:", length(passou), "| falhou:", length(falhou), "\n")
if (length(falhou)) stop("verificacao reprovada: ", paste(falhou, collapse = "; "))
set.seed(20260827)
am <- out[!is.na(id_mandato_bocel) & grepl("^tokens", metodo_pareamento)]
am <- am[sample.int(nrow(am), min(60L, nrow(am)))]
am <- merge(am[, .(uf, ano_eleicao, nome, nome_completo, metodo_pareamento, id_mandato_bocel)], mand[, .(id_mandato_bocel = id_mandato, nome_bocel = nome, urna_bocel = nome_urna_recente)], by = "id_mandato_bocel")
fwrite(am[order(uf, ano_eleicao)], "output/verificacao/ash_amostra_tokens.csv", na = "NA", quote = TRUE)
registrar_fonte("data/exercicio_assembleias_historico.csv", "assembleias_sem_historico (ALAP, ALMG, ALMT, ALESC Memoria Politica, ALESE)")
cat("22_exercicio_assembleias_historico: concluido —", nrow(out), "linhas,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados,", n_minutas, "minutas de LAI\n")
sink()
