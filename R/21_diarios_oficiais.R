# 21_diarios_oficiais.R — eventos de saida e posse em diarios oficiais municipais (Querido Diario, OKBR)
# Entrada:  data_raw/querido_diario/cobertura_cidades.csv, data_raw/querido_diario/<ibge>/<termo>.json
#           (python/fetch_querido_diario.py), data/mandatos.csv, data/pessoas.csv, data/municipios_tse_ibge.csv,
#           data_raw/parquet/cand_*.parquet (nome de urna por mandato)
# Saida:    data/diarios_cobertura_bocel.csv, data/diarios_eventos.csv, data/diarios_mandatos_saida.csv,
#           data/diarios_taxa_uf_ano.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/21_diarios_oficiais.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/21_diarios_oficiais.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", showWarnings = FALSE, recursive = TRUE)
logf <- file("logs/21_diarios_oficiais.log", open = "wt"); sink(logf, split = TRUE)
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
QD <- "data_raw/querido_diario"

## ---- PASSO 1: cobertura das cidades e cruzamento com o BOCEL
cob <- fread(file.path(QD, "cobertura_cidades.csv"), colClasses = "character", na.strings = c("", "NA"))
cob[, total_edicoes := as.integer(total_edicoes)]
cob[, coberta := !is.na(total_edicoes) & total_edicoes > 0]
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13") & ano_eleicao >= "2002" & ano_eleicao <= "2024"]
mand <- merge(mand, mun[, .(sg_ue, id_municipio_ibge)], by.x = "unidade_posicao", by.y = "sg_ue", all.x = TRUE)
mand <- merge(mand, cob[, .(id_municipio_ibge, coberta, qd_primeira = primeira_data, qd_ultima = ultima_data, qd_total = total_edicoes)], by = "id_municipio_ibge", all.x = TRUE)
mand[is.na(coberta), coberta := FALSE]
# mandato tem janela dentro do periodo coberto quando o intervalo [mandato_inicio, mandato_fim] cruza [primeira, ultima]
mand[, janela_coberta := coberta & !is.na(qd_primeira) & as.IDate(mandato_inicio) <= as.IDate(qd_ultima) & as.IDate(mandato_fim) >= as.IDate(qd_primeira)]
cob_bocel <- mand[, .(n_mandatos = .N, n_em_cidade_coberta = sum(coberta), n_com_janela_coberta = sum(janela_coberta),
                    n_sem_saida = sum(forma_saida == "nao_observado"),
                    n_sem_saida_janela_coberta = sum(forma_saida == "nao_observado" & janela_coberta)), by = .(cargo, cd_cargo)][order(cd_cargo)]
print(cob_bocel)
fwrite(cob_bocel, "data/diarios_cobertura_bocel.csv", na = "NA")
registrar_numero("qd_n_municipios_lista_cities", nrow(cob), script = script)
registrar_numero("qd_n_municipios_com_edicoes", sum(cob$coberta), script = script)
registrar_numero("qd_n_edicoes_total", sum(cob$total_edicoes, na.rm = TRUE), script = script)
registrar_numero("qd_primeira_data_min", min(cob[coberta == TRUE, primeira_data], na.rm = TRUE), script = script)
registrar_numero("qd_n_municipios_com_edicao_antes_2013", cob[coberta == TRUE & primeira_data < "2013-01-01", .N], script = script)
registrar_numero("qd_n_municipios_com_edicao_antes_2017", cob[coberta == TRUE & primeira_data < "2017-01-01", .N], script = script)
registrar_numero("qd_mediana_primeira_data", as.character(median(as.IDate(cob[coberta == TRUE, primeira_data]))), script = script)
for (i in seq_len(nrow(cob_bocel))) {
  registrar_numero(sprintf("qd_bocel_%s_mandatos_2002_2024", cob_bocel$cd_cargo[i]), cob_bocel$n_mandatos[i], script = script)
  registrar_numero(sprintf("qd_bocel_%s_em_cidade_coberta", cob_bocel$cd_cargo[i]), cob_bocel$n_em_cidade_coberta[i], script = script)
  registrar_numero(sprintf("qd_bocel_%s_com_janela_coberta", cob_bocel$cd_cargo[i]), cob_bocel$n_com_janela_coberta[i], script = script)
  registrar_numero(sprintf("qd_bocel_%s_sem_saida_com_janela_coberta", cob_bocel$cd_cargo[i]), cob_bocel$n_sem_saida_janela_coberta[i], script = script)
}
registrar_numero("qd_bocel_prefeitos_vereadores_em_cidade_coberta", mand[cd_cargo %in% c("11", "13") & coberta == TRUE, .N], script = script)
registrar_numero("qd_bocel_prefeitos_vereadores_com_janela_coberta", mand[cd_cargo %in% c("11", "13") & janela_coberta == TRUE, .N], script = script)

## ---- PASSO 3a: ler os excertos em cache
dirs <- list.dirs(QD, recursive = FALSE, full.names = FALSE)
dirs <- dirs[grepl("^\\d{7}$", dirs)]
cat("municipios com cache de excertos:", length(dirs), "\n")
consultas <- if (file.exists(file.path(QD, "consultas.csv"))) fread(file.path(QD, "consultas.csv")) else data.table(n_requisicoes = 0L)
ler_mun <- function(tid) {
  fs <- list.files(file.path(QD, tid), pattern = "\\.json$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    x <- tryCatch(fromJSON(f, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(x)) return(NULL)
    meta <- data.table(id_municipio_ibge = x$id_municipio_ibge, termo = x$termo, total_gazettes = x$total_gazettes,
                       n_coletadas = x$n_coletadas, truncado = isTRUE(x$truncado))
    if (length(x$gazettes) == 0) return(cbind(meta, data_diario = NA_character_, url_diario = NA_character_, trecho = NA_character_))
    g <- rbindlist(lapply(x$gazettes, function(z) {
      ex <- unlist(z$excerpts); if (length(ex) == 0) ex <- NA_character_
      data.table(data_diario = z$date, url_diario = z$url, trecho = ex)
    }))
    cbind(meta, g)
  }), fill = TRUE)
}
raw <- rbindlist(lapply(dirs, ler_mun), fill = TRUE)
stopifnot(nrow(raw) > 0)
meta <- unique(raw[, .(id_municipio_ibge, termo, total_gazettes, n_coletadas, truncado)])
registrar_numero("qd_n_municipios_com_excertos_em_cache", length(dirs), script = script)
registrar_numero("qd_n_municipios_com_todos_termos", sum(file.exists(file.path(QD, dirs, "_ok"))), script = script)
registrar_numero("qd_n_consultas_termo_municipio", nrow(meta), script = script)
registrar_numero("qd_n_requisicoes_api", max(consultas$n_requisicoes, na.rm = TRUE), script = script)
registrar_numero("qd_n_consultas_truncadas_em_300_edicoes", meta[truncado == TRUE, .N], script = script)
ex <- raw[!is.na(trecho)]
ex[, trecho := gsub("\\s+", " ", trecho)]
ex <- unique(ex, by = c("id_municipio_ibge", "data_diario", "url_diario", "trecho"))
registrar_numero("qd_n_excertos_brutos", nrow(ex), script = script)
ex[, tn := norm(gsub("<<|>>", "", trecho))]

## ---- PASSO 3b: evento inferido a partir do texto do excerto (padroes explicitos; prioridade do definitivo ao temporario)
CARGO_RX <- "(PREFEIT|VICE PREFEIT|VEREADOR|VEREADORA|EDIL|MANDATO ELETIVO|MANDATO DE (PREFEITO|VEREADOR|VICE))"
ex[, evento_inferido := fcase(
  grepl("FALECIMENTO D[OA] (PREFEIT|VICE PREFEIT|VEREADOR)", tn) | grepl("(PREFEIT|VEREADOR)[A ]*[^.]{0,40}(FALECEU|FALECID)", tn), "falecimento",
  (grepl("CASSACAO D[OA] (MANDATO|PREFEIT|VEREADOR|DIPLOMA)", tn) | grepl("PERDA D[OA] MANDATO", tn) | grepl("MANDATO[^.]{0,30}CASSAD", tn)) &
    grepl("PREFEIT|VEREADOR|EDIL", tn) & !grepl("CONSELHEIR|CONSELHO TUTELAR|CANDIDATO", tn), "cassacao",
  grepl("RENUNCIA (AO|DO|DE) (MANDATO|CARGO)[^.]{0,60}(PREFEIT|VEREADOR|VICE)", tn) | grepl("RENUNCIA D[OA] (PREFEIT|VICE PREFEIT|VEREADOR)", tn) |
    grepl("(PREFEIT|VEREADOR)[A ]*[^.]{0,60}RENUNCI", tn) | grepl("RENUNCI[^.]{0,60}(PREFEIT|VEREADOR|VICE PREFEIT)", tn), "renuncia",
  grepl("AFASTAMENTO D[OA] (PREFEIT|VICE PREFEIT|VEREADOR)", tn) | grepl("(PREFEIT|VEREADOR)[A ]*[^.]{0,40}AFASTAD", tn), "afastamento",
  grepl("LICENCA D[OA] (PREFEIT|VICE PREFEIT|VEREADOR)", tn) | grepl("(PREFEIT|VEREADOR)[A ]*[^.]{0,60}(LICENCA|LICENCIAD)", tn), "licenca",
  grepl("NAO TOMOU POSSE|NAO TOMAR POSSE|NAO COMPARECEU A POSSE", tn) & grepl(CARGO_RX, tn), "nao_tomou_posse",
  grepl("SUPLENTE DE VEREADOR|CONVOCACAO D[OA] SUPLENTE|CONVOCA[^.]{0,30}SUPLENTE|SUPLENTE[^.]{0,40}(ASSUM|POSSE|CONVOCAD|EMPOSSAD)", tn) &
    grepl("VEREADOR|EDIL|CAMARA MUNICIPAL|LEGISLATIV", tn) & !grepl("CONSELHEIR|CONSELHO TUTELAR|AGENTE CULTURAL", tn), "suplente_efetivado",
  grepl("POSSE D[OA] VICE PREFEIT|ASSUME A PREFEITURA|ASSUMIR A PREFEITURA|ASSUMIU A PREFEITURA|VICE PREFEIT[OA][^.]{0,40}(ASSUM|POSSE)", tn), "afastamento",
  default = NA_character_)]
# 3b-bis (29/08/2026): a renuncia de edital (contrato, vaga de concurso, estagio) e a renuncia de receita nao sao saida de
# mandato — 10 das 20 linhas 'media' da amostra de precisao vinham de editais de convocacao assinados pelo prefeito.
ex[evento_inferido == "renuncia" &
     grepl("RENUNCIA (TACITA|AUTOMATICA|EXPRESSA|AO CONTRATO|DO CONTRATO|DE RECEITA|DE RECEITAS|FISCAL|A VAGA|DA VAGA|AO ESTAGIO|AO CARGO PUBLICO|AO CONCURSO|AO DIREITO)", tn) &
     !grepl("RENUNCIA (AO|DO) MANDATO|RENUNCIA D[OA] (VEREADOR|VEREADORA|PREFEIT|VICE)|RENUNCIA AO (CARGO|EXERCICIO) DE (VEREADOR|PREFEIT)", tn),
   evento_inferido := NA_character_]
ex[, evento_por_padrao_explicito := !is.na(evento_inferido) & !(evento_inferido == "afastamento" & !grepl("AFASTAMENTO D[OA] (PREFEIT|VICE PREFEIT|VEREADOR)|(PREFEIT|VEREADOR)[A ]*[^.]{0,40}AFASTAD", tn))]
registrar_numero("qd_n_excertos_com_evento_inferido", ex[!is.na(evento_inferido), .N], script = script)
print(ex[, .N, by = evento_inferido][order(-N)])

## ---- PASSO 3c: pareamento de nomes do trecho com ocupantes do BOCEL no municipio e janela
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
occ <- merge(mand[coberta == TRUE, .(id_mandato, id_pessoa, id_municipio_ibge, sg_ue = unidade_posicao, sg_uf, cd_cargo, cargo, ano_eleicao,
                                    mandato_inicio, mandato_fim, forma_saida)], pess[, .(id_pessoa, nome)], by = "id_pessoa")
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_(200[4-9]|201[0-9]|202[0-4])", full.names = TRUE), function(f) {
  x <- as.data.table(arrow::read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO %in% c("11", "12", "13")]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_", CD_CARGO, "_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
occ <- merge(occ, unique(cand[, .(id_mandato, nome_urna = NM_URNA_CANDIDATO)], by = "id_mandato"), by = "id_mandato", all.x = TRUE)
occ[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna))]
occ[, `:=`(ini = as.IDate(mandato_inicio), fim = as.IDate(mandato_fim))]
# janela: do inicio do mandato ate 60 dias apos o fim (o diario publica o ato depois do fato)
occ[, fim_janela := fim + 60L]
# nome de urna so serve quando e unico entre os ocupantes do municipio (todos os cargos, todas as eleicoes cobertas) e tem >= 2 tokens de >= 3 letras
occ[, urna_tok := sapply(strsplit(urna_norm, " "), function(t) sum(nchar(t) >= 3))]
occ[, urna_unica := .N == 1, by = .(id_municipio_ibge, urna_norm)]
occ[, urna_util := !is.na(urna_norm) & urna_tok >= 2 & urna_unica & urna_norm != nome_norm]
# tokens do nome civil: sobrenomes (>= 2 tokens de >= 4 letras, sem stopwords) — usado so quando o conjunto identifica pessoa unica no municipio-janela
STOP <- c("DE", "DA", "DO", "DAS", "DOS", "E", "JUNIOR", "FILHO", "NETO", "SOBRINHO")

ex[, rid := .I]
ex[, data_d := as.IDate(data_diario)]
# candidatos: ocupantes do mesmo municipio com janela cobrindo a data do diario
cj <- merge(ex[, .(rid, id_municipio_ibge, data_d, tn, trecho_raw = gsub("<<|>>", "", trecho))], occ[, .(id_mandato, id_pessoa, id_municipio_ibge, sg_ue, sg_uf, cd_cargo, ano_eleicao, nome_norm, urna_norm, urna_util, ini, fim_janela)],
            by = "id_municipio_ibge", allow.cartesian = TRUE)
cj <- cj[data_d >= ini & data_d <= fim_janela]
cat("pares excerto x ocupante na janela:", nrow(cj), "\n")
cj[, nome_completo_no_trecho := nchar(nome_norm) >= 10 & stri_detect_fixed(tn, paste0(" ", nome_norm, " ")) |
     stri_startswith_fixed(tn, paste0(nome_norm, " ")) | stri_endswith_fixed(tn, paste0(" ", nome_norm))]
cj[, urna_no_trecho := urna_util & (stri_detect_fixed(tn, paste0(" ", urna_norm, " ")) | stri_startswith_fixed(tn, paste0(urna_norm, " ")) | stri_endswith_fixed(tn, paste0(" ", urna_norm)))]
# tokens: primeiro nome + ultimo sobrenome (>= 4 letras) presentes, e pessoa unica entre os candidatos do excerto
tok_ok <- function(nm, t) {
  a <- strsplit(nm, " ")[[1]]; a <- a[!(a %in% STOP)]
  if (length(a) < 2) return(FALSE)
  first <- a[1]; last <- a[length(a)]; tp <- paste0(" ", t, " ")
  nchar(first) >= 3 && nchar(last) >= 4 && stri_detect_fixed(tp, paste0(" ", first, " ")) && stri_detect_fixed(tp, paste0(" ", last, " "))
}
cj[, tokens_no_trecho := FALSE]
idx <- which(cj$nome_completo_no_trecho == FALSE & cj$urna_no_trecho == FALSE)
if (length(idx)) set(cj, i = idx, j = "tokens_no_trecho", value = unname(mapply(tok_ok, cj$nome_norm[idx], cj$tn[idx])))
cat("pares testados por tokens:", length(idx), "; com tokens no trecho:", sum(cj$tokens_no_trecho), "\n")
# excerto com tokens vale so quando um unico ocupante casa por tokens
cj[, n_tok_hits := sum(tokens_no_trecho), by = rid]
cj[, tokens_unico := tokens_no_trecho & n_tok_hits == 1]
hits <- cj[nome_completo_no_trecho | urna_no_trecho | tokens_unico]
hits[, assinatura := stri_detect_regex(tn, paste0("\\b", fifelse(nome_completo_no_trecho, nome_norm, urna_norm),
  " (PRESIDENTE|PREFEIT[OA] MUNICIPAL|PREFEIT[OA] EM EXERCICIO|VEREADOR PRESIDENTE|PRESIDENTE DA CAMARA|SECRETARI[OA] MUNICIPAL|PRIMEIR[OA] SECRETARI)"))]
# posicao do nome no trecho normalizado e distancia ao termo de saida mais proximo
hits[, nome_usado := fifelse(nome_completo_no_trecho, nome_norm, fifelse(urna_no_trecho, urna_norm, NA_character_))]
hits[, pos_nome := fifelse(!is.na(nome_usado), stri_locate_first_fixed(tn, nome_usado)[, 1], NA_integer_)]
KW <- "RENUNCI|CASSA|PERDA D[OA] MANDATO|LICENC|AFAST|FALEC|SUPLENTE|NAO TOMOU POSSE|NAO TOMAR POSSE|ASSUME A PREFEITURA|ASSUMIU A PREFEITURA"
hits[, dist_termo := {
  loc <- stri_locate_all_regex(tn, KW)
  mapply(function(l, p) if (is.na(p) || all(is.na(l[, 1]))) NA_integer_ else as.integer(min(abs(l[, 1] - p))), loc, pos_nome)
}]
# papel do nome no trecho: suplente convocado ("SUPLENTE ... SR. NOME"), orador de ata ("NOME:" / "NOME - PARTIDO:")
hits[, papel_suplente := !is.na(nome_usado) & (stri_detect_regex(tn, paste0("SUPLENTE[^.]{0,80}\\b", nome_usado, "\\b")) |
                                              stri_detect_regex(tn, paste0("\\b", nome_usado, "\\b[^.]{0,40}(SUPLENTE|PARA (ASSUMIR|TOMAR POSSE))")))]
hits[, papel_orador := !is.na(nome_usado) & stri_detect_regex(stri_trans_general(toupper(trecho_raw), "Latin-ASCII"), paste0("\\b", nome_usado, "\\s*([-–][^:]{0,30})?:"))]
## ---- PASSO 3c-bis: ancora nome x termo (regra apertada apos medida de precisao de 0,40 na amostra de 30 'alta' de 29/08/2026)
# confianca alta passa a exigir que o termo de saida e o nome completo do ocupante estejam ligados por um cargo na mesma oracao:
#   p1: TERMO ... (do|da|pelo|pela|a|ao) [honorifico] CARGO [honorifico] NOME       ("renuncia do vereador Fulano de Tal")
#   p2: CARGO [honorifico] NOME ... TERMO                                             ("vereador Fulano de Tal, falecido em")
# e que o evento seja o do termo ancorado (nao o do excerto inteiro). Exclui: ex-cargo; assinatura ("NOME PREFEITO"); renuncia a
# comissao, mesa, funcao, conselho, receita, contrato, vaga ou estagio; termo dirigido a um destinatario ("renuncia ao Exmo. Sr. Prefeito").
norm_p <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z.;:]", " ", x); gsub(" +", " ", trimws(x)) }
HON <- "(?:(?:SR|SRA|SENHOR|SENHORA|EXMO|EXMA|EXCELENTISSIMO|EXCELENTISSIMA|ILMO|ILMA|ILUSTRISSIMO|DR|DRA|VER|VERª)\\.? ?)*"
CARGO_ANC <- "(?<!EX )(?<!EX)(?:VEREADOR|VEREADORA|PREFEITO|PREFEITA|VICE PREFEITO|VICE PREFEITA|EDIL)(?: MUNICIPAL)?(?: DE [A-Z]{3,20}(?: [A-Z]{2,20}){0,3})?"
EV_ANC <- "(RENUNCIA|RENUNCIOU|LICENCA|LICENCIOU|LICENCIADO|LICENCIADA|AFASTAMENTO|AFASTADO|AFASTADA|CASSACAO|CASSADO|CASSADA|PERDA DO MANDATO|FALECIMENTO|FALECEU|FALECIDO|FALECIDA|OBITO)"
hits[, tp := norm_p(trecho_raw)]
hits[, nome_p := fifelse(nome_completo_no_trecho, nome_norm, NA_character_)]
# corroboracao de que o ato atinge o mandato (e nao um cargo da Mesa, uma comissao ou um contrato administrativo)
MANDATO_OK <- paste0(
  "RENUNCI[A-Z]* (AO|DO|A) (MANDATO|EXERCICIO DO MANDATO|CARGO ELETIVO|CARGO DE VEREADOR|CARGO DE VEREADORA|CARGO DE PREFEIT)",
  "|PEDIDO DE RENUNCIA D[OA] (CARGO DE )?(VEREADOR|VEREADORA|PREFEIT)",
  "|(EXTINCAO|EXTINTO|EXTINGUIR) [^.;:]{0,20}MANDATO",
  "|(VAGO|VAGA|VAGAR)[^.;:]{0,40}(MANDATO|CADEIRA|CARGO DE VEREADOR|CARGO DE VEREADORA|CARGO DE PREFEIT)",
  "|CONVOC[A-Z]*[^.;:]{0,80}SUPLENTE|SUPLENTE[^.;:]{0,100}(POSSE|ASSUM|SUBSTITU|CONVOCAD|TITULARIDADE|EXERCICIO DO MANDATO)",
  "|(TOMOU POSSE|TOMAR POSSE|TERMO DE POSSE|EMPOSSAD|DEU POSSE)[^.;:]{0,80}(VEREADOR|VEREADORA|PREFEIT|MANDATO|SUPLENTE|TITULARIDADE)",
  "|ASSUM[A-Z]*[^.;:]{0,40}(A PREFEITURA|O MANDATO|O CARGO DE PREFEIT|A TITULARIDADE)",
  "|LICENC[A-Z]*[^.;:]{0,80}(PELO PERIODO|NO PERIODO|A PARTIR DE|POR \\d+ |PARA TRATAR|PARA TRATO|PARTICULAR|MEDICA|SAUDE|MATERNIDADE|SEM VENCIMENTO|SEM REMUNERACAO|PARA EXERCER|PARA ASSUMIR)",
  "|(VEREADOR|VEREADORA|PREFEIT[OA])[A-Z ]{0,20}LICENCIAD",
  "|AFASTAMENTO D[OA] (CARGO DE )?(PREFEIT|VEREADOR|VICE)")
CASSA_DECIDIDA <- paste0(
  "(DECRETA|DECRETAD|DECRETOU|DECLARA|DECLARAD|DECLAROU|DECLARAR|DETERMIN[A-Z]*|RESOLVE[A-Z]*|PROMULG[A-Z]*|APROVAD[A-Z]*|CONFIRMAD[A-Z]*)",
  "[^.;:]{0,80}(PERDA D[OE] MANDATO|CASSACAO|MANDATO CASSAD)",
  "|(PERDA D[OE] MANDATO|CASSACAO)[^.;:]{0,60}(DECRETAD|DECLARAD|CONFIRMAD|APROVAD|MANTID)",
  "|MANDATO[A-Z ]{0,20}CASSAD|CASSAD[OA] O MANDATO",
  "|DECID[A-Z]*[^.;:]{0,40}(PELA CASSACAO|PELA PERDA)",
  "|PROCEDENCIA DA (ACUSACAO|DENUNCIA)|JULG[A-Z]*[^.;:]{0,30}PROCEDENTE")
CASSA_SO_PEDIDA <- paste0(
  "PEDE A CASSACAO|PEDIDO DE CASSACAO|SOLICIT[A-Z]*[^.;:]{0,20}A CASSACAO|REQUER[A-Z]*[^.;:]{0,20}CASSACAO",
  "|DENUNCIA[^.;:]{0,80}CASSACAO|RECEBIMENTO DE DENUNCIA|ABERTURA DE (PROCESSO|COMISSAO)|INSTAURACAO",
  "|EM JULGAMENTO DO PROCESSO|JULGAMENTO DO PROCESSO DE CASSACAO|VOTAR[A-Z]* (CONTRA|A FAVOR)|IRIA SOLICITAR",
  "|PROCESSO DE CASSACAO D[OA] PREFEITO MUNICIPAL, E CONTEM")
anc <- function(tp, nm) {
  if (is.na(nm)) return(NA_character_)
  p1 <- paste0(EV_ANC, "[^.;:]{0,60}?\\b(?:DO|DA|PELO|PELA|DE|A|AO) ", HON, CARGO_ANC, ",? ?", HON, nm, "\\b")
  p2 <- paste0("\\b", CARGO_ANC, ",? ?", HON, nm, "\\b[^.;:]{0,60}?\\b", EV_ANC)
  m <- stri_match_first_regex(tp, p1); if (is.na(m[1, 1])) m <- stri_match_first_regex(tp, p2)
  if (is.na(m[1, 1])) return(NA_character_)
  kw <- m[1, 2]
  # contexto do casamento: o trecho casado + 60 caracteres depois (segunda rodada de leitura, 29/08/2026: alvara, cargo da mesa,
  # comissao, projeto de lei de autoria do vereador, cassacao anulada)
  pos <- stri_locate_first_regex(tp, if (!is.na(stri_match_first_regex(tp, p1)[1, 1])) p1 else p2)
  span <- substr(tp, pos[1, 1], min(nchar(tp), pos[1, 2] + 60L))
  if (grepl("PROJETO DE LEI|DISPOE SOBRE|DE AUTORIA|REQUERIMENTO N|INDICACAO N|MOCAO", span)) return(NA_character_)
  if (grepl("CASSACAO (DE|DO|DA|DOS|DAS) (ALVARA|LICENCA|INSCRICAO|REGISTRO|CNH|HABILITACAO|PERMISSAO|CONCESSAO|APOSENTADORIA|BENEFICIO|CREDENCIAMENTO)", span)) return(NA_character_)
  if (grepl("(RENUNCIA|LICENCA|AFASTAMENTO) (AO|DO|DA|A) (CARGO DE |FUNCAO DE )?(VICE |PRIMEIR[OA] |SEGUND[OA] |1 |2 )?(PRESIDENT|SECRETARI|VICE PRESIDENT|LIDER|MEMBRO|RELATOR|TESOUREIR|MESA|COMISSAO|CPI|CONSELH)", span)) return(NA_character_)
  ctx_termo <- stri_extract_first_regex(tp, paste0(EV_ANC, "[^.;:]{0,25}"))
  if (!is.na(ctx_termo) && grepl("RENUNCIA (DE RECEITA|DE RECEITAS|FISCAL|AO CONTRATO|TACITA|AUTOMATICA|A VAGA|AO ESTAGIO|DA VAGA|AO (EXCELENTISSIMO|EXMO|SENHOR|SR|ILUSTRISSIMO|ILMO))", ctx_termo)) return(NA_character_)
  ctx_nome <- stri_extract_first_regex(tp, paste0("\\b", nm, "\\b[^.;:]{0,50}"))
  if (!is.na(ctx_nome) && grepl(paste0("^", nm, " ?,? ?(PREFEIT[OA]|VEREADOR[A]?|PRESIDENTE|SECRETARI[OA]|SEU PRESIDENTE|PROMULG)\\b"), ctx_nome)) return(NA_character_)
  if (!is.na(ctx_nome) && grepl("\\b(AO CARGO DE|DO CARGO DE|DA (COMISSAO|MESA|FUNCAO|PRESIDENCIA|LIDERANCA|VICE PRESIDENCIA|RELATORIA|REFERIDA COMISSAO|RESPECTIVA COMISSAO)|DAS COMISSOES|NAS COMISSOES|NA COMISSAO|A COMISSAO|DE MEMBRO|COMO MEMBRO|DA CPI|COMO RELATOR|DO CONSELHO|AO CONSELHO|PARA (A )?PRESIDENCIA|E A NOVA ELEICAO)\\b", ctx_nome)) return(NA_character_)
  if (grepl(paste0("GABINETE DO PREFEITO[^.;:]{0,60}\\b", nm), tp)) return(NA_character_)
  # quarta rodada de leitura (30 excertos 'alta' fora da amostra de calibragem, 29/08/2026, precisao 28/30 = 0,933): os dois
  # erros restantes eram atos que SUSPENDEM os efeitos da cassacao ("sao suspensos os efeitos do Ato n. 04, que declarou a
  # perda do mandato"; "decreta a suspensao dos efeitos do Decreto Legislativo n. 07/2017, que teve por objeto a cassacao").
  if (grepl("^(CASSA|PERDA)", kw) && grepl("ANULAD|RETOMA SUAS|REINTEGRA|IMPROCEDEN|REJEITAD|ARQUIVAD|ABSOLV|NAO DECRET|LIMINAR|REVOGAD[OA] O (ATO|DECRETO|RESOLUCAO)|SUSPENS[A-Z]*( [A-Z]{1,12}){0,4} (EFEITOS|ATO|DECRETO|DECRETO LEGISLATIVO|RESOLUCAO|CASSACAO)", tp)) return(NA_character_)
  # terceira rodada de leitura (30 excertos 'alta', 29/08/2026, precisao 23/30 = 0,767): os dois erros que restavam eram
  # (a) renuncia ou licenca de cargo interno (Mesa, comissao, CPI, lideranca) lida como saida do mandato e
  # (b) cassacao apenas pedida, denunciada ou em processamento, sem decisao. A ancora passa a exigir corroboracao explicita
  # de consequencia sobre o mandato no proprio excerto.
  if (grepl("^(RENUNC|LICENC|AFAST)", kw) && !grepl(MANDATO_OK, tp)) return(NA_character_)
  if (grepl("^(CASSA|PERDA)", kw) && (!grepl(CASSA_DECIDIDA, tp) || grepl(CASSA_SO_PEDIDA, tp))) return(NA_character_)
  fcase(grepl("^RENUNC", kw), "renuncia", grepl("^LICENC", kw), "licenca", grepl("^AFAST", kw), "afastamento",
        grepl("^CASSA|^PERDA", kw), "cassacao", grepl("^FALEC|^OBITO", kw), "falecimento", default = NA_character_)
}
hits[, evento_ancorado := unname(mapply(anc, tp, nome_p))]
cat("pareamentos por nome completo com ancora nome x termo:", sum(!is.na(hits$evento_ancorado)), "de", sum(hits$nome_completo_no_trecho), "\n")
hits[, metodo_pareamento := fcase(nome_completo_no_trecho, "nome_completo_no_trecho", urna_no_trecho, "nome_de_urna_unico_no_trecho", default = "tokens_primeiro_e_ultimo_nome_unico")]
hits[, rank := fcase(metodo_pareamento == "nome_completo_no_trecho", 1L, metodo_pareamento == "nome_de_urna_unico_no_trecho", 2L, default = 3L)]
# um excerto pode nomear mais de uma pessoa (ex.: prefeito licenciado e vice que assume): mantem todos os pareamentos por nome completo/urna,
# mas uma pessoa so entra uma vez por excerto (mandato mais recente cuja janela cobre a data)
setorder(hits, rid, id_pessoa, rank, -ano_eleicao)
hits <- hits[!duplicated(hits[, .(rid, id_pessoa)])]
hits[, n_pessoas_no_trecho := .N, by = rid]

## ---- PASSO 3d: tabela de eventos
ev <- merge(ex[, .(rid, id_municipio_ibge, data_diario, termo, evento_inferido, evento_por_padrao_explicito, trecho, url_diario)],
            hits[, .(rid, id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato, cd_cargo, sg_ue, sg_uf, ano_eleicao, metodo_pareamento, assinatura, dist_termo, papel_suplente, papel_orador, evento_ancorado)], by = "rid", all.x = TRUE)
# quando ha ancora, o evento da linha e o do termo ancorado ao nome (o excerto pode conter outro evento, de terceiro)
ev[!is.na(evento_ancorado), `:=`(evento_inferido = evento_ancorado, evento_por_padrao_explicito = TRUE)]
ev <- merge(ev, mun[, .(id_municipio_ibge, sg_ue_mun = sg_ue, sg_uf_mun = sg_uf)], by = "id_municipio_ibge", all.x = TRUE)
ev[is.na(sg_ue), `:=`(sg_ue = sg_ue_mun, sg_uf = sg_uf_mun)]
ev[, confianca := fcase(
  !is.na(evento_inferido) & metodo_pareamento %in% "nome_completo_no_trecho" & !is.na(evento_ancorado), "alta",
  !is.na(evento_inferido) & !is.na(metodo_pareamento), "media",
  !is.na(evento_inferido), "baixa",
  default = NA_character_)]
ev <- ev[!is.na(evento_inferido) | !is.na(id_mandato_bocel)]
# o evento so se atribui ao mandato quando o cargo do ocupante e compativel com o padrao do trecho
# (um excerto sobre "licenca do prefeito" que nomeia o vice que assume nao e uma licenca do vice)
ev[, cargo_compativel := fcase(
  is.na(id_mandato_bocel), NA,
  cd_cargo == "11", grepl("PREFEIT", norm(trecho)) | !grepl("VEREADOR|VICE", norm(trecho)),
  cd_cargo == "12", grepl("VICE", norm(trecho)),
  cd_cargo == "13", grepl("VEREADOR|EDIL|SUPLENTE|CAMARA", norm(trecho)),
  default = FALSE)]
ev[!is.na(id_mandato_bocel) & cargo_compativel == FALSE & confianca == "alta", confianca := "media"]
# nome que aparece como assinatura do ato (ex.: "FULANO Presidente") nao e o sujeito do evento: confianca cai para media
ev[, motivo_rebaixamento := fcase(
  is.na(id_mandato_bocel), NA_character_,
  cargo_compativel == FALSE, "cargo_incompativel_com_trecho",
  metodo_pareamento == "nome_completo_no_trecho" & is.na(evento_ancorado) & !is.na(evento_inferido), "sem_ancora_nome_cargo_termo",
  assinatura %in% TRUE, "nome_e_assinatura_do_ato",
  papel_orador %in% TRUE, "nome_e_orador_da_ata",
  papel_suplente %in% TRUE, "nome_e_suplente_convocado",
  evento_inferido == "cassacao" & grepl("IMPROCEDEN|REJEI|ARQUIV|NAO DECRET|ABSOLV|ANULAD|RETOMA SUAS|REINTEGRA|LIMINAR", norm(trecho)), "cassacao_negada_no_trecho",
  !is.na(dist_termo) & dist_termo > 120L, "nome_a_mais_de_120_caracteres_do_termo",
  default = NA_character_)]
ev[!is.na(motivo_rebaixamento) & confianca == "alta", confianca := "media"]
tabr <- ev[!is.na(motivo_rebaixamento), .N, by = motivo_rebaixamento][order(-N)]
print(tabr)
for (i in seq_len(nrow(tabr))) registrar_numero(paste0("qd_n_pareamentos_rebaixados_", tabr$motivo_rebaixamento[i]), tabr$N[i], script = script)
ev[, evento_inferido := fifelse(is.na(evento_inferido), "NA", evento_inferido)]
out <- ev[, .(id_municipio_ibge, sg_ue, sg_uf, data_diario, termo, evento_inferido, trecho, id_pessoa_bocel, id_mandato_bocel, cd_cargo_bocel = cd_cargo,
              ano_eleicao_bocel = ano_eleicao, metodo_pareamento, confianca, motivo_rebaixamento, evento_ancorado, dist_nome_termo = dist_termo, url_diario)]
registrar_numero("qd_n_pareamentos_com_ancora_nome_cargo_termo", out[!is.na(evento_ancorado), .N], script = script)
setorder(out, sg_uf, id_municipio_ibge, data_diario)
fwrite(out, "data/diarios_eventos.csv", na = "NA", quote = TRUE)
registrar_numero("qd_n_eventos_linhas", nrow(out), script = script)
registrar_numero("qd_n_eventos_com_evento_inferido", out[evento_inferido != "NA", .N], script = script)
registrar_numero("qd_n_eventos_pareados_bocel", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("qd_n_mandatos_bocel_nomeados_em_excerto", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
tab <- out[evento_inferido != "NA", .N, by = .(evento_inferido, confianca)][order(evento_inferido, confianca)]
print(tab)
for (i in seq_len(nrow(tab))) registrar_numero(sprintf("qd_n_eventos_%s_%s", tab$evento_inferido[i], tab$confianca[i]), tab$N[i], script = script)
tabm <- out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)]
print(tabm)
for (i in seq_len(nrow(tabm))) registrar_numero(sprintf("qd_n_pareamentos_%s", tabm$metodo_pareamento[i]), tabm$N[i], script = script)
fwrite(tab, "output/verificacao/diarios_eventos_por_tipo_confianca.csv")

## ---- PASSO 3e: uma linha por mandato com o evento de confianca alta mais antigo
sa <- out[confianca == "alta" & !is.na(id_mandato_bocel) & evento_inferido %in% c("renuncia", "afastamento", "licenca", "cassacao", "falecimento", "suplente_efetivado", "nao_tomou_posse")]
setorder(sa, id_mandato_bocel, data_diario)
sa <- sa[!duplicated(id_mandato_bocel)]
saida <- sa[, .(id_mandato_bocel, id_pessoa_bocel, cd_cargo_bocel, sg_uf, sg_ue, id_municipio_ibge, ano_eleicao_bocel, forma_saida = evento_inferido,
                data_fim_inferida = data_diario, termo, metodo_pareamento, confianca, url = url_diario, trecho)]
saida <- merge(saida, mand[, .(id_mandato_bocel = id_mandato, forma_saida_bocel_atual = forma_saida, fonte_bocel_atual = fonte_forma_saida)], by = "id_mandato_bocel", all.x = TRUE)
fwrite(saida, "data/diarios_mandatos_saida.csv", na = "NA", quote = TRUE)
registrar_numero("qd_n_mandatos_com_saida_inferida_alta", nrow(saida), script = script)
registrar_numero("qd_n_mandatos_saida_inferida_novos_para_bocel", saida[forma_saida_bocel_atual == "nao_observado", .N], script = script)
registrar_numero("qd_n_mandatos_saida_inferida_concorda_bocel", saida[forma_saida_bocel_atual != "nao_observado" & forma_saida_bocel_atual == forma_saida, .N], script = script)
registrar_numero("qd_n_mandatos_saida_inferida_discorda_bocel", saida[forma_saida_bocel_atual != "nao_observado" & forma_saida_bocel_atual != forma_saida, .N], script = script)
for (f in unique(saida$forma_saida)) registrar_numero(paste0("qd_n_mandatos_saida_", f), saida[forma_saida == f, .N], script = script)
for (cc in unique(saida$cd_cargo_bocel)) registrar_numero(paste0("qd_n_mandatos_saida_cargo_", cc), saida[cd_cargo_bocel == cc, .N], script = script)

## ---- taxa por UF e ano (mandatos em cidades com cache de excertos)
base <- mand[id_municipio_ibge %in% dirs, .(n_mandatos = .N, n_sem_saida = sum(forma_saida == "nao_observado")), by = .(sg_uf, ano_eleicao)]
nom <- out[!is.na(id_mandato_bocel), .(n_mandatos_nomeados = uniqueN(id_mandato_bocel)), by = .(sg_uf, ano_eleicao = ano_eleicao_bocel)]
sai <- saida[, .(n_mandatos_saida_alta = .N), by = .(sg_uf, ano_eleicao = ano_eleicao_bocel)]
taxa <- merge(merge(base, nom, by = c("sg_uf", "ano_eleicao"), all.x = TRUE), sai, by = c("sg_uf", "ano_eleicao"), all.x = TRUE)
taxa[is.na(n_mandatos_nomeados), n_mandatos_nomeados := 0L]; taxa[is.na(n_mandatos_saida_alta), n_mandatos_saida_alta := 0L]
taxa[, `:=`(taxa_nomeados = round(n_mandatos_nomeados / n_mandatos, 4), taxa_saida_alta = round(n_mandatos_saida_alta / n_mandatos, 4))]
fwrite(taxa[order(sg_uf, ano_eleicao)], "data/diarios_taxa_uf_ano.csv", na = "NA")
print(taxa[, .(n_mandatos = sum(n_mandatos), nomeados = sum(n_mandatos_nomeados), saida_alta = sum(n_mandatos_saida_alta)), by = sg_uf][order(-n_mandatos)])
registrar_numero("qd_taxa_mandatos_nomeados_em_cidades_com_cache", round(sum(taxa$n_mandatos_nomeados) / sum(taxa$n_mandatos), 4), script = script)
registrar_numero("qd_taxa_saida_alta_em_cidades_com_cache", round(sum(taxa$n_mandatos_saida_alta) / sum(taxa$n_mandatos), 4), script = script)

gravar_relatorio_verificacao(alvo = "data/diarios_eventos.csv", script = script,
  passou = c("vocabulario fechado de evento_inferido", "uma linha por mandato em diarios_mandatos_saida", "todo id_mandato_bocel existe em mandatos.csv"),
  falhou = character(), fora_de_cobertura = c("pertinencia semantica do excerto (homonimo de servidor, ato de conselho)", "periodo do diario anterior a 2013 quase inexistente na base"))
stopifnot(all(out$evento_inferido %in% c("NA", "renuncia", "afastamento", "licenca", "cassacao", "falecimento", "suplente_efetivado", "nao_tomou_posse")))
stopifnot(!any(duplicated(saida$id_mandato_bocel)), all(saida$id_mandato_bocel %in% mand$id_mandato))
cat("21_diarios_oficiais: concluido —", nrow(out), "linhas de eventos,", nrow(saida), "mandatos com saida inferida (alta)\n")
sink()
