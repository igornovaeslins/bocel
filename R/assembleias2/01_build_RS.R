# 01_build_RS.R — camada de exercicio e forma de saida dos deputados estaduais do RS,
#   construida das fontes da propria Assembleia Legislativa do Rio Grande do Sul (ALRS).
#
# Fontes (coletadas por python/assembleias2/*_RS.py, cache em data_raw/assembleias2/RS/):
#   A. Memorial do Legislativo do RS — "Quadro de Legislaturas": um PDF por legislatura com a
#      bancada inicial, a composicao final, os suplentes e notas de rodape datadas sobre cada
#      troca de titular. Unica fonte da casa para as legislaturas 50a (1999-2003) e 51a (2003-2007).
#   B. Portal da Transparencia da ALRS — "Presencas em Plenario": painel mensal, 2006-01 em diante,
#      com uma linha por deputado no exercicio do mandato naquele mes (codProponente + nome).
#      Da a composicao mes a mes das legislaturas 52a a 56a.
#   C. Endpoint interno /listarDestaqueDeputados (porta 5000) — bancada da legislatura em curso,
#      com sigla partidaria.
#
# Saida: data/assembleias2/RS.csv (21 colunas, esquema de data/exercicio_assembleias.csv)
#        data_raw/assembleias2/RS/inventario.csv
#        output/verificacao/, output/numeros_assinatura.txt (prefixo asm2rs_)
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/01_build_RS.R
set.seed(20260829)
suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(stringi); library(arrow)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
raw  <- file.path(root, "data_raw", "assembleias2", "RS")
outd <- file.path(root, "data", "assembleias2")
verd <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "01_build_RS.R")
logf   <- file.path(root, "logs", "asm2_RS_build.log")
sink(logf, split = TRUE)
cat("01_build_RS.R —", format(Sys.time()), "\n")

HOJE  <- as.IDate(Sys.Date())
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
reg <- function(k, v) registrar_numero(paste0("asm2rs_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

# normalizacao de nome: mesma regra de R/13_exercicio_assembleias.R (comparabilidade entre UFs)
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
fim_do_mes <- function(ano, mes) {
  ini <- as.Date(sprintf("%d-%02d-01", as.integer(ano), as.integer(mes)))
  prox <- as.Date(sprintf("%d-%02d-01", as.integer(ano) + (as.integer(mes) == 12L),
                          ifelse(as.integer(mes) == 12L, 1L, as.integer(mes) + 1L)))
  as.IDate(prox - 1)
}

## ------------------------------------------------------------------ 1. legislaturas da ALRS
# A instalacao da legislatura no RS e em 31 de janeiro do ano seguinte a eleicao; as janelas das
# legislaturas 50a a 54a estao impressas nos proprios PDFs do Memorial (ver data_raw/.../memorial).
LEG <- data.table(
  legislatura = as.character(50:56),
  ano_eleicao = seq(1998L, 2022L, 4L),
  leg_inicio  = as.IDate(sprintf("%d-01-31", seq(1999L, 2023L, 4L))),
  leg_fim     = as.IDate(sprintf("%d-01-31", seq(2003L, 2027L, 4L))))
LEG[, em_curso := leg_fim > HOJE]
print(LEG)

## ------------------------------------------------------------------ 2. painel mensal de presencas
arqs <- list.files(file.path(raw, "presencas"), pattern = "^presencas_\\d{4}_\\d{2}\\.json$", full.names = TRUE)
pn <- rbindlist(lapply(arqs, function(f) {
  d <- fromJSON(f, simplifyVector = TRUE)
  L <- d$linhas
  if (is.null(L) || length(L) == 0) return(NULL)
  L <- as.data.table(L)
  if (!nrow(L)) return(NULL)
  L[, .(ano = d$ano, mes = d$mes, url = d$url,
        id_fonte = as.character(codProponente), nome = trimws(nomeProponente))]
}), fill = TRUE)
pn <- unique(pn, by = c("ano", "mes", "id_fonte"))
pn[, ref := as.IDate(sprintf("%d-%02d-15", ano, mes))]
# janeiro do ano de posse mistura a legislatura que termina (ate 31/01) com a que se instala:
# o painel e mensal e nao separa as duas. Esses meses saem do painel; a legislatura que termina
# fica com dezembro como ultimo mes observado, e a data de fim continua sendo a da instalacao seguinte.
ANOS_POSSE <- seq(2003L, 2023L, 4L)
n_jan_posse <- pn[mes == 1L & ano %in% ANOS_POSSE, .N]
pn <- pn[!(mes == 1L & ano %in% ANOS_POSSE)]
cat("linhas descartadas (janeiro de ano de posse):", n_jan_posse, "\n")
pn[LEG, on = .(ref >= leg_inicio, ref < leg_fim), `:=`(legislatura = i.legislatura, ano_eleicao = i.ano_eleicao)]
cat("painel: linhas", nrow(pn), "| meses com dado", uniqueN(pn[, .(ano, mes)]),
    "| fora de legislatura", pn[is.na(legislatura), .N], "\n")
pn <- pn[!is.na(legislatura)]
print(pn[, .(meses = uniqueN(paste(ano, mes)), pessoas = uniqueN(id_fonte)), by = .(legislatura, ano_eleicao)][order(ano_eleicao)])

# o painel so cobre 2006 em diante: a 51a legislatura (2003-2007) fica coberta apenas no ultimo ano
cob_leg <- pn[, .(n_meses = uniqueN(paste(ano, mes)), primeiro = min(ref), ultimo = max(ref)), by = legislatura]
cob_leg <- merge(cob_leg, LEG, by = "legislatura")
cob_leg[, cobre_inicio := primeiro < (leg_inicio + 120L)]
LEGS_PAINEL <- cob_leg[cobre_inicio == TRUE, legislatura]
cat("legislaturas com painel desde a posse:", paste(LEGS_PAINEL, collapse = " "), "\n")

pn_ok <- pn[legislatura %in% LEGS_PAINEL]
mes_leg <- pn_ok[, .(mes_min = min(ref), mes_max = max(ref)), by = legislatura]
pes <- pn_ok[, .(nome = nome[which.max(ref)], n_meses = .N,
                 ref_ini = min(ref), ref_fim = max(ref),
                 url = url[1]), by = .(legislatura, ano_eleicao, id_fonte)]
pes <- merge(pes, mes_leg, by = "legislatura")
pes <- merge(pes, LEG[, .(legislatura, leg_inicio, leg_fim, em_curso)], by = "legislatura")
pes[, condicao := fifelse(ref_ini == mes_min, "titular", "suplente")]
pes[, saiu_antes := ref_fim < mes_max]
pes[, data_inicio_exercicio := as.character(fifelse(condicao == "titular", leg_inicio,
                                                    as.IDate(paste0(format(ref_ini, "%Y-%m"), "-01"))))]
pes[, data_fim_exercicio := as.character(fcase(
  saiu_antes == TRUE, fim_do_mes(as.integer(format(ref_fim, "%Y")), as.integer(format(ref_fim, "%m"))),
  em_curso == TRUE,   as.IDate(NA),
  default = leg_fim))]
pes[, fonte := "alrs_presencas_transparencia"]

## ------------------------------------------------------------------ 3. Memorial: legislaturas 50a e 51a
mem <- fread(file.path(raw, "memorial", "composicao_memorial.csv"), encoding = "UTF-8",
             colClasses = list(character = "legislatura"))
notas <- fread(file.path(raw, "memorial", "notas_memorial.csv"), encoding = "UTF-8",
               colClasses = list(character = c("legislatura", "numero")))
LEGS_MEM <- setdiff(LEG$legislatura, LEGS_PAINEL)
cat("legislaturas cobertas pelo Memorial:", paste(LEGS_MEM, collapse = " "), "\n")
mem <- mem[legislatura %in% LEGS_MEM]
mem[, nome_normalizado := norm_nome(nome)]
notas[, nome_normalizado := norm_nome(nome)]

ini <- mem[coluna == "inicial", .(legislatura, partido, condicao, nome, nome_normalizado, votos_fonte = as.character(votos))]
ini <- unique(ini, by = c("legislatura", "nome_normalizado"))
fin <- unique(mem[coluna == "final", .(legislatura, nome_normalizado, no_final = TRUE)])
# nota do Memorial atribuida ao deputado pelo nome normalizado, dentro da legislatura
nt <- notas[nome_normalizado != "", .(nota = paste(unique(texto), collapse = " | ")),
            by = .(legislatura, nome_normalizado)]

mm <- merge(ini, fin, by = c("legislatura", "nome_normalizado"), all = TRUE)
mm <- merge(mm, nt, by = c("legislatura", "nome_normalizado"), all.x = TRUE)
mm[is.na(no_final), no_final := FALSE]
mm[is.na(condicao), condicao := "suplente"]     # so aparece na composicao final: entrou depois
mm[is.na(nome), nome := stri_trans_totitle(nome_normalizado)]
# suplente que nunca serviu (lista de suplencia da coligacao, sem passagem pela composicao final
# nem mencao em nota) nao entra: a fonte nao o registra em exercicio
mm <- mm[!(condicao == "suplente" & no_final == FALSE & is.na(nota))]
mm <- merge(mm, LEG[, .(legislatura, ano_eleicao, leg_inicio, leg_fim, em_curso)], by = "legislatura")

## ------------------------------------------------------------------ 4. vocabulario de forma de saida
# classifica o texto da nota do Memorial; NA quando a nota nao descreve saida do mandato
classifica_nota <- function(x) {
  d <- toupper(stri_trans_general(x, "Latin-ASCII"))
  fcase(
    is.na(d), NA_character_,
    grepl("FALECEU|FALECIMENTO|MORTE|IN MEMORIAM", d), "falecimento",
    grepl("CASSA|PERDA DE MANDATO", d), "cassacao",
    grepl("RENUNCI", d), "renuncia",
    grepl("LICENCIOU-SE|LICENCIADO|LICENCA|LICENCIA-SE", d), "licenca",
    grepl("AFASTOU-SE|AFASTA-SE|AFASTAMENTO|ASSUME A SECRETARIA|ASSUMIU A SECRETARIA|PARA OCUPAR O CARGO|ASSUME COMO SECRETARI|ASSUMIU COMO SECRETARI|ASSUME A CHEFIA|ASSUMIU A CHEFIA|ASSUME A PREFEITURA|ASSUMIU A PREFEITURA|ASSUME O CARGO", d), "afastamento",
    grepl("TORNOU-SE TITULAR|TORNA-SE TITULAR|PASSA A SER TITULAR", d), "assumiu_titular",
    grepl("ASSUMIU O MANDATO|ASSUMIU MANDATO|ASSUME O MANDATO|ASSUMIU EM|ASSUME A VAGA|ASSUMIU A VAGA|ASSUMIU NA VAGA|NA SUPLENCIA DE", d), "suplente_efetivado",
    default = NA_character_)
}
mm[, saida_nota := classifica_nota(nota)]
mm[, forma_saida := fcase(
  no_final == TRUE  & condicao == "titular",  "fim_regular",
  no_final == TRUE  & condicao == "suplente", "suplente_efetivado",
  no_final == FALSE & !is.na(saida_nota),     saida_nota,
  default = "outro")]
mm[, `:=`(data_inicio_exercicio = fifelse(condicao == "titular", as.character(leg_inicio), NA_character_),
          data_fim_exercicio    = fifelse(no_final == TRUE, as.character(leg_fim), NA_character_))]
mm[, fonte := "alrs_memorial_quadro_legislatura"]

## ------------------------------------------------------------------ 5. nota do Memorial nas legislaturas do painel
# as legislaturas 52a a 54a tem quadro no Memorial: a nota entra como causa_original quando o
# painel ja mostra saida antes do fim da legislatura (nao se inventa causa para quem ficou)
mem_todas <- fread(file.path(raw, "memorial", "notas_memorial.csv"), encoding = "UTF-8",
                   colClasses = list(character = c("legislatura", "numero")))
mem_todas[, nome_normalizado := norm_nome(nome)]
nt_todas <- mem_todas[nome_normalizado != "", .(nota = paste(unique(texto), collapse = " | ")),
                      by = .(legislatura, nome_normalizado)]
pes[, nome_normalizado := norm_nome(nome)]
pes <- merge(pes, nt_todas, by = c("legislatura", "nome_normalizado"), all.x = TRUE)
# quando a nota do proprio Memorial diz que a pessoa entrou na vaga (ou na suplencia) de outro
# deputado, ela e suplente mesmo tendo aparecido ja no primeiro mes de sessoes da legislatura
pes[, entrou_por_vaga := grepl("(ASSUMIU|ASSUME|EMPOSSAD|TOMOU POSSE)[^.]{0,60}(VAGA|SUPLENCIA) DE",
                               toupper(stri_trans_general(fifelse(is.na(nota), "", nota), "Latin-ASCII")))]
pes[condicao == "titular" & entrou_por_vaga == TRUE, condicao := "suplente"]
cat("condicao corrigida de titular para suplente pela nota do Memorial:",
    pes[condicao == "suplente" & entrou_por_vaga == TRUE & ref_ini == mes_min, .N], "\n")
pes[, saida_nota := classifica_nota(nota)]
pes[, forma_saida := fcase(
  saiu_antes == TRUE & !is.na(saida_nota) & saida_nota != "suplente_efetivado", saida_nota,
  saiu_antes == TRUE, "outro",
  em_curso == TRUE, NA_character_,
  condicao == "suplente", "suplente_efetivado",
  default = "fim_regular")]
pes[, causa_original := fifelse(saiu_antes == TRUE & !is.na(nota), nota, NA_character_)]
mm[,  causa_original := nota]

## ------------------------------------------------------------------ 6. partido da legislatura em curso
api <- fromJSON(file.path(raw, "api_deputados_atual.json"), simplifyVector = TRUE)
apd <- as.data.table(api$lista)[, .(id_fonte = as.character(idDeputado), partido_api = siglaPartido)]
pes <- merge(pes, apd, by = "id_fonte", all.x = TRUE)
pes[legislatura != "56", partido_api := NA_character_]

## ------------------------------------------------------------------ 7. tabela unica, esquema do banco
URL_PAINEL <- "https://transparencia.al.rs.gov.br/parlamentares/presencas-plenario/pesquisa"
URL_MEM    <- "http://www2.al.rs.gov.br/memorial/Informa%C3%A7%C3%B5esParlamentares/Legislaturas/tabid/3543/Default.aspx"
a <- pes[, .(uf = "RS", fonte, legislatura, ano_eleicao, nome, nome_normalizado,
             nome_completo = NA_character_, data_nascimento = NA_character_,
             partido = partido_api, condicao,
             data_inicio_exercicio, data_fim_exercicio, causa_original, forma_saida,
             url = URL_PAINEL, id_fonte, votos_fonte = NA_character_, sexo_fonte = NA_character_)]
b <- mm[, .(uf = "RS", fonte, legislatura, ano_eleicao, nome, nome_normalizado,
            nome_completo = NA_character_, data_nascimento = NA_character_,
            partido, condicao,
            data_inicio_exercicio, data_fim_exercicio, causa_original, forma_saida,
            url = URL_MEM, id_fonte = NA_character_, votos_fonte, sexo_fonte = NA_character_)]
ex <- rbindlist(list(a, b), use.names = TRUE)
ex[votos_fonte == "", votos_fonte := NA_character_]
ex[, rid := .I]
cat("\nregistros por legislatura e fonte:\n"); print(ex[, .N, by = .(ano_eleicao, fonte, condicao)][order(ano_eleicao, fonte, condicao)])

## ------------------------------------------------------------------ 8. pareamento ao BOCEL
bocel_m <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("sq_candidato", "nr_candidato")))
bocel_p <- fread(file.path(root, "data", "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bocel_m[cd_cargo == 7L & sg_uf == "RS",
             .(id_mandato, id_pessoa, ano_eleicao, sg_uf, cd_cargo, sq_candidato, nr_candidato)]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome, dt_nascimento = as.character(dt_nascimento))], by = "id_pessoa")
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
urna <- rbindlist(lapply(list.files(file.path(root, "data_raw", "parquet"), pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE),
                         function(f) {
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                            "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_CANDIDATO", "NM_TIPO_ELEICAO")))
  x <- x[as.integer(CD_CARGO) == 7L & SG_UF == "RS" & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sg_uf = SG_UF, cd_cargo = as.integer(CD_CARGO),
        nr_candidato = as.character(NR_CANDIDATO), sq_candidato = as.character(SQ_CANDIDATO),
        nome_urna = NM_URNA_CANDIDATO, nome_civil = NM_CANDIDATO)]
}))
urna <- unique(urna, by = c("ano_eleicao", "sg_uf", "cd_cargo", "nr_candidato", "sq_candidato"))
dep <- merge(dep, urna, by = c("ano_eleicao", "sg_uf", "cd_cargo", "nr_candidato", "sq_candidato"), all.x = TRUE)
dep[, nome_urna_norm := norm_nome(nome_urna)]
dep[is.na(nome_urna_norm) | nome_urna_norm == "", nome_urna_norm := NA_character_]
cat("eleitos BOCEL cd_cargo 7 RS:", nrow(dep), "| com nome de urna:", dep[!is.na(nome_urna), .N], "\n")

ex[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, metodo_pareamento = NA_character_)]
ex[, nome_completo_norm := norm_nome(nome_completo)]
parear <- function(ex, dep, col_ex, col_dep, metodo) {
  a <- ex[is.na(id_mandato) & !is.na(get(col_ex)), .(rid, ano_eleicao, chave = get(col_ex))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- dep[!is.na(get(col_dep)), .(id_mandato, id_pessoa, ano_eleicao, chave = get(col_dep))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  m <- m[!id_mandato %in% ex$id_mandato]
  if (nrow(m)) ex[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
  invisible(nrow(m))
}
cat("pareamento por regra (mesma ordem de R/13_exercicio_assembleias.R):\n")
n1 <- parear(ex, dep, "nome_completo_norm", "nome_bocel_norm",  "nome_completo_x_nome_bocel")
n2 <- parear(ex, dep, "nome_normalizado",   "nome_urna_norm", "nome_parlamentar_x_urna")
n3 <- parear(ex, dep, "nome_normalizado",   "nome_bocel_norm",  "nome_parlamentar_x_nome_bocel")
n4 <- parear(ex, dep, "nome_completo_norm", "nome_urna_norm", "nome_completo_x_urna")
# regra adicional desta frente (aplicada depois das quatro, para nao alterar a ordem herdada):
# primeiro nome + ultimo sobrenome, exigindo unicidade dos dois lados dentro do ano
pu <- function(x) {
  p <- strsplit(x, " ", fixed = TRUE)
  vapply(p, function(v) if (length(v) >= 2) paste(v[1], v[length(v)]) else NA_character_, character(1))
}
ex[, chave_pu := pu(nome_normalizado)]
dep[, `:=`(pu_urna = pu(nome_urna_norm), pu_bocel = pu(nome_bocel_norm))]
dep[, nome_civil_norm := norm_nome(nome_civil)]
dep[nome_civil_norm == "", nome_civil_norm := NA_character_]
n5 <- parear(ex, dep, "chave_pu", "pu_urna", "primeiro_ultimo_nome_x_urna")
n6 <- parear(ex, dep, "chave_pu", "pu_bocel",  "primeiro_ultimo_nome_x_nome_bocel")
n7 <- parear(ex, dep, "nome_normalizado", "nome_civil_norm", "nome_parlamentar_x_nome_civil_tse")

# regras nao exatas, sempre depois das exatas e sempre exigindo par unico dos dois lados no ano:
#  (i) contencao de tokens — o nome de urna do TSE e um subconjunto proprio do nome parlamentar
#      da casa (ZANCHIN c- VILMAR ZANCHIN), com pelo menos um token de 4+ letras;
#  (ii) distancia de edicao <= 2 sobre a cadeia sem espacos (CLASSMANN vs CLASMANN).
parear_aprox <- function(ex, dep, col_dep, metodo, modo) {
  a <- ex[is.na(id_mandato) & !is.na(nome_normalizado), .(rid, ano_eleicao, a = nome_normalizado)]
  b <- dep[!is.na(get(col_dep)) & !id_mandato %in% ex$id_mandato,
           .(id_mandato, id_pessoa, ano_eleicao, b = get(col_dep))]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(0L)) }
  cj <- merge(a, b, by = "ano_eleicao", allow.cartesian = TRUE)
  if (modo == "subconjunto") {
    ta <- strsplit(cj$a, " ", fixed = TRUE); tb <- strsplit(cj$b, " ", fixed = TRUE)
    cj[, ok := mapply(function(x, y) length(y) > 0 && all(y %in% x) && !identical(x, y) &&
                        any(nchar(y) >= 4), ta, tb)]
  } else {
    cj[, ok := adist(gsub(" ", "", a), gsub(" ", "", b), ignore.case = TRUE) |> diag() <= 2 &
         abs(nchar(a) - nchar(b)) <= 3 & nchar(a) >= 8]
  }
  cj <- cj[ok == TRUE]
  if (!nrow(cj)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(0L)) }
  cj <- cj[, if (.N == 1L) .SD, by = .(ano_eleicao, rid)]        # um so candidato para a linha
  cj <- cj[, if (.N == 1L) .SD, by = .(ano_eleicao, id_mandato)] # um so destino para o mandato
  ex[cj, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(cj)))
  invisible(nrow(cj))
}
n8  <- parear_aprox(ex, dep, "nome_urna_norm",  "urna_contida_no_nome_parlamentar", "subconjunto")
n9  <- parear_aprox(ex, dep, "nome_civil_norm", "civil_contido_no_nome_parlamentar", "subconjunto")
n10 <- parear_aprox(ex, dep, "nome_urna_norm",  "distancia_edicao_2_urna", "edicao")
n11 <- parear_aprox(ex, dep, "nome_civil_norm", "distancia_edicao_2_civil", "edicao")

## ------------------------------------------------------------------ 9. saida
setorder(ex, ano_eleicao, fonte, condicao, nome_normalizado, na.last = TRUE)
saida <- ex[, .(uf, fonte, legislatura, ano_eleicao, nome, nome_normalizado, nome_completo,
                data_nascimento, partido, condicao, data_inicio_exercicio, data_fim_exercicio,
                causa_original, forma_saida, id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato,
                metodo_pareamento, url, id_fonte, votos_fonte, sexo_fonte)]
COLS <- c("uf","fonte","legislatura","ano_eleicao","nome","nome_normalizado","nome_completo",
          "data_nascimento","partido","condicao","data_inicio_exercicio","data_fim_exercicio",
          "causa_original","forma_saida","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento",
          "url","id_fonte","votos_fonte","sexo_fonte")
stopifnot(identical(names(saida), COLS))
in_set(na.omit(saida$forma_saida), VOCAB, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente", "nao_informado"), nome = "condicao")
em_faixa(saida$ano_eleicao, 1998, 2022, nome = "ano_eleicao")
checa_unica(as.data.frame(saida), c("fonte", "legislatura", "nome_normalizado"))
stopifnot(all(saida$uf == "RS"))
d <- as.IDate(c(saida$data_inicio_exercicio, saida$data_fim_exercicio))
em_faixa(as.integer(format(na.omit(d), "%Y")), 1999, 2027, nome = "anos das datas de exercicio")
dup <- saida[!is.na(id_mandato_bocel), .(n = uniqueN(nome_normalizado)), by = id_mandato_bocel][n > 1]
stopifnot(nrow(dup) == 0)
fwrite(saida, file.path(outd, "RS.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
cat("\ndata/assembleias2/RS.csv:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

## ------------------------------------------------------------------ 10. cobertura e numeros
cob <- dep[, .(n_bocel = .N), by = ano_eleicao]
cob <- merge(cob, saida[!is.na(id_mandato_bocel), .(n_pareados = uniqueN(id_mandato_bocel)), by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob <- merge(cob, saida[, .(n_fonte = .N, n_titular = sum(condicao == "titular"),
                            n_com_forma = sum(!is.na(forma_saida)),
                            fonte = paste(sort(unique(fonte)), collapse = ";")), by = ano_eleicao],
             by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L][is.na(n_fonte), n_fonte := 0L]
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
setorder(cob, ano_eleicao)
fwrite(cob, file.path(verd, "asm2_RS_cobertura_por_legislatura.csv"))
cat("\ncobertura por legislatura:\n"); print(cob)
fs <- saida[, .N, by = .(forma_saida, condicao)][order(-N)]
cat("\nforma de saida x condicao:\n"); print(fs)
fwrite(fs, file.path(verd, "asm2_RS_forma_saida.csv"))

reg("n_linhas", nrow(saida))
reg("n_colunas", ncol(saida))
reg("n_legislaturas", uniqueN(saida$ano_eleicao))
reg("legislaturas_cobertas", paste(sort(unique(saida$legislatura)), collapse = ";"))
reg("anos_eleicao_cobertos", paste(sort(unique(saida$ano_eleicao)), collapse = ";"))
reg("n_mandatos_bocel_rs_cd7", nrow(dep))
reg("n_bocel_com_nome_urna", dep[!is.na(nome_urna), .N])
reg("n_pareadas", saida[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_bocel_pareados", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]))
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]),
                                      nrow(dep), uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]) / nrow(dep)))
reg("n_com_forma_saida", saida[!is.na(forma_saida), .N])
reg("n_sem_forma_saida_em_curso", saida[is.na(forma_saida), .N])
reg("n_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("n_com_data_fim", saida[!is.na(data_fim_exercicio), .N])
reg("n_com_causa_original", saida[!is.na(causa_original), .N])
reg("n_titulares", saida[condicao == "titular", .N])
reg("n_suplentes", saida[condicao == "suplente", .N])
reg("n_fonte_painel_presencas", saida[fonte == "alrs_presencas_transparencia", .N])
reg("n_fonte_memorial", saida[fonte == "alrs_memorial_quadro_legislatura", .N])
reg("n_meses_painel_coletados", length(arqs))
reg("n_meses_painel_com_linhas", uniqueN(pn[, .(ano, mes)]))
for (i in seq_len(nrow(cob))) {
  reg(sprintf("taxa_pareamento_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
  reg(sprintf("n_linhas_%d", cob$ano_eleicao[i]), cob$n_fonte[i])
  reg(sprintf("n_com_forma_saida_%d", cob$ano_eleicao[i]), cob$n_com_forma[i])
}
for (m in sort(unique(na.omit(saida$metodo_pareamento))))
  reg(paste0("n_pareados_metodo_", m), saida[metodo_pareamento == m, .N])
for (i in seq_len(nrow(fs)))
  reg(sprintf("n_forma_saida_%s_%s", ifelse(is.na(fs$forma_saida[i]), "NA_em_curso", fs$forma_saida[i]),
              fs$condicao[i]), fs$N[i])
cat("\n01_build_RS: concluido —", format(Sys.time()), "\n")
sink()
