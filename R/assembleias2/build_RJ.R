#!/usr/bin/env Rscript
# build_RJ.R — exercicio, condicao e forma de saida dos deputados estaduais da ALERJ (RJ),
#   7a a 13a legislatura (eleicoes de 1998 a 2022), a partir das fontes da propria Casa.
#
# FONTES (coleta em python/assembleias2/fetch_RJ.py, fetch_noticias_RJ.py e
#         fetch_texto_noticias_RJ.py; parsing bruto em parse_RJ.py e parse_texto_noticias_RJ.py):
#   1. Biblioteca da ALERJ — PDF de composicao por legislatura
#      (https://www2.alerj.rj.gov.br/biblioteca/assets/documentos/pdf/legislaturas/deputados/<n>aLegislatura.pdf).
#      O cabecalho traz "Posse:" e "Termino:" da legislatura. A leitura dos PDFs mostrou DOIS
#      formatos, e o build os trata como fontes diferentes:
#        - 7a a 11a: lista numerada dos 70 ELEITOS (partido da eleicao) e, em separado, a secao
#          "SUPLENTES QUE ASSUMIRAM". Conferido contra o cadastro do TSE: 91% a 97% dos nomes da
#          lista numerada casam com os 70 eleitos, e o resto e variante de grafia; e nomes que
#          notoriamente sairam no meio (Rodrigo Neves, prefeito de Niteroi em 2013; Waguinho,
#          prefeito de Belford Roxo em 2017) continuam na lista.
#        - 12a: nao ha secao de suplentes, os partidos sao os do fim do periodo e 21 dos 70
#          eleitos de 2018 estao ausentes (Renan Ferreirinha, Chicao Bulhoes, Rodrigo Bacellar
#          etc.). E, portanto, a COMPOSICAO AO FIM da legislatura, nao a lista de eleitos.
#        - 13a: nao existe PDF (404 registrado em _probe_log.csv).
#   2. Portal da ALERJ — Deputados/QuemSao, so para as legislaturas 11a (codigo 18), 12a (19) e
#      13a (20); os codigos 16 e 17 devolvem pagina sem lista. Duas situacoes, disjuntas entre si:
#        situacaoDeputado=1 "Em exercicio"      -> em exercicio no retrato (fim da legislatura,
#                                                  para as encerradas; hoje, para a 13a);
#        situacaoDeputado=2 "Exerceram o mandato" -> exerceram e NAO estavam em exercicio no retrato.
#      A conferencia da 11a mostra que a situacao 2 mistura suplente que assumiu e devolveu a vaga
#      (11 dos 20 nomes estao na secao de suplentes do PDF) com TITULAR que saiu antes do fim
#      (Bernardo Rossi, Waguinho, Ze Luiz Anchite, Rogerio Lisboa, Farid Abrao...). E dai que sai
#      a evidencia estrutural de saida antecipada usada aqui.
#   3. Portal da ALERJ — ficha individual (PerfilDeputado/<id>): nome de registro da Casa, data de
#      nascimento (92 das 188 fichas) e o tratamento (DEPUTADO/DEPUTADA), que da sexo_fonte.
#   3b. A 13a nao tem PDF, entao a data de posse dela sai da noticia da Casa sobre a sessao de
#      instalacao (Visualizar/Noticia/55208, 01/02/2023), cujo endereco entra na url das linhas.
#   4. Acervo de noticias da Casa (Listar/ConsultarNoticias, 53.577 itens de 2001 a 2026, 268
#      paginas em cache) e o TEXTO INTEGRAL (Visualizar/Noticia/<id>) das 1.551 materias cujo
#      titulo ou resumo tem padrao de ato de mandato — a listagem trunca o resumo em ~300
#      caracteres, e e no corpo que a Casa nomeia o titular cuja vaga foi ocupada.
#
# COMO A FORMA DE SAIDA E DECIDIDA (ordem de precedencia):
#   1. ato terminal noticiado pela Casa (falecimento, cassacao, renuncia), com a frase da noticia
#      gravada em causa_original;
#   2. suplente que exerceu -> suplente_efetivado (derivado da estrutura da tabela da Casa);
#   3. titular em exercicio no retrato do fim da legislatura -> fim_regular, com a data de termino
#      publicada no PDF (so para as legislaturas ja encerradas; a 13a fica nao_observado);
#   4. titular fora do retrato do fim -> licenca, quando a Casa usa a palavra, ou outro;
#   5. sem retrato (7a a 10a) -> licenca quando a Casa escreve "deputado licenciado"; outro quando
#      escreve que assumiu prefeitura ou conselho de tribunal de contas, que nao devolvem a
#      cadeira dentro da legislatura; nao_observado no resto. Sair para uma secretaria, sem
#      retrato de fim de legislatura que confirme a ausencia, NAO vira forma de saida: fica no
#      arquivo de auditoria output/verificacao/assembleias2_RJ_atos_noticiados.csv.
#   6. Eleito que a Casa nunca listou em nenhuma fonte da legislatura e cuja morte a Casa noticiou
#      entre a eleicao e o primeiro mes de mandato -> nao_tomou_posse (etapa 9b).
#
# O QUE ESTA CAMADA NAO ATESTA: pertinencia do pareamento por nome (homonimo, apelido, nome de
# urna que nao e o nome civil); completude do acervo de noticias da Casa; veracidade do que a
# Casa publica; e, para a 7a a 10a, a saida definitiva de quem a Casa registrou apenas como
# licenciado — sem retrato de fim de legislatura nao da para saber se voltou.
#
# Entrada: data_raw/assembleias2/RJ/parsed/*.csv, data/mandatos.csv, data/pessoas.csv,
#          data_raw/parquet/cand_<ANO>.parquet (nome de urna do TSE)
# Saida:   data/assembleias2/RJ.csv (as 21 colunas de data/exercicio_assembleias_2.csv)
#          output/verificacao/assembleias2_RJ_*.csv, output/numeros_assinatura.txt (prefixo asm2rj_)
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/build_RJ.R
set.seed(20260829)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/assembleias2/build_RJ.R"
dir.create("logs", showWarnings = FALSE)
dir.create("data/assembleias2", recursive = TRUE, showWarnings = FALSE)
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/assembleias2_build_RJ.log", open = "wt"); sink(logf, split = TRUE)
cat("build_RJ.R —", format(Sys.time()), "\n")

RAW <- "data_raw/assembleias2/RJ"; PAR <- file.path(RAW, "parsed"); UF <- "RJ"
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
LEG_ANO <- c(`7` = 1998L, `8` = 2002L, `9` = 2006L, `10` = 2010L, `11` = 2014L, `12` = 2018L, `13` = 2022L)
LEG_ENCERRADA <- c(`7` = TRUE, `8` = TRUE, `9` = TRUE, `10` = TRUE, `11` = TRUE, `12` = TRUE, `13` = FALSE)
U_BIB <- "https://www2.alerj.rj.gov.br/biblioteca/assets/documentos/pdf/legislaturas/deputados/%saLegislatura.pdf"
reg <- function(k, v) registrar_numero(paste0("asm2rj_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

# normalizacao de nome: a mesma de R/13_exercicio_assembleias.R
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(fcoalesce(as.character(x), "")), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])

## ---------------------------------------------------------------- 1. fontes parseadas
bib <- fread(file.path(PAR, "biblioteca_composicao.csv"), colClasses = "character", na.strings = "")
qs  <- fread(file.path(PAR, "quemsao_composicao.csv"),   colClasses = "character", na.strings = "")
pf  <- fread(file.path(PAR, "perfis.csv"),               colClasses = "character", na.strings = "")
nt  <- fread(file.path(PAR, "noticias_eventos.csv"),     colClasses = "character", na.strings = "")
ftx <- file.path(PAR, "noticias_texto.csv")
ntx <- if (file.exists(ftx)) fread(ftx, colClasses = "character", na.strings = "") else
       data.table(id_noticia = character(), data = character(), titulo = character(),
                  texto = character(), url = character())
cat("parsed: biblioteca", nrow(bib), "| quemsao", nrow(qs), "| fichas", nrow(pf),
    "| noticias-titulo", nrow(nt), "| noticias-texto", nrow(ntx), "\n")
stopifnot(nrow(bib) > 400, nrow(qs) > 250, nrow(pf) > 150, nrow(nt) > 500)

# datas de posse e termino publicadas no cabecalho do PDF (a 13a nao tem PDF)
legdt <- unique(bib[, .(legislatura = as.character(as.integer(legislatura)),
                        posse_legislatura, termino_legislatura)])
checa_unica(as.data.frame(legdt), "legislatura")
# a 13a nao tem PDF: a data de posse vem da noticia da propria Casa sobre a sessao de instalacao
p13 <- nt[grepl("DEPUTADOS ESTADUAIS TOMAM POSSE", norm_nome(titulo)) &
          grepl("LEGISLATURA", norm_nome(titulo)) & data >= "2023-01-01" & data <= "2023-12-31"]
posse13 <- if (nrow(p13)) min(p13$data) else NA_character_
url13 <- if (nrow(p13)) p13[data == posse13][1]$url else NA_character_
cat("posse da 13a legislatura, pela noticia da Casa:", posse13, "|", url13, "\n")
legdt <- rbind(legdt, data.table(legislatura = "13", posse_legislatura = posse13,
                                 termino_legislatura = NA_character_))
# janela para filtrar noticia por legislatura (nao vira dado: so recorta o acervo)
DATA_MAX <- max(nt$data, na.rm = TRUE)
legdt[, `:=`(jan_ini = fcoalesce(posse_legislatura, paste0(LEG_ANO[legislatura] + 1L, "-02-01")),
             jan_fim = fcoalesce(termino_legislatura, DATA_MAX))]

## ---------------------------------------------------------------- 2. pool de linhas de fonte
bib[, legislatura := as.character(as.integer(legislatura))]
qs[,  legislatura := as.character(as.integer(legislatura))]
bib[, origem := fcase(legislatura == "12", "bib_composicao_final",
                      condicao == "titular", "bib_titular",
                      default = "bib_suplente")]
qs[, origem := fifelse(situacao == "1", "qs1", "qs2")]
pool <- rbindlist(list(
  bib[, .(legislatura, origem, nome_fonte, partido_fonte, tratamento_fonte,
          id_fonte = NA_character_, url = sprintf(U_BIB, legislatura))],
  qs[,  .(legislatura, origem, nome_fonte, partido_fonte, tratamento_fonte = NA_character_,
          id_fonte, url)]), use.names = TRUE)
pool[, nome_normalizado := norm_nome(nome_fonte)]
pool <- pool[nome_normalizado != ""]
pool <- unique(pool, by = c("legislatura", "origem", "nome_normalizado"))
cat("\nlinhas de fonte por legislatura x origem:\n")
print(dcast(pool[, .N, by = .(legislatura = as.integer(legislatura), origem)],
            legislatura ~ origem, value.var = "N", fill = 0L)[order(legislatura)])

# ficha individual: nome de registro, nascimento e sexo
pf2 <- unique(pf[, .(id_fonte, nome_ficha, data_nascimento, tratamento_ficha = tratamento_fonte,
                     url_ficha = url)])
checa_unica(as.data.frame(pf2), "id_fonte")
pool <- merge(pool, pf2, by = "id_fonte", all.x = TRUE, sort = FALSE)

## ---------------------------------------------------------------- 3. nomes distintos e BOCEL
nomes <- pool[, .(nome = nome_fonte[1], nome_ficha = na.omit(nome_ficha)[1],
                  data_nascimento = na.omit(data_nascimento)[1]),
              by = .(legislatura, nome_normalizado)]
nomes[, ano_eleicao := LEG_ANO[legislatura]]
nomes[, nid := .I]
nomes[, nome_ficha_norm := norm_nome(nome_ficha)][nome_ficha_norm == "", nome_ficha_norm := NA_character_]

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "7" & sg_uf == UF]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
dep <- merge(mand[, .(id_mandato, id_pessoa, ano_eleicao = as.integer(ano_eleicao),
                      sq_candidato, nr_candidato, mandato_inicio, mandato_fim)],
             pess[, .(id_pessoa, nome_bocel = nome, nome_urna_recente, dt_nascimento)], by = "id_pessoa")
urna <- rbindlist(lapply(sprintf("data_raw/parquet/cand_%d.parquet", sort(unique(dep$ano_eleicao))), function(f) {
  if (!file.exists(f)) return(NULL)
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                            "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_TIPO_ELEICAO")))
  x <- x[as.integer(CD_CARGO) == 7L & SG_UF == UF & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), nr_candidato = as.character(NR_CANDIDATO),
        sq_candidato = as.character(SQ_CANDIDATO), nome_urna = NM_URNA_CANDIDATO)]
}))
urna <- unique(urna, by = c("ano_eleicao", "nr_candidato", "sq_candidato"))
dep <- merge(dep, urna, by = c("ano_eleicao", "nr_candidato", "sq_candidato"), all.x = TRUE)
dep[, `:=`(nome_bocel_norm = norm_nome(nome_bocel), nome_urna_norm = norm_nome(nome_urna),
           nome_urna_rec_norm = norm_nome(nome_urna_recente))]
for (v in c("nome_bocel_norm", "nome_urna_norm", "nome_urna_rec_norm")) dep[get(v) == "", (v) := NA_character_]
cat("\neleitos BOCEL cd_cargo 7 RJ:", nrow(dep), "| com nome de urna do cadastro:",
    dep[!is.na(nome_urna_norm), .N], "\n")
reg("bocel_mandatos_cd7", nrow(dep))

## ---------------------------------------------------------------- 4. pareamento nome -> mandato
# Mesmas regras e mesma ordem de R/13_exercicio_assembleias.R (a, b, c, d), acrescidas de nome de
# urna recente, data de nascimento da ficha e continencia de tokens. Aqui um MESMO mandato pode
# ser alcancado por mais de uma grafia (a Biblioteca escreve "Andre L Ceciliano" e o portal
# "Andre Ceciliano"): e assim que as duas fontes se juntam na mesma pessoa, na etapa 5.
nomes[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_,
             metodo_pareamento = NA_character_)]
parear <- function(col_ex, col_dep, metodo) {
  a <- nomes[is.na(id_mandato_bocel) & !is.na(get(col_ex)), .(nid, ano_eleicao, chave = get(col_ex))]
  if (!nrow(a)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(NULL)) }
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- dep[!is.na(get(col_dep)), .(id_mandato, id_pessoa, ano_eleicao, chave = get(col_dep))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
}
cat("\npareamento por regra (ordem de R/13):\n")
parear("nome_ficha_norm",  "nome_bocel_norm",      "nome_completo_x_nome_bocel")
parear("nome_normalizado", "nome_urna_norm",     "nome_parlamentar_x_urna")
parear("nome_normalizado", "nome_bocel_norm",      "nome_parlamentar_x_nome_bocel")
parear("nome_ficha_norm",  "nome_urna_norm",     "nome_completo_x_urna")
parear("nome_normalizado", "nome_urna_rec_norm", "nome_parlamentar_x_urna_recente")
# data de nascimento da ficha, unica dos dois lados na eleicao
a <- nomes[is.na(id_mandato_bocel) & !is.na(data_nascimento), .(nid, ano_eleicao, chave = data_nascimento)]
a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
b <- dep[!is.na(dt_nascimento), .(id_mandato, id_pessoa, ano_eleicao, chave = dt_nascimento)]
b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
m <- merge(a, b, by = c("ano_eleicao", "chave"))
if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                       metodo_pareamento = "data_nascimento_unica")]
cat(sprintf("  %-34s +%d\n", "data_nascimento_unica", nrow(m)))
# tokens do nome da Casa contidos no nome civil / no nome de urna, com casamento unico
por_token <- function(col_dep, metodo) {
  a <- nomes[is.na(id_mandato_bocel), .(nid, ano_eleicao, a = nome_normalizado)]
  b <- dep[!is.na(get(col_dep)), .(ano_eleicao, b = get(col_dep), id_mandato, id_pessoa)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(NULL)) }
  cand <- merge(a, b, by = "ano_eleicao", allow.cartesian = TRUE)
  ta <- tok(cand$a); tb <- tok(cand$b)
  cand[, ok := mapply(function(x, y) length(x) >= 2L && all(x %in% y), ta, tb)]
  m <- cand[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else .SD[0], by = nid]
  if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, if (nrow(m)) nrow(m) else 0L))
}
por_token("nome_bocel_norm",  "tokens_no_nome_civil")
por_token("nome_urna_norm", "tokens_no_nome_de_urna")
# token unico e longo do nome da Casa dentro do nome de urna ("Bebeto" -> "BEBETO TETRA")
por_token1 <- function(col_dep, metodo) {
  a <- nomes[is.na(id_mandato_bocel), .(nid, ano_eleicao, a = nome_normalizado)]
  b <- dep[!is.na(get(col_dep)), .(ano_eleicao, b = get(col_dep), id_mandato, id_pessoa)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(NULL)) }
  cand <- merge(a, b, by = "ano_eleicao", allow.cartesian = TRUE)
  ta <- tok(cand$a); tb <- tok(cand$b)
  cand[, ok := mapply(function(x, y) length(x) == 1L && nchar(x[1]) >= 5L && x %in% y, ta, tb)]
  m <- cand[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else .SD[0], by = nid]
  if (nrow(m)) m <- m[, if (uniqueN(nid) == 1L) .SD[1] else .SD[0], by = id_mandato]
  if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
}
por_token1("nome_urna_norm", "token_unico_no_nome_de_urna")
# tokens do nome de urna contidos no nome da Casa ("NECA" -> "Manoel Rosa - Neca")
por_token_inv <- function(col_dep, metodo) {
  a <- nomes[is.na(id_mandato_bocel), .(nid, ano_eleicao, a = nome_normalizado)]
  b <- dep[!is.na(get(col_dep)), .(ano_eleicao, b = get(col_dep), id_mandato, id_pessoa)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(NULL)) }
  cand <- merge(a, b, by = "ano_eleicao", allow.cartesian = TRUE)
  ta <- tok(cand$a); tb <- tok(cand$b)
  cand[, ok := mapply(function(x, y) length(y) >= 1L && max(nchar(y)) >= 4L && all(y %in% x), ta, tb)]
  m <- cand[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else .SD[0], by = nid]
  if (nrow(m)) m <- m[, if (uniqueN(nid) == 1L) .SD[1] else .SD[0], by = id_mandato]
  if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
}
por_token_inv("nome_urna_norm", "urna_contida_no_nome_da_casa")
# grafia quase igual (o PDF da Casa tem erro de digitacao: "Edmilson Valetim", "Janeira Rocha")
por_dist <- function(col_dep, metodo, lim = 0.15) {
  a <- nomes[is.na(id_mandato_bocel) & nchar(nome_normalizado) >= 8, .(nid, ano_eleicao, a = nome_normalizado)]
  b <- dep[!is.na(get(col_dep)) & nchar(get(col_dep)) >= 8, .(ano_eleicao, b = get(col_dep), id_mandato, id_pessoa)]
  if (!nrow(a) || !nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(invisible(NULL)) }
  cand <- merge(a, b, by = "ano_eleicao", allow.cartesian = TRUE)
  cand[, r := mapply(function(x, y) as.numeric(adist(x, y)) / max(nchar(x), nchar(y)), a, b)]
  m <- cand[r <= lim][, if (.N == 1L) .SD, by = nid]
  if (nrow(m)) m <- m[, if (.N == 1L) .SD, by = id_mandato]
  if (nrow(m)) nomes[m, on = "nid", `:=`(id_mandato_bocel = i.id_mandato, id_pessoa_bocel = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
}
por_dist("nome_urna_norm", "grafia_proxima_do_nome_de_urna")
por_dist("nome_bocel_norm",  "grafia_proxima_do_nome_civil")

# coerencia: pareado por nome tem de bater no nascimento quando a ficha da ALERJ o traz
chk <- merge(nomes[!is.na(id_mandato_bocel) & !is.na(data_nascimento), .(nid, data_nascimento, id_pessoa_bocel)],
             pess[, .(id_pessoa_bocel = id_pessoa, dt_nascimento)], by = "id_pessoa_bocel")
dif <- chk[!is.na(dt_nascimento) & dt_nascimento != data_nascimento]
# So o ANO diferente derruba o pareamento: a ficha da ALERJ e o cadastro do TSE divergem em dia e
# mes em casos que sao a mesma pessoa (Rafael Picciani 04/04 x 02/04/1986; Janio Mendes 27/02 x
# 15/03/1965), enquanto homonimo distinto tem ano diferente.
conf <- dif[substr(dt_nascimento, 1, 4) != substr(data_nascimento, 1, 4)]
cat("nascimento divergente na ficha:", nrow(dif), "| desfeitos (ano diferente):", nrow(conf), "\n")
if (nrow(conf)) nomes[nid %in% conf$nid, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_,
                                              metodo_pareamento = NA_character_)]
reg("pareados_nascimento_divergente_dia_mes", nrow(dif) - nrow(conf))
reg("pareados_nascimento_divergente_desfeitos", nrow(conf))

## ---------------------------------------------------------------- 5. entidade (pessoa x legislatura)
# grafias que apontam para o mesmo mandato viram uma linha so; entre as nao pareadas, junta-se a
# grafia curta cuja lista de tokens esta contida na de uma unica grafia longa da mesma legislatura.
nomes[, ent := fifelse(is.na(id_mandato_bocel), paste0("N:", nome_normalizado), paste0("M:", id_mandato_bocel))]
# Grafias da mesma pessoa na mesma legislatura: a Biblioteca escreve "Dionisio Lions" e o portal
# "DIONISIO LINS"; a Biblioteca escreve "Marco Figueiredo" e o portal so "FIGUEIREDO". Duas
# grafias sao ligadas quando (a) os tokens de uma estao contidos nos da outra, ou (b) a distancia
# de edicao entre elas e de ate 15% do comprimento. A ligacao so vale se for UNICA dos dois lados
# na legislatura e se as duas grafias nao apontarem para mandatos diferentes do BOCEL.
liga <- function(v, mand_de) {
  n <- length(v)
  if (n < 2L) return(NULL)
  D <- adist(v)
  L <- outer(nchar(v), nchar(v), pmax)
  tk <- tok(v)
  pares <- data.table(i = rep(seq_len(n), each = n), j = rep(seq_len(n), times = n))[i < j]
  pares[, r := D[cbind(i, j)] / L[cbind(i, j)]]
  cont <- mapply(function(a, b) {
    x <- tk[[a]]; y <- tk[[b]]
    if (!length(x) || !length(y)) return(FALSE)
    (all(x %in% y) && max(nchar(x)) >= 4L) || (all(y %in% x) && max(nchar(y)) >= 4L)
  }, pares$i, pares$j)
  pares[, d := D[cbind(i, j)]]
  pares[, nmin := pmin(nchar(v)[i], nchar(v)[j])]
  pares <- pares[r <= 0.15 | (d <= 2L & nmin >= 9L) | cont]
  if (!nrow(pares)) return(NULL)
  ma <- mand_de[pares$i]; mb <- mand_de[pares$j]
  pares <- pares[is.na(ma) | is.na(mb) | ma == mb]
  if (!nrow(pares)) return(NULL)
  gr <- pares[, .N, by = i][N == 1L]$i
  gc <- pares[, .N, by = j][N == 1L]$j
  pares[i %in% gr & j %in% gc]
}
for (L in unique(nomes$legislatura)) {
  idx <- nomes[legislatura == L, which = TRUE]
  v <- nomes$nome_normalizado[idx]
  pr <- liga(v, nomes$id_mandato_bocel[idx])
  if (is.null(pr) || !nrow(pr)) next
  # union-find sobre os indices locais
  pai <- seq_along(v)
  acha <- function(a) { while (pai[a] != a) a <- pai[a]; a }
  for (k in seq_len(nrow(pr))) { ra <- acha(pr$i[k]); rb <- acha(pr$j[k]); if (ra != rb) pai[rb] <- ra }
  raiz <- vapply(seq_along(v), acha, 1L)
  novo_ent <- ave_ent <- character(length(v))
  for (r in unique(raiz)) {
    memb <- which(raiz == r)
    md <- na.omit(nomes$id_mandato_bocel[idx][memb])
    novo_ent[memb] <- if (length(md)) paste0("M:", md[1]) else
      paste0("N:", v[memb][which.max(nchar(v[memb]))])
  }
  nomes[idx, ent := novo_ent]
  # a grafia sem par herda o mandato da grafia pareada do mesmo grupo
  nomes[idx, `:=`(id_mandato_bocel = ifelse(is.na(id_mandato_bocel) & grepl("^M:", ent), sub("^M:", "", ent), id_mandato_bocel))]
}
nomes[is.na(id_pessoa_bocel) & !is.na(id_mandato_bocel),
      id_pessoa_bocel := mand$id_pessoa[match(id_mandato_bocel, mand$id_mandato)]]
nomes[is.na(metodo_pareamento) & !is.na(id_mandato_bocel), metodo_pareamento := "grafia_ligada_na_legislatura"]
reg("grafias_ligadas", nomes[metodo_pareamento == "grafia_ligada_na_legislatura", .N])
cat("entidades apos ligar grafias:", uniqueN(nomes[, .(legislatura, ent)]), "de", nrow(nomes), "grafias\n")

pool <- merge(pool, nomes[, .(legislatura, nome_normalizado, nid, ent, id_mandato_bocel, id_pessoa_bocel,
                              metodo_pareamento)],
              by = c("legislatura", "nome_normalizado"), all.x = TRUE)

sinal <- function(x, alvo) as.integer(any(x == alvo))
comp <- pool[, .(
  id_mandato_bocel    = na.omit(id_mandato_bocel)[1],
  id_pessoa_bocel     = na.omit(id_pessoa_bocel)[1],
  metodo_pareamento = na.omit(metodo_pareamento)[1],
  nome              = nome_fonte[which.max(nchar(nome_fonte))],
  nomes_variantes   = paste(sort(unique(nome_normalizado)), collapse = " | "),
  partido           = na.omit(partido_fonte)[1],
  id_fonte          = na.omit(id_fonte)[1],
  nome_completo     = na.omit(nome_ficha)[1],
  data_nascimento   = na.omit(data_nascimento)[1],
  tratamento        = na.omit(c(tratamento_ficha, tratamento_fonte))[1],
  em_bib_titular    = sinal(origem, "bib_titular"),
  em_bib_suplente   = sinal(origem, "bib_suplente"),
  em_bib_final      = sinal(origem, "bib_composicao_final"),
  em_qs1            = sinal(origem, "qs1"),
  em_qs2            = sinal(origem, "qs2"),
  fontes            = paste(sort(unique(origem)), collapse = "+"),
  url               = paste(sort(unique(url)), collapse = " ; ")
), by = .(legislatura, ent)]
comp[, nome_normalizado := norm_nome(nome)]
comp[, ano_eleicao := LEG_ANO[legislatura]]
comp <- merge(comp, legdt, by = "legislatura", all.x = TRUE)
comp[, rid := .I]
checa_unica(as.data.frame(comp), c("legislatura", "ent"))
cat("\nentidades (pessoa x legislatura):", nrow(comp), "\n")

## ---------------------------------------------------------------- 6. condicao
# titular quando a propria Casa a lista como eleita (7a a 11a) ou quando o nome casa com um
# mandato de eleito do TSE naquela eleicao; suplente no resto.
comp[, condicao := fifelse(em_bib_titular == 1L | !is.na(id_mandato_bocel), "titular", "suplente")]
cat("\ncondicao por legislatura:\n")
print(dcast(comp[, .N, by = .(legislatura = as.integer(legislatura), condicao)],
            legislatura ~ condicao, value.var = "N", fill = 0L)[order(legislatura)])

## ---------------------------------------------------------------- 7. situacao ao fim (estrutura)
# Retrato da Casa: quem estava em exercicio quando o portal foi montado. Existe para a 11a, 12a e
# 13a. Para a 12a, a composicao final do PDF entra como reforco de "em exercicio" para quem o
# portal nao lista em nenhuma das duas situacoes.
comp[, sit_fim := fcase(
  em_qs1 == 1L, "em_exercicio",
  em_qs2 == 1L, "encerrou_antes",
  legislatura == "12" & em_bib_final == 1L, "em_exercicio",
  default = NA_character_)]
cat("\nsituacao ao fim, por legislatura:\n")
print(dcast(comp[, .N, by = .(legislatura = as.integer(legislatura),
                              sit_fim = fcoalesce(sit_fim, "sem_retrato"))],
            legislatura ~ sit_fim, value.var = "N", fill = 0L)[order(legislatura)])

## ---------------------------------------------------------------- 8. atos noticiados pela Casa
# 8a. TITULO: padrao estrito de ato, com o nome da pessoa no proprio titulo.
nt[, tt := norm_nome(titulo)]
nt[, classe_tit := fcase(
  grepl("(^| )(MORRE|MORREU|MORTE|FALECE|FALECEU|FALECIMENTO|LUTO|PESAR|ASSASSINAD)", tt) &
    grepl("DEPUTAD|LUTO OFICIAL|NOTA DE PESAR|LAMENTA|MINUTO DE SILENCIO|SUSPENDE VOTAC", tt) &
    !grepl("EX DEPUTAD|POST MORTEM|POSTMORTEM|MEDALHA|HOMENAGE|VIUVA|PAI DO|MAE DO|FILHO DO|IRMAO DO", tt),
    "falecimento",
  grepl("RENUNCI", tt) & grepl("DEPUTAD", tt) & !grepl("EX DEPUTAD", tt), "renuncia",
  grepl("CASSAD|CASSA O MANDATO|CASSA MANDATO|PERDE O MANDATO|PERDEM O MANDATO|PERDA DE MANDATO|PERDA DO MANDATO", tt) &
    !grepl("PEDIRA|PEDIDO|PEDE |PROCESSO|VOTACAO|VOTO SECRETO|ENQUETE|RECUSADA|CCJ|PODE |PODERA|PROJETO|DEBATE|CNH|LICENCA AMBIENTAL", tt),
    "cassacao",
  grepl("LICENC", tt) & grepl("DEPUTAD", tt) &
    !grepl("AMBIENTAL|MATERNIDADE|PATERNIDADE|PREMIO|SERVIDOR|OBRA|FUNCIONAMENTO|CONCEDE", tt), "licenca",
  grepl("(^| )(TOMA POSSE|TOMAM POSSE|ASSUME|ASSUMEM|EMPOSSA|EMPOSSAD)", tt) &
    grepl("DEPUTAD|SUPLENTE|ALERJ", tt), "posse",
  default = NA_character_)]
ev_tit <- nt[!is.na(classe_tit)]
cat("\nnoticias com ato no titulo:\n"); print(ev_tit[, .N, by = classe_tit][order(-N)])

# Uma pessoa aparece na Casa com mais de uma grafia ("Dr. Jose Luiz Nanci" na Biblioteca, "JOSE
# LUIZ NANCI" no portal) e a noticia usa qualquer uma delas, quase sempre sem o tratamento. O
# alvo do casamento e, entao, o conjunto de grafias da entidade mais a versao sem o pronome de
# tratamento inicial. Grafia curta demais fica de fora: exige-se 8 caracteres e, num nome de
# token unico, que o proprio token tenha 8 caracteres ("SADINOEL", "WAGUINHO"; "DICA" nao entra).
TRAT <- "^(DR|DRA|DOUTOR|DOUTORA|PROF|PROFESSOR|PROFESSORA|PASTOR|PASTORA|CEL|CORONEL|MAJOR|CAPITAO|SARGENTO|SUBTENENTE|SUB TENENTE|DELEGADO|DELEGADA|BISPO|TIA|SENHOR|VEREADOR) "
alvo <- comp[, .(padrao = unique(c(unlist(strsplit(nomes_variantes, " \\| ", fixed = FALSE)),
                                   trimws(sub(TRAT, "", unlist(strsplit(nomes_variantes, " \\| ", fixed = FALSE))))))),
             by = .(rid, legislatura, jan_ini, jan_fim)]
alvo <- alvo[nchar(padrao) >= 8L]
alvo <- alvo[mapply(function(p) { tt <- tok(p)[[1]]; length(tt) >= 2L || (length(tt) == 1L && nchar(tt) >= 8L) },
                    padrao)]
alvo[, re := paste0("(^| )", gsub(" ", "[^A-Z]+", padrao), "( |$)")]
cat("grafias-alvo para casar noticia:", nrow(alvo), "para", uniqueN(alvo$rid), "entidades\n")

gr <- CJ(k = seq_len(nrow(alvo)), i = seq_len(nrow(ev_tit)))
gr <- cbind(alvo[gr$k, .(rid, legislatura, jan_ini, jan_fim, re)],
            ev_tit[gr$i, .(id_noticia, data_ev = data, titulo, tt, classe = classe_tit, url_ev = url)])
gr <- gr[stri_detect_regex(tt, re)]
# no titulo de ato nomeado o nome tem de vir DEPOIS da palavra do ato ("ALERJ lamenta a morte do
# deputado Fulano"), senao "deputado Fulano lamenta a morte de Beltrano" viraria obito do Fulano.
ATO_TT <- "MORRE|MORREU|MORTE|FALECE|FALECIMENTO|LUTO|PESAR|ASSASSINAD|RENUNCI|CASSAD|CASSA|PERDA DE MANDATO|PERDE O MANDATO|LICENC"
gr[, pos_nome := stri_locate_first_regex(tt, re)[, 1]]
gr[, pos_ato  := stri_locate_first_regex(tt, ATO_TT)[, 1]]
gr <- gr[classe == "posse" | (!is.na(pos_ato) & pos_nome > pos_ato)]
gr <- gr[data_ev >= jan_ini & data_ev <= jan_fim]
gr[, trecho := titulo]
cat("atos de titulo casados a uma entidade e a sua legislatura:", nrow(gr), "\n")
if (nrow(gr)) print(gr[, .N, by = classe][order(-N)])

# 8b. CORPO: a Casa nomeia quem deixou a cadeira de duas maneiras, e as duas sao lidas aqui.
#   (i) expressao de vaga ANTES do nome ("na vaga deixada por Fulano", "no lugar de Fulano",
#       "deputado licenciado Fulano"): o nome so conta se comecar nos 45 primeiros caracteres
#       depois da expressao, porque mais adiante o texto ja fala do suplente que entrou;
#   (ii) expressao de saida DEPOIS do nome ("Fulano, eleito prefeito de X", "o deputado Fulano,
#       que morreu"): a expressao so conta se vier nos 25 caracteres seguintes ao nome.
# O texto gravado em causa_original e o da noticia, com acento e pontuacao, nao o normalizado.
gc2 <- data.table()
if (nrow(ntx)) {
  ntx[, orig := paste(titulo, texto)]
  # normalizacao que PRESERVA posicao (so troca acento e apaga o que nao e letra)
  ntx[, busca := gsub("[^A-Z ]", " ", stri_trans_general(toupper(orig), "Latin-ASCII"))]
  ntx <- ntx[nchar(busca) == nchar(orig)]
  CUE_ANTES <- paste0("VAGA DEIXADA|VAGAS DEIXADAS|VAGA ABERTA|VAGAS ABERTAS|VAGA DO DEPUTADO|",
    "VAGA DA DEPUTADA|VAGAS DOS DEPUTADOS|NA VAGA DE|NAS VAGAS DE|NO LUGAR DE|NOS LUGARES DE|",
    "EM SUBSTITUICAO A|SUBSTITUI O DEPUTADO|SUBSTITUI A DEPUTADA|ENTRARAM NAS VAGAS|",
    "ENTROU NA VAGA|ASSUMIU A VAGA|ASSUMIRAM AS VAGAS|CADEIRA DEIXADA|CADEIRAS DEIXADAS|",
    "SUCEDE O DEPUTADO|DEIXA A ALERJ|DEIXAM A ALERJ|DEIXOU A ALERJ|DEIXARAM A ALERJ|",
    "POSTOS DEIXADOS|POSTO DEIXADO|LUGARES DEIXADOS|DEIXAD[OA]S? PEL[OA]|",
    "SAIDA DO DEPUTADO|SAIDA DOS DEPUTADOS|SAIDA DA DEPUTADA|",
    "DEPUTADO LICENCIADO|DEPUTADA LICENCIADA|DEPUTADOS LICENCIADOS")
  CUE_DEPOIS <- paste0("QUE MORREU|QUE FALECEU|QUE MORRERA|",
    "QUE RENUNCIOU|RENUNCIOU AO MANDATO|",
    "SE LICENCIOU|LICENCIOU SE|",
    "(ELEIT[OA]|NOV[OA]|EMPOSSAD[OA]|ATUAL) (VICE )?PREFEIT[OA]|PREFEITURA DE|",
    "ASSUMIU A SECRETARIA|ASSUMIRAM AS SECRETARIAS|(NOVO|ATUAL) SECRETARIO|",
    "(NOVA|ATUAL) SECRETARIA|SECRETARIO (DE ESTADO|MUNICIPAL)|CONSELHEIR[OA] DO TRIBUNAL")
  CUE_LIC <- paste0("DEPUTAD[OA] LICENCIAD[OA]|DEPUTAD[OA]S LICENCIAD[OA]S|",
                    "PARLAMENTAR LICENCIAD[OA]|PARLAMENTARES LICENCIADOS")
  jan <- rbindlist(list(
    ntx[, { p <- stri_locate_all_regex(busca, CUE_LIC)[[1]]
      if (all(is.na(p[, 1]))) list(cue = character(), chave = character(), trecho = character(), lado = character())
      else list(cue = stri_sub(busca, p[, 1], p[, 2]),
                chave = stri_sub(busca, p[, 2] + 1L, pmin(nchar(busca), p[, 2] + 45L)),
                trecho = trimws(gsub(" +", " ", stri_sub(orig, p[, 1], pmin(nchar(orig), p[, 2] + 180L)))),
                lado = "cue_antes") }, by = .(id_noticia, data, url)],
    ntx[, { p <- stri_locate_all_regex(busca, CUE_ANTES)[[1]]
      if (all(is.na(p[, 1]))) list(cue = character(), chave = character(), trecho = character(), lado = character())
      else list(cue = stri_sub(busca, p[, 1], p[, 2]),
                chave = stri_sub(busca, p[, 2] + 1L, pmin(nchar(busca), p[, 2] + 45L)),
                trecho = trimws(gsub(" +", " ", stri_sub(orig, p[, 1], pmin(nchar(orig), p[, 2] + 180L)))),
                lado = "cue_antes") }, by = .(id_noticia, data, url)],
    ntx[, { p <- stri_locate_all_regex(busca, CUE_DEPOIS)[[1]]
      if (all(is.na(p[, 1]))) list(cue = character(), chave = character(), trecho = character(), lado = character())
      else list(cue = stri_sub(busca, p[, 1], p[, 2]),
                chave = stri_sub(busca, pmax(1L, p[, 1] - 45L), p[, 1] - 1L),
                trecho = trimws(gsub(" +", " ", stri_sub(orig, pmax(1L, p[, 1] - 90L),
                                                         pmin(nchar(orig), p[, 2] + 90L)))),
                lado = "cue_depois") }, by = .(id_noticia, data, url)]), use.names = TRUE)
  cat("janelas de vaga no corpo das noticias:", nrow(jan), "\n")
  if (nrow(jan)) {
    cc <- CJ(k = seq_len(nrow(alvo)), j = seq_len(nrow(jan)))
    cc <- cbind(alvo[cc$k, .(rid, legislatura, jan_ini, jan_fim, padrao)],
                jan[cc$j, .(id_noticia, data_ev = data, cue, chave, trecho, lado, url_ev = url)])
    # no lado "cue_depois" o nome tem de terminar a no maximo 25 caracteres da expressao
    cc[, pat := fifelse(lado == "cue_antes",
                        paste0("(^| )", gsub(" ", "[^A-Z]+", padrao)),
                        paste0("(^| )", gsub(" ", "[^A-Z]+", padrao), "[A-Z ]{0,25}$"))]
    cc <- cc[stri_detect_regex(chave, pat)]
    cc <- cc[data_ev >= jan_ini & data_ev <= jan_fim]
    # "o parlamentar licenciado que volta a Alerj e Fulano" fala de retorno, nao de saida
    cc <- cc[!grepl("VOLTA|VOLTOU|RETORNA|RETORNOU|REASSUM", chave)]
    tn <- norm_nome(cc$trecho)
    # a classe segue o que a Casa escreve: "licenca" so quando o texto usa a palavra;
    # "outro_definitivo" quando o cargo assumido nao devolve a cadeira dentro da legislatura
    # (prefeitura, conselho de tribunal de contas); "outro_provisorio" no resto (secretaria),
    # que so vira forma de saida quando o retrato de fim de legislatura confirma a ausencia.
    cc[, classe := fcase(
      grepl("QUE MORREU|QUE FALECEU|MORTE DO DEPUTAD|MORTE DA DEPUTAD|FALECIMENTO D|ASSASSINAD", tn),
        "falecimento",
      grepl("RENUNCIOU AO MANDATO|RENUNCIA AO MANDATO|PEDIDO DE RENUNCIA|QUE RENUNCIOU", tn), "renuncia",
      grepl("MANDATO CASSADO|CASSACAO DO MANDATO|TEVE O MANDATO CASSADO|PERDA DO MANDATO|PERDA DE MANDATO", tn),
        "cassacao",
      # "licenca" so quando a propria expressao que amarra o nome fala de licenca ("deputado
      # licenciado Fulano", "Fulano, que se licenciou"): a palavra solta no paragrafo costuma
      # estar falando de outro parlamentar citado na mesma frase.
      grepl("LICENCIAD|LICENCIOU", cue), "licenca",
      grepl("PREFEIT|CONSELHEIR[OA] DO TRIBUNAL", tn), "outro_definitivo",
      default = "outro_provisorio")]
    gc2 <- unique(cc[, .(rid, id_noticia, data_ev, classe, trecho, url_ev)])
    cat("mencoes de vaga casadas a uma entidade:", nrow(gc2), "\n")
    if (nrow(gc2)) print(gc2[, .N, by = classe][order(-N)])
  }
}

# 8c. consolidacao dos atos: terminal (falecimento > cassacao > renuncia) e nao terminal
atos <- rbindlist(list(
  if (nrow(gr))  gr[,  .(rid, id_noticia, data_ev, classe, trecho, url_ev, via = "titulo")] else NULL,
  if (nrow(gc2)) gc2[, .(rid, id_noticia, data_ev, classe, trecho, url_ev, via = "corpo")] else NULL),
  use.names = TRUE)
fwrite(atos, "output/verificacao/assembleias2_RJ_atos_noticiados.csv")
TERM <- c("falecimento", "cassacao", "renuncia")
at_t <- atos[classe %in% TERM]
at_t[, prio := match(classe, TERM)]
setorder(at_t, rid, prio, data_ev)
at_t <- at_t[!duplicated(at_t, by = "rid"),
             .(rid, ato_term = classe, ato_term_data = data_ev, ato_term_txt = trecho,
               ato_term_url = url_ev, ato_term_via = via)]
at_n <- atos[classe %in% c("licenca", "outro_definitivo", "outro_provisorio")]
at_n[, prio := match(classe, c("licenca", "outro_definitivo", "outro_provisorio"))]
setorder(at_n, rid, prio, -data_ev)
at_n <- at_n[!duplicated(at_n, by = "rid"),
             .(rid, ato_nt = classe, ato_nt_data = data_ev, ato_nt_txt = trecho,
               ato_nt_url = url_ev, ato_nt_via = via)]
at_p <- atos[classe == "posse"][order(rid, data_ev)][!duplicated(rid), .(rid, data_posse = data_ev)]
comp <- merge(comp, at_t, by = "rid", all.x = TRUE)
comp <- merge(comp, at_n, by = "rid", all.x = TRUE)
comp <- merge(comp, at_p, by = "rid", all.x = TRUE)

## ---------------------------------------------------------------- 9. forma de saida
comp[, leg_encerrada := LEG_ENCERRADA[legislatura]]
comp[, forma_saida := fcase(
  !is.na(ato_term),                                    ato_term,
  condicao == "suplente",                              "suplente_efetivado",
  sit_fim == "em_exercicio" &  leg_encerrada,          "fim_regular",
  sit_fim == "em_exercicio" & !leg_encerrada,          "nao_observado",
  sit_fim == "encerrou_antes" & ato_nt == "licenca",   "licenca",
  sit_fim == "encerrou_antes",                         "outro",
  is.na(sit_fim) & ato_nt == "licenca",                "licenca",
  is.na(sit_fim) & ato_nt == "outro_definitivo",       "outro",
  default = "nao_observado")]

# causa_original: o texto da fonte. Ato nomeado (renuncia, cassacao, falecimento, licenca) exige
# frase da Casa; forma derivada da estrutura (fim_regular, suplente_efetivado, outro) leva o
# rotulo estrutural da propria fonte quando ele existe.
rot_estrut <- function(sit, fontes, leg) fcase(
  sit == "em_exercicio"   & grepl("qs1", fontes), paste0("Em exercicio (ALERJ, Deputados/QuemSao, ", leg, "a Legislatura, situacaoDeputado=1)"),
  sit == "em_exercicio",                          paste0("Composicao ao fim da ", leg, "a Legislatura (PDF da Biblioteca da ALERJ)"),
  sit == "encerrou_antes",                        paste0("Exerceram o mandato (ALERJ, Deputados/QuemSao, ", leg, "a Legislatura, situacaoDeputado=2)"),
  default = NA_character_)
comp[, causa_original := fcase(
  !is.na(ato_term), ato_term_txt,
  forma_saida == "licenca", ato_nt_txt,
  condicao == "suplente" & em_bib_suplente == 1L,
    paste0("SUPLENTES QUE ASSUMIRAM (Biblioteca da ALERJ, ", legislatura, "a Legislatura)"),
  condicao == "suplente", rot_estrut(sit_fim, fontes, legislatura),
  default = rot_estrut(sit_fim, fontes, legislatura))]
comp[forma_saida == "outro" & !is.na(ato_nt_txt) & ato_nt != "outro_provisorio",
     causa_original := fifelse(is.na(causa_original), ato_nt_txt,
                               paste0(causa_original, " | ", ato_nt_txt))]

# datas: posse e termino publicados pela Casa; data de ato quando o ato e terminal
comp[, data_inicio_exercicio := fifelse(condicao == "titular", posse_legislatura, data_posse)]
comp[, data_fim_exercicio := fcase(
  !is.na(ato_term), ato_term_data,
  forma_saida == "fim_regular", termino_legislatura,
  default = NA_character_)]
comp[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio) &
     data_inicio_exercicio > data_fim_exercicio, data_inicio_exercicio := NA_character_]
comp[legislatura == "13" & condicao == "titular" & !is.na(url13), url := paste0(url, " ; ", url13)]
# a noticia so entra na procedencia da linha quando o texto dela foi usado
comp[, usou_nt := (forma_saida == "licenca" |
                   (forma_saida == "outro" & !is.na(ato_nt_txt) & ato_nt != "outro_provisorio")) &
                  !is.na(ato_nt_url)]
comp[!is.na(ato_term_url), url := paste0(url, " ; ", ato_term_url)]
comp[is.na(ato_term_url) & usou_nt, url := paste0(url, " ; ", ato_nt_url)]
comp[!is.na(ato_term) | usou_nt, fontes := paste0(fontes, "+alerj_noticias")]
comp[, sexo_fonte := fcase(grepl("^DEPUTADA$", toupper(fcoalesce(tratamento, ""))), "F",
                           grepl("^DEPUTA+DO$", toupper(fcoalesce(tratamento, ""))), "M",
                           default = NA_character_)]
cat("\nforma_saida por condicao:\n"); print(dcast(comp[, .N, by = .(condicao, forma_saida)],
                                                  forma_saida ~ condicao, value.var = "N", fill = 0L))

## ------------------------------------------------- 9b. eleito que nunca chegou a ser listado
# Quando um mandato do BOCEL nao aparece em NENHUMA fonte de composicao da legislatura e a Casa
# noticiou a morte da pessoa entre a eleicao e o primeiro mes de mandato, o que a ALERJ registra e
# que o eleito nao chegou a tomar posse. Sao os casos de Valdeci Paiva de Jesus (assassinado em
# 24/01/2003, uma semana antes da posse da 8a) e Wagner Montes (morto em 26/01/2019, seis dias
# antes da posse da 12a). A regra so alcanca quem a Casa nunca listou na legislatura.
sem_linha <- dep[!id_mandato %in% comp$id_mandato_bocel]
novas <- data.table()
if (nrow(sem_linha) && nrow(ntx)) {
  fonte_txt <- rbind(
    nt[,  .(id_noticia, data, orig = titulo, url)],
    ntx[, .(id_noticia, data, orig, url)], fill = TRUE)
  fonte_txt[, busca := gsub("[^A-Z ]", " ", stri_trans_general(toupper(orig), "Latin-ASCII"))]
  fonte_txt <- fonte_txt[nchar(busca) == nchar(orig) &
                         grepl("MORTE|MORREU|MORRE |FALEC|ASSASSINAD|LUTO|PESAR", busca)]
  sem_linha[, `:=`(a1 = norm_nome(nome_urna), a2 = norm_nome(nome_bocel))]
  for (k in seq_len(nrow(sem_linha))) {
    ini <- as.character(as.IDate(sem_linha$mandato_inicio[k]) - 120L)
    fim <- as.character(as.IDate(sem_linha$mandato_inicio[k]) + 30L)
    cand <- fonte_txt[data >= ini & data <= fim]
    if (!nrow(cand)) next
    for (nm in unique(c(sem_linha$a1[k], sem_linha$a2[k]))) {
      if (nchar(nm) < 8L || length(tok(nm)[[1]]) < 2L) next
      pat <- paste0("(^| )", gsub(" ", "[^A-Z]+", nm), "( |$)")
      hit <- cand[stri_detect_regex(busca, pat)]
      if (!nrow(hit)) next
      # a mencao de morte tem de estar JUNTO do nome, nao em qualquer lugar da materia
      hit[, pos1 := stri_locate_first_regex(busca, pat)[, 1]]
      hit[, pos2 := stri_locate_first_regex(busca, pat)[, 2]]
      hit[, ctx := stri_sub(busca, pmax(1L, pos1 - 120L), pmin(nchar(busca), pos2 + 120L))]
      hit <- hit[grepl("MORTE|MORREU|MORRE |FALEC|ASSASSINAD|LUTO|PESAR", ctx)]
      if (!nrow(hit)) next
      hit <- hit[order(data)][1]
      nome_na_casa <- trimws(gsub("[^[:alnum:] ]", " ", stri_sub(hit$orig, hit$pos1, hit$pos2)))
      novas <- rbind(novas, data.table(
        uf = UF, fonte = "alerj_noticias",
        legislatura = names(LEG_ANO)[match(sem_linha$ano_eleicao[k], LEG_ANO)],
        ano_eleicao = sem_linha$ano_eleicao[k], nome = nome_na_casa,
        nome_normalizado = norm_nome(nome_na_casa), nome_completo = NA_character_,
        data_nascimento = NA_character_, partido = NA_character_, condicao = "titular",
        data_inicio_exercicio = NA_character_, data_fim_exercicio = NA_character_,
        causa_original = trimws(gsub(" +", " ",
          stri_sub(hit$orig, pmax(1L, hit$pos1 - 120L), pmin(nchar(hit$orig), hit$pos2 + 120L)))),
        forma_saida = "nao_tomou_posse", id_pessoa_bocel = sem_linha$id_pessoa[k],
        id_mandato_bocel = sem_linha$id_mandato[k],
        metodo_pareamento = "noticia_de_obito_antes_da_posse", url = hit$url,
        id_fonte = as.character(hit$id_noticia), votos_fonte = NA_character_, sexo_fonte = NA_character_))
      break
    }
  }
}
cat("\neleitos sem linha de composicao com obito noticiado antes da posse:", nrow(novas), "\n")
if (nrow(novas)) print(novas[, .(legislatura, nome, causa_original = substr(causa_original, 1, 90))])
reg("nao_tomou_posse_por_obito", nrow(novas))

## ---------------------------------------------------------------- 10. saida
comp[, uf := UF]
comp[, fonte := paste0("alerj_", fontes)]
saida <- comp[, .(uf, fonte, legislatura, ano_eleicao, nome, nome_normalizado, nome_completo,
                  data_nascimento, partido, condicao, data_inicio_exercicio, data_fim_exercicio,
                  causa_original, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento,
                  url, id_fonte, votos_fonte = NA_character_, sexo_fonte)]
if (nrow(novas)) saida <- rbind(saida, novas[, names(saida), with = FALSE], use.names = TRUE)
saida <- saida[!duplicated(saida[, .(legislatura, nome_normalizado)])]
setorder(saida, ano_eleicao, condicao, nome_normalizado)

in_set(saida$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente", "nao_informado"), permitir_na = FALSE, nome = "condicao")
in_set(saida$sexo_fonte, c("M", "F"), permitir_na = TRUE, nome = "sexo_fonte")
em_faixa(saida$ano_eleicao, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
in_set(saida$legislatura, as.character(7:13), permitir_na = FALSE, nome = "legislatura")
checa_unica(as.data.frame(saida), c("legislatura", "nome_normalizado"))
stopifnot(all(saida$ano_eleicao == LEG_ANO[saida$legislatura]))
dts <- unlist(saida[, .(data_inicio_exercicio, data_fim_exercicio, data_nascimento)])
dts <- dts[!is.na(dts)]
stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", dts)))
stopifnot(saida[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio),
                all(data_inicio_exercicio <= data_fim_exercicio)])
# evidencia textual onde o verificador a exige
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
stopifnot(saida[forma_saida %in% TEXTUAL, all(!is.na(causa_original) & nzchar(causa_original))])
stopifnot(all(!is.na(saida$url) & nzchar(saida$url)))
# integridade referencial com o BOCEL
stopifnot(all(na.omit(saida$id_mandato_bocel) %in% mand$id_mandato))
stopifnot(!any(duplicated(na.omit(saida$id_mandato_bocel))))
vd <- merge(saida[!is.na(id_mandato_bocel), .(id_mandato_bocel, ano_eleicao)],
            mand[, .(id_mandato_bocel = id_mandato, ano_bocel = as.integer(ano_eleicao))], by = "id_mandato_bocel")
stopifnot(nrow(vd[ano_eleicao != ano_bocel]) == 0)
# data de fim dentro da janela do mandato no BOCEL (regra de R/verifica_assembleias_portais.R)
jw <- merge(saida[!is.na(id_mandato_bocel) & !is.na(data_fim_exercicio), .(id_mandato_bocel, data_fim_exercicio)],
            mand[, .(id_mandato_bocel = id_mandato, mandato_inicio, mandato_fim)], by = "id_mandato_bocel")
fora <- jw[as.IDate(data_fim_exercicio) < as.IDate(mandato_inicio) - 60L |
           as.IDate(data_fim_exercicio) > as.IDate(mandato_fim) + 45L]
cat("linhas pareadas com data de fim fora da janela do mandato:", nrow(fora), "\n")
if (nrow(fora)) print(fora)
stopifnot(nrow(fora) == 0L)

fwrite(saida, "data/assembleias2/RJ.csv", sep = ",", na = "NA", quote = TRUE, bom = FALSE)
cat("\ndata/assembleias2/RJ.csv:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

## ---------------------------------------------------------------- 11. cobertura e registro
par <- saida[!is.na(id_mandato_bocel)]
cob <- merge(dep[, .(n_bocel = .N), by = ano_eleicao],
             saida[, .(n_fonte = .N, n_titular = sum(condicao == "titular"),
                       n_suplente = sum(condicao == "suplente"),
                       n_pareados = uniqueN(na.omit(id_mandato_bocel))), by = ano_eleicao],
             by = "ano_eleicao", all = TRUE)
cob <- merge(cob, par[forma_saida != "nao_observado", .(n_mandato_com_forma = uniqueN(id_mandato_bocel)),
                      by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_mandato_com_forma), n_mandato_com_forma := 0L]
cob[, `:=`(taxa_pareamento = round(n_pareados / n_bocel, 4),
           taxa_forma = round(n_mandato_com_forma / n_bocel, 4))]
setorder(cob, ano_eleicao)
cat("\ncobertura por eleicao:\n"); print(cob)
fwrite(cob, "output/verificacao/assembleias2_RJ_cobertura.csv")
fwrite(saida[, .N, by = .(condicao, forma_saida)][order(condicao, -N)],
       "output/verificacao/assembleias2_RJ_forma_saida.csv")
fwrite(par[forma_saida != "nao_observado" & condicao == "titular",
           .(legislatura, nome, forma_saida, data_fim_exercicio, causa_original, url)][order(legislatura, nome)],
       "output/verificacao/assembleias2_RJ_eventos_titulares.csv")
fwrite(saida[is.na(id_mandato_bocel), .(legislatura, condicao, nome, partido, fonte)][order(legislatura, nome)],
       "output/verificacao/assembleias2_RJ_nao_pareadas.csv")
fwrite(dep[!id_mandato %in% saida$id_mandato_bocel,
           .(ano_eleicao, id_mandato, nome_bocel, nome_urna)][order(ano_eleicao, nome_bocel)],
       "output/verificacao/assembleias2_RJ_mandatos_sem_linha.csv")

reg("linhas", nrow(saida))
reg("legislaturas_lista", paste(sort(unique(as.integer(saida$legislatura))), collapse = ";"))
reg("anos_eleicao", paste(sort(unique(saida$ano_eleicao)), collapse = ";"))
reg("linhas_titular", saida[condicao == "titular", .N])
reg("linhas_suplente", saida[condicao == "suplente", .N])
reg("linhas_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("linhas_com_data_fim", saida[!is.na(data_fim_exercicio), .N])
reg("linhas_com_data_nascimento", saida[!is.na(data_nascimento), .N])
reg("linhas_com_sexo_fonte", saida[!is.na(sexo_fonte), .N])
reg("pareadas", nrow(par))
reg("mandatos_bocel_pareados", uniqueN(par$id_mandato_bocel))
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f", uniqueN(par$id_mandato_bocel), nrow(dep),
                                      uniqueN(par$id_mandato_bocel) / nrow(dep)))
reg("mandatos_com_forma_saida", uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel))
reg("taxa_forma_saida_global", sprintf("%d/%d=%.4f", uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel),
                                       nrow(dep), uniqueN(par[forma_saida != "nao_observado"]$id_mandato_bocel) / nrow(dep)))
reg("noticias_acervo_itens", nrow(nt))
reg("noticias_texto_integral", nrow(ntx))
reg("atos_noticiados_casados", nrow(atos))
reg("atos_terminais_casados", nrow(at_t))
for (i in seq_len(nrow(cob))) {
  reg(sprintf("pareamento_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
  reg(sprintf("forma_saida_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_mandato_com_forma[i], cob$n_bocel[i], cob$taxa_forma[i]))
}
fs <- saida[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fs))) reg(paste0("forma_", fs$forma_saida[i]), fs$N[i])
fsp <- par[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fsp))) reg(paste0("forma_pareada_", fsp$forma_saida[i]), fsp$N[i])
mp <- saida[!is.na(metodo_pareamento), .N, by = metodo_pareamento][order(-N)]
for (i in seq_len(nrow(mp))) reg(paste0("metodo_", mp$metodo_pareamento[i]), mp$N[i])
cat("\nmetodos de pareamento:\n"); print(mp)
cat("\nbuild_RJ: concluido —", format(Sys.time()), "\n")
sink()
