# monta_CE.R — exercicio de mandato de deputado estadual do Ceara (ALECE), 1998–2022,
#   a partir das fontes da propria Casa, com pareamento ao BOCEL.
#
# Duas fontes carregam o arquivo, e cada uma responde por uma coisa distinta:
#
#   1. ATAS DO PLENARIO (data_raw/assembleias2/CE/atas_presenca.csv). Toda ata abre com a
#      relacao nominal de quem compareceu. A mudanca dessa relacao de uma edicao para a
#      outra data a entrada e a saida de cada cadeira sem depender de ato publicado. O
#      acervo online nao se deixa filtrar por legislatura — a pagina /atas/legislaturas/<id>
#      devolve sempre a 31a — mas /atas/<n>/pdf responde para n muito abaixo do primeiro id
#      da 31a, e a varredura por id recupera a serie de 2003 em diante.
#   2. VOLUMES DO MEMORIAL DA ALECE (malce_pessoas.csv, malce_eventos.csv), um por
#      legislatura de 1998 a 2018, com o quadro de titulares e de suplentes convocados e as
#      frases que enunciam renuncia, cassacao, falecimento e licenca. A frase e a fonte
#      exata da data e da causa; a serie de presenca e a espinha do exercicio.
#
# Entrada:  data_raw/assembleias2/CE/{atas_presenca,atas_parse_log,malce_pessoas,
#             malce_eventos,parsed_portal,inventario,atas_indice}.csv
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/CE.csv
#           output/verificacao/asm2ce_*.csv
#           output/numeros_assinatura.txt (registrar_numero, prefixo asm2ce_)
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/monta_CE.R
set.seed(20260830)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
raw    <- file.path(root, "data_raw", "assembleias2", "CE")
outd   <- file.path(root, "data", "assembleias2")
verd   <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(root, "logs"), showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "monta_CE.R")
logf   <- file.path(root, "logs", "asm2ce_monta_CE.log")
sink(logf, split = TRUE)
cat("monta_CE.R —", format(Sys.time()), "\n")

UF    <- "CE"
CARGO <- 7L
HOJE  <- as.IDate(Sys.Date())
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento",
           "licenca", "nao_tomou_posse", "suplente_efetivado", "assumiu_titular",
           "outro", "nao_observado")
COLS  <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado",
           "nome_completo", "data_nascimento", "partido", "condicao",
           "data_inicio_exercicio", "data_fim_exercicio", "causa_original",
           "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
           "url", "id_fonte", "votos_fonte", "sexo_fonte")
# quadrienios da Alece
LEG <- data.table(
  legislatura = as.character(25:31),
  ano_eleicao = c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L, 2022L),
  posse   = as.IDate(c("1999-02-01", "2003-02-01", "2007-02-01", "2011-02-01",
                       "2015-02-01", "2019-02-01", "2023-02-01")),
  fim_leg = as.IDate(c("2003-01-31", "2007-01-31", "2011-01-31", "2015-01-31",
                       "2019-01-31", "2023-01-31", "2027-01-31")))
LEG[, em_curso := fim_leg > HOJE]

# Limiar de saida na serie de presenca. A ausencia de uma sessao nao e saida: o deputado
# falta. So conta como saida quem para de aparecer e nao volta ate o fim da legislatura,
# com folga bastante para nao confundir com falta prolongada. Os dois criterios sao
# cumulativos e estao registrados como parametro (a sensibilidade fica fora de cobertura).
MIN_SESSOES_APOS <- 40L
MIN_DIAS_APOS    <- 150L
MIN_SESSOES_LEG  <- 60L   # abaixo disso a serie e rala demais para sustentar rotulo
DIAS_TRECHO_FINAL <- 120L   # trecho final do quadrienio, onde se afere a permanencia
MIN_SESSOES_TRECHO_FINAL <- 5L  # sem sessoes nesse trecho, a serie nao atesta permanencia

norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
# tratamento e patente que a urna carrega e o plenario nem sempre repete
HONOR <- c("DR", "DRA", "PR", "PASTOR", "PASTORA", "DEP", "DEPUTADO", "DEPUTADA", "PROF",
           "PROFA", "PROFESSOR", "PROFESSORA", "SARGENTO", "SGT", "SOLDADO", "SD", "CABO",
           "CB", "CAPITAO", "CAP", "MAJOR", "CORONEL", "TENENTE", "DELEGADO", "DELEGADA",
           "AP", "APOSTOLO", "APOSTOLA", "BISPO", "IRMAO", "IRMA", "PADRE", "MISSIONARIO",
           "VEREADOR", "DOUTOR", "DOUTORA", "SENHOR", "SENHORA", "EX", "TIO", "TIA")
sem_honor <- function(x) {
  vapply(strsplit(norm_nome(x), " ", fixed = TRUE), function(v) {
    w <- v[!v %in% HONOR & nzchar(v)]
    paste(if (length(w)) w else v, collapse = " ")
  }, character(1))
}
toks <- function(x) strsplit(sem_honor(x), " ", fixed = TRUE)
nz <- function(x) !is.na(x) & nzchar(x)
iso <- function(x) fifelse(is.na(x), NA_character_, format(as.IDate(x), "%Y-%m-%d"))

## ---------------------------------------------------------------- 1. candidatos do TSE
cat("\n== 1. universo de candidatos do TSE (CE, cd_cargo 7)\n")
# quem e titular vem do proprio BOCEL, e nao da coluna de situacao do TSE: em 1998, 2006 e
# 2010 o cadastro de candidaturas marca menos de 46 como ELEITO (a eleicao por media
# aparece com outro rotulo), e o quadro da Casa tem 46 cadeiras em todo o periodo.
bocel_m <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("sq_candidato", "nr_candidato")))
bocel_p <- fread(file.path(root, "data", "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bocel_m[cd_cargo == CARGO & sg_uf == UF,
             .(id_mandato, id_pessoa, ano_eleicao, sq_candidato)]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome)], by = "id_pessoa")
dep[, `:=`(nome_bocel_n = norm_nome(nome_bocel), nome_bocel_s = sem_honor(nome_bocel))]
checa_unica(as.data.frame(dep), "id_mandato")
cat("mandatos do BOCEL (CE, cd_cargo 7):", nrow(dep), "\n")
cand <- rbindlist(lapply(LEG$ano_eleicao, function(a) {
  f <- file.path(root, "data_raw", "parquet", sprintf("cand_%d.parquet", a))
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO",
                                            "NM_TIPO_ELEICAO", "SQ_CANDIDATO",
                                            "NR_CANDIDATO", "NM_CANDIDATO",
                                            "NM_URNA_CANDIDATO", "DS_SIT_TOT_TURNO",
                                            "SG_PARTIDO", "DT_NASCIMENTO", "DS_GENERO")))
  x <- x[SG_UF == UF & as.integer(CD_CARGO) == CARGO &
           grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sq_candidato = as.character(SQ_CANDIDATO),
        nr_candidato = as.character(NR_CANDIDATO), nome_civil = NM_CANDIDATO,
        nome_urna = NM_URNA_CANDIDATO, situacao = DS_SIT_TOT_TURNO,
        partido_tse = SG_PARTIDO, nasc_tse = as.character(DT_NASCIMENTO),
        genero = DS_GENERO)]
}))
cand <- unique(cand, by = c("ano_eleicao", "sq_candidato"))
# o mesmo candidato aparece duas vezes quando ha substituicao de candidatura (a segunda
# linha vem com situacao #NULO); fica a linha valida, para o nome nao virar chave ambigua
cand[, ord := fifelse(grepl("^ELEITO", toupper(situacao)), 1L,
                      fifelse(grepl("NULO", toupper(situacao)), 3L, 2L))]
setorder(cand, ano_eleicao, ord, sq_candidato)
cand <- cand[, .SD[1], by = .(ano_eleicao, nome_civil)]
cand[, eleito := paste(ano_eleicao, sq_candidato) %in% paste(dep$ano_eleicao, dep$sq_candidato)]
cand[, `:=`(urna_n = norm_nome(nome_urna), civil_n = norm_nome(nome_civil),
            urna_s = sem_honor(nome_urna), civil_s = sem_honor(nome_civil))]
# o sq_candidato do TSE nao e unico entre anos nesta serie (tres valores se repetem entre
# 2010 e 2014), entao toda comparacao de "cadeira ja ocupada" carrega o ano junto
cand[, chave_pessoa := paste0(ano_eleicao, "_", sq_candidato)]
stopifnot(!anyDuplicated(cand$chave_pessoa))
print(cand[, .(candidatos = .N, eleitos = sum(eleito)), by = ano_eleicao][order(ano_eleicao)])
stopifnot(all(cand[, sum(eleito), by = ano_eleicao]$V1 == 46L))

## ---------------------------------------------------------------- 2. serie de presenca
cat("\n== 2. serie de presenca nas atas do plenario\n")
pres <- fread(file.path(raw, "atas_presenca.csv"), encoding = "UTF-8",
              colClasses = list(character = c("id_ata", "nome")))
plog <- fread(file.path(raw, "atas_parse_log.csv"), encoding = "UTF-8",
              colClasses = list(character = "id_ata"))
pres[, `:=`(legislatura = as.character(legislatura), data_sessao = as.IDate(data_sessao))]
pres <- pres[legislatura %in% LEG$legislatura]
sess <- unique(pres[, .(legislatura, id_ata, data_sessao)])
setorder(sess, legislatura, data_sessao, id_ata)
sess[, k := seq_len(.N), by = legislatura]
pres <- merge(pres, sess[, .(legislatura, id_ata, k)], by = c("legislatura", "id_ata"))
nsess <- sess[, .(n_sessoes = .N, prim_sessao = min(data_sessao),
                  ult_sessao = max(data_sessao)), by = legislatura]
print(merge(nsess, LEG[, .(legislatura, ano_eleicao)], by = "legislatura")[order(legislatura)])
cat("observacoes de presenca:", nrow(pres), "| nomes distintos:",
    uniqueN(pres[, .(legislatura, nome)]), "\n")

## ---------------------------------------------------------------- 3. Malce
cat("\n== 3. quadro e frases dos volumes do Memorial (Malce)\n")
ml <- fread(file.path(raw, "malce_pessoas.csv"), encoding = "UTF-8",
            colClasses = "character", na.strings = c("", "NA"))
ml[, legislatura := as.character(as.integer(legislatura))]
ml[, ano_eleicao := as.integer(ano_eleicao)]
mev <- fread(file.path(raw, "malce_eventos.csv"), encoding = "UTF-8",
             colClasses = "character", na.strings = c("", "NA"))
mev[, legislatura := as.character(as.integer(legislatura))]
print(ml[, .N, by = .(legislatura, secao)][order(legislatura, secao)])
print(mev[, .N, by = .(legislatura, forma)][order(legislatura)])

## ---------------------------------------------------------------- 4. portal atual (31a)
pt <- fread(file.path(raw, "parsed_portal.csv"), encoding = "UTF-8",
            colClasses = "character", na.strings = c("", "NA"))
setorder(pt, slug, data_observacao)
por <- pt[, .(condicao_portal = fifelse(any(condicao == "titular"), "titular", "suplente"),
              nome_portal = nome[.N], nome_completo_portal = {
                v <- nome_completo[nz(nome_completo)]
                if (length(v)) v[length(v)] else NA_character_},
              partido_portal = partido[.N],
              votos_portal = {v <- votos[nz(votos)]
                if (length(v)) v[length(v)] else NA_character_},
              licenciado_ultima = as.integer(licenciado[.N]),
              ultima_obs = data_observacao[.N],
              url_portal = {v <- url_ficha[nz(url_ficha)]
                if (length(v)) v[length(v)] else origem[.N]}), by = slug]
cat("\n== 4. portal /deputados (31a):", nrow(por), "fichas |",
    por[condicao_portal == "titular", .N], "titulares\n")

## ---------------------------------------------------------------- 5. resolucao de nomes
# O rol da ata usa o nome de plenario, que raramente e o nome civil e nem sempre e o nome
# de urna ("Pr. Alcides Fernandes" na urna, "Alcides Fernandes" no rol; "Julinho" na urna,
# "Julio Cesar Filho" no rol). As camadas vao da igualdade exata a comparacoes cada vez
# mais frouxas, e cada camada exige par unico dos dois lados, dentro do ano da eleicao.
cat("\n== 5. resolucao dos nomes do rol contra o cadastro do TSE\n")
rol <- unique(pres[, .(legislatura, nome)])
rol <- merge(rol, LEG[, .(legislatura, ano_eleicao)], by = "legislatura")
rol[, `:=`(rid = .I, nome_n = norm_nome(nome), nome_s = sem_honor(nome),
           sq_candidato = NA_character_, metodo_nome = NA_character_)]
rol[, n_obs := pres[, .N, by = .(legislatura, nome)][rol, on = .(legislatura, nome), N]]

casa_camada <- function(rol, pool, col_rol, col_pool, metodo) {
  a <- rol[is.na(sq_candidato) & nz(get(col_rol)),
           .(rid, ano_eleicao, chave = get(col_rol))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- pool[nz(get(col_pool)), .(sq_candidato, ano_eleicao, chave = get(col_pool))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  # muitos-para-um e o caso normal aqui: a mesma pessoa aparece no rol sob mais de uma
  # grafia ("Julinho" na urna, "Julio Cesar Filho" no plenario) e as duas tem de cair na
  # mesma cadeira, senao a serie se parte em duas e inventa saida no meio do mandato
  if (nrow(m)) rol[m, on = "rid", `:=`(sq_candidato = i.sq_candidato,
                                       metodo_nome = metodo)]
  cat(sprintf("  %-36s +%d\n", metodo, nrow(m)))
  invisible(rol)
}
# subconjunto de tokens: todo token do rol aparece no nome do cadastro
casa_subset <- function(rol, pool, col_pool, metodo) {
  a <- rol[is.na(sq_candidato)]
  if (!nrow(a)) return(invisible(rol))
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    ta <- toks(a$nome_s[i])[[1]]
    ta <- ta[nchar(ta) > 2]
    if (length(ta) < 1) return(NULL)
    p <- pool[ano_eleicao == a$ano_eleicao[i]]
    if (!nrow(p)) return(NULL)
    tp <- strsplit(p[[col_pool]], " ", fixed = TRUE)
    ok <- vapply(tp, function(v) all(ta %in% v), logical(1))
    if (sum(ok) != 1L) return(NULL)
    data.table(rid = a$rid[i], sq_candidato = p$sq_candidato[ok])
  }))
  if (!is.null(pares) && nrow(pares)) {
    if (nrow(pares)) rol[pares, on = "rid", `:=`(sq_candidato = i.sq_candidato,
                                                 metodo_nome = metodo)]
    cat(sprintf("  %-36s +%d\n", metodo, nrow(pares)))
  } else cat(sprintf("  %-36s +0\n", metodo))
  invisible(rol)
}
# grafia divergente por um ou dois caracteres ("Almie Bie" por "Almir Bie"), so contra quem
# ja aparece na legislatura por outra grafia e so com par unico
casa_fuzzy <- function(rol, pool, metodo, dmax = 2L) {
  a <- rol[is.na(sq_candidato) & nchar(nome_s) >= 8]
  if (!nrow(a)) return(invisible(rol))
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    p <- pool[ano_eleicao == a$ano_eleicao[i]]
    if (!nrow(p)) return(NULL)
    d <- pmin(utils::adist(a$nome_s[i], p$urna_s)[1, ],
              utils::adist(a$nome_s[i], p$civil_s)[1, ])
    j <- which(d <= dmax)
    if (length(j) != 1L) return(NULL)
    data.table(rid = a$rid[i], sq_candidato = p$sq_candidato[j], dist = d[j])
  }))
  if (!is.null(pares) && nrow(pares)) {
    pares <- merge(pares, rol[, .(rid, ano_eleicao)], by = "rid")
    pares[, chave_pessoa := paste0(ano_eleicao, "_", sq_candidato)]
    pares <- pares[, if (.N == 1L) .SD, by = chave_pessoa]
    pares <- pares[!chave_pessoa %in% paste0(rol$ano_eleicao, "_", rol$sq_candidato)]
    if (nrow(pares)) rol[pares, on = "rid", `:=`(sq_candidato = i.sq_candidato,
                                                 metodo_nome = metodo)]
    cat(sprintf("  %-36s +%d\n", metodo, nrow(pares)))
  } else cat(sprintf("  %-36s +0\n", metodo))
  invisible(rol)
}

elei <- cand[eleito == TRUE]
for (etapa in list(list(elei, "eleito"), list(cand, "candidato"))) {
  pool <- etapa[[1]]; tag <- etapa[[2]]
  casa_camada(rol, pool, "nome_n", "urna_n",  paste0("urna_exata_", tag))
  casa_camada(rol, pool, "nome_s", "urna_s",  paste0("urna_sem_tratamento_", tag))
  casa_camada(rol, pool, "nome_n", "civil_n", paste0("civil_exato_", tag))
  casa_subset(rol, pool, "urna_s",  paste0("tokens_na_urna_", tag))
  casa_subset(rol, pool, "civil_s", paste0("tokens_no_nome_civil_", tag))
  casa_fuzzy (rol, pool, paste0("grafia_proxima_", tag))
}
# ------- ponte nome de plenario -> nome civil, montada com as fontes da propria Casa
# A ficha do portal (31a) e o volume do Memorial (30a) publicam o par "nome parlamentar /
# nome civil". A ponte casa o nome civil com o cadastro do TSE, inclusive por grafia
# proxima, porque as duas fontes divergem em detalhe ("Julio Cesar Costa Lima Filho" na
# ficha, "Julio Cesar Costa Lima Junior" no TSE).
casa_civil <- function(civis, ano, pool) {
  p <- pool[ano_eleicao == ano]
  if (!nrow(p)) return(rep(NA_character_, length(civis)))
  out <- rep(NA_character_, length(civis))
  cn <- norm_nome(civis); cs <- sem_honor(civis)
  j <- match(cn, p$civil_n); out[!is.na(j)] <- p$sq_candidato[j[!is.na(j)]]
  falta <- which(is.na(out) & nzchar(cs))
  for (i in falta) {
    ta <- strsplit(cs[i], " ", fixed = TRUE)[[1]]; ta <- ta[nchar(ta) > 2]
    tp <- strsplit(p$civil_s, " ", fixed = TRUE)
    ok <- vapply(tp, function(v) length(ta) > 1L && all(ta %in% v), logical(1))
    if (sum(ok) == 1L) { out[i] <- p$sq_candidato[ok]; next }
    d <- utils::adist(cs[i], p$civil_s)[1, ]
    k <- which(d <= 7L)
    if (length(k) == 1L) out[i] <- p$sq_candidato[k]
  }
  out
}
ponte <- rbindlist(list(
  por[nz(nome_completo_portal), .(ano_eleicao = 2022L, parl = nome_portal,
                                  civil = nome_completo_portal)],
  ml[nz(nome_parlamentar) & nz(nome_completo),
     .(ano_eleicao, parl = nome_parlamentar, civil = nome_completo)]))
ponte[, sq_candidato := casa_civil(civil, ano_eleicao[1], cand), by = ano_eleicao]
ponte <- unique(ponte[!is.na(sq_candidato), .(ano_eleicao, parl_n = norm_nome(parl),
                                              sq_candidato)])
ponte <- ponte[, if (uniqueN(sq_candidato) == 1L) .SD[1], by = .(ano_eleicao, parl_n)]
a <- rol[is.na(sq_candidato), .(rid, ano_eleicao, parl_n = nome_n)]
m <- merge(a, ponte, by = c("ano_eleicao", "parl_n"))
if (nrow(m)) rol[m, on = "rid", `:=`(sq_candidato = i.sq_candidato,
                                     metodo_nome = "ponte_nome_parlamentar")]
cat(sprintf("  %-36s +%d\n", "ponte_nome_parlamentar", nrow(m)))
# a ponte tambem vale entre legislaturas: o mesmo apelido serve a mesma pessoa
ponte_g <- merge(ponte[, .(ano_eleicao, parl_n, sq_candidato)],
                 unique(cand[, .(ano_eleicao, sq_candidato, civil_n)]),
                 by = c("ano_eleicao", "sq_candidato"))
ponte_g <- unique(ponte_g[, .(parl_n, civil_n)])[nz(civil_n)]
ponte_g <- ponte_g[, if (uniqueN(civil_n) == 1L) .SD[1], by = parl_n]
a <- rol[is.na(sq_candidato), .(rid, ano_eleicao, parl_n = nome_n)]
m <- merge(a, ponte_g, by = "parl_n")
if (nrow(m)) {
  m[, sq2 := casa_civil(civil_n, ano_eleicao[1], cand), by = ano_eleicao]
  m <- m[!is.na(sq2)]
  if (nrow(m)) rol[m, on = "rid", `:=`(sq_candidato = i.sq2,
                                       metodo_nome = "ponte_apelido_entre_legislaturas")]
}
cat(sprintf("  %-36s +%d\n", "ponte_apelido_entre_legislaturas", nrow(m)))
# ------- metade na urna, metade no nome civil ("Martinha Brandao": MARTINHA vem da urna,
# BRANDAO vem do nome civil); exige par unico no ano
a <- rol[is.na(sq_candidato) & nchar(nome_s) >= 8]
if (nrow(a)) {
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    ta <- strsplit(a$nome_s[i], " ", fixed = TRUE)[[1]]; ta <- ta[nchar(ta) > 3]
    if (length(ta) < 2L) return(NULL)
    p <- cand[ano_eleicao == a$ano_eleicao[i]]
    tu <- strsplit(p$urna_s, " ", fixed = TRUE); tc <- strsplit(p$civil_s, " ", fixed = TRUE)
    ok <- vapply(seq_len(nrow(p)), function(j)
      all(ta %in% c(tu[[j]], tc[[j]])), logical(1))
    if (sum(ok) != 1L) return(NULL)
    data.table(rid = a$rid[i], sq_candidato = p$sq_candidato[ok])
  }))
  if (!is.null(pares) && nrow(pares)) {
    rol[pares, on = "rid", `:=`(sq_candidato = i.sq_candidato,
                                metodo_nome = "tokens_urna_mais_civil")]
    cat(sprintf("  %-36s +%d\n", "tokens_urna_mais_civil", nrow(pares)))
  } else cat(sprintf("  %-36s +0\n", "tokens_urna_mais_civil"))
}
# ------- variante de grafia de um nome ja resolvido NA MESMA legislatura ("Almie Bie" por
# "Almir Bie"); nao alcanca quem ainda nao esta na Casa, e exige alvo unico
for (lg in sort(unique(rol$legislatura))) {
  a <- rol[legislatura == lg & is.na(sq_candidato) & nchar(nome_s) >= 8]
  b <- unique(rol[legislatura == lg & !is.na(sq_candidato), .(nome_s, sq_candidato)])
  if (!nrow(a) || !nrow(b)) next
  for (i in seq_len(nrow(a))) {
    d <- utils::adist(a$nome_s[i], b$nome_s)[1, ]
    k <- which(d <= 2L)
    if (length(k) != 1L) next
    rol[rid == a$rid[i], `:=`(sq_candidato = b$sq_candidato[k],
                              metodo_nome = "variante_de_grafia")]
  }
}
cat(sprintf("  %-36s +%d\n", "variante_de_grafia",
            rol[metodo_nome == "variante_de_grafia", .N]))

res <- rol[, .(n_nomes = .N, resolvidos = sum(!is.na(sq_candidato)),
               obs = sum(n_obs), obs_resolvidas = sum(n_obs[!is.na(sq_candidato)])),
           by = legislatura][order(legislatura)]
cat("\nresolucao por legislatura:\n"); print(res)
sobra <- rol[is.na(sq_candidato)][order(-n_obs)]
cat("nomes do rol sem par (top 15 por presenca):\n")
print(head(sobra[, .(legislatura, nome, n_obs)], 15))
fwrite(rol[order(legislatura, -n_obs)], file.path(verd, "asm2ce_resolucao_nomes.csv"))

## ---------------------------------------------------------------- 6. serie -> exercicio
cat("\n== 6. exercicio derivado da serie de presenca\n")
pr <- merge(pres, rol[!is.na(sq_candidato), .(legislatura, nome, sq_candidato)],
            by = c("legislatura", "nome"))
serie <- pr[, .(n_sessoes_presente = .N, k_prim = min(k), k_ult = max(k),
                data_prim = min(data_sessao), data_ult = max(data_sessao)),
            by = .(legislatura, sq_candidato)]
serie <- merge(serie, nsess, by = "legislatura")
serie <- merge(serie, LEG, by = "legislatura")
# Dois testes diretos, e nao um o complemento do outro, porque o acervo de atas tem
# buracos: quem some no meio de uma serie esburacada nao e nem saida comprovada nem
# permanencia comprovada, e fica em nao_observado.
#   'ficou ate o fim'  = aparece em ao menos uma sessao do TRECHO FINAL do quadrienio
#   'saiu na serie'    = para de aparecer, nao volta, e sobram sessoes bastante depois
# A referencia de tempo e a ULTIMA SESSAO OBSERVADA, nao o fim do quadrienio: onde o
# acervo para no meio, medir contra o fim inventaria saida para quem so nao tem ata.
trecho <- merge(sess, LEG[, .(legislatura, fim_leg)], by = "legislatura")
trecho <- trecho[data_sessao >= fim_leg - DIAS_TRECHO_FINAL,
                 .(n_sessoes_trecho_final = .N), by = legislatura]
pfim <- merge(pr, LEG[, .(legislatura, fim_leg)], by = "legislatura")
pfim <- pfim[data_sessao >= fim_leg - DIAS_TRECHO_FINAL,
             .(n_presencas_trecho_final = .N), by = .(legislatura, sq_candidato)]
serie <- merge(serie, trecho, by = "legislatura", all.x = TRUE)
serie <- merge(serie, pfim, by = c("legislatura", "sq_candidato"), all.x = TRUE)
serie[is.na(n_sessoes_trecho_final), n_sessoes_trecho_final := 0L]
serie[is.na(n_presencas_trecho_final), n_presencas_trecho_final := 0L]
serie[, `:=`(sessoes_apos = n_sessoes - k_ult,
             dias_apos = as.integer(ult_sessao - data_ult),
             serie_densa = n_sessoes >= MIN_SESSOES_LEG)]
serie[, serie_alcanca_fim := n_sessoes_trecho_final >= MIN_SESSOES_TRECHO_FINAL]
serie[, ficou_ate_o_fim := serie_densa & serie_alcanca_fim & n_presencas_trecho_final > 0L]
serie[, saiu_na_serie := serie_densa & n_presencas_trecho_final == 0L &
        sessoes_apos >= MIN_SESSOES_APOS & dias_apos >= MIN_DIAS_APOS]
cat("pessoas na serie:", nrow(serie), "| saida detectada:", serie[, sum(saiu_na_serie)],
    "| presente ate o fim:", serie[, sum(ficou_ate_o_fim)], "\n")
print(serie[, .(pessoas = .N, saiu = sum(saiu_na_serie), ate_o_fim = sum(ficou_ate_o_fim),
                n_sessoes = n_sessoes[1],
                sessoes_no_trecho_final = n_sessoes_trecho_final[1]),
            by = legislatura][order(legislatura)])

## ---------------------------------------------------------------- 7. atos do Malce
# A frase so vira rotulo se (a) nomear o ato, (b) trazer data dentro do quadrienio e
# (c) a serie NAO mostrar a pessoa de volta ao plenario depois do ato. Licenca com retorno
# nao e saida, e por isso cai fora — o que a fonte diz fica gravado em causa_original.
cat("\n== 7. atos enunciados nos volumes do Memorial\n")
mev[, `:=`(sujeito_n = norm_nome(sujeito), sujeito_s = sem_honor(sujeito),
           civil_n = norm_nome(sujeito_civil), civil_s = sem_honor(sujeito_civil))]
mev <- merge(mev, LEG[, .(legislatura, ano_eleicao_leg = ano_eleicao)], by = "legislatura")
liga_ato <- function(mev, pool, col_mev, col_pool, metodo) {
  a <- mev[is.na(sq_candidato) & nz(get(col_mev)),
           .(eid, ano_eleicao = ano_eleicao_leg, chave = get(col_mev))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- pool[nz(get(col_pool)), .(sq_candidato, ano_eleicao, chave = get(col_pool))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  if (nrow(m)) mev[m, on = "eid", `:=`(sq_candidato = i.sq_candidato, metodo_ato = metodo)]
  cat(sprintf("  %-30s +%d\n", metodo, nrow(m)))
  invisible(mev)
}
mev[, `:=`(eid = .I, sq_candidato = NA_character_, metodo_ato = NA_character_)]
liga_ato(mev, cand, "civil_n",   "civil_n", "ato_civil_exato")
liga_ato(mev, cand, "sujeito_n", "civil_n", "ato_nome_x_civil")
liga_ato(mev, cand, "sujeito_n", "urna_n",  "ato_nome_x_urna")
liga_ato(mev, cand, "sujeito_s", "urna_s",  "ato_nome_x_urna_sem_tratamento")
liga_ato(mev, cand, "civil_s",   "civil_s", "ato_civil_sem_tratamento")
cat("atos ligados ao cadastro:", mev[!is.na(sq_candidato), .N], "de", nrow(mev), "\n")

# a serie mostra a pessoa de volta depois do ato?
mev[, data_ato := as.IDate(fifelse(nz(data_evento), data_evento, NA_character_))]
volta <- merge(mev[!is.na(sq_candidato)], pr[, .(legislatura, sq_candidato, data_sessao)],
               by = c("legislatura", "sq_candidato"), allow.cartesian = TRUE)
volta <- volta[!is.na(data_ato) & data_sessao > data_ato + 30L,
               .(sessoes_depois_do_ato = .N), by = eid]
mev <- merge(mev, volta, by = "eid", all.x = TRUE)
mev[is.na(sessoes_depois_do_ato), sessoes_depois_do_ato := 0L]
# sem data exata o retorno se afere pelo ano do ato
mev <- merge(mev, pr[, .(legislatura, sq_candidato, data_sessao)],
             by = c("legislatura", "sq_candidato"), all.x = TRUE, allow.cartesian = TRUE)
mev <- mev[, .(sessoes_depois_do_ano = sum(!is.na(data_sessao) &
                 data_sessao > as.IDate(paste0(as.integer(ano_evento) + 1L, "-06-30")))),
           by = setdiff(names(mev), "data_sessao")]
mev[, retornou := (nz(data_evento) & sessoes_depois_do_ato >= 10L) |
      (!nz(data_evento) & sessoes_depois_do_ano >= 10L)]
cat("atos com retorno ao plenario depois do ato (nao sao saida):",
    mev[retornou == TRUE, .N], "\n")
print(mev[, .N, by = .(forma, retornou)][order(forma)])
fwrite(mev[order(legislatura, sujeito)], file.path(verd, "asm2ce_atos_malce.csv"))

## ---------------------------------------------------------------- 8. universo de pessoas
cat("\n== 8. montagem do universo por legislatura\n")
# titulares: os 46 eleitos de cada ano, sempre presentes no arquivo
univ <- merge(cand[eleito == TRUE, .(ano_eleicao, sq_candidato)],
              LEG[, .(legislatura, ano_eleicao)], by = "ano_eleicao")
univ[, condicao := "titular"]
# suplentes: quem aparece na serie de presenca sem ter sido eleito naquele ano
sup <- serie[, .(legislatura, sq_candidato)]
sup <- sup[!univ, on = c("legislatura", "sq_candidato")]
sup[, condicao := "suplente"]
univ <- rbindlist(list(univ[, .(legislatura, sq_candidato, condicao)], sup), use.names = TRUE)
univ <- unique(univ, by = c("legislatura", "sq_candidato"))
univ <- merge(univ, LEG, by = "legislatura")
univ <- merge(univ, cand[, .(ano_eleicao, sq_candidato, nome_civil, nome_urna, partido_tse,
                             nasc_tse, genero, situacao)],
              by = c("ano_eleicao", "sq_candidato"))
univ <- merge(univ, serie[, .(legislatura, sq_candidato, n_sessoes_presente, data_prim,
                              data_ult, saiu_na_serie, ficou_ate_o_fim, serie_densa,
                              serie_alcanca_fim, n_sessoes, n_presencas_trecho_final)],
              by = c("legislatura", "sq_candidato"), all.x = TRUE)
univ[is.na(n_sessoes_presente), `:=`(n_sessoes_presente = 0L, saiu_na_serie = FALSE,
                                     ficou_ate_o_fim = FALSE, serie_densa = FALSE,
                                     serie_alcanca_fim = FALSE,
                                     n_presencas_trecho_final = 0L)]
print(univ[, .N, by = .(legislatura, condicao)][order(legislatura, condicao)])

# ------- dados do Memorial, colados ao cadastro do TSE do ano da eleicao
# O volume indexa pelo nome civil (25a a 29a) ou pelo nome parlamentar (30a); as duas
# grafias sao testadas contra o nome civil e o nome de urna do cadastro, sempre com par
# unico dos dois lados dentro do ano.
ml[, `:=`(civil_n = norm_nome(nome_completo),
          parl_n = norm_nome(fifelse(nz(nome_parlamentar), nome_parlamentar, nome_toc)),
          civil_s = sem_honor(nome_completo),
          parl_s = sem_honor(fifelse(nz(nome_parlamentar), nome_parlamentar, nome_toc)))]
mlx <- merge(ml, LEG[, .(legislatura, ano_eleicao_leg = ano_eleicao)], by = "legislatura")
mlx[, `:=`(mid = .I, sq_candidato = NA_character_, metodo_ml = NA_character_)]
liga_ml <- function(mlx, pool, col_ml, col_pool, metodo) {
  a <- mlx[is.na(sq_candidato) & nz(get(col_ml)),
           .(mid, ano_eleicao = ano_eleicao_leg, chave = get(col_ml))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- pool[nz(get(col_pool)), .(sq_candidato, ano_eleicao, chave = get(col_pool))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  m <- m[!paste0(ano_eleicao, "_", sq_candidato) %in%
           paste0(mlx$ano_eleicao_leg, "_", mlx$sq_candidato)]
  if (nrow(m)) mlx[m, on = "mid", `:=`(sq_candidato = i.sq_candidato, metodo_ml = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
  invisible(mlx)
}
univ[, `:=`(civil_n = norm_nome(nome_civil), urna_n = norm_nome(nome_urna),
            civil_s = sem_honor(nome_civil), urna_s = sem_honor(nome_urna))]
liga_ml(mlx, cand, "civil_n", "civil_n", "malce_civil_x_civil")
liga_ml(mlx, cand, "parl_n",  "urna_n",  "malce_parlamentar_x_urna")
liga_ml(mlx, cand, "civil_s", "civil_s", "malce_civil_sem_tratamento")
liga_ml(mlx, cand, "parl_s",  "urna_s",  "malce_parlamentar_sem_tratamento")
liga_ml(mlx, cand, "civil_n", "urna_n",  "malce_civil_x_urna")
liga_ml(mlx, cand, "parl_n",  "civil_n", "malce_parlamentar_x_civil")
# tokens: todo sobrenome do nome civil do volume aparece no nome civil do cadastro
liga_ml_tokens <- function(mlx, pool, metodo) {
  a <- mlx[is.na(sq_candidato) & nz(civil_s)]
  if (!nrow(a)) return(invisible(mlx))
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    ta <- strsplit(a$civil_s[i], " ", fixed = TRUE)[[1]]
    ta <- ta[nchar(ta) > 2]
    if (length(ta) < 2) return(NULL)
    ja <- mlx[ano_eleicao_leg == a$ano_eleicao_leg[i] & !is.na(sq_candidato), sq_candidato]
    p <- pool[ano_eleicao == a$ano_eleicao_leg[i] & !sq_candidato %in% ja]
    if (!nrow(p)) return(NULL)
    tp <- strsplit(p$civil_s, " ", fixed = TRUE)
    ok <- vapply(tp, function(v) all(ta %in% v) || all(v[nchar(v) > 2] %in% ta), logical(1))
    if (sum(ok) != 1L) return(NULL)
    data.table(mid = a$mid[i], sq_candidato = p$sq_candidato[ok])
  }))
  if (!is.null(pares) && nrow(pares)) {
    pares <- merge(pares, mlx[, .(mid, ano_eleicao_leg)], by = "mid")
    pares <- pares[, if (.N == 1L) .SD, by = .(ano_eleicao_leg, sq_candidato)]
    pares <- pares[!paste0(ano_eleicao_leg, "_", sq_candidato) %in%
                     paste0(mlx$ano_eleicao_leg, "_", mlx$sq_candidato)]
    if (nrow(pares)) mlx[pares, on = "mid", `:=`(sq_candidato = i.sq_candidato,
                                                 metodo_ml = metodo)]
    cat(sprintf("  %-34s +%d\n", metodo, nrow(pares)))
  } else cat(sprintf("  %-34s +0\n", metodo))
  invisible(mlx)
}
# grafia proxima, so entre o quadro de titulares do volume e os 46 eleitos do ano, que sao
# duas listas de mesmo tamanho e mesmo conteudo; exige melhor par mutuo
liga_ml_fuzzy <- function(mlx, pool, secao_alvo, metodo, dmax = 8L) {
  for (leg in sort(unique(mlx$legislatura))) {
    a <- mlx[legislatura == leg & secao == secao_alvo & is.na(sq_candidato)]
    if (!nrow(a)) next
    ano_leg <- mlx[legislatura == leg, ano_eleicao_leg[1]]
    p <- pool[ano_eleicao == ano_leg &
                !sq_candidato %in% mlx[ano_eleicao_leg == ano_leg & !is.na(sq_candidato),
                                       sq_candidato]]
    if (secao_alvo == "titular") p <- p[eleito == TRUE]
    if (!nrow(p)) next
    d <- utils::adist(a$civil_s, p$civil_s)
    for (i in seq_len(nrow(a))) {
      j <- which.min(d[i, ])
      if (d[i, j] > dmax) next
      if (which.min(d[, j]) != i) next          # melhor par nos dois sentidos
      if (sum(d[i, ] == d[i, j]) > 1L) next
      mlx[mid == a$mid[i], `:=`(sq_candidato = p$sq_candidato[j], metodo_ml = metodo)]
      d[, j] <- .Machine$integer.max
    }
  }
  cat(sprintf("  %-34s +%d\n", metodo, mlx[metodo_ml == metodo, .N]))
  invisible(mlx)
}
liga_ml_tokens(mlx, cand, "malce_tokens_no_nome_civil")
liga_ml_fuzzy(mlx, cand, "titular", "malce_grafia_proxima_titular")
liga_ml_fuzzy(mlx, cand, "suplente", "malce_grafia_proxima_suplente", dmax = 6L)
cat("linhas do Memorial ligadas ao cadastro:", mlx[!is.na(sq_candidato), .N], "de",
    nrow(mlx), "\n")
if (mlx[is.na(sq_candidato), .N])
  print(mlx[is.na(sq_candidato), .(legislatura, secao, nome_toc, nome_completo)])
fwrite(mlx[order(legislatura, secao, nome_toc),
           .(legislatura, secao, nome_toc, nome_completo, nome_parlamentar, partido,
             data_nascimento, sq_candidato, metodo_ml, id_fonte)],
       file.path(verd, "asm2ce_ligacao_malce.csv"))
print(mlx[, .(ligadas = sum(!is.na(sq_candidato)), n = .N), by = .(legislatura, secao)][
  order(legislatura, secao)])
# suplente do Memorial que nao esta na serie entra no universo assim mesmo
novo <- mlx[!is.na(sq_candidato) & secao == "suplente",
            .(legislatura, sq_candidato, condicao = "suplente")]
novo <- unique(novo)[!univ, on = c("legislatura", "sq_candidato")]
if (nrow(novo)) {
  novo <- merge(novo, LEG, by = "legislatura")
  novo <- merge(novo, cand[, .(ano_eleicao, sq_candidato, nome_civil, nome_urna, partido_tse,
                               nasc_tse, genero, situacao)],
                by = c("ano_eleicao", "sq_candidato"))
  novo[, `:=`(n_sessoes_presente = 0L, data_prim = as.IDate(NA), data_ult = as.IDate(NA),
              saiu_na_serie = FALSE, ficou_ate_o_fim = FALSE, serie_densa = FALSE,
              serie_alcanca_fim = FALSE, n_presencas_trecho_final = 0L,
              n_sessoes = NA_integer_, civil_n = norm_nome(nome_civil),
              urna_n = norm_nome(nome_urna), civil_s = sem_honor(nome_civil),
              urna_s = sem_honor(nome_urna))]
  univ <- rbindlist(list(univ, novo), use.names = TRUE, fill = TRUE)
  cat("suplentes so do Memorial acrescentados ao universo:", nrow(novo), "\n")
}
univ <- merge(univ, mlx[!is.na(sq_candidato),
                        .(legislatura, sq_candidato, secao_malce = secao,
                          partido_malce = partido, nasc_malce = data_nascimento,
                          nome_parl_malce = nome_parlamentar, url_malce = url,
                          id_fonte_malce = id_fonte)],
              by = c("legislatura", "sq_candidato"), all.x = TRUE)
univ <- unique(univ, by = c("legislatura", "sq_candidato"))

# ------- portal atual, so para a 31a
univ <- merge(univ, cand[ano_eleicao == 2022L, .(ano_eleicao, sq_candidato,
                                                 civil_pt = civil_n)],
              by = c("ano_eleicao", "sq_candidato"), all.x = TRUE)
por[, civil_pt := norm_nome(nome_completo_portal)]
pu <- por[nz(civil_pt), .(civil_pt, nome_portal, partido_portal, votos_portal,
                          licenciado_ultima, url_portal, condicao_portal)]
pu <- pu[, if (.N == 1L) .SD, by = civil_pt]
univ <- merge(univ, pu, by = "civil_pt", all.x = TRUE)
univ[legislatura != "31", `:=`(nome_portal = NA_character_, partido_portal = NA_character_,
                               votos_portal = NA_character_, url_portal = NA_character_,
                               licenciado_ultima = NA_integer_,
                               condicao_portal = NA_character_)]

## ---------------------------------------------------------------- 9. forma de saida
cat("\n== 9. forma de saida\n")
ato <- mev[!is.na(sq_candidato),
           .(legislatura, sq_candidato, forma_ato = forma, data_ato,
             causa_ato = frase, url_ato = url, id_fonte_ato = id_fonte,
             ano_ato = ano_evento, so_pleito = as.integer(data_do_pleito_apenas),
             licenca_temporaria = as.integer(licenca_temporaria),
             sessoes_depois_do_ato)]
PRIOR <- c(falecimento = 1L, cassacao = 2L, renuncia = 3L, licenca = 4L)
ato[, pr := PRIOR[forma_ato]]
setorder(ato, legislatura, sq_candidato, pr, -data_ato, na.last = TRUE)
ato <- ato[, .SD[1], by = .(legislatura, sq_candidato)]
univ <- merge(univ, ato, by = c("legislatura", "sq_candidato"), all.x = TRUE)

# O ato so vale como saida em duas condicoes. Primeira: a propria fonte nao pode enquadrar
# a licenca como temporaria — "por 120 dias", "por quatro meses", "para tratar de assuntos
# particulares" descrevem suspensao do exercicio, e nao fim do mandato, e valem mesmo onde
# nao ha serie que mostre a volta. Segunda: a serie de presenca nao pode desmentir — quem
# segue no plenario ate o fim do quadrienio nao saiu, seja qual for a frase da biografia.
# Onde nao ha serie, e a licenca nao se diz temporaria, o texto e a unica evidencia e vale.
univ[is.na(sessoes_depois_do_ato), sessoes_depois_do_ato := 0L]
univ[, ato_vale := nz(forma_ato) & !(licenca_temporaria %in% 1L) & fcase(
  serie_densa == FALSE, TRUE,
  saiu_na_serie == TRUE, TRUE,
  ficou_ate_o_fim == TRUE, FALSE,
  default = sessoes_depois_do_ato < 10L)]
cat("atos enunciados:", univ[nz(forma_ato), .N], "| validos como saida:",
    univ[ato_vale == TRUE, .N], "| desmentidos pela serie:",
    univ[nz(forma_ato) & ato_vale == FALSE, .N], "\n")
if (univ[nz(forma_ato) & ato_vale == FALSE, .N])
  print(univ[nz(forma_ato) & ato_vale == FALSE,
             .(legislatura, nome_urna, forma_ato, ano_ato, licenca_temporaria,
               n_sessoes_presente, sessoes_depois_do_ato, saiu_na_serie,
               ficou_ate_o_fim)])
univ[, forma_saida := fcase(
  # 1. ato enunciado pela fonte, com data no quadrienio, que a serie nao desmente
  ato_vale == TRUE, forma_ato,
  # 2. suplente convocado: a ocupacao da cadeira e o proprio fato observado
  condicao == "suplente", "suplente_efetivado",
  # 3. a serie mostra a cadeira vaga antes do fim, sem ato encontrado
  saiu_na_serie == TRUE, "outro",
  # 4. presente no trecho final de uma legislatura ja encerrada
  ficou_ate_o_fim == TRUE & em_curso == FALSE, "fim_regular",
  # 5. legislatura em curso, ou serie rala demais
  default = "nao_observado")]
univ[, causa_original := fifelse(ato_vale == TRUE, causa_ato, NA_character_)]
univ[, data_inicio_exercicio := fcase(
  !is.na(data_prim) & condicao == "titular" & data_prim <= posse + 60L, iso(posse),
  !is.na(data_prim), iso(data_prim),
  condicao == "titular" & nz(id_fonte_malce), iso(posse),
  default = NA_character_)]
# a data do ato so entra quando a serie nao a contradiz; caso contrario vale a ultima
# presenca observada, que e o que a Casa de fato registrou
univ[, data_ato_coerente := !is.na(data_ato) & so_pleito %in% c(0L, NA) &
       (is.na(data_ult) | (data_ato >= data_prim - 30L & data_ato <= data_ult + 60L))]
univ[is.na(data_ato_coerente), data_ato_coerente := FALSE]
univ[, data_fim_exercicio := fcase(
  forma_saida %in% c("renuncia", "falecimento", "cassacao", "licenca") & data_ato_coerente,
    iso(data_ato),
  forma_saida %in% c("renuncia", "falecimento", "cassacao", "licenca") & !is.na(data_ult),
    iso(data_ult),
  forma_saida %in% c("renuncia", "falecimento", "cassacao", "licenca") & !is.na(data_ato),
    iso(data_ato),
  forma_saida == "outro", iso(data_ult),
  forma_saida == "fim_regular", iso(fim_leg),
  forma_saida == "suplente_efetivado" & ficou_ate_o_fim == TRUE & em_curso == FALSE,
    iso(fim_leg),
  forma_saida == "suplente_efetivado" & saiu_na_serie == TRUE, iso(data_ult),
  default = NA_character_)]
univ[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio) &
       data_fim_exercicio < data_inicio_exercicio,
     data_fim_exercicio := data_inicio_exercicio]
print(univ[, .N, by = .(condicao, forma_saida)][order(condicao, -N)])

# Conferencia do criterio da secao SUPLENTES do volume ("a presenca cronologica dos
# suplentes que assumiram ao curso do quadrienio", declarado no proprio livro): nas
# legislaturas com serie de presenca, quem aparece no plenario sem ter sido eleito tem
# de estar na secao. Se aparecesse gente fora dela, o rotulo suplente_efetivado nao se
# sustentaria como leitura da fonte.
sup_ml <- mlx[!is.na(sq_candidato) & secao == "suplente", .(legislatura, sq_candidato)]
sup_serie <- univ[condicao == "suplente" & n_sessoes_presente > 0L,
                  .(legislatura, sq_candidato)]
fora <- sup_serie[!sup_ml, on = c("legislatura", "sq_candidato")]
fora <- fora[legislatura %in% mlx$legislatura]
dentro <- merge(sup_ml, univ[, .(legislatura, sq_candidato, n_sessoes_presente)],
                by = c("legislatura", "sq_candidato"))
cat("\nsecao SUPLENTES do volume x serie de presenca: ", nrow(sup_ml),
    " suplentes no volume, ", dentro[n_sessoes_presente > 0L, .N],
    " com presenca em ata; ", nrow(fora),
    " suplentes vistos em ata fora da secao\n", sep = "")

## ---------------------------------------------------------------- 10. tabela de saida
univ[, fonte := {
  f <- rep("", .N)
  f <- fifelse(n_sessoes_presente > 0L, "alece_atas_plenario", f)
  f <- fifelse(nz(id_fonte_malce), fifelse(nzchar(f), paste0(f, "+malce_volume"),
                                           "malce_volume"), f)
  f <- fifelse(nz(url_portal), fifelse(nzchar(f), paste0(f, "+alece_portal_deputados"),
                                       "alece_portal_deputados"), f)
  fifelse(nzchar(f), f, "tse_eleitos")
}]
setorder(pr, legislatura, sq_candidato, data_sessao)
ultata <- pr[, .(url_ata = paste0("https://www.al.ce.gov.br/atas/", id_ata[.N], "/pdf")),
             by = .(legislatura, sq_candidato)]
univ <- merge(univ, ultata, by = c("legislatura", "sq_candidato"), all.x = TRUE)
# volume do Memorial da legislatura, para a linha que so tem o cadastro do TSE
vol_leg <- unique(mlx[!is.na(url), .(legislatura, url_volume = url)])[
  , .SD[1], by = legislatura]
univ <- merge(univ, vol_leg, by = "legislatura", all.x = TRUE)
univ[, url := fcase(ato_vale == TRUE & nz(url_ato), url_ato,
                    nz(url_ata), url_ata,
                    nz(url_malce), url_malce,
                    nz(url_portal), url_portal,
                    nz(url_volume), url_volume,
                    default = "https://www.al.ce.gov.br/atas/legislaturas")]
univ[, id_fonte := fcase(
  ato_vale == TRUE & nz(id_fonte_ato), id_fonte_ato,
  nz(id_fonte_malce), id_fonte_malce,
  default = paste0("tse_", sq_candidato))]
univ[, nome := fifelse(nz(nome_portal), nome_portal,
                       fifelse(nz(nome_parl_malce), nome_parl_malce, nome_urna))]
univ[, nome_completo := nome_civil]
univ[, partido := fcase(nz(partido_portal), partido_portal,
                        nz(partido_malce), partido_malce,
                        default = partido_tse)]
univ[, data_nascimento := fcase(
  grepl("^\\d{4}-\\d{2}-\\d{2}$", nasc_tse), substr(nasc_tse, 1, 10),
  grepl("^\\d{2}/\\d{2}/\\d{4}$", nasc_tse),
    paste0(substr(nasc_tse, 7, 10), "-", substr(nasc_tse, 4, 5), "-", substr(nasc_tse, 1, 2)),
  nz(nasc_malce), nasc_malce,
  default = NA_character_)]
univ[, sexo_fonte := fcase(grepl("^MASC", toupper(genero)), "M",
                           grepl("^FEM", toupper(genero)), "F",
                           default = NA_character_)]
univ[, votos_fonte := votos_portal]
univ[, nome_normalizado := norm_nome(nome)]

## ---------------------------------------------------------------- 11. pareamento ao BOCEL
cat("\n== 11. pareamento ao BOCEL\n")
univ[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_,
            metodo_pareamento = NA_character_, rid2 = .I)]
# regra 1, propria desta UF e a mais forte: o sq_candidato do TSE ja identifica o mandato
m <- merge(univ[condicao == "titular", .(rid2, sq_candidato, ano_eleicao)],
           dep[, .(sq_candidato, ano_eleicao, id_mandato, id_pessoa)],
           by = c("sq_candidato", "ano_eleicao"))
univ[m, on = "rid2", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                          metodo_pareamento = "sq_candidato_tse")]
cat(sprintf("  %-34s +%d\n", "sq_candidato_tse", nrow(m)))
# regras de R/13_exercicio_assembleias.R, na mesma ordem, para o que sobrar
parear <- function(univ, dep, col_u, col_d, metodo) {
  a <- univ[is.na(id_mandato) & nz(get(col_u)), .(rid2, ano_eleicao, chave = get(col_u))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- dep[nz(get(col_d)), .(id_mandato, id_pessoa, ano_eleicao, chave = get(col_d))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  m <- m[!id_mandato %in% univ$id_mandato]
  if (nrow(m)) univ[m, on = "rid2", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                                         metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
  invisible(univ)
}
parear(univ, dep, "civil_n", "nome_bocel_n", "nome_completo_x_nome_bocel")
parear(univ, dep, "civil_s", "nome_bocel_s", "nome_completo_sem_tratamento")
# id_pessoa para suplentes: nome civil unico entre as pessoas com mandato na UF
pess_uf <- merge(bocel_m[sg_uf == UF, .(id_pessoa)],
                 bocel_p[, .(id_pessoa, nome_norm = norm_nome(nome))], by = "id_pessoa")
pess_uf <- unique(pess_uf)[, if (uniqueN(id_pessoa) == 1L) .SD[1], by = nome_norm]
np <- merge(univ[is.na(id_pessoa) & nz(civil_n), .(rid2, nome_norm = civil_n)], pess_uf,
            by = "nome_norm")
if (nrow(np)) univ[np, on = "rid2", `:=`(id_pessoa = i.id_pessoa,
                                         metodo_pareamento = fifelse(
                                           is.na(metodo_pareamento),
                                           "pessoa_nome_completo_uf", metodo_pareamento))]
cat(sprintf("  %-34s +%d\n", "pessoa_nome_completo_uf", nrow(np)))

cob <- merge(dep[, .(n_bocel = .N), by = ano_eleicao],
             univ[!is.na(id_mandato), .(n_pareados = uniqueN(id_mandato),
                                        n_com_forma = uniqueN(
                                          id_mandato[forma_saida != "nao_observado"])),
                  by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_pareados), `:=`(n_pareados = 0L, n_com_forma = 0L)]
cob[, `:=`(taxa_pareamento = round(n_pareados / n_bocel, 4),
           taxa_forma = round(n_com_forma / n_bocel, 4))]
cob <- merge(cob, LEG[, .(ano_eleicao, legislatura)], by = "ano_eleicao")
setcolorder(cob, c("legislatura", "ano_eleicao")); setorder(cob, ano_eleicao)
cat("\ncobertura por legislatura:\n"); print(cob)
fwrite(cob, file.path(verd, "asm2ce_pareamento_legislatura.csv"))

## ---------------------------------------------------------------- 12. gravacao
setorder(univ, ano_eleicao, condicao, nome_normalizado, na.last = TRUE)
saida <- univ[, .(uf = UF, fonte, legislatura, ano_eleicao, nome, nome_normalizado,
                  nome_completo, data_nascimento, partido, condicao,
                  data_inicio_exercicio, data_fim_exercicio, causa_original, forma_saida,
                  id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato,
                  metodo_pareamento, url, id_fonte, votos_fonte, sexo_fonte)]
saida[, causa_original := gsub("\\s+", " ", causa_original)]
stopifnot(identical(names(saida), COLS))
in_set(saida$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente"), permitir_na = FALSE, nome = "condicao")
em_faixa(saida$ano_eleicao, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
em_faixa(as.integer(substr(saida$data_nascimento, 1, 4)), 1900, 2005, permitir_na = TRUE,
         nome = "ano_nascimento")
em_faixa(as.integer(substr(saida$data_inicio_exercicio, 1, 4)), 1999, 2027,
         permitir_na = TRUE, nome = "ano_inicio_exercicio")
em_faixa(as.integer(substr(saida$data_fim_exercicio, 1, 4)), 1999, 2027,
         permitir_na = TRUE, nome = "ano_fim_exercicio")
checa_unica(as.data.frame(saida), c("legislatura", "nome_normalizado"))
dup <- saida[!is.na(id_mandato_bocel), .(n = uniqueN(nome_normalizado)), by = id_mandato_bocel][n > 1]
stopifnot(nrow(dup) == 0)
chk <- merge(saida[!is.na(id_mandato_bocel), .(id_mandato_bocel, ano_eleicao)],
             bocel_m[, .(id_mandato_bocel = id_mandato, ano_bocel = ano_eleicao, cd_cargo, sg_uf)],
             by = "id_mandato_bocel")
stopifnot(nrow(chk) == saida[!is.na(id_mandato_bocel), .N], all(chk$ano_bocel == chk$ano_eleicao),
          all(chk$cd_cargo == CARGO), all(chk$sg_uf == UF))
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
stopifnot(saida[forma_saida %in% TEXTUAL, all(nz(causa_original))])
stopifnot(all(nz(saida$url)))
fwrite(saida, file.path(outd, "CE.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
cat("\ndata/assembleias2/CE.csv:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

fs <- saida[, .N, by = .(condicao, forma_saida)][order(condicao, -N)]
cat("\nforma de saida por condicao:\n"); print(fs)
fwrite(fs, file.path(verd, "asm2ce_forma_saida.csv"))
fl <- saida[, .N, by = .(legislatura, forma_saida)][order(legislatura, -N)]
fwrite(fl, file.path(verd, "asm2ce_forma_por_legislatura.csv"))
# ponte de chaves para o verificador reencontrar a serie de presenca de cada linha
fwrite(univ[, .(legislatura, nome_normalizado, sq_candidato, id_mandato_bocel = id_mandato,
                condicao, forma_saida, n_sessoes_presente, data_prim, data_ult,
                saiu_na_serie, ficou_ate_o_fim, serie_densa, serie_alcanca_fim,
                n_presencas_trecho_final)][
                  order(legislatura, nome_normalizado)],
       file.path(verd, "asm2ce_chaves.csv"))

## ---------------------------------------------------------------- 13. registro
reg <- function(k, v) registrar_numero(paste0("asm2ce_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")
inv <- fread(file.path(raw, "inventario.csv"), encoding = "UTF-8")
reg("n_fontes_sondadas", nrow(inv))
reg("n_fontes_viaveis", inv[viavel == "sim", .N])
reg("n_atas_lidas", nrow(plog))
reg("n_atas_com_rol", plog[n_nomes > 0, .N])
reg("n_atas_rol_bate_total", plog[casou == 1, .N])
reg("n_observacoes_presenca", nrow(pres))
reg("n_observacoes_presenca_resolvidas", nrow(pr))
reg("n_sessoes_por_legislatura",
    paste(nsess[order(legislatura), sprintf("%s:%d", legislatura, n_sessoes)], collapse = ";"))
reg("n_nomes_no_rol", nrow(rol))
reg("n_nomes_resolvidos", rol[!is.na(sq_candidato), .N])
reg("taxa_resolucao_nomes", sprintf("%d/%d=%.4f", rol[!is.na(sq_candidato), .N], nrow(rol),
                                    rol[!is.na(sq_candidato), .N] / nrow(rol)))
reg("taxa_resolucao_observacoes", sprintf("%d/%d=%.4f", nrow(pr), nrow(pres),
                                          nrow(pr) / nrow(pres)))
reg("n_atas_por_legislatura",
    paste(plog[nz(as.character(legislatura)) & n_nomes > 0,
               .N, by = legislatura][order(legislatura),
               sprintf("%s:%d", legislatura, N)], collapse = ";"))
reg("n_atas_vazias_no_acervo", plog[vazia == 1, .N])
reg("n_atas_com_rol_sem_data", plog[n_nomes > 0 & !nz(as.character(data_sessao)), .N])
reg("anos_sem_ata_util",
    paste(setdiff(1999:2026,
                  sort(unique(as.integer(substr(plog[nz(as.character(data_sessao)) &
                                                       n_nomes > 0, data_sessao], 1, 4))))),
          collapse = ";"))
reg("n_malce_pessoas", nrow(ml))
reg("n_malce_pessoas_ligadas", mlx[!is.na(sq_candidato), .N])
reg("n_atos_malce", nrow(mev))
reg("n_atos_malce_ligados", mev[!is.na(sq_candidato), .N])
reg("n_atos_malce_com_retorno", mev[retornou == TRUE, .N])
reg("n_atos_validos_como_saida", univ[ato_vale == TRUE, .N])
reg("n_atos_desmentidos_pela_serie",
    univ[nz(forma_ato) & ato_vale == FALSE & !(licenca_temporaria %in% 1L), .N])
reg("n_licencas_temporarias_pela_fonte", univ[licenca_temporaria %in% 1L, .N])
reg("n_suplentes_no_volume", nrow(sup_ml))
reg("n_suplentes_do_volume_com_presenca", dentro[n_sessoes_presente > 0L, .N])
reg("n_suplentes_em_ata_fora_da_secao_do_volume", nrow(fora))
reg("limiar_sessoes_apos", MIN_SESSOES_APOS)
reg("limiar_dias_apos", MIN_DIAS_APOS)
reg("limiar_dias_trecho_final", DIAS_TRECHO_FINAL)
reg("limiar_sessoes_no_trecho_final", MIN_SESSOES_TRECHO_FINAL)
reg("n_linhas", nrow(saida))
reg("n_linhas_titular", saida[condicao == "titular", .N])
reg("n_linhas_suplente", saida[condicao == "suplente", .N])
reg("n_legislaturas_cobertas", uniqueN(saida$legislatura))
reg("n_mandatos_bocel_ce_cargo7", nrow(dep))
reg("n_mandatos_bocel_pareados", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]))
reg("n_linhas_pareadas", saida[!is.na(id_mandato_bocel), .N])
reg("n_linhas_com_id_pessoa", saida[!is.na(id_pessoa_bocel), .N])
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f", cob[, sum(n_pareados)], cob[, sum(n_bocel)],
                                      cob[, sum(n_pareados) / sum(n_bocel)]))
reg("n_mandatos_com_forma_observada",
    uniqueN(saida[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", id_mandato_bocel]))
reg("taxa_forma_observada",
    sprintf("%d/%d=%.4f",
            uniqueN(saida[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", id_mandato_bocel]),
            nrow(dep),
            uniqueN(saida[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", id_mandato_bocel]) /
              nrow(dep)))
for (i in seq_len(nrow(cob))) {
  reg(sprintf("taxa_pareamento_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
  reg(sprintf("taxa_forma_observada_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_com_forma[i], cob$n_bocel[i], cob$taxa_forma[i]))
}
for (m in sort(unique(na.omit(saida$metodo_pareamento))))
  reg(paste0("n_pareados_metodo_", m), saida[metodo_pareamento == m, .N])
for (i in seq_len(nrow(fs)))
  reg(paste0("n_", fs$condicao[i], "_forma_", fs$forma_saida[i]), fs$N[i])
fst <- saida[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fst))) reg(paste0("n_forma_saida_", fst$forma_saida[i]), fst$N[i])
reg("n_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("n_com_data_fim", saida[!is.na(data_fim_exercicio), .N])
reg("n_com_causa_original", saida[!is.na(causa_original), .N])
reg("n_com_data_nascimento", saida[!is.na(data_nascimento), .N])
reg("n_com_partido", saida[!is.na(partido), .N])
cat("\nmonta_CE: concluido —", format(Sys.time()), "\n")
sink()
