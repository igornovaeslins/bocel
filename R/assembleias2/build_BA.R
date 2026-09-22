# build_BA.R — exercicio de mandato dos deputados estaduais da Bahia (ALBA) e pareamento ao BOCEL.
#
# Fonte: portal da Assembleia Legislativa da Bahia (www.al.ba.gov.br), duas camadas:
#   (a) lista de deputados por legislatura (14a a 19a em /deputados/ex-deputados-estaduais/
#       legislatura/{n} e 20a em /deputados/legislatura-atual): quem ocupou a cadeira em cada
#       legislatura, com nome parlamentar e partido, sem datas;
#   (b) ficha individual (/deputados/ex-deputado-estadual/{id}): nome civil completo, data de
#       nascimento, sexo e o historico narrativo "Mandato Eletivo", mantido pelo Departamento de
#       Pesquisa da casa, que registra os periodos (1999-2003 etc.) e os eventos de saida
#       ("renunciou em 30/12/2004", "efetivou-se em 2 de janeiro de 2001, na vaga do deputado X",
#       "licenciou-se de 13/11/2008 a 31/03/2010", "assumiu o mandato de 03 a 31 janeiro de 2007").
#   (c) lista dos 63 deputados em exercicio hoje (/deputados/deputados-estaduais), usada apenas
#       para separar, na 20a legislatura (em curso), quem esta em exercicio de quem ja saiu.
#
# Entrada:  data_raw/assembleias2/BA/{listas,perfis,trechos_mandato,em_exercicio}.csv
#           (coleta em python/assembleias2/fetch_alba_BA.py, parsing bruto em parse_alba_BA.py)
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/BA.csv (21 colunas do padrao data/exercicio_assembleias.csv)
#           data_raw/assembleias2/BA/inventario.csv
#           output/verificacao/asm2ba_*.csv
#           output/numeros_assinatura.txt (registrar_numero, prefixo asm2ba_)
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/build_BA.R
set.seed(20260829)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/assembleias2/build_BA.R"
RAW  <- "data_raw/assembleias2/BA"
OUTD <- "data/assembleias2"
VERD <- "output/verificacao"
dir.create(OUTD, showWarnings = FALSE, recursive = TRUE)
dir.create(VERD, showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE)
logf <- file("logs/assembleias2_build_BA.log", open = "wt"); sink(logf, split = TRUE)
cat("build_BA.R —", format(Sys.time()), "\n")

HOJE  <- as.IDate("2026-08-29")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
# legislatura -> eleicao de origem e janela de exercicio (a 14a legislatura comeca em 1999)
LEG <- data.table(legislatura = as.character(14:20), ano_eleicao = seq(1998L, 2022L, 4L))
LEG[, `:=`(ini = as.IDate(sprintf("%d-01-01", ano_eleicao + 1L)),
           fim = as.IDate(sprintf("%d-12-31", ano_eleicao + 4L)))]

reg <- function(k, v) registrar_numero(paste0("asm2ba_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")
norm_nome <- function(x) {                    # mesma normalizacao de R/13_exercicio_assembleias.R
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
sa <- function(x) tolower(stri_trans_general(as.character(x), "Latin-ASCII"))

## ---------------------------------------------------------------- 1. entrada
listas <- fread(file.path(RAW, "listas.csv"), colClasses = "character", encoding = "UTF-8")
perfis <- fread(file.path(RAW, "perfis.csv"), colClasses = "character", encoding = "UTF-8")
trechos <- fread(file.path(RAW, "trechos_mandato.csv"), colClasses = "character", encoding = "UTF-8")
exerc  <- fread(file.path(RAW, "em_exercicio.csv"), colClasses = "character", encoding = "UTF-8")
cat("listas:", nrow(listas), "| perfis:", nrow(perfis), "| trechos:", nrow(trechos),
    "| em exercicio hoje:", nrow(exerc), "\n")

base <- merge(listas, LEG, by = "legislatura", all.x = TRUE)
stopifnot(!anyNA(base$ano_eleicao))
base <- merge(base, perfis[, .(id_fonte, nome_completo, nascimento_br, sexo, mandato_eletivo)],
              by = "id_fonte", all.x = TRUE)
base[, data_nascimento := {
  m <- stri_match_first_regex(nascimento_br, "^(\\d{2})/(\\d{2})/(\\d{4})$")
  fifelse(is.na(m[, 1]), NA_character_, sprintf("%s-%s-%s", m[, 4], m[, 3], m[, 2]))
}]
base[, sexo_fonte := fcase(sa(sexo) %like% "^masc", "M", sa(sexo) %like% "^femin", "F",
                           default = NA_character_)]
base[, em_exercicio_hoje := id_fonte %in% exerc$id_fonte]
base[nome_completo == "", nome_completo := NA_character_]

## ---------------------------------------------------------------- 2. trecho da narrativa por legislatura
# um par (id_fonte, legislatura) pode ter mais de um periodo citado; concatena na ordem do texto
tr <- trechos[cargo_inferido %chin% c("estadual", "estadual_suplente") & legislatura != ""]
tr <- tr[legislatura %chin% LEG$legislatura]
tr <- tr[, .(prefixo = paste(prefixo, collapse = " || "),
             trecho  = paste(trecho,  collapse = " || "),
             suplente_narrativa = any(cargo_inferido == "estadual_suplente")),
         by = .(id_fonte, legislatura)]
base <- merge(base, tr, by = c("id_fonte", "legislatura"), all.x = TRUE)
base[is.na(suplente_narrativa), suplente_narrativa := FALSE]
cat("linhas da lista com trecho da narrativa:", base[!is.na(trecho), .N], "de", nrow(base), "\n")

## ---------------------------------------------------------------- 3. limpeza do trecho
# eventos atribuidos a TERCEIROS ("na vaga do deputado X", "com a renuncia do deputado Y",
# "substituindo o deputado Z") nao sao evento de saida desta pessoa: saem antes de classificar.
limpar_terceiro <- function(x) {
  y <- x
  padroes <- c(
    "n[ao]s? vagas? d[oe][a-z]*\\s+[^,;.|]*",
    "no lugar d[oe][a-z]*\\s+[^,;.|]*",
    "em substitui[cç][aã]o a[oa]?\\s+[^,;.|]*",
    "substituindo\\s+[^,;.|]*",
    "(com|ap[oó]s|em raz[aã]o d[ao]|devido [aà]|por conta d[ao])\\s+a?\\s*(ren[uú]ncia|morte|falecimento|cassa[cç][aã]o|licen[cç]a|afastamento|posse)\\s+d[oe][a-z]*\\s+[^,;.|]*",
    "assumiu (interinamente )?o governo do estado[^,;.|]*",
    "assumiu interinamente[^,;.|]*",
    "com a convoca[cç][aã]o d[oe][a-z]*\\s+[^,;.|]*")
  for (p in padroes) y <- stri_replace_all_regex(y, p, " ", opts_regex = stri_opts_regex(case_insensitive = TRUE))
  gsub("\\s+", " ", trimws(y))
}
base[, trecho_limpo := limpar_terceiro(fifelse(is.na(trecho), "", trecho))]
base[, txt := sa(trecho_limpo)]   # so o trecho desta legislatura
base[, ctx := sa(paste(fifelse(is.na(prefixo), "", prefixo), trecho_limpo))]  # com o prefixo que abre o periodo

## ---------------------------------------------------------------- 4. datas citadas no trecho
MESES <- c(jan = 1, fev = 2, mar = 3, abr = 4, mai = 5, jun = 6, jul = 7, ago = 8,
           set = 9, out = 10, nov = 11, dez = 12)
mes_num <- function(x) {
  x <- substr(sa(x), 1, 3)
  unname(MESES[x])
}
iso <- function(y, m, d) fifelse(is.na(y) | is.na(m) | is.na(d), NA_character_,
                                 sprintf("%04d-%02d-%02d", y, m, d))
# devolve, para o texto t, a data completa (dd/mm/aaaa ou "d de mes de aaaa") que aparece
# depois da posicao p; NA se so houver mes/ano
data_apos <- function(t, p) {
  vapply(seq_along(t), function(i) {
    if (is.na(p[i]) || p[i] < 0) return(NA_character_)
    s <- substr(t[i], p[i], nchar(t[i]))
    m1 <- stri_match_first_regex(s, "(\\d{1,2})\\s*[/.]\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{4})")
    m2 <- stri_match_first_regex(sa(s), "(\\d{1,2})\\s*[º°]?\\s*(?:de\\s*|[/.]\\s*)(jan|fev|mar|abr|mai|jun|jul|ago|set|out|nov|dez)[a-z]*\\.?\\s*(?:de\\s*|[/.]\\s*)?(\\d{4})")
    p1 <- if (!is.na(m1[1, 1])) stri_locate_first_fixed(s, m1[1, 1])[1, 1] else NA_integer_
    p2 <- if (!is.na(m2[1, 1])) stri_locate_first_fixed(sa(s), m2[1, 1])[1, 1] else NA_integer_
    if (!is.na(p1) && (is.na(p2) || p1 <= p2))
      return(iso(as.integer(m1[1, 4]), as.integer(m1[1, 3]), as.integer(m1[1, 2])))
    if (!is.na(p2))
      return(iso(as.integer(m2[1, 4]), mes_num(m2[1, 3]), as.integer(m2[1, 2])))
    NA_character_
  }, character(1))
}
pos_regex <- function(t, p) {
  loc <- stri_locate_first_regex(t, p, opts_regex = stri_opts_regex(case_insensitive = TRUE))
  as.integer(loc[, 1])
}
# intervalo explicito "de D1 a D2" (duas datas completas) ou "de D1 a D2 <mes> de <ano>"
intervalo <- function(t) {
  m <- stri_match_first_regex(sa(t),
      "de\\s*(\\d{1,2})\\s*[º°]?\\s*a\\s*(\\d{1,2})\\s*[º°]?\\s*(?:de\\s*)?(jan|fev|mar|abr|mai|jun|jul|ago|set|out|nov|dez)[a-z]*\\.?\\s*(?:de\\s*)?(\\d{4})")
  ini <- iso(as.integer(m[, 5]), mes_num(m[, 4]), as.integer(m[, 2]))
  fim <- iso(as.integer(m[, 5]), mes_num(m[, 4]), as.integer(m[, 3]))
  # "de 05/01/2009 a 30/03/2010", "entre 10/02/2023 e 06/04/2026"
  m2 <- stri_match_first_regex(t,
      "(\\d{1,2})\\s*[/.]\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{4})\\s*(?:a|at[ée]|e|-)\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{4})")
  ini2 <- iso(as.integer(m2[, 4]), as.integer(m2[, 3]), as.integer(m2[, 2]))
  fim2 <- iso(as.integer(m2[, 7]), as.integer(m2[, 6]), as.integer(m2[, 5]))
  # "... ate 29 de mar. 2022" (so o fim)
  m3 <- stri_match_first_regex(sa(t),
      "at[ée]\\s*(\\d{1,2})\\s*[º°]?\\s*(?:de\\s*|[/.]\\s*)(jan|fev|mar|abr|mai|jun|jul|ago|set|out|nov|dez)[a-z]*\\.?\\s*(?:de\\s*)?(\\d{4})")
  fim3 <- iso(as.integer(m3[, 4]), mes_num(m3[, 3]), as.integer(m3[, 2]))
  m4 <- stri_match_first_regex(t, "at[ée]\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{1,2})\\s*[/.]\\s*(\\d{4})")
  fim4 <- iso(as.integer(m4[, 4]), as.integer(m4[, 3]), as.integer(m4[, 2]))
  list(ini = fcoalesce(ini, ini2), fim = fcoalesce(fim, fim2, fim3, fim4))
}
iv <- intervalo(base$trecho_limpo)
base[, `:=`(iv_ini = iv$ini, iv_fim = iv$fim)]

## ---------------------------------------------------------------- 5. condicao e forma de saida
# cues de evento, com a posicao no trecho limpo: vale o ULTIMO evento citado
CUES <- list(
  cassacao    = "cassa|perdeu o mandato|perda d[eo] mandato|deixou o mandato|decis[aã]o do tribunal|declarada pelo tse|pelo tribunal superior eleitoral",
  falecimento = "faleceu|falecimento|morreu|in memoriam|\\bmorte\\b",
  renuncia    = "renunci",
  afastamento = "afastou-se|afastamento|pediu afastamento|exonerad",
  licenca     = "licenciou-se|licen[cç]a|licenciad",
  posse_sup   = "efetivou-se|efetivado|assumiu o mandato|assumiu o cargo|assumiu na vaga|assumiu de|tomou posse|empossad")
for (k in names(CUES)) base[, (paste0("p_", k)) := pos_regex(trecho_limpo, CUES[[k]])]
pcols <- paste0("p_", names(CUES))
base[, evento := {
  m <- as.matrix(.SD)
  apply(m, 1, function(v) if (all(is.na(v))) NA_character_ else names(CUES)[which.max(replace(v, is.na(v), -1))])
}, .SDcols = pcols]
base[, pos_evento := {
  m <- as.matrix(.SD)
  apply(m, 1, function(v) if (all(is.na(v))) NA_integer_ else max(v, na.rm = TRUE))
}, .SDcols = pcols]
base[, pos_posse := p_posse_sup]
base[, reassumiu := grepl("reassumi|retornou ao (mandato|exerc)|retornou ao parlamento", txt)]
base[, efetivou := grepl("efetivou-se|efetivad", txt)]

# condicao: (i) narrativa diz suplente; (ii) narrativa registra posse em vaga/efetivacao;
#           (iii) pareado a mandato de eleito no BOCEL -> titular; (iv) narrativa diz eleito/reeleito
base[, cond_narrativa := fcase(
  suplente_narrativa, "suplente",
  grepl("\\bsuplente\\b", sa(prefixo)), "suplente",
  grepl("efetivou-se|assumiu (o mandato|o cargo|na vaga)", txt) & !grepl("eleit[oa] deputad", sa(prefixo)), "suplente",
  grepl("eleit[oa]s?\\s+(deputad|para|constituinte)|deputad[oa] estadual|deputad[oa]s estaduais|reeleit", sa(prefixo)), "titular",
  default = NA_character_)]

## ---------------------------------------------------------------- 6. BOCEL: eleitos cd_cargo 7 na BA
bocel_m <- fread("data/mandatos.csv", na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("sq_candidato", "nr_candidato")))
bocel_p <- fread("data/pessoas.csv", na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bocel_m[cd_cargo == 7L & sg_uf == "BA",
             .(id_mandato, id_pessoa, ano_eleicao, sg_uf, cd_cargo, sq_candidato, nr_candidato)]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome, dt_nascimento = as.character(dt_nascimento))],
             by = "id_pessoa")
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
urna <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE),
                         function(f) {
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                            "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_TIPO_ELEICAO")))
  x <- x[SG_UF == "BA" & as.integer(CD_CARGO) == 7L & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sg_uf = SG_UF, cd_cargo = as.integer(CD_CARGO),
        nr_candidato = as.character(NR_CANDIDATO), sq_candidato = as.character(SQ_CANDIDATO),
        nome_urna = NM_URNA_CANDIDATO)]
}))
urna <- urna[!duplicated(urna[, .(ano_eleicao, sg_uf, cd_cargo, nr_candidato, sq_candidato)])]
dep <- merge(dep, urna, by = c("ano_eleicao", "sg_uf", "cd_cargo", "nr_candidato", "sq_candidato"), all.x = TRUE)
dep[, `:=`(nome_urna_norm = norm_nome(nome_urna), nasc_bocel = substr(dt_nascimento, 1, 10))]
cat("eleitos BOCEL cd_cargo 7 BA:", nrow(dep), "| com nome de urna:", dep[!is.na(nome_urna), .N], "\n")

## ---------------------------------------------------------------- 7. pareamento
base[, `:=`(nome_normalizado = norm_nome(nome), nome_completo_norm = norm_nome(nome_completo))]
base[nome_completo_norm == "", nome_completo_norm := NA_character_]
base[, rid := .I]
base[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, metodo_pareamento = NA_character_)]
parear <- function(col_ex, col_dep, metodo) {
  a <- base[is.na(id_mandato) & !is.na(get(col_ex)) & get(col_ex) != "",
            .(rid, ano_eleicao, chave = get(col_ex))]
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- dep[!is.na(get(col_dep)) & get(col_dep) != "",
           .(id_mandato, id_pessoa, ano_eleicao, chave = get(col_dep))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  m <- m[!id_mandato %in% base$id_mandato]
  if (nrow(m)) base[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                                        metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
}
cat("pareamento por regra (ordem de R/13_exercicio_assembleias.R, depois as regras proprias da BA):\n")
parear("nome_completo_norm", "nome_bocel_norm", "nome_completo_x_nome_bocel")
parear("nome_normalizado",   "nome_urna_norm", "nome_parlamentar_x_urna")
parear("nome_normalizado",   "nome_bocel_norm",  "nome_parlamentar_x_nome_bocel")
parear("nome_completo_norm", "nome_urna_norm", "nome_completo_x_urna")
# regras proprias desta frente, so possiveis porque a ficha da ALBA traz nascimento e nome civil
base[, chave_nasc := fifelse(is.na(data_nascimento), NA_character_, data_nascimento)]
dep[, chave_nasc := nasc_bocel]
parear("chave_nasc", "chave_nasc", "nascimento_x_ano_eleicao")
prim_ult <- function(x) {
  x <- gsub("\\b(DE|DA|DO|DOS|DAS|E|FILHO|JUNIOR|NETO|SOBRINHO)\\b", " ", x)
  x <- gsub(" +", " ", trimws(x))
  p <- strsplit(x, " ")
  vapply(p, function(v) if (length(v) >= 2) paste(v[1], v[length(v)]) else NA_character_, character(1))
}
base[, chave_pu := prim_ult(nome_completo_norm)]
dep[, chave_pu := prim_ult(nome_bocel_norm)]
parear("chave_pu", "chave_pu", "primeiro_ultimo_sobrenome")
base[, chave_pu2 := prim_ult(nome_normalizado)]
dep[, chave_pu2 := prim_ult(nome_urna_norm)]
parear("chave_pu2", "chave_pu2", "primeiro_ultimo_urna")
cat("linhas pareadas:", base[!is.na(id_mandato), .N], "de", nrow(base), "\n")

# pessoa sem mandato pareado: nome civil unico entre pessoas com mandato na BA
pess <- merge(bocel_m[sg_uf == "BA", .(id_pessoa)], bocel_p[, .(id_pessoa, nome_norm = norm_nome(nome))],
              by = "id_pessoa")
pess <- unique(pess)[, if (uniqueN(id_pessoa) == 1L) .SD[1], by = nome_norm]
np <- merge(base[is.na(id_pessoa) & !is.na(nome_completo_norm), .(rid, nome_norm = nome_completo_norm)],
            pess, by = "nome_norm")
if (nrow(np)) base[np, on = "rid", `:=`(id_pessoa = i.id_pessoa, metodo_pareamento = "pessoa_nome_completo_uf")]
cat(sprintf("  %-34s +%d\n", "pessoa_nome_completo_uf", nrow(np)))

## ---------------------------------------------------------------- 8. condicao final
base[, condicao := fcase(
  !is.na(cond_narrativa), cond_narrativa,
  !is.na(id_mandato), "titular",
  default = "nao_informado")]
# quem esta pareado a um mandato de eleito no TSE e titular, mesmo que a narrativa fale de suplencia
# em OUTRA legislatura; a narrativa ja foi recortada por legislatura, entao o conflito e raro:
conflito <- base[condicao == "suplente" & !is.na(id_mandato), .N]
cat("linhas suplente pela narrativa mas pareadas a eleito do TSE:", conflito, "\n")
base[condicao == "suplente" & !is.na(id_mandato), condicao := "titular"]

## ---------------------------------------------------------------- 9. forma de saida
base[, em_curso := legislatura == "20"]
# hierarquia: o evento CITADO POR ULTIMO no trecho da legislatura manda; sem evento citado, o
# registro da casa afirma o periodo inteiro (fim_regular). Na 20a legislatura (2023-2027, em
# curso) quem esta na lista dos 63 em exercicio hoje nao encerrou o mandato (forma_saida NA).
base[, forma_saida := fcase(
  is.na(trecho) & em_curso & em_exercicio_hoje, NA_character_,
  is.na(trecho), "nao_observado",
  evento == "cassacao", "cassacao",
  evento == "falecimento", "falecimento",
  evento == "renuncia", "renuncia",
  evento == "afastamento", "afastamento",
  evento == "licenca" & !reassumiu, "licenca",
  evento == "licenca" &  reassumiu & !em_curso, "fim_regular",
  # suplente cujo exercicio termina antes do fim da legislatura: quem volta e o titular, e esse
  # desfecho nao tem termo no vocabulario fechado -> 'outro', com o texto da casa em causa_original
  condicao == "suplente" & !is.na(iv_fim) & as.IDate(iv_fim) < (fim - 31L), "outro",
  condicao == "suplente" & !em_curso, "suplente_efetivado",
  em_curso & em_exercicio_hoje, NA_character_,
  em_curso, "nao_observado",
  default = "fim_regular")]
# na 20a legislatura quem ja nao esta em exercicio e nao tem evento fica como nao_observado
base[em_curso & !em_exercicio_hoje & is.na(forma_saida), forma_saida := "nao_observado"]

## ---------------------------------------------------------------- 10. datas de exercicio
# a data de posse do suplente e a primeira data completa depois do cue de posse, mas antes do
# proximo evento (senao "efetivou-se em jan. 2005, afastou-se em 01/01/2007" daria posse em 2007)
base[, trecho_ate_evento := fifelse(!is.na(pos_evento) & !is.na(pos_posse) & pos_evento > pos_posse,
                                    substr(trecho_limpo, 1, pos_evento - 1L), trecho_limpo)]
base[, data_inicio_exercicio := fifelse(condicao == "suplente",
                                        fcoalesce(iv_ini, data_apos(trecho_ate_evento, pos_posse)),
                                        NA_character_)]
base[, data_fim_exercicio := fcase(
  forma_saida %chin% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca"),
    data_apos(trecho_limpo, pos_evento),
  condicao == "suplente" & !is.na(iv_fim), iv_fim,
  default = NA_character_)]
# uma data so vale se cair dentro da janela da legislatura (mais 12 meses de folga na virada)
dentro <- function(d, ini, fim) {
  x <- as.IDate(d)
  fifelse(is.na(x) | x < (ini - 31L) | x > (fim + 31L), NA_character_, d)
}
base[, data_inicio_exercicio := dentro(data_inicio_exercicio, ini, fim)]
base[, data_fim_exercicio    := dentro(data_fim_exercicio, ini, fim)]
base[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio) &
       as.IDate(data_fim_exercicio) < as.IDate(data_inicio_exercicio),
     data_fim_exercicio := NA_character_]

## ---------------------------------------------------------------- 11. causa original
base[, causa_original := fifelse(is.na(trecho) | trecho == "", NA_character_,
                                 substr(gsub("^[ ,;.|]+", "", trecho), 1, 400))]
base[!is.na(causa_original) & causa_original == "", causa_original := NA_character_]

## ---------------------------------------------------------------- 12. saida no esquema do banco
setorder(base, ano_eleicao, condicao, nome_normalizado)
saida <- base[, .(uf = "BA", fonte = "alba_ficha_parlamentar", legislatura, ano_eleicao,
                  nome, nome_normalizado, nome_completo, data_nascimento, partido, condicao,
                  data_inicio_exercicio, data_fim_exercicio, causa_original, forma_saida,
                  id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato, metodo_pareamento,
                  url = sprintf("https://www.al.ba.gov.br/deputados/ex-deputado-estadual/%s", id_fonte),
                  id_fonte, votos_fonte = NA_character_, sexo_fonte)]
saida[partido == "", partido := NA_character_]
saida[nome_completo == "", nome_completo := NA_character_]

## ---------------------------------------------------------------- 13. asserts
checa_unica(as.data.frame(saida), c("legislatura", "id_fonte"))
in_set(saida$forma_saida, VOCAB, permitir_na = TRUE, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente", "nao_informado"), permitir_na = FALSE, nome = "condicao")
in_set(saida$uf, "BA", permitir_na = FALSE, nome = "uf")
em_faixa(saida$ano_eleicao, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
em_faixa(as.integer(substr(saida$data_inicio_exercicio, 1, 4)), 1999, 2027, nome = "ano_inicio_exercicio")
em_faixa(as.integer(substr(saida$data_fim_exercicio, 1, 4)), 1999, 2027, nome = "ano_fim_exercicio")
em_faixa(as.integer(substr(saida$data_nascimento, 1, 4)), 1900, 2005, nome = "ano_nascimento")
# id_mandato_bocel tem de existir em mandatos.csv com cd_cargo 7 e sg_uf BA e bater a eleicao
pareadas <- saida[!is.na(id_mandato_bocel)]
stopifnot(all(pareadas$id_mandato_bocel %in% dep$id_mandato))
chk <- merge(pareadas[, .(id_mandato_bocel, ano_eleicao)], dep[, .(id_mandato_bocel = id_mandato,
             ano_bocel = ano_eleicao)], by = "id_mandato_bocel")
stopifnot(nrow(chk) == nrow(pareadas), all(chk$ano_eleicao == chk$ano_bocel))
# um mandato do BOCEL nao pode ser atribuido a duas pessoas distintas da fonte
dup <- pareadas[, .(n = uniqueN(nome_normalizado)), by = id_mandato_bocel][n > 1]
stopifnot(nrow(dup) == 0)
stopifnot(!anyDuplicated(pareadas[, .(id_mandato_bocel)]))
cat("asserts: ok\n")

fwrite(saida, file.path(OUTD, "BA.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
cat("data/assembleias2/BA.csv:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

## ---------------------------------------------------------------- 14. inventario das fontes sondadas
inv <- rbindlist(list(
 list("https://www.al.ba.gov.br/deputados/ex-deputados-estaduais/legislatura/{14..19}", "html_lista_legislatura",
      "quem ocupou a cadeira em cada legislatura: nome parlamentar, partido e id do deputado; sem datas e sem condicao",
      "1a (1947-1951) a 19a (2019-2023); usadas a 14a a 19a", "sim",
      "GET curl_cffi impersonate=chrome; 200; 6 paginas em cache; 70/70/70/71/69/72 cards"),
 list("https://www.al.ba.gov.br/deputados/legislatura-atual", "html_lista_legislatura",
      "20a legislatura (2023-2027): 72 nomes que ocuparam a cadeira, com partido", "20a (2023-2027)", "sim",
      "GET 200; 72 cards"),
 list("https://www.al.ba.gov.br/deputados/deputados-estaduais", "html_lista_em_exercicio",
      "os 63 deputados em exercicio na data da coleta; separa, na 20a legislatura, quem saiu de quem esta em exercicio",
      "hoje (29/08/2026)", "sim", "GET 200; 63 cards"),
 list("https://www.al.ba.gov.br/deputados/ex-deputado-estadual/{id}", "html_ficha_individual",
      "nome civil completo, data e municipio de nascimento, sexo, profissao, filiacao partidaria e o historico narrativo 'Mandato Eletivo' com periodos e eventos (renuncia, licenca, efetivacao de suplente, afastamento), mantido pelo Departamento de Pesquisa da casa",
      "toda a trajetoria de cada deputado listado", "sim",
      "GET 200 em 236/236 fichas; 1,2 s entre requisicoes; cache em data_raw/assembleias2/BA/perfil_<id>.html"),
 list("https://sapl.al.ba.leg.br/", "sapl_api",
      "instancia SAPL Interlegis (parlamentares, mandatos, afastamentos), como em AC/AL/AM/PB/PI/RO/RR/TO",
      "nenhuma", "nao", "DNS nao resolve (curl 6, Could not resolve host); a ALBA nao tem instancia SAPL"),
 list("https://www.al.ba.gov.br/dados-abertos", "html", "area de dados abertos", "nenhuma", "nao",
      "GET 404 (pagina de erro de 39.974 bytes); a rota nao existe no portal"),
 list("https://www.al.ba.gov.br/diario-oficial", "html", "diario no proprio portal", "nenhuma", "nao",
      "GET 404; o diario da casa fica no portal da EGBA"),
 list("https://egbanet.egba.ba.gov.br/alba", "portal_diario_oficial",
      "Diario Oficial do Poder Legislativo da Bahia em PDF/HTML/FLIP, por data, com busca por palavra",
      "edicoes por data; indice de texto integral so de 2022 em diante", "parcial",
      "GET 200; /alba/buscanova responde 500 no GET, a busca real e o endpoint JSON /busca/busca/buscar/query"),
 list("https://egbanet.egba.ba.gov.br/busca/busca/buscar/query/{p}[/di:][/df:]/?q=&subtheme=alba",
      "json_elasticsearch",
      "texto integral (OCR) das paginas do Diario da ALBA; agregacao por ano confirma 2022, 2023, 2024, 2025 e 2026",
      "2022-2026", "parcial",
      "GET 200; 8.239 paginas com 'Assembleia Legislativa', 0 antes de 2022; o diario e um boletim noticioso, nao um registro de atos da Mesa: 'tomou posse' 54 paginas, 'assumiu o mandato' 13, 'convocacao do suplente' 1, e as mencoes a licenca aparecem em materia jornalistica, sem ato datado"),
 list("https://albalegis.nopapercloud.com.br/spl/sessoes.aspx", "html_spl",
      "sessoes plenarias (SPL nopaper)", "sessoes recentes", "nao",
      "GET 200; lista sessoes e nao a composicao nem afastamentos"),
 list("https://www.al.ba.gov.br/transparencia/legislacao", "html",
      "atos normativos e legislacao da casa", "atual", "nao",
      "GET 200; sem atos de posse, licenca ou convocacao de suplente"),
 list("https://www.al.ba.gov.br/boletim-interno", "html",
      "boletim informativo diario (materias)", "2026", "nao", "GET 200; conteudo jornalistico"),
 list("https://www.al.ba.gov.br/historia-do-legislativo/mesas-diretoras", "html",
      "composicao das Mesas Diretoras por bienio", "historica", "nao",
      "GET 200; so a Mesa, nao o conjunto dos deputados nem a saida")))
setnames(inv, c("url", "tipo", "oferece", "periodo_coberto", "viavel", "testado"))
fwrite(inv, file.path(RAW, "inventario.csv"), na = "NA", quote = TRUE)

## ---------------------------------------------------------------- 15. relatorios e numeros
cob <- merge(dep[, .(n_bocel = .N), by = ano_eleicao],
             saida[!is.na(id_mandato_bocel), .(n_pareados = uniqueN(id_mandato_bocel)), by = ano_eleicao],
             by = "ano_eleicao", all.x = TRUE)
cob <- merge(cob, saida[, .(n_fonte = .N, n_titular = sum(condicao == "titular"),
                            n_suplente = sum(condicao == "suplente")), by = ano_eleicao],
             by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
cob <- merge(cob, saida[!is.na(forma_saida) & forma_saida != "nao_observado",
                        .(n_com_forma_saida = .N), by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob[is.na(n_com_forma_saida), n_com_forma_saida := 0L]
fwrite(cob, file.path(VERD, "asm2ba_cobertura_por_eleicao.csv"))
cat("\nCobertura por eleicao:\n"); print(cob)
fs <- saida[, .N, by = .(condicao, forma_saida)][order(condicao, -N)]
fwrite(fs, file.path(VERD, "asm2ba_forma_saida.csv"))
cat("\nForma de saida por condicao:\n"); print(fs)
fwrite(saida[forma_saida %chin% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca",
                                  "suplente_efetivado", "outro"),
             .(legislatura, nome, nome_completo, condicao, data_inicio_exercicio,
               data_fim_exercicio, forma_saida, causa_original)][order(legislatura, nome)],
       file.path(VERD, "asm2ba_eventos_para_conferencia.csv"))
fwrite(saida[is.na(id_mandato_bocel), .(ano_eleicao, legislatura, nome, nome_completo,
                                      data_nascimento, condicao, forma_saida)][order(ano_eleicao, nome)],
       file.path(VERD, "asm2ba_sem_pareamento.csv"))

reg("n_linhas", nrow(saida))
reg("n_colunas", ncol(saida))
reg("n_perfis_coletados", nrow(perfis))
reg("n_linhas_com_nome_completo", saida[!is.na(nome_completo), .N])
reg("n_linhas_com_data_nascimento", saida[!is.na(data_nascimento), .N])
reg("n_linhas_com_sexo", saida[!is.na(sexo_fonte), .N])
reg("n_linhas_com_narrativa_da_legislatura", base[!is.na(trecho), .N])
reg("n_mandatos_bocel_cd7_ba", nrow(dep))
reg("n_linhas_pareadas", saida[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_bocel_pareados", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]))
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]),
                                      nrow(dep), uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]) / nrow(dep)))
reg("n_linhas_com_id_pessoa", saida[!is.na(id_pessoa_bocel), .N])
reg("n_com_forma_saida_observada", saida[!is.na(forma_saida) & forma_saida != "nao_observado", .N])
reg("n_forma_saida_na_em_curso", saida[is.na(forma_saida), .N])
reg("n_com_data_inicio_exercicio", saida[!is.na(data_inicio_exercicio), .N])
reg("n_com_data_fim_exercicio", saida[!is.na(data_fim_exercicio), .N])
reg("n_titular", saida[condicao == "titular", .N])
reg("n_suplente", saida[condicao == "suplente", .N])
reg("n_condicao_nao_informado", saida[condicao == "nao_informado", .N])
for (i in seq_len(nrow(cob))) {
  reg(sprintf("taxa_pareamento_%d", cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
  reg(sprintf("n_com_forma_saida_%d", cob$ano_eleicao[i]), cob$n_com_forma_saida[i])
}
fst <- saida[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fst)))
  reg(paste0("n_forma_saida_", fifelse(is.na(fst$forma_saida[i]), "NA_em_curso", fst$forma_saida[i])), fst$N[i])
for (m in sort(unique(na.omit(saida$metodo_pareamento))))
  reg(paste0("n_pareados_metodo_", m), saida[metodo_pareamento == m, .N])
reg("n_fontes_inventariadas", nrow(inv))
reg("n_fontes_viaveis", inv[viavel == "sim", .N])
reg("n_fontes_recusadas", inv[viavel == "nao", .N])
reg("legislaturas_cobertas", paste(sort(unique(saida$legislatura)), collapse = ";"))
cat("\nbuild_BA: concluido —", format(Sys.time()), "\n")
sink()
