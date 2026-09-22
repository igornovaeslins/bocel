# auditoria_fusao_documentos.R — pessoas do banco cujas candidaturas trazem documentos de duas pessoas (19/09/2026)
#
# A deduplicacao do R/03 une candidaturas por titulo, por CPF ou por nome com nascimento, e a uniao e transitiva. Quando o
# cadastro do TSE traz o titulo de uma pessoa na candidatura de outra, o titulo sozinho funde as duas. A leitura dos casos
# da pendencia 4 achou dois (auditoria_incompatibilidade_leitura.R). O vice-prefeito do Recife de 2004 tem o CPF, o nome e
# o nascimento de Luciano Siqueira com o titulo de Joao Paulo, e o prefeito de Marau de 2008 tem o CPF de Vilmar Zanchin
# com o titulo do irmao gemeo Vilmo. O R/09 audita so a ligacao por nome e nascimento, e esta auditoria cobre as outras
# duas no banco inteiro.
#
# Para cada pessoa, refaz a uniao dentro dela sem o titulo (CPF ou nome com nascimento) e, em separado, sem o CPF (titulo
# ou nome com nascimento). Se a pessoa se parte em blocos e dois blocos trazem documentos validos e diferentes da chave
# retirada, a chave retirada foi a unica ponte entre dois documentos. CPF e titulo tem digitos verificadores, e dois
# numeros distintos que passam nos dois digitos dificilmente sao erro de digitacao do mesmo numero. Nao corrige nada, e
# so classifica a leitura de cada ponte encontrada.
#
# 21/09/2026: a decisao na pendencia 4 separou os casos de duas_pessoas_nome_e_nascimento_diferentes (nome e
# nascimento tambem diferentes entre os blocos, os dois casos acima inclusive) em id_pessoa distintos, dentro do proprio
# fecho de uniao do R/03. Esta auditoria roda depois, sobre o banco ja separado, e por isso essa leitura tem de dar
# sempre zero pessoas; se aparecer alguma, a separacao do R/03 falhou ou um caso novo surgiu na atualizacao dos dados, e
# os dois pedem nova decisao. As pontes ambiguas (mesmo nome ou mesmo nascimento entre os blocos, ou os dois
# iguais com documento reemitido) continuam fundidas de proposito e tem de bater, pessoa por pessoa, com a marca em
# dedup_suspeita_fusao de pessoas.csv, que o R/03 grava a partir da mesma leitura.
#
# Saidas: output/verificacao/fusao_documentos_pessoas.csv (uma linha por pessoa suspeita, com os blocos),
#         output/verificacao/fusao_documentos_mandatos.csv (os mandatos dessas pessoas com os documentos), chaves fusdoc_*.
# Execucao: cd ~/bocel && Rscript --vanilla R/auditoria_fusao_documentos.R
suppressPackageStartupMessages({ library(data.table); library(arrow); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/auditoria_fusao_documentos.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

m <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("", "NA"),
           select = c("id_mandato", "id_pessoa", "ano_eleicao", "sg_uf", "sg_ue", "nm_ue", "cd_cargo", "cargo", "nr_candidato",
                      "sq_candidato"))
reg("fusdoc_mandatos", nrow(m))

## ---------------------------------------------------------------- documentos do TSE por candidatura
digitos <- function(x) gsub("[^0-9]", "", x)
mat <- function(d, n) matrix(as.integer(unlist(strsplit(d, ""))), ncol = n, byrow = TRUE)
cpf_valido <- function(d) {
  ok <- !is.na(d) & nchar(d) == 11L & !grepl("^(\\d)\\1{10}$", d)
  if (!any(ok)) return(ok)
  x <- mat(d[ok], 11L)
  r1 <- (x[, 1:9] %*% (10:2)) %% 11; v1 <- ifelse(r1 < 2, 0, 11 - r1)
  r2 <- (x[, 1:10] %*% (11:2)) %% 11; v2 <- ifelse(r2 < 2, 0, 11 - r2)
  ok[ok] <- as.vector(v1 == x[, 10] & v2 == x[, 11])
  ok
}
# titulo de 12 digitos: 8 de sequencia, 2 da UF de emissao, 2 verificadores; SP (01) e MG (02) trocam resto 0 por 1
titulo_valido <- function(d) {
  ok <- !is.na(d) & nchar(d) == 12L & !grepl("^0+$", d)
  if (!any(ok)) return(ok)
  x <- mat(d[ok], 12L)
  uf <- x[, 9] * 10 + x[, 10]
  r1 <- as.vector((x[, 1:8] %*% (2:9)) %% 11)
  v1 <- ifelse(r1 == 10, 0, ifelse(r1 == 0 & uf %in% 1:2, 1, r1))
  r2 <- (x[, 9] * 7 + x[, 10] * 8 + v1 * 9) %% 11
  v2 <- ifelse(r2 == 10, 0, ifelse(r2 == 0 & uf %in% 1:2, 1, r2))
  ok[ok] <- v1 == x[, 11] & v2 == x[, 12] & uf >= 1 & uf <= 28
  ok
}
norm_nome <- function(x) stri_trim_both(gsub(" +", " ", gsub("[^A-Z ]", " ", toupper(stri_trans_general(x, "Latin-ASCII")))))

doc <- rbindlist(lapply(sort(unique(m$ano_eleicao)), function(a) {
  sq <- unique(m[ano_eleicao == a, sq_candidato])
  x <- as.data.table(open_dataset(sprintf("data_raw/parquet/cand_%s.parquet", a)) |>
                       dplyr::filter(SQ_CANDIDATO %in% sq) |>
                       dplyr::select(SG_UE, CD_CARGO, NR_CANDIDATO, SQ_CANDIDATO, NM_CANDIDATO, NR_CPF_CANDIDATO,
                                     NR_TITULO_ELEITORAL_CANDIDATO, DT_NASCIMENTO, DS_SIT_TOT_TURNO) |> dplyr::collect())
  x <- unique(x[, lapply(.SD, as.character)])
  x[, ano_eleicao := a]
}))
setnames(doc, c("SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO"), c("sg_ue", "cd_cargo", "nr_candidato", "sq_candidato"))
# Em 2004 o sequencial se repete dentro da mesma unidade e cargo (14 candidaturas eleitas), e o numero do candidato, que
# ja entra no id_mandato, desfaz a ambiguidade
chave <- c("ano_eleicao", "sg_ue", "cd_cargo", "nr_candidato", "sq_candidato")
doc[, `:=`(nome = norm_nome(NM_CANDIDATO), cpf = digitos(NR_CPF_CANDIDATO), titulo = digitos(NR_TITULO_ELEITORAL_CANDIDATO),
           nasc = fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", trimws(DT_NASCIMENTO)), trimws(DT_NASCIMENTO), NA_character_),
           elt = toupper(trimws(DS_SIT_TOT_TURNO)) %chin% c("ELEITO", "ELEITO POR QP", "ELEITO POR MEDIA",
                                                            "ELEITO POR MÉDIA", "MEDIA", "MÉDIA"),
           ord = .I)]
doc[nchar(titulo) %in% 9:11, titulo := formatC(as.numeric(titulo), width = 12, flag = "0", format = "f", digits = 0)]
docu <- unique(doc[, c(chave, "nome", "cpf", "titulo", "nasc"), with = FALSE])
# Em 2004 a mesma chave ainda cobre duas pessoas em quatro candidaturas, tres de substituicao de chapa depois do
# registro (prefeito e vice de Rio Negrinho, vice de Sao Joao d'Alianca) e uma de cadastro duplicado com um dia de
# diferenca no nascimento (vice de Itapeva). O R/03 fica com a linha do turno decisivo e, no empate, com a primeira, e aqui
# a escolha repete a dele, com a linha do eleito na frente quando o cadastro marca a situacao.
reg("fusdoc_candidaturas_com_duas_pessoas_na_mesma_chave", nrow(docu[, .N, by = chave][N > 1L]))
setorder(doc, ano_eleicao, sg_ue, cd_cargo, nr_candidato, sq_candidato, -elt, ord)
doc <- doc[, .SD[1], by = chave][, c(chave, "nome", "cpf", "titulo", "nasc"), with = FALSE]
stopifnot(!anyDuplicated(doc, by = chave))
doc[, `:=`(cpf_ok = cpf_valido(cpf), tit_ok = titulo_valido(titulo))]
# os digitos verificadores do titulo vem de regra lembrada, e a taxa de aprovacao no cadastro inteiro confere a regra
reg("fusdoc_titulos_com_12_digitos_que_passam_no_verificador_pct",
    round(100 * doc[nchar(titulo) == 12L & !grepl("^0+$", titulo), mean(tit_ok)], 2))
reg("fusdoc_cpfs_com_11_digitos_que_passam_no_verificador_pct",
    round(100 * doc[nchar(cpf) == 11L, mean(cpf_ok)], 2))

x <- merge(m, doc, by = chave, all.x = TRUE)
reg("fusdoc_mandatos_sem_candidatura_no_tse", x[is.na(nome), .N])
x[, `:=`(cpf_v = fifelse(cpf_ok %in% TRUE, cpf, NA_character_), tit_v = fifelse(tit_ok %in% TRUE, titulo, NA_character_),
         knom = fifelse(!is.na(nome) & nzchar(nome) & !is.na(nasc), paste(nome, nasc), NA_character_))]

## ---------------------------------------------------------------- blocos dentro da pessoa
# Rotulo por propagacao do minimo dentro dos grupos de cada chave mantida, ate estabilizar.
blocos <- function(d, chaves) {
  d <- copy(d)[, lab := .I]
  repeat {
    antes <- d$lab
    for (k in chaves) d[!is.na(get(k)), lab := min(lab), by = c("id_pessoa", k)]
    if (identical(antes, d$lab)) break
  }
  d$lab
}
cand <- x[id_pessoa %chin% x[, .(n = max(uniqueN(na.omit(cpf_v)), uniqueN(na.omit(tit_v)))), by = id_pessoa][n > 1L, id_pessoa]]
reg("fusdoc_pessoas_com_dois_ou_mais_cpfs_ou_titulos_validos", uniqueN(cand$id_pessoa))
cand[, bloco_sem_titulo := blocos(cand, c("cpf_v", "knom"))]
cand[, bloco_sem_cpf := blocos(cand, c("tit_v", "knom"))]

resumo_ponte <- function(d, bloco, doc_col, rotulo) {
  b <- d[, .(docs = list(unique(na.omit(get(doc_col)))), nomes = paste(sort(unique(na.omit(nome))), collapse = " / "),
             nascs = paste(sort(unique(na.omit(nasc))), collapse = " / "),
             mandatos = paste(sort(paste(ano_eleicao, cargo, nm_ue)), collapse = "; ")), by = c("id_pessoa", bloco)]
  # a pessoa conta quando ha dois blocos com documento valido da chave retirada e sem documento em comum
  b[, tem := lengths(docs) > 0L]
  p <- b[tem == TRUE, {
    todos <- unlist(docs)
    .(n_blocos = .N, docs_distintos = uniqueN(todos), sem_sobreposicao = !anyDuplicated(todos),
      blocos = paste(sprintf("[%s | %s | nasc %s | %s]", vapply(docs, paste, "", collapse = ","), nomes, nascs, mandatos),
                     collapse = " <> "))
  }, by = id_pessoa][n_blocos > 1L & sem_sobreposicao == TRUE]
  p[, ponte := rotulo]
  p
}
pt <- resumo_ponte(cand, "bloco_sem_titulo", "cpf_v", "titulo_une_cpfs_distintos")
pc <- resumo_ponte(cand, "bloco_sem_cpf", "tit_v", "cpf_une_titulos_distintos")
sus <- rbindlist(list(pt, pc))

# nome e nascimento entre blocos, para separar o erro de cadastro da mesma pessoa (mesmo nome e nascimento, documento
# reemitido ou digitado errado) da fusao de duas pessoas
bl <- rbindlist(list(
  cand[id_pessoa %chin% pt$id_pessoa, .(id_pessoa, ponte = "titulo_une_cpfs_distintos", bloco = bloco_sem_titulo, nome, nasc)],
  cand[id_pessoa %chin% pc$id_pessoa, .(id_pessoa, ponte = "cpf_une_titulos_distintos", bloco = bloco_sem_cpf, nome, nasc)]))
cmp <- bl[, {
  nb <- split(nome, bloco); db <- split(nasc, bloco)
  pares <- combn(length(nb), 2L, simplify = FALSE)
  nome_comum <- vapply(pares, function(p) length(intersect(na.omit(nb[[p[1]]]), na.omit(nb[[p[2]]]))) > 0L, TRUE)
  nasc_comum <- vapply(pares, function(p) length(intersect(na.omit(db[[p[1]]]), na.omit(db[[p[2]]]))) > 0L, TRUE)
  dist_min <- vapply(pares, function(p) min(adist(unique(na.omit(nb[[p[1]]])), unique(na.omit(nb[[p[2]]])))), 0)
  .(algum_par_sem_nome_comum = any(!nome_comum), algum_par_sem_nasc_comum = any(!nasc_comum),
    menor_distancia_de_nome = max(dist_min))
}, by = .(id_pessoa, ponte)]
sus <- merge(sus, cmp, by = c("id_pessoa", "ponte"))
sus[, leitura := fcase(algum_par_sem_nome_comum & algum_par_sem_nasc_comum, "duas_pessoas_nome_e_nascimento_diferentes",
                       algum_par_sem_nome_comum, "nome_diferente_mesmo_nascimento",
                       algum_par_sem_nasc_comum, "mesmo_nome_nascimento_diferente",
                       default = "mesmo_nome_e_nascimento_documento_reemitido")]
setcolorder(sus, c("id_pessoa", "ponte", "leitura"))
setorder(sus, ponte, leitura, id_pessoa)
fwrite(sus, "output/verificacao/fusao_documentos_pessoas.csv", quote = TRUE)
mm <- cand[id_pessoa %chin% sus$id_pessoa,
           .(id_pessoa, id_mandato, ano_eleicao, sg_uf, cargo, nm_ue, nome, nasc, cpf, cpf_ok, titulo, tit_ok,
             bloco_sem_titulo, bloco_sem_cpf)]
setorder(mm, id_pessoa, ano_eleicao)
fwrite(mm, "output/verificacao/fusao_documentos_mandatos.csv", quote = TRUE)

stopifnot(!anyDuplicated(sus, by = c("id_pessoa", "ponte")))
for (p in unique(sus$ponte)) {
  reg(paste0("fusdoc_", p), sus[ponte == p, .N])
  for (l in unique(sus[ponte == p, leitura])) reg(paste0("fusdoc_", p, "_", l), sus[ponte == p & leitura == l, .N])
}
reg("fusdoc_pessoas_suspeitas", uniqueN(sus$id_pessoa))
reg("fusdoc_mandatos_das_pessoas_suspeitas", nrow(mm))
print(sus[, .N, by = .(ponte, leitura)])

## ---------------------------------------------------------------- separacao aplicada (21/09/2026)
# duas_pessoas_nome_e_nascimento_diferentes tem de estar sempre em zero depois da separacao do R/03; qualquer pessoa
# aqui e falha da separacao ou caso novo, e reprova a auditoria em vez de so registrar o numero.
n_duas_pessoas <- sus[leitura == "duas_pessoas_nome_e_nascimento_diferentes", .N]
stopifnot(n_duas_pessoas == 0L)
# as pontes ambiguas que continuam fundidas tem de ser exatamente as pessoas marcadas em dedup_suspeita_fusao de
# pessoas.csv, gravada pelo R/03 a partir da mesma leitura; divergencia aqui e o banco e a auditoria fora de sincronia
pf <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character",
            select = c("id_pessoa", "dedup_suspeita_fusao"))
marcadas <- pf[!is.na(dedup_suspeita_fusao), id_pessoa]
reg("fusdoc_pessoas_marcadas_dedup_suspeita_fusao", length(marcadas))
stopifnot(setequal(sus$id_pessoa, marcadas))
