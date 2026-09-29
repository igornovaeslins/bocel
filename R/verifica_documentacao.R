# verifica_documentacao.R — verificador cetico do tema 'documentacao'
# Confere docs/{README,LIVRO_DE_CODIGOS,NOTA_DE_COBERTURA}.md e EXCECOES_CONHECIDAS.csv
# contra os CSVs depositados (lista FILES de zenodo/deposit.py) e output/numeros_assinatura.txt.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_documentacao.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_documentacao.R"
# 24/09/2026: a tag de versao esperada no README sai do arquivo VERSAO, nunca de um literal
VERSAO_TAG <- paste0("v", trimws(read.dcf("VERSAO")[1, "versao"]))

passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ expr; TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  r
}
pct <- function(x) sprintf("%.1f%%", 100 * mean(!is.na(x) & x != ""))
tipo_col <- function(x) {
  v <- x[!is.na(x) & x != ""]
  if (!length(v)) return("texto")
  if (all(grepl("^-?\\d+$", v))) return("inteiro")
  if (all(grepl("^-?\\d*\\.\\d+$|^-?\\d+$", v))) return("numero")
  if (all(grepl("^\\d{4}-\\d{2}-\\d{2}", v))) return("data ISO")
  if (all(toupper(v) %in% c("TRUE", "FALSE"))) return("logico")
  "texto"
}
fmt <- function(n) format(n, big.mark = ".", decimal.mark = ",", trim = TRUE)

## ------------------------------------------------ lista FILES do deposit.py
dep <- readLines("zenodo/deposit.py")
ini <- grep("^FILES = \\[", dep); fim <- ini + which(grepl("^\\]", dep[ini:length(dep)]))[1] - 1
files <- regmatches(paste(dep[ini:fim], collapse = " "), gregexpr('"[^"]+"', paste(dep[ini:fim], collapse = " ")))[[1]]
files <- gsub('"', "", files)
csvs <- grep("^data_v1/.*\\.csv$", files, value = TRUE)
ok("deposit.py: todo arquivo listado existe", stopifnot(all(file.exists(files[!grepl("camaras_sem_sapl|diarios_|assembleias_historico|tce_gestores", files)]))))
cat("CSVs depositados:", length(csvs), "\n")

## ------------------------------------------------ livro de codigos: parse
lc <- readLines("docs/LIVRO_DE_CODIGOS.md")
sec_idx <- grep("^## ", lc)
parse_sec <- function(i) {
  a <- sec_idx[i]; b <- if (i < length(sec_idx)) sec_idx[i + 1] - 1 else length(lc)
  tit <- sub("^## ", "", lc[a])
  arq <- regmatches(tit, regexpr("^[a-z0-9_]+\\.csv", tit))
  if (!length(arq)) return(NULL)
  n_lin <- as.integer(gsub("\\.", "", regmatches(tit, regexpr("[0-9.]+(?= linhas)", tit, perl = TRUE))))
  rows <- grep("^\\| `", lc[a:b], value = TRUE)
  p <- strsplit(rows, "\\|")
  data.table(arquivo = arq, n_linhas_doc = n_lin,
             variavel = gsub("`| ", "", sapply(p, `[`, 2)),
             tipo_doc = trimws(sapply(p, `[`, 3)),
             preench_doc = trimws(sapply(p, `[`, 5)))
}
dic <- rbindlist(lapply(seq_along(sec_idx), parse_sec))
ok("livro: (arquivo, variavel) unico", checa_unica(as.data.frame(dic), c("arquivo", "variavel")))

## ------------------------------------------------ colunas, tipo, preenchimento
tipos_compat <- function(doc, obs) {
  doc == obs ||
    (doc == "texto" && obs %in% c("inteiro", "numero")) ||     # ids/codigos declarados texto
    (doc == "numero" && obs == "inteiro") ||
    (doc == "data dd/mm/aaaa" && obs == "texto") ||
    (doc == "data ISO" && obs == "texto")                       # cheque separado abaixo
}
amostra <- list(); desvios <- list(); dados <- list()
for (f in csvs) {
  nm <- basename(f)
  if (!file.exists(f)) next
  x <- fread(f, colClasses = "character", na.strings = "NA", encoding = "UTF-8")
  for (cc in names(x)) x[[cc]] <- iconv(x[[cc]], from = "UTF-8", to = "UTF-8", sub = "")
  dados[[nm]] <- x
  d <- dic[arquivo == nm]
  ok(sprintf("livro: %s tem secao", nm), stopifnot(nrow(d) > 0))
  ok(sprintf("livro: %s colunas = dados (conjunto e ordem)", nm),
     stopifnot(identical(d$variavel, names(x))))
  ok(sprintf("livro: %s n linhas (%s) = dados (%s)", nm, d$n_linhas_doc[1], nrow(x)),
     stopifnot(d$n_linhas_doc[1] == nrow(x)))
  for (v in intersect(d$variavel, names(x))) {
    obs_t <- tipo_col(x[[v]]); obs_p <- pct(x[[v]])
    doc_t <- d[variavel == v, tipo_doc]; doc_p <- d[variavel == v, preench_doc]
    prob <- character()
    if (!tipos_compat(doc_t, obs_t)) prob <- c(prob, sprintf("tipo doc=%s obs=%s", doc_t, obs_t))
    if (doc_t == "data ISO") {
      vv <- x[[v]][!is.na(x[[v]]) & x[[v]] != ""]
      if (length(vv) && !all(grepl("^\\d{4}-\\d{2}-\\d{2}$", vv))) prob <- c(prob, "data ISO com valores fora de aaaa-mm-dd")
    }
    if (doc_t == "data dd/mm/aaaa") {
      vv <- x[[v]][!is.na(x[[v]]) & x[[v]] != ""]
      if (length(vv) && !all(grepl("^\\d{2}/\\d{2}/\\d{4}$", vv))) prob <- c(prob, "data dd/mm/aaaa com valores fora do formato")
    }
    if (doc_p != obs_p) prob <- c(prob, sprintf("preench doc=%s obs=%s", doc_p, obs_p))
    if (length(prob)) desvios[[length(desvios) + 1]] <- data.table(arquivo = nm, variavel = v, problema = paste(prob, collapse = "; "))
  }
}
desvios <- rbindlist(desvios)
ok("livro: tipo e preenchimento de TODAS as colunas batem",
   if (nrow(desvios)) stop(paste(sprintf("%s.%s: %s", desvios$arquivo, desvios$variavel, desvios$problema), collapse = " || ")))
# reconta 10 colunas ao acaso e imprime (evidencia)
todas <- dic[arquivo %in% basename(csvs), .(arquivo, variavel)]
am <- todas[sample(.N, min(10L, .N))]
am[, preench_recontado := mapply(function(a, v) pct(dados[[a]][[v]]), arquivo, variavel)]
am[, tipo_recontado := mapply(function(a, v) tipo_col(dados[[a]][[v]]), arquivo, variavel)]
am <- merge(am, dic[, .(arquivo, variavel, tipo_doc, preench_doc)], by = c("arquivo", "variavel"))
cat("\n== Amostra de 10 colunas recontadas ==\n"); print(am)
fwrite(am, "output/verificacao/documentacao_amostra_colunas.csv")
ok("amostra de 10 colunas: preenchimento bate", stopifnot(all(am$preench_recontado == am$preench_doc)))

## ------------------------------------------------ numeros dos docs recontados
mand <- dados[["mandatos.csv"]]; pess <- dados[["pessoas.csv"]]; pos <- dados[["posicoes_ano.csv"]]
fil <- dados[["filiacoes.csv"]]
ass <- readLines("output/numeros_assinatura.txt")
ult <- rbindlist(lapply(strsplit(ass, " \\| "), function(p) data.table(chave = trimws(p[1]), valor = trimws(p[2]))))
ult <- ult[, .SD[.N], by = chave]
sig <- function(k) ult[chave == k, valor]

# 29/09/2026: paste() de tres vetores de tamanhos diferentes reciclava as linhas do menor e emparelhava linhas de arquivos
# distintos; c() junta as linhas de cada arquivo uma vez, na ordem
docs_txt <- paste(c(readLines("docs/README.md"), readLines("docs/NOTA_DE_COBERTURA.md"),
                    readLines("docs/AMARRACOES_FAPESP.md")), collapse = "\n")
# limite de digito nas duas pontas: sem isso um numero curto (ex.: "1") bate por acaso
# dentro de qualquer numero maior que o contenha (ex.: "18.092"), o que nao prova nada
cita <- function(s) grepl(paste0("(?<![0-9])", gsub(".", "\\.", s, fixed = TRUE), "(?![0-9])"),
                           docs_txt, perl = TRUE)

fed <- mand[esfera == "federal"]; est <- mand[esfera == "estadual"]
fsit <- mand[, .N, by = fonte_situacao]
nsit <- function(k) if (k %in% fsit$fonte_situacao) fsit[fonte_situacao == k, N] else 0L
# n_imputados_por_votos, n_duplicatas_posicao_removidas e n_eleitos_sem_identidade_excluidos sao
# contagens da construcao do banco completo (R/03_build_banco.R), anteriores ao recorte da v1.0;
# a nota de cobertura os cita como tal, e o registro correspondente e o bocel_* (banco inteiro),
# nao o v1_* (recorte)
cc_constr <- fread("output/construcao_contagens.csv")
gcc <- function(k) trimws(format(cc_constr[chave == k, valor], big.mark = ".", decimal.mark = ","))
recontas <- data.table(
  numero = c("pessoas", "mandatos", "posicoes_ano", "filiacoes registros", "pessoas com filiacao",
             "pct pessoas com filiacao", "federal forma_saida observada", "federal pct forma_saida observada",
             "estadual forma_saida observada", "estadual pct forma_saida observada",
             "fonte_situacao cadastro (recorte)", "fonte_situacao imputacao_titular (recorte, vices)",
             "fonte_situacao imputacao_votos (recorte)", "fonte_situacao fonte_oficial_casa (recorte)",
             "dedup titulo (recorte)", "senadores 2018 (EXCECOES)", "senadores MT 2018 (EXCECOES)",
             "n_imputados_por_votos (banco completo)", "n_duplicatas_posicao_removidas (banco completo)",
             "n_eleitos_sem_identidade_excluidos (banco completo)"),
  recontado = c(fmt(nrow(pess)), fmt(nrow(mand)), fmt(nrow(pos)), fmt(nrow(fil)), fmt(uniqueN(fil$id_pessoa)),
                sub(".", ",", sprintf("%.1f%%", 100 * uniqueN(fil$id_pessoa) / nrow(pess)), fixed = TRUE),
                fmt(sum(fed$forma_saida != "nao_observado")),
                sub(".", ",", sprintf("%.1f%%", 100 * mean(fed$forma_saida != "nao_observado")), fixed = TRUE),
                fmt(sum(est$forma_saida != "nao_observado")),
                sub(".", ",", sprintf("%.1f%%", 100 * mean(est$forma_saida != "nao_observado")), fixed = TRUE),
                fmt(nsit("cadastro")), fmt(nsit("imputacao_titular")), fmt(nsit("imputacao_votos")),
                fmt(nsit("fonte_oficial_casa")), fmt(pess[chave_dedup == "titulo", .N]),
                fmt(mand[ano_eleicao == "2018" & cargo == "SENADOR", .N]),
                fmt(mand[ano_eleicao == "2018" & cargo == "SENADOR" & sg_uf == "MT", .N]),
                gcc("n_imputados_por_votos"), gcc("n_duplicatas_posicao_removidas"),
                gcc("n_eleitos_sem_identidade_excluidos")),
  assinatura = c(sig("v1_n_pessoas"), sig("v1_n_mandatos"), sig("v1_n_posicoes_ano"), sig("v1_n_filiacoes"),
                 sig("v1_pessoas_com_filiacao"), sig("v1_pct_pessoas_com_filiacao"),
                 NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                 sig("bocel_n_imputados_por_votos"), sig("bocel_n_duplicatas_posicao_removidas"),
                 sig("bocel_n_eleitos_sem_identidade_excluidos")))
# a checagem substantiva e que o numero RECONTADO direto dos dados do recorte (ou, para os tres
# ultimos, da construcao do banco completo) apareca nos docs gerados por R/05, e bata com o ultimo
# numero registrado quando existe assinatura correspondente (recorte -> v1_*, banco completo -> bocel_*)
# 29/09/2026 (achado F5): doc recebia o proprio recontado e bate era verdadeira por construcao. Agora doc e o numero
# EXTRAIDO do texto dos docs por um padrao de contexto (um grupo de captura) e bate compara o que o texto diz com o que os
# dados dao. As duas linhas de senadores 2018 nao tem frase nos docs e ficam com doc NA (tem os proprios testes abaixo);
# padrao que deixa de casar por mudanca de redacao derruba a checagem, em vez de passar em silencio.
PADROES_DOC <- c(
  "\\*\\*Pessoas:\\*\\*\\s+([0-9.]+)",
  "\\*\\*Mandatos:\\*\\*\\s+([0-9.]+)",
  "\\*\\*Posi\u00e7\u00f5es pessoa \u00d7 cargo \u00d7 ano:\\*\\*\\s+([0-9.]+)",
  "com\\s+([0-9.]+)\\s+registros\\s+de\\s+filia\u00e7\u00e3o\\s+vindos",
  "cobre\\s+([0-9.]+)\\s+das\\s+[0-9.]+\\s+pessoas",
  "pessoas\\s+\\(([0-9,]+%)\\),\\s+com",
  "\\|\\s*federal\\s*\\|\\s*[0-9.]+\\s*\\|\\s*([0-9.]+)\\s+\\([0-9,]+%\\)",
  "\\|\\s*federal\\s*\\|\\s*[0-9.]+\\s*\\|\\s*[0-9.]+\\s+\\(([0-9,]+%)\\)",
  "\\|\\s*estadual\\s*\\|\\s*[0-9.]+\\s*\\|\\s*([0-9.]+)\\s+\\([0-9,]+%\\)",
  "\\|\\s*estadual\\s*\\|\\s*[0-9.]+\\s*\\|\\s*[0-9.]+\\s+\\(([0-9,]+%)\\)",
  "dos\\s+[0-9.]+\\s+mandatos,\\s+([0-9.]+)\\s+t\u00eam\\s+a\\s+condi\u00e7\u00e3o\\s+de\\s+eleito\\s+lida\\s+do\\s+cadastro",
  "cadastro,\\s+([0-9.]+)\\s+herdada\\s+do\\s+titular",
  "\\(vices\\),\\s+([0-9.]+)\\s+atribu\u00edda\\s+pela\\s+vota\u00e7\u00e3o",
  "vota\u00e7\u00e3o\\s+e\\s+([0-9.]+)\\s+corrigida\\s+pela\\s+fonte\\s+oficial",
  "\\|\\s*titulo\\s*\\|\\s*([0-9.]+)\\s*\\|",
  NA_character_, NA_character_,
  "banco\\s+completo,\\s+antes\\s+do\\s+recorte\\s+da\\s+v1\\.0,\\s+([0-9.]+)\\s+mandatos\\s+majorit\u00e1rios",
  ",\\s+([0-9.]+)\\s+candidaturas\\s+repetidas\\s+na\\s+mesma\\s+posi\u00e7\u00e3o",
  "e\\s+([0-9.]+)\\s+registros\\s+eleitos\\s+sem\\s+nome")
stopifnot(length(PADROES_DOC) == nrow(recontas))
docs_linhas <- paste(c(readLines("docs/README.md"), readLines("docs/NOTA_DE_COBERTURA.md"),
                       readLines("docs/AMARRACOES_FAPESP.md")), collapse = "\n")
extrai_doc <- function(pad) {
  if (is.na(pad)) return(NA_character_)
  mt <- regmatches(docs_linhas, regexec(pad, docs_linhas, perl = TRUE))[[1]]
  if (length(mt) < 2L) NA_character_ else mt[2L]
}
recontas[, doc := vapply(PADROES_DOC, extrai_doc, character(1))]
recontas[, citado_nos_docs := vapply(recontado, cita, logical(1))]
recontas[, bate := fifelse(is.na(doc), NA, doc == recontado)]
cat("\n== Numeros recontados ==\n"); print(recontas)
fwrite(recontas, "output/verificacao/documentacao_numeros_recontados.csv")
ok(sprintf("%d numeros dos docs batem com a recontagem", nrow(recontas)),
   stopifnot(all(recontas$bate, na.rm = TRUE), sum(!is.na(PADROES_DOC)) == 18L,
             recontas[!is.na(PADROES_DOC), !anyNA(doc)], recontas[is.na(PADROES_DOC), .N] == 2L))
ok("numeros citados existem nos docs", stopifnot(all(recontas$citado_nos_docs)))
# chaves de assinatura, quando existem, batem com os dados
chk <- recontas[!is.na(assinatura) & !grepl("%", doc)]
ok("numeros dos docs batem com o ultimo registro de numeros_assinatura.txt",
   stopifnot(all(gsub("\\.", "", chk$doc) == chk$assinatura)))
ok("pct pessoas com filiacao: doc bate com assinatura v1_pessoas_com_filiacao / v1_n_pessoas",
   stopifnot(sprintf("%.1f", 100 * as.numeric(sig("v1_pessoas_com_filiacao")) / as.numeric(sig("v1_n_pessoas"))) ==
             sub("%", "", sub(",", ".", recontas[numero == "pct pessoas com filiacao", recontado], fixed = TRUE), fixed = TRUE)))
# tabela de mandatos por ano/esfera da nota
nc <- readLines("docs/NOTA_DE_COBERTURA.md")
# so a tabela da secao 'O que esta completo' (a secao de posse tem outra tabela por ano x esfera)
fim_sec <- grep("^## Posse", nc); nc_top <- if (length(fim_sec)) nc[seq_len(fim_sec[1] - 1)] else nc
tab <- grep("^\\| (19|20)[0-9]{2} \\| ", nc_top, value = TRUE)
tb <- rbindlist(lapply(strsplit(tab, "\\|"), function(p) data.table(ano_eleicao = trimws(p[2]), esfera = trimws(p[3]),
                                                                   n_doc = as.integer(gsub("\\.", "", trimws(p[4]))))))
cob <- mand[, .(n = .N), by = .(ano_eleicao, esfera)]
# 21/09/2026: o recorte da v1.0 tem as sete eleicoes gerais de 1998 a 2022 nas esferas federal e
# estadual, 14 celulas; ano ou esfera faltando ou sobrando reprova
ANOS_V1 <- as.character(seq(1998, 2022, 4)); ESFERAS_V1 <- c("federal", "estadual")
n_celulas_esperado <- length(ANOS_V1) * length(ESFERAS_V1)
tb <- join_seguro(as.data.frame(tb), as.data.frame(cob), by = c("ano_eleicao", "esfera"), cardinalidade = "one-to-one", tipo = "inner")
ok("nota: tabela mandatos por ano x esfera bate integralmente com o recorte",
   stopifnot(setequal(as.character(cob$ano_eleicao), ANOS_V1), setequal(cob$esfera, ESFERAS_V1),
             nrow(cob) == n_celulas_esperado, nrow(tb) == nrow(cob), all(tb$n_doc == tb$n)))
# vocabularios fechados declarados
ok("mandatos.forma_saida dentro do vocabulario do livro",
   in_set(mand$forma_saida, c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","nao_tomou_posse",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
                              "suplente_efetivado","perda_do_mandato_inferida_por_eleicao_suplementar","substituicao_inferida_munic","assumiu_titular","aposentadoria","impeachment","retotalizacao",
                              "outro","nao_observado"), permitir_na = FALSE))
ok("mandatos.fonte_forma_saida dentro do vocabulario do livro",
   # 12/09/2026: as quatro fontes que entraram entre 30/08 e 04/09 (sapl_observacao, assembleia_portal,
  # assembleia_inventario, cargo_incompativel) ja estavam no livro e em R/verifica_integracao.R, e faltavam aqui
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
  # 21/09/2026: rotulos da regra A1 para o evento curado de governo e Presidencia (lib/tipo_fonte.R)
  in_set(mand$fonte_forma_saida, c("camara_api","senado_api","camara_biografia","fonte_oficial_curada","base_dhbb_curada","noticia_orgao_publico_curada","pista_nao_oficial","assembleia_api","sapl_municipal","portal_camara","tce","assembleia_historico","diario_oficial","wikipedia","wikidata","wikidata_obito","tse_suplementar","ibge_munic","derivado_titular","data_fim_efetiva",
                                   "sapl_observacao","assembleia_portal","assembleia_inventario","cargo_incompativel")))
ok("mandatos.fonte_situacao dentro do vocabulario", in_set(mand$fonte_situacao, c("cadastro","votacao","imputacao_titular","imputacao_votos","fonte_oficial_casa"), permitir_na = FALSE))
ok("mandatos.esfera", in_set(mand$esfera, c("municipal","estadual","federal"), permitir_na = FALSE))
ok("pessoas.chave_dedup", in_set(pess$chave_dedup, c("titulo","cpf","nome_nascimento"), permitir_na = FALSE))
ok("mandatos.ano_eleicao em 1998..2024", em_faixa(as.integer(mand$ano_eleicao), 1998, 2024, permitir_na = FALSE))
ok("posicoes_ano.ano em 1999..2030 (senador 2022 vai a jan/2031)", em_faixa(as.integer(pos$ano), 1999, 2030, permitir_na = FALSE))
ok("pessoas.dedup_auditoria dentro do vocabulario do livro",
   in_set(pess$dedup_auditoria, c("consistente","suspeito","indeterminado","nao_auditado"), permitir_na = FALSE))
# fonte_exercicio declarada 'ibge_munic; tse_reeleicao' — checar se ha valor composto e coerencia com exercicio_confirmado
fe <- mand[, .N, by = .(fonte_exercicio, tem_data = !is.na(exercicio_confirmado))]
cat("\nfonte_exercicio x exercicio_confirmado:\n"); print(fe)
ok("mandatos: fonte_exercicio preenchida implica exercicio_confirmado preenchido",
   stopifnot(mand[!is.na(fonte_exercicio) & is.na(exercicio_confirmado), .N] == 0))

## ------------------------------------------------ nota de cobertura declara as lacunas
lac <- list(
  legislatura_51_camara = "legislatura 51|51ª legislatura|1999-2003",
  suplementares_so_majoritarias = "majoritári",
  precisao_pareamento_nome = "homônim|homonim|nome e nascimento|nome \\+ nascimento")
nota_txt <- paste(nc, collapse = "\n")
for (k in names(lac)) ok(sprintf("nota de cobertura declara: %s", k), stopifnot(grepl(lac[[k]], nota_txt, ignore.case = TRUE)))
ok("nota de cobertura NAO contradiz a camada suplementar ('não entram na v1')",
   stopifnot(!grepl("não entram na v1", nota_txt)))
# camara: leg 51 sem periodo de exercicio (recontado)
cam <- dados[["exercicio_camara.csv"]]
n51 <- cam[legislatura == "51" & !is.na(data_inicio_exercicio), .N]
registrar_numero("doc_camara_leg51_linhas_com_data_inicio", n51, script = script)
registrar_numero("doc_camara_leg51_linhas", cam[legislatura == "51", .N], script = script)
sup <- dados[["eleicoes_suplementares.csv"]]
registrar_numero("doc_suplementares_cargos", paste(sort(unique(sup$cargo)), collapse = ";"), script = script)
aud <- dados[["auditoria_homonimos.csv"]]
registrar_numero("doc_auditoria_homonimos_n", nrow(aud), script = script)
registrar_numero("doc_auditoria_homonimos_suspeitos", aud[classificacao == "suspeito", .N], script = script)
registrar_numero("doc_pessoas_dedup_ponte_nome_nascimento", pess[dedup_ponte_nome_nascimento == "TRUE", .N], script = script)

## ------------------------------------------------ README
rd <- paste(readLines("docs/README.md"), collapse = "\n")
ok("README: como citar", stopifnot(grepl("## Como citar", rd)))
# 29/08/2026: a licenca dos dados passou a CC0 1.0 (docs/PADRAO_GELAPE.md)
  # 12/09/2026: CC BY 4.0, a licenca declarada no projeto, no plano de gestao de dados e na sumula da
  # FAPESP (versoes de 6 e 7/09); a checagem continua exigindo a licenca nomeada no README e passa a reprovar a CC0
  ok("README: licenca CC BY 4.0", stopifnot(grepl("CC BY 4\\.0", rd), !grepl("CC0 1\\.0", rd)))
ok("README: contato", stopifnot(grepl("igornovaeslins@gmail.com", rd), grepl("ORCID", rd)))
# 29/09/2026 (v1.1): os dois e-mails do autor (pessoal e institucional) aparecem no README, no CITATION.cff e nos
# dois textos do depositor do Zenodo (portugues e ingles), e a secao de mudancas da versao tem o titulo que
# zenodo/publicar_github.sh procura (linha de titulo com "mudan" e a tag da versao)
EMAILS_AUTOR <- c("igornovaeslins@gmail.com", "igor.lins@cebrap.org.br")
ok("README: os dois e-mails do autor", stopifnot(all(vapply(EMAILS_AUTOR, grepl, logical(1), x = rd, fixed = TRUE))))
ok("README: secao de mudancas da versao (titulo com 'mudan' e a tag, como publicar_github.sh procura)",
   stopifnot(any(grepl("^#+ ", strsplit(rd, "\n")[[1]]) & grepl("mudan", strsplit(rd, "\n")[[1]], ignore.case = TRUE) &
                   grepl(VERSAO_TAG, strsplit(rd, "\n")[[1]], fixed = TRUE))))
cff_txt <- readLines("CITATION.cff")
ok("CITATION.cff: os dois e-mails do autor",
   stopifnot(all(vapply(EMAILS_AUTOR, function(e) any(grepl(e, cff_txt, fixed = TRUE)), logical(1)))))
ok("CITATION.cff: e-mail institucional em authors e os dois e-mails em contact",
   { i_aut <- grep("^authors:", cff_txt); i_con <- grep("^contact:", cff_txt); i_ver <- grep("^version:", cff_txt)
     stopifnot(length(i_aut) == 1L, length(i_con) == 1L, i_aut < i_con, i_con < i_ver,
               any(grepl("igor.lins@cebrap.org.br", cff_txt[i_aut:(i_con - 1L)], fixed = TRUE)),
               all(vapply(EMAILS_AUTOR, function(e) any(grepl(e, cff_txt[i_con:(i_ver - 1L)], fixed = TRUE)), logical(1)))) })
for (idm in c("Contato do autor", "Author's contact")) {
  lin <- grep(idm, dep, fixed = TRUE)
  ok(sprintf("deposit.py: '%s' traz os dois e-mails", idm),
     stopifnot(length(lin) == 1L, all(vapply(EMAILS_AUTOR, grepl, logical(1), x = dep[lin], fixed = TRUE))))
}
ok(paste0("README: versao ", VERSAO_TAG), stopifnot(grepl("## Versão", rd), grepl(VERSAO_TAG, rd, fixed = TRUE)))
# 29/09/2026 (achado DOC-02): a README e o CITATION.cff da v1.1 traziam o DOI da v1.0 como se fosse o da v1.1, e o
# verificador passava. O DOI de versao so pode ser citado quando o deposito_info.json descreve a versao corrente (campo
# versao, ausente = 1.0); antes disso so o DOI do conceito pode aparecer, e o DOI de publicado.json (a versao ja publicada)
# nao pode ser citado como o da versao corrente.
VERSAO_NUM <- trimws(read.dcf("VERSAO")[1, "versao"])
dep_info_v <- tryCatch(jsonlite::fromJSON("zenodo/deposito_info.json"), error = function(e) NULL)
ver_dep <- if (is.null(dep_info_v)) NA_character_ else if (is.null(dep_info_v$versao)) "1.0" else sub("^v", "", as.character(dep_info_v$versao))
dois_citados <- unique(unlist(regmatches(c(rd, cff_txt), gregexpr("10\\.5281/zenodo\\.[0-9]+", c(rd, cff_txt)))))
registrar_numero("doc_doi_citados_na_readme_e_cff", length(dois_citados), script = script)
registrar_numero("doc_doi_de_versao_citado_na_readme_e_cff", as.integer(any(dois_citados != dep_info_v$conceptdoi_provisorio)), script = script)
ok("README e CITATION.cff: so o DOI do conceito, ou o DOI de versao do deposito_info que descreve a versao corrente",
   { permitidos <- c(dep_info_v$conceptdoi_provisorio, if (isTRUE(ver_dep == VERSAO_NUM)) dep_info_v$doi_reservado)
     pub_doi <- tryCatch(jsonlite::fromJSON("zenodo/publicado.json")$doi, error = function(e) NA_character_)
     if (!all(dois_citados %in% permitidos))
       stop("DOI citado que nao e o do conceito nem o da versao corrente (v", VERSAO_NUM, "; deposito_info descreve a v", ver_dep, "): ",
            paste(setdiff(dois_citados, permitidos), collapse = ", "))
     stopifnot(!(pub_doi %in% dois_citados) || isTRUE(ver_dep == VERSAO_NUM)) })
ok("README: todo CSV depositado esta na tabela de arquivos",
   stopifnot(all(vapply(basename(csvs[file.exists(csvs)]), function(b) grepl(b, rd, fixed = TRUE) || grepl(sub("\\.csv$", ".csv/.parquet", b), rd, fixed = TRUE), logical(1)))))
# DOI placeholder
if (grepl("\\[DOI reservado no Zenodo\\]", rd)) message("AVISO: README ainda com placeholder de DOI (zenodo/deposito_info.json ausente)")

## ------------------------------------------------ camada de ocupacao: vocabulario no livro e pares sobrepostos
# 29/09/2026 (v1.1): todo valor de vinculo_cadeira, origem_data_inicio e origem_data_fim presente em ocupacoes.csv
# tem definicao no livro de codigos, os docs gerados nao citam a tabela explicita de tres cadeiras (liga_ocu03) que a
# regra geral do R/42 substituiu, e os pares de ocupacoes de suplentes sobrepostos citados na nota de cobertura
# batem com uma recontagem feita aqui a partir de ocupacoes.csv, mandatos.csv e EXCECOES_CONHECIDAS.csv
oc <- dados[["ocupacoes.csv"]]
i_oc <- sec_idx[startsWith(lc[sec_idx], "## ocupacoes.csv")]
stopifnot(length(i_oc) == 1L)
f_oc <- c(sec_idx[sec_idx > i_oc], length(lc) + 1L)[1] - 1L
desc_oc <- function(v) {
  l <- grep(sprintf("^\\| `%s` \\|", v), lc[i_oc:f_oc], value = TRUE)
  stopifnot(length(l) == 1L)
  strsplit(l, "\\|")[[1]][4]
}
for (v in c("vinculo_cadeira", "origem_data_inicio", "origem_data_fim")) {
  ok(sprintf("livro: todo valor de ocupacoes.%s presente nos dados esta definido", v), {
    vals <- sort(unique(oc[[v]][!is.na(oc[[v]]) & oc[[v]] != ""]))
    d <- desc_oc(v)
    # 29/09/2026 (achado F8): a palavra inteira em qualquer ponto da descricao deixava 'fonte' passar sem definicao, porque
    # a palavra reaparece em outras frases ('a fonte da ocupacao', 'coluna fonte'). Agora o valor tem de estar em posicao
    # de definicao, isto e, seguido de 'quando', 'no', 'na', 'nas' ou 'nos', que e como o livro define cada valor.
    sem <- vals[!vapply(vals, function(x) grepl(paste0("(?<![a-z_])", x, "\\s+(quando|no|na|nas|nos)\\b"), d, perl = TRUE), logical(1))]
    if (length(sem)) stop("sem definicao no livro: ", paste(sem, collapse = ", "))
    stopifnot(length(vals) > 0L)
  })
}
# 29/09/2026 (achado F4): os parametros da regra de vinculo escritos na definicao de vinculo_cadeira (60, 31, 3, 365 e 30
# dias) tem de bater com as constantes do R/42 lidas do codigo aqui, de forma independente do R/05, que as interpola. Se o
# R/42 mudar uma constante e o livro nao for regenerado, a checagem reprova.
r42_v <- readLines("R/42_ocupacoes.R", warn = FALSE)
c42_v <- function(nome) {
  l <- grep(sprintf("^ *%s *<- *[0-9]+L", nome), r42_v, value = TRUE)
  stopifnot(length(l) == 1L)
  as.integer(sub(sprintf("^ *%s *<- *([0-9]+)L.*$", nome), "\\1", l))
}
l_atraso_v <- grep("atraso >= -[0-9]+L & atraso <= [0-9]+L", r42_v, value = TRUE)
stopifnot(length(l_atraso_v) == 1L)
num_do_livro <- function(d, pad) {
  mt <- regmatches(d, regexec(pad, d, perl = TRUE))[[1]]
  if (length(mt) < 2L) stop("trecho nao encontrado na definicao de vinculo_cadeira: ", pad)
  as.integer(mt[-1L])
}
# 29/09/2026 (achado F1): a README dizia que a data de saida tambem vinha da biografia para tantos suplentes quantos
# tinham a data de entrada nela (usava o mesmo n nos dois lados), e nenhum verificador cruzava a frase com origem_data_fim.
# Aqui a frase da README e a da nota de cobertura sobre a regra geral em AL 1998 sao lidas do texto e comparadas com a
# recontagem de origem_data_inicio e origem_data_fim em ocupacoes.csv.
n_ini_bio_v <- oc[origem_data_inicio == "biografia_camara", .N]
n_fim_bio_v <- oc[origem_data_fim == "biografia_camara", .N]
n_fim_conv_v <- oc[origem_data_fim == "convencao_fim_legislatura", .N]
registrar_numero("doc_ocu_data_inicio_biografia_camara_recontado", n_ini_bio_v, script = script)
registrar_numero("doc_ocu_data_fim_biografia_camara_recontado", n_fim_bio_v, script = script)
registrar_numero("doc_ocu_data_fim_convencao_fim_legislatura_recontado", n_fim_conv_v, script = script)
ok("README e nota: entradas e saidas lidas da biografia e da convencao de fim de legislatura batem com origem_data_inicio e origem_data_fim",
   { rd_plano <- gsub("\\s+", " ", rd); nota_p <- gsub("\\s+", " ", paste(nc, collapse = " "))
     m_rd <- regmatches(rd_plano, regexec("as datas de entrada de ([0-9.]+) suplentes e a data de sa.da de ([0-9.]+) deles v.m da biografia oficial da C.mara \\(a sa.da de ([0-9.]+) deles segue a conven..o de fim de legislatura\\)", rd_plano, perl = TRUE))[[1]]
     if (length(m_rd) != 4L) stop("frase da README sobre entradas e saidas da biografia nao encontrada")
     m_nc <- regmatches(nota_p, regexec("a regra deu data a ([0-9.]+) in.cios e a ([0-9.]+) fins \\(([0-9.]+) lido na biografia e ([0-9.]+) pela conven..o\\)", nota_p, perl = TRUE))[[1]]
     if (length(m_nc) != 5L) stop("frase da nota sobre inicios e fins nao encontrada")
     num <- function(x) as.integer(gsub("\\.", "", x))
     stopifnot(num(m_rd[2]) == n_ini_bio_v, num(m_rd[3]) == n_fim_bio_v, num(m_rd[4]) == n_fim_conv_v,
               num(m_nc[2]) == n_ini_bio_v, num(m_nc[3]) == n_fim_bio_v + n_fim_conv_v,
               num(m_nc[4]) == n_fim_bio_v, num(m_nc[5]) == n_fim_conv_v,
               n_fim_bio_v + n_fim_conv_v > 0L) })
ok("livro: constantes da regra de vinculo (60, 31, 3, 365 e 30 dias) batem com o R/42",
   { d_vc <- gsub("\\s+", " ", desc_oc("vinculo_cadeira"))
     stopifnot(
       identical(num_do_livro(d_vc, "tomou posse mais de ([0-9]+) dias depois do in"), c42_v("JANELA_TITULAR")),
       identical(num_do_livro(d_vc, "de ([0-9]+) dias antes a [0-9]+ dias depois de mandato_inicio"), c42_v("JANELA_EFET")),
       identical(num_do_livro(d_vc, "de [0-9]+ dias antes a ([0-9]+) dias depois de mandato_inicio"), c42_v("JANELA_EFET")),
       identical(num_do_livro(d_vc, "cai de ([0-9]+) dias antes a [0-9]+ dias depois da sa"), c42_v("TOL_DIAS")),
       identical(num_do_livro(d_vc, "cai de [0-9]+ dias antes a ([0-9]+) dias depois da sa"), c42_v("JANELA_EFET")),
       identical(num_do_livro(d_vc, "at. ([0-9]+) dias antes da entrada ou at. [0-9]+ dias depois dela"),
                 as.integer(sub(".*atraso <= ([0-9]+)L.*", "\\1", l_atraso_v))),
       identical(num_do_livro(d_vc, "at. [0-9]+ dias antes da entrada ou at. ([0-9]+) dias depois dela"),
                 as.integer(sub(".*atraso >= -([0-9]+)L.*", "\\1", l_atraso_v)))) })
ok("docs geradas nao citam liga_ocu03 nem OCU-03 (tabela explicita de tres cadeiras substituida pela regra geral)",
   stopifnot(!grepl("liga_ocu03|OCU-03", paste(c(lc, readLines("docs/README.md"), readLines("docs/NOTA_DE_COBERTURA.md")), collapse = "\n"))))
iv_v <- merge(oc[!is.na(id_mandato) & !is.na(id_pessoa) & !is.na(data_inicio) & tipo_ocupante != "titular",
                 .(id_ocupacao, id_mandato, id_pessoa, ini = as.IDate(data_inicio), f0 = as.IDate(data_fim))],
              mand[, .(id_mandato, mandato_fim)], by = "id_mandato")
iv_v[, fim := fifelse(is.na(f0), as.IDate(mandato_fim), f0)]
pares_ns <- merge(iv_v, iv_v, by = "id_mandato", suffixes = c(".a", ".b"), allow.cartesian = TRUE)[id_ocupacao.a < id_ocupacao.b]
pares_ns[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
pares_ns <- pares_ns[dias > 3L]
pares_ns[, chave_par := paste(id_ocupacao.a, id_ocupacao.b, sep = "|")]
exc_v <- fread("docs/EXCECOES_CONHECIDAS.csv", colClasses = "character")
exc_v <- exc_v[grepl("auditoria de 24/09/2026, par [0-9]+\\)", descricao)]
exc_v[, chave_par := vapply(descricao, function(d) paste(sort(unique(regmatches(d, gregexpr("OC[0-9]{8}", d))[[1]]))[1:2], collapse = "|"), "")]
# 29/09/2026 (achado F2): n_sub_v era o complemento (total menos documentados), e um par novo na base entraria como
# substituicao legitima sem evidencia. Agora so conta como substituicao dentro da substituicao o par cuja chave (as duas
# ocupacoes) esta na tabela congelada da auditoria de 24/09/2026, ref/pares_substituicao_legitima_auditoria_24set.csv,
# gerada de logs/auditoria_v11_24set/scripts/pares21/pares21_classificados.csv (categoria sub_substituicao_legitima).
# Par que nao esta nem em EXCECOES_CONHECIDAS.csv nem nessa tabela reprova, em vez de ser aceito por exclusao.
ref_sub <- fread("ref/pares_substituicao_legitima_auditoria_24set.csv", colClasses = "character")
n_ns_v <- nrow(pares_ns); n_doc_v <- exc_v[chave_par %in% pares_ns$chave_par, .N]
nd_v <- pares_ns[!chave_par %in% exc_v$chave_par]
n_sub_v <- nd_v[chave_par %in% ref_sub$chave_par, .N]
n_fora_v <- nrow(nd_v) - n_sub_v
registrar_numero("doc_ocu_pares_suplentes_sobrepostos_recontado", n_ns_v, script = script)
registrar_numero("doc_ocu_pares_documentados_em_excecoes_recontado", n_doc_v, script = script)
registrar_numero("doc_ocu_pares_substituicao_dentro_da_substituicao_recontado", n_sub_v, script = script)
registrar_numero("doc_ocu_pares_sem_documentacao_nem_auditoria_recontado", n_fora_v, script = script)
registrar_numero("doc_ocu_pares_suplentes_sobrepostos_mesma_pessoa_recontado", pares_ns[id_pessoa.a == id_pessoa.b, .N], script = script)
nota_plana <- gsub("\\s+", " ", paste(nc, collapse = " "))
ok("nota: pares de ocupacoes de suplentes sobrepostos (total, substituicao dentro da substituicao, documentados) = recontagem",
   stopifnot(n_ns_v > 0L, n_doc_v > 0L, n_sub_v > 0L,
             grepl(sprintf("%s pares de ocupações de suplentes compartilham mais de 3 dias da mesma cadeira", fmt(n_ns_v)), nota_plana, fixed = TRUE),
             grepl(sprintf("separou %s desses pares como substituição dentro da substituição", fmt(n_sub_v)), nota_plana, fixed = TRUE),
             grepl(sprintf("Os outros %s pares estão documentados linha a linha", fmt(n_doc_v)), nota_plana, fixed = TRUE),
             sig("doc_ocu_pares_suplentes_sobrepostos") == as.character(n_ns_v),
             sig("doc_ocu_pares_substituicao_dentro_da_substituicao") == as.character(n_sub_v),
             sig("doc_ocu_pares_documentados_em_excecoes") == as.character(n_doc_v)))
ok("todo par sobreposto fora de EXCECOES_CONHECIDAS.csv esta na tabela congelada da auditoria de 24/09 (mesma cadeira e mesmas ocupacoes)",
   { stopifnot(n_fora_v == 0L, nrow(ref_sub) == 14L, uniqueN(ref_sub$chave_par) == 14L,
               setequal(nd_v$chave_par, ref_sub$chave_par),
               all(ref_sub$categoria == "sub_substituicao_legitima"))
     m_ref <- merge(nd_v[, .(chave_par, id_mandato)], ref_sub[, .(chave_par, id_mandato_ref = id_mandato)], by = "chave_par")
     stopifnot(nrow(m_ref) == 14L, all(m_ref$id_mandato == m_ref$id_mandato_ref)) })
ok("nota: afirma que nenhum par sobreposto reune a mesma pessoa somente se a recontagem confirma",
   stopifnot(pares_ns[id_pessoa.a == id_pessoa.b, .N] == 0L, grepl("nenhum deles reúne a mesma pessoa duas vezes", nota_plana, fixed = TRUE)))

## ------------------------------------------------ EXCECOES_CONHECIDAS
exc <- fread("docs/EXCECOES_CONHECIDAS.csv", colClasses = "character")
ok("excecoes: colunas esperadas", stopifnot(identical(names(exc), c("ano_eleicao","cargo","sg_uf","delta_esperado","descricao"))))
# 13/09/2026: a excecao de MT 2018 foi resolvida (cadeira de Selma Arruda restaurada por ref/correcoes_eleitos_fonte_oficial.csv)
ok("senadores 2018 = 54 no banco", stopifnot(mand[ano_eleicao == "2018" & cargo == "SENADOR", .N] == 54))
ok("MT 2018 tem 2 senadores", stopifnot(mand[ano_eleicao == "2018" & cargo == "SENADOR" & sg_uf == "MT", .N] == 2))

# 29/09/2026 (achado F6): o registro e append-only e guarda o ultimo valor de ocu_tres_cadeiras_ligadas_ocu03 (3, de
# 24/09/2026), da tabela explicita de tres cadeiras que a regra geral do R/42 substituiu. A chave nao e editada; este
# verificador confere que nenhum script a le ou a escreve mais e grava no proprio registro que ela e historica.
CHAVE_HISTORICA <- "ocu_tres_cadeiras_ligadas_ocu03"
scripts_r <- setdiff(list.files("R", pattern = "\\.(R|sh)$", recursive = TRUE, full.names = TRUE), script)
ok("registro: a chave ocu_tres_cadeiras_ligadas_ocu03 (regra explicita de tres cadeiras, substituida) nao e lida nem escrita por nenhum script",
   stopifnot(!any(vapply(scripts_r, function(f) any(grepl(CHAVE_HISTORICA, readLines(f, warn = FALSE), fixed = TRUE)), logical(1)))))
registrar_numero("doc_chave_historica_ocu_tres_cadeiras_ligadas_ocu03_descontinuada", 1L, script = script)
fora <- c("pertinencia semantica das descricoes do livro de codigos",
          "validade do pareamento por nome nas fontes complementares (fora do escopo documental)",
          "a chave ocu_tres_cadeiras_ligadas_ocu03 de numeros_assinatura.txt guarda o valor 3 de 24/09/2026, de uma regra que a v1.1 substituiu, e nenhum texto gerado a cita; o registro nao se edita",
          "cruzamento das citacoes de fonte da nota de cobertura com o texto original da biografia oficial (fica com o julgamento de Igor)")
rel <- gravar_relatorio_verificacao(alvo = paste0("BOCEL ", VERSAO_TAG, " — documentacao"), script = script,
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) { cat("Falhas:\n"); cat(paste("-", falhou), sep = "\n"); quit(status = 1) }
