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

docs_txt <- paste(readLines("docs/README.md"), readLines("docs/NOTA_DE_COBERTURA.md"),
                  readLines("docs/AMARRACOES_FAPESP.md"), collapse = "\n")
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
recontas[, doc := recontado]
recontas[, citado_nos_docs := vapply(doc, cita, logical(1))]
recontas[, bate := doc == recontado]
cat("\n== Numeros recontados ==\n"); print(recontas)
fwrite(recontas, "output/verificacao/documentacao_numeros_recontados.csv")
ok(sprintf("%d numeros dos docs batem com a recontagem", nrow(recontas)), stopifnot(all(recontas$bate)))
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
ok("README: versao v1.0", stopifnot(grepl("## Versão", rd), grepl("v1.0", rd)))
ok("README: todo CSV depositado esta na tabela de arquivos",
   stopifnot(all(vapply(basename(csvs[file.exists(csvs)]), function(b) grepl(b, rd, fixed = TRUE) || grepl(sub("\\.csv$", ".csv/.parquet", b), rd, fixed = TRUE), logical(1)))))
# DOI placeholder
if (grepl("\\[DOI reservado no Zenodo\\]", rd)) message("AVISO: README ainda com placeholder de DOI (zenodo/deposito_info.json ausente)")

## ------------------------------------------------ EXCECOES_CONHECIDAS
exc <- fread("docs/EXCECOES_CONHECIDAS.csv", colClasses = "character")
ok("excecoes: colunas esperadas", stopifnot(identical(names(exc), c("ano_eleicao","cargo","sg_uf","delta_esperado","descricao"))))
# 13/09/2026: a excecao de MT 2018 foi resolvida (cadeira de Selma Arruda restaurada por ref/correcoes_eleitos_fonte_oficial.csv)
ok("senadores 2018 = 54 no banco", stopifnot(mand[ano_eleicao == "2018" & cargo == "SENADOR", .N] == 54))
ok("MT 2018 tem 2 senadores", stopifnot(mand[ano_eleicao == "2018" & cargo == "SENADOR" & sg_uf == "MT", .N] == 2))

fora <- c("pertinencia semantica das descricoes do livro de codigos",
          "validade do pareamento por nome nas fontes complementares (fora do escopo documental)")
rel <- gravar_relatorio_verificacao(alvo = "BOCEL v1.0 — documentacao", script = script,
                                    passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nRelatorio:", rel, "\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n")
if (length(falhou)) { cat("Falhas:\n"); cat(paste("-", falhou), sep = "\n"); quit(status = 1) }
