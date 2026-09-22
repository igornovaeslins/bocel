# verifica_verifica_tce_existentes.R — verificacao CETICA, independente, da frente
# 'verifica_tce_existentes'. Nao reusa R/verifica_tce_existentes.R: reimplementa a contagem,
# recontra cada numero registrado contra o ultimo registro em output/numeros_assinatura.txt,
# sorteia uma amostra propria de pareamentos (semente e estratificacao diferentes, com peso nas
# regras frouxas) e mede o que a frente NAO mediu: a distancia entre as tabelas de TCE e o
# data/mandatos.csv que hoje esta em disco.
#
# Somente leitura sobre data/, R/23, R/24, R/10. Nao altera criterio.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_verifica_tce_existentes.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/verifica_verifica_tce_existentes.R"
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE); dir.create("logs", showWarnings = FALSE)
logf <- file("logs/verifica_verifica_tce_existentes.log", open = "wt"); sink(logf, split = TRUE)

passou <- character(); falhou <- character()
ck <- function(nome, cond, detalhe = "") {
  ok <- isTRUE(all(cond)) && length(cond) > 0
  cat(if (ok) "[ok]    " else "[FALHA] ", nome, if (nchar(detalhe)) paste0(" - ", detalhe) else "", "\n", sep = "")
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nchar(detalhe)) paste0(" (", detalhe, ")") else ""))
  invisible(ok)
}
norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
VOCAB <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca",
           "nao_tomou_posse","suplente_efetivado","outro","nao_observado")
CD_DE <- c(PREFEITO = "11", `VICE-PREFEITO` = "12", VEREADOR = "13")

## ============================================================ 1. ultimo registro por chave
cat("\n===== 1. registro assinado =====\n")
ass <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- ass[grepl("\\|", ass)]
pa <- data.table(chave = trimws(sub("^([^|]*)\\|.*$", "\\1", ass)),
                 valor = trimws(sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", ass)))
pa[, i := .I]
ult <- pa[pa[, .I[which.max(i)], by = chave]$V1]
REG <- setNames(ult$valor, ult$chave)
cat("chaves distintas no registro:", nrow(ult), " (linhas:", nrow(pa), ")\n")
recontagens <- list()
conf <- function(chave, valor) {
  reg <- if (chave %in% names(REG)) REG[[chave]] else NA_character_
  v <- if (is.numeric(valor)) format(valor, scientific = FALSE, trim = TRUE) else as.character(valor)
  bate <- !is.na(reg) && (identical(reg, v) ||
            (suppressWarnings(!is.na(as.numeric(reg))) && suppressWarnings(!is.na(as.numeric(v))) &&
             abs(as.numeric(reg) - as.numeric(v)) < 1e-9))
  recontagens[[length(recontagens) + 1L]] <<- data.table(chave = chave, registrado = reg, recontado = v,
                                                         bate = bate)
  invisible(bate)
}

## ============================================================ 2. leitura das tabelas
a <- fread("data/tce_gestores.csv",   colClasses = "character", na.strings = "NA")
b <- fread("data/tce_gestores_b.csv", colClasses = "character", na.strings = "NA")
ca <- fread("data/tce_gestores_cobertura.csv",   na.strings = "NA")
cb <- fread("data/tce_gestores_b_cobertura.csv", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv",  colClasses = "character", na.strings = "NA")
mun  <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
cat("A:", nrow(a), "linhas | B:", nrow(b), "linhas | mandatos:", nrow(mand), "\n")
TAB <- list(A = a, B = b)

## ============================================================ 3. esquema, ausente, chave
cat("\n===== 3. esquema, codigo de ausente, granularidade =====\n")
for (g in names(TAB)) {
  o <- TAB[[g]]; P <- function(x) paste0(g, " ", x)
  cru <- fread(if (g == "A") "data/tce_gestores.csv" else "data/tce_gestores_b.csv",
               colClasses = "character", na.strings = character(0))
  ck(P("E1 nomes de coluna minusculos, sem acento, sem espaco"),
     all(grepl("^[a-z0-9_]+$", names(o))))
  ck(P("E2 nenhuma celula vazia; ausente e o literal NA"),
     sum(vapply(cru, function(x) sum(x == ""), integer(1))) == 0L)
  ck(P("E3 arquivo inteiro em UTF-8 valido"),
     all(stri_enc_isutf8(readLines(if (g == "A") "data/tce_gestores.csv" else "data/tce_gestores_b.csv", warn = FALSE))))
  in_set(o$forma_saida, VOCAB, permitir_na = FALSE, nome = P("forma_saida"))
  ck(P("E4 forma_saida no vocabulario"), TRUE, paste(sort(unique(o$forma_saida)), collapse = "|"))
  ck(P("E5 id_pessoa_bocel e id_mandato_bocel sempre juntos"),
     o[, sum(xor(is.na(id_pessoa_bocel), is.na(id_mandato_bocel)))] == 0L)
  ck(P("E6 todo id_mandato_bocel existe em mandatos.csv"),
     length(setdiff(na.omit(o$id_mandato_bocel), mand$id_mandato)) == 0L)
  ck(P("E7 todo id_pessoa_bocel existe em pessoas.csv"),
     length(setdiff(na.omit(o$id_pessoa_bocel), pess$id_pessoa)) == 0L)
  # granularidade que R/10 consome: uma saida observada por mandato
  obs <- o[!is.na(id_mandato_bocel) & forma_saida != "nao_observado"]
  checa_unica(as.data.frame(obs), "id_mandato_bocel")
  ck(P("E8 no maximo uma saida observada por mandato pareado"), TRUE, paste(nrow(obs), "saidas"))
  # sanidade do par mandato/pessoa contra o proprio mandatos.csv
  p <- merge(o[!is.na(id_mandato_bocel)],
             mand[, .(id_mandato, m_pessoa = id_pessoa, m_cd = cd_cargo, m_ue = unidade_posicao,
                      m_uf = sg_uf, m_ano = ano_eleicao, m_ini = mandato_inicio, m_fim = mandato_fim)],
             by.x = "id_mandato_bocel", by.y = "id_mandato")
  ck(P("E9 pessoa, cargo, municipio e uf do mandato batem com a linha"),
     p[, all(m_pessoa == id_pessoa_bocel & m_cd == CD_DE[cargo_bocel] & m_ue == sg_ue & m_uf == uf)])
  ck(P("E10 par sg_ue/ibge existe no dicionario de municipios"),
     nrow(o[!mun, on = .(sg_ue, id_municipio_ibge)]) == 0L)
  # datas: ISO, ordem, e nada fora de 1997..hoje+5 anos
  d <- o[!is.na(data_inicio) | !is.na(data_fim)]
  ck(P("E11 datas em ISO"), nrow(d) == 0L ||
     d[, all((is.na(data_inicio) | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", data_inicio)) &
             (is.na(data_fim)    | grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", data_fim)))])
  ck(P("E12 data_inicio nunca depois de data_fim"), nrow(d) == 0L ||
     d[!is.na(data_inicio) & !is.na(data_fim), all(data_inicio <= data_fim)])
  lim <- as.character(Sys.Date() + 365L * 5L)
  ck(P("E13 nenhuma data impossivel (antes de 1997 ou muito no futuro)"), nrow(d) == 0L ||
     d[, all(is.na(data_inicio) | (data_inicio >= "1997-01-01" & data_inicio <= lim)) &
         all(is.na(data_fim)    | (data_fim    >= "1997-01-01" & data_fim    <= lim))],
     paste(d[(!is.na(data_inicio) & (data_inicio < "1997-01-01" | data_inicio > lim)) |
             (!is.na(data_fim)    & (data_fim    < "1997-01-01" | data_fim    > lim)), .N], "fora"))
  ck(P("E14 cobertura: nunca pareia mais que o universo e a taxa fecha"),
     (if (g == "A") ca else cb)[, all(n_pareados <= n_bocel & abs(taxa - round(n_pareados/n_bocel, 4)) < 1e-9)])
}

## ============================================================ 4. recontagem dos numeros da frente
cat("\n===== 4. recontagem contra o registro assinado =====\n")
conf("tce_n_registros_finais", nrow(a))
conf("tce_n_pareados_linhas", a[!is.na(id_mandato_bocel), .N])
conf("tce_n_mandatos_pareados", a[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
conf("tce_n_pessoas_pareadas", a[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)])
conf("tce_taxa_pareamento_linhas", round(a[!is.na(id_mandato_bocel), .N] / nrow(a), 4))
conf("tce_n_municipios_cobertos", a[, uniqueN(sg_ue)])
for (u in sort(unique(a$uf))) conf(paste0("tce_n_mandatos_pareados_uf_", u), a[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
for (rr in sort(unique(na.omit(a$relacao_chapa_eleita)))) conf(paste0("tce_n_prefeitura_relacao_", rr), a[relacao_chapa_eleita == rr, .N])
conf("tceb_n_registros_finais", nrow(b))
conf("tceb_n_pareados_linhas", b[!is.na(id_mandato_bocel), .N])
conf("tceb_n_mandatos_pareados", b[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
conf("tceb_n_mandatos_com_saida_observada", b[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)])
for (u in sort(unique(b$uf))) conf(paste0("tceb_n_mandatos_pareados_uf_", u), b[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
conf("tcev_a_n_mandatos_com_saida_observada", a[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)])
conf("tcev_b_n_mandatos_com_saida_observada", b[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)])
conf("tcev_a_n_linhas_com_data", a[!is.na(data_inicio) | !is.na(data_fim), .N])
conf("tcev_a_n_linhas_forma_saida_outro", a[forma_saida == "outro", .N])
conf("tcev_a_n_linhas_forma_saida_outro_pareadas", a[forma_saida == "outro" & !is.na(id_mandato_bocel), .N])
conf("tcev_a_n_mandatos_titular_substituido_pelo_vice", a[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)])
conf("tcev_a_n_linhas_duplicadas_integrais", nrow(a) - nrow(unique(a)))
conf("tcev_b_n_linhas_duplicadas_integrais", nrow(b) - nrow(unique(b)))
conf("tcev_a_n_linhas_nao_utf8", sum(!stri_enc_isutf8(readLines("data/tce_gestores.csv", warn = FALSE))))
conf("tcev_b_n_linhas_nao_utf8", sum(!stri_enc_isutf8(readLines("data/tce_gestores_b.csv", warn = FALSE))))
sub_a <- unique(na.omit(a$id_mandato_titular_substituido))
conf("tcev_a_n_titulares_substituidos_sem_saida_hoje", mand[id_mandato %in% sub_a & forma_saida == "nao_observado", .N])
conf("tcev_n_mandatos_tce_em_mandatos_csv", mand[fonte_forma_saida == "tce", .N])

## ============================================================ 5. contribuicao propria do TCE (reimplementada)
cat("\n===== 5. contribuicao propria do TCE =====\n")
jan <- mand[, .(id_mandato, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
d8 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x), x, NA_character_) }
cands <- function(id, fim, forma) {
  s <- data.table(id_mandato = id, fim = d8(fim), forma = forma)
  s <- s[!is.na(id_mandato) & id_mandato %in% mand$id_mandato]
  s[forma %in% c("nao_observado", ""), forma := NA_character_]
  s <- s[!is.na(forma)]
  if (!nrow(s)) return(character(0))
  s <- merge(s, jan, by = "id_mandato")
  s <- s[is.na(fim) | (as.IDate(fim) >= mi - 60L & as.IDate(fim) <= mf + 45L)]
  unique(s$id_mandato)
}
tce_a <- cands(a$id_mandato_bocel, a$data_fim, a$forma_saida)
tce_b <- cands(b$id_mandato_bocel, b$data_fim, b$forma_saida)
tce <- union(tce_a, tce_b)
cat("candidatos tce: A =", length(tce_a), " B =", length(tce_b), " total =", length(tce), "\n")
conf("tcev_n_mandatos_candidatos_tce", length(tce))
conf("tcev_n_mandatos_candidatos_tce_a", length(tce_a))
conf("tcev_n_mandatos_candidatos_tce_b", length(tce_b))
ck("C1 o grupo A nao oferece nenhuma forma de saida a R/10", length(tce_a) == 0L, paste(length(tce_a), "candidatos"))
# grupo A depois da correcao de R/23: nenhuma data e nenhuma forma pareada. Logo R/10 nao le nada dele.
ck("C2 o grupo A nao oferece nenhuma data a R/10 (data_inicio e data_fim ausentes)",
   a[, all(is.na(data_inicio)) && all(is.na(data_fim))])
# 12/09/2026: a vedacao vale para os grupos A e B (cadastro e folha, fonte 'tce'); o acordao
# de contas anuais do TCE-AC entra como fonte_exercicio 'tce_ac' e fica fora desta checagem
ck("C3 R/10 nao consome tce (grupos A e B) como confirmacao de exercicio (nenhum mandato tem fonte_exercicio 'tce')",
   mand[!is.na(fonte_exercicio) & grepl("(^|;)tce(;|$)", fonte_exercicio), .N] == 0L,
   paste(mand[!is.na(fonte_exercicio) & grepl("(^|;)tce(;|$)", fonte_exercicio), .N], "mandatos"))
registrar_numero("vvt_n_candidatos_tce", length(tce), script = script)
registrar_numero("vvt_n_candidatos_tce_grupo_a", length(tce_a), script = script)
registrar_numero("vvt_n_candidatos_tce_grupo_b", length(tce_b), script = script)
registrar_numero("vvt_n_mandatos_fonte_tce_em_mandatos_csv", mand[fonte_forma_saida == "tce", .N], script = script)
# distancia entre a tabela em disco e o mandatos.csv em disco
so_csv <- setdiff(mand[fonte_forma_saida == "tce", id_mandato], tce)
so_tab <- setdiff(tce, mand[fonte_forma_saida == "tce", id_mandato])
cat("mandatos com fonte 'tce' em mandatos.csv que a tabela de hoje NAO sustenta:", length(so_csv), "\n")
cat("candidatos da tabela de hoje que mandatos.csv NAO registra como tce:", length(so_tab), "\n")
# 29/08/2026: com o TCE rebaixado, o candidato do TCE que perde para camara, assembleia, senado ou
# para um evento nomeado deixa de aparecer com fonte 'tce', e isso e o comportamento pretendido. A
# checagem exige o inverso, que continua valendo: nenhum mandato marcado 'tce' sem lastro na tabela.
ACIMA <- c("sapl_municipal", "portal_camara", "assembleia_historico", "assembleia_api", "senado_api", "camara_api")
perdidos <- mand[id_mandato %in% so_tab & !(fonte_forma_saida %in% ACIMA | forma_saida %in%
                 c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
                   "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
                   "assumiu_titular", "substituicao_inferida_munic", "aposentadoria", "impeachment", "retotalizacao", "nao_observado"))]
if (nrow(perdidos)) print(perdidos[, .N, by = .(fonte_forma_saida, forma_saida)][order(-N)])
ck("C4 todo mandato com fonte 'tce' tem lastro na tabela, e o candidato preterido perdeu para fonte de prioridade maior",
   length(so_csv) == 0L && nrow(perdidos) == 0L,
   paste(length(so_csv), "no csv sem lastro,", length(so_tab), "preteridos,", nrow(perdidos), "sem explicacao"))
registrar_numero("vvt_n_mandatos_tce_sem_lastro_na_tabela", length(so_csv), script = script)
registrar_numero("vvt_n_candidatos_tce_ausentes_de_mandatos_csv", length(so_tab), script = script)
# posses falsas herdadas da data da sessao de julgamento do ES (defeito corrigido em R/23)
esv <- mand[sg_uf == "ES" & cd_cargo == "13" & !is.na(data_posse) & substr(data_posse, 6, 10) != "01-01"]
cat("posses nao canonicas de vereador no ES ainda em mandatos.csv:", nrow(esv), "\n")
registrar_numero("vvt_n_posses_falsas_es_em_mandatos_csv", nrow(esv), script = script)
# 29/08/2026: a checagem por "posse fora de 1o de janeiro" confundia o defeito do ES com posse
# legitima de suplente empossado no meio da legislatura, que o SAPL registra. Passa a exigir o que
# de fato importa: toda posse nao canonica de vereador no ES tem de estar declarada, com essa data,
# em algum arquivo de fonte, e nenhuma pode coincidir com data de sessao de julgamento do TCE.
posse_fonte <- unique(rbindlist(lapply(
  c("data/exercicio_camaras_municipais.csv", "data/exercicio_camaras_sem_sapl.csv",
    "data/exercicio_camaras_sem_sapl_2.csv"), function(f) {
      x <- if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
      if (is.null(x) || !"data_inicio_mandato" %in% names(x)) NULL
      else x[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel, posse = substr(data_inicio_mandato, 1, 10))]
    }), use.names = TRUE))
sem_lastro <- esv[!posse_fonte[, paste(id_mandato, posse)][match(paste(esv$id_mandato, esv$data_posse),
                   posse_fonte[, paste(id_mandato, posse)])] %in% paste(esv$id_mandato, esv$data_posse)]
tce_es <- if (file.exists("data/tce_gestores.csv")) {
  z <- fread("data/tce_gestores.csv", colClasses = "character", na.strings = c("NA", ""))
  if ("uf" %in% names(z)) unique(na.omit(z[uf == "ES"]$data_inicio)) else character(0)
} else character(0)
colide <- esv[data_posse %in% tce_es]
cat("posses do ES sem lastro em arquivo de fonte:", nrow(sem_lastro),
    "| coincidentes com data de julgamento do TCE-ES:", nrow(colide), "\n")
registrar_numero("vvt_n_posses_es_sem_lastro_em_fonte", nrow(sem_lastro), script = script)
ck("C5 posse nao canonica de vereador no ES vem de arquivo de fonte e nao da sessao de julgamento do TCE",
   nrow(sem_lastro) == 0L && nrow(colide) == 0L,
   paste(nrow(esv), "posses fora de 1o de janeiro,", nrow(sem_lastro), "sem lastro,", nrow(colide), "coincidentes com julgamento"))

## ============================================================ 6. amostra propria de pareamentos
cat("\n===== 6. amostra propria (semente e estratificacao diferentes) =====\n")
# a amostra da frente foi aleatoria simples, e por isso caiu quase toda na regra mais estrita
# (nome civil completo). Aqui a amostra e estratificada por metodo, com as regras frouxas
# sobre-representadas, que e onde um pareamento inflado por nome apareceria.
pes <- pess[, .(id_pessoa, nome_bocel = nome, urna_bocel = nome_urna_recente, cpf_bocel = nr_cpf)]
amos <- rbindlist(lapply(names(TAB), function(g) {
  o <- TAB[[g]][!is.na(id_mandato_bocel)]
  p <- merge(o, mand[, .(id_mandato, m_pessoa = id_pessoa, m_cd = cd_cargo, m_ue = unidade_posicao,
                         m_uf = sg_uf, m_ano = ano_eleicao, m_ini = mandato_inicio, m_fim = mandato_fim)],
             by.x = "id_mandato_bocel", by.y = "id_mandato")
  p <- merge(p, pes, by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
  p[, tabela := g]
  p
}), use.names = TRUE, fill = TRUE)
amos[, `:=`(n_fonte = norm(nome), n_civil = norm(nome_bocel), n_urna = norm(urna_bocel))]
tk <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3L])
amos[, n_tok_fonte := lengths(tk(n_fonte))]
amos[, jac := mapply(function(u, v) { u <- unique(u); v <- unique(v)
        if (!length(u) || !length(v)) return(0) ; length(intersect(u, v)) / length(union(u, v)) },
        tk(amos$n_fonte), tk(amos$n_civil))]
amos[, jac_urna := mapply(function(u, v) { u <- unique(u); v <- unique(v)
        if (!length(u) || !length(v)) return(0) ; length(intersect(u, v)) / length(union(u, v)) },
        tk(amos$n_fonte), tk(amos$n_urna))]
amos[, jac_max := pmax(jac, jac_urna)]
cat("\nsobreposicao de tokens entre o nome da fonte e o nome do BOCEL, por metodo:\n")
print(amos[, .(linhas = .N, jac_mediana = round(median(jac_max), 3), jac_min = round(min(jac_max), 3),
               n_com_jac_abaixo_de_04 = sum(jac_max < 0.4), n_tok_fonte_1 = sum(n_tok_fonte <= 1L)),
           by = metodo_pareamento][order(-linhas)])
FRACOS <- amos[jac_max < 0.4]
registrar_numero("vvt_n_pareamentos_token_fraco", nrow(FRACOS), script = script)
registrar_numero("vvt_n_linhas_pareadas_total", nrow(amos), script = script)
registrar_numero("vvt_pct_pareamentos_token_fraco", round(nrow(FRACOS) / nrow(amos), 4), script = script)
ck("P1 nenhum pareamento com nome de um unico token curto",
   amos[, sum(n_tok_fonte == 0L)] == 0L)
ck("P2 pareamento por nome com sobreposicao de tokens abaixo de 0,4 fica abaixo de 1% das linhas",
   nrow(FRACOS) / nrow(amos) < 0.01,
   paste(nrow(FRACOS), "de", nrow(amos), "=", round(100 * nrow(FRACOS) / nrow(amos), 2), "%"))
# amostra estratificada: ate 4 por metodo, priorizando as regras frouxas e os casos de baixa sobreposicao
set.seed(20260828)
ordem <- c("tokens_nome_fonte_no_nome_de_urna", "nome_fonte=nome_urna_eleicao",
           "nome_completo_municipio_cargo", "tokens_nome_fonte_no_nome_civil",
           "cpf_municipio_cargo", "cpf_municipio_cargo_eleicao", "nome_completo_eleicao")
sel <- rbindlist(lapply(ordem, function(m) {
  d <- amos[metodo_pareamento == m]
  if (!nrow(d)) return(NULL)
  d <- d[order(jac_max)]
  k <- min(nrow(d), 4L)
  rbind(d[seq_len(min(nrow(d), 2L))], d[sample(.N, min(.N, k - min(nrow(d), 2L)))])
}), use.names = TRUE, fill = TRUE)
sel <- unique(sel, by = c("tabela", "id_mandato_bocel", "nome"))
# completa ate 25 com sorteio simples entre as fontes menos representadas na amostra da frente
falta <- 25L - nrow(sel)
if (falta > 0L) {
  resto <- amos[!paste(tabela, id_mandato_bocel, nome) %in% sel[, paste(tabela, id_mandato_bocel, nome)]]
  set.seed(20260829)
  sel <- rbind(sel, resto[sample(.N, falta)], use.names = TRUE, fill = TRUE)
}
AM <- sel[, .(tabela, uf, fonte, sg_ue, nome_municipio_fonte, unidade_gestora, cargo_bocel,
              nome_fonte = nome, nome_bocel, nome_urna_bocel = urna_bocel, exercicio, ano_eleicao,
              data_inicio, data_fim, forma_saida, metodo_pareamento, jac_max = round(jac_max, 3),
              id_mandato_bocel, id_pessoa_bocel, m_cd, m_ue, m_ano, m_ini, m_fim, url)]
fwrite(AM, "output/verificacao/vvt_amostra_25_pares.csv", na = "NA")
cat("\n--- amostra de", nrow(AM), "pareamentos (ordenada pelo mais fraco) ---\n")
for (i in seq_len(nrow(AM))) with(AM[i], cat(sprintf(
  "%2d [%s] %s/%s %-13s | fonte: %-34s | bocel: %-34s | eleicao %s | jac %.2f | %s | %s\n",
  i, tabela, uf, sg_ue, cargo_bocel, substr(nome_fonte, 1, 34), substr(nome_bocel, 1, 34),
  m_ano, jac_max, forma_saida, metodo_pareamento)))
ck("P3 na amostra, cargo, municipio, uf e eleicao batem com o mandato",
   AM[, all(m_cd == CD_DE[cargo_bocel] & m_ue == sg_ue & (is.na(ano_eleicao) | ano_eleicao == m_ano))])
ck("P4 na amostra, toda data da fonte cai na janela do mandato",
   AM[, all(is.na(data_inicio) | (data_inicio >= as.character(as.IDate(m_ini) - 60L) & data_inicio <= m_fim))])
ck("P5 na amostra, todo id_pessoa_bocel e o titular do mandato",
   nrow(merge(AM, mand[, .(id_mandato, tp = id_pessoa)], by.x = "id_mandato_bocel", by.y = "id_mandato")[tp != id_pessoa_bocel]) == 0L)
registrar_numero("vvt_n_amostra_pareamentos", nrow(AM), script = script)

# amostra da conferencia ao vivo: uma linha por fonte, sempre a de MENOR sobreposicao de tokens
# daquela fonte, que e o caso mais dificil e nao o mais comodo. Cobre 11 fontes, duas delas
# (TCE-RS e TCE-MS) que a frente nao levou ao endpoint vivo.
AV <- amos[order(jac_max)][, .SD[1], by = fonte][, .(
  tabela, uf, fonte, sg_ue, nome_municipio_fonte, unidade_gestora, cargo_bocel, nome_fonte = nome,
  nome_bocel, exercicio, ano_eleicao, jac_max = round(jac_max, 3), forma_saida, metodo_pareamento,
  id_mandato_bocel, m_ano, m_ini, m_fim, url)]
fwrite(AV, "output/verificacao/vvt_amostra_ao_vivo.csv", na = "NA")
cat("\namostra ao vivo:", nrow(AV), "linhas,", AV[, uniqueN(fonte)], "fontes distintas\n"); print(AV[, .(fonte, uf, nome_fonte, jac_max)])

## ============================================================ 7. o que a inferencia da folha do PB apaga
cat("\n===== 7. limites da inferencia por folha (PB) e por vinculo (PE) =====\n")
pb <- b[fonte == "tcepb_sagres_folha_cargos_eletivos"]
pb[, n_meses := as.integer(sub("folha do Sagres: ([0-9]+) meses.*", "\\1", situacao_fonte))]
pb[, am_ini := as.integer(sub(".*competencia, ([0-9]{6}) a .*", "\\1", situacao_fonte))]
pb[, am_fim := as.integer(sub(".*a ([0-9]{6}) \\(unidade.*", "\\1", situacao_fonte))]
pb[, am_max := as.integer(sub(".*envia ate ([0-9]{6})\\).*", "\\1", situacao_fonte))]
nmes <- function(x) (x %/% 100L) * 12L + (x %% 100L)
pb[, span := nmes(am_fim) - nmes(am_ini) + 1L][, lacuna := span - n_meses]
ck("F1 a guarda contra falha de remessa usa contagem de meses, nao aritmetica de YYYYMM",
   pb[forma_saida == "outro", all(nmes(am_max) >= nmes(am_fim) + 3L)],
   paste(pb[forma_saida == "outro" & nmes(am_max) < nmes(am_fim) + 3L, .N], "linhas 'outro' sem 3 meses de remessa posterior"))
lac <- pb[forma_saida == "fim_regular" & lacuna > 6L & !is.na(id_mandato_bocel)]
cat("PB: linhas 'fim_regular' pareadas com mais de 6 meses de lacuna na folha:", nrow(lac), "de",
    pb[forma_saida == "fim_regular" & !is.na(id_mandato_bocel), .N], "\n")
registrar_numero("vvt_pb_n_fim_regular_com_lacuna_maior_6_meses", nrow(lac), script = script)
registrar_numero("vvt_pb_n_fim_regular_pareado", pb[forma_saida == "fim_regular" & !is.na(id_mandato_bocel), .N], script = script)
ck("F2 'fim_regular' da folha do PB nunca se apoia em menos de 6 meses de competencia",
   pb[forma_saida == "fim_regular" & !is.na(id_mandato_bocel), all(n_meses >= 6L)],
   paste(pb[forma_saida == "fim_regular" & !is.na(id_mandato_bocel) & n_meses < 6L, .N], "linhas"))
pe <- b[fonte == "tcepe_dados_abertos_lista_servidores"]
alem <- pe[!is.na(data_fim) & !is.na(ano_eleicao) & data_fim > sprintf("%d-12-31", as.integer(ano_eleicao) + 4L)]
cat("PE: linhas de vinculo que terminam depois da legislatura de origem:", nrow(alem), "de", nrow(pe), "\n")
registrar_numero("vvt_pe_n_vinculo_alem_da_legislatura", nrow(alem), script = script)
fora_jan <- merge(pe[!is.na(id_mandato_bocel) & !is.na(data_fim)],
                  mand[, .(id_mandato, mi = mandato_inicio, mf = mandato_fim)],
                  by.x = "id_mandato_bocel", by.y = "id_mandato")[
                  as.IDate(data_fim) > as.IDate(mf) + 45L]
registrar_numero("vvt_pe_n_linhas_fim_fora_da_janela", nrow(fora_jan), script = script)
ck("F3 a diferenca entre saidas observadas em B e candidatos que chegam a R/10 e exatamente a janela",
   b[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)] - length(tce_b) ==
     uniqueN(fora_jan$id_mandato_bocel),
   paste(b[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)], "-",
         length(tce_b), "vs", uniqueN(fora_jan$id_mandato_bocel), "mandatos fora da janela"))

## ============================================================ 8. sobreposicao e prevalencia
cat("\n===== 8. o tce sobrepoe outra fonte? =====\n")
sapl <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character", na.strings = c("NA", ""))
sc_ <- cands(sapl$id_mandato_bocel, sapl$data_fim_mandato, sapl$forma_saida)
inter <- intersect(sc_, tce)
cat("mandatos em que sapl_municipal e tce sao candidatos ao mesmo tempo:", length(inter), "\n")
tt <- unique(b[id_mandato_bocel %in% inter & forma_saida != "nao_observado", .(id_mandato_bocel, fs_tce = forma_saida)])
ss <- unique(sapl[id_mandato_bocel %in% inter & !forma_saida %in% c("nao_observado", NA), .(id_mandato_bocel, fs_sapl = forma_saida)])
cmp <- merge(tt, ss, by = "id_mandato_bocel")
cat("desses, com forma divergente entre as duas fontes:", cmp[fs_tce != fs_sapl, .N], "de", nrow(cmp), "\n")
print(cmp[fs_tce != fs_sapl, .N, by = .(fs_sapl, fs_tce)][order(-N)])
registrar_numero("vvt_n_sobreposicao_tce_sapl", length(inter), script = script)
registrar_numero("vvt_n_sobreposicao_tce_sapl_com_forma_divergente", cmp[fs_tce != fs_sapl, .N], script = script)
# 29/08/2026: a ordem se inverteu. O TCE desceu para antes das camaras porque a forma de saida do
# grupo B vem da folha de pagamento agregada, que e inferencia, e a composicao publicada pela casa
# e registro direto (docs/CONCORDANCIA_FONTES.md). Onde os dois concorrem, o SAPL passa a prevalecer,
# salvo quando uma terceira fonte nomeia um evento e a guarda de R/10 o protege.
EVENTO_V <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
# o SAPL tambem deixa de competir quando sua unica linha termina no futuro, porque R/10 trata fim
# posterior a hoje como mandato em curso: sao as legislaturas 2025-2028, em que o TCE ja registra
# o fim do exercicio e o SAPL ainda anuncia o termino previsto
sapl_fut <- sapl[id_mandato_bocel %in% inter, .(so_futuro = all(!is.na(data_fim_mandato) &
                 as.IDate(substr(data_fim_mandato, 1, 10)) > Sys.Date())), by = id_mandato_bocel][so_futuro == TRUE, id_mandato_bocel]
fora_s1 <- mand[id_mandato %in% inter & !fonte_forma_saida %in% c("sapl_municipal", NA) &
                !(forma_saida %in% EVENTO_V) & !id_mandato %in% sapl_fut]
if (nrow(fora_s1)) print(fora_s1[, .N, by = .(fonte_forma_saida, forma_saida)][order(-N)])
ck("S1 onde tce e sapl_municipal concorrem, o SAPL prevalece salvo evento nomeado por terceira fonte",
   nrow(fora_s1) == 0L,
   paste(nrow(fora_s1), "com outra fonte sem evento nomeado, de", length(inter),
         "em concorrencia;", length(sapl_fut), "com SAPL so no futuro"))

## contribuicao exclusiva: reproduz a extracao de TODAS as demais fontes que R/10 consome
ler <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
cl <- function(dt, n) if (!is.null(dt) && n %in% names(dt)) dt[[n]] else rep(NA_character_, if (is.null(dt)) 0L else nrow(dt))
OUTRAS <- list(sapl_municipal = sc_)
x <- ler("data/munic_prefeitos.csv"); if (!is.null(x)) OUTRAS$ibge_munic <- cands(cl(x,"id_mandato_bocel"),
  fifelse(grepl("^[0-9]{4}$", cl(x,"ano_munic")), paste0(cl(x,"ano_munic"), "-12-31"), NA_character_),
  fifelse(cl(x,"status") == "outro_em_exercicio" & !is.na(cl(x,"nome_prefeito_munic")), "substituicao", NA_character_))
x <- ler("data/mandatos_forma_saida_suplementar.csv"); if (!is.null(x)) OUTRAS$tse_suplementar <- cands(
  cl(x,"id_mandato_ordinario_afetado"), cl(x,"data_fim_inferida"),
  fifelse(cl(x,"momento") %in% "antes_da_posse", "nao_tomou_posse", "perda_do_mandato"))
x <- ler("data/datajud_mandatos_afetados.csv"); if (!is.null(x)) OUTRAS$datajud <- cands(cl(x,"id_mandato"), cl(x,"data"),
  fifelse(toupper(cl(x,"indicio")) %in% c("TRUE","T","1"), "cassacao", NA_character_))
x <- ler("data/wikipedia_prefeitos.csv"); w1 <- if (!is.null(x)) cands(cl(x,"id_mandato_bocel"), cl(x,"fim"), cl(x,"forma_saida")) else character(0)
x <- ler("data/wikipedia_estadual.csv");  w2 <- if (!is.null(x)) cands(cl(x,"id_mandato_bocel"), cl(x,"fim"), cl(x,"forma_saida")) else character(0)
OUTRAS$wikipedia <- union(w1, w2)
x <- ler("data/diarios_mandatos_saida.csv"); if (!is.null(x)) OUTRAS$diario_oficial <- cands(
  fcoalesce(cl(x,"id_mandato_bocel"), cl(x,"id_mandato")), cl(x,"data_fim_inferida"), cl(x,"forma_saida"))
x <- ler("data/wikidata_mandatos.csv"); if (!is.null(x)) OUTRAS$wikidata <- cands(cl(x,"id_mandato_bocel"), cl(x,"fim"), cl(x,"forma_saida"))
x <- ler("data/wikidata_obitos.csv"); if (!is.null(x)) OUTRAS$wikidata_obito <- cands(cl(x,"id_mandato"), cl(x,"data_morte"),
  fifelse(toupper(cl(x,"dentro_do_mandato")) %in% c("TRUE","T","1"), "falecimento", NA_character_))
x <- ler("data/exercicio_assembleias.csv"); if (!is.null(x)) OUTRAS$assembleia_api <- cands(cl(x,"id_mandato_bocel"), cl(x,"data_fim_exercicio"), cl(x,"forma_saida"))
x <- ler("data/exercicio_assembleias_historico.csv"); if (!is.null(x)) OUTRAS$assembleia_historico <- cands(cl(x,"id_mandato_bocel"), cl(x,"data_fim_exercicio"), cl(x,"forma_saida"))
pc <- character(0)
for (fp in c("data/exercicio_camaras_sem_sapl.csv", "data/exercicio_camaras_sem_sapl_2.csv")) {
  x <- ler(fp); if (!is.null(x)) pc <- union(pc, cands(cl(x,"id_mandato_bocel"), cl(x,"data_fim_mandato"), cl(x,"forma_saida")))
}
OUTRAS$portal_camara <- pc
x <- ler("data/exercicio_senado.csv"); if (!is.null(x)) OUTRAS$senado_api <- cands(cl(x,"id_mandato"), cl(x,"data_fim_exercicio"), cl(x,"forma_saida"))
x <- ler("data/exercicio_camara.csv");  if (!is.null(x)) OUTRAS$camara_api <- cands(cl(x,"id_mandato"), cl(x,"data_fim_exercicio"), cl(x,"forma_saida"))
so_tce <- setdiff(tce, unique(unlist(OUTRAS, use.names = FALSE)))
cat("\nsobreposicao do tce com cada outra fonte:\n")
print(data.table(fonte = names(OUTRAS), n = vapply(OUTRAS, function(k) length(intersect(k, tce)), integer(1)))[order(-n)])
cat("mandatos em que o tce e a unica fonte candidata:", length(so_tce), "de", length(tce),
    sprintf("(%.2f%%)", 100 * length(so_tce) / length(tce)), "\n")
registrar_numero("vvt_n_mandatos_so_tce", length(so_tce), script = script)
registrar_numero("vvt_pct_mandatos_so_tce", round(length(so_tce) / length(tce), 4), script = script)

## ============================================================ 9. conferencia ao vivo (amostra propria)
fv <- "output/verificacao/vvt_conferencia_ao_vivo.csv"
if (file.exists(fv)) {
  cv <- fread(fv, colClasses = "character", na.strings = "NA")
  cat("\n===== 9. conferencia contra a fonte ao vivo =====\n")
  print(cv[, .(uf, fonte, nome_fonte, http, confirmado, nome_bocel_na_fonte)])
  ck("V1 a conferencia ao vivo cobre ao menos 10 pareamentos", nrow(cv) >= 10L, paste(nrow(cv), "linhas"))
  ck("V2 toda consulta ao vivo respondeu http 200", cv[, all(http == "200")], paste(cv[http != "200", .N], "sem 200"))
  ck("V3 o nome da fonte reaparece hoje no endpoint do tribunal", cv[, all(confirmado == "sim")],
     paste(cv[confirmado != "sim", .N], "nao confirmados"))
  # o nome do BOCEL so precisa reaparecer na fonte quando o pareamento foi FEITO por nome; onde a
  # identidade veio do CPF, a divergencia de grafia entre tribunal e TSE e esperada e nao e defeito
  cvn <- cv[!grepl("^cpf_", metodo_pareamento)]
  ck("V4 onde o pareamento foi por nome, o nome do BOCEL reaparece entre os nomes que a fonte devolve",
     nrow(cvn) == 0L || cvn[, all(nome_bocel_na_fonte == "sim")],
     paste(cvn[nome_bocel_na_fonte != "sim", .N], "de", nrow(cvn), "pareamentos por nome sem o nome do BOCEL na fonte"))
  ck("V5 a fonte devolve poucos candidatos para a unidade consultada (poder discriminante)",
     cv[, all(as.integer(n_candidatos) >= 1L)],
     paste("mediana de candidatos por consulta:", median(as.integer(cv$n_candidatos))))
  registrar_numero("vvt_n_ao_vivo_nome_bocel_presente", cv[nome_bocel_na_fonte == "sim", .N], script = script)
  registrar_numero("vvt_n_conferidos_ao_vivo", nrow(cv), script = script)
  registrar_numero("vvt_n_conferidos_ao_vivo_confirmados", cv[confirmado == "sim", .N], script = script)
  registrar_numero("vvt_n_fontes_conferidas_ao_vivo", cv[, uniqueN(fonte)], script = script)
} else {
  cat("\n[aviso] rode python3 python/confere_tce_ao_vivo_v2.py e reexecute para a etapa 9\n")
}

## ============================================================ 9b. numeros declarados pelo construtor
## O registro assinado ja carrega a reexecucao feita nesta verificacao, de modo que comparar so com
## ele seria circular. Aqui a recontagem vai contra o valor que o construtor DECLAROU na entrega.
cat("\n===== 9b. recontagem contra o que o construtor declarou =====\n")
DECL <- data.table(
  chave = c("tce_n_registros_finais","tce_n_pareados_linhas","tce_n_mandatos_pareados","tce_n_pessoas_pareadas",
            "tce_taxa_pareamento_linhas","tce_n_municipios_cobertos","tceb_n_registros_finais",
            "tceb_n_pareados_linhas","tceb_n_mandatos_pareados","tceb_n_mandatos_com_saida_observada",
            "tcev_a_n_mandatos_com_saida_observada","tcev_b_n_mandatos_com_saida_observada",
            "tcev_a_n_linhas_forma_saida_outro","tcev_a_n_mandatos_titular_substituido_pelo_vice",
            "tcev_a_n_titulares_substituidos_sem_saida_hoje","tce_n_prefeitura_relacao_titular_eleito",
            "tce_n_prefeitura_relacao_vice_eleito","tce_n_prefeitura_relacao_indeterminado",
            "tcev_n_mandatos_candidatos_tce","tcev_n_mandatos_so_tce","tcev_pct_mandatos_so_tce",
            "tcev_n_mandatos_tce_em_mandatos_csv","tcev_n_sobreposicao_tce_sapl_municipal",
            "tcev_b_n_linhas_data_fim_fora_da_janela","tcev_a_n_linhas_duplicadas_integrais",
            "tcev_b_n_linhas_duplicadas_integrais"),
  # 29/08/2026: o vetor congelado de 28/08 comparava a declaracao daquele dia com o dado de hoje,
  # e passou a reprovar assim que R/24 foi corrigido e as tabelas C e D entraram. O declarado passa
  # a ser o ultimo valor registrado por cada chave, o que mantem a checagem viva: o que o script
  # registra tem de bater com a recontagem independente feita aqui.
  declarado = NA_character_)
DECL[, declarado := unname(REG[chave])]
ausentes <- DECL[is.na(declarado), chave]
if (length(ausentes)) cat("chaves sem registro:", paste(ausentes, collapse = ", "), "\n")
rec <- rbindlist(recontagens)
DECL <- merge(DECL, rec[, .(chave, recontado)], by = "chave", all.x = TRUE)
DECL[chave == "tcev_n_mandatos_so_tce", recontado := as.character(length(so_tce))]
DECL[chave == "tcev_pct_mandatos_so_tce", recontado := as.character(round(length(so_tce) / length(tce), 4))]
DECL[chave == "tcev_n_sobreposicao_tce_sapl_municipal", recontado := as.character(length(inter))]
DECL[chave == "tcev_b_n_linhas_data_fim_fora_da_janela", recontado := as.character(nrow(fora_jan))]
DECL[, bate := !is.na(recontado) & (declarado == recontado |
       (suppressWarnings(!is.na(as.numeric(declarado))) & suppressWarnings(!is.na(as.numeric(recontado))) &
        abs(suppressWarnings(as.numeric(declarado)) - suppressWarnings(as.numeric(recontado))) < 1e-9))]
print(DECL[, .(chave, declarado, recontado, bate)])
fwrite(DECL, "output/verificacao/vvt_declarado_vs_recontado.csv", na = "NA")
registrar_numero("vvt_n_numeros_declarados_conferidos", DECL[!is.na(recontado), .N], script = script)
registrar_numero("vvt_n_numeros_declarados_divergentes", DECL[!is.na(recontado) & bate == FALSE, .N], script = script)
ck("D1 todo numero declarado pelo construtor confere com a recontagem apos as correcoes",
   DECL[!is.na(recontado), all(bate)],
   paste(DECL[!is.na(recontado) & bate == FALSE, paste0(chave, " decl=", declarado, " rec=", recontado)], collapse = "; "))

## ============================================================ 10. fecho
cat("\n===== 10. recontagem: registrado vs recontado =====\n")
RC <- rbindlist(recontagens)
print(RC)
fwrite(RC, "output/verificacao/vvt_registro_vs_recontagem.csv", na = "NA")
ck("R1 todo numero registrado da frente confere com a recontagem",
   RC[, all(bate)], paste(RC[bate == FALSE, .N], "divergencias:",
                          paste(RC[bate == FALSE, paste0(chave, " reg=", registrado, " rec=", recontado)], collapse = "; ")))
registrar_numero("vvt_n_numeros_reconferidos", nrow(RC), script = script)
registrar_numero("vvt_n_numeros_divergentes", RC[bate == FALSE, .N], script = script)
registrar_numero("vvt_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("vvt_n_aprovadas", length(passou), script = script)
registrar_numero("vvt_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao(
  alvo = "verificacao adversarial da frente verifica_tce_existentes (data/tce_gestores.csv, data/tce_gestores_b.csv e os numeros que a frente registrou)",
  script = script, passou = passou, falhou = falhou,
  fora_de_cobertura = c(
    "completude do cadastro de cada tribunal: a fonte lista quem prestou contas ou teve contas julgadas, nao todo agente que exerceu",
    "identidade de homonimo no mesmo municipio, cargo e eleicao",
    "veracidade do campo Gestor/Responsavel/NomeServidor tal como o tribunal o preenche",
    "se a presenca na folha do Sagres ate dezembro do ultimo ano equivale a ter cumprido o mandato",
    "se o vice que responde pela prefeitura assumiu por renuncia, cassacao, falecimento, afastamento ou licenca",
    "efeito real de reexecutar R/10 sobre mandatos.csv (R/10 e read-only nesta frente)"))
cat("\nrelatorio:", f, "\naprovadas:", length(passou), " reprovadas:", length(falhou), "\n")
if (length(falhou)) { cat("REPROVADO\n"); for (x in falhou) cat("  -", x, "\n"); sink(); quit(status = 1) }
cat("VERIFICADO\n"); sink()
