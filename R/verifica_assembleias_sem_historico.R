# verifica_assembleias_sem_historico.R — verificacao cetica da frente 'assembleias sem historico'
# (saida de R/22_exercicio_assembleias_historico.R). Reexecucao independente: asserts de rigor,
# recontagem dos numeros registrados, deteccao de contaminacao de fonte, amostra de pareamentos.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_assembleias_sem_historico.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_assembleias_sem_historico.R"
dir.create("logs", showWarnings = FALSE)
logf <- file("logs/verifica_assembleias_sem_historico.log", open = "wt"); sink(logf, split = TRUE)
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
METODOS <- c("nome_completo_civil", "nome_urna", "nome_parlamentar_civil", "data_nascimento_unica", "tokens_nome_completo_no_civil", "tokens_no_nome_civil", "tokens_no_nome_de_urna")
passou <- character(); falhou <- character()
chk <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else { falhou <<- c(falhou, msg); cat("FALHOU:", msg, "\n") } }

out <- fread("data/exercicio_assembleias_historico.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/exercicio_assembleias_historico_cobertura.csv", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("7", "8")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
cat("linhas:", nrow(out), "| pareadas:", sum(!is.na(out$id_mandato_bocel)), "\n")

## ---- 1. arquivo: colunas, codigo de ausente, minusculas sem acento
chk(identical(names(out), names(fread("data/exercicio_assembleias.csv", nrows = 1))), "colunas identicas a exercicio_assembleias.csv")
chk(all(names(out) == tolower(names(out))) && !any(grepl("[^a-z0-9_]", names(out))), "nomes de coluna minusculos sem acento")
raw <- readLines("data/exercicio_assembleias_historico.csv", warn = FALSE)
chk(!any(grepl(',"",|,"",|,,', raw)), "ausente codificado como NA (sem campo vazio)")

## ---- 2. asserts de rigor
par <- out[!is.na(id_mandato_bocel)]
checa_unica(as.data.frame(par), "id_mandato_bocel"); passou <- c(passou, "checa_unica id_mandato_bocel")
checa_unica(as.data.frame(out), c("uf", "legislatura", "nome_normalizado", "nome_completo", "condicao", "id_fonte")); passou <- c(passou, "checa_unica linha da fonte")
in_set(out$forma_saida, VOCAB, permitir_na = TRUE); passou <- c(passou, "in_set forma_saida")
in_set(out$condicao, c("titular", "suplente"), permitir_na = FALSE); passou <- c(passou, "in_set condicao")
in_set(out$uf, c("AP", "MG", "MT", "SC", "SE"), permitir_na = FALSE); passou <- c(passou, "in_set uf")
in_set(par$metodo_pareamento, METODOS, permitir_na = FALSE); passou <- c(passou, "in_set metodo_pareamento")
em_faixa(as.integer(out$ano_eleicao), 1998, 2022, permitir_na = FALSE); passou <- c(passou, "em_faixa ano_eleicao")
chk(all(as.integer(out$ano_eleicao) %% 4 == 2), "ano_eleicao e ano de eleicao estadual")
em_faixa(as.integer(substr(out$data_nascimento, 1, 4)), 1900, 2004); passou <- c(passou, "em_faixa ano de nascimento")
em_faixa(cob$taxa, 0, 1); passou <- c(passou, "em_faixa taxa")
chk(all(is.na(out$data_inicio_exercicio)) && all(is.na(out$data_fim_exercicio)), "sem datas (as fontes nao as trazem): nada inventado")
# legislatura -> ano: conferido nas paginas (MT 2a=1951, SE 14a=01/02/1999, AP V=2007/2011, MG 19a=2019-2023)
leg <- as.integer(out$legislatura)
chk(all(as.integer(out$ano_eleicao) == fifelse(out$uf == "AP", 4L * leg + 1986L, 4L * leg + 1942L)), "legislatura -> ano da eleicao coerente")
# join um-para-um com mandatos.csv
j <- join_seguro(as.data.frame(par), as.data.frame(mand[, .(id_mandato_bocel = id_mandato, id_pessoa_m = id_pessoa, sg_uf, ano_m = ano_eleicao, fs_bocel = forma_saida, fonte_fs = fonte_forma_saida, cargo_m = cargo)]),
                 by = "id_mandato_bocel", cardinalidade = "one-to-one", tipo = "left", unmatched = "error")
setDT(j); passou <- c(passou, "join_seguro one-to-one com mandatos.csv")
chk(all(j$id_pessoa_bocel == j$id_pessoa_m), "id_pessoa_bocel = id_pessoa do mandato")
chk(all(j$uf == j$sg_uf) && all(j$ano_eleicao == j$ano_m), "uf e ano do mandato pareado coincidem")
chk(all(j$cargo_m == "DEPUTADO ESTADUAL"), "todos os mandatos pareados sao de deputado estadual")
j <- merge(j, pess[, .(id_pessoa_bocel = id_pessoa, nome_bocel = nome, urna_bocel = nome_urna_recente, dt_nasc_bocel = dt_nascimento)], by = "id_pessoa_bocel", all.x = TRUE)
chk(all(is.na(j$data_nascimento) | is.na(j$dt_nasc_bocel) | j$data_nascimento == j$dt_nasc_bocel), "nascimento da fonte = nascimento do BOCEL quando ambos existem")
chk(nrow(j[condicao == "suplente" & grepl("^tokens", metodo_pareamento)]) == 0, "suplente nao pareado por tokens")
chk(nrow(j[condicao == "suplente" & !is.na(forma_saida)]) == 0, "suplente sem forma_saida")
chk(nrow(j[uf == "MG" & ano_eleicao == "2022" & forma_saida == "fim_regular"]) == 0 && nrow(j[uf == "SE" & ano_eleicao == "2022" & forma_saida == "fim_regular"]) == 0, "legislatura em curso sem fim_regular")
chk(all(j[uf %in% c("AP", "MT", "SC"), forma_saida] == "nao_observado" | is.na(j[uf %in% c("AP", "MT", "SC"), forma_saida])), "AP/MT/SC sem forma de saida inventada")

## ---- 3. contaminacao SC: memoriapolitica tem duas series de numeracao (ids 95-109 = pre-1947)
sc_antigo <- list.files("data_raw/assembleias_sem_historico/SC", pattern = "^legislatura_1[34]_10[89]\\.html$")
cat("SC: arquivos da serie antiga com numero 13/14:", paste(sc_antigo, collapse = ", "), "\n")
sc98 <- out[uf == "SC" & ano_eleicao == "1998"]
cat("SC 1998: linhas =", nrow(sc98), "(40 eleitos esperados + suplentes); pareadas =", sum(!is.na(sc98$id_mandato_bocel)), "\n")
nomes_109 <- {
  f <- "data_raw/assembleias_sem_historico/SC/legislatura_14_109.html"
  if (file.exists(f)) { tx <- paste(readLines(f, warn = FALSE), collapse = " "); tx <- if (validUTF8(tx)) tx else iconv(tx, "latin1", "UTF-8"); tx <- gsub("<[^>]+>", " ", tx)
    i <- regexpr("Foram eleitos", tx); s <- substr(tx, i, i + 3000); s <- sub("^[^:]*:\\s*", "", s); s <- sub("\\..*$", "", s)
    trimws(unlist(strsplit(s, ";"))) } else character() }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); gsub(" +", " ", trimws(gsub("[^A-Z ]", " ", x))) }
cont <- sc98[nome_normalizado %in% norm(nomes_109)]
cat("SC 1998: linhas vindas da 14a Legislatura de 1930 (id 109):", nrow(cont), "| pareadas ao BOCEL:", sum(!is.na(cont$id_mandato_bocel)), "\n")
if (nrow(cont[!is.na(id_mandato_bocel)])) print(cont[!is.na(id_mandato_bocel), .(nome, id_mandato_bocel, metodo_pareamento)])
chk(nrow(cont) == 0, "SC 1998 sem nomes da serie antiga (1930) da Memoria Politica")
chk(sum(!is.na(cont$id_mandato_bocel)) == 0, "nenhum nome de 1930 pareado a mandato de 1998")

## ---- 4. recontagem dos numeros registrados
rec <- list(
  ash_n_linhas_fonte = nrow(out),
  ash_n_mandatos_pareados = uniqueN(par$id_mandato_bocel),
  ash_n_mandatos_pareados_com_saida = uniqueN(par[forma_saida != "nao_observado" & !is.na(forma_saida), id_mandato_bocel]),
  ash_n_mandatos_novos_vs_wikipedia = sum(cob$n_novos_vs_wiki, na.rm = TRUE),
  ash_n_minutas_lai = length(list.files("docs/LAI_ASSEMBLEIAS", pattern = "^[A-Z]{2}_pedido_LAI\\.md$")),
  ash_n_casas = nrow(fread("data_raw/assembleias_sem_historico/inventario_fontes.csv")),
  ash_n_com_fonte_estruturada = sum(fread("data_raw/assembleias_sem_historico/inventario_fontes.csv")$viavel),
  ash_n_com_fonte_nova_coletada = uniqueN(out$uf))
for (u in c("ap", "mg", "mt", "sc", "se")) {
  rec[[paste0("ash_", u, "_n_mandatos_pareados")]] <- uniqueN(par[uf == toupper(u), id_mandato_bocel])
  rec[[paste0("ash_", u, "_taxa_pareamento")]] <- round(uniqueN(par[uf == toupper(u) & condicao == "titular", id_mandato_bocel]) / nrow(mand[sg_uf == toupper(u)]), 4)
}
ass <- grep("^ash_", readLines("output/numeros_assinatura.txt", warn = FALSE), value = TRUE)
ass <- data.table(V1 = trimws(sub("\\|.*$", "", ass)), V2 = trimws(sapply(strsplit(ass, "\\|", fixed = FALSE), `[`, 2)))
ult <- ass[, .SD[.N], by = V1]
comp <- data.table(chave = names(rec), recontado = as.character(unlist(rec)))
comp[, registrado := ult$V2[match(chave, ult$V1)]]
comp[, bate := as.numeric(recontado) == as.numeric(registrado)]
print(comp)
chk(all(comp$bate), "todos os numeros registrados batem com a recontagem")
# taxa por UF: o construtor usa n_bocel de cob (cargo 7/8 na UF); confere contra mandatos.csv
chk(all(cob[, .(n = sum(n_bocel)), by = uf][, n] == mand[, .N, by = sg_uf][match(cob[, unique(uf)], sg_uf), N]), "n_bocel da cobertura = mandatos.csv por UF")
cat("mandatos cargo 7 por UF sem forma de saida antes desta frente (fonte_forma_saida NA ou de terceiros):\n")
print(mand[sg_uf %in% unique(out$uf), .N, by = .(sg_uf, fonte_forma_saida)][order(sg_uf)])

## ---- 5. conflito com forma de saida ja existente de outra fonte (wikidata/wikipedia)
we <- fread("data/wikipedia_estadual.csv", colClasses = "character", na.strings = "NA")[!is.na(id_mandato_bocel), .(id_mandato_bocel, fs_wiki = forma_saida)]
cf <- merge(j[!is.na(forma_saida) & forma_saida != "nao_observado", .(id_mandato_bocel, uf, ano_eleicao, nome, forma_saida, causa_original)], we, by = "id_mandato_bocel")
cat("pareados com forma de saida nesta frente e tambem na wikipedia_estadual:", nrow(cf), "| discordantes:", nrow(cf[fs_wiki != forma_saida & fs_wiki != "nao_observado"]), "\n")
if (nrow(cf[fs_wiki != forma_saida & fs_wiki != "nao_observado"])) print(cf[fs_wiki != forma_saida & fs_wiki != "nao_observado"])
cat("forma_saida desta frente x forma_saida hoje em mandatos.csv (integrada por R/10):\n")
print(j[, .N, by = .(uf, forma_saida, fs_bocel, fonte_fs)][order(uf, -N)])

## ---- 6. MG: situacao x forma; suplentes que exerceram e pareados
cat("MG pareados: causa_original x forma_saida\n"); print(j[uf == "MG", .N, by = .(sub(" \\| perfil.*$", "", causa_original), forma_saida)][order(-N)])
cat("MG pareados cujo perfil menciona suplencia:\n"); print(j[uf == "MG" & grepl("supl", causa_original, ignore.case = TRUE), .(nome, ano_eleicao, condicao, causa_original, forma_saida, metodo_pareamento)])
cat("MG por legislatura: n_fonte titular vs 77 cadeiras (excedente = suplentes que exerceram sem perfil de suplencia)\n")
print(out[uf == "MG", .(n_fonte = .N, n_titular = sum(condicao == "titular"), n_pareado = sum(!is.na(id_mandato_bocel)), n_pareado_fim_regular = sum(forma_saida == "fim_regular", na.rm = TRUE)), by = ano_eleicao])
cat("MG nao pareados (deveriam ser suplentes ou grafias divergentes):\n"); print(out[uf == "MG" & is.na(id_mandato_bocel), .N, by = .(condicao, sub(" \\| perfil.*$", "", causa_original))][order(-N)])

## ---- 7. amostra de 25 pares (10 exatos, 15 por tokens) para inspecao manual + 10 para conferencia ao vivo
set.seed(20260827)
ex <- j[!grepl("^tokens", metodo_pareamento)][sample(.N, 10)]
tk <- j[grepl("^tokens", metodo_pareamento)][sample(.N, 15)]
am <- rbind(ex, tk)[, .(uf, ano_eleicao, legislatura, nome, nome_completo, data_nascimento, metodo_pareamento, id_mandato_bocel, nome_bocel, urna_bocel, dt_nasc_bocel, forma_saida, causa_original, url)]
fwrite(am, "output/verificacao/vash_amostra_25.csv", na = "NA", quote = TRUE)
print(am[, .(uf, ano_eleicao, nome, nome_completo, metodo_pareamento, nome_bocel, urna_bocel, forma_saida)], nrows = 30)
# criterio mecanico: tokens da fonte (>=3 letras) contidos no civil ou na urna do BOCEL
tok <- function(x) lapply(strsplit(norm(fcoalesce(x, "")), " "), function(t) t[nchar(t) >= 3])
am[, ok_mecanico := mapply(function(a, b, c, d) { ta <- unique(c(tok(a)[[1]], tok(b)[[1]])); tb <- unique(c(tok(c)[[1]], tok(d)[[1]])); mean(ta %in% tb) }, nome, nome_completo, nome_bocel, urna_bocel)]
cat("amostra: fracao media de tokens da fonte presentes no BOCEL =", round(mean(am$ok_mecanico), 3), "| pares com fracao < 0,5:", sum(am$ok_mecanico < 0.5), "\n")
if (any(am$ok_mecanico < 0.5)) print(am[ok_mecanico < 0.5, .(uf, ano_eleicao, nome, nome_completo, nome_bocel, urna_bocel)])
chk(all(am$ok_mecanico >= 0.5), "amostra de 25: nenhum par com menos da metade dos tokens em comum")
# pareamento por tokens com 2 tokens comuns (JOSE/MARIA/SILVA/SANTOS...): candidatos a homonimo
comuns <- c("JOSE", "MARIA", "SILVA", "SANTOS", "SOUZA", "SOUSA", "OLIVEIRA", "ANTONIO", "CARLOS", "JOAO", "PAULO", "FRANCISCO", "LUIZ", "LUIS", "PEREIRA", "COSTA", "LIMA")
tk_all <- j[grepl("^tokens", metodo_pareamento)]
tk_all[, toks := lapply(tok(fifelse(metodo_pareamento == "tokens_nome_completo_no_civil", nome_completo, nome)), unique)]
tk_all[, so_comuns := vapply(toks, function(t) all(t %in% comuns), logical(1))]
cat("pareados por tokens:", nrow(tk_all), "| so com tokens muito comuns:", sum(tk_all$so_comuns), "\n")
if (any(tk_all$so_comuns)) print(tk_all[so_comuns == TRUE, .(uf, ano_eleicao, nome, nome_completo, nome_bocel, urna_bocel)])

## ---- 8. registro
for (k in names(rec)) registrar_numero(paste0("v", k), rec[[k]], script = script)
registrar_numero("vash_sc1998_linhas_serie_1930", nrow(cont), script = script)
registrar_numero("vash_sc1998_serie_1930_pareadas", sum(!is.na(cont$id_mandato_bocel)), script = script)
registrar_numero("vash_amostra25_pares_tokens_abaixo_metade", sum(am$ok_mecanico < 0.5), script = script)
registrar_numero("vash_tokens_so_nomes_comuns", sum(tk_all$so_comuns), script = script)
rel <- gravar_relatorio_verificacao("data/exercicio_assembleias_historico.csv", script, passou = passou, falhou = falhou,
  fora_de_cobertura = c("pertinencia semantica do pareamento por tokens alem da amostra de 25",
                        "completude das listas (ALAP so com pagina ativa; ALESC prosa editada)",
                        "significado exato de 'exerceram o mandato' e 'afastados' na ALMG (situacao ao fim da legislatura, sem data)"))
cat("relatorio:", rel, "| passou:", length(passou), "| falhou:", length(falhou), "\n")
if (length(falhou)) cat("REPROVADO: ", paste(falhou, collapse = "; "), "\n")
sink()
