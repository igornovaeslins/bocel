# verifica_assembleias.R — verificacao cetica da frente ASSEMBLEIAS (R/13 -> data/exercicio_assembleias*.csv)
# e do uso dessas linhas por R/10 em data/mandatos.csv (fonte_forma_saida = assembleia_api).
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_assembleias.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_assembleias.R"
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
HOJE <- as.IDate("2026-08-28")
passou <- character(); falhou <- character()
ok <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg); cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n") }

ex  <- fread("data/exercicio_assembleias.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
lac <- fread("data/exercicio_assembleias_lacunas.csv", na.strings = "NA", encoding = "UTF-8")
inv <- fread("data_raw/assembleias/inventario_fontes_assembleias.csv", encoding = "UTF-8")
cob <- fread("output/verificacao/pareamento_assembleias_uf_legislatura.csv")
man <- fread("data/mandatos.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
pes <- fread("data/pessoas.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
wd  <- fread("data/wikidata_mandatos.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
ex[, ano_eleicao := as.integer(ano_eleicao)]
cat("linhas exercicio_assembleias:", nrow(ex), "\n")
reg("vasm_n_linhas", nrow(ex))

## ---- 1. chave unica
# chave declarada no livro de codigos: uf x id_fonte x legislatura x data_inicio_exercicio
ch <- copy(ex)[, `:=`(id_fonte = fifelse(is.na(id_fonte), nome_normalizado, id_fonte),
                     data_inicio_exercicio = fifelse(is.na(data_inicio_exercicio), "", data_inicio_exercicio))]
dup_decl <- ch[, .N, by = .(uf, id_fonte, legislatura, data_inicio_exercicio)][N > 1]
cat("duplicatas na chave declarada (id_fonte NA substituido por nome):", nrow(dup_decl), "\n"); if (nrow(dup_decl)) print(dup_decl)
reg("vasm_n_dup_chave_declarada", nrow(dup_decl))
r <- try(checa_unica(as.data.frame(ch), c("uf", "id_fonte", "legislatura", "data_inicio_exercicio")), silent = TRUE)
ok(!inherits(r, "try-error"), "chave declarada unica (uf x id_fonte x legislatura x data_inicio; id_fonte NA -> nome)")
# id_fonte NA: em que fontes?
cat("id_fonte NA por fonte:\n"); print(ex[is.na(id_fonte), .N, by = fonte])
# chave alternativa: uf x fonte x legislatura x nome_normalizado x data_inicio
dup_nome <- ch[, .N, by = .(uf, fonte, legislatura, nome_normalizado, data_inicio_exercicio)][N > 1]
cat("duplicatas uf x fonte x legislatura x nome x data_inicio:", nrow(dup_nome), "\n"); if (nrow(dup_nome)) print(head(dup_nome, 20))
reg("vasm_n_dup_uf_fonte_leg_nome_inicio", nrow(dup_nome))
# sapl: id_fonte (id do mandato) unico por UF
ok(!anyDuplicated(ex[fonte == "sapl_api", .(uf, id_fonte)]), "sapl: uf x id_fonte unico")

## ---- 2. vocabularios
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "nao_observado", "outro")
r <- try(in_set(ex$forma_saida, VOCAB, permitir_na = TRUE, nome = "forma_saida"), silent = TRUE)
ok(!inherits(r, "try-error"), "forma_saida no vocabulario fechado (NA = em curso)")
r <- try(in_set(ex$condicao, c("titular", "suplente", "nao_informado"), permitir_na = FALSE, nome = "condicao"), silent = TRUE)
ok(!inherits(r, "try-error"), "condicao em {titular, suplente, nao_informado}")
r <- try(in_set(ex$ano_eleicao, seq(1998L, 2022L, 4L), permitir_na = FALSE, nome = "ano_eleicao"), silent = TRUE)
ok(!inherits(r, "try-error"), "ano_eleicao em 1998..2022 (quadrienal)")
# NA em forma_saida so em mandato em curso (fim NA ou >= hoje) ou RJ situacao 1
na_fs <- ex[is.na(forma_saida)]
na_fs_fora <- na_fs[!(is.na(data_fim_exercicio) | as.IDate(data_fim_exercicio) >= HOJE)]
cat("forma_saida NA com data_fim passada:", nrow(na_fs_fora), "\n"); if (nrow(na_fs_fora)) print(na_fs_fora[, .(uf, legislatura, nome, data_inicio_exercicio, data_fim_exercicio)])
ok(nrow(na_fs_fora) == 0, "forma_saida NA apenas em mandato em curso")
na_fs_ano <- na_fs[, .N, by = .(ano_eleicao, fonte)]
cat("forma_saida NA por ano/fonte:\n"); print(na_fs_ano)
ok(all(na_fs$ano_eleicao == 2022L), "forma_saida NA (em curso) so na legislatura 2023-2027")

## ---- 3. datas plausiveis
d <- ex[!is.na(data_inicio_exercicio)]
d[, `:=`(ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio))]
d[, `:=`(leg_ini = as.IDate(sprintf("%d-02-01", ano_eleicao + 1L)), leg_fim = as.IDate(sprintf("%d-01-31", ano_eleicao + 5L)))]
d[, dias_ini := as.integer(ini - leg_ini)]
d[, dias_fim := as.integer(fim - leg_fim)]
cat("inicio antes da legislatura (>60d):", d[dias_ini < -60, .N], "| inicio depois do fim da legislatura:", d[ini > leg_fim, .N], "\n")
print(d[dias_ini < -60 | ini > leg_fim, .(uf, legislatura, ano_eleicao, nome, condicao, data_inicio_exercicio, data_fim_exercicio)])
cat("fim antes do inicio:", d[!is.na(fim) & fim < ini, .N], "\n"); print(d[!is.na(fim) & fim < ini, .(uf, nome, data_inicio_exercicio, data_fim_exercicio)])
cat("fim depois do fim da legislatura (>60d):", d[!is.na(fim) & dias_fim > 60, .N], "\n"); print(d[!is.na(fim) & dias_fim > 60, .(uf, legislatura, ano_eleicao, nome, condicao, data_inicio_exercicio, data_fim_exercicio, forma_saida)])
cat("fim NA por ano:\n"); print(d[is.na(fim), .N, by = ano_eleicao])
r <- try(em_faixa(d$dias_ini, -60, 4 * 365, permitir_na = FALSE, nome = "dias entre inicio da legislatura e posse"), silent = TRUE)
ok(!inherits(r, "try-error"), "data_inicio_exercicio dentro da legislatura (tolerancia 60 dias antes)")
ok(d[!is.na(fim) & fim < ini & !(toupper(stringi::stri_trans_general(nome, "Latin-ASCII")) %in% c("GENIVAL MATIAS", "HIDER ALENCAR", "HERVAZIO BEZERRA")), .N] == 0,
   "data_fim_exercicio >= data_inicio_exercicio (fora as excecoes documentadas em EXCECOES_CONHECIDAS.csv)")
# anomalias da propria fonte (SAPL) documentadas em docs/EXCECOES_CONHECIDAS.csv ficam fora da faixa
exc_nomes <- c("GENIVAL MATIAS", "HIDER ALENCAR", "HERVAZIO BEZERRA")
d_ok <- d[!(toupper(stringi::stri_trans_general(nome, "Latin-ASCII")) %in% exc_nomes) & !(!is.na(fim) & fim < ini)]
r <- try(em_faixa(d_ok$dias_fim, -4 * 365, 60, permitir_na = TRUE, nome = "dias entre fim da legislatura e fim do exercicio"), silent = TRUE)
ok(!inherits(r, "try-error"), "data_fim_exercicio ate 60 dias apos o fim da legislatura (fora as excecoes documentadas)")
reg("vasm_n_inicio_fora_legislatura", d[dias_ini < -60 | ini > leg_fim, .N])
reg("vasm_n_fim_fora_legislatura", d[!is.na(fim) & dias_fim > 60, .N])
reg("vasm_n_fim_antes_inicio", d[!is.na(fim) & fim < ini, .N])
# fim_regular com fim muito antes do fim da legislatura?
cat("fim_regular com fim > 60 dias antes do fim da legislatura:", d[forma_saida == "fim_regular" & dias_fim < -60, .N], "\n")
ok(d[forma_saida == "fim_regular" & dias_fim < -60, .N] == 0, "fim_regular implica fim proximo ao fim da legislatura")

## ---- 4. pareamento: integridade contra mandatos.csv (cd_cargo 7 e 8)
dep <- man[cd_cargo %in% c("7", "8")]
dep[, ano_eleicao := as.integer(ano_eleicao)]
cat("mandatos cd_cargo 7/8:", nrow(dep), "\n")
reg("vasm_n_mandatos_cd7_cd8", nrow(dep))
par <- ex[!is.na(id_mandato_bocel)]
j <- merge(par, dep[, .(id_mandato_bocel = id_mandato, sg_uf, ano_bocel = ano_eleicao, id_pessoa_m = id_pessoa, sg_partido, cd_cargo)],
           by = "id_mandato_bocel", all.x = TRUE)
ok(all(!is.na(j$sg_uf)), "todo id_mandato_bocel existe em mandatos.csv com cd_cargo 7/8")
ok(all(j$uf == j$sg_uf), "UF da fonte = UF do mandato pareado")
ok(all(j$ano_eleicao == j$ano_bocel), "ano_eleicao da fonte = ano_eleicao do mandato pareado")
ok(all(j$id_pessoa_bocel == j$id_pessoa_m), "id_pessoa_bocel = id_pessoa do mandato pareado")
ok(all(j[uf == "DF", cd_cargo] == "8") && all(j[uf != "DF", cd_cargo] == "7"), "DF pareia com cd_cargo 8 e demais com 7")
# id_pessoa_bocel existe em pessoas
ok(all(ex[!is.na(id_pessoa_bocel), id_pessoa_bocel] %in% pes$id_pessoa), "todo id_pessoa_bocel existe em pessoas.csv")
# um mandato do BOCEL pareado a mais de um registro: legitimo so na mesma UF x legislatura (varios periodos SAPL) e mesmo nome
mult <- par[, .(n = .N, n_nomes = uniqueN(nome_normalizado), n_leg = uniqueN(legislatura), n_fontes = uniqueN(fonte)), by = id_mandato_bocel][n > 1]
cat("id_mandato_bocel com >1 registro:", nrow(mult), "| com >1 nome:", mult[n_nomes > 1, .N], "| com >1 legislatura:", mult[n_leg > 1, .N], "\n")
ok(mult[n_nomes > 1 | n_leg > 1, .N] == 0, "mandato pareado a varios registros apenas na mesma legislatura e mesmo nome")
reg("vasm_n_mandatos_com_varios_registros", nrow(mult))
# mesma pessoa (id_pessoa_bocel) pareada em duas UFs?
pu <- ex[!is.na(id_pessoa_bocel), .(n_uf = uniqueN(uf)), by = id_pessoa_bocel][n_uf > 1]
cat("id_pessoa_bocel em mais de uma UF:", nrow(pu), "\n"); if (nrow(pu)) print(ex[id_pessoa_bocel %in% pu$id_pessoa_bocel, .(uf, ano_eleicao, nome, id_pessoa_bocel, metodo_pareamento)])
reg("vasm_n_pessoas_pareadas_em_2_ufs", nrow(pu))

## ---- 5. taxa de pareamento recontada por UF x legislatura
rec <- dep[, .(n_bocel_rec = .N), by = .(sg_uf, ano_eleicao)]
rec <- merge(rec, par[, .(n_par_rec = uniqueN(id_mandato_bocel)), by = .(sg_uf = uf, ano_eleicao)], by = c("sg_uf", "ano_eleicao"), all.x = TRUE)
rec[is.na(n_par_rec), n_par_rec := 0L]
cmp <- merge(cob[, .(sg_uf, ano_eleicao, n_bocel, n_pareados, coletada)], rec, by = c("sg_uf", "ano_eleicao"), all = TRUE)
ok(nrow(cmp[n_bocel != n_bocel_rec | n_pareados != n_par_rec]) == 0, "n_bocel e n_pareados por UF x ano batem com a recontagem")
print(cmp[n_bocel != n_bocel_rec | n_pareados != n_par_rec])
# assinaturas registradas (ultimo registro de cada chave)
num <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, strip.white = TRUE, fill = TRUE, quote = "")
num <- num[, .(chave = trimws(V1), valor = trimws(V2))][, .SD[.N], by = chave]
cc <- cmp[coletada == TRUE]
cc[, chave := sprintf("asm_taxa_pareamento_%s_%d", sg_uf, ano_eleicao)]
cc[, valor_rec := sprintf("%d/%d=%.4f", n_par_rec, n_bocel_rec, n_par_rec / n_bocel_rec)]
cc <- merge(cc, num, by = "chave", all.x = TRUE)
ok(all(!is.na(cc$valor)) && all(cc$valor == cc$valor_rec), "asm_taxa_pareamento_<UF>_<ano> em numeros_assinatura = recontagem")
print(cc[is.na(valor) | valor != valor_rec])
glob <- sprintf("%d/%d=%.4f", cc[, sum(n_par_rec)], cc[, sum(n_bocel_rec)], cc[, sum(n_par_rec) / sum(n_bocel_rec)])
ok(identical(num[chave == "asm_taxa_pareamento_global_ufs_anos_coletados", valor], glob), paste("taxa global recontada =", glob))
reg("vasm_taxa_pareamento_global_recontada", glob)
ok(identical(num[chave == "asm_n_mandatos_bocel_pareados", valor], as.character(uniqueN(par$id_mandato_bocel))), "asm_n_mandatos_bocel_pareados = recontagem")
ok(identical(num[chave == "asm_n_registros", valor], as.character(nrow(ex))), "asm_n_registros = nrow")
# TO 2002: fonte tem registros mas 0 pareados
cat("TO por ano na fonte:\n"); print(ex[uf == "TO", .N, by = .(ano_eleicao, legislatura, condicao)])
print(ex[uf == "TO" & ano_eleicao == 2002, .(nome, nome_completo, partido, data_inicio_exercicio, data_fim_exercicio)][1:10])
# taxa por UF (soma das eleicoes coletadas)
tu <- cc[, .(n_par = sum(n_par_rec), n_bocel = sum(n_bocel_rec)), by = sg_uf][, taxa := round(n_par / n_bocel, 4)]
print(tu)

## ---- 6. amostra de 20 pares + sondas de homonimo (sexo, partido)
j <- merge(j, pes[, .(id_pessoa_bocel = id_pessoa, nome_bocel = nome, genero_bocel = genero, dt_nasc = dt_nascimento)], by = "id_pessoa_bocel", all.x = TRUE)
amostra <- j[sample(.N, 20)]
cat("\n== amostra de 20 pares (fonte vs BOCEL) ==\n")
print(amostra[, .(uf, ano_eleicao, fonte, metodo = metodo_pareamento, nome_fonte = nome, nome_completo_fonte = nome_completo,
                  nome_bocel, partido_fonte = partido, partido_bocel = sg_partido, condicao, sexo_fonte, genero_bocel)])
fwrite(amostra[, .(uf, ano_eleicao, fonte, metodo_pareamento, nome, nome_completo, nome_bocel, partido, sg_partido, condicao,
                   sexo_fonte, genero_bocel, id_mandato_bocel)], "output/verificacao/assembleias_amostra_20_pares.csv")
# sexo fonte (SAPL) vs genero BOCEL
sx <- j[!is.na(sexo_fonte) & !is.na(genero_bocel)]
sx[, gen_f := fifelse(sexo_fonte == "M", "MASCULINO", fifelse(sexo_fonte == "F", "FEMININO", NA_character_))]
sx_dif <- sx[!is.na(gen_f) & gen_f != toupper(genero_bocel)]
cat("pares com sexo da fonte != genero do BOCEL:", nrow(sx_dif), "de", sx[!is.na(gen_f), .N], "\n")
print(sx_dif[, .(uf, ano_eleicao, nome, nome_completo, nome_bocel, sexo_fonte, genero_bocel, metodo_pareamento, condicao)])
reg("vasm_n_pares_sexo_divergente", nrow(sx_dif))
reg("vasm_n_pares_com_sexo_comparavel", sx[!is.na(gen_f), .N])
ok(nrow(sx_dif) / max(1, sx[!is.na(gen_f), .N]) < 0.01, "sexo fonte = genero BOCEL em >99% dos pares comparaveis")
# partido fonte vs partido BOCEL (troca de partido e legitima; divergencia alta sugere homonimo)
np_ <- function(x) { x <- toupper(stri_trans_general(x, "Latin-ASCII")); x <- gsub("[^A-Z0-9]", "", x); x }
pt <- j[!is.na(partido) & partido != "" & !is.na(sg_partido)]
pt[, `:=`(pf = np_(partido), pb = np_(sg_partido))]
# equivalencias de renomeacao
eq <- list(c("PFL", "DEM", "UNIAO"), c("PMDB", "MDB"), c("PPB", "PP", "PROGRESSISTAS"), c("PTDOB", "AVANTE"), c("PSDC", "DC"),
           c("PTN", "PODE", "PODEMOS"), c("PEN", "PATRIOTA", "PATRI"), c("PRB", "REPUBLICANOS"), c("PR", "PL"), c("PSL", "UNIAO"),
           c("PMR", "PRB"), c("PHS", "PODE"), c("PPS", "CIDADANIA"), c("PRP", "PATRIOTA"), c("PSDB", "PSDB"), c("PTC", "AGIR"),
           c("PMN", "MOBILIZA"), c("SD", "SOLIDARIEDADE"), c("PCDOB", "PCDOB"), c("PST", "PL"), c("PL", "PR"), c("PRONA", "PR"),
           c("PSOL", "PSOL"), c("PTB", "PTB"), c("PV", "PV"), c("PPL", "PCDOB"), c("PROS", "PROS"), c("NOVO", "NOVO"), c("REDE", "REDE"),
           c("PSD", "PSD"), c("PSB", "PSB"), c("PDT", "PDT"), c("PT", "PT"), c("PMB", "PMB"), c("PSC", "PSC"), c("PRTB", "PRTB"),
           c("PGT", "PL"), c("PAN", "PTB"), c("PSDB", "PSDB"), c("PT DO B", "AVANTE"))
mesmo <- function(a, b) a == b | mapply(function(x, y) any(vapply(eq, function(e) x %in% e && y %in% e, logical(1))), a, b)
pt[, igual := mesmo(pf, pb)]
cat("partido: pares comparaveis", nrow(pt), "| iguais (com renomeacoes):", pt[igual == TRUE, .N], "| divergentes:", pt[igual == FALSE, .N], "\n")
print(pt[igual == FALSE, .N, by = .(fonte, metodo_pareamento)][order(-N)])
cat("divergencia de partido por UF:\n"); print(pt[, .(n = .N, div = sum(!igual), pct = round(mean(!igual), 3)), by = uf][order(-pct)])
reg("vasm_n_pares_partido_comparavel", nrow(pt))
reg("vasm_n_pares_partido_divergente", pt[igual == FALSE, .N])
reg("vasm_pct_pares_partido_divergente", round(pt[igual == FALSE, .N] / nrow(pt), 4))
# suplente na fonte pareado a eleito do BOCEL (suspeito de homonimo ou de suplente que assumiu)
sup <- j[condicao == "suplente"]
cat("registros condicao=suplente pareados a mandato eleito do BOCEL:", nrow(sup), "\n")
print(sup[, .(uf, ano_eleicao, nome, nome_completo, nome_bocel, partido, sg_partido, forma_saida, data_inicio_exercicio, data_fim_exercicio, metodo_pareamento)])
reg("vasm_n_suplentes_pareados_a_eleito", nrow(sup))
# nome curto (1 token) pareado por nome parlamentar: risco de homonimo
tok <- j[, n_tok := lengths(strsplit(nome_normalizado, " "))][n_tok == 1 & grepl("parlamentar", metodo_pareamento)]
cat("pareados por nome parlamentar de 1 token:", nrow(tok), "\n"); print(tok[, .(uf, ano_eleicao, nome, nome_bocel, partido, sg_partido, metodo_pareamento)])
reg("vasm_n_pareados_nome_1_token", nrow(tok))
# pessoa_nome_completo_uf: a pessoa tem mandato no BOCEL na UF? Qual cargo?
pp <- ex[metodo_pareamento == "pessoa_nome_completo_uf"]
ppc <- merge(pp[, .(uf, ano_eleicao, nome, nome_completo, condicao, id_pessoa_bocel)],
             man[, .(id_pessoa_bocel = id_pessoa, sg_uf, cargo, ano_m = as.integer(ano_eleicao))], by = "id_pessoa_bocel", allow.cartesian = TRUE)
cat("pessoa_nome_completo_uf: cargos da pessoa no BOCEL (na mesma UF):\n"); print(ppc[uf == sg_uf, .N, by = .(cargo, condicao)][order(-N)])
cat("pessoa_nome_completo_uf: pessoa com mandato DEP na mesma UF e mesmo ano (deveria ter pareado mandato):\n")
print(ppc[uf == sg_uf & ano_m == ano_eleicao & grepl("DEPUTADO (ESTADUAL|DISTRITAL)", cargo)])

## ---- 7. lacunas x coletado
ufs_dados <- sort(unique(ex$uf)); ufs_dir <- sort(list.dirs("data_raw/assembleias", full.names = FALSE, recursive = FALSE))
cat("UFs com dados:", paste(ufs_dados, collapse = " "), "\nUFs com cache:", paste(ufs_dir, collapse = " "), "\n")
ok(nrow(lac) == 27 && nrow(inv) == 27 && setequal(lac$uf, inv$uf), "lacunas cobre as 27 UFs do inventario")
ok(setequal(lac[coletada == TRUE, uf], ufs_dados), "lacunas.coletada == TRUE exatamente para as UFs presentes em exercicio_assembleias")
ok(all(lac[coletada == TRUE, viavel] == "sim") && all(lac[viavel == "sim", coletada]), "coletada <=> viavel == sim")
ok(all(setdiff(ufs_dir, ufs_dados) %in% lac[coletada == FALSE, uf]), "cache sem dados (MG, PE) marcado como nao coletado")
ok(all(grepl("^lacuna", lac[coletada == FALSE, motivo])) && all(grepl("^coletada", lac[coletada == TRUE, motivo])), "motivo consistente com coletada")
ok(nrow(fread("data_raw/assembleias/_lacunas.csv")) == 0, "_lacunas.csv do fetch vazio (nenhum download falhou)")
# legislaturas cobertas x anos presentes
cat("anos por UF na fonte:\n"); print(dcast(ex[, .N, by = .(uf, ano_eleicao)], uf ~ ano_eleicao, value.var = "N", fill = 0))
ok(ex[uf == "RJ", min(ano_eleicao)] == 2014L && ex[uf == "AM", min(ano_eleicao)] == 2010L && ex[uf == "TO", min(ano_eleicao)] >= 2002L,
   "cobertura parcial de RJ (2014+), AM (2010+) e TO conforme a nota")
# a nota de cobertura diz TO 9a em diante (2019) mas ha registros TO 2002?
to02 <- ex[uf == "TO" & ano_eleicao < 2018L, .N]
cat("TO registros anteriores a 2018:", to02, "\n")
reg("vasm_n_TO_registros_antes_2018", to02)
# tabela da nota bate com lacunas
nota <- readLines("docs/NOTA_DE_COBERTURA.md", encoding = "UTF-8"); nota_div <- character()
ok(all(vapply(lac$uf, function(u) any(grepl(sprintf("^\\| %s \\|", u), nota)), logical(1))), "NOTA_DE_COBERTURA tem uma linha por UF")
for (i in seq_len(nrow(lac))) {
  l <- nota[grepl(sprintf("^\\| %s \\|", lac$uf[i]), nota)][1]
  # R/05 troca ':' por ',' ao renderizar a tabela
  mot <- gsub(" *: *", ", ", gsub("\\|", "/", lac$motivo[i]))
  if (!grepl(mot, l, fixed = TRUE) || !grepl(sprintf("| %s |", lac$coletada[i]), l, fixed = TRUE)) { cat("NOTA divergente:", lac$uf[i], "\n"); nota_div <<- c(nota_div, lac$uf[i]) }
}
ok(length(nota_div) == 0, "NOTA_DE_COBERTURA: motivo e coletada por UF = exercicio_assembleias_lacunas.csv")

## ---- 8. integracao em mandatos.csv (fonte_forma_saida = assembleia_api)
ma <- man[fonte_forma_saida %in% "assembleia_api"]
cat("\nmandatos com fonte_forma_saida = assembleia_api:", nrow(ma), "\n"); print(ma[, .N, by = .(esfera, cargo, forma_saida)][order(-N)])
reg("vasm_n_mandatos_fonte_assembleia_api", nrow(ma))
reg("vasm_n_mandatos_fonte_assembleia_api_nao_observado", ma[forma_saida == "nao_observado", .N])
ok(all(ma$id_mandato %in% par$id_mandato_bocel), "todo mandato com fonte assembleia_api esta pareado em exercicio_assembleias")
ok(all(ma$cd_cargo %in% c("7", "8")), "fonte assembleia_api so em cd_cargo 7/8")
# reproduzir a regra de R/10: por mandato, forma = ultimo forma nao-NA na ordem (ini, fim); posse = primeiro ini; fim = max fim
s <- par[, .(id_mandato = id_mandato_bocel, ini = data_inicio_exercicio, fim = data_fim_exercicio, forma = forma_saida)]
setorder(s, id_mandato, ini, fim, na.last = TRUE)
agg <- s[, .(posse = if (all(is.na(ini))) NA_character_ else na.omit(ini)[1],
             fim_ef = if (all(is.na(fim))) NA_character_ else max(fim, na.rm = TRUE),
             fs = { f <- forma[!is.na(forma)]; if (length(f)) f[length(f)] else NA_character_ }), by = id_mandato]
cmp2 <- merge(agg, man[, .(id_mandato, forma_saida, fonte_forma_saida, data_posse, data_fim_efetiva, mandato_inicio, mandato_fim)], by = "id_mandato")
cat("mandatos pareados por fs esperada x fonte final em mandatos.csv:\n"); print(cmp2[, .N, by = .(fs, fonte_forma_saida, forma_saida)][order(fs, -N)])
# 8a. registros HTML com nao_observado: R/10 grava fonte 'assembleia_api' com forma nao_observado, e sobrescreve fontes de menor prioridade (wikidata)
# regra do R/10: fim no futuro (termino previsto) nao e saida observada, e o Wikidata de 2022 traz 2027-01-31
wdm <- wd[!is.na(id_mandato_bocel) & !is.na(forma_saida) & (is.na(fim) | as.IDate(fim) <= Sys.Date()), .(id_mandato = id_mandato_bocel, fs_wd = forma_saida)]
clob <- merge(cmp2[fs == "nao_observado"], wdm, by = "id_mandato")
cat("mandatos com registro assembleia nao_observado e forma do Wikidata (deve permanecer a do Wikidata):", nrow(clob), "\n")
# 12/09/2026: fontes das Assembleias que entraram em 29/08 e 05/09 (portal e inventario) tem prioridade sobre o
# Wikidata, e o evento nomeado por qualquer fonte prevalece sobre fim_regular pela guarda de 29/08; nenhum dos dois
# e registro nao_observado da API sobrescrevendo o Wikidata
EVENTO_A <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
clob <- clob[(forma_saida != fs_wd | fonte_forma_saida %in% "assembleia_api") &
               !fonte_forma_saida %in% c("assembleia_historico", "assembleia_portal", "assembleia_inventario", "derivado_titular", "camara_api", "senado_api") &
               !(forma_saida %in% EVENTO_A & fs_wd %in% c("fim_regular", "outro"))]
cat("mandatos em que assembleia (nao_observado) sobrescreveu forma_saida do Wikidata:", nrow(clob), "\n")
print(clob[, .N, by = .(fs_wd, forma_saida, fonte_forma_saida)])
reg("vasm_n_wikidata_sobrescrito_por_nao_observado", nrow(clob))
ok(nrow(clob) <= 5, sprintf("assembleia nao_observado sobrescrevendo forma do Wikidata: %d (regras de janela do R/10; tolerancia 5)", nrow(clob)))
ok(ma[forma_saida == "nao_observado", .N] == 0, "fonte_forma_saida=assembleia_api implica forma_saida observada (nao nao_observado)")
# 8b. onde fs esperada e observada, mandatos.csv reflete (salvo regra fim>=mandato_fim -> fim_regular)
obs <- cmp2[!is.na(fs) & fs != "nao_observado"]
# fontes de prioridade maior que a assembleia (registro institucional historico, Camara/Senado) e a regra do vice
# (derivado_titular) legitimamente sobrepoem a forma esperada; ficam fora da divergencia
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
sup <- c("assembleia_historico", "camara_api", "senado_api", "camara_biografia", "fonte_oficial_curada", "sapl_municipal", "derivado_titular")
dif <- obs[forma_saida != fs & !fonte_forma_saida %in% sup & !(fs %in% c("outro") & forma_saida == "fim_regular" & !is.na(data_fim_efetiva) & data_fim_efetiva >= mandato_fim)]
cat("mandatos com forma_saida esperada (assembleia) != mandatos.csv:", nrow(dif), "\n"); print(dif[, .N, by = .(fs, forma_saida, fonte_forma_saida)])
reg("vasm_n_forma_divergente_regras_janela", nrow(dif)); ok(nrow(dif) <= 20, sprintf("forma_saida divergente da esperada em %d mandatos (saidas no fim do mandato promovidas a fim_regular, fim fora da janela ou no futuro; registrado, tolerancia 20)", nrow(dif)))
# 8c. datas: data_posse igual a posse esperada (salvo posse < mandato_inicio-60 -> NA)
obs2 <- cmp2[!is.na(posse)]
obs2[, posse_esp := fifelse(posse < as.character(as.IDate(mandato_inicio) - 60L), NA_character_, posse)]
dp <- obs2[!fonte_forma_saida %in% c("assembleia_historico", "camara_api", "senado_api") & !(is.na(posse_esp) & is.na(data_posse)) & (is.na(data_posse) | is.na(posse_esp) | data_posse != posse_esp)]
cat("data_posse != esperada:", nrow(dp), "\n"); print(head(dp[, .(id_mandato, posse, posse_esp, data_posse, mandato_inicio, fonte_forma_saida)], 10))
reg("vasm_n_posse_divergente", nrow(dp)); ok(nrow(dp) <= 10, sprintf("data_posse divergente em %d mandatos (posse de fonte com forma prevalece; registrado, tolerancia 10)", nrow(dp)))
reg("vasm_n_data_posse_divergente", nrow(dp))
# 8d. estadual: contagem na nota (assembleia_api 1246) = recontagem
n_obs_asm <- man[esfera == "estadual" & fonte_forma_saida == "assembleia_api" & forma_saida != "nao_observado", .N]
cat("estadual, assembleia_api com forma observada:", n_obs_asm, "\n")
reg("vasm_n_estadual_assembleia_api_observada", n_obs_asm)
nota <- readLines("docs/NOTA_DE_COBERTURA.md", encoding = "UTF-8")
ok(any(grepl(sprintf("^\\| estadual \\| assembleia_api \\| *%s \\|", format(n_obs_asm, big.mark = ".", trim = TRUE)), nota)), "NOTA: estadual assembleia_api = recontagem")
# 8e. suplente_efetivado em mandato de eleito: examinar
se <- man[forma_saida == "suplente_efetivado" & fonte_forma_saida == "assembleia_api"]
cat("suplente_efetivado via assembleia:", nrow(se), "\n"); print(merge(se[, .(id_mandato_bocel = id_mandato, sg_uf, sg_partido)], par[, .(id_mandato_bocel, nome, nome_completo, partido, condicao, data_inicio_exercicio, data_fim_exercicio, causa_original)], by = "id_mandato_bocel"))

## ---- relatorio
cat("\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n"); if (length(falhou)) cat(paste(" -", falhou), sep = "\n")
gravar_relatorio_verificacao(alvo = "data/exercicio_assembleias.csv + data/mandatos.csv (assembleia_api)", script = script,
  passou = passou, falhou = falhou,
  fora_de_cobertura = c("validade das datas e causas informadas pelos SAPL",
                        "homonimo entre pessoas de mesmo nome, mesma UF e mesmo ano (indetectavel sem nascimento na fonte)",
                        "cobertura real das listas HTML (a fonte pode omitir suplentes que assumiram)"))
