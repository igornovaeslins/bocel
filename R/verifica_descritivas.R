# verifica_descritivas.R — verificacao cetica independente das quatro tabelas descritivas depositadas
# sem verificador proprio ate 05/09/2026: data/raca_eleitos.csv (R/28), data/migracao_partidaria.csv
# (R/30), data/migracao_territorial.csv e data/migracao_territorial_candidaturas.csv (R/31).
#
# Por que. As quatro tabelas entram no zip do deposito (zenodo/deposit.py) e ate aqui so o esquema
# delas era conferido por verifica_documentacao/integracao_docs; nenhum script recontava chave,
# unicidade, integridade referencial contra data/mandatos.csv e data/pessoas.csv, vocabularios e
# faixas. Este verificador nao regrava nada em data/: le as saidas, aplica os asserts de rigor
# (lib/asserts_rigor.R) e grava o relatorio padrao de verificacao. Toda checagem roda ate o fim
# (nenhuma aborta o script), o veredito e reprovado se qualquer uma falhar.
#
# O que confere, por tabela:
#   raca_eleitos               chave id_mandato unica; todo id_mandato existe em mandatos.csv com a
#                              mesma pessoa, ano, cargo e UF; 2010..2024 (anos do parquet de R/28);
#                              vocabulario de cor_raca, genero, regiao e cargo; regiao NA so em BR;
#                              negra = TRUE sse PRETA/PARDA e NA quando a cor nao foi informada
#   migracao_partidaria        chave (id_pessoa, fonte, data_origem, data_evento, partido_origem,
#                              partido_destino) unica; id_pessoa existe em pessoas.csv e tem mandato;
#                              fonte no vocabulario; troca_efetiva = (origem_canon != destino_canon);
#                              data_evento >= data_origem; dias = diferenca; ano_evento = ano da data;
#                              nas fontes de mandato, (id_pessoa, ano_origem, cargo_origem) e um mandato
#   migracao_territorial       chave (id_pessoa, ano_origem, ano_destino, cargo_origem, cargo_destino,
#                              unidade_origem, unidade_destino) unica; origem e destino sao mandatos da
#                              pessoa; ano_origem <= ano_destino; tipo deriva de uf/regiao/unidade;
#                              regiao = regiao da UF
#   migracao_territorial_candidaturas  chave (id_pessoa, ano_candidatura, uf_candidatura,
#                              cargo_candidatura) unica; pessoa existe e tem mandato ate o ano;
#                              uf_candidatura fora de ufs_com_mandato_ate_entao; ultimo_ano_mandato
#                              <= ano_candidatura; regiao NA sse uf = BR
# Entrada: data/raca_eleitos.csv, data/migracao_partidaria.csv, data/migracao_territorial.csv,
#          data/migracao_territorial_candidaturas.csv, data/mandatos.csv, data/pessoas.csv
# Saida:   logs/verifica_descritivas.log, output/verificacao/relatorio_verificacao_<ts>.json,
#          chaves vdesc_* em output/numeros_assinatura.txt
# Execucao: Rscript --vanilla R/verifica_descritivas.R  (a partir da raiz do repositorio)
set.seed(20260905)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_descritivas.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- "logs/verifica_descritivas.log"; sink(logf, split = TRUE)
cat("verifica_descritivas.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(paste0("vdesc_", k), v, script = script)
passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { cat("FALHA:", nome, "—", conditionMessage(e), "\n"); FALSE })
  if (r) { passou <<- c(passou, nome); cat("OK:", nome, "\n") } else falhou <<- c(falhou, nome)
  invisible(r)
}

## 0. referencias ------------------------------------------------------------------------------------
UFS <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
REG <- c(AC="Norte", AM="Norte", AP="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
         AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste", PE="Nordeste",
         PI="Nordeste", RN="Nordeste", SE="Nordeste", DF="Centro-Oeste", GO="Centro-Oeste",
         MS="Centro-Oeste", MT="Centro-Oeste", ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
         PR="Sul", RS="Sul", SC="Sul")
REGIOES <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
CARGOS <- c("PRESIDENTE", "VICE-PRESIDENTE", "GOVERNADOR", "VICE-GOVERNADOR", "SENADOR", "DEPUTADO FEDERAL",
            "DEPUTADO ESTADUAL", "DEPUTADO DISTRITAL", "PREFEITO", "VICE-PREFEITO", "VEREADOR")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
mm <- unique(mand[, .(id_pessoa, ano_eleicao, cargo, sg_uf, unidade_posicao)])
for (f in c("data/raca_eleitos.csv", "data/migracao_partidaria.csv", "data/migracao_territorial.csv", "data/migracao_territorial_candidaturas.csv"))
  stopifnot(file.exists(f))

## 1. raca_eleitos --------------------------------------------------------------------------------------
r <- fread("data/raca_eleitos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cat("\n[raca_eleitos] linhas:", nrow(r), "\n")
ok("raca: 11 colunas na ordem canonica", stopifnot(identical(names(r), c("id_mandato","id_pessoa","ano_eleicao","cargo","sg_uf","regiao","genero","cor_raca","negra","instrucao","ocupacao"))))
ok("raca: chave id_mandato unica", checa_unica(as.data.frame(r), "id_mandato"))
ok("raca: todo id_mandato existe em mandatos.csv", stopifnot(all(r$id_mandato %in% mand$id_mandato)))
jr <- merge(r, mand[, .(id_mandato, id_pessoa_m = id_pessoa, ano_m = ano_eleicao, cargo_m = cargo, uf_m = sg_uf)], by = "id_mandato")
ok("raca: pessoa, ano, cargo e UF iguais aos do mandato", stopifnot(nrow(jr) == nrow(r), all(jr$id_pessoa == jr$id_pessoa_m), all(jr$ano_eleicao == jr$ano_m), all(jr$cargo == jr$cargo_m), all(jr$sg_uf == jr$uf_m)))
ok("raca: ano_eleicao em 2010..2024 e par", { em_faixa(as.integer(r$ano_eleicao), 2010, 2024, permitir_na = FALSE, nome = "ano_eleicao"); stopifnot(all(as.integer(r$ano_eleicao) %% 2L == 0L)) })
ok("raca: cobre todo mandato de 2010 em diante", stopifnot(all(mand[as.integer(ano_eleicao) >= 2010L, id_mandato] %in% r$id_mandato)))
ok("raca: cargo no vocabulario", in_set(r$cargo, CARGOS, permitir_na = FALSE, nome = "cargo"))
ok("raca: sg_uf em UF ou BR", in_set(r$sg_uf, c(UFS, "BR"), permitir_na = FALSE, nome = "sg_uf"))
ok("raca: regiao e a da UF, NA so em BR", { in_set(r$regiao, REGIOES, nome = "regiao"); stopifnot(all(is.na(r$regiao) == (r$sg_uf == "BR")), all(r[sg_uf != "BR", regiao == REG[sg_uf]])) })
ok("raca: genero no vocabulario de pessoas.csv", in_set(r$genero, unique(na.omit(pess$genero)), nome = "genero"))
# 05/09/2026: o TSE grava '#NE#' (campo nao exigido) em 2010 e 2012; R/28 converte a NA a lista
# '', '#NULO#', '#NE', 'NAO INFORMADO', 'NAO DIVULGAVEL', que nao contem '#NE#'. A cor autodeclarada
# so existe a partir de 2014, logo o vocabulario declarado aqui e o das categorias do TSE mais NA.
COR <- c("BRANCA", "PRETA", "PARDA", "AMARELA", "INDÍGENA")
n_ne <- r[cor_raca == "#NE#", .N]; reg("raca_cor_nao_exigida_ne", n_ne)
cat("cor_raca por ano:\n"); print(dcast(r[, .N, by = .(ano_eleicao, cor_raca = fcoalesce(cor_raca, "NA"))], ano_eleicao ~ cor_raca, value.var = "N", fill = 0L))
ok("raca: cor_raca nas cinco categorias do TSE ou NA", in_set(r$cor_raca, COR, nome = "cor_raca"))
ok("raca: negra = TRUE sse PRETA ou PARDA, FALSE nas demais categorias", stopifnot(all(r[cor_raca %in% c("PRETA", "PARDA"), negra == "TRUE"]), all(r[cor_raca %in% c("BRANCA", "AMARELA", "INDÍGENA"), negra == "FALSE"])))
ok("raca: negra e NA quando cor_raca nao e uma categoria autodeclarada", stopifnot(all(r[!cor_raca %in% COR, is.na(negra)])))
ok("raca: negra so em TRUE/FALSE/NA", in_set(r$negra, c("TRUE", "FALSE"), nome = "negra"))
ok("raca: instrucao e ocupacao sem NA", stopifnot(!anyNA(r$instrucao), !anyNA(r$ocupacao)))
reg("raca_n_linhas", nrow(r)); reg("raca_n_negra_true", r[negra == "TRUE", .N]); reg("raca_n_cor_na", sum(is.na(r$cor_raca)))

## 2. migracao_partidaria ---------------------------------------------------------------------------------
mp <- fread("data/migracao_partidaria.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cat("\n[migracao_partidaria] linhas:", nrow(mp), "\n"); print(mp[, .N, by = fonte])
ok("migp: 17 colunas na ordem canonica", stopifnot(identical(names(mp), c("id_pessoa","fonte","partido_origem","partido_destino","origem_canon","destino_canon","troca_efetiva","data_origem","data_evento","ano_evento","dias","sg_uf","regiao","ano_origem","ano_destino","cargo_origem","cargo_destino"))))
K_MP <- c("id_pessoa", "fonte", "data_origem", "data_evento", "partido_origem", "partido_destino")
n_dup_mp <- sum(duplicated(mp, by = K_MP)); reg("migp_linhas_chave_repetida", n_dup_mp)
if (n_dup_mp) { cat("linhas com chave repetida por fonte:\n"); print(mp[duplicated(mp, by = K_MP), .N, by = fonte]) }
ok("migp: chave (pessoa, fonte, data_origem, data_evento, partido_origem, partido_destino) unica", checa_unica(as.data.frame(mp), K_MP))
ok("migp: id_pessoa existe em pessoas.csv", stopifnot(all(mp$id_pessoa %in% pess$id_pessoa)))
ok("migp: toda pessoa tem mandato no BOCEL", stopifnot(all(mp$id_pessoa %in% mand$id_pessoa)))
ok("migp: fonte no vocabulario", in_set(mp$fonte, c("filiacao", "mandato_para_mandato", "mandato_para_candidatura"), permitir_na = FALSE, nome = "fonte"))
ok("migp: partidos e canonicos sem NA", stopifnot(!anyNA(mp$partido_origem), !anyNA(mp$partido_destino), !anyNA(mp$origem_canon), !anyNA(mp$destino_canon)))
ok("migp: troca_efetiva = (origem_canon != destino_canon)", stopifnot(all((mp$troca_efetiva == "TRUE") == (mp$origem_canon != mp$destino_canon))))
ok("migp: partido_origem != partido_destino", stopifnot(all(mp$partido_origem != mp$partido_destino)))
ok("migp: datas ISO e data_evento >= data_origem", stopifnot(all(grepl("^\\d{4}-\\d{2}-\\d{2}$", mp$data_origem)), all(grepl("^\\d{4}-\\d{2}-\\d{2}$", mp$data_evento)), all(mp$data_evento >= mp$data_origem)))
ok("migp: dias = data_evento - data_origem", stopifnot(all(as.integer(mp$dias) == as.integer(as.IDate(mp$data_evento) - as.IDate(mp$data_origem)))))
ok("migp: ano_evento = ano de data_evento, em 1980..2026", { stopifnot(all(mp$ano_evento == substr(mp$data_evento, 1, 4))); em_faixa(as.integer(mp$ano_evento), 1980, 2026, permitir_na = FALSE, nome = "ano_evento") })
# ZZ e o codigo do TSE para filiacao no exterior (35 linhas da fonte filiacao em 05/09/2026): valor da
# fonte declarado no conjunto, com regiao NA como em BR
reg("migp_n_uf_zz_exterior", mp[sg_uf == "ZZ", .N])
ok("migp: sg_uf em UF, BR ou ZZ (exterior); regiao e a da UF, NA so em BR/ZZ", { in_set(mp$sg_uf, c(UFS, "BR", "ZZ"), permitir_na = FALSE, nome = "sg_uf"); in_set(mp$regiao, REGIOES, nome = "regiao"); stopifnot(all(is.na(mp$regiao) == (mp$sg_uf %in% c("BR", "ZZ"))), all(mp[!sg_uf %in% c("BR", "ZZ"), regiao == REG[sg_uf]])) })
ok("migp: ano_origem, ano_destino e cargos so nas fontes de mandato", stopifnot(all(is.na(mp[fonte == "filiacao", ano_origem])), all(!is.na(mp[fonte != "filiacao", ano_origem])), all(!is.na(mp[fonte != "filiacao", cargo_origem]))))
ok("migp: cargos no vocabulario", { in_set(mp$cargo_origem, CARGOS, nome = "cargo_origem"); in_set(mp$cargo_destino, CARGOS, nome = "cargo_destino") })
x <- mp[fonte != "filiacao"]
a <- merge(x, mm[, .(id_pessoa, ano_origem = ano_eleicao, cargo_origem = cargo, uf_m = sg_uf)], by = c("id_pessoa", "ano_origem", "cargo_origem"), all.x = TRUE)
ok("migp: (pessoa, ano_origem, cargo_origem) e um mandato do BOCEL nas fontes de mandato", stopifnot(!anyNA(a$uf_m)))
# sg_uf da linha e a UF do mandato de destino (R/30, bloco B): semi-juncao pela chave completa, porque a
# mesma pessoa pode ter dois mandatos no mesmo ano em UFs distintas e um merge parcial multiplicaria linhas
b <- mp[fonte == "mandato_para_mandato", .(id_pessoa, ano_destino, cargo_destino, sg_uf)]
b_ok <- !is.na(mm[b, on = .(id_pessoa, ano_eleicao = ano_destino, cargo = cargo_destino, sg_uf), which = TRUE, mult = "first"])
ok("migp: mandato_para_mandato: destino e um mandato do BOCEL com a UF gravada", stopifnot(all(b_ok)))
ok("migp: mandato_para_candidatura: ano_destino > ano_origem e cargo_destino = cargo_origem", stopifnot(mp[fonte == "mandato_para_candidatura", all(as.integer(ano_destino) > as.integer(ano_origem) & cargo_destino == cargo_origem)]))
n_m2m_mesmo_ano <- mp[fonte == "mandato_para_mandato" & ano_destino == ano_origem, .N]; reg("migp_m2m_pares_no_mesmo_ano", n_m2m_mesmo_ano)
cat("mandato_para_mandato com origem e destino no mesmo ano de eleicao (dois mandatos na mesma eleicao):", n_m2m_mesmo_ano, "\n")
ok("migp: mandato_para_mandato: ano_destino > ano_origem", stopifnot(mp[fonte == "mandato_para_mandato", all(as.integer(ano_destino) > as.integer(ano_origem))]))
reg("migp_n_linhas", nrow(mp)); reg("migp_n_troca_efetiva", mp[troca_efetiva == "TRUE", .N])
for (f in unique(mp$fonte)) reg(paste0("migp_n_", f), mp[fonte == f, .N])

## 3. migracao_territorial ---------------------------------------------------------------------------------
mt <- fread("data/migracao_territorial.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cat("\n[migracao_territorial] linhas:", nrow(mt), "\n"); print(mt[, .N, by = tipo])
ok("migt: 14 colunas na ordem canonica", stopifnot(identical(names(mt), c("id_pessoa","ano_origem","ano_destino","cargo_origem","cargo_destino","uf_origem","uf_destino","regiao_origem","regiao_destino","unidade_origem","unidade_destino","municipio_origem","municipio_destino","tipo"))))
K_MT <- c("id_pessoa", "ano_origem", "ano_destino", "cargo_origem", "cargo_destino", "unidade_origem", "unidade_destino")
n_dup_mt <- sum(duplicated(mt, by = K_MT)); reg("migt_linhas_chave_repetida", n_dup_mt)
if (n_dup_mt) { cat("linhas com chave repetida:\n"); print(mt[duplicated(mt, by = K_MT) | duplicated(mt, by = K_MT, fromLast = TRUE)]) }
ok("migt: chave (pessoa, anos, cargos, unidades) unica", checa_unica(as.data.frame(mt), K_MT))
ok("migt: id_pessoa existe em pessoas.csv", stopifnot(all(mt$id_pessoa %in% pess$id_pessoa)))
d_o <- merge(mt, mm[, .(id_pessoa, ano_origem = ano_eleicao, cargo_origem = cargo, unidade_origem = unidade_posicao, uf_m = sg_uf)], by = c("id_pessoa", "ano_origem", "cargo_origem", "unidade_origem"), all.x = TRUE)
ok("migt: origem e um mandato da pessoa (ano, cargo, unidade) com a UF gravada", stopifnot(!anyNA(d_o$uf_m), all(d_o$uf_origem == d_o$uf_m)))
d_d <- merge(mt, mm[, .(id_pessoa, ano_destino = ano_eleicao, cargo_destino = cargo, unidade_destino = unidade_posicao, uf_m = sg_uf)], by = c("id_pessoa", "ano_destino", "cargo_destino", "unidade_destino"), all.x = TRUE)
ok("migt: destino e um mandato da pessoa (ano, cargo, unidade) com a UF gravada", stopifnot(!anyNA(d_d$uf_m), all(d_d$uf_destino == d_d$uf_m)))
ok("migt: anos em 1998..2024 e ano_origem <= ano_destino", { em_faixa(as.integer(mt$ano_origem), 1998, 2024, permitir_na = FALSE, nome = "ano_origem"); em_faixa(as.integer(mt$ano_destino), 1998, 2024, permitir_na = FALSE, nome = "ano_destino"); stopifnot(all(as.integer(mt$ano_origem) <= as.integer(mt$ano_destino))) })
n_mesmo_ano <- mt[ano_origem == ano_destino, .N]; reg("migt_pares_no_mesmo_ano", n_mesmo_ano)
cat("pares de mandatos consecutivos no mesmo ano de eleicao (pessoa com dois mandatos na mesma eleicao):", n_mesmo_ano, "\n")
ok("migt: cargos e UFs no vocabulario", { in_set(mt$cargo_origem, CARGOS, permitir_na = FALSE, nome = "cargo_origem"); in_set(mt$cargo_destino, CARGOS, permitir_na = FALSE, nome = "cargo_destino"); in_set(mt$uf_origem, c(UFS, "BR"), permitir_na = FALSE, nome = "uf_origem"); in_set(mt$uf_destino, c(UFS, "BR"), permitir_na = FALSE, nome = "uf_destino") })
ok("migt: regiao e a da UF, NA so em BR", stopifnot(all(is.na(mt$regiao_origem) == (mt$uf_origem == "BR")), all(is.na(mt$regiao_destino) == (mt$uf_destino == "BR")), all(mt[uf_origem != "BR", regiao_origem == REG[uf_origem]]), all(mt[uf_destino != "BR", regiao_destino == REG[uf_destino]])))
ok("migt: tipo no vocabulario", in_set(mt$tipo, c("mesma unidade", "entre UFs da mesma regiao", "entre regioes", "entre municipios da mesma UF"), permitir_na = FALSE, nome = "tipo"))
# tipo recontado pela regra de R/31 tal como escrita: regiao diferente > UF diferente > municipio diferente
# (ambos municipais) > mesma unidade. A regra compara REG[uf], que e NA para BR, e o fcase trata NA como
# FALSE: um par governador -> presidente sai como "entre UFs da mesma regiao". Espelhado aqui como esta;
# o numero de pares com BR fica registrado para o autor decidir o rotulo (05/09/2026).
mt[, tipo_rec := fcase(uf_origem != uf_destino & REG[uf_origem] != REG[uf_destino], "entre regioes",
                       uf_origem != uf_destino, "entre UFs da mesma regiao",
                       !is.na(municipio_origem) & !is.na(municipio_destino) & unidade_origem != unidade_destino, "entre municipios da mesma UF",
                       default = "mesma unidade")]
ok("migt: tipo reconta a partir de UF, regiao e unidade (regra de R/31)", stopifnot(all(mt$tipo == mt$tipo_rec)))
reg("migt_pares_com_uf_br", mt[uf_origem == "BR" | uf_destino == "BR", .N])
# "mesma unidade" e o rotulo residual de R/31 e cobre a mudanca de cargo dentro da mesma UF (vereador ->
# deputado estadual, unidade diferente); o que a regra garante e a mesma UF, e e isso que se confere
reg("migt_mesma_unidade_com_unidade_diferente", mt[tipo == "mesma unidade" & unidade_origem != unidade_destino, .N])
ok("migt: mesma unidade => mesma UF", stopifnot(mt[tipo == "mesma unidade", all(uf_origem == uf_destino)]))
ok("migt: entre municipios da mesma UF => mesma UF, ambos municipais, unidades distintas", stopifnot(mt[tipo == "entre municipios da mesma UF", all(uf_origem == uf_destino & !is.na(municipio_origem) & !is.na(municipio_destino) & unidade_origem != unidade_destino)]))
ok("migt: municipio preenchido sse cargo municipal", stopifnot(all(is.na(mt$municipio_origem) == !mt$cargo_origem %in% c("PREFEITO", "VICE-PREFEITO", "VEREADOR")), all(is.na(mt$municipio_destino) == !mt$cargo_destino %in% c("PREFEITO", "VICE-PREFEITO", "VEREADOR"))))
reg("migt_n_linhas", nrow(mt)); for (t in unique(mt$tipo)) reg(paste0("migt_n_", gsub("[^a-z]+", "_", tolower(t))), mt[tipo == t, .N])

## 4. migracao_territorial_candidaturas ----------------------------------------------------------------
mc <- fread("data/migracao_territorial_candidaturas.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cat("\n[migracao_territorial_candidaturas] linhas:", nrow(mc), "\n")
ok("migc: 7 colunas na ordem canonica", stopifnot(identical(names(mc), c("id_pessoa","ano_candidatura","uf_candidatura","cargo_candidatura","ufs_com_mandato_ate_entao","ultimo_ano_mandato","regiao_candidatura"))))
K_MC <- c("id_pessoa", "ano_candidatura", "uf_candidatura", "cargo_candidatura")
ok("migc: chave (pessoa, ano, UF, cargo) unica", checa_unica(as.data.frame(mc), K_MC))
ok("migc: id_pessoa existe em pessoas.csv e tem mandato", stopifnot(all(mc$id_pessoa %in% pess$id_pessoa), all(mc$id_pessoa %in% mand$id_pessoa)))
ok("migc: ano_candidatura em 1998..2024 e par", { em_faixa(as.integer(mc$ano_candidatura), 1998, 2024, permitir_na = FALSE, nome = "ano_candidatura"); stopifnot(all(as.integer(mc$ano_candidatura) %% 2L == 0L)) })
ok("migc: uf_candidatura em UF ou BR; regiao NA sse BR", { in_set(mc$uf_candidatura, c(UFS, "BR"), permitir_na = FALSE, nome = "uf_candidatura"); in_set(mc$regiao_candidatura, REGIOES, nome = "regiao_candidatura"); stopifnot(all(is.na(mc$regiao_candidatura) == (mc$uf_candidatura == "BR")), all(mc[uf_candidatura != "BR", regiao_candidatura == REG[uf_candidatura]])) })
ok("migc: uf_candidatura fora de ufs_com_mandato_ate_entao", stopifnot(!any(mapply(function(u, l) u %in% strsplit(l, ";", fixed = TRUE)[[1]], mc$uf_candidatura, mc$ufs_com_mandato_ate_entao))))
ok("migc: ultimo_ano_mandato <= ano_candidatura", stopifnot(all(as.integer(mc$ultimo_ano_mandato) <= as.integer(mc$ano_candidatura))))
ult <- mand[, .(id_pessoa, ano = as.integer(ano_eleicao), sg_uf)]
rec <- mc[, {
  h <- ult[id_pessoa == .BY$id_pessoa & ano <= as.integer(.BY$ano_candidatura)]
  .(ufs_rec = paste(sort(unique(h$sg_uf)), collapse = ";"), ult_rec = if (nrow(h)) max(h$ano) else NA_integer_)
}, by = .(id_pessoa, ano_candidatura)]
mc2 <- merge(mc, rec, by = c("id_pessoa", "ano_candidatura"))
ok("migc: ufs_com_mandato_ate_entao e ultimo_ano_mandato recontam de mandatos.csv", stopifnot(nrow(mc2) == nrow(mc), all(mc2$ufs_com_mandato_ate_entao == mc2$ufs_rec), all(as.integer(mc2$ultimo_ano_mandato) == mc2$ult_rec)))
reg("migc_n_linhas", nrow(mc)); reg("migc_n_uf_br", mc[uf_candidatura == "BR", .N])

## 5. relatorio ------------------------------------------------------------------------------------------
reg("n_checks_passaram", length(passou)); reg("n_checks_falharam", length(falhou))
fora <- c("veracidade da autodeclaracao de cor ou raca ao TSE e da data de filiacao (dado administrativo)",
          "pertinencia da equivalencia de siglas (EQUIV em R/30) que define troca_efetiva",
          "duplicata de mandato a montante (mesma pessoa com dois mandatos de vereador na mesma eleicao, pendencia de R/03): aqui aparece como chave repetida, e nao e corrigida")
gravar_relatorio_verificacao(alvo = "data/raca_eleitos.csv + data/migracao_partidaria.csv + data/migracao_territorial.csv + data/migracao_territorial_candidaturas.csv",
                             script = script, passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nverifica_descritivas:", length(passou), "checks ok;", length(falhou), "falharam\n")
if (length(falhou)) { cat(paste("-", falhou), sep = "\n"); sink(); quit(status = 1) }
sink()
