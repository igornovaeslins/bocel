# 28_raca.R — cor/raca autodeclarada (DS_COR_RACA, TSE) entre eleitos e candidatos, 2010-2024
# A coluna nao entrou no parquet do 01; este script a le direto dos zips e monta a chave de
# candidatura do 03 (ano, unidade da posicao, cargo, numero, SQ).
# Entrada: data_raw/consulta_cand/consulta_cand_<ANO>.zip, data/mandatos.csv, data/pessoas.csv
# Saida:   data_raw/parquet/raca_<ANO>.parquet, data/raca_eleitos.csv, output/descritivas/raca_*.csv, docs/RACA.md
# Execucao: cd ~/bocel && Rscript --vanilla R/28_raca.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/28_raca.R"
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
tab <- function(d) paste(c(paste0("| ", paste(names(d), collapse = " | "), " |"),
                           paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
                           apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))), collapse = "\n")
chr <- function(d) d[, lapply(.SD, as.character)]
REG <- c(AC="Norte", AM="Norte", AP="Norte", PA="Norte", RO="Norte", RR="Norte", TO="Norte",
         AL="Nordeste", BA="Nordeste", CE="Nordeste", MA="Nordeste", PB="Nordeste", PE="Nordeste",
         PI="Nordeste", RN="Nordeste", SE="Nordeste", DF="Centro-Oeste", GO="Centro-Oeste",
         MS="Centro-Oeste", MT="Centro-Oeste", ES="Sudeste", MG="Sudeste", RJ="Sudeste", SP="Sudeste",
         PR="Sul", RS="Sul", SC="Sul")
ORD <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")
ANOS <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024)
CARGOS <- c("VEREADOR", "PREFEITO", "DEPUTADO ESTADUAL", "DEPUTADO FEDERAL")
unidade_posicao <- function(cd, uf, ue) fcase(cd %in% 11:13, ue, cd %in% 1:2, "BR", default = uf)

## ---------------------------------------------------------------- extracao
for (ano in ANOS) {
  dest <- sprintf("data_raw/parquet/raca_%d.parquet", ano)
  if (file.exists(dest)) next
  zip <- sprintf("data_raw/consulta_cand/consulta_cand_%d.zip", ano)
  tmp <- tempfile(); dir.create(tmp)
  unzip(zip, exdir = tmp)
  csvs <- list.files(tmp, pattern = sprintf("^consulta_cand_%d_[A-Z]+\\.csv$", ano), full.names = TRUE)
  x <- unique(rbindlist(lapply(csvs, function(f) {
    fread(f, sep = ";", encoding = "Latin-1", quote = '"', colClasses = "character", showProgress = FALSE,
          select = c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO", "NR_CANDIDATO",
                     "SQ_CANDIDATO", "DS_COR_RACA", "DS_GRAU_INSTRUCAO", "DS_OCUPACAO", "DS_SIT_TOT_TURNO"))
  }), use.names = TRUE))
  x <- x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, cd := as.integer(CD_CARGO)]
  x[, ue_pos := unidade_posicao(cd, SG_UF, SG_UE)]
  x[, chave := paste(as.integer(ANO_ELEICAO), ue_pos, cd, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
  x <- unique(x[, .(chave, sg_uf = SG_UF, cd_cargo = cd, cor_raca = toupper(DS_COR_RACA),
                    instrucao = DS_GRAU_INSTRUCAO, ocupacao = DS_OCUPACAO)], by = "chave")
  write_parquet(x, dest)
  unlink(tmp, recursive = TRUE)
  cat("extraido", ano, nrow(x), "candidaturas\n")
}
cand <- rbindlist(lapply(sprintf("data_raw/parquet/raca_%d.parquet", ANOS), read_parquet))
cand[, ano := as.integer(sub("^(\\d{4})_.*", "\\1", chave))]
cand[, regiao := factor(REG[sg_uf], levels = ORD)]
# 12/09/2026: o cadastro do TSE grava a ausencia como "#NE#" (com a cerquilha final), que escapava da lista e entrava
# como categoria de cor (R/verifica_descritivas.R)
cand[cor_raca %in% c("", "#NULO#", "#NULO", "#NE", "#NE#", "NÃO INFORMADO", "NAO INFORMADO", "NÃO DIVULGÁVEL"), cor_raca := NA_character_]
cand[, negra := fifelse(cor_raca %in% c("PRETA", "PARDA"), TRUE, fifelse(is.na(cor_raca), NA, FALSE))]

## ---------------------------------------------------------------- eleitos
m <- fread("data/mandatos.csv", na.strings = "NA")
p <- fread("data/pessoas.csv", na.strings = "NA")
m <- merge(m, p[, .(id_pessoa, genero)], by = "id_pessoa", all.x = TRUE)
m[, chave := sub("^M", "", id_mandato)]
el <- merge(m[ano_eleicao %in% ANOS], cand[, .(chave, cor_raca, negra, instrucao, ocupacao)], by = "chave", all.x = TRUE)
el[, regiao := factor(REG[sg_uf], levels = ORD)]
fwrite(el[, .(id_mandato, id_pessoa, ano_eleicao, cargo, sg_uf, regiao, genero, cor_raca, negra, instrucao, ocupacao)],
       "data/raca_eleitos.csv", na = "NA", quote = TRUE)
reg("raca_n_eleitos_2010_2024", nrow(el))
reg("raca_pct_eleitos_sem_cor_declarada", round(100 * el[, mean(is.na(cor_raca))], 2))

## 1. distribuicao entre eleitos por ano
t1 <- dcast(el[!is.na(cor_raca), .N, by = .(ano_eleicao, cor_raca)], ano_eleicao ~ cor_raca, value.var = "N", fill = 0)
tot <- el[!is.na(cor_raca), .N, by = ano_eleicao]
t1 <- merge(t1, tot, by = "ano_eleicao")
t1[, pct_negra := round(100 * (PRETA + PARDA) / N, 1)]
fwrite(t1[order(ano_eleicao)], "output/descritivas/raca_eleitos_ano.csv")

## 2. eleitos negros por regiao e ano
t2 <- el[!is.na(negra) & !is.na(regiao), .(pct_negra = round(100 * mean(negra), 1)), by = .(regiao, ano_eleicao)]
t2w <- dcast(t2, regiao ~ ano_eleicao, value.var = "pct_negra")[order(match(regiao, ORD))]
fwrite(t2w, "output/descritivas/raca_regiao_ano.csv")

## 3. por cargo (2024 municipal, 2022 estadual/federal)
t3 <- el[!is.na(negra) & ((ano_eleicao == 2024 & cargo %in% c("VEREADOR", "PREFEITO", "VICE-PREFEITO")) |
                          (ano_eleicao == 2022 & cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL", "GOVERNADOR", "SENADOR"))),
         .(eleitos = .N, pct_negra = round(100 * mean(negra), 1)), by = .(cargo, ano_eleicao)][order(-eleitos)]
fwrite(t3, "output/descritivas/raca_cargo.csv")

## 4. taxa de exito: candidatos x eleitos, por raca e cargo (vereador e prefeito, 2024)
cand[, eleito := grepl("^ELEITO|MÉDIA|MEDIA", toupper(fifelse(is.na(cor_raca), "", "")))]  # placeholder
ce <- cand[ano %in% c(2020, 2024) & cd_cargo %in% c(11, 13) & !is.na(negra)]
ce[, eleito := chave %in% sub("^M", "", m$id_mandato)]
t4 <- ce[, .(candidatos = .N, eleitos = sum(eleito), taxa = round(100 * mean(eleito), 2)),
         by = .(ano, cargo = fifelse(cd_cargo == 11, "PREFEITO", "VEREADOR"), negra)]
t4[, grupo := fifelse(negra, "negros", "brancos e demais")]
fwrite(dcast(t4, ano + cargo ~ grupo, value.var = c("candidatos", "eleitos", "taxa")), "output/descritivas/raca_taxa_exito.csv")

## 5. raca x genero (2024 vereadores e prefeitos; 2022 deputados)
t5 <- el[!is.na(negra) & genero %in% c("MASCULINO", "FEMININO") &
         ((ano_eleicao == 2024 & cargo %in% c("VEREADOR", "PREFEITO")) | (ano_eleicao == 2022 & cargo %in% c("DEPUTADO ESTADUAL", "DEPUTADO FEDERAL"))),
         .N, by = .(cargo, genero, negra)]
t5[, grupo := paste0(fifelse(genero == "FEMININO", "mulher ", "homem "), fifelse(negra, "negro(a)", "branco(a)"))]
t5w <- dcast(t5, cargo ~ grupo, value.var = "N", fill = 0)
tt <- t5[, .(tot = sum(N)), by = cargo]
t5w <- merge(t5w, tt, by = "cargo")
for (cc in setdiff(names(t5w), c("cargo", "tot"))) t5w[[paste0("pct_", cc)]] <- round(100 * t5w[[cc]] / t5w$tot, 1)
fwrite(t5w, "output/descritivas/raca_genero.csv")

## 6. votacao mediana por raca (vereador 2024, deputado federal 2022)
t6 <- el[!is.na(negra) & !is.na(votos_turno_decisivo) & votos_turno_decisivo > 0 &
         ((ano_eleicao == 2024 & cargo == "VEREADOR") | (ano_eleicao == 2022 & cargo == "DEPUTADO FEDERAL") | (ano_eleicao == 2024 & cargo == "PREFEITO")),
         .(mediana_votos = as.integer(median(votos_turno_decisivo))), by = .(cargo, negra)]
t6[, grupo := fifelse(negra, "negros", "brancos e demais")]
fwrite(dcast(t6, cargo ~ grupo, value.var = "mediana_votos"), "output/descritivas/raca_votos.csv")

## ---- numeros
# 12/09/2026: com o #NE# convertido a NA, 2010 e 2012 ficam sem nenhuma cor declarada e sem linha em t1; o ano sem
# declaracao nao ganha percentual (antes gravava linha vazia no registro), e a lista desses anos fica registrada
anos_sem_cor <- setdiff(ANOS, t1$ano_eleicao)
reg("raca_anos_sem_cor_declarada", if (length(anos_sem_cor)) paste(anos_sem_cor, collapse = ";") else "nenhum")
for (a in intersect(ANOS, t1$ano_eleicao)) reg(paste0("raca_pct_eleitos_negros_", a), t1[ano_eleicao == a, pct_negra])
for (r in ORD) reg(paste0("raca_pct_negros_2024_", tolower(gsub("-", "_", iconv(r, to = "ASCII//TRANSLIT")))), t2[regiao == r & ano_eleicao == 2024, pct_negra])
reg("raca_taxa_exito_vereador_2024_negros", t4[ano == 2024 & cargo == "VEREADOR" & negra == TRUE, taxa])
reg("raca_taxa_exito_vereador_2024_brancos", t4[ano == 2024 & cargo == "VEREADOR" & negra == FALSE, taxa])

md <- c("# Cor e raça entre os eleitos (BOCEL)", "",
  sprintf("Cor ou raça autodeclarada ao TSE no registro de candidatura. A tabela de mandatos cobre as eleições de 2010 a 2024, e nas de %s o cadastro não traz cor declarada, de modo que as tabelas abaixo começam na primeira eleição com declaração. Gerado por `%s` sobre %s mandatos, %s%% sem declaração. 'Negros' soma pretos e pardos, na convenção do IBGE. Números em `output/numeros_assinatura.txt` (chaves `raca_*`).",
          paste(anos_sem_cor, collapse = " e "), script, format(nrow(el), big.mark = ".", decimal.mark = ","), sub(".", ",", sprintf("%.2f", 100 * el[, mean(is.na(cor_raca))]), fixed = TRUE)), "",
  "## Eleitos por cor ou raça, por eleição", "", tab(chr(t1[order(ano_eleicao)])), "",
  "## Eleitos negros por região (%)", "", tab(chr(t2w)), "",
  "## Por cargo (municipal 2024, estadual e federal 2022)", "", tab(chr(t3)), "",
  "## Taxa de êxito eleitoral", "", tab(chr(fread("output/descritivas/raca_taxa_exito.csv"))), "",
  "## Cor e gênero", "", tab(chr(t5w)), "",
  "## Votação mediana do eleito", "", tab(chr(fread("output/descritivas/raca_votos.csv"))))
writeLines(md, "docs/RACA.md")
cat("28_raca: concluido\n")
print(t1[order(ano_eleicao)]); print(t2w); print(t3); print(t4); print(t5w); print(t6)
