# 31_migracao_territorial.R — mudanca de territorio na carreira eletiva
# Cada linha e um deslocamento entre mandatos consecutivos da mesma pessoa, com o tipo
# (mesmo municipio, entre municipios da mesma UF, entre UFs, entre regioes).
# Entrada: data/mandatos.csv, data/pessoas.csv, data/municipios_tse_ibge.csv
# Saida:   data/migracao_territorial.csv, output/descritivas/migracao_territorial_*.csv, docs/MIGRACAO_TERRITORIAL.md
# Execucao: cd ~/bocel && Rscript --vanilla R/31_migracao_territorial.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/31_migracao_territorial.R"
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
m <- fread("data/mandatos.csv", na.strings = "NA")
tse <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
m[, sg_ue_c := as.character(sg_ue)]
m <- merge(m, tse[, .(sg_ue, nome_ibge)], by.x = "sg_ue_c", by.y = "sg_ue", all.x = TRUE)
m[, `:=`(regiao = REG[sg_uf],
         municipio = fifelse(esfera == "municipal", fcoalesce(nome_ibge, nm_ue), NA_character_))]
setorder(m, id_pessoa, ano_eleicao, cargo)
m[, `:=`(uf_ant = shift(sg_uf), un_ant = shift(unidade_posicao), mun_ant = shift(municipio),
         ano_ant = shift(ano_eleicao), cargo_ant = shift(cargo), esf_ant = shift(esfera)), by = id_pessoa]
# 12/09/2026: a migracao exige eleicao posterior; dois mandatos da mesma pessoa na mesma eleicao (pendencia do nucleo)
# geravam par de ano igual e linha repetida
mv <- m[!is.na(un_ant) & ano_eleicao > ano_ant]
mv[, tipo := fcase(
  sg_uf != uf_ant & REG[sg_uf] != REG[uf_ant], "entre regioes",
  sg_uf != uf_ant, "entre UFs da mesma regiao",
  esfera == "municipal" & esf_ant == "municipal" & unidade_posicao != un_ant, "entre municipios da mesma UF",
  default = "mesma unidade")]
mig <- mv[, .(id_pessoa, ano_origem = ano_ant, ano_destino = ano_eleicao,
              cargo_origem = cargo_ant, cargo_destino = cargo,
              uf_origem = uf_ant, uf_destino = sg_uf,
              regiao_origem = REG[uf_ant], regiao_destino = REG[sg_uf],
              unidade_origem = un_ant, unidade_destino = unidade_posicao,
              municipio_origem = mun_ant, municipio_destino = municipio, tipo)]
fwrite(mig, "data/migracao_territorial.csv", na = "NA", quote = TRUE)

## descritivas
d1 <- mig[, .(transicoes = .N, pessoas = uniqueN(id_pessoa)), by = tipo][order(-transicoes)]
d1[, pct := round(100 * transicoes / sum(transicoes), 2)]
fwrite(d1, "output/descritivas/migracao_territorial_tipo.csv")
mud <- mig[tipo != "mesma unidade"]
d2 <- mud[, .(mudancas = .N), by = .(regiao_origem)]
base <- mig[, .(transicoes = .N), by = .(regiao_origem)]
d2 <- merge(base, d2, by = "regiao_origem", all.x = TRUE)[is.na(mudancas), mudancas := 0][, pct := round(100 * mudancas / transicoes, 1)]
fwrite(d2[order(match(regiao_origem, ORD))], "output/descritivas/migracao_territorial_regiao.csv")
d3 <- mud[uf_origem != uf_destino, .N, by = .(par = paste(uf_origem, "->", uf_destino))][order(-N)][1:15]
fwrite(d3, "output/descritivas/migracao_territorial_pares_uf.csv")
d4 <- mud[, .N, by = .(cargo_origem, cargo_destino)][order(-N)][1:12]
fwrite(d4, "output/descritivas/migracao_territorial_cargos.csv")
# saldo por UF: entradas menos saidas em mudancas entre UFs
ent <- mud[uf_origem != uf_destino, .N, by = .(uf = uf_destino)]; setnames(ent, "N", "entradas")
sai <- mud[uf_origem != uf_destino, .N, by = .(uf = uf_origem)];  setnames(sai, "N", "saidas")
d5 <- merge(ent, sai, by = "uf", all = TRUE)[is.na(entradas), entradas := 0][is.na(saidas), saidas := 0][, saldo := entradas - saidas]
fwrite(d5[order(-saldo)], "output/descritivas/migracao_territorial_saldo_uf.csv")
# pessoas que ocuparam cargo em mais de uma UF
pu <- m[, .(ufs = uniqueN(sg_uf), municipios = uniqueN(unidade_posicao[esfera == "municipal"])), by = id_pessoa]
d6 <- pu[, .(pessoas = .N, pct_2ufs = round(100 * mean(ufs >= 2), 2), pct_2municipios = round(100 * mean(municipios >= 2), 2))]
fwrite(d6, "output/descritivas/migracao_territorial_pessoas.csv")

reg("mig_terr_transicoes", nrow(mig))
reg("mig_terr_mudancas", nrow(mud))
reg("mig_terr_pct_mudanca", round(100 * nrow(mud) / nrow(mig), 2))
reg("mig_terr_entre_ufs", mud[uf_origem != uf_destino, .N])
reg("mig_terr_pessoas_2ufs", pu[ufs >= 2, .N])
reg("mig_terr_pessoas_2municipios", pu[municipios >= 2, .N])


## ---------------------------------------------------------------- candidaturas em outra UF
# A mobilidade de eleitos e rara; a tentativa aparece nas candidaturas. Liga-se a pessoa do BOCEL
# pelo titulo de eleitor e, na falta dele, pelo CPF, e marca a candidatura em UF diferente de
# todas as UFs em que a pessoa ja tinha mandato ate aquele ano.
suppressPackageStartupMessages(library(arrow))
pes <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
num <- function(x) gsub("\\D", "", x)
# 21/09/2026: a separacao de pessoas fundidas por documento (R/03) deixa o titulo ou o CPF digitado errado
# no cadastro do TSE em mais de um id_pessoa. Esse documento nao serve de ponte, e a candidatura de titulo
# repetido segue pelo CPF quando ele e unico
tit_rep <- pes[!is.na(nr_titulo_eleitoral), .N, by = nr_titulo_eleitoral][N > 1L, nr_titulo_eleitoral]
cpf_rep <- pes[!is.na(nr_cpf), .N, by = nr_cpf][N > 1L, nr_cpf]
reg("mig_terr_titulos_repetidos_fora_da_ponte", length(tit_rep))
reg("mig_terr_cpfs_repetidos_fora_da_ponte", length(cpf_rep))
map_tit <- pes[!is.na(nr_titulo_eleitoral) & !nr_titulo_eleitoral %chin% tit_rep, .(chave = nr_titulo_eleitoral, id_pessoa)]
map_cpf <- pes[!is.na(nr_cpf) & !nr_cpf %chin% cpf_rep, .(chave = nr_cpf, id_pessoa)]
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_[0-9]{4}[.]parquet$", full.names = TRUE), function(f) {
  x <- as.data.table(read_parquet(f, col_select = c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "SG_UF", "CD_CARGO",
                                                    "NR_TITULO_ELEITORAL_CANDIDATO", "NR_CPF_CANDIDATO", "DS_CARGO")))
  x <- x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano = as.integer(ANO_ELEICAO), sg_uf = SG_UF, cargo = toupper(DS_CARGO),
        titulo = num(NR_TITULO_ELEITORAL_CANDIDATO), cpf = num(NR_CPF_CANDIDATO))]
}))
cand[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
cand[nchar(titulo) > 0 & !is.na(titulo), titulo := formatC(titulo, width = 12, flag = "0")]
cand[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
c1 <- merge(cand[!is.na(titulo)], map_tit, by.x = "titulo", by.y = "chave")
c2 <- merge(cand[(is.na(titulo) | titulo %chin% tit_rep) & !is.na(cpf)], map_cpf, by.x = "cpf", by.y = "chave")
cid <- unique(rbindlist(list(c1, c2), use.names = TRUE, fill = TRUE)[, .(id_pessoa, ano, sg_uf, cargo)])
# UFs em que a pessoa ja teve mandato, por ano
mu <- m[, .(id_pessoa, ano_eleicao, sg_uf)]
setorder(mu, id_pessoa, ano_eleicao)
cc <- merge(cid, mu, by = "id_pessoa", allow.cartesian = TRUE, suffixes = c("", "_mand"))
cc <- cc[ano_eleicao <= ano]
ja <- cc[, .(ufs_anteriores = paste(sort(unique(sg_uf_mand)), collapse = ";"),
             ultimo_ano_mandato = max(ano_eleicao)), by = .(id_pessoa, ano, sg_uf, cargo)]
ja[, fora := !mapply(function(u, l) u %in% strsplit(l, ";")[[1]], sg_uf, ufs_anteriores)]
cand_out <- ja[fora == TRUE, .(id_pessoa, ano_candidatura = ano, uf_candidatura = sg_uf, cargo_candidatura = cargo,
                               ufs_com_mandato_ate_entao = ufs_anteriores, ultimo_ano_mandato,
                               regiao_candidatura = REG[sg_uf])]
fwrite(cand_out, "data/migracao_territorial_candidaturas.csv", na = "NA", quote = TRUE)
d7 <- cand_out[, .(candidaturas = .N, pessoas = uniqueN(id_pessoa)), by = .(ano_candidatura)][order(ano_candidatura)]
fwrite(d7, "output/descritivas/migracao_territorial_candidaturas_ano.csv")
d8 <- cand_out[, .N, by = .(par = paste(sub(";.*", "", ufs_com_mandato_ate_entao), "->", uf_candidatura))][order(-N)][1:15]
fwrite(d8, "output/descritivas/migracao_territorial_candidaturas_pares.csv")
d9 <- cand_out[, .N, by = cargo_candidatura][order(-N)][1:8]
fwrite(d9, "output/descritivas/migracao_territorial_candidaturas_cargo.csv")
reg("mig_terr_candidaturas_fora_da_uf", nrow(cand_out))
reg("mig_terr_pessoas_candidatas_fora_da_uf", cand_out[, uniqueN(id_pessoa)])
cat("candidaturas em UF nova:", nrow(cand_out), "| pessoas:", cand_out[, uniqueN(id_pessoa)], "\n")
print(d7); print(d8[1:8]); print(d9)

md <- c("# Migração territorial de políticos (BOCEL)", "",
  sprintf("Gerado por `%s`. Cada linha de `data/migracao_territorial.csv` liga dois mandatos consecutivos da mesma pessoa e classifica o deslocamento. Números em `output/numeros_assinatura.txt` (chaves `mig_terr_*`).", script), "",
  "## Tipos de transição", "", tab(chr(d1)), "",
  "## Mudança de território por região de origem (%)", "", tab(chr(d2[order(match(regiao_origem, ORD))])), "",
  "## Fluxos entre UFs mais frequentes", "", tab(chr(d3)), "",
  "## Cargos na mudança de território", "", tab(chr(d4)), "",
  "## Saldo migratório por UF (entradas menos saídas)", "",
  tab(chr(rbindlist(list(d5[order(-saldo)][1:8], d5[order(saldo)][1:8])))), "",
  "## Pessoas com carreira em mais de um território", "", tab(chr(d6)), "",
  "## Candidaturas em UF onde a pessoa nunca teve mandato", "", tab(chr(d7)), "",
  "### Trajetos mais frequentes", "", tab(chr(d8)), "",
  "### Cargo disputado fora", "", tab(chr(d9)))
writeLines(md, "docs/MIGRACAO_TERRITORIAL.md")
cat("31_migracao_territorial: concluido —", nrow(mig), "transicoes,", nrow(mud), "mudancas\n")
print(d1); print(d2[order(match(regiao_origem, ORD))]); print(d3[1:8]); print(d4[1:8]); print(d5[order(-saldo)][1:6]); print(d6)
