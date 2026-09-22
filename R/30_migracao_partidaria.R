# 30_migracao_partidaria.R — trocas de partido ao longo da carreira
# Tres origens, marcadas na coluna fonte:
#   filiacao            — sequencia de filiacoes datadas do TSE (lista atual e listas historicas)
#   mandato_para_mandato — partido de um mandato para o do mandato seguinte da mesma pessoa
#   mandato_para_candidatura — partido do mandato para o da candidatura seguinte (sinais_tse_exercicio)
# Entrada: data/{filiacoes,mandatos,pessoas,sinais_tse_exercicio}.csv
# Saida:   data/migracao_partidaria.csv, output/descritivas/migracao_partidaria_*.csv, docs/MIGRACAO_PARTIDARIA.md
# Execucao: cd ~/bocel && Rscript --vanilla R/30_migracao_partidaria.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/30_migracao_partidaria.R"
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
# siglas que mudaram de nome ou se fundiram: a troca e nominal, nao de legenda
EQUIV <- list(c("PMDB","MDB"), c("PFL","DEM"), c("PPB","PP","PPR"), c("PL","PR","PL"), c("PRB","REPUBLICANOS"),
              c("PPS","CIDADANIA"), c("PTN","PODE","PODEMOS"), c("PATRIOTA","PEN","PRD"), c("PSDC","DC"),
              c("PT do B","AVANTE","PTdoB"), c("PRP","PATRIOTA"), c("PSL","UNIÃO","UNIAO","DEM"), c("PHS","PODE"),
              c("PMN","MOBILIZA"), c("PSC","PODE"), c("PROS","SOLIDARIEDADE"), c("PTC","AGIR"), c("PSDB","PSDB"))
canon <- function(x) {
  y <- toupper(trimws(x))
  for (g in EQUIV) { g <- toupper(g); y[y %in% g] <- g[length(g)] }
  y
}

mand <- fread("data/mandatos.csv", na.strings = "NA")
pes  <- fread("data/pessoas.csv", na.strings = "NA")
mand[, regiao := REG[sg_uf]]

## ---------------------------------------------------------------- A. filiacoes datadas
fil <- fread("data/filiacoes.csv", na.strings = "NA")
fil <- fil[!is.na(data_filiacao) & !is.na(sigla_partido)]
fil[, sigla := toupper(trimws(sigla_partido))]
fil <- unique(fil[, .(id_pessoa, sigla, sg_uf, data_filiacao)], by = c("id_pessoa", "sigla", "data_filiacao"))
setorder(fil, id_pessoa, data_filiacao)
fil[, `:=`(sigla_ant = shift(sigla), data_ant = shift(data_filiacao), uf_ant = shift(sg_uf)), by = id_pessoa]
tr_fil <- fil[!is.na(sigla_ant) & sigla != sigla_ant,
              .(id_pessoa, partido_origem = sigla_ant, partido_destino = sigla,
                data_evento = data_filiacao, data_origem = data_ant, sg_uf = sg_uf, fonte = "filiacao")]

## ---------------------------------------------------------------- B. entre mandatos
mm <- mand[!is.na(sg_partido), .(id_pessoa, ano_eleicao, sg_partido = toupper(sg_partido), cargo, sg_uf, regiao)]
setorder(mm, id_pessoa, ano_eleicao)
mm[, `:=`(part_ant = shift(sg_partido), ano_ant = shift(ano_eleicao), cargo_ant = shift(cargo)), by = id_pessoa]
# 12/09/2026: a transicao entre mandatos exige eleicao posterior. A pessoa com dois mandatos na mesma eleicao
# (pendencia do nucleo, vereador 2000 repetido e vice que tambem aparece eleito vereador) gerava par de ano igual
reg("mig_part_pares_mesma_eleicao_excluidos", mm[!is.na(part_ant) & sg_partido != part_ant & ano_eleicao <= ano_ant, .N])
tr_man <- mm[!is.na(part_ant) & sg_partido != part_ant & ano_eleicao > ano_ant,
             .(id_pessoa, partido_origem = part_ant, partido_destino = sg_partido,
               data_evento = as.IDate(sprintf("%d-10-01", ano_eleicao)), data_origem = as.IDate(sprintf("%d-10-01", ano_ant)),
               sg_uf, fonte = "mandato_para_mandato", ano_origem = ano_ant, ano_destino = ano_eleicao,
               cargo_origem = cargo_ant, cargo_destino = cargo)]

## ---------------------------------------------------------------- C. mandato -> candidatura seguinte
si <- fread("data/sinais_tse_exercicio.csv", na.strings = "NA")
si <- si[candidatura_seguinte == TRUE & mudou_partido == TRUE & !is.na(partido_mandato) & !is.na(partido_candidatura_seguinte)]
si <- merge(si[, .(id_mandato, id_pessoa, ano_eleicao, ano_candidatura_seguinte, cargo,
                   partido_mandato = toupper(partido_mandato), partido_candidatura_seguinte = toupper(partido_candidatura_seguinte))],
            mand[, .(id_mandato, sg_uf, regiao)], by = "id_mandato")
# 12/09/2026: a pessoa com mais de um mandato na mesma eleicao (pendencia de identidade do nucleo) leva cada
# mandato a mesma candidatura seguinte, com cargo de origem diferente e a mesma chave de troca. Sem cargo
# que desempate, a transicao sai da tabela, como no bloco B, e a contagem fica registrada
dois <- mand[, .N, by = .(id_pessoa, ano_eleicao)][N > 1L]
reg("mig_part_pessoa_eleicao_com_mais_de_um_mandato", nrow(dois))
reg("mig_part_pessoa_eleicao_com_mais_de_um_mandato_vice_imputado",
    uniqueN(mand[dois, on = c("id_pessoa", "ano_eleicao")][regra_chapa %in% c("numero_do_titular", "numero_titular_imputado", "sq_coligacao", "composicao_coligacao")],
            by = c("id_pessoa", "ano_eleicao")))
n_si_antes <- nrow(si)
si <- si[!dois, on = c("id_pessoa", "ano_eleicao")]
reg("mig_part_candidatura_seguinte_excluida_por_dois_mandatos", n_si_antes - nrow(si))
tr_can <- si[,.(id_pessoa, partido_origem = partido_mandato, partido_destino = partido_candidatura_seguinte,
                 data_evento = as.IDate(sprintf("%d-08-15", ano_candidatura_seguinte)),
                 data_origem = as.IDate(sprintf("%d-10-01", ano_eleicao)), sg_uf,
                 fonte = "mandato_para_candidatura", ano_origem = ano_eleicao, ano_destino = ano_candidatura_seguinte,
                 cargo_origem = cargo, cargo_destino = cargo)]

mig <- rbindlist(list(tr_fil, tr_man, tr_can), use.names = TRUE, fill = TRUE)
mig[, `:=`(regiao = REG[sg_uf], ano_evento = as.integer(substr(as.character(data_evento), 1, 4)))]
mig[, `:=`(origem_canon = canon(partido_origem), destino_canon = canon(partido_destino))]
mig[, troca_efetiva := origem_canon != destino_canon]   # FALSE = mudanca de nome ou incorporacao
mig[, dias := as.integer(data_evento - data_origem)]
setorder(mig, id_pessoa, data_evento)
mig <- mig[, .(id_pessoa, fonte, partido_origem, partido_destino, origem_canon, destino_canon, troca_efetiva,
               data_origem, data_evento, ano_evento, dias, sg_uf, regiao, ano_origem, ano_destino, cargo_origem, cargo_destino)]
# 12/09/2026: dois mandatos da mesma pessoa na mesma eleicao levam a mesma candidatura seguinte e repetem a linha
n_antes_unique <- nrow(mig)
mig <- unique(mig)
reg("mig_part_linhas_repetidas_removidas", n_antes_unique - nrow(mig))
fwrite(mig, "data/migracao_partidaria.csv", na = "NA", quote = TRUE)

## ---------------------------------------------------------------- descritivas
d1 <- mig[, .(transicoes = .N, efetivas = sum(troca_efetiva), pessoas = uniqueN(id_pessoa)), by = fonte][order(-transicoes)]
fwrite(d1, "output/descritivas/migracao_partidaria_fonte.csv")
d2 <- mig[troca_efetiva == TRUE & fonte == "mandato_para_mandato" & !is.na(regiao),
          .(trocas = .N), by = .(regiao, ano_destino)]
tot <- mm[!is.na(part_ant), .(pares = .N), by = .(regiao = REG[sg_uf], ano_destino = ano_eleicao)]
d2 <- merge(tot, d2, by = c("regiao", "ano_destino"), all.x = TRUE)[is.na(trocas), trocas := 0][, pct := round(100 * trocas / pares, 1)]
fwrite(dcast(d2, regiao ~ ano_destino, value.var = "pct")[order(match(regiao, ORD))], "output/descritivas/migracao_partidaria_regiao_ano.csv")
d3 <- mig[troca_efetiva == TRUE & fonte == "mandato_para_mandato", .(saidas = .N), by = .(partido = origem_canon)]
d4 <- mig[troca_efetiva == TRUE & fonte == "mandato_para_mandato", .(entradas = .N), by = .(partido = destino_canon)]
d34 <- merge(d3, d4, by = "partido", all = TRUE)[is.na(saidas), saidas := 0][is.na(entradas), entradas := 0]
d34[, saldo := entradas - saidas][order(-saldo)]
fwrite(d34[order(-saldo)], "output/descritivas/migracao_partidaria_saldo.csv")
d5 <- mig[troca_efetiva == TRUE & fonte == "mandato_para_mandato", .N, by = .(par = paste(origem_canon, "->", destino_canon))][order(-N)][1:15]
fwrite(d5, "output/descritivas/migracao_partidaria_pares.csv")
np <- mig[troca_efetiva == TRUE, .(trocas = .N), by = id_pessoa]
d6 <- merge(pes[, .(id_pessoa, n_mandatos)], np, by = "id_pessoa", all.x = TRUE)[is.na(trocas), trocas := 0]
d6s <- d6[, .(pessoas = .N, media_trocas = round(mean(trocas), 2), pct_alguma = round(100 * mean(trocas > 0), 1),
              pct_3mais = round(100 * mean(trocas >= 3), 1)), by = .(mandatos = pmin(n_mandatos, 5))][order(mandatos)]
fwrite(d6s, "output/descritivas/migracao_partidaria_por_carreira.csv")

reg("mig_part_transicoes_total", nrow(mig))
reg("mig_part_transicoes_efetivas", mig[troca_efetiva == TRUE, .N])
reg("mig_part_pessoas_com_troca", mig[troca_efetiva == TRUE, uniqueN(id_pessoa)])
reg("mig_part_entre_mandatos_pct", round(100 * mig[fonte == "mandato_para_mandato" & troca_efetiva == TRUE, .N] / mm[!is.na(part_ant), .N], 2))

md <- c("# Migração partidária (BOCEL)", "",
  sprintf("Gerado por `%s`. Cada linha de `data/migracao_partidaria.csv` é uma troca de legenda de uma pessoa, com a origem marcada em `fonte`. A coluna `troca_efetiva` separa a troca real da mudança de nome ou incorporação (PMDB para MDB, PFL para DEM, PSL para UNIÃO e assim por diante). Números em `output/numeros_assinatura.txt` (chaves `mig_part_*`).", script), "",
  "## Volume por origem do registro", "", tab(chr(d1)), "",
  "## Troca entre mandatos consecutivos, por região (%)", "", tab(chr(fread("output/descritivas/migracao_partidaria_regiao_ano.csv"))), "",
  "## Saldo de eleitos entre mandatos (dez maiores e dez menores)", "", tab(chr(rbindlist(list(d34[order(-saldo)][1:10], d34[order(saldo)][1:10])))), "",
  "## Trajetos mais frequentes entre mandatos", "", tab(chr(d5)), "",
  "## Trocas por tamanho de carreira", "", tab(chr(d6s)))
writeLines(md, "docs/MIGRACAO_PARTIDARIA.md")
cat("30_migracao_partidaria: concluido —", nrow(mig), "transicoes\n")
print(d1); print(d34[order(-saldo)][1:8]); print(d5[1:8]); print(d6s)
