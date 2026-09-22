# 29_interrupcao.R — geografia da interrupcao de mandato, com medidas de cobertura nacional
# Entrada: data/mandatos.csv, data/eleicoes_suplementares.csv, data/munic_prefeitos.csv,
#          data/datajud_sinal_unidade_eleicao.csv, data/municipios_tse_ibge.csv
# Saida:   output/descritivas/interrupcao_*.csv e docs/INTERRUPCAO.md
# Execucao: cd ~/bocel && Rscript --vanilla R/29_interrupcao.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/29_interrupcao.R"
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
m[, regiao := factor(REG[sg_uf], levels = ORD)]

## ---- A. eleicoes suplementares: cobertura nacional (o TSE registra todas)
sup <- fread("data/eleicoes_suplementares.csv", na.strings = "NA")
sup[, regiao := factor(REG[sg_uf], levels = ORD)]
supm <- sup[cd_cargo == 11]                       # prefeito
base <- m[cargo == "PREFEITO", .(mandatos = .N), by = .(sg_uf, regiao)]
a_uf <- merge(supm[, .(pleitos = .N), by = sg_uf], base, by = "sg_uf", all.y = TRUE)
a_uf[is.na(pleitos), pleitos := 0]
a_uf[, por_mil := round(1000 * pleitos / mandatos, 1)]
setorder(a_uf, -por_mil)
fwrite(a_uf, "output/descritivas/interrupcao_suplementares_uf.csv")
a_reg <- a_uf[, .(pleitos = sum(pleitos), mandatos = sum(mandatos)), by = regiao][, por_mil := round(1000 * pleitos / mandatos, 1)][order(match(regiao, ORD))]
fwrite(a_reg, "output/descritivas/interrupcao_suplementares_regiao.csv")
a_ano <- supm[, .N, by = .(ano_arquivo)][order(ano_arquivo)]
fwrite(a_ano, "output/descritivas/interrupcao_suplementares_ano.csv")

## ---- B. MUNIC: prefeito em exercicio diferente do eleito, nas edicoes com nome (2004 e 2005)
mu <- fread("data/munic_prefeitos.csv", na.strings = "NA")
mu <- mu[ano_munic %in% c(2004, 2005) & !is.na(nome_prefeito_munic) & !is.na(id_mandato_bocel)]
mu <- merge(mu, m[, .(id_mandato_bocel = id_mandato, sg_uf, regiao)], by = "id_mandato_bocel")
b_uf <- mu[, .(municipios = .N, outro = sum(status == "outro_em_exercicio"),
               pct = round(100 * mean(status == "outro_em_exercicio"), 1)), by = .(sg_uf, regiao)][order(-pct)]
fwrite(b_uf, "output/descritivas/interrupcao_munic_uf.csv")
b_reg <- mu[, .(municipios = .N, outro = sum(status == "outro_em_exercicio"),
                pct = round(100 * mean(status == "outro_em_exercicio"), 1)), by = regiao][order(match(regiao, ORD))]
fwrite(b_reg, "output/descritivas/interrupcao_munic_regiao.csv")

## ---- C. dentro do observado: saidas nao regulares entre os mandatos com forma observada
obs <- m[forma_saida != "nao_observado"]
NAOREG <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
            "perda_do_mandato_inferida_por_eleicao_suplementar", "substituicao_inferida_munic", "aposentadoria", "impeachment", "retotalizacao",
            "assumiu_titular", "nao_tomou_posse", "suplente_efetivado")
c_reg <- obs[!is.na(regiao), .(observados = .N, nao_regular = sum(forma_saida %in% NAOREG),
                               pct = round(100 * mean(forma_saida %in% NAOREG), 1)), by = regiao][order(match(regiao, ORD))]
fwrite(c_reg, "output/descritivas/interrupcao_condicional_regiao.csv")

## ---- D. DataJud: procedencia em acao de cassacao por mil mandatos municipais
dj <- fread("data/datajud_sinal_unidade_eleicao.csv", na.strings = "NA")
dj[, regiao := factor(REG[uf], levels = ORD)]
d_uf <- dj[!is.na(regiao), .(processos = sum(n_processos), com_indicio = sum(n_com_indicio_cassacao)), by = .(uf, regiao)]
base_uf <- m[esfera == "municipal", .(mandatos = .N), by = .(uf = sg_uf)]
d_uf <- merge(d_uf, base_uf, by = "uf", all.x = TRUE)
d_uf[, indicio_por_mil := round(1000 * com_indicio / mandatos, 2)]
setorder(d_uf, -indicio_por_mil)
fwrite(d_uf, "output/descritivas/interrupcao_datajud_uf.csv")

## ---- numeros
for (r in ORD) {
  k <- tolower(gsub("-", "_", iconv(r, to = "ASCII//TRANSLIT")))
  reg(paste0("int_suplementares_por_mil_", k), a_reg[regiao == r, por_mil])
  reg(paste0("int_munic_pct_outro_", k), b_reg[regiao == r, pct])
}
reg("int_suplementares_total_prefeito", nrow(supm))
reg("int_munic_pct_outro_brasil", round(100 * mu[, mean(status == "outro_em_exercicio")], 2))
reg("int_uf_maior_suplementares", a_uf[1, por_mil]); reg("int_uf_menor_suplementares", a_uf[.N, por_mil])

md <- c("# Onde o mandato é interrompido (BOCEL)", "",
  sprintf("Gerado por `%s`. Três medidas independentes, porque a camada de forma de saída ainda tem cobertura desigual entre regiões. Números em `output/numeros_assinatura.txt` (chaves `int_*`).", script), "",
  "## 1. Eleições suplementares para prefeito, por mil mandatos", "",
  "Esta é a medida de cobertura nacional, porque o TSE registra todo pleito suplementar, e ele só ocorre quando a chapa eleita perde o mandato.", "",
  "### Por região", "", tab(chr(a_reg)), "",
  "### Dez UFs com a maior taxa", "", tab(chr(a_uf[1:10])), "",
  "### Dez UFs com a menor taxa", "", tab(chr(a_uf[(.N-9):.N])), "",
  "### Por eleição de origem", "", tab(chr(a_ano)), "",
  "## 2. MUNIC 2004 e 2005: prefeito em exercício diferente do eleito", "",
  "As duas edições que trazem o nome do prefeito cobrem quase todos os municípios, e por isso medem diretamente a substituição no meio do mandato.", "",
  "### Por região", "", tab(chr(b_reg)), "",
  "### Dez UFs com a maior taxa", "", tab(chr(b_uf[1:10])), "",
  "## 3. Entre os mandatos com saída observada, quantos saíram fora do fim regular", "",
  "Esta medida depende da cobertura da camada, então compara composição entre regiões, e o nível fica fora de leitura.", "", tab(chr(c_reg)), "",
  "## 4. DataJud: indício de procedência em ação de cassação, por mil mandatos municipais", "",
  "A API do CNJ não expõe as partes, e por isso o indicador é do município do órgão julgador, sem atribuição a mandato.", "",
  tab(chr(d_uf[1:12])))
writeLines(md, "docs/INTERRUPCAO.md")
cat("29_interrupcao: concluido\n")
print(a_reg); print(a_uf[1:8]); print(b_reg); print(b_uf[1:8]); print(c_reg); print(d_uf[1:8]); print(a_ano)
