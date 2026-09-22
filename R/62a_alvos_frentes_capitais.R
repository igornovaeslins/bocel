# 62a_alvos_frentes_capitais.R — alvos das frentes de coleta da fase 6, capitais (13/09/2026)
# Grava data_raw/capitais/_alvo_prefeitos_vices.csv e, por UF, data_raw/capitais/<UF>/_alvo_vereadores.csv e a lista de
# suplencia da Camara da capital. A capital se identifica pelo codigo da unidade eleitoral de 2020, estavel entre eleicoes
# (o nome muda de grafia com e sem acento). Nao entra na cadeia. Execucao: cd ~/bocel && Rscript --vanilla R/62a_alvos_frentes_capitais.R
suppressPackageStartupMessages(library(data.table))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
REF <- as.IDate(readLines("output/data_referencia.txt")[1])
cap <- c(AC="RIO BRANCO",AL="MACEIÓ",AM="MANAUS",AP="MACAPÁ",BA="SALVADOR",CE="FORTALEZA",ES="VITÓRIA",GO="GOIÂNIA",MA="SÃO LUÍS",MG="BELO HORIZONTE",MS="CAMPO GRANDE",MT="CUIABÁ",PA="BELÉM",PB="JOÃO PESSOA",PE="RECIFE",PI="TERESINA",PR="CURITIBA",RJ="RIO DE JANEIRO",RN="NATAL",RO="PORTO VELHO",RR="BOA VISTA",RS="PORTO ALEGRE",SC="FLORIANÓPOLIS",SE="ARACAJU",SP="SÃO PAULO",TO="PALMAS")
m <- fread("data/mandatos.csv", colClasses="character", na.strings="NA")[cd_cargo %chin% c("11","12","13")]
ues <- unique(m[cd_cargo=="11" & ano_eleicao=="2020" & nm_ue == cap[sg_uf], .(sg_uf, sg_ue, capital = nm_ue)])
stopifnot(nrow(ues) == 26L)
m <- merge(m, ues, by=c("sg_uf","sg_ue"))
p <- fread("data/pessoas.csv", colClasses="character", na.strings="NA", select=c("id_pessoa","nome","nome_urna_recente","dt_nascimento"))
m <- merge(m, p, by="id_pessoa", all.x=TRUE)
m[, encerrado := as.IDate(mandato_fim) < REF]
m[, situacao_alvo := fcase(!encerrado, "em_curso",
  forma_saida == "nao_observado", "sem_forma",
  forma_saida %chin% c("outro","licenca","afastamento","substituicao_inferida_munic") | fonte_forma_saida %chin% c("wikidata","wikipedia","cargo_incompativel","ibge_munic","derivado_titular"), "forma_a_confirmar",
  default = "com_forma_de_fonte_institucional")]
sel <- function(d) d[, .(uf=sg_uf, capital, sg_ue, ano_eleicao, cargo, mandato_inicio, mandato_fim, id_mandato, id_pessoa, nome_civil=nome, nome_urna=nome_urna_recente, dt_nascimento, sg_partido, votos=votos_turno_decisivo, forma_saida_atual=forma_saida, fonte_atual=fonte_forma_saida, data_fim_atual=data_fim_efetiva, situacao_alvo)]
ex <- sel(m[cd_cargo %chin% c("11","12")]); setorder(ex, uf, ano_eleicao, cargo)
fwrite(ex, "data_raw/capitais/_alvo_prefeitos_vices.csv")
sup <- fread("data/lista_suplencia.csv", colClasses="character", na.strings="NA")
for (u in ues$sg_uf) {
  dir.create(file.path("data_raw/capitais", u), showWarnings=FALSE)
  v <- sel(m[cd_cargo=="13" & sg_uf==u]); setorder(v, ano_eleicao, -situacao_alvo, nome_urna)
  fwrite(v, file.path("data_raw/capitais", u, "_alvo_vereadores.csv"))
  s <- sup[cd_cargo=="13" & sg_ue == ues[sg_uf==u, sg_ue]]
  if (nrow(s)) fwrite(s, file.path("data_raw/capitais", u, "_lista_suplencia.csv"))
}
print(dcast(m[encerrado==TRUE, .N, by=.(cargo, situacao_alvo)], cargo ~ situacao_alvo, value.var="N", fill=0))
print(dcast(m[encerrado==TRUE & cd_cargo=="13", .N, by=.(sg_uf, situacao_alvo)], sg_uf ~ situacao_alvo, value.var="N", fill=0))
for (cg in c("11", "12", "13")) for (s in c("sem_forma", "forma_a_confirmar", "com_forma_de_fonte_institucional"))
  registrar_numero(sprintf("alvo_cap_cargo%s_encerrados_%s", cg, s), m[cd_cargo == cg & encerrado == TRUE & situacao_alvo == s, .N], script = "R/62a_alvos_frentes_capitais.R")
