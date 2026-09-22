# 61b_alvos_frentes_assembleias.R — alvos das frentes de coleta da fase 5 (13/09/2026)
# Grava, por UF, data_raw/assembleias3/<UF>/_alvo_mandatos.csv com os mandatos de deputado estadual e distrital, a forma
# atual e a situacao do alvo (sem_forma, forma_a_confirmar, com_forma_da_casa, em_curso), e a lista de suplencia da UF.
# Nao entra na cadeia de reconstrucao. Execucao: cd ~/bocel && Rscript --vanilla R/61b_alvos_frentes_assembleias.R
suppressPackageStartupMessages(library(data.table))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
REF <- as.IDate(readLines("output/data_referencia.txt")[1])
m <- fread("data/mandatos.csv", colClasses="character", na.strings="NA")[cd_cargo %chin% c("7","8")]
p <- fread("data/pessoas.csv", colClasses="character", na.strings="NA", select=c("id_pessoa","nome","nome_urna_recente","dt_nascimento"))
m <- merge(m, p, by="id_pessoa", all.x=TRUE)
m[, encerrado := as.IDate(mandato_fim) < REF]
m[, situacao_alvo := fcase(!encerrado, "em_curso",
  forma_saida == "nao_observado", "sem_forma",
  forma_saida %chin% c("outro","licenca","afastamento") | fonte_forma_saida %chin% c("wikidata","wikipedia","assembleia_inventario","cargo_incompativel"), "forma_a_confirmar",
  default = "com_forma_da_casa")]
sup <- if (file.exists("data/lista_suplencia.csv")) fread("data/lista_suplencia.csv", colClasses="character", na.strings="NA") else NULL
for (u in sort(unique(m$sg_uf))) {
  d <- m[sg_uf == u, .(uf=sg_uf, ano_eleicao, mandato_inicio, mandato_fim, id_mandato, id_pessoa, nome_civil=nome, nome_urna=nome_urna_recente, dt_nascimento, sg_partido, votos=votos_turno_decisivo, forma_saida_atual=forma_saida, fonte_atual=fonte_forma_saida, data_fim_atual=data_fim_efetiva, situacao_alvo)]
  setorder(d, ano_eleicao, -situacao_alvo, nome_urna)
  dir.create(file.path("data_raw/assembleias3", u), showWarnings=FALSE)
  fwrite(d, file.path("data_raw/assembleias3", u, "_alvo_mandatos.csv"))
  if (!is.null(sup)) { s <- sup[sg_uf == u & cd_cargo %chin% c("7","8")]; if (nrow(s)) fwrite(s, file.path("data_raw/assembleias3", u, "_lista_suplencia.csv")) }
}
print(dcast(m[encerrado==TRUE, .N, by=.(sg_uf, situacao_alvo)], sg_uf ~ situacao_alvo, value.var="N", fill=0))
for (s in c("sem_forma", "forma_a_confirmar", "com_forma_da_casa"))
  registrar_numero(paste0("alvo_asm_encerrados_", s), m[encerrado == TRUE & situacao_alvo == s, .N], script = "R/61b_alvos_frentes_assembleias.R")
