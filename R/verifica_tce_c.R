# verifica_tce_c.R — verificacao independente de data/tce_gestores_c.csv e data/tce_gestores_c_cobertura.csv
# (frente dos Tribunais de Contas, grupo C: MG, PR, GO, MT, DF, TO). Nao reescreve nada: le as saidas de
# R/33_tce_gestores_c.R, o cache bruto em data_raw/tce/go/ e a evidencia das casas sem fonte em
# data_raw/tce/<uf>/probe_<uf>.json, confere as invariantes e grava o relatorio em output/verificacao/.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce_c.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_tce_c.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce_c.log", open = "wt"); sink(logf, split = TRUE)
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
UF_C <- c("MG", "PR", "GO", "MT", "DF", "TO")
passou <- character(); falhou <- character(); fora <- character()
ck <- function(nome, ok, detalhe = "") {
  if (isTRUE(ok)) passou <<- c(passou, nome)
  else falhou <<- c(falhou, paste0(nome, if (nzchar(detalhe)) paste0(" — ", detalhe) else ""))
  cat(if (isTRUE(ok)) "OK   " else "FALHA", nome, detalhe, "\n")
}

o <- fread("data/tce_gestores_c.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/tce_gestores_c_cobertura.csv")
inv <- fread("data_raw/tce/inventario_tce_c.csv", colClasses = "character")
COLS <- names(fread("data/tce_gestores.csv", nrows = 0))

## 1-3. contrato de colunas e vocabularios
ck("01 colunas identicas as de data/tce_gestores.csv, na mesma ordem", identical(names(o), COLS),
   paste(setdiff(COLS, names(o)), collapse = ","))
ck("02 forma_saida no vocabulario fechado", all(o$forma_saida %in% VOCAB),
   paste(setdiff(unique(o$forma_saida), VOCAB), collapse = ","))
ck("03 cargo_bocel em PREFEITO/VEREADOR ou NA", all(is.na(o$cargo_bocel) | o$cargo_bocel %in% c("PREFEITO", "VEREADOR")))
ck("04 tipo_unidade em prefeitura/camara/outra", all(o$tipo_unidade %in% c("prefeitura", "camara", "outra")))

## 5-7. municipio
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
ck("05 sg_ue sempre preenchido", o[is.na(sg_ue), .N] == 0L, paste(o[is.na(sg_ue), .N], "linhas"))
ck("06 par sg_ue/ibge existe no mapa TSE-IBGE",
   nrow(fsetdiff(unique(o[, .(sg_ue, id_municipio_ibge)]), unique(mun[, .(sg_ue, id_municipio_ibge)]))) == 0L)
ck("07 UF da linha bate com a UF do municipio",
   nrow(merge(unique(o[, .(sg_ue, uf)]), unique(mun[, .(sg_ue, sg_uf)]), by = "sg_ue")[uf != sg_uf]) == 0L)

## 8-10. exercicio e eleicao de referencia
CICLO <- seq(1996L, 2024L, 4L)
d <- copy(o)[, `:=`(ex = as.integer(exercicio), ae = as.integer(ano_eleicao))]
ck("08 exercicio sempre presente", d[is.na(ex), .N] == 0L)
ck("09 ano_eleicao e um ano de eleicao municipal", d[!is.na(ae) & !(ae %in% CICLO), .N] == 0L)
ck("10 ano_eleicao deriva do exercicio pela regra da posse em 1o de janeiro",
   d[!is.na(ae) & ae != ((ex - 1L) %/% 4L) * 4L, .N] == 0L)

## 11-16. pareamento
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13"),
          .(id_mandato, id_pessoa, sg_ue_bocel = unidade_posicao, ano_eleicao_bocel = ano_eleicao, cargo_ref = cargo)]
p <- merge(o[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
ck("11 todo id_mandato_bocel existe em data/mandatos.csv", p[is.na(sg_ue_bocel), .N] == 0L, paste(p[is.na(sg_ue_bocel), .N], "orfaos"))
ck("12 municipio do pareamento bate", p[!is.na(sg_ue_bocel) & sg_ue != sg_ue_bocel, .N] == 0L)
ck("13 cargo do pareamento bate", p[!is.na(cargo_ref) & cargo_bocel != cargo_ref, .N] == 0L)
ck("14 id_pessoa do pareamento bate", p[!is.na(id_pessoa) & id_pessoa_bocel != id_pessoa, .N] == 0L)
ck("15 eleicao do pareamento bate", p[!is.na(ano_eleicao) & !is.na(ano_eleicao_bocel) & ano_eleicao != ano_eleicao_bocel, .N] == 0L)
ck("16 metodo_pareamento presente sempre que ha mandato", o[!is.na(id_mandato_bocel) & is.na(metodo_pareamento), .N] == 0L)
ck("17 nenhuma linha sem cargo_bocel foi pareada", o[is.na(cargo_bocel) & !is.na(id_mandato_bocel), .N] == 0L)

## 18-21. forma de saida: as fontes do grupo C observam quem respondia, nao como o mandato terminou
ck("18 unica forma de saida declarada e 'outro', e sempre por substituicao pelo vice",
   o[forma_saida != "nao_observado" & (forma_saida != "outro" | is.na(id_mandato_titular_substituido)), .N] == 0L)
ck("19 todo titular substituido existe em data/mandatos.csv",
   o[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)] ==
     length(intersect(unique(na.omit(o$id_mandato_titular_substituido)), mand$id_mandato)))
ck("20 substituicao pelo vice so em linha de prefeitura",
   o[!is.na(id_mandato_titular_substituido) & tipo_unidade != "prefeitura", .N] == 0L)
ck("21 relacao_chapa_eleita coerente com a substituicao",
   o[!is.na(id_mandato_titular_substituido) & relacao_chapa_eleita != "vice_eleito", .N] == 0L)

## 22-25. recontagem independente do bruto de GO
pdfc <- fread("data_raw/tce/go/contas_julgadas_camaras.csv", colClasses = "character")
csvc <- fread("data_raw/tce/go/contas_irregulares.csv", colClasses = "character")
ck("22 linhas do PDF das camaras na saida nao excedem o bruto",
   o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras", .N] <= nrow(pdfc),
   paste(o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras", .N], "de", nrow(pdfc)))
ck("23 linhas do rol da Ficha Limpa na saida nao excedem o bruto",
   o[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa", .N] <= nrow(csvc),
   paste(o[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa", .N], "de", nrow(csvc)))
ck("24 nenhum prefeito atribuido a linha das listas 2 e 3 do rol, que excluem prefeitos por definicao",
   o[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa" & cargo_bocel == "PREFEITO" &
       !grepl("Contas de Prefeitos e Ex-Prefeitos", cargo_fonte), .N] == 0L)
ck("25 no PDF so as contas de governo (balanco/balancete) viram PREFEITO",
   o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & cargo_bocel == "PREFEITO" &
       !grepl("\\(BALANC", cargo_fonte), .N] == 0L)

## 26-27. cobertura
ck("26 cobertura nunca passa de 1", cob[taxa > 1, .N] == 0L)
ck("27 cobertura reproduz a contagem de mandatos pareados",
   cob[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
   paste(cob[, sum(n_pareados)], "vs", o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]))

## 28-31. inventario e evidencia das casas sem fonte
ck("28 inventario cobre as 6 UFs do grupo C", setequal(unique(inv$uf), UF_C), paste(setdiff(UF_C, inv$uf), collapse = ","))
ck("29 inventario tem url, campos e observacao em toda linha",
   inv[is.na(url) | url == "" | is.na(campos) | campos == "" | is.na(observacao) | observacao == "", .N] == 0L)
ck("30 toda UF sem fonte tem a evidencia da recusa gravada",
   all(vapply(setdiff(UF_C, unique(o$uf)), function(u)
     file.exists(sprintf("data_raw/tce/%s/probe_%s.json", tolower(u), tolower(u))), TRUE)))
ck("31 so entram na saida UFs marcadas como viaveis no inventario",
   all(unique(o$uf) %in% inv[viavel %in% c("sim", "parcial"), uf]))
pmg <- fromJSON("data_raw/tce/mg/probe_mg.json", simplifyVector = FALSE)
st_mg <- unlist(lapply(pmg, function(z) if (grepl("apimoci", z$url)) z$status else NULL))
ck("32 a API do TCE-MG que serve as planilhas do TRE responde 401 sem sessao de captcha",
   length(st_mg) > 0 && all(st_mg == 401), paste(st_mg, collapse = ","))

fora <- c("pertinencia semantica do pareamento por nome sem CPF: em municipio pequeno o homonimo nao e distinguivel",
          "atribuicao de cargo no rol da Ficha Limpa: as listas 2 e 3 do TCM-GO nao nomeiam a unidade gestora quando ela e a propria prefeitura, e por isso essas linhas ficam sem cargo eletivo em vez de virarem prefeito",
          "completude do cadastro do TCM-GO: a relacao de contas julgadas pelas camaras cobre os processos ja julgados, e o exercicio ainda nao julgado nao e distinguivel do exercicio sem prestacao",
          "leitura substantiva de 'outro' quando quem responde pela prefeitura e o vice eleito: e evidencia de que o titular saiu, nao prova da forma da saida",
          "a resolucao de municipio por distancia de edicao e por tokens contidos supoe que a grafia do TCM-GO e uma variante do nome do IBGE, e nao um municipio distinto",
          "as casas sem fonte (MG, PR, MT, TO) podem ter cadastro atras de autenticacao ou por LAI; a camada so atesta que nao ha ponto de acesso publico e roteirizavel")
registrar_numero("tcec_verif_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tcec_verif_n_aprovadas", length(passou), script = script)
registrar_numero("tcec_verif_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao("data/tce_gestores_c.csv + data/tce_gestores_c_cobertura.csv",
                                  "R/33_tce_gestores_c.R", passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nrelatorio:", f, "\n", length(passou), "aprovadas,", length(falhou), "reprovadas\n")
if (length(falhou)) { sink(); stop("verificacao reprovada: ", paste(falhou, collapse = " | ")) }
sink()
