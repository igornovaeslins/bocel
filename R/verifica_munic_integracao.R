# verifica_munic_integracao.R — verificacao cetica da frente MUNIC e da sua integracao em mandatos.csv
# Reconta a partir dos arquivos: chave unica de munic_prefeitos.csv, correspondencia TSE-IBGE,
# amostra de 20 'outro_em_exercicio' nas edicoes com nome (2004, 2005), recontagem por edicao contra
# output/numeros_assinatura.txt e contagem de 'substituicao_inferida_munic' em mandatos.csv.
# Execucao: Rscript --vanilla R/verifica_munic_integracao.R   (a partir da raiz do repositorio)
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(stringdist) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
logf <- "logs/verifica_munic_integracao.log"; sink(logf, split = TRUE)
cat("verifica_munic_integracao:", format(Sys.time()), "\n")
script <- "R/verifica_munic_integracao.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")
falhas <- character(); passou <- character()
chk <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else { falhas <<- c(falhas, msg); cat("FALHA:", msg, "\n") } }
norm_nome <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }

out  <- fread("data/munic_prefeitos.csv", colClasses = list(character = c("id_municipio_ibge", "id_municipio_ibge6", "sg_ue")), na.strings = "NA")
corr <- fread("data/municipios_tse_ibge.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pes  <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")

## ---------------------------------------------------------------- 1. chave unica e correspondencia TSE-IBGE
invisible(checa_unica(as.data.frame(out), c("id_municipio_ibge", "ano_munic")))
invisible(checa_unica(as.data.frame(corr), "sg_ue"))
invisible(checa_unica(as.data.frame(corr), "id_municipio_ibge"))
passou <- c(passou, "checa_unica: (id_municipio_ibge, ano_munic) em munic_prefeitos; sg_ue e id_municipio_ibge em municipios_tse_ibge")
chk(nrow(corr) == 5569L, sprintf("correspondencia tem 5.569 prefeituras (tem %d)", nrow(corr)))
chk(all(!is.na(corr$id_municipio_ibge)), "toda prefeitura tem codigo IBGE")
chk(uniqueN(mand[cd_cargo == "11", sg_ue]) == nrow(corr), "prefeituras da correspondencia = sg_ue distintos com cargo 11 em mandatos.csv")
chk(all(corr$uf_divergente == "FALSE"), "nenhuma UF divergente entre TSE e IBGE")
# cardinalidade do join out x corr: many-to-one; as linhas sem sg_ue (Brasilia, Fernando de Noronha) ficam fora
jj <- join_seguro(as.data.frame(out[!is.na(sg_ue), .(id_municipio_ibge, ano_munic, sg_ue_out = sg_ue)]),
                  as.data.frame(corr[, .(id_municipio_ibge, sg_ue)]), by = "id_municipio_ibge",
                  cardinalidade = "many-to-one", tipo = "left")
chk(nrow(jj) == out[!is.na(sg_ue), .N] && all(jj$sg_ue_out == jj$sg_ue), "join_seguro many-to-one out x correspondencia: sg_ue coerente e sem inflacao")
reg("munic_cet_n_prefeituras_corr", nrow(corr))
reg("munic_cet_n_linhas_sem_sg_ue", out[is.na(sg_ue), .N])
# edicao -> eleicao de origem
chk(all(out[!is.na(ano_eleicao_bocel), ano_eleicao_bocel == ((ano_munic - 1L) %/% 4L) * 4L]), "ano_eleicao_bocel coerente com a edicao (2004->2000, 2005->2004, ...)")
chk(all(out[!is.na(id_mandato_bocel), substr(id_mandato_bocel, 2, 5) == as.character(ano_eleicao_bocel)]), "id_mandato_bocel e da eleicao ano_eleicao_bocel")
chk(all(out[!is.na(nome_prefeito_munic), ano_munic] %in% c(2004L, 2005L)) && all(out[ano_munic %in% c(2004L, 2005L), mean(!is.na(nome_prefeito_munic)) > 0.99]),
    "nome do prefeito presente so em 2004 e 2005 (em mais de 99% das linhas dessas edicoes; os marcadores de ausencia viram NA)")
chk(all(out[ano_munic %in% c(2004L, 2005L) & is.na(nome_prefeito_munic), status == "indeterminado"]),
    "linha de 2004/2005 sem nome (marcador de ausencia) fica 'indeterminado', nunca 'outro_em_exercicio'")
reg("munic_cet_n_linhas_2004_2005_sem_nome", out[ano_munic %in% c(2004L, 2005L) & is.na(nome_prefeito_munic), .N])

## ---------------------------------------------------------------- 2. amostra de 20 'outro_em_exercicio' com nome
o <- out[status == "outro_em_exercicio" & !is.na(nome_prefeito_munic) & !is.na(id_mandato_bocel)]
cat("outro_em_exercicio com nome (2004/2005):", nrow(o), "\n")
# mecanico, sobre TODOS: nome da MUNIC difere do eleito (normalizado)
o[, nm_munic := norm_nome(nome_prefeito_munic)][, nm_bocel := norm_nome(nome_prefeito_bocel)]
chk(all(o$nm_munic != o$nm_bocel), "em todo 'outro_em_exercicio' com nome, o nome da MUNIC difere do nome do eleito")
chk(all(o$match_nome_parcial == FALSE), "em todo 'outro_em_exercicio' com nome, match_nome_parcial e FALSE")
o <- merge(o, pes[, .(id_pessoa_vice = id_pessoa, nome_vice = nome, nome_urna_vice = nome_urna_recente)], by = "id_pessoa_vice", all.x = TRUE)
o[, nm_vice := norm_nome(nome_vice)][, nm_urna_vice := norm_nome(nome_urna_vice)]
o[, jw_vice := ifelse(is.na(nm_vice), NA_real_, 1 - stringdist(nm_munic, nm_vice, method = "jw", p = 0.1))]
o[, jw_urna_vice := ifelse(is.na(nm_urna_vice), NA_real_, 1 - stringdist(nm_munic, nm_urna_vice, method = "jw", p = 0.1))]
# substituto 'vice' por nome: JW com o nome ou nome de urna do vice deve ser alto
v <- o[substituto_provavel == "vice" & criterio_substituto == "nome"]
cat("substituto 'vice' por nome:", nrow(v), "| JW(munic, vice) min:", round(min(pmax(v$jw_vice, v$jw_urna_vice, na.rm = TRUE)), 3), "\n")
chk(all(v$vice_match_nome == TRUE), "substituto 'vice' por nome tem vice_match_nome TRUE")
chk(all(pmax(v$jw_vice, v$jw_urna_vice, na.rm = TRUE) >= 0.85), "substituto 'vice' por nome: JW >= 0,85 com nome ou nome de urna do vice (recalculado)")
t3 <- o[substituto_provavel == "terceiro"]
chk(all(t3$vice_match_nome == FALSE), "substituto 'terceiro' (com nome) tem vice_match_nome FALSE")
cat("\n== amostra de 20 'outro_em_exercicio' com nome (2004/2005), seed 20260828 ==\n")
am <- o[sample(.N, 20)]
print(am[, .(ano_munic, sg_ue, nome_prefeito_munic, nome_prefeito_bocel, jw_nome, nome_vice, jw_vice = round(jw_vice, 3), substituto_provavel, criterio_substituto)], nrows = 20)
# na amostra: nome difere do eleito (JW < 0,92) e, se substituto=vice, JW com vice >= 0,85
chk(all(am$jw_nome < 0.92 | am$match_sexo %in% FALSE | am$match_idade_ampla %in% FALSE), "amostra: nome da MUNIC de fato difere do eleito (JW < 0,92 ou sexo/idade incompativel)")
chk(all(am[substituto_provavel == "vice" & criterio_substituto == "nome", pmax(jw_vice, jw_urna_vice, na.rm = TRUE) >= 0.85]), "amostra: quando o substituto e 'vice' por nome, o nome bate com o vice")
reg("munic_cet_n_outro_com_nome", nrow(o))
reg("munic_cet_n_outro_com_nome_subst_vice", o[substituto_provavel == "vice", .N])
reg("munic_cet_n_outro_com_nome_subst_terceiro", o[substituto_provavel == "terceiro", .N])
reg("munic_cet_n_outro_com_nome_subst_indeterminado", o[!substituto_provavel %in% c("vice", "terceiro"), .N])

## ---------------------------------------------------------------- 3. recontagem por edicao contra numeros_assinatura.txt
# parse robusto do registro (chave | valor | ...): so os dois primeiros campos
ass_l <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- rbindlist(lapply(strsplit(ass_l[grepl("|", ass_l, fixed = TRUE)], "|", fixed = TRUE), function(p) data.table(V1 = trimws(p[1]), V2 = trimws(p[2]))))
setnames(ass, c("chave", "valor"))
ult <- ass[grepl("^munic_[0-9]{4}_|^munic_n_linhas|^munic_n_prefeituras|^bocel_forma_saida_municipal_substituicao|^bocel_mandatos_com_exercicio", chave), .SD[.N], by = chave]
rec <- list(munic_n_linhas_munic_prefeitos = nrow(out), munic_n_prefeituras_bocel = nrow(corr),
            munic_n_prefeituras_com_codigo_ibge = corr[!is.na(id_municipio_ibge), .N])
for (y in sort(unique(out$ano_munic))) {
  d <- out[ano_munic == y]
  rec[[sprintf("munic_%d_n_municipios", y)]] <- nrow(d)
  rec[[sprintf("munic_%d_n_pareados_bocel", y)]] <- d[!is.na(id_mandato_bocel), .N]
  rec[[sprintf("munic_%d_n_eleito_em_exercicio", y)]] <- d[status == "eleito_em_exercicio", .N]
  rec[[sprintf("munic_%d_n_outro_em_exercicio", y)]] <- d[status == "outro_em_exercicio", .N]
  rec[[sprintf("munic_%d_n_indeterminado", y)]] <- d[status == "indeterminado", .N]
  rec[[sprintf("munic_%d_n_substituto_vice", y)]] <- d[substituto_provavel %in% "vice", .N]
  rec[[sprintf("munic_%d_n_substituto_terceiro", y)]] <- d[substituto_provavel %in% "terceiro", .N]
}
sm <- mand[forma_saida == "substituicao_inferida_munic"]
rec$bocel_forma_saida_municipal_substituicao_inferida_munic <- nrow(sm)
rec$bocel_mandatos_com_exercicio_confirmado <- sum(!is.na(mand$exercicio_confirmado))
cmp <- merge(data.table(chave = names(rec), recontado = as.character(unlist(rec))), ult[, .(chave, registrado = valor)], by = "chave", all.x = TRUE)
cmp[, confere := !is.na(registrado) & recontado == registrado]
print(cmp)
chk(all(cmp$confere), sprintf("todos os %d numeros recontados conferem com o ultimo registro em numeros_assinatura.txt", nrow(cmp)))
reg("munic_cet_n_chaves_recontadas", nrow(cmp)); reg("munic_cet_n_chaves_divergentes", sum(!cmp$confere))

## ---------------------------------------------------------------- 4. integracao em mandatos.csv
cat("\nsubstituicao_inferida_munic em mandatos.csv:", nrow(sm), "\n"); print(sm[, .N, by = .(ano_eleicao, cargo, fonte_forma_saida)])
chk(all(sm$ano_eleicao %in% c("2000", "2004")), "todo 'substituicao_inferida_munic' e de eleicao 2000 ou 2004 (edicoes 2004/2005)")
chk(all(sm$cargo == "PREFEITO" & sm$fonte_forma_saida == "ibge_munic"), "todo 'substituicao_inferida_munic' e PREFEITO com fonte ibge_munic")
chk(all(sm$id_mandato %in% o$id_mandato_bocel), "todo 'substituicao_inferida_munic' corresponde a um 'outro_em_exercicio' com nome em munic_prefeitos")
# os 'outro' com nome que nao viraram substituicao foram sobrescritos por fonte de prioridade maior
oi <- mand[id_mandato %in% o$id_mandato_bocel]
print(oi[, .N, by = .(forma_saida, fonte_forma_saida)])
chk(all(oi[forma_saida != "substituicao_inferida_munic", fonte_forma_saida %in% c("tse_suplementar", "wikipedia", "wikidata", "wikidata_obito", "data_fim_efetiva")]),
    "os 'outro' com nome sem 'substituicao_inferida_munic' foram sobrescritos so por fonte de prioridade maior")
chk(nrow(sm) + oi[forma_saida != "substituicao_inferida_munic", .N] == nrow(o), "substituicao_inferida_munic + sobrescritos = outro_em_exercicio com nome")
# nenhuma edicao sem nome (2009+) gerou substituicao: mandatos 2008/2012/2016/2020 com 'outro' na MUNIC ficam nao_observado ou outra fonte
o2 <- out[status == "outro_em_exercicio" & is.na(nome_prefeito_munic) & !is.na(id_mandato_bocel), id_mandato_bocel]
chk(mand[id_mandato %in% o2 & fonte_forma_saida %in% "ibge_munic", .N] == 0, "nenhum mandato recebeu forma de saida da MUNIC a partir de edicao sem nome (2009+)")
# datas: data_fim_efetiva da substituicao e exercicio_confirmado da MUNIC preenchidos (correcao em R/10)
chk(all(!is.na(sm$data_fim_efetiva)), "todo 'substituicao_inferida_munic' tem data_fim_efetiva")
# a data de fim pode ter sido sobrescrita por fonte de prioridade maior sem forma de saida (Wikidata com fim e forma NA)
wd <- fread("data/wikidata_mandatos.csv", colClasses = "character", na.strings = "NA")
sm[, fim_esperado := paste0(as.integer(ano_eleicao) + fifelse(ano_eleicao == "2000", 4L, 1L), "-12-31")]
wp <- if (file.exists("data/wikipedia_prefeitos.csv")) fread("data/wikipedia_prefeitos.csv", colClasses = "character", na.strings = "NA") else data.table(fim = character(), id_mandato_bocel = character())
sm[, fim_wd := id_mandato %in% c(wd[!is.na(fim), id_mandato_bocel], wp[!is.na(fim), id_mandato_bocel])]
chk(all(sm$data_fim_efetiva == sm$fim_esperado | sm$fim_wd),
    "data_fim_efetiva da substituicao = 31/12 do ano da edicao (2004 para eleitos em 2000; 2005 para 2004), salvo fim vindo do Wikidata")
reg("munic_cet_n_subst_inferida_munic_com_fim_do_wikidata", sm[data_fim_efetiva != fim_esperado, .N])
# 12/09/2026: a fonte exata, porque grepl("ibge_munic") pegava tambem ibge_munic_ampliado (R/49, 04/09)
em <- mand[grepl("(^|;)ibge_munic(;|$)", fonte_exercicio)]
chk(all(!is.na(em$exercicio_confirmado)), sprintf("todo mandato com fonte_exercicio ibge_munic tem exercicio_confirmado (%d mandatos)", nrow(em)))
ok <- out[status == "eleito_em_exercicio" & !is.na(id_mandato_bocel)]
chk(nrow(em) == uniqueN(ok$id_mandato_bocel), "mandatos com fonte_exercicio ibge_munic = 'eleito_em_exercicio' distintos em munic_prefeitos")
# exercicio_confirmado >= 31/12 da edicao (tse_reeleicao pode ser posterior)
em2 <- merge(em[, .(id_mandato, exercicio_confirmado)], ok[, .(id_mandato = id_mandato_bocel, dt_munic = sprintf("%d-12-31", ano_munic))], by = "id_mandato")
chk(all(em2$exercicio_confirmado >= em2$dt_munic), "exercicio_confirmado >= data de referencia da MUNIC (a mais recente prevalece)")
em3 <- merge(mand[fonte_exercicio == "ibge_munic", .(id_mandato, exercicio_confirmado)], em2[, .(id_mandato, dt_munic)], by = "id_mandato")
chk(nrow(em3) == mand[fonte_exercicio == "ibge_munic", .N] && all(em3$exercicio_confirmado == em3$dt_munic),
    "onde a MUNIC e a unica fonte de exercicio, a data e 31/12 da edicao")
# nomes que sao marcador de ausencia nao podem restar nas edicoes com nome
chk(out[grepl("DISPON|INFORMAD|RECUSA", stri_trans_general(toupper(nome_prefeito_munic), "Latin-ASCII")), .N] == 0,
    "nenhum marcador de ausencia ('Não disponível' etc.) restou em nome_prefeito_munic")
reg("munic_cet_n_subst_inferida_munic", nrow(sm))
reg("munic_cet_n_subst_inferida_munic_eleicao_2000", sm[ano_eleicao == "2000", .N])
reg("munic_cet_n_subst_inferida_munic_eleicao_2004", sm[ano_eleicao == "2004", .N])
reg("munic_cet_n_outro_com_nome_sobrescritos_por_fonte_maior", oi[forma_saida != "substituicao_inferida_munic", .N])
reg("munic_cet_n_mandatos_fonte_exercicio_ibge_munic", nrow(em))
reg("munic_cet_n_mandatos_fonte_exercicio_ibge_munic_sem_data", sum(is.na(em$exercicio_confirmado)))
reg("munic_cet_n_subst_inferida_munic_sem_data_fim", sum(is.na(sm$data_fim_efetiva)))

## ---------------------------------------------------------------- relatorio
cat("\nPASSOU:", length(passou), "| FALHOU:", length(falhas), "\n"); if (length(falhas)) print(falhas)
fora <- c("veracidade do nome declarado a MUNIC (dado administrativo do IBGE)",
          "identidade real do substituto (vice por semelhanca de nome e apenas provavel)",
          "data exata da substituicao (a MUNIC so informa quem respondia na entrevista; 31/12 e convencao)",
          "pertinencia semantica do pareamento nome-a-nome (amostra impressa para leitura do autor)")
gravar_relatorio_verificacao(alvo = "data/mandatos.csv", script = script, passou = passou, falhou = falhas, fora_de_cobertura = fora)
cat("verifica_munic_integracao: concluido\n"); sink()
if (length(falhas)) quit(status = 1)
