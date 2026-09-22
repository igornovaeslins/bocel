# verifica_tce.R — verificacao determinista da frente Tribunais de Contas (grupo A).
# Re-le data/tce_gestores.csv e data/tce_gestores_cobertura.csv produzidos por R/23_tce_gestores.R e
# confere as invariantes contra data/mandatos.csv, data/pessoas.csv e data/municipios_tse_ibge.csv.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/verifica_tce.R"
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce.log", open = "wt"); sink(logf, split = TRUE)
passou <- character(); falhou <- character()
ck <- function(nome, cond, detalhe = "") {
  ok <- isTRUE(all(cond)) && length(cond) > 0
  cat(if (ok) "[ok]   " else "[FALHA] ", nome, if (nchar(detalhe)) paste0(" — ", detalhe) else "", "\n", sep = "")
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nchar(detalhe)) paste0(" (", detalhe, ")") else ""))
}
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
OBRIG <- c("uf", "tribunal", "unidade_gestora", "tipo_unidade", "id_municipio_ibge", "sg_ue", "nome", "cpf",
           "cargo_fonte", "cargo_bocel", "data_inicio", "data_fim", "situacao_fonte", "forma_saida",
           "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento", "url")

ck("01 arquivos existem", all(file.exists("data/tce_gestores.csv", "data/tce_gestores_cobertura.csv", "data_raw/tce/inventario_tce.csv")))
o <- fread("data/tce_gestores.csv", colClasses = "character", na.strings = "NA")
cb <- fread("data/tce_gestores_cobertura.csv", na.strings = "NA")
inv <- fread("data_raw/tce/inventario_tce.csv", colClasses = "character")

ck("02 colunas obrigatorias presentes", all(OBRIG %in% names(o)), paste(setdiff(OBRIG, names(o)), collapse = ","))
ck("03 nomes de coluna minusculos sem acento", all(grepl("^[a-z0-9_]+$", names(o))), paste(grep("^[a-z0-9_]+$", names(o), invert = TRUE, value = TRUE), collapse = ","))
ck("04 colunas de cobertura", all(c("uf", "cargo", "ano_eleicao", "n_bocel", "n_pareados", "taxa") %in% names(cb)))
ck("05 inventario cobre as 13 casas do grupo A",
   setequal(inv$uf, c("RS", "SC", "PR", "SP", "MG", "RJ", "ES", "MS", "MT", "GO", "DF", "TO", "BA")), paste(inv$uf, collapse = ","))
ck("06 inventario com tipo no vocabulario", all(inv$tipo %in% c("api", "ckan", "csv", "html", "nenhum")), paste(unique(inv$tipo), collapse = ","))
ck("07 inventario com viabilidade declarada em toda casa", all(inv$viavel %in% c("sim", "parcial", "nao")))
ck("08 inventario com url em toda casa", all(nchar(inv$url) > 10))

ck("09 base nao vazia", nrow(o) > 0, paste(nrow(o), "linhas"))
ck("10 forma_saida no vocabulario fechado", all(o$forma_saida %in% VOCAB), paste(setdiff(unique(o$forma_saida), VOCAB), collapse = ","))
ck("11 tipo_unidade em {prefeitura, camara}", all(o$tipo_unidade %in% c("prefeitura", "camara")))
ck("12 cargo_bocel no vocabulario", all(o$cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO", "VEREADOR")))
ck("13 cargo_bocel coerente com tipo_unidade",
   o[tipo_unidade == "prefeitura", all(cargo_bocel == "PREFEITO")] && o[tipo_unidade == "camara", all(cargo_bocel == "VEREADOR")])
ck("14 nome sempre preenchido", all(!is.na(o$nome) & nchar(o$nome) >= 5))
ck("15 municipio sempre resolvido", all(!is.na(o$sg_ue) & !is.na(o$id_municipio_ibge)))
ck("16 uf da linha bate com a uf do tribunal", all(o$uf == substr(o$tribunal, nchar(o$tribunal) - 1, nchar(o$tribunal))))
ck("17 cpf so quando tem 11 digitos", o[!is.na(cpf), all(nchar(cpf) == 11L)])
ck("18 datas no formato ISO quando presentes", o[!is.na(data_inicio), all(grepl("^\\d{4}-\\d{2}-\\d{2}$", data_inicio))] &&
   o[!is.na(data_fim), all(grepl("^\\d{4}-\\d{2}-\\d{2}$", data_fim))])

mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
ck("19 par sg_ue/ibge existe no dicionario de municipios",
   nrow(o[!mun, on = .(sg_ue, id_municipio_ibge)]) == 0L, paste(nrow(o[!mun, on = .(sg_ue, id_municipio_ibge)]), "linhas fora"))

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
p <- o[!is.na(id_mandato_bocel)]
ck("20 todo id_mandato_bocel existe em mandatos.csv", all(p$id_mandato_bocel %in% mand$id_mandato),
   paste(uniqueN(setdiff(p$id_mandato_bocel, mand$id_mandato)), "ids ausentes"))
ck("21 todo id_pessoa_bocel existe em pessoas.csv", all(o[!is.na(id_pessoa_bocel)]$id_pessoa_bocel %in% pess$id_pessoa))
p <- merge(p, mand[, .(id_mandato, m_cd_cargo = cd_cargo, m_sg_ue = unidade_posicao, m_uf = sg_uf,
                       m_id_pessoa = id_pessoa, m_ano = as.integer(ano_eleicao),
                       m_ini = mandato_inicio, m_fim = mandato_fim)],
           by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
ck("22 cargo do mandato pareado bate com cargo_bocel",
   p[, all(fifelse(cargo_bocel == "PREFEITO", m_cd_cargo == "11", m_cd_cargo == "13"))],
   paste(p[!(fifelse(cargo_bocel == "PREFEITO", m_cd_cargo == "11", m_cd_cargo == "13")), .N], "divergencias"))
ck("23 municipio do mandato pareado bate com o da linha", p[, all(m_sg_ue == sg_ue)], paste(p[m_sg_ue != sg_ue, .N], "divergencias"))
ck("24 uf do mandato pareado bate com a da linha", p[, all(m_uf == uf)])
ck("25 id_pessoa_bocel e o titular do mandato pareado", p[, all(m_id_pessoa == id_pessoa_bocel)])
ck("26 um mandato pareado nunca aponta duas pessoas", p[, uniqueN(id_pessoa_bocel), by = id_mandato_bocel][, all(V1 == 1L)])
pe <- p[!is.na(exercicio)]
ck("27 exercicio da fonte cai dentro do mandato pareado",
   pe[, all(as.integer(exercicio) >= as.integer(substr(m_ini, 1, 4)) & as.integer(exercicio) <= as.integer(substr(m_fim, 1, 4)))],
   paste(pe[!(as.integer(exercicio) >= as.integer(substr(m_ini, 1, 4)) & as.integer(exercicio) <= as.integer(substr(m_fim, 1, 4))), .N], "fora da janela"))
ck("28 ano_eleicao da linha bate com o do mandato pareado", p[!is.na(ano_eleicao), all(as.integer(ano_eleicao) == m_ano)])

pc <- p[grepl("^cpf_", metodo_pareamento)]
pc <- merge(pc, pess[, .(id_pessoa, nr_cpf)], by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
ck("29 pareamento por cpf confere com pessoas.nr_cpf", nrow(pc) == 0L || pc[, all(so_dig(nr_cpf) == cpf)],
   paste(nrow(pc), "linhas por cpf"))
ck("30 metodo_pareamento presente sempre que ha mandato", o[!is.na(id_mandato_bocel), all(!is.na(metodo_pareamento))])
ck("31 metodo_pareamento ausente quando nao ha mandato", o[is.na(id_mandato_bocel), all(is.na(metodo_pareamento))])
ck("32 metodos no conjunto declarado",
   all(na.omit(unique(o$metodo_pareamento)) %in% c("cpf_municipio_cargo_eleicao", "cpf_municipio_cargo", "nome_completo_eleicao",
     "nome_fonte=nome_urna_eleicao", "tokens_nome_fonte_no_nome_civil", "tokens_nome_fonte_no_nome_de_urna", "nome_completo_municipio_cargo")),
   paste(na.omit(unique(o$metodo_pareamento)), collapse = "|"))

sub <- o[!is.na(id_mandato_titular_substituido)]
ck("33 forma_saida 'outro' so onde ha titular substituido pelo vice",
   o[forma_saida == "outro", all(!is.na(id_mandato_titular_substituido))] && sub[, all(forma_saida == "outro")])
ck("34 mandato substituido e sempre de prefeito e do mesmo municipio",
   nrow(sub) == 0L || merge(sub[, .(id_mandato_titular_substituido, sg_ue)], mand[, .(id_mandato, cd_cargo, unidade_posicao)],
     by.x = "id_mandato_titular_substituido", by.y = "id_mandato")[, all(cd_cargo == "11" & unidade_posicao == sg_ue)])
ck("35 relacao com a chapa so em linha de prefeitura",
   o[!is.na(relacao_chapa_eleita), all(tipo_unidade == "prefeitura")])
ck("36 relacao no conjunto declarado",
   all(na.omit(unique(o$relacao_chapa_eleita)) %in% c("titular_eleito", "vice_eleito", "outra_pessoa_pareada", "indeterminado")))

ck("37 cobertura nunca pareia mais que o universo do BOCEL", cb[, all(n_pareados <= n_bocel)],
   paste(cb[n_pareados > n_bocel, .N], "linhas com excesso"))
ck("38 taxa da cobertura confere com a divisao", cb[, all(abs(taxa - round(n_pareados / n_bocel, 4)) < 1e-9)])
alvo <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "13") & sg_uf %in% unique(o$uf)]
ck("39 n_bocel da cobertura confere com mandatos.csv",
   isTRUE(all.equal(cb[, sum(n_bocel)], alvo[, uniqueN(id_mandato)])), paste(cb[, sum(n_bocel)], "vs", alvo[, uniqueN(id_mandato)]))
ck("40 total pareado da cobertura confere com a base",
   cb[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
   paste(cb[, sum(n_pareados)], "vs", o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]))
ck("41 cobertura cobre toda uf presente na base", setequal(unique(cb$uf), unique(o$uf)))
ck("42 nenhuma uf fora do grupo A", all(o$uf %in% inv$uf))
ck("43 toda uf da base tem fonte declarada viavel no inventario",
   all(unique(o$uf) %in% inv[viavel %in% c("sim", "parcial"), uf]))

registrar_numero("tce_verif_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tce_verif_n_aprovadas", length(passou), script = script)
registrar_numero("tce_verif_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao(
  alvo = "frente TCE grupo A: data/tce_gestores.csv e data/tce_gestores_cobertura.csv",
  script = "R/23_tce_gestores.R", passou = passou, falhou = falhou,
  fora_de_cobertura = c(
    "completude do cadastro do tribunal (a fonte lista quem prestou contas, nao todo agente que exerceu)",
    "pertinencia semantica do pareamento por tokens de nome e por nome civil sem cpf",
    "identidade de homonimos no mesmo municipio quando a fonte nao expoe cpf",
    "veracidade do campo Gestor/Responsavel tal como o tribunal o preenche",
    "forma de saida propriamente dita: as fontes do grupo A observam exercicio, nao desligamento",
    "se o vice que responde pela prefeitura assumiu por renuncia, cassacao, falecimento, afastamento ou licenca"))
cat("\nrelatorio:", f, "\naprovadas:", length(passou), " reprovadas:", length(falhou), "\n")
if (length(falhou)) { cat("REPROVADO\n"); sink(); quit(status = 1) }
cat("VERIFICADO\n"); sink()
