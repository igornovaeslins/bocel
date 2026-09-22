# 23_tce_gestores.R — cadastros de gestores/responsaveis por unidade gestora municipal nos Tribunais de
# Contas do grupo A (RS, SC, PR, SP, MG, RJ, ES, MS, MT, GO, DF, TO, BA), coletados por python/fetch_tce.py.
# O que as fontes viaveis do grupo A observam e QUEM respondia pela unidade gestora (prefeitura ou camara)
# num exercicio financeiro — nao a forma de saida. Por isso a contribuicao e de confirmacao de exercicio, e
# forma_saida so recebe valor no caso estreito em que a fonte mostra o vice-prefeito eleito respondendo pela
# prefeitura num exercicio do mandato do titular (saida observada, forma nao especificada -> "outro").
# Entrada:  data_raw/tce/inventario_tce.csv, data_raw/tce/<uf>/*, data/mandatos.csv, data/pessoas.csv,
#           data/municipios_tse_ibge.csv, data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:    data/tce_gestores.csv, data/tce_gestores_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/23_tce_gestores.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(readxl); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/23_tce_gestores.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/23_tce_gestores.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
CICLO <- seq(1996L, 2024L, 4L)
# exercicio financeiro -> eleicao que originou o mandato (posse em 1o de janeiro do ano seguinte a eleicao)
elei_de <- function(ex) { y <- ((as.integer(ex) - 1L) %/% 4L) * 4L; fifelse(y %in% CICLO, y, NA_integer_) }

## ------------------------------------------------ passo 1: inventario das 13 casas
inv <- fread("data_raw/tce/inventario_tce.csv", colClasses = "character")
print(inv[, .(uf, tribunal, tipo, viavel)])
registrar_numero("tce_n_casas_inventariadas", nrow(inv), script = script)
registrar_numero("tce_n_tribunais_com_fonte", inv[viavel %in% c("sim", "parcial"), .N], script = script)
registrar_numero("tce_n_tribunais_fonte_plena", inv[viavel == "sim", .N], script = script)
registrar_numero("tce_n_tribunais_sem_fonte", inv[viavel == "nao", .N], script = script)

## ------------------------------------------------ resolvedor de municipio (uf + nome -> sg_ue, ibge)
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
mun[, nome_norm := norm(nome_ibge)]
mun_u <- mun[, .N, by = .(sg_uf, nome_norm)][N == 1L, .(sg_uf, nome_norm)]
lk_nome <- merge(mun[, .(sg_uf, nome_norm, sg_ue, id_municipio_ibge)], mun_u, by = c("sg_uf", "nome_norm"))
lk_ibge <- unique(mun[, .(id_municipio_ibge, sg_ue_ibge = sg_ue, sg_uf_ibge = sg_uf)])[, if (.N == 1L) .SD, by = id_municipio_ibge]
res_nome <- function(uf, nome) {
  d <- data.table(sg_uf = uf, nome_norm = norm(nome))
  d[lk_nome, on = .(sg_uf, nome_norm), `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge)]
  d[, .(sg_ue, id_municipio_ibge)]
}

## ------------------------------------------------ passo 2: leitura das fontes coletadas
fontes <- list()
vaziog <- function() data.table(uf = character(), tribunal = character(), fonte = character(), unidade_gestora = character(),
                                tipo_unidade = character(), nome_municipio_fonte = character(), id_municipio_ibge = character(),
                                sg_ue = character(), nome = character(), cpf = character(), cargo_fonte = character(),
                                exercicio = integer(), data_inicio = character(), data_fim = character(),
                                situacao_fonte = character(), url = character())

## RJ — API de dados abertos: parecer previo das contas de governo, responsavel = chefe do Executivo do exercicio
f <- "data_raw/tce/rj/prestacao_contas_municipio.json"
if (file.exists(f)) {
  x <- as.data.table(fromJSON(f))
  x <- x[!is.na(Responsavel) & nchar(trimws(Responsavel)) >= 5]
  r <- res_nome("RJ", x$Municipio)
  fontes[["rj"]] <- data.table(uf = "RJ", tribunal = "TCE-RJ", fonte = "dados_abertos_prestacao_contas_municipio",
    unidade_gestora = paste("PREFEITURA MUNICIPAL DE", toupper(x$Municipio)), tipo_unidade = "prefeitura",
    nome_municipio_fonte = x$Municipio, id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue,
    nome = x$Responsavel, cpf = NA_character_, cargo_fonte = "Responsavel pelas contas de governo",
    exercicio = as.integer(x$Ano), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("parecer previo ", x$Indicador, "; processo ", x$Processo),
    url = "https://dados.tcerj.tc.br/api/v1/prestacao_contas_municipio")
}

## ES — CKAN: julgamento das contas do prefeito pela camara. Duas evidencias por linha:
##   (a) Responsavel = prefeito do exercicio; (b) NomeVereador = vereador em exercicio na data do julgamento.
f <- "data_raw/tce/es/julgamentodecontas.csv"
if (file.exists(f)) {
  x <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character")
  setnames(x, make.unique(names(x)))
  x[, dt_julg := substr(DataJulgamento, 1, 10)]
  a <- unique(x[!is.na(Responsavel) & nchar(trimws(Responsavel)) >= 5, .(nome_mun = nome, Ano, Responsavel, res_pp = get("Resultado parecer previo"), res_cam = get("Resultado julgamento câmara"), dt_julg)])
  r <- res_nome("ES", a$nome_mun)
  fontes[["es_pref"]] <- data.table(uf = "ES", tribunal = "TCE-ES", fonte = "ckan_es_julgamento_de_contas",
    unidade_gestora = paste("PREFEITURA MUNICIPAL DE", toupper(a$nome_mun)), tipo_unidade = "prefeitura",
    nome_municipio_fonte = a$nome_mun, id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue,
    nome = a$Responsavel, cpf = NA_character_, cargo_fonte = "Responsavel pelas contas anuais (prefeito)",
    exercicio = as.integer(a$Ano), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("parecer previo ", a$res_pp, "; julgamento camara ", a$res_cam),
    url = "https://dados.es.gov.br/dataset/b669d51c-1443-44b9-9011-0d0968dd6270")
  b <- unique(x[!is.na(NomeVereador) & nchar(trimws(NomeVereador)) >= 5 & !is.na(dt_julg) & dt_julg != "",
               .(nome_mun = nome, NomeVereador, PartidoVereador, dt_julg)])
  r <- res_nome("ES", b$nome_mun)
  fontes[["es_ver"]] <- data.table(uf = "ES", tribunal = "TCE-ES", fonte = "ckan_es_julgamento_de_contas_vereadores",
    unidade_gestora = paste("CAMARA MUNICIPAL DE", toupper(b$nome_mun)), tipo_unidade = "camara",
    nome_municipio_fonte = b$nome_mun, id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue,
    nome = b$NomeVereador, cpf = NA_character_, cargo_fonte = "Vereador votante no julgamento das contas",
    # a data da sessao de julgamento nao e posse nem fim de exercicio: e o instante em que o vereador
    # aparece em exercicio. Escrita em data_inicio, R/10 a lia como data de posse e gravava 2.270 posses
    # falsas em mandatos.csv (ex.: posse 2012-08-27 num mandato iniciado em 2009-01-01). Fica so no
    # texto de situacao_fonte, que nenhuma etapa de integracao interpreta como data. (29/ago/2026)
    exercicio = as.integer(substr(b$dt_julg, 1, 4)), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("em exercicio na sessao de julgamento de ", b$dt_julg, "; partido ", b$PartidoVereador),
    url = "https://dados.es.gov.br/dataset/b669d51c-1443-44b9-9011-0d0968dd6270")
}

## RS — CKAN: contas julgadas irregulares e pareceres previos desfavoraveis. CD_ORGAO resolve o municipio
## pela tabela auxiliar de orgaos auditados; CPF vem completo.
f <- "data_raw/tce/rs/contas-irregulares.csv"
if (file.exists(f)) {
  x <- fread(f, colClasses = "character")
  og <- fread("data_raw/tce/rs/orgaos_auditados_rs.csv", colClasses = "character")
  og <- og[ESFERA == "MUNICIPAL", .(CD_ORGAO, SETOR_GOVERNAMENTAL, NOME_ORGAO_AUX = NOME_ORGAO, CD_MUNICIPIO_IBGE)]
  x <- merge(x, og, by = "CD_ORGAO", all.x = TRUE)
  x <- x[!is.na(CD_MUNICIPIO_IBGE) & grepl("^(PM|CM) DE ", NOME_ORGAO)]
  x[, tipo_unidade := fifelse(grepl("^PM DE ", NOME_ORGAO), "prefeitura", "camara")]
  x[lk_ibge, on = .(CD_MUNICIPIO_IBGE = id_municipio_ibge), sg_ue := i.sg_ue_ibge]
  fontes[["rs"]] <- data.table(uf = "RS", tribunal = "TCE-RS", fonte = "ckan_tcers_contas_irregulares",
    unidade_gestora = x$NOME_ORGAO, tipo_unidade = x$tipo_unidade,
    nome_municipio_fonte = sub("^(PM|CM) DE ", "", x$NOME_ORGAO), id_municipio_ibge = x$CD_MUNICIPIO_IBGE, sg_ue = x$sg_ue,
    nome = x$NOME, cpf = so_dig(x$CPF), cargo_fonte = "Responsavel por contas do orgao no exercicio",
    exercicio = as.integer(x$EXERCICIO), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0(x$TEXTO_ACORDAO, "; transito ", x$DT_TRANSITO_JULGADO),
    url = "https://dados.tce.rs.gov.br/dados/decisoes/contas-irregulares.csv")
}

## SC — relacao de responsaveis por contas irregulares ou parecer pela rejeicao. O municipio so aparece
## quando o cargo textual o nomeia; CPF vem mascarado.
f <- "data_raw/tce/sc/contas-irregulares.csv"
if (file.exists(f)) {
  x <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character")
  x[, cg := stri_trans_general(Cargo, "Latin-ASCII")]
  x[, tipo_unidade := fcase(grepl("^Prefeit[oa]", cg, ignore.case = TRUE), "prefeitura",
                            grepl("Camara (Municipal|de Vereadores)", cg, ignore.case = TRUE), "camara",
                            default = NA_character_)]
  x[, nome_mun := trimws(sub("/SC$", "", sub("^.*?\\b(?:de|do|da)\\s+", "", sub("^.*C[aâ]mara (?:Municipal|de Vereadores) de ", "", cg), perl = TRUE)))]
  x[grepl("^Prefeit", cg), nome_mun := trimws(sub("/SC$", "", sub("^Prefeit[oa] Municipal( de)? ?", "", cg)))]
  x[grepl("C[aâ]mara", cg), nome_mun := trimws(sub("/SC$", "", sub("^.*C[aâ]mara (?:Municipal|de Vereadores) de ", "", cg)))]
  x <- x[!is.na(tipo_unidade) & nchar(nome_mun) >= 3 & !grepl("C[aâ]mara|Prefeit", nome_mun)]
  r <- res_nome("SC", x$nome_mun)
  fontes[["sc"]] <- data.table(uf = "SC", tribunal = "TCE-SC", fonte = "tcesc_responsaveis_contas_irregulares",
    unidade_gestora = x$Cargo, tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$nome_mun,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$Responsavel, cpf = NA_character_,
    cargo_fonte = x$Cargo, exercicio = NA_integer_, data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("contas irregulares/parecer pela rejeicao; processo ", x$Processo, "; transito ", x[[5]]),
    url = "https://servicos.tcesc.tc.br/contas-irregulares-ou-parecer-rejeicao/?exportar=csv")
}

## SP — relacao de responsaveis por contas julgadas irregulares (contas anuais). CPF mascarado nos digitos
## do meio; conserva-se a mascara para conferencia, nao para pareamento.
f <- "data_raw/tce/sp/irregulares_contas_anuais.xlsx"
if (file.exists(f)) {
  x <- as.data.table(read_excel(f, skip = 5, col_types = "text"))
  setnames(x, paste0("v", seq_along(x)))
  x <- x[, .(nome = v3, cpf_masc = v4, processo = v5, materia = v6,
             origem = toupper(trimws(v7)), transito = v8, exercicio = v9)]
  x <- x[!is.na(nome) & grepl("^(PREFEITURA MUNICIPAL DE|CAMARA MUNICIPAL DE) ", origem)]
  x[, tipo_unidade := fifelse(grepl("^PREFEITURA", origem), "prefeitura", "camara")]
  x[, nome_mun := sub("^(PREFEITURA|CAMARA) MUNICIPAL DE ", "", origem)]
  r <- res_nome("SP", x$nome_mun)
  fontes[["sp"]] <- data.table(uf = "SP", tribunal = "TCE-SP", fonte = "tcesp_responsaveis_contas_irregulares",
    unidade_gestora = x$origem, tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$nome_mun,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$nome, cpf = NA_character_,
    cargo_fonte = x$materia, exercicio = as.integer(x$exercicio), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("contas julgadas irregulares; processo ", x$processo, "; cpf mascarado ", x$cpf_masc, "; transito ", x$transito),
    url = "https://www.tce.sp.gov.br/relacao-de-responsaveis-por-contas-julgadas-irregulares")
}

## SP — segunda relacao: prestacoes de contas de repasses. O responsavel e o ordenador do repasse, que nem
## sempre e o chefe do orgao; entra como fonte separada e so vira mandato quando o pareamento por nome,
## municipio, cargo e eleicao devolve um unico mandato daquele cargo.
f <- "data_raw/tce/sp/irregulares_prest_contas.xlsx"
if (file.exists(f)) {
  x <- as.data.table(read_excel(f, skip = 4, col_types = "text"))
  setnames(x, paste0("v", seq_along(x)))
  x <- x[, .(nome = v3, cpf_masc = v4, processo = v5, materia = v6,
             origem = toupper(trimws(v7)), transito = v8, exercicio = v9)]
  x <- x[!is.na(nome) & grepl("^(PREFEITURA MUNICIPAL DE|CAMARA MUNICIPAL DE) ", origem)]
  x[, tipo_unidade := fifelse(grepl("^PREFEITURA", origem), "prefeitura", "camara")]
  x[, nome_mun := sub("^(PREFEITURA|CAMARA) MUNICIPAL DE ", "", origem)]
  r <- res_nome("SP", x$nome_mun)
  fontes[["sp2"]] <- data.table(uf = "SP", tribunal = "TCE-SP", fonte = "tcesp_responsaveis_prestacao_contas_repasses",
    unidade_gestora = x$origem, tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$nome_mun,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$nome, cpf = NA_character_,
    cargo_fonte = x$materia, exercicio = as.integer(x$exercicio), data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("prestacao de contas julgada irregular; processo ", x$processo, "; cpf mascarado ", x$cpf_masc, "; transito ", x$transito),
    url = "https://www.tce.sp.gov.br/relacao-de-responsaveis-por-contas-julgadas-irregulares")
}

## MS — portal-services: contas julgadas irregulares. CPF completo, sem exercicio financeiro.
f <- "data_raw/tce/ms/contas_irregulares.json"
if (file.exists(f)) {
  x <- as.data.table(fromJSON(f))
  x[, ug := stri_trans_general(toupper(NomeUg), "Latin-ASCII")]
  x <- x[grepl("^(PREFEITURA MUNICIPAL|CAMARA MUNICIPAL)", ug) & !is.na(Sancionado)]
  x[, tipo_unidade := fifelse(grepl("^PREFEITURA", ug), "prefeitura", "camara")]
  r <- res_nome("MS", x$NomeUa)
  fontes[["ms"]] <- data.table(uf = "MS", tribunal = "TCE-MS", fonte = "tcems_contas_julgadas_irregulares",
    unidade_gestora = x$ug, tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$NomeUa,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$Sancionado, cpf = so_dig(x$CPFSancionado),
    cargo_fonte = "Sancionado em contas julgadas irregulares", exercicio = NA_integer_,
    data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0(x$DecisaoTCE, "; processo ", x$CodProcessoTce, "; decisao ", substr(x$DataDecisaoTce, 1, 10)),
    url = "https://portal-services.tce.ms.gov.br/portal-services/contasJulgadas/getContasJulgadasIrregulares.json")
}

## BA — TCM-BA, quadro geral de prestacao de contas: gestor da unidade (prefeitura ou camara) por exercicio.
## E a unica fonte do grupo A que traz um cadastro completo de gestores por unidade e ano, e nao so a lista
## dos que tiveram contas rejeitadas.
f <- "data_raw/tce/ba/detalhes.jsonl"
if (file.exists(f)) {
  x <- rbindlist(lapply(readLines(f, warn = FALSE), function(l) as.data.table(fromJSON(l))), use.names = TRUE, fill = TRUE)
  setnames(x, make.names(names(x)))
  gcol <- grep("^Gestor$", names(x), value = TRUE); ecol <- grep("^Exerc", names(x), value = TRUE)
  x <- x[http == 200 & !is.na(get(gcol)) & nchar(trimws(get(gcol))) >= 5]
  x[, `:=`(gestor = trimws(get(gcol)), exerc = as.integer(get(ecol)))]
  x[, tipo_unidade := fifelse(ent == "P", "prefeitura", "camara")]
  # no quadro geral so a linha da Prefeitura traz o nome do municipio; o codigo da unidade resolve as demais
  mp_ba <- fromJSON("data_raw/tce/ba/municipios_codigo_nome.json")
  # mp_ba[codigo] devolve NULL para codigo ausente e unlist() DROPA o NULL, encurtando o vetor e
  # desalinhando o nome do municipio de todas as linhas seguintes. match() mantem o comprimento e
  # deixa NA onde nao ha codigo, que fcoalesce resolve pelo nome da propria pagina. (29/ago/2026)
  x[, nome_municipio := fcoalesce(unlist(mp_ba, use.names = FALSE)[match(as.character(muni), names(mp_ba))], nome_municipio)]
  x <- unique(x[, .(nome_municipio_fonte = nome_municipio, tipo_unidade, exerc, gestor, muni,
                    unidade = if ("unidade" %in% names(x)) unidade else NA_character_,
                    transito = if ("Transitado.em.Julgado" %in% names(x)) get("Transitado.em.Julgado") else NA_character_,
                    decisao = if ("Decisão.pela.Câmara" %in% names(x)) get("Decisão.pela.Câmara") else NA_character_)])
  r <- res_nome("BA", x$nome_municipio_fonte)
  fontes[["ba"]] <- data.table(uf = "BA", tribunal = "TCM-BA", fonte = "tcmba_quadro_geral_prestacao_contas",
    unidade_gestora = fcoalesce(x$unidade, paste(fifelse(x$tipo_unidade == "prefeitura", "PREFEITURA DE", "CAMARA MUNICIPAL DE"), x$nome_municipio_fonte)),
    tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$nome_municipio_fonte,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$gestor, cpf = NA_character_,
    cargo_fonte = "Gestor da unidade na prestacao de contas anual", exercicio = x$exerc,
    data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("transitado em julgado ", x$transito, "; decisao da camara ", x$decisao),
    url = paste0("https://www.tcm.ba.gov.br/index.php/consulta/legislacao/decisoes/contas-anuais/detalhe-conta-anual/?ano=", x$exerc, "&ent=", substr(toupper(x$tipo_unidade), 1, 1), "&muni=", x$muni, "&des="))
}

g <- rbindlist(c(list(vaziog()), fontes), use.names = TRUE, fill = TRUE)
stopifnot(nrow(g) > 0)
g[, nome := trimws(gsub("\\s+", " ", nome))]
g <- g[nchar(nome) >= 5]
g[, cpf := fifelse(!is.na(cpf) & nchar(cpf) == 11L, cpf, NA_character_)]
cat("linhas brutas por fonte:\n"); print(g[, .N, by = .(uf, fonte)][order(uf)])
registrar_numero("tce_n_registros_brutos", nrow(g), script = script)
registrar_numero("tce_n_registros_sem_municipio_resolvido", g[is.na(sg_ue), .N], script = script)
g <- g[!is.na(sg_ue)]

## cargo no vocabulario do BOCEL e eleicao de referencia
g[, cargo_bocel := fifelse(tipo_unidade == "prefeitura", "PREFEITO", "VEREADOR")]
g[, ano_eleicao := elei_de(exercicio)]
g[, nome_normalizado := norm(nome)]
registrar_numero("tce_n_registros", nrow(g), script = script)
registrar_numero("tce_n_registros_com_cpf", g[!is.na(cpf), .N], script = script)
registrar_numero("tce_n_registros_com_exercicio", g[!is.na(exercicio), .N], script = script)
registrar_numero("tce_n_municipios_cobertos", g[, uniqueN(sg_ue)], script = script)

## ------------------------------------------------ BOCEL: mandatos de prefeito, vice e vereador
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, sg_uf, ano_eleicao = as.integer(ano_eleicao), cd_cargo, cargo)],
              pess[, .(id_pessoa, nome_bocel = nome, nr_cpf)], by = "id_pessoa")
mand[, nome_norm := norm(nome_bocel)]
mand[, cpf_bocel := fifelse(!is.na(nr_cpf) & nchar(so_dig(nr_cpf)) == 11L, so_dig(nr_cpf), NA_character_)]
ufs_alvo <- unique(g$uf)
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_(19|20)\\d{2}\\.parquet$", full.names = TRUE), function(fp) {
  x <- as.data.table(read_parquet(fp, col_select = c("ANO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO %in% c("11", "12", "13") & SG_UF %in% ufs_alvo]
}), use.names = TRUE)
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_", CD_CARGO, "_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, unique(cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))]), by = "id_mandato", all.x = TRUE)
mand <- mand[sg_ue %in% unique(g$sg_ue)]
mp <- mand[cd_cargo != "12"]   # universo pareavel: prefeito e vereador (o vice entra so no sinal de substituicao)
setkey(mp, sg_ue, ano_eleicao, cd_cargo)

## ------------------------------------------------ pareamento
## Um registro do TC casa com um mandato do BOCEL quando coincidem municipio, cargo e, quando a fonte informa
## o exercicio, a eleicao que o originou. A identidade vem do CPF quando a fonte o expoe; senao do nome.
g[, rid := .I]
g[, cd_alvo := fifelse(cargo_bocel == "PREFEITO", "11", "13")]
vazio <- function() data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (!is.null(d) && nrow(d)) unique(d[, .(rid, id_mandato, id_pessoa, metodo)], by = "rid") else vazio()
feitos <- integer(0)
regra <- function(chave_g, chave_m, metodo, so_com_ano = TRUE, dt_m = mp) {
  r <- g[!rid %in% feitos]
  # sem exercicio na fonte (SC, MS) o pareamento dispensa a eleicao; com exercicio ele e obrigatorio, e um
  # exercicio anterior a 1997 simplesmente nao tem mandato correspondente no BOCEL
  r <- if (so_com_ano) r[!is.na(ano_eleicao)] else r[is.na(exercicio)]
  if (!nrow(r)) return(vazio())
  m <- merge(r[, c("rid", chave_g), with = FALSE], dt_m[, c(chave_m, "id_mandato", "id_pessoa"), with = FALSE],
             by.x = chave_g, by.y = chave_m, allow.cartesian = TRUE)
  m <- m[, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (!nrow(m)) return(vazio())
  m[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)]
}
res <- list()
# (1) CPF + municipio + cargo + eleicao
mp_cpf <- mp[!is.na(cpf_bocel)]
res[[1]] <- regra(c("cpf", "sg_ue", "cd_alvo", "ano_eleicao"), c("cpf_bocel", "sg_ue", "cd_cargo", "ano_eleicao"), "cpf_municipio_cargo_eleicao", TRUE, mp_cpf)
feitos <- c(feitos, res[[1]]$rid)
# (2) CPF + municipio + cargo (fonte sem exercicio; so quando ha um unico mandato daquele CPF no municipio)
res[[2]] <- regra(c("cpf", "sg_ue", "cd_alvo"), c("cpf_bocel", "sg_ue", "cd_cargo"), "cpf_municipio_cargo", FALSE, mp_cpf)
feitos <- c(feitos, res[[2]]$rid)
# (3) nome civil completo + municipio + cargo + eleicao
res[[3]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_completo_eleicao")
feitos <- c(feitos, res[[3]]$rid)
# (4) nome da fonte igual ao nome de urna + municipio + cargo + eleicao
res[[4]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_urna_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_fonte=nome_urna_eleicao", TRUE, mp[!is.na(nome_urna_norm)])
feitos <- c(feitos, res[[4]]$rid)
# (5) tokens do nome da fonte contidos no nome civil, mandato unico no municipio-cargo-eleicao
r5 <- g[!rid %in% feitos & !is.na(ano_eleicao)]
res[[5]] <- if (nrow(r5)) {
  c5 <- merge(r5[, .(rid, sg_ue, cd_alvo, ano_eleicao, nome_normalizado)], mp[, .(sg_ue, cd_cargo, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
              by.x = c("sg_ue", "cd_alvo", "ano_eleicao"), by.y = c("sg_ue", "cd_cargo", "ano_eleicao"), allow.cartesian = TRUE)
  if (nrow(c5)) {
    ta <- lapply(strsplit(c5$nome_normalizado, " "), function(t) t[nchar(t) >= 3])
    tb <- lapply(strsplit(c5$nome_norm, " "), function(t) t[nchar(t) >= 3])
    c5[, ok := mapply(function(a, b) length(a) >= 2L && all(a %in% b), ta, tb)]
    d <- c5[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
    if (nrow(d)) d[, metodo := "tokens_nome_fonte_no_nome_civil"][, .(rid, id_mandato, id_pessoa, metodo)] else vazio()
  } else vazio()
} else vazio()
feitos <- c(feitos, res[[5]]$rid)
# (6) tokens do nome da fonte contidos no nome de urna, mandato unico
r6 <- g[!rid %in% feitos & !is.na(ano_eleicao)]
res[[6]] <- if (nrow(r6)) {
  c6 <- merge(r6[, .(rid, sg_ue, cd_alvo, ano_eleicao, nome_normalizado)], mp[!is.na(nome_urna_norm), .(sg_ue, cd_cargo, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
              by.x = c("sg_ue", "cd_alvo", "ano_eleicao"), by.y = c("sg_ue", "cd_cargo", "ano_eleicao"), allow.cartesian = TRUE)
  if (nrow(c6)) {
    ta <- lapply(strsplit(c6$nome_normalizado, " "), function(t) t[nchar(t) >= 3])
    tb <- lapply(strsplit(c6$nome_urna_norm, " "), function(t) t[nchar(t) >= 3])
    c6[, ok := mapply(function(a, b) length(a) >= 1L && any(nchar(a) >= 4L) && all(a %in% b), ta, tb)]
    d <- c6[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
    if (nrow(d)) d[, metodo := "tokens_nome_fonte_no_nome_de_urna"][, .(rid, id_mandato, id_pessoa, metodo)] else vazio()
  } else vazio()
} else vazio()
feitos <- c(feitos, res[[6]]$rid)
# (7) nome civil completo + municipio + cargo, sem exercicio na fonte e mandato unico na serie
res[[7]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo"), c("nome_norm", "sg_ue", "cd_cargo"), "nome_completo_municipio_cargo", FALSE)
par <- rbindlist(lapply(res, sel), use.names = TRUE)[!duplicated(rid)]
cat("pareamentos por regra:\n"); print(par[, .N, by = metodo][order(-N)])
g[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
g[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# fontes sem exercicio financeiro (SC, MS) herdam a eleicao do mandato pareado, marcada como tal
g[, ano_eleicao_fonte := ano_eleicao]
g[is.na(exercicio) & is.na(ano_eleicao) & !is.na(id_mandato_bocel), ano_eleicao := mp[.SD, on = .(id_mandato = id_mandato_bocel), x.ano_eleicao]]

## ------------------------------------------------ relacao com a chapa eleita e forma de saida
## Para cada registro de prefeitura num exercicio do mandato, compara-se o gestor com a chapa eleita naquele
## municipio. Quando quem responde pela prefeitura e o VICE eleito, a saida do titular esta observada, ainda
## que a fonte nao diga a forma — unico caso em que este script escreve forma_saida diferente de nao_observado.
tokens <- function(x) lapply(strsplit(fcoalesce(x, ""), " "), function(t) t[nchar(t) >= 3L])
compat <- function(a, b) { ta <- tokens(a); tb <- tokens(b); mapply(function(u, v) length(u) >= 2L && all(u %in% v), ta, tb) }
urna12 <- unique(cand[CD_CARGO %in% c("11", "12"), .(id_mandato, urna_norm = norm(NM_URNA_CANDIDATO))])
chapa <- merge(mand[cd_cargo %in% c("11", "12"), .(id_mandato, id_pessoa, sg_ue, ano_eleicao, cd_cargo, nome_norm)],
               urna12, by = "id_mandato", all.x = TRUE)
tit <- chapa[cd_cargo == "11"][, if (.N == 1L) .SD else NULL, by = .(sg_ue, ano_eleicao)]
setnames(tit, c("id_mandato", "id_pessoa", "nome_norm", "urna_norm"), c("tit_id_mandato", "tit_id_pessoa", "tit_norm", "tit_urna"))
vic <- chapa[cd_cargo == "12"][, if (.N == 1L) .SD else NULL, by = .(sg_ue, ano_eleicao)]
setnames(vic, c("id_mandato", "id_pessoa", "nome_norm", "urna_norm"), c("vice_id_mandato", "vice_id_pessoa", "vice_norm", "vice_urna"))
pr <- g[cargo_bocel == "PREFEITO" & !is.na(ano_eleicao), .(rid, sg_ue, ano_eleicao, nome_normalizado, id_pessoa_bocel)]
pr <- merge(pr, tit[, .(sg_ue, ano_eleicao, tit_id_mandato, tit_id_pessoa, tit_norm, tit_urna)], by = c("sg_ue", "ano_eleicao"))
pr <- merge(pr, vic[, .(sg_ue, ano_eleicao, vice_id_pessoa, vice_norm, vice_urna)], by = c("sg_ue", "ano_eleicao"), all.x = TRUE)
pr[, e_titular := (!is.na(id_pessoa_bocel) & id_pessoa_bocel == tit_id_pessoa) |
     compat(nome_normalizado, tit_norm) | compat(tit_norm, nome_normalizado) |
     (!is.na(tit_urna) & (compat(nome_normalizado, tit_urna) | compat(tit_urna, nome_normalizado)))]
pr[, e_vice := !e_titular & !is.na(vice_norm) &
     ((!is.na(id_pessoa_bocel) & id_pessoa_bocel == vice_id_pessoa) |
      compat(nome_normalizado, vice_norm) | compat(vice_norm, nome_normalizado) |
      (!is.na(vice_urna) & (compat(nome_normalizado, vice_urna) | compat(vice_urna, nome_normalizado))))]
pr[, relacao := fcase(e_titular, "titular_eleito", e_vice, "vice_eleito",
                      !is.na(id_pessoa_bocel) & id_pessoa_bocel != tit_id_pessoa, "outra_pessoa_pareada",
                      default = "indeterminado")]
g[, `:=`(relacao_chapa_eleita = NA_character_, id_mandato_titular_substituido = NA_character_)]
g[pr$rid, relacao_chapa_eleita := pr$relacao]
g[pr[relacao == "vice_eleito"]$rid, id_mandato_titular_substituido := pr[relacao == "vice_eleito"]$tit_id_mandato]
print(pr[, .N, by = relacao][order(-N)])
g[, forma_saida := fifelse(!is.na(id_mandato_titular_substituido), "outro", "nao_observado")]
stopifnot(all(g$forma_saida %in% VOCAB))

## ------------------------------------------------ saida
out <- g[, .(uf, tribunal, unidade_gestora, tipo_unidade, id_municipio_ibge, sg_ue, nome, cpf, cargo_fonte, cargo_bocel,
             data_inicio, data_fim, situacao_fonte, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url,
             fonte, nome_municipio_fonte, exercicio, ano_eleicao_fonte, ano_eleicao, relacao_chapa_eleita, id_mandato_titular_substituido)]
setorder(out, uf, sg_ue, tipo_unidade, exercicio, nome, na.last = TRUE)
# ES e SC chegam em Latin-1 e as colunas que passam direto da fonte para a saida (nome_municipio_fonte,
# unidade_gestora, cargo_fonte) conservavam o byte Latin-1, deixando o csv invalido em UTF-8 em 4.340
# celulas; as colunas que passam por norm() ja saiam convertidas. enc2utf8 uniformiza a saida inteira.
for (cl in names(out)[vapply(out, is.character, logical(1))]) set(out, j = cl, value = enc2utf8(out[[cl]]))
fwrite(out, "data/tce_gestores.csv", na = "NA", quote = TRUE)

## cobertura: mandatos do BOCEL por UF/cargo/eleicao contra os pareados
alvo <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "13") & sg_uf %in% ufs_alvo,
          .(uf = sg_uf, cargo, ano_eleicao = as.integer(ano_eleicao), id_mandato)]
cob <- alvo[, .(n_bocel = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
pc <- unique(out[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel)])
pc <- merge(pc, alvo, by = "id_mandato")[, .(n_pareados = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
cob <- merge(cob, pc, by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob[, taxa := round(n_pareados / n_bocel, 4)]
setorder(cob, uf, cargo, ano_eleicao)
fwrite(cob, "data/tce_gestores_cobertura.csv", na = "NA")

## ------------------------------------------------ numeros
registrar_numero("tce_n_registros_finais", nrow(out), script = script)
registrar_numero("tce_n_pareados_linhas", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tce_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tce_n_pessoas_pareadas", out[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)], script = script)
registrar_numero("tce_taxa_pareamento_linhas", round(out[!is.na(id_mandato_bocel), .N] / nrow(out), 4), script = script)
for (rr in sort(unique(na.omit(out$relacao_chapa_eleita)))) registrar_numero(paste0("tce_n_prefeitura_relacao_", rr), out[relacao_chapa_eleita == rr, .N], script = script)
registrar_numero("tce_n_saida_titular_observada_por_vice", out[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)], script = script)
for (u in sort(unique(out$uf))) {
  registrar_numero(paste0("tce_n_registros_uf_", u), out[uf == u, .N], script = script)
  registrar_numero(paste0("tce_n_mandatos_pareados_uf_", u), out[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
for (cg in sort(unique(out$cargo_bocel))) {
  registrar_numero(paste0("tce_n_registros_cargo_", cg), out[cargo_bocel == cg, .N], script = script)
  registrar_numero(paste0("tce_n_mandatos_pareados_cargo_", cg), out[cargo_bocel == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
uc <- cob[, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados)), by = .(uf, cargo)][, taxa := round(n_pareados / n_bocel, 4)]
for (i in seq_len(nrow(uc))) registrar_numero(paste0("tce_taxa_", uc$uf[i], "_", gsub("[^A-Z]", "", uc$cargo[i])), uc$taxa[i], script = script)
print(out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)), municipios = uniqueN(sg_ue)), by = .(uf, tribunal, fonte)][order(uf)])
print(uc[order(-n_pareados)]); print(out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)])
cat("23_tce_gestores: concluido —", nrow(out), "registros,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
sink()
