# 24_tce_gestores_b.R — cadastros de gestores/agentes eletivos por unidade gestora municipal nos Tribunais
# de Contas do GRUPO B (PE, CE, PB, RN, PI, MA, AL, SE, PA/TCM-PA, AM, AC, RO, RR, AP), coletados por
# python/fetch_tce_b.py.
#
# Diferenca em relacao ao grupo A: duas das fontes viaveis aqui sao cadastros de vinculo, e nao listas de
# contas julgadas. A folha do Sagres/TCE-PB e o cadastro de servidores do TCE-PE trazem cargo eletivo com
# data de admissao, data de afastamento e (em PE) o tipo do ato de afastamento, de modo que a contribuicao
# vai alem da confirmacao de exercicio e chega a forma de saida.
#
# Regras de forma de saida, por fonte:
#   PE  TipoAtoPessoalAfastamento = Falecimento -> falecimento; 'Saida de cargo eletivo' com DataAfastamento
#       a 45 dias ou menos do fim da legislatura -> fim_regular, e antes disso -> outro; sem data de
#       afastamento -> nao_observado.
#   PB  a folha e mensal; o ultimo mes de competencia do vinculo eletivo, comparado ao fim da legislatura,
#       da fim_regular (chega a novembro ou dezembro do ultimo ano) ou outro (para 3 meses ou mais antes,
#       E a unidade gestora continua enviando folha depois disso — a guarda contra falha de remessa).
#       Legislatura ainda aberta -> nao_observado.
#   PI  o Portal da Cidadania so devolve o prefeito em exercicio, e a lista de responsaveis com contas
#       julgadas irregulares so identifica quem respondia pela unidade num exercicio -> nao_observado.
#
# Limite declarado do cadastro de PE: quando a pessoa se reelege, o TCE nao abre vinculo novo, de modo que
# a linha vai da primeira posse ate a saida definitiva e pode terminar depois do fim da legislatura em que
# comecou. A linha e atribuida ao mandato da eleicao que abriu o vinculo, e para esse mandato o fim_regular
# esta correto; os mandatos seguintes da mesma pessoa ficam sem linha propria
# (tceb_n_linhas_vinculo_alem_da_legislatura conta esses casos).
#
# Entrada:  data_raw/tce/inventario_tce_b.csv, data_raw/tce/{pb,pe,pi,ce}/*, data/mandatos.csv,
#           data/pessoas.csv, data/municipios_tse_ibge.csv, data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:    data/tce_gestores_b.csv, data/tce_gestores_b_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/24_tce_gestores_b.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "data_referencia.R")); DATA_REF <- data_referencia(root)  # data fixa da versao, nao o dia da execucao (l. 444)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/24_tce_gestores_b.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/24_tce_gestores_b.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
d10 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
dbr <- function(x) { x <- trimws(as.character(x)); fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", x), paste0(substr(x, 7, 10), "-", substr(x, 4, 5), "-", substr(x, 1, 2)), NA_character_) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
CICLO <- seq(1996L, 2024L, 4L)
# exercicio financeiro (ou ano de uma data dentro do mandato) -> eleicao que o originou (posse em 1o de janeiro)
elei_de <- function(ex) { y <- ((as.integer(ex) - 1L) %/% 4L) * 4L; fifelse(!is.na(y) & y %in% CICLO, y, NA_integer_) }
fim_leg <- function(ano_eleicao) as.IDate(sprintf("%d-12-31", ano_eleicao + 4L))
HOJE <- DATA_REF

## ------------------------------------------------ passo 1: inventario das 15 casas do grupo B
inv <- fread("data_raw/tce/inventario_tce_b.csv", colClasses = "character")
print(inv[, .(uf, tribunal, sistema, http_status, oferece_gestores, acesso)])
registrar_numero("tceb_n_casas_inventariadas", nrow(inv), script = script)
registrar_numero("tceb_n_ufs_inventariadas", inv[, uniqueN(uf)], script = script)
registrar_numero("tceb_n_casas_acesso_aberto", inv[grepl("^aberto", acesso), .N], script = script)
registrar_numero("tceb_n_casas_sem_fonte_de_gestores", inv[!grepl("^sim", oferece_gestores), .N], script = script)
registrar_numero("tceb_n_casas_http_200", inv[http_status == "200", .N], script = script)

## ------------------------------------------------ resolvedor de municipio (uf + nome -> sg_ue, ibge)
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
mun[, nome_norm := norm(nome_ibge)]
mun_u <- mun[, .N, by = .(sg_uf, nome_norm)][N == 1L, .(sg_uf, nome_norm)]
lk_nome <- merge(mun[, .(sg_uf, nome_norm, sg_ue, id_municipio_ibge)], mun_u, by = c("sg_uf", "nome_norm"))
lk_ibge <- unique(mun[, .(id_municipio_ibge, sg_ue_i = sg_ue)])[, if (.N == 1L) .SD else NULL, by = id_municipio_ibge]
N_PREFIXO <- 0L
res_nome <- function(uf, nome) {
  d <- data.table(sg_uf = uf, nome_norm = norm(nome))
  d[ALIAS, on = .(sg_uf, nome_norm = de), nome_norm := i.para]
  d[lk_nome, on = .(sg_uf, nome_norm), `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge)]
  # segunda passada: o nome da fonte e prefixo de exatamente um municipio da UF, terminando em palavra
  # inteira. Cobre as formas curtas dos tribunais (Cajazeiras por Cajazeiras do Piaui, Tamboril por
  # Tamboril do Piaui). Contado em tceb_n_resolvidos_por_prefixo.
  falta <- unique(d[is.na(sg_ue) & nchar(nome_norm) >= 5, .(sg_uf, nome_norm)])
  if (nrow(falta)) {
    cc <- lk_nome[sg_uf %in% falta$sg_uf]
    pr <- rbindlist(lapply(seq_len(nrow(falta)), function(i) {
      k <- cc[sg_uf == falta$sg_uf[i] & startsWith(nome_norm, paste0(falta$nome_norm[i], " "))]
      if (nrow(k) == 1L) data.table(sg_uf = falta$sg_uf[i], nome_norm = falta$nome_norm[i], sg_ue2 = k$sg_ue, ibge2 = k$id_municipio_ibge) else NULL
    }), use.names = TRUE)
    if (length(pr) && nrow(pr)) {
      d[pr, on = .(sg_uf, nome_norm), `:=`(sg_ue = fifelse(is.na(sg_ue), i.sg_ue2, sg_ue),
                                           id_municipio_ibge = fifelse(is.na(id_municipio_ibge), i.ibge2, id_municipio_ibge))]
      N_PREFIXO <<- N_PREFIXO + nrow(pr)
    }
  }
  d[, .(sg_ue, id_municipio_ibge)]
}
res_ibge <- function(ibge) {
  d <- data.table(id_municipio_ibge = as.character(ibge))
  d[lk_ibge, on = "id_municipio_ibge", sg_ue := i.sg_ue_i]
  d[, .(sg_ue, id_municipio_ibge)]
}
# nome da unidade gestora -> tipo e nome do municipio
tipo_ug <- function(ug) { u <- norm(ug); fcase(grepl("^CAMARA", u), "camara", grepl("^PREFEITURA|^MUNICIPIO DE", u), "prefeitura", default = "outra") }
mun_de_ug <- function(ug) trimws(gsub("^(CAMARA|PREFEITURA|MUNICIPIO)( DE VEREADORES| DOS VEREADORES| MUNICIPAL)?( MUNICIPAL)?( DA CIDADE)?( DE| DA| DO| DOS| DAS)?\\s*", "", norm(ug)))
# municipios cujo nome na fonte do TC nao e o nome do IBGE: renomeacao (Santarem/PB virou Joca Claudino em
# 2010) e grafia divergente na base do tribunal. Verificado um a um contra data/municipios_tse_ibge.csv.
ALIAS <- data.table(sg_uf = c("PB", "PE", "PE"), de = c("SANTAREM", "IGUARACY", "SAO CAETANO"),
                    para = c("JOCA CLAUDINO", "IGUARACI", "SAO CAITANO"))
# descricao do cargo -> cargo no vocabulario do BOCEL
cargo_de <- function(x) {
  u <- gsub("^[0-9]+\\s*-\\s*", "", norm(x)); u <- gsub("\\s*-\\s*[A-Z0-9]+$", "", u); u <- trimws(gsub("^C?PREF |^PREF ", "", u))
  fcase(grepl("VICE ?-? ?PREFEIT", u) | grepl("^VICEPREFEIT", u), "VICE-PREFEITO",
        grepl("^PREFEIT", u), "PREFEITO",
        grepl("VEREADOR", u), "VEREADOR",
        default = NA_character_)
}

fontes <- list()
vaziog <- function() data.table(uf = character(), tribunal = character(), fonte = character(), unidade_gestora = character(),
                                tipo_unidade = character(), nome_municipio_fonte = character(), id_municipio_ibge = character(),
                                sg_ue = character(), nome = character(), cpf = character(), cargo_fonte = character(),
                                cargo_bocel = character(), exercicio = integer(), ano_eleicao = integer(),
                                data_inicio = character(), data_fim = character(), situacao_fonte = character(),
                                forma_saida = character(), url = character())

## ------------------------------------------------ PB: folha do Sagres (TCE-PB), 2013-2026
arqs_pb <- list.files("data_raw/tce/pb", pattern = "^gestores_folha_\\d{4}\\.csv$", full.names = TRUE)
cat("PB: arquivos de folha =", length(arqs_pb), "\n")
if (length(arqs_pb)) {
  # o cabecalho da folha muda de 'municipio' para 'nome_municipio' a partir da remessa de 2020
  pb <- rbindlist(lapply(arqs_pb, function(f) {
    x <- fread(f, colClasses = "character")
    if ("nome_municipio" %in% names(x)) setnames(x, "nome_municipio", "municipio")
    x
  }), use.names = TRUE, fill = TRUE)
  registrar_numero("tceb_pb_n_linhas_folha_eletivas", nrow(pb), script = script)
  pb[, `:=`(cargo_bocel = cargo_de(descricao_cargo), tipo_unidade = tipo_ug(descricao_unidade_gestora),
            am = suppressWarnings(as.integer(ano_mes)), nome_norm = norm(nome_servidor))]
  pb <- pb[!is.na(cargo_bocel) & !is.na(am) & am >= 201301L & nchar(nome_norm) >= 5]
  pb[, `:=`(ano_comp = am %/% 100L, mes_comp = am %% 100L)]
  pb[, ano_eleicao := elei_de(ano_comp)]
  pb <- pb[!is.na(ano_eleicao)]
  pb[is.na(municipio) | trimws(municipio) == "", municipio := mun_de_ug(descricao_unidade_gestora)]
  r <- res_nome("PB", pb$municipio)
  pb[, `:=`(sg_ue = r$sg_ue, id_municipio_ibge = r$id_municipio_ibge)]
  # ate onde cada unidade gestora enviou folha dentro da legislatura: guarda contra falha de remessa
  pb[, am_max_ug := max(am), by = .(descricao_unidade_gestora, ano_eleicao)]
  # um vinculo = pessoa x unidade x cargo x legislatura
  pb[, adm := dbr(data_admissao)]
  min_ou_na <- function(x) { x <- x[!is.na(x)]; if (length(x)) min(x) else NA_character_ }
  sp <- pb[, .(am_ini = min(am), am_fim = max(am), n_meses = uniqueN(am), am_max_ug = max(am_max_ug),
               data_admissao = min_ou_na(adm), cargo_fonte = descricao_cargo[1],
               nome = nome_servidor[1], cpf = so_dig(cpf_cnpj[1]), url = url[1],
               sg_ue = sg_ue[1], id_municipio_ibge = id_municipio_ibge[1], municipio = municipio[1]),
           by = .(descricao_unidade_gestora, tipo_unidade, nome_norm, cargo_bocel, ano_eleicao)]
  # o mes de competencia vira intervalo de datas: primeiro dia do primeiro mes, ultimo dia do ultimo mes
  # YYYYMM nao e contagem de meses: am + 3 salta o fim do ano (201911 + 3 = 201914, que nenhuma
  # competencia atinge, e a comparacao passa a aceitar 2 meses de remessa no lugar de 3). nmes()
  # converte a competencia em meses corridos para a guarda contra falha de remessa. (29/ago/2026)
  nmes <- function(a) (a %/% 100L) * 12L + (a %% 100L)
  pdia <- function(a) as.IDate(sprintf("%d-%02d-01", a %/% 100L, a %% 100L))
  udia <- function(a) pdia(fifelse(a %% 100L == 12L, a + 89L, a + 1L)) - 1L
  sp[, `:=`(data_inicio = as.character(pdia(am_ini)), data_fim = as.character(udia(am_fim)))]
  # a data de admissao substitui o inicio quando cai dentro da legislatura e antes do primeiro mes de folha
  sp[, ini_leg := sprintf("%d-01-01", ano_eleicao + 1L)]
  sp[!is.na(data_admissao) & data_admissao >= ini_leg & data_admissao < data_inicio, data_inicio := data_admissao]
  sp[, `:=`(fimleg = fim_leg(ano_eleicao), am_fim_leg = (ano_eleicao + 4L) * 100L + 12L)]
  sp[, forma_saida := fcase(
    fimleg >= HOJE, "nao_observado",                                   # legislatura ainda em curso
    am_fim >= am_fim_leg - 1L, "fim_regular",                          # folha ate novembro ou dezembro do ultimo ano
    am_fim <= am_fim_leg - 3L & nmes(am_max_ug) >= nmes(am_fim) + 3L, "outro",  # saiu antes, com a unidade ainda enviando folha
    default = "nao_observado")]
  sp[, situacao_fonte := sprintf("folha do Sagres: %d meses de competencia, %d a %d (unidade envia ate %d)", n_meses, am_ini, am_fim, am_max_ug)]
  fontes[["pb"]] <- data.table(uf = "PB", tribunal = "TCE-PB", fonte = "tcepb_sagres_folha_cargos_eletivos",
    unidade_gestora = sp$descricao_unidade_gestora, tipo_unidade = sp$tipo_unidade, nome_municipio_fonte = sp$municipio,
    id_municipio_ibge = sp$id_municipio_ibge, sg_ue = sp$sg_ue, nome = sp$nome,
    cpf = fifelse(nchar(sp$cpf) == 11L, sp$cpf, NA_character_), cargo_fonte = sp$cargo_fonte, cargo_bocel = sp$cargo_bocel,
    exercicio = NA_integer_, ano_eleicao = sp$ano_eleicao, data_inicio = sp$data_inicio, data_fim = sp$data_fim,
    situacao_fonte = sp$situacao_fonte, forma_saida = sp$forma_saida, url = sp$url)
}

## ------------------------------------------------ PE: cadastro de servidores (TCE-PE), vinculo Eletivo
le_pe <- function(f) {
  if (!file.exists(f)) return(NULL)
  x <- fromJSON(f, simplifyVector = TRUE)
  y <- as.data.table(x$resposta$conteudo)
  if (!nrow(y)) return(NULL)
  y[, url := x$url]
  y
}
pe <- rbindlist(lapply(list.files("data_raw/tce/pe", pattern = "^servidores_.*\\.json$", full.names = TRUE), le_pe), use.names = TRUE, fill = TRUE)
cat("PE: linhas brutas do cadastro =", nrow(pe), "\n")
if (nrow(pe)) {
  registrar_numero("tceb_pe_n_linhas_cadastro", nrow(pe), script = script)
  pe <- pe[NomeTipoVinculo == "Eletivo"]
  registrar_numero("tceb_pe_n_linhas_vinculo_eletivo", nrow(pe), script = script)
  pe[, `:=`(cargo_bocel = cargo_de(NomeCargo), tipo_unidade = tipo_ug(NomeUJ),
            di = d10(fcoalesce(DataAdmissao, DataIngresso)), df = d10(DataAfastamento))]
  pe <- pe[!is.na(cargo_bocel) & !is.na(di) & nchar(trimws(NomeServidor)) >= 5]
  pe <- unique(pe, by = c("NomeUJ", "NomeServidor", "cargo_bocel", "di", "df"))
  pe[, ano_eleicao := elei_de(as.integer(substr(di, 1, 4)))]
  pe <- pe[!is.na(ano_eleicao)]
  r <- res_nome("PE", mun_de_ug(pe$NomeUJ))
  pe[, `:=`(sg_ue = r$sg_ue, id_municipio_ibge = r$id_municipio_ibge)]
  pe[, `:=`(fimleg = fim_leg(ano_eleicao), af = norm(TipoAtoPessoalAfastamento))]
  pe[, forma_saida := fcase(
    grepl("FALECIMENTO|OBITO", af), "falecimento",
    !is.na(df) & as.IDate(df) >= fimleg - 45L, "fim_regular",
    !is.na(df) & as.IDate(df) <  fimleg - 45L, "outro",
    default = "nao_observado")]
  fontes[["pe"]] <- data.table(uf = "PE", tribunal = "TCE-PE", fonte = "tcepe_dados_abertos_lista_servidores",
    unidade_gestora = pe$NomeUJ, tipo_unidade = pe$tipo_unidade, nome_municipio_fonte = mun_de_ug(pe$NomeUJ),
    id_municipio_ibge = pe$id_municipio_ibge, sg_ue = pe$sg_ue, nome = pe$NomeServidor,
    cpf = fifelse(nchar(so_dig(pe$CPFServidor)) == 11L, so_dig(pe$CPFServidor), NA_character_),
    cargo_fonte = pe$NomeCargo, cargo_bocel = pe$cargo_bocel, exercicio = suppressWarnings(as.integer(pe$AnoRemessa)),
    ano_eleicao = pe$ano_eleicao, data_inicio = pe$di, data_fim = pe$df,
    situacao_fonte = paste0("ingresso: ", pe$TipoAtoPessoalIngresso, "; afastamento: ", fcoalesce(pe$TipoAtoPessoalAfastamento, "sem registro")),
    forma_saida = pe$forma_saida, url = pe$url)
}

## ------------------------------------------------ PI (a): prefeito em exercicio, Portal da Cidadania
f <- "data_raw/tce/pi/gestores_atuais.json"
if (file.exists(f)) {
  x <- as.data.table(fromJSON(f, simplifyVector = TRUE))
  x <- x[!is.na(gestor) & nchar(trimws(gestor)) >= 5]
  x[, di := d10(inicio_gestao)]
  x[, ano_eleicao := elei_de(as.integer(substr(di, 1, 4)))]
  r <- res_ibge(x$codIBGE)
  fontes[["pi_gestor"]] <- data.table(uf = "PI", tribunal = "TCE-PI", fonte = "tcepi_portal_cidadania_gestor",
    unidade_gestora = paste("PREFEITURA MUNICIPAL DE", norm(x$nome_municipio)), tipo_unidade = "prefeitura",
    nome_municipio_fonte = x$nome_municipio, id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue,
    nome = x$gestor, cpf = NA_character_, cargo_fonte = "Gestor (prefeito) em exercicio", cargo_bocel = "PREFEITO",
    exercicio = NA_integer_, ano_eleicao = x$ano_eleicao, data_inicio = x$di, data_fim = NA_character_,
    situacao_fonte = "prefeito em exercicio na consulta", forma_saida = "nao_observado", url = x$url)
}

## ------------------------------------------------ PI (b): responsaveis com contas julgadas irregulares
f <- "data_raw/tce/pi/responsaveis_contas_irregulares.csv"
if (file.exists(f)) {
  x <- fread(f, colClasses = "character")
  x[, eu := norm(ente_unidade)]
  x[, tipo_unidade := fcase(grepl(" CAMARA MUNICIPAL$| CAMARA$", eu), "camara",
                            grepl(" PREFEITURA$| PREFEITURA MUNICIPAL$", eu), "prefeitura", default = "outra")]
  x[, nome_municipio_fonte := trimws(gsub(" (CAMARA MUNICIPAL|CAMARA|PREFEITURA MUNICIPAL|PREFEITURA)$", "", eu))]
  registrar_numero("tceb_pi_n_responsaveis_pdf", nrow(x), script = script)
  registrar_numero("tceb_pi_n_responsaveis_prefeitura_ou_camara", x[tipo_unidade != "outra", .N], script = script)
  x <- x[tipo_unidade != "outra" & nchar(trimws(responsavel)) >= 5]
  r <- res_nome("PI", x$nome_municipio_fonte)
  fontes[["pi_irreg"]] <- data.table(uf = "PI", tribunal = "TCE-PI", fonte = "tcepi_responsaveis_contas_irregulares",
    unidade_gestora = x$eu, tipo_unidade = x$tipo_unidade, nome_municipio_fonte = x$nome_municipio_fonte,
    id_municipio_ibge = r$id_municipio_ibge, sg_ue = r$sg_ue, nome = x$responsavel,
    cpf = fifelse(nchar(so_dig(x$cpf)) == 11L, so_dig(x$cpf), NA_character_),
    cargo_fonte = "Responsavel por contas julgadas irregulares ou com parecer pela reprovacao",
    cargo_bocel = fifelse(x$tipo_unidade == "prefeitura", "PREFEITO", "VEREADOR"),
    exercicio = suppressWarnings(as.integer(x$exercicio)), ano_eleicao = elei_de(x$exercicio),
    data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0(x$julgamento, "; processo ", x$processo, "; transito em julgado ", x$transito_julgado),
    forma_saida = "nao_observado", url = x$url)
}

## ------------------------------------------------ CE: registro de que a API publicada nao devolve linhas
f <- "data_raw/tce/ce/tentativas.json"
if (file.exists(f)) {
  t <- as.data.table(fromJSON(f, simplifyVector = TRUE))
  registrar_numero("tceb_ce_n_consultas_gestores", t[grepl("/gestores", url), .N], script = script)
  registrar_numero("tceb_ce_n_consultas_gestores_vazias", t[grepl("/gestores", url) & elements %in% 0, .N], script = script)
}

## ------------------------------------------------ consolidacao
g <- rbindlist(c(list(vaziog()), fontes), use.names = TRUE, fill = TRUE)
stopifnot(nrow(g) > 0)
g[, nome := trimws(gsub("\\s+", " ", nome))]
g <- g[nchar(nome) >= 5]
g[, nome_normalizado := norm(nome)]
cat("linhas brutas por fonte:\n"); print(g[, .N, by = .(uf, fonte)][order(uf, fonte)])
registrar_numero("tceb_n_registros_brutos", nrow(g), script = script)
registrar_numero("tceb_n_registros_sem_municipio_resolvido", g[is.na(sg_ue), .N], script = script)
registrar_numero("tceb_n_resolvidos_por_prefixo", N_PREFIXO, script = script)
g <- g[!is.na(sg_ue)]
registrar_numero("tceb_n_registros", nrow(g), script = script)
registrar_numero("tceb_n_registros_com_cpf", g[!is.na(cpf), .N], script = script)
registrar_numero("tceb_n_municipios_cobertos", g[, uniqueN(sg_ue)], script = script)
registrar_numero("tceb_n_ufs_com_dado", g[, uniqueN(uf)], script = script)
stopifnot(all(g$forma_saida %in% VOCAB))

## ------------------------------------------------ BOCEL: mandatos de prefeito, vice-prefeito e vereador
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, sg_uf, ano_eleicao = as.integer(ano_eleicao), cd_cargo, cargo)],
              pess[, .(id_pessoa, nome_bocel = nome, nr_cpf)], by = "id_pessoa")
mand[, nome_norm := norm(nome_bocel)]
mand[, cpf_bocel := fifelse(nchar(so_dig(nr_cpf)) == 11L, so_dig(nr_cpf), NA_character_)]
ufs_alvo <- sort(unique(g$uf))
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_(19|20)\\d{2}\\.parquet$", full.names = TRUE), function(fp) {
  x <- as.data.table(read_parquet(fp, col_select = c("ANO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO %in% c("11", "12", "13") & SG_UF %in% ufs_alvo]
}), use.names = TRUE)
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_", CD_CARGO, "_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, unique(cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))]), by = "id_mandato", all.x = TRUE)
mp <- mand[sg_ue %in% unique(g$sg_ue)]
registrar_numero("tceb_n_mandatos_bocel_no_universo", nrow(mp), script = script)

## ------------------------------------------------ pareamento
## Casa municipio + cargo + eleicao; a identidade vem do CPF quando a fonte o expoe (PB e PE mascaram o CPF,
## de modo que na pratica quase tudo cai nas regras de nome) e, senao, do nome civil ou do nome de urna.
g[, rid := .I]
g[, cd_alvo := fcase(cargo_bocel == "PREFEITO", "11", cargo_bocel == "VICE-PREFEITO", "12", default = "13")]
vazio <- function() data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (!is.null(d) && nrow(d)) unique(d[, .(rid, id_mandato, id_pessoa, metodo)], by = "rid") else vazio()
feitos <- integer(0)
# so_com_ano = FALSE e a regra de recurso para fonte que nao informa o ano; ela se aplica somente as linhas
# sem ano de eleicao, para que nenhum pareamento cruze legislaturas
regra <- function(chave_g, chave_m, metodo, so_com_ano = TRUE, dt_m = mp) {
  r <- g[!rid %in% feitos]
  r <- if (so_com_ano) r[!is.na(ano_eleicao)] else r[is.na(ano_eleicao)]
  if (!nrow(r) || !nrow(dt_m)) return(vazio())
  m <- merge(r[, c("rid", chave_g), with = FALSE], dt_m[, c(chave_m, "id_mandato", "id_pessoa"), with = FALSE],
             by.x = chave_g, by.y = chave_m, allow.cartesian = TRUE)
  m <- m[, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (!nrow(m)) return(vazio())
  m[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)]
}
tokreg <- function(col_m, metodo, min_tok, exige4) {
  r <- g[!rid %in% feitos & !is.na(ano_eleicao)]
  dm <- mp[!is.na(get(col_m))]
  if (!nrow(r) || !nrow(dm)) return(vazio())
  cc <- merge(r[, .(rid, sg_ue, cd_alvo, ano_eleicao, nome_normalizado)],
              dm[, c("sg_ue", "cd_cargo", "ano_eleicao", col_m, "id_mandato", "id_pessoa"), with = FALSE],
              by.x = c("sg_ue", "cd_alvo", "ano_eleicao"), by.y = c("sg_ue", "cd_cargo", "ano_eleicao"), allow.cartesian = TRUE)
  if (!nrow(cc)) return(vazio())
  ta <- lapply(strsplit(cc$nome_normalizado, " "), function(t) t[nchar(t) >= 3])
  tb <- lapply(strsplit(cc[[col_m]], " "), function(t) t[nchar(t) >= 3])
  cc[, ok := mapply(function(a, b) length(a) >= min_tok && (!exige4 || any(nchar(a) >= 4L)) && all(a %in% b), ta, tb)]
  d <- cc[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (nrow(d)) d[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)] else vazio()
}
res <- list()
res[[1]] <- regra(c("cpf", "sg_ue", "cd_alvo", "ano_eleicao"), c("cpf_bocel", "sg_ue", "cd_cargo", "ano_eleicao"), "cpf_municipio_cargo_eleicao", TRUE, mp[!is.na(cpf_bocel)]); feitos <- c(feitos, res[[1]]$rid)
res[[2]] <- regra(c("cpf", "sg_ue", "cd_alvo"), c("cpf_bocel", "sg_ue", "cd_cargo"), "cpf_municipio_cargo", FALSE, mp[!is.na(cpf_bocel)]); feitos <- c(feitos, res[[2]]$rid)
res[[3]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_completo_eleicao"); feitos <- c(feitos, res[[3]]$rid)
res[[4]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_urna_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_fonte=nome_urna_eleicao", TRUE, mp[!is.na(nome_urna_norm)]); feitos <- c(feitos, res[[4]]$rid)
res[[5]] <- tokreg("nome_norm", "tokens_nome_fonte_no_nome_civil", 2L, FALSE); feitos <- c(feitos, res[[5]]$rid)
res[[6]] <- tokreg("nome_urna_norm", "tokens_nome_fonte_no_nome_de_urna", 1L, TRUE); feitos <- c(feitos, res[[6]]$rid)
res[[7]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo"), c("nome_norm", "sg_ue", "cd_cargo"), "nome_completo_municipio_cargo", FALSE)
par <- rbindlist(lapply(res, sel), use.names = TRUE)[!duplicated(rid)]
cat("pareamentos por regra:\n"); print(par[, .N, by = metodo][order(-N)])
g[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
g[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]

## um mandato do BOCEL recebe uma unica linha com saida observada por fonte: fica a de periodo mais longo
setorder(g, id_mandato_bocel, fonte, -data_fim, data_inicio, na.last = TRUE)
g[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", dup := seq_len(.N) > 1L, by = .(id_mandato_bocel)]
registrar_numero("tceb_n_linhas_saida_duplicada_descartada", g[dup %in% TRUE, .N], script = script)
g[dup %in% TRUE, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = "descartado_duplicata")]

## ------------------------------------------------ saida
out <- g[, .(uf, tribunal, unidade_gestora, tipo_unidade, id_municipio_ibge, sg_ue, nome, cpf, cargo_fonte, cargo_bocel,
             data_inicio, data_fim, situacao_fonte, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url,
             fonte, nome_municipio_fonte, exercicio, ano_eleicao)]
setorder(out, uf, sg_ue, tipo_unidade, ano_eleicao, nome, na.last = TRUE)
fwrite(out, "data/tce_gestores_b.csv", na = "NA", quote = TRUE)

## cobertura: mandatos do BOCEL por UF/cargo/eleicao contra os pareados
alvo <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13") & sg_uf %in% ufs_alvo,
          .(uf = sg_uf, cargo, ano_eleicao = as.integer(ano_eleicao), id_mandato)]
cob <- alvo[, .(n_bocel = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
pm <- unique(out[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel, com_saida = forma_saida != "nao_observado")])[
        , .(com_saida = any(com_saida)), by = id_mandato]
pm <- merge(pm, alvo, by = "id_mandato")
pc <- pm[, .(n_pareados = uniqueN(id_mandato), n_com_saida = uniqueN(id_mandato[com_saida])), by = .(uf, cargo, ano_eleicao)]
cob <- merge(cob, pc, by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), `:=`(n_pareados = 0L, n_com_saida = 0L)]
cob[, `:=`(taxa = round(n_pareados / n_bocel, 4), taxa_saida = round(n_com_saida / n_bocel, 4))]
setorder(cob, uf, cargo, ano_eleicao)
fwrite(cob, "data/tce_gestores_b_cobertura.csv", na = "NA")

## ------------------------------------------------ numeros
registrar_numero("tceb_n_registros_finais", nrow(out), script = script)
registrar_numero("tceb_n_pareados_linhas", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tceb_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tceb_n_pessoas_pareadas", out[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)], script = script)
registrar_numero("tceb_taxa_pareamento_linhas", round(out[!is.na(id_mandato_bocel), .N] / nrow(out), 4), script = script)
registrar_numero("tceb_n_mandatos_com_saida_observada", out[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tceb_n_mandatos_com_data_inicio", out[!is.na(id_mandato_bocel) & !is.na(data_inicio), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tceb_n_linhas_vinculo_alem_da_legislatura", out[!is.na(data_fim) & !is.na(ano_eleicao) & data_fim > sprintf("%d-12-31", ano_eleicao + 4L), .N], script = script)
for (fs in VOCAB) registrar_numero(paste0("tceb_n_forma_saida_", fs), out[!is.na(id_mandato_bocel) & forma_saida == fs, uniqueN(id_mandato_bocel)], script = script)
for (u in ufs_alvo) {
  registrar_numero(paste0("tceb_n_registros_uf_", u), out[uf == u, .N], script = script)
  registrar_numero(paste0("tceb_n_mandatos_pareados_uf_", u), out[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
  registrar_numero(paste0("tceb_n_municipios_uf_", u), out[uf == u, uniqueN(sg_ue)], script = script)
}
for (cg in sort(unique(out$cargo_bocel))) {
  k <- gsub("[^A-Z]", "", cg)
  registrar_numero(paste0("tceb_n_registros_cargo_", k), out[cargo_bocel == cg, .N], script = script)
  registrar_numero(paste0("tceb_n_mandatos_pareados_cargo_", k), out[cargo_bocel == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
uc <- cob[, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados), n_com_saida = sum(n_com_saida)), by = .(uf, cargo)][, taxa := round(n_pareados / n_bocel, 4)]
for (i in seq_len(nrow(uc))) registrar_numero(paste0("tceb_taxa_", uc$uf[i], "_", gsub("[^A-Z]", "", uc$cargo[i])), uc$taxa[i], script = script)
for (a in sort(unique(out[!is.na(ano_eleicao)]$ano_eleicao))) registrar_numero(paste0("tceb_n_mandatos_pareados_eleicao_", a), out[ano_eleicao == a & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
print(out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)), com_saida = sum(!is.na(id_mandato_bocel) & forma_saida != "nao_observado"), municipios = uniqueN(sg_ue)), by = .(uf, tribunal, fonte)][order(uf, fonte)])
print(uc[order(-n_pareados)]); print(out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)])
print(out[!is.na(id_mandato_bocel), .N, by = .(cargo_bocel, forma_saida)][order(cargo_bocel, -N)])
cat("24_tce_gestores_b: concluido —", nrow(out), "registros,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
sink()
