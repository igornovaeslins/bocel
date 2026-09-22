# 34_tce_gestores_d.R — responsaveis por unidade gestora municipal nos Tribunais de Contas do NORTE e
# NORDESTE que faltavam (GRUPO D: CE, RN, MA, AL, SE, PA/TCM-PA, AM, AC, RO, RR, AP), coletados por
# python/fetch_tce_d.py. Os grupos A (BA, ES, MS, RJ, RS, SC, SP) e B (PB, PE, PI) nao sao tocados aqui.
#
# Diferenca em relacao aos grupos A e B: nenhuma das casas do grupo D publica cadastro de vinculo. O que
# resta publico e a relacao de responsaveis com contas julgadas irregulares (art. 11, par. 5o, da Lei
# 9.504/1997) e, no Maranhao, tambem a de inadimplentes. Essas listas dizem QUEM respondia por uma unidade
# gestora num exercicio; nao dizem quando o mandato terminou nem por que. Por isso a forma de saida de
# TODA linha desta frente e 'nao_observado', e a contribuicao e de pareamento e de cobertura, nao de
# forma de saida — o que fica registrado em tced_n_mandatos_com_saida_observada (zero por construcao).
#
# Regra de cargo, por tipo de unidade gestora:
#   prefeitura (PREFEITURA, MUNICIPIO DE, GABINETE DO PREFEITO, PODER EXECUTIVO) -> PREFEITO
#   camara     (CAMARA MUNICIPAL, CAMARA DO MUNICIPIO, CAMARA DE VEREADORES)     -> VEREADOR
#   outra unidade municipal (fundo, secretaria municipal, autarquia)             -> cargo indeterminado
#   unidade estadual                                                             -> descartada
# Quando a propria fonte informa o cargo (MA e AP), o cargo da fonte prevalece sobre o tipo da unidade.
# As linhas de cargo indeterminado sao pareadas por nome civil completo e municipio, exigindo mandato
# unico, e contadas a parte (tced_n_pareados_cargo_indeterminado) para que o numero seja descontavel.
#
# O municipio vem do campo proprio quando a fonte o tem (CE e PA) e, senao, do sufixo do nome da unidade
# validado contra data/municipios_tse_ibge.csv — so entra sufixo que seja nome de municipio da UF.
#
# Entrada:  data_raw/tce/inventario_tce_d.csv, data_raw/tce/{ce,rn,ma,am,pa,rr,ap,ro}/*, data/mandatos.csv,
#           data/pessoas.csv, data/municipios_tse_ibge.csv, data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:    data/tce_gestores_d.csv, data/tce_gestores_d_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/34_tce_gestores_d.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/34_tce_gestores_d.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/34_tce_gestores_d.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z0-9 ]", " ", x); gsub(" +", " ", trimws(x)) }
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
# coluna que vem inteira NA e lida como logical; o coalesce exige o mesmo tipo
chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
d10 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
dbr <- function(x) { x <- trimws(as.character(x)); fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", x), paste0(substr(x, 7, 10), "-", substr(x, 4, 5), "-", substr(x, 1, 2)), NA_character_) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
CICLO <- seq(1996L, 2024L, 4L)
# exercicio financeiro -> eleicao que o originou (posse em 1o de janeiro do ano seguinte a eleicao)
elei_de <- function(ex) { ex <- suppressWarnings(as.integer(ex)); y <- ((ex - 1L) %/% 4L) * 4L; fifelse(!is.na(y) & y %in% CICLO, y, NA_integer_) }

## ------------------------------------------------ passo 1: inventario das 13 sondas do grupo D
inv <- fread("data_raw/tce/inventario_tce_d.csv", colClasses = "character")
print(inv[, .(uf, tribunal, sistema, http_status, oferece_gestores, acesso)])
registrar_numero("tced_n_sondas_inventariadas", nrow(inv), script = script)
registrar_numero("tced_n_ufs_inventariadas", inv[, uniqueN(uf)], script = script)
registrar_numero("tced_n_sondas_com_fonte_de_responsaveis", inv[grepl("^sim", oferece_gestores), .N], script = script)
registrar_numero("tced_n_ufs_com_fonte_de_responsaveis", inv[grepl("^sim", oferece_gestores), uniqueN(uf)], script = script)
registrar_numero("tced_n_sondas_sem_fonte", inv[!grepl("^sim", oferece_gestores), .N], script = script)
registrar_numero("tced_n_sondas_http_200", inv[http_status == "200", .N], script = script)

## ------------------------------------------------ resolvedor de municipio
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
mun[, nome_norm := norm(nome_ibge)]
mun_u <- mun[, .N, by = .(sg_uf, nome_norm)][N == 1L, .(sg_uf, nome_norm)]
lk_nome <- merge(mun[, .(sg_uf, nome_norm, sg_ue, id_municipio_ibge)], mun_u, by = c("sg_uf", "nome_norm"))
lk_nome[, ntok := lengths(strsplit(nome_norm, " "))]
MAX_TOK <- lk_nome[, max(ntok)]
# municipios cujo nome na fonte do TC nao e o nome do cadastro do BOCEL. Cada par foi conferido um a um
# contra data/municipios_tse_ibge.csv: Itapaje e a grafia atual de Itapage (CE), Santa Izabel e grafia
# do TCM-PA para Santa Isabel do Para, Redencao do Para e a forma longa de Redencao (PA), e as duas
# ultimas sao erros de digitacao do mural do TCE-MA.
ALIAS <- data.table(
  sg_uf = c("CE", "PA", "PA", "MA", "MA"),
  de    = c("ITAPAJE", "SANTA IZABEL DO PARA", "REDENCAO DO PARA", "GOVERNADOR NEWTON BELO", "VILA NOVA DOS MATIRIOS"),
  para  = c("ITAPAGE", "SANTA ISABEL DO PARA", "REDENCAO", "GOVERNADOR NEWTON BELLO", "VILA NOVA DOS MARTIRIOS"))

res_nome <- function(uf, nome) {
  d <- data.table(i = seq_along(nome), sg_uf = uf, nome_norm = norm(nome))
  d[ALIAS, on = .(sg_uf, nome_norm = de), nome_norm := i.para]
  d[lk_nome, on = .(sg_uf, nome_norm), `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge)]
  setorder(d, i)
  d[, .(sg_ue, id_municipio_ibge)]
}
# o municipio pelo SUFIXO do nome da unidade gestora, validado contra o cadastro: so entra o sufixo que
# for nome de municipio da UF. Testa do sufixo mais longo para o mais curto, para que 'SANTAREM NOVO' nao
# seja lido como 'NOVO'. Contado em tced_n_resolvidos_por_sufixo.
N_SUFIXO <- 0L
res_sufixo <- function(uf, texto) {
  d <- data.table(i = seq_along(texto), sg_uf = uf, t = norm(texto))
  d[, tok := strsplit(t, " ")]
  d[, ntok_t := lengths(tok)]
  d[, `:=`(sg_ue = NA_character_, id_municipio_ibge = NA_character_)]
  CONECT <- c("DE", "DO", "DA", "DOS", "DAS", "D")
  for (k in seq(MAX_TOK, 1L)) {
    falta <- d[is.na(sg_ue) & ntok_t >= k]
    if (!nrow(falta)) next
    # o sufixo so vale se for a string inteira ou vier depois de um conectivo ('... DE <municipio>'),
    # para que 'FUNDO MUNICIPAL DE SAUDE' nao vire o municipio 'Saude'
    falta[, cand := vapply(tok, function(x) paste(tail(x, k), collapse = " "), character(1))]
    falta[, ok := vapply(seq_len(.N), function(j) ntok_t[j] == k || tok[[j]][ntok_t[j] - k] %in% CONECT, logical(1))]
    falta <- falta[ok == TRUE]
    if (!nrow(falta)) next
    falta[ALIAS, on = .(sg_uf, cand = de), cand := i.para]
    m <- merge(falta[, .(i, sg_uf, cand)], lk_nome[ntok == k, .(sg_uf, nome_norm, sg_ue, id_municipio_ibge)],
               by.x = c("sg_uf", "cand"), by.y = c("sg_uf", "nome_norm"))
    if (nrow(m)) {
      d[m, on = "i", `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge)]
      N_SUFIXO <<- N_SUFIXO + nrow(m)
    }
  }
  setorder(d, i)
  d[, .(sg_ue, id_municipio_ibge)]
}
coalesce_res <- function(a, b) data.table(sg_ue = fcoalesce(a$sg_ue, b$sg_ue),
                                          id_municipio_ibge = fcoalesce(a$id_municipio_ibge, b$id_municipio_ibge))

# tipo da unidade gestora a partir do nome
tipo_ug <- function(ug) {
  u <- norm(ug)
  fcase(grepl("^CAMARA|CAMARA MUNICIPAL|CAMARA DE VEREADORES|CAMARA DO MUNICIPIO", u), "camara",
        grepl("^PREFEITURA|^MUNICIPIO DE|^MUNICIPIO DO|^GABINETE DO PREFEITO|^GABINETE DA PREFEITURA|^ADMINISTRACAO DO GABINETE DO PREFEITO|^PODER EXECUTIVO|^CHEFIA DE GABINETE|^GABINETE CIVIL", u), "prefeitura",
        grepl("ESTADO|ESTADUAL|^SECRETARIA DE ESTADO|^GOVERNO DO|^ASSEMBLEIA|^TRIBUNAL|^MINISTERIO|^UNIVERSIDADE|^DEPARTAMENTO DE POLICIA FEDERAL|^POLICIA MILITAR|^CASA CIVIL|^COMPANHIA|^AGENCIA", u), "estadual",
        default = "outra")
}
# cargo declarado pela fonte -> cargo no vocabulario do BOCEL (NA quando nao e cargo eletivo municipal)
cargo_de <- function(x) {
  u <- norm(x)
  fcase(grepl("VICE ?-? ?PREFEIT|^VICEPREFEIT", u), "VICE-PREFEITO",
        grepl("^PREFEIT|PREFEITO MUNICIPAL|PREFEITA MUNICIPAL|PREFEIT[OA] DO MUNICIPIO", u), "PREFEITO",
        grepl("^VEREADOR|PRESIDENTE DA CAMARA|^PRESIDENTE DE CAMARA", u), "VEREADOR",
        default = NA_character_)
}

fontes <- list()
vaziog <- function() data.table(uf = character(), tribunal = character(), fonte = character(), unidade_gestora = character(),
                                tipo_unidade = character(), nome_municipio_fonte = character(), id_municipio_ibge = character(),
                                sg_ue = character(), nome = character(), cpf = character(), cargo_fonte = character(),
                                cargo_bocel = character(), exercicio = integer(), ano_eleicao = integer(),
                                data_inicio = character(), data_fim = character(), situacao_fonte = character(),
                                forma_saida = character(), url = character())
mk <- function(uf, trib, fonte, ug, tipo, mun_fonte, res, nome, cpf, cargo_fonte, cargo, exerc, ano, sit, url) {
  cpf <- so_dig(cpf)
  data.table(uf = uf, tribunal = trib, fonte = fonte, unidade_gestora = ug, tipo_unidade = tipo,
             nome_municipio_fonte = mun_fonte, id_municipio_ibge = res$id_municipio_ibge, sg_ue = res$sg_ue,
             nome = nome, cpf = fifelse(nchar(cpf) == 11L, cpf, NA_character_), cargo_fonte = cargo_fonte,
             cargo_bocel = cargo, exercicio = suppressWarnings(as.integer(exerc)), ano_eleicao = ano,
             data_inicio = NA_character_, data_fim = NA_character_, situacao_fonte = sit,
             forma_saida = "nao_observado", url = url)
}
le_json <- function(f) { if (!file.exists(f)) return(NULL); x <- fromJSON(f, simplifyVector = TRUE); e <- as.data.table(x$elements); if (!nrow(e)) return(NULL); e[, url := x$url]; e[] }

## ------------------------------------------------ CE: Lista de Contas Irregulares (planilha)
f <- "data_raw/tce/ce/contas_irregulares.csv"
if (file.exists(f)) {
  ce <- fread(f, colClasses = "character")
  registrar_numero("tced_ce_n_linhas_planilha", nrow(ce), script = script)
  registrar_numero("tced_ce_n_linhas_localidade_estado", ce[norm(localidade) %in% c("CEARA", ""), .N], script = script)
  ce <- ce[!norm(localidade) %in% c("CEARA", "") & nchar(trimws(nome)) >= 5]
  r <- res_nome("CE", ce$localidade)
  fontes[["ce"]] <- mk("CE", "TCE-CE", "tcece_lista_contas_irregulares", NA_character_, "outra", ce$localidade, r,
                       ce$nome, NA, ce$especie, NA_character_, NA, NA_integer_,
                       paste0(ce$especie, "; processo ", ce$processo, "; transito em julgado ", ce$transito_julgado,
                              "; debito ", ce$debito), ce$url)
}

## ------------------------------------------------ RN: relacao de responsaveis (CPF completo)
f <- "data_raw/tce/rn/contas_irregulares.csv"
if (file.exists(f)) {
  rn <- fread(f, colClasses = "character")
  registrar_numero("tced_rn_n_linhas", nrow(rn), script = script)
  # 'PREF.MUN.X' e a forma curta do cadastro do TCE-RN
  rn[, ug := gsub("^PREF ?MUN ?", "PREFEITURA MUNICIPAL DE ", norm(orgao))]
  rn[, tipo_unidade := tipo_ug(ug)]
  rn <- rn[tipo_unidade != "estadual" & nchar(trimws(responsavel)) >= 5]
  r <- res_sufixo("RN", rn$ug)
  fontes[["rn"]] <- mk("RN", "TCE-RN", "tcern_relacao_responsaveis_contas_irregulares", rn$orgao, rn$tipo_unidade,
                       NA_character_, r, rn$responsavel, rn$cpf, NA_character_,
                       fifelse(rn$tipo_unidade == "camara", "VEREADOR", fifelse(rn$tipo_unidade == "prefeitura", "PREFEITO", NA_character_)),
                       NA, NA_integer_,
                       paste0("inelegibilidade de ", rn$data_inicial, " a ", rn$data_final), rn$url)
}

## ------------------------------------------------ MA: mural de gestores irregulares
ma1 <- le_json("data_raw/tce/ma/responsaveisirregulares.json")
if (!is.null(ma1)) {
  registrar_numero("tced_ma_n_linhas_irregulares", nrow(ma1), script = script)
  ma1[, tipo_unidade := tipo_ug(origem)]
  ma1[, cargo_bocel := cargo_de(cargo)]
  ma1[is.na(cargo_bocel) & tipo_unidade == "camara", cargo_bocel := "VEREADOR"]
  ma1[is.na(cargo_bocel) & tipo_unidade == "prefeitura", cargo_bocel := "PREFEITO"]
  ma1 <- ma1[tipo_unidade != "estadual" & nchar(trimws(nomeGestor)) >= 5]
  r <- res_sufixo("MA", ma1$origem)
  fontes[["ma_irreg"]] <- mk("MA", "TCE-MA", "tcema_mural_responsaveis_irregulares", ma1$origem, ma1$tipo_unidade,
                             NA_character_, r, ma1$nomeGestor, NA, ma1$cargo, ma1$cargo_bocel, ma1$exercicio,
                             elei_de(ma1$exercicio),
                             paste0(ma1$resultadoDeliberacao, "; processo ", ma1$numeroProcesso, "/", ma1$anoProcesso,
                                    "; transito ", ma1$dataTransito), ma1$url)
}
ma2 <- le_json("data_raw/tce/ma/responsaveisinadimplentes.json")
if (!is.null(ma2)) {
  registrar_numero("tced_ma_n_linhas_inadimplentes", nrow(ma2), script = script)
  ma2[, tipo_unidade := tipo_ug(entidade)]
  ma2[, cargo_bocel := cargo_de(cargo)]
  ma2[is.na(cargo_bocel) & tipo_unidade == "camara", cargo_bocel := "VEREADOR"]
  ma2[is.na(cargo_bocel) & tipo_unidade == "prefeitura", cargo_bocel := "PREFEITO"]
  ma2 <- ma2[tipo_unidade != "estadual" & !is.na(responsavel) & nchar(trimws(responsavel)) >= 5]
  # a lista de inadimplentes tem coluna propria de municipio; o sufixo da entidade e o recurso
  r <- coalesce_res(res_nome("MA", ma2$municipio), res_sufixo("MA", ma2$entidade))
  fontes[["ma_inad"]] <- mk("MA", "TCE-MA", "tcema_mural_responsaveis_inadimplentes", ma2$entidade, ma2$tipo_unidade,
                            ma2$municipio, r, ma2$responsavel, NA, ma2$cargo, ma2$cargo_bocel, ma2$exercicio,
                            elei_de(ma2$exercicio),
                            paste0("inadimplente; ", ma2$tipoPrestacao, "; ato declaratorio ", ma2$atoDeclaratorio), ma2$url)
}

## ------------------------------------------------ AM: duas listas do portal da transparencia
for (nm in c("contas_irregulares", "contas_irregulares_fins_eleitorais")) {
  am <- le_json(file.path("data_raw/tce/am", paste0(nm, ".json")))
  if (is.null(am)) next
  registrar_numero(paste0("tced_am_n_linhas_", nm), nrow(am), script = script)
  am[, tipo_unidade := tipo_ug(orgao)]
  am <- am[tipo_unidade != "estadual" & !is.na(responsavel) & nchar(trimws(responsavel)) >= 5]
  if (!nrow(am)) next
  r <- res_sufixo("AM", am$orgao)
  fontes[[paste0("am_", nm)]] <- mk("AM", "TCE-AM", paste0("tceam_", nm), am$orgao, am$tipo_unidade, NA_character_, r,
                                    am$responsavel, am$cpf, NA_character_,
                                    fifelse(am$tipo_unidade == "camara", "VEREADOR", fifelse(am$tipo_unidade == "prefeitura", "PREFEITO", NA_character_)),
                                    am$exercicio, elei_de(am$exercicio),
                                    paste0(am$natureza, "; ", fcoalesce(chr(am$julgamento_acordao), "sem julgamento registrado"),
                                           "; ", fcoalesce(chr(am$numero_acordao_decisao), "sem acordao")), am$url)
}

## ------------------------------------------------ PA: grade de contas irregulares do TCM-PA
f <- "data_raw/tce/pa/tcmpa_contas_irregulares.csv"
if (file.exists(f)) {
  pa <- fread(f, colClasses = "character")
  registrar_numero("tced_pa_n_linhas_grade", nrow(pa), script = script)
  pa[, tipo_unidade := tipo_ug(orgao)]
  # CORRECAO DO VERIFICADOR (R/verifica_tce_norte_nordeste.R, checagem 64): o filtro de unidade estadual
  # estava declarado no cabecalho e aplicado nas outras sete fontes, mas faltava aqui, e 5 linhas de
  # 'AGENCIA DISTRITAL' de Belem sobreviviam com tipo_unidade == 'estadual' na saida (nenhuma pareada).
  # PENDENCIA: essas agencias distritais sao orgaos MUNICIPAIS de Belem, e nao estaduais; quem
  # as tipou assim foi o padrao '^AGENCIA' de tipo_ug(). Rever o padrao e decisao de taxonomia, nao mecanica.
  pa <- pa[tipo_unidade != "estadual" & nchar(trimws(ordenador)) >= 5 & nchar(trimws(municipio)) >= 3]
  r <- res_nome("PA", pa$municipio)
  fontes[["pa"]] <- mk("PA", "TCM-PA", "tcmpa_contas_irregulares", pa$orgao, pa$tipo_unidade, pa$municipio, r,
                       pa$ordenador, NA, NA_character_,
                       fifelse(pa$tipo_unidade == "camara", "VEREADOR", fifelse(pa$tipo_unidade == "prefeitura", "PREFEITO", NA_character_)),
                       pa$exercicio, elei_de(pa$exercicio),
                       paste0(pa$tipo_ato, " ", pa$num_ato, "; processo ", pa$processo_principal,
                              "; publicacao ", pa$publicacao_formatada), pa$url)
}

## ------------------------------------------------ RR: responsabilizacoes publicas
rr <- le_json("data_raw/tce/rr/responsabilizacoes_publicas.json")
if (!is.null(rr)) {
  registrar_numero("tced_rr_n_linhas", nrow(rr), script = script)
  rr[, tipo_unidade := tipo_ug(orgao)]
  rr <- rr[tipo_unidade != "estadual" & !is.na(responsavel) & nchar(trimws(responsavel)) >= 5]
  r <- res_sufixo("RR", rr$orgao)
  fontes[["rr"]] <- mk("RR", "TCE-RR", "tcerr_responsabilizacoes_publicas", rr$orgao, rr$tipo_unidade, NA_character_, r,
                       rr$responsavel, NA, NA_character_,
                       fifelse(rr$tipo_unidade == "camara", "VEREADOR", fifelse(rr$tipo_unidade == "prefeitura", "PREFEITO", NA_character_)),
                       rr$exercicio, elei_de(rr$exercicio),
                       paste0(fcoalesce(chr(rr$descricaoFinalidadeCondenacao), "sem finalidade registrada"),
                              "; ", fcoalesce(chr(rr$deliberacao), "sem deliberacao"),
                              "; protocolo ", fcoalesce(chr(rr$protocoloFormatado), "sem protocolo")), rr$url)
}

## ------------------------------------------------ AP: contas irregulares (cargo por extenso)
f <- "data_raw/tce/ap/contas_irregulares.csv"
if (file.exists(f)) {
  ap <- fread(f, colClasses = "character")
  registrar_numero("tced_ap_n_linhas", nrow(ap), script = script)
  ap[, cargo_bocel := cargo_de(cargo_fonte)]
  ap[, tipo_unidade := fcase(cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO"), "prefeitura",
                             cargo_bocel == "VEREADOR", "camara",
                             grepl("MUNICIP", norm(cargo_fonte)), "outra", default = "estadual")]
  ap <- ap[tipo_unidade != "estadual" & nchar(trimws(responsavel)) >= 5]
  r <- res_sufixo("AP", ap$cargo_fonte)
  fontes[["ap"]] <- mk("AP", "TCE-AP", "tceap_contas_irregulares", ap$cargo_fonte, ap$tipo_unidade, NA_character_, r,
                       ap$responsavel, NA, ap$cargo_fonte, ap$cargo_bocel, NA, NA_integer_,
                       paste0(ap$tipo_julgamento, "; ", ap$descricao, "; processo ", ap$processo,
                              "; julgamento ", ap$data_julgamento), ap$url)
}

## ------------------------------------------------ RO: lista de 2016 (CPF completo)
f <- "data_raw/tce/ro/contas_julgadas_irregulares.csv"
if (file.exists(f)) {
  ro <- fread(f, colClasses = "character")
  registrar_numero("tced_ro_n_linhas_pdf", nrow(ro), script = script)
  ro[, tipo_unidade := tipo_ug(interessado)]
  ro <- ro[tipo_unidade != "estadual" & nchar(trimws(nome)) >= 5]
  r <- res_sufixo("RO", ro$interessado)
  fontes[["ro"]] <- mk("RO", "TCE-RO", "tcero_lista_contas_julgadas_irregulares", ro$interessado, ro$tipo_unidade,
                       NA_character_, r, ro$nome, ro$cpf, NA_character_,
                       fifelse(ro$tipo_unidade == "camara", "VEREADOR", fifelse(ro$tipo_unidade == "prefeitura", "PREFEITO", NA_character_)),
                       ro$exercicio, elei_de(ro$exercicio),
                       paste0(ro$assunto, "; processo ", ro$processo, "/", ro$ano_processo,
                              "; acordao ", ro$nr_acordao, "/", ro$ano_acordao), ro$url)
}

## ------------------------------------------------ consolidacao
g <- rbindlist(c(list(vaziog()), fontes), use.names = TRUE, fill = TRUE)
stopifnot(nrow(g) > 0)
g[, nome := trimws(gsub("\\s+", " ", nome))]
g <- g[nchar(nome) >= 5]
g[, nome_normalizado := norm(nome)]
cat("linhas brutas por fonte:\n"); print(g[, .N, by = .(uf, fonte)][order(uf, fonte)])
registrar_numero("tced_n_registros_brutos", nrow(g), script = script)
registrar_numero("tced_n_registros_sem_municipio_resolvido", g[is.na(sg_ue), .N], script = script)
registrar_numero("tced_n_resolvidos_por_sufixo", N_SUFIXO, script = script)
g <- g[!is.na(sg_ue)]
registrar_numero("tced_n_registros", nrow(g), script = script)
registrar_numero("tced_n_registros_com_cpf", g[!is.na(cpf), .N], script = script)
# CORRECAO DO VERIFICADOR (checagem 63): a chave dizia 'com_exercicio' e contava ano_eleicao DERIVADA, que
# perde o registro de exercicio 1900; agora cada nome conta o que promete
registrar_numero("tced_n_registros_com_exercicio", g[!is.na(exercicio), .N], script = script)
registrar_numero("tced_n_registros_com_ano_eleicao_derivada", g[!is.na(ano_eleicao), .N], script = script)
registrar_numero("tced_n_registros_exercicio_anterior_a_1990", g[!is.na(exercicio) & exercicio < 1990L, .N], script = script)
registrar_numero("tced_n_registros_cargo_indeterminado", g[is.na(cargo_bocel), .N], script = script)
registrar_numero("tced_n_municipios_cobertos", g[, uniqueN(sg_ue)], script = script)
registrar_numero("tced_n_ufs_com_dado", g[, uniqueN(uf)], script = script)
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
registrar_numero("tced_n_mandatos_bocel_no_universo", nrow(mp), script = script)

## ------------------------------------------------ pareamento
## Casa municipio + cargo + eleicao; a identidade vem do CPF quando a fonte o expoe (RN, RO e parte de AM)
## e, senao, do nome civil ou do nome de urna. A linha de cargo indeterminado (fundo, secretaria) so casa
## por nome civil completo dentro do municipio, e apenas quando ha um unico mandato candidato.
g[, rid := .I]
g[, cd_alvo := fcase(cargo_bocel == "PREFEITO", "11", cargo_bocel == "VICE-PREFEITO", "12", cargo_bocel == "VEREADOR", "13",
                     default = NA_character_)]
vazio <- function() data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (!is.null(d) && nrow(d)) unique(d[, .(rid, id_mandato, id_pessoa, metodo)], by = "rid") else vazio()
feitos <- integer(0)
# so_com_ano = FALSE e a regra de recurso para fonte que nao informa o exercicio; ela se aplica somente as
# linhas sem ano de eleicao, para que nenhum pareamento com ano cruze legislaturas
regra <- function(chave_g, chave_m, metodo, so_com_ano = TRUE, dt_g = NULL, dt_m = mp) {
  r <- if (is.null(dt_g)) g else dt_g
  r <- r[!rid %in% feitos]
  r <- if (so_com_ano) r[!is.na(ano_eleicao)] else r[is.na(ano_eleicao)]
  if (!nrow(r) || !nrow(dt_m)) return(vazio())
  m <- merge(r[, c("rid", chave_g), with = FALSE], dt_m[, c(chave_m, "id_mandato", "id_pessoa"), with = FALSE],
             by.x = chave_g, by.y = chave_m, allow.cartesian = TRUE)
  m <- m[, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (!nrow(m)) return(vazio())
  m[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)]
}
tokreg <- function(col_m, metodo, min_tok, exige4) {
  r <- g[!rid %in% feitos & !is.na(ano_eleicao) & !is.na(cd_alvo)]
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
gc_ <- function() g[!is.na(cd_alvo)]      # linhas com cargo determinado
gi_ <- function() g[is.na(cd_alvo)]       # linhas de unidade sem cargo determinado
res <- list()
res[[1]] <- regra(c("cpf", "sg_ue", "cd_alvo", "ano_eleicao"), c("cpf_bocel", "sg_ue", "cd_cargo", "ano_eleicao"), "cpf_municipio_cargo_eleicao", TRUE, gc_(), mp[!is.na(cpf_bocel)]); feitos <- c(feitos, res[[1]]$rid)
res[[2]] <- regra(c("cpf", "sg_ue", "cd_alvo"), c("cpf_bocel", "sg_ue", "cd_cargo"), "cpf_municipio_cargo", FALSE, gc_(), mp[!is.na(cpf_bocel)]); feitos <- c(feitos, res[[2]]$rid)
res[[3]] <- regra(c("cpf", "sg_ue"), c("cpf_bocel", "sg_ue"), "cpf_municipio", FALSE, gi_(), mp[!is.na(cpf_bocel)]); feitos <- c(feitos, res[[3]]$rid)
res[[4]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_completo_eleicao", TRUE, gc_()); feitos <- c(feitos, res[[4]]$rid)
res[[5]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"), c("nome_urna_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_fonte=nome_urna_eleicao", TRUE, gc_(), mp[!is.na(nome_urna_norm)]); feitos <- c(feitos, res[[5]]$rid)
res[[6]] <- tokreg("nome_norm", "tokens_nome_fonte_no_nome_civil", 2L, FALSE); feitos <- c(feitos, res[[6]]$rid)
res[[7]] <- tokreg("nome_urna_norm", "tokens_nome_fonte_no_nome_de_urna", 1L, TRUE); feitos <- c(feitos, res[[7]]$rid)
res[[8]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo"), c("nome_norm", "sg_ue", "cd_cargo"), "nome_completo_municipio_cargo", FALSE, gc_()); feitos <- c(feitos, res[[8]]$rid)
# cargo indeterminado (fundo, secretaria municipal): nome civil completo dentro do municipio, exigindo
# mandato unico entre os tres cargos. Quando a fonte informa o exercicio, a eleicao entra na chave, para
# que o pareamento nao atravesse legislaturas.
res[[9]] <- regra(c("nome_normalizado", "sg_ue", "ano_eleicao"), c("nome_norm", "sg_ue", "ano_eleicao"), "nome_completo_municipio_eleicao_cargo_indeterminado", TRUE, gi_())
feitos <- c(feitos, res[[9]]$rid)
res[[10]] <- regra(c("nome_normalizado", "sg_ue"), c("nome_norm", "sg_ue"), "nome_completo_municipio_cargo_indeterminado", FALSE, gi_())
par <- rbindlist(lapply(res, sel), use.names = TRUE)[!duplicated(rid)]
cat("pareamentos por regra:\n"); print(par[, .N, by = metodo][order(-N)])
g[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
g[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]

## ------------------------------------------------ saida
out <- g[, .(uf, tribunal, unidade_gestora, tipo_unidade, id_municipio_ibge, sg_ue, nome, cpf, cargo_fonte, cargo_bocel,
             data_inicio, data_fim, situacao_fonte, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url,
             fonte, nome_municipio_fonte, exercicio, ano_eleicao)]
setorder(out, uf, sg_ue, tipo_unidade, ano_eleicao, nome, na.last = TRUE)
fwrite(out, "data/tce_gestores_d.csv", na = "NA", quote = TRUE)

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
fwrite(cob, "data/tce_gestores_d_cobertura.csv", na = "NA")

## ------------------------------------------------ numeros
registrar_numero("tced_n_registros_finais", nrow(out), script = script)
registrar_numero("tced_n_pareados_linhas", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tced_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tced_n_pessoas_pareadas", out[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)], script = script)
registrar_numero("tced_taxa_pareamento_linhas", round(out[!is.na(id_mandato_bocel), .N] / nrow(out), 4), script = script)
registrar_numero("tced_n_mandatos_com_saida_observada", out[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tced_n_pareados_cargo_indeterminado", out[!is.na(id_mandato_bocel) & is.na(cargo_bocel), .N], script = script)
registrar_numero("tced_n_mandatos_pareados_cargo_indeterminado", out[!is.na(id_mandato_bocel) & is.na(cargo_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tced_n_pareados_por_cpf", out[grepl("^cpf", metodo_pareamento), .N], script = script)
registrar_numero("tced_n_municipios_com_mandato_pareado", out[!is.na(id_mandato_bocel), uniqueN(sg_ue)], script = script)
for (fs in VOCAB) registrar_numero(paste0("tced_n_forma_saida_", fs), out[!is.na(id_mandato_bocel) & forma_saida == fs, uniqueN(id_mandato_bocel)], script = script)
for (u in ufs_alvo) {
  registrar_numero(paste0("tced_n_registros_uf_", u), out[uf == u, .N], script = script)
  registrar_numero(paste0("tced_n_mandatos_pareados_uf_", u), out[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
  registrar_numero(paste0("tced_n_municipios_uf_", u), out[uf == u, uniqueN(sg_ue)], script = script)
  registrar_numero(paste0("tced_taxa_pareamento_uf_", u), round(out[uf == u & !is.na(id_mandato_bocel), .N] / max(1L, out[uf == u, .N]), 4), script = script)
}
for (cg in sort(unique(out[!is.na(cargo_bocel)]$cargo_bocel))) {
  k <- gsub("[^A-Z]", "", cg)
  registrar_numero(paste0("tced_n_registros_cargo_", k), out[cargo_bocel == cg, .N], script = script)
  registrar_numero(paste0("tced_n_mandatos_pareados_cargo_", k), out[cargo_bocel == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
uc <- cob[, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados)), by = .(uf, cargo)][, taxa := round(n_pareados / n_bocel, 4)]
for (i in seq_len(nrow(uc))) registrar_numero(paste0("tced_taxa_", uc$uf[i], "_", gsub("[^A-Z]", "", uc$cargo[i])), uc$taxa[i], script = script)
for (a in sort(unique(out[!is.na(ano_eleicao)]$ano_eleicao))) registrar_numero(paste0("tced_n_mandatos_pareados_eleicao_", a), out[ano_eleicao == a & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
print(out[, .(linhas = .N, pareadas = sum(!is.na(id_mandato_bocel)), municipios = uniqueN(sg_ue)), by = .(uf, tribunal, fonte)][order(uf, fonte)])
print(uc[order(-n_pareados)]); print(out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)])
print(out[, .N, by = .(tipo_unidade, pareado = !is.na(id_mandato_bocel))][order(tipo_unidade, -N)])
cat("34_tce_gestores_d: concluido —", nrow(out), "registros,", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
sink()
