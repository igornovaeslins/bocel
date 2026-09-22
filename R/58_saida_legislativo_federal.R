# 58_saida_legislativo_federal.R — forma de saida do mandato de senador e de deputado federal pela historia
# completa de exercicio registrada pela propria casa (13/09/2026)
#
# Por que existe. A integracao gravava como forma de saida a causa do ultimo periodo de exercicio, de modo que o
# parlamentar licenciado para ser ministro ou secretario, e que ainda nao voltou ou voltou so depois, aparecia com
# o mandato encerrado por licenca ou afastamento. Licenca e afastamento temporario nao encerram mandato (CF, art.
# 56), e a decisao na pendencia 1 exige o registro de volta antes de chamar licenca de saida. As APIs do
# Senado e da Camara trazem cada periodo de exercicio com inicio, fim e causa, o que permite separar o ato que
# encerra o mandato do periodo fora do exercicio.
#
# Regra por mandato do TSE:
#   1. o primeiro ato definitivo dentro do mandato (renuncia, falecimento, perda do mandato, cassacao do diploma,
#      aposentadoria) encerra o mandato na data do ato;
#   2. sem ato definitivo, o mandato ja vencido terminou no prazo (fim_regular na data convencional), mesmo que o
#      titular estivesse licenciado no fim;
#   3. sem ato definitivo e com o mandato ainda correndo, o mandato esta em curso (sem forma de saida);
#   4. cada periodo fora do exercicio por licenca, afastamento, suspensao ou decisao judicial vai para a tabela de
#      interregnos, com a data de volta quando a casa a registra.
# A legislatura 51 da Camara (eleicao de 1998) chega da API sem eventos e fica marcada como sem historico, para a
# fonte seguinte (biografia oficial do deputado) preencher.
#
# Entrada:  data/exercicio_senado.csv e data/exercicio_camara.csv (R/07), data/mandatos.csv (cd_cargo 5 e 6)
# Saida:    data/saida_legislativo_federal.csv (uma linha por mandato), data/interregnos_legislativo_federal.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/58_saida_legislativo_federal.R
set.seed(20260913)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
source(file.path(root, "lib", "asserts_rigor.R"))
script <- "R/58_saida_legislativo_federal.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
# data de referencia fixa do banco (a mesma do R/53), para a reconstrucao em outro dia nao mudar o que esta em curso
f_ref <- "output/data_referencia.txt"
HOJE <- as.IDate(if (file.exists(f_ref)) trimws(readLines(f_ref, warn = FALSE)[1]) else format(Sys.Date(), "%Y-%m-%d"))

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("5", "6")]

## ---------------------------------------------------------------- classificacao das causas
# Senado: SiglaCausaAfastamento da API de Dados Abertos
DEF_SENADO <- c(REN = "renuncia", FAL = "falecimento", PER = "cassacao", CAS = "cassacao")
TEMP_SENADO <- c(LCS = "licenca", LSP = "licenca", LS = "licenca", LP = "licenca", AFO = "afastamento",
                 DJ = "afastamento_decisao_judicial")
# Camara: descricao do evento de saida no historico do deputado
classe_camara <- function(txt) fcase(
  grepl("Término da Legislatura", txt), "fim",
  grepl("definitivo - Renúncia", txt), "renuncia",
  grepl("definitivo - Falecimento|dataFalecimento", txt), "falecimento",
  grepl("definitivo - Perda de Mandato", txt), "cassacao",
  grepl("definitivo - Aposentadoria", txt), "aposentadoria",
  grepl("Suspensão", txt), "suspensao",
  grepl("Decisão Judicial", txt), "afastamento_decisao_judicial",
  grepl("Licença", txt), "licenca",
  grepl("Afastamento (sem|com) prazo determinado", txt), "afastamento",
  default = NA_character_)
DEFINITIVO <- c("renuncia", "falecimento", "cassacao", "aposentadoria")
TEMPORARIO <- c("licenca", "afastamento", "afastamento_decisao_judicial", "suspensao")

## ---------------------------------------------------------------- periodos de exercicio do titular
se <- fread("data/exercicio_senado.csv", colClasses = "character", na.strings = "NA")
ps <- se[!is.na(id_mandato),
         .(casa = "senado", id_mandato, id_pessoa, ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio),
           classe = fcase(sigla_causa == "TER", "fim", sigla_causa %chin% names(DEF_SENADO), unname(DEF_SENADO[sigla_causa]),
                          sigla_causa %chin% names(TEMP_SENADO), unname(TEMP_SENADO[sigla_causa]), default = NA_character_),
           causa_original = causa_afastamento, fonte = "senado_api", url_fonte, legislatura)]
ca <- fread("data/exercicio_camara.csv", colClasses = "character", na.strings = "NA")
# 13/09/2026: o mandato do TSE vale mesmo quando a Camara registra a pessoa como suplente que assumiu depois
# (retotalizacao ou decisao judicial: Marcos Rogerio, RO 2010, empossado em 17/11/2011; Priscila Costa, CE 2022)
pc <- ca[!is.na(id_mandato),
         .(casa = "camara", id_mandato, id_pessoa, ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio),
           classe = classe_camara(descricao_saida), causa_original = descricao_saida, fonte = "camara_api", url_fonte,
           legislatura, sem_historico = situacao_final %chin% c("listado_sem_historico", "sem_registro_de_saida", "nao_pareado_camara"))]
ps[, sem_historico := FALSE]
per <- rbindlist(list(ps, pc), use.names = TRUE)
# causa de saida sem classe conhecida reprova: toda causa da fonte tem de cair numa classe declarada
sem_classe <- per[!sem_historico & !is.na(fim) & is.na(classe), unique(causa_original)]
if (length(sem_classe)) stop("causa sem classe: ", paste(sem_classe, collapse = " | "))
per <- merge(per, mand[, .(id_mandato, cargo, sg_uf, ano_eleicao, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))],
             by = "id_mandato")
# periodo que pertence a outro mandato da mesma pessoa (reeleito com a mesma pagina de historico) nao conta
per <- per[is.na(fim) | (fim >= mi - 60L & fim <= mf + 45L)]
setorder(per, id_mandato, fim, ini, na.last = TRUE)

## ---------------------------------------------------------------- forma de saida por mandato
res <- per[, {
  sh <- all(sem_historico)
  d <- which(classe %chin% DEFINITIVO & !is.na(fim))
  # 13/09/2026: ato definitivo seguido de novo periodo de exercicio no mesmo mandato foi revertido (Chico das Verduras,
  # RR 2010, perdeu o mandato em 05/2011 e voltou em 12/2012 por decisao do TSE) e nao encerra o mandato
  d <- d[vapply(d, function(k) !any(!is.na(ini) & ini > fim[k] + 1L), logical(1))]
  posse <- if (all(is.na(ini))) NA_character_ else as.character(min(ini, na.rm = TRUE))
  if (sh) {
    list(forma_saida = NA_character_, data_fim_efetiva = NA_character_, causa_original = NA_character_,
         em_curso = mf[1] >= HOJE, afastado_no_fim = NA, data_posse = posse, cobertura = "sem_historico_na_api")
  } else if (length(d)) {
    k <- d[1]
    list(forma_saida = classe[k], data_fim_efetiva = as.character(fim[k]), causa_original = causa_original[k],
         em_curso = FALSE, afastado_no_fim = FALSE, data_posse = posse, cobertura = "historico_da_casa")
  } else if (mf[1] < HOJE) {
    ult <- which.max(fifelse(is.na(fim), mf[1], fim))
    fora <- !is.na(classe[ult]) & classe[ult] %chin% TEMPORARIO
    list(forma_saida = "fim_regular", data_fim_efetiva = as.character(mf[1]),
         causa_original = if (fora) paste0("mandato vencido com o titular fora do exercicio desde ", fim[ult], " (", causa_original[ult], ")") else "Término do mandato",
         em_curso = FALSE, afastado_no_fim = fora, data_posse = posse, cobertura = "historico_da_casa")
  } else {
    list(forma_saida = NA_character_, data_fim_efetiva = NA_character_, causa_original = NA_character_,
         em_curso = TRUE, afastado_no_fim = NA, data_posse = posse, cobertura = "historico_da_casa")
  }
}, by = .(id_mandato, casa, cargo, sg_uf, ano_eleicao, fonte)]
res <- merge(res, per[, .(url_fonte = url_fonte[1]), by = id_mandato], by = "id_mandato")

## ---------------------------------------------------------------- legislatura 51 da Camara pela biografia oficial
# A API entrega a legislatura 51 (eleicao de 1998) sem eventos. A biografia oficial do deputado (R/59) registra a posse
# da legislatura 1999-2003, as renuncias, as perdas de mandato, a aposentadoria e a data de falecimento, e passa a ser a
# fonte de todos os mandatos dessa eleicao. Evento sem data ou frase de rodape capturada junto da secao nao conta.
bio_ev <- fread("data/camara_biografia_eventos.csv", colClasses = "character", na.strings = "NA")
bio_ps <- fread("data/camara_biografia_posses.csv", colClasses = "character", na.strings = "NA")
id_cam <- unique(ca[!is.na(id_mandato) & !is.na(id_deputado_camara), .(id_mandato, id_deputado_camara)])
m98 <- merge(mand[cd_cargo == "6" & ano_eleicao == "1998", .(id_mandato, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))],
             id_cam, by = "id_mandato", all.x = TRUE)
primeira_data <- function(x) { d <- strsplit(x, ";")[[1]]; d <- d[grepl("^\\d{4}-\\d{2}-\\d{2}$", d)]; if (length(d)) d[1] else NA_character_ }
bev <- bio_ev[id_deputado_camara %in% m98$id_deputado_camara]
bev[, data_ev := vapply(datas, primeira_data, character(1))]
def <- bev[!is.na(data_ev) & ((tipo == "renuncia" & grepl("Renunci", trecho)) | (tipo == "perda_mandato" & grepl("Perdeu|cassad", trecho)) |
                               tipo == "aposentadoria" | tipo == "falecimento"),
           .(id_deputado_camara, classe = fifelse(tipo == "perda_mandato", "cassacao", tipo), data_ev = as.IDate(data_ev), trecho, url)]
def <- merge(def, m98[, .(id_mandato, id_deputado_camara, mi, mf)], by = "id_deputado_camara", allow.cartesian = TRUE)
def <- def[data_ev >= mi & data_ev <= mf]
setorder(def, id_mandato, data_ev)
def <- def[, .SD[1], by = id_mandato]
pos98 <- bio_ps[leg_inicio == "1999", .(data_posse_bio = min(data_posse)), by = id_deputado_camara]
b98 <- merge(m98, pos98, by = "id_deputado_camara", all.x = TRUE)
# posse da biografia fora da janela do mandato e erro da propria pagina (legislatura 1999-2003 com posse em 1995): a
# pagina continua valendo como registro do mandato, sem a data de posse
b98[, posse_fora := !is.na(data_posse_bio) & (as.IDate(data_posse_bio) < mi - 60L | as.IDate(data_posse_bio) > mf)]
reg("slf_camara_1998_posse_da_biografia_fora_da_janela", b98[posse_fora == TRUE, .N])
b98[posse_fora == TRUE, data_posse_bio := "fora_da_janela"]
b98 <- merge(b98, def[, .(id_mandato, classe, data_ev, trecho, url)], by = "id_mandato", all.x = TRUE)
b98[, `:=`(forma_bio = fcase(!is.na(classe), classe, !is.na(data_posse_bio), "fim_regular", default = NA_character_),
           fim_bio = fcase(!is.na(classe), as.character(data_ev), !is.na(data_posse_bio), as.character(mf), default = NA_character_),
           causa_bio = fcase(!is.na(classe), trecho, !is.na(data_posse_bio), "Término do mandato (biografia sem renúncia, perda, aposentadoria ou falecimento na legislatura)",
                             default = NA_character_),
           url_bio = fifelse(is.na(id_deputado_camara), NA_character_, sprintf("https://www.camara.leg.br/deputados/%s/biografia", id_deputado_camara)))]
idx98 <- match(b98$id_mandato, res$id_mandato)
res[idx98, `:=`(forma_saida = b98$forma_bio, data_fim_efetiva = b98$fim_bio, causa_original = b98$causa_bio,
                data_posse = fifelse(is.na(b98$data_posse_bio) | b98$data_posse_bio == "fora_da_janela", data_posse, b98$data_posse_bio), em_curso = FALSE,
                afastado_no_fim = NA, cobertura = fifelse(is.na(b98$forma_bio), "biografia_sem_posse_na_legislatura", "biografia_oficial_camara"),
                fonte = "camara_biografia", url_fonte = b98$url_bio)]
reg("slf_camara_1998_pela_biografia", b98[!is.na(forma_bio), .N])
reg("slf_camara_1998_biografia_sem_posse", b98[is.na(forma_bio), .N])

## ---------------------------------------------------------------- eventos curados com fonte oficial
# Mandato do TSE cuja saida a casa nao registra no historico (perda da vaga por retotalizacao das sobras em 2025, que
# o historico da Camara deixa como Exercicio e a composicao atual desmente) ou que nunca teve posse (eleito que morreu
# ou renunciou antes). Prioridade sobre a API e a biografia; cada linha traz fonte, trecho e confianca.
cur_ev_f <- "ref/eventos_legislativo_federal_fonte_oficial.csv"
if (file.exists(cur_ev_f)) {
  cev <- fread(cur_ev_f, colClasses = "character", na.strings = c("", "NA"))
  stopifnot(all(cev$id_mandato %chin% res$id_mandato), !anyDuplicated(cev$id_mandato),
            all(cev$forma_saida %chin% c("fim_regular", DEFINITIVO, "retotalizacao", "nao_tomou_posse")),
            all(grepl("^\\d{4}-\\d{2}-\\d{2}$", cev$data_evento)), all(cev$confianca %chin% c("alta", "media")))
  ic <- match(cev$id_mandato, res$id_mandato)
  res[ic, `:=`(forma_saida = cev$forma_saida, data_fim_efetiva = cev$data_evento, causa_original = cev$causa, em_curso = FALSE,
               afastado_no_fim = NA, cobertura = "fonte_oficial_curada", fonte = "fonte_oficial_curada", url_fonte = cev$fonte_1)]
  reg("slf_eventos_curados_fonte_oficial", nrow(cev))
}

# conferencia da API pela biografia nas legislaturas 52 a 57: ato definitivo de um lado sem o mesmo ato, a menos de 15 dias, do outro
bio_def <- bio_ev[tipo %in% c("renuncia", "perda_mandato", "aposentadoria") & !is.na(leg_inicio), .(id_deputado_camara, leg_inicio, tipo, datas, trecho)]
bio_def[, `:=`(data_ev = as.IDate(vapply(datas, primeira_data, character(1))), classe = fifelse(tipo == "perda_mandato", "cassacao", tipo))]
api_def <- merge(res[casa == "camara" & forma_saida %chin% c("renuncia", "cassacao", "aposentadoria") & ano_eleicao != "1998",
                     .(id_mandato, ano_eleicao, forma_saida, data_fim_efetiva)], id_cam, by = "id_mandato")
api_def[, leg_inicio := as.character(as.integer(ano_eleicao) + 1L)]
# so deputados com mandato do TSE naquela legislatura (renuncia de suplente efetivado nao tem par do lado do mandato)
tse_leg <- unique(merge(res[casa == "camara", .(id_mandato, leg_inicio = as.character(as.integer(ano_eleicao) + 1L))], id_cam, by = "id_mandato")[, .(id_deputado_camara, leg_inicio)])
bio_def <- bio_def[tse_leg, on = .(id_deputado_camara, leg_inicio), nomatch = 0L]
cmp <- merge(api_def, bio_def, by = c("id_deputado_camara", "leg_inicio"), all = TRUE, allow.cartesian = TRUE)
cmp <- cmp[as.integer(leg_inicio) >= 2003]
cmp[, bate := !is.na(forma_saida) & !is.na(classe) & forma_saida == classe & (is.na(data_ev) | abs(as.IDate(data_fim_efetiva) - data_ev) <= 15L)]
ok_ids <- cmp[bate == TRUE, unique(paste(id_deputado_camara, leg_inicio))]
div <- cmp[!paste(id_deputado_camara, leg_inicio) %in% ok_ids]
fwrite(div, "output/verificacao/camara_api_x_biografia_atos_definitivos.csv", na = "NA", quote = TRUE)
reg("slf_camara_api_x_biografia_concordam", length(ok_ids))
reg("slf_camara_api_x_biografia_divergem", uniqueN(div, by = c("id_deputado_camara", "leg_inicio")))

## ---------------------------------------------------------------- interregnos
it <- per[classe %chin% TEMPORARIO & !is.na(fim)]
prox <- per[!is.na(ini), .(id_mandato, ini_prox = ini)]
it[, ordem := .I]
volta <- prox[it, on = .(id_mandato, ini_prox > fim), .(ordem = i.ordem, ini_prox = x.ini_prox), mult = "first"]
it <- merge(it, volta, by = "ordem", all.x = TRUE)
it[, `:=`(inicio_fora = as.character(fim + 1L),
          retorno_observado = !is.na(ini_prox),
          fim_fora = fifelse(!is.na(ini_prox), as.character(ini_prox - 1L),
                             fifelse(mf < HOJE, as.character(mf), NA_character_)))]
interregnos <- it[, .(id_mandato, id_pessoa, casa, cargo, sg_uf, ano_eleicao, tipo = classe, causa_original,
                      inicio_fora, fim_fora, retorno_observado, fonte, url_fonte)]
setorder(interregnos, id_mandato, inicio_fora)

## ---------------------------------------------------------------- ocupantes sem mandato no TSE
# Quem a casa registra como titular sem ter mandato no arquivo do TSE ocupou uma cadeira que o banco precisa mostrar.
# O suplente efetivado (Camara, efetivado = TRUE) entra pela lista partidaria no R/42. O eleito empossado que depois
# perdeu a vaga por cassacao, anulacao ou retotalizacao, e o substituto empossado por decisao judicial (Senado), sao
# poucas dezenas de pessoas, e a cadeira de cada um vem de ref/ocupantes_federais_fonte_oficial.csv, com a fonte
# oficial do ato. A ligacao por proximidade de datas acertava so parte dos casos e foi descartada.
sem_mand <- rbindlist(list(
  se[condicao == "titular" & is.na(id_mandato), .(casa = "senado", cd_cargo = "5", sg_uf, legislatura, id_pessoa, nome_fonte = nome_senado,
      nome_alt = nome_parlamentar, efetivado = FALSE, data_inicio = data_inicio_exercicio, data_fim = data_fim_exercicio,
      causa_original = causa_afastamento, fonte = "senado_api", url_fonte)],
  ca[condicao == "titular" & is.na(id_mandato), .(casa = "camara", cd_cargo = "6", sg_uf, legislatura, id_pessoa, nome_fonte = nome_civil,
      nome_alt = nome_parlamentar, efetivado = efetivado %in% "TRUE", data_inicio = data_inicio_exercicio, data_fim = data_fim_exercicio,
      causa_original = descricao_saida, fonte = "camara_api", url_fonte)]), use.names = TRUE)
# periodo que e so o registro de morte, sem entrada e sem outro exercicio na legislatura, nao e ocupacao: o arquivo em
# massa da Camara estende a legislatura final do ex-deputado ate a data da morte (Simao Sessim, morto em 2021, sem
# mandato na legislatura 56)
sem_mand[, so_morte := is.na(data_inicio) & grepl("Falecimento|dataFalecimento", causa_original) & .N == 1L, by = .(casa, sg_uf, legislatura, nome_alt)]
reg("slf_ocupantes_descartados_so_registro_de_morte", sem_mand[so_morte == TRUE, .N])
sem_mand <- sem_mand[so_morte == FALSE][, so_morte := NULL]
sem_mand[, tipo_ocupante := fifelse(efetivado, "suplente_efetivado", NA_character_)]
cur_f <- "ref/ocupantes_federais_fonte_oficial.csv"
sem_mand[, `:=`(id_mandato_cadeira = NA_character_, fonte_oficial = NA_character_, url_oficial = NA_character_)]
if (file.exists(cur_f)) {
  cur <- fread(cur_f, colClasses = "character", na.strings = c("", "NA"))
  # linha curada pode valer so para os periodos iniciados a partir de uma data (inicio_a_partir_de), para separar
  # condicoes sucessivas da mesma pessoa na mesma cadeira (Carlos Favaro convocado em 2020 e eleito na suplementar)
  if (!"inicio_a_partir_de" %in% names(cur)) cur[, inicio_a_partir_de := NA_character_]
  # data como numero AAAAMMDD; para cada periodo vale a linha curada mais recente com inicio_a_partir_de ate o inicio dele
  cur[, inicio_a_partir_de := fifelse(is.na(inicio_a_partir_de) | !nzchar(inicio_a_partir_de), 0, suppressWarnings(as.numeric(gsub("-", "", inicio_a_partir_de))))]
  sem_mand[, `:=`(linha_sm = .I, ini_chave = fifelse(is.na(data_inicio), 0, as.numeric(gsub("-", "", data_inicio))))]
  jj <- merge(sem_mand[, .(linha_sm, casa, sg_uf, legislatura, nome_alt, ini_chave)],
              cur[, .(casa, sg_uf, legislatura, nome_alt, inicio_a_partir_de, id_c = id_mandato_cadeira, tipo_c = tipo_ocupante, fonte_c = fonte_oficial)],
              by = c("casa", "sg_uf", "legislatura", "nome_alt"), allow.cartesian = TRUE)
  jj <- jj[inicio_a_partir_de <= ini_chave][order(linha_sm, -inicio_a_partir_de)][, .SD[1], by = linha_sm]
  sem_mand[jj, on = "linha_sm", `:=`(id_mandato_cadeira = i.id_c, tipo_ocupante = i.tipo_c, fonte_oficial = i.fonte_c)]
  sem_mand[, c("linha_sm", "ini_chave") := NULL]
}
fwrite(sem_mand, "data/ocupantes_legislativo_federal.csv", na = "NA", quote = TRUE)
reg("slf_ocupantes_sem_mandato_no_tse_periodos", nrow(sem_mand))
reg("slf_ocupantes_suplente_efetivado_periodos", sem_mand[efetivado == TRUE, .N])
reg("slf_ocupantes_nao_efetivados_pessoas", sem_mand[efetivado == FALSE, uniqueN(paste(casa, sg_uf, legislatura, nome_alt))])
reg("slf_ocupantes_nao_efetivados_sem_cadeira_curada", sem_mand[efetivado == FALSE & is.na(id_mandato_cadeira), uniqueN(paste(casa, sg_uf, legislatura, nome_alt))])

## ---------------------------------------------------------------- conferencias
stopifnot(!anyDuplicated(res$id_mandato))
# todo mandato de senador e deputado federal do TSE tem linha
faltam <- mand[!id_mandato %chin% res$id_mandato]
if (nrow(faltam)) stop("mandatos sem linha: ", nrow(faltam))
# nenhuma forma fora do vocabulario e nenhuma licenca ou afastamento como forma de saida
stopifnot(all(na.omit(res$forma_saida) %chin% c("fim_regular", DEFINITIVO, "retotalizacao", "nao_tomou_posse")))
# mandato encerrado com historico tem forma; em curso nao tem
stopifnot(res[cobertura %chin% c("historico_da_casa", "biografia_oficial_camara", "fonte_oficial_curada") & em_curso == FALSE & is.na(forma_saida), .N] == 0L,
          res[em_curso == TRUE & !is.na(forma_saida), .N] == 0L)
# saida definitiva dentro da janela do mandato
stopifnot(res[forma_saida %chin% DEFINITIVO, all(as.IDate(data_fim_efetiva) <= as.IDate(mand$mandato_fim[match(id_mandato, mand$id_mandato)]) + 45L)])

# precisao da data de saida (pendencia 2, opcao B): ato registrado pela casa ou por fonte oficial, ou data convencional do fim
res[, precisao_data_fim := fcase(is.na(forma_saida), NA_character_,
                                 forma_saida == "fim_regular" & data_fim_efetiva == mand$mandato_fim[match(id_mandato, mand$id_mandato)], "convencional",
                                 forma_saida == "nao_tomou_posse" & cobertura == "fonte_oficial_curada", NA_character_,
                                 default = "ato")]
if (exists("cev")) {
  pc <- cev[, .(id_mandato, precisao_data)]
  res[pc, on = "id_mandato", precisao_data_fim := i.precisao_data]
}
setcolorder(res, c("id_mandato", "casa", "cargo", "sg_uf", "ano_eleicao", "data_posse", "data_fim_efetiva", "precisao_data_fim", "forma_saida",
                   "causa_original", "em_curso", "afastado_no_fim", "cobertura", "fonte", "url_fonte"))
setorder(res, casa, ano_eleicao, sg_uf, id_mandato)
fwrite(res, "data/saida_legislativo_federal.csv", na = "NA", quote = TRUE)
fwrite(interregnos, "data/interregnos_legislativo_federal.csv", na = "NA", quote = TRUE)

for (cs in c("senado", "camara")) {
  r <- res[casa == cs]
  reg(sprintf("slf_%s_mandatos", cs), nrow(r))
  reg(sprintf("slf_%s_em_curso", cs), r[em_curso == TRUE, .N])
  reg(sprintf("slf_%s_sem_historico", cs), r[cobertura == "sem_historico_na_api", .N])
  reg(sprintf("slf_%s_encerrados_com_forma", cs), r[em_curso == FALSE & !is.na(forma_saida), .N])
  reg(sprintf("slf_%s_fim_regular_afastado_no_fim", cs), r[afastado_no_fim %in% TRUE, .N])
  for (f in c("fim_regular", DEFINITIVO, "retotalizacao", "nao_tomou_posse")) reg(sprintf("slf_%s_%s", cs, f), r[forma_saida %chin% f, .N])
  reg(sprintf("slf_%s_interregnos", cs), interregnos[casa == cs, .N])
  reg(sprintf("slf_%s_interregnos_sem_retorno_observado", cs), interregnos[casa == cs & retorno_observado == FALSE, .N])
}
cat("58_saida_legislativo_federal: concluido\n")
print(dcast(res[, .N, by = .(casa, ano_eleicao, forma = fifelse(is.na(forma_saida), fifelse(em_curso, "em_curso", cobertura), forma_saida))],
            casa + forma ~ ano_eleicao, fill = 0))
print(interregnos[, .N, by = .(casa, tipo, retorno_observado)][order(casa, -N)])
