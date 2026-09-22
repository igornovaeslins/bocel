# 49_munic_ampliado.R — o que cada edicao da MUNIC/IBGE permite afirmar sobre quem estava em
# exercicio da prefeitura, alem do nome.
#
# Motivo desta frente. R/11_munic_prefeitos.R leu as seis edicoes que trazem o bloco "atual
# prefeito" (2004, 2005, 2009, 2013, 2017, 2021) e classificou o exercicio por nome (2004, 2005)
# e por sexo+idade (as demais). Tres atributos que a propria MUNIC coleta ficaram fora da regra:
# o partido pelo qual o prefeito foi eleito, a escolaridade e a declaracao de que estava em
# exercicio do mandato no ano anterior. A cor/raca de 2021 tambem ficou sem contraparte no TSE.
# Este script constroi essas quatro dimensoes contra o eleito do TSE, mede a precisao da regra
# multiatributo na unica edicao que permite medi-la (2005, que tem nome e perfil ao mesmo tempo)
# e devolve uma linha por municipio x edicao dizendo o que a edicao permite afirmar.
#
# Regra de aquisicao: nenhuma requisicao de rede. Tudo vem de data_raw/ ja em disco.
#
# Entrada:  data_raw/munic/inventario_variaveis_munic.csv (R/10_munic_inventario.R)
#           data_raw/munic/<ano>/<base>.xls|xlsx        (python/fetch_munic.py)
#           data_raw/munic/manifest_munic.csv, data_raw/munic/diretorio_municipio_bd.csv
#           data_raw/consulta_cand/consulta_cand_<ano>.zip  (grau de instrucao e cor/raca do TSE)
#           data/municipios_tse_ibge.csv, data/mandatos.csv, data/pessoas.csv  (somente leitura)
# Saida:    data/munic_exercicio_ampliado.csv
#           output/numeros_assinatura.txt (prefixo mamp_), output/verificacao/relatorio_*.json
# Execucao: Rscript --vanilla R/49_munic_ampliado.R   (a partir da raiz do repositorio)
set.seed(20260830)
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringi)
  library(stringdist)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
mdir <- file.path(root, "data_raw", "munic")
dir.create("logs", showWarnings = FALSE)
logf <- file.path(root, "logs", "49_munic_ampliado.log")
sink(logf, split = TRUE)
cat("49_munic_ampliado:", format(Sys.time()), "\n")
script <- "R/49_munic_ampliado.R"
reg <- function(chave, valor) registrar_numero(paste0("mamp_", chave), valor, script = script,
                                               out = "output/numeros_assinatura.txt")

norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
norm_txt <- function(x) stri_trans_general(tolower(trimws(x)), "Latin-ASCII")

## ---------------------------------------------------------------- 1. edicoes e variaveis
inv <- fread(file.path(mdir, "inventario_variaveis_munic.csv"), na.strings = "NA")
man <- fread(file.path(mdir, "manifest_munic.csv"), na.strings = "NA")
url_ano <- man[status == "cache", .(url = url[1]), by = ano]

anos_bloco <- sort(inv[microdado_disponivel == TRUE & tem_bloco_prefeito == TRUE, ano_munic])
anos_sem_bloco <- sort(inv[microdado_disponivel == TRUE & tem_bloco_prefeito == FALSE, ano_munic])
anos_sem_micro <- sort(inv[microdado_disponivel == FALSE, ano_munic])
cat("edicoes com bloco do prefeito:", paste(anos_bloco, collapse = ", "), "\n")
cat("edicoes com microdado e sem bloco do prefeito:", paste(anos_sem_bloco, collapse = ", "), "\n")
cat("edicoes sem microdado:", paste(anos_sem_micro, collapse = ", "), "\n")

ler_bloco <- function(ano) {
  i <- inv[ano_munic == ano]
  f <- file.path(mdir, i$arquivo)
  d <- as.data.table(suppressMessages(read_excel(f, sheet = i$aba_prefeito, col_types = "text")))
  nm <- names(d)
  pick <- function(v) if (is.na(v)) rep(NA_character_, nrow(d)) else {
    j <- which(toupper(nm) == toupper(v)); stopifnot(length(j) == 1); trimws(d[[j]]) }
  data.table(
    ano_munic = ano,
    cod_munic_raw = gsub("\\D", "", d[[1]]),
    nome_munic = pick(i$var_nome),
    exercicio_ano_anterior = pick(i$var_exercicio_ano_anterior),
    sexo_munic = pick(i$var_sexo),
    idade_munic = suppressWarnings(as.integer(pick(i$var_idade))),
    escolaridade_munic = pick(i$var_escolaridade),
    partido_eleito_munic = pick(i$var_partido_eleito),
    partido_atual_munic = pick(i$var_partido_atual),
    cor_raca_munic = pick(i$var_cor_raca))
}
mu <- rbindlist(lapply(anos_bloco, ler_bloco))
mu <- mu[!is.na(cod_munic_raw) & nchar(cod_munic_raw) >= 6]
mu[, id_municipio_ibge6 := substr(cod_munic_raw, 1, 6)]

dir_bd <- fread(file.path(mdir, "diretorio_municipio_bd.csv"), colClasses = "character",
                na.strings = c("", "NA"))
dir_bd <- dir_bd[!is.na(id_municipio)]
mu <- merge(mu, unique(dir_bd[, .(id_municipio_ibge6 = substr(id_municipio, 1, 6),
                                  id_municipio_ibge = id_municipio)]),
            by = "id_municipio_ibge6", all.x = TRUE)
stopifnot(!any(duplicated(mu[, .(id_municipio_ibge, ano_munic)])))

# marcadores de ausencia e de recusa nao sao resposta (mesma correcao de 11_munic_prefeitos.R)
marc <- c("", "NAO DISPONIVEL", "NAO INFORMADO", "NAO SABE", "RECUSA", "NAO RESPONDEU",
          "SEM INFORMACAO", "IGNORADO")
lim <- function(x) { y <- norm_nome(x); fifelse(is.na(y) | y %in% marc, NA_character_, x) }
for (v in c("nome_munic", "escolaridade_munic", "partido_eleito_munic", "partido_atual_munic",
            "cor_raca_munic", "exercicio_ano_anterior", "sexo_munic"))
  mu[, (v) := lim(get(v))]
mu[idade_munic < 18 | idade_munic > 110, idade_munic := NA_integer_]
mu[, nome_norm_munic := norm_nome(nome_munic)]
mu[, sexo_munic := fcase(grepl("^MASC", toupper(sexo_munic)), "MASCULINO",
                         grepl("^FEM", toupper(sexo_munic)), "FEMININO", default = NA_character_)]
mu[, exerc_ant_munic := fcase(norm_txt(exercicio_ano_anterior) == "sim", TRUE,
                              norm_txt(exercicio_ano_anterior) == "nao", FALSE, default = NA)]
# qual variavel de partido a edicao traz: "eleito" e comparavel ao partido da eleicao; "atual"
# muda por troca de legenda no mandato e por isso nao entra na regra de identificacao
tp <- inv[, .(ano_munic, tem_part_eleito = !is.na(var_partido_eleito),
              tem_part_atual = !is.na(var_partido_atual))]
mu <- merge(mu, tp, by = "ano_munic", all.x = TRUE)
mu[, tipo_partido_munic := fcase(tem_part_eleito == TRUE, "eleito",
                                 tem_part_atual == TRUE, "atual", default = NA_character_)]
mu[, partido_munic := fifelse(tipo_partido_munic == "eleito", partido_eleito_munic, partido_atual_munic)]

ref_map <- setNames(inv$data_referencia, inv$ano_munic)
mu[, data_referencia := unname(ref_map[as.character(ano_munic)])]
mu[, ano_eleicao_bocel := ((ano_munic - 1L) %/% 4L) * 4L]
cat("linhas lidas das edicoes com bloco:", nrow(mu), "\n")

## ---------------------------------------------------------------- 2. o eleito do TSE no banco
corr <- fread("data/municipios_tse_ibge.csv", colClasses = list(character = c("sg_ue", "id_municipio_ibge", "id_municipio_ibge6")), na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = list(character = c("sg_ue", "unidade_posicao",
                                                                  "sq_candidato")), na.strings = "NA")
pes <- fread("data/pessoas.csv", na.strings = "NA")
pes[, nome_norm := norm_nome(nome)]
pes[, nome_urna_norm := norm_nome(nome_urna_recente)]

anos_el <- sort(unique(mu$ano_eleicao_bocel))
pref <- mand[cd_cargo == 11L & ano_eleicao %in% c(anos_el, anos_el - 4L),
             .(id_mandato, id_pessoa, sq_candidato, sg_ue, sg_uf, ano_eleicao, sg_partido,
               mandato_inicio, mandato_fim)]
stopifnot(!any(duplicated(pref[, .(sg_ue, ano_eleicao)])))
vicem <- mand[cd_cargo == 12L & ano_eleicao %in% anos_el,
              .(sg_ue, ano_eleicao, id_mandato_vice = id_mandato, id_pessoa_vice = id_pessoa,
                sq_candidato_vice = sq_candidato)]
vicem <- vicem[!duplicated(vicem[, .(sg_ue, ano_eleicao)])]

## ---------------------------------------------------------------- 3. atributos do TSE em disco
# grau de instrucao e cor/raca do candidato eleito, lidos dos zips consulta_cand ja baixados.
# A chave do TSE em eleicao municipal e (ano, sg_ue, cd_cargo, sq_candidato): sq_candidato sozinho
# nao identifica candidato nos anos antigos.
ler_cand_attr <- function(ano) {
  z <- file.path("data_raw", "consulta_cand", sprintf("consulta_cand_%d.zip", ano))
  if (!file.exists(z)) return(NULL)
  fs <- grep(sprintf("^consulta_cand_%d_[A-Z]{2}[.]csv$", ano), unzip(z, list = TRUE)$Name, value = TRUE)
  cols <- c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "SQ_CANDIDATO", "DS_GRAU_INSTRUCAO", "DS_COR_RACA")
  x <- rbindlist(lapply(fs, function(f) {
    y <- fread(cmd = sprintf("unzip -p %s %s", shQuote(z), shQuote(f)), sep = ";",
               encoding = "Latin-1", quote = '"', colClasses = "character", showProgress = FALSE)
    for (k in setdiff(cols, names(y))) y[, (k) := NA_character_]
    y <- y[CD_CARGO %in% c("11", "12"), ..cols]
    y
  }), use.names = TRUE)
  unique(x)
}
cand <- rbindlist(lapply(anos_el, ler_cand_attr), use.names = TRUE)
cand[, ano_eleicao := as.integer(ANO_ELEICAO)]
cand[, sg_ue := formatC(suppressWarnings(as.integer(SG_UE)), width = 5, flag = "0")]
cand[, sq_candidato := SQ_CANDIDATO]
cand <- cand[!is.na(sg_ue) & sg_ue != "   NA"]
cand <- unique(cand[, .(ano_eleicao, sg_ue, cd_cargo = CD_CARGO, sq_candidato,
                        instrucao_tse = DS_GRAU_INSTRUCAO, cor_raca_tse = DS_COR_RACA)])
cand <- cand[!duplicated(cand[, .(ano_eleicao, sg_ue, cd_cargo, sq_candidato)])]
cat("candidaturas prefeito/vice lidas dos zips do TSE:", nrow(cand), "\n")

## ---------------------------------------------------------------- 4. montagem municipio x edicao
x <- merge(mu, corr[, .(id_municipio_ibge, sg_ue, sg_uf)], by = "id_municipio_ibge", all.x = TRUE)
x <- merge(x, pref[, .(sg_ue, ano_eleicao_bocel = ano_eleicao, id_mandato_eleito = id_mandato,
                       id_pessoa_eleito = id_pessoa, sq_cand_eleito = sq_candidato,
                       partido_tse = sg_partido, mandato_inicio, mandato_fim, sg_uf_mand = sg_uf)],
           by = c("sg_ue", "ano_eleicao_bocel"), all.x = TRUE)
x[is.na(sg_uf), sg_uf := sg_uf_mand][, sg_uf_mand := NULL]
x <- merge(x, pes[, .(id_pessoa_eleito = id_pessoa, nome_bocel = nome, nome_norm_bocel = nome_norm,
                      nome_urna_norm_bocel = nome_urna_norm, dt_nascimento, genero)],
           by = "id_pessoa_eleito", all.x = TRUE)
x <- merge(x, cand[cd_cargo == "11", .(ano_eleicao_bocel = ano_eleicao, sg_ue, sq_cand_eleito = sq_candidato,
                                       instrucao_tse, cor_raca_tse)],
           by = c("ano_eleicao_bocel", "sg_ue", "sq_cand_eleito"), all.x = TRUE)
# vice eleito na mesma eleicao (hipotese de substituicao)
x <- merge(x, vicem[, .(sg_ue, ano_eleicao_bocel = ano_eleicao, id_mandato_vice, id_pessoa_vice,
                        sq_candidato_vice)], by = c("sg_ue", "ano_eleicao_bocel"), all.x = TRUE)
x <- merge(x, pes[, .(id_pessoa_vice = id_pessoa, nome_norm_vice = nome_norm,
                      nome_urna_norm_vice = nome_urna_norm, dt_nasc_vice = dt_nascimento,
                      genero_vice = genero)], by = "id_pessoa_vice", all.x = TRUE)
stopifnot(!any(duplicated(x[, .(id_municipio_ibge, ano_munic)])))

## ---------------------------------------------------------------- 5. harmonizacao dos atributos
# escolaridade em escala ordinal comum. O TSE nao tem categoria de pos-graduacao (o teto e
# "superior completo"); a MUNIC tem. As duas escalas so sao comparaveis colapsando o topo.
# A MUNIC tambem nao tem categoria abaixo de "fundamental incompleto"; analfabeto e le-e-escreve
# do TSE sao colapsados nela.
esc_classe <- function(s) {
  t <- norm_txt(s)
  fcase(
    is.na(t), NA_integer_,
    grepl("analfabeto|le e escreve", t), 2L,
    grepl("fundamental|1o grau|1 grau|primario", t) & grepl("incompleto", t), 2L,
    grepl("fundamental|1o grau|1 grau|primario", t) & grepl("completo", t), 3L,
    grepl("medio|2o grau|2 grau|secundario", t) & grepl("incompleto", t), 4L,
    grepl("medio|2o grau|2 grau|secundario", t) & grepl("completo", t), 5L,
    grepl("superior", t) & grepl("incompleto", t), 6L,
    grepl("superior", t) & grepl("completo", t), 7L,
    grepl("pos-graduacao|pos graduacao|especializacao|mestrado|doutorado", t), 7L,
    default = NA_integer_)
}
x[, esc_munic := esc_classe(escolaridade_munic)]
x[, esc_tse := esc_classe(instrucao_tse)]

# partido: siglas que mudaram de nome entre a eleicao e a pesquisa
alias <- c(PMDB = "MDB", PFL = "DEM", DEM = "DEM", UNIAO = "DEM", `UNIÃO` = "DEM", PPB = "PP",
           PPR = "PP", PROGRESSISTAS = "PP", PP = "PP", PL = "PL", PR = "PL", PRB = "REPUBLICANOS",
           REPUBLICANOS = "REPUBLICANOS", PPS = "CIDADANIA", CIDADANIA = "CIDADANIA",
           PTN = "PODEMOS", PODE = "PODEMOS", PODEMOS = "PODEMOS", `PT DO B` = "AVANTE",
           PTDOB = "AVANTE", AVANTE = "AVANTE", PEN = "PATRIOTA", PATRIOTA = "PATRIOTA",
           PSDC = "DC", DC = "DC", PMR = "REPUBLICANOS", SD = "SOLIDARIEDADE",
           SDD = "SOLIDARIEDADE", SOLIDARIEDADE = "SOLIDARIEDADE", PSL = "PSL", PRONA = "PL")
norm_part <- function(p) {
  q <- toupper(stri_trans_general(trimws(p), "Latin-ASCII"))
  q <- gsub("[^A-Z0-9 ]", "", q)
  ifelse(is.na(q), NA_character_, ifelse(q %in% names(alias), unname(alias[q]), q))
}
x[, partido_munic_norm := norm_part(partido_munic)]
x[, partido_tse_norm := norm_part(partido_tse)]

# idade completada em 31/12 do ano da pesquisa (referencia calibrada em R/11_munic_prefeitos.R)
idade_em <- function(dt_nasc, ano) {
  d <- as.IDate(dt_nasc); ref <- as.IDate(sprintf("%d-12-31", ano))
  as.integer(floor(as.numeric(ref - d) / 365.25))
}
x[, idade_tse := idade_em(dt_nascimento, ano_munic)]
x[, idade_vice_tse := idade_em(dt_nasc_vice, ano_munic)]
bin <- c("MASCULINO", "FEMININO")
x[!genero %in% bin, genero := NA_character_]
x[!genero_vice %in% bin, genero_vice := NA_character_]

# cor/raca: so 2021 na MUNIC; o TSE so divulga a partir de 2014 ("#NE" antes disso)
raca_classe <- function(s) {
  t <- norm_txt(s)
  fcase(is.na(t) | grepl("^#|nao informado|nao divulgavel|recusa|ignorado", t), NA_character_,
        grepl("branca", t), "BRANCA", grepl("preta", t), "PRETA", grepl("parda", t), "PARDA",
        grepl("amarela", t), "AMARELA", grepl("indigena", t), "INDIGENA", default = NA_character_)
}
x[, raca_munic := raca_classe(cor_raca_munic)]
x[, raca_tse := raca_classe(cor_raca_tse)]

# "estava em exercicio no ano anterior": esperado a partir do proprio banco. A edicao ocorre no
# primeiro ano do mandato; o eleito so estaria em exercicio no ano anterior se ja fosse o prefeito
# eleito no pleito anterior no mesmo municipio. Quem foi vice no mandato anterior pode ter assumido,
# e por isso essa situacao fica declarada como indeterminada e nao entra na regra.
pref_ant <- pref[, .(sg_ue, ano_eleicao_ant = ano_eleicao, id_pessoa_ant = id_pessoa)]
x <- merge(x, pref_ant[, .(sg_ue, ano_eleicao_bocel = ano_eleicao_ant + 4L, id_pessoa_ant)],
           by = c("sg_ue", "ano_eleicao_bocel"), all.x = TRUE)
vice_ant <- mand[cd_cargo == 12L, .(sg_ue, ano_eleicao_bocel = ano_eleicao + 4L, id_pessoa_vice_ant = id_pessoa)]
vice_ant <- vice_ant[!duplicated(vice_ant[, .(sg_ue, ano_eleicao_bocel)])]
x <- merge(x, vice_ant, by = c("sg_ue", "ano_eleicao_bocel"), all.x = TRUE)
x[, exerc_ant_esperado := fcase(
  is.na(id_pessoa_eleito) | is.na(id_pessoa_ant), NA_character_,
  id_pessoa_eleito == id_pessoa_ant, "sim",
  !is.na(id_pessoa_vice_ant) & id_pessoa_eleito == id_pessoa_vice_ant, "possivel_vice_que_assumiu",
  default = "nao")]

## ---------------------------------------------------------------- 6. concordancia por dimensao
cmp_nome <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  exato <- ok & a == b
  jw <- ifelse(ok, 1 - stringdist(a, b, method = "jw", p = 0.1), NA_real_)
  part <- c("DE", "DA", "DO", "DAS", "DOS", "E", "DR", "DRA")
  tok <- mapply(function(p, q) { if (is.na(p) || is.na(q)) return(NA)
    tp <- setdiff(strsplit(p, " ")[[1]], part); tq <- setdiff(strsplit(q, " ")[[1]], part)
    if (length(tp) < 2 || length(tq) < 2) return(FALSE)
    all(tp %in% tq) || all(tq %in% tp) }, a, b, USE.NAMES = FALSE)
  list(jw = jw, parcial = ifelse(ok, exato | jw >= 0.92 | tok %in% TRUE, NA))
}
cn <- cmp_nome(x$nome_norm_munic, x$nome_norm_bocel)
cu <- cmp_nome(x$nome_norm_munic, x$nome_urna_norm_bocel)
x[, jw_nome := round(cn$jw, 3)]
x[, match_nome := fifelse(is.na(cn$parcial), NA, cn$parcial | cu$parcial %in% TRUE)]
# guarda de homonimo: nome curto (menos de 8 caracteres) ou de um unico token nao decide sozinho
n_tok <- function(s) ifelse(is.na(s), NA_integer_, lengths(strsplit(s, " ")))
x[, nome_frag := !is.na(nome_norm_munic) & (nchar(nome_norm_munic) < 8 | n_tok(nome_norm_munic) < 2)]

x[, match_sexo := fifelse(is.na(sexo_munic) | is.na(genero), NA, sexo_munic == genero)]
x[, dif_idade := idade_munic - idade_tse]
x[, match_idade := fifelse(is.na(dif_idade), NA, abs(dif_idade) <= 1L)]
x[, match_idade_ampla := fifelse(is.na(dif_idade), NA, abs(dif_idade) <= 3L)]
x[, match_partido := fifelse(tipo_partido_munic == "eleito" & !is.na(partido_munic_norm) &
                             !is.na(partido_tse_norm), partido_munic_norm == partido_tse_norm, NA)]
# escolaridade pode subir entre a eleicao e a pesquisa; queda de grau nao tem leitura substantiva
x[, match_escolaridade := fifelse(is.na(esc_munic) | is.na(esc_tse), NA, esc_munic == esc_tse)]
x[, match_escolaridade_ampla := fifelse(is.na(esc_munic) | is.na(esc_tse), NA,
                                        esc_munic >= esc_tse & (esc_munic - esc_tse) <= 1L)]
x[, match_raca := fifelse(is.na(raca_munic) | is.na(raca_tse), NA, raca_munic == raca_tse)]
x[, match_exerc_ant := fifelse(is.na(exerc_ant_munic) | is.na(exerc_ant_esperado) |
                               exerc_ant_esperado == "possivel_vice_que_assumiu", NA,
                               exerc_ant_munic == (exerc_ant_esperado == "sim"))]

dims <- c("match_partido", "match_escolaridade_ampla", "match_sexo", "match_idade", "match_raca",
          "match_exerc_ant")
x[, n_dim_comparadas := rowSums(!is.na(.SD)), .SDcols = dims]
x[, n_dim_concordantes := rowSums(.SD == TRUE, na.rm = TRUE), .SDcols = dims]
x[, n_dim_discordantes := n_dim_comparadas - n_dim_concordantes]

## ---------------------------------------------------------------- 7. calibracao na edicao de 2005
# 2005 e a unica edicao que traz nome e o perfil completo ao mesmo tempo. O nome define a verdade
# de referencia; as regras multiatributo sao aplicadas sem olhar o nome e comparadas a ela.
cal <- x[ano_munic == 2005L & !is.na(match_nome) & !is.na(id_mandato_eleito)]
verdade <- cal$match_nome
prec <- function(pred, alvo) { s <- sum(pred, na.rm = TRUE)
  if (s == 0) return(NA_real_) else round(sum(pred & alvo, na.rm = TRUE) / s, 4) }
regras <- list(
  # confirmacao do eleito
  conf_sexo_idade = cal$match_sexo %in% TRUE & cal$match_idade %in% TRUE,
  conf_partido = cal$match_partido %in% TRUE,
  conf_partido_escolaridade = cal$match_partido %in% TRUE & cal$match_escolaridade_ampla %in% TRUE,
  conf_4dim_sem_partido = cal$n_dim_comparadas >= 4L & cal$n_dim_discordantes == 0L &
    cal$match_sexo %in% TRUE & cal$match_idade %in% TRUE,
  conf_todas_sem_discordancia = cal$n_dim_comparadas >= 4L & cal$n_dim_discordantes == 0L)
calib <- list()
for (k in names(regras)) {
  calib[[paste0("precisao_2005_", k)]] <- prec(regras[[k]], verdade)
  calib[[paste0("cobertura_2005_", k)]] <- round(mean(regras[[k]], na.rm = TRUE), 4)
}
# deteccao de outra pessoa em exercicio
neg <- !verdade
regras_neg <- list(
  outro_sexo_ou_idade = cal$match_sexo %in% FALSE | cal$match_idade_ampla %in% FALSE,
  outro_1_discordancia = cal$n_dim_discordantes >= 1L,
  outro_2_discordancias = cal$n_dim_discordantes >= 2L,
  outro_3_discordancias = cal$n_dim_discordantes >= 3L)
for (k in names(regras_neg)) {
  calib[[paste0("precisao_2005_", k)]] <- prec(regras_neg[[k]], neg)
  calib[[paste0("recall_2005_", k)]] <- round(sum(regras_neg[[k]] & neg, na.rm = TRUE) / sum(neg), 4)
}
calib[["n_calibracao_2005"]] <- nrow(cal)
calib[["n_calibracao_2005_outro_pelo_nome"]] <- sum(neg)
for (k in names(calib)) reg(k, calib[[k]])
cat("\ncalibracao na edicao de 2005 (verdade = nome):\n"); print(unlist(calib))

## ---------------------------------------------------------------- 8. o que cada linha permite afirmar
# A regra adotada e a que a calibracao de 2005 mede: confirmacao exige nenhuma discordancia em pelo
# menos tres dimensoes; a afirmacao de que ha outra pessoa exige duas discordancias, porque uma so
# tem precisao baixa demais para sustentar substituicao.
x[, afirmacao := fcase(
  is.na(id_mandato_eleito), "sem_prefeito_no_bocel",
  match_nome %in% TRUE & nome_frag == FALSE, "exercicio_do_eleito_confirmado_por_nome",
  match_nome %in% TRUE & nome_frag == TRUE & n_dim_discordantes == 0L & n_dim_comparadas >= 1L,
    "exercicio_do_eleito_confirmado_por_nome",
  match_nome %in% FALSE & jw_nome < 0.85 & n_dim_discordantes >= 1L,
    "outra_pessoa_em_exercicio_por_nome",
  match_nome %in% FALSE & jw_nome < 0.85 & n_dim_comparadas == 0L,
    "outra_pessoa_em_exercicio_por_nome",
  !is.na(match_nome), "indeterminado_evidencia_insuficiente",
  n_dim_comparadas >= 3L & n_dim_discordantes == 0L, "exercicio_do_eleito_confirmado_por_atributos",
  n_dim_discordantes >= 2L, "outra_pessoa_em_exercicio_por_atributos",
  default = "indeterminado_evidencia_insuficiente")]
x[, criterio := fcase(
  afirmacao == "sem_prefeito_no_bocel", "sem_prefeito_no_bocel",
  grepl("por_nome", afirmacao), "nome_normalizado_no_municipio_e_eleicao",
  grepl("por_atributos", afirmacao),
    paste0("atributos_", paste0(sub("^match_", "", dims), collapse = "_")),
  default = "evidencia_insuficiente")]

# hipotese de substituicao: o vice eleito na mesma chapa
cv <- cmp_nome(x$nome_norm_munic, x$nome_norm_vice)
cvu <- cmp_nome(x$nome_norm_munic, x$nome_urna_norm_vice)
x[, vice_nome := fifelse(is.na(cv$parcial), NA, cv$parcial | cvu$parcial %in% TRUE)]
x[, vice_sexo := fifelse(is.na(sexo_munic) | is.na(genero_vice), NA, sexo_munic == genero_vice)]
x[, vice_idade := fifelse(is.na(idade_munic) | is.na(idade_vice_tse), NA,
                          abs(idade_munic - idade_vice_tse) <= 1L)]
x[, substituto_provavel := NA_character_]
x[grepl("^outra_pessoa", afirmacao), substituto_provavel := fcase(
  is.na(id_pessoa_vice), "indeterminado_sem_vice_no_bocel",
  vice_nome %in% TRUE, "vice",
  vice_nome %in% FALSE, "terceiro",
  vice_sexo %in% TRUE & vice_idade %in% TRUE, "vice",
  vice_sexo %in% FALSE, "terceiro",
  default = "indeterminado")]

## ---------------------------------------------------------------- 9. tabela de saida
x[, titular := fcase(grepl("confirmado", afirmacao), TRUE,
                     grepl("^outra_pessoa", afirmacao), FALSE, default = NA)]
x[, id_pessoa_bocel := fifelse(grepl("confirmado", afirmacao), id_pessoa_eleito, NA_character_)]
x[, id_mandato_bocel := fifelse(grepl("confirmado", afirmacao), id_mandato_eleito, NA_character_)]
x[, metodo_pareamento := fcase(
  !is.na(id_mandato_bocel), criterio,
  grepl("^outra_pessoa", afirmacao), "perfil_divergente_do_eleito_sem_pareamento",
  afirmacao == "sem_prefeito_no_bocel", "sem_prefeito_no_bocel",
  default = "evidencia_insuficiente_sem_pareamento")]
x <- merge(x, url_ano, by.x = "ano_munic", by.y = "ano", all.x = TRUE)

out <- x[, .(
  sg_ue, id_municipio_ibge, uf = sg_uf, dominio = "ftp.ibge.gov.br",
  legislatura_numero = NA_integer_,
  legislatura_inicio = as.character(mandato_inicio), legislatura_fim = as.character(mandato_fim),
  ano_eleicao_bocel,
  nome_fonte = nome_munic, nome_parlamentar = nome_munic, nome_normalizado = nome_norm_munic,
  titular,
  data_inicio_mandato = NA_character_, data_fim_mandato = NA_character_,
  tipo_afastamento = NA_character_, forma_saida = "nao_observado",
  id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, sistema = "munic",
  so_legislatura_atual = FALSE, url,
  # evidencia da MUNIC
  ano_munic, data_referencia, afirmacao, substituto_provavel,
  sexo_munic, idade_munic, escolaridade_munic, esc_munic, cor_raca_munic,
  partido_munic, tipo_partido_munic, exercicio_ano_anterior,
  # contraparte do TSE/BOCEL
  id_mandato_prefeito_eleito = id_mandato_eleito, id_pessoa_prefeito_eleito = id_pessoa_eleito,
  nome_prefeito_eleito = nome_bocel, genero_tse = genero, idade_tse, instrucao_tse, esc_tse,
  partido_tse, cor_raca_tse, exerc_ant_esperado,
  # concordancia dimensao a dimensao
  match_nome, jw_nome, nome_frag, match_sexo, match_idade, dif_idade, match_partido,
  match_escolaridade, match_escolaridade_ampla, match_raca, match_exerc_ant,
  n_dim_comparadas, n_dim_concordantes, n_dim_discordantes,
  id_mandato_vice, id_pessoa_vice)]
setorder(out, ano_munic, sg_ue)

voc <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
         "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
stopifnot(all(out$forma_saida %in% voc))
stopifnot(!any(duplicated(out[, .(id_municipio_ibge, ano_munic)])))
stopifnot(all(!is.na(out$sg_ue) | is.na(out$id_mandato_bocel)))
stopifnot(all(is.na(out$id_mandato_bocel) | out$id_mandato_bocel %in% mand$id_mandato))
stopifnot(all(is.na(out$substituto_provavel) | grepl("^outra_pessoa", out$afirmacao)))
# 05/09/2026: consulta_cand e lida em Latin-1 (fread encoding = "Latin-1", secao 3) e a MUNIC em
# UTF-8 (readxl); sem esta conversao o CSV saia com as duas codificacoes misturadas na mesma linha
# (instrucao_tse, partido_tse, cor_raca_tse em Latin-1) e nao decodificava como UTF-8, ao contrario
# do que o pacote de deposito promete. Toda coluna de texto sai em UTF-8 declarado.
for (v in names(out)[vapply(out, is.character, logical(1))]) out[, (v) := enc2utf8(get(v))]
stopifnot(all(validUTF8(unlist(out[, names(out)[vapply(out, is.character, logical(1))], with = FALSE]))))
fwrite(out, "data/munic_exercicio_ampliado.csv", na = "NA")

## ---------------------------------------------------------------- 10. resumo e registro
res <- out[, .(n = .N,
               n_com_nome = sum(!is.na(nome_fonte)),
               n_pareados = sum(!is.na(id_mandato_bocel)),
               n_conf_nome = sum(afirmacao == "exercicio_do_eleito_confirmado_por_nome"),
               n_conf_atrib = sum(afirmacao == "exercicio_do_eleito_confirmado_por_atributos"),
               n_outro_nome = sum(afirmacao == "outra_pessoa_em_exercicio_por_nome"),
               n_outro_atrib = sum(afirmacao == "outra_pessoa_em_exercicio_por_atributos"),
               n_indet = sum(afirmacao == "indeterminado_evidencia_insuficiente"),
               n_sem_bocel = sum(afirmacao == "sem_prefeito_no_bocel"),
               n_dim_media = round(mean(n_dim_comparadas), 2),
               n_part_comp = sum(!is.na(match_partido)), n_part_ok = sum(match_partido %in% TRUE),
               n_esc_comp = sum(!is.na(match_escolaridade_ampla)), n_esc_ok = sum(match_escolaridade_ampla %in% TRUE),
               n_exerc_comp = sum(!is.na(match_exerc_ant)), n_exerc_ok = sum(match_exerc_ant %in% TRUE),
               n_raca_comp = sum(!is.na(match_raca)), n_raca_ok = sum(match_raca %in% TRUE)),
           by = ano_munic]
print(res)

reg("n_linhas", nrow(out))
reg("n_municipios", uniqueN(out$id_municipio_ibge))
reg("n_ufs", uniqueN(out$uf[!is.na(out$uf)]))
reg("n_edicoes_na_tabela", uniqueN(out$ano_munic))
reg("edicoes_na_tabela", paste(sort(unique(out$ano_munic)), collapse = ";"))
reg("edicoes_com_microdado_sem_bloco_do_prefeito", paste(anos_sem_bloco, collapse = ";"))
reg("n_edicoes_com_microdado_sem_bloco_do_prefeito", length(anos_sem_bloco))
reg("edicoes_sem_microdado", paste(anos_sem_micro, collapse = ";"))
reg("n_mandatos_pareados", out[!is.na(id_mandato_bocel), .N])
reg("n_mandatos_distintos_pareados", uniqueN(out$id_mandato_bocel[!is.na(out$id_mandato_bocel)]))
reg("n_dimensao_partido_comparada", out[!is.na(match_partido), .N])
reg("n_dimensao_escolaridade_comparada", out[!is.na(match_escolaridade_ampla), .N])
reg("n_dimensao_exercicio_anterior_comparada", out[!is.na(match_exerc_ant), .N])
reg("n_dimensao_raca_comparada", out[!is.na(match_raca), .N])
reg("prop_concordancia_partido", round(out[!is.na(match_partido), mean(match_partido)], 4))
reg("prop_concordancia_escolaridade", round(out[!is.na(match_escolaridade_ampla), mean(match_escolaridade_ampla)], 4))
reg("prop_concordancia_exercicio_anterior", round(out[!is.na(match_exerc_ant), mean(match_exerc_ant)], 4))
reg("prop_concordancia_raca", round(out[!is.na(match_raca), mean(match_raca)], 4))
for (i in seq_len(nrow(res))) {
  y <- res$ano_munic[i]
  reg(sprintf("%d_n_linhas", y), res$n[i])
  reg(sprintf("%d_n_pareados", y), res$n_pareados[i])
  reg(sprintf("%d_n_confirmado_por_nome", y), res$n_conf_nome[i])
  reg(sprintf("%d_n_confirmado_por_atributos", y), res$n_conf_atrib[i])
  reg(sprintf("%d_n_outra_pessoa", y), res$n_outro_nome[i] + res$n_outro_atrib[i])
  reg(sprintf("%d_n_indeterminado", y), res$n_indet[i])
  reg(sprintf("%d_n_dimensoes_media", y), res$n_dim_media[i])
}

passou <- c(
  sprintf("chave (id_municipio_ibge, ano_munic) unica em %d linhas", nrow(out)),
  "forma_saida restrita ao vocabulario fechado (todas as linhas em nao_observado: a MUNIC nao documenta saida)",
  "id_mandato_bocel preenchido so quando a evidencia confirma o eleito, e sempre existente em data/mandatos.csv",
  "substituto_provavel preenchido apenas nas linhas de outra pessoa em exercicio",
  sprintf("precisao da regra multiatributo medida na edicao de 2005 contra o nome (n = %d)", nrow(cal)),
  "nenhuma requisicao de rede: MUNIC e consulta_cand lidos de data_raw/",
  "toda coluna de texto gravada em UTF-8 valido (validUTF8), sem mistura com o Latin-1 do TSE")
fora <- c(
  "veracidade do que o municipio declarou ao IBGE (dado administrativo)",
  "data da entrevista por municipio: a base nao a traz, e a idade foi calculada em 31 de dezembro do ano da pesquisa",
  "motivo e data da interrupcao do mandato: a MUNIC nao pergunta, e por isso forma_saida fica em nao_observado",
  "1999, 2001 e 2002 nao tem microdado por municipio; 2006, 2008, 2011, 2012, 2014, 2015, 2018, 2019, 2020, 2023 e 2024 nao tem bloco de perfil do prefeito",
  "identificacao de quem substituiu quando nao e o vice da chapa")
gravar_relatorio_verificacao(alvo = "data/munic_exercicio_ampliado.csv", script = script,
                             passou = passou, fora_de_cobertura = fora)
cat("\n49_munic_ampliado: concluido;", nrow(out), "linhas\n")
sink()
