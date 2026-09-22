# 07_exercicio_camara.R — posse, exercicio e forma de saida dos deputados federais
# (legislaturas 51 a 57, eleicoes 1998-2022) a partir da API de Dados Abertos da Camara.
# Entrada:  data_raw/camara/{lista_leg_*.json, deputados/*.json, historico/*.json, deputados_bulk.csv}
#           data/pessoas.csv, data/mandatos.csv
# Saida:    data/exercicio_camara.csv|parquet|rds
#           output/verificacao/camara_pareamento_por_legislatura.csv
#           output/verificacao/camara_forma_saida.csv
#           output/numeros_assinatura.txt (registrar_numero)
# Execucao: cd ~/bocel && Rscript --vanilla R/07_exercicio_camara.R
# Nao altera nenhum arquivo existente de data/.
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
  library(jsonlite)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
SCRIPT <- "R/07_exercicio_camara.R"
raw <- file.path(root, "data_raw", "camara")
dir.create("output/verificacao", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE)
logf <- file("logs/07_exercicio_camara.log", open = "wt")
logm <- function(...) { m <- paste0(format(Sys.time(), "%H:%M:%S "), paste(..., collapse = " ")); cat(m, "\n"); writeLines(m, logf) }
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a[1]
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("[^A-Z ]", "", x)
  gsub(" +", " ", trimws(x))
}
LEGS <- 51:57
leg_de_ano <- function(ano) (ano - 1998L) %/% 4L + 51L      # 1998 -> 51 ... 2022 -> 57
ano_de_leg <- function(leg) 1998L + (leg - 51L) * 4L

## ---------------------------------------------------------------- listas por legislatura
listas <- rbindlist(lapply(LEGS, function(l) {
  j <- fromJSON(file.path(raw, sprintf("lista_leg_%d.json", l)))
  data.table(id_deputado = as.integer(j$dados$id), legislatura_lista = l)
}))
listas[, origem_lista := "lista_legislatura"]
# arquivo em massa (deputados.csv): faixa idLegislaturaInicial..Final e segunda fonte de
# "listado na legislatura" (a lista da API omite parte dos deputados da 51)
bulk_f <- file.path(raw, "deputados_bulk.csv")
if (file.exists(bulk_f)) {
  bulk0 <- fread(bulk_f, sep = ";", encoding = "UTF-8", colClasses = "character")
  setnames(bulk0, tolower(names(bulk0)))
  bulk0[, `:=`(id_deputado = as.integer(sub(".*/", "", uri)),
               li = as.integer(idlegislaturainicial), lf = as.integer(idlegislaturafinal))]
  faixa <- bulk0[!is.na(li) & !is.na(lf) & lf >= min(LEGS) & li <= max(LEGS),
                 .(legislatura_lista = seq(max(li, min(LEGS)), min(lf, max(LEGS)))), by = id_deputado]
  faixa[, origem_lista := "arquivo_massa"]
  n_listados_so_massa <- faixa[!paste(id_deputado, legislatura_lista) %in% listas[, paste(id_deputado, legislatura_lista)], .N]
  listas <- unique(rbindlist(list(listas, faixa), use.names = TRUE), by = c("id_deputado", "legislatura_lista"))
} else n_listados_so_massa <- NA_integer_
ids <- sort(unique(listas$id_deputado))
n_deputados_api <- length(ids)
logm("ids unicos na API (leg 51-57):", n_deputados_api, "| pares id-legislatura so no arquivo em massa:", n_listados_so_massa)

## ---------------------------------------------------------------- detalhe
det <- rbindlist(lapply(ids, function(i) {
  f <- file.path(raw, "deputados", paste0(i, ".json"))
  if (!file.exists(f)) return(data.table(id_deputado = i, detalhe_ok = FALSE))
  d <- fromJSON(f, simplifyVector = FALSE)$dados
  data.table(id_deputado = i, detalhe_ok = TRUE,
             nome_civil = d$nomeCivil %||% NA_character_,
             nome_parlamentar = d$ultimoStatus$nome %||% NA_character_,
             cpf = gsub("\\D", "", d$cpf %||% NA_character_),
             dt_nascimento = d$dataNascimento %||% NA_character_,
             dt_falecimento = d$dataFalecimento %||% NA_character_,
             sexo = d$sexo %||% NA_character_,
             sg_uf_ultimo = d$ultimoStatus$siglaUf %||% NA_character_)
}), fill = TRUE)
det[cpf == "" | nchar(cpf) != 11, cpf := NA_character_]
det[dt_nascimento == "", dt_nascimento := NA_character_]
det[dt_falecimento == "", dt_falecimento := NA_character_]
# arquivo em massa: completa cpf, nascimento e falecimento quando o detalhe os omite
bulk_f <- file.path(raw, "deputados_bulk.csv")
if (file.exists(bulk_f)) {
  bulk <- fread(bulk_f, sep = ";", encoding = "UTF-8", colClasses = "character")
  setnames(bulk, tolower(names(bulk)))
  bulk[, id_deputado := as.integer(sub(".*/", "", uri))]
  bulk[, cpf_b := gsub("\\D", "", cpf)][nchar(cpf_b) != 11, cpf_b := NA_character_]
  bulk[datanascimento == "", datanascimento := NA_character_]
  bulk[datafalecimento == "", datafalecimento := NA_character_]
  det <- merge(det, bulk[, .(id_deputado, cpf_b, nasc_b = datanascimento, fal_b = datafalecimento,
                             nome_civil_b = nomecivil)], by = "id_deputado", all.x = TRUE)
  det[is.na(cpf) & !is.na(cpf_b), cpf := cpf_b]
  det[is.na(dt_nascimento) & !is.na(nasc_b), dt_nascimento := nasc_b]
  det[is.na(dt_falecimento) & !is.na(fal_b), dt_falecimento := fal_b]
  det[is.na(nome_civil) & !is.na(nome_civil_b), nome_civil := nome_civil_b]
  det[, c("cpf_b", "nasc_b", "fal_b", "nome_civil_b") := NULL]
}
det[, nome_norm := norm_nome(nome_civil)]
logm("detalhes lidos:", det[detalhe_ok == TRUE, .N], "| com cpf:", det[!is.na(cpf), .N],
     "| com nascimento:", det[!is.na(dt_nascimento), .N])

## ---------------------------------------------------------------- historico (eventos)
ev <- rbindlist(lapply(ids, function(i) {
  f <- file.path(raw, "historico", paste0(i, ".json"))
  if (!file.exists(f)) return(NULL)
  d <- fromJSON(f, simplifyVector = FALSE)$dados
  if (!length(d)) return(NULL)
  rbindlist(lapply(d, function(h) data.table(
    id_deputado = i,
    legislatura = as.integer(h$idLegislatura %||% NA_integer_),
    data_hora = h$dataHora %||% NA_character_,
    situacao = h$situacao %||% NA_character_,
    condicao_eleitoral = h$condicaoEleitoral %||% NA_character_,
    descricao = h$descricaoStatus %||% NA_character_,
    sg_uf = h$siglaUf %||% NA_character_,
    sg_partido = h$siglaPartido %||% NA_character_,
    nome_evento = trimws(h$nome %||% NA_character_))))
}))
n_hist_ok <- length(unique(ev$id_deputado))
logm("historicos com evento:", n_hist_ok, "| eventos:", nrow(ev))
# tz = "UTC": em America/Sao_Paulo a meia-noite do dia de inicio do horario de verao nao existe
# (ex. 2006-11-05T00:00, 2013-10-20T00:00) e o parse devolvia NA, o que perdia a data do
# evento e quebrava a ordem (correcao da verificacao de 28/08/2026; so a data importa aqui)
ev[, dh := as.POSIXct(data_hora, format = "%Y-%m-%dT%H:%M", tz = "UTC")]
ev[is.na(dh), dh := as.POSIXct(substr(data_hora, 1, 10), format = "%Y-%m-%d", tz = "UTC")]
stopifnot(!anyNA(ev$dh))
# 13/09/2026: o evento pode vir com anotacao antes do tipo ("SGM Adjunta ... conversou com o deputado ... / Entrada - Posse
# de Eleito Titular", Rafael Fera, RO, 31/07/2025), e a posse sem periodo aberto sumia; o tipo vale em qualquer segmento
ev[, tipo := fcase(
  grepl("(^|/ )Entrada", descricao), "entrada",
  grepl("(^|/ )Sa[ií]da", descricao), "saida",
  grepl("Efetiva", descricao), "efetivacao",
  default = "outro")]
# "Entrada - Reassunção / DIVERSO - Convocação" e um evento composto: conta como entrada
# ordem em empate de hora: saida < efetivacao < entrada
ev[, ordem := fcase(tipo == "saida", 0L, tipo == "efetivacao", 1L, tipo == "entrada", 2L, default = 3L)]
setorder(ev, id_deputado, dh, ordem)

## vocabulario fechado de forma de saida
forma_de <- function(situacao, descricao) {
  s <- toupper(situacao %||% ""); d <- toupper(descricao)
  fcase(
    grepl("TERMINO DA LEGISLATURA|TÉRMINO DA LEGISLATURA", d), "fim_regular",
    grepl("RENUNCIA|RENÚNCIA", d), "renuncia",
    grepl("FALECIMENTO", d), "falecimento",
    grepl("PERDA DE MANDATO|CASSA", d), "cassacao",
    grepl("COM PRAZO DETERMINADO", d) & grepl("LICEN", d), "licenca",
    grepl("LICEN", s) & grepl("COM PRAZO DETERMINADO", d), "licenca",
    grepl("AFASTAMENTO", d), "afastamento",
    grepl("SUSPENS", d), "afastamento",
    default = "outro")
}
forma_de_v <- function(situacao, descricao) mapply(forma_de, situacao, descricao, USE.NAMES = FALSE)

## ---------------------------------------------------------------- periodos de exercicio
periodos_de <- function(e) {
  # e: eventos de um deputado, ja ordenados
  out <- list(); aberto <- NULL
  fechar <- function(ab, fim, sit, desc, forma) {
    ab$data_fim_exercicio <- fim; ab$situacao_final <- sit
    ab$descricao_saida <- desc; ab$forma_saida <- forma
    ab
  }
  for (k in seq_len(nrow(e))) {
    r <- e[k]
    if (r$tipo == "entrada") {
      # efetivacao seguida de "Entrada - Reassuncao" no mesmo instante e uma so entrada
      if (!is.null(aberto) && identical(aberto$data_inicio_exercicio, format(r$dh, "%Y-%m-%d")) &&
          isTRUE(aberto$efetivado)) {
        aberto$descricao_entrada <- paste(aberto$descricao_entrada, r$descricao, sep = " / ")
        next
      }
      if (!is.null(aberto)) out[[length(out) + 1]] <- fechar(aberto, NA_character_, "sem_registro_de_saida", NA_character_, "outro")
      cond <- toupper(r$condicao_eleitoral %||% "")
      aberto <- list(legislatura = r$legislatura, data_inicio_exercicio = format(r$dh, "%Y-%m-%d"),
                     condicao = if (cond == "SUPLENTE") "suplente" else "titular",
                     efetivado = cond == "EFETIVADO", descricao_entrada = r$descricao,
                     sg_uf = r$sg_uf, sg_partido = r$sg_partido, nome_evento = r$nome_evento)
    } else if (r$tipo == "saida") {
      # saida registrada pela Camara em legislatura diferente da do periodo aberto (ex. renuncia
      # ao mandato seguinte datada antes do termino da legislatura corrente): nao fecha o
      # periodo aberto; vira saida sem entrada previa na legislatura da propria saida
      # (correcao da verificacao de 28/08/2026; 1 caso, id 160672)
      if (!is.null(aberto) && !is.na(r$legislatura) && !isTRUE(aberto$legislatura == r$legislatura)) {
        out[[length(out) + 1]] <- list(legislatura = r$legislatura, data_inicio_exercicio = NA_character_,
                                       condicao = "titular", efetivado = FALSE, descricao_entrada = NA_character_,
                                       sg_uf = r$sg_uf, sg_partido = r$sg_partido, nome_evento = r$nome_evento,
                                       data_fim_exercicio = format(r$dh, "%Y-%m-%d"), situacao_final = r$situacao,
                                       descricao_saida = r$descricao, forma_saida = forma_de(r$situacao, r$descricao))
        next
      }
      if (is.null(aberto)) {
        cond <- toupper(r$condicao_eleitoral %||% "")
        aberto <- list(legislatura = r$legislatura, data_inicio_exercicio = NA_character_,
                       condicao = if (cond == "SUPLENTE") "suplente" else "titular",
                       efetivado = cond == "EFETIVADO", descricao_entrada = NA_character_,
                       sg_uf = r$sg_uf, sg_partido = r$sg_partido, nome_evento = r$nome_evento)
      }
      out[[length(out) + 1]] <- fechar(aberto, format(r$dh, "%Y-%m-%d"), r$situacao,
                                       r$descricao, forma_de(r$situacao, r$descricao))
      aberto <- NULL
    } else if (r$tipo == "efetivacao") {
      if (!is.null(aberto)) {
        if (aberto$condicao == "suplente" || isTRUE(aberto$efetivado) == FALSE) {
          out[[length(out) + 1]] <- fechar(aberto, format(r$dh, "%Y-%m-%d"), r$situacao,
                                           r$descricao, "suplente_efetivado")
          aberto <- list(legislatura = r$legislatura, data_inicio_exercicio = format(r$dh, "%Y-%m-%d"),
                         condicao = "titular", efetivado = TRUE, descricao_entrada = r$descricao,
                         sg_uf = r$sg_uf, sg_partido = r$sg_partido, nome_evento = r$nome_evento)
        }
      } else {
        # efetivacao de suplente que aguardava convocacao: abre periodo como titular efetivado
        aberto <- list(legislatura = r$legislatura, data_inicio_exercicio = format(r$dh, "%Y-%m-%d"),
                       condicao = "titular", efetivado = TRUE, descricao_entrada = r$descricao,
                       sg_uf = r$sg_uf, sg_partido = r$sg_partido, nome_evento = r$nome_evento)
      }
    }
  }
  if (!is.null(aberto)) {
    if (isTRUE(aberto$legislatura == max(LEGS))) {
      out[[length(out) + 1]] <- fechar(aberto, NA_character_, "Exercício", NA_character_, NA_character_)
    } else {
      out[[length(out) + 1]] <- fechar(aberto, NA_character_, "sem_registro_de_saida", NA_character_, "outro")
    }
  }
  if (!length(out)) return(NULL)
  rbindlist(lapply(out, as.data.table), fill = TRUE)
}
per <- ev[, periodos_de(.SD), by = id_deputado]
per <- per[legislatura %in% LEGS]
logm("periodos de exercicio (leg 51-57):", nrow(per))

## ---------------------------------------------------------------- pareamento com o BOCEL
pess <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character")
mand <- fread("data/mandatos.csv", na.strings = "NA", colClasses = "character")
mand[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo))]
depf <- mand[cd_cargo == 6L & ano_eleicao %in% ano_de_leg(LEGS),
             .(id_mandato, id_pessoa, ano_eleicao, sg_uf_bocel = sg_uf, legislatura = leg_de_ano(ano_eleicao))]
pess[, nome_norm := norm_nome(nome)]

# regra 1: CPF
p_cpf <- pess[!is.na(nr_cpf), .(cpf = nr_cpf, id_pessoa_cpf = id_pessoa)]
# 21/09/2026: CPF que a separacao de pessoas fundidas (R/03) deixou em dois id_pessoa nao escolhe
# nenhum dos dois, e o deputado segue para a regra 2, de nome e nascimento, que os distingue
p_cpf <- p_cpf[!cpf %chin% p_cpf[duplicated(cpf), cpf]]
det <- merge(det, p_cpf, by = "cpf", all.x = TRUE)
# regra 2: nome civil normalizado + data de nascimento
p_nn <- pess[!is.na(dt_nascimento) & nome_norm != "", .(nome_norm, dt_nascimento, id_pessoa_nn = id_pessoa)]
p_nn <- p_nn[!duplicated(p_nn[, .(nome_norm, dt_nascimento)])]
det <- merge(det, p_nn, by = c("nome_norm", "dt_nascimento"), all.x = TRUE)
# regra 3: nome civil normalizado unico entre as pessoas com mandato de deputado federal no BOCEL
p_nu <- pess[id_pessoa %in% depf$id_pessoa & nome_norm != "", .(nome_norm, id_pessoa_nu = id_pessoa)]
p_nu <- p_nu[, if (.N == 1L) .SD, by = nome_norm]
p_nu <- p_nu[nome_norm %in% pess[, .N, by = nome_norm][N == 1L, nome_norm]]
det <- merge(det, p_nu, by = "nome_norm", all.x = TRUE)
det[, id_pessoa := fcase(!is.na(id_pessoa_cpf), id_pessoa_cpf,
                         !is.na(id_pessoa_nn), id_pessoa_nn,
                         !is.na(id_pessoa_nu), id_pessoa_nu,
                         default = NA_character_)]
det[, regra_pareamento := fcase(!is.na(id_pessoa_cpf), "cpf",
                                !is.na(id_pessoa_nn), "nome_nascimento",
                                !is.na(id_pessoa_nu), "nome_unico_dep_fed",
                                default = NA_character_)]
# uma pessoa do BOCEL nao pode receber dois ids da Camara pela regra 3
det[regra_pareamento == "nome_unico_dep_fed" & id_pessoa %in% det[regra_pareamento != "nome_unico_dep_fed", id_pessoa],
    `:=`(id_pessoa = NA_character_, regra_pareamento = NA_character_)]
# regra 4 (13/09/2026): mesma data de nascimento, mesmo primeiro nome e mandato de deputado federal do TSE na mesma UF
# e legislatura, com par unico. Cobre quem mudou de sobrenome entre o registro de candidatura e a posse (eleita em
# 2002 como Elani Costa Theophilo e empossada como Elani Carvalho Costa, que o banco dava como sem registro na Camara)
leg_uf <- unique(per[, .(id_deputado, legislatura, sg_uf)])
c4 <- merge(leg_uf, det[is.na(id_pessoa) & !is.na(dt_nascimento), .(id_deputado, dt_nascimento, primeiro = sub(" .*", "", nome_norm))],
            by = "id_deputado")
p4 <- merge(depf[, .(id_pessoa, legislatura, sg_uf = sg_uf_bocel)],
            pess[!is.na(dt_nascimento), .(id_pessoa, dt_nascimento, primeiro = sub(" .*", "", nome_norm))], by = "id_pessoa")
j4 <- merge(c4, p4, by = c("legislatura", "sg_uf", "dt_nascimento", "primeiro"))
j4 <- j4[, if (uniqueN(id_pessoa) == 1L) .SD[1L], by = id_deputado]
j4 <- j4[!id_pessoa %in% det[!is.na(id_pessoa), id_pessoa] & !duplicated(id_pessoa)]
det[j4, on = "id_deputado", `:=`(id_pessoa = i.id_pessoa, regra_pareamento = "nascimento_uf_legislatura_primeiro_nome")]
registrar_numero("camara_pareados_regra_nascimento_uf_legislatura", nrow(j4), script = "R/07_exercicio_camara.R")
n_pareados_bocel <- det[!is.na(id_pessoa), uniqueN(id_deputado)]
logm("deputados da API pareados ao BOCEL:", n_pareados_bocel, "de", n_deputados_api,
     "| por cpf:", det[regra_pareamento == "cpf", .N], "| por nome+nascimento:", det[regra_pareamento == "nome_nascimento", .N])

## ---------------------------------------------------------------- montar tabela final
per <- merge(per, det[, .(id_deputado, id_pessoa, regra_pareamento, nome_civil, nome_parlamentar,
                          cpf, dt_nascimento, dt_falecimento)], by = "id_deputado", all.x = TRUE)
# id_mandato: mandato de deputado federal da pessoa na eleicao que abre a legislatura
per <- merge(per, depf[, .(id_pessoa, legislatura, id_mandato)], by = c("id_pessoa", "legislatura"), all.x = TRUE)

# eleitos do BOCEL sem linha de exercicio na legislatura: nao tomou posse (pareado) ou nao pareado
cob <- merge(depf, unique(per[!is.na(id_mandato), .(id_mandato, tem_exercicio = TRUE)]), by = "id_mandato", all.x = TRUE)
cob[is.na(tem_exercicio), tem_exercicio := FALSE]
cob <- merge(cob, unique(det[!is.na(id_pessoa), .(id_pessoa, id_deputado, nome_civil, nome_parlamentar, cpf,
                                                    dt_nascimento, dt_falecimento, regra_pareamento)][!duplicated(id_pessoa)]),
             by = "id_pessoa", all.x = TRUE)
cob[, pareado := !is.na(id_deputado)]
# janela de cada legislatura (API /legislaturas/{id}: 51 = 1999-02-01 a 2003-01-31)
leg_ini <- function(l) sprintf("%d-02-01", 1999L + 4L * (l - 51L))
leg_fim <- function(l) sprintf("%d-01-31", 2003L + 4L * (l - 51L))
# o historico da API nao tem evento de posse/saida para a legislatura 51 (nenhum dos listados);
# quem a Camara lista na legislatura e sem entrada no historico fica como
# "listado_sem_historico": exerceu segundo a lista, datas nao observadas
cob <- merge(cob, unique(listas[, .(id_deputado, legislatura = legislatura_lista, listado = TRUE)]),
             by = c("id_deputado", "legislatura"), all.x = TRUE)
cob[is.na(listado), listado := FALSE]
faltantes <- cob[tem_exercicio == FALSE]
# ultimo evento substantivo (nao placeholder) na legislatura, para os pareados sem entrada
ult_ev <- ev[tipo != "entrada" & !grepl("^Nome no in", descricao), .SD[.N], by = .(id_deputado, legislatura)][
  , .(id_deputado, legislatura, sit_ev = situacao, desc_ev = descricao, dh_ev = format(dh, "%Y-%m-%d"))]
faltantes <- merge(faltantes, ult_ev, by = c("id_deputado", "legislatura"), all.x = TRUE)
faltantes[, faleceu_na_leg := !is.na(dt_falecimento) & dt_falecimento >= leg_ini(legislatura) & dt_falecimento <= leg_fim(legislatura)]
faltantes[, `:=`(
  # "nao tomou posse" so e inferido onde o historico da legislatura existe (52-57);
  # na 51, sem historico, a ausencia na lista nao permite inferir (ha eleitos que exerceram
  # e nao constam da lista da API)
  forma_saida = fcase(
    !pareado, "outro",
    !listado & legislatura == 51L, "outro",
    !listado, "nao_tomou_posse",
    grepl("Negativa|Impossibilidade de Assumir", fifelse(is.na(desc_ev), "", desc_ev)), "nao_tomou_posse",
    listado & faleceu_na_leg, "falecimento",
    default = "outro"),
  situacao_final = fcase(
    !pareado, "nao_pareado_camara",
    !listado & legislatura == 51L, "nao_listado_na_legislatura_sem_historico",
    !listado, "nao_listado_na_legislatura",
    !is.na(sit_ev), sit_ev,
    listado & faleceu_na_leg, "falecimento_arquivo_massa",
    default = "listado_sem_historico"),
  descricao_saida = fcase(
    !is.na(desc_ev), desc_ev,
    listado & faleceu_na_leg, "dataFalecimento (arquivo em massa deputados.csv)",
    default = NA_character_),
  data_fim_exercicio = fifelse(listado & faleceu_na_leg & is.na(desc_ev), dt_falecimento, NA_character_))]
extra <- faltantes[, .(
  id_deputado, id_pessoa, id_mandato, legislatura,
  data_inicio_exercicio = NA_character_, data_fim_exercicio,
  condicao = "titular", efetivado = FALSE,
  descricao_entrada = fifelse(listado, "listado na legislatura pela API (/deputados?idLegislatura), sem evento no historico", NA_character_),
  situacao_final, descricao_saida, forma_saida,
  sg_uf = sg_uf_bocel, sg_partido = NA_character_, nome_evento = NA_character_,
  regra_pareamento, nome_civil, nome_parlamentar, cpf, dt_nascimento, dt_falecimento)]
n_nao_tomou_posse <- extra[forma_saida == "nao_tomou_posse", .N]
n_eleitos_nao_pareados <- extra[situacao_final == "nao_pareado_camara", .N]
n_listado_sem_hist <- extra[situacao_final %in% c("listado_sem_historico", "falecimento_arquivo_massa"), .N]
logm("eleitos BOCEL sem periodo no historico:", nrow(extra), "| nao tomou posse:", n_nao_tomou_posse,
     "| listados sem historico:", n_listado_sem_hist, "| nao pareados:", n_eleitos_nao_pareados)

# suplentes listados na legislatura sem nenhuma entrada no historico e sem eleicao no BOCEL:
# na legislatura 51 (sem historico) entram como suplentes com datas nao observadas;
# nas demais, quem esta listado sem entrada e convocado que nao assumiu, e nao entra
sup51 <- listas[legislatura_lista == 51L & !id_deputado %in% c(per[legislatura == 51L, id_deputado], cob[legislatura == 51L, id_deputado])]
sup51 <- merge(sup51, det[, .(id_deputado, id_pessoa, regra_pareamento, nome_civil, nome_parlamentar, cpf, dt_nascimento, dt_falecimento)], by = "id_deputado")
uf51 <- ev[legislatura == 51L & !is.na(sg_uf), .(sg_uf = sg_uf[.N], sg_partido = sg_partido[.N]), by = id_deputado]
sup51 <- merge(sup51, uf51, by = "id_deputado", all.x = TRUE)
# sem evento na 51 com UF: usa a UF do ultimoStatus do detalhe (correcao da verificacao de 28/08/2026)
sup51 <- merge(sup51, det[, .(id_deputado, sg_uf_ultimo)], by = "id_deputado", all.x = TRUE)
n_uf51_ultimo_status <- sup51[is.na(sg_uf) & !is.na(sg_uf_ultimo), .N]
sup51[is.na(sg_uf), sg_uf := sg_uf_ultimo]
sup51[, faleceu_na_leg := !is.na(dt_falecimento) & dt_falecimento >= leg_ini(51L) & dt_falecimento <= leg_fim(51L)]
extra_sup51 <- sup51[, .(
  id_deputado, id_pessoa, id_mandato = NA_character_, legislatura = 51L,
  data_inicio_exercicio = NA_character_,
  data_fim_exercicio = fifelse(faleceu_na_leg, dt_falecimento, NA_character_),
  condicao = "suplente", efetivado = FALSE,
  descricao_entrada = "listado na legislatura pela API (/deputados?idLegislatura), sem evento no historico",
  situacao_final = fifelse(faleceu_na_leg, "falecimento_arquivo_massa", "listado_sem_historico"),
  descricao_saida = fifelse(faleceu_na_leg, "dataFalecimento (arquivo em massa deputados.csv)", NA_character_),
  forma_saida = fifelse(faleceu_na_leg, "falecimento", "outro"),
  sg_uf, sg_partido, nome_evento = NA_character_,
  regra_pareamento, nome_civil, nome_parlamentar, cpf, dt_nascimento, dt_falecimento)]
n_sup51 <- nrow(extra_sup51)
logm("suplentes listados na legislatura 51 sem historico:", n_sup51)
extra <- rbindlist(list(extra, extra_sup51), use.names = TRUE)
# listados sem entrada nas legislaturas 52-57 (convocados que nao assumiram): so registrados
list_sem_entrada <- listas[legislatura_lista >= 52L][
  !paste(id_deputado, legislatura_lista) %in% per[, paste(id_deputado, legislatura)], .N, by = legislatura_lista]

# saida registrada no historico sem evento de entrada anterior (licencas consecutivas, saida
# na primeira linha da legislatura): o inicio do periodo fica NA e a linha e marcada
n_saida_sem_entrada <- per[is.na(data_inicio_exercicio) & is.na(descricao_entrada), .N]
per[is.na(data_inicio_exercicio) & is.na(descricao_entrada),
    descricao_entrada := "sem evento de entrada no historico antes desta saida (inicio nao observado)"]
final <- rbindlist(list(per, extra), use.names = TRUE, fill = TRUE)
final[, `:=`(fonte = "camara_api",
             url_fonte = sprintf("https://dadosabertos.camara.leg.br/api/v2/deputados/%d/historico", id_deputado),
             ano_eleicao = ano_de_leg(legislatura))]
final[is.na(id_deputado), url_fonte := NA_character_]
final <- final[, .(id_pessoa, id_mandato, id_deputado_camara = id_deputado, legislatura, ano_eleicao,
                   nome_civil, nome_parlamentar, cpf, dt_nascimento, dt_falecimento,
                   sg_uf, sg_partido, condicao, efetivado,
                   data_inicio_exercicio, data_fim_exercicio, situacao_final, forma_saida,
                   descricao_entrada, descricao_saida, regra_pareamento, fonte, url_fonte)]
setorder(final, legislatura, sg_uf, nome_civil, data_inicio_exercicio, na.last = TRUE)

## ---------------------------------------------------------------- asserts
stopifnot(all(final$forma_saida %in% c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento",
                                       "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", NA)))
stopifnot(all(final$condicao %in% c("titular", "suplente")))
stopifnot(all(final$legislatura %in% LEGS))
stopifnot(!anyDuplicated(final[!is.na(data_inicio_exercicio), .(id_deputado_camara, legislatura, data_inicio_exercicio, condicao)]))
# datas de fim posteriores ao inicio
chk <- final[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio)]
stopifnot(all(as.IDate(chk$data_fim_exercicio) >= as.IDate(chk$data_inicio_exercicio)))
# em exercicio sem saida so na legislatura corrente
stopifnot(all(final[is.na(forma_saida) & !is.na(data_inicio_exercicio), legislatura] == max(LEGS)))
# cada mandato do BOCEL aparece
stopifnot(all(depf$id_mandato %in% final$id_mandato))

## ---------------------------------------------------------------- taxa de pareamento por legislatura
tx <- cob[, .(eleitos_bocel = .N, pareados = sum(pareado), com_exercicio = sum(tem_exercicio),
              taxa_pareamento = round(sum(pareado) / .N, 4)), by = .(legislatura, ano_eleicao)]
setorder(tx, legislatura)
fwrite(tx, "output/verificacao/camara_pareamento_por_legislatura.csv")
fs <- final[, .N, by = forma_saida][order(-N)]
fwrite(fs, "output/verificacao/camara_forma_saida.csv")
print(tx); print(fs)
print(final[, .N, by = .(condicao, efetivado)])

## ---------------------------------------------------------------- salvar
salvar <- function(dt, nome) {
  fwrite(dt, file.path("data", paste0(nome, ".csv")), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
  write_parquet(dt, file.path("data", paste0(nome, ".parquet")))
  saveRDS(dt, file.path("data", paste0(nome, ".rds")))
  logm(nome, ":", nrow(dt), "linhas x", ncol(dt), "colunas")
}
salvar(final, "exercicio_camara")

## ---------------------------------------------------------------- registrar numeros
reg <- function(k, v) registrar_numero(k, v, script = SCRIPT, out = "output/numeros_assinatura.txt")
reg("camara_n_deputados_api", n_deputados_api)
reg("camara_n_historicos_com_evento", n_hist_ok)
reg("camara_n_eventos_historico", nrow(ev))
reg("camara_n_pareados_bocel", n_pareados_bocel)
reg("camara_n_pareados_por_cpf", det[regra_pareamento == "cpf", .N])
reg("camara_n_pareados_por_nome_nascimento", det[regra_pareamento == "nome_nascimento", .N])
reg("camara_n_linhas_exercicio", nrow(final))
reg("camara_n_periodos_exercicio", nrow(per))
reg("camara_n_eleitos_bocel_dep_fed", nrow(depf))
reg("camara_n_eleitos_nao_tomou_posse", n_nao_tomou_posse)
reg("camara_n_eleitos_nao_pareados", n_eleitos_nao_pareados)
reg("camara_n_pareados_por_nome_unico_dep_fed", det[regra_pareamento == "nome_unico_dep_fed", .N])
reg("camara_n_eleitos_listados_sem_historico", n_listado_sem_hist)
reg("camara_n_suplentes_leg51_listados_sem_historico", n_sup51)
reg("camara_n_suplentes_fora_bocel", final[condicao == "suplente" & is.na(id_pessoa), uniqueN(id_deputado_camara)])
reg("camara_n_falecimentos_arquivo_massa", final[situacao_final == "falecimento_arquivo_massa", .N])
reg("camara_n_pares_id_legislatura_so_arquivo_massa", n_listados_so_massa)
reg("camara_n_periodos_leg51_no_historico", per[legislatura == 51L, .N])
reg("camara_n_saidas_sem_entrada_previa", n_saida_sem_entrada)
reg("camara_n_uf_sup51_do_ultimo_status", n_uf51_ultimo_status)
for (i in seq_len(nrow(list_sem_entrada))) reg(sprintf("camara_n_listados_sem_entrada_leg%d", list_sem_entrada$legislatura_lista[i]), list_sem_entrada$N[i])
for (L in LEGS) reg(sprintf("camara_n_listados_api_leg%d", L), listas[legislatura_lista == L, .N])
for (L in LEGS) reg(sprintf("camara_n_com_periodo_exercicio_leg%d", L), tx[legislatura == L, com_exercicio])
for (i in seq_len(nrow(tx))) reg(sprintf("camara_taxa_pareamento_leg%d", tx$legislatura[i]),
                                 sprintf("%d/%d=%.4f", tx$pareados[i], tx$eleitos_bocel[i], tx$taxa_pareamento[i]))
for (i in seq_len(nrow(fs))) reg(sprintf("camara_n_forma_saida_%s", fs$forma_saida[i] %||% "em_exercicio"), fs$N[i])
gravar_relatorio_verificacao(
  alvo = "data/exercicio_camara.csv", script = SCRIPT,
  passou = c("vocabulario_forma_saida_fechado", "condicao_titular_suplente", "legislaturas_51_57",
             "sem_periodo_duplicado", "fim_posterior_ao_inicio", "exercicio_aberto_so_na_leg_57",
             "todo_mandato_dep_fed_do_bocel_presente"),
  fora_de_cobertura = c("exatidao_das_datas_da_camara", "pareamento_por_nome_em_homonimos",
                        "suplentes_sem_cpf_na_api"))
logm("07_exercicio_camara: concluido.")
close(logf)
