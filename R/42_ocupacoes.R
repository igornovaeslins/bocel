# 42_ocupacoes.R — a cadeira e quem a ocupou
#
# Decisao de 30/08/2026 (pendencia 5): o suplente que assume tem mandato e precisa
# aparecer "sem aumentar o N total de cadeiras, por lugar, mas aumentando o N de ocupantes
# dessas cadeiras". Este script separa os dois niveis. A cadeira continua em mandatos.csv,
# com uma linha por cadeira ganha na eleicao e o N intacto. Sobre ela vem data/ocupacoes.csv,
# com uma linha por cadeira x pessoa x periodo: o titular e a primeira ocupacao, e cada
# suplente convocado, vice que assume ou interino e outra.
#
# O que o suplente muda no dado: a cadeira pode trocar de partido por dentro, sem eleicao,
# quando o convocado vem de outra legenda da coligacao ou da federacao. A coluna
# partido_difere_do_titular marca exatamente isso.
#
# Vinculo com a cadeira. O senador suplente ja nasce ligado a cadeira pela chapa. Nas
# proporcionais a fonte nem sempre diz qual titular saiu, e por isso id_mandato so e
# preenchido quando ha como saber; vinculo_cadeira declara o caminho em cada linha, e
# ocupacao sem cadeira identificada continua contando como ocupante da casa.
#
# Entrada: data/mandatos.csv, data/lista_suplencia.csv, data/suplentes_identidade.csv,
#          data/exercicio_*.csv, data/wikipedia_*.csv
# Saida:   data/ocupacoes.csv|parquet
# Execucao: Rscript --vanilla R/42_ocupacoes.R
set.seed(20260830)
suppressPackageStartupMessages({library(data.table); library(arrow); library(stringi)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root,"lib","proveniencia.R"))
ESTE <- file.path(root,"R","42_ocupacoes.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
norm <- function(x) {
  y <- stri_trans_general(toupper(x), "Latin-ASCII")
  y <- gsub("[^A-Z ]", "", y); gsub(" +", " ", trimws(y))
}
d <- function(x) as.IDate(x)

mand <- fread(file.path(root,"data","mandatos.csv"))
for (cc in c("mandato_inicio","mandato_fim","data_posse","data_fim_efetiva"))
  if (cc %in% names(mand)) mand[[cc]] <- as.character(mand[[cc]])
lista <- fread(file.path(root,"data","lista_suplencia.csv"))
lista[, `:=`(nome_n = norm(nome), urna_n = norm(nome_urna))]

## ---------------------------------------------------------------- 1. o titular
# Primeira ocupacao de toda cadeira, com o que a camada de posse e saida ja apurou.
oc_tit <- mand[, .(
  id_mandato, id_lista = NA_character_, id_pessoa,
  ano_eleicao, cd_cargo, cargo, esfera, sg_uf, sg_ue, nm_ue, unidade_posicao,
  tipo_ocupante = "titular", ordem_ocupacao = 1L,
  sg_partido_ocupante = sg_partido, sg_partido_titular = sg_partido,
  data_inicio = fifelse(!is.na(data_posse), as.character(data_posse), mandato_inicio),
  data_fim = fifelse(!is.na(data_fim_efetiva), as.character(data_fim_efetiva), NA_character_),
  origem_data_inicio = fifelse(!is.na(data_posse), "fonte", "convencao"),
  forma_saida, fonte_forma_saida,
  vinculo_cadeira = "eleicao",
  fonte = fifelse(is.na(fonte_forma_saida), "tse", fonte_forma_saida), url = NA_character_
)]
reg("ocu_titulares", nrow(oc_tit))

## ---------------------------------------------------------------- 2. ocupantes nao titulares
# Cada fonte entra normalizada no mesmo formato. `chave_unidade` e a unidade da posicao
# (UF nos cargos estaduais e federais, municipio nos municipais), que junto de cargo e ano
# da eleicao delimita o universo de suplentes elegiveis daquela cadeira.
partes <- list()
add <- function(x) if (!is.null(x) && nrow(x)) partes[[length(partes) + 1L]] <<- x

le <- function(f, ...) { p <- file.path(root,"data",f); if (file.exists(p)) fread(p, ...) else NULL }

## Camara dos Deputados
x <- le("exercicio_camara.csv")
if (!is.null(x)) add(x[condicao == "suplente", .(
  fonte = "camara_api", cd_cargo = 6L, ano_eleicao = as.integer(ano_eleicao),
  unidade = sg_uf, sg_uf, id_pessoa_fonte = id_pessoa,
  nome_fonte = nome_civil, nome_alt = nome_parlamentar, partido_fonte = sg_partido,
  data_inicio = as.character(data_inicio_exercicio), data_fim = as.character(data_fim_exercicio),
  forma_saida, titular_nome = NA_character_, url = url_fonte,
  tipo_ocupante = "suplente")])

## Senado
x <- le("exercicio_senado.csv")
if (!is.null(x)) add(x[condicao == "suplente", .(
  fonte = "senado_api", cd_cargo = 5L, ano_eleicao = as.integer(ano_eleicao_ref),
  unidade = sg_uf, sg_uf, id_pessoa_fonte = id_pessoa,
  nome_fonte = nome_senado, nome_alt = nome_parlamentar, partido_fonte = NA_character_,
  data_inicio = as.character(data_inicio_exercicio), data_fim = as.character(data_fim_exercicio),
  forma_saida, titular_nome, url = url_fonte, tipo_ocupante = "suplente")])

## Camara e Senado: quem exerceu como titular sem mandato no arquivo do TSE (R/58, 13/09/2026). O suplente efetivado
## entra pela lista partidaria; o empossado antes da retotalizacao, o eleito cassado depois, o substituto por decisao
## judicial, o terceiro colocado convocado e o eleito em suplementar trazem a cadeira da tabela curada com fonte oficial.
x <- le("ocupantes_legislativo_federal.csv", colClasses = "character", na.strings = "NA")
if (!is.null(x)) {
  x <- x[!is.na(tipo_ocupante)]
  x[, ano_leg := as.integer(1998L + (as.integer(legislatura) - 51L) * 4L)]
  add(x[, .(
    fonte = fonte, cd_cargo = as.integer(cd_cargo),
    ano_eleicao = fifelse(!is.na(id_mandato_cadeira), as.integer(substr(id_mandato_cadeira, 2, 5)), ano_leg),
    unidade = sg_uf, sg_uf, id_pessoa_fonte = id_pessoa, nome_fonte, nome_alt, partido_fonte = NA_character_,
    data_inicio, data_fim, forma_saida = NA_character_, titular_nome = NA_character_, url = url_fonte,
    tipo_ocupante, id_mandato_explicito = id_mandato_cadeira)])
}

## Assembleias (tres coletas, mesmo esquema)
for (f in c("exercicio_assembleias_2.csv","exercicio_assembleias.csv","exercicio_assembleias_historico.csv")) {
  x <- le(f)
  if (is.null(x) || !"condicao" %in% names(x)) next
  add(x[condicao == "suplente", .(
    fonte = paste0("assembleia:", sub("^exercicio_assembleias_?", "", sub("\\.csv$","",f))),
    cd_cargo = fifelse(uf == "DF", 8L, 7L), ano_eleicao = as.integer(ano_eleicao),
    unidade = uf, sg_uf = uf, id_pessoa_fonte = id_pessoa_bocel,
    nome_fonte = fifelse(!is.na(nome_completo), nome_completo, nome), nome_alt = nome,
    partido_fonte = partido,
    data_inicio = as.character(data_inicio_exercicio), data_fim = as.character(data_fim_exercicio),
    forma_saida, titular_nome = NA_character_, url, tipo_ocupante = "suplente")])
}

## Camaras municipais (SAPL e portais sem SAPL)
for (f in c("exercicio_camaras_municipais.csv","exercicio_camaras_sem_sapl.csv",
            "exercicio_camaras_sem_sapl_2.csv")) {
  x <- le(f)
  if (is.null(x) || !"titular" %in% names(x)) next
  add(x[titular %in% c(FALSE, "False", "false"), .(
    fonte = sub("\\.csv$","",f), cd_cargo = 13L, ano_eleicao = as.integer(ano_eleicao_bocel),
    unidade = as.character(sg_ue), sg_uf = uf, id_pessoa_fonte = id_pessoa_bocel,
    nome_fonte = nome_fonte, nome_alt = nome_parlamentar, partido_fonte = NA_character_,
    data_inicio = as.character(data_inicio_mandato), data_fim = as.character(data_fim_mandato),
    forma_saida, titular_nome = NA_character_, url, tipo_ocupante = "suplente")])
}

## Wikipedia estadual (suplente e substituto)
x <- le("wikipedia_estadual.csv")
# a coluna cargo da propria fonte diz de que cadeira se trata; as 78 linhas com
# condicao 'substituto' sao governador que assumiu, e fixar o cargo em deputado estadual
# punha o ocupante na cadeira errada (achado da auditoria de 30/08/2026)
if (!is.null(x)) add(x[condicao %in% c("suplente","substituto"), .(
  fonte = "wikipedia",
  cd_cargo = fcase(grepl("GOVERNADOR", toupper(cargo)), 3L,
                   uf == "DF", 8L, default = 7L),
  ano_eleicao = as.integer(ano_eleicao_bocel), unidade = uf, sg_uf = uf,
  id_pessoa_fonte = id_pessoa_bocel, nome_fonte = nome_wiki, nome_alt = nome_wiki,
  partido_fonte = partido_wiki, data_inicio = as.character(inicio), data_fim = as.character(fim),
  forma_saida, titular_nome = NA_character_, url = url_pagina,
  tipo_ocupante = fifelse(grepl("GOVERNADOR", toupper(cargo)), "vice_assumiu", "suplente"))])

## Prefeituras: quem assume nao e suplente, e o vice eleito ou um interino
x <- le("wikipedia_prefeitos.csv")
if (!is.null(x)) add(x[condicao %in% c("interino","vice_em_exercicio"), .(
  fonte = "wikipedia", cd_cargo = 11L, ano_eleicao = as.integer(ano_eleicao_bocel),
  unidade = as.character(sg_ue), sg_uf = uf, id_pessoa_fonte = id_pessoa_bocel,
  nome_fonte = nome_wiki, nome_alt = nome_wiki, partido_fonte = partido_wiki,
  data_inicio = as.character(inicio), data_fim = as.character(fim), forma_saida,
  titular_nome = NA_character_, url,
  tipo_ocupante = fifelse(condicao == "vice_em_exercicio", "vice_assumiu", "interino"))])

ocup <- rbindlist(partes, use.names = TRUE, fill = TRUE)
ocup[, linha := .I]
# O SAPL marca como suplente 347 pessoas que o TSE registra como eleitas (auditoria do proprio
# banco). Quando a pessoa da linha ja e titular de cadeira
# na mesma unidade, cargo e eleicao, a linha nao descreve ocupacao nova e sai.
tit_ref <- mand[, .(id_pessoa, ano_eleicao, cd_cargo, unidade = unidade_posicao, ja_titular = TRUE)]
ocup[tit_ref, on = .(id_pessoa_fonte = id_pessoa, ano_eleicao, cd_cargo, unidade),
     ja_titular := i.ja_titular]
n_falso <- ocup[ja_titular %in% TRUE, .N]
reg("ocu_falso_suplente_descartado", n_falso)
if (n_falso) fwrite(ocup[ja_titular %in% TRUE],
                    file.path(root, "output", "verificacao", "falso_suplente_descartado.csv"))
ocup <- ocup[!(ja_titular %in% TRUE)]
ocup[, linha := .I]
reg("ocu_linhas_de_fonte", nrow(ocup))
print(ocup[, .N, by = .(fonte, tipo_ocupante)][order(-N)])

## ---------------------------------------------------------------- 3. identidade do ocupante
# Onde a fonte ja tinha pareado a pessoa (porque ela foi eleita alguma vez), esse id manda.
# O resto pareia contra a lista de suplencia da mesma eleicao, cargo e unidade, por nome
# completo e depois por nome de urna, exigindo que o nome leve a uma unica pessoa.
ocup[, `:=`(nome_n = norm(nome_fonte), alt_n = norm(nome_alt))]
casar <- function(col_ocup, col_lista, rotulo) {
  alvo <- ocup[is.na(id_pessoa_fonte) & is.na(id_pessoa_casado) & !is.na(get(col_ocup)) & get(col_ocup) != ""]
  if (!nrow(alvo)) return(invisible(NULL))
  cand <- lista[!is.na(get(col_lista)) & get(col_lista) != "",
                .(n_pessoas = uniqueN(id_pessoa), id_pessoa = id_pessoa[1],
                  id_lista = id_lista[1], sg_partido_sup = sg_partido[1],
                  ordem_suplencia = ordem_suplencia[1], id_mandato_cadeira = id_mandato_cadeira[1]),
                by = c("ano_eleicao","cd_cargo","unidade_posicao", col_lista)]
  cand <- cand[n_pessoas == 1]
  setnames(cand, col_lista, "chave_nome")
  alvo <- merge(alvo[, .(linha, ano_eleicao, cd_cargo, unidade, chave_nome = get(col_ocup))],
                cand, by.x = c("ano_eleicao","cd_cargo","unidade","chave_nome"),
                by.y = c("ano_eleicao","cd_cargo","unidade_posicao","chave_nome"))
  ocup[alvo, on = "linha", `:=`(id_pessoa_casado = i.id_pessoa, id_lista = i.id_lista,
                                sg_partido_sup = i.sg_partido_sup,
                                ordem_suplencia = i.ordem_suplencia,
                                id_mandato_chapa = i.id_mandato_cadeira,
                                regra_pareamento_ocupacao = rotulo)]
  invisible(NULL)
}
ocup[, `:=`(id_pessoa_casado = NA_character_, id_lista = NA_character_,
            sg_partido_sup = NA_character_, ordem_suplencia = NA_integer_,
            id_mandato_chapa = NA_character_, regra_pareamento_ocupacao = NA_character_)]
casar("nome_n", "nome_n", "nome_completo_x_lista")
casar("alt_n", "nome_n", "nome_parlamentar_x_lista")
casar("nome_n", "urna_n", "nome_completo_x_urna")
casar("alt_n", "urna_n", "nome_parlamentar_x_urna")
ocup[, id_pessoa := fifelse(!is.na(id_pessoa_fonte), id_pessoa_fonte, id_pessoa_casado)]
reg("ocu_com_pessoa_ja_pareada_na_fonte", ocup[!is.na(id_pessoa_fonte), .N])
reg("ocu_pessoa_casada_com_lista_suplencia", ocup[is.na(id_pessoa_fonte) & !is.na(id_pessoa_casado), .N])
reg("ocu_sem_identidade", ocup[is.na(id_pessoa), .N])
print(ocup[, .N, by = regra_pareamento_ocupacao][order(-N)])

## ---------------------------------------------------------------- 4. a cadeira ocupada
# A suplencia pertence a lista, e por isso o suplente so pode ter ocupado uma cadeira que a
# propria lista ganhou. Com data/mandatos_lista.csv (cadeira -> lista) o vinculo se faz em
# tres caminhos, do mais firme ao mais fraco, e cada linha declara qual usou:
#   chapa_senado      a chapa do Senado ja nomeia a cadeira do titular
#   lista_vaga_unica  a lista tem uma unica cadeira que vagou, e nao ha o que confundir
#   lista_data        entre as cadeiras que vagaram, a que vagou mais perto da entrada
# Quando nenhum caminho resolve, id_mandato fica vazio e vinculo_cadeira diz
# casa_legislatura: a ocupacao continua contando como ocupante da casa, sem inventar cadeira.
ml <- fread(file.path(root,"data","mandatos_lista.csv"))
ml[, `:=`(data_fim_efetiva = as.IDate(as.character(data_fim_efetiva)),
          mandato_fim = as.IDate(as.character(mandato_fim)))]
VAGA <- c("renuncia","cassacao","falecimento","licenca","afastamento","nao_tomou_posse",
          "assumiu_titular","suplente_efetivado","outro")
ml[, vagou := forma_saida %in% VAGA |
     (!is.na(data_fim_efetiva) & !is.na(mandato_fim) & data_fim_efetiva < mandato_fim)]
ml[, data_vacancia := data_fim_efetiva]

ocup[, ini_d := as.IDate(data_inicio)]
ocup[, id_mandato := id_mandato_chapa]
ocup[, vinculo_cadeira := fifelse(!is.na(id_mandato_chapa), "chapa_senado", NA_character_)]
# 13/09/2026: cadeira nomeada pela tabela curada com fonte oficial (ocupantes federais sem mandato no TSE)
if (!"id_mandato_explicito" %in% names(ocup)) ocup[, id_mandato_explicito := NA_character_]
ocup[!is.na(id_mandato_explicito), `:=`(id_mandato = id_mandato_explicito, vinculo_cadeira = "fonte_da_casa")]

# Cargo majoritario nao tem fila de suplencia, e a cadeira e uma so na unidade: quem assume
# a prefeitura, o governo ou a presidencia so pode estar ocupando aquela cadeira. O vinculo,
# portanto, sai da propria unidade e da eleicao, sem inferencia.
MAJ <- c(1L, 3L, 11L)
cad_maj <- mand[cd_cargo %in% MAJ, .(n = .N, id_mandato = id_mandato[1]),
             by = .(ano_eleicao, cd_cargo, unidade_posicao)][n == 1L]
ocup[cad_maj, on = .(ano_eleicao, cd_cargo, unidade = unidade_posicao),
     `:=`(id_mandato = fifelse(is.na(id_mandato), i.id_mandato, id_mandato),
          vinculo_cadeira = fifelse(is.na(vinculo_cadeira), "cadeira_unica_da_unidade", vinculo_cadeira))]
reg("ocu_vinculo_cadeira_unica", ocup[vinculo_cadeira %in% "cadeira_unica_da_unidade", .N])

alvo <- ocup[is.na(id_mandato) & !is.na(id_lista), .(linha, id_lista, ini_d)]
cand_cad <- ml[vagou == TRUE, .(id_lista, id_mandato, data_vacancia, sg_partido_tit = sg_partido)]
j <- merge(alvo, cand_cad, by = "id_lista", allow.cartesian = TRUE)
# uma unica cadeira vaga na lista resolve sozinha
u <- j[, .(n = .N, id_mandato = id_mandato[1]), by = linha][n == 1]
ocup[u, on = "linha", `:=`(id_mandato = i.id_mandato, vinculo_cadeira = "lista_vaga_unica")]
# varias cadeiras vagas: fica a que vagou mais perto da entrada do suplente
mult <- j[linha %in% j[, .N, by = linha][N > 1]$linha & !linha %in% u$linha]
# A cadeira tem de ter vagado ANTES de o suplente entrar, com tolerancia de 30 dias para o
# atraso de registro, e a menos de um ano de distancia. Sem isso o argmin ligava o convocado a
# cadeiras que so vagaram depois (130 de 300 casos, ate 1.413 dias adiante), o que invertia a
# ordem causal do vinculo. Achado da auditoria de 30/08/2026.
mult[, atraso := as.integer(ini_d) - as.integer(data_vacancia)]   # positivo: vagou antes
mult <- mult[!is.na(atraso) & atraso >= -30L & atraso <= 365L]
mult[, dist := abs(atraso)]
setorder(mult, linha, dist, na.last = TRUE)
esc <- mult[!is.na(dist), .SD[1], by = linha]
ocup[esc, on = "linha", `:=`(id_mandato = i.id_mandato, vinculo_cadeira = "lista_data_estrita")]
ocup[is.na(vinculo_cadeira), vinculo_cadeira := "casa_legislatura"]
reg("ocu_vinculo_chapa_senado", ocup[vinculo_cadeira == "chapa_senado", .N])
reg("ocu_vinculo_lista_vaga_unica", ocup[vinculo_cadeira == "lista_vaga_unica", .N])
reg("ocu_vinculo_lista_data_estrita", ocup[vinculo_cadeira == "lista_data_estrita", .N])
reg("ocu_sem_cadeira_identificada", ocup[vinculo_cadeira == "casa_legislatura", .N])
print(ocup[, .N, by = vinculo_cadeira][order(-N)])

## ---------------------------------------------------------------- 5. montagem

mand_ref <- mand[, .(id_mandato, sg_partido_tit = sg_partido, nm_ue_m = nm_ue,
                     esfera_m = esfera, cargo_m = cargo, unidade_posicao)]
setorder(ocup, ano_eleicao, unidade, cd_cargo, id_mandato, data_inicio, linha, na.last = TRUE)
ocup[, ordem_ocupacao := seq_len(.N) + 1L, by = .(ano_eleicao, unidade, cd_cargo, id_mandato)]
# sem cadeira identificada nao ha sequencia de ocupacao a numerar: o agrupamento juntava todos
# os NA da mesma casa e a ordem chegava a 90, que nao e posicao em cadeira nenhuma
ocup[is.na(id_mandato), ordem_ocupacao := NA_integer_]

oc_out <- ocup[, .(
  id_mandato, id_lista, id_pessoa, ano_eleicao, cd_cargo,
  # 21/09/2026: o vice que assumiu governo (cd_cargo 3, fonte wikipedia) caia em cargo NA e esfera municipal
  cargo = fcase(cd_cargo == 1L, "PRESIDENTE", cd_cargo == 2L, "VICE-PRESIDENTE",
                cd_cargo == 3L, "GOVERNADOR", cd_cargo == 4L, "VICE-GOVERNADOR",
                cd_cargo == 5L, "SENADOR", cd_cargo == 6L, "DEPUTADO FEDERAL",
                cd_cargo == 7L, "DEPUTADO ESTADUAL", cd_cargo == 8L, "DEPUTADO DISTRITAL",
                cd_cargo == 11L, "PREFEITO", cd_cargo == 12L, "VICE-PREFEITO", cd_cargo == 13L, "VEREADOR"),
  esfera = fcase(cd_cargo %in% c(1L,2L,5L,6L), "federal", cd_cargo %in% c(3L,4L,7L,8L), "estadual",
                 cd_cargo %in% c(11L,12L,13L), "municipal"),
  sg_uf, sg_ue = fifelse(cd_cargo %in% c(11L,13L), unidade, NA_character_),
  nm_ue = NA_character_, unidade_posicao = unidade,
  tipo_ocupante, ordem_ocupacao,
  sg_partido_ocupante = fifelse(!is.na(sg_partido_sup), sg_partido_sup, partido_fonte),
  sg_partido_titular = NA_character_,
  data_inicio, data_fim,
  origem_data_inicio = fifelse(!is.na(data_inicio), "fonte", NA_character_),
  forma_saida, fonte_forma_saida = fonte,
  vinculo_cadeira, fonte, url,
  ordem_suplencia, regra_pareamento_ocupacao
)]
oc_tit[, `:=`(ordem_suplencia = NA_integer_, regra_pareamento_ocupacao = NA_character_)]
oc <- rbindlist(list(oc_tit, oc_out), use.names = TRUE, fill = TRUE)
# partido do titular da cadeira, para medir a troca de legenda por dentro
oc[mand_ref, on = "id_mandato", sg_partido_titular := i.sg_partido_tit]
oc[tipo_ocupante == "titular", sg_partido_titular := sg_partido_ocupante]
# A sigla do ocupante so vale quando e sigla mesmo. O parse da Wikipedia trazia nome de partido
# por extenso e ate frase inteira sobre mudanca de nome de municipio, e a comparacao crua
# inflava a contagem de troca de legenda. Alem disso, so o suplente convocado troca o partido da
# cadeira por dentro: vice e interino ocupam por sucessao no Executivo, que e outro fenomeno.
siglas <- unique(c(mand$sg_partido, fread(file.path(root,"data","lista_suplencia.csv"),
                                          select = "sg_partido")$sg_partido))
siglas <- toupper(trimws(siglas[!is.na(siglas) & nchar(siglas) <= 20]))
oc[, sg_partido_ocupante := {
  v <- toupper(trimws(sg_partido_ocupante))
  fifelse(!is.na(v) & v %in% siglas, v, NA_character_)
}]
oc[, sg_partido_titular := toupper(trimws(sg_partido_titular))]
oc[, partido_difere_do_titular := fifelse(
  tipo_ocupante != "suplente" | is.na(sg_partido_ocupante) | is.na(sg_partido_titular), NA,
  sg_partido_ocupante != sg_partido_titular)]
reg("ocu_partido_ocupante_descartado_por_nao_ser_sigla",
    oc[tipo_ocupante != "titular" & is.na(sg_partido_ocupante), .N])
# datas que a fonte deu invertidas ficam marcadas, e nao corrigidas em silencio
oc[, datas_inconsistentes := !is.na(data_inicio) & !is.na(data_fim) & data_inicio > data_fim]
reg("ocu_datas_inconsistentes", oc[datas_inconsistentes == TRUE, .N])
oc[, id_ocupacao := paste0("OC", formatC(.I, width = 8, flag = "0"))]
setcolorder(oc, c("id_ocupacao","id_mandato","id_pessoa","tipo_ocupante","ordem_ocupacao"))

fwrite(oc, file.path(root,"data","ocupacoes.csv"), quote = TRUE, na = "NA")
write_parquet(oc, file.path(root,"data","ocupacoes.parquet"))
reg("ocu_linhas", nrow(oc))
reg("ocu_nao_titulares", oc[tipo_ocupante != "titular", .N])
reg("ocu_cadeiras_com_mais_de_um_ocupante",
    oc[!is.na(id_mandato), .N, by = id_mandato][N > 1, .N])
reg("ocu_pessoas_distintas", uniqueN(oc$id_pessoa, na.rm = TRUE))
reg("ocu_troca_de_partido_na_cadeira", oc[partido_difere_do_titular %in% TRUE, .N])
cat("\n42_ocupacoes: concluido |", nrow(oc), "ocupacoes,",
    oc[tipo_ocupante != "titular", .N], "nao titulares\n")
print(oc[, .N, by = tipo_ocupante][order(-N)])
