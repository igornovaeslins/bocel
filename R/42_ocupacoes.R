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
# Desde a v1.1 (28/09/2026) a cadeira vaga no inicio da legislatura, que a fonte nao liga ao
# suplente, e decidida por uma regra geral (secao 4b) que gera lista_vaga_inicial e
# lista_sucessao_vaga, e origem_data_inicio/origem_data_fim dizem de onde veio cada data.
#
# Entrada: data/mandatos.csv, data/lista_suplencia.csv, data/suplentes_identidade.csv,
#          data/exercicio_*.csv, data/wikipedia_*.csv, data/mandatos_lista.csv,
#          data/ocupantes_legislativo_federal.csv, data/camara_biografia_eventos.csv,
#          data/camara_biografia_posses.csv (estes tres so na regra da secao 4b)
# Saida:   data/ocupacoes.csv|parquet
# Execucao: Rscript --vanilla R/42_ocupacoes.R
set.seed(20260830)
suppressPackageStartupMessages({library(data.table); library(arrow); library(stringi)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
## 29/09/2026: o pareamento de nomes (norm, abaixo) depende do locale. Em LC_CTYPE=C (shell sem LANG, cron), toupper() nao
## converte letra acentuada, o Latin-ASCII a passa a minuscula e o gsub("[^A-Z ]") a apaga; o nome com acento deixa de casar
## com a lista e a identidade do suplente se perde (medido: rodada em C deu lista_vaga_unica 665 e chapa_senado 100, contra
## 773 e 157 na rodada de 24/09/2026 em UTF-8). O script fixa a ctype em UTF-8 e aborta se nao conseguir.
if (!isTRUE(l10n_info()[["UTF-8"]]))
  for (loc in c("en_US.UTF-8", "pt_BR.UTF-8", "C.UTF-8")) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", loc)))) break
stopifnot("42_ocupacoes.R exige locale UTF-8 (LC_ALL=en_US.UTF-8)" = isTRUE(l10n_info()[["UTF-8"]]))
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
  # 28/09/2026 (v1.1): origem_data_fim acompanha data_fim em todas as linhas. Aqui data_fim so existe quando
  # a camada de posse e saida apurou data_fim_efetiva, e a origem e sempre a fonte dessa camada.
  origem_data_fim = fifelse(!is.na(data_fim_efetiva), "fonte", NA_character_),
  forma_saida, fonte_forma_saida,
  vinculo_cadeira = "eleicao",
  fonte = fifelse(is.na(fonte_forma_saida), "tse", fonte_forma_saida), url = NA_character_
)]
reg("ocu_titulares", nrow(oc_tit))
## OCU-07 (24/09/2026, achado da auditoria de 24/09/2026, ver docs/EXCECOES_CONHECIDAS.csv):
## Walter Pinheiro (OC00000896, deputado federal, BA 1998) tem data_posse com dia e mes trocados
## na fonte do TSE (1999-01-02, anterior ao proprio inicio do mandato em 1999-02-01). Correcao
## curada e rastreavel, so para este mandato; origem_data_inicio marca a curadoria, nunca a fonte.
n_ocu07 <- oc_tit[id_mandato == "M1998_BA_6_1310_500000506013101", .N]
oc_tit[id_mandato == "M1998_BA_6_1310_500000506013101",
       `:=`(data_inicio = "1999-02-01", origem_data_inicio = "curadoria_ocu07")]
reg("ocu_data_inicio_corrigida_curadoria_ocu07", n_ocu07)

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
# caminhos, do mais firme ao mais fraco, e cada linha declara qual usou:
#   chapa_senado      a chapa do Senado ja nomeia a cadeira do titular
#   lista_vaga_unica  a lista tem uma unica cadeira que vagou, e nao ha o que confundir
#   lista_data_estrita entre as cadeiras que vagaram, a que vagou mais perto da entrada
#   lista_vaga_inicial, lista_sucessao_vaga (v1.1, secao 4b) a cadeira vaga no inicio da legislatura
#                     e a que se abre quando o ocupante dela sai em definitivo
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
## 28/09/2026 (v1.1): a tabela explicita liga_ocu03 (24/09/2026), que nomeava a cadeira de Helenildo
## Ribeiro (AL 1998), Selma Schons (PR 2002) e Jorge Braz (RJ 2018) uma a uma, saiu daqui. Esses tres
## casos, e os demais da mesma classe, passam a ser decididos pela regra geral da secao 4b, que le a
## lista de suplencia, a saida do titular e os sinais de efetivacao da Casa. A cadeira nomeada pela
## tabela curada com fonte oficial (id_mandato_explicito, R/58) continua valendo em primeiro lugar.
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
## ---------------------------------------------------------------- 4b. regra geral da cadeira vaga no inicio
## REGRA GERAL (28/09/2026, v1.1). Substitui a tabela liga_ocu03 de 24/09/2026.
## Problema. Na Camara a fonte nem sempre diz qual titular o suplente substituiu. Quando o eleito nao tomou
## posse, saiu antes de comecar ou tomou posse tarde, a cadeira ficava sem ocupante ligado, ou so o tinha
## porque tres casos foram nomeados a mao. A regra decide a cadeira a partir da lista, da saida do titular e
## dos sinais de efetivacao da Casa, e serve a todo caso da mesma classe.
## Dominio (so deputado federal). Cadeira cujo titular tem forma_saida nao_tomou_posse, ou nao tem data de
## inicio, ou a tem mais de 60 dias depois de mandato_inicio, ou a tem depois da propria data de fim.
## Lista. A suplencia pertence a lista. De 2002 a 2022 id_lista ja e a coligacao (ou a federacao, ou o
## partido isolado). Em 1998 id_lista e do partido, mas quem tem suplentes e a coligacao; a chave vira (1998,
## UF, nome da coligacao) e a ordem de suplencia e recalculada por votos nominais decrescentes dentro dela,
## porque a ordem gravada nas listas de 1998 e por partido.
## Vaga permanente. (a) a cadeira do dominio, aberta desde mandato_inicio; (b) a saida definitiva de titular
## (renuncia, cassacao, falecimento, aposentadoria, retotalizacao); (c) a saida definitiva de ocupante
## suplente (renuncia, cassacao, falecimento). A saida vem de mandatos, da API ou da biografia da Camara,
## nessa prioridade, uma por pessoa e legislatura.
## Sinal de efetivacao. Suplente que virou titular, em tres fontes da Casa ja no banco: exercicio_camara
## (entrada "Posse como Titular" ou "Efetiva"), ocupantes_legislativo_federal (suplente_efetivado) e a secao
## "Suplencias e Efetivacoes" da biografia, unica fonte da legislatura 51. A legislatura de um sinal sai da
## propria data, porque a biografia traz leg_inicio trocado em alguns casos.
## Pareamento. Uma vaga casa com um sinal quando o sinal cai na janela da vaga (de 31 dias antes a 31 dias
## depois de mandato_inicio na vaga inicial; de 3 dias antes a 31 dias depois da saida nas demais), o
## efetivado esta na lista da vaga e a aresta e unica dos dois lados. Firmado o par numa vaga inicial ou de
## sucessao, a saida do entrante abre a vaga seguinte da mesma cadeira. A ordem de suplencia pulada entra
## como alerta e nao bloqueia, porque renuncia a suplencia e recusa explicam a maior parte dos saltos.
## Precedencia, do vinculo mais firme ao mais fraco, e a regra nunca desfaz um vinculo mais firme:
##   0. fonte_da_casa (id_mandato_explicito, R/58): mantido.
##   1. chapa_senado e cadeira_unica_da_unidade: fora da regra.
##   2. lista_vaga_unica e lista_data_estrita: mantidos; a regra so registra se concorda ou diverge.
##   3. lista_vaga_inicial: a vaga e a cadeira do dominio; a ocupacao sem cadeira recebe.
##   4. lista_sucessao_vaga: a vaga e a saida do entrante anterior; a ocupacao sem cadeira recebe.
##   5. mesma_pessoa: as demais linhas da pessoa na eleicao, depois do sinal, herdam cadeira e rotulo do par.
##   Sem decisao: o que nenhum caminho resolve segue para casa_legislatura, como antes, com o motivo registrado.
## Datas. Somente onde a fonte nao deu; linha ja datada pela API conserva a data. Inicio: posse da
## biografia a 31 dias do sinal (origem biografia_camara); senao a primeira data da clausula de efetivacao da
## biografia (biografia_camara); senao a data do proprio sinal (sinal_efetivacao_api). Fim, so no ultimo
## episodio da pessoa: saida definitiva valida (fonte ou biografia_camara); senao o fim da legislatura
## (convencao_fim_legislatura), salvo na eleicao 2022, que segue sem data.
## Efeitos que a documentacao precisa conhecer. (i) Helenildo, Selma e Jorge Braz deixam de ser
## fonte_da_casa e passam a lista_*; a cadeira e a mesma da tabela antiga. (ii) O deslocamento de linhas
## muda id_ocupacao nas cadeiras tocadas, ordem_ocupacao de quem vem depois do entrante, sg_partido_titular e
## partido_difere_do_titular de quem passa a ter cadeira. (iii) Parte das linhas ligadas fica com id_lista
## vazio, porque quem entra com id_pessoa conhecido nao passa pelo casamento com a lista da secao 3.
## Fontes (biografia da Camara, ja armazenada em data/camara_biografia_eventos.csv):
##   Helenildo Ribeiro, AL 1998: https://www.camara.leg.br/deputados/73582/biografia
##     "Assumiu e foi efetivado no mandato de Deputado Federal para a legislatura 1999-2003, em 8 de abril de 1999."
##   Talvane Albuquerque, AL 1998: https://www.camara.leg.br/deputados/74523/biografia
##     "Empossado e efetivado no mandato de Deputado Federal, na legislatura 1999-2003, exerceu o mandato de
##     3 de fevereiro a 7 de abril de 1999." e "Perdeu o mandato de Deputado Federal na legislatura 1999-2003".
##   Selma Schons, PR 2002: https://www.camara.leg.br/deputados/74403/biografia
##     "Assumiu e foi efetivada no mandato de Deputada Federal na legislatura 2003-2007".
##   Jorge Braz, RJ 2018: https://www.camara.leg.br/deputados/205550/biografia
##     "Foi efetivado(a), em 1 de Fevereiro de 2019, assumindo o mandato de Deputado(a) Federal, na Legislatura 2019-2023."
##   Sinal da API (data/exercicio_camara.csv, campo descricao_entrada): "Entrada - Posse de Suplente - Posse como Titular".
## O Diario da Camara dos Deputados nao esta armazenado no banco e nao e citado aqui como fonte.
ocup[, `:=`(origem_data_inicio = fifelse(!is.na(data_inicio), "fonte", NA_character_),
            origem_data_fim = fifelse(!is.na(data_fim), "fonte", NA_character_))]
regra <- local({
  JANELA_TITULAR <- 60L   # titular sem ocupacao iniciada ate 60 dias apos o inicio do mandato
  JANELA_EFET    <- 31L   # efetivacao no inicio: de 31 dias antes a 31 dias depois de mandato_inicio, e ate 31 dias depois de uma saida
  TOL_DIAS       <- 3L
  # sob Rscript --vanilla o locale e C: todo texto da biografia passa por stringi, que le UTF-8
  ascii <- function(x) {
    y <- stri_trans_general(as.character(x), "Latin-ASCII")
    y <- stri_replace_all_regex(y, "[\\u00ba\\u00b0]", "o")
    stri_replace_all_regex(y, "\\u00aa", "a")
  }
  norm_nm <- function(x) stri_trim_both(stri_replace_all_regex(ascii(x), "\\s+", " "))
  # legislatura que contem a data (de 1o de fevereiro a 31 de janeiro, quatro anos depois)
  eleicao_de_data <- function(x) 1998L + 4L * ((as.integer(format(x - 31L, "%Y")) - 1999L) %/% 4L)
  E_de_leg <- function(leg) 1998L + 4L * (as.integer(leg) - 51L)
  nz <- function(x) !is.na(x) & nzchar(x)

  ## -------- fontes da Casa
  exc  <- fread(file.path(root,"data","exercicio_camara.csv"),
                colClasses = c(id_pessoa = "character", id_deputado_camara = "character"))
  for (v in c("data_inicio_exercicio","data_fim_exercicio")) exc[, (v) := as.IDate(get(v))]
  olf  <- fread(file.path(root,"data","ocupantes_legislativo_federal.csv"), colClasses = "character", na.strings = "NA")
  bpos <- fread(file.path(root,"data","camara_biografia_posses.csv"), colClasses = c(id_deputado_camara = "character"))
  bpos[, data_posse := as.IDate(data_posse)]
  bev  <- fread(file.path(root,"data","camara_biografia_eventos.csv"), colClasses = c(id_deputado_camara = "character"))

  ## -------- identidade id_deputado_camara <-> id_pessoa
  dep_de_url <- function(u) fifelse(!is.na(u) & grepl("deputados/[0-9]+", u), sub(".*deputados/([0-9]+).*", "\\1", u), NA_character_)
  olf[, id_dep := dep_de_url(url_fonte)]
  mapdep <- unique(rbind(
    exc[!is.na(id_pessoa), .(id_dep = id_deputado_camara, id_pessoa)],
    ocup[fonte == "camara_api" & !is.na(id_pessoa), .(id_dep = dep_de_url(url), id_pessoa)],
    olf[casa == "camara" & !is.na(id_pessoa) & !is.na(id_dep), .(id_dep, id_pessoa)]))[!is.na(id_dep)]
  reg("ocu_regra_mapdep_pares_id_dep_id_pessoa", nrow(mapdep))
  reg("ocu_regra_mapdep_id_dep_com_mais_de_uma_pessoa", mapdep[, .N, by = id_dep][N > 1L, .N])

  ## -------- chave de lista (coligacao) e ordem de suplencia
  chave_de <- function(ano, uf, tp, nm, id_lista)
    fifelse(ano == 1998L & !is.na(tp) & stri_detect_regex(tp, "^COLIGA"), paste0("1998_", uf, "_C_", norm_nm(nm)), id_lista)
  m6 <- mand[cd_cargo == 6L, .(id_mandato, ano_eleicao, sg_uf, id_pessoa, sg_partido, tp_agremiacao, nm_coligacao,
                               mandato_inicio = as.IDate(mandato_inicio), mandato_fim = as.IDate(mandato_fim),
                               forma_saida, data_fim_efetiva = as.IDate(data_fim_efetiva))]
  m6 <- merge(m6, ml[cd_cargo == 6L, .(id_mandato, id_lista)], by = "id_mandato", all.x = TRUE)
  stopifnot(!anyNA(m6$id_lista), !anyDuplicated(m6$id_mandato))
  m6[, chave := chave_de(ano_eleicao, sg_uf, tp_agremiacao, nm_coligacao, id_lista)]
  s6 <- lista[cd_cargo == 6L]
  s6[, chave := chave_de(ano_eleicao, sg_uf, tp_agremiacao, nm_coligacao, id_lista)]
  reg("ocu_regra_chaves_de_lista_cadeiras_camara", uniqueN(m6$chave))
  reg("ocu_regra_chaves_de_lista_suplentes_camara", uniqueN(s6$chave))
  s6[, ord_chave := ordem_suplencia]
  s6[ano_eleicao == 1998L & stri_detect_regex(chave, "^1998_.*_C_"),
     ord_chave := frank(-votos_nominais, ties.method = "min"), by = chave]
  s6[, empate_ord := duplicated(paste(chave, ord_chave)) | duplicated(paste(chave, ord_chave), fromLast = TRUE)]
  reg("ocu_regra_suplentes_1998_coligacao_ordem_recalculada_diverge_da_gravada",
      s6[ano_eleicao == 1998L & stri_detect_regex(chave, "_C_") & ord_chave != ordem_suplencia, .N])
  reg("ocu_regra_suplentes_com_empate_na_ordem_da_chave", s6[empate_ord == TRUE, .N])

  ## -------- dominio: cadeiras cujo titular nao exerceu no inicio
  tit <- oc_tit[cd_cargo == 6L, .(id_mandato, tit_ini = as.IDate(data_inicio), tit_fim = as.IDate(data_fim))]
  stopifnot(!anyDuplicated(tit$id_mandato))
  cad <- merge(m6[, .(id_mandato, ano_eleicao, sg_uf, id_pessoa_tit = id_pessoa, sg_partido, chave, id_lista,
                      mandato_inicio, mandato_fim, forma_saida, data_fim_efetiva)], tit, by = "id_mandato", all.x = TRUE)
  cad[, `:=`(crit_a = forma_saida %in% "nao_tomou_posse",
             crit_b = is.na(tit_ini) | tit_ini > mandato_inicio + JANELA_TITULAR |
               (!is.na(tit_fim) & !is.na(tit_ini) & tit_fim < tit_ini))]
  cad[, dominio := crit_a | crit_b]
  cad[, tipo_vaga_inicial := fcase(crit_a, "nao_tomou_posse",
                                   !is.na(tit_ini) & !is.na(tit_fim) & tit_fim < tit_ini, "saiu_antes_de_comecar",
                                   is.na(tit_ini), "sem_data_de_inicio",
                                   tit_ini > mandato_inicio + JANELA_TITULAR, "titular_tardio",
                                   default = NA_character_)]
  reg("ocu_regra_dominio_cadeiras", cad[dominio == TRUE, .N])
  for (tv in c("nao_tomou_posse","saiu_antes_de_comecar","sem_data_de_inicio","titular_tardio"))
    reg(paste0("ocu_regra_dominio_", tv), cad[tipo_vaga_inicial %in% tv, .N])

  ## -------- sinais de efetivacao (suplente que virou titular)
  MESES <- c(janeiro = 1L, fevereiro = 2L, marco = 3L, abril = 4L, maio = 5L, junho = 6L, julho = 7L,
             agosto = 8L, setembro = 9L, outubro = 10L, novembro = 11L, dezembro = 12L)
  MES_RE <- "janeiro|fevereiro|marco|abril|maio|junho|julho|agosto|setembro|outubro|novembro|dezembro"
  TOK_RE <- paste0("(?i)(?<![\\d/])(\\d{1,2})o?\\s+(?:de\\s*(", MES_RE, ")(?:\\s*de\\s*(\\d{4}))?|a\\s+(?=\\d{1,2}o?\\s+de\\s*(?:", MES_RE, ")))")
  vazio_datas <- function() data.table(ini = integer(), fim = integer(), data = as.IDate(character()), inicio_de_faixa = logical())
  # datas por extenso ("de 3 de fevereiro a 7 de abril de 1999"); mes e ano ausentes herdam do token seguinte (ate 45 caracteres)
  datas_pt <- function(tx) {
    if (is.na(tx) || !nzchar(tx)) return(vazio_datas())
    loc <- stri_locate_all_regex(tx, TOK_RE)[[1]]
    if (is.na(loc[1, 1])) return(vazio_datas())
    mm <- stri_match_all_regex(tx, TOK_RE)[[1]]
    dd <- data.table(ini = loc[, 1], fim = loc[, 2], dia = as.integer(mm[, 2]),
                     mes = unname(MESES[stri_trans_tolower(mm[, 3])]), ano = as.integer(mm[, 4]))
    dd[, dia_so := is.na(mes)]
    n <- nrow(dd)
    dd[, inicio_de_faixa := TRUE]
    if (n > 1L) {
      for (i in (n - 1L):1L) if ((dd$ini[i + 1L] - dd$fim[i]) <= 45L) {
        if (is.na(dd$mes[i])) dd$mes[i] <- dd$mes[i + 1L]
        if (is.na(dd$ano[i])) dd$ano[i] <- dd$ano[i + 1L]
      }
      for (i in 2:n) {
        gap <- stri_sub(tx, dd$fim[i - 1L] + 1L, dd$ini[i] - 1L)
        if (dd$dia_so[i - 1L] || grepl("^\\s*(a|ate)\\s*$", gap, ignore.case = TRUE)) dd$inicio_de_faixa[i] <- FALSE
      }
    }
    dd[, data := as.IDate(sprintf("%04d-%02d-%02d", ano, mes, dia), format = "%Y-%m-%d")]
    dd[, .(ini, fim, data, inicio_de_faixa)]
  }
  # eventos da biografia; trechos truncados em abreviatura ("Dep.") sao reunidos ao seguinte do mesmo deputado
  ABREV_FIM <- "\\b(Dep|Dr|Dra|Sr|Sra|Prof|Profa|Cel|Gen|Maj|Cap|Pe|Ten|Ir|Des|Eng)\\.\\s*$"
  bev[, seguinte_mesmo_dep := shift(id_deputado_camara, type = "lead") == id_deputado_camara]
  bev[, junta := stri_detect_regex(fifelse(is.na(trecho), "", trecho), ABREV_FIM) & seguinte_mesmo_dep %in% TRUE]
  bev[, grupo := cumsum(shift(junta, fill = FALSE) == FALSE)]
  bev2 <- bev[, .(id_deputado_camara = id_deputado_camara[1], tipo = tipo[1],
                  datas = paste(unique(unlist(strsplit(datas[!is.na(datas) & nzchar(datas)], ";"))), collapse = ";"),
                  trecho = paste(trecho, collapse = " "), url = url[1]), by = grupo]
  bev2[, tx := stri_replace_all_regex(ascii(trecho), "\\s+", " ")]
  reg("ocu_regra_bio_eventos_linhas", nrow(bev)); reg("ocu_regra_bio_eventos_apos_reunir_truncados", nrow(bev2))
  # efetivacoes da biografia: uma clausula (separada por ";") por sinal. d_assume = primeira data da clausula,
  # d_efet = primeira data depois da palavra "efetivad" (igual a d_assume quando a data vem antes ou nao ha outra).
  sinais_bev <- rbindlist(lapply(which(bev2$tipo == "efetivacao"), function(i) {
    cls <- strsplit(bev2$tx[i], ";", fixed = TRUE)[[1]]
    rbindlist(lapply(cls, function(cl) {
      if (!grepl("efetivad", cl, ignore.case = TRUE)) return(NULL)
      dd <- datas_pt(cl)[!is.na(data)]
      if (!nrow(dd)) return(NULL)
      kw <- stri_locate_first_regex(cl, "(?i)efetivad")[1, 1]
      di <- dd[inicio_de_faixa == TRUE]
      d_ass <- dd$data[1]
      d_efe <- if (any(di$ini > kw)) di[ini > kw]$data[1] else d_ass
      d_fim <- if (nrow(dd) >= 2L && dd$inicio_de_faixa[2] == FALSE) dd$data[2] else as.IDate(NA)
      data.table(id_dep = bev2$id_deputado_camara[i], d_assume = d_ass, d_efet = d_efe, d_fim_faixa = d_fim,
                 trecho = substr(cl, 1, 240), url = bev2$url[i])
    }))
  }))
  sinais_bev <- unique(sinais_bev)
  reg("ocu_regra_bio_sinais_efetivacao_clausulas", nrow(sinais_bev))
  reg("ocu_regra_bio_sinais_sem_id_pessoa_no_mapdep", sinais_bev[!id_dep %in% mapdep$id_dep, .N])

  ef_api <- exc[condicao == "titular" & efetivado %in% TRUE & !is.na(data_inicio_exercicio) &
                  stri_detect_regex(fifelse(is.na(descricao_entrada), "", descricao_entrada), "Posse como Titular|Efetiva"),
                .(id_dep = id_deputado_camara, id_pessoa, E = E_de_leg(legislatura), data = data_inicio_exercicio, fonte_sinal = "api_exercicio")]
  ef_olf <- olf[casa == "camara" & tipo_ocupante == "suplente_efetivado" & nz(data_inicio),
                .(id_dep, id_pessoa, E = E_de_leg(legislatura), data = as.IDate(data_inicio), fonte_sinal = "api_ocupantes")]
  ef_bio <- rbind(sinais_bev[, .(id_dep, data = d_assume, fonte_sinal = "biografia")],
                  sinais_bev[, .(id_dep, data = d_efet, fonte_sinal = "biografia")])[!is.na(data)]
  ef_bio[, E := eleicao_de_data(data)]
  ef <- rbindlist(list(ef_api, ef_olf, ef_bio), fill = TRUE)
  ef <- merge(ef, unique(mapdep[, .(id_dep, id_pessoa_map = id_pessoa)]), by = "id_dep", all.x = TRUE, allow.cartesian = TRUE)
  ef[is.na(id_pessoa), id_pessoa := id_pessoa_map][, id_pessoa_map := NULL]
  ef[, pkey := fifelse(!is.na(id_pessoa), id_pessoa, paste0("dep", id_dep))]
  ef <- unique(ef[, .(pkey, id_pessoa, id_dep, E, data, fonte_sinal)])
  reg("ocu_regra_sinais_efetivacao_linhas", nrow(ef))
  reg("ocu_regra_sinais_efetivacao_pessoas_distintas", uniqueN(ef$pkey))
  reg("ocu_regra_sinais_efetivacao_sem_id_pessoa", ef[is.na(id_pessoa), uniqueN(pkey)])
  # entrantes: sinal + lista de suplentes da mesma eleicao (por id_pessoa)
  s6u <- unique(s6[, .(id_pessoa, E = ano_eleicao, sg_uf, chave, id_lista, ord_chave, ordem_suplencia, votos_nominais, empate_ord,
                       nome_lista = nome, pessoa_tambem_eleita)])
  efl <- merge(ef[!is.na(id_pessoa)], s6u, by = c("id_pessoa", "E"), allow.cartesian = TRUE)
  reg("ocu_regra_sinais_efetivacao_com_lista_de_suplentes", efl[, uniqueN(paste(pkey, E))])
  reg("ocu_regra_sinais_efetivacao_sem_lista_de_suplentes", nrow(unique(ef[, .(pkey, E)])) - efl[, uniqueN(paste(pkey, E))])

  ## -------- saidas definitivas (a cadeira fica vaga em definitivo)
  pess_chave <- unique(rbind(m6[, .(pkey = id_pessoa, E = ano_eleicao, chave, papel = "titular_eleito")],
                             s6u[, .(pkey = id_pessoa, E, chave, papel = "suplente")]))
  PERM <- c("renuncia", "cassacao", "falecimento", "aposentadoria", "retotalizacao")
  sai_tit <- cad[dominio == FALSE & forma_saida %in% PERM & !is.na(data_fim_efetiva) & !is.na(id_pessoa_tit),
                 .(pkey = id_pessoa_tit, E = ano_eleicao, data = data_fim_efetiva, forma = forma_saida, fonte_saida = "mandatos", id_mandato_saida = id_mandato)]
  sai_ocu <- nc_ocup <- ocup[cd_cargo %in% 5:6 & tipo_ocupante %in% c("suplente", "suplente_efetivado") &
                               forma_saida %in% c("renuncia", "cassacao", "falecimento") & !is.na(data_fim) & !is.na(id_pessoa),
                             .(pkey = id_pessoa, E = ano_eleicao, data = as.IDate(data_fim), forma = forma_saida,
                               fonte_saida = "ocupacoes_api", id_mandato_saida = NA_character_)]
  bx <- bev2[tipo %in% c("perda_mandato", "renuncia", "falecimento", "aposentadoria")]
  bx[, data := as.IDate(vapply(seq_len(.N), function(i) {
    dd <- strsplit(datas[i], ";", fixed = TRUE)[[1]]; dd <- dd[nzchar(dd)]
    if (length(dd)) return(dd[1])
    dt <- datas_pt(tx[i])[!is.na(data)]
    if (nrow(dt)) as.character(dt$data[1]) else NA_character_
  }, character(1)))]
  sai_bio <- merge(bx[!is.na(data), .(id_dep = id_deputado_camara, data, forma = tipo)], unique(mapdep[, .(id_dep, pkey = id_pessoa)]), by = "id_dep")
  sai_bio[, `:=`(E = eleicao_de_data(data), fonte_saida = "biografia_camara", id_mandato_saida = NA_character_)]
  sai_bio[forma == "perda_mandato", forma := "cassacao"]
  sai <- rbindlist(list(sai_tit, sai_ocu, sai_bio[, .(pkey, E, data, forma, fonte_saida, id_mandato_saida)]), use.names = TRUE)
  sai <- merge(sai, pess_chave, by = c("pkey", "E"))            # so conta quem esta numa lista da eleicao
  sai[, prio := match(fonte_saida, c("mandatos", "ocupacoes_api", "biografia_camara"))]
  setorder(sai, pkey, E, prio, data)
  sai <- sai[, .SD[1], by = .(pkey, E)]                          # uma saida definitiva por pessoa e legislatura
  reg("ocu_regra_saidas_definitivas_de_pessoas_com_lista", nrow(sai))
  reg("ocu_regra_saidas_definitivas_de_titulares", sai[papel == "titular_eleito", .N])
  reg("ocu_regra_saidas_definitivas_de_ocupantes_suplentes", sai[papel == "suplente", .N])
  sai <- merge(sai, unique(m6[, .(E = ano_eleicao, mi = mandato_inicio, mf = mandato_fim)]), by = "E")
  sai <- sai[data >= mi & data <= mf + 31L][, c("mi", "mf") := NULL]
  reg("ocu_regra_saidas_definitivas_dentro_da_legislatura", nrow(sai))
  stopifnot(uniqueN(m6[, .(ano_eleicao, mandato_inicio, mandato_fim)]) == uniqueN(m6$ano_eleicao))

  ## -------- ordem de suplencia como alerta (nao bloqueia)
  # Para cada par vaga-entrante conta-se quantos suplentes de ordem menor (mais votados) da mesma coligacao ainda nao tinham
  # entrado ate a data do sinal. Entrar conta: linha da API com data, posse na biografia, sinal de efetivacao, em qualquer
  # vaga da lista, ate o mesmo dia. Ficam fora da contagem os suplentes que sao titulares eleitos na mesma eleicao.
  bpos_p <- merge(bpos[, .(id_dep = id_deputado_camara, data = data_posse)], unique(mapdep[, .(id_dep, id_pessoa)]), by = "id_dep")
  bpos_p[, E := eleicao_de_data(data)]
  entradas <- rbind(ef[!is.na(id_pessoa), .(id_pessoa, E, data)], bpos_p[, .(id_pessoa, E, data)],
                    ocup[cd_cargo %in% 5:6 & !is.na(id_pessoa) & !is.na(data_inicio), .(id_pessoa, E = ano_eleicao, data = as.IDate(data_inicio))])
  titulares_eleitos <- m6[, paste(id_pessoa, ano_eleicao)]
  ordem_pulada_de <- function(pk, Ee, ch, ds) {
    o <- s6u[chave == ch & id_pessoa == pk & E == Ee, ord_chave]
    if (!length(o) || is.na(o[1])) return(NA_integer_)
    baixos <- s6u[chave == ch & E == Ee & ord_chave < o[1] & !paste(id_pessoa, E) %in% titulares_eleitos, unique(id_pessoa)]
    sum(!baixos %in% entradas[E == Ee & data <= ds, id_pessoa])
  }

  ## -------- pareamento vaga permanente x suplente efetivado
  # Vaga permanente = (a) cadeira do dominio, aberta desde mandato_inicio; (b) saida definitiva de titular; (c) saida definitiva
  # de ocupante. Uma vaga so casa com quem foi efetivado na sua janela e esta na lista da mesma coligacao. A ligacao so ocorre
  # quando a aresta e unica dos dois lados, depois de descontar os pares ja firmados na rodada anterior. Ligada uma vaga ancora
  # (a) ou de sucessao, a saida do ocupante abre a vaga seguinte, e o processo se repete ate estabilizar.
  seat_de_tit <- unique(m6[, .(pkey = id_pessoa, E = ano_eleicao, seat_tit = id_mandato)])
  dom_key <- cad[dominio == TRUE, paste(id_pessoa_tit, ano_eleicao)]
  vag <- rbindlist(list(
    cad[dominio == TRUE, .(chave, E = ano_eleicao, vtipo = "inicial", seat = id_mandato, data = mandato_inicio, pkey_saida = NA_character_, ancora = TRUE)],
    merge(sai[!paste(pkey, E) %in% dom_key], seat_de_tit, by = c("pkey", "E"), all.x = TRUE)[
      , .(chave, E, vtipo = fifelse(papel == "titular_eleito", "saida_titular", "saida_ocupante"),
          seat = fifelse(papel == "titular_eleito", seat_tit, NA_character_), data, pkey_saida = pkey, ancora = FALSE)]),
    use.names = TRUE)
  vag <- vag[chave %in% cad[dominio == TRUE, chave]]
  reg("ocu_regra_vagas_permanentes_nas_listas_do_dominio", nrow(vag))
  reg("ocu_regra_vagas_cadeiras_do_dominio", vag[vtipo == "inicial", .N])
  reg("ocu_regra_vagas_saidas_de_titular", vag[vtipo == "saida_titular", .N])
  reg("ocu_regra_vagas_saidas_de_ocupante_suplente", vag[vtipo == "saida_ocupante", .N])

  pareia_chave <- function(ch) {
    V <- copy(vag[chave == ch]); V[, vid := .I]
    V[, `:=`(j_ini = data - fifelse(vtipo == "inicial", JANELA_EFET, TOL_DIAS), j_fim = data + JANELA_EFET, ativa = TRUE, caminho = NA_character_)]
    V[ancora == TRUE & vtipo == "inicial", caminho := "lista_vaga_inicial"]
    Ent <- efl[chave == ch]
    if (!nrow(Ent)) return(list(V = V, pares = NULL, arestas_restantes = NULL))
    Ent <- Ent[!paste(pkey, E) %in% dom_key]                     # o eleito que nao tomou posse nao e entrante
    ativos <- unique(Ent$pkey)
    pares <- list()
    arestas <- function() {
      if (!any(V$ativa) || !length(ativos)) return(data.table(vid = integer(), pkey = character(), data_sinal = as.IDate(character())))
      x <- CJ(vid = V[ativa == TRUE, vid], k = seq_len(nrow(Ent)))
      x[, `:=`(pkey = Ent$pkey[k], dsin = Ent$data[k], Ee = Ent$E[k])]
      x <- x[pkey %in% ativos]
      x[, `:=`(ji = V$j_ini[vid], jf = V$j_fim[vid], Ev = V$E[vid], ps = V$pkey_saida[vid], dv = V$data[vid])]
      x <- x[Ev == Ee & dsin >= ji & dsin <= jf & (is.na(ps) | ps != pkey)]
      x[, dist := abs(as.integer(dsin - dv))]
      setorder(x, vid, pkey, dist)
      x[, .SD[1], by = .(vid, pkey)][, .(vid, pkey, data_sinal = dsin)]
    }
    repeat {
      a <- arestas()
      if (!nrow(a)) break
      a[, `:=`(gv = .N), by = vid][, ge := .N, by = pkey]
      u <- a[gv == 1L & ge == 1L]
      if (!nrow(u)) break
      for (r in seq_len(nrow(u))) {
        v <- u$vid[r]; e <- u$pkey[r]
        op <- ordem_pulada_de(e, V$E[v], ch, u$data_sinal[r])
        pares[[length(pares) + 1L]] <- data.table(chave = ch, E = V$E[v], vid = v, pkey = e, data_sinal = u$data_sinal[r], vtipo = V$vtipo[v],
                                                  ancora = V$ancora[v], caminho = V$caminho[v], seat = V$seat[v], data_vaga = V$data[v],
                                                  ordem_pulada = op, alerta_ordem = isTRUE(op > 0L))
        V[v, ativa := FALSE]; ativos <- setdiff(ativos, e)
        if (isTRUE(V$ancora[v])) {                                # a saida deste entrante e a proxima vaga da mesma cadeira
          V[pkey_saida == e & !is.na(pkey_saida), `:=`(ancora = TRUE, seat = V$seat[v], caminho = "lista_sucessao_vaga")]
        }
      }
    }
    list(V = V, pares = if (length(pares)) rbindlist(pares) else NULL, arestas_restantes = arestas())
  }
  saida <- lapply(unique(vag$chave), pareia_chave); names(saida) <- unique(vag$chave)
  pares <- rbindlist(lapply(saida, `[[`, "pares"), fill = TRUE)
  Vfim  <- rbindlist(lapply(names(saida), function(k) { v <- saida[[k]]$V; v[, chave := k]; v }), fill = TRUE)
  Arest <- rbindlist(lapply(names(saida), function(k) { a <- saida[[k]]$arestas_restantes; if (!is.null(a) && nrow(a)) { a[, chave := k]; a } }), fill = TRUE)
  reg("ocu_regra_pares_vaga_entrante_firmados", nrow(pares))
  reg("ocu_regra_pares_com_vaga_ancora", pares[ancora == TRUE, .N])
  reg("ocu_regra_pares_com_alerta_de_ordem_de_suplencia", pares[ancora == TRUE & alerta_ordem == TRUE, .N])
  reg("ocu_regra_cadeiras_ligadas_lista_vaga_inicial", pares[ancora == TRUE & caminho == "lista_vaga_inicial", uniqueN(seat)])
  reg("ocu_regra_pares_lista_vaga_inicial", pares[ancora == TRUE & caminho == "lista_vaga_inicial", .N])
  reg("ocu_regra_cadeiras_ligadas_lista_sucessao_vaga", pares[ancora == TRUE & caminho == "lista_sucessao_vaga", uniqueN(seat)])
  reg("ocu_regra_pares_lista_sucessao_vaga", pares[ancora == TRUE & caminho == "lista_sucessao_vaga", .N])
  reg("ocu_regra_pares_com_vaga_que_nao_e_ancora_descontados", pares[ancora == FALSE, .N])
  pa <- pares[ancora == TRUE, .(chave, E, id_pessoa = pkey, seat_regra = seat, caminho_regra = caminho, data_sinal, data_vaga, ordem_pulada)]
  cat("\n== 4b. pares ancorados (vaga x entrante) ==\n")
  print(merge(pa, unique(s6u[, .(id_pessoa, E, nome_lista)]), by = c("id_pessoa", "E"), all.x = TRUE)[
    , .(chave, seat_regra, caminho_regra, id_pessoa, nome_lista, data_vaga, data_sinal, ordem_pulada)])

  ## -------- motivo de cada vaga ancora sem par (por que a regra nao decide; nunca se chuta)
  sem_par <- Vfim[ancora == TRUE & ativa == TRUE]
  motivo_vaga <- function(i) {
    v <- sem_par[i]
    a <- if (nrow(Arest)) Arest[chave == v$chave & vid == v$vid] else Arest
    if (nrow(a) > 0L) {
      ge <- vapply(a$pkey, function(k) nrow(Arest[chave == v$chave & pkey == k]), integer(1))
      return(if (nrow(a) > 1L) "mais_de_um_entrante_efetivado_na_janela" else "entrante_disputado_por_mais_de_uma_vaga")
    }
    ent_fora <- ef[E == v$E & data >= v$j_ini & data <= v$j_fim & !pkey %in% efl[chave == v$chave, pkey]]
    ent_fora <- merge(ent_fora, unique(rbind(exc[, .(id_dep = id_deputado_camara, uf = sg_uf)], olf[casa == "camara", .(id_dep, uf = sg_uf)])), by = "id_dep")
    ent_fora <- ent_fora[uf == sub("^.*?_([A-Z]{2})_.*$", "\\1", v$seat)]
    if (nrow(ent_fora) > 0L) return("entrante_efetivado_na_janela_fora_da_lista")
    "sem_sinal_de_efetivacao_na_janela"
  }
  if (nrow(sem_par)) sem_par[, motivo := vapply(seq_len(.N), motivo_vaga, character(1))] else sem_par[, motivo := character()]
  sem_par[, tratada_caminho0 := seat %in% ocup[vinculo_cadeira == "fonte_da_casa" & cd_cargo == 6L, id_mandato]]
  reg("ocu_regra_vagas_ancora_sem_par", nrow(sem_par))
  reg("ocu_regra_vagas_ancora_sem_par_ja_tratadas_pela_fonte_da_casa", sem_par[tratada_caminho0 == TRUE, .N])
  reg("ocu_regra_vagas_ancora_sem_par_sem_ocupante_de_fonte_da_casa", sem_par[tratada_caminho0 == FALSE, .N])
  for (m in c("sem_sinal_de_efetivacao_na_janela", "entrante_efetivado_na_janela_fora_da_lista",
              "mais_de_um_entrante_efetivado_na_janela", "entrante_disputado_por_mais_de_uma_vaga"))
    reg(paste0("ocu_regra_vagas_sem_par_motivo_", m), sem_par[motivo == m, .N])
  cat("\n== 4b. vagas ancora sem par ==\n"); print(sem_par[, .(chave, seat, vtipo, data, motivo, tratada_caminho0)])

  ## -------- das vagas para as ocupacoes: todas as linhas da pessoa na legislatura, a partir do sinal
  nc <- ocup[cd_cargo %in% 5:6, .(linha, id_pessoa, ano_eleicao, cd_cargo, sg_uf, tipo_ocupante, id_mandato, vinculo_cadeira, id_lista,
                                   data_inicio = as.IDate(data_inicio), data_fim = as.IDate(data_fim),
                                   origem_data_inicio, origem_data_fim)]
  s6k <- s6u[!is.na(id_pessoa)][order(id_pessoa, E, sg_uf)][, .SD[1], by = .(id_pessoa, E, sg_uf)]
  nc <- merge(nc, s6k[, .(id_pessoa, ano_eleicao = E, sg_uf, chave_lista = chave, id_lista_via_pessoa = id_lista,
                          ordem_suplencia_chave = ord_chave)],
              by = c("id_pessoa", "ano_eleicao", "sg_uf"), all.x = TRUE, sort = FALSE)
  setorder(nc, linha)
  reg("ocu_regra_ocupacoes_camara_nao_titulares", nc[cd_cargo == 6L, .N])
  reg("ocu_regra_camara_sem_id_lista_na_base", nc[cd_cargo == 6L & is.na(id_lista), .N])
  reg("ocu_regra_camara_sem_id_lista_na_base_e_com_lista_via_pessoa", nc[cd_cargo == 6L & is.na(id_lista) & !is.na(id_lista_via_pessoa), .N])
  pa <- merge(pa, s6k[, .(id_pessoa, E, ordem_suplencia_chave = ord_chave, empate_ord)], by = c("id_pessoa", "E"))
  reg("ocu_regra_pares_ancorados_com_ordem_pulada_maior_que_zero", pa[ordem_pulada > 0L, .N])
  reg("ocu_regra_pares_ancorados_com_empate_na_ordem", pa[empate_ord == TRUE, .N])
  lig <- merge(nc[cd_cargo == 6L & !is.na(id_pessoa)],
               pa[, .(id_pessoa, ano_eleicao = E, seat_regra, caminho_regra, data_sinal, data_vaga, ordem_pulada, empate_ord)],
               by = c("id_pessoa", "ano_eleicao"))
  lig <- lig[is.na(data_inicio) | data_inicio >= data_sinal - TOL_DIAS]       # episodios de antes da efetivacao ficam de fora
  setorder(lig, id_pessoa, ano_eleicao, data_inicio, linha)
  lig[, episodio_regra := fifelse(seq_len(.N) == 1L, "ancora", "mesma_pessoa"), by = .(id_pessoa, ano_eleicao)]
  reg("ocu_regra_ocupacoes_ligadas_pela_regra_pura", nrow(lig))
  reg("ocu_regra_ocupacoes_ligadas_episodio_ancora", lig[episodio_regra == "ancora", .N])
  reg("ocu_regra_ocupacoes_ligadas_episodio_mesma_pessoa", lig[episodio_regra == "mesma_pessoa", .N])

  ## -------- datas propostas (so valem onde a fonte nao deu)
  lig <- merge(lig, unique(mapdep[, .(id_pessoa, id_dep)])[, .SD[1], by = id_pessoa], by = "id_pessoa", all.x = TRUE, sort = FALSE)
  fonte_do_sinal <- function(ip, Ei, ds) {
    f <- ef[id_pessoa == ip & E == Ei & data == ds, fonte_sinal]
    if (!length(f)) return(NA_character_)
    if (any(f != "biografia")) "sinal_efetivacao_api" else "biografia_camara"
  }
  data_inicio_da_regra <- function(ip, idd, Ei, ds) {
    dp <- bpos_p[id_pessoa == ip & E == Ei & abs(as.integer(data - ds)) <= JANELA_EFET, data]
    if (length(dp)) return(list(min(dp), "biografia_camara"))
    db <- if (!is.na(idd)) sinais_bev[id_dep == idd & abs(as.integer(d_assume - ds)) <= JANELA_EFET, d_assume] else as.IDate(character())
    if (length(db)) return(list(db[which.min(abs(as.integer(db - ds)))], "biografia_camara"))
    list(ds, fonte_do_sinal(ip, Ei, ds))
  }
  lig[, c("data_inicio_regra", "origem_data_inicio_regra") := {
    di <- as.IDate(rep(NA_character_, .N)); oi <- rep(NA_character_, .N)
    for (i in seq_len(.N)) if (episodio_regra[i] == "ancora") {
      r <- data_inicio_da_regra(id_pessoa[i], id_dep[i], ano_eleicao[i], data_sinal[i]); di[i] <- r[[1]]; oi[i] <- r[[2]]
    }
    list(di, oi)
  }]
  lig[, data_inicio_proposta := fifelse(!is.na(data_inicio), data_inicio, data_inicio_regra)]
  lig[, origem_data_inicio_proposta := fifelse(!is.na(data_inicio), origem_data_inicio, origem_data_inicio_regra)]
  lig <- merge(lig, sai[, .(id_pessoa = pkey, ano_eleicao = E, data_saida = data, fonte_saida)], by = c("id_pessoa", "ano_eleicao"), all.x = TRUE, sort = FALSE)
  lig <- merge(lig, unique(m6[, .(ano_eleicao, mandato_fim_leg = mandato_fim)]), by = "ano_eleicao", all.x = TRUE, sort = FALSE)
  setorder(lig, id_pessoa, ano_eleicao, data_inicio_proposta, linha)
  lig[, ultimo_episodio := seq_len(.N) == .N, by = .(id_pessoa, ano_eleicao)]
  # a saida definitiva so vale para o ultimo episodio da pessoa na legislatura, e so se for posterior ao inicio proposto
  lig[, saida_valida := ultimo_episodio & !is.na(data_saida) & !is.na(data_inicio_proposta) & data_saida >= data_inicio_proposta]
  lig[, c("data_fim_regra", "origem_data_fim_regra") := {
    dz <- as.IDate(rep(NA_character_, .N)); oz <- rep(NA_character_, .N)
    k <- saida_valida; dz[k] <- data_saida[k]; oz[k] <- fifelse(fonte_saida[k] == "biografia_camara", "biografia_camara", "fonte")
    k2 <- ultimo_episodio & !k & ano_eleicao != 2022L & !is.na(data_inicio_proposta)
    dz[k2] <- mandato_fim_leg[k2]; oz[k2] <- "convencao_fim_legislatura"
    list(dz, oz)
  }]
  lig[, data_fim_proposta := fifelse(!is.na(data_fim), data_fim, data_fim_regra)]
  lig[, origem_data_fim_proposta := fifelse(!is.na(data_fim), origem_data_fim, origem_data_fim_regra)]

  ## -------- precedencia e resultado por ocupacao
  R <- merge(nc[cd_cargo == 6L],
             lig[, .(linha, id_mandato_regra = seat_regra, caminho_regra, episodio_regra, data_sinal, ordem_pulada, empate_ord,
                     data_inicio_proposta, origem_data_inicio_proposta, data_fim_proposta, origem_data_fim_proposta)],
             by = "linha", all.x = TRUE, sort = FALSE)
  setorder(R, linha)
  stopifnot(nrow(R) == nc[cd_cargo == 6L, .N], !anyDuplicated(R$linha))
  R[, em_dominio := !is.na(chave_lista) & chave_lista %in% cad[dominio == TRUE, chave]]
  R[, `:=`(oficial = !is.na(id_mandato) & vinculo_cadeira == "fonte_da_casa",
           caminho2 = !is.na(id_mandato) & vinculo_cadeira %in% c("lista_vaga_unica", "lista_data_estrita"),
           decide = !is.na(id_mandato_regra))]
  R[, situacao := fcase(decide & is.na(id_mandato), "nova_ligacao",
                        decide & oficial & id_mandato_regra == id_mandato, "fonte_da_casa_concorda",
                        decide & oficial, "fonte_da_casa_diverge",
                        decide & caminho2 & id_mandato_regra == id_mandato, "caminho2_concorda",
                        decide & caminho2, "caminho2_diverge",
                        decide & id_mandato_regra == id_mandato, "outro_vinculo_concorda",
                        decide, "outro_vinculo_diverge",
                        default = NA_character_)]
  R[, id_mandato_final := fifelse(situacao %in% "nova_ligacao", id_mandato_regra, id_mandato)]
  R[, vinculo_final := fifelse(situacao %in% "nova_ligacao", caminho_regra, vinculo_cadeira)]
  R[, set_ini := situacao %in% "nova_ligacao" & is.na(data_inicio) & !is.na(data_inicio_proposta)]
  R[, set_fim := situacao %in% "nova_ligacao" & is.na(data_fim) & !is.na(data_fim_proposta)]
  R[, ini_final := fifelse(set_ini, data_inicio_proposta, data_inicio)]
  R[, fim_final := fifelse(set_fim, data_fim_proposta, data_fim)]
  reg("ocu_regra_ocupacoes_com_cadeira_nova_pela_regra", R[situacao %in% "nova_ligacao", .N])
  reg("ocu_regra_cadeiras_com_ocupante_novo_pela_regra", R[situacao %in% "nova_ligacao", uniqueN(id_mandato_regra)])
  reg("ocu_regra_ocupacoes_com_cadeira_nova_lista_vaga_inicial", R[situacao %in% "nova_ligacao" & caminho_regra == "lista_vaga_inicial", .N])
  reg("ocu_regra_ocupacoes_com_cadeira_nova_lista_sucessao_vaga", R[situacao %in% "nova_ligacao" & caminho_regra == "lista_sucessao_vaga", .N])
  reg("ocu_regra_ocupacoes_com_cadeira_nova_episodio_mesma_pessoa", R[situacao %in% "nova_ligacao" & episodio_regra == "mesma_pessoa", .N])
  reg("ocu_regra_ocupacoes_fonte_da_casa_regra_concorda", R[situacao %in% "fonte_da_casa_concorda", .N])
  reg("ocu_regra_ocupacoes_fonte_da_casa_regra_diverge", R[situacao %in% "fonte_da_casa_diverge", .N])
  reg("ocu_regra_ocupacoes_caminho2_ligadas_e_regra_tambem_liga", R[situacao %in% c("caminho2_concorda", "caminho2_diverge"), .N])
  reg("ocu_regra_ocupacoes_caminho2_regra_concorda", R[situacao %in% "caminho2_concorda", .N])
  reg("ocu_regra_ocupacoes_caminho2_regra_diverge", R[situacao %in% "caminho2_diverge", .N])
  reg("ocu_regra_ocupacoes_outro_vinculo_em_conflito_mantido_o_atual", R[situacao %in% c("outro_vinculo_diverge"), .N])
  reg("ocu_regra_ocupacoes_com_data_inicio_dada_pela_regra", R[set_ini == TRUE, .N])
  reg("ocu_regra_ocupacoes_com_data_inicio_biografia_camara", R[set_ini == TRUE & origem_data_inicio_proposta %in% "biografia_camara", .N])
  reg("ocu_regra_ocupacoes_com_data_inicio_sinal_efetivacao_api", R[set_ini == TRUE & origem_data_inicio_proposta %in% "sinal_efetivacao_api", .N])
  reg("ocu_regra_ocupacoes_com_data_fim_dada_pela_regra", R[set_fim == TRUE, .N])
  for (o in c("fonte", "biografia_camara", "convencao_fim_legislatura"))
    reg(paste0("ocu_regra_ocupacoes_com_data_fim_origem_", o), R[set_fim == TRUE & origem_data_fim_proposta %in% o, .N])
  # nada muda fora das listas com cadeira do dominio, e nenhum vinculo mais firme e desfeito
  stopifnot(R[situacao %in% "nova_ligacao", all(em_dominio)],
            R[oficial | caminho2, all(id_mandato_final == id_mandato & vinculo_final == vinculo_cadeira)])
  cat("\n== 4b. ocupacoes que ganham cadeira ==\n")
  print(R[situacao %in% "nova_ligacao", .(linha, id_pessoa, tipo_ocupante, ano_eleicao, sg_uf, id_mandato_regra, caminho_regra, episodio_regra,
                                         data_inicio, ini_final, origem_data_inicio_proposta, data_fim, fim_final, origem_data_fim_proposta)])
  cat("\n== 4b. ligadas por caminho anterior ou fonte oficial em que a regra tambem liga ==\n")
  print(R[situacao %in% c("fonte_da_casa_concorda", "fonte_da_casa_diverge", "caminho2_concorda", "caminho2_diverge", "outro_vinculo_diverge"),
          .(linha, id_pessoa, vinculo_cadeira, id_mandato, id_mandato_regra, caminho_regra, situacao)])

  ## -------- exclusividade: numa cadeira nao ha duas pessoas ao mesmo tempo
  # Compara os episodios vinculados antes e depois da regra (sobreposicao > TOL_DIAS). A regra nao desliga vinculo de outro
  # caminho; ela conta o conflito, e a decisao fica para o julgamento.
  lim_leg <- unique(m6[, .(id_mandato, mandato_fim)])
  mk_iv <- function(dt) {
    x <- merge(dt[!is.na(id_mandato) & !is.na(id_pessoa)], lim_leg, by = "id_mandato")
    x[is.na(fim), fim := mandato_fim]; x[!is.na(ini)]
  }
  sobrepostos <- function(x) {
    j <- merge(x, x, by = "id_mandato", suffixes = c(".a", ".b"), allow.cartesian = TRUE)[k.a < k.b & id_pessoa.a != id_pessoa.b]
    j[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
    j[dias > TOL_DIAS, .(id_mandato, k.a, k.b, dias)]
  }
  tit_iv <- oc_tit[cd_cargo == 6L, .(k = -.I, id_pessoa, id_mandato, ini = as.IDate(data_inicio), fim = as.IDate(data_fim))]
  ex_antes  <- sobrepostos(mk_iv(rbind(tit_iv, R[, .(k = linha, id_pessoa, id_mandato, ini = data_inicio, fim = data_fim)])))
  ex_depois <- sobrepostos(mk_iv(rbind(tit_iv, R[, .(k = linha, id_pessoa, id_mandato = id_mandato_final, ini = ini_final, fim = fim_final)])))
  ex_depois[, novo := !paste(k.a, k.b) %in% ex_antes[, paste(k.a, k.b)]]
  reg("ocu_exclusividade_pares_sobrepostos_antes_da_regra", nrow(ex_antes))
  reg("ocu_exclusividade_pares_sobrepostos_depois_da_regra", nrow(ex_depois))
  reg("ocu_exclusividade_pares_novos_pela_regra", ex_depois[novo == TRUE, .N])
  # k negativo e o titular da cadeira, k positivo e a linha de ocupacao nao titular
  reg("ocu_exclusividade_ocupacoes_federais_em_conflito_depois_da_regra",
      length(unique(c(ex_depois$k.a, ex_depois$k.b)[c(ex_depois$k.a, ex_depois$k.b) > 0L])))
  reg("ocu_exclusividade_ocupacoes_alteradas_pela_regra_em_conflito",
      R[situacao %in% "nova_ligacao" & linha %in% c(ex_depois$k.a, ex_depois$k.b), .N])
  reg("ocu_exclusividade_pares_de_caminho2_sobrepostos_a_outro_ocupante",
      nrow(ex_depois[k.a %in% R[caminho2 == TRUE, linha] | k.b %in% R[caminho2 == TRUE, linha]]))

  ## -------- os casos que a regra precisa reproduzir
  casos <- data.table(
    id_pessoa = c("BOCEL0279852", "BOCEL0033384", "BOCEL0034572", "BOCEL0149063"),
    cadeira_esperada = c(rep("M1998_AL_6_4580_200000206045801", 2), "M2002_PR_6_1345_724", "M2018_RJ_6_1010_190000626396"),
    inicio_esperado = as.IDate(c("1999-02-03", "1999-04-08", "2003-02-01", "2019-02-01")),
    fim_esperado = as.IDate(c("1999-04-07", "2003-01-31", "2007-01-31", "2023-01-31")))
  casos <- merge(casos, R[episodio_regra %in% "ancora", .(id_pessoa, linha, cadeira_regra = id_mandato_regra, caminho_regra,
                                                         ini_final, fim_final)], by = "id_pessoa", all.x = TRUE, sort = FALSE)
  casos[, ok := (cadeira_regra == cadeira_esperada & ini_final == inicio_esperado & fim_final == fim_esperado) %in% TRUE]
  reg("ocu_regra_casos_obrigatorios_reproduzidos_de_4", casos[, sum(ok)])
  cat("\n== 4b. casos obrigatorios ==\n"); print(casos)
  # 29/09/2026 (achado F3 da revisao de 28/09): os 4 casos do gabarito sao o conjunto em que a regra foi calibrada, e
  # a reproducao deles era so registrada. Passa a abortar a reconstrucao, porque uma regra que deixa de
  # reproduzir o proprio gabarito nao pode gerar ocupacoes.csv.
  stopifnot(casos[, all(ok)])
  ch_al <- m6[id_mandato == "M1998_AL_6_4580_200000206045801", chave]
  ord_al <- s6u[chave == ch_al][order(ord_chave)][1:3, votos_nominais]
  reg("ocu_regra_ordem_coligacao_AL1998_votos_do_1o_ao_3o", paste(ord_al, collapse = "/"))

  list(R = R, dominio = cad[dominio == TRUE, id_mandato], sem_par = sem_par, casos = casos)
})
ap <- regra$R[situacao %in% "nova_ligacao"]
ocup[ap, on = "linha", `:=`(id_mandato = i.id_mandato_regra, vinculo_cadeira = i.caminho_regra)]
ocup[ap[set_ini == TRUE], on = "linha", `:=`(data_inicio = as.character(i.data_inicio_proposta),
                                              origem_data_inicio = i.origem_data_inicio_proposta, ini_d = i.data_inicio_proposta)]
ocup[ap[set_fim == TRUE], on = "linha", `:=`(data_fim = as.character(i.data_fim_proposta),
                                              origem_data_fim = i.origem_data_fim_proposta)]
reg("ocu_regra_dominio_cadeiras_sem_nenhum_ocupante_nao_titular_ligado",
    length(setdiff(regra$dominio, ocup[!is.na(id_mandato), id_mandato])))
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
  # 28/09/2026 (v1.1): a origem passa a vir da propria ocupacao, porque a regra da secao 4b data a linha por outras fontes
  origem_data_inicio, origem_data_fim,
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
## OCU-02 (24/09/2026, achado da auditoria de 24/09/2026): a Wikipedia as vezes da o fim do vice
## que assumiu um dia depois do proprio fim do mandato da cadeira (52 casos de vice_assumiu em
## GOVERNADOR). Nenhuma ocupacao pode terminar depois do fim do mandato que a origina, e por isso
## data_fim e limitada aqui ao mandato_fim da propria cadeira, de forma geral, para toda ocupacao
## com cadeira identificada (nao so o caso conhecido).
mand_fim <- mand[, .(id_mandato, mandato_fim_d = as.IDate(as.character(mandato_fim)))]
oc[mand_fim, on = "id_mandato", mandato_fim_d := i.mandato_fim_d]
oc[, data_fim_antes_do_limite := data_fim]
oc[!is.na(mandato_fim_d) & !is.na(data_fim) & as.IDate(data_fim) > mandato_fim_d,
   data_fim := as.character(mandato_fim_d)]
oc[, limitada_ao_fim_do_mandato := !is.na(data_fim_antes_do_limite) & data_fim_antes_do_limite != data_fim]
reg("ocu_data_fim_limitada_ao_fim_do_mandato", oc[limitada_ao_fim_do_mandato == TRUE, .N])
## 29/09/2026 (achado DADOS-01 e DOC-01 da revisao de 28/09): a chave acima conta o banco inteiro (todas as
## esferas, 492.455 ocupacoes), e o README e a NOTA descrevem o ocupacoes.csv do recorte (cd_cargo 1 a 6, o mesmo
## filtro do R/64). Os textos leem a chave do recorte e a composicao por tipo_ocupante, e nao a do banco inteiro.
reg("ocu_data_fim_limitada_ao_fim_do_mandato_recorte", oc[limitada_ao_fim_do_mandato == TRUE & cd_cargo %in% 1:6, .N])
reg("ocu_data_fim_limitada_ao_fim_do_mandato_recorte_vice_assumiu",
    oc[limitada_ao_fim_do_mandato == TRUE & cd_cargo %in% 1:6 & tipo_ocupante == "vice_assumiu", .N])
reg("ocu_data_fim_limitada_ao_fim_do_mandato_recorte_governador",
    oc[limitada_ao_fim_do_mandato == TRUE & cd_cargo == 3L, .N])
reg("ocu_data_fim_limitada_ao_fim_do_mandato_fora_do_recorte", oc[limitada_ao_fim_do_mandato == TRUE & !cd_cargo %in% 1:6, .N])
oc[, `:=`(mandato_fim_d = NULL, data_fim_antes_do_limite = NULL, limitada_ao_fim_do_mandato = NULL)]
## Par 20 (28/09/2026, v1.1, decisao de Igor "Fechar a primeira em 27/04/2022"): Jose Valdevan
## (BOCEL0249016, deputado federal, SE 2018) tem duas ocupacoes na cadeira M2018_SE_6_1313_260000623622,
## ambas sem data de fim na API, e a primeira (desde 2019-02-01) ficava sobreposta a segunda (desde
## 2022-06-03). A biografia da Camara registra a perda do mandato em 27 de abril de 2022 por recontagem
## de votos, data em que o titular da cadeira no banco (BOCEL0079076) comeca a exercer. A primeira
## ocupacao fecha em 2022-04-27, com origem_data_fim "curadoria_biografia_camara", e a segunda segue
## como veio da API. A mesma biografia registra outra perda, em 14/06/2022, que nao entra nesta curadoria.
## Fonte: https://www.camara.leg.br/deputados/204385/biografia
##   "Perdeu o mandato de Deputado(a) Federal, na Legislatura 2019-2023, em 27 de Abril de 2022, em virtude de Perda de Mandato por Recontagem de Votos"
## O par esta listado em docs/EXCECOES_CONHECIDAS.csv.
k_par20 <- oc[id_pessoa == "BOCEL0249016" & id_mandato == "M2018_SE_6_1313_260000623622" &
                tipo_ocupante == "eleito_cassado_depois" & data_inicio == "2019-02-01" & is.na(data_fim), which = TRUE]
stopifnot(length(k_par20) == 1L)
oc[k_par20, `:=`(data_fim = "2022-04-27", origem_data_fim = "curadoria_biografia_camara")]
reg("ocu_par20_primeira_ocupacao_fechada_curadoria", length(k_par20))
# invariante da coluna nova: origem_data_fim tem valor se, e somente se, data_fim tem valor, em todos os cargos
stopifnot(identical(is.na(oc$origem_data_fim), is.na(oc$data_fim)))
reg("ocu_origem_data_fim_preenchida", oc[!is.na(origem_data_fim), .N])
for (o in c("fonte", "biografia_camara", "convencao_fim_legislatura", "curadoria_biografia_camara"))
  reg(paste0("ocu_origem_data_fim_", o), oc[origem_data_fim %in% o, .N])
for (o in c("fonte", "convencao", "curadoria_ocu07", "biografia_camara", "sinal_efetivacao_api"))
  reg(paste0("ocu_origem_data_inicio_", o), oc[origem_data_inicio %in% o, .N])
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
