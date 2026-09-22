# 54_sapl_observacao.R — o interregno que estava escrito em texto livre no SAPL
#
# Decisao de 03/09/2026: comecar pela re-extracao do que ja esta em disco. A coleta do
# SAPL (python/fetch_sapl_municipal.py -> R/15) carregou o campo `observacao` de cada mandato ate
# data/exercicio_camaras_municipais.csv e nunca o leu. E nele que a Casa escreve o interregno com
# as duas pontas e a causa, do tipo "ASSUMIU O CARGO, NO LUGAR DO VEREADOR X, EM 12/08/2022, EM
# RAZAO DO TITULAR ASSUMIR O CARGO DE SECRETARIO MUNICIPAL. VOLTOU A CONDICAO DE SUPLENTE EM
# 31/12/2022". Uma linha dessas resolve tres coisas que faltam no banco de uma vez: a data em que
# o titular saiu do exercicio, a causa da saida, e o nome do titular cuja cadeira o suplente
# ocupou, que e o vinculo que R/42 hoje nao consegue fazer em 6.085 ocupacoes.
#
# O que este script NAO faz: nao mexe em mandatos.csv nem em ocupacoes.csv. Ele produz duas
# tabelas de eventos, com a precisao declarada linha a linha, para R/10 e R/42 consumirem depois.
# A leitura do texto segue regra explicita e deterministica, para ser reproduzivel.
#
# Entrada: data/exercicio_camaras_municipais.csv (R/15), data/mandatos.csv, data/pessoas.csv,
#          data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:   data/sapl_observacao_eventos.csv   (uma linha por registro de mandato com texto lido)
#          data/sapl_observacao_titular.csv   (uma linha por mandato do BOCEL com saida datada)
#          output/verificacao/sapl_observacao_nao_lidas.csv (o que a regra nao classificou)
# Execucao: cd ~/bocel && Rscript --vanilla R/54_sapl_observacao.R
set.seed(20260903)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
ESTE <- file.path(root, "R", "54_sapl_observacao.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)

norm <- function(x) {
  y <- stri_trans_general(toupper(x), "Latin-ASCII")
  y <- gsub("[^A-Z ]", " ", y); gsub(" +", " ", trimws(y))
}
# texto de trabalho: sem acento, maiusculo, com a pontuacao que delimita nome preservada
folda <- function(x) {
  y <- stri_trans_general(as.character(x), "Latin-ASCII")
  y <- gsub("[\r\n\t]+", " ", toupper(y))
  y <- gsub("[^A-Z0-9 ,.;:/()-]", " ", y)
  gsub(" +", " ", trimws(y))
}

## ---------------------------------------------------------------- 1. o texto
ex <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character",
            na.strings = "NA", encoding = "UTF-8")
ex[, linha := .I]
# id_mandato_sapl e identificador LOCAL de cada instancia e repete entre camaras; a chave
# global do registro e o dominio somado a ele.
ex[, chave_sapl := paste0(dominio, "#", id_mandato_sapl)]
ex[, obs := trimws(fifelse(is.na(observacao), "", observacao))]
ex[, obs := fifelse(toupper(obs) %in% c("NONE", "NULL", "-", "0"), "", obs)]
n_linhas <- nrow(ex); n_obs <- ex[obs != "", .N]
cat("linhas do SAPL:", n_linhas, "| com observacao:", n_obs, "\n")
reg("obs_linhas_sapl", n_linhas)
reg("obs_com_texto", n_obs)

tx <- ex[obs != ""]
tx[, TXT := folda(obs)]
# texto curto que so repete o que o campo estruturado ja diz nao e evento
RUIDO <- "^(SUPLENTE|VEREADOR[A]?|VEREADOR[A]? ELEITO[A]?|VEREADOR[A]? SUPLENTE|ELEIT[OA] POR (QP|MEDIA)|FONTE DE DADOS: TSE|P[A-Z]{1,3}|PMDB|PSDB|PTB|NONE)[ .]*$"
tx[, so_rotulo := grepl(RUIDO, TXT)]
reg("obs_so_rotulo", tx[so_rotulo == TRUE, .N])

## ---------------------------------------------------------------- 2. datas dentro do texto
MESES <- c("JANEIRO","FEVEREIRO","MARCO","ABRIL","MAIO","JUNHO","JULHO",
           "AGOSTO","SETEMBRO","OUTUBRO","NOVEMBRO","DEZEMBRO")
extrai_datas <- function(TXT) {
  # numerica dd/mm/aaaa
  m1 <- gregexpr("([0-9]{1,2})/([0-9]{1,2})/([0-9]{4})", TXT)
  s1 <- regmatches(TXT, m1)[[1]]; p1 <- as.integer(m1[[1]]); if (p1[1] == -1L) { s1 <- character(); p1 <- integer() }
  d1 <- if (length(s1)) {
    pt <- tstrsplit(s1, "/", fixed = TRUE)
    sprintf("%04d-%02d-%02d", as.integer(pt[[3]]), as.integer(pt[[2]]), as.integer(pt[[1]]))
  } else character()
  # por extenso: "17 DE OUTUBRO DE 2007", "1o DE JANEIRO DE 1997"
  pe <- sprintf("([0-9]{1,2})[O.]? DE (%s) DE ([0-9]{4})", paste(MESES, collapse = "|"))
  m2 <- gregexpr(pe, TXT)
  s2 <- regmatches(TXT, m2)[[1]]; p2 <- as.integer(m2[[1]]); if (p2[1] == -1L) { s2 <- character(); p2 <- integer() }
  d2 <- if (length(s2)) vapply(s2, function(s) {
    g <- regmatches(s, regexec(pe, s))[[1]]
    sprintf("%04d-%02d-%02d", as.integer(g[4]), match(g[3], MESES), as.integer(g[2]))
  }, character(1), USE.NAMES = FALSE) else character()
  d <- c(d1, d2); p <- c(p1, p2)
  if (!length(d)) return(list(datas = character(), pos = integer()))
  o <- order(p); d <- d[o]; p <- p[o]
  # descarta data impossivel
  v <- !is.na(as.IDate(d, format = "%Y-%m-%d")) & substr(d, 1, 4) >= "1988" & substr(d, 1, 4) <= "2030"
  list(datas = d[v], pos = p[v])
}
dt_list <- lapply(tx$TXT, extrai_datas)
tx[, n_datas := vapply(dt_list, function(z) length(z$datas), integer(1))]
reg("obs_com_data", tx[n_datas > 0, .N])

## ---------------------------------------------------------------- 3. causa e papel
# A causa segue o vocabulario de forma_saida do banco, com um detalhe a mais para nao perder a
# distincao entre licenca para cargo no Executivo e licenca de saude, que e o achado substantivo.
classifica <- function(TXT) {
  fcase(
    grepl("FALEC|OBITO|IN MEMORIA|MORTE D", TXT), "falecimento",
    grepl("CASSA(D|CAO)|PERD(A|EU) (O|DO) MANDATO|INFIDELIDADE PARTIDARIA|PERDA DO CARGO", TXT), "cassacao",
    grepl("RENUNC", TXT), "renuncia",
    grepl("PRISAO|PREVENTIVA|DECIS(AO|SAO) JUDICIAL|LIMINAR|MEDIDA CAUTELAR|POR DETERMINACAO JUDICIAL|AFASTAD[OA] POR DECIS", TXT), "afastamento_judicial",
    grepl("SECRETARI[AO]|ASSUMIU A PASTA|NOMEAD[OA] PARA O CARGO|ASSUMIU O CARGO DE SECRET|CHEFE DE GABINETE|ASSUMIU A REPRESENTACAO DO MUNICIPIO", TXT), "licenca_cargo_executivo",
    grepl("LICEN[CS]A (MEDICA|SAUDE|PARA TRATAMENTO)|MOTIVO DE SAUDE|TRATAMENTO DE SAUDE|LICENCIOU-SE POR MOTIVO DE SAUDE", TXT), "licenca_saude",
    grepl("MATERNIDADE|GESTANTE|LICENCA GESTANTE", TXT), "licenca_maternidade",
    grepl("INTERESSE PARTICULAR|ASSUNTOS PARTICULARES|INTERESSES PARTICULARES", TXT), "licenca_particular",
    grepl("MISSAO OFICIAL", TXT), "missao_oficial",
    grepl("LICEN[CS]", TXT), "licenca_sem_causa",
    grepl("AFASTOU|AFASTAD[OA]|AFASTAMENTO|SUSPENSAO DO MANDATO", TXT), "afastamento_sem_causa",
    default = NA_character_)
}
tx[, causa := classifica(TXT)]
# forma_saida canonica do banco
tx[, forma_saida_causa := fcase(
  causa == "falecimento", "falecimento",
  causa == "cassacao", "cassacao",
  causa == "renuncia", "renuncia",
  causa %in% c("afastamento_judicial", "afastamento_sem_causa"), "afastamento",
  causa %in% c("licenca_cargo_executivo","licenca_saude","licenca_maternidade",
               "licenca_particular","missao_oficial","licenca_sem_causa"), "licenca",
  default = NA_character_)]

# Papel do registro. Um mesmo texto pode narrar os dois lados; manda quem o registro descreve.
# "no lugar de X", "em substituicao a X", "assumiu a vaga de X" dizem que ESTA linha e do suplente.
ENTROU <- "NO LUGAR D|EM SUBSTITUICAO A|ASSUMIU A VAGA|ASSUMIU A SUPLENCIA|ASSUMIU O MANDATO|ASSUMIU O CARGO DE VEREADOR|ASSUMIU COMO VEREADOR|TOMOU POSSE EM SUBSTITUICAO|SUBSTITUINDO O|VAGA DEIXADA"
SAIU <- "LICENCIOU|LICENCIAD|AFASTOU-SE|AFASTAD[OA]|RENUNC|FOI CASSAD|CASSAD[OA]|FALEC|ASSUMIU A SECRETARIA|ASSUMIU O CARGO DE SECRET|NOMEAD[OA] PARA O CARGO|EXONERAD"
tx[, papel := fcase(grepl(ENTROU, TXT), "suplente_assumiu",
                    grepl(SAIU, TXT),   "titular_saiu",
                    default = NA_character_)]
# o campo estruturado desempata quando o texto nao diz
tx[is.na(papel) & titular == "FALSE" & !is.na(causa), papel := "suplente_assumiu"]
tx[is.na(papel) & titular == "TRUE"  & !is.na(causa), papel := "titular_saiu"]

## ---------------------------------------------------------------- 4. o titular nomeado no texto
# Cada padrao captura o nome ate um delimitador. O nome so vale se tiver ao menos dois tokens
# de tres letras, o que corta captura de "O CARGO", "A VAGA" e afins.
PADROES <- c(
  "NO LUGAR D[EOA]S? (?:VEREADOR[AE]?S? |EX[- ]?VEREADOR[AE]?S? |SENHOR[A]? |SR[A]?\\.? |VER\\.? |TITULAR |O |A )*([A-Z][A-Z ]{5,60})",
  "EM SUBSTITUICAO A[OS]? (?:VEREADOR[AE]?S? |EX[- ]?VEREADOR[AE]?S? |SENHOR[A]? |SR[A]?\\.? |VER\\.? |TITULAR )*([A-Z][A-Z ]{5,60})",
  "ASSUMIU A VAGA D[EOA]S? (?:VEREADOR[AE]?S? |EX[- ]?VEREADOR[AE]?S? |SENHOR[A]? |SR[A]?\\.? |VER\\.? |TITULAR )*([A-Z][A-Z ]{5,60})",
  "VAGA DEIXADA (?:POR|PEL[OA]) (?:VEREADOR[AE]?S? |EX[- ]?VEREADOR[AE]?S? |SENHOR[A]? |SR[A]?\\.? )*([A-Z][A-Z ]{5,60})",
  "SUBSTITUINDO O? ?A? ?(?:VEREADOR[AE]?S? |EX[- ]?VEREADOR[AE]?S? |TITULAR )*([A-Z][A-Z ]{5,60})"
)
CORTA <- " QUE | EM | POR | DEVIDO | NO DIA | A PARTIR | DURANTE | PELO | PELA | ATE | E QUE |, |\\.|;|:|/"
captura <- function(TXT) {
  for (p in PADROES) {
    g <- regmatches(TXT, regexec(p, TXT))[[1]]
    if (length(g) >= 2) {
      nm <- g[2]
      nm <- strsplit(nm, CORTA)[[1]][1]
      nm <- norm(nm)
      # tira titulo residual grudado na frente
      nm <- sub("^(VEREADOR[A]?|EX VEREADOR[A]?|SENHOR[A]?|SR[A]?|VER|TITULAR|O|A) ", "", nm)
      tk <- strsplit(nm, " ")[[1]]; tk <- tk[nchar(tk) >= 3]
      if (length(tk) >= 2) return(paste(tk, collapse = " "))
    }
  }
  NA_character_
}
tx[, titular_nome := vapply(TXT, captura, character(1), USE.NAMES = FALSE)]
reg("obs_com_titular_nomeado", tx[!is.na(titular_nome), .N])

## ---------------------------------------------------------------- 5. as duas pontas do interregno
# A primeira data do texto e a entrada. Uma data que venha depois de um marcador de retorno e a
# saida do suplente, que e tambem a volta do titular ao exercicio.
RETORNO <- "VOLTOU|RETORNOU|REASSUMIU|REASSUMINDO|VOLTANDO A CONDICAO|TER SIDO EXONERAD|EXONERAD[OA] DO REFERIDO|ATE O DIA|ENCERROU|TERMINO"
pos_retorno <- function(TXT) { m <- regexpr(RETORNO, TXT); if (m == -1L) NA_integer_ else as.integer(m) }
tx[, pos_ret := vapply(TXT, pos_retorno, integer(1), USE.NAMES = FALSE)]
tx[, data_evento := NA_character_]
tx[, data_retorno := NA_character_]
for (i in which(tx$n_datas > 0)) {
  z <- dt_list[[i]]; pr <- tx$pos_ret[i]
  antes <- if (is.na(pr)) rep(TRUE, length(z$pos)) else z$pos < pr
  if (any(antes))  set(tx, i, "data_evento",  z$datas[antes][1])
  if (!is.na(pr) && any(!antes)) set(tx, i, "data_retorno", z$datas[!antes][1])
}
# a data do texto nunca pode cair fora da legislatura que a Casa declara
tx[, `:=`(li = as.IDate(legislatura_inicio), lf = as.IDate(legislatura_fim))]
tx[!is.na(data_evento) & !is.na(li) & !is.na(lf) &
     (as.IDate(data_evento) < li - 60L | as.IDate(data_evento) > lf + 60L),
   data_evento := NA_character_]
tx[!is.na(data_retorno) & !is.na(li) & !is.na(lf) &
     (as.IDate(data_retorno) < li - 60L | as.IDate(data_retorno) > lf + 60L),
   data_retorno := NA_character_]
tx[!is.na(data_evento) & !is.na(data_retorno) & as.IDate(data_retorno) <= as.IDate(data_evento),
   data_retorno := NA_character_]

## ---------------------------------------------------------------- 6. pareamento do titular nomeado
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA",
              encoding = "UTF-8")[cd_cargo == "13"]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano_eleicao,
                       sg_partido, mandato_inicio, mandato_fim, forma_saida_atual = forma_saida,
                       fonte_atual = fonte_forma_saida, data_fim_atual = data_fim_efetiva)],
              pess[, .(id_pessoa, nome)], by = "id_pessoa")
mand[, nome_norm := norm(nome)]
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(read_parquet(f, col_select = c("ANO_ELEICAO","SG_UE","CD_CARGO",
                                                    "NR_CANDIDATO","SQ_CANDIDATO","NM_URNA_CANDIDATO")))
  x[CD_CARGO == "13"]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, unique(cand[, .(id_mandato, urna_norm = norm(NM_URNA_CANDIDATO))]),
              by = "id_mandato", all.x = TRUE)

alvo <- tx[!is.na(titular_nome), .(linha, sg_ue, ano_eleicao = ano_eleicao_bocel, titular_nome)]
casa <- function(alvo, col_ref, rotulo) {
  if (!nrow(alvo)) return(data.table(linha = integer(), id_mandato_titular = character(),
                                     id_pessoa_titular = character(), regra_titular = character()))
  ref <- mand[!is.na(get(col_ref)) & get(col_ref) != "",
              .(n = uniqueN(id_mandato), id_mandato = id_mandato[1], id_pessoa = id_pessoa[1]),
              by = c("sg_ue", "ano_eleicao", col_ref)][n == 1]
  setnames(ref, col_ref, "chave")
  j <- merge(alvo, ref, by.x = c("sg_ue","ano_eleicao","titular_nome"),
             by.y = c("sg_ue","ano_eleicao","chave"))
  j[, .(linha, id_mandato_titular = id_mandato, id_pessoa_titular = id_pessoa, regra_titular = rotulo)]
}
p1 <- casa(alvo, "nome_norm", "nome_completo")
p2 <- casa(alvo[!linha %in% p1$linha], "urna_norm", "nome_de_urna")
# terceira regra: todos os tokens do nome capturado contidos no nome civil ou no de urna, unico
resto <- alvo[!linha %in% c(p1$linha, p2$linha)]
p3 <- data.table(linha = integer(), id_mandato_titular = character(),
                 id_pessoa_titular = character(), regra_titular = character())
if (nrow(resto)) {
  j <- merge(resto, mand[, .(sg_ue, ano_eleicao, nome_norm, urna_norm, id_mandato, id_pessoa)],
             by = c("sg_ue","ano_eleicao"), allow.cartesian = TRUE)
  if (nrow(j)) {
    tk <- strsplit(j$titular_nome, " ")
    a <- strsplit(j$nome_norm, " "); b <- strsplit(fifelse(is.na(j$urna_norm), "", j$urna_norm), " ")
    j[, contido := mapply(function(t, x, y) length(t) >= 2 && (all(t %in% x) || all(t %in% y)), tk, a, b)]
    p3 <- j[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = linha][
      , .(linha, id_mandato_titular = id_mandato, id_pessoa_titular = id_pessoa,
          regra_titular = "tokens_no_nome_civil_ou_urna")]
  }
}
par <- rbindlist(list(p1, p2, p3), use.names = TRUE)
par <- par[!duplicated(linha)]
tx[, `:=`(id_mandato_titular = NA_character_, id_pessoa_titular = NA_character_,
          regra_titular = NA_character_)]
tx[par, on = "linha", `:=`(id_mandato_titular = i.id_mandato_titular,
                           id_pessoa_titular = i.id_pessoa_titular,
                           regra_titular = i.regra_titular)]
# o suplente nao pode ser o proprio titular que ele diz substituir
tx[!is.na(id_mandato_titular) & !is.na(id_mandato_bocel) & id_mandato_titular == id_mandato_bocel,
   `:=`(id_mandato_titular = NA_character_, id_pessoa_titular = NA_character_,
        regra_titular = "descartado_e_o_proprio")]
reg("obs_titular_pareado", tx[!is.na(id_mandato_titular), .N])
cat("titular nomeado:", tx[!is.na(titular_nome), .N],
    "| pareado ao BOCEL:", tx[!is.na(id_mandato_titular), .N], "\n")
print(tx[, .N, by = regra_titular][order(-N)])

## ---------------------------------------------------------------- 7. interregno nao e saida
# Correcao de modelagem feita na primeira rodada deste script. A leitura ingenua tratava toda
# licenca narrada no texto como forma de saida, e com isso contradizia `fim_regular` em 516
# mandatos que na verdade terminaram normalmente depois de o titular voltar. Licenca com retorno
# e interregno DENTRO do mandato, e nao o fim dele. Isso responde com dado a pendencia 1, cuja decisao
# recusou tanto contar toda licenca como saida
# quanto rebaixar tudo para nao_observado, e pediu o tempo da licenca ou o registro da volta.
#
# A regra de separacao usa duas evidencias independentes de retorno: o proprio texto, quando
# narra a volta, e a data de fim do mandato no campo estruturado, quando ela alcanca o fim da
# legislatura. Falecimento, cassacao e renuncia encerram o mandato por definicao e nao entram
# nessa distincao.
IRREVERSIVEL <- c("falecimento", "cassacao", "renuncia")
tx[, fim_leg := as.IDate(legislatura_fim)]
tx[, dfim := as.IDate(data_fim_mandato)]
tx[, retorno_evidenciado := fcase(
  !is.na(data_retorno), "texto_narra_a_volta",
  !is.na(dfim) & !is.na(fim_leg) & dfim >= fim_leg - 45L, "mandato_alcanca_o_fim_da_legislatura",
  default = NA_character_)]
tx[, tipo_evento := fcase(
  causa %in% IRREVERSIVEL, "fim_de_mandato",
  !is.na(causa) & !is.na(retorno_evidenciado), "interregno_temporario",
  !is.na(causa), "afastamento_sem_retorno_observado",
  default = NA_character_)]
# so o fim de mandato de verdade vira forma de saida; o interregno nao toca em forma_saida
tx[tipo_evento != "fim_de_mandato", forma_saida_causa := NA_character_]

## ---------------------------------------------------------------- 8. confianca da leitura
# alta: o texto nomeia o titular, o pareamento fechou e ha data
# media: ha causa e data, sem titular pareado
# baixa: ha causa sem data, ou data sem causa
tx[, confianca := fcase(
  !is.na(id_mandato_titular) & !is.na(data_evento) & !is.na(causa), "alta",
  !is.na(causa) & !is.na(data_evento), "media",
  !is.na(causa) | !is.na(data_evento), "baixa",
  default = "nula")]
tx[, precisao_data := fcase(!is.na(data_evento), "ato_no_texto", default = NA_character_)]

ev <- tx[so_rotulo == FALSE & confianca != "nula",
         .(sg_ue, id_municipio_ibge, uf, dominio, ano_eleicao = ano_eleicao_bocel,
           legislatura_numero, legislatura_inicio, legislatura_fim,
           nome_fonte, nome_parlamentar, titular_registro = titular,
           id_pessoa_bocel, id_mandato_bocel, metodo_pareamento,
           papel, causa, tipo_evento, retorno_evidenciado, forma_saida_causa,
           data_evento, data_retorno, precisao_data,
           titular_nome, id_mandato_titular, id_pessoa_titular, regra_titular,
           confianca, chave_sapl, id_mandato_sapl, url, observacao = obs)]
setorder(ev, uf, sg_ue, ano_eleicao, data_evento, na.last = TRUE)
fwrite(ev, "data/sapl_observacao_eventos.csv", na = "NA", quote = TRUE)
cat("eventos gravados:", nrow(ev), "\n")
reg("obs_eventos", nrow(ev))
for (cf in c("alta","media","baixa")) reg(paste0("obs_eventos_", cf), ev[confianca == cf, .N])
for (te in c("fim_de_mandato","interregno_temporario","afastamento_sem_retorno_observado"))
  reg(paste0("obs_eventos_", te), ev[tipo_evento == te, .N])
reg("obs_eventos_com_data", ev[!is.na(data_evento), .N])
reg("obs_eventos_com_as_duas_pontas", ev[!is.na(data_evento) & !is.na(data_retorno), .N])
print(ev[, .N, by = .(confianca, papel)][order(-N)])
print(ev[, .N, by = .(tipo_evento, causa)][order(-N)])

## ---------------------------------------------------------------- 9. o mandato do titular
# Duas origens. (a) O registro do proprio titular que narra o que lhe aconteceu. (b) O registro
# do suplente que nomeia o titular substituido, caso em que a entrada do suplente data a saida
# do titular do exercicio. Onde as duas existem para o mesmo mandato, fica a do proprio titular.
cols <- function(d, org) d[, .(id_mandato = .id, tipo_evento, causa, forma_saida_obs = forma_saida_causa,
                               data_evento, data_retorno, retorno_evidenciado,
                               origem = org, confianca, url, trecho = observacao)]
a <- ev[papel == "titular_saiu" & !is.na(id_mandato_bocel) & !is.na(causa)][, .id := id_mandato_bocel]
b <- ev[papel == "suplente_assumiu" & !is.na(id_mandato_titular) & !is.na(causa)][, .id := id_mandato_titular]
tit <- rbindlist(list(cols(a, "registro_do_titular"), cols(b, "registro_do_suplente")), use.names = TRUE)
setorder(tit, id_mandato, origem, -confianca, data_evento, na.last = TRUE)
n_conflito <- tit[, .N, by = id_mandato][N > 1, .N]
tit_u <- tit[, .SD[1], by = id_mandato]
tit_u <- merge(tit_u, mand[, .(id_mandato, sg_ue, ano_eleicao, nome, sg_partido,
                               mandato_inicio, mandato_fim, forma_saida_atual,
                               fonte_atual, data_fim_atual)], by = "id_mandato", all.x = TRUE)
tit_u[, sem_saida_hoje := is.na(forma_saida_atual) | forma_saida_atual == "nao_observado"]
tit_u[, ganho := fcase(
  tipo_evento == "fim_de_mandato" & sem_saida_hoje, "forma_de_saida_nova",
  tipo_evento == "fim_de_mandato" & !sem_saida_hoje, "forma_de_saida_ja_conhecida",
  tipo_evento == "interregno_temporario", "interregno_dentro_do_mandato",
  default = "afastamento_sem_retorno_observado")]
fwrite(tit_u[order(sg_ue, ano_eleicao)], "data/sapl_observacao_titular.csv", na = "NA", quote = TRUE)
cat("mandatos de titular alcancados pelo texto:", nrow(tit_u), "\n")
print(tit_u[, .N, by = ganho][order(-N)])
print(tit_u[, .N, by = .(tipo_evento, causa)][order(-N)])
reg("obs_titular_mandatos", nrow(tit_u))
reg("obs_titular_conflito_de_origem", n_conflito)
for (g in unique(tit_u$ganho)) reg(paste0("obs_titular_", g), tit_u[ganho == g, .N])
reg("obs_titular_com_retorno_datado", tit_u[!is.na(data_retorno), .N])
reg("obs_titular_interregno_com_as_duas_pontas",
    tit_u[tipo_evento == "interregno_temporario" & !is.na(data_evento) & !is.na(data_retorno), .N])
# discordancia real: o texto diz fim de mandato e o banco diz outra coisa
disc <- tit_u[tipo_evento == "fim_de_mandato" & !is.na(forma_saida_atual) &
                forma_saida_atual != "nao_observado" & forma_saida_atual != forma_saida_obs]
fwrite(disc, "output/verificacao/sapl_observacao_discorda_do_banco.csv", na = "NA", quote = TRUE)
reg("obs_titular_discorda_do_banco", nrow(disc))
print(disc[, .N, by = .(forma_saida_atual, forma_saida_obs)][order(-N)])

## ---------------------------------------------------------------- 10. o que a regra nao leu
nao <- tx[so_rotulo == FALSE & confianca == "nula", .(uf, sg_ue, ano_eleicao_bocel, nome_fonte,
                                                      titular, id_mandato_sapl, url, observacao = obs)]
fwrite(nao, "output/verificacao/sapl_observacao_nao_lidas.csv", na = "NA", quote = TRUE)
cat("textos nao classificados:", nrow(nao), "\n")
reg("obs_nao_classificadas", nrow(nao))
cat("54_sapl_observacao: concluido\n")
