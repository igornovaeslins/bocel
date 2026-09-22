# 55_sapl_presenca.R — a janela de exercicio do vereador, tirada da lista de presenca em plenario
#
# Por que. Ate aqui o banco so sabia datar o interregno onde a Casa escreveu o ato num campo de
# texto, o que alcanca pouco mais de mil mandatos. A lista de presenca por sessao plenaria e outro
# tipo de evidencia: ela nao depende de a Casa narrar nada, e sim de o secretario registrar quem
# sentou na cadeira naquele dia. A primeira e a ultima presenca de uma pessoa dentro de uma
# legislatura delimitam o periodo em que ela ocupou a cadeira, e uma sequencia de sessoes perdidas
# no meio, com outra pessoa presente na mesma janela, e o interregno visto de fora.
#
# Como. A presenca (sessao/sessaoplenariapresenca) traz o par sessao-parlamentar e nao traz data;
# a data vem do join com sessao/sessaoplenaria, campo data_inicio. O parlamentar liga-se ao BOCEL
# pelo registro de mandato ja pareado em R/15.
#
# Precisao declarada. A data de entrada por presenca fica atras da data do ato, porque o convocado
# so aparece na primeira sessao seguinte a posse. A medida dessa folga sai no proprio script, pelo
# confronto com os mandatos que o SAPL ja data, e vai para a coluna precisao_data.
#
# Entrada: data_raw/sapl_presenca/<uf>/<sg_ue>/{sessaoplenaria,sessaoplenariapresenca}.json
#          data_raw/sapl_municipal/<uf>/<sg_ue>/mandato.json (parlamentar de cada mandato)
#          data/exercicio_camaras_municipais.csv (pareamento com o BOCEL), data/mandatos.csv
# Saida:   data/sapl_presenca_exercicio.csv   (pessoa x legislatura x janela de presenca)
#          data/sapl_presenca_lacunas.csv     (afastamento visto por sessoes perdidas)
#          data/sapl_presenca_cobertura.csv   (o que cada instancia serviu)
# Execucao: cd ~/bocel && Rscript --vanilla R/55_sapl_presenca.R
set.seed(20260903)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
ESTE <- file.path(root, "R", "55_sapl_presenca.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
d10 <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x), x, NA_character_) }

## ---------------------------------------------------------------- 1. leitura do cache
PRES <- "data_raw/sapl_presenca"
dirs <- list.dirs(PRES, recursive = TRUE)
dirs <- dirs[file.exists(file.path(dirs, "sessaoplenariapresenca.json"))]
cat("instancias com cache de presenca:", length(dirs), "\n")
reg("pres_instancias_com_cache", length(dirs))

ler <- function(f) {
  if (!file.exists(f)) return(NULL)
  x <- tryCatch(fromJSON(f, simplifyVector = TRUE, flatten = TRUE), error = function(e) NULL)
  if (is.null(x) || length(x) == 0 || !is.data.frame(x)) return(NULL)
  as.data.table(x)
}
col <- function(dt, nm) if (!is.null(dt) && nm %in% names(dt)) as.character(dt[[nm]]) else
  rep(NA_character_, if (is.null(dt)) 0L else nrow(dt))

partes <- vector("list", length(dirs)); cobs <- vector("list", length(dirs))
for (k in seq_along(dirs)) {
  d <- dirs[k]
  uf <- basename(dirname(d)); sg_ue <- basename(d)
  se <- ler(file.path(d, "sessaoplenaria.json"))
  pr <- ler(file.path(d, "sessaoplenariapresenca.json"))
  cobs[[k]] <- data.table(uf = uf, sg_ue = sg_ue,
                          n_sessoes = if (is.null(se)) 0L else nrow(se),
                          n_presencas = if (is.null(pr)) 0L else nrow(pr))
  if (is.null(se) || is.null(pr)) next
  S <- data.table(sessao = as.character(se$id), data_sessao = d10(col(se, "data_inicio")),
                  legislatura = col(se, "legislatura"),
                  sessao_legislativa = col(se, "sessao_legislativa"),
                  tipo_sessao = col(se, "tipo"))
  S <- S[!is.na(data_sessao)]
  if (!nrow(S)) next
  P <- data.table(sessao = col(pr, "sessao_plenaria"), parlamentar = col(pr, "parlamentar"))
  P <- P[!is.na(sessao) & !is.na(parlamentar)]
  # A Casa as vezes grava o mesmo parlamentar duas vezes na mesma sessao (medido em 3 de 296
  # registros em Ipira/BA). Presenca e fato sobre pessoa x sessao, e nao contagem de linhas,
  # entao o par se resolve aqui, antes de qualquer contagem.
  P <- unique(P, by = c("sessao", "parlamentar"))
  if (!nrow(P)) next
  X <- merge(P, S, by = "sessao")
  if (!nrow(X)) next
  X[, `:=`(uf = uf, sg_ue = sg_ue)]
  partes[[k]] <- X
}
pres <- rbindlist(partes[!vapply(partes, is.null, logical(1))], use.names = TRUE)
cob <- rbindlist(cobs, use.names = TRUE)
fwrite(cob[order(uf, sg_ue)], "data/sapl_presenca_cobertura.csv", na = "NA")
cat("presencas datadas:", nrow(pres), "| instancias com presenca datada:", pres[, uniqueN(paste(uf, sg_ue))], "\n")
reg("pres_presencas_datadas", nrow(pres))
reg("pres_instancias_com_presenca_datada", pres[, uniqueN(paste(uf, sg_ue))])
reg("pres_sessoes_datadas", pres[, uniqueN(paste(uf, sg_ue, sessao))])
stopifnot(nrow(pres) > 0)

## ---------------------------------------------------------------- 2. parlamentar -> mandato do BOCEL
# O identificador de parlamentar e local da instancia; a ponte com o BOCEL passa pelo registro de
# mandato, que R/15 ja pareou. Um parlamentar pode ter varios mandatos, um por legislatura, e por
# isso a chave da ponte e (instancia, parlamentar, legislatura).
mfs <- list.files("data_raw/sapl_municipal", pattern = "^mandato\\.json$", recursive = TRUE, full.names = TRUE)
mp <- rbindlist(lapply(mfs, function(f) {
  x <- ler(f); if (is.null(x)) return(NULL)
  data.table(uf = basename(dirname(dirname(f))), sg_ue = basename(dirname(f)),
             id_mandato_sapl = as.character(x$id), parlamentar = col(x, "parlamentar"),
             legislatura = col(x, "legislatura"), titular_sapl = col(x, "titular"),
             data_inicio_sapl = d10(col(x, "data_inicio_mandato")),
             data_fim_sapl = d10(col(x, "data_fim_mandato")))
}), use.names = TRUE)
ex <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
ex[, chave_sapl := paste0(dominio, "#", id_mandato_sapl)]
# o dominio nao esta no caminho do cache, entao a ponte usa (uf, sg_ue, id_mandato_sapl)
pon <- merge(mp, ex[, .(uf, sg_ue, id_mandato_sapl, dominio, chave_sapl, ano_eleicao_bocel,
                        legislatura_inicio, legislatura_fim, nome_fonte, nome_parlamentar,
                        id_pessoa_bocel, id_mandato_bocel, titular, metodo_pareamento)],
             by = c("uf", "sg_ue", "id_mandato_sapl"), all.x = TRUE)
cat("registros de mandato com ponte:", pon[!is.na(id_mandato_bocel), .N], "de", nrow(pon), "\n")
reg("pres_mandatos_com_ponte_bocel", pon[!is.na(id_mandato_bocel), .N])

# A ponte tem mais de um registro de mandato para a mesma pessoa na mesma legislatura sempre que
# a Casa abre um registro por periodo de exercicio, o caso de quem foi convocado como suplente e
# depois efetivado (Fortaleza/CE, legislatura 18, tem tres registros para o parlamentar 14). A
# presenca nao sabe atribuir a sessao a um desses registros, porque o que ela observa e a pessoa
# sentada na cadeira. A unidade da janela e portanto pessoa x legislatura, e a ponte colapsa aqui,
# antes da juncao, para nao multiplicar linha de presenca por registro administrativo.
setorder(pon, uf, sg_ue, parlamentar, legislatura, data_inicio_sapl, na.last = TRUE)
pon_n <- pon[, .N, by = .(uf, sg_ue, parlamentar, legislatura)]
reg("pres_pessoas_legislatura_com_mais_de_um_registro", pon_n[N > 1, .N])
pon <- pon[, .(
  n_registros_sapl  = .N,
  id_mandato_sapl   = paste(unique(id_mandato_sapl), collapse = ";"),
  dominio           = dominio[1],
  chave_sapl        = chave_sapl[1],
  # o inicio e o fim do periodo coberto pelos registros, porque a janela cobre a legislatura toda
  data_inicio_sapl  = if (all(is.na(data_inicio_sapl))) NA_character_ else min(data_inicio_sapl, na.rm = TRUE),
  data_fim_sapl     = if (all(is.na(data_fim_sapl)))    NA_character_ else max(data_fim_sapl,    na.rm = TRUE),
  # a coluna titular passa a dizer se a pessoa foi titular em ALGUM dos registros
  titular_sapl      = if (any(titular_sapl %in% c("TRUE","True","true"))) "TRUE" else titular_sapl[1],
  # o vinculo com o BOCEL so sobrevive se for unico; dois mandatos distintos do BOCEL para a mesma
  # pessoa na mesma legislatura sao ambiguidade da fonte, e ambiguidade nao vira dado
  ano_eleicao_bocel   = ano_eleicao_bocel[1],
  legislatura_inicio = legislatura_inicio[1],
  legislatura_fim   = legislatura_fim[1],
  nome_fonte        = nome_fonte[1],
  nome_parlamentar  = nome_parlamentar[1],
  id_pessoa_bocel     = if (uniqueN(na.omit(id_pessoa_bocel))  == 1L) na.omit(id_pessoa_bocel)[1]  else NA_character_,
  id_mandato_bocel    = if (uniqueN(na.omit(id_mandato_bocel)) == 1L) na.omit(id_mandato_bocel)[1] else NA_character_,
  titular           = titular[1],
  metodo_pareamento = metodo_pareamento[1]
), by = .(uf, sg_ue, parlamentar, legislatura)]
stopifnot(uniqueN(pon[, paste(uf, sg_ue, parlamentar, legislatura)]) == nrow(pon))
cat("pessoas x legislatura apos colapsar a ponte:", nrow(pon),
    "| com mais de um registro de mandato:", pon[n_registros_sapl > 1, .N], "\n")
reg("pres_ponte_pessoa_legislatura", nrow(pon))
reg("pres_ponte_com_mandato_bocel", pon[!is.na(id_mandato_bocel), .N])

## ---------------------------------------------------------------- 3. janela de presenca
jan <- pres[, .(n_presencas = .N, primeira_presenca = min(data_sessao), ultima_presenca = max(data_sessao)),
            by = .(uf, sg_ue, parlamentar, legislatura)]
# cadencia da Casa naquela legislatura: as sessoes existentes, tenha ou nao presenca registrada
ses <- unique(pres[, .(uf, sg_ue, legislatura, sessao, data_sessao)])
cad <- ses[, .(n_sessoes_leg = .N, primeira_sessao = min(data_sessao), ultima_sessao = max(data_sessao)),
           by = .(uf, sg_ue, legislatura)]
jan <- merge(jan, cad, by = c("uf", "sg_ue", "legislatura"))
jan <- merge(jan, pon[, .(uf, sg_ue, parlamentar, legislatura, id_mandato_sapl, n_registros_sapl,
                          dominio, chave_sapl,
                          ano_eleicao_bocel, legislatura_inicio, legislatura_fim, nome_fonte,
                          nome_parlamentar, id_pessoa_bocel, id_mandato_bocel, titular,
                          data_inicio_sapl, data_fim_sapl)],
             by = c("uf", "sg_ue", "parlamentar", "legislatura"), all.x = TRUE)
# Um mandato do BOCEL nao pode ter duas janelas de presenca: se dois identificadores de parlamentar
# da mesma Casa apontam para o mesmo mandato, o cadastro tem a pessoa em duplicidade e nao ha como
# saber qual assento a presenca observa. O vinculo cai, a janela fica, e o caso vai para auditoria.
amb <- jan[!is.na(id_mandato_bocel), .N, by = id_mandato_bocel][N > 1]
reg("pres_mandatos_bocel_ambiguos", nrow(amb))
if (nrow(amb)) {
  fwrite(jan[id_mandato_bocel %in% amb$id_mandato_bocel,
             .(uf, sg_ue, dominio, legislatura, parlamentar, nome_fonte, id_mandato_bocel, n_presencas,
               primeira_presenca, ultima_presenca)][order(id_mandato_bocel)],
         "output/verificacao/sapl_presenca_mandatos_ambiguos.csv", na = "NA")
  jan[id_mandato_bocel %in% amb$id_mandato_bocel, `:=`(id_mandato_bocel = NA_character_,
                                                   id_pessoa_bocel = NA_character_)]
  cat("mandatos do BOCEL com mais de uma janela, vinculo desfeito:", nrow(amb), "\n")
}

jan[, `:=`(pp = as.IDate(primeira_presenca), up = as.IDate(ultima_presenca),
           ps = as.IDate(primeira_sessao), us = as.IDate(ultima_sessao))]
# entrada tardia e saida precoce, medidas contra a propria cadencia da Casa
jan[, dias_ate_a_primeira := as.integer(pp - ps)]
jan[, dias_da_ultima_ao_fim := as.integer(us - up)]
# Contagem por posicao, sem varrer a tabela linha a linha: a primeira presenca e sempre uma data
# de sessao do proprio grupo, entao a posicao dela na sequencia ordenada ja da quantas sessoes
# vieram antes.
setorder(ses, uf, sg_ue, legislatura, data_sessao)
ses[, idx := seq_len(.N), by = .(uf, sg_ue, legislatura)]
jan[ses, on = .(uf, sg_ue, legislatura, primeira_presenca = data_sessao),
    sessoes_antes_da_entrada := i.idx - 1L]
jan[ses, on = .(uf, sg_ue, legislatura, ultima_presenca = data_sessao),
    idx_ultima := i.idx]
jan[, sessoes_depois_da_saida := n_sessoes_leg - idx_ultima]
setorder(jan, uf, sg_ue, legislatura, primeira_presenca)
fwrite(jan[, .(uf, sg_ue, dominio, legislatura, parlamentar, nome_fonte, nome_parlamentar,
               ano_eleicao_bocel, id_pessoa_bocel, id_mandato_bocel, titular, chave_sapl,
               n_registros_sapl,
               data_inicio_sapl, data_fim_sapl, legislatura_inicio, legislatura_fim,
               n_sessoes_leg, primeira_sessao, ultima_sessao,
               n_presencas, primeira_presenca, ultima_presenca,
               dias_ate_a_primeira, dias_da_ultima_ao_fim,
               sessoes_antes_da_entrada, sessoes_depois_da_saida)],
       "data/sapl_presenca_exercicio.csv", na = "NA", quote = TRUE)
cat("janelas de presenca:", nrow(jan), "| pareadas ao BOCEL:", jan[!is.na(id_mandato_bocel), .N], "\n")
reg("pres_janelas", nrow(jan))
reg("pres_janelas_pareadas", jan[!is.na(id_mandato_bocel), .N])
reg("pres_entrada_tardia_3_sessoes", jan[sessoes_antes_da_entrada >= 3, .N])
reg("pres_saida_precoce_3_sessoes", jan[sessoes_depois_da_saida >= 3, .N])

## ---------------------------------------------------------------- 4. calibragem da folga
# Quanto a primeira presenca atrasa em relacao a data que o proprio SAPL registra como inicio do
# mandato. So entram os mandatos que comecam fora de 1 de janeiro, que sao justamente os
# interregnos ja datados pela Casa, e por isso servem de gabarito.
gab <- jan[!is.na(data_inicio_sapl) & substr(data_inicio_sapl, 6, 10) != "01-01" & !is.na(primeira_presenca)]
gab[, folga := as.integer(pp - as.IDate(data_inicio_sapl))]
if (nrow(gab) >= 30) {
  q <- quantile(abs(gab$folga), c(.25, .5, .75, .9), na.rm = TRUE)
  cat("calibragem da folga entrada (n =", nrow(gab), "): mediana", q[[2]], "dias | p75", q[[3]], "| p90", q[[4]], "\n")
  reg("pres_calibragem_n", nrow(gab))
  reg("pres_calibragem_mediana_dias", as.integer(q[[2]]))
  reg("pres_calibragem_p75_dias", as.integer(q[[3]]))
  reg("pres_calibragem_p90_dias", as.integer(q[[4]]))
  reg("pres_calibragem_ate_7_dias", gab[abs(folga) <= 7, .N])
  reg("pres_calibragem_ate_30_dias", gab[abs(folga) <= 30, .N])
} else {
  cat("gabarito insuficiente para calibrar a folga:", nrow(gab), "pares\n")
  reg("pres_calibragem_n", nrow(gab))
}

## ---------------------------------------------------------------- 5. lacuna no meio do mandato
# Afastamento visto de fora: a pessoa esta presente antes e depois, e no meio perde uma sequencia
# de sessoes. O limiar exige tanto tempo quanto numero de sessoes, para nao confundir recesso e
# falta isolada com afastamento.
MIN_SESSOES <- 3L; MIN_DIAS <- 45L
pp2 <- unique(pres[, .(uf, sg_ue, legislatura, parlamentar, data_sessao)])
pp2[ses, on = .(uf, sg_ue, legislatura, data_sessao), idx := i.idx]
setorder(pp2, uf, sg_ue, legislatura, parlamentar, idx)
pp2[, `:=`(prox = shift(data_sessao, type = "lead"), idx_prox = shift(idx, type = "lead")),
    by = .(uf, sg_ue, legislatura, parlamentar)]
lac <- pp2[!is.na(prox)]
lac[, dias := as.integer(as.IDate(prox) - as.IDate(data_sessao))]
# sessoes perdidas = quantas sessoes da Casa caem entre as duas presencas consecutivas
lac[, sessoes_perdidas := idx_prox - idx - 1L]
lac <- lac[dias >= MIN_DIAS & sessoes_perdidas >= MIN_SESSOES]
if (nrow(lac)) {
  setnames(lac, c("data_sessao", "prox"), c("ultima_presenca_antes", "primeira_presenca_depois"))
  # Quem esteve presente na janela e nao estava presente antes dela e candidato a suplente. Isso
  # equivale a dizer que a PRIMEIRA presenca dessa pessoa na legislatura cai dentro da janela, o
  # que se resolve com uma juncao por intervalo em vez de varredura por linha.
  estreia <- pp2[, .(idx_estreia = min(idx)), by = .(uf, sg_ue, legislatura, parlamentar)]
  lac[, id_lacuna := .I]
  ent <- estreia[lac[, .(id_lacuna, uf, sg_ue, legislatura, a = idx, b = idx_prox, dono = parlamentar)],
                 on = .(uf, sg_ue, legislatura), allow.cartesian = TRUE]
  ent <- ent[idx_estreia > a & idx_estreia < b & parlamentar != dono]
  agg <- ent[, .(n_entrantes = .N, entrantes_na_janela = paste(parlamentar, collapse = ";")),
             by = id_lacuna]
  lac[agg, on = "id_lacuna", `:=`(n_entrantes = i.n_entrantes,
                                  entrantes_na_janela = i.entrantes_na_janela)]
  lac[is.na(n_entrantes), `:=`(n_entrantes = 0L, entrantes_na_janela = "")]
  lac <- merge(lac, pon[, .(uf, sg_ue, parlamentar, legislatura, dominio, ano_eleicao_bocel,
                            nome_fonte, id_pessoa_bocel, id_mandato_bocel, titular)],
               by = c("uf", "sg_ue", "parlamentar", "legislatura"), all.x = TRUE)
  lac[, confianca := fcase(n_entrantes >= 1 & !is.na(id_mandato_bocel), "alta",
                           n_entrantes >= 1, "media", default = "baixa")]
  setorder(lac, uf, sg_ue, legislatura, ultima_presenca_antes)
  fwrite(lac, "data/sapl_presenca_lacunas.csv", na = "NA", quote = TRUE)
}
cat("lacunas de ao menos", MIN_SESSOES, "sessoes e", MIN_DIAS, "dias:", nrow(lac),
    "| com entrante na janela:", if (nrow(lac)) lac[n_entrantes >= 1, .N] else 0, "\n")
reg("pres_lacunas", nrow(lac))
reg("pres_lacunas_com_entrante", if (nrow(lac)) lac[n_entrantes >= 1, .N] else 0L)
reg("pres_lacunas_pareadas_bocel", if (nrow(lac)) lac[!is.na(id_mandato_bocel), .N] else 0L)
reg("pres_lacuna_min_sessoes", MIN_SESSOES); reg("pres_lacuna_min_dias", MIN_DIAS)
if (nrow(lac)) print(lac[, .N, by = confianca][order(-N)])

## ---------------------------------------------------------------- 6. confronto com o texto livre
# Checagem cruzada entre duas evidencias independentes. R/54 leu o ato que a Casa escreveu; aqui a
# presenca mostra a cadeira vazia. Onde as duas alcancam o mesmo mandato, a taxa de corroboracao
# mede as duas de uma vez.
if (file.exists("data/sapl_observacao_titular.csv") && nrow(lac)) {
  ot <- fread("data/sapl_observacao_titular.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
  ot <- ot[!is.na(data_evento)]
  cru <- merge(ot[, .(id_mandato, data_evento, tipo_evento, causa)],
               lac[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel,
                                             ultima_presenca_antes, primeira_presenca_depois)],
               by = "id_mandato", allow.cartesian = TRUE)
  if (nrow(cru)) {
    cru[, dist := pmin(abs(as.integer(as.IDate(data_evento) - as.IDate(ultima_presenca_antes))),
                       abs(as.integer(as.IDate(data_evento) - as.IDate(primeira_presenca_depois))))]
    melhor <- cru[, .(dist = min(dist)), by = .(id_mandato, data_evento, tipo_evento)]
    cat("mandatos alcancados pelas duas fontes:", nrow(melhor),
        "| ato a menos de 60 dias da lacuna:", melhor[dist <= 60, .N], "\n")
    reg("pres_cruzamento_com_texto", nrow(melhor))
    reg("pres_cruzamento_corroborado_60d", melhor[dist <= 60, .N])
    fwrite(melhor, "output/verificacao/sapl_presenca_cruza_texto.csv", na = "NA")
  } else {
    cat("nenhum mandato alcancado pelas duas fontes ainda\n"); reg("pres_cruzamento_com_texto", 0L)
  }
}
cat("55_sapl_presenca: concluido\n")
