# 10_integrar_exercicio.R — integra posse, exercicio e forma de saida (frentes 07-09, 11-14)
# em data/mandatos.csv e as flags de deduplicacao em data/pessoas.csv.
# Entrada: data/mandatos.csv, data/pessoas.csv e, quando existirem:
#   data/exercicio_camara.csv, data/exercicio_senado.csv, data/mandatos_forma_saida_suplementar.csv,
#   data/pessoas_flags_dedup.csv, data/munic_prefeitos.csv, data/wikidata_mandatos.csv,
#   data/wikidata_obitos.csv, data/exercicio_assembleias.csv, data/sinais_tse_exercicio.csv
# Saida:  data/mandatos.csv|parquet|rds e data/pessoas.csv|parquet|rds (sobrescritos, com colunas novas)
# Execucao: cd ~/bocel && Rscript --vanilla R/10_integrar_exercicio.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "data_referencia.R")); DATA_REF <- data_referencia(root)  # data fixa da versao, nao o dia da execucao (l. 444)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

ler <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
col <- function(dt, nome) if (!is.null(dt) && nome %in% names(dt)) dt[[nome]] else rep(NA_character_, if (is.null(dt)) 0 else nrow(dt))
d8  <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }

VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "perda_do_mandato_inferida_por_eleicao_suplementar",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
           "substituicao_inferida_munic", "assumiu_titular", "aposentadoria", "impeachment", "retotalizacao", "outro", "nao_observado")

mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand[, `:=`(data_posse = NA_character_, data_fim_efetiva = NA_character_, precisao_data_fim = NA_character_,
            forma_saida = "nao_observado", fonte_forma_saida = NA_character_,
            exercicio_confirmado = NA_character_, fonte_exercicio = NA_character_)]

# prioridade das fontes por mandato: registro institucional (Camara, Senado, Assembleia) >
# Wikidata > suplementar TSE > MUNIC. Aplica-se na ordem inversa para a maior prevalecer.
n_guarda_evento <- 0L
aplicar <- function(src, nome_fonte, apenas_preenche = FALSE) {
  if (is.null(src) || nrow(src) == 0) return(invisible(0L))
  src <- src[!is.na(id_mandato) & id_mandato %in% mand$id_mandato]
  # 'nao_observado' vindo da fonte (lista por legislatura sem datas) nao e forma observada:
  # nao recebe fonte nem sobrepoe forma observada por fonte de prioridade inferior
  # (verifica_integracao.R, 28/ago/2026: 24 formas do Wikidata sobrepostas e 1.315 'nao_observado' com fonte)
  src[forma %in% "nao_observado", forma := NA_character_]
  # linha da fonte cujo fim cai fora da janela do mandato (lista que junta mandatos consecutivos,
  # ou pareamento ao mandato errado) nao concorre; as demais linhas da mesma fonte seguem
  jan <- mand[match(src$id_mandato, id_mandato), .(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
  src <- src[is.na(fim) | (as.IDate(fim) >= jan$mi - 60L & as.IDate(fim) <= jan$mf + 45L)]
  if (nrow(src) == 0) return(invisible(0L))
  # uma linha por mandato: primeira posse, ultimo fim, forma de saida do ultimo periodo
  setorder(src, id_mandato, ini, fim, na.last = TRUE)
  agg <- src[, .(posse = na.omit(ini)[1] %||% NA_character_,
                 fim_ef = if (all(is.na(fim))) NA_character_ else max(fim, na.rm = TRUE),
                 fs = { f <- forma[!is.na(forma)]; if (length(f)) f[length(f)] else NA_character_ }),
             by = id_mandato]
  agg[!fs %in% VOCAB & !is.na(fs), fs := "outro"]
  idx <- match(agg$id_mandato, mand$id_mandato)
  # posse anterior a janela do mandato (statement que abrange mandatos consecutivos) nao e posse deste
  agg[!is.na(posse) & posse < as.character(as.IDate(mand$mandato_inicio[idx]) - 60L), posse := NA_character_]
  # posse depois do fim convencional e registro de outro mandato (numeracao de legislatura errada na fonte)
  agg[!is.na(posse) & posse > mand$mandato_fim[idx], posse := NA_character_]
  # executivos: posse mais de 400 dias apos o inicio convencional e de outro mandato (lista que junta mandatos)
  exec_cd <- mand$cd_cargo[idx] %in% c("1", "2", "3", "4", "11", "12")
  agg[exec_cd & !is.na(posse) & posse > as.character(as.IDate(mand$mandato_inicio[idx]) + 400L), posse := NA_character_]
  # fim muito posterior ao fim convencional pertence a um mandato seguinte: nem fim nem forma valem aqui
  tarde <- !is.na(agg$fim_ef) & agg$fim_ef > as.character(as.IDate(mand$mandato_fim[idx]) + 45L)
  agg[tarde, `:=`(fim_ef = NA_character_, fs = NA_character_)]
  # saida antecipada declarada mas fim igual ou posterior ao fim convencional: a nota da fonte fala de
  # evento de mandato posterior (ex.: reeleito que renunciou no segundo mandato); aqui e fim regular
  # fim no futuro (termino previsto, ainda nao ocorrido): mandato em curso, sem saida observada
  agg[!is.na(fim_ef) & fim_ef > as.character(DATA_REF), `:=`(fim_ef = NA_character_, fs = NA_character_)]
  # fim anterior a janela do mandato pertence a mandato anterior: nao vale aqui
  agg[!is.na(fim_ef) & fim_ef < as.character(as.IDate(mand$mandato_inicio[idx]) - 60L), `:=`(fim_ef = NA_character_, fs = NA_character_)]
  # 'fim_regular' declarado pela fonte com fim bem antes do fim convencional e contraditorio: vira 'outro'
  agg[agg$fs %in% "fim_regular" & !is.na(fim_ef) & fim_ef < as.character(as.IDate(mand$mandato_fim[idx]) - 60L), fs := "outro"]
  cedo <- agg$fs %in% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse")
  agg[cedo & !is.na(fim_ef) & fim_ef >= as.character(as.IDate(mand$mandato_fim[idx]) - 1L), fs := "fim_regular"]
  # 29/08/2026: a fonte que so lista o mandato inteiro nao apaga o evento nomeado por outra.
  # A relacao por legislatura devolve 'fim_regular' por ausencia de anotacao, ao passo que
  # renuncia, cassacao, falecimento, afastamento e licenca sao afirmacao positiva de um ato.
  # R/amostra_divergencia_fontes.R mediu a concordancia entre fontes independentes: dos 1.442
  # mandatos cobertos por mais de uma fonte, 85,4% sao unanimes, e a discordancia se concentra
  # em 'fim_regular' de um lado contra evento nomeado do outro. A guarda vale nos dois sentidos
  # da prioridade e nao dispensa a ordem das fontes, que continua governando os demais casos.
  # 'outro' fica de fora da protecao por ser residuo de regra de coerencia, e nao ato nomeado.
  EVENTO <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
  # 30/08/2026: 'outro' entrou na guarda ao lado de 'fim_regular'. Os dois sao residuo, um por
  # ausencia de anotacao e outro por saida sem ato identificado, e nenhum deve apagar o ato nomeado.
  guarda <- agg$fs %in% c("fim_regular", "outro") & mand$forma_saida[idx] %in% EVENTO
  if (any(guarda)) {
    n_guarda_evento <<- n_guarda_evento + sum(guarda)
    agg[guarda, `:=`(fs = NA_character_, fim_ef = NA_character_)]
  }
  # posse: fonte de maior prioridade prevalece quando traz forma; senao so preenche vazio
  selp <- !is.na(agg$posse) & (!is.na(agg$fs) | is.na(mand$data_posse[idx]))
  mand[idx[selp], `:=`(data_posse = agg$posse[selp])]
  # fonte em modo 'apenas preenche' nunca substitui forma ja observada: entra so na lacuna.
  # Usado pela deducao de incompatibilidade de cargo, cuja evidencia estabelece que o mandato
  # terminou antes do fim, mas nao o ato pelo qual terminou.
  sel <- !is.na(agg$fs)
  if (apenas_preenche) sel <- sel & (is.na(mand$forma_saida[idx]) | mand$forma_saida[idx] %in% "nao_observado")
  mand[idx[sel], `:=`(forma_saida = agg$fs[sel], fonte_forma_saida = nome_fonte)]
  # forma nova sem fim proprio e fim antigo incompativel (fim regular com fim bem antes do fim
  # convencional): o fim antigo pertencia a outra leitura e sai
  inc <- sel & is.na(agg$fim_ef) & agg$fs %in% "fim_regular" &
         !is.na(mand$data_fim_efetiva[idx]) & mand$data_fim_efetiva[idx] < as.character(as.IDate(mand$mandato_fim[idx]) - 60L)
  mand[idx[inc], data_fim_efetiva := NA_character_]
  # fim: so substitui o existente quando a fonte tambem traz a forma (fim e forma andam juntos)
  sel2 <- !is.na(agg$fim_ef) & (!is.na(agg$fs) | is.na(mand$data_fim_efetiva[idx]))
  # 12/09/2026: em modo 'apenas preenche' o fim segue a forma. Sem esta guarda a incompatibilidade de cargo
  # deixava a forma de outra fonte e trocava o fim dela (10 prefeitos com substituicao inferida pela MUNIC
  # ficaram com o fim da posse como deputado e a fonte ibge_munic)
  if (apenas_preenche) sel2 <- sel2 & (sel | is.na(mand$data_fim_efetiva[idx]))
  mand[idx[sel2], data_fim_efetiva := agg$fim_ef[sel2]]
  cat(sprintf("%-22s %7d mandatos\n", nome_fonte, nrow(agg)))
  invisible(nrow(agg))
}
`%||%` <- function(a, b) if (length(a) == 0 || is.na(a[1])) b else a[1]

## ---- MUNIC (menor prioridade): outro em exercicio => substituicao inferida.
# So nas edicoes com NOME do prefeito (2004 e 2005): por sexo+idade a precisao medida
# na calibracao de 2005 foi 0,50, insuficiente para afirmar a substituicao.
mu <- ler("data/munic_prefeitos.csv")
if (!is.null(mu) && "status" %in% names(mu)) {
  tem_nome <- !is.na(col(mu, "nome_prefeito_munic")) & col(mu, "nome_prefeito_munic") != ""
  # data de referencia da MUNIC: data_referencia vem como "2004 (ano da entrevista)" ou "2013-03/2013-11";
  # a data usada e 31/12 do ano da edicao (ano_munic), a mesma referencia em que R/11 calcula a idade.
  # (correcao 28/08/2026: a regex anterior exigia "^\\d{4}$" e nunca casava, deixando data_fim_efetiva
  # e exercicio_confirmado NA em todos os mandatos preenchidos pela MUNIC.)
  dt_munic <- function(ano) fifelse(grepl("^\\d{4}$", ano), paste0(ano, "-12-31"), NA_character_)
  s <- mu[status == "outro_em_exercicio" & tem_nome & !is.na(col(mu, "id_mandato_bocel")),
          .(id_mandato = id_mandato_bocel, ini = NA_character_,
            fim = dt_munic(ano_munic),
            forma = "substituicao_inferida_munic")]
  aplicar(s, "ibge_munic")
  ok <- mu[status == "eleito_em_exercicio" & !is.na(col(mu, "id_mandato_bocel")),
           .(id_mandato = id_mandato_bocel, dt = dt_munic(ano_munic))]
  ok <- ok[, .(dt = max(dt, na.rm = TRUE)), by = id_mandato]
  i <- match(ok$id_mandato, mand$id_mandato)
  mand[i[!is.na(i)], `:=`(exercicio_confirmado = ok$dt[!is.na(i)], fonte_exercicio = "ibge_munic")]
}

## ---- sinais TSE (reeleicao): confirma exercicio, nao define saida
st <- ler("data/sinais_tse_exercicio.csv")
if (!is.null(st) && "exercicio_confirmado_em" %in% names(st)) {
  s <- st[!is.na(exercicio_confirmado_em) & id_mandato %in% mand$id_mandato]
  i <- match(s$id_mandato, mand$id_mandato)
  mand[i, `:=`(exercicio_confirmado = fifelse(is.na(exercicio_confirmado) | s$exercicio_confirmado_em > exercicio_confirmado,
                                              s$exercicio_confirmado_em, exercicio_confirmado),
               fonte_exercicio = fifelse(is.na(fonte_exercicio), "tse_reeleicao", paste(fonte_exercicio, "tse_reeleicao", sep = ";")))]
  cat(sprintf("%-22s %7d mandatos com exercicio confirmado\n", "tse_reeleicao", nrow(s)))
}

## ---- lista de presenca em plenario (R/55): confirma exercicio com data, nao define saida
# A prova mais direta de exercicio que o banco tem: o secretario da Casa registrou a pessoa
# presente na sessao daquele dia. Entra como exercicio confirmado na data da ultima presenca, que
# e a ultima vez em que a pessoa foi vista na cadeira. Nao toca em forma de saida, porque ausencia
# em plenario nao diz por que a cadeira esvaziou.
pz <- ler("data/sapl_presenca_exercicio.csv")
if (!is.null(pz) && "id_mandato_bocel" %in% names(pz)) {
  s <- pz[!is.na(id_mandato_bocel) & !is.na(ultima_presenca),
          .(id_mandato = id_mandato_bocel, dt = d8(ultima_presenca))]
  s <- s[!is.na(dt) & id_mandato %in% mand$id_mandato]
  # a presenca nao pode confirmar exercicio fora da janela do proprio mandato
  jn <- mand[match(s$id_mandato, id_mandato), .(mi = mandato_inicio, mf = mandato_fim)]
  s <- s[dt >= as.character(as.IDate(jn$mi) - 60L) & dt <= as.character(as.IDate(jn$mf) + 45L)]
  s <- s[, .(dt = max(dt)), by = id_mandato]
  i <- match(s$id_mandato, mand$id_mandato)
  mand[i, `:=`(exercicio_confirmado = fifelse(is.na(exercicio_confirmado) | s$dt > exercicio_confirmado,
                                              s$dt, exercicio_confirmado),
               fonte_exercicio = fifelse(is.na(fonte_exercicio), "sapl_presenca",
                                         paste(fonte_exercicio, "sapl_presenca", sep = ";")))]
  cat(sprintf("%-22s %7d mandatos com exercicio confirmado\n", "sapl_presenca", nrow(s)))
}

## ---- TCE-AC (R/57): o acordao de contas anuais confirma exercicio, nao define posse nem saida
# 12/09/2026. O Tribunal de Contas do Acre julga a prestacao de contas anual de cada
# prefeitura e camara, e o acordao nomeia quem respondeu pelo exercicio (o prefeito, ou o presidente da
# camara, que e vereador). A fonte nao traz posse, saida nem causa, e as datas de tce_gestores_e.csv
# delimitam o periodo julgado, que e o ano do exercicio quando o extrato nao declara periodo proprio. Entra
# como exercicio confirmado na data de inicio desse periodo, dentro da janela do mandato. A checagem C3 de
# R/verifica_verifica_tce_existentes.R, que veda tribunal de contas como confirmacao de exercicio, passou a
# valer para os grupos A e B (folha de pagamento, fonte 'tce').
te <- ler("data/tce_gestores_e.csv")
if (!is.null(te) && "id_mandato_bocel" %in% names(te)) {
  s <- te[!is.na(id_mandato_bocel) & !is.na(data_inicio), .(id_mandato = id_mandato_bocel, dt = d8(data_inicio))]
  s <- s[!is.na(dt) & id_mandato %in% mand$id_mandato]
  jn <- mand[match(s$id_mandato, id_mandato), .(mi = mandato_inicio, mf = mandato_fim)]
  s <- s[dt >= as.character(as.IDate(jn$mi) - 60L) & dt <= as.character(as.IDate(jn$mf) + 45L)]
  s <- s[, .(dt = max(dt)), by = id_mandato]
  i <- match(s$id_mandato, mand$id_mandato)
  mand[i, `:=`(exercicio_confirmado = fifelse(is.na(exercicio_confirmado) | s$dt > exercicio_confirmado,
                                              s$dt, exercicio_confirmado),
               fonte_exercicio = fifelse(is.na(fonte_exercicio), "tce_ac",
                                         paste(fonte_exercicio, "tce_ac", sep = ";")))]
  cat(sprintf("%-22s %7d mandatos com exercicio confirmado\n", "tce_ac", nrow(s)))
  registrar_numero("bocel_mandatos_exercicio_confirmado_tce_ac", nrow(s), script = "R/10_integrar_exercicio.R")
}

## ---- MUNIC ampliada (R/49): confirma exercicio do prefeito eleito, nao define saida
# R/49 mediu a regra multiatributo contra 2005, a unica edicao com nome e perfil ao mesmo tempo:
# confirmar o ELEITO em exercicio tem precisao de 0,985. Afirmar que OUTRA pessoa estava no cargo
# tem precisao de 0,498 pelo sexo e idade, e por isso a substituicao nao entra aqui; fica na
# pendencia de julgamento. So o lado confirmatorio da regra e usado, na data de referencia da
# edicao, que e a data em que o IBGE observou a prefeitura.
ma <- ler("data/munic_exercicio_ampliado.csv")
if (!is.null(ma) && "afirmacao" %in% names(ma)) {
  # data_referencia da MUNIC e texto livre ("2004 (ano da entrevista)", "2013-03/2013-11"); a data
  # usada e 31/12 do ano da edicao, a mesma convencao ja adotada por R/11 acima.
  s <- ma[afirmacao %in% c("exercicio_do_eleito_confirmado_por_nome",
                           "exercicio_do_eleito_confirmado_por_atributos") &
            !is.na(id_mandato_bocel) & grepl("^\\d{4}$", ano_munic),
          .(id_mandato = id_mandato_bocel, dt = paste0(ano_munic, "-12-31"))]
  s <- s[!is.na(dt) & id_mandato %in% mand$id_mandato]
  jn <- mand[match(s$id_mandato, id_mandato), .(mi = mandato_inicio, mf = mandato_fim)]
  s <- s[dt >= as.character(as.IDate(jn$mi) - 60L) & dt <= as.character(as.IDate(jn$mf) + 45L)]
  s <- s[, .(dt = max(dt)), by = id_mandato]
  i <- match(s$id_mandato, mand$id_mandato)
  mand[i, `:=`(exercicio_confirmado = fifelse(is.na(exercicio_confirmado) | s$dt > exercicio_confirmado,
                                              s$dt, exercicio_confirmado),
               fonte_exercicio = fifelse(is.na(fonte_exercicio), "ibge_munic_ampliado",
                                         paste(fonte_exercicio, "ibge_munic_ampliado", sep = ";")))]
  cat(sprintf("%-22s %7d mandatos com exercicio confirmado\n", "ibge_munic_ampliado", nrow(s)))
}

## ---- suplementares TSE
su <- ler("data/mandatos_forma_saida_suplementar.csv")
if (!is.null(su)) {
  # momento 'antes_da_posse' (pleito novo antes de 1o de janeiro): o eleito ordinario nao
  # tomou posse; 'durante_o_mandato': perda do mandato inferida
  s <- su[, .(id_mandato = col(su, "id_mandato_ordinario_afetado"),
              ini = NA_character_, fim = d8(col(su, "data_fim_inferida")),
              forma = fifelse(col(su, "momento") %in% "antes_da_posse", "nao_tomou_posse",
                              "perda_do_mandato_inferida_por_eleicao_suplementar"))]
  aplicar(s, "tse_suplementar")
  # sucessor pela via suplementar
  if ("sucessor_via_suplementar" %in% names(su)) {
    k <- su[!is.na(sucessor_via_suplementar)]
    i <- match(k[[if ("id_mandato_ordinario_afetado" %in% names(k)) "id_mandato_ordinario_afetado" else "id_mandato"]], mand$id_mandato)
    mand[i[!is.na(i)], `:=`(sucessor_id = k$sucessor_via_suplementar[!is.na(i)], via_sucessao = "eleicao_suplementar")]
  }
}

## ---- CNPJ da Receita (responsavel legal da prefeitura, mensal 2021+): nome bate => exercicio
## confirmado; nome diferente do eleito => substituicao (mesma logica da MUNIC com nome)
cn <- ler("data/cnpj_responsavel_prefeitura.csv")
if (!is.null(cn) && "status" %in% names(cn)) {
  s <- cn[status == "outro_em_exercicio" & !is.na(col(cn, "id_mandato_bocel")),
          .(id_mandato = id_mandato_bocel, ini = NA_character_, fim = d8(col(.SD, "data_referencia")),
            forma = "substituicao_inferida_munic")]
  s <- s[, .SD[which.min(fim)], by = id_mandato]
  aplicar(s, "receita_cnpj")
  ok <- cn[status == "eleito_em_exercicio" & !is.na(col(cn, "id_mandato_bocel")),
           .(id_mandato = id_mandato_bocel, dt = d8(col(.SD, "data_referencia")))][!is.na(dt), .(dt = max(dt)), by = id_mandato]
  i <- match(ok$id_mandato, mand$id_mandato)
  mand[i[!is.na(i)], `:=`(exercicio_confirmado = fifelse(is.na(exercicio_confirmado) | ok$dt[!is.na(i)] > exercicio_confirmado, ok$dt[!is.na(i)], exercicio_confirmado),
                          fonte_exercicio = fifelse(is.na(fonte_exercicio), "receita_cnpj", paste(fonte_exercicio, "receita_cnpj", sep = ";")))]
  cat(sprintf("%-22s %7d mandatos com exercicio confirmado\n", "receita_cnpj", nrow(ok)))
}

## ---- DataJud (processos de cassacao pareados a mandatos): cassacao quando ha indicio julgado
dj <- ler("data/datajud_mandatos_afetados.csv")
if (!is.null(dj) && "indicio" %in% names(dj)) {
  s <- dj[toupper(indicio) %in% c("TRUE", "T", "1") & !is.na(id_mandato),
          .(id_mandato, ini = NA_character_, fim = d8(col(.SD, "data")), forma = "cassacao")]
  aplicar(s, "datajud")
}

## ---- Wikipedia (listas de prefeitos; governadores, vices e deputados estaduais)
wp <- ler("data/wikipedia_prefeitos.csv")
if (!is.null(wp)) {
  # vice em exercicio: a data em que assumiu a prefeitura nao e a posse do mandato de vice
  # (verifica_wikipedia_prefeitos.R, 28/ago/2026)
  s <- wp[, .(id_mandato = col(wp, "id_mandato_bocel"),
              ini = fifelse(col(wp, "condicao") %in% "vice_em_exercicio", NA_character_, d8(col(wp, "inicio"))),
              fim = d8(col(wp, "fim")), forma = col(wp, "forma_saida"))]
  aplicar(s[!is.na(forma) | !is.na(ini)], "wikipedia")
}
we <- ler("data/wikipedia_estadual.csv")
if (!is.null(we)) {
  s <- we[, .(id_mandato = col(we, "id_mandato_bocel"), ini = d8(col(we, "inicio")), fim = d8(col(we, "fim")),
              forma = col(we, "forma_saida"))]
  aplicar(s[!is.na(forma) | !is.na(ini)], "wikipedia")
}

## ---- Diarios oficiais (Querido Diario): evento de saida de confianca alta pareado ao mandato
do_ <- ler("data/diarios_mandatos_saida.csv")
if (!is.null(do_)) {
  s <- do_[, .(id_mandato = fcoalesce(col(do_, "id_mandato_bocel"), col(do_, "id_mandato")), ini = NA_character_, fim = d8(col(do_, "data_fim_inferida")),
               forma = col(do_, "forma_saida"))]
  aplicar(s, "diario_oficial")
}

## ---- Wikidata (mandatos e obitos)
wd <- ler("data/wikidata_mandatos.csv")
if (!is.null(wd)) {
  s <- wd[, .(id_mandato = col(wd, "id_mandato_bocel"), ini = d8(col(wd, "inicio")), fim = d8(col(wd, "fim")),
              forma = col(wd, "forma_saida"))]
  aplicar(s[!is.na(forma) | !is.na(ini)], "wikidata")
}
ob <- ler("data/wikidata_obitos.csv")
if (!is.null(ob) && "dentro_do_mandato" %in% names(ob)) {
  s <- ob[toupper(dentro_do_mandato) %in% c("TRUE", "T", "1"), .(id_mandato = col(.SD, "id_mandato"),
          ini = NA_character_, fim = d8(col(.SD, "data_morte")), forma = "falecimento")]
  aplicar(s, "wikidata_obito")
}

## ---- Assembleias
as <- ler("data/exercicio_assembleias.csv")
if (!is.null(as)) {
  s <- as[, .(id_mandato = col(as, "id_mandato_bocel"), ini = d8(col(as, "data_inicio_exercicio")),
              fim = d8(col(as, "data_fim_exercicio")), forma = col(as, "forma_saida"))]
  aplicar(s, "assembleia_api")
}

# 29/08/2026: o bloco do TCE desceu de prioridade e passou a ser aplicado ANTES das camaras
# e assembleias. A forma de saida do grupo B vem da folha de pagamento agregada por mes de
# competencia, que e inferencia, enquanto a composicao publicada pela propria casa e registro
# direto de quem ocupou a cadeira. Na ordem anterior o TCE prevalecia nos 959 mandatos em que
# as duas fontes concorrem e convertia em fim_regular 1 cassacao, 6 licencas e 4 falecimentos
# que o SAPL registrava.
## ---- Tribunais de Contas: cadastros de gestores (prefeitos, presidentes de camara), registro institucional
# tce_gestores_e.csv (TCE-AC) nao entra neste laco: as datas dele delimitam o exercicio julgado, e
# aqui virariam posse e fim do mandato
for (ftce in c("data/tce_gestores.csv", "data/tce_gestores_b.csv", "data/tce_gestores_c.csv", "data/tce_gestores_d.csv")) {
  tc <- ler(ftce)
  if (!is.null(tc)) {
    s <- tc[, .(id_mandato = col(tc, "id_mandato_bocel"), ini = d8(col(tc, "data_inicio")), fim = d8(col(tc, "data_fim")),
                forma = col(tc, "forma_saida"))]
    aplicar(s, "tce")
  }
}

## ---- Camaras municipais com SAPL (registro institucional, vereadores)
cm <- ler("data/exercicio_camaras_municipais.csv")
if (!is.null(cm)) {
  s <- cm[, .(id_mandato = col(cm, "id_mandato_bocel"), ini = d8(col(cm, "data_inicio_mandato")),
              fim = d8(col(cm, "data_fim_mandato")), forma = col(cm, "forma_saida"))]
  aplicar(s, "sapl_municipal")
}

## ---- SAPL municipal, campo de texto livre 'observacao' lido por R/54 (04/09/2026)
# A mesma Casa, a mesma linha de cadastro, evidencia mais especifica: onde o campo tipificado diz
# apenas que houve afastamento, o texto narra o ato e a data. Por isso entra depois de
# 'sapl_municipal' e prevalece sobre ele. So o fim de mandato vem para ca; o interregno com
# retorno nao e forma de saida, por decisao registrada na frente do interregno, e vai para
# data/interregnos.csv. O afastamento sem retorno observado tambem fica de fora, porque a Casa
# pode simplesmente nao ter escrito a volta.
so <- ler("data/sapl_observacao_titular.csv")
if (!is.null(so)) {
  fim_m <- so[col(so, "tipo_evento") == "fim_de_mandato"]
  if (nrow(fim_m)) {
    s <- fim_m[, .(id_mandato = col(fim_m, "id_mandato"), ini = NA_character_,
                   fim = d8(col(fim_m, "data_evento")), forma = col(fim_m, "forma_saida_obs"))]
    aplicar(s, "sapl_observacao")
  }
}

## ---- Camaras municipais sem SAPL (portais com outros sistemas; registro institucional)
cs <- ler("data/exercicio_camaras_sem_sapl.csv")
if (!is.null(cs)) {
  s <- cs[, .(id_mandato = col(cs, "id_mandato_bocel"), ini = d8(col(cs, "data_inicio_mandato")),
              fim = d8(col(cs, "data_fim_mandato")), forma = col(cs, "forma_saida"))]
  aplicar(s, "portal_camara")
}
## ---- Assembleias: fontes historicas (PDFs, memoriais) das casas sem historico no portal
ah <- ler("data/exercicio_assembleias_historico.csv")
if (!is.null(ah)) {
  s <- ah[, .(id_mandato = col(ah, "id_mandato_bocel"), ini = d8(col(ah, "data_inicio_exercicio")),
              fim = d8(col(ah, "data_fim_exercicio")), forma = col(ah, "forma_saida"))]
  aplicar(s, "assembleia_historico")
}

## ---- Assembleias: coleta por portal da propria casa (17 UFs sem registro proprio, 29/08/2026)
ap <- ler("data/exercicio_assembleias_2.csv")
if (!is.null(ap)) {
  s <- ap[, .(id_mandato = col(ap, "id_mandato_bocel"), ini = d8(col(ap, "data_inicio_exercicio")),
              fim = d8(col(ap, "data_fim_exercicio")), forma = col(ap, "forma_saida"))]
  aplicar(s, "assembleia_portal")
}

## ---- Assembleias: inventario de fontes administrativas (R/50), so onde ha data
# Folha de pagamento, frequencia e portal de transparencia das casas estaduais. A fonte diz que a
# pessoa estava em exercicio, e nas 271 linhas em que ha data de inicio ou de fim isso vira posse e
# encerramento; onde nao ha data, a linha nao entra, porque condicao sem data nao data nada.
ai <- ler("data/exercicio_assembleias_inventario.csv")
if (!is.null(ai)) {
  s <- ai[!is.na(id_mandato_bocel) &
            (!is.na(data_inicio_exercicio) | !is.na(data_fim_exercicio)),
          .(id_mandato = id_mandato_bocel, ini = d8(data_inicio_exercicio),
            fim = d8(data_fim_exercicio), forma = forma_saida)]
  if (nrow(s)) aplicar(s, "assembleia_inventario")
}

## ---- Camaras sem SAPL, segunda rodada de coletores (mesma fonte 'portal_camara')
cs2 <- ler("data/exercicio_camaras_sem_sapl_2.csv")
if (!is.null(cs2)) {
  s <- cs2[, .(id_mandato = col(cs2, "id_mandato_bocel"), ini = d8(col(cs2, "data_inicio_mandato")),
               fim = d8(col(cs2, "data_fim_mandato")), forma = col(cs2, "forma_saida"))]
  aplicar(s, "portal_camara")
}
## ---- Senado e Camara (maior prioridade)
# 13/09/2026: a forma de saida de senador e deputado federal vem do historico completo de exercicio lido pelo R/58, e
# essa leitura e autoritativa. O mandato com historico da casa recebe forma, data de posse e fim do R/58, inclusive
# o mandato em curso, que fica sem forma mesmo que fonte de prioridade menor traga licenca ou afastamento, porque
# licenca e afastamento temporario nao encerram mandato (pendencia 1). Nenhuma etapa posterior mexe nesses mandatos.
autoritativos <- character()
slf <- ler("data/saida_legislativo_federal.csv")
if (!is.null(slf)) {
  hist <- slf[col(slf, "cobertura") %in% c("historico_da_casa", "biografia_oficial_camara", "fonte_oficial_curada")]
  hist <- hist[col(hist, "id_mandato") %in% mand$id_mandato]
  idx <- match(col(hist, "id_mandato"), mand$id_mandato)
  fs58 <- col(hist, "forma_saida")
  mand[idx, `:=`(forma_saida = fifelse(is.na(fs58), "nao_observado", fs58),
                 fonte_forma_saida = fifelse(is.na(fs58), NA_character_, col(hist, "fonte")),
                 data_fim_efetiva = col(hist, "data_fim_efetiva"),
                 data_posse = fifelse(is.na(col(hist, "data_posse")), data_posse, col(hist, "data_posse")),
                 precisao_data_fim = col(hist, "precisao_data_fim"))]
  autoritativos <- col(hist, "id_mandato")
  cat(sprintf("%-22s %7d mandatos (autoritativo)\n", "legislativo_federal", length(autoritativos)))
}
# 13/09/2026: presidente, vice-presidente, governador e vice-governador pela saida dos executivos (R/60), com evento curado
# de fonte oficial para toda saida antecipada e a cadeia das listas para o mandato cumprido. O mandato sem confirmacao
# fica sem forma, e a presuncao de fim regular do universo comparavel nao alcanca cargo executivo.
sexe <- ler("data/saida_executivos.csv")
if (!is.null(sexe)) {
  sexe <- sexe[col(sexe, "id_mandato") %in% mand$id_mandato]
  idx <- match(col(sexe, "id_mandato"), mand$id_mandato)
  fe <- col(sexe, "forma_saida")
  mand[idx, `:=`(forma_saida = fifelse(is.na(fe), "nao_observado", fe),
                 fonte_forma_saida = fifelse(is.na(fe), NA_character_, col(sexe, "fonte")),
                 data_fim_efetiva = col(sexe, "data_fim_efetiva"),
                 precisao_data_fim = col(sexe, "precisao_data_fim"))]
  autoritativos <- c(autoritativos, col(sexe, "id_mandato"))
  cat(sprintf("%-22s %7d mandatos (autoritativo)\n", "executivos", nrow(sexe)))
}
# 13/09/2026: deputado estadual e distrital pela curadoria com fonte oficial (R/61), autoritativa nos mandatos que ela
# resolve, com ato definitivo nao revertido ou prova de fim regular pela composicao final da casa. O fim regular curado
# ja chega sem os mandatos em que a propria casa nomeia ato definitivo, e o ato curado prevalece sobre licenca ou
# afastamento que a fonte da casa tratava como saida (pendencia 1). Os demais mandatos seguem com as fontes acima.
sas <- ler("data/saida_assembleias.csv")
if (!is.null(sas) && nrow(sas)) {
  sas <- sas[col(sas, "id_mandato") %in% mand$id_mandato]
  idx <- match(col(sas, "id_mandato"), mand$id_mandato)
  mand[idx, `:=`(forma_saida = col(sas, "forma_saida"), fonte_forma_saida = "fonte_oficial_curada",
                 data_fim_efetiva = col(sas, "data_fim_efetiva"), precisao_data_fim = col(sas, "precisao_data_fim"))]
  autoritativos <- c(autoritativos, col(sas, "id_mandato"))
  cat(sprintf("%-22s %7d mandatos (autoritativo)\n", "assembleias_curadas", nrow(sas)))
}

## ---- Incompatibilidade de cargo, deduzida do proprio banco (R/39). Entra por ultimo e APENAS
## ---- na lacuna: estabelece que o mandato terminou antes do fim, nao o ato pelo qual terminou.
inc <- ler("data/saida_cargo_incompativel.csv")
if (!is.null(inc)) {
  s <- inc[, .(id_mandato = col(inc, "id_mandato"), ini = NA_character_,
               fim = d8(col(inc, "data_fim_inferida")), forma = col(inc, "forma_saida"))]
  aplicar(s[!id_mandato %in% autoritativos], "cargo_incompativel", apenas_preenche = TRUE)
}

## ---- vices: quando o titular da chapa sai antes do fim (renuncia, morte, cassacao, perda do mandato,
## substituicao observada), o vice assume a titularidade; a saida do vice fica 'assumiu_titular',
## com a data de saida do titular, fonte 'derivado_titular'. So se o vice nao tem saida propria observada.
vice_de <- c(`2` = "1", `4` = "3", `12` = "11")
tit <- mand[cd_cargo %in% c("1", "3", "11") & forma_saida %in% c("renuncia", "falecimento", "cassacao", "afastamento",
                                                                 "perda_do_mandato_inferida_por_eleicao_suplementar", "substituicao_inferida_munic"),
            .(ano_eleicao, unidade_posicao, cd_tit = cd_cargo, saida_tit = data_fim_efetiva, forma_tit = forma_saida)]
tit <- tit[, .SD[1], by = .(ano_eleicao, unidade_posicao, cd_tit)]
mand[, cd_tit := vice_de[cd_cargo]]
mand <- merge(mand, tit, by = c("ano_eleicao", "unidade_posicao", "cd_tit"), all.x = TRUE, sort = FALSE)
sel_v <- !is.na(mand$forma_tit) & mand$forma_saida == "nao_observado" & !mand$id_mandato %in% autoritativos
mand[sel_v, `:=`(forma_saida = "assumiu_titular", fonte_forma_saida = "derivado_titular",
                 data_fim_efetiva = fifelse(is.na(data_fim_efetiva), saida_tit, data_fim_efetiva))]
n_vice_assumiu <- sum(sel_v)
mand[, c("cd_tit", "saida_tit", "forma_tit") := NULL]
cat(sprintf("%-22s %7d vices com titularidade assumida\n", "derivado_titular", n_vice_assumiu))

# fim anterior a posse e inconsistencia da fonte: o fim fica desconhecido
mand[!is.na(data_posse) & !is.na(data_fim_efetiva) & data_fim_efetiva < data_posse, data_fim_efetiva := NA_character_]
# coerencia final: forma fim_regular vinda de registro institucional com fim precoce herdado de
# outra fonte (que so trouxe data) fica sem fim; a data pertencia a outra leitura do mandato
mand[forma_saida == "fim_regular" & !is.na(data_fim_efetiva) &
     data_fim_efetiva < as.character(as.IDate(mandato_fim) - 60L), data_fim_efetiva := NA_character_]
# fim efetivo igual ou posterior ao fim convencional nao e saida antecipada
mand[!is.na(data_fim_efetiva) & data_fim_efetiva >= mandato_fim & forma_saida %in% c("outro", "nao_observado"),
     `:=`(forma_saida = "fim_regular",
          fonte_forma_saida = fifelse(is.na(fonte_forma_saida), "data_fim_efetiva", fonte_forma_saida))]
stopifnot(all(mand$forma_saida %in% VOCAB))

## ---- flags de deduplicacao em pessoas
# 21/09/2026 (pendencia 11, opcao b): dedup_ponte_nome_nascimento tem de marcar quem so tem nome e
# nascimento como chave de identidade em toda candidatura (nunca titulo, nunca CPF), a coluna
# so_nome_nascimento de R/09_auditoria_homonimos.R. ponte_nome_nascimento e outra coisa, um subconjunto
# de pessoas com contradicao interna (titulo/CPF/genero divergente) que o nome+nascimento uniu apesar
# de titulo e CPF existirem; ela ja fica registrada, sem afrouxar nada, em dedup_auditoria.
fl <- ler("data/pessoas_flags_dedup.csv")
pess[, `:=`(dedup_ponte_nome_nascimento = NA_character_, dedup_auditoria = NA_character_)]
if (!is.null(fl)) {
  i <- match(fl$id_pessoa, pess$id_pessoa)
  pess[i[!is.na(i)], `:=`(dedup_ponte_nome_nascimento = col(fl, "so_nome_nascimento")[!is.na(i)],
                          dedup_auditoria = col(fl, "classificacao")[!is.na(i)])]
}
pess[is.na(dedup_ponte_nome_nascimento), dedup_ponte_nome_nascimento := "FALSE"]

## ---- salvar
salvar <- function(dt, nome) {
  fwrite(dt, file.path("data", paste0(nome, ".csv")), na = "NA", quote = TRUE)
  write_parquet(dt, file.path("data", paste0(nome, ".parquet")))
  saveRDS(dt, file.path("data", paste0(nome, ".rds")))
}
salvar(mand, "mandatos"); salvar(pess, "pessoas")

fs <- mand[, .N, by = .(esfera, forma_saida)][order(esfera, -N)]
fwrite(fs, "output/verificacao/forma_saida_por_esfera.csv")
print(fs)
script <- "R/10_integrar_exercicio.R"
for (i in seq_len(nrow(fs))) registrar_numero(sprintf("bocel_forma_saida_%s_%s", fs$esfera[i], fs$forma_saida[i]), fs$N[i], script = script)
registrar_numero("bocel_mandatos_com_data_posse", sum(!is.na(mand$data_posse)), script = script)
registrar_numero("bocel_mandatos_com_exercicio_confirmado", sum(!is.na(mand$exercicio_confirmado)), script = script)
registrar_numero("bocel_mandatos_forma_saida_observada", sum(mand$forma_saida != "nao_observado"), script = script)
registrar_numero("int_evento_protegido_de_fim_regular", n_guarda_evento)
cat("10_integrar_exercicio: concluido\n")
