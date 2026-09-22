# amostra_sapl_observacao_precisao.R — sorteia a amostra de leitura manual da frente do texto livre
# do SAPL (R/54_sapl_observacao.R) e, quando a amostra ja esta julgada, mede a precisao por estrato.
#
# Por que a amostra. A verificacao mecanica (R/verifica_sapl_observacao.R) cobre coerencia interna
# e concordancia com o campo tipificado da Casa, e nao alcanca a pergunta que decide a frente: o
# trecho de fato diz o que a regra leu. Isso e leitura de fonte, e sai da amostra julgada a mao.
#
# Desenho. 30 eventos de confianca alta ou media com data, estratificados por tipo de evento, mais
# 10 de confianca baixa, mais 10 do arquivo do que a regra NAO classificou, que mede o falso
# negativo. Julgar preenchendo a coluna `julgamento` com certo, errado ou parcial, e `nota` com o
# que estiver errado.
#
# Fase 1 (sorteio). So roda quando output/verificacao/sapl_observacao_amostra_50.csv nao existe, ou
# quando a variavel de ambiente BOCEL_RESORTEAR_AMOSTRA=1 e dada; do contrario o arquivo julgado seria
# regravado e o julgamento perdido (05/09/2026).
#
# Fase 2 (precisao, 05/09/2026). Le a amostra e usa, linha a linha, `julgamento` (do autor) quando
# preenchido e `julgamento_automatico` quando `julgamento` esta vazio. As colunas
# `julgamento_automatico` e `justificativa` foram preenchidas em 05/09/2026 por leitura do texto
# original da Casa, com o criterio declarado:
#   certo   = papel, causa, tipo_evento e data_evento sustentados pelo texto; para nao_classificada,
#             texto sem evento de exercicio;
#   parcial = papel e tipo sustentados, mas causa imprecisa dentro da mesma forma de saida
#             (licenca_*, afastamento_*) ou data_evento que nao e a do evento lido; para as linhas
#             de confianca baixa sem papel, data certa com papel ou causa nao lidos; para
#             nao_classificada, substituicao narrada sem data nem causa (fora do alcance da regra);
#   errado  = sem evento no texto, ou papel, tipo ou forma de saida contrariados pelo texto; para
#             nao_classificada, evento com causa ou data que a regra deveria ter lido.
# As chaves registradas trazem o sufixo `_automatico` enquanto qualquer linha usada vier de
# `julgamento_automatico`; quando o autor preencher `julgamento` nas 51 linhas, o sufixo cai.
#
# Entrada: data/sapl_observacao_eventos.csv, output/verificacao/sapl_observacao_nao_lidas.csv,
#          output/verificacao/sapl_observacao_amostra_50.csv (fase 2)
# Saida:   output/verificacao/sapl_observacao_amostra_50.csv (fase 1),
#          output/verificacao/sapl_observacao_precisao_estrato.csv e relatorio JSON (fase 2)
# Execucao: Rscript --vanilla R/amostra_sapl_observacao_precisao.R  (a partir da raiz do repositorio)
set.seed(20260903)
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/amostra_sapl_observacao_precisao.R"
F_AM <- "output/verificacao/sapl_observacao_amostra_50.csv"

## ---------------------------------------------------------------- fase 1: sorteio
if (!file.exists(F_AM) || Sys.getenv("BOCEL_RESORTEAR_AMOSTRA") == "1") {
  ev  <- fread("data/sapl_observacao_eventos.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
  nao <- fread("output/verificacao/sapl_observacao_nao_lidas.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")

  # 30 com data, estratificados por tipo de evento na proporcao do universo, minimo 5 por estrato
  alvo <- ev[!is.na(data_evento) & confianca %in% c("alta", "media")]
  prop <- alvo[, .N, by = tipo_evento][, n_sortear := pmax(5L, round(30 * N / sum(N)))]
  a1 <- rbindlist(lapply(seq_len(nrow(prop)), function(i) {
    d <- alvo[tipo_evento == prop$tipo_evento[i]]
    d[sample(.N, min(.N, prop$n_sortear[i]))]
  }))
  a1[, estrato := paste0("com_data_", tipo_evento)]

  a2 <- ev[confianca == "baixa"][sample(.N, 10)][, estrato := "confianca_baixa"]
  a3 <- nao[sample(.N, 10)][, estrato := "nao_classificada"]
  a3[, `:=`(papel = NA_character_, causa = NA_character_, tipo_evento = NA_character_,
            data_evento = NA_character_, data_retorno = NA_character_, titular_nome = NA_character_,
            id_mandato_titular = NA_character_, confianca = "nula", nome_fonte = nome_fonte,
            ano_eleicao = ano_eleicao_bocel)]

  COLS <- c("estrato","uf","sg_ue","ano_eleicao","nome_fonte","papel","causa","tipo_evento",
            "data_evento","data_retorno","titular_nome","id_mandato_titular","confianca",
            "url","observacao")
  am <- rbindlist(list(a1[, ..COLS], a2[, ..COLS], a3[, ..COLS]), use.names = TRUE, fill = TRUE)
  am[, idx := .I]
  am[, `:=`(julgamento = "", nota = "", julgamento_automatico = "", justificativa = "")]
  setcolorder(am, c("idx", "julgamento", "nota", "julgamento_automatico", "justificativa", COLS))
  fwrite(am, F_AM, quote = TRUE, na = "NA")
  cat("amostra sorteada:", nrow(am), "linhas\n")
  print(am[, .N, by = estrato])
  registrar_numero("aobs_amostra_total", nrow(am), script = script)
  for (e in unique(am$estrato)) registrar_numero(paste0("aobs_amostra_", e), am[estrato == e, .N], script = script)
  cat("julgar a mao em", F_AM, "\n")
} else cat("amostra ja sorteada em", F_AM, "(BOCEL_RESORTEAR_AMOSTRA=1 para sortear de novo)\n")

## ---------------------------------------------------------------- fase 2: precisao por estrato
am <- fread(F_AM, colClasses = "character", na.strings = "NA", encoding = "UTF-8")
if (!"julgamento_automatico" %in% names(am)) am[, julgamento_automatico := ""]
am[, `:=`(julgamento = fcoalesce(julgamento, ""), julgamento_automatico = fcoalesce(julgamento_automatico, ""))]
am[, fonte_julgamento := fcase(julgamento != "", "autor", julgamento_automatico != "", "automatico", default = "sem_julgamento")]
am[, julg := fifelse(julgamento != "", julgamento, julgamento_automatico)]
VOC <- c("certo", "parcial", "errado")
stopifnot(all(am$julg %in% c(VOC, "")))
n_julg <- am[julg != "", .N]
cat("linhas julgadas:", n_julg, "/", nrow(am), "\n"); print(am[, .N, by = fonte_julgamento])
if (n_julg == 0L) { cat("nenhuma linha julgada: a precisao nao e medida\n"); quit(save = "no") }
sufixo <- if (am[julg != "" & fonte_julgamento == "automatico", .N] > 0L) "_automatico" else ""
registrar_numero(paste0("aobs_julgadas_pelo_autor"), am[fonte_julgamento == "autor", .N], script = script)
registrar_numero(paste0("aobs_julgadas_automaticamente"), am[fonte_julgamento == "automatico", .N], script = script)

prec <- am[julg != "", .(n = .N, certo = sum(julg == "certo"), parcial = sum(julg == "parcial"), errado = sum(julg == "errado")), by = estrato]
prec[, `:=`(precisao_estrita = round(certo / n, 3), precisao_com_parcial = round((certo + parcial) / n, 3))]
tot <- am[julg != "", .(estrato = "todos", n = .N, certo = sum(julg == "certo"), parcial = sum(julg == "parcial"), errado = sum(julg == "errado"))]
tot[, `:=`(precisao_estrita = round(certo / n, 3), precisao_com_parcial = round((certo + parcial) / n, 3))]
prec <- rbindlist(list(prec[order(estrato)], tot))
prec[, fonte_julgamento := if (sufixo == "") "autor" else "automatico (julgamento do autor vazio)"]
print(prec)
fwrite(prec, "output/verificacao/sapl_observacao_precisao_estrato.csv", quote = TRUE, na = "NA")
for (i in seq_len(nrow(prec))) {
  e <- prec$estrato[i]
  registrar_numero(sprintf("aobs_prec_%s_n%s", e, sufixo), prec$n[i], script = script)
  registrar_numero(sprintf("aobs_prec_%s_certo%s", e, sufixo), prec$certo[i], script = script)
  registrar_numero(sprintf("aobs_prec_%s_parcial%s", e, sufixo), prec$parcial[i], script = script)
  registrar_numero(sprintf("aobs_prec_%s_errado%s", e, sufixo), prec$errado[i], script = script)
  registrar_numero(sprintf("aobs_prec_%s_precisao_estrita%s", e, sufixo), prec$precisao_estrita[i], script = script)
  registrar_numero(sprintf("aobs_prec_%s_precisao_com_parcial%s", e, sufixo), prec$precisao_com_parcial[i], script = script)
}
gravar_relatorio_verificacao(
  alvo = F_AM, script = script,
  passou = c(sprintf("%d de %d linhas julgadas (%s)", n_julg, nrow(am), if (sufixo == "") "autor" else "julgamento automatico, coluna do autor vazia"),
             sprintf("precisao estrita total %s; com parcial %s", tot$precisao_estrita, tot$precisao_com_parcial),
             sprintf("%d estratos com precisao registrada (chaves aobs_prec_*%s)", nrow(prec) - 1L, sufixo)),
  falhou = character(),
  fora_de_cobertura = c(if (sufixo != "") "o julgamento e automatico (leitura do texto da Casa por quem executou o grupo em 05/09/2026), nao do autor; a coluna `julgamento` continua vazia para ele",
                        "a amostra mede a precisao da regra sobre o texto; nao mede a veracidade do que a Casa escreveu"))
cat("precisao registrada com sufixo", if (sufixo == "") "(nenhum: julgamento do autor)" else sufixo, "\n")
