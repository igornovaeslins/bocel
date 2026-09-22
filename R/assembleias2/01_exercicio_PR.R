# 01_exercicio_PR.R — posse, exercicio e forma de saida dos deputados estaduais do Parana
#   a partir do Diario da Assembleia (acervo proprio da ALEP 1999-2011 + Diario Oficial da
#   Assembleia no DIOE 2011-2026), com pareamento ao BOCEL.
# Entrada:  data_raw/assembleias2/PR/cabecalhos.jsonl  (expediente, pagina 1 de cada edicao)
#           data_raw/assembleias2/PR/textos.jsonl      (texto integral, acervo ALEP)
#           data/exercicio_assembleias.csv (lista de composicao ja coletada, uf == PR)
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/PR.csv (21 colunas)
#           output/verificacao/asm2_PR_*.csv, output/numeros_assinatura.txt
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/01_exercicio_PR.R
set.seed(20260829)
suppressPackageStartupMessages({
  library(data.table); library(jsonlite); library(stringi); library(arrow)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source("R/assembleias2/00_funcoes_PR.R")
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

raw    <- file.path(root, "data_raw", "assembleias2", "PR")
outd   <- file.path(root, "data", "assembleias2")
verd   <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "01_exercicio_PR.R")
logf   <- file.path(root, "logs", "asm2_PR_construcao.log")
sink(logf, split = TRUE)
cat("01_exercicio_PR.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(paste0("asm2pr_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")

## ------------------------------------------------------------------ 1. expediente -> painel
cab <- rbindlist(lapply(readLines(file.path(raw, "cabecalhos.jsonl"), warn = FALSE), function(l) {
  o <- fromJSON(l)
  data.table(data = o$data, origem = o$origem, arquivo = o$arquivo,
             texto_p1 = if (is.null(o$texto_p1)) NA_character_ else o$texto_p1)
}))
cab[, data := as.IDate(data)]
cat("edicoes com expediente lido:", nrow(cab), "| ALEP:", cab[origem == "alep_acervo", .N],
    "| DIOE:", cab[origem == "dioe_alep", .N], "\n")
cab[, bloco := vapply(texto_p1, bloco_rep, character(1))]
pan <- cab[!is.na(bloco) & nchar(bloco) > 80, nomes_bloco(bloco), by = .(data, origem, arquivo)]
blocos <- cab[!is.na(bloco) & nchar(bloco) > 80, .(arquivo, bloco = limpa(bloco))]
# uma edicao valida traz a bancada inteira (54 cadeiras no PR); descarta expediente truncado
n_ed <- pan[, .(n_nomes = .N), by = .(data, arquivo)]
ok_ed <- n_ed[n_nomes >= 40 & n_nomes <= 70]
cat("edicoes com relacao nominal completa:", nrow(ok_ed), "de", nrow(n_ed), "\n")
pan <- pan[ok_ed[, .(data, arquivo)], on = .(data, arquivo)]
pan[, j__ := 1L]; L2 <- copy(LEGS)[, j__ := 1L]
pan <- merge(pan, L2, by = "j__", allow.cartesian = TRUE)[data >= leg_inicio & data <= leg_fim]
pan[, j__ := NULL]
cat("observacoes deputado x edicao:", nrow(pan), "\n")
print(pan[, .(edicoes = uniqueN(arquivo), nomes = uniqueN(nome_norm)), by = legislatura][order(legislatura)])

## ------------------------------------------------------------------ 2. variantes de grafia
# duas grafias que aparecem na MESMA edicao sao necessariamente pessoas distintas;
# fora disso, unifica por similaridade alta ou por continencia de vocabulos.
tokens <- function(x) strsplit(x, " ", fixed = TRUE)
unificar <- function(dt) {           # dt: nome_norm, n, por legislatura
  v <- dt$nome_norm; n <- dt$n; k <- length(v)
  if (k <= 1) return(data.table(nome_norm = v, canonico = v))
  tk <- tokens(v)
  co <- dt$co                        # lista de arquivos em que cada grafia aparece
  pai <- seq_len(k)
  acha <- function(i) { while (pai[i] != i) i <- pai[i]; i }
  une  <- function(i, j) { a <- acha(i); b <- acha(j); if (a != b) pai[b] <<- a }
  for (i in 1:(k - 1)) for (j in (i + 1):k) {
    # co-ocorrer na mesma edicao normalmente prova que sao pessoas distintas; a excecao e a
    # grafia de apoio unico contra uma de apoio macico, que e ruido do extrator na mesma pagina
    ruido <- min(n[i], n[j]) <= 2L && max(n[i], n[j]) >= 10L
    if (length(intersect(co[[i]], co[[j]])) && !ruido) next
    a <- tk[[i]]; b <- tk[[j]]
    ult <- a[length(a)] == b[length(b)]
    pri <- a[1] == b[1]
    # continencia so vale entre nomes de tamanho parecido; senao uma grafia truncada que junta
    # dois deputados ("FERNANDO RIBAS CARLI MILTINHO PUPIO") faz ponte entre os dois
    cont <- (all(a %in% b) || all(b %in% a)) && length(intersect(a, b)) >= 2 &&
      abs(length(a) - length(b)) <= 2L
    # sobrenome comum nao basta: "JONAS GUIMARAES" e "PLAUTO MIRO GUIMARAES" sao duas pessoas.
    # Exige primeiro vocabulo igual, continencia, ou dois vocabulos em comum.
    jw <- 1 - stringdist_jw(v[i], v[j])
    compat <- length(a) == 1L || length(b) == 1L || a[1L] == b[1L] || jw >= 0.90 ||
      all(a %in% b) || all(b %in% a) || length(intersect(a, b)) >= 2L
    if (!compat) next
    # grafia do sobrenome varia no expediente (ANNIBELLI/ANIBELLI, PUPPIO/PUPIO, CARTARIO/CATARIO):
    # compara tambem o ultimo vocabulo por similaridade, e nao so por igualdade
    jw_ult <- 1 - stringdist_jw(a[length(a)], b[length(b)])
    quase_ult <- jw_ult >= 0.88 && min(nchar(a[length(a)]), nchar(b[length(b)])) >= 4
    if ((ult && pri) || (cont && (ult || pri)) || (jw >= 0.93 && ult) ||
        (pri && quase_ult) || (jw >= 0.90 && quase_ult) ||
        (quase_ult && abs(length(a) - length(b)) <= 1L &&
           (all(a %in% b) || all(b %in% a) || length(intersect(a, b)) >= 1))) une(i, j)
  }
  gr <- vapply(seq_len(k), acha, integer(1))
  dt[, g := gr]
  dt[, canonico := nome_norm[which.max(n)], by = g]
  dt[, .(nome_norm, canonico)]
}
stringdist_jw <- function(a, b) stringdist::stringdist(a, b, method = "jw", p = 0.1)
if (!requireNamespace("stringdist", quietly = TRUE)) stop("pacote stringdist ausente")

# grafia que e a colagem de dois nomes da mesma legislatura ("FERNANDO RIBAS CARLI MILTINHO
# PUPIO") e falha do extrator ao emendar duas entradas da relacao nominal, e nao um parlamentar
todas <- pan[, .(n = .N), by = .(legislatura, nome_norm)]
colagem <- todas[stri_count_fixed(nome_norm, " ") >= 3L]
if (nrow(colagem)) {
  ehcol <- vapply(seq_len(nrow(colagem)), function(k) {
    L <- colagem$legislatura[k]; v <- unlist(stri_split_fixed(colagem$nome_norm[k], " "))
    outras <- todas[legislatura == L & nome_norm != colagem$nome_norm[k], nome_norm]
    any(vapply(1:(length(v) - 1L), function(i)
      paste(v[1:i], collapse = " ") %in% outras &&
      paste(v[(i + 1L):length(v)], collapse = " ") %in% outras, logical(1)))
  }, logical(1))
  fora <- colagem[ehcol]
  cat("grafias que sao colagem de dois nomes (descartadas):", nrow(fora), "\n")
  if (nrow(fora)) { print(fora); pan <- pan[!fora, on = .(legislatura, nome_norm)] }
}

gr <- pan[, .(n = .N, co = list(unique(arquivo))), by = .(legislatura, nome_norm)]
uni <- gr[, unificar(copy(.SD)), by = legislatura, .SDcols = c("nome_norm", "n", "co")]
pan <- merge(pan, uni, by = c("legislatura", "nome_norm"))
cat("grafias:", uniqueN(pan[, .(legislatura, nome_norm)]),
    "-> pessoas por legislatura:", uniqueN(pan[, .(legislatura, canonico)]), "\n")

## ------------------------------------------------------------------ 3. periodo de exercicio
# a edicao de 1 de fevereiro ainda imprime a bancada que sai; quem so aparece num unico dia da
# legislatura, e em nenhum outro, e leitura da relacao anterior, nao posse nova
n_dias <- pan[, .(n_dias = uniqueN(data)), by = .(legislatura, canonico)]
pan <- merge(pan, n_dias, by = c("legislatura", "canonico"))
descartadas <- pan[n_dias < 2L, uniqueN(paste(legislatura, canonico))]
cat("pessoas com um unico dia de expediente na legislatura (descartadas do painel):", descartadas, "\n")
pan_1dia <- unique(pan[n_dias < 2L, .(legislatura, canonico, data)])
pan <- pan[n_dias >= 2L]

grade <- unique(pan[, .(legislatura, data, arquivo)])
setorder(grade, legislatura, data)
per <- pan[, .(
  nome_diario   = nome[which.max(nchar(nome))],
  n_edicoes     = uniqueN(arquivo),
  primeira      = min(data), ultima = max(data),
  partido       = { p <- partido[!is.na(partido)]; if (length(p)) names(sort(table(p), decreasing = TRUE))[1] else NA_character_ },
  partidos      = paste(sort(unique(na.omit(partido))), collapse = ";"),
  anotacoes     = paste(sort(unique(na.omit(anotacao))), collapse = ";")
), by = .(legislatura, ano_eleicao, leg_inicio, leg_fim, canonico)]
prim_ed <- grade[, .(prim_data = min(data), ult_data = max(data)), by = legislatura]
per <- merge(per, prim_ed, by = "legislatura")
per[, `:=`(desde_inicio = primeira <= prim_data,
           ate_o_fim    = ultima  >= ult_data - 45L)]
cat("pessoa x legislatura:", nrow(per), "\n")
print(per[, .(.N, desde_inicio = sum(desde_inicio), ate_o_fim = sum(ate_o_fim)), by = legislatura][order(legislatura)])
reg("painel_n_edicoes_lidas", nrow(cab))
reg("painel_n_edicoes_com_relacao_nominal", uniqueN(pan$arquivo))
reg("painel_n_observacoes_deputado_edicao", nrow(pan))
reg("painel_n_grafias", uniqueN(pan[, .(legislatura, nome_norm)]))
reg("painel_n_pessoas_legislatura", nrow(per))
for (L in sort(unique(per$legislatura))) {
  reg(paste0("painel_edicoes_leg_", L), uniqueN(pan[legislatura == L, arquivo]))
  reg(paste0("painel_pessoas_leg_", L), per[legislatura == L, .N])
}
saveRDS(list(pan = pan, per = per, grade = grade, pan_1dia = pan_1dia, blocos = blocos), file.path(raw, "painel_expediente.rds"))
cat("\n(parcial) 01_exercicio_PR — painel gravado —", format(Sys.time()), "\n")
sink()
