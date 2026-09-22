# 25_exercicio_camaras_sem_sapl_2.R — SEGUNDA rodada de coletores nas camaras municipais SEM SAPL: familias
# identificadas nos grupos que a rodada 1 deixou sem coletor (desconhecido, WordPress, Joomla, IO, Fator, Digitaliza,
# IPM, Elotech, Actcon, Camara Sem Papel, Administracao Publica), coletadas por python/fetch_camaras_sem_sapl_2.py
# no mesmo formato intermediario da rodada 1 (parlamentares.json por municipio).
# Entrada:  data_raw/camaras_sem_sapl_2/inventario_familias.csv, data_raw/camaras_sem_sapl_2/<uf>/<sg_ue>/parlamentares.json,
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:    data/exercicio_camaras_sem_sapl_2.csv, data/exercicio_camaras_sem_sapl_2_cobertura.csv,
#           output/verificacao/camaras_sem_sapl_2_familias.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/25_exercicio_camaras_sem_sapl_2.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "data_referencia.R")); DATA_REF <- data_referencia(root)  # data fixa da versao, nao o dia da execucao (l. 444)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/25_exercicio_camaras_sem_sapl_2.R"
dir.create("logs", showWarnings = FALSE)
logf <- file("logs/25_exercicio_camaras_sem_sapl_2.log", open = "wt"); sink(logf, split = TRUE)
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
## data valida = ISO com ano plausivel; o Webline de Rafard/SP publica 01/01/0001 como marcador de
## periodo nao informado, e uma data impossivel nao pode chegar ao arquivo entregue
d10 <- function(x) { x <- substr(as.character(x), 1, 10)
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", x) & !is.na(x)
  ok[ok] <- as.integer(substr(x[ok], 1, 4)) >= 1900L & as.integer(substr(x[ok], 1, 4)) <= 2035L
  fifelse(ok, x, NA_character_) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")

## ------------------------------------------------ inventario de familias (classificacao dos portais sem coletor na rodada 1)
inv <- fread("data_raw/camaras_sem_sapl_2/inventario_familias.csv", colClasses = "character")
alvo <- inv[coletado_rodada1 == "FALSE"]
registrar_numero("csem2_n_municipios_rodada1", inv[coletado_rodada1 == "TRUE", .N], script = script)
registrar_numero("csem2_n_municipios_sem_coletor_rodada1", nrow(alvo), script = script)
fam <- alvo[, .N, by = familia][order(-N)]
print(fam)
for (i in seq_len(nrow(fam))) registrar_numero(paste0("csem2_n_por_familia_", fam$familia[i]), fam$N[i], script = script)
fwrite(fam, "output/verificacao/camaras_sem_sapl_2_familias.csv")

## ------------------------------------------------ leitura do cache da coleta
arqs <- list.files("data_raw/camaras_sem_sapl_2", pattern = "^parlamentares\\.json$", recursive = TRUE, full.names = TRUE)
cat("municipios com parlamentares.json:", length(arqs), "\n")
registrar_numero("csem2_n_municipios_com_json", length(arqs), script = script)
col <- function(dt, nm) if (nm %in% names(dt)) as.character(dt[[nm]]) else rep(NA_character_, nrow(dt))
ex <- rbindlist(lapply(arqs, function(f) {
  x <- tryCatch(fromJSON(f, simplifyVector = TRUE, flatten = TRUE), error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(NULL)
  x <- as.data.table(x)
  if (!nrow(x)) return(NULL)
  p <- strsplit(f, "/")[[1]]
  data.table(uf = p[length(p) - 2], sg_ue = p[length(p) - 1],
             sistema = col(x, "sistema"), dominio = col(x, "dominio"), url = col(x, "url"),
             legislatura_numero = col(x, "legislatura_numero"), legislatura_inicio = d10(col(x, "legislatura_inicio")),
             legislatura_fim = d10(col(x, "legislatura_fim")), nome_fonte = col(x, "nome_fonte"), nome_parlamentar = col(x, "nome_parlamentar"),
             titular = col(x, "titular"), data_inicio_mandato = d10(col(x, "data_inicio")), data_fim_mandato = d10(col(x, "data_fim")),
             situacao = col(x, "situacao"), so_legislatura_atual = col(x, "so_legislatura_atual"), ano_eleicao_fonte = col(x, "ano_eleicao"))
}), use.names = TRUE, fill = TRUE)
stopifnot(nrow(ex) > 0)
registrar_numero("csem2_n_linhas_brutas", nrow(ex), script = script)
ex <- ex[!is.na(nome_fonte) & nchar(trimws(nome_fonte)) >= 3]
## nenhum municipio da rodada 1 pode reaparecer aqui
rod1 <- inv[coletado_rodada1 == "TRUE", unique(sg_ue)]
registrar_numero("csem2_n_municipios_repetidos_da_rodada1", ex[sg_ue %in% rod1, uniqueN(sg_ue)], script = script)
ex <- ex[!sg_ue %in% rod1]
ex <- merge(ex, fread("data/municipios_tse_ibge.csv", colClasses = "character")[, .(sg_ue, id_municipio_ibge)], by = "sg_ue", all.x = TRUE)
ex[, titular := fifelse(is.na(titular), NA, toupper(titular) %in% c("TRUE", "T", "1", "TITULAR"))]
ex[, so_legislatura_atual := toupper(so_legislatura_atual) %in% c("TRUE", "T", "1")]
ex <- merge(ex, alvo[, .(sg_ue, familia)], by = "sg_ue", all.x = TRUE)

## eleicao de referencia (mesma regra de R/15 e R/20): ano anterior ao inicio da legislatura no ciclo quadrienal;
## senao ano informado pela fonte; senao ano anterior ao inicio do mandato
CICLO <- seq(1996L, 2024L, 4L)
ex[, `:=`(ya = suppressWarnings(as.integer(ano_eleicao_fonte)), yb = as.integer(substr(legislatura_inicio, 1, 4)) - 1L,
          yc = as.integer(substr(data_inicio_mandato, 1, 4)) - 1L)]
ex[, ano_eleicao_bocel := fcase(!is.na(yb) & yb %in% CICLO, yb,
                              !is.na(ya) & ya %in% CICLO, ya,
                              !is.na(yc) & yc %in% CICLO, yc,
                              default = NA_integer_)]
registrar_numero("csem2_n_linhas_sem_ano_eleicao", ex[is.na(ano_eleicao_bocel), .N], script = script)
ex <- ex[!is.na(ano_eleicao_bocel) & ano_eleicao_bocel >= 1996L & ano_eleicao_bocel <= 2024L]
ex[, nome_normalizado := norm(nome_fonte)]
ex[, nome_parl_norm := fifelse(is.na(nome_parlamentar), NA_character_, norm(nome_parlamentar))]
## uma linha por municipio-legislatura-nome (paginas com secoes repetidas duplicam o mesmo parlamentar)
setorder(ex, sg_ue, ano_eleicao_bocel, nome_normalizado, -titular, na.last = TRUE)
ex <- ex[!duplicated(ex[, .(sg_ue, ano_eleicao_bocel, nome_normalizado)])]

## forma de saida: situacao textual da fonte; senao data de fim contra o fim da legislatura (mesma regra de R/20)
ex[, desc := toupper(stri_trans_general(fcoalesce(situacao, ""), "Latin-ASCII"))]
ex[, fim_leg := as.IDate(fcoalesce(legislatura_fim, fifelse(!is.na(ano_eleicao_bocel), sprintf("%d-12-31", ano_eleicao_bocel + 4L), NA_character_)))]
## a linha do suplente carrega, na situacao, o motivo do afastamento do TITULAR que ele substituiu (webline publica
## suplente, substituido, periodo e motivo na mesma linha): o motivo classifica a saida do titular, nunca a do suplente,
## por isso o ramo do suplente vem antes de todos os motivos
ex[, forma_saida := fcase(
  grepl("^SUPLENTE EFETIVADO", desc), "suplente_efetivado",
  grepl("CASSA|PERDA D", desc), "cassacao",
  grepl("RENUNC", desc), "renuncia",
  grepl("FALEC|MORTE|OBITO|IN MEMORI", desc), "falecimento",
  grepl("LICEN", desc), "licenca",
  grepl("AFAST|PRISAO|PRISA", desc), "afastamento",
  grepl("NAO TOMOU POSSE|NAO EMPOSSAD", desc), "nao_tomou_posse",
  !is.na(data_fim_mandato) & !is.na(fim_leg) & as.IDate(data_fim_mandato) >= fim_leg - 45L, "fim_regular",
  !is.na(data_fim_mandato) & !is.na(fim_leg) & as.IDate(data_fim_mandato) < fim_leg - 45L, "outro",
  is.na(data_fim_mandato) & !is.na(fim_leg) & fim_leg < DATA_REF & grepl("^SUPLENTE", desc), "suplente_efetivado",
  is.na(data_fim_mandato) & !is.na(fim_leg) & fim_leg < DATA_REF & titular %in% TRUE, "fim_regular",
  default = "nao_observado")]
stopifnot(all(ex$forma_saida %in% VOCAB))

## ------------------------------------------------ pareamento com o BOCEL (vereador, mesmo municipio, mesma eleicao)
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "13"]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano_eleicao)], pess[, .(id_pessoa, nome)], by = "id_pessoa")
mand[, nome_norm := norm(nome)]
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(arrow::read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO == "13"]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))], by = "id_mandato", all.x = TRUE)
mand[, ano_eleicao := as.integer(ano_eleicao)]
mand <- mand[sg_ue %in% unique(ex$sg_ue)]

tok <- function(x) lapply(strsplit(fcoalesce(x, ""), " "), function(t) t[nchar(t) >= 3])
vazio <- function() data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (nrow(d)) d[, .(rid, id_mandato, id_pessoa, metodo)] else vazio()
ex[, rid := .I]
# (1) nome civil completo igual
m1 <- merge(ex[, .(rid, sg_ue, ano_eleicao_bocel, nome_normalizado)], mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nome_normalizado"), by.y = c("sg_ue", "ano_eleicao", "nome_norm"))
m1 <- m1[, if (.N == 1) .SD, by = rid][, metodo := "nome_completo"]
# (2) nome da fonte igual ao nome de urna do TSE
r2 <- ex[!rid %in% m1$rid]
m2 <- merge(r2[, .(rid, sg_ue, ano_eleicao_bocel, nm = fcoalesce(nome_parl_norm, nome_normalizado))], mand[!is.na(nome_urna_norm), .(sg_ue, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel", "nm"), by.y = c("sg_ue", "ano_eleicao", "nome_urna_norm"))
m2 <- m2[, if (.N == 1) .SD, by = rid][, metodo := "nome_fonte=nome_urna"]
# (3) tokens do nome da fonte (>= 2, todos com >= 3 letras) contidos no nome civil, pessoa unica no municipio-eleicao
r3 <- ex[!rid %in% c(m1$rid, m2$rid)]
c3 <- merge(r3[, .(rid, sg_ue, ano_eleicao_bocel, nm = fcoalesce(nome_parl_norm, nome_normalizado))], mand[, .(sg_ue, ano_eleicao, nome_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), allow.cartesian = TRUE)
m3 <- if (nrow(c3)) {
  tr <- tok(c3$nm); tn <- tok(c3$nome_norm)
  c3[, contido := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tr, tn)]
  c3[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_nome_fonte_no_nome_civil"]
} else vazio()
# (4) tokens do nome da fonte (apelido, >= 1 token com >= 4 letras) contidos no nome de urna, pessoa unica
r4 <- ex[!rid %in% c(m1$rid, m2$rid, sel(m3)$rid)]
c4 <- merge(r4[, .(rid, sg_ue, ano_eleicao_bocel, nm = fcoalesce(nome_parl_norm, nome_normalizado))], mand[!is.na(nome_urna_norm), .(sg_ue, ano_eleicao, nome_urna_norm, id_mandato, id_pessoa)],
            by.x = c("sg_ue", "ano_eleicao_bocel"), by.y = c("sg_ue", "ano_eleicao"), allow.cartesian = TRUE)
m4 <- if (nrow(c4)) {
  tr <- tok(c4$nm); tu <- tok(c4$nome_urna_norm)
  c4[, contido := mapply(function(a, b) length(a) >= 1 && any(nchar(a) >= 4) && all(a %in% b), tr, tu)]
  c4[contido == TRUE][, if (uniqueN(id_mandato) == 1) .SD[1], by = rid][, metodo := "tokens_nome_fonte_no_nome_de_urna"]
} else vazio()
cat("pareamentos por regra:", nrow(m1), nrow(m2), nrow(sel(m3)), nrow(sel(m4)), "\n")
par <- rbindlist(list(sel(m1), sel(m2), sel(m3), sel(m4)), use.names = TRUE)[!duplicated(rid)]
ex[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
ex[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# um mandato do BOCEL recebe uma unica linha: fica a de titular (ou NA) com data de inicio mais antiga
setorder(ex, id_mandato_bocel, -titular, data_inicio_mandato, na.last = TRUE)
ex[!is.na(id_mandato_bocel), dup := seq_len(.N) > 1, by = id_mandato_bocel]
ex[dup %in% TRUE, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = "descartado_duplicata")]

out <- ex[, .(sg_ue, id_municipio_ibge, uf, dominio, legislatura_numero, legislatura_inicio, legislatura_fim, ano_eleicao_bocel,
              nome_fonte, nome_parlamentar, nome_normalizado, titular, data_inicio_mandato, data_fim_mandato,
              tipo_afastamento = situacao, forma_saida, id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, sistema,
              so_legislatura_atual, url)]
setorder(out, uf, sg_ue, ano_eleicao_bocel, nome_normalizado)
fwrite(out, "data/exercicio_camaras_sem_sapl_2.csv", na = "NA", quote = TRUE)

## ------------------------------------------------ cobertura por UF e eleicao
todos <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo == "13", .(sg_ue = unidade_posicao, ano_eleicao = as.integer(ano_eleicao))]
cob <- todos[, .(n_mandatos_bocel = .N), by = .(sg_ue, ano_eleicao)]
cob <- merge(cob, fread("data/municipios_tse_ibge.csv", colClasses = "character")[, .(sg_ue, uf = sg_uf)], by = "sg_ue")
cob <- cob[, .(n_mandatos_bocel = sum(n_mandatos_bocel), n_municipios_bocel = uniqueN(sg_ue)), by = .(uf, ano_eleicao)]
par_c <- out[!is.na(id_mandato_bocel), .(n_pareados = uniqueN(id_mandato_bocel), n_camaras_coletadas = uniqueN(sg_ue)), by = .(uf, ano_eleicao = ano_eleicao_bocel)]
cob <- merge(cob, par_c, by = c("uf", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), `:=`(n_pareados = 0L, n_camaras_coletadas = 0L)]
cob[, taxa := round(n_pareados / n_mandatos_bocel, 4)]
fwrite(cob[order(uf, ano_eleicao)], "data/exercicio_camaras_sem_sapl_2_cobertura.csv", na = "NA")

## ------------------------------------------------ numeros
registrar_numero("csem2_n_camaras_coletadas", out[, uniqueN(sg_ue)], script = script)
registrar_numero("csem2_n_camaras_com_pareamento", out[!is.na(id_mandato_bocel), uniqueN(sg_ue)], script = script)
registrar_numero("csem2_n_camaras_so_legislatura_atual", out[so_legislatura_atual == TRUE, uniqueN(sg_ue)], script = script)
registrar_numero("csem2_n_linhas_coletadas", nrow(out), script = script)
registrar_numero("csem2_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("csem2_n_mandatos_pareados_com_saida_observada", out[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)], script = script)
registrar_numero("csem2_taxa_pareamento_linhas", round(out[!is.na(id_mandato_bocel), .N] / nrow(out), 4), script = script)
registrar_numero("csem2_n_legislaturas_distintas_por_camara_mediana", as.numeric(median(out[, uniqueN(ano_eleicao_bocel), by = sg_ue]$V1)), script = script)
sis <- out[, .(camaras = uniqueN(sg_ue), linhas = .N, pareadas = sum(!is.na(id_mandato_bocel))), by = sistema][order(-pareadas)]
for (i in seq_len(nrow(sis))) {
  registrar_numero(paste0("csem2_n_camaras_coletadas_", sis$sistema[i]), sis$camaras[i], script = script)
  registrar_numero(paste0("csem2_n_mandatos_pareados_sistema_", sis$sistema[i]), sis$pareadas[i], script = script)
}
for (f in unique(out$forma_saida)) registrar_numero(paste0("csem2_n_forma_saida_", f), out[forma_saida == f & !is.na(id_mandato_bocel), .N], script = script)
for (m in unique(na.omit(out$metodo_pareamento))) registrar_numero(paste0("csem2_n_metodo_", m), out[metodo_pareamento == m, .N], script = script)
ufs <- cob[, .(n_mandatos_bocel = sum(n_mandatos_bocel), n_pareados = sum(n_pareados)), by = uf][, taxa := round(n_pareados / n_mandatos_bocel, 4)][order(-n_pareados)]
for (i in seq_len(nrow(ufs))) {
  registrar_numero(paste0("csem2_taxa_pareamento_uf_", ufs$uf[i]), ufs$taxa[i], script = script)
  registrar_numero(paste0("csem2_n_pareados_uf_", ufs$uf[i]), ufs$n_pareados[i], script = script)
}
## ganho sobre a rodada 1: mandatos que a rodada 2 pareia e a rodada 1 nao tinha
r1 <- fread("data/exercicio_camaras_sem_sapl.csv", colClasses = "character", na.strings = "NA")
registrar_numero("csem2_n_mandatos_pareados_novos_sobre_rodada1",
                 out[!is.na(id_mandato_bocel) & !id_mandato_bocel %in% r1[!is.na(id_mandato_bocel), id_mandato_bocel], uniqueN(id_mandato_bocel)], script = script)
registrar_numero("csem2_n_camaras_novas_sobre_rodada1", out[!sg_ue %in% r1$sg_ue, uniqueN(sg_ue)], script = script)
print(sis); print(ufs[n_pareados > 0]); print(out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)])
print(out[!is.na(id_mandato_bocel), .N, by = forma_saida][order(-N)])
cat("25_exercicio_camaras_sem_sapl_2: concluido —", nrow(out), "linhas,", out[, uniqueN(sg_ue)], "camaras,",
    out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
sink()
