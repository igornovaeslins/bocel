# 33_tce_gestores_c.R — cadastros de gestores/responsaveis por unidade gestora municipal nos Tribunais de
# Contas do GRUPO C (MG, PR, GO com TCE-GO e TCM-GO, MT, DF e TO), o que faltava no Sudeste, Sul e
# Centro-Oeste. Coleta em python/fetch_tce_c.py; inventario em data_raw/tce/inventario_tce_c.csv.
#
# Das oito casas inventariadas, uma so entrega cadastro: o TCM-GO, por duas fontes.
#
#   (a) "Contas Julgadas pelas Camaras Municipais" (PDF assinado, convertido para CSV na coleta). Uma
#       linha por processo de contas de governo julgado pela camara municipal, com o PREFEITO responsavel
#       pelo exercicio nomeado. E a unica fonte do grupo C que cobre o universo e nao apenas os
#       reprovados, e a unica que permite pareamento em escala.
#   (b) Rol da Lei da Ficha Limpa (CSV aberto em ws.tcm.go.gov.br). Tres listas; so a primeira,
#       "Contas de Prefeitos e Ex-Prefeitos", identifica cargo eletivo — as outras duas cobrem, por
#       definicao do proprio Tribunal, responsaveis do Executivo EXCETO prefeitos, do Legislativo e de
#       fundos e autarquias, e por isso entram no arquivo com cargo_bocel NA e sem pareamento, salvo as
#       linhas cuja unidade gestora e a camara (sufixo CAM), atribuidas a VEREADOR.
#
# O que estas fontes observam e QUEM respondia pela unidade gestora num exercicio, nao a forma de saida.
# A contribuicao e, portanto, de confirmacao de exercicio. forma_saida so recebe valor no caso estreito
# em que quem responde pela prefeitura num exercicio do mandato e o VICE-PREFEITO eleito: ai a saida do
# titular esta observada, ainda que a fonte nao diga a forma, e o registro recebe "outro" — a mesma
# regra de R/23_tce_gestores.R, para que os tres grupos sejam comparaveis.
#
# Resolucao de municipio: o TCM-GO grafa o nome sem acento e sem as preposicoes ("ABADIA GOIAS" por
# Abadia de Goias), as vezes sem o sufixo "de Goias" ("AGUAS LINDAS") e, no PDF, truncado na largura da
# coluna ("SAO MIGUEL PASSA" por Sao Miguel do Passa Quatro). Por isso a resolucao e feita em quatro
# regras encadeadas, todas exigindo unicidade, e o metodo fica registrado por linha.
#
# Entrada:  data_raw/tce/inventario_tce_c.csv, data_raw/tce/go/*, data/mandatos.csv, data/pessoas.csv,
#           data/municipios_tse_ibge.csv, data_raw/parquet/cand_*.parquet (nome de urna)
# Saida:    data/tce_gestores_c.csv, data/tce_gestores_c_cobertura.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/33_tce_gestores_c.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/33_tce_gestores_c.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/33_tce_gestores_c.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) {
  x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII")
  x <- gsub("['`´‘’]", "", x)          # d'Abadia -> DABADIA, e nao D ABADIA
  x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x))
}
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
dbr <- function(x) { x <- trimws(as.character(x)); fifelse(grepl("^\\d{2}/\\d{2}/\\d{4}$", x),
                     paste0(substr(x, 7, 10), "-", substr(x, 4, 5), "-", substr(x, 1, 2)), NA_character_) }
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
CICLO <- seq(1996L, 2024L, 4L)
# exercicio financeiro -> eleicao que originou o mandato (posse em 1o de janeiro do ano seguinte a eleicao)
elei_de <- function(ex) { y <- ((as.integer(ex) - 1L) %/% 4L) * 4L; fifelse(!is.na(y) & y %in% CICLO, y, NA_integer_) }

## ------------------------------------------------ passo 1: inventario das casas do grupo C
inv <- fread("data_raw/tce/inventario_tce_c.csv", colClasses = "character")
print(inv[, .(uf, tribunal, tipo, viavel)])
registrar_numero("tcec_n_casas_inventariadas", nrow(inv), script = script)
registrar_numero("tcec_n_ufs_inventariadas", inv[, uniqueN(uf)], script = script)
registrar_numero("tcec_n_tribunais_inventariados", inv[, uniqueN(paste(uf, tribunal))], script = script)
registrar_numero("tcec_n_tribunais_com_fonte", inv[viavel %in% c("sim", "parcial"), uniqueN(paste(uf, tribunal))], script = script)
registrar_numero("tcec_n_tribunais_sem_fonte", inv[viavel == "nao", uniqueN(paste(uf, tribunal))], script = script)
registrar_numero("tcec_n_fontes_viaveis", inv[viavel == "sim", .N], script = script)

## ------------------------------------------------ resolvedor de municipio para GO (quatro regras)
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
STOP <- c("DE", "DA", "DO", "DAS", "DOS", "E")
chave <- function(x, tirar_uf = FALSE) {
  st <- if (tirar_uf) c(STOP, "GOIAS", "GO") else STOP
  vapply(strsplit(norm(x), " "), function(z) paste(z[!z %in% st], collapse = " "), "")
}
mgo <- mun[sg_uf == "GO"][, `:=`(k1 = chave(nome_ibge), k2 = chave(nome_ibge, TRUE))]
u1 <- mgo[, .N, by = k1][N == 1L, k1]; u2 <- mgo[, .N, by = k2][N == 1L, k2]
res_go <- function(nomes) {
  d <- data.table(nome = nomes, k1 = chave(nomes), k2 = chave(nomes, TRUE))
  d[, `:=`(sg_ue = NA_character_, id_municipio_ibge = NA_character_, metodo_municipio = NA_character_)]
  # (1) nome sem preposicoes, exato
  d[mgo[k1 %in% u1], on = "k1", `:=`(sg_ue = i.sg_ue, id_municipio_ibge = i.id_municipio_ibge,
                                     metodo_municipio = "nome_sem_preposicoes")]
  # (2) idem, tambem sem o sufixo "de Goias"
  d[mgo[k2 %in% u2], on = "k2", `:=`(ue_x = i.sg_ue, ib_x = i.id_municipio_ibge)]
  d[is.na(sg_ue) & !is.na(ue_x), `:=`(sg_ue = ue_x, id_municipio_ibge = ib_x,
                                      metodo_municipio = "nome_sem_sufixo_goias")]
  d[, c("ue_x", "ib_x") := NULL]
  # (3) tokens do nome da fonte contidos no nome do IBGE (cobre o truncamento na largura da coluna)
  pend <- which(is.na(d$sg_ue))
  for (i in pend) {
    tk <- strsplit(d$k2[i], " ")[[1]]
    if (length(tk) < 2L) next
    hit <- mgo[vapply(strsplit(k2, " "), function(z) all(tk %in% z), TRUE)]
    if (nrow(hit) == 1L) set(d, i, c("sg_ue", "id_municipio_ibge", "metodo_municipio"),
                             list(hit$sg_ue, hit$id_municipio_ibge, "tokens_contidos_no_nome_ibge"))
  }
  # (4) distancia de edicao <= 2 e unica (cobre a grafia trocada: PASSO por PASSA)
  pend <- which(is.na(d$sg_ue))
  for (i in pend) {
    if (nchar(d$k2[i]) < 8L) next
    dd <- as.integer(adist(d$k2[i], mgo$k2))
    j <- which(dd <= 2L)
    if (length(j) == 1L) set(d, i, c("sg_ue", "id_municipio_ibge", "metodo_municipio"),
                             list(mgo$sg_ue[j], mgo$id_municipio_ibge[j], "distancia_edicao_<=2"))
  }
  d
}

## ------------------------------------------------ passo 2: leitura das fontes coletadas
fontes <- list()
vaziog <- function() data.table(uf = character(), tribunal = character(), fonte = character(),
  unidade_gestora = character(), tipo_unidade = character(), nome_municipio_fonte = character(),
  nome = character(), cpf = character(), cargo_fonte = character(), cargo_bocel = character(),
  exercicio = integer(), data_inicio = character(), data_fim = character(),
  situacao_fonte = character(), url = character())

## GO (a) — contas de governo julgadas pelas camaras municipais: o responsavel e o prefeito do exercicio.
## So as contas de governo (balanco geral e balancetes) sustentam essa atribuicao; as tomadas de contas
## especiais que aparecem na mesma relacao ficam com cargo_bocel NA.
f <- "data_raw/tce/go/contas_julgadas_camaras.csv"
if (file.exists(f)) {
  x <- fread(f, colClasses = "character")
  x[, exercicio := as.integer(sub("^.*/", "", mes_ano))]
  x[, contas_de_governo := grepl("^BALANC", assunto)]
  fontes[["go_camaras"]] <- data.table(uf = "GO", tribunal = "TCM-GO",
    fonte = "tcmgo_contas_de_governo_julgadas_pelas_camaras",
    unidade_gestora = paste("PREFEITURA MUNICIPAL DE", x$municipio), tipo_unidade = "prefeitura",
    nome_municipio_fonte = x$municipio, nome = x$nome, cpf = NA_character_,
    cargo_fonte = paste0("Responsavel pelas contas de governo (", x$assunto, ")"),
    cargo_bocel = fifelse(x$contas_de_governo, "PREFEITO", NA_character_),
    exercicio = x$exercicio, data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("julgamento pela camara em ", dbr(x$data_julgamento), "; resultado ",
                            x$resultado, "; processo ", x$processo),
    url = "https://www.tcmgo.tc.br/site/tcm-em-acao/julgamento-das-contas-de-governo-pelas-camaras-municipais/")
}

## GO (b) — rol da Lei da Ficha Limpa. O sufixo do campo Municipio identifica a unidade gestora.
f <- "data_raw/tce/go/contas_irregulares.csv"
if (file.exists(f)) {
  x <- fread(f, colClasses = "character")
  setnames(x, c("Município", "Mês/Ano", "Dt. Trânsito Julgado", "Data Julgamento", "Acórdão/Resolução", "Processo/Fase"),
           c("municipio_ug", "mes_ano", "dt_transito", "dt_julg", "acordao", "processo"), skip_absent = TRUE)
  x[, sufixo := fifelse(grepl(" - ", municipio_ug), sub("^.* - ", "", municipio_ug), NA_character_)]
  x[, mun_fonte := trimws(sub(" - .*$", "", municipio_ug))]
  x[, exercicio := as.integer(sub("^.*/", "", mes_ano))]
  x[, e_prefeito := TipoLista == "Contas de Prefeitos e Ex-Prefeitos"]
  x[, e_camara := !is.na(sufixo) & toupper(sufixo) %in% c("CAM", "CAMARA")]
  fontes[["go_ficha_limpa"]] <- data.table(uf = "GO", tribunal = "TCM-GO",
    fonte = "tcmgo_rol_contas_irregulares_ficha_limpa",
    unidade_gestora = x$municipio_ug,
    tipo_unidade = fcase(x$e_camara, "camara", is.na(x$sufixo), "prefeitura", default = "outra"),
    nome_municipio_fonte = x$mun_fonte, nome = x$Nome, cpf = NA_character_,
    cargo_fonte = paste0(x$TipoLista, "; ", x$Assunto),
    cargo_bocel = fcase(x$e_prefeito, "PREFEITO", x$e_camara, "VEREADOR", default = NA_character_),
    exercicio = x$exercicio, data_inicio = NA_character_, data_fim = NA_character_,
    situacao_fonte = paste0("contas julgadas irregulares ou parecer pela rejeicao; processo ", x$processo,
                            "; cpf mascarado ", x$CPF, "; transito ", x$dt_transito, "; ", x$acordao),
    url = "https://ws.tcm.go.gov.br/api/rest/dados/contas-irregulares")
}

g <- rbindlist(c(list(vaziog()), fontes), use.names = TRUE, fill = TRUE)
stopifnot(nrow(g) > 0)
g[, nome := trimws(gsub("\\s+", " ", nome))]
g <- g[nchar(nome) >= 5]
cat("linhas brutas por fonte:\n"); print(g[, .N, by = .(uf, tribunal, fonte)][order(uf, fonte)])
registrar_numero("tcec_n_registros_brutos", nrow(g), script = script)

## municipio
rg <- res_go(g$nome_municipio_fonte)
g[, `:=`(sg_ue = rg$sg_ue, id_municipio_ibge = rg$id_municipio_ibge, metodo_municipio = rg$metodo_municipio)]
cat("resolucao de municipio:\n"); print(g[, .N, by = metodo_municipio][order(-N)])
registrar_numero("tcec_n_registros_sem_municipio_resolvido", g[is.na(sg_ue), .N], script = script)
print(unique(g[is.na(sg_ue), .(nome_municipio_fonte)]))
g <- g[!is.na(sg_ue)]
g[, ano_eleicao := elei_de(exercicio)]
g[, nome_normalizado := norm(nome)]
registrar_numero("tcec_n_registros", nrow(g), script = script)
registrar_numero("tcec_n_registros_com_cpf", g[!is.na(cpf), .N], script = script)
registrar_numero("tcec_n_registros_com_exercicio", g[!is.na(exercicio), .N], script = script)
registrar_numero("tcec_n_registros_com_cargo_eletivo", g[!is.na(cargo_bocel), .N], script = script)
registrar_numero("tcec_n_municipios_cobertos", g[, uniqueN(sg_ue)], script = script)
registrar_numero("tcec_exercicio_min", g[, min(exercicio, na.rm = TRUE)], script = script)
registrar_numero("tcec_exercicio_max", g[, max(exercicio, na.rm = TRUE)], script = script)

## ------------------------------------------------ BOCEL: mandatos de prefeito, vice e vereador
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, sg_uf,
                       ano_eleicao = as.integer(ano_eleicao), cd_cargo, cargo)],
              pess[, .(id_pessoa, nome_bocel = nome, nr_cpf)], by = "id_pessoa")
mand[, nome_norm := norm(nome_bocel)]
ufs_alvo <- unique(g$uf)
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_(19|20)\\d{2}\\.parquet$", full.names = TRUE), function(fp) {
  x <- as.data.table(read_parquet(fp, col_select = c("ANO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO",
                                                     "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))
  x[CD_CARGO %in% c("11", "12", "13") & SG_UF %in% ufs_alvo]
}), use.names = TRUE)
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_", CD_CARGO, "_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
mand <- merge(mand, unique(cand[, .(id_mandato, nome_urna_norm = norm(NM_URNA_CANDIDATO))]),
              by = "id_mandato", all.x = TRUE)
mand <- mand[sg_ue %in% unique(g$sg_ue)]
mp <- mand[cd_cargo != "12"]   # universo pareavel; o vice entra so no sinal de substituicao
setkey(mp, sg_ue, ano_eleicao, cd_cargo)

## ------------------------------------------------ pareamento
## Municipio, cargo e a eleicao que originou o exercicio; a identidade vem do nome, porque nenhuma das
## duas fontes do TCM-GO expoe CPF sem mascara.
g[, rid := .I]
g[, cd_alvo := fcase(cargo_bocel == "PREFEITO", "11", cargo_bocel == "VEREADOR", "13", default = NA_character_)]
vazio <- function() data.table(rid = integer(), id_mandato = character(), id_pessoa = character(), metodo = character())
sel <- function(d) if (!is.null(d) && nrow(d)) unique(d[, .(rid, id_mandato, id_pessoa, metodo)], by = "rid") else vazio()
feitos <- integer(0)
regra <- function(chave_g, chave_m, metodo, dt_m = mp) {
  r <- g[!rid %in% feitos & !is.na(cd_alvo) & !is.na(ano_eleicao)]
  if (!nrow(r)) return(vazio())
  m <- merge(r[, c("rid", chave_g), with = FALSE],
             dt_m[, c(chave_m, "id_mandato", "id_pessoa"), with = FALSE],
             by.x = chave_g, by.y = chave_m, allow.cartesian = TRUE)
  m <- m[, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (!nrow(m)) return(vazio())
  m[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)]
}
res <- list()
# (1) nome civil completo + municipio + cargo + eleicao
res[[1]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"),
                  c("nome_norm", "sg_ue", "cd_cargo", "ano_eleicao"), "nome_completo_eleicao")
feitos <- c(feitos, res[[1]]$rid)
# (2) nome da fonte igual ao nome de urna + municipio + cargo + eleicao
res[[2]] <- regra(c("nome_normalizado", "sg_ue", "cd_alvo", "ano_eleicao"),
                  c("nome_urna_norm", "sg_ue", "cd_cargo", "ano_eleicao"),
                  "nome_fonte=nome_urna_eleicao", mp[!is.na(nome_urna_norm)])
feitos <- c(feitos, res[[2]]$rid)
# (3) tokens do nome da fonte contidos no nome civil, mandato unico no municipio-cargo-eleicao
tok_regra <- function(col_m, metodo, min_tok, dt_m) {
  r <- g[!rid %in% feitos & !is.na(cd_alvo) & !is.na(ano_eleicao)]
  if (!nrow(r)) return(vazio())
  cc <- merge(r[, .(rid, sg_ue, cd_alvo, ano_eleicao, nome_normalizado)],
              dt_m[, c("sg_ue", "cd_cargo", "ano_eleicao", col_m, "id_mandato", "id_pessoa"), with = FALSE],
              by.x = c("sg_ue", "cd_alvo", "ano_eleicao"), by.y = c("sg_ue", "cd_cargo", "ano_eleicao"),
              allow.cartesian = TRUE)
  if (!nrow(cc)) return(vazio())
  ta <- lapply(strsplit(cc$nome_normalizado, " "), function(t) t[nchar(t) >= 3])
  tb <- lapply(strsplit(cc[[col_m]], " "), function(t) t[nchar(t) >= 3])
  cc[, ok := mapply(function(a, b) length(a) >= min_tok && all(a %in% b), ta, tb)]
  d <- cc[ok == TRUE][, if (uniqueN(id_mandato) == 1L) .SD[1] else NULL, by = rid]
  if (nrow(d)) d[, metodo := metodo][, .(rid, id_mandato, id_pessoa, metodo)] else vazio()
}
res[[3]] <- tok_regra("nome_norm", "tokens_nome_fonte_no_nome_civil", 2L, mp)
feitos <- c(feitos, res[[3]]$rid)
# (4) tokens do nome da fonte contidos no nome de urna
res[[4]] <- tok_regra("nome_urna_norm", "tokens_nome_fonte_no_nome_de_urna", 2L, mp[!is.na(nome_urna_norm)])
feitos <- c(feitos, res[[4]]$rid)
par <- rbindlist(lapply(res, sel), use.names = TRUE)[!duplicated(rid)]
cat("pareamentos por regra:\n"); print(par[, .N, by = metodo][order(-N)])
g[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
g[par$rid, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
g[, ano_eleicao_fonte := ano_eleicao]

## ------------------------------------------------ relacao com a chapa eleita e forma de saida
tokens <- function(x) lapply(strsplit(fcoalesce(x, ""), " "), function(t) t[nchar(t) >= 3L])
compat <- function(a, b) { ta <- tokens(a); tb <- tokens(b); mapply(function(u, v) length(u) >= 2L && all(u %in% v), ta, tb) }
urna12 <- unique(cand[CD_CARGO %in% c("11", "12"), .(id_mandato, urna_norm = norm(NM_URNA_CANDIDATO))])
chapa <- merge(mand[cd_cargo %in% c("11", "12"), .(id_mandato, id_pessoa, sg_ue, ano_eleicao, cd_cargo, nome_norm)],
               urna12, by = "id_mandato", all.x = TRUE)
tit <- chapa[cd_cargo == "11"][, if (.N == 1L) .SD else NULL, by = .(sg_ue, ano_eleicao)]
setnames(tit, c("id_mandato", "id_pessoa", "nome_norm", "urna_norm"), c("tit_id_mandato", "tit_id_pessoa", "tit_norm", "tit_urna"))
vic <- chapa[cd_cargo == "12"][, if (.N == 1L) .SD else NULL, by = .(sg_ue, ano_eleicao)]
setnames(vic, c("id_mandato", "id_pessoa", "nome_norm", "urna_norm"), c("vice_id_mandato", "vice_id_pessoa", "vice_norm", "vice_urna"))
pr <- g[cargo_bocel == "PREFEITO" & !is.na(ano_eleicao), .(rid, sg_ue, ano_eleicao, nome_normalizado, id_pessoa_bocel)]
pr <- merge(pr, tit[, .(sg_ue, ano_eleicao, tit_id_mandato, tit_id_pessoa, tit_norm, tit_urna)], by = c("sg_ue", "ano_eleicao"))
pr <- merge(pr, vic[, .(sg_ue, ano_eleicao, vice_id_pessoa, vice_norm, vice_urna)], by = c("sg_ue", "ano_eleicao"), all.x = TRUE)
pr[, e_titular := (!is.na(id_pessoa_bocel) & id_pessoa_bocel == tit_id_pessoa) |
     compat(nome_normalizado, tit_norm) | compat(tit_norm, nome_normalizado) |
     (!is.na(tit_urna) & (compat(nome_normalizado, tit_urna) | compat(tit_urna, nome_normalizado)))]
pr[, e_vice := !e_titular & !is.na(vice_norm) &
     ((!is.na(id_pessoa_bocel) & id_pessoa_bocel == vice_id_pessoa) |
      compat(nome_normalizado, vice_norm) | compat(vice_norm, nome_normalizado) |
      (!is.na(vice_urna) & (compat(nome_normalizado, vice_urna) | compat(vice_urna, nome_normalizado))))]
pr[, relacao := fcase(e_titular, "titular_eleito", e_vice, "vice_eleito",
                      !is.na(id_pessoa_bocel) & id_pessoa_bocel != tit_id_pessoa, "outra_pessoa_pareada",
                      default = "indeterminado")]
g[, `:=`(relacao_chapa_eleita = NA_character_, id_mandato_titular_substituido = NA_character_)]
g[pr$rid, relacao_chapa_eleita := pr$relacao]
g[pr[relacao == "vice_eleito"]$rid, id_mandato_titular_substituido := pr[relacao == "vice_eleito"]$tit_id_mandato]
print(pr[, .N, by = relacao][order(-N)])
g[, forma_saida := fifelse(!is.na(id_mandato_titular_substituido), "outro", "nao_observado")]
stopifnot(all(g$forma_saida %in% VOCAB))

## ------------------------------------------------ saida (mesmas colunas de data/tce_gestores.csv)
out <- g[, .(uf, tribunal, unidade_gestora, tipo_unidade, id_municipio_ibge, sg_ue, nome, cpf, cargo_fonte,
             cargo_bocel, data_inicio, data_fim, situacao_fonte, forma_saida, id_pessoa_bocel, id_mandato_bocel,
             metodo_pareamento, url, fonte, nome_municipio_fonte, exercicio, ano_eleicao_fonte, ano_eleicao,
             relacao_chapa_eleita, id_mandato_titular_substituido)]
setorder(out, uf, sg_ue, tipo_unidade, exercicio, nome, na.last = TRUE)
fwrite(out, "data/tce_gestores_c.csv", na = "NA", quote = TRUE)
stopifnot(identical(names(out), names(fread("data/tce_gestores.csv", nrows = 0))))

## cobertura: mandatos do BOCEL por UF/cargo/eleicao contra os pareados
alvo <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[
  cd_cargo %in% c("11", "13") & sg_uf %in% ufs_alvo, .(uf = sg_uf, cargo, ano_eleicao = as.integer(ano_eleicao), id_mandato)]
cob <- alvo[, .(n_bocel = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
pc <- unique(out[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel)])
pc <- merge(pc, alvo, by = "id_mandato")[, .(n_pareados = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
cob <- merge(cob, pc, by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob[, taxa := round(n_pareados / n_bocel, 4)]
setorder(cob, uf, cargo, ano_eleicao)
fwrite(cob, "data/tce_gestores_c_cobertura.csv", na = "NA")

## ------------------------------------------------ numeros
registrar_numero("tcec_n_registros_finais", nrow(out), script = script)
registrar_numero("tcec_n_pareados_linhas", out[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tcec_n_mandatos_pareados", out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
registrar_numero("tcec_n_pessoas_pareadas", out[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)], script = script)
registrar_numero("tcec_taxa_pareamento_linhas_com_cargo",
                 round(out[!is.na(id_mandato_bocel), .N] / max(1L, out[!is.na(cargo_bocel), .N]), 4), script = script)
registrar_numero("tcec_n_municipios_pareados", out[!is.na(id_mandato_bocel), uniqueN(sg_ue)], script = script)
for (rr in sort(unique(na.omit(out$relacao_chapa_eleita))))
  registrar_numero(paste0("tcec_n_prefeitura_relacao_", rr), out[relacao_chapa_eleita == rr, .N], script = script)
registrar_numero("tcec_n_saida_titular_observada_por_vice",
                 out[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)], script = script)
for (fs in VOCAB) registrar_numero(paste0("tcec_n_forma_saida_", fs),
                 out[!is.na(id_mandato_bocel) & forma_saida == fs, uniqueN(id_mandato_bocel)], script = script)
for (u in sort(unique(out$uf))) {
  registrar_numero(paste0("tcec_n_registros_uf_", u), out[uf == u, .N], script = script)
  registrar_numero(paste0("tcec_n_pareados_uf_", u), out[uf == u & !is.na(id_mandato_bocel), .N], script = script)
  registrar_numero(paste0("tcec_n_mandatos_pareados_uf_", u), out[uf == u & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
for (cg in sort(unique(na.omit(out$cargo_bocel)))) {
  registrar_numero(paste0("tcec_n_registros_cargo_", cg), out[cargo_bocel == cg, .N], script = script)
  registrar_numero(paste0("tcec_n_pareados_cargo_", cg), out[cargo_bocel == cg & !is.na(id_mandato_bocel), .N], script = script)
  registrar_numero(paste0("tcec_n_mandatos_pareados_cargo_", cg), out[cargo_bocel == cg & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
}
registrar_numero("tcec_n_registros_sem_cargo_eletivo_identificado", out[is.na(cargo_bocel), .N], script = script)
uc <- cob[, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados)), by = .(uf, cargo)][, taxa := round(n_pareados / n_bocel, 4)]
for (i in seq_len(nrow(uc))) registrar_numero(paste0("tcec_taxa_", uc$uf[i], "_", gsub("[^A-Z]", "", uc$cargo[i])), uc$taxa[i], script = script)
print(out[, .(linhas = .N, com_cargo = sum(!is.na(cargo_bocel)), pareadas = sum(!is.na(id_mandato_bocel)),
              municipios = uniqueN(sg_ue)), by = .(uf, tribunal, fonte)][order(uf, fonte)])
print(uc[order(-n_pareados)])
print(out[!is.na(id_mandato_bocel), .N, by = metodo_pareamento][order(-N)])
print(cob[n_pareados > 0][order(cargo, ano_eleicao)])
cat("33_tce_gestores_c: concluido —", nrow(out), "registros,",
    out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], "mandatos pareados\n")
sink()
