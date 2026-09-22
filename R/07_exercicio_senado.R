# 07_exercicio_senado.R — posse, exercicio e forma de saida dos senadores (legislaturas 51-57)
# Entrada:  data_raw/senado/lista_legislatura_<n>.json, mandatos_<codigo>.json, detalhe_<codigo>.json
#           (coletados por R/coleta/senado.R da API de Dados Abertos do Senado)
#           data/mandatos.csv (cd_cargo 5) e data/pessoas.csv (BOCEL v1.0)
# Saida:    data/exercicio_senado.csv|parquet  (uma linha por exercicio de mandato)
#           output/verificacao/pareamento_senado_legislatura.csv
#           output/verificacao/senado_forma_saida.csv
#           output/numeros_assinatura.txt (registrar_numero)
# Execucao: cd ~/bocel && Rscript --vanilla R/07_exercicio_senado.R
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
  library(jsonlite)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root  <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
raw   <- file.path(root, "data_raw", "senado")
outd  <- file.path(root, "data")
verd  <- file.path(root, "output", "verificacao")
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "07_exercicio_senado.R")
logf  <- file.path(root, "logs", "07_exercicio_senado.log")
sink(logf, split = TRUE)
cat("07_exercicio_senado.R —", format(Sys.time()), "\n")

BASE_URL <- "https://legis.senado.leg.br/dadosabertos"
LEGS     <- 51:57
leg_de_ano <- function(ano) 51L + (as.integer(ano) - 1998L) %/% 4L
ano_de_leg <- function(leg) 1998L + (as.integer(leg) - 51L) * 4L

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a[1]
as_lista <- function(x) if (is.null(x)) list() else if (!is.null(names(x))) list(x) else x
chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("[^A-Z ]", "", x)
  gsub(" +", " ", trimws(x))
}

## ---------------------------------------------------------------- 1. listas por legislatura
listas <- rbindlist(lapply(LEGS, function(n) {
  f <- file.path(raw, sprintf("lista_legislatura_%d.json", n))
  stopifnot(file.exists(f))
  d <- fromJSON(f, simplifyVector = FALSE)
  p <- as_lista(d$ListaParlamentarLegislatura$Parlamentares$Parlamentar)
  rbindlist(lapply(p, function(x) {
    id <- x$IdentificacaoParlamentar
    data.table(legislatura_lista = n,
               codigo_senador   = chr(id$CodigoParlamentar),
               nome_parlamentar = chr(id$NomeParlamentar),
               nome_completo    = chr(id$NomeCompletoParlamentar),
               sexo             = chr(id$SexoParlamentar),
               partido_atual    = chr(id$SiglaPartidoParlamentar))
  }))
}))
parl <- unique(listas[, .(codigo_senador, nome_parlamentar, nome_completo, sexo, partido_atual)])
parl <- parl[!duplicated(codigo_senador)]
cat("parlamentares (codigos unicos) nas listas 51-57:", nrow(parl), "\n")

## ---------------------------------------------------------------- 2. detalhe: data de nascimento
det <- rbindlist(lapply(parl$codigo_senador, function(cod) {
  f <- file.path(raw, sprintf("detalhe_%s.json", cod))
  if (!file.exists(f)) return(data.table(codigo_senador = cod, dt_nascimento_senado = NA_character_,
                                         nome_completo_det = NA_character_))
  d <- fromJSON(f, simplifyVector = FALSE)$DetalheParlamentar$Parlamentar
  data.table(codigo_senador = cod,
             dt_nascimento_senado = chr(d$DadosBasicosParlamentar$DataNascimento),
             nome_completo_det = chr(d$IdentificacaoParlamentar$NomeCompletoParlamentar))
}))
parl <- merge(parl, det, by = "codigo_senador", all.x = TRUE)
parl[is.na(nome_completo) & !is.na(nome_completo_det), nome_completo := nome_completo_det]
parl[, nome_completo_det := NULL]
parl[, nome_norm := norm_nome(nome_completo)]
n_sem_nasc <- parl[is.na(dt_nascimento_senado), .N]
cat("parlamentares sem data de nascimento na API:", n_sem_nasc, "\n")

## ---------------------------------------------------------------- 3. mandatos e exercicios
ler_mandatos <- function(cod) {
  f <- file.path(raw, sprintf("mandatos_%s.json", cod))
  if (!file.exists(f)) return(NULL)
  d <- fromJSON(f, simplifyVector = FALSE)$MandatoParlamentar$Parlamentar
  ms <- as_lista(d$Mandatos$Mandato)
  if (!length(ms)) return(NULL)
  rbindlist(lapply(ms, function(m) {
    ex <- as_lista(m$Exercicios$Exercicio)
    base <- data.table(
      codigo_senador   = cod,
      codigo_mandato   = chr(m$CodigoMandato),
      sg_uf            = chr(m$UfParlamentar),
      leg_primeira     = as.integer(chr(m$PrimeiraLegislaturaDoMandato$NumeroLegislatura)),
      leg_segunda      = as.integer(chr(m$SegundaLegislaturaDoMandato$NumeroLegislatura)),
      participacao     = chr(m$DescricaoParticipacao),
      titular_codigo   = chr(m$Titular$CodigoParlamentar),
      titular_nome     = chr(m$Titular$NomeParlamentar),
      n_exercicios     = length(ex))
    if (!length(ex)) {
      return(cbind(base, data.table(codigo_exercicio = NA_character_, data_inicio_exercicio = NA_character_,
                                    data_fim_exercicio = NA_character_, sigla_causa = NA_character_,
                                    causa_afastamento = NA_character_)))
    }
    cbind(base, rbindlist(lapply(ex, function(e) data.table(
      codigo_exercicio      = chr(e$CodigoExercicio),
      data_inicio_exercicio = chr(e$DataInicio),
      data_fim_exercicio    = chr(e$DataFim),
      sigla_causa           = trimws(chr(e$SiglaCausaAfastamento)),
      causa_afastamento     = chr(e$DescricaoCausaAfastamento)))))
  }), fill = TRUE)
}
mand_all <- rbindlist(lapply(parl$codigo_senador, ler_mandatos), fill = TRUE)
cat("linhas mandato x exercicio (todas as legislaturas):", nrow(mand_all), "\n")
# a chave do frente: mandatos cuja primeira legislatura e 51..57 (eleicoes 1998-2022)
mand <- mand_all[leg_primeira %in% LEGS]
mand[, ano_eleicao_ref := ano_de_leg(leg_primeira)]
mand[, condicao := fcase(participacao == "Titular", "titular",
                         grepl("Suplente", participacao), "suplente",
                         default = NA_character_)]
cat("mandatos (leg 51-57) por participacao:\n"); print(mand[, .N, by = .(participacao, condicao)])

## ---------------------------------------------------------------- 4. forma de saida (vocabulario fechado)
# Vocabulario: fim_regular, renuncia, falecimento, cassacao, afastamento, licenca,
#              nao_tomou_posse, suplente_efetivado, outro; NA = exercicio em curso (sem DataFim).
# 'Retorno do titular' (RET) encerra o exercicio do suplente e nao e saida do titular: vai para
# 'outro' com a causa preservada em causa_afastamento. 'suplente_efetivado' marca o exercicio de
# suplente que terminou com o fim do mandato (TER), isto e, o suplente ficou ate o fim.
mapa_saida <- c(TER = "fim_regular", REN = "renuncia", FAL = "falecimento",
                PER = "cassacao", CAS = "cassacao", AFO = "afastamento", DJ = "afastamento",
                LSP = "licenca", LS = "licenca", LP = "licenca", LCS = "licenca", RET = "outro")
mand[, forma_saida := fcase(
  n_exercicios == 0L & condicao == "titular", "nao_tomou_posse",
  n_exercicios == 0L, NA_character_,
  is.na(data_fim_exercicio), NA_character_,
  condicao == "suplente" & sigla_causa == "TER", "suplente_efetivado",
  !is.na(sigla_causa) & sigla_causa %in% names(mapa_saida), unname(mapa_saida[sigla_causa]),
  default = "outro")]
# suplente que nunca exerceu nao entra (nao assumiu); titular sem exercicio entra (nao_tomou_posse)
mand <- mand[!(condicao == "suplente" & n_exercicios == 0L)]
mand <- mand[!is.na(condicao) | n_exercicios > 0L]
mand <- merge(mand, parl, by = "codigo_senador", all.x = TRUE)

## ---------------------------------------------------------------- 5. pareamento com o BOCEL
bocel_m <- fread(file.path(outd, "mandatos.csv"), na.strings = "NA", encoding = "UTF-8")
bocel_p <- fread(file.path(outd, "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
bocel_p[, nome_norm := norm_nome(nome)]
bocel_p[, dt_nascimento := as.character(dt_nascimento)]
sen_bocel <- merge(bocel_m[cd_cargo == 5L, .(id_mandato, id_pessoa, ano_eleicao, sg_uf)],
                 bocel_p[, .(id_pessoa, nome_bocel = nome, nome_norm, dt_nascimento)], by = "id_pessoa")
sen_bocel[, leg_primeira := leg_de_ano(ano_eleicao)]
cat("senadores eleitos no BOCEL (cd_cargo 5):", nrow(sen_bocel), "\n")

# 5a. pessoa: nome completo normalizado + data de nascimento, contra TODO o cadastro do BOCEL
# (suplentes que assumiram e estao no BOCEL por outro cargo tambem recebem id_pessoa)
chave_p <- bocel_p[!is.na(nome_norm) & !is.na(dt_nascimento), .(id_pessoa, nome_norm, dt_nascimento)]
chave_p <- chave_p[!duplicated(paste(nome_norm, dt_nascimento))]
pess_sen <- unique(mand[, .(codigo_senador, nome_norm, dt_nascimento_senado)])
pess_sen <- merge(pess_sen, chave_p, by.x = c("nome_norm", "dt_nascimento_senado"),
                  by.y = c("nome_norm", "dt_nascimento"), all.x = TRUE)
pess_sen[!is.na(id_pessoa), metodo_pareamento := "nome_nascimento"]

# 5b. fallback (so titulares, chave mandato): nome + UF + ano da eleicao contra os eleitos do BOCEL
tit <- unique(mand[condicao == "titular", .(codigo_senador, nome_norm, dt_nascimento_senado,
                                              sg_uf, leg_primeira)])
fb1 <- merge(tit, sen_bocel[, .(nome_norm, sg_uf, leg_primeira, id_pessoa_fb = id_pessoa)],
             by = c("nome_norm", "sg_uf", "leg_primeira"))
fb1 <- fb1[!duplicated(codigo_senador), .(codigo_senador, id_pessoa_fb, metodo_fb = "nome_uf_ano")]
# 5c. fallback 2: data de nascimento + UF + ano (nome grafado diferente nas duas fontes)
fb2 <- merge(tit[!is.na(dt_nascimento_senado)],
             sen_bocel[, .(dt_nascimento, sg_uf, leg_primeira, id_pessoa_fb = id_pessoa)],
             by.x = c("dt_nascimento_senado", "sg_uf", "leg_primeira"),
             by.y = c("dt_nascimento", "sg_uf", "leg_primeira"))
fb2 <- fb2[!duplicated(codigo_senador), .(codigo_senador, id_pessoa_fb, metodo_fb = "nascimento_uf_ano")]
fb <- rbindlist(list(fb1, fb2))[!duplicated(codigo_senador)]
pess_sen <- merge(pess_sen, fb, by = "codigo_senador", all.x = TRUE)
pess_sen[is.na(id_pessoa) & !is.na(id_pessoa_fb), `:=`(id_pessoa = id_pessoa_fb, metodo_pareamento = metodo_fb)]
pess_sen[, c("id_pessoa_fb", "metodo_fb") := NULL]
# um id_pessoa nao pode apontar para dois codigos de senador
dup_id <- pess_sen[!is.na(id_pessoa), .N, by = id_pessoa][N > 1]
if (nrow(dup_id)) { cat("AVISO: id_pessoa com mais de um codigo de senador:\n"); print(dup_id) }

mand <- merge(mand, pess_sen[, .(codigo_senador, id_pessoa, metodo_pareamento)],
              by = "codigo_senador", all.x = TRUE)
# id_mandato: titular pareado, mesma pessoa, mesmo ano de eleicao de referencia (e mesma UF)
mand <- merge(mand, sen_bocel[, .(id_pessoa, ano_eleicao_ref = ano_eleicao, sg_uf, id_mandato)],
              by = c("id_pessoa", "ano_eleicao_ref", "sg_uf"), all.x = TRUE)
mand[condicao != "titular", id_mandato := NA_character_]

## ---------------------------------------------------------------- 6. taxa de pareamento por legislatura
# denominador: eleitos do BOCEL (27/54; 53 em 2018 por excecao documentada)
cob <- merge(sen_bocel[, .(n_bocel = .N), by = .(ano_eleicao, leg_primeira)],
             mand[condicao == "titular" & !is.na(id_mandato), .(n_pareados = uniqueN(id_mandato)),
                  by = .(leg_primeira)], by = "leg_primeira", all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob <- merge(cob, mand[condicao == "titular", .(n_titulares_api = uniqueN(codigo_mandato)),
                       by = leg_primeira], by = "leg_primeira", all.x = TRUE)
cob <- merge(cob, mand[condicao == "suplente", .(n_suplentes_exerceram = uniqueN(codigo_senador)),
                       by = leg_primeira], by = "leg_primeira", all.x = TRUE)
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
setcolorder(cob, c("leg_primeira", "ano_eleicao"))
cat("\nTaxa de pareamento por legislatura (eleitos BOCEL x titulares da API):\n"); print(cob)
fwrite(cob, file.path(verd, "pareamento_senado_legislatura.csv"))

nao_pareados <- sen_bocel[!id_mandato %in% mand$id_mandato]
cat("\nEleitos do BOCEL sem mandato pareado na API:", nrow(nao_pareados), "\n")
if (nrow(nao_pareados)) print(nao_pareados[, .(ano_eleicao, sg_uf, nome_bocel, dt_nascimento)])
fwrite(nao_pareados, file.path(verd, "senado_eleitos_bocel_nao_pareados.csv"))
# uma linha por codigo_mandato (um senador pode ter dois mandatos de titular na mesma legislatura:
# Carlos Favaro MT 56, mandatos 582 e 592) — correcao do verificador, 28/08/2026
tit_sem_bocel <- unique(mand[condicao == "titular" & is.na(id_mandato),
                           .(codigo_senador, codigo_mandato, nome_completo, sg_uf, leg_primeira, dt_nascimento_senado)])
cat("\nTitulares da API sem eleito correspondente no BOCEL:", nrow(tit_sem_bocel), "\n")
if (nrow(tit_sem_bocel)) print(tit_sem_bocel)
fwrite(tit_sem_bocel, file.path(verd, "senado_titulares_api_sem_bocel.csv"))

## ---------------------------------------------------------------- 7. saida
mand[, `:=`(fonte = "senado_api",
            url_fonte = sprintf("%s/senador/%s/mandatos", BASE_URL, codigo_senador),
            legislatura = leg_primeira)]
setorder(mand, leg_primeira, sg_uf, condicao, codigo_senador, data_inicio_exercicio, na.last = TRUE)
saida <- mand[, .(id_pessoa, id_mandato, codigo_senador, nome_senado = nome_completo,
                  nome_parlamentar, dt_nascimento = dt_nascimento_senado, sexo,
                  sg_uf, legislatura, leg_segunda, ano_eleicao_ref, codigo_mandato, condicao,
                  participacao, titular_codigo, titular_nome, codigo_exercicio,
                  data_inicio_exercicio, data_fim_exercicio, sigla_causa, causa_afastamento,
                  forma_saida, metodo_pareamento, fonte, url_fonte)]
stopifnot(!anyDuplicated(saida[!is.na(codigo_exercicio), .(codigo_mandato, codigo_exercicio)]))
stopifnot(all(is.na(saida$forma_saida) | saida$forma_saida %in%
              c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
                "nao_tomou_posse", "suplente_efetivado", "outro")))
fwrite(saida, file.path(outd, "exercicio_senado.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
write_parquet(saida, file.path(outd, "exercicio_senado.parquet"))
cat("\nexercicio_senado:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

fs <- saida[, .N, by = .(condicao, forma_saida)][order(condicao, -N)]
cat("\nForma de saida por condicao:\n"); print(fs)
fwrite(fs, file.path(verd, "senado_forma_saida.csv"))

## ---------------------------------------------------------------- 8. registro de numeros
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
reg("senado_n_senadores_api_listas_51_57", nrow(parl))
reg("senado_n_senadores_api_com_mandato_51_57", uniqueN(mand$codigo_senador))
reg("senado_n_titulares_api_51_57", uniqueN(mand[condicao == "titular", codigo_senador]))
reg("senado_n_mandatos_titular_api_51_57", uniqueN(mand[condicao == "titular", codigo_mandato]))
reg("senado_n_suplentes_que_exerceram_51_57", uniqueN(mand[condicao == "suplente", codigo_senador]))
reg("senado_n_sem_data_nascimento_api", n_sem_nasc)
reg("senado_n_eleitos_bocel_cd5", nrow(sen_bocel))
reg("senado_n_pareados_bocel_pessoas", uniqueN(mand[!is.na(id_pessoa), codigo_senador]))
reg("senado_n_pareados_bocel_mandatos", uniqueN(mand[!is.na(id_mandato), id_mandato]))
reg("senado_n_eleitos_bocel_nao_pareados", nrow(nao_pareados))
reg("senado_n_titulares_api_sem_bocel", nrow(tit_sem_bocel))
reg("senado_n_linhas_exercicio", nrow(saida))
for (i in seq_len(nrow(cob))) {
  reg(sprintf("senado_taxa_pareamento_leg%d_%d", cob$leg_primeira[i], cob$ano_eleicao[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
}
for (m in unique(na.omit(saida$metodo_pareamento))) {
  reg(paste0("senado_n_pareados_metodo_", m), uniqueN(saida[metodo_pareamento == m, codigo_senador]))
}
fs_tot <- saida[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fs_tot))) {
  reg(paste0("senado_n_forma_saida_", fs_tot$forma_saida[i] %||% "NA_em_exercicio"), fs_tot$N[i])
}
gravar_relatorio_verificacao(
  alvo = "data/exercicio_senado.csv", script = script,
  passou = c("chave (codigo_mandato, codigo_exercicio) unica",
             "forma_saida no vocabulario fechado ou NA",
             sprintf("taxa de pareamento minima por legislatura = %.4f", min(cob$taxa_pareamento))),
  fora_de_cobertura = c("validade das datas de exercicio informadas pelo Senado",
                        "homonimos sem data de nascimento na API (pareamento por nome+UF+ano)"))
cat("\n07_exercicio_senado: concluido —", format(Sys.time()), "\n")
sink()
