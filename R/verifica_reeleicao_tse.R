# verifica_reeleicao_tse.R — verificacao independente da frente SINAIS TSE
# (python/fetch_divulgacand_reeleicao.py, R/14_sinais_tse_exercicio.R -> data/sinais_tse_exercicio.csv)
# Reconta a partir dos arquivos de saida e das fontes brutas (data_raw/parquet/cand_<ANO>.parquet,
# data_raw/divulgacand/reeleicao_<ANO>.csv): chave unica, dominios, coerencia da candidatura
# seguinte com o cadastro (todas as linhas + amostra nominal de 20), st_reeleicao 'S' so onde a
# fonte marca, exercicio_confirmado_em, mudou_partido, integracao em mandatos.csv e comparacao
# com o ultimo registro de cada chave em output/numeros_assinatura.txt.
# Execucao: Rscript --vanilla R/verifica_reeleicao_tse.R   (a partir da raiz do repositorio)
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
dir.create("logs", showWarnings = FALSE)
logf <- "logs/verifica_reeleicao_tse.log"; sink(logf, split = TRUE)
cat("verifica_reeleicao_tse:", format(Sys.time()), "\n")
script <- "R/verifica_reeleicao_tse.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")
falhas <- character(); passou <- character()
chk <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else { falhas <<- c(falhas, msg); cat("FALHA:", msg, "\n") } }
ultimo <- function(chave) {
  l <- readLines("output/numeros_assinatura.txt")
  l <- l[startsWith(l, paste0(chave, " | "))]
  if (!length(l)) return(NA_character_)
  trimws(strsplit(l[length(l)], " | ", fixed = TRUE)[[1]][2])
}

## ---------------------------------------------------------------- arquivos
sin  <- fread("data/sinais_tse_exercicio.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
cols_prom <- c("id_mandato","id_pessoa","ano_eleicao","cd_cargo","cargo","esfera","unidade_posicao",
               "ano_candidatura_seguinte","candidatura_seguinte","mesmo_cargo_mesma_unidade",
               "cd_cargo_candidatura_seguinte","unidade_candidatura_seguinte","sq_candidatura_seguinte",
               "st_reeleicao","fonte_st_reeleicao","exercicio_confirmado_em","partido_mandato",
               "partido_candidatura_seguinte","mudou_partido","situacao_candidatura_seguinte",
               "resultado_candidatura_seguinte")
chk(identical(names(sin), cols_prom), "sinais_tse_exercicio.csv tem exatamente as 21 colunas prometidas, na ordem")
raw <- readLines("data/sinais_tse_exercicio.csv")
chk(!any(grepl(",,|,$|,\"\",", raw)), "nenhuma celula vazia (ausente = NA)")
rm(raw)

## ---------------------------------------------------------------- chave e dominios
invisible(checa_unica(as.data.frame(sin), "id_mandato"))
chk(nrow(sin) == nrow(mand), sprintf("uma linha por mandato: %d linhas = %d mandatos", nrow(sin), nrow(mand)))
chk(setequal(sin$id_mandato, mand$id_mandato), "conjunto de id_mandato identico ao de mandatos.csv")
sin[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo),
           ano_seg = as.integer(ano_candidatura_seguinte), cd_seg = as.integer(cd_cargo_candidatura_seguinte),
           cs = fifelse(is.na(candidatura_seguinte), NA, candidatura_seguinte == "TRUE"),
           mesmo = fifelse(is.na(mesmo_cargo_mesma_unidade), NA, mesmo_cargo_mesma_unidade == "TRUE"),
           mp = fifelse(is.na(mudou_partido), NA, mudou_partido == "TRUE"))]
in_set(sin$st_reeleicao, c("S", "N"), permitir_na = TRUE, nome = "st_reeleicao")
in_set(sin$fonte_st_reeleicao, c("consulta_cand", "divulgacand_api"), permitir_na = TRUE, nome = "fonte_st_reeleicao")
in_set(sin$candidatura_seguinte, c("TRUE", "FALSE"), permitir_na = TRUE, nome = "candidatura_seguinte")
in_set(sin$mesmo_cargo_mesma_unidade, c("TRUE", "FALSE"), permitir_na = TRUE, nome = "mesmo_cargo_mesma_unidade")
in_set(sin$mudou_partido, c("TRUE", "FALSE"), permitir_na = TRUE, nome = "mudou_partido")
in_set(sin$esfera, c("municipal", "estadual", "federal"), permitir_na = FALSE, nome = "esfera")
in_set(sin$cd_cargo, c(1:8, 11:13), permitir_na = FALSE, nome = "cd_cargo")
em_faixa(sin$ano_eleicao, 1998, 2024, permitir_na = FALSE, nome = "ano_eleicao")
em_faixa(sin$ano_seg, 2002, 2024, permitir_na = TRUE, nome = "ano_candidatura_seguinte")
passou <- c(passou, "dominios (st_reeleicao S/N, fonte, logicos, esfera, cd_cargo, anos) dentro do esperado")

# id_pessoa, ano, cargo, esfera, unidade e partido copiados de mandatos.csv
m2 <- mand[, .(id_mandato, id_pessoa_m = id_pessoa, ano_m = as.integer(ano_eleicao), cd_m = as.integer(cd_cargo),
               cargo_m = cargo, esfera_m = esfera, ue_m = unidade_posicao, partido_m = sg_partido,
               exercicio_confirmado, fonte_exercicio, reeleicao_declarada)]
j <- join_seguro(as.data.frame(sin), as.data.frame(m2), by = "id_mandato", cardinalidade = "one-to-one", tipo = "inner")
setDT(j)
chk(nrow(j) == nrow(sin), "join sinais x mandatos e 1:1 e completo")
chk(j[, all(id_pessoa == id_pessoa_m & ano_eleicao == ano_m & cd_cargo == cd_m & cargo == cargo_m &
                esfera == esfera_m & unidade_posicao == ue_m & partido_mandato == partido_m)],
    "id_pessoa, ano, cargo, esfera, unidade e partido_mandato identicos aos de mandatos.csv")

## ---------------------------------------------------------------- janela e coerencia interna
sin[, dur := fifelse(cd_cargo == 5L, 8L, 4L)]
sin[, esperado := ano_eleicao + dur]
chk(sin[, all(is.na(cs) == (esperado > 2024L))], "candidatura_seguinte e NA exatamente quando a eleicao seguinte cai depois de 2024")
chk(sin[cs %in% TRUE, all(ano_seg == esperado)], "ano_candidatura_seguinte = ano do mandato + 4 (8 para senador)")
chk(sin[cs %in% TRUE, all(!is.na(sq_candidatura_seguinte) & !is.na(cd_seg) & !is.na(unidade_candidatura_seguinte) &
                          !is.na(mesmo) & !is.na(partido_candidatura_seguinte))],
    "candidatura_seguinte TRUE => sq, cargo, unidade, mesmo_cargo e partido seguintes preenchidos")
chk(sin[!cs %in% TRUE, all(is.na(ano_seg) & is.na(sq_candidatura_seguinte) & is.na(cd_seg) & is.na(unidade_candidatura_seguinte) &
                           is.na(mesmo) & is.na(st_reeleicao) & is.na(fonte_st_reeleicao) & is.na(exercicio_confirmado_em) &
                           is.na(partido_candidatura_seguinte) & is.na(mp) & is.na(situacao_candidatura_seguinte) &
                           is.na(resultado_candidatura_seguinte))],
    "candidatura_seguinte FALSE ou NA => todas as colunas da candidatura seguinte em NA")
chk(sin[cs %in% TRUE, all(mesmo == (cd_seg == cd_cargo & unidade_candidatura_seguinte == unidade_posicao))],
    "mesmo_cargo_mesma_unidade = (cargo seguinte == cargo) & (unidade seguinte == unidade)")
chk(sin[, all(is.na(st_reeleicao) == is.na(fonte_st_reeleicao))], "st_reeleicao e fonte_st_reeleicao preenchidos juntos")
# exercicio_confirmado_em: so com mesmo cargo/unidade e 'S'; data = 15/ago do ano da eleicao seguinte
ec_esp <- sin[, mesmo %in% TRUE & st_reeleicao %in% "S"]
chk(all(!is.na(sin$exercicio_confirmado_em) == ec_esp), "exercicio_confirmado_em preenchido exatamente quando mesmo cargo/unidade e st_reeleicao = S")
chk(sin[ec_esp, all(exercicio_confirmado_em == sprintf("%d-08-15", ano_seg))], "exercicio_confirmado_em = <ano da eleicao seguinte>-08-15")
chk(sin[ec_esp, all(as.IDate(exercicio_confirmado_em) > as.IDate(sprintf("%d-01-01", ano_eleicao + 1L)) &
                     as.IDate(exercicio_confirmado_em) <= as.IDate(sprintf("%d-12-31", esperado)))],
    "exercicio_confirmado_em cai dentro da janela convencional do mandato")
# mudou_partido
chk(sin[, all(is.na(mp) == !(cs %in% TRUE & !is.na(partido_candidatura_seguinte) & !is.na(partido_mandato)))],
    "mudou_partido NA exatamente quando nao ha candidatura seguinte ou partido ausente")
chk(sin[!is.na(mp), all(mp == (partido_mandato != partido_candidatura_seguinte))], "mudou_partido = (partido_mandato != partido_candidatura_seguinte)")
# mudou_partido = TRUE por simples troca de nome/fusao da legenda (mesma agremiacao): quantificar
renome <- data.table(de = c("PMDB", "PRB", "PPS", "PR", "PTN", "PT DO B", "PEN", "PSDC", "PFL", "PMR", "PTdoB", "PSN", "PL", "PRONA", "PHS", "PATRI", "PATRIOTA", "PSL", "DEM", "PPB", "PPR", "PMN", "PST", "PGT", "PRP"),
                     para = c("MDB", "REPUBLICANOS", "CIDADANIA", "PL", "PODE", "AVANTE", "PATRIOTA", "DC", "DEM", "PRB", "AVANTE", "PHS", "PR", "PR", "PODE", "PATRIOTA", "PRD", "UNIÃO", "UNIÃO", "PP", "PPB", "PMN", "PL", "PL", "PATRIOTA"))
sin[, ren := paste(partido_mandato, partido_candidatura_seguinte) %in% paste(renome$de, renome$para)]
n_ren <- sin[mp %in% TRUE & ren, .N]
cat("\nmudou_partido = TRUE por troca de nome ou incorporacao da legenda (mesma agremiacao):", n_ren, "de", sin[mp %in% TRUE, .N], "\n")
print(sin[mp %in% TRUE & ren, .N, by = .(partido_mandato, partido_candidatura_seguinte)][order(-N)])
reg("sinais_verif_n_mudou_partido_por_renomeacao_legenda", n_ren)
reg("sinais_verif_prop_mudou_partido_por_renomeacao_legenda", round(n_ren / sin[mp %in% TRUE, .N], 4))

## ---------------------------------------------------------------- cadastro bruto (parquet) e cache DivulgaCand
ne <- function(x) fifelse(is.na(x) | x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue) fcase(cd_cargo %in% 11:13, sg_ue, cd_cargo %in% 1:2, "BR", default = sg_uf)
cols <- c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "NR_TURNO", "SG_UF", "SG_UE", "CD_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO",
          "NM_CANDIDATO", "NR_CPF_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "DT_NASCIMENTO", "SG_PARTIDO", "ST_REELEICAO")
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE), function(f) {
  x <- setDT(read_parquet(f)); for (v in setdiff(cols, names(x))) x[, (v) := NA_character_]
  x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)), ..cols]
}), use.names = TRUE)
cand[, `:=`(ano = as.integer(ANO_ELEICAO), cd = as.integer(CD_CARGO), nr_turno = as.integer(NR_TURNO),
            titulo = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)), cpf = ne(num_only(NR_CPF_CANDIDATO)),
            st = ne(ST_REELEICAO), partido = SG_PARTIDO)]
cand[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
cand[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
cand[!is.na(titulo), titulo := formatC(titulo, width = 12, flag = "0")]
cand[, ue_pos := unidade_posicao(cd, SG_UF, SG_UE)]
cand[, chave := paste(ano, ue_pos, cd, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
setorder(cand, chave, -nr_turno)
cand1 <- cand[!duplicated(chave)]
cat("\ncandidaturas ordinarias no parquet (uma por chave):", nrow(cand1), "\n")

api <- rbindlist(lapply(list.files("data_raw/divulgacand", pattern = "^reeleicao_\\d{4}\\.csv$", full.names = TRUE),
                        fread, colClasses = "character"), use.names = TRUE, fill = TRUE)
api[, ano := as.integer(ano_eleicao)]
api[, st_api := fcase(st_reeleicao %in% c("True", "TRUE", "true", "S"), "S", st_reeleicao %in% c("False", "FALSE", "false", "N"), "N", default = NA_character_)]
chk(!anyDuplicated(api[, .(ano, sq_candidato)]), "cache DivulgaCand: uma linha por (ano, sq_candidato)")
n_api_sem <- api[is.na(st_api), .N]
cat("cache DivulgaCand:", nrow(api), "linhas;", api[http == "200", .N], "com http 200;", n_api_sem, "sem st_REELEICAO (erro de rede ou 404)\n")
print(api[, .N, by = .(ano, http)][order(ano, http)])
reg("sinais_verif_n_cache_divulgacand", nrow(api))
reg("sinais_verif_n_cache_divulgacand_sem_resposta", n_api_sem)

## ---------------------------------------------------------------- st_reeleicao 'S' so quando a fonte marca (todas as linhas)
s_lido <- sin[cs %in% TRUE & !is.na(st_reeleicao),
              .(id_mandato, ano_seg, sq = sq_candidatura_seguinte, cd_seg, ue = unidade_candidatura_seguinte, st_reeleicao, fonte_st_reeleicao,
                partido_candidatura_seguinte)]
# a candidatura seguinte existe no parquet com esse sq, ano, cargo e unidade
c_seg <- unique(cand1[, .(ano_seg = ano, sq = SQ_CANDIDATO, cd_seg = cd, ue = ue_pos, st_arq = st, partido_arq = partido)],
                by = c("ano_seg", "sq", "cd_seg", "ue"))
todos <- sin[cs %in% TRUE, .(id_mandato, ano_seg, sq = sq_candidatura_seguinte, cd_seg, ue = unidade_candidatura_seguinte,
                             partido_candidatura_seguinte, st_reeleicao, fonte_st_reeleicao)]
jt <- join_seguro(as.data.frame(todos), as.data.frame(c_seg), by = c("ano_seg", "sq", "cd_seg", "ue"), cardinalidade = "many-to-one", tipo = "left")
setDT(jt)
n_sem_par <- jt[is.na(partido_arq), .N]
chk(n_sem_par == 0, sprintf("toda candidatura seguinte (ano, sq, cargo, unidade) existe no cadastro bruto (%d sem par)", n_sem_par))
chk(jt[, all(partido_candidatura_seguinte == partido_arq)], "partido_candidatura_seguinte = SG_PARTIDO da candidatura seguinte no cadastro")
# consulta_cand: st_reeleicao igual ao ST_REELEICAO do arquivo
jc <- jt[fonte_st_reeleicao %in% "consulta_cand"]
chk(jc[, all(st_reeleicao == st_arq)], sprintf("fonte consulta_cand: st_reeleicao identico ao ST_REELEICAO do parquet em %d linhas", nrow(jc)))
# divulgacand_api: st_reeleicao igual ao cache; o arquivo nao traz o campo
ja <- jt[fonte_st_reeleicao %in% "divulgacand_api"]
ja <- merge(ja, api[, .(ano_seg = ano, sq = sq_candidato, st_api)], by = c("ano_seg", "sq"), all.x = TRUE)
chk(ja[, all(is.na(st_arq))], "fonte divulgacand_api usada so onde o arquivo consulta_cand nao traz ST_REELEICAO")
chk(ja[, all(!is.na(st_api) & st_reeleicao == st_api)], sprintf("fonte divulgacand_api: st_reeleicao identico ao cache em %d linhas", nrow(ja)))
# nenhum 'S' sem fonte
chk(sin[st_reeleicao %in% "S", all(fonte_st_reeleicao %in% c("consulta_cand", "divulgacand_api"))], "todo 'S' tem fonte declarada")
# candidaturas seguintes (mesmo cargo/unidade) que ficaram sem st_reeleicao: por que
sem_st <- jt[is.na(st_reeleicao) & id_mandato %in% sin[mesmo %in% TRUE, id_mandato]]
sem_st <- merge(sem_st, api[, .(ano_seg = ano, sq = sq_candidato, http)], by = c("ano_seg", "sq"), all.x = TRUE)
cat("\ncandidaturas seguintes no mesmo cargo/unidade sem st_reeleicao, por ano e situacao no cache:\n")
print(sem_st[, .N, by = .(ano_seg, no_cache = !is.na(http), http)][order(ano_seg)])
reg("sinais_verif_n_mesmo_cargo_sem_st_reeleicao", nrow(sem_st))

## ---------------------------------------------------------------- anos em que ST_REELEICAO vem vazio ou so com 'N'
fill <- cand1[, .(n = .N, n_preenchido = sum(!is.na(st)), n_S = sum(st %in% "S")), by = ano][order(ano)]
vazios <- fill[n_preenchido == 0, ano]
chk(identical(paste(vazios, collapse = ";"), ultimo("sinais_anos_st_reeleicao_vazio_consulta_cand")),
    sprintf("anos sem ST_REELEICAO no consulta_cand recontados = registrados (%s)", paste(vazios, collapse = ";")))
chk(all(fill[n_preenchido > 0 & n_preenchido < n, .N] == 0), "em cada ano o ST_REELEICAO do arquivo esta todo preenchido ou todo vazio")
# taxa de 'S' entre quem concorreu ao MESMO cargo na MESMA unidade logo apos ser eleito
# (proxy de completude da marcacao: a maioria desses candidatos e titular em exercicio)
tx <- sin[mesmo %in% TRUE, .(n_mesmo_cargo = .N, n_st_lido = sum(!is.na(st_reeleicao)), n_S = sum(st_reeleicao %in% "S"),
                             n_N = sum(st_reeleicao %in% "N"), fonte = paste(sort(unique(na.omit(fonte_st_reeleicao))), collapse = ";")),
          by = .(ano_candidatura = ano_seg, esfera)][order(ano_candidatura, esfera)]
tx[, prop_S := round(n_S / n_st_lido, 3)]
cat("\nproporcao de 'S' entre candidaturas seguintes ao mesmo cargo/unidade, por ano da candidatura e esfera:\n"); print(tx)
fwrite(tx, "output/verificacao/sinais_st_reeleicao_prop_S_ano_esfera.csv", na = "NA")
for (i in seq_len(nrow(tx))) reg(sprintf("sinais_verif_prop_S_mesmo_cargo_%d_%s", tx$ano_candidatura[i], tx$esfera[i]), tx$prop_S[i])
subfill <- tx[prop_S < 0.6]
cat("\nanos x esfera com ST_REELEICAO preenchido mas 'S' abaixo de 60% dos que repetem cargo/unidade (marcacao incompleta):\n"); print(subfill)
reg("sinais_verif_anos_esfera_st_reeleicao_subpreenchido",
    paste(subfill[, paste0(ano_candidatura, "-", esfera, "(", prop_S, ")")], collapse = ";"))

## ---------------------------------------------------------------- candidatura_seguinte coerente com o cadastro (todas as linhas)
# identidade de cada mandato = titulo/cpf da propria candidatura de origem
ident <- cand1[, .(chave, titulo, cpf)]
mo <- sin[!is.na(cs), .(id_mandato, chave = sub("^M", "", id_mandato), ano_seg_esp = esperado, cs, id_pessoa)]
mo <- merge(mo, ident, by = "chave", all.x = TRUE)
chk(nrow(mo) == sin[!is.na(cs), .N] && !anyDuplicated(mo$id_mandato), "identidade (titulo/cpf) da candidatura de origem: uma por mandato")
# FALSE => nenhuma candidatura no ano seguinte com o mesmo titulo ou o mesmo cpf
ct <- unique(cand1[!is.na(titulo), .(ano, titulo)]); cc <- unique(cand1[!is.na(cpf), .(ano, cpf)])
mo[, tem_tit := !is.na(titulo) & paste(ano_seg_esp, titulo) %in% paste(ct$ano, ct$titulo)]
mo[, tem_cpf := !is.na(cpf) & paste(ano_seg_esp, cpf) %in% paste(cc$ano, cc$cpf)]
# 21/09/2026: a separacao de pessoas fundidas por documento (R/03, output/verificacao/fusao_documentos_separacao.csv)
# deixa o mesmo titulo em mandatos de pessoas distintas. O R/14 nao usa esse titulo como ponte, e a candidatura
# seguinte de mesmo titulo pode ser da outra pessoa. Sai da checagem so o titulo repetido entre pessoas separadas
sep_ids <- fread("output/verificacao/fusao_documentos_separacao.csv", colClasses = "character",
                 na.strings = c("", "NA"))$id_pessoa_esperado
tit_m <- merge(sin[, .(chave = sub("^M", "", id_mandato), id_pessoa)], ident, by = "chave")
tit_rep_m <- tit_m[!is.na(titulo), .(n = uniqueN(id_pessoa), so_sep = all(id_pessoa %chin% sep_ids)), by = titulo][n > 1L]
chk(all(tit_rep_m$so_sep), sprintf("titulo em mandatos de mais de uma pessoa so entre pessoas separadas da fusao (%d titulos)", nrow(tit_rep_m)))
reg("sinais_verif_n_titulos_repetidos_entre_pessoas_separadas", nrow(tit_rep_m))
mo[, tit_separado := id_pessoa %chin% sep_ids & titulo %chin% tit_rep_m$titulo]
reg("sinais_verif_n_cand_seguinte_false_titulo_de_pessoa_separada", mo[cs %in% FALSE & tem_tit & tit_separado, .N])
n_falso_tit <- mo[cs %in% FALSE & tem_tit & !tit_separado, .N]
chk(n_falso_tit == 0,sprintf("candidatura_seguinte FALSE => nenhuma candidatura no ano seguinte com o mesmo titulo (%d violacoes)", n_falso_tit))
# mesmo cpf mas titulo diferente: o 14 da precedencia ao titulo; quando o cadastro do TSE repete um titulo
# em varios candidatos do mesmo municipio (2004 sobretudo), a candidatura vai para a pessoa do titulo
n_falso_neg <- mo[cs %in% FALSE & !tem_tit & tem_cpf, .N]
cat("\ncandidatura_seguinte FALSE com candidatura de mesmo CPF (titulo diferente) no ano seguinte:", n_falso_neg, "\n")
reg("sinais_verif_n_cand_seguinte_perdida_por_titulo_divergente_mesmo_cpf", n_falso_neg)
if (n_falso_neg > 0) {
  fn <- mo[cs %in% FALSE & !tem_tit & tem_cpf]
  fn2 <- merge(fn[, .(id_mandato, id_pessoa, ano = ano_seg_esp, titulo, cpf, tem_tit, tem_cpf)],
               cand1[, .(ano, cpf, titulo_seg = titulo, nome_seg = NM_CANDIDATO, cd_seg = cd, ue_seg = ue_pos, sq_seg = SQ_CANDIDATO)], by = c("ano", "cpf"))
  # o titulo da candidatura seguinte aparece em mais de um candidato do mesmo ano (defeito do cadastro)?
  tit_rep <- cand1[!is.na(titulo), .(n_cand = uniqueN(paste(NR_CANDIDATO, SQ_CANDIDATO, ue_pos))), by = .(ano, titulo)]
  fn2 <- merge(fn2, tit_rep[, .(ano, titulo_seg = titulo, n_cand_mesmo_titulo = n_cand)], by = c("ano", "titulo_seg"), all.x = TRUE)
  print(fn2[1:min(15, nrow(fn2)), .(id_mandato, ano, cpf, titulo, titulo_seg, nome_seg, n_cand_mesmo_titulo)]); print(fn2[, .N, by = .(ano, titulo_repetido = n_cand_mesmo_titulo > 1)])
  reg("sinais_verif_n_cand_seguinte_perdida_titulo_repetido_no_cadastro", fn2[n_cand_mesmo_titulo > 1, .N])
  fwrite(fn2, "output/verificacao/sinais_tse_falsos_negativos_candidatura_seguinte.csv", na = "NA")
}
# TRUE => a candidatura seguinte pertence a mesma pessoa (id_pessoa da candidatura seguinte pelo titulo/cpf da candidatura de origem)
seg_id <- sin[cs %in% TRUE, .(id_mandato, chave_seg = paste(ano_seg, unidade_candidatura_seguinte, cd_seg, NA, sq_candidatura_seguinte, sep = "_"),
                              ano_seg, sq = sq_candidatura_seguinte, cd_seg, ue = unidade_candidatura_seguinte)]
seg_id <- merge(seg_id, unique(cand1[, .(ano_seg = ano, sq = SQ_CANDIDATO, cd_seg = cd, ue = ue_pos, titulo_seg = titulo, cpf_seg = cpf, nome_seg = NM_CANDIDATO, nasc_seg = DT_NASCIMENTO)],
                              by = c("ano_seg", "sq", "cd_seg", "ue")), by = c("ano_seg", "sq", "cd_seg", "ue"), all.x = TRUE)
seg_id <- merge(seg_id, mo[, .(id_mandato, titulo, cpf)], by = "id_mandato")
seg_id[, mesma_chave := (!is.na(titulo) & !is.na(titulo_seg) & titulo == titulo_seg) | (!is.na(cpf) & !is.na(cpf_seg) & cpf == cpf_seg)]
n_true <- nrow(seg_id); n_chave_direta <- seg_id[mesma_chave %in% TRUE, .N]
cat(sprintf("\ncandidatura_seguinte TRUE: %d; com titulo ou cpf identico ao da candidatura de origem: %d (%.2f%%); restantes ligados por outra chave da pessoa (cpf/nome+nascimento via fecho transitivo do 03)\n",
            n_true, n_chave_direta, 100 * n_chave_direta / n_true))
reg("sinais_verif_n_cand_seguinte_true", n_true)
reg("sinais_verif_n_cand_seguinte_mesmo_titulo_ou_cpf", n_chave_direta)
chk(n_chave_direta / n_true > 0.95, "mais de 95% das candidaturas seguintes compartilham titulo ou cpf com a candidatura de origem")
# os restantes: a pessoa e a mesma pelo nome + nascimento?
resto <- seg_id[!mesma_chave %in% TRUE]
orig <- merge(resto[, .(id_mandato, chave = sub("^M", "", id_mandato))], cand1[, .(chave, nome_o = NM_CANDIDATO, nasc_o = DT_NASCIMENTO)], by = "chave")
resto <- merge(resto, orig[, .(id_mandato, nome_o, nasc_o)], by = "id_mandato")
nn <- function(x) gsub(" +", " ", trimws(gsub("[^A-Z ]", "", stri_trans_general(toupper(x), "Latin-ASCII"))))
resto[, mesmo_nome_nasc := nn(nome_o) == nn(nome_seg) & nasc_o == nasc_seg]
cat("restantes:", nrow(resto), "| mesmo nome+nascimento:", resto[mesmo_nome_nasc %in% TRUE, .N], "| divergem em nome ou nascimento:", resto[!mesmo_nome_nasc %in% TRUE, .N], "\n")
reg("sinais_verif_n_cand_seguinte_so_nome_nascimento", resto[mesmo_nome_nasc %in% TRUE, .N])
reg("sinais_verif_n_cand_seguinte_chave_indireta", resto[!mesmo_nome_nasc %in% TRUE, .N])
if (resto[!mesmo_nome_nasc %in% TRUE, .N]) print(resto[!mesmo_nome_nasc %in% TRUE, .(id_mandato, nome_o, nasc_o, nome_seg, nasc_seg, titulo, titulo_seg, cpf, cpf_seg)][1:15])

## ---------------------------------------------------------------- amostra nominal de 20 mandatos (conferencia a olho no log)
am <- sin[!is.na(cs)][sample(.N, 20)]
am <- merge(am, pess[, .(id_pessoa, nome)], by = "id_pessoa")
am <- merge(am, mo[, .(id_mandato, titulo, cpf, tem_tit, tem_cpf)], by = "id_mandato")
am <- merge(am, seg_id[, .(id_mandato, nome_seg, titulo_seg, mesma_chave)], by = "id_mandato", all.x = TRUE)
am[, ano_seg_esp := esperado]
am[, cadastro_ano_seguinte := fifelse(tem_tit | tem_cpf, "candidatura encontrada pelo titulo/cpf", "nenhuma candidatura pelo titulo/cpf")]
am[, coerente := (cs & !is.na(nome_seg)) | (!cs & !(tem_tit | tem_cpf))]
cat("\namostra de 20 mandatos: candidatura_seguinte x cadastro bruto do ano seguinte\n")
print(am[, .(id_mandato, nome = substr(nome, 1, 28), cargo, ano_seg_esp, candidatura_seguinte, cadastro_ano_seguinte,
             nome_seg = substr(nome_seg, 1, 28), mesma_chave, st_reeleicao, fonte_st_reeleicao, exercicio_confirmado_em, coerente)])
fwrite(am[, .(id_mandato, nome, cargo, unidade_posicao, ano_eleicao, ano_seg_esp, candidatura_seguinte, mesmo_cargo_mesma_unidade,
              cadastro_ano_seguinte, nome_seg, titulo, titulo_seg, mesma_chave, st_reeleicao, fonte_st_reeleicao, exercicio_confirmado_em,
              partido_mandato, partido_candidatura_seguinte, mudou_partido, coerente)],
       "output/verificacao/sinais_tse_amostra_20_mandatos.csv", na = "NA")
chk(all(am$coerente), "amostra de 20 mandatos: candidatura_seguinte coerente com o cadastro bruto")

## ---------------------------------------------------------------- integracao em mandatos.csv
tem_tse <- grepl("tse_reeleicao", j$fonte_exercicio)
n_tse <- sum(tem_tse)
chk(all(tem_tse == !is.na(j$exercicio_confirmado_em)), sprintf("fonte_exercicio contem 'tse_reeleicao' exatamente nos %d mandatos com exercicio_confirmado_em", n_tse))
chk(j[tem_tse, all(!is.na(exercicio_confirmado) & exercicio_confirmado >= exercicio_confirmado_em)],
    "exercicio_confirmado em mandatos.csv >= exercicio_confirmado_em (a data mais recente prevalece)")
# 12/09/2026: alinhado as fontes de confirmacao que entraram depois de 29/08 (MUNIC ampliada, CNPJ, presenca em
# plenario, TCE-AC). A igualdade de datas vale quando o TSE e a unica fonte; o vocabulario e conferido por fonte
FONTES_EXERC_V <- c("ibge_munic", "ibge_munic_ampliado", "tse_reeleicao", "receita_cnpj", "sapl_presenca", "tce_ac")
chk(j[tem_tse & fonte_exercicio == "tse_reeleicao", all(exercicio_confirmado == exercicio_confirmado_em)],
    "so com o TSE como fonte, exercicio_confirmado = exercicio_confirmado_em")
chk(all(unlist(strsplit(j[!is.na(exercicio_confirmado), fonte_exercicio], ";", fixed = TRUE)) %in% FONTES_EXERC_V),
    "fonte_exercicio no vocabulario (ibge_munic, ibge_munic_ampliado, tse_reeleicao, receita_cnpj, sapl_presenca, tce_ac)")
chk(j[, all(is.na(exercicio_confirmado) == is.na(fonte_exercicio))], "exercicio_confirmado e fonte_exercicio preenchidos juntos")
print(j[, .N, by = .(esfera, fonte_exercicio)][order(esfera, -N)])
# reeleicao_declarada do mandato de destino (mesma origem TSE) vs st_reeleicao lido
dest <- j[mesmo %in% TRUE & fonte_st_reeleicao %in% "consulta_cand", .(id_pessoa, ano_eleicao = ano_seg, cd_cargo, unidade_posicao, st_reeleicao)]
dest <- merge(dest, m2[, .(id_pessoa = id_pessoa_m, ano_eleicao = ano_m, cd_cargo = cd_m, unidade_posicao = ue_m, reeleicao_declarada)],
              by = c("id_pessoa", "ano_eleicao", "cd_cargo", "unidade_posicao"))
n_div <- dest[st_reeleicao != reeleicao_declarada, .N]
cat("\nreeleitos comparaveis:", nrow(dest), "| divergencias st_reeleicao x reeleicao_declarada:", n_div, "\n")
chk(identical(as.character(nrow(dest)), ultimo("sinais_check_n_reeleitos_comparaveis")) && identical(as.character(n_div), ultimo("sinais_check_n_divergentes_reeleicao_declarada")),
    "reeleitos comparaveis e divergencias recontados = registrados")

## ---------------------------------------------------------------- recontagem x numeros_assinatura
rec <- list(sinais_n_mandatos = nrow(sin),
            sinais_n_mandatos_janela_seguinte = sin[!is.na(cs), .N],
            sinais_n_candidatura_seguinte = sin[cs %in% TRUE, .N],
            sinais_n_mesmo_cargo_unidade = sin[mesmo %in% TRUE, .N],
            sinais_n_mesmo_cargo_st_reeleicao_lido = sin[mesmo %in% TRUE & !is.na(st_reeleicao), .N],
            sinais_n_exercicio_confirmado = sin[!is.na(exercicio_confirmado_em), .N],
            sinais_n_mesmo_cargo_st_reeleicao_N = sin[mesmo %in% TRUE & st_reeleicao %in% "N", .N],
            sinais_n_st_reeleicao_fonte_api = sin[fonte_st_reeleicao %in% "divulgacand_api", .N],
            sinais_n_mudou_partido = sin[mp %in% TRUE, .N],
            bocel_mandatos_com_exercicio_confirmado = sum(!is.na(mand$exercicio_confirmado)))
for (k in names(rec)) {
  r <- ultimo(k)
  chk(identical(as.character(rec[[k]]), r), sprintf("%s recontado (%s) = registrado (%s)", k, rec[[k]], r))
  reg(paste0("verif_", k), rec[[k]])
}
for (a in sort(unique(cand1$ano))) {
  n_s <- cand1[ano == a & st %in% "S", .N]
  # o registro do 14 soma S do arquivo e da API; reconta com a API
  n_s_api <- api[ano == a & st_api %in% "S" & sq_candidato %in% cand1[ano == a, SQ_CANDIDATO], .N]
  tot <- if (a %in% vazios) n_s_api else n_s
  chk(identical(as.character(tot), ultimo(sprintf("sinais_n_st_reeleicao_S_%d", a))), sprintf("n de 'S' em %d recontado (%d) = registrado (%s)", a, tot, ultimo(sprintf("sinais_n_st_reeleicao_S_%d", a))))
}
reg("sinais_verif_n_exercicio_confirmado_mandatos_fonte_tse", n_tse)

## ---------------------------------------------------------------- relatorio
cat("\nPASSOU:", length(passou), "| FALHOU:", length(falhas), "\n"); if (length(falhas)) print(falhas)
fora <- c("veracidade do ST_REELEICAO declarado ao TSE (o campo e autodeclaracao no registro de candidatura)",
          sprintf("'N' nao e evidencia de nao exercicio nos anos x esfera em que a marcacao e incompleta: %s", paste(subfill[, paste0(ano_candidatura, "-", esfera)], collapse = ", ")),
          sprintf("mudou_partido conta troca de nome ou incorporacao da legenda como mudanca (%d dos %d TRUE)", n_ren, sin[mp %in% TRUE, .N]),
          sprintf("identidade de pessoa entre candidaturas (regra do 03, titulo > cpf > nome+nascimento); %d candidaturas seguintes de mesmo CPF ficam fora porque o cadastro do TSE atribui o titulo a outro candidato", n_falso_neg))
gravar_relatorio_verificacao(alvo = "data/sinais_tse_exercicio.csv", script = script, passou = passou, falhou = falhas, fora_de_cobertura = fora)
cat("verifica_reeleicao_tse: concluido\n"); sink()
if (length(falhas)) quit(status = 1)
