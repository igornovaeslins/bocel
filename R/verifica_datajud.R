# verifica_datajud.R — verificacao cetica da frente 18 (DataJud/CNJ: processos de cassacao)
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_datajud.R
# Le os jsonl brutos (data_raw/datajud), os dois csv de saida do 18, a correspondencia TSE-IBGE
# e a amostra conferida contra a API ao vivo (output/verificacao/amostra_api.csv, gerada por python), e
# reconta os numeros contra output/numeros_assinatura.txt.
set.seed(20260829)
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_datajud.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")
passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  invisible(r)
}
ultimo <- function(chave) {
  l <- readLines("output/numeros_assinatura.txt"); l <- l[startsWith(l, paste0(chave, " | "))]
  if (!length(l)) return(NA_character_); trimws(strsplit(l[length(l)], "|", fixed = TRUE)[[1]][2])
}

## ------------------------------------------------------------ carga
proc  <- fread("data/datajud_processos_cassacao.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
sinal <- fread("data/datajud_sinal_unidade_eleicao.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
tse_ibge <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
files <- list.files("data_raw/datajud", pattern = "\\.jsonl$", full.names = TRUE)

## ------------------------------------------------------------ 1. estrutura e chave
ok("colunas proc", stopifnot(all(c("tribunal","grau","numero_processo","classe","assuntos","cargo_assunto","relevante_cassacao",
  "data_ajuizamento","ano_ajuizamento","uf","orgao_julgador","id_municipio_ibge","sg_ue","n_movimentos","ultimo_movimento",
  "data_ultimo_movimento","indicio_cassacao","data_indicio","movimento_indicio") %in% names(proc))))
ok("chave unica proc", checa_unica(as.data.frame(proc), c("tribunal", "numero_processo")))
ok("chave unica sinal", checa_unica(as.data.frame(sinal), c("uf", "sg_ue", "cargo_assunto", "eleicao_ref")))
ok("28 tribunais", stopifnot(uniqueN(proc$tribunal) == 28L))
ok("tribunais no conjunto", in_set(proc$tribunal, c(paste0("TRE-", c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")), "TSE"), permitir_na = FALSE))
ok("uf = tribunal", stopifnot(all(proc$uf == fifelse(proc$tribunal == "TSE", "BR", sub("^TRE-", "", proc$tribunal)))))
ok("grau em G1/G2/SUP", in_set(proc$grau, c("G1", "G2", "SUP"), permitir_na = FALSE))
ok("logicos", { in_set(proc$relevante_cassacao, c("TRUE","FALSE"), permitir_na = FALSE); in_set(proc$indicio_cassacao, c("TRUE","FALSE"), permitir_na = FALSE) })
# a API foi filtrada por dataAjuizamento >= 2002, mas a base traz anos fora da faixa (erro de digitacao na origem)
reg("verif_dj_n_ano_fora_2002_2026", proc[!(as.integer(ano_ajuizamento) %in% 2002:2026), .N])
reg("verif_dj_anos_fora_faixa", paste(sort(unique(proc[!(as.integer(ano_ajuizamento) %in% 2002:2026), ano_ajuizamento])), collapse = ";"))
reg("verif_dj_n_sinal_eleicao_ref_fora_faixa", sinal[!(as.integer(eleicao_ref) %in% 2002:2026), .N])
ok("ano_ajuizamento 2002-2026 (fora = pendencia registrada)", em_faixa(as.integer(proc[as.integer(ano_ajuizamento) %in% 2002:2026, ano_ajuizamento]), 2002, 2026, permitir_na = FALSE))
ok("numero_processo 20 digitos", stopifnot(all(grepl("^[0-9]{20}$", proc$numero_processo))))
# segmento J=6 (eleitoral) no numero CNJ: posicao 14
ok("numero CNJ segmento eleitoral", stopifnot(all(substr(proc$numero_processo, 14, 14) == "6")))
reg("verif_dj_n_processos", nrow(proc)); reg("verif_dj_n_tribunais", uniqueN(proc$tribunal))

## ------------------------------------------------------------ 2. bruto x saida
n_bruto <- sum(vapply(files, function(f) length(readLines(f, warn = FALSE)), integer(1)))
ok_lidos <- vapply(sub("\\.jsonl$", ".jsonl.ok", files), function(f) as.integer(readLines(f)), integer(1))
ok("linhas jsonl = contagem .ok", stopifnot(all(ok_lidos == vapply(files, function(f) length(readLines(f, warn = FALSE)), integer(1)))))
reg("verif_dj_n_linhas_bruto", n_bruto)
# chaves brutas (tribunal, numero, grau) para medir o que o unique() do 18 descarta
chaves <- rbindlist(lapply(files, function(f) {
  l <- readLines(f, warn = FALSE)
  data.table(tribunal = toupper(sub('.*"tribunal": "([^"]+)".*', "\\1", l)),
             grau = sub('.*"grau": "([^"]+)".*', "\\1", l),
             numero = sub('.*"numeroProcesso": "([^"]+)".*', "\\1", l),
             classe = sub('.*"classe": "([^"]+)".*', "\\1", l),
             atualizacao = sub('.*"dataHoraUltimaAtualizacao": "([^"]+)".*', "\\1", l))
}))
chaves[, ultima := atualizacao == max(atualizacao), by = .(tribunal, numero, grau)]
reg("verif_dj_n_versoes_repetidas_mesmo_grau", chaves[, .N, by = .(tribunal, numero, grau)][N > 1, .N])
ok("bruto: n = jsonl", stopifnot(nrow(chaves) == n_bruto))
ok("unique(tribunal,numero) do bruto = nrow(proc)", stopifnot(uniqueN(chaves, by = c("tribunal", "numero")) == nrow(proc)))
dup <- chaves[, .N, by = .(tribunal, numero)][N > 1]
reg("verif_dj_n_numeros_repetidos_no_bruto", nrow(dup))
dup_g <- merge(chaves, dup[, .(tribunal, numero)], by = c("tribunal", "numero"))[, .(graus = paste(sort(unique(grau)), collapse = "+"), classes = uniqueN(classe)), by = .(tribunal, numero)]
reg("verif_dj_repetidos_por_graus", paste(sprintf("%s=%d", names(table(dup_g$graus)), as.integer(table(dup_g$graus))), collapse = ";"))
reg("verif_dj_repetidos_com_classe_distinta", dup_g[classes > 1, .N])
reg("verif_dj_n_por_grau", paste(sprintf("%s=%d", names(table(proc$grau)), as.integer(table(proc$grau))), collapse = ";"))

## ------------------------------------------------------------ 3. classes e assuntos
classes_esperadas <- c("Ação de Investigação Judicial Eleitoral", "Ação de Impugnação de Mandato Eletivo", "Recurso contra Expedição de Diploma",
  "Representação", "Representação Especial", "Representação Criminal/Notícia de Crime", "Representação Criminal", "Representação por Excesso de Prazo")
ok("classes no conjunto", in_set(proc$classe, classes_esperadas, permitir_na = FALSE))
for (cl in unique(proc$classe)) {
  k <- paste0("dj_n_", gsub("[^a-z]", "_", tolower(iconv(cl, to = "ASCII//TRANSLIT"))))
  ok(paste("assinatura", k), stopifnot(as.integer(ultimo(k)) == proc[classe == cl, .N]))
}
reg("verif_dj_n_aije", proc[classe == "Ação de Investigação Judicial Eleitoral", .N])
reg("verif_dj_n_aime", proc[classe == "Ação de Impugnação de Mandato Eletivo", .N])
reg("verif_dj_n_rced", proc[classe == "Recurso contra Expedição de Diploma", .N])
reg("verif_dj_n_assuntos_vazios", proc[is.na(assuntos) | assuntos == "", .N])
reg("verif_dj_n_com_cargo_assunto", proc[!is.na(cargo_assunto), .N])
reg("verif_dj_cargos_assunto", paste(sprintf("%s=%d", names(sort(table(proc$cargo_assunto), decreasing = TRUE))[1:8], as.integer(sort(table(proc$cargo_assunto), decreasing = TRUE))[1:8]), collapse = ";"))

## ------------------------------------------------------------ 4. regra de relevancia
norm <- function(s) toupper(stringi::stri_trans_general(s, "Latin-ASCII"))
rel <- grepl("INVESTIGACAO JUDICIAL|IMPUGNACAO DE MANDATO|EXPEDICAO DE DIPLOMA", norm(proc$classe)) |
       grepl("CASSA|CAPTACAO ILICITA|PERDA D[OE] MANDATO|ABUSO", norm(proc$assuntos))
ok("relevante_cassacao reproduzido", stopifnot(all(rel == (proc$relevante_cassacao == "TRUE"))))
ok("AIJE/AIME/RCED sempre relevantes", stopifnot(all(proc[classe %in% classes_esperadas[1:3], relevante_cassacao] == "TRUE")))
ok("assinatura dj_n_relevantes_cassacao", stopifnot(as.integer(ultimo("dj_n_relevantes_cassacao")) == sum(rel)))
reg("verif_dj_n_relevantes", sum(rel))
reg("verif_dj_n_relevantes_por_classe", paste(sprintf("%s=%d", names(table(proc$classe[rel])), as.integer(table(proc$classe[rel]))), collapse = ";"))

## ------------------------------------------------------------ 5. regra de indicio, reparse do bruto (amostra)
ok("indicio TRUE tem data e movimento", stopifnot(proc[indicio_cassacao == "TRUE", all(!is.na(data_indicio) & !is.na(movimento_indicio))]))
ok("indicio FALSE sem data", stopifnot(proc[indicio_cassacao == "FALSE", all(is.na(data_indicio) & is.na(movimento_indicio))]))
ok("movimento_indicio bate a regra", stopifnot(proc[indicio_cassacao == "TRUE", all(grepl("CASSA|PERDA D[OE] MANDATO|PROCEDEN", movimento_indicio) & !grepl("IMPROCEDEN|NAO PROVIMENTO|NEGADO", movimento_indicio))]))
ok("assinatura dj_n_com_indicio_cassacao", stopifnot(as.integer(ultimo("dj_n_com_indicio_cassacao")) == proc[indicio_cassacao == "TRUE", .N]))
reg("verif_dj_n_com_indicio", proc[indicio_cassacao == "TRUE", .N])
reg("verif_dj_taxa_indicio_entre_relevantes", round(proc[relevante_cassacao == "TRUE", mean(indicio_cassacao == "TRUE")], 4))
reg("verif_dj_movimentos_indicio_top", paste(sprintf("%s=%d", names(sort(table(trimws(proc$movimento_indicio)), decreasing = TRUE))[1:6], as.integer(sort(table(trimws(proc$movimento_indicio)), decreasing = TRUE))[1:6]), collapse = ";"))
# reparse independente de 3.000 linhas brutas sorteadas
set.seed(20260828)
am <- rbindlist(lapply(files, function(f) { l <- readLines(f, warn = FALSE); data.table(linha = l[sample.int(length(l), min(110L, length(l)))]) }))
am <- am[sample.int(nrow(am), min(3000L, nrow(am)))]
rep <- rbindlist(lapply(am$linha, function(s) {
  j <- fromJSON(s, simplifyVector = FALSE)
  nm <- vapply(j$movimentos, function(m) norm(paste(m$nome, paste(unlist(m$complementos), collapse = " "))), character(1))
  ind <- grepl("CASSA|PERDA D[OE] MANDATO|PROCEDEN", nm) & !grepl("IMPROCEDEN|NAO PROVIMENTO|NEGADO", nm)
  data.table(tribunal = toupper(j$tribunal), numero_processo = j$numeroProcesso, grau = j$grau, atualizacao = j$dataHoraUltimaAtualizacao, ind = any(ind), nmov = length(j$movimentos),
             ibge = as.character(if (is.null(j$codigoMunicipioIBGE)) NA else j$codigoMunicipioIBGE),
             ano = as.integer(substr(j$dataAjuizamento, 1, 4)))
}))
# so a versao mais recente de cada (tribunal, numero, grau) e comparavel com a saida do 18
rep <- merge(rep, chaves[ultima == TRUE, .(tribunal, numero_processo = numero, grau, atualizacao, ultima)], by = c("tribunal", "numero_processo", "grau", "atualizacao"))
rep <- unique(rep, by = c("tribunal", "numero_processo"))
cmp <- as.data.table(join_seguro(as.data.frame(rep), as.data.frame(proc[, .(tribunal, numero_processo, grau_p = grau, indicio_cassacao, n_movimentos, id_municipio_ibge, ano_ajuizamento)]),
                   by = c("tribunal", "numero_processo"), cardinalidade = "one-to-one"))
mesmo_grau <- cmp[grau == grau_p]
ok("reparse: indicio bate (mesmo grau)", stopifnot(all(mesmo_grau$ind == (mesmo_grau$indicio_cassacao == "TRUE"))))
ok("reparse: n_movimentos bate (mesmo grau)", stopifnot(all(mesmo_grau$nmov == as.integer(mesmo_grau$n_movimentos))))
ok("reparse: ibge e ano batem (mesmo grau)", stopifnot(all(mesmo_grau$ibge == mesmo_grau$id_municipio_ibge, na.rm = TRUE), all(mesmo_grau$ano == as.integer(mesmo_grau$ano_ajuizamento))))
reg("verif_dj_reparse_n", nrow(cmp)); reg("verif_dj_reparse_n_mesmo_grau", nrow(mesmo_grau))

## ------------------------------------------------------------ 6. amostra contra a API ao vivo (python, 20 com + 20 sem indicio)
# 29/08/2026: a amostra vinha de um diretorio temporario de sessao, gerado a mao e inexistente
# na reexecucao, o que derrubava o verificador sem defeito no dado. Passou a sair de
# python/confere_datajud_ao_vivo.py, com semente fixa, gravada dentro do repositorio.
AM_API <- "output/verificacao/datajud_amostra_api.csv"
if (!file.exists(AM_API)) system2("python3", c("python/confere_datajud_ao_vivo.py", "20"))
am_api <- fread(AM_API, colClasses = "character")
ok("API: 40 processos amostrados", stopifnot(nrow(am_api) == 40L, sum(am_api$indicio_csv == "TRUE") == 20L))
ok("API: todos encontrados", stopifnot(all(as.integer(am_api$n_hits) >= 1L)))
ok("API: indicio identico nos 40", stopifnot(all(toupper(am_api$indicio_api) == am_api$indicio_csv)))
ok("API: n_movimentos identico nos 40", stopifnot(all(as.integer(am_api$nmov_api) == as.integer(am_api$nmov_csv))))
ok("API: classe identica nos 40", stopifnot(all(am_api$classe_api == am_api$classe_csv)))
ok("API: ibge identico nos 40", stopifnot(all(am_api$ibge_api == am_api$ibge_csv)))
reg("verif_dj_api_n_amostra", nrow(am_api)); reg("verif_dj_api_n_indicio_bate", sum(toupper(am_api$indicio_api) == am_api$indicio_csv))
reg("verif_dj_api_n_com_2_hits", sum(as.integer(am_api$n_hits) > 1L))
reg("verif_dj_api_n_classe_ibge_batem", sum(am_api$classe_csv == am_api$classe_api & am_api$ibge_csv == am_api$ibge_api))

## ------------------------------------------------------------ 7. eleicao_ref por cargo
ano <- as.integer(proc$ano_ajuizamento)
mun <- grepl("PREFEITO|VEREADOR", toupper(proc$cargo_assunto)); mun[is.na(mun)] <- FALSE
# regra corrigida em 2026-08-29: cargo municipal -> calendario municipal; cargo geral -> calendario geral;
# sem cargo -> ultima eleicao de qualquer tipo ate o ano do ajuizamento
eref_esp <- fifelse(mun, ano - (ano %% 4L), fifelse(is.na(proc$cargo_assunto), ano - (ano %% 2L), ano - ((ano - 2L) %% 4L)))
proc[, eleicao_ref_v := eref_esp]
# reproduzo o sinal a partir de proc
sinal_v <- proc[relevante_cassacao == "TRUE" & (!is.na(sg_ue) | uf != "BR"),
                .(n_processos = .N, n_aije = sum(classe == "Ação de Investigação Judicial Eleitoral"), n_aime = sum(classe == "Ação de Impugnação de Mandato Eletivo"),
                  n_rced = sum(classe == "Recurso contra Expedição de Diploma"), n_com_indicio_cassacao = sum(indicio_cassacao == "TRUE")),
                by = .(uf, sg_ue, id_municipio_ibge, cargo_assunto, eleicao_ref = as.character(eleicao_ref_v))]
cmp_s <- as.data.table(join_seguro(as.data.frame(sinal_v), as.data.frame(sinal[, .(uf, sg_ue, id_municipio_ibge, cargo_assunto, eleicao_ref, n_processos_s = as.integer(n_processos), n_aije_s = as.integer(n_aije),
                                       n_aime_s = as.integer(n_aime), n_rced_s = as.integer(n_rced), n_ind_s = as.integer(n_com_indicio_cassacao))]),
                     by = c("uf", "sg_ue", "id_municipio_ibge", "cargo_assunto", "eleicao_ref"), cardinalidade = "one-to-one"))
ok("sinal reproduzido linha a linha", stopifnot(nrow(cmp_s) == nrow(sinal), all(cmp_s$n_processos == cmp_s$n_processos_s), all(cmp_s$n_aije == cmp_s$n_aije_s),
  all(cmp_s$n_aime == cmp_s$n_aime_s), all(cmp_s$n_rced == cmp_s$n_rced_s), all(cmp_s$n_com_indicio_cassacao == cmp_s$n_ind_s)))
ok("sinal soma = relevantes com municipio ou TRE", stopifnot(sum(as.integer(sinal$n_processos)) == proc[relevante_cassacao == "TRUE" & (!is.na(sg_ue) | uf != "BR"), .N]))
ok("eleicao_ref municipal multiplo de 4", stopifnot(all(as.integer(sinal[grepl("PREFEITO|VEREADOR", toupper(cargo_assunto)), eleicao_ref]) %% 4L == 0L)))
ok("eleicao_ref geral = 2 mod 4", stopifnot(all(as.integer(sinal[!is.na(cargo_assunto) & !grepl("PREFEITO|VEREADOR", toupper(cargo_assunto)), eleicao_ref]) %% 4L == 2L)))
ok("eleicao_ref sem cargo = ano par", stopifnot(all(as.integer(sinal[is.na(cargo_assunto), eleicao_ref]) %% 2L == 0L)))
# sem cargo e em zona eleitoral (G1), o ajuizamento concentra-se em ano municipal: a maioria do sinal sem cargo deve cair em ano multiplo de 4
reg("verif_dj_sinal_sem_cargo_por_eleicao", paste(sprintf("%s=%d", names(table(sinal[is.na(cargo_assunto), eleicao_ref])), as.integer(table(sinal[is.na(cargo_assunto), eleicao_ref]))), collapse = ";"))
ok("eleicao_ref em faixa (excluidos os anos fora de faixa da origem)", em_faixa(as.integer(sinal[as.integer(eleicao_ref) %in% 1998:2026, eleicao_ref]), 2002, 2026, permitir_na = FALSE))
ok("n_com_indicio <= n_processos", stopifnot(all(as.integer(sinal$n_com_indicio_cassacao) <= as.integer(sinal$n_processos))))
ok("primeira_data_indicio so com indicio", stopifnot(all((as.integer(sinal$n_com_indicio_cassacao) > 0) == !is.na(sinal$primeira_data_indicio))))
# ajuizamento ANTES da eleicao de referencia municipal (ano da eleicao, mes < outubro): a regra por ano usa a eleicao do proprio ano
rel_mun <- proc[relevante_cassacao == "TRUE" & mun & !is.na(sg_ue)]
reg("verif_dj_n_sinal_mun_ajuizado_antes_de_out_do_ano_eleitoral", rel_mun[ano_ajuizamento == as.character(eleicao_ref_v) & substr(data_ajuizamento, 6, 7) < "10", .N])
reg("verif_dj_n_sinal_mun_ajuizado_no_ano_eleitoral", rel_mun[ano_ajuizamento == as.character(eleicao_ref_v), .N])
reg("verif_dj_n_sinal_cargo_NA", sinal[is.na(cargo_assunto), sum(as.integer(n_processos))])
reg("verif_dj_n_sinal_cargo_NA_em_zona_eleitoral", proc[relevante_cassacao == "TRUE" & is.na(cargo_assunto) & grepl("ZONA", toupper(orgao_julgador)), .N])
reg("verif_dj_n_sinal_linhas", nrow(sinal))
reg("verif_dj_sinal_por_eleicao", paste(sprintf("%s=%d", names(table(sinal$eleicao_ref)), as.integer(table(sinal$eleicao_ref))), collapse = ";"))

## ------------------------------------------------------------ 8. municipio do orgao julgador e TSE-IBGE
ok("assinatura dj_n_com_municipio", stopifnot(as.integer(ultimo("dj_n_com_municipio")) == proc[!is.na(sg_ue), .N]))
ok("sg_ue vem da tabela TSE-IBGE", stopifnot(nrow(as.data.table(join_seguro(as.data.frame(proc[!is.na(sg_ue), .(id_municipio_ibge, sg_ue)]), as.data.frame(tse_ibge[, .(id_municipio_ibge, sg_ue_t = sg_ue)]), by = "id_municipio_ibge", cardinalidade = "many-to-one"))[sg_ue != sg_ue_t]) == 0L))
ok("tse_ibge: ibge unico", checa_unica(as.data.frame(tse_ibge), "id_municipio_ibge"))
sem_sg <- proc[is.na(sg_ue)]
reg("verif_dj_n_sem_sg_ue", nrow(sem_sg))
reg("verif_dj_sem_sg_ue_por_tribunal", paste(sprintf("%s=%d", names(table(sem_sg$tribunal)), as.integer(table(sem_sg$tribunal))), collapse = ";"))
reg("verif_dj_n_ibge_NA", proc[is.na(id_municipio_ibge), .N])
reg("verif_dj_n_ibge_nao_na_tabela", sem_sg[!is.na(id_municipio_ibge), uniqueN(id_municipio_ibge)])
reg("verif_dj_ibge_nao_na_tabela_lista", paste(head(sem_sg[!is.na(id_municipio_ibge), unique(id_municipio_ibge)], 15), collapse = ";"))
# UF do codigo IBGE (2 digitos) x UF do tribunal
ufcod <- c("12"="AC","27"="AL","16"="AP","13"="AM","29"="BA","23"="CE","53"="DF","32"="ES","52"="GO","21"="MA","51"="MT","50"="MS","31"="MG","15"="PA","25"="PB","41"="PR","26"="PE","22"="PI","33"="RJ","24"="RN","43"="RS","11"="RO","14"="RR","42"="SC","35"="SP","28"="SE","17"="TO")
tre <- proc[uf != "BR" & !is.na(id_municipio_ibge)]
ok("UF do IBGE = UF do TRE", stopifnot(all(ufcod[substr(tre$id_municipio_ibge, 1, 2)] == tre$uf)))
reg("verif_dj_n_uf_ibge_diverge_tre", sum(ufcod[substr(tre$id_municipio_ibge, 1, 2)] != tre$uf, na.rm = TRUE))
# G2 (tribunal) tem municipio = capital? proporcao de G2 cujo municipio e capital
reg("verif_dj_n_G2", proc[grau == "G2", .N])
reg("verif_dj_n_G2_com_sg_ue", proc[grau == "G2" & !is.na(sg_ue), .N])
reg("verif_dj_n_G2_relevante_no_sinal", proc[grau == "G2" & relevante_cassacao == "TRUE" & !is.na(sg_ue), .N])
reg("verif_dj_n_zona_eleitoral", proc[grepl("ZONA", toupper(orgao_julgador)), .N])
reg("verif_dj_n_sinal_processos_G2", proc[grau == "G2" & relevante_cassacao == "TRUE" & (!is.na(sg_ue) | uf != "BR"), .N])

## ------------------------------------------------------------ 9. documentacao
nota <- paste(readLines("docs/NOTA_DE_COBERTURA.md", warn = FALSE), collapse = " ")
ok("nota: API nao expoe partes", stopifnot(grepl("DataJud", nota) && grepl("n[aã]o exp[oõ]em as partes", nota)))
ok("nota: sinal agregado sem atribuicao a mandato", stopifnot(grepl("sinal agregado", nota) && grepl("sem atribui[cç][aã]o a mandato", nota)))
readme <- paste(readLines("docs/README.md", warn = FALSE), collapse = " ")
ok("readme cita os dois csv", stopifnot(grepl("datajud_processos_cassacao.csv", readme), grepl("datajud_sinal_unidade_eleicao.csv", readme)))
lc <- paste(readLines("docs/LIVRO_DE_CODIGOS.md", warn = FALSE), collapse = " ")
ok("livro: n linhas proc", stopifnot(grepl(format(nrow(proc), big.mark = "."), lc)))
ok("livro: n linhas sinal", stopifnot(grepl(format(nrow(sinal), big.mark = "."), lc)))
ok("assinatura dj_n_processos", stopifnot(as.integer(ultimo("dj_n_processos")) == nrow(proc)))
ok("assinatura dj_n_tribunais", stopifnot(as.integer(ultimo("dj_n_tribunais")) == 28L))

## ------------------------------------------------------------ relatorio
dir.create("output/verificacao", showWarnings = FALSE, recursive = TRUE)
writeLines(c("# verifica_datajud — relatorio", paste("data:", Sys.Date()), "", "## passou", paste("-", passou), "", "## falhou", if (length(falhou)) paste("-", falhou) else "- (nenhum)"),
           "output/verificacao/verifica_datajud.md")
reg("verif_dj_n_checks_passaram", length(passou)); reg("verif_dj_n_checks_falharam", length(falhou))
# 05/09/2026: alem do .md, o relatorio JSON padrao de verificacao (antes so o markdown era gravado)
gravar_relatorio_verificacao(alvo = "data/datajud_processos_cassacao.csv + data/datajud_sinal_unidade_eleicao.csv", script = script,
                             passou = passou, falhou = falhou,
                             fora_de_cobertura = c("pertinencia juridica de tratar Representacao com assunto Abuso como litigio de cassacao",
                                                   "competencia real de cada orgao julgador; verificada so a reproducao mecanica das regras e a fidelidade a API"))
cat("verifica_datajud:", length(passou), "passaram,", length(falhou), "falharam\n")
if (length(falhou)) { cat("FALHAS:", paste(falhou, collapse = " | "), "\n"); quit(status = 1) }
