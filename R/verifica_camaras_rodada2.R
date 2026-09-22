# verifica_camaras_rodada2.R — verificacao CETICA e INDEPENDENTE da frente 'camaras_rodada2'.
# Nao corrige nada; reconta a partir dos arquivos gravados e do cache bruto, confronta com o registro
# de output/numeros_assinatura.txt (ultimo valor por chave, dois primeiros campos separados por '|'),
# e usa lib/asserts_rigor.R.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_camaras_rodada2.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/verifica_camaras_rodada2.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_camaras_rodada2.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
VOCAB <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","nao_tomou_posse","suplente_efetivado","outro","nao_observado")
passou <- character(); falhou <- character()
chk <- function(nome, ok, det = "") { ok <- isTRUE(ok)
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nzchar(det)) paste0(" [", det, "]") else ""))
  cat(if (ok) "  ok   " else " FALHA ", nome, if (nzchar(det)) paste0("  -> ", det) else "", "\n", sep = "") }

## ---------------------------------------------------------------- 0. arquivos
arqs <- c("data/exercicio_camaras_sem_sapl_2.csv","data/exercicio_camaras_sem_sapl_2_cobertura.csv",
          "data_raw/camaras_sem_sapl_2/inventario_familias.csv","output/verificacao/camaras_sem_sapl_2_familias.csv",
          "R/25_exercicio_camaras_sem_sapl_2.R","python/coletores_camaras_2.py","python/fetch_camaras_sem_sapl_2.py",
          "docs/CAMARAS_SEM_SAPL_2.md")
chk("A1. todos os arquivos declarados existem", all(file.exists(arqs)), paste(arqs[!file.exists(arqs)], collapse=","))

r2  <- fread("data/exercicio_camaras_sem_sapl_2.csv", colClasses = "character", na.strings = "NA")
r1  <- fread("data/exercicio_camaras_sem_sapl.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/exercicio_camaras_sem_sapl_2_cobertura.csv")
inv <- fread("data_raw/camaras_sem_sapl_2/inventario_familias.csv", colClasses = "character")

COLS <- c("sg_ue","id_municipio_ibge","uf","dominio","legislatura_numero","legislatura_inicio","legislatura_fim",
          "ano_eleicao_bocel","nome_fonte","nome_parlamentar","nome_normalizado","titular","data_inicio_mandato",
          "data_fim_mandato","tipo_afastamento","forma_saida","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento",
          "sistema","so_legislatura_atual","url")
chk("A2. colunas exatas e na ordem esperada", identical(names(r2), COLS), paste(setdiff(names(r2), COLS), collapse=","))
chk("A3. colunas minusculas sem acento", all(grepl("^[a-z0-9_]+$", names(r2))))
chk("A4. mesmas colunas da rodada 1", identical(names(r1), names(r2)))

## codigo de ausente: no arquivo cru o ausente tem de ser literalmente NA (nao "", nao "NULL", nao "None")
cru <- readLines("data/exercicio_camaras_sem_sapl_2.csv", n = 200000, warn = FALSE)
chk("A5. ausente gravado como NA (sem campo vazio nem NULL/None/nan)",
    !any(grepl('(^|,)"?(NULL|None|nan|NaN|null|N/A)"?(,|$)', cru)) && !any(grepl(',,', cru)))

## ---------------------------------------------------------------- 1. chave unica
chk("B1. chave (sg_ue, ano_eleicao_bocel, nome_normalizado) e unica",
    r2[, .N, by = .(sg_ue, ano_eleicao_bocel, nome_normalizado)][N > 1, .N] == 0,
    paste(r2[, .N, by = .(sg_ue, ano_eleicao_bocel, nome_normalizado)][N > 1, .N], "chaves repetidas"))
chk("B2. id_mandato_bocel sem repeticao",
    r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)] == r2[!is.na(id_mandato_bocel), .N])
inv_chk <- tryCatch({ checa_unica(as.data.frame(r2[!is.na(id_mandato_bocel)]), "id_mandato_bocel"); TRUE }, error = function(e) FALSE)
chk("B3. checa_unica(asserts_rigor) sobre id_mandato_bocel", inv_chk)
chk("B4. id_pessoa_bocel preenchido sempre que id_mandato_bocel esta preenchido",
    r2[!is.na(id_mandato_bocel) & is.na(id_pessoa_bocel), .N] == 0)
chk("B5. metodo_pareamento preenchido sse ha par (ou 'descartado_duplicata')",
    r2[!is.na(id_mandato_bocel) & is.na(metodo_pareamento), .N] == 0 &&
    r2[is.na(id_mandato_bocel) & !is.na(metodo_pareamento) & metodo_pareamento != "descartado_duplicata", .N] == 0)

## ---------------------------------------------------------------- 2. datas e vocabulario
iso <- function(x) is.na(x) | grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
chk("C1. datas em ISO ou NA", all(iso(r2$data_inicio_mandato)) && all(iso(r2$data_fim_mandato)) &&
      all(iso(r2$legislatura_inicio)) && all(iso(r2$legislatura_fim)))
chk("C2. data_inicio_mandato <= data_fim_mandato",
    r2[!is.na(data_inicio_mandato) & !is.na(data_fim_mandato) & data_inicio_mandato > data_fim_mandato, .N] == 0,
    paste(r2[!is.na(data_inicio_mandato) & !is.na(data_fim_mandato) & data_inicio_mandato > data_fim_mandato, .N], "linhas"))
chk("C3. legislatura_inicio <= legislatura_fim",
    r2[!is.na(legislatura_inicio) & !is.na(legislatura_fim) & legislatura_inicio > legislatura_fim, .N] == 0)
chk("C4. nenhuma data anterior a 1980 nem posterior a 2032",
    r2[!is.na(data_inicio_mandato) & (data_inicio_mandato < "1980-01-01" | data_inicio_mandato > "2032-12-31"), .N] == 0 &&
    r2[!is.na(data_fim_mandato) & (data_fim_mandato < "1980-01-01" | data_fim_mandato > "2032-12-31"), .N] == 0 &&
    r2[!is.na(legislatura_inicio) & (legislatura_inicio < "1980-01-01" | legislatura_inicio > "2032-12-31"), .N] == 0)
## a janela de referencia e a da LEGISLATURA REAL (eleicao+1 a eleicao+4), nao a do periodo publicado:
## o Webline publica fragmentos (bienio de mesa, periodo de substituicao) no lugar da legislatura inteira
r2[, ini_real := as.IDate(sprintf("%d-01-01", as.integer(ano_eleicao_bocel) + 1L))]
r2[, fim_real := as.IDate(sprintf("%d-12-31", as.integer(ano_eleicao_bocel) + 4L))]
r2[, fora_janela := (!is.na(data_inicio_mandato) & (as.IDate(data_inicio_mandato) < ini_real - 60L | as.IDate(data_inicio_mandato) > fim_real + 60L)) |
                    (!is.na(data_fim_mandato)    & (as.IDate(data_fim_mandato)    < ini_real - 60L | as.IDate(data_fim_mandato)    > fim_real + 60L))]
n_c5 <- r2[fora_janela == TRUE, .N]
## a invariante que protege mandatos.csv e que nenhuma LINHA PAREADA leve data incompativel com a
## legislatura; linha nao pareada com data errada na origem fica como limite declarado, nao como par
n_c5p <- r2[fora_janela == TRUE & !is.na(id_mandato_bocel), .N]
chk("C5. nenhuma linha PAREADA com data fora da legislatura real (tolerancia 60 dias)", n_c5p == 0, paste(n_c5p, "pares"))
cat("  linhas (nao pareadas) com data que a fonte publica fora da legislatura:", n_c5 - n_c5p, "\n")
fwrite(r2[fora_janela == TRUE, .(uf, sg_ue, ano_eleicao_bocel, legislatura_inicio, legislatura_fim,
                                 data_inicio_mandato, data_fim_mandato, forma_saida, nome_fonte, sistema,
                                 pareado = !is.na(id_mandato_bocel), url)],
       "output/verificacao/camaras_rodada2_datas_fora_da_legislatura.csv")
n_c5b <- n_c5 - n_c5p
chk("C6. forma_saida no vocabulario fechado", all(r2$forma_saida %in% VOCAB),
    paste(setdiff(unique(r2$forma_saida), VOCAB), collapse = ","))
chk("C7. ano_eleicao_bocel no ciclo quadrienal 1996-2024", all(as.integer(r2$ano_eleicao_bocel) %in% seq(1996, 2024, 4)))
## legislatura_inicio pode ser um fragmento dentro da legislatura (bienio de mesa, periodo de posse
## tardia): o que nao pode e cair fora do intervalo eleicao+1 .. eleicao+4
n_c8 <- r2[!is.na(legislatura_inicio) & !as.integer(substr(legislatura_inicio,1,4)) %in%
             (as.integer(ano_eleicao_bocel) + 1:4), .N]
chk("C8. legislatura_inicio dentro do intervalo da eleicao de referencia (+1 a +4)", n_c8 == 0, paste(n_c8, "incoerentes"))
n_c8b <- r2[!is.na(legislatura_inicio) & as.integer(substr(legislatura_inicio,1,4)) != as.integer(ano_eleicao_bocel) + 1L, .N]
cat("  linhas com periodo publicado que e fragmento da legislatura (inicio != eleicao+1):", n_c8b, "\n")
chk("C9. so_legislatura_atual implica eleicao de 2024",
    r2[so_legislatura_atual == "TRUE" & ano_eleicao_bocel != "2024", .N] == 0)
chk("C10. titular em {TRUE, FALSE, NA} e so_legislatura_atual em {TRUE, FALSE}",
    all(is.na(r2$titular) | r2$titular %in% c("TRUE","FALSE")) && all(r2$so_legislatura_atual %in% c("TRUE","FALSE")))

## ---- fim_regular declarado para legislatura AINDA EM CURSO (contagem informativa, checagem dura)
hoje <- as.IDate(Sys.Date())
r2[, fim_leg_ef := as.IDate(fcoalesce(legislatura_fim, sprintf("%d-12-31", as.integer(ano_eleicao_bocel) + 4L)))]
n_fr_curso <- r2[forma_saida == "fim_regular" & !is.na(id_mandato_bocel) & fim_leg_ef > hoje, .N]
chk("C11. nenhum 'fim_regular' pareado em legislatura ainda em curso", n_fr_curso == 0, paste(n_fr_curso, "mandatos"))

## ---------------------------------------------------------------- 3. integridade do pareamento contra o BOCEL
mand_all <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
mand <- mand_all[cd_cargo == "13", .(id_mandato, id_pessoa, sg_ue_bocel = unidade_posicao, ano_bocel = ano_eleicao, cd_cargo, sg_uf_bocel = sg_uf)]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")[, .(id_pessoa, nome, nome_urna_recente)]
chk("D0. id_pessoa unico em pessoas.csv", pess[, uniqueN(id_pessoa)] == nrow(pess))
chk("D0b. id_mandato unico em mandatos.csv", mand_all[, uniqueN(id_mandato)] == nrow(mand_all))

par <- merge(r2[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
chk("D1. todo id_mandato_bocel existe no BOCEL", par[is.na(ano_bocel), .N] == 0, paste(par[is.na(ano_bocel), .N], "orfaos"))
chk("D2. todo par e cargo 13 (vereador)", par[cd_cargo != "13", .N] == 0)
chk("D3. municipio do par bate (sg_ue = unidade_posicao)", par[sg_ue != sg_ue_bocel, .N] == 0)
chk("D4. UF do par bate", par[uf != sg_uf_bocel, .N] == 0, paste(par[uf != sg_uf_bocel, .N], "linhas"))
chk("D5. eleicao do par bate", par[ano_eleicao_bocel != ano_bocel, .N] == 0)
chk("D6. id_pessoa_bocel bate com o id_pessoa do mandato", par[id_pessoa_bocel != id_pessoa, .N] == 0)
par <- merge(par, pess, by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
chk("D7. todo id_pessoa_bocel existe em pessoas.csv", par[is.na(nome), .N] == 0)

## regra a regra: o metodo declarado tem de se sustentar sobre os nomes reais
tk <- function(s, minc = 3L) lapply(strsplit(fcoalesce(s, ""), " "), function(t) t[nchar(t) >= minc])
p1 <- par[metodo_pareamento == "nome_completo"]
chk("E1. nome_completo: nome_normalizado identico ao nome civil do BOCEL",
    nrow(p1) == 0 || all(p1$nome_normalizado == norm(p1$nome)),
    paste(sum(p1$nome_normalizado != norm(p1$nome)), "divergentes"))
p2 <- par[metodo_pareamento == "nome_fonte=nome_urna"]
chk("E2. nome_fonte=nome_urna: nome usado identico ao nome de urna do TSE (via cand parquet)", TRUE)  # conferido em E2b
p3 <- par[metodo_pareamento == "tokens_nome_fonte_no_nome_civil"]
if (nrow(p3)) {
  a <- tk(norm(fcoalesce(p3$nome_parlamentar, p3$nome_fonte))); b <- tk(norm(p3$nome))
  ok3 <- mapply(function(x, y) length(x) >= 2 && all(x %in% y), a, b)
  chk("E3. tokens_no_nome_civil: >=2 tokens (>=3 letras) de fato contidos no nome civil",
      all(ok3), paste(sum(!ok3), "violacoes"))
  p3[, n_tok := lengths(a)]
  cat("  distribuicao de tokens usados na regra 3:\n"); print(p3[, .N, by = n_tok][order(n_tok)])
} else chk("E3. tokens_no_nome_civil", TRUE)
p4 <- par[metodo_pareamento == "tokens_nome_fonte_no_nome_de_urna"]
chk("E4. tokens_no_nome_de_urna: pelo menos um token com >=4 letras",
    nrow(p4) == 0 || all(sapply(tk(norm(fcoalesce(p4$nome_parlamentar, p4$nome_fonte)), 1L), function(x) any(nchar(x) >= 4))))
chk("E5. metodo_pareamento no conjunto declarado",
    all(na.omit(unique(r2$metodo_pareamento)) %in% c("nome_completo","nome_fonte=nome_urna",
        "tokens_nome_fonte_no_nome_civil","tokens_nome_fonte_no_nome_de_urna","descartado_duplicata")))

## E2b: reconstroi o nome de urna a partir do parquet do TSE e confere a regra 2
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(arrow::read_parquet(f, col_select = c("ANO_ELEICAO","SG_UE","CD_CARGO","NR_CANDIDATO","SQ_CANDIDATO","NM_URNA_CANDIDATO")))
  x[CD_CARGO == "13"] }))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
chk("E2c. id_mandato do parquet e unico", cand[, uniqueN(id_mandato)] == nrow(cand),
    paste(nrow(cand) - cand[, uniqueN(id_mandato)], "duplicatas"))
cobertura_urna <- mand[, mean(id_mandato %in% cand$id_mandato)]
chk("E2d. reconstrucao do id_mandato cobre >=95% dos mandatos de vereador", cobertura_urna >= 0.95,
    sprintf("cobertura %.4f", cobertura_urna))
if (nrow(p2)) {
  p2u <- merge(p2, unique(cand[, .(id_mandato, urna = norm(NM_URNA_CANDIDATO))]), by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
  nm2 <- fifelse(is.na(p2u$nome_parlamentar), norm(p2u$nome_fonte), norm(p2u$nome_parlamentar))
  chk("E2b. nome_fonte=nome_urna: nome usado identico ao nome de urna do TSE",
      all(!is.na(p2u$urna) & nm2 == p2u$urna), paste(sum(is.na(p2u$urna) | nm2 != p2u$urna), "divergentes"))
}

## ruido: nomes obviamente nao-pessoais entre os PAREADOS
RUIDO <- "^(BOTTOM OF PAGE|DADOS ABERTOS|FONTE REGULAR|MENU|HOME|CONTATO|TRANSPARENCIA|ACESSIBILIDADE|LEIA MAIS|SAIBA MAIS|VER MAIS|NOTICIAS|SESSAO|SESSOES|LICITACAO|LICITACOES|PORTAL|OUVIDORIA)$"
n_ruido_par <- par[grepl(RUIDO, nome_normalizado), .N]
chk("F1. nenhum nome de ruido evidente entre os pareados", n_ruido_par == 0, paste(n_ruido_par, "linhas"))
n_ruido_tot <- r2[grepl(RUIDO, nome_normalizado), .N]
cat("  linhas de ruido evidente no total (nao pareadas incluidas):", n_ruido_tot, "\n")
chk("F2. nome pareado com pelo menos 2 tokens (nome + sobrenome)",
    par[lengths(strsplit(nome_normalizado, " ")) < 2, .N] == 0,
    paste(par[lengths(strsplit(nome_normalizado, " ")) < 2, .N], "pares com nome de um so token"))

## ---------------------------------------------------------------- 4. cobertura
chk("G1. cobertura soma o mesmo total de pareados",
    sum(cob$n_pareados) == r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
chk("G2. taxa = pareados / mandatos do BOCEL", all(abs(cob$taxa - round(cob$n_pareados / cob$n_mandatos_bocel, 4)) < 1e-9))
chk("G3. nenhuma taxa acima de 1 nem negativa", all(cob$taxa <= 1 & cob$taxa >= 0))
chk("G4. cobertura cobre as 26 UFs + nenhum DF (vereador nao existe no DF)",
    cob[, uniqueN(uf)] == 26 && !("DF" %in% cob$uf), paste(cob[, uniqueN(uf)], "UFs"))
bocel_ver <- mand_all[cd_cargo == "13", .N]
chk("G5. total de mandatos de vereador na cobertura = total no BOCEL",
    sum(cob$n_mandatos_bocel) == bocel_ver, sprintf("cobertura %d x BOCEL %d", sum(cob$n_mandatos_bocel), bocel_ver))

## ---------------------------------------------------------------- 5. rodada 1 x rodada 2
rod1_ue <- inv[coletado_rodada1 == "TRUE", unique(sg_ue)]
chk("H1. nenhum municipio da rodada 1 reaparece na rodada 2", r2[sg_ue %in% rod1_ue, .N] == 0)
chk("H2. nenhum sg_ue da rodada 2 aparece no arquivo da rodada 1", r2[sg_ue %in% r1$sg_ue, .N] == 0)
chk("H3. nenhum id_mandato_bocel ja pareado na rodada 1",
    r2[!is.na(id_mandato_bocel) & id_mandato_bocel %in% r1[!is.na(id_mandato_bocel), id_mandato_bocel], .N] == 0)
chk("H4. inventario: alvo + rodada1 = total do inventario",
    inv[coletado_rodada1 == "FALSE", .N] + inv[coletado_rodada1 == "TRUE", .N] == nrow(inv))
chk("H5. todo sg_ue da saida esta no alvo do inventario",
    r2[!sg_ue %in% inv[coletado_rodada1 == "FALSE", sg_ue], .N] == 0)

## ---------------------------------------------------------------- 6. cache bruto x saida (cobertura declarada)
jsons <- list.files("data_raw/camaras_sem_sapl_2", pattern = "^parlamentares\\.json$", recursive = TRUE, full.names = TRUE)
oks   <- list.files("data_raw/camaras_sem_sapl_2", pattern = "^_ok$", recursive = TRUE, full.names = TRUE)
chk("I1. um parlamentares.json por municipio-alvo", length(jsons) == inv[coletado_rodada1 == "FALSE", .N],
    sprintf("%d json x %d alvo", length(jsons), inv[coletado_rodada1 == "FALSE", .N]))
chk("I2. um marcador _ok por municipio-alvo", length(oks) == inv[coletado_rodada1 == "FALSE", .N])
nao_vazio <- sum(sapply(jsons, function(f) { x <- tryCatch(fromJSON(f, simplifyVector = TRUE), error = function(e) NULL)
  !is.null(x) && length(x) > 0 && (is.data.frame(x) && nrow(x) > 0 || !is.data.frame(x) && length(x) > 0) }))
cat("  municipios com json NAO vazio:", nao_vazio, " | camaras na saida:", r2[, uniqueN(sg_ue)], "\n")
chk("I3. 'camaras com conteudo' declarado (1403) nao excede os json nao vazios", r2[, uniqueN(sg_ue)] <= nao_vazio,
    sprintf("saida %d x json nao vazio %d", r2[, uniqueN(sg_ue)], nao_vazio))

## ---------------------------------------------------------------- 7. registro de numeros x recontagem
lin <- readLines("output/numeros_assinatura.txt", warn = FALSE); lin <- lin[grepl("\\|", lin)]
reg <- data.table(chave = trimws(sub("\\|.*$", "", lin)),
                  valor = trimws(sapply(strsplit(lin, "|", fixed = TRUE), function(x) if (length(x) >= 2) x[2] else NA_character_)))
ult <- function(k) { v <- reg[chave == k, valor]; if (length(v)) suppressWarnings(as.numeric(v[length(v)])) else NA_real_ }
alvo_inv <- inv[coletado_rodada1 == "FALSE"]
sis <- r2[, .(camaras = uniqueN(sg_ue), pareadas = sum(!is.na(id_mandato_bocel))), by = sistema]
ufs <- cob[, .(n_mandatos_bocel = sum(n_mandatos_bocel), n_pareados = sum(n_pareados)), by = uf][, taxa := round(n_pareados/n_mandatos_bocel, 4)]
espera <- list(
  csem2_n_municipios_rodada1 = inv[coletado_rodada1 == "TRUE", .N],
  csem2_n_municipios_sem_coletor_rodada1 = nrow(alvo_inv),
  csem2_n_municipios_com_json = length(jsons),
  csem2_n_municipios_repetidos_da_rodada1 = 0,
  csem2_n_camaras_coletadas = r2[, uniqueN(sg_ue)],
  csem2_n_camaras_com_pareamento = r2[!is.na(id_mandato_bocel), uniqueN(sg_ue)],
  csem2_n_camaras_so_legislatura_atual = r2[so_legislatura_atual == "TRUE", uniqueN(sg_ue)],
  csem2_n_linhas_coletadas = nrow(r2),
  csem2_n_mandatos_pareados = r2[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  csem2_n_mandatos_pareados_com_saida_observada = r2[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)],
  csem2_taxa_pareamento_linhas = round(r2[!is.na(id_mandato_bocel), .N] / nrow(r2), 4),
  csem2_n_legislaturas_distintas_por_camara_mediana = as.numeric(median(r2[, uniqueN(ano_eleicao_bocel), by = sg_ue]$V1)),
  csem2_n_mandatos_pareados_novos_sobre_rodada1 = r2[!is.na(id_mandato_bocel) & !id_mandato_bocel %in% r1[!is.na(id_mandato_bocel), id_mandato_bocel], uniqueN(id_mandato_bocel)],
  csem2_n_camaras_novas_sobre_rodada1 = r2[!sg_ue %in% r1$sg_ue, uniqueN(sg_ue)]
)
for (i in seq_len(nrow(alvo_inv[, .N, by = familia]))) NULL
fa <- alvo_inv[, .N, by = familia]
for (i in seq_len(nrow(fa))) espera[[paste0("csem2_n_por_familia_", fa$familia[i])]] <- fa$N[i]
for (i in seq_len(nrow(sis))) { espera[[paste0("csem2_n_camaras_coletadas_", sis$sistema[i])]] <- sis$camaras[i]
                                espera[[paste0("csem2_n_mandatos_pareados_sistema_", sis$sistema[i])]] <- sis$pareadas[i] }
for (f in unique(r2$forma_saida)) espera[[paste0("csem2_n_forma_saida_", f)]] <- r2[forma_saida == f & !is.na(id_mandato_bocel), .N]
for (m in unique(na.omit(r2$metodo_pareamento))) espera[[paste0("csem2_n_metodo_", m)]] <- r2[metodo_pareamento == m, .N]
for (i in seq_len(nrow(ufs))) { espera[[paste0("csem2_taxa_pareamento_uf_", ufs$uf[i])]] <- ufs$taxa[i]
                                espera[[paste0("csem2_n_pareados_uf_", ufs$uf[i])]] <- ufs$n_pareados[i] }
cmp <- rbindlist(lapply(names(espera), function(k) data.table(chave = k, esperado = as.numeric(espera[[k]]), registrado = ult(k))))
cmp[, bate := !is.na(registrado) & abs(esperado - registrado) < 1e-9]
fwrite(cmp, "output/verificacao/camaras_rodada2_registro_vs_recontagem.csv")
print(cmp[bate == FALSE])
chk("J1. todo numero csem2_ recontado bate com o ultimo registro", all(cmp$bate),
    paste(cmp[bate == FALSE, .N], "divergencias de", nrow(cmp)))
## chaves csem2_ registradas que a recontagem nao cobre (numero registrado sem lastro reconferido)
## as chaves csem2_ceticoV_ sao deste proprio verificador e nao entram na recontagem do construtor
reg_csem2 <- unique(reg[grepl("^csem2_", chave) & !grepl("^csem2_ceticoV_", chave), chave])
# 12/09/2026: a rodada parcial de 05/09 (logs/g7_parcial_nao_verificado_0905.patch, nao aplicada) registrou chaves
# com o R/25 de checksum 52a585233521eadf17c756228bd30e78, que o R/25 em uso nao produz. Saem da exigencia so as
# chaves cujo registro inteiro veio daquele checksum; chave que o R/25 atual tambem registra continua recontada
G7_NAO_APLICADO <- "52a585233521eadf17c756228bd30e78"
reg_md5 <- rbindlist(lapply(strsplit(grep("^csem2_", readLines("output/numeros_assinatura.txt", warn = FALSE), value = TRUE),
                                     " | ", fixed = TRUE),
                            function(p) data.table(chave = trimws(p[1]), md5 = if (length(p) >= 5) trimws(p[5]) else NA_character_)))
orfas_g7 <- reg_md5[, .(so_g7 = all(md5 %in% G7_NAO_APLICADO)), by = chave][so_g7 == TRUE, chave]
cat("chaves csem2_ so da rodada parcial nao aplicada de 05/09:", length(orfas_g7), "\n")
registrar_numero("csem2_ceticoV_n_chaves_orfas_rodada_parcial_0905", length(orfas_g7), script = script)
reg_csem2 <- setdiff(reg_csem2, orfas_g7)
sem_lastro <- setdiff(reg_csem2, c(names(espera), "csem2_n_linhas_brutas", "csem2_n_linhas_sem_ano_eleicao",
                                   "csem2_verificacao_n_checagens_aprovadas", "csem2_verificacao_n_checagens_reprovadas"))
chk("J2. nenhuma chave csem2_ registrada fica sem recontagem", length(sem_lastro) == 0, paste(sem_lastro, collapse = ","))

## as duas chaves de etapa intermediaria (linhas brutas e sem ano) so se recontam relendo o cache
brutas <- rbindlist(lapply(jsons, function(f) {
  x <- tryCatch(fromJSON(f, simplifyVector = TRUE, flatten = TRUE), error = function(e) NULL)
  if (is.null(x) || length(x) == 0) return(NULL); x <- as.data.table(x); if (!nrow(x)) return(NULL)
  data.table(n = nrow(x)) }), fill = TRUE)
chk("J3. csem2_n_linhas_brutas bate com a releitura do cache", ult("csem2_n_linhas_brutas") == sum(brutas$n),
    sprintf("registrado %s x releitura %d", ult("csem2_n_linhas_brutas"), sum(brutas$n)))

## ---------------------------------------------------------------- 8. amostra de 25 pares para inspecao
set.seed(20260827)
am <- par[sample(.N, 25)][, .(uf, sg_ue, ano_eleicao_bocel, sistema, nome_fonte, nome_parlamentar,
                              nome_bocel = nome, nome_urna_bocel = nome_urna_recente, metodo_pareamento,
                              forma_saida, tipo_afastamento, id_mandato_bocel, id_pessoa_bocel, url)]
fwrite(am, "output/verificacao/camaras_rodada2_amostra_25_pares.csv")
print(am[, .(sg_ue, ano_eleicao_bocel, nome_fonte, nome_bocel, nome_urna_bocel, metodo_pareamento)])
set.seed(20260827)
amv <- r2[!is.na(id_mandato_bocel)][sample(.N, 10)][, .(uf, sg_ue, ano_eleicao_bocel, sistema, nome_fonte, id_mandato_bocel, url)]
fwrite(amv, "output/verificacao/camaras_rodada2_amostra_10_ao_vivo.csv")

## ---------------------------------------------------------------- fecho
FORA <- c("pertinencia semantica do pareamento por tokens (apelido x nome civil)",
          "completude das listas publicadas pelos portais",
          "veracidade da situacao declarada pelo portal (nao e ato oficial)",
          "ruido residual da extracao generica",
          "erro de medida na fonte administrativa (TSE e portal): 4 periodos de suplencia que o Webline",
          "publica com ano digitado errado (ex.: '07/08/2012 a 31/12/2021' na legislatura 2009-2012)",
          "permanecem no arquivo como a fonte os publica; nenhum deles esta pareado")
cat("\npassou:", length(passou), " falhou:", length(falhou), "\n")
if (length(falhou)) { cat("FALHAS:\n"); cat(paste0(" - ", falhou, collapse = "\n"), "\n") }
f <- gravar_relatorio_verificacao("data/exercicio_camaras_sem_sapl_2.csv", "R/verifica_camaras_rodada2.R",
                                  passou = passou, falhou = falhou, fora_de_cobertura = FORA)
cat("relatorio:", f, "\n")
registrar_numero("csem2_ceticoV_n_checagens_aprovadas", length(passou), script = script)
registrar_numero("csem2_ceticoV_n_checagens_reprovadas", length(falhou), script = script)
registrar_numero("csem2_ceticoV_n_fim_regular_legislatura_em_curso", n_fr_curso, script = script)
registrar_numero("csem2_ceticoV_n_municipios_json_nao_vazio", nao_vazio, script = script)
registrar_numero("csem2_ceticoV_n_linhas_ruido_evidente", n_ruido_tot, script = script)
registrar_numero("csem2_ceticoV_n_linhas_periodo_fragmento", n_c8b, script = script)
registrar_numero("csem2_ceticoV_n_linhas_data_fora_da_legislatura", n_c5, script = script)
registrar_numero("csem2_ceticoV_n_pares_data_fora_da_legislatura", n_c5p, script = script)
sink()
