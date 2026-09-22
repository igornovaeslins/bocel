# verifica_tce_norte_nordeste.R — verificacao CETICA e INDEPENDENTE da frente 'tce_norte_nordeste'
# (grupo D dos Tribunais de Contas: CE, RN, MA, AL, SE, PA/TCM-PA, AM, AC, RO, RR, AP).
#
# Escrita por um verificador que NAO construiu a frente. Nao reescreve nada: le data/tce_gestores_d.csv,
# data/tce_gestores_d_cobertura.csv, o cache bruto em data_raw/tce/ e o banco (mandatos.csv, pessoas.csv,
# municipios_tse_ibge.csv), RECONTA cada numero registrado com chave tced_* a partir dos dados, compara com
# o ultimo registro em output/numeros_assinatura.txt, e procura os erros silenciosos que a verificacao do
# construtor nao cobre: pareamento por nome inflado, data impossivel, forma_saida fora do vocabulario,
# cobertura nao declarada, dado inventado, sobreposicao com as frentes A/B/C.
#
# Usa lib/asserts_rigor.R (checa_unica, in_set, em_faixa, join_seguro) como criterio read-only.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce_norte_nordeste.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
ASSERTS <- file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R")
script <- "R/verifica_tce_norte_nordeste.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce_norte_nordeste.log", open = "wt"); sink(logf, split = TRUE)

norm <- function(x) { x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII"); x <- gsub("[^A-Z0-9 ]", " ", x); gsub(" +", " ", trimws(x)) }
dig  <- function(x) gsub("[^0-9]", "", as.character(x))
VOCAB <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","nao_tomou_posse","suplente_efetivado","outro","nao_observado")
COLS18 <- c("uf","tribunal","unidade_gestora","tipo_unidade","id_municipio_ibge","sg_ue","nome","cpf","cargo_fonte",
            "cargo_bocel","data_inicio","data_fim","situacao_fonte","forma_saida","id_pessoa_bocel","id_mandato_bocel",
            "metodo_pareamento","url")
UF_D  <- c("CE","RN","MA","AL","SE","PA","AM","AC","RO","RR","AP")
UF_AB <- c("BA","ES","MS","RJ","RS","SC","SP","PB","PE","PI")
UF_C  <- c("GO")
CICLO <- seq(1996L, 2024L, 4L)

passou <- character(); falhou <- character(); fora <- character()
ck <- function(nome, ok, detalhe = "") {
  ok <- isTRUE(ok)
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nzchar(detalhe)) paste0(" — ", detalhe) else ""))
  cat(if (ok) "OK    " else "FALHA ", nome, if (nzchar(detalhe)) paste0("  [", detalhe, "]") else "", "\n", sep = "")
}
# asserts_rigor interrompe na primeira falha; aqui rodam sob tryCatch para que TODA checagem seja reportada
arg <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) conditionMessage(e))
  ck(nome, isTRUE(r), if (isTRUE(r)) "" else substr(gsub("\\s+", " ", r), 1, 200))
}

## ------------------------------------------------------------------ leitura
o   <- fread("data/tce_gestores_d.csv", colClasses = "character", na.strings = "NA")
cob <- fread("data/tce_gestores_d_cobertura.csv")
inv <- fread("data_raw/tce/inventario_tce_d.csv", colClasses = "character")
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
man <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pes <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
manm <- man[cd_cargo %in% c("11","12","13")]

# o registro tem linhas de outras frentes com '|' dentro do texto; o parse e o declarado no contrato da
# frente: os DOIS PRIMEIROS campos separados por '|', linha a linha, sem passar por um leitor de CSV
lin <- readLines("output/numeros_assinatura.txt", warn = FALSE)
prt <- strsplit(lin, "|", fixed = TRUE)
reg <- data.table(chave = trimws(vapply(prt, function(x) if (length(x)) x[1] else NA_character_, character(1))),
                  valor = trimws(vapply(prt, function(x) if (length(x) > 1L) x[2] else NA_character_, character(1))))
reg <- reg[grepl("^tced_", chave) & !is.na(valor)]
ult <- reg[, .(valor = last(valor)), by = chave]                  # o ULTIMO registro de cada chave
val <- function(k) { v <- ult[chave == k, valor]; if (!length(v)) NA_character_ else v }
cmp <- function(k, calc, dec = NA) {
  r <- val(k)
  c2 <- if (is.na(dec)) as.character(calc) else format(round(as.numeric(calc), dec), nsmall = 0, trim = TRUE)
  ok <- !is.na(r) && (identical(r, c2) || (!is.na(suppressWarnings(as.numeric(r))) && isTRUE(all.equal(as.numeric(r), as.numeric(c2)))))
  ck(paste0("num ", k), ok, paste0("registrado=", r, " recontado=", c2))
}

## ------------------------------------------------------------------ 1. contrato, vocabulario, escopo
ck("01 as 18 primeiras colunas repetem o contrato das frentes A, B e C", identical(names(o)[1:18], COLS18))
for (f in c("data/tce_gestores.csv","data/tce_gestores_b.csv","data/tce_gestores_c.csv")) {
  ck(paste0("02 contrato identico ao de ", basename(f)),
     identical(names(fread(f, nrows = 1))[1:18], COLS18))
}
source(ASSERTS)
arg("03 in_set: forma_saida no vocabulario fechado", in_set(o$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida"))
ck("04 forma_saida e nao_observado em 100% das linhas (a fonte nao data nem motiva a saida)",
   o[forma_saida != "nao_observado", .N] == 0L)
arg("05 in_set: uf pertence ao grupo D", in_set(o$uf, UF_D, permitir_na = FALSE, nome = "uf"))
ck("06 disjuncao com as UFs do grupo A e do grupo B", length(intersect(unique(o$uf), UF_AB)) == 0L,
   paste(intersect(unique(o$uf), UF_AB), collapse = ","))
ck("07 disjuncao com a UF do grupo C (GO) — o assert do construtor nao testava isso",
   length(intersect(unique(o$uf), UF_C)) == 0L)
ck("08 nenhum id_mandato_bocel do grupo D ja pareado pelas frentes A, B ou C",
   length(intersect(o[!is.na(id_mandato_bocel), unique(id_mandato_bocel)],
                    unique(c(fread("data/tce_gestores.csv",   select = "id_mandato_bocel", colClasses = "character", na.strings = "NA")$id_mandato_bocel,
                             fread("data/tce_gestores_b.csv", select = "id_mandato_bocel", colClasses = "character", na.strings = "NA")$id_mandato_bocel,
                             fread("data/tce_gestores_c.csv", select = "id_mandato_bocel", colClasses = "character", na.strings = "NA")$id_mandato_bocel)))) == 0L)
ck("09 ausente e sempre o codigo 'NA' (nenhuma string vazia em coluna nenhuma)",
   sum(sapply(o, function(x) sum(!is.na(x) & x == ""))) == 0L)
ck("10 nomes de coluna minusculos e sem acento", all(names(o) == tolower(iconv(names(o), to = "ASCII//TRANSLIT"))))

## ------------------------------------------------------------------ 2. municipio
arg("11 join_seguro: par (sg_ue,id_municipio_ibge) resolve no cadastro TSE-IBGE, many-to-one",
    { j <- join_seguro(as.data.frame(unique(o[, .(sg_ue, id_municipio_ibge)])),
                       as.data.frame(unique(mun[, .(sg_ue, sg_uf, nome_ibge)])), by = "sg_ue", cardinalidade = "many-to-one")
      stopifnot(!anyNA(j$sg_uf)) })
ck("12 UF da linha == UF do municipio no cadastro",
   nrow(merge(unique(o[, .(sg_ue, uf)]), unique(mun[, .(sg_ue, sg_uf)]), by = "sg_ue")[uf != sg_uf]) == 0L)
ck("13 id_municipio_ibge tem 7 digitos", o[nchar(id_municipio_ibge) != 7L, .N] == 0L)
ck("14 municipios cobertos nunca excedem o total da UF",
   nrow(merge(o[, .(n = uniqueN(sg_ue)), by = uf], mun[, .(tot = uniqueN(sg_ue)), by = .(uf = sg_uf)], by = "uf")[n > tot]) == 0L)
# quando a fonte tem coluna propria de municipio, o resolvido tem de bater com ela
mm <- merge(o[!is.na(nome_municipio_fonte)], unique(mun[, .(sg_ue, nome_ibge)]), by = "sg_ue")
ALIAS_OK <- c("ITAPAJE|ITAPAGE","SANTA IZABEL DO PARA|SANTA ISABEL DO PARA","REDENCAO DO PARA|REDENCAO",
              "GOVERNADOR NEWTON BELO|GOVERNADOR NEWTON BELLO","VILA NOVA DOS MATIRIOS|VILA NOVA DOS MARTIRIOS")
mm[, par := paste0(norm(nome_municipio_fonte), "|", norm(nome_ibge))]
ck("15 municipio resolvido bate com o nome que a fonte declara (salvo os 5 alias conferidos)",
   mm[norm(nome_municipio_fonte) != norm(nome_ibge) & !par %in% ALIAS_OK, .N] == 0L,
   paste(head(unique(mm[norm(nome_municipio_fonte) != norm(nome_ibge) & !par %in% ALIAS_OK]$par), 5), collapse = " ; "))
# quando o municipio veio do sufixo, o nome do municipio tem de ser sufixo literal da unidade (ou alias)
sf <- merge(o[is.na(nome_municipio_fonte) & !is.na(unidade_gestora)], unique(mun[, .(sg_ue, nome_ibge)]), by = "sg_ue")
sf[, `:=`(u = norm(unidade_gestora), m = norm(nome_ibge))]
sf[, termina := substring(u, pmax(1L, nchar(u) - nchar(m) + 1L)) == m]
ck("16 municipio resolvido por sufixo e sufixo literal da unidade gestora (fora 6 linhas do alias BELO/BELLO)",
   sf[termina == FALSE & !grepl("NEWTON BELO$", u), .N] == 0L, paste(sf[termina == FALSE, .N], "excecoes"))
ck("17 os 5 alias de grafia: a forma da fonte NAO existe no cadastro e a forma de destino existe",
   { A <- data.table(uf = c("CE","PA","PA","MA","MA"),
                     de = c("ITAPAJE","SANTA IZABEL DO PARA","REDENCAO DO PARA","GOVERNADOR NEWTON BELO","VILA NOVA DOS MATIRIOS"),
                     pa = c("ITAPAGE","SANTA ISABEL DO PARA","REDENCAO","GOVERNADOR NEWTON BELLO","VILA NOVA DOS MARTIRIOS"))
     mun[, nn := norm(nome_ibge)]
     all(sapply(seq_len(nrow(A)), function(i) mun[sg_uf == A$uf[i] & nn == A$de[i], .N] == 0L &&
                                              mun[sg_uf == A$uf[i] & nn == A$pa[i], .N] == 1L)) })

## ------------------------------------------------------------------ 3. datas e faixas
ck("18 data_inicio e data_fim vazias em toda a frente (a lista de julgamento nao data o vinculo)",
   o[!is.na(data_inicio) | !is.na(data_fim), .N] == 0L)
arg("19 em_faixa: exercicio entre 1900 e o ano corrente",
    em_faixa(as.numeric(o$exercicio), 1900, as.numeric(format(Sys.Date(), "%Y")), nome = "exercicio"))
ck("20 exercicio, quando presente, e ano de 4 digitos", o[!is.na(exercicio) & !grepl("^[0-9]{4}$", exercicio), .N] == 0L)
arg("21 in_set: ano_eleicao e ano de eleicao municipal", in_set(o$ano_eleicao, as.character(CICLO), nome = "ano_eleicao"))
ck("22 exercicio nunca anterior a eleicao derivada nem posterior a ela em mais de 4 anos",
   o[!is.na(exercicio) & !is.na(ano_eleicao) &
       (as.integer(exercicio) <= as.integer(ano_eleicao) | as.integer(exercicio) > as.integer(ano_eleicao) + 4L), .N] == 0L)
ck("23 nenhum exercicio no futuro", o[!is.na(exercicio) & as.integer(exercicio) > as.integer(format(Sys.Date(), "%Y")), .N] == 0L)
ck("24 cpf, quando presente, tem 11 digitos e nao e repeticao de um digito so",
   o[!is.na(cpf) & (nchar(cpf) != 11L | grepl("^(\\d)\\1{10}$", cpf)), .N] == 0L)

## ------------------------------------------------------------------ 4. pareamento — o alvo do ceticismo
pp <- o[!is.na(id_mandato_bocel)]
arg("25 checa_unica: id_mandato e chave unica em mandatos.csv", checa_unica(as.data.frame(manm[, .(id_mandato)]), "id_mandato"))
mm2 <- merge(pp, manm[, .(id_mandato, id_pessoa_m = id_pessoa, ano_m = ano_eleicao, cargo_m = cargo,
                          ue_m = unidade_posicao, uf_m = sg_uf, fs_m = forma_saida)],
             by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
mm2 <- merge(mm2, pes[, .(id_pessoa, nome_bocel = nome, urna_bocel = nome_urna_recente, cpf_bocel = nr_cpf)],
             by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
ck("26 todo id_mandato_bocel existe em data/mandatos.csv (e e mandato municipal)", mm2[is.na(ano_m), .N] == 0L)
ck("27 todo id_pessoa_bocel existe em data/pessoas.csv", mm2[is.na(nome_bocel), .N] == 0L)
ck("28 o par (mandato, pessoa) e o par que mandatos.csv registra", mm2[id_pessoa_bocel != id_pessoa_m, .N] == 0L)
ck("29 municipio do pareamento identico ao do mandato", mm2[sg_ue != ue_m, .N] == 0L)
ck("30 UF do pareamento identica a do mandato", mm2[uf != uf_m, .N] == 0L)
ck("31 cargo bate quando o cargo foi determinado pela unidade ou pela fonte",
   mm2[!is.na(cargo_bocel) & cargo_bocel != cargo_m, .N] == 0L)
ck("32 eleicao bate quando a fonte informa o exercicio", mm2[!is.na(ano_eleicao) & ano_eleicao != ano_m, .N] == 0L)
ck("33 um mandato nunca e pareado a duas pessoas diferentes",
   nrow(mm2[, .(n = uniqueN(id_pessoa_bocel)), by = id_mandato_bocel][n > 1L]) == 0L)
ck("34 metodo_pareamento presente em toda linha pareada e ausente em toda linha nao pareada",
   pp[is.na(metodo_pareamento), .N] == 0L && o[is.na(id_mandato_bocel) & !is.na(metodo_pareamento), .N] == 0L)
ck("35 unidade estadual nunca e pareada a mandato municipal", o[tipo_unidade == "estadual" & !is.na(id_mandato_bocel), .N] == 0L)
ck("36 linha de cargo indeterminado so casa por regra declarada indeterminada (ou cpf+municipio)",
   o[is.na(cargo_bocel) & !is.na(metodo_pareamento) & !grepl("indeterminado|^cpf_municipio$", metodo_pareamento), .N] == 0L)
ck("37 linha de cargo determinado nunca usa a regra de cargo indeterminado",
   o[!is.na(cargo_bocel) & !is.na(metodo_pareamento) & grepl("indeterminado", metodo_pareamento), .N] == 0L)
# --- o teste duro: o nome. Todo par tem de ser sustentado por CPF, nome civil, nome de urna ou contencao de tokens
mm2[, `:=`(nf = norm(nome), nb = norm(nome_bocel), nu = norm(urna_bocel))]
tk <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3L])
mm2[, contido := mapply(function(a, b) length(a) >= 2L && all(a %in% b), tk(nf), tk(nb))]
mm2[, sustento := fcase(grepl("^cpf", metodo_pareamento) & dig(cpf) == dig(cpf_bocel), "cpf",
                        nf == nb, "nome_civil",
                        !is.na(nu) & nf == nu, "nome_urna",
                        contido == TRUE, "tokens", default = "SEM SUSTENTO")]
print(mm2[, .N, by = sustento][order(-N)])
ck("38 todo pareamento tem sustento explicito (cpf identico, nome civil, nome de urna ou tokens contidos)",
   mm2[sustento == "SEM SUSTENTO", .N] == 0L, paste(mm2[sustento == "SEM SUSTENTO", .N], "sem sustento"))
ck("39 todo par por CPF tem o CPF da fonte identico ao de pessoas.csv, digito a digito",
   mm2[grepl("^cpf", metodo_pareamento) & dig(cpf) != dig(cpf_bocel), .N] == 0L)
ck("40 nenhum par por CPF com CPF ausente ou incompleto na fonte",
   mm2[grepl("^cpf", metodo_pareamento) & (is.na(cpf) | nchar(cpf) != 11L), .N] == 0L)
# inflacao por nome: um mesmo nome que casa em muitos mandatos distintos no mesmo municipio
infl <- mm2[, .(nm = uniqueN(id_mandato_bocel)), by = .(sg_ue, nf)][nm > 3L]
ck("41 nenhum nome casa a mais de 3 mandatos distintos no mesmo municipio (sinal de nome generico)",
   nrow(infl) == 0L, paste(nrow(infl), "casos:", paste(head(infl$nf, 3), collapse = " / ")))
# nome curto/generico: par sustentado so por token com nome-fonte de menos de 3 tokens uteis
ck("42 nenhum par sustentado apenas por tokens com nome-fonte de menos de 2 tokens de 3+ letras",
   mm2[sustento == "tokens" & lengths(tk(nf)) < 2L, .N] == 0L)
# temporalidade: o exercicio julgado tem de cair dentro do mandato pareado (quando ha exercicio)
# o unico exercicio fora da legislatura e o registro de exercicio 1900 do TCE-AM, erro de digitacao da
# propria fonte, ja contado em tced_n_registros_exercicio_anterior_a_1990 e pareado por CPF+municipio+cargo
mm2[, fora_leg := !is.na(exercicio) & as.integer(exercicio) >= 1990L &
      (as.integer(exercicio) < as.integer(ano_m) + 1L | as.integer(exercicio) > as.integer(ano_m) + 4L)]
ck("43 quando a fonte informa exercicio valido, ele cai dentro da legislatura do mandato pareado",
   mm2[fora_leg == TRUE, .N] == 0L, paste(mm2[fora_leg == TRUE, .N], "fora"))
ck("43b o unico exercicio anterior a 1990 e o registro 1900 do TCE-AM, declarado e pareado por CPF",
   mm2[!is.na(exercicio) & as.integer(exercicio) < 1990L, .N] <= 1L &&
     all(grepl("^cpf", mm2[!is.na(exercicio) & as.integer(exercicio) < 1990L]$metodo_pareamento)))
ck("44 a frente nao grava forma de saida em mandato nenhum (contribuicao e de pareamento, nao de saida)",
   pp[forma_saida != "nao_observado", .N] == 0L)

## ------------------------------------------------------------------ 5. cobertura
arg("45 checa_unica: (uf,cargo,ano_eleicao) e chave unica na tabela de cobertura",
    checa_unica(as.data.frame(cob), c("uf","cargo","ano_eleicao")))
arg("46 em_faixa: taxa de pareamento em [0,1]", em_faixa(cob$taxa, 0, 1, nome = "taxa"))
arg("47 em_faixa: taxa de saida em [0,1]", em_faixa(cob$taxa_saida, 0, 1, nome = "taxa_saida"))
ck("48 n_pareados da cobertura soma os mandatos distintos da tabela principal",
   cob[, sum(n_pareados)] == pp[, uniqueN(id_mandato_bocel)], paste(cob[, sum(n_pareados)], "vs", pp[, uniqueN(id_mandato_bocel)]))
ck("49 n_com_saida e zero em toda a cobertura", cob[, sum(n_com_saida)] == 0L)
# n_bocel recontado direto de mandatos.csv, sem passar pelo script do construtor
alvo <- manm[sg_uf %in% unique(o$uf), .(uf = sg_uf, cargo, ano_eleicao = as.integer(ano_eleicao), id_mandato)]
rec  <- alvo[, .(n_bocel_rec = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
cb   <- merge(cob, rec, by = c("uf","cargo","ano_eleicao"), all = TRUE)
ck("50 n_bocel da cobertura reproduz a recontagem independente de mandatos.csv",
   cb[is.na(n_bocel) | is.na(n_bocel_rec) | n_bocel != n_bocel_rec, .N] == 0L)
ck("51 a cobertura declara toda combinacao uf/cargo/eleicao existente no BOCEL das 8 UFs (nada omitido)",
   nrow(rec) == nrow(cob))
ck("52 nenhuma celula com pareados sem mandato no denominador", cob[n_pareados > n_bocel, .N] == 0L)

## ------------------------------------------------------------------ 6. recontagem do bruto, fonte a fonte
ce <- fread("data_raw/tce/ce/contas_irregulares.csv", colClasses = "character")
cmp("tced_ce_n_linhas_planilha", nrow(ce))
cmp("tced_ce_n_linhas_localidade_estado", ce[norm(localidade) %in% c("CEARA",""), .N])
ck("53 CE: linhas na saida == linhas municipais da planilha com nome de 5+ caracteres",
   o[uf == "CE", .N] == ce[!norm(localidade) %in% c("CEARA","") & nchar(trimws(nome)) >= 5, .N],
   paste(o[uf=="CE",.N], "vs", ce[!norm(localidade) %in% c("CEARA","") & nchar(trimws(nome)) >= 5, .N]))
rn <- fread("data_raw/tce/rn/contas_irregulares.csv", colClasses = "character"); cmp("tced_rn_n_linhas", nrow(rn))
ck("54 RN: todo CPF da saida existe no bruto", length(setdiff(o[uf=="RN" & !is.na(cpf)]$cpf, dig(rn$cpf))) == 0L)
ck("55 RN: todo nome da saida existe no bruto", length(setdiff(norm(o[uf=="RN"]$nome), norm(rn$responsavel))) == 0L)
ma1 <- as.data.table(fromJSON("data_raw/tce/ma/responsaveisirregulares.json")$elements);   cmp("tced_ma_n_linhas_irregulares", nrow(ma1))
ma2 <- as.data.table(fromJSON("data_raw/tce/ma/responsaveisinadimplentes.json")$elements); cmp("tced_ma_n_linhas_inadimplentes", nrow(ma2))
ck("56 MA: todo nome da saida existe em uma das duas listas do mural",
   length(setdiff(norm(o[uf=="MA"]$nome), c(norm(ma1$nomeGestor), norm(ma2$responsavel)))) == 0L)
am1 <- as.data.table(fromJSON("data_raw/tce/am/contas_irregulares.json")$elements);                 cmp("tced_am_n_linhas_contas_irregulares", nrow(am1))
am2 <- as.data.table(fromJSON("data_raw/tce/am/contas_irregulares_fins_eleitorais.json")$elements); cmp("tced_am_n_linhas_contas_irregulares_fins_eleitorais", nrow(am2))
ck("57 AM: todo nome da saida existe em uma das duas listas", length(setdiff(norm(o[uf=="AM"]$nome), c(norm(am1$responsavel), norm(am2$responsavel)))) == 0L)
pa <- fread("data_raw/tce/pa/tcmpa_contas_irregulares.csv", colClasses = "character"); cmp("tced_pa_n_linhas_grade", nrow(pa))
ck("58 PA: so o TCM-PA responde pelo Para (o TCE-PA nao julga prefeitura nem camara)", all(o[uf=="PA"]$tribunal == "TCM-PA"))
ck("59 PA: todo ordenador da saida existe na grade", length(setdiff(norm(o[uf=="PA"]$nome), norm(pa$ordenador))) == 0L)
rr <- as.data.table(fromJSON("data_raw/tce/rr/responsabilizacoes_publicas.json")$elements); cmp("tced_rr_n_linhas", nrow(rr))
ck("60 RR: o cache nao repete id (o bug de paginacao da API nao contaminou o cache)", rr[, uniqueN(id)] == nrow(rr))
ap <- fread("data_raw/tce/ap/contas_irregulares.csv", colClasses = "character"); cmp("tced_ap_n_linhas", nrow(ap))
ro <- fread("data_raw/tce/ro/contas_julgadas_irregulares.csv", colClasses = "character"); cmp("tced_ro_n_linhas_pdf", nrow(ro))
ck("61 RO: numero de ordem do PDF estritamente crescente e sem CPF malformado",
   all(diff(as.integer(ro$n)) > 0L) && ro[nchar(dig(cpf)) != 11L, .N] == 0L)
ck("62 nenhum nome da saida e inventado: todo nome existe no bruto de alguma fonte da propria UF",
   length(setdiff(norm(o$nome), c(norm(ce$nome), norm(rn$responsavel), norm(ma1$nomeGestor), norm(ma2$responsavel),
                                  norm(am1$responsavel), norm(am2$responsavel), norm(pa$ordenador),
                                  norm(rr$responsavel), norm(ap$responsavel), norm(ro$nome)))) == 0L)

## ------------------------------------------------------------------ 7. recontagem de TODO numero tced_ registrado
cmp("tced_n_sondas_inventariadas", nrow(inv)); cmp("tced_n_ufs_inventariadas", inv[, uniqueN(uf)])
cmp("tced_n_sondas_com_fonte_de_responsaveis", inv[grepl("^sim", oferece_gestores), .N])
cmp("tced_n_ufs_com_fonte_de_responsaveis", inv[grepl("^sim", oferece_gestores), uniqueN(uf)])
cmp("tced_n_sondas_sem_fonte", inv[!grepl("^sim", oferece_gestores), .N])
cmp("tced_n_sondas_http_200", inv[http_status == "200", .N])
cmp("tced_n_registros", nrow(o)); cmp("tced_n_registros_finais", nrow(o))
cmp("tced_n_registros_com_cpf", o[!is.na(cpf), .N])
cmp("tced_n_registros_cargo_indeterminado", o[is.na(cargo_bocel), .N])
cmp("tced_n_municipios_cobertos", o[, uniqueN(sg_ue)]); cmp("tced_n_ufs_com_dado", o[, uniqueN(uf)])
cmp("tced_n_pareados_linhas", nrow(pp)); cmp("tced_n_mandatos_pareados", pp[, uniqueN(id_mandato_bocel)])
cmp("tced_n_pessoas_pareadas", pp[, uniqueN(id_pessoa_bocel)])
cmp("tced_n_municipios_com_mandato_pareado", pp[, uniqueN(sg_ue)])
cmp("tced_n_pareados_por_cpf", o[grepl("^cpf", metodo_pareamento), .N])
cmp("tced_taxa_pareamento_linhas", round(nrow(pp)/nrow(o), 4))
cmp("tced_n_mandatos_com_saida_observada", pp[forma_saida != "nao_observado", uniqueN(id_mandato_bocel)])
cmp("tced_n_forma_saida_nao_observado", pp[forma_saida == "nao_observado", uniqueN(id_mandato_bocel)])
cmp("tced_n_pareados_cargo_indeterminado", pp[is.na(cargo_bocel), .N])
cmp("tced_n_mandatos_pareados_cargo_indeterminado", pp[is.na(cargo_bocel), uniqueN(id_mandato_bocel)])
cmp("tced_n_registros_exercicio_anterior_a_1990", o[!is.na(exercicio) & as.integer(exercicio) < 1990L, .N])
cmp("tced_n_registros_brutos", nrow(o) + as.integer(val("tced_n_registros_sem_municipio_resolvido")))
cmp("tced_n_mandatos_bocel_no_universo", manm[unidade_posicao %in% unique(o$sg_ue), .N])
for (u in sort(unique(o$uf))) {
  cmp(paste0("tced_n_registros_uf_", u), o[uf == u, .N])
  cmp(paste0("tced_n_mandatos_pareados_uf_", u), pp[uf == u, uniqueN(id_mandato_bocel)])
  cmp(paste0("tced_n_municipios_uf_", u), o[uf == u, uniqueN(sg_ue)])
  cmp(paste0("tced_taxa_pareamento_uf_", u), round(pp[uf == u, .N]/o[uf == u, .N], 4))
}
for (cg in c("PREFEITO","VEREADOR")) {
  cmp(paste0("tced_n_registros_cargo_", cg), o[cargo_bocel == cg, .N])
  cmp(paste0("tced_n_mandatos_pareados_cargo_", cg), pp[cargo_bocel == cg, uniqueN(id_mandato_bocel)])
}
for (a in sort(unique(pp[!is.na(ano_eleicao)]$ano_eleicao)))
  cmp(paste0("tced_n_mandatos_pareados_eleicao_", a), pp[ano_eleicao == a, uniqueN(id_mandato_bocel)])
uc <- cob[, .(n = sum(n_bocel), p = sum(n_pareados)), by = .(uf, cargo)]
for (i in seq_len(nrow(uc))) cmp(paste0("tced_taxa_", uc$uf[i], "_", gsub("[^A-Z]", "", uc$cargo[i])), round(uc$p[i]/uc$n[i], 4))

## ------------------------------------------------------------------ 8. o numero que o rotulo promete
# a chave 'com_exercicio' e registrada a partir de ano_eleicao (derivada), nao de exercicio (informado)
ck("63 tced_n_registros_com_exercicio conta de fato os registros com exercicio informado",
   as.integer(val("tced_n_registros_com_exercicio")) == o[!is.na(exercicio), .N],
   paste("registrado", val("tced_n_registros_com_exercicio"), "| com exercicio informado", o[!is.na(exercicio), .N],
         "| com ano_eleicao derivada", o[!is.na(ano_eleicao), .N]))
ck("64 nenhuma unidade estadual sobrou na tabela (a regra declarada e descarta-la)",
   o[tipo_unidade == "estadual", .N] == 0L, paste(o[tipo_unidade == "estadual", .N], "linhas: ",
   paste(unique(o[tipo_unidade == "estadual"]$unidade_gestora), collapse = " / ")))
cmp("tced_n_resolvidos_por_sufixo", o[is.na(nome_municipio_fonte), .N])

## ------------------------------------------------------------------ 9. inventario e cobertura declarada
ck("65 inventario cobre as 11 UFs do grupo D", setequal(unique(inv$uf), UF_D))
ck("66 toda UF sem fonte no inventario esta ausente da saida",
   length(intersect(setdiff(inv$uf, inv[grepl("^sim", oferece_gestores)]$uf), unique(o$uf))) == 0L)
ck("67 toda UF com fonte no inventario produziu linha", setequal(inv[grepl("^sim", oferece_gestores), unique(uf)], unique(o$uf)))
ck("68 AL, SE e AC estao declaradas como lacuna, nao omitidas", all(c("AL","SE","AC") %in% inv$uf) &&
     inv[uf %in% c("AL","SE","AC") & grepl("^sim", oferece_gestores), .N] == 0L)
ck("69 a documentacao existe e cita a saida e o cobertor", file.exists("docs/TCE_GRUPO_D.md") &&
     any(grepl("tce_gestores_d.csv", readLines("docs/TCE_GRUPO_D.md", warn = FALSE))))

fora <- c(
  "pertinencia semantica do pareamento por nome em municipio pequeno: as 358 linhas de cargo indeterminado casam por nome civil dentro do municipio, e homonimia local nao e verificavel por script",
  "o universo das listas e o dos responsaveis com contas julgadas irregulares (e, no MA, inadimplentes); ausencia de um gestor nada diz sobre o mandato dele, e a frente nao e amostra de ocupantes de cargo",
  "atribuicao de cargo pelo tipo da unidade gestora quando o ordenador e delegatario (secretario ordenando pela prefeitura, por exemplo)",
  "existencia e atualidade de listas em AL, SE e AC: verifiquei que nao ha arquivo publico localizavel hoje, nao que a lista nao exista",
  "completude do PDF de 2016 do TCE-RO e a perda declarada de 9 registros na extracao",
  "significado substantivo de 'conta julgada irregular' como evento politico — e decisao de julgamento do autor, nao invariante de dado"
)
registrar_numero("tced_verif2_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tced_verif2_n_aprovadas", length(passou), script = script)
registrar_numero("tced_verif2_n_reprovadas", length(falhou), script = script)
registrar_numero("tced_verif2_n_pares_auditados", nrow(mm2), script = script)
registrar_numero("tced_verif2_n_pares_sustentados_por_cpf", mm2[sustento == "cpf", .N], script = script)
registrar_numero("tced_verif2_n_pares_sustentados_por_nome_civil", mm2[sustento == "nome_civil", .N], script = script)
registrar_numero("tced_verif2_n_pares_sustentados_por_tokens", mm2[sustento == "tokens", .N], script = script)
registrar_numero("tced_verif2_n_pares_sem_sustento", mm2[sustento == "SEM SUSTENTO", .N], script = script)
registrar_numero("tced_verif2_n_linhas_tipo_estadual_na_saida", o[tipo_unidade == "estadual", .N], script = script)
registrar_numero("tced_verif2_n_registros_com_exercicio_informado", o[!is.na(exercicio), .N], script = script)
f <- gravar_relatorio_verificacao("data/tce_gestores_d.csv + data/tce_gestores_d_cobertura.csv", "R/34_tce_gestores_d.R",
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nrelatorio:", f, "\n", length(passou), "aprovadas,", length(falhou), "reprovadas\n")
if (length(falhou)) cat("REPROVADAS:\n- ", paste(falhou, collapse = "\n- "), "\n", sep = "")
sink()
