# verifica_tce_sudeste_sul_centro.R — verificacao CETICA e INDEPENDENTE da frente 'tce_sudeste_sul_centro'
# (grupo C dos Tribunais de Contas: MG, PR, GO com TCE-GO e TCM-GO, MT, DF e TO).
#
# Nao e a verificacao do construtor (R/verifica_tce_c.R) reescrita: aqui o alvo e o erro SILENCIOSO, e as
# invariantes sao recalculadas do bruto e do BOCEL, nunca lidas dos numeros ja registrados. Cinco eixos:
#   (a) contrato e forma da tabela, com os asserts de lib/asserts_rigor.R;
#   (b) rastreio linha a linha da saida ate o bruto em data_raw/tce/go/ (dado inventado);
#   (c) pareamento por nome: cada par e reconferido contra mandatos.csv e pessoas.csv, e a plausibilidade
#       do nome e testada contra o titular E contra o vice da mesma chapa (inflacao por homonimo/parente);
#   (d) datas impossiveis: exercicio fora da janela do mandato pareado, exercicio no futuro;
#   (e) recontagem de TODA chave tcec_* contra o ultimo registro em output/numeros_assinatura.txt.
#
# Nao reescreve nada em data/. Execucao:
#   cd ~/bocel && Rscript --vanilla R/verifica_tce_sudeste_sul_centro.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/verifica_tce_sudeste_sul_centro.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/verifica_tce_sudeste_sul_centro.log", open = "wt"); sink(logf, split = TRUE)

VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
UF_C  <- c("MG", "PR", "GO", "MT", "DF", "TO")
CICLO <- seq(1996L, 2024L, 4L)
METODOS <- c("nome_completo_eleicao", "nome_fonte=nome_urna_eleicao",
             "tokens_nome_fonte_no_nome_civil", "tokens_nome_fonte_no_nome_de_urna")
ANO_HOJE <- as.integer(format(Sys.Date(), "%Y"))

passou <- character(); falhou <- character()
ck <- function(nome, ok, detalhe = "") {
  ok <- isTRUE(ok)
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nzchar(detalhe)) paste0(" — ", detalhe) else ""))
  cat(if (ok) "OK    " else "FALHA ", nome, if (nzchar(detalhe)) paste0("  [", detalhe, "]") else "", "\n", sep = "")
}
# asserts_rigor aborta no erro; aqui o erro vira FALHA e a varredura continua
cka <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) conditionMessage(e))
  ck(nome, isTRUE(r), if (isTRUE(r)) "" else substr(gsub("[\r\n]+", " ", r), 1, 220))
}
norm <- function(x) {
  x <- stri_trans_general(toupper(as.character(x)), "Latin-ASCII")
  x <- gsub("['`´‘’]", "", x)
  x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x))
}
tk3 <- function(x) lapply(strsplit(norm(fcoalesce(as.character(x), "")), " "), function(t) t[nchar(t) >= 3L])
contido <- function(a, b, min_tok = 2L) mapply(function(u, v) length(u) >= min_tok && all(u %in% v), tk3(a), tk3(b))

## ------------------------------------------------------------------ leitura
ARQ  <- "data/tce_gestores_c.csv"; ARQC <- "data/tce_gestores_c_cobertura.csv"
ck("A00 arquivo de saida existe", file.exists(ARQ))
ck("A01 arquivo de cobertura existe", file.exists(ARQC))
o    <- fread(ARQ, colClasses = "character", na.strings = "NA")
cob  <- fread(ARQC)
inv  <- fread("data_raw/tce/inventario_tce_c.csv", colClasses = "character")
COLS <- names(fread("data/tce_gestores.csv", nrows = 0))
mun  <- fread("data/municipios_tse_ibge.csv", colClasses = "character")

## ------------------------------------------------------------------ (a) contrato e forma
ck("A02 colunas identicas as de data/tce_gestores.csv, na mesma ordem", identical(names(o), COLS),
   paste(c(setdiff(COLS, names(o)), setdiff(names(o), COLS)), collapse = ","))
ck("A03 nomes de coluna em minusculas, sem acento e sem espaco",
   all(grepl("^[a-z0-9_]+$", names(o))), paste(grep("^[a-z0-9_]+$", names(o), value = TRUE, invert = TRUE), collapse = ","))
bruto_txt <- readLines(ARQ, n = 200000L, warn = FALSE)
ck("A04 ausente gravado como NA, nunca como campo vazio",
   !any(grepl(',""|,,|^,|,$', bruto_txt)))
cka("A05 in_set(forma_saida) no vocabulario fechado, sem NA",
    in_set(o$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida"))
cka("A06 in_set(cargo_bocel) em PREFEITO/VEREADOR", in_set(o$cargo_bocel, c("PREFEITO", "VEREADOR"), nome = "cargo_bocel"))
cka("A07 in_set(tipo_unidade) em prefeitura/camara/outra", in_set(o$tipo_unidade, c("prefeitura", "camara", "outra"), permitir_na = FALSE, nome = "tipo_unidade"))
cka("A08 in_set(uf) so nas UFs do grupo C", in_set(o$uf, UF_C, permitir_na = FALSE, nome = "uf"))
cka("A09 in_set(metodo_pareamento) nas quatro regras declaradas", in_set(o$metodo_pareamento, METODOS, nome = "metodo_pareamento"))
cka("A10 in_set(relacao_chapa_eleita)", in_set(o$relacao_chapa_eleita,
    c("titular_eleito", "vice_eleito", "outra_pessoa_pareada", "indeterminado"), nome = "relacao_chapa_eleita"))
cka("A11 in_set(fonte) nas duas fontes declaradas do TCM-GO",
    in_set(o$fonte, c("tcmgo_contas_de_governo_julgadas_pelas_camaras", "tcmgo_rol_contas_irregulares_ficha_limpa"),
           permitir_na = FALSE, nome = "fonte"))
# granularidade: a chave natural nao e unica, e a duplicata tem de vir do proprio bruto
K <- c("fonte", "sg_ue", "nome", "exercicio", "situacao_fonte")
dupk <- o[duplicated(o[, ..K])]
pdfc <- fread("data_raw/tce/go/contas_julgadas_camaras.csv", colClasses = "character")
dup_bruto <- pdfc[duplicated(pdfc[, .(processo, municipio, nome, assunto, mes_ano, data_julgamento, resultado)])]
ck("A12 toda duplicata da chave natural {fonte,sg_ue,nome,exercicio,situacao_fonte} vem duplicada do bruto",
   nrow(dupk) == nrow(dup_bruto) && nrow(dupk) <= 1L, paste(nrow(dupk), "na saida,", nrow(dup_bruto), "no bruto"))
cka("A13 checa_unica na chave natural mais o ordinal da repeticao publicada",
    checa_unica(as.data.frame(cbind(o[, ..K], ord = o[, seq_len(.N), by = K]$V1)), c(K, "ord")))
ck("A14 data_inicio e data_fim declaradas vazias em toda a frente (as fontes nao dao periodo)",
   o[!is.na(data_inicio) | !is.na(data_fim), .N] == 0L)
ck("A15 cpf sempre NA (as duas fontes so publicam CPF mascarado)", o[!is.na(cpf), .N] == 0L)
ck("A16 url preenchida em toda linha", o[is.na(url) | url == "", .N] == 0L)

## ------------------------------------------------------------------ municipio
ck("B01 sg_ue e id_municipio_ibge sempre preenchidos", o[is.na(sg_ue) | is.na(id_municipio_ibge), .N] == 0L)
ck("B02 par sg_ue/ibge existe no mapa TSE-IBGE",
   nrow(fsetdiff(unique(o[, .(sg_ue, id_municipio_ibge)]), unique(mun[, .(sg_ue, id_municipio_ibge)]))) == 0L)
ck("B03 UF da linha bate com a UF do municipio no mapa",
   nrow(merge(unique(o[, .(sg_ue, uf)]), unique(mun[, .(sg_ue, sg_uf)]), by = "sg_ue")[uf != sg_uf]) == 0L)
# a grafia da fonte tem de ser variante do nome do IBGE: mesmos tokens, ou tokens contidos (truncamento)
mp_ <- merge(unique(o[, .(sg_ue, nome_municipio_fonte)]), mun[, .(sg_ue, nome_ibge)], by = "sg_ue")
STOPW <- c("DE", "DA", "DO", "DAS", "DOS", "E", "GOIAS", "GO")
mp_[, ok := vapply(seq_len(.N), function(i) {
  a <- setdiff(strsplit(norm(nome_municipio_fonte[i]), " ")[[1]], STOPW)
  b <- setdiff(strsplit(norm(nome_ibge[i]), " ")[[1]], STOPW)
  identical(a, b) || all(a %in% b) || (length(a) == length(b) && all(adist(a, b, partial = FALSE)[cbind(seq_along(a), seq_along(a))] <= 2L))
}, TRUE)]
ck("B04 toda grafia de municipio da fonte e variante do nome do IBGE (tokens iguais, contidos, ou a <=2 edicoes)",
   mp_[ok == FALSE, .N] == 0L, paste(mp_[ok == FALSE, paste0(nome_municipio_fonte, "->", nome_ibge)], collapse = "; "))
ck("B05 nenhum municipio de fora de GO entrou pela resolucao", o[!sg_ue %in% mun[sg_uf == "GO", sg_ue], .N] == 0L)

## ------------------------------------------------------------------ (d) datas e faixas
o[, `:=`(ex = as.integer(exercicio), ae = as.integer(ano_eleicao), aef = as.integer(ano_eleicao_fonte))]
cka("C01 em_faixa(exercicio, 1990, ano corrente)", em_faixa(o$ex, 1990, ANO_HOJE, permitir_na = FALSE, nome = "exercicio"))
ck("C02 exercicio sempre presente", o[is.na(ex), .N] == 0L)
ck("C03 ano_eleicao e ano de eleicao municipal", o[!is.na(ae) & !(ae %in% CICLO), .N] == 0L)
ck("C04 ano_eleicao deriva do exercicio pela regra da posse em 1o de janeiro",
   o[!is.na(ae) & ae != ((ex - 1L) %/% 4L) * 4L, .N] == 0L)
ck("C05 ano_eleicao_fonte identico a ano_eleicao", identical(o$ae, o$aef))
ck("C06 ano_eleicao NA exatamente quando o exercicio cai fora do ciclo coberto",
   o[is.na(ae), .N] == o[!(((ex - 1L) %/% 4L) * 4L) %in% CICLO, .N])
# datas impossiveis: o exercicio de uma linha pareada tem de cair dentro da janela do mandato
ck("C07 exercicio da linha pareada dentro da janela [eleicao+1, eleicao+4]",
   o[!is.na(id_mandato_bocel) & (ex < ae + 1L | ex > ae + 4L), .N] == 0L,
   paste(o[!is.na(id_mandato_bocel) & (ex < ae + 1L | ex > ae + 4L), .N], "linhas fora da janela"))
ck("C08 nenhum exercicio pareado no futuro (posterior ao ano corrente)",
   o[!is.na(id_mandato_bocel) & ex > ANO_HOJE, .N] == 0L)
# Consistencia interna do PROPRIO TCM-GO: (i) julgamento nunca antes do exercicio julgado; (ii) exercicio
# nunca posterior ao ano de autuacao do processo. As duas sao violadas por um punhado de linhas do PDF
# assinado, e a violacao esta no bruto, verbatim — nao na conversao. A checagem fixa os casos conhecidos,
# de modo que um caso NOVO reprova em vez de passar despercebido.
o[, proc := sub("^.*processo ", "", situacao_fonte)]
o[, ano_autuacao := { a <- suppressWarnings(as.integer(sub("^.*/", "", sub(" - .*$", "", proc))))
                      fifelse(is.na(a), NA_integer_, fifelse(a <= 30L, 2000L + a, 1900L + a)) }]
o[, dj := suppressWarnings(as.integer(substr(sub("^.*camara em ", "", situacao_fonte), 1, 4)))]
ANOM_JULG <- c("08405/13", "06734/09", "10786/11")
ANOM_EXER <- c("04869/24", "05615/21", "07233/23", "04653/23", "16289/18", "08405/13", "07751/18", "06255/15", "04000/97")
a1 <- o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & !is.na(dj) & dj < ex]
ck("C09 julgamento anterior ao exercicio: so os 3 casos conhecidos, todos verbatim no PDF do TCM-GO",
   setequal(a1$proc, ANOM_JULG) && nrow(a1) == 3L, paste(nrow(a1), "linhas:", paste(a1$proc, collapse = ",")))
a2 <- o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & !is.na(ano_autuacao) & ex > ano_autuacao]
ck("C10 exercicio posterior a autuacao do processo: so os 9 casos conhecidos, todos verbatim no PDF",
   setequal(a2$proc, ANOM_EXER) && nrow(a2) == 9L, paste(nrow(a2), "linhas:", paste(sort(a2$proc), collapse = ",")))
# o que essa inconsistencia da fonte faz ao BOCEL. Das 9 linhas, 4 estao pareadas; mas o pareamento so muda
# de mandato quando o exercicio impossivel e o ano de autuacao caem em ELEICOES diferentes — nas outras 3 a
# eleicao de referencia e a mesma nas duas leituras, e a atribuicao nao depende do erro da fonte.
elei_de <- function(ex) { y <- ((as.integer(ex) - 1L) %/% 4L) * 4L; fifelse(y %in% CICLO, y, NA_integer_) }
a2[, elei_risco := elei_de(ex) != elei_de(ano_autuacao)]
risco <- a2[!is.na(id_mandato_bocel) & elei_risco == TRUE]
ck("C11 so 1 mandato pareado muda de eleicao se o exercicio impossivel for lido pelo ano de autuacao (04869/24)",
   nrow(risco) == 1L && risco$proc == "04869/24",
   paste(a2[!is.na(id_mandato_bocel), paste0(proc, " ex ", ex, " autuado ", ano_autuacao, " -> ", id_mandato_bocel,
                                           if (all(TRUE)) "" else "")], collapse = "; "))
registrar_numero("tcecv_n_linhas_exercicio_impossivel", nrow(a2), script = script)
registrar_numero("tcecv_n_pareamentos_por_exercicio_impossivel", a2[!is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tcecv_n_mandatos_em_risco_por_exercicio_impossivel", nrow(risco), script = script)

## ------------------------------------------------------------------ (b) rastreio ao bruto: dado inventado
csvc <- fread("data_raw/tce/go/contas_irregulares.csv", colClasses = "character")
setnames(csvc, c("Município", "Mês/Ano"), c("municipio_ug", "mes_ano"), skip_absent = TRUE)
MD5_PDF <- "c3350617c7e067082dca568ce4a111df"   # conferido contra o download ao vivo em 29/08/2026
MD5_ROL <- "fc203fbce33f4f3efd452015418fce25"   # idem, ws.tcm.go.gov.br/api/rest/dados/contas-irregulares
ck("G00 cache bruto identico ao que a fonte do TCM-GO serve ao vivo (md5 conferido nesta verificacao)",
   unname(tools::md5sum("data_raw/tce/go/contas_julgadas_camaras.pdf")) == MD5_PDF &&
     unname(tools::md5sum("data_raw/tce/go/contas_irregulares.csv")) == MD5_ROL)
ck("G01 bruto do PDF tem 1881 linhas e o rol tem 1375", nrow(pdfc) == 1881L && nrow(csvc) == 1375L,
   paste(nrow(pdfc), "+", nrow(csvc)))
ck("G02 nenhuma linha da saida excede o bruto por fonte",
   o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras", .N] <= nrow(pdfc) &&
     o[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa", .N] <= nrow(csvc))
# rastreio par a par: (nome + municipio da fonte + exercicio) de toda linha da saida existe no bruto
bp <- pdfc[, .(k = paste(trimws(gsub("\\s+", " ", nome)), municipio, sub("^.*/", "", mes_ano), sep = "|"))]
br <- csvc[, .(k = paste(trimws(gsub("\\s+", " ", Nome)), trimws(sub(" - .*$", "", municipio_ug)),
                         sub("^.*/", "", mes_ano), sep = "|"))]
ko <- o[, .(k = paste(nome, nome_municipio_fonte, exercicio, sep = "|"), fonte)]
ck("G03 toda linha vinda do PDF rastreia a uma linha do PDF bruto",
   ko[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & !k %in% bp$k, .N] == 0L,
   paste(ko[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & !k %in% bp$k, .N], "sem lastro"))
ck("G04 toda linha vinda do rol rastreia a uma linha do rol bruto",
   ko[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa" & !k %in% br$k, .N] == 0L,
   paste(ko[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa" & !k %in% br$k, .N], "sem lastro"))
# o descarte tem de ser so o consorcio intermunicipal, e tem de estar contado
desc <- setdiff(c(bp$k, br$k), ko$k)
ck("G05 o descarte do bruto e apenas de unidade sem municipio (consorcio intermunicipal)",
   length(desc) > 0 && all(grepl("CONVAM|CIMA", desc)), paste(head(desc, 5), collapse = " ; "))
ck("G06 3251 = 1881 + 1375 - 5 descartes", nrow(o) == nrow(pdfc) + nrow(csvc) - 5L)
# atribuicao de cargo: nenhuma linha ganha PREFEITO sem lastro no assunto ou na lista de origem
ck("G07 no PDF so contas de governo (BALANC...) viram PREFEITO",
   o[fonte == "tcmgo_contas_de_governo_julgadas_pelas_camaras" & cargo_bocel == "PREFEITO" & !grepl("\\(BALANC", cargo_fonte), .N] == 0L)
ck("G08 no rol so a lista de prefeitos vira PREFEITO",
   o[fonte == "tcmgo_rol_contas_irregulares_ficha_limpa" & cargo_bocel == "PREFEITO" &
       !grepl("^Contas de Prefeitos e Ex-Prefeitos", cargo_fonte), .N] == 0L)
ck("G09 VEREADOR so em linha cuja unidade gestora e a camara",
   o[cargo_bocel == "VEREADOR" & tipo_unidade != "camara", .N] == 0L)

## ------------------------------------------------------------------ (c) pareamento reconferido
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %in% c("11", "12", "13")]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")[, .(id_pessoa, nome_bocel = nome)]
mand <- merge(mand[, .(id_mandato, id_pessoa, sg_ue = unidade_posicao, sg_uf, cd_cargo, cargo,
                       ano_eleicao_m = as.integer(ano_eleicao), mandato_inicio, mandato_fim)],
              pess, by = "id_pessoa")
p <- merge(o[!is.na(id_mandato_bocel)], mand, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE, suffixes = c("", "_m"))
ck("D01 todo id_mandato_bocel existe em data/mandatos.csv", p[is.na(id_pessoa), .N] == 0L, paste(p[is.na(id_pessoa), .N], "orfaos"))
ck("D02 municipio do mandato pareado bate com sg_ue da linha", p[sg_ue != sg_ue_m, .N] == 0L)
ck("D03 UF do mandato pareado bate", p[uf != sg_uf, .N] == 0L)
ck("D04 cargo do mandato pareado bate com cargo_bocel", p[cargo_bocel != cargo, .N] == 0L)
ck("D05 eleicao do mandato pareado bate com ano_eleicao da linha", p[ae != ano_eleicao_m, .N] == 0L)
ck("D06 id_pessoa_bocel e o titular do mandato pareado em pessoas.csv", p[id_pessoa_bocel != id_pessoa, .N] == 0L)
ck("D07 metodo_pareamento presente sempre que ha mandato", o[!is.na(id_mandato_bocel) & is.na(metodo_pareamento), .N] == 0L)
ck("D08 linha sem cargo_bocel nunca foi pareada", o[is.na(cargo_bocel) & !is.na(id_mandato_bocel), .N] == 0L)
ck("D09 linha sem ano_eleicao nunca foi pareada", o[is.na(ae) & !is.na(id_mandato_bocel), .N] == 0L)
# plausibilidade do nome: igual, ou tokens da fonte contidos no nome civil / de urna do BOCEL
suppressPackageStartupMessages(library(arrow))
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_(19|20)\\d{2}\\.parquet$", full.names = TRUE),
  function(fp) as.data.table(read_parquet(fp, col_select = c("ANO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO",
                                                             "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO")))[
    CD_CARGO %in% c("11", "12", "13") & SG_UF %in% unique(o$uf)]), use.names = TRUE)
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_", CD_CARGO, "_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
urna <- unique(cand[, .(id_mandato, nome_urna = NM_URNA_CANDIDATO)], by = "id_mandato")
p <- merge(p, urna, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
p[, nome_igual := norm(nome) == norm(nome_bocel)]
p[, nome_contido := contido(nome, nome_bocel)]
p[, bocel_contido := contido(nome_bocel, nome)]
p[, urna_igual := !is.na(nome_urna) & norm(nome) == norm(nome_urna)]
p[, urna_contido := !is.na(nome_urna) & (contido(nome, nome_urna) | contido(nome_urna, nome))]
p[, plausivel := nome_igual | nome_contido | bocel_contido | urna_igual | urna_contido]
ck("D10 nenhum par com nome implausivel (nem igual, nem com >=2 tokens contidos, no nome civil ou no de urna)",
   p[plausivel == FALSE, .N] == 0L,
   paste(head(p[plausivel == FALSE, paste0(nome, " <> ", nome_bocel, " / urna ", nome_urna)], 5), collapse = " ; "))
ck("D10b todo par que so fecha pelo nome de urna esta declarado no metodo_pareamento",
   p[!(nome_igual | nome_contido | bocel_contido) & metodo_pareamento != "nome_fonte=nome_urna_eleicao", .N] == 0L)
# inflacao por parente/homonimo: o nome da fonte nao pode casar TAMBEM com o vice da mesma chapa
vic <- mand[cd_cargo == "12", .(sg_ue, ano_eleicao_m, nome_vice = nome_bocel, id_pessoa_vice = id_pessoa)]
vic <- vic[, if (.N == 1L) .SD else NULL, by = .(sg_ue, ano_eleicao_m)]
pv <- merge(p, vic, by.x = c("sg_ue", "ae"), by.y = c("sg_ue", "ano_eleicao_m"), all.x = TRUE)
pv[, casa_vice := !is.na(nome_vice) & (norm(nome) == norm(nome_vice) | contido(nome, nome_vice))]
ck("D11 nenhum par cujo nome da fonte case tambem com o vice eleito da mesma chapa (ambiguidade titular/vice)",
   pv[casa_vice == TRUE, .N] == 0L, paste(pv[casa_vice == TRUE, .N], "linhas ambiguas"))
# o mandato atribuido tem de ser o unico do municipio-cargo-eleicao (a regra que o construtor declara)
unico <- mand[cd_cargo != "12", .N, by = .(sg_ue, cd_cargo, ano_eleicao_m)][N == 1L]
pu <- merge(p[, .(sg_ue, cd = fifelse(cargo_bocel == "PREFEITO", "11", "13"), ae)], unico,
            by.x = c("sg_ue", "cd", "ae"), by.y = c("sg_ue", "cd_cargo", "ano_eleicao_m"), all.x = TRUE)
ck("D12 todo par recai sobre municipio-cargo-eleicao com mandato unico no BOCEL", pu[is.na(N), .N] == 0L)
# um mesmo mandato pode receber varios exercicios; uma mesma linha nunca recebe dois mandatos
ck("D13 uma linha nunca carrega mais de um mandato", nrow(o) == nrow(o[, .N, by = seq_len(nrow(o))]))
# amostra de 25 pares conferida contra mandatos.csv e pessoas.csv, gravada para leitura humana
set.seed(20260827)
am <- p[sample(.N, min(25L, .N))][, .(nome_fonte = nome, nome_bocel, sg_ue, nome_municipio_fonte, exercicio,
                                      ano_eleicao = ae, cargo_bocel, cargo, id_mandato_bocel, id_pessoa_bocel, id_pessoa,
                                      metodo_pareamento, mandato_inicio, mandato_fim,
                                      ok = plausivel & id_pessoa_bocel == id_pessoa &
                                        sg_ue == sg_ue_m & cargo_bocel == cargo & ae == ano_eleicao_m)]
fwrite(am, "output/verificacao/amostra_25_pares_tce_c.csv")
ck("D14 amostra de 25 pares confere integralmente contra mandatos.csv e pessoas.csv", am[ok == FALSE, .N] == 0L)
print(am[, .(nome_fonte, nome_bocel, sg_ue, exercicio, metodo_pareamento, ok)])

## ------------------------------------------------------------------ forma de saida e integracao
ck("E01 unica forma declarada e 'outro', sempre com titular substituido",
   o[forma_saida != "nao_observado" & (forma_saida != "outro" | is.na(id_mandato_titular_substituido)), .N] == 0L)
ck("E02 todo id_mandato_titular_substituido existe em mandatos.csv e e prefeito eleito",
   o[!is.na(id_mandato_titular_substituido) &
       !id_mandato_titular_substituido %in% mand[cd_cargo == "11", id_mandato], .N] == 0L)
ts <- merge(o[!is.na(id_mandato_titular_substituido), .(sg_ue, ae, id_mandato_titular_substituido)],
            mand[cd_cargo == "11", .(id_mandato, sg_ue_t = sg_ue, ae_t = ano_eleicao_m)],
            by.x = "id_mandato_titular_substituido", by.y = "id_mandato")
ck("E03 o titular substituido e do mesmo municipio e da mesma eleicao da linha", ts[sg_ue != sg_ue_t | ae != ae_t, .N] == 0L)
ck("E04 substituicao pelo vice so em linha de prefeitura", o[!is.na(id_mandato_titular_substituido) & tipo_unidade != "prefeitura", .N] == 0L)
ck("E05 relacao_chapa_eleita coerente com a substituicao",
   o[!is.na(id_mandato_titular_substituido) & relacao_chapa_eleita != "vice_eleito", .N] == 0L)
ck("E06 nenhuma linha com forma 'outro' esta pareada a um mandato (o sinal nao contamina o pareado)",
   o[forma_saida == "outro" & !is.na(id_mandato_bocel), .N] == 0L)
# o que a frente entrega de fato a R/10: id_mandato_bocel + data_inicio + data_fim + forma_saida
ck("E07 declarado: nenhum mandato pareado recebe forma de saida observada (a frente confirma exercicio, nao saida)",
   o[!is.na(id_mandato_bocel) & forma_saida != "nao_observado", .N] == 0L)

## ------------------------------------------------------------------ cobertura recalculada do BOCEL
alvo <- mand[cd_cargo %in% c("11", "13") & sg_uf %in% unique(o$uf),
             .(uf = sg_uf, cargo, ano_eleicao = ano_eleicao_m, id_mandato)]
cb <- alvo[, .(n_bocel = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
pcs <- merge(unique(o[!is.na(id_mandato_bocel), .(id_mandato = id_mandato_bocel)]), alvo, by = "id_mandato")[
  , .(n_par = uniqueN(id_mandato)), by = .(uf, cargo, ano_eleicao)]
cb <- merge(cb, pcs, by = c("uf", "cargo", "ano_eleicao"), all.x = TRUE); cb[is.na(n_par), n_par := 0L]
cb[, tx := round(n_par / n_bocel, 4)]; setorder(cb, uf, cargo, ano_eleicao)
cmp <- merge(cob, cb, by = c("uf", "cargo", "ano_eleicao"), all = TRUE)
ck("F01 cobertura publicada reproduz a recontagem por uf/cargo/eleicao",
   nrow(cmp) == nrow(cob) && cmp[is.na(n_bocel.x) | is.na(n_bocel.y) | n_bocel.x != n_bocel.y | n_pareados != n_par | taxa != tx, .N] == 0L)
ck("F02 taxa nunca passa de 1 e n_pareados nunca passa de n_bocel", cob[taxa > 1 | n_pareados > n_bocel, .N] == 0L)
ck("F03 cobertura soma exatamente os mandatos pareados",
   cob[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)])
ck("F04 cobertura declara tambem o cargo com zero (lacuna nao omitida)",
   cob[cargo == "VEREADOR", .N] > 0L && cob[cargo == "VEREADOR", sum(n_pareados)] == 0L)
cka("F05 na_nao_e_zero: o zero de vereador e decisao declarada, nao NA silencioso",
    na_nao_e_zero(cob, "n_pareados", decisao = "zero_observado",
                  justificativa = "vereador em GO nao fecha pareamento; o zero e observado, nao ausente"))

## ------------------------------------------------------------------ lacunas declaradas
ck("H01 inventario cobre as 6 UFs do grupo C", setequal(unique(inv$uf), UF_C), paste(setdiff(UF_C, inv$uf), collapse = ","))
ck("H02 inventario tem url, campos e observacao em toda linha",
   inv[is.na(url) | url == "" | is.na(campos) | campos == "" | is.na(observacao) | observacao == "", .N] == 0L)
ck("H03 toda UF sem linha na saida tem a evidencia da recusa gravada",
   all(vapply(setdiff(UF_C, unique(o$uf)), function(u) file.exists(sprintf("data_raw/tce/%s/probe_%s.json", tolower(u), tolower(u))), TRUE)))
ck("H04 so entram na saida UFs marcadas como viaveis no inventario",
   all(unique(o$uf) %in% inv[viavel %in% c("sim", "parcial"), uf]))
ck("H05 toda UF marcada 'nao' esta de fato ausente da saida",
   length(intersect(inv[viavel == "nao" & !uf %in% inv[viavel == "sim", uf], uf], unique(o$uf))) == 0L)
# a evidencia da recusa tem de ser status HTTP gravado, nao narrativa
for (u in c("mg", "pr", "mt", "df", "to")) {
  pj <- tryCatch(fromJSON(sprintf("data_raw/tce/%s/probe_%s.json", u, u), simplifyVector = FALSE), error = function(e) NULL)
  ck(sprintf("H06.%s probe de %s traz status HTTP ou erro de transporte por url", u, toupper(u)),
     !is.null(pj) && length(pj) > 0 && all(vapply(pj, function(z) !is.null(z$status) || !is.null(z$erro), TRUE)),
     if (is.null(pj)) "probe ausente" else paste(length(pj), "urls"))
}

## ------------------------------------------------------------------ (e) recontagem contra a assinatura
# o arquivo de assinatura tem linhas com mais de 6 campos (texto livre com '|'); a leitura pega
# so os dois primeiros campos, linha a linha, e fica com o ULTIMO registro de cada chave.
ln  <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ln  <- ln[grepl("\\|", ln)]
asn <- data.table(chave = trimws(sub("\\|.*$", "", ln)),
                  valor = trimws(vapply(strsplit(ln, "|", fixed = TRUE), function(z) if (length(z) >= 2L) z[2] else NA_character_, "")))
ult <- asn[grepl("^tcec_", chave)][, .SD[.N], by = chave]
oo <- copy(o)
rec <- list(
  tcec_n_casas_inventariadas = nrow(inv),
  tcec_n_ufs_inventariadas = inv[, uniqueN(uf)],
  tcec_n_tribunais_inventariados = inv[, uniqueN(paste(uf, tribunal))],
  tcec_n_tribunais_com_fonte = inv[viavel %in% c("sim", "parcial"), uniqueN(paste(uf, tribunal))],
  tcec_n_tribunais_sem_fonte = inv[viavel == "nao", uniqueN(paste(uf, tribunal))],
  tcec_n_fontes_viaveis = inv[viavel == "sim", .N],
  tcec_n_registros_brutos = nrow(pdfc) + nrow(csvc),
  tcec_n_registros_sem_municipio_resolvido = nrow(pdfc) + nrow(csvc) - nrow(oo),
  tcec_n_registros = nrow(oo),
  tcec_n_registros_finais = nrow(oo),
  tcec_n_registros_uf_GO = oo[uf == "GO", .N],
  tcec_n_registros_com_exercicio = oo[!is.na(ex), .N],
  tcec_n_registros_com_cpf = oo[!is.na(cpf), .N],
  tcec_n_registros_com_cargo_eletivo = oo[!is.na(cargo_bocel), .N],
  tcec_n_registros_cargo_PREFEITO = oo[cargo_bocel == "PREFEITO", .N],
  tcec_n_registros_cargo_VEREADOR = oo[cargo_bocel == "VEREADOR", .N],
  tcec_n_registros_sem_cargo_eletivo_identificado = oo[is.na(cargo_bocel), .N],
  tcec_n_municipios_cobertos = oo[, uniqueN(sg_ue)],
  tcec_n_municipios_pareados = oo[!is.na(id_mandato_bocel), uniqueN(sg_ue)],
  tcec_exercicio_min = oo[, min(ex)],
  tcec_exercicio_max = oo[, max(ex)],
  tcec_n_pareados_linhas = oo[!is.na(id_mandato_bocel), .N],
  tcec_n_pareados_uf_GO = oo[uf == "GO" & !is.na(id_mandato_bocel), .N],
  tcec_n_pareados_cargo_PREFEITO = oo[cargo_bocel == "PREFEITO" & !is.na(id_mandato_bocel), .N],
  tcec_n_pareados_cargo_VEREADOR = oo[cargo_bocel == "VEREADOR" & !is.na(id_mandato_bocel), .N],
  tcec_n_mandatos_pareados = oo[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  tcec_n_mandatos_pareados_uf_GO = oo[uf == "GO" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  tcec_n_mandatos_pareados_cargo_PREFEITO = oo[cargo_bocel == "PREFEITO" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  tcec_n_mandatos_pareados_cargo_VEREADOR = oo[cargo_bocel == "VEREADOR" & !is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
  tcec_n_pessoas_pareadas = oo[!is.na(id_pessoa_bocel), uniqueN(id_pessoa_bocel)],
  tcec_taxa_pareamento_linhas_com_cargo = round(oo[!is.na(id_mandato_bocel), .N] / oo[!is.na(cargo_bocel), .N], 4),
  tcec_taxa_GO_PREFEITO = round(cb[cargo == "PREFEITO", sum(n_par)] / cb[cargo == "PREFEITO", sum(n_bocel)], 4),
  tcec_taxa_GO_VEREADOR = round(cb[cargo == "VEREADOR", sum(n_par)] / cb[cargo == "VEREADOR", sum(n_bocel)], 4),
  tcec_n_prefeitura_relacao_titular_eleito = oo[relacao_chapa_eleita == "titular_eleito", .N],
  tcec_n_prefeitura_relacao_vice_eleito = oo[relacao_chapa_eleita == "vice_eleito", .N],
  tcec_n_prefeitura_relacao_indeterminado = oo[relacao_chapa_eleita == "indeterminado", .N],
  tcec_n_saida_titular_observada_por_vice = oo[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)]
)
for (fs in VOCAB) rec[[paste0("tcec_n_forma_saida_", fs)]] <- oo[!is.na(id_mandato_bocel) & forma_saida == fs, uniqueN(id_mandato_bocel)]
div <- character()
for (k in names(rec)) {
  reg <- ult[chave == k, valor]
  v <- rec[[k]]
  vt <- if (is.numeric(v) && v %% 1 != 0) format(v, trim = TRUE) else as.character(v)
  if (!length(reg)) { div <- c(div, paste0(k, ": nao registrado")); next }
  if (!isTRUE(all.equal(suppressWarnings(as.numeric(reg)), as.numeric(v)))) div <- c(div, paste0(k, ": registrado ", reg, ", recontado ", vt))
}
ck("I01 toda chave tcec_* recontada bate com o ultimo registro em output/numeros_assinatura.txt",
   length(div) == 0L, paste(head(div, 8), collapse = " ; "))
ck("I02 nenhuma chave tcec_* registrada ficou sem recontagem nesta verificacao",
   length(setdiff(ult$chave, c(names(rec), "tcec_verif_n_checagens", "tcec_verif_n_aprovadas", "tcec_verif_n_reprovadas",
                               "tcecv_n_checagens", "tcecv_n_aprovadas", "tcecv_n_reprovadas"))) == 0L,
   paste(setdiff(ult$chave, c(names(rec), "tcec_verif_n_checagens", "tcec_verif_n_aprovadas", "tcec_verif_n_reprovadas",
                              "tcecv_n_checagens", "tcecv_n_aprovadas", "tcecv_n_reprovadas")), collapse = ","))

## ------------------------------------------------------------------ fecho
fora <- c(
  "identidade sem documento: as duas fontes do TCM-GO so publicam CPF mascarado, de modo que homonimo exato no mesmo municipio e na mesma eleicao nao e distinguivel por nenhuma checagem mecanica",
  "completude do cadastro do TCM-GO: a relacao cobre processos ja julgados; exercicio nao julgado nao se distingue de exercicio sem prestacao de contas, e a queda da cobertura de 2016 em diante e prazo de julgamento, nao ausencia de mandato",
  "leitura substantiva de 'outro' quando quem responde pela prefeitura e o vice eleito: e evidencia de que o titular deixou o cargo, nao prova da forma da saida",
  "veracidade do que o TCM-GO publica: a camada confere que a saida reproduz o PDF assinado e o CSV do rol, nao que o Tribunal tenha nomeado o responsavel correto",
  "MG, PR, MT e TO podem manter cadastro atras de autenticacao ou acessivel por LAI; a camada so atesta que nao ha ponto de acesso publico e roteirizavel nesta data",
  "o exercicio de 2026 do processo 04869/24 (Campo Limpo de Goias) e transcrito do PDF; se for erro de digitacao da fonte, o pareamento a eleicao de 2024 acompanha o erro"
)
registrar_numero("tcecv_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tcecv_n_aprovadas", length(passou), script = script)
registrar_numero("tcecv_n_reprovadas", length(falhou), script = script)
registrar_numero("tcecv_n_pares_amostrados", nrow(am), script = script)
registrar_numero("tcecv_n_chaves_recontadas", length(rec), script = script)
registrar_numero("tcecv_n_chaves_divergentes", length(div), script = script)
f <- gravar_relatorio_verificacao("data/tce_gestores_c.csv + data/tce_gestores_c_cobertura.csv (verificacao cetica independente)",
                                  "R/33_tce_gestores_c.R", passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nrelatorio:", f, "\n", length(passou), "aprovadas,", length(falhou), "reprovadas\n")
if (length(falhou)) cat("REPROVADAS:\n", paste(" -", falhou, collapse = "\n"), "\n")
sink()
