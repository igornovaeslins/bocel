# verifica_wikidata.R — verificacao cetica independente de data/wikidata_mandatos.csv e
# data/wikidata_obitos.csv (frente Wikidata, construida por python/fetch_wikidata.py e
# R/12_wikidata_mandatos.R). Reconta os numeros a partir dos arquivos de saida, aplica os
# asserts de rigor e procura erros silenciosos (statement duplicado, mandato atribuido fora da
# janela, obito fora do exercicio, pareamento sem nascimento igual).
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_wikidata.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_wikidata.R"
logf <- "logs/verifica_wikidata.log"; sink(logf, split = TRUE)
cat("verifica_wikidata.R —", format(Sys.time()), "\n")
problemas <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); cat("PROBLEMA:", msg, "\n") }
# 05/09/2026: o verificador so acumulava problemas e nao gravava relatorio_verificacao_*.json; cada secao
# que termina sem problema novo entra em `passou`, e o relatorio JSON e gravado antes do sink().
passou <- character(); .n_prob <- 0L
secao <- function(nome) { if (length(problemas) == .n_prob) passou <<- c(passou, nome); .n_prob <<- length(problemas) }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", "", x); gsub(" +", " ", trimws(x)) }
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])

## 1. arquivos, colunas, codigo de ausente ---------------------------------------------------
f_w <- "data/wikidata_mandatos.csv"; f_o <- "data/wikidata_obitos.csv"
stopifnot(file.exists(f_w), file.exists(f_o))
bruto <- fread(f_w, na.strings = NULL, colClasses = "character", encoding = "UTF-8")
cols <- c("qid","statement","nome_wikidata","nome_completo","dt_nascimento","dt_morte","cargo","posicao_wd",
          "entidade","ibge_entidade","uf","inicio","fim","causa_fim_original","forma_saida","partido_wd",
          "eleicao_wd","substitui_qid","substituido_por_qid","id_pessoa_bocel","id_mandato_bocel","tipo_pareamento","url")
faltam <- setdiff(cols, names(bruto)); if (length(faltam)) prob(paste("colunas ausentes:", paste(faltam, collapse = ",")))
n_vazio <- sum(bruto == "", na.rm = TRUE); cat("celulas vazias:", n_vazio, "\n")
if (n_vazio > 0) prob(sprintf("%d celulas vazias em vez de 'NA'", n_vazio))

w <- fread(f_w, na.strings = "NA", colClasses = "character", encoding = "UTF-8")
ob <- fread(f_o, na.strings = "NA", colClasses = "character", encoding = "UTF-8")
mand <- fread("data/mandatos.csv", na.strings = "NA", colClasses = "character")
pess <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character")
cat("statements:", nrow(w), " obitos:", nrow(ob), "\n")

secao("1. arquivos, colunas, codigo de ausente")
## 2. chaves e vocabularios -----------------------------------------------------------------
checa_unica(as.data.frame(w), "statement")
checa_unica(as.data.frame(ob), c("id_pessoa", "id_mandato"))
vocab <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca","nao_tomou_posse","suplente_efetivado","outro")
in_set(w$forma_saida, vocab, nome = "forma_saida")
in_set(w$cargo, c("PRESIDENTE", "VICE-PRESIDENTE", "GOVERNADOR","VICE-GOVERNADOR","DEPUTADO ESTADUAL","DEPUTADO DISTRITAL","PREFEITO"), permitir_na = FALSE, nome = "cargo")
ufs <- c("BR", "AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
in_set(w$uf, ufs, nome = "uf")
in_set(w$tipo_pareamento, c("nome_completo+nascimento","rotulo+nascimento","tokens_rotulo+nascimento",
                            "token_rotulo+nascimento+uf+cargo","nome+uf+cargo+ano","tokens_rotulo+uf+cargo+ano"), nome = "tipo_pareamento")
in_set(ob$dentro_do_mandato, "TRUE", permitir_na = FALSE, nome = "dentro_do_mandato")
in_set(ob$fonte, "wikidata", permitir_na = FALSE, nome = "fonte")
em_faixa(as.integer(substr(w$inicio, 1, 4)), 1500, 2030, nome = "ano_inicio")  # o Wikidata traz prefeitos coloniais (P6 municipal)
em_faixa(as.integer(substr(w$fim, 1, 4)), 1500, 2030, nome = "ano_fim")
em_faixa(as.integer(substr(w$dt_nascimento, 1, 4)), 1500, 2010, nome = "ano_nascimento")
if (w[!is.na(inicio) & !is.na(fim) & fim < inicio, .N] > 0) prob(sprintf("%d statements com fim < inicio", w[!is.na(inicio) & !is.na(fim) & fim < inicio, .N]))
if (any(!grepl("^Q\\d+$", w$qid))) prob("qid fora do padrao Q\\d+")
if (any(!grepl("^[Qq][0-9]+-", w$statement))) prob("statement fora do padrao Q<id>-<uuid>")  # um statement legado usa "q" minusculo
cat("statements P6 (identificador comeca pelo QID da entidade, nao da pessoa):", w[!startsWith(statement, qid), .N], "\n")
if (any(w$url != paste0("https://www.wikidata.org/wiki/", w$qid))) prob("url nao corresponde ao qid")
if (any(!is.na(w$id_mandato_bocel) & is.na(w$id_pessoa_bocel))) prob("mandato atribuido sem pessoa pareada")
if (any(is.na(w$id_pessoa_bocel) != is.na(w$tipo_pareamento))) prob("tipo_pareamento inconsistente com id_pessoa_bocel")

secao("2. chaves e vocabularios")
## 3. pareamento: pessoa e mandato existem, cargo/UF/janela conferem ---------------------------
p <- w[!is.na(id_pessoa_bocel)]
if (any(!p$id_pessoa_bocel %in% pess$id_pessoa)) prob("id_pessoa_bocel inexistente em pessoas.csv")
m <- join_seguro(as.data.frame(w[!is.na(id_mandato_bocel)]),
                 as.data.frame(mand[, .(id_mandato_bocel = id_mandato, id_pessoa_m = id_pessoa, cargo_m = cargo, uf_m = sg_uf,
                                        mandato_inicio, mandato_fim, forma_saida_m = forma_saida, fonte_forma_saida, data_fim_efetiva)]),
                 by = "id_mandato_bocel", cardinalidade = "many-to-one", tipo = "inner")
m <- as.data.table(m)
if (nrow(m) != w[!is.na(id_mandato_bocel), .N]) prob("id_mandato_bocel inexistente em mandatos.csv")
if (any(m$id_pessoa_bocel != m$id_pessoa_m)) prob("mandato atribuido pertence a outra pessoa")
if (any(!(m$cargo == m$cargo_m | (m$cargo == "DEPUTADO ESTADUAL" & m$cargo_m == "DEPUTADO DISTRITAL")))) prob("cargo do statement difere do cargo do mandato")
if (any(!is.na(m$uf) & m$uf != m$uf_m)) prob("UF do statement difere da UF do mandato")
# referencia (vespera do fim, ou inicio) dentro da janela convencional
m[, ref := fifelse(!is.na(fim), as.IDate(fim) - 1L, as.IDate(inicio))]
m[, `:=`(ini_c = as.IDate(mandato_inicio), fim_c = as.IDate(mandato_fim), ini_d = as.IDate(inicio))]
m[, comeca_junto := !is.na(ini_d) & ini_d >= ini_c - 60L & ini_d <= ini_c + 60L]
fora <- m[is.na(ref) | ref < ini_c - 60L | (!is.na(fim) & ref < ini_c) | ref > fim_c + 60L | (ref > fim_c & !comeca_junto)]
cat("statements atribuidos com referencia fora da janela:", nrow(fora), "\n")
if (nrow(fora)) prob(sprintf("%d statements atribuidos com referencia fora da janela do mandato", nrow(fora)))
# statement de legislatura anterior (fim ate 60 dias apos o inicio do mandato) atribuido ao mandato seguinte
ant <- m[!is.na(fim) & !is.na(ini_d) & ini_d < ini_c - 60L & ref <= ini_c + 60L]
if (nrow(ant)) prob(sprintf("%d statements da legislatura anterior atribuidos ao mandato seguinte", nrow(ant)))
# um mandato nao pode receber statements de duas pessoas
if (m[, uniqueN(id_pessoa_bocel), by = id_mandato_bocel][V1 > 1, .N] > 0) prob("mandato com statements de pessoas diferentes")

secao("3. pareamento: pessoa e mandato existem, cargo/UF/janela conferem")
## 4. regras de pareamento: nascimento igual e tokens contidos ------------------------------
p <- merge(p, pess[, .(id_pessoa_bocel = id_pessoa, nome_bocel = nome, nasc_bocel = dt_nascimento)], by = "id_pessoa_bocel")
p[, nome_norm := norm(nome_bocel)]
p[, nasc_igual := !is.na(dt_nascimento) & dt_nascimento == nasc_bocel]
p[, tokens_contidos := mapply(function(a, b) length(a) > 0 && all(a %in% b), tok(norm(nome_wikidata)), tok(nome_norm))]
p[, algum_token := mapply(function(a, b) length(a) > 0 && any(a %in% b), tok(norm(nome_wikidata)), tok(nome_norm))]
regras_nasc <- c("nome_completo+nascimento","rotulo+nascimento","tokens_rotulo+nascimento","token_rotulo+nascimento+uf+cargo")
if (p[tipo_pareamento %in% regras_nasc & !nasc_igual, .N] > 0) prob("pareamento por nascimento com nascimento diferente")
if (p[tipo_pareamento == "nome_completo+nascimento" & norm(fcoalesce(nome_completo, nome_wikidata)) != nome_norm, .N] > 0) prob("regra nome_completo+nascimento com nome diferente")
if (p[tipo_pareamento == "tokens_rotulo+nascimento" & !tokens_contidos, .N] > 0) prob("regra tokens_rotulo+nascimento com token fora do nome civil")
if (p[tipo_pareamento == "token_rotulo+nascimento+uf+cargo" & !algum_token, .N] > 0) prob("regra token_rotulo+nascimento+uf+cargo sem token no nome civil")
if (p[tipo_pareamento == "tokens_rotulo+uf+cargo+ano" & !tokens_contidos, .N] > 0) prob("regra tokens_rotulo+uf+cargo+ano com token fora do nome civil")
if (p[tipo_pareamento == "nome+uf+cargo+ano" & norm(fcoalesce(nome_completo, nome_wikidata)) != nome_norm, .N] > 0) prob("regra nome+uf+cargo+ano com nome diferente")
# regras sem nascimento: a pessoa tem mandato do cargo na UF
sem_nasc <- p[tipo_pareamento %in% c("nome+uf+cargo+ano","tokens_rotulo+uf+cargo+ano","token_rotulo+nascimento+uf+cargo")]
chk <- merge(sem_nasc[, .(statement, id_pessoa_bocel, cargo, uf)],
             unique(mand[, .(id_pessoa_bocel = id_pessoa, cargo_m = cargo, uf_m = sg_uf)]), by = "id_pessoa_bocel", allow.cartesian = TRUE)
chk <- chk[(cargo == cargo_m | (cargo == "DEPUTADO ESTADUAL" & cargo_m == "DEPUTADO DISTRITAL")) & uf == uf_m]
if (!all(sem_nasc$statement %in% chk$statement)) prob("pareamento por UF+cargo sem mandato do cargo na UF")
# nascimento divergente nas regras sem nascimento (documentado, nao e erro): quantos e de que tipo
div <- p[tipo_pareamento %in% c("nome+uf+cargo+ano","tokens_rotulo+uf+cargo+ano") & !is.na(dt_nascimento)]
div[, tipo_div := fcase(dt_nascimento == nasc_bocel, "igual",
                        substr(dt_nascimento, 6, 10) == "01-01", "wikidata_so_ano",
                        substr(dt_nascimento, 1, 4) == substr(nasc_bocel, 1, 4) & substr(dt_nascimento, 6, 7) == substr(nasc_bocel, 9, 10) & substr(dt_nascimento, 9, 10) == substr(nasc_bocel, 6, 7), "dia_mes_trocados",
                        substr(dt_nascimento, 1, 4) == substr(nasc_bocel, 1, 4), "mesmo_ano_outro_dia",
                        default = "ano_diferente")]
cat("regras sem nascimento, statements com nascimento no Wikidata:", nrow(div), "\n"); print(div[, .N, by = tipo_div])
fwrite(div[, .(statement, qid, nome_wikidata, dt_nascimento, nome_bocel, nasc_bocel, tipo_div, cargo, uf, inicio, tipo_pareamento, url)],
       "output/verificacao/wikidata_pareamentos_nascimento_divergente.csv")

secao("4. regras de pareamento: nascimento igual e tokens contidos")
## 5. amostra por regra para conferencia manual --------------------------------------------
am <- p[, if (.N > 25) .SD[sample(.N, 25)] else .SD, by = tipo_pareamento]
fwrite(am[, .(tipo_pareamento, qid, url, nome_wikidata, nome_completo, dt_nascimento, cargo, uf, inicio, fim,
              id_pessoa_bocel, nome_bocel, nasc_bocel, id_mandato_bocel, nasc_igual, tokens_contidos)],
       "output/verificacao/wikidata_amostra_pareamentos.csv")
cat("amostra por regra:\n"); print(am[, .N, by = tipo_pareamento])

secao("5. amostra por regra para conferencia manual")
## 6. obitos ------------------------------------------------------------------------------
o <- join_seguro(as.data.frame(ob), as.data.frame(mand[, .(id_mandato, id_pessoa_m = id_pessoa, cargo_m = cargo, mandato_inicio, mandato_fim, forma_saida_m = forma_saida, fonte_forma_saida)]),
                 by = "id_mandato", cardinalidade = "one-to-one", tipo = "inner")
o <- as.data.table(o)
if (nrow(o) != nrow(ob)) prob("obito com id_mandato inexistente")
if (any(o$id_pessoa != o$id_pessoa_m)) prob("obito: mandato de outra pessoa")
if (any(o$cargo != o$cargo_m)) prob("obito: cargo difere do mandato")
if (any(!(o$data_morte >= o$mandato_inicio & o$data_morte <= o$mandato_fim))) prob("obito fora da janela do mandato")
# a data de morte tem de ser a do Wikidata da pessoa pareada
dm <- unique(w[!is.na(id_pessoa_bocel) & !is.na(dt_morte), .(id_pessoa = id_pessoa_bocel, dt_morte)])
if (dm[, .N, by = id_pessoa][N > 1, .N] > 0) prob("pessoa do BOCEL com mais de uma data de morte no Wikidata")
o2 <- merge(o, dm, by = "id_pessoa", all.x = TRUE)
if (any(is.na(o2$dt_morte) | o2$dt_morte != o2$data_morte)) prob("data_morte do obito difere do dt_morte do Wikidata")
# pessoa com morte no Wikidata dentro de uma janela de mandato mas ausente de obitos (sem statement que a exclua)
todos <- merge(dm, mand[, .(id_pessoa, id_mandato, mandato_inicio, mandato_fim)], by = "id_pessoa", allow.cartesian = TRUE)
todos <- todos[dt_morte >= mandato_inicio & dt_morte <= mandato_fim]
saiu <- w[!is.na(id_mandato_bocel) & !is.na(fim), .(fim_wd = max(as.IDate(fim))), by = .(id_mandato = id_mandato_bocel)]
todos <- merge(todos, saiu, by = "id_mandato", all.x = TRUE)
esperados <- todos[is.na(fim_wd) | fim_wd >= as.IDate(dt_morte) - 1L]
excluidos <- todos[!is.na(fim_wd) & fim_wd < as.IDate(dt_morte) - 1L]
cat("obitos esperados (recontagem):", nrow(esperados), " excluidos por saida anterior no Wikidata:", nrow(excluidos), "\n")
if (nrow(excluidos)) print(excluidos)
if (!setequal(paste(esperados$id_pessoa, esperados$id_mandato), paste(ob$id_pessoa, ob$id_mandato))) prob("obitos.csv difere da recontagem")
if (any(paste(excluidos$id_pessoa, excluidos$id_mandato) %in% paste(ob$id_pessoa, ob$id_mandato))) prob("obito mantido apesar de saida anterior registrada no Wikidata")
# forma_saida falecimento no statement pareado ao mandato do obito
fs <- w[id_mandato_bocel %in% ob$id_mandato & !is.na(forma_saida)]
if (fs[!forma_saida %in% c("falecimento", "renuncia", "cassacao", "afastamento"), .N] > 0) prob("statement de mandato com obito sem forma_saida falecimento")
# integracao: mandatos com fonte wikidata_obito devem estar em obitos e ter forma falecimento
mo <- mand[fonte_forma_saida == "wikidata_obito"]
if (any(!mo$id_mandato %in% ob$id_mandato)) prob("mandato com fonte wikidata_obito ausente de obitos.csv")
if (any(mo$forma_saida != "falecimento")) prob("mandato com fonte wikidata_obito sem forma falecimento")

secao("6. obitos")
## 7. forma_saida no statement ------------------------------------------------------------
cat("statements com forma_saida (pela causa do Wikidata) sem mandato atribuido:", w[!is.na(forma_saida) & is.na(id_mandato_bocel), .N], "\n")
if (w[!is.na(forma_saida) & is.na(id_mandato_bocel) & is.na(causa_fim_original), .N] > 0) prob("forma_saida sem causa e sem mandato atribuido")
cn <- norm(w$causa_fim_original)
if (w[grepl("RENUNCIA", cn) & forma_saida != "renuncia", .N] > 0) prob("causa renuncia sem forma renuncia")
if (w[grepl("MORTE|FALECIMENTO|OBITO|DEATH", cn) & forma_saida != "falecimento", .N] > 0) prob("causa morte sem forma falecimento")
fr <- m[forma_saida == "fim_regular" & is.na(causa_fim_original)]
if (fr[as.IDate(fim) < as.IDate(mandato_fim) - 45L, .N] > 0) prob("fim_regular inferido com fim mais de 45 dias antes do fim convencional")
print(w[, .N, by = .(cargo, forma_saida)][order(cargo, -N)])

secao("7. forma_saida no statement")
## 8. cobertura por cargo recontada x numeros registrados --------------------------------
cob <- w[, .(n_statements = .N, n_pareados_pessoa = sum(!is.na(id_pessoa_bocel)),
             n_pareados_mandato = uniqueN(id_mandato_bocel[!is.na(id_mandato_bocel)])), by = cargo]
tot <- mand[cargo %in% cob$cargo, .(n_bocel = .N), by = cargo]
cob <- merge(cob, tot, by = "cargo")[, cobertura := round(n_pareados_mandato / n_bocel, 4)]
print(cob)
arq <- fread("output/verificacao/wikidata_cobertura_por_cargo.csv")
cmp <- merge(cob, arq, by = "cargo", suffixes = c("", "_arq"))
if (any(cmp$n_statements != cmp$n_statements_arq | cmp$n_pareados_mandato != cmp$n_pareados_mandato_arq | cmp$n_bocel != cmp$n_bocel_arq))
  prob("wikidata_cobertura_por_cargo.csv difere da recontagem")
# parse robusto do registro (chave | valor | ...): so os dois primeiros campos
ass_l <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- rbindlist(lapply(strsplit(ass_l[grepl("|", ass_l, fixed = TRUE)], "|", fixed = TRUE), function(p) data.table(V1 = trimws(p[1]), V2 = trimws(p[2]))))
setnames(ass, c("chave", "valor"))
ult <- ass[, .SD[.N], by = chave]
reg <- function(k) { v <- ult[chave == k, valor]; if (length(v)) as.numeric(v) else NA_real_ }
for (i in seq_len(nrow(cob))) {
  k <- gsub(" |-", "_", tolower(cob$cargo[i]))
  if (!identical(reg(sprintf("wd_%s_statements", k)), as.numeric(cob$n_statements[i]))) prob(sprintf("wd_%s_statements registrado difere", k))
  if (!identical(reg(sprintf("wd_%s_mandatos_pareados", k)), as.numeric(cob$n_pareados_mandato[i]))) prob(sprintf("wd_%s_mandatos_pareados registrado difere", k))
}
if (!identical(reg("wd_n_obitos_em_mandato"), as.numeric(nrow(ob)))) prob("wd_n_obitos_em_mandato registrado difere")
if (!identical(reg("wd_n_forma_saida_observada"), as.numeric(w[!is.na(forma_saida) & !is.na(id_mandato_bocel), .N]))) prob("wd_n_forma_saida_observada registrado difere")
# integracao em mandatos.csv
int <- mand[fonte_forma_saida %in% c("wikidata", "wikidata_obito"), .N, by = fonte_forma_saida]
print(int)
if (any(!mand[fonte_forma_saida == "wikidata", id_mandato] %in% w$id_mandato_bocel)) prob("mandato com fonte wikidata sem statement atribuido")

secao("8. cobertura por cargo recontada x numeros registrados")
## 9. registro -----------------------------------------------------------------------------
for (i in seq_len(nrow(cob))) {
  k <- gsub(" |-", "_", tolower(cob$cargo[i]))
  registrar_numero(sprintf("verif_wd_%s_mandatos_distintos_pareados", k), cob$n_pareados_mandato[i], script = script)
  registrar_numero(sprintf("verif_wd_%s_cobertura", k), cob$cobertura[i], script = script)
}
registrar_numero("verif_wd_statements", nrow(w), script = script)
registrar_numero("verif_wd_statements_duplicados", sum(duplicated(w$statement)), script = script)
registrar_numero("verif_wd_obitos", nrow(ob), script = script)
registrar_numero("verif_wd_obitos_excluidos_saida_anterior", nrow(excluidos), script = script)
registrar_numero("verif_wd_pareamentos_nascimento_divergente", div[tipo_div != "igual", .N], script = script)
registrar_numero("verif_wd_mandatos_fonte_wikidata", mand[fonte_forma_saida == "wikidata", .N], script = script)
registrar_numero("verif_wd_mandatos_fonte_wikidata_obito", mand[fonte_forma_saida == "wikidata_obito", .N], script = script)
registrar_numero("verif_wd_n_problemas", length(problemas), script = script)
cat("\nPROBLEMAS:", length(problemas), "\n"); if (length(problemas)) cat(paste("-", problemas), sep = "\n")
fora <- c("veracidade das datas e causas do Wikidata alem da amostra por regra (output/verificacao/wikidata_amostra_pareamentos.csv)",
          "homonimos nas regras sem nascimento (nome+uf+cargo+ano, tokens_rotulo+uf+cargo+ano): o pareamento e por nome")
gravar_relatorio_verificacao(alvo = paste(f_w, f_o, sep = " + "), script = script, passou = passou, falhou = problemas, fora_de_cobertura = fora)
sink()
if (length(problemas)) quit(status = 1)
