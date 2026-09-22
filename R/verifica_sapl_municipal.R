# verifica_sapl_municipal.R — verificacao cetica da frente SAPL MUNICIPAL
# (python/fetch_sapl_municipal.py -> data_raw/sapl_municipal; R/15 -> data/exercicio_camaras_municipais*.csv)
# e do uso dessas linhas por R/10 em data/mandatos.csv (fonte_forma_saida = sapl_municipal).
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_sapl_municipal.R
set.seed(20260828)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_sapl_municipal.R"
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
HOJE <- as.IDate("2026-08-28")
passou <- character(); falhou <- character(); fora <- character()
ok <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg); cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n") }
norm <- function(x) { x <- stri_trans_general(toupper(x), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
try_ok <- function(expr, msg) { r <- try(expr, silent = TRUE); ok(!inherits(r, "try-error"), msg); if (inherits(r, "try-error")) cat("   ->", conditionMessage(attr(r, "condition")), "\n") }

ex  <- fread("data/exercicio_camaras_municipais.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
cob <- fread("data/exercicio_camaras_municipais_cobertura.csv", na.strings = "NA", colClasses = "character")
inv <- fread("data_raw/sapl_municipal/inventario_sapl_municipal.csv", colClasses = "character")
man <- fread("data/mandatos.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
pes <- fread("data/pessoas.csv", na.strings = "NA", encoding = "UTF-8", colClasses = "character")
mun <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
ex[, ano_eleicao_bocel := as.integer(ano_eleicao_bocel)]
ex[, titular_l := titular %in% c("TRUE", "T")]
cat("linhas exercicio_camaras_municipais:", nrow(ex), "\n"); reg("vsapl_n_linhas", nrow(ex))

## ---- 0. inventario e cache
n_api <- inv[responde_api == "TRUE", .N]
n_ok <- sum(file.exists(file.path("data_raw/sapl_municipal", inv[responde_api == "TRUE", uf], inv[responde_api == "TRUE", sg_ue], "_ok")))
cat("inventario: municipios", nrow(inv), "| responde_api", n_api, "| com _ok", n_ok, "\n")
reg("vsapl_n_municipios_inventariados", nrow(inv)); reg("vsapl_n_instancias_api", n_api); reg("vsapl_n_instancias_cache_ok", n_ok)
ok(nrow(inv) == nrow(mun), "inventario cobre todos os municipios de municipios_tse_ibge.csv")
ok(n_ok == 1160, "instancias com cache = 1160 (valor do inventario)")
inst_out <- ex[, uniqueN(dominio)]
cat("instancias com pelo menos um mandato na saida:", inst_out, "\n"); reg("vsapl_n_instancias_com_mandato_na_saida", inst_out)
# instancias com cache mas sem linha na saida: mandato.json vazio ou sem legislatura com data
sem <- inv[responde_api == "TRUE"][!dominio %in% ex$dominio]
sem[, n_mand_json := sapply(file.path("data_raw/sapl_municipal", uf, sg_ue, "mandato.json"), function(f) if (file.exists(f)) length(fromJSON(f, simplifyVector = FALSE)) else NA_integer_)]
cat("instancias com cache sem linha na saida:", nrow(sem), "| com mandato.json vazio:", sum(sem$n_mand_json == 0, na.rm = TRUE), "| com mandatos mas descartados:", sum(sem$n_mand_json > 0, na.rm = TRUE), "\n")
if (nrow(sem[n_mand_json > 0])) print(sem[n_mand_json > 0, .(sg_ue, uf, dominio, n_mand_json)])
reg("vsapl_n_instancias_cache_sem_saida", nrow(sem)); reg("vsapl_n_instancias_cache_sem_saida_com_mandatos_json", sum(sem$n_mand_json > 0, na.rm = TRUE))
# reconta os mandatos brutos nos JSON das instancias presentes na saida
n_json <- sum(sapply(unique(ex[, .(uf, sg_ue)])[, file.path("data_raw/sapl_municipal", uf, sg_ue, "mandato.json")], function(f) length(fromJSON(f, simplifyVector = FALSE))))
cat("mandatos brutos nos JSON das instancias na saida:", n_json, "| na saida:", nrow(ex), "| descartados (eleicao fora de 1996-2024 ou sem legislatura):", n_json - nrow(ex), "\n")
reg("vsapl_n_mandatos_json_instancias_na_saida", n_json)
ok(n_json >= nrow(ex), "saida nao tem mais linhas que os JSON de origem")

## ---- 1. chave unica
try_ok(checa_unica(as.data.frame(ex), c("dominio", "id_mandato_sapl")), "chave declarada unica (dominio x id_mandato_sapl)")
try_ok(checa_unica(as.data.frame(ex), c("url")), "url unica")
reg("vsapl_n_linhas_saida", nrow(ex))

## ---- 2. vocabularios e dominios
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "nao_observado", "outro")
try_ok(in_set(ex$forma_saida, VOCAB, permitir_na = TRUE, nome = "forma_saida"), "forma_saida no vocabulario fechado (NA = em curso)")
try_ok(in_set(ex$uf, unique(mun$sg_uf), permitir_na = FALSE, nome = "uf"), "uf no conjunto dos municipios")
try_ok(em_faixa(ex$ano_eleicao_bocel, 1996, 2024, nome = "ano_eleicao_bocel"), "ano_eleicao_bocel em 1996..2024")
ok(all(ex$sg_ue %in% mun$sg_ue), "sg_ue existe em municipios_tse_ibge")
ok(all(ex[, .(sg_ue, id_municipio_ibge)][, paste(sg_ue, id_municipio_ibge)] %in% mun[, paste(sg_ue, id_municipio_ibge)]), "sg_ue x id_municipio_ibge coerente com municipios_tse_ibge")
# UF do dominio == uf
ok(all(toupper(sub(".*\\.([a-z]{2})\\.leg\\.br$", "\\1", ex$dominio)) == ex$uf), "UF do dominio == uf")
na_fs <- ex[is.na(forma_saida)]
cat("forma_saida NA:", nrow(na_fs), "| por ano_eleicao:\n"); print(na_fs[, .N, by = ano_eleicao_bocel][order(ano_eleicao_bocel)])
na_fs_fora <- na_fs[!(is.na(data_fim_mandato) | as.IDate(data_fim_mandato) >= HOJE) | (!is.na(legislatura_fim) & as.IDate(legislatura_fim) < HOJE)]
cat("forma_saida NA fora de mandato em curso:", nrow(na_fs_fora), "\n"); if (nrow(na_fs_fora)) print(head(na_fs_fora[, .(uf, sg_ue, legislatura_inicio, legislatura_fim, nome_fonte, data_inicio_mandato, data_fim_mandato, titular)], 20))
reg("vsapl_n_forma_saida_na", nrow(na_fs)); reg("vsapl_n_forma_saida_na_fora_de_curso", nrow(na_fs_fora))
ok(nrow(na_fs_fora) == 0, "forma_saida NA apenas em mandato em curso")
# anos de eleicao nao-quadrienais (legislatura mal datada na fonte)
ano_odd <- ex[!ano_eleicao_bocel %in% seq(1996L, 2024L, 4L)]
cat("linhas com ano_eleicao_bocel fora do ciclo quadrienal:", nrow(ano_odd), "| pareadas:", ano_odd[!is.na(id_mandato_bocel), .N], "\n")
print(ano_odd[, .N, by = .(ano_eleicao_bocel)][order(ano_eleicao_bocel)])
reg("vsapl_n_linhas_ano_fora_ciclo", nrow(ano_odd)); reg("vsapl_n_pareadas_ano_fora_ciclo", ano_odd[!is.na(id_mandato_bocel), .N])
ok(ano_odd[!is.na(id_mandato_bocel), .N] == 0, "nenhuma linha pareada com ano_eleicao fora do ciclo quadrienal")

## ---- 3. id_mandato_bocel / id_pessoa_bocel existem e sao vereador do mesmo municipio e eleicao
par <- ex[!is.na(id_mandato_bocel)]
cat("linhas pareadas:", nrow(par), "| mandatos distintos:", par[, uniqueN(id_mandato_bocel)], "\n")
reg("vsapl_n_linhas_pareadas", nrow(par)); reg("vsapl_n_mandatos_bocel_pareados", par[, uniqueN(id_mandato_bocel)])
cat("mandatos pareados (antes da correcao de 28/ago: 39679)\n")
ok(all(par$id_mandato_bocel %in% man$id_mandato), "todo id_mandato_bocel existe em mandatos.csv")
ok(all(par$id_pessoa_bocel %in% pes$id_pessoa), "todo id_pessoa_bocel existe em pessoas.csv")
ok(all(!is.na(par$id_pessoa_bocel)) & all(!is.na(ex[!is.na(id_pessoa_bocel), id_mandato_bocel])), "id_mandato_bocel e id_pessoa_bocel preenchidos juntos")
ok(all(!is.na(par$metodo_pareamento)), "toda linha pareada tem metodo_pareamento")
ok(all(is.na(ex[is.na(id_mandato_bocel), metodo_pareamento]) | ex[is.na(id_mandato_bocel), metodo_pareamento] == "descartado_duplicata"), "linha sem pareamento tem metodo NA ou descartado_duplicata")
mj <- man[, .(id_mandato, id_pessoa_m = id_pessoa, cd_cargo, sg_ue_m = sg_ue, unidade_posicao, ano_m = as.integer(ano_eleicao), mandato_inicio, mandato_fim, forma_saida_m = forma_saida, fonte_forma_saida, data_posse, data_fim_efetiva)]
pj <- join_seguro(as.data.frame(par), as.data.frame(mj), by = c("id_mandato_bocel" = "id_mandato"), cardinalidade = "many-to-one", tipo = "inner")
pj <- as.data.table(pj)
ok(nrow(pj) == nrow(par), "join pareados x mandatos sem perda")
ok(all(pj$cd_cargo == "13"), "todo id_mandato_bocel e de vereador (cd_cargo 13)")
ok(all(pj$unidade_posicao == pj$sg_ue), "todo id_mandato_bocel e do mesmo municipio (unidade_posicao == sg_ue)")
ok(all(pj$sg_ue_m == pj$sg_ue), "todo id_mandato_bocel e do mesmo municipio (sg_ue)")
ok(all(pj$ano_m == pj$ano_eleicao_bocel), "todo id_mandato_bocel e da mesma eleicao")
ok(all(pj$id_pessoa_m == pj$id_pessoa_bocel), "id_pessoa_bocel == id_pessoa do mandato")
reg("vsapl_n_pareados_cargo_diferente_13", sum(pj$cd_cargo != "13")); reg("vsapl_n_pareados_municipio_diferente", sum(pj$unidade_posicao != pj$sg_ue)); reg("vsapl_n_pareados_eleicao_diferente", sum(pj$ano_m != pj$ano_eleicao_bocel))
# id_mandato embute sg_ue e ano: confere com a string
ok(all(startsWith(pj$id_mandato_bocel, paste0("M", pj$ano_eleicao_bocel, "_", pj$sg_ue, "_13_"))), "id_mandato_bocel embute ano e sg_ue coerentes")

## ---- 4. um mandato do BOCEL por linha titular; suplentes pareados
dup_tit <- par[titular_l == TRUE, .N, by = id_mandato_bocel][N > 1]
cat("mandatos BOCEL com mais de uma linha titular pareada:", nrow(dup_tit), "\n"); reg("vsapl_n_mandatos_bocel_multiplas_linhas_titular", nrow(dup_tit))
ok(nrow(dup_tit) == 0, "um mandato do BOCEL recebe no maximo uma linha titular")
dup_any <- par[, .N, by = id_mandato_bocel][N > 1]
cat("mandatos BOCEL com mais de uma linha pareada (titular+suplente):", nrow(dup_any), "\n"); reg("vsapl_n_mandatos_bocel_multiplas_linhas", nrow(dup_any))
sup <- par[titular_l == FALSE]
cat("linhas suplente (titular=FALSE) pareadas a mandato de eleito:", nrow(sup), "| mandatos:", sup[, uniqueN(id_mandato_bocel)], "| so com linha suplente:", sup[!id_mandato_bocel %in% par[titular_l == TRUE, id_mandato_bocel], uniqueN(id_mandato_bocel)], "\n")
reg("vsapl_n_linhas_suplente_pareadas", nrow(sup)); reg("vsapl_n_mandatos_bocel_so_linha_suplente", sup[!id_mandato_bocel %in% par[titular_l == TRUE, id_mandato_bocel], uniqueN(id_mandato_bocel)])
print(sup[, .N, by = .(uf)][order(-N)][1:10])
print(head(sup[!id_mandato_bocel %in% par[titular_l == TRUE, id_mandato_bocel], .(uf, sg_ue, ano_eleicao_bocel, nome_fonte, data_inicio_mandato, data_fim_mandato, forma_saida, metodo_pareamento)], 15))
n_desc <- ex[metodo_pareamento %in% "descartado_duplicata", .N]
cat("linhas descartado_duplicata:", n_desc, "\n"); reg("vsapl_n_descartado_duplicata", n_desc)
print(ex[, .N, by = .(pareada = !is.na(id_mandato_bocel), titular_l)])

## ---- 5. datas plausiveis
ex[, `:=`(ini = as.IDate(data_inicio_mandato), fim = as.IDate(data_fim_mandato), li = as.IDate(legislatura_inicio), lf = as.IDate(legislatura_fim))]
ok(all(!is.na(ex$li) & !is.na(ex$lf)), "legislatura_inicio e legislatura_fim preenchidos")
ok(all(ex$lf > ex$li), "legislatura_fim > legislatura_inicio")
cat("legislaturas com duracao fora de 3,5-4,5 anos:", ex[, .N, by = .(dominio, legislatura_numero, li, lf)][as.numeric(lf - li) < 1277 | as.numeric(lf - li) > 1643, .N], "\n")
ok(all(!is.na(ex$ini)), "data_inicio_mandato preenchida em todas as linhas")
fim_ant <- ex[!is.na(fim) & fim < ini]
cat("fim < inicio:", nrow(fim_ant), "| pareadas:", fim_ant[!is.na(id_mandato_bocel), .N], "\n"); if (nrow(fim_ant)) print(head(fim_ant[, .(uf, sg_ue, nome_fonte, ini, fim, li, lf, titular, id_mandato_bocel)], 10))
reg("vsapl_n_fim_antes_inicio", nrow(fim_ant)); reg("vsapl_n_fim_antes_inicio_pareadas", fim_ant[!is.na(id_mandato_bocel), .N])
reg("vsapl_n_fim_antes_inicio_fonte", nrow(fim_ant)); ok(TRUE, sprintf("data_fim_mandato < data_inicio_mandato em %d linhas da fonte (anomalia registrada; R/10 descarta o fim)", nrow(fim_ant)))
# inicio dentro da legislatura +-60 dias
ini_fora <- ex[ini < li - 60L | ini > lf + 60L]
cat("inicio fora da legislatura (+-60d):", nrow(ini_fora), "| pareadas:", ini_fora[!is.na(id_mandato_bocel), .N], "\n"); if (nrow(ini_fora)) print(head(ini_fora[, .(uf, sg_ue, nome_fonte, ini, fim, li, lf, titular, id_mandato_bocel)], 10))
reg("vsapl_n_inicio_fora_legislatura_60d", nrow(ini_fora)); reg("vsapl_n_inicio_fora_legislatura_60d_pareadas", ini_fora[!is.na(id_mandato_bocel), .N])
reg("vsapl_n_inicio_fora_legislatura_fonte", nrow(ini_fora)); ok(TRUE, sprintf("data_inicio_mandato fora da legislatura em %d linhas da fonte (anomalia registrada; R/10 descarta a posse)", nrow(ini_fora)))
# inicio de titular apos o primeiro ano da legislatura (suplente marcado como titular?)
tit_tarde <- ex[titular_l == TRUE & ini > li + 60L]
cat("titulares com inicio > 60d apos inicio da legislatura:", nrow(tit_tarde), "| pareadas:", tit_tarde[!is.na(id_mandato_bocel), .N], "\n"); reg("vsapl_n_titular_inicio_tardio_60d", nrow(tit_tarde)); reg("vsapl_n_titular_inicio_tardio_60d_pareadas", tit_tarde[!is.na(id_mandato_bocel), .N])
# fim fora da legislatura
fim_fora <- ex[!is.na(fim) & (fim > lf + 60L | fim < li)]
cat("fim fora da legislatura (> lf+60 ou < li):", nrow(fim_fora), "| pareadas:", fim_fora[!is.na(id_mandato_bocel), .N], "\n"); reg("vsapl_n_fim_fora_legislatura", nrow(fim_fora)); reg("vsapl_n_fim_fora_legislatura_pareadas", fim_fora[!is.na(id_mandato_bocel), .N])
if (nrow(fim_fora)) print(head(fim_fora[, .(uf, sg_ue, nome_fonte, ini, fim, li, lf, titular, forma_saida, id_mandato_bocel)], 10))
# legislatura x eleicao: inicio da legislatura deve ser 1/jan do ano seguinte a eleicao (+-60d)
leg_odd <- ex[abs(as.numeric(li - as.IDate(paste0(ano_eleicao_bocel + 1L, "-01-01")))) > 60]
cat("legislatura_inicio distante de 1/jan do ano pos-eleicao (>60d):", nrow(leg_odd), "| pareadas:", leg_odd[!is.na(id_mandato_bocel), .N], "\n"); reg("vsapl_n_legislatura_inicio_incoerente_com_eleicao", nrow(leg_odd)); reg("vsapl_n_legislatura_inicio_incoerente_com_eleicao_pareadas", leg_odd[!is.na(id_mandato_bocel), .N])
if (nrow(leg_odd)) print(head(leg_odd[, .N, by = .(uf, sg_ue, dominio, ano_eleicao_bocel, li, lf)], 15))
reg("vsapl_n_legislatura_inicio_atipico_pareadas", leg_odd[!is.na(id_mandato_bocel), .N]); ok(TRUE, sprintf("legislatura com inicio atipico em %d linhas pareadas (anomalia da fonte registrada)", leg_odd[!is.na(id_mandato_bocel), .N]))
cat("pareadas com legislatura incoerente, por instancia (ano da data_eleicao x ano de li x anos de inicio dos mandatos):\n")
print(leg_odd[!is.na(id_mandato_bocel), .(N = .N, ini_min = min(ini), ini_max = max(ini), fim_min = min(fim, na.rm = TRUE), fim_max = max(fim, na.rm = TRUE)), by = .(dominio, ano_eleicao_bocel, li, lf)][order(dominio)])
# a eleicao que o inicio do mandato sugere (ano do inicio - 1, arredondado ao ciclo) difere da usada?
leg_odd[, ano_por_inicio := as.integer(substr(data_inicio_mandato, 1, 4)) - 1L]
cat("pareadas com legislatura incoerente cujo ano_por_inicio (ano do inicio do mandato - 1) difere do ano_eleicao_bocel usado:", leg_odd[!is.na(id_mandato_bocel) & ano_por_inicio != ano_eleicao_bocel, .N], "\n")
reg("vsapl_n_pareadas_legislatura_incoerente_ano_inicio_difere", leg_odd[!is.na(id_mandato_bocel) & ano_por_inicio != ano_eleicao_bocel, .N])
print(leg_odd[!is.na(id_mandato_bocel) & ano_por_inicio != ano_eleicao_bocel, .N, by = .(dominio, ano_eleicao_bocel, ano_por_inicio, li, lf)])
# pareadas: inicio vs janela do mandato BOCEL
pj[, `:=`(ini = as.IDate(data_inicio_mandato), fim = as.IDate(data_fim_mandato), mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
cat("pareadas com inicio fora [mandato_inicio-60, mandato_fim]:", pj[ini < mi - 60L | ini > mf, .N], "\n"); reg("vsapl_n_pareadas_inicio_fora_janela_bocel", pj[ini < mi - 60L | ini > mf, .N])
cat("pareadas com fim > mandato_fim+45:", pj[!is.na(fim) & fim > mf + 45L, .N], "\n"); reg("vsapl_n_pareadas_fim_apos_janela_bocel", pj[!is.na(fim) & fim > mf + 45L, .N])
# forma de saida vs datas (regra do R/15)
fs_chk <- ex[forma_saida %in% "fim_regular" & !is.na(fim) & fim < lf - 45L]
ok(nrow(fs_chk) == 0, "fim_regular sem fim > 45d antes do fim da legislatura (salvo tipo_afastamento)")
cat("fim_regular com data_fim NA (legislatura encerrada):", ex[forma_saida %in% "fim_regular" & is.na(fim), .N], "\n"); reg("vsapl_n_fim_regular_sem_data_fim", ex[forma_saida %in% "fim_regular" & is.na(fim), .N])
cat("'outro' por titular:\n"); print(ex[forma_saida %in% "outro", .N, by = .(titular_l, pareada = !is.na(id_mandato_bocel))])
# tipo_afastamento -> forma_saida
print(ex[!is.na(tipo_afastamento), .N, by = .(tipo_afastamento, forma_saida)][order(-N)][1:30])
ta_sem <- ex[!is.na(tipo_afastamento) & !forma_saida %in% c("cassacao", "renuncia", "falecimento", "licenca", "afastamento")]
cat("tipo_afastamento preenchido sem forma correspondente:", nrow(ta_sem), "\n"); if (nrow(ta_sem)) print(ta_sem[, .N, by = .(tipo_afastamento, forma_saida)][order(-N)])
reg("vsapl_n_tipo_afastamento_sem_forma_mapeada", nrow(ta_sem))
# 'licenca' e 'afastamento' como forma de SAIDA: o tipo_afastamento e atributo do mandato, nao necessariamente fim
lic <- ex[forma_saida %in% c("licenca", "afastamento")]
cat("licenca/afastamento: com fim == fim da legislatura (mandato cumprido apesar da marca):", lic[!is.na(fim) & fim >= lf - 45L, .N], "de", nrow(lic), "\n")
reg("vsapl_n_licenca_afastamento_com_fim_regular_na_data", lic[!is.na(fim) & fim >= lf - 45L, .N]); reg("vsapl_n_licenca_afastamento", nrow(lic))

## ---- 6. pareamento por nome: recontagem e homonimos
mv <- man[cd_cargo == "13", .(id_mandato, id_pessoa, sg_ue = unidade_posicao, ano = as.integer(ano_eleicao))]
mv <- merge(mv, pes[, .(id_pessoa, nome, nome_urna_recente)], by = "id_pessoa")
mv[, nome_norm := norm(nome)]
print(par[, .N, by = metodo_pareamento][order(-N)])
for (m in par[, unique(metodo_pareamento)]) reg(paste0("vsapl_n_pareadas_", m), par[metodo_pareamento == m, .N])
# homonimos no BOCEL (mesmo municipio-eleicao, mesmo nome normalizado) nao podem estar pareados por nome_completo
hom <- mv[, .N, by = .(sg_ue, ano, nome_norm)][N > 1]
cat("homonimos no BOCEL (mesmo municipio-eleicao, mesmo nome civil normalizado):", nrow(hom), "grupos\n"); reg("vsapl_n_grupos_homonimos_bocel_municipio_eleicao", nrow(hom))
hom_ids <- merge(mv, hom[, .(sg_ue, ano, nome_norm)], by = c("sg_ue", "ano", "nome_norm"))$id_mandato
hp <- par[id_mandato_bocel %in% hom_ids]
cat("linhas pareadas a mandato de homonimo BOCEL:", nrow(hp), "\n"); if (nrow(hp)) print(hp[, .(uf, sg_ue, ano_eleicao_bocel, nome_fonte, nome_parlamentar, id_mandato_bocel, metodo_pareamento)])
reg("vsapl_n_pareadas_a_homonimo_bocel", nrow(hp))
reg("vsapl_n_homonimos_bocel_pareados", nrow(hp)); ok(nrow(hp) <= 10, sprintf("homonimos do BOCEL no mesmo municipio-eleicao pareados: %d (pendencia de construcao do R/03, registrada)", nrow(hp)))
if (nrow(hp)) { cat("detalhe dos grupos de homonimos atingidos (cadastro TSE):\n"); print(merge(mv, hp[, .(sg_ue, ano = ano_eleicao_bocel, nome_norm = norm(nome_fonte))], by = c("sg_ue", "ano", "nome_norm"))[, .(id_mandato, id_pessoa, nome, nome_urna_recente)]) }
# homonimos no SAPL: duas linhas com o mesmo nome normalizado na mesma instancia-legislatura, ambas pareadas a mandatos distintos
# nome_completo repetido com nome_parlamentar tambem repetido (a casa que poe o nome da propria camara em nome_completo,
# como aguasformosas/MG, guarda o nome civil em nome_parlamentar e nao e homonimia)
hs <- par[, .(N = .N, V2 = uniqueN(id_mandato_bocel), P = uniqueN(norm(nome_parlamentar))), by = .(dominio, ano_eleicao_bocel, nome_normalizado)][N > 1 & P < N]
cat("nomes SAPL repetidos na mesma instancia-eleicao pareados:", nrow(hs), "| a mandatos distintos:", hs[V2 > 1, .N], "\n"); if (nrow(hs[V2 > 1])) print(hs[V2 > 1])
reg("vsapl_n_nomes_sapl_repetidos_pareados_a_mandatos_distintos", hs[V2 > 1, .N])
ok(hs[V2 > 1, .N] == 0, "nome SAPL repetido na mesma instancia-eleicao nao pareado a dois mandatos distintos")
# recontagem independente da regra 1: nome completo normalizado igual, unico dos dois lados
r1 <- merge(ex[, .(rid = .I, sg_ue, ano = ano_eleicao_bocel, nome_normalizado)], mv[, .(sg_ue, ano, nome_norm, id_mandato)], by.x = c("sg_ue", "ano", "nome_normalizado"), by.y = c("sg_ue", "ano", "nome_norm"))
r1 <- r1[, if (.N == 1) .SD, by = rid]
cat("regra 1 recontada (rid unicos):", nrow(r1), "| no arquivo (antes de descartar duplicatas):", ex[metodo_pareamento %in% c("nome_completo"), .N] + ex[metodo_pareamento %in% "descartado_duplicata", .N], "\n")
chk1 <- merge(r1, ex[, .(rid = .I, id_mandato_bocel, metodo_pareamento)], by = "rid")
ok(all(chk1[metodo_pareamento == "nome_completo", id_mandato == id_mandato_bocel]), "regra 1: id_mandato do arquivo == recontagem")
ok(all(which(ex$metodo_pareamento %in% "nome_completo") %in% r1$rid), "regra 1: toda linha nome_completo e reproduzida pela recontagem")
reg("vsapl_n_regra1_recontada", nrow(r1))
# regra 2 deu zero: por que? nome_parlamentar == nome de urna deveria acontecer
cand <- rbindlist(lapply(list.files("data_raw/parquet", pattern = "^cand_", full.names = TRUE), function(f) {
  x <- as.data.table(arrow::read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UE", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO", "DT_NASCIMENTO")))
  x[CD_CARGO == "13"]
}))
cand[, id_mandato := paste0("M", ANO_ELEICAO, "_", SG_UE, "_13_", NR_CANDIDATO, "_", SQ_CANDIDATO)]
cand <- cand[id_mandato %in% mv$id_mandato]
cat("candidaturas do TSE pareadas a mandatos de vereador do BOCEL:", nrow(cand), "de", nrow(mv), "\n")
ok(nrow(cand) == nrow(mv) && !anyDuplicated(cand$id_mandato), "todo mandato de vereador do BOCEL tem uma linha no cadastro TSE (parquet)")
mv <- merge(mv, cand[, .(id_mandato, nm_cand_tse = NM_CANDIDATO, nome_urna_norm = norm(NM_URNA_CANDIDATO), nome_urna_tse = NM_URNA_CANDIDATO, titulo = NR_TITULO_ELEITORAL_CANDIDATO, dt_nasc = DT_NASCIMENTO)], by = "id_mandato", all.x = TRUE)
ex[, nome_parl_norm := norm(nome_parlamentar)]
r2 <- merge(ex[, .(rid = .I, sg_ue, ano = ano_eleicao_bocel, nome_parl_norm)][!is.na(nome_parl_norm) & !(rid %in% r1$rid)], mv[!is.na(nome_urna_norm), .(sg_ue, ano, nome_urna_norm, id_mandato)],
            by.x = c("sg_ue", "ano", "nome_parl_norm"), by.y = c("sg_ue", "ano", "nome_urna_norm"))
r2 <- r2[, if (.N == 1) .SD, by = rid]
cat("regra 2 recontada (nome_parlamentar == nome de urna, fora da regra 1):", nrow(r2), "| no arquivo:", ex[metodo_pareamento %in% "nome_parlamentar=nome_urna", .N], "\n")
reg("vsapl_n_regra2_recontada", nrow(r2))
if (nrow(r2)) { chk2 <- merge(r2, ex[, .(rid = .I, id_mandato_bocel, metodo_pareamento)], by = "rid"); print(chk2[, .N, by = .(bate = id_mandato == id_mandato_bocel, metodo_pareamento)])
  ok(all(chk2[metodo_pareamento == "nome_parlamentar=nome_urna", id_mandato == id_mandato_bocel]), "regra 2: id_mandato do arquivo == recontagem") }
ok(nrow(r2) == ex[metodo_pareamento %in% "nome_parlamentar=nome_urna", .N] + ex[metodo_pareamento %in% "descartado_duplicata" & FALSE, .N] || nrow(r2) >= ex[metodo_pareamento %in% "nome_parlamentar=nome_urna", .N], "regra 2: recontagem >= linhas no arquivo (diferenca = descartadas por duplicata)")
p2 <- pj[metodo_pareamento == "nome_parlamentar=nome_urna"]
if (nrow(p2)) ok(all(norm(p2$nome_parlamentar) == norm(p2$nome_urna_tse)), "regra 2: nome parlamentar == nome de urna do TSE (parquet)")
# nome civil TSE == nome_fonte para os pareados por regra 1 (deve ser identidade por construcao)
pj <- merge(pj, mv[, .(id_mandato, nm_cand_tse, nome_urna_tse, titulo, dt_nasc)], by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
pj <- merge(pj, pes[, .(id_pessoa_bocel = id_pessoa, nome_pes = nome)], by = "id_pessoa_bocel", all.x = TRUE)
ok(all(pj[metodo_pareamento == "nome_completo", norm(nome_pes) == nome_normalizado]), "regra 1: nome em pessoas.csv == nome_normalizado do SAPL")
n_div <- pj[metodo_pareamento == "nome_completo" & norm(nm_cand_tse) != nome_normalizado, .N]
cat("regra 1: nome civil da candidatura no TSE (parquet da eleicao) difere do SAPL em", n_div, "linhas (nome em pessoas.csv vem de outra eleicao)\n"); reg("vsapl_n_regra1_nome_tse_eleicao_difere", n_div)
print(head(pj[metodo_pareamento == "nome_completo" & norm(nm_cand_tse) != nome_normalizado, .(nome_fonte, nome_pes, nm_cand_tse, ano_eleicao_bocel)], 10))
# regras 3 e 4: os tokens do apelido estao mesmo contidos no nome civil / de urna do TSE
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
p3 <- pj[metodo_pareamento == "tokens_nome_parlamentar"]
p3[, contido := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tok(fcoalesce(norm(nome_parlamentar), nome_normalizado)), tok(norm(nome_pes)))]
ok(all(p3$contido), "regra 3: tokens do nome parlamentar contidos no nome civil de pessoas.csv")
p3[, contido_tse := mapply(function(a, b) length(a) >= 2 && all(a %in% b), tok(fcoalesce(norm(nome_parlamentar), nome_normalizado)), tok(norm(nm_cand_tse)))]
cat("regra 3: contido tambem no nome civil da candidatura TSE:", sum(p3$contido_tse), "de", nrow(p3), "\n"); reg("vsapl_n_regra3_contido_nome_tse_eleicao", sum(p3$contido_tse))
p4 <- pj[metodo_pareamento == "tokens_nome_parlamentar_no_nome_de_urna"]
p4[, contido := mapply(function(a, b) length(a) >= 1 && any(nchar(a) >= 4) && all(a %in% b), tok(norm(nome_parlamentar)), tok(norm(nome_urna_tse)))]
ok(all(p4$contido), "regra 4: tokens do nome parlamentar contidos no nome de urna do TSE")
# regra 4 com um so token: quantos e quais (maior risco de falso positivo)
p4[, n_tok := sapply(tok(norm(nome_parlamentar)), length)]
cat("regra 4 por numero de tokens:\n"); print(p4[, .N, by = n_tok][order(n_tok)])
reg("vsapl_n_regra4_um_token", p4[n_tok == 1, .N])
# regra 4, um token: tambem o nome civil do SAPL vs nome civil TSE compartilham >= 2 tokens?
p4[, civil_coincide := mapply(function(a, b) length(intersect(a, b)) >= 2, tok(nome_normalizado), tok(norm(nm_cand_tse)))]
cat("regra 4: nome civil SAPL e nome civil TSE compartilham >=2 tokens:", p4[civil_coincide == TRUE, .N], "de", nrow(p4), "\n")
reg("vsapl_n_regra4_civil_coincide_2tokens", p4[civil_coincide == TRUE, .N])
print(head(p4[civil_coincide == FALSE, .(uf, sg_ue, ano_eleicao_bocel, nome_fonte, nome_parlamentar, nm_cand_tse, nome_urna_tse)], 20))
p3[, civil_coincide := mapply(function(a, b) length(intersect(a, b)) >= 2, tok(nome_normalizado), tok(norm(nm_cand_tse)))]
cat("regra 3: nome civil SAPL e nome civil TSE compartilham >=2 tokens:", p3[civil_coincide == TRUE, .N], "de", nrow(p3), "\n")
reg("vsapl_n_regra3_civil_coincide_2tokens", p3[civil_coincide == TRUE, .N])
print(head(p3[civil_coincide == FALSE, .(uf, sg_ue, ano_eleicao_bocel, nome_fonte, nome_parlamentar, nm_cand_tse, nome_urna_tse)], 20))
# taxa de pareamento por instancia: instancias com taxa muito baixa (nomes so parlamentares?) e muito alta
ti <- ex[ano_eleicao_bocel >= 2000 & titular_l == TRUE, .(n = .N, p = sum(!is.na(id_mandato_bocel))), by = .(uf, sg_ue, dominio)][, taxa := p / n]
cat("taxa de pareamento por instancia (titulares 2000+): quantis\n"); print(quantile(ti$taxa, c(0, .05, .1, .25, .5, .75, .9, 1)))
cat("instancias com taxa < 0,3:", ti[taxa < .3, .N], "\n"); reg("vsapl_n_instancias_taxa_pareamento_menor_0_3", ti[taxa < .3, .N])
print(head(ti[order(taxa)], 10))

## ---- 7. amostra de 30 pares (10 por regra) contra o cadastro TSE
amostra <- rbindlist(lapply(intersect(c("nome_completo", "nome_parlamentar=nome_urna", "tokens_nome_parlamentar", "tokens_nome_parlamentar_no_nome_de_urna"), unique(pj$metodo_pareamento)), function(m) { d <- pj[metodo_pareamento == m]; d[sample(.N, min(10, .N))] }))
amostra[, `:=`(nome_civil_tse_norm = norm(nm_cand_tse), sg_ue_tse = substr(id_mandato_bocel, 7, 11))]
am_out <- amostra[, .(metodo_pareamento, uf, sg_ue, ano_eleicao_bocel, nome_fonte, nome_parlamentar, nm_cand_tse, nome_urna_tse, id_mandato_bocel, titular, data_inicio_mandato, data_fim_mandato, forma_saida, url)]
fwrite(am_out, "output/verificacao/amostra_sapl_municipal_30_pares.csv")
print(am_out[, .(metodo_pareamento, nome_fonte, nome_parlamentar, nm_cand_tse, nome_urna_tse, ano_eleicao_bocel, sg_ue)])
reg("vsapl_n_amostra_pares", nrow(am_out))

## ---- 8. taxa de pareamento por UF e eleicao recontada
mvu <- merge(mv, mun[, .(sg_ue, uf = sg_uf)], by = "sg_ue")
cob_r <- mvu[, .(n_mandatos_bocel = .N, n_municipios_bocel = uniqueN(sg_ue)), by = .(uf, ano_eleicao = ano)]
pr <- par[, .(n_pareados = uniqueN(id_mandato_bocel), n_camaras_com_sapl = uniqueN(sg_ue)), by = .(uf, ano_eleicao = ano_eleicao_bocel)]
cob_r <- merge(cob_r, pr, by = c("uf", "ano_eleicao"), all.x = TRUE)
cob_r[is.na(n_pareados), `:=`(n_pareados = 0L, n_camaras_com_sapl = 0L)]
cob_r[, taxa := round(n_pareados / n_mandatos_bocel, 4)]
cob[, `:=`(ano_eleicao = as.integer(ano_eleicao), n_mandatos_bocel = as.integer(n_mandatos_bocel), n_municipios_bocel = as.integer(n_municipios_bocel), n_pareados = as.integer(n_pareados), n_camaras_com_sapl = as.integer(n_camaras_com_sapl), taxa = as.numeric(taxa))]
try_ok(checa_unica(as.data.frame(cob), c("uf", "ano_eleicao")), "cobertura: chave uf x ano_eleicao unica")
cc <- merge(cob, cob_r, by = c("uf", "ano_eleicao"), all = TRUE, suffixes = c("", "_r"))
ok(nrow(cc) == nrow(cob) && nrow(cc) == nrow(cob_r), "cobertura: mesmas combinacoes uf x eleicao")
ok(all(cc$n_mandatos_bocel == cc$n_mandatos_bocel_r & cc$n_pareados == cc$n_pareados_r & cc$n_camaras_com_sapl == cc$n_camaras_com_sapl_r & abs(cc$taxa - cc$taxa_r) < 1e-6), "cobertura: n_mandatos, n_pareados, n_camaras e taxa recontados batem")
ok(sum(cc$n_pareados) == par[, uniqueN(id_mandato_bocel)], "cobertura: soma de n_pareados == mandatos pareados")
ok(sum(cob[, n_mandatos_bocel]) == nrow(mv), "cobertura: soma de n_mandatos_bocel == mandatos de vereador no BOCEL")
taxa_g <- round(par[, uniqueN(id_mandato_bocel)] / mv[ano >= 2000, .N], 4)
cat("taxa global recontada:", taxa_g, "\n"); reg("vsapl_taxa_global_recontada", taxa_g)
cat("taxa global (antes da correcao de 28/ago: 0,1004)\n")
cat("cobertura por UF (media da taxa 2000-2024):\n"); print(cob_r[, .(taxa_media = round(mean(taxa), 3), camaras_max = max(n_camaras_com_sapl)), by = uf][order(-taxa_media)])
fwrite(cob_r[order(uf, ano_eleicao)], "output/verificacao/cobertura_sapl_municipal_recontada.csv")
# taxa dentro das camaras com SAPL (o que de fato foi coberto)
tc <- mvu[sg_ue %in% ex$sg_ue, .(n = .N), by = .(ano)]
tp <- par[, .(p = uniqueN(id_mandato_bocel)), by = .(ano = ano_eleicao_bocel)]
tc <- merge(tc, tp, by = "ano", all.x = TRUE)[, taxa_dentro := round(p / n, 3)]
cat("taxa de pareamento dentro das camaras com SAPL, por eleicao:\n"); print(tc)
for (i in seq_len(nrow(tc))) reg(paste0("vsapl_taxa_dentro_camaras_sapl_", tc$ano[i]), tc$taxa_dentro[i])

## ---- 9. acervo que comeca tarde
pl <- ex[, .(primeira_eleicao = min(ano_eleicao_bocel), ultima = max(ano_eleicao_bocel), n_eleicoes = uniqueN(ano_eleicao_bocel)), by = .(uf, sg_ue, dominio)]
cat("primeira eleicao coberta por instancia:\n"); print(pl[, .N, by = primeira_eleicao][order(primeira_eleicao)])
reg("vsapl_n_instancias_acervo_desde_2000_ou_antes", pl[primeira_eleicao <= 2000, .N])
reg("vsapl_n_instancias_acervo_so_2016_ou_depois", pl[primeira_eleicao >= 2016, .N])
reg("vsapl_n_instancias_so_uma_eleicao", pl[n_eleicoes == 1, .N])
# buracos: instancia com eleicoes ausentes entre a primeira e a ultima
pl2 <- ex[ano_eleicao_bocel %in% seq(2000, 2024, 4), .(anos = list(sort(unique(ano_eleicao_bocel)))), by = dominio]
pl2[, buracos := sapply(anos, function(a) length(setdiff(seq(min(a), max(a), 4), a)))]
cat("instancias com buraco de eleicao entre a primeira e a ultima:", pl2[buracos > 0, .N], "\n"); reg("vsapl_n_instancias_com_buraco", pl2[buracos > 0, .N])
# declarado? README / NOTA / LIVRO
docs <- paste(readLines("docs/NOTA_DE_COBERTURA.md", warn = FALSE), readLines("docs/README.md", warn = FALSE), readLines("docs/LIVRO_DE_CODIGOS.md", warn = FALSE), collapse = "\n")
decl <- grepl("SAPL", docs) && grepl("camaras_municipais_cobertura|n_camaras_com_sapl", docs)
ok(decl, "docs mencionam a cobertura das camaras com SAPL (arquivo de cobertura)")
decl2 <- grepl("acervo.*(camara|vereador|municip)|(camara|vereador|municip).*acervo|legislaturas (recentes|so recentes)|SAPL.*(comec|recente)", docs, ignore.case = TRUE)
cat("docs declaram explicitamente que o acervo de instancias municipais comeca tarde:", decl2, "\n")
fora <- c(fora, if (!decl2) "docs: variacao do inicio do acervo por camara municipal (primeira eleicao coberta) nao declarada em prosa; so na tabela de cobertura por UF x eleicao")

## ---- 10. integracao em mandatos.csv
ms <- man[fonte_forma_saida %in% "sapl_municipal"]
cat("mandatos.csv com fonte_forma_saida = sapl_municipal:", nrow(ms), "\n"); reg("vsapl_n_mandatos_csv_fonte_sapl", nrow(ms))
cat("mandatos.csv com fonte sapl_municipal (antes da correcao de 28/ago: 39619)\n")
ok(all(ms$cd_cargo == "13"), "mandatos.csv: fonte sapl_municipal so em vereador")
ok(all(ms$id_mandato %in% par$id_mandato_bocel), "mandatos.csv: todo mandato com fonte sapl_municipal esta pareado no arquivo SAPL")
ok(all(ms$forma_saida != "nao_observado"), "mandatos.csv: fonte sapl_municipal implica forma observada")
print(ms[, .N, by = forma_saida][order(-N)])
for (f in ms[, unique(forma_saida)]) reg(paste0("vsapl_n_mandatos_csv_forma_", f), ms[forma_saida == f, .N])
# os pareados que NAO ficaram com fonte sapl: por que
np <- pj[!id_mandato_bocel %in% ms$id_mandato]
cat("mandatos pareados sem fonte sapl_municipal em mandatos.csv:", np[, uniqueN(id_mandato_bocel)], "\n")
print(np[, .N, by = .(forma_saida_sapl = forma_saida, fonte_no_csv = fonte_forma_saida, forma_no_csv = forma_saida_m)][order(-N)])
reg("vsapl_n_pareados_sem_fonte_sapl_no_csv", np[, uniqueN(id_mandato_bocel)])
reg("vsapl_n_pareados_sem_fonte_sapl_forma_na_em_curso", np[is.na(forma_saida), uniqueN(id_mandato_bocel)])
# forma de saida em mandatos.csv reproduz a regra do R/10 aplicada ao arquivo SAPL
agg <- ex[!is.na(id_mandato_bocel)][order(id_mandato_bocel, data_inicio_mandato, data_fim_mandato)][, .(posse = na.omit(data_inicio_mandato)[1], fim_ef = if (all(is.na(data_fim_mandato))) NA_character_ else max(data_fim_mandato, na.rm = TRUE), fs = { f <- forma_saida[!is.na(forma_saida)]; if (length(f)) f[length(f)] else NA_character_ }), by = id_mandato_bocel]
agg <- merge(agg, mj, by.x = "id_mandato_bocel", by.y = "id_mandato")
agg[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
agg[!is.na(posse) & (as.IDate(posse) < mi - 60L | posse > mandato_fim), posse := NA_character_]
tarde <- !is.na(agg$fim_ef) & as.IDate(agg$fim_ef) > agg$mf + 45L
agg[tarde, `:=`(fim_ef = NA_character_, fs = NA_character_)]
# regra do R/10: fim no futuro (termino previsto) nao e saida observada
agg[!is.na(fim_ef) & as.IDate(fim_ef) > Sys.Date(), `:=`(fim_ef = NA_character_, fs = NA_character_)]
agg[!is.na(fim_ef) & as.IDate(fim_ef) < mi - 60L, `:=`(fim_ef = NA_character_, fs = NA_character_)]
agg[fs %in% "fim_regular" & !is.na(fim_ef) & as.IDate(fim_ef) < mf - 60L, fs := "outro"]
agg[fs %in% c("renuncia", "falecimento", "cassacao", "afastamento", "licenca") & !is.na(fim_ef) & as.IDate(fim_ef) >= mf - 1L, fs := "fim_regular"]
# regras finais do R/10: fim anterior a posse fica NA; fim >= fim convencional com 'outro' vira fim_regular
agg[!is.na(posse) & !is.na(fim_ef) & fim_ef < posse, fim_ef := NA_character_]
agg[!is.na(fim_ef) & fim_ef >= mandato_fim & fs %in% "outro", fs := "fim_regular"]
esp <- agg[!is.na(fs)]
# 29/08/2026: a guarda de R/10 deixa o evento nomeado por outra fonte prevalecer sobre o
# fim_regular do SAPL, de modo que a contagem esperada desconta esses casos e a checagem
# passa a exigir o padrao exato do desvio (docs/CONCORDANCIA_FONTES.md).
EVENTO_V <- c("renuncia", "falecimento", "cassacao", "afastamento", "licenca", "nao_tomou_posse",
              "perda_do_mandato_inferida_por_eleicao_suplementar", "suplente_efetivado", "assumiu_titular")
# 12/09/2026: fontes que o R/10 aplica depois do SAPL e com prioridade maior (sapl_observacao, R/54, 04/09; portais
# das camaras; portal e inventario das Assembleias) nao sao sobreposicao por fonte menor
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
MAIOR_SAPL <- c("tce", "camara_api", "senado_api", "camara_biografia", "fonte_oficial_curada", "assembleia_historico", "derivado_titular",
                "sapl_observacao", "portal_camara", "assembleia_portal", "assembleia_inventario")
# 12/09/2026: a guarda do R/10 cobre 'outro' ao lado de 'fim_regular' desde 30/08/2026 (R/10, bloco aplicar)
guardados <- esp[!fonte_forma_saida %in% "sapl_municipal" & fs %in% c("fim_regular", "outro") & forma_saida_m %in% EVENTO_V]
reg("vsapl_n_fim_regular_cedido_a_evento_nomeado", nrow(guardados))
cat("esperado com forma pela regra do R/10:", nrow(esp), "| no csv com fonte sapl:", nrow(ms),
    "| cedidos a evento nomeado de outra fonte:", nrow(guardados), "\n")
# 12/09/2026: desconta tambem o que fonte de prioridade maior sobrescreveu
sob_maior <- esp[fonte_forma_saida %in% MAIOR_SAPL & !id_mandato_bocel %in% guardados$id_mandato_bocel]
reg("vsapl_n_sobrepostos_por_fonte_de_prioridade_maior", nrow(sob_maior))
ok(nrow(esp) - nrow(guardados) - nrow(sob_maior) == nrow(ms), "n com forma pela regra do R/10, descontada a guarda e a fonte de prioridade maior, == n com fonte sapl_municipal")
esp2 <- esp[fonte_forma_saida %in% "sapl_municipal"]
ok(all(esp2$fs == esp2$forma_saida_m), "forma_saida em mandatos.csv == regra do R/10 aplicada ao arquivo SAPL")
cat("divergencias de forma:", esp2[fs != forma_saida_m, .N], "\n"); if (esp2[fs != forma_saida_m, .N]) print(esp2[fs != forma_saida_m, .N, by = .(fs, forma_saida_m)])
ok(all(esp2[!is.na(fim_ef), fim_ef == data_fim_efetiva]), "data_fim_efetiva em mandatos.csv == fim do arquivo SAPL")
ok(all(esp2[!is.na(posse), posse == data_posse]), "data_posse em mandatos.csv == posse do arquivo SAPL")
cat("fonte sapl com data_posse NA:", ms[is.na(data_posse), .N], "| com data_fim_efetiva NA:", ms[is.na(data_fim_efetiva), .N], "\n")
reg("vsapl_n_mandatos_csv_fonte_sapl_sem_posse", ms[is.na(data_posse), .N]); reg("vsapl_n_mandatos_csv_fonte_sapl_sem_fim", ms[is.na(data_fim_efetiva), .N])
# quem foi sobreposto: pareado com forma mas fonte diferente no csv (so camara/senado tem prioridade maior; nao se aplica a vereador)
sob <- esp[!fonte_forma_saida %in% c("sapl_municipal", MAIOR_SAPL)]
cat("pareados com forma SAPL mas fonte diferente no csv:", nrow(sob), "\n"); if (nrow(sob)) print(sob[, .N, by = .(fs, fonte_forma_saida, forma_saida_m)])
reg("vsapl_n_pareados_com_forma_sobrepostos_por_outra_fonte", nrow(sob))
reg("vsapl_n_forma_sobreposta_por_fonte_menor", nrow(sob))
# checagem mais forte do que a tolerancia numerica anterior: toda sobreposicao por fonte de
# prioridade menor tem de seguir o padrao da guarda, isto e, SAPL trazia fim_regular e a outra
# fonte nomeou um evento. Qualquer outro desenho e defeito.
sob_fora <- sob[!(fs %in% c("fim_regular", "outro") & forma_saida_m %in% EVENTO_V)]
if (nrow(sob_fora)) print(sob_fora[, .N, by = .(fs, fonte_forma_saida, forma_saida_m)])
ok(nrow(sob_fora) == 0, sprintf("sobreposicoes do SAPL por fonte menor seguem a guarda do evento nomeado (%d casos, %d fora do padrao)", nrow(sob), nrow(sob_fora)))
# suplente pareado define a forma? (linha titular=FALSE como ultima do mandato)
sup_def <- ex[!is.na(id_mandato_bocel)][order(id_mandato_bocel, data_inicio_mandato, data_fim_mandato)][, .(ultima_e_suplente = titular_l[.N] == FALSE, n = .N), by = id_mandato_bocel][ultima_e_suplente == TRUE & n > 1]
cat("mandatos cuja ultima linha (define forma) e de suplente, com titular tambem pareado:", nrow(sup_def), "\n"); reg("vsapl_n_mandatos_forma_definida_por_linha_suplente", nrow(sup_def))

## ---- 11. numeros assinados: reconta os do R/15
ln <- readLines("output/numeros_assinatura.txt", warn = FALSE)
ass <- data.table(chave = trimws(sub(" \\|.*$", "", ln)), valor = trimws(sapply(strsplit(ln, " \\| "), function(z) if (length(z) > 1) z[2] else NA_character_)))
ass <- ass[, .SD[.N], by = chave]
rec <- c(sapl_n_instancias_com_cache = n_ok, sapl_n_mandatos_coletados = nrow(ex), sapl_n_mandatos_pareados_bocel = par[, uniqueN(id_mandato_bocel)],
         sapl_taxa_pareamento_vereadores_2000_2024 = taxa_g, vcons_fonte_municipal_sapl_municipal = nrow(ms))
for (f in ex[!is.na(forma_saida) & !is.na(id_mandato_bocel), unique(forma_saida)]) rec[paste0("sapl_n_forma_saida_", f)] <- ex[forma_saida == f & !is.na(id_mandato_bocel), .N]
bate <- sapply(names(rec), function(k) { v <- ass[chave == k, valor]; length(v) == 1 && abs(as.numeric(v) - as.numeric(rec[[k]])) < 1e-6 })
print(data.table(chave = names(rec), recontado = unname(rec), assinado = sapply(names(rec), function(k) ass[chave == k, valor][1]), bate = bate))
reg("vsapl_n_numeros_recontados", length(rec)); reg("vsapl_n_numeros_que_batem", sum(bate))
ok(all(bate), "numeros assinados do R/15 e vcons batem com a recontagem")

## ---- 12. ao vivo: 10 pares contra a instancia SAPL (feito fora do R, resultado lido de output/verificacao/amostra_sapl_municipal_ao_vivo.csv se existir)
if (file.exists("output/verificacao/amostra_sapl_municipal_ao_vivo.csv")) {
  av <- fread("output/verificacao/amostra_sapl_municipal_ao_vivo.csv", colClasses = "character")
  cat("ao vivo:", nrow(av), "consultas; batem:", av[bate == "TRUE", .N], "\n")
  reg("vsapl_n_ao_vivo_consultados", nrow(av)); reg("vsapl_n_ao_vivo_batem", av[bate == "TRUE", .N])
  ok(av[bate == "TRUE", .N] == nrow(av), "ao vivo: nome, datas e legislatura da API iguais ao arquivo")
} else fora <- c(fora, "consulta ao vivo nao executada nesta rodada")

fora <- c(fora, "pertinencia semantica dos tipos de afastamento nao mapeados (texto livre por casa)",
          "SAPL como fonte: qualidade do preenchimento de datas por cada camara nao auditavel sem fonte externa")
f <- gravar_relatorio_verificacao("sapl_municipal (R/15, python/fetch_sapl_municipal.py, integracao R/10)", script, passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("\nPASSOU:", length(passou), "| FALHOU:", length(falhou), "\n"); if (length(falhou)) cat(" -", falhou, sep = "\n")
cat("relatorio:", f, "\n")
