#!/usr/bin/env Rscript
# Verificador adversarial das tabelas de suplencia e ocupacao (R/40, R/41, R/42), escrito em
# 30/08/2026 junto com a decisao de que o suplente entra no banco sem inflar o N de
# cadeiras. O teste que governa este arquivo e o dessa invariante: a cadeira e uma so, o que
# cresce e a contagem de ocupantes.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente
script <- "R/verifica_ocupacoes.R"
pass <- 0L; fail <- 0L; falhas <- character(); passou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) {
    falhas <<- c(falhas, paste0(nome, " — ", conditionMessage(e))); FALSE })
  if (r) { pass <<- pass + 1L; passou <<- c(passou, nome) } else fail <<- fail + 1L
  cat(if (r) "PASS  " else "FALHA ", nome, "\n", sep = "")
}
reg <- function(k, v) registrar_numero(k, v, script = script)

for (f in c("data/ocupacoes.csv","data/lista_suplencia.csv","data/pessoas_suplentes.csv",
            "data/mandatos_lista.csv","data/mandatos.csv","data/pessoas.csv"))
  if (!file.exists(f)) { cat("verifica_ocupacoes: falta ", f, "\n", sep=""); quit(save="no", status=1) }

oc  <- fread("data/ocupacoes.csv", na.strings = c("NA",""))
m   <- fread("data/mandatos.csv", na.strings = c("NA",""))
p   <- fread("data/pessoas.csv", select = "id_pessoa", colClasses = "character")
ps  <- fread("data/pessoas_suplentes.csv", na.strings = c("NA",""))
ls_ <- fread("data/lista_suplencia.csv", na.strings = c("NA",""))
ml  <- fread("data/mandatos_lista.csv", na.strings = c("NA",""))

## ------------------------------------------------------- a invariante da cadeira
ok("uma ocupacao de titular por cadeira, sem sobra nem falta",
   stopifnot(oc[tipo_ocupante == "titular", .N] == nrow(m),
             uniqueN(oc[tipo_ocupante == "titular"]$id_mandato) == nrow(m),
             setequal(oc[tipo_ocupante == "titular"]$id_mandato, m$id_mandato)))
ok("ocupacao nao cria cadeira que nao exista em mandatos.csv",
   stopifnot(all(is.na(oc$id_mandato) | oc$id_mandato %in% m$id_mandato)))
# 12/09/2026: 481.345 -> 484.771 com a chapa dos vices fechada no R/03; constante trocada por essa correcao
# 13/09/2026: 484.772 com a cadeira de Selma Arruda restaurada
ok("mandatos.csv permanece com 484.772 cadeiras", stopifnot(nrow(m) == 484772L))
# o N de cadeiras por lugar e ano nao pode mudar com a entrada dos suplentes
cad <- m[, .(cadeiras = .N), by = .(ano_eleicao, cd_cargo, unidade_posicao)]
cad_oc <- oc[tipo_ocupante == "titular", .(cadeiras = .N), by = .(ano_eleicao, cd_cargo, unidade_posicao)]
ok("N de cadeiras por lugar, cargo e eleicao identico ao do nucleo",
   stopifnot(nrow(merge(cad, cad_oc, by = c("ano_eleicao","cd_cargo","unidade_posicao"))) == nrow(cad),
             merge(cad, cad_oc, by = c("ano_eleicao","cd_cargo","unidade_posicao"))[
               cadeiras.x != cadeiras.y, .N] == 0L))
reg("voc_cadeiras", nrow(m))
reg("voc_ocupacoes", nrow(oc))
reg("voc_ocupantes_nao_titulares", oc[tipo_ocupante != "titular", .N])

## ------------------------------------------------------- vocabulario e esquema
ok("tipo_ocupante em vocabulario fechado",
   # 13/09/2026: ocupantes federais sem mandato no TSE (R/58 e tabela curada com fonte oficial)
   in_set(oc$tipo_ocupante, c("titular","suplente","vice_assumiu","interino","suplente_efetivado","substituto_por_decisao",
                              "convocado_terceiro_colocado","eleito_em_suplementar","empossado_antes_da_retotalizacao",
                              "entrou_por_retotalizacao","eleito_cassado_depois"), permitir_na = FALSE))
# 29/09/2026: a regra geral de ocupacao (R/42, secao 4b) acrescenta lista_vaga_inicial e lista_sucessao_vaga ao
# vocabulario; os sete valores anteriores continuam admitidos, nenhum saiu
VINCULOS <- c("eleicao","chapa_senado","lista_vaga_unica","lista_data_estrita",
              "cadeira_unica_da_unidade","casa_legislatura","fonte_da_casa",
              "lista_vaga_inicial","lista_sucessao_vaga")
ok("vinculo_cadeira em vocabulario fechado",
   in_set(oc$vinculo_cadeira, VINCULOS, permitir_na = FALSE))
ok("forma_saida em vocabulario fechado",
   in_set(oc$forma_saida, c("fim_regular","renuncia","falecimento","cassacao","afastamento",
                            "licenca","nao_tomou_posse","suplente_efetivado","assumiu_titular",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
                            "outro","nao_observado","substituicao_inferida_munic","aposentadoria","impeachment","retotalizacao",
                            "perda_do_mandato_inferida_por_eleicao_suplementar"), permitir_na = TRUE))
ok("id_ocupacao unico", checa_unica(as.data.frame(oc), "id_ocupacao"))
iso <- function(v) is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", as.character(v))
ok("datas de ocupacao em ISO", stopifnot(all(iso(oc$data_inicio)), all(iso(oc$data_fim))))
# a fonte as vezes publica fim antes do inicio; a linha nao e corrigida em silencio,
# fica marcada, e o que o assert exige e que toda ela esteja marcada
ok("data invertida vinda da fonte esta marcada, nunca corrigida em silencio",
   stopifnot(oc[!is.na(data_inicio) & !is.na(data_fim) & data_inicio > data_fim,
                all(datas_inconsistentes)],
             oc[datas_inconsistentes == TRUE, .N] < 100L))
reg("voc_datas_invertidas_marcadas", oc[datas_inconsistentes == TRUE, .N])
ok("ordem de ocupacao comeca no titular",
   stopifnot(oc[tipo_ocupante == "titular", all(ordem_ocupacao == 1L)],
             oc[tipo_ocupante != "titular" & !is.na(ordem_ocupacao), all(ordem_ocupacao >= 2L)]))

## ------------------------------------------------------- identidade
ok("id do nucleo intacto: toda pessoa de mandatos.csv esta em pessoas.csv",
   stopifnot(all(m$id_pessoa %in% p$id_pessoa)))
ok("cadastro de suplentes nao colide com o cadastro do nucleo",
   stopifnot(length(intersect(ps$id_pessoa, p$id_pessoa)) == 0L))
ok("id_pessoa da ocupacao existe em um dos dois cadastros",
   stopifnot(all(is.na(oc$id_pessoa) | oc$id_pessoa %in% c(p$id_pessoa, ps$id_pessoa))))
ok("id de suplente segue o padrao do banco",
   stopifnot(all(grepl("^BOCEL\\d{7}$", ps$id_pessoa))))
reg("voc_pessoas_nucleo", nrow(p))
reg("voc_pessoas_suplentes", nrow(ps))

## ------------------------------------------------------- a fila de suplencia
ok("ordem de suplencia comeca em 1 e nao tem buraco",
   stopifnot(ls_[, .(ok = all(sort(ordem_suplencia) == seq_len(.N))), by = id_lista][ok == FALSE, .N] == 0L))
val <- ls_[!is.na(ordem_suplencia_tse)]
acerto <- val[ordem_suplencia == ordem_suplencia_tse, .N] / nrow(val)
ok("ordem derivada reproduz a do TSE em 2016 acima de 99%", stopifnot(acerto > 0.99))
reg("voc_ordem_acerto_pct", round(100 * acerto, 2))
reg("voc_suplentes", nrow(ls_))

## ------------------------------------------------------- o vinculo com a cadeira
liga <- oc[!is.na(id_mandato) & tipo_ocupante != "titular"]
idx <- match(liga$id_mandato, m$id_mandato)
ok("cargo da ocupacao bate com o da cadeira", stopifnot(all(liga$cd_cargo == m$cd_cargo[idx])))
ok("UF da ocupacao bate com a da cadeira", stopifnot(all(liga$sg_uf == m$sg_uf[idx])))
ok("eleicao da ocupacao bate com a da cadeira", stopifnot(all(liga$ano_eleicao == m$ano_eleicao[idx])))
# a suplencia pertence a lista: o suplente so ocupa cadeira da propria lista
sup_lig <- oc[tipo_ocupante == "suplente" & !is.na(id_mandato) & !is.na(id_lista) &
                vinculo_cadeira %in% c("lista_vaga_unica","lista_data")]
if (nrow(sup_lig)) {
  cad_lista <- ml[, .(id_mandato, id_lista_cadeira = id_lista)]
  chk <- merge(sup_lig[, .(id_ocupacao, id_mandato, id_lista)], cad_lista, by = "id_mandato")
  ok("suplente so ocupa cadeira da propria lista",
     stopifnot(chk[id_lista != id_lista_cadeira, .N] == 0L))
  reg("voc_suplentes_ligados_a_cadeira", nrow(sup_lig))
}
# a auditoria de 30/08/2026 encontrou 16 ocupacoes comecando depois do fim do mandato da
# cadeira e 4 antes do inicio, sem que nenhum assert reclamasse
if (nrow(liga)) {
  jan <- merge(liga[!is.na(data_inicio), .(id_ocupacao, id_mandato, data_inicio)],
               m[, .(id_mandato, mandato_inicio, mandato_fim)], by = "id_mandato")
  fora <- jan[as.IDate(data_inicio) > as.IDate(mandato_fim) + 45L |
                as.IDate(data_inicio) < as.IDate(mandato_inicio) - 45L]
  reg("voc_ocupacao_fora_da_janela_do_mandato", nrow(fora))
  ok("ocupacao cai dentro da janela do mandato da cadeira", stopifnot(nrow(fora) == 0L))
}
## OCU-02 (24/09/2026): nenhuma ocupacao, titular incluido, termina depois do fim do mandato
## que a origina (R/42 ja limita data_fim ao mandato_fim na fonte; aqui e so a garantia).
todos_com_cadeira <- oc[!is.na(id_mandato) & !is.na(data_fim)]
if (nrow(todos_com_cadeira)) {
  jan_fim <- merge(todos_com_cadeira[, .(id_ocupacao, id_mandato, data_fim)],
                    m[, .(id_mandato, mandato_fim)], by = "id_mandato")
  fora_fim <- jan_fim[as.IDate(data_fim) > as.IDate(mandato_fim)]
  reg("voc_ocupacao_termina_depois_do_fim_do_mandato", nrow(fora_fim))
  ok("nenhuma ocupacao termina depois do fim do mandato da cadeira", stopifnot(nrow(fora_fim) == 0L))
}
ok("ordem de ocupacao so existe onde a cadeira foi identificada",
   stopifnot(oc[is.na(id_mandato), all(is.na(ordem_ocupacao))]))
ok("troca de partido so se mede em suplente convocado",
   stopifnot(oc[partido_difere_do_titular %in% TRUE, all(tipo_ocupante == "suplente")]))
ok("ocupacao do titular sempre ligada a cadeira pela eleicao",
   stopifnot(oc[tipo_ocupante == "titular", all(vinculo_cadeira == "eleicao")]))

## ------------------------------------------------------- a troca de partido por dentro
tp <- oc[partido_difere_do_titular %in% TRUE]
ok("troca de partido so onde os dois partidos sao conhecidos e diferentes",
   stopifnot(tp[is.na(sg_partido_ocupante) | is.na(sg_partido_titular) |
                  sg_partido_ocupante == sg_partido_titular, .N] == 0L))
reg("voc_troca_de_partido_na_cadeira", nrow(tp))
cat("\ncadeiras com mais de um ocupante:", oc[!is.na(id_mandato), .N, by = id_mandato][N > 1, .N], "\n")
reg("voc_cadeiras_com_mais_de_um_ocupante", oc[!is.na(id_mandato), .N, by = id_mandato][N > 1, .N])

## ------------------------------------------------------- a camada de ocupacao da v1.1 (29/09/2026)
# Regra geral de vinculo do R/42 (secao 4b), datas com origem propria e o par 20 (SE 2018) fechado por curadoria.
# Nenhuma checagem anterior foi retirada ou afrouxada; o que segue so acrescenta.
TOL_DIAS <- 3L   # mesma tolerancia do R/42 para sobreposicao entre ocupantes de uma cadeira
CLASSE_REGRA <- c("lista_vaga_inicial", "lista_sucessao_vaga")
ORIG_INI <- c("fonte", "convencao", "curadoria_ocu07", "biografia_camara", "sinal_efetivacao_api")
ORIG_FIM <- c("fonte", "biografia_camara", "convencao_fim_legislatura", "curadoria_biografia_camara")

ok("origem_data_inicio em vocabulario fechado, preenchida se e somente se ha data_inicio",
   { in_set(oc$origem_data_inicio, ORIG_INI, permitir_na = TRUE)
     stopifnot(identical(is.na(oc$origem_data_inicio), is.na(oc$data_inicio))) })
ok("origem_data_fim existe, em vocabulario fechado, preenchida se e somente se ha data_fim",
   { stopifnot("origem_data_fim" %in% names(oc))
     in_set(oc$origem_data_fim, ORIG_FIM, permitir_na = TRUE)
     stopifnot(identical(is.na(oc$origem_data_fim), is.na(oc$data_fim))) })
# as origens que so a regra produz nao aparecem em ocupacao ligada por outro caminho
ok("origem de data dada pela regra so aparece em ocupacao ligada pela regra",
   stopifnot(oc[origem_data_inicio %in% c("biografia_camara", "sinal_efetivacao_api"), all(vinculo_cadeira %in% CLASSE_REGRA)],
             oc[origem_data_fim %in% c("biografia_camara", "convencao_fim_legislatura"), all(vinculo_cadeira %in% CLASSE_REGRA)]))
for (v in VINCULOS) reg(paste0("voc_vinculo_", v), oc[vinculo_cadeira %in% v, .N])
for (v in ORIG_INI) reg(paste0("voc_origem_data_inicio_", v), oc[origem_data_inicio %in% v, .N])
for (v in ORIG_FIM) reg(paste0("voc_origem_data_fim_", v), oc[origem_data_fim %in% v, .N])

# a classe da regra: toda ocupacao ligada por lista_vaga_inicial ou lista_sucessao_vaga tem cadeira e data de inicio decididas
# e, quando tem fim, fim nao anterior ao inicio. O id_lista do ocupante nao e conferido com o da cadeira porque a
# lista de 1998 e a da coligacao (P25 no ocupante, P45 na cadeira do AL) e o ocupante da Camara vem sem id_lista.
cl <- oc[vinculo_cadeira %in% CLASSE_REGRA]
ok("ocupacao da classe da regra tem cadeira, data de inicio decidida e nenhuma data invertida",
   stopifnot(nrow(cl) > 0L, all(!is.na(cl$id_mandato)), all(!is.na(cl$data_inicio)),
             all(is.na(cl$data_fim) | as.IDate(cl$data_fim) >= as.IDate(cl$data_inicio)),
             all(cl$tipo_ocupante %in% c("suplente", "suplente_efetivado")),
             all(cl$esfera == "federal")))
reg("voc_regra_ocupacoes_lista_vaga_inicial", cl[vinculo_cadeira == "lista_vaga_inicial", .N])
reg("voc_regra_ocupacoes_lista_sucessao_vaga", cl[vinculo_cadeira == "lista_sucessao_vaga", .N])
reg("voc_regra_cadeiras_com_ocupante_novo", uniqueN(cl$id_mandato))

# os casos que a regra precisa reproduzir: Talvane e Helenildo (AL 1998), PR 2002 e RJ 2018
casos <- data.table(
  id_pessoa  = c("BOCEL0279852", "BOCEL0033384", "BOCEL0034572", "BOCEL0149063"),
  id_mandato = c("M1998_AL_6_4580_200000206045801", "M1998_AL_6_4580_200000206045801",
                 "M2002_PR_6_1345_724", "M2018_RJ_6_1010_190000626396"),
  vinc = c("lista_vaga_inicial", "lista_sucessao_vaga", "lista_vaga_inicial", "lista_vaga_inicial"),
  ini = c("1999-02-03", "1999-04-08", "2003-02-01", "2019-02-01"),
  org_ini = c("biografia_camara", "biografia_camara", "fonte", "fonte"),
  fim = c("1999-04-07", "2003-01-31", "2007-01-31", "2023-01-31"),
  org_fim = c("biografia_camara", "convencao_fim_legislatura", "fonte", "fonte"))
ok("casos AL 1998, PR 2002 e RJ 2018 ligados a cadeira, com datas e origens decididas",
   { chk <- merge(casos, oc[tipo_ocupante != "titular",
                            .(id_pessoa, id_mandato, vinculo_cadeira, data_inicio, data_fim, origem_data_inicio, origem_data_fim)],
                  by = c("id_pessoa", "id_mandato"), all.x = TRUE, sort = FALSE)
     stopifnot(nrow(chk) == nrow(casos),
               all((chk$vinc == chk$vinculo_cadeira) %in% TRUE),
               all((chk$ini == as.character(chk$data_inicio)) %in% TRUE),
               all((chk$fim == as.character(chk$data_fim)) %in% TRUE),
               all((chk$org_ini == chk$origem_data_inicio) %in% TRUE),
               all((chk$org_fim == chk$origem_data_fim) %in% TRUE)) })
# 3 cadeiras, cada uma com exatamente o ocupante esperado e sem outro ocupante ligado pela regra
ok("as tres cadeiras dos casos tem so os ocupantes esperados na classe da regra",
   stopifnot(setequal(unique(cl[id_mandato %in% casos$id_mandato, id_pessoa]), casos$id_pessoa)))
# Helenildo comeca na data da biografia, nao na convencao do mandato (1999-02-01)
ok("AL 1998: Helenildo comeca em 1999-04-08 pela biografia e Talvane fecha em 1999-04-07",
   stopifnot(oc[id_pessoa == "BOCEL0033384" & id_mandato == "M1998_AL_6_4580_200000206045801",
                as.character(data_inicio) == "1999-04-08" & origem_data_inicio == "biografia_camara"],
             oc[id_pessoa == "BOCEL0279852" & id_mandato == "M1998_AL_6_4580_200000206045801",
                as.character(data_fim) == "1999-04-07"]))

# par 20 (SE 2018): a primeira ocupacao de Jose Valdevan termina em 27/04/2022, com origem rastreavel a biografia
p20 <- oc[id_pessoa == "BOCEL0249016" & id_mandato == "M2018_SE_6_1313_260000623622"][order(as.IDate(data_inicio))]
ok("par 20 (SE 2018): primeira ocupacao fechada em 2022-04-27 por curadoria da biografia, e a segunda comeca depois",
   stopifnot(nrow(p20) == 2L, as.character(p20$data_fim[1]) == "2022-04-27",
             p20$origem_data_fim[1] == "curadoria_biografia_camara",
             as.IDate(p20$data_inicio[2]) > as.IDate(p20$data_fim[1]),
             oc[origem_data_fim %in% "curadoria_biografia_camara", .N] == 1L))
reg("voc_par20_ocupacoes_fechadas_por_curadoria", oc[origem_data_fim %in% "curadoria_biografia_camara", .N])

## sobreposicao: intervalos por ocupacao (fim aberto = fim do mandato da cadeira)
iv <- merge(oc[!is.na(id_mandato) & !is.na(id_pessoa) & !is.na(data_inicio),
               .(id_ocupacao, id_mandato, id_pessoa, esfera, cd_cargo, tipo_ocupante, vinculo_cadeira,
                 ini = as.IDate(data_inicio), fim_ = as.IDate(data_fim))],
            m[, .(id_mandato, mandato_fim = as.IDate(mandato_fim))], by = "id_mandato")
iv[, fim := fifelse(is.na(fim_), mandato_fim, fim_)]
# (a) a mesma pessoa em duas ocupacoes da mesma cadeira ao mesmo tempo. Conta e registra; so a esfera federal e imposta,
# porque o unico caso conhecido nela era o par 20. Toque de fronteira (fim de uma = inicio da outra) nao e sobreposicao.
mp <- merge(iv, iv, by = c("id_mandato", "id_pessoa"), suffixes = c(".a", ".b"), allow.cartesian = TRUE)[id_ocupacao.a < id_ocupacao.b]
mp[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
reg("voc_mesma_pessoa_mesma_cadeira_pares", nrow(mp))
reg("voc_mesma_pessoa_mesma_cadeira_pares_sobrepostos", mp[dias > 0L, .N])
reg("voc_mesma_pessoa_mesma_cadeira_pares_sobrepostos_federais", mp[dias > 0L & esfera.a == "federal", .N])
reg("voc_mesma_pessoa_mesma_cadeira_pares_sobrepostos_nao_federais", mp[dias > 0L & esfera.a != "federal", .N])
reg("voc_mesma_pessoa_mesma_cadeira_cadeiras_com_sobreposicao", mp[dias > 0L, uniqueN(id_mandato)])
reg("voc_mesma_pessoa_mesma_cadeira_pares_toque_de_fronteira", mp[dias == 0L, .N])
ok("nenhuma pessoa com duas ocupacoes sobrepostas na mesma cadeira federal (par 20 fechado)",
   stopifnot(mp[dias > 0L & esfera.a == "federal", .N] == 0L))
# (b) numa cadeira da classe da regra nao ha duas pessoas ao mesmo tempo (sobreposicao acima da tolerancia)
ex_cl <- merge(iv[id_mandato %in% cl$id_mandato], iv[id_mandato %in% cl$id_mandato], by = "id_mandato",
               suffixes = c(".a", ".b"), allow.cartesian = TRUE)[id_pessoa.a < id_pessoa.b]
ex_cl[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
reg("voc_exclusividade_cadeiras_da_classe_da_regra", uniqueN(cl$id_mandato))
reg("voc_exclusividade_classe_da_regra_pares_sobrepostos", ex_cl[dias > 0L, .N])
reg("voc_exclusividade_classe_da_regra_pares_sobrepostos_acima_da_tolerancia", ex_cl[dias > TOL_DIAS, .N])
ok("cadeira da classe da regra nao tem duas pessoas ao mesmo tempo (tolerancia de 3 dias)",
   stopifnot(ex_cl[dias > TOL_DIAS, .N] == 0L))
# (c) o restante das cadeiras de deputado federal: contado e registrado, nao imposto. Os pares vindos de
# lista_vaga_unica ou lista_data_estrita sem decisao de vaga e os poucos de fonte oficial em disputa ficam relatados.
fd <- iv[cd_cargo == 6L]
ex_fd <- merge(fd, fd, by = "id_mandato", suffixes = c(".a", ".b"), allow.cartesian = TRUE)[id_pessoa.a < id_pessoa.b]
ex_fd[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
ex_fd <- ex_fd[dias > TOL_DIAS]
C2 <- c("lista_vaga_unica", "lista_data_estrita")
n_fd <- nrow(ex_fd)
n_fd_c2 <- ex_fd[vinculo_cadeira.a %in% C2 | vinculo_cadeira.b %in% C2, .N]
reg("voc_exclusividade_deputado_federal_pares_sobrepostos_acima_da_tolerancia", n_fd)
reg("voc_exclusividade_deputado_federal_cadeiras_com_pares_sobrepostos", ex_fd[, uniqueN(id_mandato)])
reg("voc_exclusividade_deputado_federal_pares_com_lado_ligado_por_lista_vaga_unica_ou_data_estrita", n_fd_c2)
reg("voc_exclusividade_deputado_federal_pares_sem_lado_ligado_por_lista", n_fd - n_fd_c2)
# 29/09/2026 (achado DADOS-04): os testes (a) a (c) usam so ocupacao com id_pessoa, porque comparam pessoas. A ocupacao de
# ocupante nao identificado (id_pessoa NA) fica fora deles; a contagem abaixo registra quanto fica de fora e quantos
# pares de deputado federal acima da tolerancia aparecem quando o ocupante sem id_pessoa entra como pessoa distinta.
# Nada e afrouxado, os asserts acima seguem sobre o mesmo conjunto.
iv_np <- merge(oc[!is.na(id_mandato) & !is.na(data_inicio) & cd_cargo == 6L,
                  .(id_ocupacao, id_mandato, id_pessoa, ini = as.IDate(data_inicio), fim_ = as.IDate(data_fim))],
               m[, .(id_mandato, mandato_fim = as.IDate(mandato_fim))], by = "id_mandato")
iv_np[, fim := fifelse(is.na(fim_), mandato_fim, fim_)]
ex_np <- merge(iv_np, iv_np, by = "id_mandato", suffixes = c(".a", ".b"), allow.cartesian = TRUE)[
  id_ocupacao.a < id_ocupacao.b & (is.na(id_pessoa.a) | is.na(id_pessoa.b) | id_pessoa.a != id_pessoa.b)]
ex_np[, dias := as.integer(pmin(fim.a, fim.b) - pmax(ini.a, ini.b))]
ex_np <- ex_np[dias > TOL_DIAS]
reg("voc_ocupacoes_com_cadeira_sem_id_pessoa", oc[!is.na(id_mandato) & is.na(id_pessoa), .N])
reg("voc_ocupacoes_de_deputado_federal_com_cadeira_sem_id_pessoa", iv_np[is.na(id_pessoa), .N])
reg("voc_exclusividade_deputado_federal_pares_acima_da_tolerancia_com_ocupante_sem_id_pessoa", ex_np[, .N])
reg("voc_exclusividade_deputado_federal_cadeiras_com_pares_acima_da_tolerancia_com_ocupante_sem_id_pessoa", ex_np[, uniqueN(id_mandato)])
reg("voc_exclusividade_deputado_federal_pares_fora_da_checagem_por_falta_de_id_pessoa", nrow(ex_np) - n_fd)
ok("a contagem sem o filtro de id_pessoa contem a contagem com o filtro (74 pares, ate hoje, viram 76 com o ocupante sem id_pessoa)",
   stopifnot(nrow(ex_np) >= n_fd, ex_np[, uniqueN(id_mandato)] >= ex_fd[, uniqueN(id_mandato)]))

# as linhas de docs/EXCECOES_CONHECIDAS.csv das sobreposicoes de fim de legislatura e do par 6 (auditoria de 24/09/2026)
# descrevem pares que ainda existem nos dados; se a curadoria os resolver, a linha fica obsoleta e este teste avisa
exc <- fread("docs/EXCECOES_CONHECIDAS.csv", colClasses = "character", na.strings = "NA")
exc_par <- exc[grepl("auditoria de 24/09/2026, par (6|12|13|14|15|21)\\)", descricao)]
ok("as seis linhas de EXCECOES_CONHECIDAS das sobreposicoes (pares 6, 12, 13, 14, 15 e 21) batem com os dados",
   { stopifnot(nrow(exc_par) == 6L)
     for (i in seq_len(nrow(exc_par))) {
       d <- exc_par$descricao[i]
       cad <- regmatches(d, regexpr("M[0-9]{4}_[A-Z]{2}_6_[0-9A-Za-z_]+", d))
       ids <- unique(regmatches(d, gregexpr("OC[0-9]{8}", d))[[1]])
       par_i <- sub(".*par ([0-9]+)\\).*", "\\1", d)
       stopifnot(length(cad) == 1L, length(ids) == 2L)
       x <- iv[id_ocupacao %in% ids]
       if (nrow(x) != 2L || any(x$id_mandato != cad) || x[, min(fim) - max(ini)] <= TOL_DIAS)
         stop("par ", par_i, ": as ocupacoes ", paste(ids, collapse = " e "), " nao se sobrepoem na cadeira ", cad,
              " como a linha descreve; a linha de EXCECOES_CONHECIDAS.csv esta obsoleta")
     } })
reg("voc_excecoes_linhas_de_sobreposicao_conferidas", nrow(exc_par))

## ------------------------------------------------------- universo comparavel
mm <- fread("data/mandatos.csv", select = c("id_mandato","esfera","sg_uf","sg_ue","cd_cargo",
                                            "ano_eleicao","forma_saida","universo_comparavel","chave_tse"))
ok("universo comparavel e logico sem vazio", stopifnot(all(!is.na(mm$universo_comparavel))))
# dentro do universo a ausencia de saida significa que o mandato terminou, e por isso toda
# unidade-eleicao marcada precisa ter ao menos uma saida observada
un <- mm[universo_comparavel == TRUE,
         .(k = sum(forma_saida != "nao_observado")),
         by = .(esfera, uni = fifelse(esfera == "municipal", sg_ue,
                                      fifelse(esfera == "estadual", sg_uf, as.character(cd_cargo))),
                ano_eleicao)]
ok("toda unidade-eleicao do universo tem saida observada", stopifnot(un[k == 0, .N] == 0L))
ok("chave de juncao com o TSE e unica", checa_unica(as.data.frame(mm), "chave_tse"))
for (e in c("federal","estadual","municipal"))
  reg(paste0("voc_saida_no_universo_", e),
      round(100 * mm[esfera == e & universo_comparavel == TRUE, mean(forma_saida != "nao_observado")], 1))

## ------------------------------------------------------- fora de cobertura
fc <- c("veracidade do registro de exercicio publicado por cada casa legislativa",
        "identidade do suplente pareado so por nome dentro da lista, sem titulo nem CPF",
        "cadeira do suplente quando a fonte nao nomeia o titular substituido (vinculo casa_legislatura)",
        "licenca temporaria do titular que a fonte nao distingue de saida definitiva",
        "dia e mes trocados em data_inicio ou data_fim fora dos casos que a auditoria de 24/09/2026 localizou (docs/EXCECOES_CONHECIDAS.csv); nao houve varredura exaustiva de toda a fonte",
        # 29/09/2026: o que a camada de ocupacao da v1.1 nao atesta
        "validade da regra de vaga e sucessao (R/42, secao 4b) fora dos casos com fonte oficial, porque o dominio de 18 cadeiras federais e o pareamento por chave de lista inferem a vaga sem ato publicado",
        "origem_data_fim igual a fonte inclui a data limitada ao fim do mandato pela regra OCU-02, e a coluna nao separa o fim publicado do fim cortado",
        paste0("cadeiras de deputado federal com duas pessoas ao mesmo tempo por mais de ", TOL_DIAS, " dias, em numero de ", n_fd,
               " pares, dos quais ", n_fd_c2, " tem um lado ligado por lista_vaga_unica ou lista_data_estrita sem decisao de vaga, ",
               "contados sem curadoria e com seis pares documentados em docs/EXCECOES_CONHECIDAS.csv"),
        # 29/09/2026 (achados DADOS-04 e F3)
        paste0("ocupante sem id_pessoa: ", oc[!is.na(id_mandato) & is.na(id_pessoa), .N], " ocupacoes com cadeira e sem pessoa ficam fora dos testes de sobreposicao e de exclusividade, ",
               "que comparam pessoas; em deputado federal isso deixa de fora ", nrow(ex_np) - n_fd, " pares acima da tolerancia (", n_fd,
               " pares no teste, ", nrow(ex_np), " com o ocupante sem id_pessoa)"),
        "duas ligacoes de cadeira feitas pela regra sem gabarito independente (OC00489947 e OC00491741, R/42 secao 4b), cuja reproducao pelos casos de calibracao nao as valida onde a regra decidiu sozinha",
        paste0("mesma pessoa em duas ocupacoes sobrepostas na mesma cadeira fora da esfera federal, com ",
               mp[dias > 0L & esfera.a != "federal", .N], " pares em ", mp[dias > 0L & esfera.a != "federal", uniqueN(id_mandato)],
               " cadeira(s) vindos da fonte e sem correcao aqui"))
gravar_relatorio_verificacao("ocupacoes e suplencia (R/40, R/41, R/42)", script,
                             passou = passou, falhou = falhas, fora_de_cobertura = fc)
cat("\nverifica_ocupacoes: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("\nfalhas:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); quit(save="no", status=1) }
