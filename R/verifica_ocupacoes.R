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
ok("vinculo_cadeira em vocabulario fechado",
   in_set(oc$vinculo_cadeira, c("eleicao","chapa_senado","lista_vaga_unica","lista_data_estrita",
                                "cadeira_unica_da_unidade","casa_legislatura","fonte_da_casa"), permitir_na = FALSE))
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
        "licenca temporaria do titular que a fonte nao distingue de saida definitiva")
gravar_relatorio_verificacao("ocupacoes e suplencia (R/40, R/41, R/42)", script,
                             passou = passou, falhou = falhas, fora_de_cobertura = fc)
cat("\nverifica_ocupacoes: PASSOU:", pass, "| FALHOU:", fail, "\n")
if (fail) { cat("\nfalhas:\n"); cat(paste0(" - ", falhas, collapse = "\n"), "\n"); quit(save="no", status=1) }
