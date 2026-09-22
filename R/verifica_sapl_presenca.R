# verifica_sapl_presenca.R — verificacao cetica de R/55_sapl_presenca.R
#
# O que esta verificacao cobre: conservacao e unicidade das tres tabelas de presenca, coerencia
# aritmetica das janelas contra a cadencia de sessoes da propria Casa, respeito ao limiar
# declarado da lacuna, validade do pareamento com o BOCEL (municipio, eleicao e cargo), e tres
# checagens INDEPENDENTES do proprio script: (a) discriminacao, comparando a entrada tardia de
# quem o SAPL diz ter comecado fora de 1 de janeiro contra quem comecou no dia 1, que e o
# contrafactual natural; (b) corroboracao cruzada com o ato escrito que R/54 leu do campo de
# texto livre, evidencia independente da presenca; (c) recesso, medindo quanto da lacuna e
# artefato de calendario e nao afastamento.
#
# FORA DE COBERTURA: se quem assinou a presenca e quem de fato ocupou a cadeira; se a ausencia
# prolongada e afastamento formal ou falta; erro de digitacao no cadastro do SAPL, alimentado a
# mao pela Casa; e a generalizacao das 455 instancias com presenca datada para as 5.569 camaras
# do pais, que e amostra de conveniencia e nao amostra probabilistica.
#
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_sapl_presenca.R
set.seed(20260904)
suppressPackageStartupMessages({ library(data.table) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_sapl_presenca.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
passou <- character(); falhou <- character(); fora <- character()
ok <- function(cond, msg) {
  if (isTRUE(cond)) passou <<- c(passou, msg) else falhou <<- c(falhou, msg)
  cat(if (isTRUE(cond)) "PASS " else "FAIL ", msg, "\n")
}
D <- function(x) as.IDate(x)

jan <- fread("data/sapl_presenca_exercicio.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
lac <- fread("data/sapl_presenca_lacunas.csv",   colClasses = "character", na.strings = "NA", encoding = "UTF-8")
cob <- fread("data/sapl_presenca_cobertura.csv", colClasses = "character", na.strings = "NA", encoding = "UTF-8")
man <- fread("data/mandatos.csv", colClasses = "character", na.strings = c("NA", ""), encoding = "UTF-8")
num <- c("n_sessoes_leg","n_presencas","dias_ate_a_primeira","dias_da_ultima_ao_fim",
         "sessoes_antes_da_entrada","sessoes_depois_da_saida")
jan[, (num) := lapply(.SD, as.integer), .SDcols = num]
numl <- c("idx","idx_prox","dias","sessoes_perdidas","n_entrantes")
lac[, (numl) := lapply(.SD, as.integer), .SDcols = numl]
cob[, `:=`(n_sessoes = as.integer(n_sessoes), n_presencas = as.integer(n_presencas))]
cat("janelas:", nrow(jan), "| lacunas:", nrow(lac), "| instancias no cache:", nrow(cob), "\n\n")

## ---- 1. conservacao: nada foi inventado
ok(nrow(jan) > 0 && nrow(lac) > 0, "as duas tabelas de presenca sairam preenchidas")
ok(all(paste(jan$uf, jan$sg_ue) %in% paste(cob$uf, cob$sg_ue)),
   "toda janela vem de instancia que esta no cache coletado")
ok(all(paste(lac$uf, lac$sg_ue) %in% paste(cob$uf, cob$sg_ue)),
   "toda lacuna vem de instancia que esta no cache coletado")
ok(uniqueN(jan[, paste(uf, sg_ue, parlamentar, legislatura)]) == nrow(jan),
   "uma janela por instancia, parlamentar e legislatura")
ok(uniqueN(lac$id_lacuna) == nrow(lac), "id_lacuna e unico")
ok(cob[, sum(n_presencas)] >= jan[, sum(n_presencas)],
   "a soma das presencas das janelas nao excede o que o cache traz")

## ---- 2. aritmetica da janela contra a cadencia da Casa
ok(jan[n_presencas < 1, .N] == 0, "toda janela tem ao menos uma presenca")
ok(jan[D(primeira_presenca) > D(ultima_presenca), .N] == 0,
   "a primeira presenca nunca vem depois da ultima")
ok(jan[D(primeira_presenca) < D(primeira_sessao) | D(ultima_presenca) > D(ultima_sessao), .N] == 0,
   "a janela de presenca cabe dentro da cadencia de sessoes da legislatura")
ok(jan[n_presencas > n_sessoes_leg, .N] == 0,
   "ninguem comparece a mais sessoes do que a Casa realizou na legislatura")
ok(jan[sessoes_antes_da_entrada < 0 | sessoes_depois_da_saida < 0, .N] == 0,
   "as contagens de sessoes antes da entrada e depois da saida nao sao negativas")
ok(jan[!is.na(sessoes_antes_da_entrada) & sessoes_antes_da_entrada >= n_sessoes_leg, .N] == 0,
   "a entrada nunca cai depois da ultima sessao contada")
ok(jan[dias_ate_a_primeira < 0 | dias_da_ultima_ao_fim < 0, .N] == 0,
   "as folgas em dias contra a cadencia da Casa nao sao negativas")

## ---- 3. o limiar declarado da lacuna e respeitado
MIN_SESSOES <- 3L; MIN_DIAS <- 45L
ok(lac[dias < MIN_DIAS, .N] == 0, "nenhuma lacuna publicada fica abaixo do limiar de 45 dias")
ok(lac[sessoes_perdidas < MIN_SESSOES, .N] == 0,
   "nenhuma lacuna publicada fica abaixo do limiar de tres sessoes perdidas")
ok(lac[idx_prox - idx - 1L != sessoes_perdidas, .N] == 0,
   "sessoes perdidas confere com a distancia entre as duas presencas na sequencia da Casa")
ok(lac[D(ultima_presenca_antes) >= D(primeira_presenca_depois), .N] == 0,
   "a lacuna abre antes de fechar")
ok(lac[abs(as.integer(D(primeira_presenca_depois) - D(ultima_presenca_antes)) - dias) > 0, .N] == 0,
   "a duracao em dias e a diferenca entre as duas datas")

## ---- 4. a graduacao de confianca corresponde ao que ela afirma
ok(lac[confianca == "alta" & (n_entrantes < 1 | is.na(id_mandato_bocel)), .N] == 0,
   "confianca alta exige entrante na janela e mandato pareado ao BOCEL")
ok(lac[confianca == "media" & (n_entrantes < 1 | !is.na(id_mandato_bocel)), .N] == 0,
   "confianca media exige entrante na janela sem mandato pareado")
ok(lac[confianca == "baixa" & n_entrantes >= 1, .N] == 0,
   "confianca baixa nao tem entrante na janela")
ok(all(unique(lac$confianca) %in% c("alta","media","baixa")),
   "a coluna de confianca usa apenas o vocabulario declarado")
lac[, n_listados := fifelse(is.na(entrantes_na_janela) | entrantes_na_janela == "", 0L,
                            lengths(strsplit(entrantes_na_janela, ";", fixed = TRUE)))]
ok(lac[n_listados != n_entrantes, .N] == 0,
   "a lista nominal de entrantes tem exatamente o numero de entrantes declarado")
ok(lac[n_entrantes >= 1 & parlamentar %in% strsplit(entrantes_na_janela, ";", fixed = TRUE)[[1]], .N] >= 0 &&
     lac[, sum(mapply(function(p, e) p %in% strsplit(e, ";", fixed = TRUE)[[1]],
                      parlamentar, fifelse(is.na(entrantes_na_janela), "", entrantes_na_janela)))] == 0,
   "o dono da lacuna nunca aparece como entrante da propria janela")

## ---- 5. o pareamento com o BOCEL aponta para o mandato certo
m13 <- man[cd_cargo == "13"]
jb <- jan[!is.na(id_mandato_bocel)]
ok(all(jb$id_mandato_bocel %in% m13$id_mandato),
   "todo mandato pareado existe em mandatos.csv como vereador")
ok(all(na.omit(unique(jb$id_pessoa_bocel)) %in% man$id_pessoa),
   "toda pessoa pareada existe no cadastro do banco")
chk <- merge(jb[, .(uf, sg_ue, ano_eleicao_bocel, id_mandato = id_mandato_bocel)],
             m13[, .(id_mandato, sg_ue_m = sg_ue, ano_m = ano_eleicao, uf_m = sg_uf)],
             by = "id_mandato")
ok(chk[sg_ue != sg_ue_m, .N] == 0, "o mandato pareado e do mesmo municipio da instancia do SAPL")
ok(chk[uf != uf_m, .N] == 0, "o mandato pareado e da mesma UF da instancia do SAPL")
ok(chk[ano_eleicao_bocel != ano_m, .N] == 0, "o mandato pareado e da mesma eleicao do registro")
ok(uniqueN(jb$id_mandato_bocel) == nrow(jb),
   "cada mandato do BOCEL recebe no maximo uma janela de presenca")

## ---- 6. CHECAGEM INDEPENDENTE (a): discriminacao contra o contrafactual
# Quem o SAPL registra comecando fora de 1 de janeiro entrou no meio da legislatura, e por isso
# deve perder sessoes no comeco; quem comeca em 1 de janeiro e o titular eleito, que nao deve.
# Se a presenca mede exercicio, a diferenca entre os dois grupos tem de ser grande. Se as duas
# taxas fossem parecidas, a entrada tardia estaria medindo ausencia, e nao convocacao.
g <- jan[!is.na(data_inicio_sapl)]
g[, meio_da_legislatura := substr(data_inicio_sapl, 6, 10) != "01-01"]
tx <- g[, .(n = .N, tardia = sum(sessoes_antes_da_entrada >= 3, na.rm = TRUE)), by = meio_da_legislatura]
tx[, taxa := tardia / n]
print(tx[order(-meio_da_legislatura)])
t_meio <- tx[meio_da_legislatura == TRUE, taxa]; t_jan <- tx[meio_da_legislatura == FALSE, taxa]
if (length(t_meio) && length(t_jan) && tx[, min(n)] >= 30) {
  reg("vpres_taxa_tardia_meio_legislatura", round(t_meio, 4))
  reg("vpres_taxa_tardia_inicio_regular", round(t_jan, 4))
  reg("vpres_razao_discriminacao", round(t_meio / max(t_jan, 1e-9), 3))
  ok(t_meio > t_jan,
     "quem o SAPL diz ter entrado no meio da legislatura perde mais sessoes iniciais do que o titular regular")
  ok(t_meio >= 2 * t_jan,
     "a entrada tardia e ao menos duas vezes mais frequente no grupo que entrou no meio")
} else {
  fora <- c(fora, "grupos pequenos demais para a checagem de discriminacao")
}

## ---- 7. CHECAGEM INDEPENDENTE (b): corroboracao pelo ato escrito que R/54 leu
if (file.exists("output/verificacao/sapl_presenca_cruza_texto.csv")) {
  cru <- fread("output/verificacao/sapl_presenca_cruza_texto.csv", na.strings = "NA", encoding = "UTF-8")
  cru[, dist := as.integer(dist)]
  taxa60 <- cru[dist <= 60, .N] / max(nrow(cru), 1)
  cat("cruzamento com o texto: n =", nrow(cru), "| mediana da distancia =",
      as.integer(median(cru$dist, na.rm = TRUE)), "dias | dentro de 60 dias =",
      sprintf("%.1f%%", 100 * taxa60), "\n")
  reg("vpres_cruzamento_n", nrow(cru))
  reg("vpres_cruzamento_mediana_dias", as.integer(median(cru$dist, na.rm = TRUE)))
  reg("vpres_cruzamento_taxa_60d", round(taxa60, 4))
  ok(nrow(cru) >= 30, "ha mandatos suficientes alcancados pelas duas fontes")
  ok(taxa60 >= 0.60,
     "ao menos 60% dos atos escritos caem a menos de 60 dias da lacuna vista pela presenca")
} else {
  fora <- c(fora, "sem cruzamento com o texto livre nesta execucao")
}

## ---- 8. CHECAGEM INDEPENDENTE (c): a lacuna nao e artefato de recesso
# O recesso nao produz sessao, e sessao inexistente nao entra na contagem de sessoes perdidas.
# A checagem confirma isso pelo dado: a fracao de lacunas contidas inteiramente na virada do ano
# tem de ser pequena, e a duracao mediana tem de superar com folga a duracao de um recesso.
lac[, so_recesso := (substr(ultima_presenca_antes, 6, 7) == "12" & substr(primeira_presenca_depois, 6, 7) %in% c("01","02"))]
fr <- lac[so_recesso == TRUE, .N] / nrow(lac)
reg("vpres_lacunas_na_virada_do_ano", lac[so_recesso == TRUE, .N])
reg("vpres_fracao_virada_do_ano", round(fr, 4))
reg("vpres_lacuna_mediana_dias", as.integer(median(lac$dias, na.rm = TRUE)))
cat("lacunas contidas na virada do ano:", lac[so_recesso == TRUE, .N],
    sprintf("(%.1f%%)", 100 * fr), "| duracao mediana:", as.integer(median(lac$dias)), "dias\n")
ok(fr <= 0.25, "no maximo um quarto das lacunas cai na virada do ano")
ok(median(lac$dias, na.rm = TRUE) > 60, "a lacuna mediana dura mais que um recesso")

## ---- 9. calibragem da folga, medida contra o gabarito do proprio SAPL
gab <- jan[!is.na(data_inicio_sapl) & substr(data_inicio_sapl, 6, 10) != "01-01" & !is.na(primeira_presenca)]
gab[, folga := as.integer(D(primeira_presenca) - D(data_inicio_sapl))]
if (nrow(gab) >= 30) {
  reg("vpres_folga_n", nrow(gab))
  reg("vpres_folga_mediana_dias", as.integer(median(abs(gab$folga), na.rm = TRUE)))
  reg("vpres_folga_ate_30_dias", gab[abs(folga) <= 30, .N])
  reg("vpres_folga_taxa_ate_30_dias", round(gab[abs(folga) <= 30, .N] / nrow(gab), 4))
  ok(median(abs(gab$folga), na.rm = TRUE) <= 60,
     "a data de entrada por presenca fica a menos de dois meses da data que a Casa registra, na mediana")
  ok(gab[folga < -30, .N] / nrow(gab) <= 0.20,
     "a presenca raramente antecede em mais de um mes o inicio do mandato que a Casa registra")
} else {
  fora <- c(fora, "gabarito pequeno demais para calibrar a folga")
}

## ---- 10. cobertura, para o registro e para a declaracao de parcialidade
reg("vpres_instancias_no_cache", nrow(cob))
reg("vpres_instancias_com_presenca", cob[n_presencas > 0, .N])
reg("vpres_ufs_alcancadas", uniqueN(jan$uf))
reg("vpres_municipios_alcancados", uniqueN(jan[, paste(uf, sg_ue)]))
reg("vpres_janelas", nrow(jan)); reg("vpres_janelas_pareadas", nrow(jb))
reg("vpres_lacunas", nrow(lac))
for (cf in c("alta","media","baixa")) reg(paste0("vpres_lacunas_", cf), lac[confianca == cf, .N])
cat("\ncobertura:", uniqueN(jan[, paste(uf, sg_ue)]), "municipios em", uniqueN(jan$uf), "UFs\n")

## ---- 11. veredito
fora <- c(fora,
  "se quem assinou a lista de presenca e quem de fato ocupou a cadeira naquele dia",
  "se a ausencia prolongada e afastamento formal, licenca ou falta",
  "erro de digitacao no cadastro do SAPL, que a Casa alimenta a mao",
  "generalizacao das instancias coletadas para as 5.569 camaras, por ser amostra de conveniencia")
cat("\nPASS:", length(passou), "| FAIL:", length(falhou), "| FORA DE COBERTURA:", length(fora), "\n")
if (length(falhou)) { cat("\nfalhas:\n"); cat(paste0("  - ", falhou, collapse = "\n"), "\n") }
f <- gravar_relatorio_verificacao("R/55_sapl_presenca.R", script,
                                  passou = passou, falhou = falhou, fora_de_cobertura = fora)
cat("relatorio:", f, "\n")
if (length(falhou)) quit(status = 1)
