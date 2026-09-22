# verifica_munic.R — verificacao independente da frente MUNIC (R/10_munic_inventario.R, R/11_munic_prefeitos.R)
# Reconta a partir dos arquivos de saida, confere chave unica, dominios, codigo NA, cardinalidade dos
# joins e compara com o ultimo registro de cada chave munic_* em output/numeros_assinatura.txt.
# Execucao: Rscript --vanilla R/verifica_munic.R   (a partir da raiz do repositorio)
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(readxl); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
logf <- "logs/verifica_munic.log"; sink(logf, split = TRUE)
cat("verifica_munic:", format(Sys.time()), "\n")
script <- "R/verifica_munic.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")
falhas <- character(); passou <- character()
chk <- function(cond, msg) { if (isTRUE(cond)) passou <<- c(passou, msg) else { falhas <<- c(falhas, msg); cat("FALHA:", msg, "\n") } }

## ---------------------------------------------------------------- arquivos e colunas
out <- fread("data/munic_prefeitos.csv", colClasses = list(character = c("id_municipio_ibge", "id_municipio_ibge6", "sg_ue")), na.strings = "NA")
corr <- fread("data/municipios_tse_ibge.csv", colClasses = "character", na.strings = "NA")
res <- fread("data/munic_resumo_substituicoes.csv", na.strings = "NA")
inv <- fread("data_raw/munic/inventario_variaveis_munic.csv", na.strings = "NA")
cols_out <- c("id_municipio_ibge","id_municipio_ibge6","ano_munic","data_referencia","sg_ue","nome_prefeito_munic","sexo","idade",
              "escolaridade","cor_raca","partido","partido_eleito_munic","partido_atual_munic","exercicio_ano_anterior","ano_eleicao_bocel",
              "id_mandato_bocel","id_pessoa_bocel","nome_prefeito_bocel","genero_bocel","dt_nascimento_bocel","idade_bocel","match_nome",
              "match_nome_parcial","jw_nome","match_sexo","dif_idade","match_idade","match_idade_ampla","status","criterio_status",
              "substituto_provavel","criterio_substituto","id_mandato_vice","id_pessoa_vice","vice_match_nome","vice_match_sexo","vice_match_idade")
chk(all(cols_out %in% names(out)), "munic_prefeitos.csv tem todas as colunas prometidas")
chk(all(c("sg_ue","sg_uf","id_municipio_ibge","nome_ibge","origem","id_municipio_ibge6","uf_divergente") %in% names(corr)), "municipios_tse_ibge.csv tem as colunas prometidas")
chk(all(names(out) == tolower(names(out))) && !any(grepl("[^a-z0-9_]", names(out))), "colunas minusculas sem acento")
# codigo de ausente: nenhuma celula vazia no CSV bruto
raw <- readLines("data/munic_prefeitos.csv", n = -1L)
chk(!any(grepl(",,|,$|,\"\",", raw)), "nenhuma celula vazia em munic_prefeitos.csv (ausente = NA)")
rawc <- readLines("data/municipios_tse_ibge.csv")
chk(!any(grepl(",,|,$|,\"\",", rawc)), "nenhuma celula vazia em municipios_tse_ibge.csv")

## ---------------------------------------------------------------- chaves unicas e dominios
invisible(checa_unica(as.data.frame(out), c("id_municipio_ibge", "ano_munic")))
invisible(checa_unica(as.data.frame(corr), "sg_ue"))
invisible(checa_unica(as.data.frame(corr[!is.na(id_municipio_ibge)]), "id_municipio_ibge"))
invisible(checa_unica(as.data.frame(inv), "ano_munic"))
invisible(checa_unica(as.data.frame(out[!is.na(id_mandato_bocel)]), c("id_mandato_bocel", "ano_munic")))
passou <- c(passou, "checa_unica: (id_municipio_ibge, ano_munic); sg_ue; id_municipio_ibge; ano_munic no inventario; (id_mandato_bocel, ano_munic)")
in_set(out$status, c("eleito_em_exercicio","outro_em_exercicio","indeterminado"), permitir_na = FALSE)
in_set(out$substituto_provavel, c("vice","terceiro","indeterminado","indeterminado_sem_vice_no_bocel"))
in_set(out$criterio_status, c("sem_prefeito_bocel","nome","sexo_idade","sexo_ou_idade_incompleto","sem_atributo"), permitir_na = FALSE)
in_set(out$sexo, c("MASCULINO","FEMININO"))
in_set(out$genero_bocel, c("MASCULINO","FEMININO","NÃO INFORMADO","NÃO DIVULGÁVEL"))
# genero nao binario do TSE nao pode decidir status nem substituto (erro silencioso corrigido em R/11)
chk(all(out[!genero_bocel %in% c("MASCULINO","FEMININO"), is.na(match_sexo)]), "genero_bocel fora de M/F nao gera match_sexo")
chk(out[!genero_bocel %in% c("MASCULINO","FEMININO") & criterio_status != "nome" & status == "outro_em_exercicio", .N] == 0,
    "nenhum 'outro_em_exercicio' decidido por genero nao informado/nao divulgavel")
reg("munic_verif_n_outro_por_genero_nao_binario", out[!genero_bocel %in% c("MASCULINO","FEMININO") & criterio_status != "nome" & status == "outro_em_exercicio", .N])
in_set(out$ano_munic, c(2004L,2005L,2009L,2013L,2017L,2021L), permitir_na = FALSE)
em_faixa(out$idade, 18, 110)
em_faixa(out$idade_bocel, 18, 100)
em_faixa(out$jw_nome, 0, 1)
passou <- c(passou, "in_set/em_faixa: status, substituto, criterio, sexo, genero, ano_munic, idade, idade_bocel, jw")
chk(all(is.na(out$substituto_provavel) == (out$status != "outro_em_exercicio")), "substituto_provavel sse outro_em_exercicio")
chk(all(is.na(out$id_mandato_bocel) == (out$criterio_status == "sem_prefeito_bocel")), "sem_prefeito_bocel sse id_mandato_bocel NA")
chk(all(out[!is.na(ano_eleicao_bocel), ano_eleicao_bocel == ((ano_munic - 1L) %/% 4L) * 4L]), "ano_eleicao_bocel = municipal anterior ao ano da MUNIC")
chk(all(nchar(out$id_municipio_ibge) == 7L) && all(substr(out$id_municipio_ibge, 1, 6) == out$id_municipio_ibge6), "id_municipio_ibge com 7 digitos coerente com os 6")
chk(all(nchar(corr$sg_ue) == 5L), "sg_ue com 5 digitos na correspondencia")
chk(all(corr[!is.na(id_municipio_ibge), nchar(id_municipio_ibge) == 7L]), "id_municipio_ibge com 7 digitos na correspondencia")
chk(all(out[is.na(sg_ue), criterio_status == "sem_prefeito_bocel"]), "sg_ue NA apenas em municipio sem prefeitura no BOCEL (Brasilia, Fernando de Noronha)")
reg("munic_verif_n_linhas_sg_ue_na", out[is.na(sg_ue), .N])
reg("munic_verif_n_linhas_sem_prefeito_bocel", out[criterio_status == "sem_prefeito_bocel", .N])
# datas impossiveis
dn <- as.IDate(out$dt_nascimento_bocel)
chk(all(is.na(dn) | (dn >= as.IDate("1900-01-01") & dn <= as.IDate("2003-12-31"))), "dt_nascimento_bocel plausivel")
chk(all(out[!is.na(idade_bocel), idade_bocel == floor(as.numeric(as.IDate(sprintf("%d-12-31", ano_munic)) - as.IDate(dt_nascimento_bocel)) / 365.25)]), "idade_bocel recalculada em 31/12 confere")
chk(all(out[!is.na(dif_idade), dif_idade == idade - idade_bocel]), "dif_idade = idade - idade_bocel")
chk(all(out[!is.na(match_idade), match_idade == (abs(dif_idade) <= 1)]), "match_idade = |dif| <= 1")
chk(all(out[!is.na(match_sexo), match_sexo == (sexo == genero_bocel)]), "match_sexo = sexo == genero_bocel")

## ---------------------------------------------------------------- BOCEL: prefeito vigente e cardinalidade do join
pos <- fread("data/posicoes_ano.csv", colClasses = list(character = "sg_ue"), na.strings = "NA")
anos <- sort(unique(out$ano_munic))
pv <- pos[cd_cargo == 11L & ano %in% anos, .(sg_ue, ano_munic = ano, id_mandato)]
invisible(checa_unica(as.data.frame(pv), c("sg_ue", "ano_munic")))
pv <- merge(pv, corr[, .(sg_ue, id_municipio_ibge)], by = "sg_ue", all.x = TRUE)
chk(all(!is.na(pv$id_municipio_ibge)), "todo prefeito vigente do BOCEL tem codigo IBGE")
# join many-to-one: cada linha MUNIC recebe no maximo um prefeito; o id_mandato em out e o do BOCEL
j <- merge(out[, .(id_municipio_ibge, ano_munic, id_mandato_bocel)], pv, by = c("id_municipio_ibge", "ano_munic"), all.x = TRUE)
chk(nrow(j) == nrow(out), "join out x posicoes_ano nao infla linhas")
chk(all(j[!is.na(id_mandato_bocel), id_mandato_bocel == id_mandato]), "id_mandato_bocel em out = prefeito vigente em posicoes_ano")
chk(all(j[is.na(id_mandato_bocel), is.na(id_mandato)]), "linhas sem prefeito no BOCEL de fato nao tem prefeito vigente")
# prefeituras do BOCEL ausentes da MUNIC em cada edicao
falt <- pv[!out, on = c("id_municipio_ibge", "ano_munic")][, .N, by = ano_munic]
cat("prefeituras do BOCEL ausentes da MUNIC por edicao:\n"); print(falt)
nao_par <- out[is.na(id_mandato_bocel), .(ano_munic, id_municipio_ibge, sg_ue)]
cat("municipios MUNIC sem prefeito no BOCEL:\n"); print(nao_par[, .N, by = ano_munic]); print(unique(nao_par$id_municipio_ibge))

## ---------------------------------------------------------------- leitura bruta: n de municipios por edicao direto do xls
bases <- inv[microdado_disponivel == TRUE & tem_bloco_prefeito == TRUE, .(ano_munic, arquivo, aba_prefeito, var_nome, var_sexo, var_idade)]
n_bruto <- rbindlist(lapply(seq_len(nrow(bases)), function(i) {
  d <- suppressMessages(read_excel(file.path("data_raw/munic", bases$arquivo[i]), sheet = bases$aba_prefeito[i], col_types = "text"))
  cod <- gsub("\\D", "", d[[1]])
  data.table(ano_munic = bases$ano_munic[i], n_bruto = sum(!is.na(cod) & nchar(cod) >= 6), n_cod_unicos = uniqueN(cod[!is.na(cod) & nchar(cod) >= 6]))
}))
n_bruto <- merge(n_bruto, out[, .(n_out = .N), by = ano_munic], by = "ano_munic")
print(n_bruto)
chk(all(n_bruto$n_bruto == n_bruto$n_out & n_bruto$n_cod_unicos == n_bruto$n_out), "n de municipios por edicao em out = linhas com codigo no xls bruto (sem inflacao nem perda)")

## ---------------------------------------------------------------- recontagem dos numeros registrados
ass <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, strip.white = TRUE, colClasses = "character")
setnames(ass, c("chave","valor","ep","data","checksum","out"))
ass <- ass[grepl("^munic_", chave)]
ult <- ass[, .SD[.N], by = chave]           # ultimo registro de cada chave (script final do construtor)
rec <- list(
  munic_n_linhas_munic_prefeitos = nrow(out),
  munic_n_prefeituras_bocel = nrow(corr),
  munic_n_prefeituras_com_codigo_ibge = corr[!is.na(id_municipio_ibge), .N],
  munic_n_prefeituras_pareadas_por_nome_uf = corr[origem == "nome_uf", .N],
  munic_n_edicoes_com_nome_prefeito = uniqueN(out[!is.na(nome_prefeito_munic), ano_munic]),
  munic_edicoes_com_nome_prefeito = paste(sort(unique(out[!is.na(nome_prefeito_munic), ano_munic])), collapse = ";"),
  munic_edicoes_com_microdado_e_bloco_prefeito = paste(anos, collapse = ";"),
  munic_edicoes_inventariadas = paste(sort(inv$ano_munic), collapse = ";"),
  munic_edicoes_sem_microdado = paste(inv[microdado_disponivel == FALSE, sort(ano_munic)], collapse = ";"),
  munic_edicoes_sem_bloco_prefeito = paste(inv[microdado_disponivel == TRUE & tem_bloco_prefeito == FALSE, sort(ano_munic)], collapse = ";"))
for (y in anos) {
  o <- out[ano_munic == y]
  rec[[sprintf("munic_%d_n_municipios", y)]] <- nrow(o)
  rec[[sprintf("munic_%d_n_pareados_bocel", y)]] <- o[!is.na(id_mandato_bocel), .N]
  rec[[sprintf("munic_%d_n_eleito_em_exercicio", y)]] <- o[status == "eleito_em_exercicio", .N]
  rec[[sprintf("munic_%d_n_outro_em_exercicio", y)]] <- o[status == "outro_em_exercicio", .N]
  rec[[sprintf("munic_%d_n_indeterminado", y)]] <- o[status == "indeterminado", .N]
  rec[[sprintf("munic_%d_n_substituto_vice", y)]] <- o[substituto_provavel %in% "vice", .N]
  rec[[sprintf("munic_%d_n_substituto_terceiro", y)]] <- o[substituto_provavel %in% "terceiro", .N]
}
# calibracao 2005 recontada a partir do arquivo de saida
cal <- out[ano_munic == 2005L & !is.na(idade) & !is.na(idade_bocel)]
cal_ok <- cal[match_nome_parcial %in% TRUE]
ref_jul <- floor(as.numeric(as.IDate("2005-07-01") - as.IDate(cal_ok$dt_nascimento_bocel)) / 365.25)
rec$munic_calib_2005_n_nome_pareado_com_idade <- nrow(cal_ok)
rec$munic_calib_2005_prop_idade_mais_menos_1_ref_31dez <- round(mean(abs(cal_ok$dif_idade) <= 1), 4)
rec$munic_calib_2005_prop_idade_mais_menos_1_ref_1jul <- round(mean(abs(cal_ok$idade - ref_jul) <= 1), 4)
rec$munic_calib_2005_prop_idade_mais_menos_3_ref_31dez <- round(mean(abs(cal_ok$dif_idade) <= 3), 4)
rec$munic_calib_2005_n_outros_por_nome <- cal[match_nome_parcial %in% FALSE & jw_nome < 0.85, .N]
rec$munic_calib_2005_n_outros_por_nome_que_passariam_por_sexo_idade <- cal[match_nome_parcial %in% FALSE & jw_nome < 0.85 & match_sexo %in% TRUE & match_idade %in% TRUE, .N]
cmp <- data.table(chave = names(rec), recontado = as.character(unlist(rec)))
cmp <- merge(cmp, ult[, .(chave, registrado = valor)], by = "chave", all.x = TRUE)
cmp[, confere := !is.na(registrado) & recontado == registrado]
print(cmp[confere == FALSE])
chk(all(cmp$confere), sprintf("todos os %d numeros munic_* recontados conferem com o ultimo registro em numeros_assinatura.txt", nrow(cmp)))
chk(all(res$n_municipios_munic == out[, .N, by = ano_munic][order(ano_munic), N]), "munic_resumo_substituicoes.csv confere com munic_prefeitos.csv")
# valores reportados pelo construtor no JSON de entrega que divergem do script final (calibracao)
json_calib <- c(munic_calib_2005_n_nome_pareado_com_idade = "5321", munic_calib_2005_n_outros_por_nome = "166",
                munic_calib_2005_n_outros_por_nome_que_passariam_por_sexo_idade = "15")
div <- cmp[chave %in% names(json_calib)][, json := json_calib[chave]][recontado != json]
cat("calibracao: valores do JSON do construtor que divergem da recontagem:\n"); print(div)
reg("munic_verif_n_chaves_recontadas", nrow(cmp))
reg("munic_verif_n_chaves_divergentes_do_ultimo_registro", sum(!cmp$confere))
reg("munic_verif_n_chaves_calib_divergentes_do_json_entregue", nrow(div))

## ---------------------------------------------------------------- homonimos: amostras para inspecao
cat("\n== amostra de 20 pares 'eleito_em_exercicio' por nome (2004/2005) ==\n")
am1 <- out[criterio_status == "nome" & status == "eleito_em_exercicio"][sample(.N, 20)]
print(am1[, .(ano_munic, sg_ue, id_municipio_ibge, nome_prefeito_munic, nome_prefeito_bocel, match_nome, jw_nome, sexo, genero_bocel, idade, idade_bocel)])
cat("\n== amostra de 20 'eleito' por nome NAO exato (parcial/JW) ==\n")
am2 <- out[criterio_status == "nome" & status == "eleito_em_exercicio" & match_nome == FALSE][sample(.N, 20)]
print(am2[, .(ano_munic, nome_prefeito_munic, nome_prefeito_bocel, match_nome_parcial, jw_nome, sexo, genero_bocel, idade, idade_bocel)])
cat("\n== amostra de 20 'outro_em_exercicio' por nome com substituto 'vice' ==\n")
pes <- fread("data/pessoas.csv", na.strings = "NA")[, .(id_pessoa_vice = id_pessoa, nome_vice = nome)]
am3 <- merge(out[criterio_status == "nome" & substituto_provavel %in% "vice"], pes, by = "id_pessoa_vice")[sample(.N, 20)]
print(am3[, .(ano_munic, nome_prefeito_munic, nome_prefeito_bocel, nome_vice, jw_nome)])
cat("\n== amostra de 20 'outro_em_exercicio' por sexo+idade (2009+) ==\n")
am4 <- out[criterio_status == "sexo_idade" & status == "outro_em_exercicio"][sample(.N, 20)]
print(am4[, .(ano_munic, sg_ue, nome_prefeito_bocel, sexo, genero_bocel, idade, idade_bocel, dif_idade, substituto_provavel)])
cat("\n== eleitos por nome com idade fora de +-3 (2005): cauda de erro da idade declarada ==\n")
print(out[ano_munic == 2005 & status == "eleito_em_exercicio" & match_idade_ampla %in% FALSE, .(nome_prefeito_munic, nome_prefeito_bocel, idade, idade_bocel, dif_idade)][1:15])
# distribuicao de dif_idade entre pareados por nome exato (2005): concentracao em 0 e -1 esperada
cat("\n dif_idade entre nome exato 2005:\n"); print(table(out[ano_munic == 2005 & match_nome == TRUE, dif_idade], useNA = "ifany"))
# dominios de partido e sexo por edicao
cat("\n partido por edicao (top):\n"); print(out[, .N, by = .(ano_munic, partido)][order(ano_munic, -N)][, head(.SD, 6), by = ano_munic])
cat("\n exercicio_ano_anterior por edicao:\n"); print(out[, .N, by = .(ano_munic, exercicio_ano_anterior)][order(ano_munic)])

## ---------------------------------------------------------------- validacao externa: eleicoes suplementares do TSE
if (file.exists("data/eleicoes_suplementares.csv")) {
  sup <- fread("data/eleicoes_suplementares.csv", colClasses = "character", na.strings = "NA")
  cat("\ncolunas de eleicoes_suplementares.csv:", paste(names(sup), collapse = ", "), "\n")
  if (all(c("sg_ue", "dt_eleicao_suplementar", "cd_cargo") %in% names(sup))) {
    sup <- sup[cd_cargo == "11"]
    sup[, ano_sup := as.integer(substr(dt_eleicao_suplementar, 1, 4))]
    sup[, sg_ue := formatC(as.integer(sg_ue), width = 5, flag = "0")]
    # suplementar de prefeito entre a eleicao ordinaria de origem e o ano da MUNIC -> espera-se 'outro'
    o2 <- copy(out)[!is.na(id_mandato_bocel)]
    o2[, sup_antes := FALSE]
    for (y in anos) {
      s <- sup[ano_sup > ((y - 1L) %/% 4L) * 4L & ano_sup <= y, unique(sg_ue)]
      o2[ano_munic == y & sg_ue %in% s, sup_antes := TRUE]
    }
    tab <- o2[, .(n = .N, prop_outro = round(mean(status == "outro_em_exercicio"), 3),
                  prop_eleito = round(mean(status == "eleito_em_exercicio"), 3)), by = .(ano_munic, sup_antes)][order(ano_munic, sup_antes)]
    cat("\nvalidacao externa: status x eleicao suplementar de prefeito antes da MUNIC\n"); print(tab)
    reg("munic_verif_prop_outro_com_suplementar_antes", o2[sup_antes == TRUE, round(mean(status == "outro_em_exercicio"), 4)])
    reg("munic_verif_prop_outro_sem_suplementar_antes", o2[sup_antes == FALSE, round(mean(status == "outro_em_exercicio"), 4)])
    reg("munic_verif_n_com_suplementar_antes", o2[sup_antes == TRUE, .N])
  }
}

## ---------------------------------------------------------------- erro da regra sexo+idade, medido em 2005 (onde ha nome)
# aplica a regra usada em 2009+ (sem nome) aos municipios de 2005 e compara com o status decidido pelo nome
c5 <- out[ano_munic == 2005L & criterio_status == "nome" & !is.na(match_sexo) & !is.na(match_idade)]
c5[, status_sexo_idade := fcase(match_sexo & match_idade, "eleito_em_exercicio",
                                !match_sexo | !match_idade_ampla, "outro_em_exercicio", default = "indeterminado")]
cm <- c5[, .N, by = .(status_nome = status, status_sexo_idade)][order(status_nome, status_sexo_idade)]
cat("\nmatriz de confusao 2005: status por nome (linha) x status pela regra sexo+idade (coluna)\n"); print(dcast(cm, status_nome ~ status_sexo_idade, value.var = "N", fill = 0))
vp <- c5[status_sexo_idade == "outro_em_exercicio" & status == "outro_em_exercicio", .N]
fp <- c5[status_sexo_idade == "outro_em_exercicio" & status == "eleito_em_exercicio", .N]
fn <- c5[status_sexo_idade != "outro_em_exercicio" & status == "outro_em_exercicio", .N]
reg("munic_verif_2005_regra_sexo_idade_n_outro_verdadeiro_positivo", vp)
reg("munic_verif_2005_regra_sexo_idade_n_outro_falso_positivo", fp)
reg("munic_verif_2005_regra_sexo_idade_n_outro_falso_negativo", fn)
reg("munic_verif_2005_regra_sexo_idade_precisao_outro", round(vp / (vp + fp), 4))
reg("munic_verif_2005_regra_sexo_idade_sensibilidade_outro", round(vp / (vp + fn), 4))
cat(sprintf("regra sexo+idade em 2005: precisao de 'outro' = %.3f (%d VP, %d FP); sensibilidade = %.3f (%d FN)\n", vp/(vp+fp), vp, fp, vp/(vp+fn), fn))

## ---------------------------------------------------------------- relatorio
cat("\nPASSOU:", length(passou), "| FALHOU:", length(falhas), "\n"); if (length(falhas)) print(falhas)
fora <- c("veracidade do dado declarado a MUNIC", "identidade real do substituto (so provavel)",
          sprintf("em 2009/2013/2017/2021 o status vem so de sexo+idade; medida em 2005 contra o nome, a regra tem precisao %.3f e sensibilidade %.3f para 'outro_em_exercicio' (cerca de metade dos 'outro' dessas edicoes e ruido de idade declarada)", vp/(vp+fp), vp/(vp+fn)),
          "pertinencia semantica do pareamento nome-a-nome (amostras inspecionadas a olho, nao mecanicamente)")
gravar_relatorio_verificacao(alvo = "data/munic_prefeitos.csv", script = script, passou = passou, falhou = falhas, fora_de_cobertura = fora)
cat("verifica_munic: concluido\n"); sink()
if (length(falhas)) quit(status = 1)
