# 11_munic_prefeitos.R — prefeito(a) em exercicio segundo a MUNIC/IBGE pareado ao prefeito eleito do BOCEL
# Entrada:  data_raw/munic/<ano>/ (python/fetch_munic.py), data_raw/munic/inventario_variaveis_munic.csv
#           (R/10_munic_inventario.R), data_raw/munic/diretorio_municipio_bd.csv (Base dos Dados,
#           br_bd_diretorios_brasil.municipio), data/mandatos.csv, data/posicoes_ano.csv, data/pessoas.csv
# Saida:    data/municipios_tse_ibge.csv          correspondencia codigo TSE (sg_ue) x IBGE 7 digitos
#           data/munic_prefeitos.csv              uma linha por (id_municipio_ibge, ano_munic)
#           data/munic_resumo_substituicoes.csv   contagens por ano
#           output/numeros_assinatura.txt (registrar_numero), output/verificacao/relatorio_verificacao_*.json
# Execucao: Rscript --vanilla R/11_munic_prefeitos.R   (a partir da raiz do repositorio)
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringi)
  library(stringdist)
})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
mdir <- file.path(root, "data_raw", "munic")
logf <- file.path(root, "logs", "11_munic_prefeitos.log")
sink(logf, split = TRUE)
cat("11_munic_prefeitos:", format(Sys.time()), "\n")
script <- "R/11_munic_prefeitos.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")

norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}

## ---------------------------------------------------------------- correspondencia TSE x IBGE
dir_bd <- fread(file.path(mdir, "diretorio_municipio_bd.csv"), colClasses = "character", na.strings = c("", "NA"))
dir_bd <- dir_bd[!is.na(id_municipio)]
dir_bd[, id_municipio_tse := ifelse(is.na(id_municipio_tse), NA_character_,
                                    formatC(as.integer(id_municipio_tse), width = 5, flag = "0"))]
mand <- fread("data/mandatos.csv", colClasses = list(character = c("sg_ue", "unidade_posicao")), na.strings = "NA")
pref_bocel <- unique(mand[cd_cargo == 11L, .(sg_ue, sg_uf, nm_ue)])
pref_bocel[, nome_norm := norm_nome(nm_ue)]
dir_bd[, nome_norm := norm_nome(nome)]
corr <- merge(unique(pref_bocel[, .(sg_ue, sg_uf)]),
              dir_bd[!is.na(id_municipio_tse), .(sg_ue = id_municipio_tse, id_municipio_ibge = id_municipio,
                                                  nome_ibge = nome, sigla_uf = sigla_uf)],
              by = "sg_ue", all.x = TRUE)
corr[, origem := ifelse(is.na(id_municipio_ibge), NA_character_, "diretorio_bd")]
# fallback: nome normalizado + UF (municipios que o diretorio nao traz com codigo TSE)
falta <- corr[is.na(id_municipio_ibge), sg_ue]
if (length(falta)) {
  cand <- pref_bocel[sg_ue %in% falta]
  alt <- merge(cand, dir_bd[, .(sg_uf = sigla_uf, nome_norm, id_municipio_ibge = id_municipio, nome_ibge = nome)],
               by = c("sg_uf", "nome_norm"))
  alt <- unique(alt[, .(sg_ue, id_municipio_ibge, nome_ibge)])
  alt <- alt[, .SD[1], by = sg_ue]
  corr[alt, on = "sg_ue", `:=`(id_municipio_ibge = i.id_municipio_ibge, nome_ibge = i.nome_ibge, origem = "nome_uf")]
}
stopifnot(!any(duplicated(corr$sg_ue)))
corr[, id_municipio_ibge6 := substr(id_municipio_ibge, 1, 6)]
corr[, uf_divergente := !is.na(sigla_uf) & sigla_uf != sg_uf]
corr[, sigla_uf := NULL]
setorder(corr, sg_uf, sg_ue)
fwrite(corr, "data/municipios_tse_ibge.csv", na = "NA")
n_corr_ok <- corr[!is.na(id_municipio_ibge), .N]
cat("correspondencia TSE-IBGE:", nrow(corr), "prefeituras;", n_corr_ok, "com codigo IBGE;",
    corr[origem == "nome_uf", .N], "por nome+UF;", corr[uf_divergente == TRUE, .N], "UF divergente\n")
print(corr[is.na(id_municipio_ibge)])
reg("munic_n_prefeituras_bocel", nrow(corr))
reg("munic_n_prefeituras_com_codigo_ibge", n_corr_ok)
reg("munic_n_prefeituras_pareadas_por_nome_uf", corr[origem == "nome_uf", .N])

## ---------------------------------------------------------------- leitura do bloco prefeito
inv <- fread(file.path(mdir, "inventario_variaveis_munic.csv"), na.strings = "NA")
anos_bloco <- inv[microdado_disponivel == TRUE & tem_bloco_prefeito == TRUE, ano_munic]
cat("edicoes com bloco prefeito e microdado:", paste(anos_bloco, collapse = ", "), "\n")

ler_bloco <- function(ano) {
  i <- inv[ano_munic == ano]
  f <- file.path(mdir, i$arquivo)
  d <- as.data.table(suppressMessages(read_excel(f, sheet = i$aba_prefeito, col_types = "text")))
  nm <- names(d)
  pick <- function(v) if (is.na(v)) rep(NA_character_, nrow(d)) else {
    j <- which(toupper(nm) == toupper(v)); stopifnot(length(j) == 1); trimws(d[[j]]) }
  cod <- gsub("\\D", "", d[[1]])
  data.table(
    ano_munic = ano,
    cod_munic_raw = cod,
    nome_prefeito_munic = pick(i$var_nome),
    exercicio_ano_anterior = pick(i$var_exercicio_ano_anterior),
    sexo = pick(i$var_sexo),
    idade = suppressWarnings(as.integer(pick(i$var_idade))),
    escolaridade = pick(i$var_escolaridade),
    partido_eleito_munic = pick(i$var_partido_eleito),
    partido_atual_munic = pick(i$var_partido_atual),
    cor_raca = pick(i$var_cor_raca))
}
mu <- rbindlist(lapply(anos_bloco, ler_bloco))
mu <- mu[!is.na(cod_munic_raw) & nchar(cod_munic_raw) >= 6]
# 2004-2013: codigo IBGE de 6 digitos (sem verificador); 2017+: 7 digitos
mu[, id_municipio_ibge6 := substr(cod_munic_raw, 1, 6)]
mu <- merge(mu, unique(dir_bd[, .(id_municipio_ibge6 = substr(id_municipio, 1, 6), id_municipio_ibge = id_municipio)]),
            by = "id_municipio_ibge6", all.x = TRUE)
cat("MUNIC linhas:", nrow(mu), "| sem codigo IBGE de 7 digitos no diretorio:", mu[is.na(id_municipio_ibge), .N], "\n")
stopifnot(!any(duplicated(mu[, .(id_municipio_ibge, ano_munic)])))
# padronizacao
mu[, sexo := fcase(grepl("^MASC", toupper(sexo)), "MASCULINO", grepl("^FEM", toupper(sexo)), "FEMININO", default = NA_character_)]
mu[, partido := fifelse(!is.na(partido_eleito_munic), partido_eleito_munic, partido_atual_munic)]
mu[partido %in% c("", "-", "Recusa", "Não sabe", "Sem partido"), partido := NA_character_]
mu[, nome_norm_munic := norm_nome(nome_prefeito_munic)]
# marcador de ausencia no campo de nome ("Não disponível", 15 municipios em 2004) nao e nome:
# vira NA, senao e comparado ao eleito e classificado como 'outro_em_exercicio' sem evidencia
# (correcao 28/08/2026, verifica_munic_integracao.R)
marc_ausente <- c("", "NAO DISPONIVEL", "NAO INFORMADO", "NAO SABE", "RECUSA", "NAO RESPONDEU", "SEM INFORMACAO")
n_marc <- mu[nome_norm_munic %in% marc_ausente & !is.na(nome_prefeito_munic), .N]
cat("nomes da MUNIC que sao marcador de ausencia (viram NA):", n_marc, "\n")
mu[nome_norm_munic %in% marc_ausente, `:=`(nome_prefeito_munic = NA_character_, nome_norm_munic = NA_character_)]
reg("munic_n_nomes_marcador_de_ausencia_convertidos_em_na", n_marc)
mu[idade < 18 | idade > 110, idade := NA_integer_]
ref_map <- setNames(inv$data_referencia, inv$ano_munic)
mu[, data_referencia := unname(ref_map[as.character(ano_munic)])]

## ---------------------------------------------------------------- prefeito e vice vigentes no BOCEL
pos <- fread("data/posicoes_ano.csv", colClasses = list(character = "sg_ue"), na.strings = "NA")
pes <- fread("data/pessoas.csv", na.strings = "NA")
pes[, nome_norm := norm_nome(nome)]
pes[, nome_urna_norm := norm_nome(nome_urna_recente)]
vig <- pos[cd_cargo %in% c(11L, 12L) & ano %in% anos_bloco, .(id_mandato, id_pessoa, cd_cargo, sg_ue, ano_eleicao, ano_munic = ano)]
vig <- merge(vig, pes[, .(id_pessoa, nome_bocel = nome, nome_norm_bocel = nome_norm, nome_urna_norm_bocel = nome_urna_norm,
                          dt_nascimento, genero)], by = "id_pessoa", all.x = TRUE)
vig <- merge(vig, corr[, .(sg_ue, id_municipio_ibge)], by = "sg_ue", all.x = TRUE)
stopifnot(!any(duplicated(vig[, .(sg_ue, cd_cargo, ano_munic)])))
pref <- vig[cd_cargo == 11L]
vice <- vig[cd_cargo == 12L, .(id_municipio_ibge, ano_munic, ano_eleicao_vice = ano_eleicao,
                               id_mandato_vice = id_mandato, id_pessoa_vice = id_pessoa,
                               nome_norm_vice = nome_norm_bocel, nome_urna_norm_vice = nome_urna_norm_bocel,
                               dt_nasc_vice = dt_nascimento, genero_vice = genero)]

## ---------------------------------------------------------------- pareamento
x <- merge(mu, pref[, .(id_municipio_ibge, ano_munic, sg_ue, id_mandato_bocel = id_mandato, id_pessoa_bocel = id_pessoa,
                        ano_eleicao_bocel = ano_eleicao, nome_bocel, nome_norm_bocel, nome_urna_norm_bocel, dt_nascimento, genero)],
           by = c("id_municipio_ibge", "ano_munic"), all.x = TRUE)
x <- merge(x, vice, by = c("id_municipio_ibge", "ano_munic"), all.x = TRUE)
x[!is.na(ano_eleicao_vice) & ano_eleicao_vice != ano_eleicao_bocel, c(names(vice)[-(1:2)]) := NA]

# idade compativel: idade completada em 31/12 do ano da pesquisa vs dt_nascimento, tolerancia +-1.
# A referencia em 31/12 foi calibrada na edicao de 2005 (unica com nome, sexo e idade): entre os
# prefeitos pareados pelo nome, 94% ficam a +-1 ano com 31/12 contra 86% com 1/7 (numeros registrados
# abaixo em munic_calib_*). Diferenca de 2 a 3 anos e zona cinzenta (indeterminado); acima, outra pessoa.
idade_em <- function(dt_nasc, ano) {
  d <- as.IDate(dt_nasc); ref <- as.IDate(sprintf("%d-12-31", ano))
  as.integer(floor(as.numeric(ref - d) / 365.25))
}
# nome: igual apos normalizacao; ou parcial (JW >= 0.92 ou todos os tokens de um contidos no outro)
cmp_nome <- function(a, b) {
  ok <- !is.na(a) & !is.na(b)
  exato <- ok & a == b
  jw <- ifelse(ok, 1 - stringdist(a, b, method = "jw", p = 0.1), NA_real_)
  # contencao de tokens: ignora particulas e exige ao menos dois tokens em cada lado
  # (evita que um nome de urna de um token, como "AMORIM", pareie qualquer homonimo parcial)
  part <- c("DE", "DA", "DO", "DAS", "DOS", "E", "DR", "DRA")
  tok <- mapply(function(p, q) { if (is.na(p) || is.na(q)) return(NA)
    tp <- setdiff(strsplit(p, " ")[[1]], part); tq <- setdiff(strsplit(q, " ")[[1]], part)
    if (length(tp) < 2 || length(tq) < 2) return(FALSE)
    all(tp %in% tq) || all(tq %in% tp) }, a, b, USE.NAMES = FALSE)
  list(exato = ifelse(ok, exato, NA), jw = jw, parcial = ifelse(ok, exato | jw >= 0.92 | tok %in% TRUE, NA))
}
cn <- cmp_nome(x$nome_norm_munic, x$nome_norm_bocel)
cu <- cmp_nome(x$nome_norm_munic, x$nome_urna_norm_bocel)
x[, match_nome := cn$exato]
x[, match_nome_parcial := fifelse(is.na(cn$parcial), NA, cn$parcial | cu$parcial %in% TRUE)]
x[, jw_nome := cn$jw]
# genero do TSE fora de {MASCULINO, FEMININO} ("NAO INFORMADO", "NAO DIVULGAVEL") nao compara: vira NA,
# senao a comparacao devolve FALSE e classifica o eleito como 'outro' sem evidencia (corrigido na verificacao)
bin <- c("MASCULINO", "FEMININO")
x[!genero %in% bin, genero := NA_character_]
x[!genero_vice %in% bin, genero_vice := NA_character_]
x[, match_sexo := fifelse(is.na(sexo) | is.na(genero), NA, sexo == genero)]
x[, idade_bocel := idade_em(dt_nascimento, ano_munic)]
x[, dif_idade := idade - idade_bocel]
x[, match_idade := fifelse(is.na(dif_idade), NA, abs(dif_idade) <= 1L)]
x[, match_idade_ampla := fifelse(is.na(dif_idade), NA, abs(dif_idade) <= 3L)]

# status
x[, status := fcase(
  is.na(id_mandato_bocel), "indeterminado",
  # com nome: o nome decide; sexo/idade desempatam a grafia divergente (JW entre 0,85 e 0,92)
  !is.na(match_nome_parcial) & match_nome_parcial == TRUE, "eleito_em_exercicio",
  !is.na(match_nome_parcial) & match_nome_parcial == FALSE & jw_nome >= 0.85 & match_sexo %in% TRUE & match_idade %in% TRUE, "eleito_em_exercicio",
  !is.na(match_nome_parcial) & match_nome_parcial == FALSE & (jw_nome < 0.85 | match_sexo %in% FALSE | match_idade_ampla %in% FALSE), "outro_em_exercicio",
  !is.na(match_nome_parcial) & match_nome_parcial == FALSE, "indeterminado",
  # sem nome: sexo e idade
  match_sexo %in% TRUE & match_idade %in% TRUE, "eleito_em_exercicio",
  match_sexo %in% FALSE | match_idade_ampla %in% FALSE, "outro_em_exercicio",
  default = "indeterminado")]
x[, criterio_status := fcase(
  is.na(id_mandato_bocel), "sem_prefeito_bocel",
  !is.na(match_nome_parcial), "nome",
  !is.na(match_sexo) & !is.na(match_idade), "sexo_idade",
  !is.na(match_sexo) | !is.na(match_idade), "sexo_ou_idade_incompleto",
  default = "sem_atributo")]

# substituto provavel (apenas outro_em_exercicio): compara com o vice eleito da mesma eleicao
cv <- cmp_nome(x$nome_norm_munic, x$nome_norm_vice)
cvu <- cmp_nome(x$nome_norm_munic, x$nome_urna_norm_vice)
x[, vice_nome := fifelse(is.na(cv$parcial), NA, cv$parcial | cvu$parcial %in% TRUE)]
x[, vice_sexo := fifelse(is.na(sexo) | is.na(genero_vice), NA, sexo == genero_vice)]
x[, idade_vice := idade_em(dt_nasc_vice, ano_munic)]
x[, vice_idade := fifelse(is.na(idade) | is.na(idade_vice), NA, abs(idade - idade_vice) <= 1L)]
x[, vice_idade_ampla := fifelse(is.na(idade) | is.na(idade_vice), NA, abs(idade - idade_vice) <= 3L)]
x[, substituto_provavel := NA_character_]
x[status == "outro_em_exercicio", substituto_provavel := fcase(
  is.na(id_pessoa_vice), "indeterminado_sem_vice_no_bocel",
  vice_nome %in% TRUE, "vice",
  !is.na(vice_nome) & vice_nome == FALSE, "terceiro",
  vice_sexo %in% TRUE & vice_idade %in% TRUE, "vice",
  vice_sexo %in% FALSE | vice_idade_ampla %in% FALSE, "terceiro",
  default = "indeterminado")]

## ---------------------------------------------------------------- calibracao (2005: nome + sexo + idade)
cal <- x[ano_munic == 2005L & !is.na(idade) & !is.na(idade_bocel)]
cal_ok <- cal[match_nome_parcial %in% TRUE]
ref_jul <- as.integer(floor(as.numeric(as.IDate("2005-07-01") - as.IDate(cal_ok$dt_nascimento)) / 365.25))
calib <- list(
  munic_calib_2005_n_nome_pareado_com_idade = nrow(cal_ok),
  munic_calib_2005_prop_idade_mais_menos_1_ref_31dez = round(mean(abs(cal_ok$dif_idade) <= 1), 4),
  munic_calib_2005_prop_idade_mais_menos_1_ref_1jul = round(mean(abs(cal_ok$idade - ref_jul) <= 1), 4),
  munic_calib_2005_prop_idade_mais_menos_3_ref_31dez = round(mean(abs(cal_ok$dif_idade) <= 3), 4),
  munic_calib_2005_n_outros_por_nome = cal[match_nome_parcial %in% FALSE & jw_nome < 0.85, .N],
  munic_calib_2005_n_outros_por_nome_que_passariam_por_sexo_idade =
    cal[match_nome_parcial %in% FALSE & jw_nome < 0.85 & match_sexo %in% TRUE & match_idade %in% TRUE, .N])
for (k in names(calib)) reg(k, calib[[k]])
print(calib)
x[, criterio_substituto := fcase(
  status != "outro_em_exercicio", NA_character_,
  !is.na(vice_nome), "nome",
  !is.na(vice_sexo) & !is.na(vice_idade), "sexo_idade",
  default = "insuficiente")]

## ---------------------------------------------------------------- saida
out <- x[, .(id_municipio_ibge, id_municipio_ibge6, ano_munic, data_referencia, sg_ue,
             nome_prefeito_munic, sexo, idade, escolaridade, cor_raca, partido, partido_eleito_munic, partido_atual_munic,
             exercicio_ano_anterior,
             ano_eleicao_bocel, id_mandato_bocel, id_pessoa_bocel, nome_prefeito_bocel = nome_bocel, genero_bocel = genero,
             dt_nascimento_bocel = dt_nascimento, idade_bocel,
             match_nome, match_nome_parcial, jw_nome = round(jw_nome, 3), match_sexo, dif_idade, match_idade, match_idade_ampla,
             status, criterio_status,
             substituto_provavel, criterio_substituto, id_mandato_vice, id_pessoa_vice,
             vice_match_nome = vice_nome, vice_match_sexo = vice_sexo, vice_match_idade = vice_idade)]
out <- merge(out, corr[, .(id_municipio_ibge, sg_ue2 = sg_ue)], by = "id_municipio_ibge", all.x = TRUE)
out[is.na(sg_ue), sg_ue := sg_ue2][, sg_ue2 := NULL]
setorder(out, ano_munic, id_municipio_ibge)
stopifnot(!any(duplicated(out[, .(id_municipio_ibge, ano_munic)])))
stopifnot(all(out$status %in% c("eleito_em_exercicio", "outro_em_exercicio", "indeterminado")))
stopifnot(all(is.na(out$substituto_provavel) == (out$status != "outro_em_exercicio")))
fwrite(out, "data/munic_prefeitos.csv", na = "NA")

res <- out[, .(
  n_municipios_munic = .N,
  n_pareados_bocel = sum(!is.na(id_mandato_bocel)),
  tem_nome = any(!is.na(nome_prefeito_munic)),
  n_com_nome = sum(!is.na(nome_prefeito_munic)),
  n_com_sexo = sum(!is.na(sexo)), n_com_idade = sum(!is.na(idade)), n_com_partido = sum(!is.na(partido)),
  n_eleito_em_exercicio = sum(status == "eleito_em_exercicio"),
  n_outro_em_exercicio = sum(status == "outro_em_exercicio"),
  n_indeterminado = sum(status == "indeterminado"),
  n_subst_vice = sum(substituto_provavel %in% "vice"),
  n_subst_terceiro = sum(substituto_provavel %in% "terceiro"),
  n_subst_indeterminado = sum(status == "outro_em_exercicio" & !substituto_provavel %in% c("vice", "terceiro")),
  n_match_nome_exato = sum(match_nome %in% TRUE), n_match_nome_parcial = sum(match_nome_parcial %in% TRUE),
  n_match_sexo = sum(match_sexo %in% TRUE), n_match_idade = sum(match_idade %in% TRUE)
), by = .(ano_munic, ano_eleicao_bocel = ((ano_munic - 1L) %/% 4L) * 4L)]
res[, prop_outro_entre_pareados := round(n_outro_em_exercicio / n_pareados_bocel, 4)]
res[, prop_vice_entre_outros := round(n_subst_vice / pmax(n_outro_em_exercicio, 1L), 4)]
fwrite(res, "data/munic_resumo_substituicoes.csv", na = "NA")
print(res)

## ---------------------------------------------------------------- registro
anos_nome <- res[tem_nome == TRUE, ano_munic]
reg("munic_edicoes_inventariadas", paste(inv$ano_munic, collapse = ";"))
reg("munic_edicoes_com_microdado_e_bloco_prefeito", paste(anos_bloco, collapse = ";"))
reg("munic_edicoes_com_nome_prefeito", paste(anos_nome, collapse = ";"))
reg("munic_n_edicoes_com_nome_prefeito", length(anos_nome))
reg("munic_edicoes_sem_microdado", paste(inv[microdado_disponivel == FALSE, ano_munic], collapse = ";"))
reg("munic_edicoes_sem_bloco_prefeito", paste(inv[microdado_disponivel == TRUE & tem_bloco_prefeito == FALSE, ano_munic], collapse = ";"))
reg("munic_n_linhas_munic_prefeitos", nrow(out))
for (i in seq_len(nrow(res))) {
  y <- res$ano_munic[i]
  reg(sprintf("munic_%d_n_municipios", y), res$n_municipios_munic[i])
  reg(sprintf("munic_%d_n_pareados_bocel", y), res$n_pareados_bocel[i])
  reg(sprintf("munic_%d_n_eleito_em_exercicio", y), res$n_eleito_em_exercicio[i])
  reg(sprintf("munic_%d_n_outro_em_exercicio", y), res$n_outro_em_exercicio[i])
  reg(sprintf("munic_%d_n_indeterminado", y), res$n_indeterminado[i])
  reg(sprintf("munic_%d_n_substituto_vice", y), res$n_subst_vice[i])
  reg(sprintf("munic_%d_n_substituto_terceiro", y), res$n_subst_terceiro[i])
}
passou <- c("chave (id_municipio_ibge, ano_munic) unica em munic_prefeitos.csv",
            "status em {eleito_em_exercicio, outro_em_exercicio, indeterminado}",
            "substituto_provavel preenchido sse status == outro_em_exercicio",
            sprintf("correspondencia TSE-IBGE unica por sg_ue (%d prefeituras, %d com codigo IBGE)", nrow(corr), n_corr_ok),
            "toda edicao com bloco prefeito e microdado foi lida (2004, 2005, 2009, 2013, 2017, 2021)")
fora <- c("veracidade do nome/sexo/idade declarados a MUNIC (dado administrativo do IBGE)",
          "data exata da entrevista por municipio (a base nao a traz; idade calculada em 31 de dezembro do ano da pesquisa)",
          "status 'outro_em_exercicio' inferido por sexo+idade em 2009/2013/2017/2021 e nao por nome; 'vice' por sexo+idade e apenas provavel",
          "1999 (sem base no FTP), 2001 e 2002 (so tabelas agregadas) ficam sem pareamento")
gravar_relatorio_verificacao(alvo = "data/munic_prefeitos.csv", script = script, passou = passou, fora_de_cobertura = fora)
cat("\n11_munic_prefeitos: concluido.\n")
sink()
