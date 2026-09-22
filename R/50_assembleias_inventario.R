# 50_assembleias_inventario.R — exercicio de deputados estaduais a partir do que a sondagem das
# Assembleias ja deixou em disco (data_raw/inventario_assembleias/*_amostras), sem nenhuma
# requisicao nova a servidor. A frente ataca as dez UFs com menos de 20% de cobertura de forma de
# saida no estadual (RN, DF, AP, SC, MT, MS, GO, PA, ES, TO).
#
# O que entra, por UF e por fonte (tudo lido de arquivo local):
#   RN  alrn_api_mandato          api-transparencia: mandato por legislatura com vigencia_inicio,
#                                 vigencia_fim e exclusao_data (legislaturas 61, 62, 63)
#   RN  alrn_folha_eletivo        folha de pagamento (codUnidade 101), vinculo ELETIVO, com CPF,
#                                 DataAdmissao e DataExoneracao (marco/2012 e marco/2024)
#   MT  almt_folha_servidores     eloweb: servidores por exercicio, cargo DEPUTADO ESTADUAL, classe
#                                 PARLAMENTAR ou SUPLENTE, com dataAdmissao e dataDemissao
#   GO  alego_folha_remuneracao   transparencia: folha de jun/2015, vinculo DEPUTADOS, com admissao
#                                 e desligamento do vinculo
#   ES  ales_vinculos_deputados   vinculos do setor DEPUTADOS ESTADUAIS com DataAdmissao,
#                                 DataDemissao e CPF
#   ES  ales_folha_base           bases de remuneracao 2012 e 2019, cargo DEPUTADO ESTADUAL (relacao
#                                 nominal mes a mes)
#   DF  cldf_quadro_pessoal       quadro demonstrativo de pessoal de jun/2018, Tipo = DEPUTADO
#   DF  cldf_verba_indenizatoria  verbas indenizatorias de 2015, NOME_PARLAMENTAR com CPF
#   AP  alap_relacao_deputados    'Relacao Deputados' do portal da transparencia (abril de varios
#                                 anos), nome civil e cargo
#   PA  alepa_frequencia          'Frequencia dos Deputados' (abril de varios anos), nome, partido,
#                                 e contagem de presenca/licenca/ausencia no mes
#   MS  alems_ceap                CEAP por mes (jun/2012 e jun/2015): relacao nominal dos deputados
#   SC  alesc_transparencia_deputados  subsidios dos deputados por mes de referencia
#
# Regra de leitura das datas (declarada, nao escondida):
#   - vinculo de folha com [admissao, demissao] e um intervalo de exercicio afirmado pela fonte;
#     a legislatura entra na tabela quando (a) o intervalo a intersecta ou (b) a pessoa aparece numa
#     relacao nominal/exercicio daquela legislatura;
#   - forma_saida = fim_regular quando a saida cai no fim da legislatura (ate 31 dias antes) ou
#     quando o vinculo cobre a legislatura inteira e segue adiante;
#   - forma_saida = outro quando ha saida datada dentro da legislatura, antes do fim: a fonte
#     registra o fim do exercicio mas nao a causa;
#   - forma_saida = nao_observado no resto, inclusive em legislatura ainda em curso;
#   - suplente convocado nao encerra o mandato do titular: forma_saida fica NA.
# Entrada:  data_raw/inventario_assembleias/*_amostras/*, data/mandatos.csv, data/pessoas.csv,
#           data/exercicio_assembleias{,_2,_historico}.csv (so para medir o ganho marginal)
# Saida:    data/exercicio_assembleias_inventario.csv
# Execucao: cd ~/bocel && Rscript --vanilla R/50_assembleias_inventario.R
set.seed(20260830)
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/50_assembleias_inventario.R"
dir.create("logs", showWarnings = FALSE); dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
logf <- file("logs/50_assembleias_inventario.log", open = "wt"); sink(logf, split = TRUE)

RAW <- "data_raw/inventario_assembleias"
CO <- file.path(RAW, "centro-oeste_amostras"); NE <- file.path(RAW, "nordeste_amostras")
NO <- file.path(RAW, "norte_amostras");        SS <- file.path(RAW, "sul-sudeste_amostras")
HOJE <- as.Date("2026-08-30")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")

norm <- function(x) { x <- stri_trans_general(toupper(fcoalesce(as.character(x), "")), "Latin-ASCII"); x <- gsub("[^A-Z ]", " ", x); gsub(" +", " ", trimws(x)) }
limpa <- function(x) trimws(gsub("\\s+", " ", gsub("&nbsp;| ", " ", x)))
d_br  <- function(x) { m <- stri_match_first_regex(fcoalesce(as.character(x), ""), "(\\d{2})/(\\d{2})/(\\d{4})"); ifelse(is.na(m[, 1]), NA_character_, sprintf("%s-%s-%s", m[, 4], m[, 3], m[, 2])) }
d_iso <- function(x) { x <- fcoalesce(as.character(x), ""); y <- stri_extract_first_regex(x, "\\d{4}-\\d{2}-\\d{2}"); ifelse(is.na(y), NA_character_, y) }
so_cpf <- function(x) { x <- gsub("\\D", "", fcoalesce(as.character(x), "")); ifelse(nchar(x) == 11, x, NA_character_) }
le_txt <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# legislatura estadual: 1 de fevereiro do ano seguinte a eleicao ate 31 de janeiro quatro anos depois
leg_ini <- function(ano) as.Date(sprintf("%d-02-01", as.integer(ano) + 1L))
leg_fim <- function(ano) as.Date(sprintf("%d-01-31", as.integer(ano) + 5L))
# ano de eleicao que governa um mes observado (AAAA-MM) e um exercicio de folha (AAAA)
ano_do_mes <- function(ym) { a <- as.integer(substr(ym, 1, 4)); m <- as.integer(substr(ym, 6, 7))
  base <- ifelse(m == 1L, a - 1L, a); base - ((base - 1999L) %% 4L) - 1L }
ELEICOES <- seq(1998L, 2022L, by = 4L)

## ============================================================ parsers (um por fonte)
# formato comum: uf, fonte, url, nome, nome_completo, partido, condicao, cpf, dt_ini, dt_fim,
#                meses (string 'AAAA-MM;AAAA-MM'), exercicios (string 'AAAA;AAAA'), causa_original
reg <- function(...) data.table(...)

## --- RN: mandatos da API de transparencia (legislaturas 61, 62, 63)
parse_rn_mandato <- function() {
  f <- file.path(NE, "rn_api_mandato_bare.json"); if (!file.exists(f)) return(NULL)
  registrar_fonte(f, "ALRN api-transparencia /mandato/", "https://api-transparencia.al.rn.leg.br/mandato/")
  d <- as.data.table(fromJSON(f)$data)
  d[, `:=`(dt_ini = d_iso(vigencia_inicio), dt_fim = d_iso(vigencia_fim),
           l_ini = d_iso(legislatura_vigencia_inicio), l_fim = d_iso(legislatura_vigencia_fim))]
  d[, ano := as.integer(substr(l_ini, 1, 4)) - 1L]
  d[, cond := fifelse(dt_ini > l_ini, "suplente", "titular")]
  # quando vigencia_fim repete a data de fim da legislatura, o registro nao esta observando saida
  # nenhuma: e a data da legislatura copiada. ALVARO DIAS (61a legislatura) aparece com vigencia ate
  # 2019-01-31 embora tenha deixado a cadeira para a prefeitura de Natal - o campo nao serve de
  # testemunha de fim regular, so de que a pessoa teve o mandato naquela legislatura.
  d[, dt_fim_obs := fifelse(dt_fim == l_fim, NA_character_, dt_fim)]
  reg(uf = "RN", fonte = "alrn_api_mandato", url = "https://api-transparencia.al.rn.leg.br/mandato/",
      nome = limpa(d$nomeParlamentar), nome_completo = NA_character_, partido = NA_character_,
      condicao = d$cond, cpf = NA_character_, dt_ini = d$dt_ini, dt_fim = d$dt_fim_obs,
      meses = "", exercicios = "", legislatura = as.character(d$legislatura_numero),
      ano_forcado = d$ano,
      causa_original = sprintf("mandato %s a %s na %sa legislatura (%s a %s)%s", d$dt_ini, d$dt_fim,
                               d$legislatura_numero, d$l_ini, d$l_fim,
                               fifelse(is.na(d$exclusao_data), "", paste0("; exclusao_data ", d_iso(d$exclusao_data)))))
}

## --- RN: folha de pagamento, vinculo ELETIVO (marco/2012 e marco/2024)
parse_rn_folha <- function() {
  fs <- list.files(NE, pattern = "^rn_folha_\\d{4}_\\d{2}_u101\\.json$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    ym <- sub("^rn_folha_(\\d{4})_(\\d{2})_u101\\.json$", "\\1-\\2", basename(f))
    j <- fromJSON(f); x <- if (is.list(j) && !is.null(j$data)) as.data.table(j$data) else NULL
    if (is.null(x) || !nrow(x) || !"Vinculo" %in% names(x)) return(NULL)
    x <- x[trimws(Vinculo) == "ELETIVO"]; if (!nrow(x)) return(NULL)
    registrar_fonte(f, "ALRN folhaPagamento codUnidade 101", "https://api-transparencia.al.rn.leg.br/folhaPagamento/")
    reg(uf = "RN", fonte = "alrn_folha_eletivo",
        url = sprintf("https://api-transparencia.al.rn.leg.br/folhaPagamento/?codUnidade=101&Ano=%s&Mes=%d", substr(ym, 1, 4), as.integer(substr(ym, 6, 7))),
        nome = limpa(x$Nome), nome_completo = limpa(x$Nome), partido = NA_character_,
        condicao = "em_exercicio", cpf = so_cpf(x$CPF), dt_ini = d_br(x$DataAdmissao), dt_fim = d_br(x$DataExoneracao),
        meses = ym, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("folha ELETIVO de %s; admissao %s%s", ym, fcoalesce(d_br(x$DataAdmissao), "nao informada"),
                                 fifelse(is.na(d_br(x$DataExoneracao)), "", paste0("; exoneracao ", d_br(x$DataExoneracao)))))
  }), fill = TRUE)
}

## --- MT: servidores por exercicio (eloweb), cargo DEPUTADO ESTADUAL
parse_mt_folha <- function() {
  fs <- list.files(CO, pattern = "^MT_(serv|servidores|deputados)_.*\\.json$", full.names = TRUE)
  out <- rbindlist(lapply(fs, function(f) {
    j <- try(fromJSON(f, simplifyDataFrame = TRUE), silent = TRUE); if (inherits(j, "try-error")) return(NULL)
    x <- if (is.data.frame(j)) as.data.table(j) else if (!is.null(j$content)) as.data.table(j$content) else NULL
    if (is.null(x) || !nrow(x) || !"codigoCargo" %in% names(x)) return(NULL)
    x <- x[codigoCargo == 500L]; if (!nrow(x)) return(NULL)
    anos <- unique(stri_extract_all_regex(basename(f), "(19|20)\\d{2}")[[1]])
    ex <- if (length(anos) == 1L) anos else ""      # 'uniao_1999_2023' nao fixa exercicio observado
    registrar_fonte(f, "ALMT eloweb portaltransparencia-api /servidores", "https://almt.eloweb.net/portaltransparencia-api/api/servidores")
    reg(uf = "MT", fonte = "almt_folha_servidores",
        url = "https://almt.eloweb.net/portaltransparencia-api/api/servidores?entidade=1",
        nome = limpa(x$nome), nome_completo = limpa(x$nome), partido = NA_character_,
        condicao = fifelse(grepl("SUPLENTE", fcoalesce(x$descricaoClasse, ""), fixed = TRUE), "suplente", "titular"),
        cpf = NA_character_, dt_ini = d_iso(x$dataAdmissao),
        dt_fim = if ("dataDemissao" %in% names(x)) d_iso(x$dataDemissao) else NA_character_,
        meses = "", exercicios = ex, legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("folha ALMT classe %s%s; admissao %s%s", fcoalesce(x$descricaoClasse, "NA"),
                                 fifelse(ex == "", "", paste0(", exercicio ", ex)), fcoalesce(d_iso(x$dataAdmissao), "nao informada"),
                                 if ("dataDemissao" %in% names(x)) fifelse(is.na(d_iso(x$dataDemissao)), "", paste0("; demissao ", d_iso(x$dataDemissao))) else ""))
  }), fill = TRUE)
  if (!nrow(out)) return(NULL)
  # o mesmo vinculo aparece em varios exercicios: junta os exercicios observados e guarda a demissao mais tardia
  out[, k := paste(norm(nome), fcoalesce(dt_ini, ""), condicao)]
  out[, exercicios := paste(sort(unique(exercicios[exercicios != ""])), collapse = ";"), by = k]
  out[, dt_fim := if (all(is.na(dt_fim))) NA_character_ else max(dt_fim, na.rm = TRUE), by = k]
  out[, causa_original := sprintf("folha ALMT (%s), exercicios observados %s; admissao %s%s",
                                  condicao, fifelse(exercicios == "", "nenhum", exercicios),
                                  fcoalesce(dt_ini, "nao informada"),
                                  fifelse(is.na(dt_fim), "", paste0("; demissao ", dt_fim)))]
  out[!duplicated(k)][, k := NULL]
}

## --- GO: folha de jun/2015, vinculo DEPUTADOS (traz admissao e desligamento do vinculo)
parse_go_folha <- function() {
  f <- file.path(CO, "GO_folha_2015_06_deputados.json"); if (!file.exists(f)) return(NULL)
  registrar_fonte(f, "ALEGO transparencia /api/transparencia/remuneracoes", "https://transparencia.al.go.leg.br/api/transparencia/remuneracoes")
  x <- as.data.table(fromJSON(f))
  part <- limpa(sub("^DEP\\.?\\s*EST\\.?", "", fcoalesce(x$cargo, "")))
  reg(uf = "GO", fonte = "alego_folha_remuneracao",
      url = "https://transparencia.al.go.leg.br/api/transparencia/remuneracoes?ano=2015&mes=6",
      nome = limpa(x$nome), nome_completo = limpa(x$nome), partido = fifelse(part == "", NA_character_, part),
      condicao = fifelse(grepl("SUPLEMENTAR", fcoalesce(x$vinculo, ""), fixed = TRUE), "suplente", "em_exercicio"),
      cpf = NA_character_, dt_ini = d_br(x$admissao), dt_fim = d_br(x$desligamento),
      meses = "2015-06", exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
      causa_original = sprintf("folha ALEGO jun/2015, vinculo %s, cargo '%s'; admissao %s%s", fcoalesce(x$vinculo, "NA"),
                               limpa(fcoalesce(x$cargo, "")), fcoalesce(d_br(x$admissao), "nao informada"),
                               fifelse(is.na(d_br(x$desligamento)), "", paste0("; desligamento ", d_br(x$desligamento)))))
}

## --- ES: vinculos do setor DEPUTADOS ESTADUAIS (admissao, demissao, CPF)
parse_es_vinculos <- function() {
  f <- file.path(SS, "ES_amostra_vinculos_deputados_2012.json"); if (!file.exists(f)) return(NULL)
  registrar_fonte(f, "ALES transparencia: vinculos do setor DEPUTADOS ESTADUAIS", "https://www.al.es.gov.br/Transparencia")
  x <- as.data.table(fromJSON(f))
  reg(uf = "ES", fonte = "ales_vinculos_deputados", url = "https://www.al.es.gov.br/Transparencia",
      nome = limpa(x$Nome), nome_completo = limpa(x$Nome), partido = NA_character_, condicao = "em_exercicio",
      cpf = so_cpf(x$Documento), dt_ini = d_br(x$DataAdmissao), dt_fim = d_br(x$DataDemissao),
      meses = "", exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
      causa_original = sprintf("vinculo ALES %s, cargo %s; admissao %s%s", fcoalesce(x$Vinculo, ""), fcoalesce(x$Cargo, ""),
                               fcoalesce(d_br(x$DataAdmissao), "nao informada"),
                               fifelse(is.na(d_br(x$DataDemissao)), "", paste0("; demissao ", d_br(x$DataDemissao)))))
}

## --- ES: bases de remuneracao 2012 e 2019, cargo DEPUTADO ESTADUAL (relacao nominal mes a mes)
parse_es_base <- function() {
  fs <- list.files(SS, pattern = "^ES_base_\\d{4}\\.csv$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    registrar_fonte(f, "ALES base de remuneracao (CSV do portal da transparencia)", "https://www.al.es.gov.br/Transparencia")
    x <- fread(f, sep = ";", encoding = "Latin-1", colClasses = "character", showProgress = FALSE)
    x <- x[toupper(trimws(cargo)) == "DEPUTADO ESTADUAL"]; if (!nrow(x)) return(NULL)
    x[, ym := sprintf("%s-%s", substr(mes_ano, 4, 7), substr(mes_ano, 1, 2))]
    y <- x[, .(meses = paste(sort(unique(ym)), collapse = ";")), by = .(nome = limpa(nome_do_servidor))]
    reg(uf = "ES", fonte = "ales_folha_base", url = "https://www.al.es.gov.br/Transparencia",
        nome = y$nome, nome_completo = y$nome, partido = NA_character_, condicao = "em_exercicio",
        cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
        meses = y$meses, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = paste0("cargo DEPUTADO ESTADUAL na base de remuneracao; meses observados ", y$meses))
  }), fill = TRUE)
}

## --- DF: quadro demonstrativo de pessoal de jun/2018, Tipo = DEPUTADO
parse_df_qdp <- function() {
  f <- file.path(CO, "DF_qdp_2018_06.csv"); if (!file.exists(f)) return(NULL)
  registrar_fonte(f, "CLDF dados abertos: quadro demonstrativo de pessoal 2018-06", "https://dados.cl.df.gov.br/dataset/quadro-demonstrativo-de-pessoal-mensal")
  x <- fread(f, colClasses = "character", showProgress = FALSE)
  x <- x[trimws(Tipo) == "DEPUTADO"]; if (!nrow(x)) return(NULL)
  reg(uf = "DF", fonte = "cldf_quadro_pessoal", url = "https://dados.cl.df.gov.br/dataset/quadro-demonstrativo-de-pessoal-mensal",
      nome = limpa(x$Nome), nome_completo = limpa(x$Nome), partido = NA_character_, condicao = "em_exercicio",
      cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
      meses = "2018-06", exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
      causa_original = sprintf("quadro de pessoal de jun/2018, Tipo DEPUTADO, lotacao '%s'", limpa(fcoalesce(x[["Lotação"]], ""))))
}

## --- DF: verbas indenizatorias de 2015 (NOME_PARLAMENTAR com CPF)
parse_df_verba <- function() {
  f <- file.path(CO, "DF_verba_2015.csv"); if (!file.exists(f)) return(NULL)
  registrar_fonte(f, "CLDF dados abertos: verbas indenizatorias 2015", "https://dados.cl.df.gov.br/dataset/verbas-indenizatorias")
  x <- fread(f, sep = ";", colClasses = "character", showProgress = FALSE)
  x[, nm := limpa(sub("^Deputad[oa]\\s+", "", NOME_PARLAMENTAR))]
  x[, ym := substr(DATA_COMPROVANTE, 1, 7)]
  y <- x[nm != "", .(meses = paste(sort(unique(ym[grepl("^\\d{4}-\\d{2}$", ym)])), collapse = ";"),
                     cpf = so_cpf(sprintf("%011.0f", suppressWarnings(as.numeric(CPF_PARLAMENTAR[1])))) ), by = nm]
  reg(uf = "DF", fonte = "cldf_verba_indenizatoria", url = "https://dados.cl.df.gov.br/dataset/verbas-indenizatorias",
      nome = y$nm, nome_completo = NA_character_, partido = NA_character_, condicao = "em_exercicio",
      cpf = y$cpf, dt_ini = NA_character_, dt_fim = NA_character_,
      meses = y$meses, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
      causa_original = paste0("prestacao de contas da verba indenizatoria em 2015; meses com comprovante ", y$meses))
}

## --- AP: 'Relacao Deputados' do portal da transparencia (abril de varios anos), nome civil
parse_ap_relacao <- function() {
  fs <- list.files(NO, pattern = "^AP_dep_remun_\\d{4}_\\d{2}(_p2)?\\.html$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    ym <- sub("^AP_dep_remun_(\\d{4})_(\\d{2}).*$", "\\1-\\2", basename(f))
    s <- le_txt(f)
    m <- stri_match_all_regex(s, "nomeB=([^&]+)&secao=DEPUTADOS ESTADUAIS&funcao=([^&]*?)\\s*-&")[[1]]
    if (is.null(m) || nrow(m) == 0L || is.na(m[1, 1])) return(NULL)
    registrar_fonte(f, "ALAP transparencia: relacao de deputados", "https://www.al.ap.leg.br/transparencia/index.php?pg=remuneracao_consulta")
    d <- data.table(nome = limpa(m[, 2]), cargo = limpa(m[, 3]))[!duplicated(nome)]
    reg(uf = "AP", fonte = "alap_relacao_deputados",
        url = sprintf("https://www.al.ap.leg.br/transparencia/index.php?pg=remuneracao_consulta&anoB=%s&mesB=%s", substr(ym, 1, 4), substr(ym, 6, 7)),
        nome = d$nome, nome_completo = d$nome, partido = NA_character_, condicao = "em_exercicio",
        cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
        meses = ym, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("relacao de deputados de %s, cargo '%s'", ym, d$cargo))
  }), fill = TRUE)
}

## --- PA: 'Frequencia dos Deputados' (abril de varios anos): nome e contagem do mes
# so a familia PA_freq_*: nas capturas PA_frequencia_* o parametro de ano foi ignorado pelo portal e
# as cinco paginas trazem a mesma bancada de 39 nomes de 2026 (conferido nos arquivos, 30/ago/2026).
# o partido do cartao e o do perfil atual, nao o do ano consultado, e por isso nao entra.
parse_pa_freq <- function() {
  fs <- list.files(NO, pattern = "^PA_freq_\\d{4}_\\d{2}\\.html$", full.names = TRUE)
  out <- rbindlist(lapply(fs, function(f) {
    ym <- sub("^PA_freq_(\\d{4})_(\\d{2})\\.html$", "\\1-\\2", basename(f))
    s <- le_txt(f)
    m <- stri_match_all_regex(s, "/Institucional/Deputado/(\\d+)'[^>]*>\\s*<h3><b>(.*?)</b></h3>\\s*</a>\\s*<p><b>Presente:\\s*</b>(\\d+)</p><p><b>Licenciado:\\s*</b>(\\d+)</p><p><b>Ausente:\\s*</b>(\\d+)</p>")[[1]]
    if (is.null(m) || nrow(m) == 0L || is.na(m[1, 1])) return(NULL)
    registrar_fonte(f, "ALEPA transparencia: frequencia dos deputados", "https://www.alepa.pa.gov.br/Transparencia/Page/Frequencia")
    rot <- limpa(m[, 3])
    part <- stri_match_first_regex(rot, "\\(([^)]+)\\)\\s*$")[, 2]
    nm <- limpa(sub("\\s*\\([^)]*\\)\\s*$", "", rot))
    d <- data.table(id = m[, 2], nome = nm, partido = part, pres = m[, 4], lic = m[, 5], aus = m[, 6])[!duplicated(nome)]
    reg(uf = "PA", fonte = "alepa_frequencia", url = "https://www.alepa.pa.gov.br/Transparencia/Page/Frequencia",
        nome = d$nome, nome_completo = NA_character_, partido = NA_character_, condicao = "em_exercicio",
        cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
        meses = ym, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("frequencia de %s: presente %s, licenciado %s, ausente %s", ym, d$pres, d$lic, d$aus))
  }), fill = TRUE)
  if (!nrow(out)) return(NULL)
  out[, k := paste(norm(nome), meses)][!duplicated(k)][, k := NULL]
}

## --- MS: CEAP por mes (jun/2012, jun/2015): relacao nominal dos deputados
parse_ms_ceap <- function() {
  fs <- list.files(CO, pattern = "^MS_ceap_\\d{4}_\\d{2}\\.json$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    ym <- sub("^MS_ceap_(\\d{4})_(\\d{2})\\.json$", "\\1-\\2", basename(f))
    j <- fromJSON(f); x <- as.data.table(j$rows); if (!nrow(x)) return(NULL)
    registrar_fonte(f, "ALEMS transparencia2: CEAP por mes", "https://transparencia2.al.ms.gov.br/ceap")
    nm <- limpa(sub("^Dep\\.?\\s*", "", x$deputado))
    reg(uf = "MS", fonte = "alems_ceap",
        url = sprintf("https://transparencia2.al.ms.gov.br/ceap/data?ano=%s&mes=%d", substr(ym, 1, 4), as.integer(substr(ym, 6, 7))),
        nome = nm, nome_completo = NA_character_, partido = NA_character_, condicao = "em_exercicio",
        cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
        meses = ym, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("CEAP de %s com despesa do parlamentar (total %s)", ym, fcoalesce(x$total, "")))
  }), fill = TRUE)
}

## --- SC: subsidios dos deputados por mes de referencia
parse_sc_transp <- function() {
  fs <- list.files(SS, pattern = "^SC_dep_\\d{4}_\\d{2}\\.html$", full.names = TRUE)
  rbindlist(lapply(fs, function(f) {
    ym <- sub("^SC_dep_(\\d{4})_(\\d{2})\\.html$", "\\1-\\2", basename(f))
    s <- le_txt(f)
    m <- stri_match_all_regex(s, "<td class=\"fw-semibold\">([^<]+)</td>\\s*<td class=\"text-center\">(\\d{2}/\\d{4})</td>")[[1]]
    if (is.null(m) || nrow(m) == 0L || is.na(m[1, 1])) return(NULL)
    registrar_fonte(f, "ALESC transparencia: subsidios dos deputados", "https://transparencia.alesc.sc.gov.br/deputados")
    ref <- sprintf("%s-%s", substr(m[, 3], 4, 7), substr(m[, 3], 1, 2))
    d <- data.table(nome = limpa(m[, 2]), ref = ref)[!duplicated(nome)]
    reg(uf = "SC", fonte = "alesc_transparencia_deputados",
        url = sprintf("https://transparencia.alesc.sc.gov.br/deputados?ano=%s&mes=%d", substr(ym, 1, 4), as.integer(substr(ym, 6, 7))),
        nome = d$nome, nome_completo = NA_character_, partido = NA_character_, condicao = "em_exercicio",
        cpf = NA_character_, dt_ini = NA_character_, dt_fim = NA_character_,
        meses = d$ref, exercicios = "", legislatura = NA_character_, ano_forcado = NA_integer_,
        causa_original = sprintf("relacao de subsidios do mes de referencia %s", d$ref))
  }), fill = TRUE)
}

## ============================================================ uniao das fontes
fontes <- rbindlist(list(parse_rn_mandato(), parse_rn_folha(), parse_mt_folha(), parse_go_folha(),
                         parse_es_vinculos(), parse_es_base(), parse_df_qdp(), parse_df_verba(),
                         parse_ap_relacao(), parse_pa_freq(), parse_ms_ceap(), parse_sc_transp()), fill = TRUE)
stopifnot(nrow(fontes) > 0)
cat("registros brutos por fonte:\n"); print(fontes[, .N, by = .(uf, fonte)][order(uf, fonte)])

## ------------------------------------------------ expansao para (registro x legislatura)
fontes[, rid := .I]
cands <- rbindlist(lapply(seq_len(nrow(fontes)), function(i) {
  r <- fontes[i]
  anos <- integer(0)
  if (!is.na(r$ano_forcado)) anos <- c(anos, r$ano_forcado)
  if (nzchar(r$meses))      anos <- c(anos, ano_do_mes(strsplit(r$meses, ";")[[1]]))
  if (nzchar(r$exercicios)) { y <- as.integer(strsplit(r$exercicios, ";")[[1]]); anos <- c(anos, y - ((y - 1999L) %% 4L) - 1L) }
  if (!is.na(r$dt_ini) && !is.na(r$dt_fim)) {
    a <- as.Date(r$dt_ini); b <- as.Date(r$dt_fim)
    anos <- c(anos, ELEICOES[leg_ini(ELEICOES) <= b & leg_fim(ELEICOES) >= a])
  }
  anos <- sort(unique(anos[anos %in% ELEICOES]))
  # com intervalo de vinculo declarado, so vale a legislatura que o intervalo alcanca
  if (!is.na(r$dt_ini) && !is.na(r$dt_fim)) {
    a <- as.Date(r$dt_ini); b <- as.Date(r$dt_fim)
    anos <- anos[leg_ini(anos) <= b & leg_fim(anos) >= a]
  } else if (!is.na(r$dt_ini)) {
    anos <- anos[leg_fim(anos) >= as.Date(r$dt_ini)]
  }
  if (!length(anos)) return(NULL)
  data.table(rid = r$rid, ano_eleicao = anos)
}), fill = TRUE)
d <- merge(fontes, cands, by = "rid")
d[, `:=`(li = leg_ini(ano_eleicao), lf = leg_fim(ano_eleicao))]
d[, ini_leg := fifelse(!is.na(dt_ini) & as.Date(fcoalesce(dt_ini, "1900-01-01")) >= li & as.Date(fcoalesce(dt_ini, "1900-01-01")) <= lf, dt_ini, NA_character_)]
d[, fim_leg := fifelse(!is.na(dt_fim) & as.Date(fcoalesce(dt_fim, "1900-01-01")) >= li & as.Date(fcoalesce(dt_fim, "1900-01-01")) <= lf, dt_fim, NA_character_)]
d[, cobre := !is.na(dt_ini) & !is.na(dt_fim) & as.Date(fcoalesce(dt_ini, "2999-01-01")) <= li & as.Date(fcoalesce(dt_fim, "1900-01-01")) >= lf]
d[, em_curso := lf > HOJE]

## ------------------------------------------------ forma de saida (regra declarada no cabecalho)
d[, forma_saida := fcase(
  !is.na(fim_leg) & as.Date(fim_leg) >= lf - 31L, "fim_regular",
  !is.na(fim_leg),                                "outro",
  cobre == TRUE,                                  "fim_regular",
  default = "nao_observado")]
d[em_curso == TRUE & forma_saida == "fim_regular", forma_saida := "nao_observado"]
d[condicao == "suplente", forma_saida := NA_character_]      # suplente convocado nao encerra o mandato do titular
stopifnot(all(is.na(d$forma_saida) | d$forma_saida %in% VOCAB))

## ------------------------------------------------ legislatura (so onde a numeracao esta evidenciada)
# RN traz o numero na propria fonte; MT, SC e GO usam a numeracao continua de 4 anos ja fixada no repo
d[is.na(legislatura) & uf %chin% c("MT", "SC", "GO"), legislatura := as.character((ano_eleicao - 1942L) %/% 4L)]

d[, nome_normalizado := norm(nome)]
d[, nome_completo_norm := norm(nome_completo)]
d[, cargo := fifelse(uf == "DF", "DEPUTADO DISTRITAL", "DEPUTADO ESTADUAL")]
cat("linhas (registro x legislatura) por UF e eleicao:\n")
print(dcast(d[, .N, by = .(uf, ano_eleicao)], uf ~ ano_eleicao, value.var = "N", fill = 0L))

## ============================================================ pareamento com o BOCEL
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")[cd_cargo %chin% c("7", "8") & sg_uf %chin% unique(d$uf)]
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mand <- merge(mand[, .(id_mandato, id_pessoa, cargo, sg_uf, ano_eleicao = as.integer(ano_eleicao), fs_bocel = forma_saida, fonte_fs = fonte_forma_saida)],
              pess[, .(id_pessoa, nome, nome_urna_recente, dt_nascimento, nr_cpf)], by = "id_pessoa")
mand[, `:=`(nome_norm = norm(nome), urna_norm = norm(nome_urna_recente), cpf_norm = so_cpf(nr_cpf))]
chave <- c("sg_uf", "cargo", "ano_eleicao")
tok <- function(x) lapply(strsplit(x, " "), function(t) t[nchar(t) >= 3])
unico <- function(m, metodo) { if (!nrow(m)) return(data.table(rid2 = integer(), id_mandato = character(), id_pessoa = character(), metodo = character()))
  m[, if (uniqueN(id_mandato) == 1) .SD[1], by = rid2][, metodo := metodo][, .(rid2, id_mandato, id_pessoa, metodo)] }
# nome curto (< 8 caracteres) ou de um so token nao pareia por nome: risco de homonimo
d[, rid2 := .I]
d[, nome_seguro := nchar(nome_normalizado) >= 8L & lengths(tok(nome_normalizado)) >= 2L]
d[, completo_seguro := nchar(nome_completo_norm) >= 8L & lengths(tok(nome_completo_norm)) >= 2L]

feito <- integer(0); pares <- list()
add <- function(m) { if (nrow(m)) { pares[[length(pares) + 1L]] <<- m; feito <<- c(feito, m$rid2) }; invisible(NULL) }
# (0) CPF: o pareamento mais forte (ES vinculos, DF verba, RN folha)
add(unico(merge(d[!is.na(cpf), .(rid2, sg_uf = uf, cargo, ano_eleicao, cpf)],
                mand[!is.na(cpf_norm), .(sg_uf, cargo, ano_eleicao, cpf = cpf_norm, id_mandato, id_pessoa)], by = c(chave, "cpf")), "cpf"))
# (1) nome completo da fonte = nome civil do BOCEL
add(unico(merge(d[!rid2 %in% feito & completo_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, k = nome_completo_norm)],
                mand[, .(sg_uf, cargo, ano_eleicao, k = nome_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_completo_civil"))
# (2) nome da fonte = nome de urna
add(unico(merge(d[!rid2 %in% feito & nome_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, k = nome_normalizado)],
                mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, k = urna_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_urna"))
# (3) nome da fonte = nome civil
add(unico(merge(d[!rid2 %in% feito & nome_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, k = nome_normalizado)],
                mand[, .(sg_uf, cargo, ano_eleicao, k = nome_norm, id_mandato, id_pessoa)], by = c(chave, "k")), "nome_parlamentar_civil"))
# (4) tokens do nome da fonte contidos no nome civil do BOCEL (>= 2 tokens), mandato unico
contido <- function(cand, metodo, min_tok = 2L) {
  if (!nrow(cand)) return(cand[0][, .(rid2, id_mandato, id_pessoa)][, metodo := character()])
  ta <- tok(cand$a); tb <- tok(cand$b)
  cand[, ok := mapply(function(x, y) length(x) >= min_tok && all(x %in% y), ta, tb)]
  unico(cand[ok == TRUE], metodo)
}
c4 <- merge(d[!rid2 %in% feito & completo_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, a = nome_completo_norm)],
            mand[, .(sg_uf, cargo, ano_eleicao, b = nome_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
add(contido(c4, "tokens_nome_completo_no_civil"))
c5 <- merge(d[!rid2 %in% feito & nome_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, a = nome_normalizado)],
            mand[, .(sg_uf, cargo, ano_eleicao, b = nome_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
add(contido(c5, "tokens_no_nome_civil"))
c6 <- merge(d[!rid2 %in% feito & nome_seguro == TRUE, .(rid2, sg_uf = uf, cargo, ano_eleicao, a = nome_normalizado)],
            mand[!is.na(urna_norm), .(sg_uf, cargo, ano_eleicao, b = urna_norm, id_mandato, id_pessoa)], by = chave, allow.cartesian = TRUE)
add(contido(c6, "tokens_no_nome_de_urna"))

par <- rbindlist(pares, fill = TRUE)[!duplicated(rid2)]
d[, `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = NA_character_)]
if (nrow(par)) d[par$rid2, `:=`(id_mandato_bocel = par$id_mandato, id_pessoa_bocel = par$id_pessoa, metodo_pareamento = par$metodo)]
# suplente convocado nao ocupa o mandato do titular: nao pareia por token
d[condicao == "suplente" & grepl("^tokens", fcoalesce(metodo_pareamento, "")),
  `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = "descartado_suplente_token")]
# um mandato recebe no maximo uma linha por fonte
setorder(d, uf, fonte, ano_eleicao, -forma_saida, nome_normalizado, na.last = TRUE)
rep <- d[!is.na(id_mandato_bocel) & duplicated(d[, .(fonte, id_mandato_bocel)])]
cat("linhas repetidas no mesmo mandato e fonte (pareamento desfeito):", nrow(rep), "\n")
d[!is.na(id_mandato_bocel) & duplicated(d[, .(fonte, id_mandato_bocel)]),
  `:=`(id_mandato_bocel = NA_character_, id_pessoa_bocel = NA_character_, metodo_pareamento = paste0(fcoalesce(metodo_pareamento, ""), "_repetido"))]

## ------------------------------------------------ saida
out <- d[, .(sg_ue = NA_character_, uf, fonte, legislatura, ano_eleicao, nome, nome_normalizado, nome_completo,
             data_nascimento = NA_character_, partido, condicao,
             data_inicio_exercicio = ini_leg, data_fim_exercicio = fim_leg, causa_original, forma_saida,
             id_pessoa_bocel, id_mandato_bocel, metodo_pareamento, url)]
out[, sg_ue := NULL]
setorder(out, uf, fonte, ano_eleicao, nome_normalizado)
fwrite(out, "data/exercicio_assembleias_inventario.csv", na = "NA", quote = TRUE)
registrar_fonte("data/exercicio_assembleias_inventario.csv", "50_assembleias_inventario.R", NA)

## ============================================================ numeros e verificacao
n_linhas <- nrow(out)
n_uf <- uniqueN(out$uf)
n_par <- out[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]
n_fs  <- out[!is.na(forma_saida) & forma_saida != "nao_observado", .N]
n_fs_par <- out[!is.na(id_mandato_bocel) & !is.na(forma_saida) & forma_saida != "nao_observado", uniqueN(id_mandato_bocel)]
n_datas <- out[!is.na(data_inicio_exercicio) | !is.na(data_fim_exercicio), .N]
registrar_numero("ainv_linhas_totais", n_linhas, script = script)
registrar_numero("ainv_ufs_alcancadas", n_uf, script = script)
registrar_numero("ainv_mandatos_pareados", n_par, script = script)
registrar_numero("ainv_linhas_com_forma_saida_observada", n_fs, script = script)
registrar_numero("ainv_mandatos_com_forma_saida_observada", n_fs_par, script = script)
registrar_numero("ainv_linhas_com_data_de_exercicio", n_datas, script = script)
cat(sprintf("\nlinhas %d | UFs %d | mandatos pareados %d | linhas com forma de saida observada %d | mandatos com forma de saida %d | linhas com data %d\n",
            n_linhas, n_uf, n_par, n_fs, n_fs_par, n_datas))
cat("\nforma de saida por UF:\n"); print(dcast(out[, .N, by = .(uf, forma_saida = fcoalesce(forma_saida, "NA_suplente"))], uf ~ forma_saida, value.var = "N", fill = 0L))

# ganho marginal sobre o que ja esta nas tres tabelas de assembleia
ja <- rbindlist(lapply(c("data/exercicio_assembleias.csv", "data/exercicio_assembleias_2.csv", "data/exercicio_assembleias_historico.csv"),
                       function(f) fread(f, colClasses = "character", na.strings = "NA")[, .(uf, id_mandato_bocel, forma_saida)]), fill = TRUE)
ja_obs <- unique(ja[!is.na(id_mandato_bocel) & !is.na(forma_saida) & forma_saida != "nao_observado", id_mandato_bocel])
novos <- setdiff(out[!is.na(id_mandato_bocel) & !is.na(forma_saida) & forma_saida != "nao_observado", unique(id_mandato_bocel)], ja_obs)
registrar_numero("ainv_mandatos_forma_saida_ineditos", length(novos), script = script)
cat("mandatos com forma de saida que as tabelas de assembleia ainda nao tinham:", length(novos), "\n")

# precisao: onde o BOCEL ja tem forma de saida de outra fonte, a regra aqui concorda?
chk <- merge(out[!is.na(id_mandato_bocel) & !is.na(forma_saida) & forma_saida != "nao_observado", .(id_mandato_bocel, fs_novo = forma_saida)],
             mand[!is.na(fs_bocel), .(id_mandato_bocel = id_mandato, fs_bocel, fonte_fs)], by = "id_mandato_bocel")
chk <- unique(chk)
n_chk <- nrow(chk); n_ok <- chk[fs_novo == fs_bocel, .N]
prec <- if (n_chk > 0L) round(100 * n_ok / n_chk, 1) else NA_real_
registrar_numero("ainv_confronto_com_bocel_n", n_chk, script = script)
registrar_numero("ainv_confronto_com_bocel_concordancia_pct", prec, ep = "forma_saida ja registrada no mandatos.csv", script = script)
cat(sprintf("confronto com a forma de saida ja registrada no BOCEL: %d mandatos, %d coincidem (%s%%)\n", n_chk, n_ok, prec))
if (n_chk) print(chk[fs_novo != fs_bocel][, .N, by = .(fs_bocel, fs_novo)][order(-N)])

# checagem de coerencia do pareamento: nome pareado tem de bater com o nome do BOCEL em pelo menos um token longo.
# 05/09/2026: a referencia era so o nome civil, e os 5 pares reprovados (Zeca Pirao, Chico Vigilante x2, Juarezao,
# Bob Fllay) sao nomes de urna identicos ao do TSE, pareados pelas regras (0) CPF e (2) nome_urna que o proprio
# script declara; a checagem passa a aceitar token em comum com o nome civil OU com o nome de urna, que e o que
# as regras de pareamento usam. Reverter: trocar `nome_bocel, urna_bocel` por `nome_bocel` na linha do mapply.
cch <- merge(out[!is.na(id_pessoa_bocel), .(id_pessoa_bocel, nome_normalizado)],
             pess[, .(id_pessoa_bocel = id_pessoa, nome_bocel = norm(nome), urna_bocel = norm(nome_urna_recente))], by = "id_pessoa_bocel")
cch[, ok := mapply(function(a, b, u) any(nchar(a) >= 4 & (a %in% b | a %in% u)), tok(cch$nome_normalizado), tok(cch$nome_bocel), tok(cch$urna_bocel))]
n_incoerente <- cch[ok == FALSE, .N]
registrar_numero("ainv_pareamentos_sem_token_em_comum", n_incoerente, script = script)
cat("pareamentos sem nenhum token longo em comum com o nome do BOCEL:", n_incoerente, "\n")

falhou <- character(0)
if (!all(is.na(out$forma_saida) | out$forma_saida %chin% VOCAB)) falhou <- c(falhou, "forma_saida fora do vocabulario")
if (out[!is.na(id_mandato_bocel), .N] != out[!is.na(id_mandato_bocel), uniqueN(paste(fonte, id_mandato_bocel))]) falhou <- c(falhou, "mandato repetido dentro da mesma fonte")
if (n_incoerente > 0L) falhou <- c(falhou, "pareamento sem token em comum")
gravar_relatorio_verificacao(
  alvo = "data/exercicio_assembleias_inventario.csv", script = script,
  passou = c(sprintf("%d linhas, %d UFs, %d mandatos pareados", n_linhas, n_uf, n_par),
             sprintf("%d linhas com forma de saida observada em %d mandatos", n_fs, n_fs_par),
             sprintf("%d mandatos com forma de saida ineditos nas tabelas de assembleia", length(novos)),
             sprintf("concordancia com a forma de saida ja registrada no BOCEL: %s%% em %d mandatos", prec, n_chk),
             "nenhuma requisicao a servidor: toda a entrada veio de data_raw/inventario_assembleias"),
  falhou = falhou,
  fora_de_cobertura = c("a causa da saida antecipada (renuncia, cassacao, licenca, posse em outro cargo) nao esta nas fontes de folha e de relacao nominal: fica em 'outro'",
                        "suplente convocado nao e pareado ao mandato do titular; o BOCEL so tem titulares eleitos",
                        "paginacao incompleta das amostras de folha (MT, ES): a relacao nominal de alguns exercicios cobre menos que a bancada"))
sink(); close(logf)
