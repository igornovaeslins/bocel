# 13_exercicio_assembleias.R — exercicio de mandato de deputados estaduais/distritais a partir
#   das fontes das Assembleias Legislativas (SAPL e listas por legislatura), com pareamento ao BOCEL
# Entrada:  data_raw/assembleias/<uf>/ (coletados por python/fetch_assembleias.py)
#           data_raw/assembleias/inventario_fontes_assembleias.csv
#           data/mandatos.csv (cd_cargo 7 e 8), data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet (nome de urna)
# Saida:    data/exercicio_assembleias.csv|parquet
#           data/exercicio_assembleias_lacunas.csv (UF x motivo)
#           output/verificacao/pareamento_assembleias_uf_legislatura.csv
#           output/verificacao/assembleias_forma_saida.csv
#           output/numeros_assinatura.txt (registrar_numero)
# Execucao: cd ~/bocel && Rscript --vanilla R/13_exercicio_assembleias.R
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
  library(jsonlite)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
raw    <- file.path(root, "data_raw", "assembleias")
outd   <- file.path(root, "data")
verd   <- file.path(root, "output", "verificacao")
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "13_exercicio_assembleias.R")
logf   <- file.path(root, "logs", "13_exercicio_assembleias.log")
sink(logf, split = TRUE)
cat("13_exercicio_assembleias.R —", format(Sys.time()), "\n")

HOJE  <- as.IDate("2026-08-28")
ANOS  <- seq(1998L, 2022L, 4L)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || is.na(a[1])) b else a[1]
chr <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) NA_character_ else as.character(x[[1]])
norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
html_txt <- function(x) {
  x <- gsub("<[^>]+>", "", x)
  x <- stri_replace_all_regex(x, "&nbsp;|&#160;", " ")
  x <- stri_replace_all_regex(x, "&amp;", "&")
  x <- stri_replace_all_regex(x, "&#(\\d+);", "")  # entidades numericas: tratadas abaixo por nome
  gsub("\\s+", " ", trimws(x))
}
# entidades numericas (&#195; etc.) -> caractere
decode_ent <- function(x) {
  m <- gregexpr("&#\\d+;", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(v)
    if (length(v)) intToUtf8(as.integer(gsub("\\D", "", v)), multiple = TRUE) else v)
  x <- gsub("&nbsp;", " ", x, fixed = TRUE)
  gsub("&amp;", "&", x, fixed = TRUE)
}
leg_ano <- function(n, base_n, base_ano = 1998L) base_ano + (as.integer(n) - base_n) * 4L
vazio <- function() data.table(uf = character(), fonte = character(), legislatura = character(),
                                nome = character(), nome_completo = character(), data_nascimento = character(),
                                partido = character(), condicao = character(),
                                data_inicio_exercicio = character(), data_fim_exercicio = character(),
                                causa_original = character(), forma_saida = character(),
                                ano_eleicao = integer(), url = character(), id_fonte = character(),
                                votos_fonte = character(), sexo_fonte = character())

## ---------------------------------------------------------------- 1. SAPL (API Interlegis)
SAPL_UF <- c(AC = "https://sapl.al.ac.leg.br", AL = "https://sapl.al.al.leg.br", AM = "https://sapl.al.am.leg.br",
             PB = "https://sapl3.al.pb.leg.br", PI = "https://sapl.al.pi.leg.br", RO = "https://sapl.al.ro.leg.br",
             RR = "https://sapl.al.rr.leg.br", TO = "https://sapl.al.to.leg.br")
# vocabulario fechado de forma_saida:
#   fim_regular, renuncia, falecimento, cassacao, afastamento, licenca, nao_tomou_posse,
#   suplente_efetivado, nao_observado, outro ; NA = mandato em curso
classificar_saida <- function(desc_afast, indicador, obs, fim_mandato, fim_leg, em_curso) {
  d <- toupper(stri_trans_general(desc_afast %||% "", "Latin-ASCII"))
  o <- toupper(stri_trans_general(obs %||% "", "Latin-ASCII"))
  # tolerancia de 60 dias: varios SAPL encerram o mandato em 31/12 e a legislatura em 31/01
  antecipado <- !is.na(fim_mandato) & !is.na(fim_leg) & (fim_leg - fim_mandato) > 60
  if (grepl("FALEC|MORTE", d) || grepl("FALEC", o)) return("falecimento")
  if (grepl("CASSA|PERDA DE MANDATO", d) || grepl("CASSAD", o)) return("cassacao")
  if (grepl("RENUNC|ELEITO PARA OUTRO|MOTIVOS PESSOAIS", d) || grepl("RENUNC", o)) return("renuncia")
  if (grepl("EXTINCAO", d)) return("outro")
  if (nzchar(d) && identical(indicador, "F")) return("outro")
  if (grepl("LICEN|SAUDE|INTERESSE PARTICULAR|ASSUNTOS PARTICULARES", d)) return("licenca")
  if (grepl("MISSAO|SUSPENS|CARGO|INVESTIDURA|OCUPACAO", d)) return("afastamento")
  if (em_curso) return(NA_character_)
  if (antecipado) return("outro")
  "fim_regular"
}
ler_sapl <- function(uf) {
  d <- file.path(raw, uf)
  ler <- function(ep) {
    f <- file.path(d, sprintf("sapl_%s.json", ep))
    if (!file.exists(f)) return(NULL)
    fromJSON(f, simplifyVector = TRUE)$results
  }
  leg <- as.data.table(ler("legislatura"))
  man <- as.data.table(ler("mandato"))
  par <- as.data.table(ler("parlamentar"))
  fil <- as.data.table(ler("filiacao"))
  pty <- as.data.table(ler("partido"))
  taf <- as.data.table(ler("tipoafastamento"))
  if (!nrow(man) || !nrow(leg)) return(vazio())
  leg <- leg[, .(legislatura = id, numero_leg = numero, leg_inicio = as.IDate(data_inicio), leg_fim = as.IDate(data_fim))]
  leg[, ano_eleicao := as.integer(format(leg_inicio, "%Y")) - 1L]
  man <- merge(man, leg, by = "legislatura", all.x = TRUE)
  man <- merge(man, par[, .(parlamentar = id, nome_completo, nome_parlamentar, sexo)], by = "parlamentar", all.x = TRUE)
  if (nrow(taf)) man <- merge(man, taf[, .(tipo_afastamento = id, desc_afast = descricao, indicador)],
                              by = "tipo_afastamento", all.x = TRUE)
  else man[, `:=`(desc_afast = NA_character_, indicador = NA_character_)]
  # partido: ultima filiacao iniciada ate o inicio do mandato; senao a primeira filiacao registrada
  if (nrow(fil) && nrow(pty)) {
    fil <- merge(fil[, .(parlamentar, partido, data = as.IDate(data))], pty[, .(partido = id, sigla)], by = "partido")
    setorder(fil, parlamentar, data)
    man[, data_inicio_mandato := as.IDate(data_inicio_mandato)]
    man[, partido_sigla := {
      f <- fil[parlamentar == .BY$parlamentar]
      if (!nrow(f)) NA_character_ else {
        ok <- f[!is.na(data) & data <= (data_inicio_mandato[1] %||% as.IDate("2100-01-01"))]
        if (nrow(ok)) ok$sigla[nrow(ok)] else f$sigla[1]
      }
    }, by = .(parlamentar, id)]
  } else man[, partido_sigla := NA_character_]
  man[, data_fim := as.IDate(data_fim_mandato)]
  man[, em_curso := is.na(data_fim) | data_fim >= HOJE]
  man[, forma_saida := mapply(classificar_saida, desc_afast, indicador, observacao, data_fim, leg_fim, em_curso)]
  # suplente que ficou ate o fim da legislatura: suplente_efetivado (mesma convencao do Senado)
  man[titular %in% FALSE & forma_saida %in% "fim_regular", forma_saida := "suplente_efetivado"]
  man[, causa_original := fifelse(!is.na(desc_afast), desc_afast,
                                  fifelse(!is.na(observacao) & nzchar(observacao),
                                          substr(gsub("[[:space:]]+", " ", observacao), 1, 200), NA_character_))]
  man[, .(uf = uf, fonte = "sapl_api", legislatura = as.character(numero_leg),
          nome = nome_parlamentar, nome_completo, data_nascimento = NA_character_,
          partido = partido_sigla, condicao = fifelse(titular %in% TRUE, "titular", "suplente"),
          data_inicio_exercicio = as.character(data_inicio_mandato), data_fim_exercicio = as.character(data_fim),
          causa_original, forma_saida, ano_eleicao,
          url = sprintf("%s/api/parlamentares/mandato/%s/", SAPL_UF[[uf]], id),
          id_fonte = as.character(id), votos_fonte = as.character(votos_recebidos), sexo_fonte = sexo)]
}
sapl <- rbindlist(lapply(names(SAPL_UF), ler_sapl), use.names = TRUE)
cat("SAPL: linhas (todas as legislaturas):", nrow(sapl), "\n")

## ---------------------------------------------------------------- 2. listas HTML por legislatura
ler_arq <- function(f) paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html_base <- function(uf, fonte, leg, ano, nome, partido = NA_character_, condicao = "nao_informado",
                      url, id_fonte = NA_character_, extra_saida = "nao_observado") {
  if (!length(nome)) return(vazio())
  data.table(uf = uf, fonte = fonte, legislatura = as.character(leg), nome = nome, nome_completo = NA_character_,
             data_nascimento = NA_character_, partido = partido, condicao = condicao,
             data_inicio_exercicio = NA_character_, data_fim_exercicio = NA_character_,
             causa_original = NA_character_, forma_saida = extra_saida, ano_eleicao = as.integer(ano),
             url = url, id_fonte = id_fonte, votos_fonte = NA_character_, sexo_fonte = NA_character_)
}
# SP: /deputado/legislaturas?idLegislatura=n (14..20); nome + matricula
ler_sp <- function() rbindlist(lapply(14:20, function(n) {
  f <- file.path(raw, "SP", sprintf("legislatura_%d.html", n)); if (!file.exists(f)) return(vazio())
  t <- ler_arq(f)
  # bloco da tabela (primeira coluna: link do parlamentar)
  m <- stri_match_all_regex(t, '<a href="/deputado/\\?matricula=(\\d+)">([^<]+)</a>')[[1]]
  if (is.na(m[1, 1])) return(vazio())
  html_base("SP", "alesp_relacao_legislaturas", n, leg_ano(n, 14L), trimws(decode_ent(m[, 3])),
            url = sprintf("https://www.al.sp.gov.br/deputado/legislaturas?idLegislatura=%d", n), id_fonte = m[, 2])
}))
# PR: cards com alt="Foto do deputado X" e href perfil/slug
PR_SLUGS <- c("14-legislatura", "15-legislatura", "16-legislatura",
              "17-legislatura-1-e-2-sessoes-legislativas", "17-legislatura-3-e-4-sessoes-legislativas",
              "18-legislatura-1-e-2-sessoes-legislativas", "18-legislatura-3-e-4-sessoes-legislativas",
              "19-legislatura-1-e-2-sessoes-legislativas", "19a-legislatura-3a-e-4a-sessoes-legislativas",
              "20a-legislatura-1a-e-2a-sessoes-legislativas", "20a-legislatura-1a-e-2a-sessoes-legislativas-2")
ler_pr <- function() {
  x <- rbindlist(lapply(PR_SLUGS, function(s) {
    f <- file.path(raw, "PR", sprintf("legislatura_%s.html", s)); if (!file.exists(f)) return(vazio())
    t <- ler_arq(f)
    n <- as.integer(sub("^(\\d+).*", "\\1", s))
    m <- stri_match_all_regex(t, 'deputados/perfil/([a-z0-9-]+)"[\\s\\S]*?alt="Foto do deputado ([^"]*)"')[[1]]
    if (is.na(m[1, 1])) return(vazio())
    html_base("PR", "alep_legislatura", n, leg_ano(n, 14L), trimws(decode_ent(m[, 3])),
              url = sprintf("https://www.assembleia.pr.leg.br/deputados/legislatura?legislatura=%s", s), id_fonte = m[, 2])
  }))
  # a mesma pessoa aparece nas duas metades da legislatura: uma linha por pessoa x legislatura
  x[!duplicated(x[, .(legislatura, id_fonte)])]
}
# BA: cards deputado-nome / partido-nome
ler_ba <- function() {
  arqs <- c(setNames(sprintf("legislatura_%d.html", 14:19), 14:19), "20" = "legislatura_atual.html")
  rbindlist(lapply(names(arqs), function(n) {
    f <- file.path(raw, "BA", arqs[[n]]); if (!file.exists(f)) return(vazio())
    t <- ler_arq(f)
    m <- stri_match_all_regex(t, '<div class="deputado-nome">\\s*<a href="/deputados/[a-z-]+/(\\d+)">\\s*<span>([^<]+)</span>\\s*</a>\\s*</div>\\s*<div class="partido-nome">([^<]*)</div>')[[1]]
    if (is.na(m[1, 1])) return(vazio())
    url <- if (n == "20") "https://www.al.ba.gov.br/deputados/legislatura-atual" else
      sprintf("https://www.al.ba.gov.br/deputados/ex-deputados-estaduais/legislatura/%s", n)
    html_base("BA", "alba_legislatura", n, leg_ano(n, 14L), trimws(decode_ent(m[, 3])), partido = trimws(m[, 4]),
              url = url, id_fonte = m[, 2])
  }))
}
# GO: tabela Nome/Partido; id 27..33 = 14a..20a
ler_go <- function() rbindlist(lapply(27:33, function(i) {
  f <- file.path(raw, "GO", sprintf("legislatura_%d.html", i)); if (!file.exists(f)) return(vazio())
  t <- ler_arq(f)
  m <- stri_match_all_regex(t, '<tr>\\s*<td><a target="_blank" href="/deputados/perfil/(\\d+)">([^<]+)</a></td>\\s*<td>([^<]*)</td>')[[1]]
  if (is.na(m[1, 1])) return(vazio())
  n <- i - 13L
  html_base("GO", "alego_legislaturas_anteriores", n, leg_ano(n, 14L), trimws(decode_ent(m[, 3])), partido = trimws(m[, 4]),
            url = sprintf("https://portal.al.go.leg.br/legislaturas/legislaturas-anteriores/%d", i), id_fonte = m[, 2])
}))
# DF: div.deputado (titulares; secao "Suplentes" depois) e cards da legislatura atual
ler_df <- function() {
  x <- rbindlist(lapply(3:8, function(n) {
    f <- file.path(raw, "DF", sprintf("legislatura_%d.html", n)); if (!file.exists(f)) return(vazio())
    t <- ler_arq(f)
    pos_sup <- regexpr("Suplentes", t, fixed = TRUE)
    m <- stri_match_all_regex(t, '<div class="deputado">\\s*<p><img alt="([^"]*)"[^>]*>[^<]*</p>\\s*<p[^>]*>([^<]*)</p>\\s*<p[^>]*>([^<]*)</p>', omit_no_match = TRUE)[[1]]
    if (!nrow(m)) return(vazio())
    loc <- stri_locate_all_regex(t, '<div class="deputado">')[[1]][, 1]
    cond <- if (pos_sup > 0) fifelse(loc > pos_sup, "suplente", "titular") else rep("titular", nrow(m))
    html_base("DF", "cldf_legislaturas_anteriores", n, leg_ano(n, 3L), trimws(decode_ent(m[, 3])),
              partido = trimws(decode_ent(m[, 4])), condicao = cond,
              url = sprintf("https://www.cl.df.gov.br/legislaturas-anteriores (%da legislatura)", n))
  }))
  f <- file.path(raw, "DF", "legislatura_atual.html")
  if (file.exists(f)) {
    t <- ler_arq(f)
    m <- stri_match_all_regex(t, '<span class="card-title">([^<]+)</span>\\s*<span class="card-text">([^<]*)</span>', omit_no_match = TRUE)[[1]]
    if (nrow(m)) x <- rbind(x, html_base("DF", "cldf_legislatura_atual", 9, 2022L, trimws(decode_ent(m[, 2])),
                                         partido = trimws(decode_ent(m[, 3])), condicao = "titular",
                                         url = "https://www.cl.df.gov.br/deputados-2023-2026"))
  }
  x
}
# RJ: QuemSao legislatura 18..20 (11a..13a) x situacao (1 em exercicio, 2 exerceram)
ler_rj <- function() rbindlist(lapply(18:20, function(n) rbindlist(lapply(1:2, function(s) {
  f <- file.path(raw, "RJ", sprintf("legislatura_%d_sit%d.html", n, s)); if (!file.exists(f)) return(vazio())
  t <- ler_arq(f)
  m <- stri_match_all_regex(t, '<div class="partido">([^<]*)</div>\\s*<div class="nome"><a href="/Deputados/PerfilDeputado/(\\d+)[^"]*">([^<]+)</a></div>', omit_no_match = TRUE)[[1]]
  if (!nrow(m)) return(vazio())
  x <- html_base("RJ", "alerj_quemsao", n - 7L, leg_ano(n, 18L, 2014L), trimws(decode_ent(m[, 4])), partido = trimws(m[, 2]),
                 url = sprintf("https://www.alerj.rj.gov.br/Deputados/QuemSao?legislatura=%d&situacaoDeputado=%d", n, s),
                 id_fonte = m[, 3])
  x[, causa_original := if (s == 1) "em exercicio (situacaoDeputado=1)" else "exerceu o mandato (situacaoDeputado=2)"]
  # NA (em curso) so na legislatura em andamento (20 = 13a, 2023-2027); nas encerradas a lista nao informa a saida
  # (correcao 2026-08-28, verifica_assembleias: 140 registros de 2014/2018 com situacaoDeputado=1 estavam NA)
  x[, forma_saida := if (s == 1 && n == 20) NA_character_ else "nao_observado"]
  x
}))))
htmls <- rbindlist(list(ler_sp(), ler_pr(), ler_ba(), ler_go(), ler_df(), ler_rj()), use.names = TRUE)
# RJ: a legislatura 13a (2023-2027) esta em curso; quem "exerceu" saiu antes do fim
cat("HTML: linhas por UF:\n"); print(htmls[, .N, by = .(uf, fonte)])

## ---------------------------------------------------------------- 3. base unificada (janela 1998-2022)
ex <- rbindlist(list(sapl, htmls), use.names = TRUE)
ex <- ex[ano_eleicao %in% ANOS]
ex[, nome_normalizado := norm_nome(nome)]
ex[, nome_completo_norm := norm_nome(nome_completo)]
ex[nome_normalizado == "", nome_normalizado := NA_character_]
cat("registros na janela 1998-2022:", nrow(ex), "\n")

## ---------------------------------------------------------------- 4. BOCEL: eleitos cd_cargo 7/8 + nome de urna
bocel_m <- fread(file.path(outd, "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("sq_candidato", "nr_candidato")))
bocel_p <- fread(file.path(outd, "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = list(character = c("nr_titulo_eleitoral", "nr_cpf")))
dep <- bocel_m[cd_cargo %in% c(7L, 8L), .(id_mandato, id_pessoa, ano_eleicao, sg_uf, cd_cargo, sq_candidato, nr_candidato)]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome, dt_nascimento = as.character(dt_nascimento))], by = "id_pessoa")
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
# nome de urna da candidatura de origem (cadastro TSE, data_raw/parquet)
urna <- rbindlist(lapply(list.files(file.path(root, "data_raw", "parquet"), pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE),
                         function(f) {
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_TIPO_ELEICAO")))
  x <- x[as.integer(CD_CARGO) %in% c(7L, 8L) & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sg_uf = SG_UF, cd_cargo = as.integer(CD_CARGO),
        nr_candidato = as.character(NR_CANDIDATO), sq_candidato = as.character(SQ_CANDIDATO), nome_urna = NM_URNA_CANDIDATO)]
}))
urna <- urna[!duplicated(urna[, .(ano_eleicao, sg_uf, cd_cargo, nr_candidato, sq_candidato)])]
dep <- merge(dep, urna, by = c("ano_eleicao", "sg_uf", "cd_cargo", "nr_candidato", "sq_candidato"), all.x = TRUE)
dep[, nome_urna_norm := norm_nome(nome_urna)]
cat("eleitos BOCEL cd_cargo 7/8:", nrow(dep), "| com nome de urna:", dep[!is.na(nome_urna), .N], "\n")

## ---------------------------------------------------------------- 5. pareamento (nome + UF + ano de eleicao)
# Regras, em ordem: (a) nome completo da fonte = nome completo do BOCEL; (b) nome parlamentar = nome de urna;
# (c) nome parlamentar = nome completo do BOCEL; (d) nome completo da fonte = nome de urna.
# Cada regra exige unicidade nos dois lados (um candidato no BOCEL e um registro na fonte por UF x ano x nome).
ex[, rid := .I]
parear <- function(ex, dep, col_ex, col_dep, metodo) {
  a <- ex[is.na(id_mandato) & !is.na(get(col_ex)), .(rid, sg_uf = uf, ano_eleicao, chave = get(col_ex))]
  a <- a[, if (.N == 1L) .SD, by = .(sg_uf, ano_eleicao, chave)]
  b <- dep[!is.na(get(col_dep)), .(id_mandato, id_pessoa, sg_uf, ano_eleicao, chave = get(col_dep))]
  b <- b[, if (.N == 1L) .SD, by = .(sg_uf, ano_eleicao, chave)]
  m <- merge(a, b, by = c("sg_uf", "ano_eleicao", "chave"))
  m <- m[!id_mandato %in% ex$id_mandato]
  ex[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa, metodo_pareamento = metodo)]
  cat(sprintf("  %-28s +%d\n", metodo, nrow(m)))
  invisible(ex)
}
ex[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_, metodo_pareamento = NA_character_)]
cat("pareamento por regra:\n")
parear(ex, dep, "nome_completo_norm", "nome_bocel_norm", "nome_completo_x_nome_bocel")
parear(ex, dep, "nome_normalizado", "nome_urna_norm", "nome_parlamentar_x_urna")
parear(ex, dep, "nome_normalizado", "nome_bocel_norm", "nome_parlamentar_x_nome_bocel")
parear(ex, dep, "nome_completo_norm", "nome_urna_norm", "nome_completo_x_urna")
# um id_mandato nao pode ser atribuido a dois registros da mesma fonte na mesma legislatura
# (uma pessoa pode ter mais de um exercicio no SAPL; isso e legitimo)
# pessoa (sem mandato pareado): nome completo unico no cadastro do BOCEL entre pessoas com mandato na UF
pess_uf <- merge(bocel_m[, .(id_pessoa, sg_uf)], bocel_p[, .(id_pessoa, nome_norm = norm_nome(nome))], by = "id_pessoa")
pess_uf <- unique(pess_uf[, .(id_pessoa, sg_uf, nome_norm)])
pess_uf <- pess_uf[, if (uniqueN(id_pessoa) == 1L) .SD[1], by = .(sg_uf, nome_norm)]
np <- ex[is.na(id_pessoa) & !is.na(nome_completo_norm)]
np <- merge(np[, .(rid, sg_uf = uf, nome_norm = nome_completo_norm)], pess_uf, by = c("sg_uf", "nome_norm"))
ex[np, on = "rid", `:=`(id_pessoa = i.id_pessoa, metodo_pareamento = "pessoa_nome_completo_uf")]
cat(sprintf("  %-28s +%d\n", "pessoa_nome_completo_uf", nrow(np)))

## ---------------------------------------------------------------- 6. taxa de pareamento por UF x legislatura
cob <- dep[, .(n_bocel = .N), by = .(sg_uf, ano_eleicao)]
cob <- merge(cob, ex[!is.na(id_mandato), .(n_pareados = uniqueN(id_mandato)), by = .(sg_uf = uf, ano_eleicao)],
             by = c("sg_uf", "ano_eleicao"), all.x = TRUE)
cob <- merge(cob, ex[, .(n_fonte = .N, n_fonte_titulares = sum(condicao == "titular"),
                         fonte = paste(unique(fonte), collapse = ";")), by = .(sg_uf = uf, ano_eleicao)],
             by = c("sg_uf", "ano_eleicao"), all.x = TRUE)
cob[is.na(n_pareados), n_pareados := 0L]
cob[, coletada := !is.na(n_fonte)]
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
cob[, legislatura_ano_inicio := ano_eleicao + 1L]
setorder(cob, sg_uf, ano_eleicao)
fwrite(cob, file.path(verd, "pareamento_assembleias_uf_legislatura.csv"))
cob_uf <- cob[coletada == TRUE, .(n_bocel = sum(n_bocel), n_pareados = sum(n_pareados), n_fonte = sum(n_fonte),
                                  legislaturas = .N), by = sg_uf][, taxa := round(n_pareados / n_bocel, 4)][order(sg_uf)]
cat("\nTaxa de pareamento por UF (eleicoes coletadas):\n"); print(cob_uf)

## ---------------------------------------------------------------- 7. lacunas por UF
inv <- fread(file.path(raw, "inventario_fontes_assembleias.csv"), encoding = "UTF-8")
ufs_coletadas <- sort(unique(ex$uf))
lac <- inv[, .(uf, casa, viavel, tipo_fonte, cobertura_legislaturas,
               coletada = uf %in% ufs_coletadas,
               motivo = fcase(uf %in% ufs_coletadas & viavel == "sim" & tipo_fonte == "sapl_api", "coletada: API SAPL com datas de mandato",
                              uf %in% ufs_coletadas & viavel == "sim", "coletada: lista por legislatura sem datas de posse/saida (forma_saida nao_observado)",
                              viavel == "parcial", paste0("lacuna: fonte so cobre a legislatura atual (", observacao, ")"),
                              viavel == "nao", paste0("lacuna: sem lista estruturada (", observacao, ")"),
                              default = "lacuna: nao coletada"))]
# cobertura parcial dentro das coletadas
lac[uf == "RJ", motivo := paste(motivo, "; legislaturas anteriores a 2015 ausentes do portal")]
lac[uf == "AM", motivo := paste(motivo, "; acervo SAPL comeca na 17a legislatura (2011)")]
lac[uf == "TO", motivo := paste(motivo, "; acervo SAPL comeca na 9a legislatura (2019)")]
lac[uf == "PB", motivo := paste(motivo, "; acervo SAPL comeca na 14a legislatura (1999)")]
fwrite(lac[order(uf)], file.path(outd, "exercicio_assembleias_lacunas.csv"), sep = ",", na = "NA", quote = TRUE)
cat("\nUFs coletadas:", paste(ufs_coletadas, collapse = " "), "\n")

## ---------------------------------------------------------------- 8. saida
setorder(ex, uf, ano_eleicao, condicao, nome_normalizado, data_inicio_exercicio, na.last = TRUE)
saida <- ex[, .(uf, fonte, legislatura, ano_eleicao, nome, nome_normalizado, nome_completo, data_nascimento,
                partido, condicao, data_inicio_exercicio, data_fim_exercicio, causa_original, forma_saida,
                id_pessoa_bocel = id_pessoa, id_mandato_bocel = id_mandato, metodo_pareamento, url, id_fonte,
                votos_fonte, sexo_fonte)]
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "nao_observado", "outro")
stopifnot(all(is.na(saida$forma_saida) | saida$forma_saida %in% VOCAB))
stopifnot(all(saida$condicao %in% c("titular", "suplente", "nao_informado")))
stopifnot(all(saida$ano_eleicao %in% ANOS))
stopifnot(!anyDuplicated(saida[fonte == "sapl_api", .(uf, id_fonte)]))
# um id_mandato do BOCEL nao pode apontar para duas pessoas distintas da fonte (mesma UF x ano x fonte)
dup <- saida[!is.na(id_mandato_bocel), .(n = uniqueN(nome_normalizado)), by = .(id_mandato_bocel)][n > 1]
stopifnot(nrow(dup) == 0)
fwrite(saida, file.path(outd, "exercicio_assembleias.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
write_parquet(saida, file.path(outd, "exercicio_assembleias.parquet"))
cat("\nexercicio_assembleias:", nrow(saida), "linhas x", ncol(saida), "colunas\n")
fs <- saida[, .N, by = .(fonte_tipo = fifelse(fonte == "sapl_api", "sapl_api", "html"), condicao, forma_saida)][order(fonte_tipo, condicao, -N)]
cat("\nForma de saida por tipo de fonte e condicao:\n"); print(fs)
fwrite(fs, file.path(verd, "assembleias_forma_saida.csv"))

## ---------------------------------------------------------------- 9. registro de numeros
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
reg("asm_n_casas_inventariadas", nrow(inv))
reg("asm_n_ufs_viaveis_historico", inv[viavel == "sim", .N])
reg("asm_n_ufs_viaveis_so_atual", inv[viavel == "parcial", .N])
reg("asm_n_ufs_sem_fonte", inv[viavel == "nao", .N])
reg("asm_ufs_viaveis_historico", paste(inv[viavel == "sim", uf], collapse = ";"))
reg("asm_n_ufs_coletadas", length(ufs_coletadas))
reg("asm_ufs_coletadas", paste(ufs_coletadas, collapse = ";"))
reg("asm_n_ufs_sapl_api", uniqueN(saida[fonte == "sapl_api", uf]))
reg("asm_n_registros", nrow(saida))
reg("asm_n_registros_sapl", saida[fonte == "sapl_api", .N])
reg("asm_n_registros_html", saida[fonte != "sapl_api", .N])
reg("asm_n_registros_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("asm_n_registros_suplente", saida[condicao == "suplente", .N])
reg("asm_n_eleitos_bocel_cd7_cd8", nrow(dep))
reg("asm_n_eleitos_bocel_com_nome_urna", dep[!is.na(nome_urna), .N])
reg("asm_n_eleitos_bocel_nas_ufs_x_anos_coletados", cob[coletada == TRUE, sum(n_bocel)])
reg("asm_n_mandatos_bocel_pareados", uniqueN(saida[!is.na(id_mandato_bocel), id_mandato_bocel]))
reg("asm_n_registros_com_id_pessoa", saida[!is.na(id_pessoa_bocel), .N])
reg("asm_taxa_pareamento_global_ufs_anos_coletados",
    sprintf("%d/%d=%.4f", cob[coletada == TRUE, sum(n_pareados)], cob[coletada == TRUE, sum(n_bocel)],
            cob[coletada == TRUE, sum(n_pareados) / sum(n_bocel)]))
for (i in seq_len(nrow(cob_uf))) reg(sprintf("asm_taxa_pareamento_uf_%s", cob_uf$sg_uf[i]),
                                     sprintf("%d/%d=%.4f", cob_uf$n_pareados[i], cob_uf$n_bocel[i], cob_uf$taxa[i]))
cc <- cob[coletada == TRUE]
for (i in seq_len(nrow(cc))) reg(sprintf("asm_taxa_pareamento_%s_%d", cc$sg_uf[i], cc$ano_eleicao[i]),
                                 sprintf("%d/%d=%.4f", cc$n_pareados[i], cc$n_bocel[i], cc$taxa_pareamento[i]))
for (m in unique(na.omit(saida$metodo_pareamento))) reg(paste0("asm_n_pareados_metodo_", m), saida[metodo_pareamento == m, .N])
fst <- saida[, .N, by = forma_saida][order(-N)]
for (i in seq_len(nrow(fst))) reg(paste0("asm_n_forma_saida_", fst$forma_saida[i] %||% "NA_em_curso"), fst$N[i])
gravar_relatorio_verificacao(
  alvo = "data/exercicio_assembleias.csv", script = script,
  passou = c("forma_saida no vocabulario fechado ou NA", "condicao em {titular, suplente, nao_informado}",
             "ano_eleicao em 1998..2022", "id_fonte unico por UF nas fontes SAPL",
             "id_mandato_bocel nao aponta para dois nomes distintos",
             sprintf("taxa de pareamento global (UFs x anos coletados) = %.4f", cob[coletada == TRUE, sum(n_pareados) / sum(n_bocel)])),
  fora_de_cobertura = c("validade das datas e causas informadas pelos SAPL das Assembleias",
                        "homonimos no pareamento por nome (sem data de nascimento nas fontes das Assembleias)",
                        "listas HTML (SP, PR, BA, GO, DF, RJ) nao informam posse, saida nem, em SP e PR, partido",
                        "13 UFs sem fonte historica estruturada (ver data/exercicio_assembleias_lacunas.csv)"))
cat("\n13_exercicio_assembleias: concluido —", format(Sys.time()), "\n")
sink()
