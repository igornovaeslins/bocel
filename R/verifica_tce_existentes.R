# verifica_tce_existentes.R — verificacao determinista das DUAS tabelas de Tribunais de Contas ja
# construidas: data/tce_gestores.csv (grupo A, R/23) e data/tce_gestores_b.csv (grupo B, R/24).
#
# Verifica o que as duas tabelas prometem: chave e granularidade, esquema de colunas, codigo de ausente
# 'NA', vocabulario fechado de forma_saida, integridade referencial de id_mandato_bocel e id_pessoa_bocel
# contra data/mandatos.csv e data/pessoas.csv, e datas dentro da janela do mandato pareado. Mede tambem
# o quanto a fonte 'tce' acrescenta sobre as demais fontes de forma de saida que R/10 consome.
#
# Nao altera R/23, R/24, R/10 nem data/mandatos.csv. Entrada somente-leitura.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_tce_existentes.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(stringi) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
script <- "R/verifica_tce_existentes.R"
dir.create("output/verificacao", recursive = TRUE, showWarnings = FALSE)
dir.create("logs", showWarnings = FALSE)
logf <- file("logs/verifica_tce_existentes.log", open = "wt"); sink(logf, split = TRUE)

passou <- character(); falhou <- character()
ck <- function(nome, cond, detalhe = "") {
  ok <- isTRUE(all(cond)) && length(cond) > 0
  cat(if (ok) "[ok]    " else "[FALHA] ", nome, if (nchar(detalhe)) paste0(" — ", detalhe) else "", "\n", sep = "")
  if (ok) passou <<- c(passou, nome) else falhou <<- c(falhou, paste0(nome, if (nchar(detalhe)) paste0(" (", detalhe, ")") else ""))
  invisible(ok)
}
so_dig <- function(x) gsub("[^0-9]", "", as.character(x))
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "outro", "nao_observado")
CARGOS <- c("PREFEITO", "VICE-PREFEITO", "VEREADOR")
CD_DE <- c(PREFEITO = "11", `VICE-PREFEITO` = "12", VEREADOR = "13")
OBRIG <- c("uf", "tribunal", "unidade_gestora", "tipo_unidade", "id_municipio_ibge", "sg_ue", "nome",
           "cpf", "cargo_fonte", "cargo_bocel", "data_inicio", "data_fim", "situacao_fonte", "forma_saida",
           "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento", "url", "fonte",
           "nome_municipio_fonte", "exercicio", "ano_eleicao")
METODOS <- c("cpf_municipio_cargo_eleicao", "cpf_municipio_cargo", "nome_completo_eleicao",
             "nome_fonte=nome_urna_eleicao", "tokens_nome_fonte_no_nome_civil",
             "tokens_nome_fonte_no_nome_de_urna", "nome_completo_municipio_cargo", "descartado_duplicata")

## ------------------------------------------------------------------ base de referencia do BOCEL
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")
mun  <- fread("data/municipios_tse_ibge.csv", colClasses = "character")
mref <- mand[cd_cargo %in% c("11", "12", "13"),
             .(id_mandato, m_cd = cd_cargo, m_ue = unidade_posicao, m_uf = sg_uf, m_pessoa = id_pessoa,
               m_ano = ano_eleicao, m_ini = mandato_inicio, m_fim = mandato_fim)]
ck("00 mandatos.csv e pessoas.csv legiveis", nrow(mand) > 0 && nrow(pess) > 0, paste(nrow(mand), "mandatos"))

## ------------------------------------------------------------------ bloco por tabela
TAB <- list(
  A = list(f = "data/tce_gestores.csv",   fc = "data/tce_gestores_cobertura.csv",
           inv = "data_raw/tce/inventario_tce.csv",   cargos = c("PREFEITO", "VEREADOR"),
           extra = c("ano_eleicao_fonte", "relacao_chapa_eleita", "id_mandato_titular_substituido")),
  B = list(f = "data/tce_gestores_b.csv", fc = "data/tce_gestores_b_cobertura.csv",
           inv = "data_raw/tce/inventario_tce_b.csv", cargos = CARGOS, extra = character())
)
tabs <- list()
for (g in names(TAB)) {
  cfg <- TAB[[g]]; P <- function(x) paste0(g, " ", x)
  cat("\n================= tabela ", g, ": ", cfg$f, " =================\n", sep = "")
  ck(P("01 arquivos existem"), all(file.exists(cfg$f, cfg$fc, cfg$inv)))
  o  <- fread(cfg$f, colClasses = "character", na.strings = "NA")
  cb <- fread(cfg$fc, na.strings = "NA")
  # leitura crua: nenhuma celula pode ser string vazia; ausente e o literal 'NA'
  cru <- fread(cfg$f, colClasses = "character", na.strings = character(0))
  tabs[[g]] <- o

  ## --- esquema
  ck(P("02 colunas obrigatorias presentes"), all(OBRIG %in% names(o)), paste(setdiff(OBRIG, names(o)), collapse = ","))
  ck(P("03 colunas declaradas e nada alem"), setequal(names(o), c(OBRIG, cfg$extra)),
     paste(setdiff(names(o), c(OBRIG, cfg$extra)), collapse = ","))
  ck(P("04 nomes de coluna minusculos sem acento"), all(grepl("^[a-z0-9_]+$", names(o))),
     paste(grep("^[a-z0-9_]+$", names(o), invert = TRUE, value = TRUE), collapse = ","))
  n_vazio <- sum(vapply(cru, function(x) sum(x == ""), integer(1)))
  ck(P("05 nenhuma celula vazia (ausente e o literal NA)"), n_vazio == 0L, paste(n_vazio, "celulas vazias"))
  ck(P("06 base nao vazia"), nrow(o) > 0, paste(nrow(o), "linhas"))
  # o arquivo publicado tem de ser UTF-8 valido: as fontes de ES e SC chegam em Latin-1 e as colunas
  # que passam direto para a saida ja conservaram o byte Latin-1 (R/23, corrigido em 29/ago/2026)
  n_nao_utf8 <- sum(!stri_enc_isutf8(readLines(cfg$f, warn = FALSE)))
  ck(P("06b arquivo inteiro em UTF-8 valido"), n_nao_utf8 == 0L, paste(n_nao_utf8, "linhas com byte invalido"))
  registrar_numero(paste0("tcev_", tolower(g), "_n_linhas_nao_utf8"), n_nao_utf8, script = script)

  ## --- vocabularios fechados (asserts_rigor)
  in_set(o$forma_saida, VOCAB, permitir_na = FALSE, nome = P("forma_saida"))
  ck(P("07 forma_saida no vocabulario fechado"), TRUE, paste(sort(unique(o$forma_saida)), collapse = "|"))
  in_set(o$cargo_bocel, cfg$cargos, permitir_na = FALSE, nome = P("cargo_bocel"))
  ck(P("08 cargo_bocel no vocabulario da tabela"), TRUE, paste(sort(unique(o$cargo_bocel)), collapse = "|"))
  # 'outra' e a unidade gestora municipal que nao e prefeitura nem camara (autarquia, instituto de
  # previdencia) e que a folha do Sagres/PB usa para pagar um agente eletivo; nunca deve parear
  in_set(o$tipo_unidade, c("prefeitura", "camara", "outra"), permitir_na = FALSE, nome = P("tipo_unidade"))
  ck(P("09 tipo_unidade em {prefeitura, camara, outra}"), TRUE,
     paste(paste0(names(table(o$tipo_unidade)), "=", as.integer(table(o$tipo_unidade))), collapse = " "))
  ck(P("09b unidade gestora 'outra' nunca pareia mandato"),
     o[tipo_unidade == "outra", .N] == 0L || o[tipo_unidade == "outra", all(is.na(id_mandato_bocel))],
     paste(o[tipo_unidade == "outra" & !is.na(id_mandato_bocel), .N], "pareadas"))
  in_set(o$metodo_pareamento, METODOS, permitir_na = TRUE, nome = P("metodo_pareamento"))
  ck(P("10 metodo_pareamento no conjunto declarado"), TRUE, paste(sort(unique(na.omit(o$metodo_pareamento))), collapse = "|"))
  ck(P("11 cargo_bocel coerente com tipo_unidade"),
     o[tipo_unidade == "camara", all(cargo_bocel == "VEREADOR")] &&
     o[tipo_unidade == "prefeitura", all(cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO"))],
     paste(o[tipo_unidade == "camara" & cargo_bocel != "VEREADOR", .N] +
           o[tipo_unidade == "prefeitura" & !cargo_bocel %in% c("PREFEITO", "VICE-PREFEITO"), .N], "divergencias"))
  ck(P("12 uf da linha bate com a sigla do tribunal"), all(o$uf == substr(o$tribunal, nchar(o$tribunal) - 1L, nchar(o$tribunal))))
  ck(P("13 nome sempre preenchido com 5 caracteres ou mais"), all(!is.na(o$nome) & nchar(o$nome) >= 5L))
  ck(P("14 municipio sempre resolvido"), all(!is.na(o$sg_ue) & !is.na(o$id_municipio_ibge)))
  ck(P("15 par sg_ue/ibge existe no dicionario de municipios"),
     nrow(o[!mun, on = .(sg_ue, id_municipio_ibge)]) == 0L,
     paste(nrow(o[!mun, on = .(sg_ue, id_municipio_ibge)]), "linhas fora"))
  ck(P("16 cpf so quando tem 11 digitos"), o[!is.na(cpf), .N] == 0L || o[!is.na(cpf), all(nchar(cpf) == 11L)])
  ck(P("17 datas em ISO quando presentes"),
     o[!is.na(data_inicio), .N] == 0L || o[!is.na(data_inicio), all(grepl("^\\d{4}-\\d{2}-\\d{2}$", data_inicio))])
  ck(P("18 data_inicio nunca posterior a data_fim"),
     o[!is.na(data_inicio) & !is.na(data_fim), .N] == 0L || o[!is.na(data_inicio) & !is.na(data_fim), all(data_inicio <= data_fim)])
  ck(P("19 metodo presente sempre que ha mandato pareado"), o[!is.na(id_mandato_bocel), all(!is.na(metodo_pareamento))])

  ## --- chave e granularidade
  # a tabela e um registro de linha da fonte e nao declara chave unica de linha; o que nao pode e uma
  # linha repetida contar duas vezes para um mandato, isto e, carregar saida observada
  n_dup_linha <- nrow(o) - nrow(unique(o))
  dupl <- o[duplicated(o) | duplicated(o, fromLast = TRUE)]
  ck(P("20 linha repetida nunca carrega saida observada nem pareamento distinto"),
     nrow(dupl) == 0L || dupl[, all(forma_saida == "nao_observado")],
     paste(n_dup_linha, "linhas repetidas em", nrow(o)))
  # a granularidade que o contrato com R/10 exige: no maximo uma saida observada por mandato
  obs <- o[!is.na(id_mandato_bocel) & forma_saida != "nao_observado"]
  checa_unica(as.data.frame(obs), "id_mandato_bocel")
  ck(P("21 no maximo uma saida observada por mandato pareado"), TRUE, paste(nrow(obs), "saidas observadas"))
  ck(P("22 um mandato pareado nunca aponta duas pessoas"),
     o[!is.na(id_mandato_bocel), uniqueN(id_pessoa_bocel), by = id_mandato_bocel][, all(V1 == 1L)])

  ## --- integridade referencial
  fora_m <- setdiff(o[!is.na(id_mandato_bocel)]$id_mandato_bocel, mand$id_mandato)
  ck(P("23 todo id_mandato_bocel existe em mandatos.csv"), length(fora_m) == 0L, paste(length(fora_m), "ids ausentes"))
  fora_p <- setdiff(o[!is.na(id_pessoa_bocel)]$id_pessoa_bocel, pess$id_pessoa)
  ck(P("24 todo id_pessoa_bocel existe em pessoas.csv"), length(fora_p) == 0L, paste(length(fora_p), "ids ausentes"))
  ck(P("25 id_pessoa_bocel e id_mandato_bocel aparecem juntos"),
     o[, sum(xor(is.na(id_pessoa_bocel), is.na(id_mandato_bocel)))] == 0L)

  p <- merge(o[!is.na(id_mandato_bocel)], mref, by.x = "id_mandato_bocel", by.y = "id_mandato", all.x = TRUE)
  ck(P("26 cargo do mandato pareado bate com cargo_bocel"),
     p[, all(m_cd == CD_DE[cargo_bocel])], paste(p[m_cd != CD_DE[cargo_bocel], .N], "divergencias"))
  ck(P("27 municipio do mandato pareado bate"), p[, all(m_ue == sg_ue)], paste(p[m_ue != sg_ue, .N], "divergencias"))
  ck(P("28 uf do mandato pareado bate"), p[, all(m_uf == uf)], paste(p[m_uf != uf, .N], "divergencias"))
  ck(P("29 id_pessoa_bocel e o titular do mandato pareado"), p[, all(m_pessoa == id_pessoa_bocel)],
     paste(p[m_pessoa != id_pessoa_bocel, .N], "divergencias"))
  ck(P("30 ano_eleicao da linha bate com o do mandato pareado"),
     p[!is.na(ano_eleicao), all(ano_eleicao == m_ano)], paste(p[!is.na(ano_eleicao) & ano_eleicao != m_ano, .N], "divergencias"))
  # 'exercicio' e o exercicio financeiro em toda fonte, MENOS no cadastro de servidores do TCE-PE, onde
  # a coluna carrega o AnoRemessa (ano em que o tribunal transmitiu o registro, quase sempre o corrente).
  # A distincao importa porque so o exercicio financeiro localiza o registro dentro do mandato.
  pe_ <- p[!is.na(exercicio)]
  fora_e <- pe_[!(as.integer(exercicio) >= as.integer(substr(m_ini, 1, 4)) &
                  as.integer(exercicio) <= as.integer(substr(m_fim, 1, 4)))]
  ck(P("31 exercicio fora do mandato so no cadastro do TCE-PE, onde a coluna e AnoRemessa"),
     nrow(fora_e) == 0L || all(fora_e$fonte == "tcepe_dados_abertos_lista_servidores"),
     paste(nrow(fora_e), "fora de", nrow(pe_), "; fontes:", paste(unique(fora_e$fonte), collapse = ",")))
  # e o ano de eleicao derivado da data de admissao, e nao o AnoRemessa, que localiza a linha do TCE-PE
  ppe <- p[fonte == "tcepe_dados_abertos_lista_servidores" & !is.na(data_inicio)]
  ck(P("31b no TCE-PE a eleicao vem da data de admissao, nunca do AnoRemessa"),
     nrow(ppe) == 0L || ppe[, all(as.integer(ano_eleicao) == ((as.integer(substr(data_inicio, 1, 4)) - 1L) %/% 4L) * 4L)],
     paste(nrow(ppe), "linhas do TCE-PE com data de admissao"))
  registrar_numero(paste0("tcev_", tolower(g), "_n_linhas_exercicio_fora_do_mandato"), nrow(fora_e), script = script)
  pcpf <- merge(p[grepl("^cpf_", metodo_pareamento)], pess[, .(id_pessoa, nr_cpf)],
                by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
  ck(P("32 pareamento por cpf confere com pessoas.nr_cpf"),
     nrow(pcpf) == 0L || pcpf[, all(so_dig(nr_cpf) == cpf)], paste(nrow(pcpf), "linhas por cpf"))

  ## --- datas dentro da janela do mandato (a janela que R/10 aplica: inicio-60, fim+45)
  jini <- function(x) as.character(as.IDate(x) - 60L); jfim <- function(x) as.character(as.IDate(x) + 45L)
  pi_ <- p[!is.na(data_inicio)]
  fora_i <- pi_[!(data_inicio >= jini(m_ini) & data_inicio <= m_fim)]
  ck(P("33 data_inicio dentro da janela do mandato"), nrow(fora_i) == 0L,
     paste(nrow(fora_i), "fora de", nrow(pi_)))
  pf_ <- p[!is.na(data_fim)]
  fora_f <- pf_[!(data_fim >= jini(m_ini) & data_fim <= jfim(m_fim))]
  ck(P("34 data_fim fora da janela so no limite declarado do cadastro de vinculo do TCE-PE"),
     nrow(fora_f) == 0L || all(fora_f$fonte == "tcepe_dados_abertos_lista_servidores"),
     paste(nrow(fora_f), "fora de", nrow(pf_), "; fontes:", paste(unique(fora_f$fonte), collapse = ",")))
  registrar_numero(paste0("tcev_", tolower(g), "_n_linhas_data_fim_fora_da_janela"), nrow(fora_f), script = script)

  ## --- cobertura
  ck(P("35 cobertura nunca pareia mais que o universo"), cb[, all(n_pareados <= n_bocel)],
     paste(cb[n_pareados > n_bocel, .N], "linhas com excesso"))
  ck(P("36 taxa da cobertura confere com a divisao"), cb[, all(abs(taxa - round(n_pareados / n_bocel, 4)) < 1e-9)])
  cds <- CD_DE[cfg$cargos]
  alvo <- mand[cd_cargo %in% cds & sg_uf %in% unique(o$uf)]
  ck(P("37 n_bocel da cobertura confere com mandatos.csv"), cb[, sum(n_bocel)] == alvo[, uniqueN(id_mandato)],
     paste(cb[, sum(n_bocel)], "vs", alvo[, uniqueN(id_mandato)]))
  ck(P("38 total pareado da cobertura confere com a base"),
     cb[, sum(n_pareados)] == o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)],
     paste(cb[, sum(n_pareados)], "vs", o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)]))
  ck(P("39 cobertura cobre toda uf presente na base"), setequal(unique(cb$uf), unique(o$uf)))

  ## --- a tabela nao pode ser mais antiga que o cache do qual foi construida
  mt_out <- file.info(cfg$f)$mtime
  arqs <- list.files("data_raw/tce", recursive = TRUE, full.names = TRUE)
  arqs <- arqs[grepl(paste0("^data_raw/tce/(", paste(tolower(unique(o$uf)), collapse = "|"), ")/"), arqs)]
  mt_cache <- if (length(arqs)) max(file.info(arqs)$mtime) else as.POSIXct(NA)
  ck(P("40 tabela mais recente que o cache das ufs que a compoem"),
     !is.na(mt_cache) && mt_out >= mt_cache,
     paste("tabela", format(mt_out, "%Y-%m-%d %H:%M"), "vs cache", format(mt_cache, "%Y-%m-%d %H:%M")))

  ## --- numeros da tabela
  pre <- paste0("tcev_", tolower(g), "_")
  registrar_numero(paste0(pre, "n_linhas"), nrow(o), script = script)
  registrar_numero(paste0(pre, "n_linhas_pareadas"), o[!is.na(id_mandato_bocel), .N], script = script)
  registrar_numero(paste0(pre, "n_mandatos_pareados"), o[!is.na(id_mandato_bocel), uniqueN(id_mandato_bocel)], script = script)
  registrar_numero(paste0(pre, "n_mandatos_com_saida_observada"), nrow(obs), script = script)
  registrar_numero(paste0(pre, "n_linhas_duplicadas_integrais"), n_dup_linha, script = script)
  registrar_numero(paste0(pre, "n_municipios"), o[, uniqueN(sg_ue)], script = script)
}

## ------------------------------------------------------------------ amostra de conferencia manual
## 25 pareamentos por tabela, sorteados entre as linhas pareadas, com o registro da fonte e o do BOCEL
## lado a lado para conferencia de nome, municipio, cargo e periodo.
cat("\n================= amostra de pareamentos =================\n")
amostras <- list()
for (g in names(TAB)) {
  o <- tabs[[g]]
  p <- merge(o[!is.na(id_mandato_bocel)], mref, by.x = "id_mandato_bocel", by.y = "id_mandato")
  p <- merge(p, pess[, .(id_pessoa, nome_bocel = nome, nome_urna_bocel = nome_urna_recente)],
             by.x = "id_pessoa_bocel", by.y = "id_pessoa", all.x = TRUE)
  set.seed(20260827)
  s <- p[sample(.N, 25L)]
  s <- s[, .(tabela = g, uf, fonte, sg_ue, nome_municipio_fonte, unidade_gestora, cargo_bocel,
             nome_fonte = nome, nome_bocel, nome_urna_bocel, exercicio, ano_eleicao,
             data_inicio, data_fim, forma_saida, metodo_pareamento,
             id_mandato_bocel, id_pessoa_bocel, m_cd, m_ue, m_ano, m_ini, m_fim, url)]
  amostras[[g]] <- s
  cat("\n--- tabela", g, "---\n")
  for (i in seq_len(nrow(s))) with(s[i], cat(sprintf(
    "%2d %s/%s %-13s | fonte: %-38s | bocel: %-38s | eleicao %s (mandato %s a %s) | %s | %s\n",
    i, uf, sg_ue, cargo_bocel, substr(nome_fonte, 1, 38), substr(nome_bocel, 1, 38),
    ifelse(is.na(ano_eleicao), m_ano, ano_eleicao), m_ini, m_fim, forma_saida, metodo_pareamento)))
}
am <- rbindlist(amostras)
fwrite(am, "output/verificacao/tce_amostra_pareamentos.csv", na = "NA")
ck("41 amostra de 50 pareamentos gravada", nrow(am) == 50L, "output/verificacao/tce_amostra_pareamentos.csv")
ck("42 na amostra, cargo, municipio, uf e eleicao batem com o mandato",
   am[, all(m_cd == CD_DE[cargo_bocel] & m_ue == sg_ue & (is.na(ano_eleicao) | ano_eleicao == m_ano))])
ck("43 na amostra, periodo da fonte cai dentro da janela do mandato",
   am[, all(is.na(data_inicio) | (data_inicio >= as.character(as.IDate(m_ini) - 60L) & data_inicio <= m_fim))])

## ------------------------------------------------------------------ quanto o TCE acrescenta de fato
## Reproduz a extracao que R/10 faz de cada fonte (id_mandato + forma, 'nao_observado' nao conta como
## forma observada, e a linha cujo fim cai fora da janela do mandato nao concorre) e conta os mandatos
## em que 'tce' e a unica fonte candidata a forma de saida.
cat("\n================= contribuicao propria do TCE =================\n")
ler <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("NA", "")) else NULL
cl  <- function(dt, n) if (!is.null(dt) && n %in% names(dt)) dt[[n]] else rep(NA_character_, if (is.null(dt)) 0L else nrow(dt))
d8  <- function(x) { x <- substr(as.character(x), 1, 10); fifelse(grepl("^\\d{4}-\\d{2}-\\d{2}$", x), x, NA_character_) }
jan <- mand[, .(id_mandato, mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]
cands <- function(id, fim, forma) {
  s <- data.table(id_mandato = id, fim = d8(fim), forma = forma)
  s <- s[!is.na(id_mandato) & id_mandato %in% mand$id_mandato]
  s[forma %in% c("nao_observado", ""), forma := NA_character_]
  s <- s[!is.na(forma)]
  if (!nrow(s)) return(character(0))
  s <- merge(s, jan, by = "id_mandato")
  s <- s[is.na(fim) | (as.IDate(fim) >= mi - 60L & as.IDate(fim) <= mf + 45L)]
  unique(s$id_mandato)
}
FONTES <- list()
x <- ler("data/munic_prefeitos.csv")
if (!is.null(x)) FONTES$ibge_munic <- cands(cl(x, "id_mandato_bocel"),
  fifelse(grepl("^\\d{4}$", cl(x, "ano_munic")), paste0(cl(x, "ano_munic"), "-12-31"), NA_character_),
  fifelse(cl(x, "status") == "outro_em_exercicio" & !is.na(cl(x, "nome_prefeito_munic")), "substituicao_inferida_munic", NA_character_))
x <- ler("data/mandatos_forma_saida_suplementar.csv")
if (!is.null(x)) FONTES$tse_suplementar <- cands(cl(x, "id_mandato_ordinario_afetado"), cl(x, "data_fim_inferida"),
  fifelse(cl(x, "momento") %in% "antes_da_posse", "nao_tomou_posse", "perda_do_mandato"))
x <- ler("data/cnpj_responsavel_prefeitura.csv")
if (!is.null(x)) FONTES$receita_cnpj <- cands(cl(x, "id_mandato_bocel"), cl(x, "data_referencia"),
  fifelse(cl(x, "status") == "outro_em_exercicio", "substituicao_inferida_munic", NA_character_))
x <- ler("data/datajud_mandatos_afetados.csv")
if (!is.null(x)) FONTES$datajud <- cands(cl(x, "id_mandato"), cl(x, "data"),
  fifelse(toupper(cl(x, "indicio")) %in% c("TRUE", "T", "1"), "cassacao", NA_character_))
x <- ler("data/wikipedia_prefeitos.csv"); w1 <- if (!is.null(x)) cands(cl(x, "id_mandato_bocel"), cl(x, "fim"), cl(x, "forma_saida")) else character(0)
x <- ler("data/wikipedia_estadual.csv");  w2 <- if (!is.null(x)) cands(cl(x, "id_mandato_bocel"), cl(x, "fim"), cl(x, "forma_saida")) else character(0)
FONTES$wikipedia <- union(w1, w2)
x <- ler("data/diarios_mandatos_saida.csv")
if (!is.null(x)) FONTES$diario_oficial <- cands(fcoalesce(cl(x, "id_mandato_bocel"), cl(x, "id_mandato")), cl(x, "data_fim_inferida"), cl(x, "forma_saida"))
x <- ler("data/wikidata_mandatos.csv")
if (!is.null(x)) FONTES$wikidata <- cands(cl(x, "id_mandato_bocel"), cl(x, "fim"), cl(x, "forma_saida"))
x <- ler("data/wikidata_obitos.csv")
if (!is.null(x)) FONTES$wikidata_obito <- cands(cl(x, "id_mandato"), cl(x, "data_morte"),
  fifelse(toupper(cl(x, "dentro_do_mandato")) %in% c("TRUE", "T", "1"), "falecimento", NA_character_))
x <- ler("data/exercicio_assembleias.csv")
if (!is.null(x)) FONTES$assembleia_api <- cands(cl(x, "id_mandato_bocel"), cl(x, "data_fim_exercicio"), cl(x, "forma_saida"))
x <- ler("data/exercicio_assembleias_historico.csv")
if (!is.null(x)) FONTES$assembleia_historico <- cands(cl(x, "id_mandato_bocel"), cl(x, "data_fim_exercicio"), cl(x, "forma_saida"))
x <- ler("data/exercicio_camaras_municipais.csv")
if (!is.null(x)) FONTES$sapl_municipal <- cands(cl(x, "id_mandato_bocel"), cl(x, "data_fim_mandato"), cl(x, "forma_saida"))
pc <- character(0)
for (f in c("data/exercicio_camaras_sem_sapl.csv", "data/exercicio_camaras_sem_sapl_2.csv")) {
  x <- ler(f); if (!is.null(x)) pc <- union(pc, cands(cl(x, "id_mandato_bocel"), cl(x, "data_fim_mandato"), cl(x, "forma_saida")))
}
FONTES$portal_camara <- pc
x <- ler("data/exercicio_senado.csv")
if (!is.null(x)) FONTES$senado_api <- cands(cl(x, "id_mandato"), cl(x, "data_fim_exercicio"), cl(x, "forma_saida"))
x <- ler("data/exercicio_camara.csv")
if (!is.null(x)) FONTES$camara_api <- cands(cl(x, "id_mandato"), cl(x, "data_fim_exercicio"), cl(x, "forma_saida"))
tce <- character(0); tce_por_tab <- list()
for (g in names(TAB)) {
  o <- tabs[[g]]
  k <- cands(o$id_mandato_bocel, o$data_fim, o$forma_saida)
  tce_por_tab[[g]] <- k; tce <- union(tce, k)
}
outras <- unique(unlist(FONTES, use.names = FALSE))
so_tce <- setdiff(tce, outras)
cat("\ncandidatos por fonte (mandatos com forma de saida observada que sobrevive a janela):\n")
print(data.table(fonte = c(names(FONTES), "tce"), n = c(vapply(FONTES, length, integer(1)), length(tce)))[order(-n)])
cat("\ncandidatos do tce por tabela: A =", length(tce_por_tab$A), " B =", length(tce_por_tab$B), "\n")
cat("mandatos em que o tce e a unica fonte candidata:", length(so_tce), "de", length(tce), "\n")
sob <- mand[id_mandato %in% so_tce, .N, by = .(sg_uf, cargo, fonte_forma_saida)][order(-N)]
print(sob)
cat("\nsobreposicao do tce com cada outra fonte:\n")
print(data.table(fonte = names(FONTES), n_em_comum = vapply(FONTES, function(k) length(intersect(k, tce)), integer(1)))[order(-n_em_comum)])
fwrite(mand[id_mandato %in% so_tce, .(id_mandato, sg_uf, sg_ue = unidade_posicao, cargo, ano_eleicao,
                                      forma_saida, fonte_forma_saida)],
       "output/verificacao/tce_mandatos_exclusivos.csv", na = "NA")
ck("44 todo mandato exclusivo do tce esta em mandatos.csv com fonte tce",
   mand[id_mandato %in% so_tce, all(fonte_forma_saida %in% "tce")],
   paste(mand[id_mandato %in% so_tce & !fonte_forma_saida %in% "tce", .N], "com outra fonte prevalecendo"))
ck("45 a fonte tce em mandatos.csv nao excede os candidatos do tce",
   mand[fonte_forma_saida == "tce", .N] <= length(tce),
   paste(mand[fonte_forma_saida == "tce", .N], "em mandatos.csv vs", length(tce), "candidatos"))

registrar_numero("tcev_n_mandatos_candidatos_tce", length(tce), script = script)
registrar_numero("tcev_n_mandatos_candidatos_tce_a", length(tce_por_tab$A), script = script)
registrar_numero("tcev_n_mandatos_candidatos_tce_b", length(tce_por_tab$B), script = script)
registrar_numero("tcev_n_mandatos_so_tce", length(so_tce), script = script)
registrar_numero("tcev_pct_mandatos_so_tce", round(length(so_tce) / max(length(tce), 1L), 4), script = script)
registrar_numero("tcev_n_mandatos_tce_em_mandatos_csv", mand[fonte_forma_saida == "tce", .N], script = script)
for (nm in names(FONTES)) registrar_numero(paste0("tcev_n_sobreposicao_tce_", nm), length(intersect(FONTES[[nm]], tce)), script = script)

## ------------------------------------------------------------------ grupo A: por que nao ha saida
## As nove fontes do grupo A nao trazem data de fim de gestao; o campo de data que existe (ES) e a data
## da sessao de julgamento das contas. Registra-se aqui a contagem que sustenta o diagnostico.
cat("\n================= grupo A: forma de saida =================\n")
oa <- tabs$A
print(oa[, .(linhas = .N, com_data_inicio = sum(!is.na(data_inicio)), com_data_fim = sum(!is.na(data_fim)),
             com_exercicio = sum(!is.na(exercicio))), by = fonte][order(-linhas)])
print(oa[, .N, by = .(forma_saida, pareada = !is.na(id_mandato_bocel))])
print(oa[, .N, by = relacao_chapa_eleita][order(-N)])
ck("46 nenhuma fonte do grupo A traz periodo de gestao: data_inicio e data_fim sempre ausentes",
   oa[, all(is.na(data_inicio)) && all(is.na(data_fim))],
   paste(oa[!is.na(data_inicio) | !is.na(data_fim), .N], "linhas com data"))
ck("47 a data da sessao de julgamento do ES fica em situacao_fonte, fora de qualquer coluna de data",
   oa[fonte == "ckan_es_julgamento_de_contas_vereadores", all(grepl("sessao de julgamento de \\d{4}-\\d{2}-\\d{2}", situacao_fonte))])
ck("48 no grupo A toda forma de saida observada esta em linha nao pareada",
   oa[forma_saida != "nao_observado", all(is.na(id_mandato_bocel))],
   paste(oa[forma_saida != "nao_observado" & !is.na(id_mandato_bocel), .N], "pareadas com saida"))
registrar_numero("tcev_a_n_linhas_forma_saida_outro", oa[forma_saida == "outro", .N], script = script)
registrar_numero("tcev_a_n_linhas_forma_saida_outro_pareadas", oa[forma_saida == "outro" & !is.na(id_mandato_bocel), .N], script = script)
registrar_numero("tcev_a_n_mandatos_titular_substituido_pelo_vice", oa[!is.na(id_mandato_titular_substituido), uniqueN(id_mandato_titular_substituido)], script = script)
registrar_numero("tcev_a_n_linhas_com_data", oa[!is.na(data_inicio) | !is.na(data_fim), .N], script = script)
registrar_numero("tcev_a_n_fontes_com_data_de_fim_de_gestao", 0L, script = script)

## O que a reatribuicao renderia, se a decisao for que 'vice eleito respondendo pela prefeitura' e saida do
## titular: os mandatos de prefeito nomeados em id_mandato_titular_substituido que hoje nao tem forma de
## saida de nenhuma fonte. Fica medido, nao aplicado: a fonte nao distingue saida definitiva de licenca.
sub <- unique(na.omit(oa$id_mandato_titular_substituido))
sub_sem_saida <- mand[id_mandato %in% sub & forma_saida == "nao_observado", id_mandato]
cat("\nmandatos de titular substituido pelo vice:", length(sub),
    "| hoje sem forma de saida de nenhuma fonte:", length(sub_sem_saida), "\n")
print(mand[id_mandato %in% sub, .N, by = .(sg_uf, forma_saida, fonte_forma_saida)][order(-N)])
ck("49 todo mandato de titular substituido e de prefeito", mand[id_mandato %in% sub, all(cd_cargo == "11")])
ck("50 o titular substituido nunca e a propria pessoa da linha do vice",
   nrow(oa[!is.na(id_mandato_titular_substituido) & !is.na(id_pessoa_bocel)]) == 0L ||
   merge(oa[!is.na(id_mandato_titular_substituido) & !is.na(id_pessoa_bocel),
            .(id_mandato_titular_substituido, id_pessoa_bocel)],
         mand[, .(id_mandato, tit_pessoa = id_pessoa)],
         by.x = "id_mandato_titular_substituido", by.y = "id_mandato")[, all(tit_pessoa != id_pessoa_bocel)])
registrar_numero("tcev_a_n_titulares_substituidos_sem_saida_hoje", length(sub_sem_saida), script = script)

## ------------------------------------------------------------------ conferencia ao vivo (python/confere_tce_ao_vivo.py)
fv <- "output/verificacao/tce_conferencia_ao_vivo.csv"
if (file.exists(fv)) {
  cv <- fread(fv, colClasses = "character", na.strings = "NA")
  cat("\n================= conferencia contra a fonte ao vivo =================\n")
  print(cv[, .(uf, fonte, nome_fonte, http, confirmado)])
  ck("51 conferencia ao vivo cobre 10 pareamentos", nrow(cv) == 10L, paste(nrow(cv), "linhas"))
  ck("52 toda conferencia ao vivo respondeu http 200", cv[, all(http == "200")],
     paste(cv[http != "200", .N], "sem resposta"))
  ck("53 todo pareamento conferido bate com a fonte ao vivo", cv[, all(confirmado == "sim")],
     paste(cv[confirmado != "sim", .N], "nao confirmados"))
  registrar_numero("tcev_n_conferidos_ao_vivo", nrow(cv), script = script)
  registrar_numero("tcev_n_conferidos_ao_vivo_confirmados", cv[confirmado == "sim", .N], script = script)
  registrar_numero("tcev_n_fontes_conferidas_ao_vivo", cv[, uniqueN(fonte)], script = script)
} else {
  ck("51 conferencia ao vivo cobre 10 pareamentos", FALSE, "rode python3 python/confere_tce_ao_vivo.py")
}

## ------------------------------------------------------------------ fecho
registrar_numero("tcev_n_checagens", length(passou) + length(falhou), script = script)
registrar_numero("tcev_n_aprovadas", length(passou), script = script)
registrar_numero("tcev_n_reprovadas", length(falhou), script = script)
f <- gravar_relatorio_verificacao(
  alvo = "tabelas de TCE ja construidas: data/tce_gestores.csv (grupo A) e data/tce_gestores_b.csv (grupo B)",
  script = script, passou = passou, falhou = falhou,
  fora_de_cobertura = c(
    "completude do cadastro do tribunal: a fonte lista quem prestou ou teve contas julgadas, nao todo agente que exerceu",
    "pertinencia do pareamento por tokens de nome quando a fonte nao expoe cpf",
    "identidade de homonimos no mesmo municipio, cargo e eleicao",
    "veracidade do campo Gestor/Responsavel tal como o tribunal o preenche",
    "no grupo A, forma de saida: as fontes observam exercicio financeiro e julgamento de contas, nao desligamento",
    "se o vice que responde pela prefeitura assumiu por renuncia, cassacao, falecimento, afastamento ou licenca",
    "no grupo B, se o ultimo mes de folha do Sagres (PB) e saida ou falha de remessa da unidade gestora"))
cat("\nrelatorio:", f, "\naprovadas:", length(passou), " reprovadas:", length(falhou), "\n")
if (length(falhou)) { cat("REPROVADO\n"); for (x in falhou) cat("  -", x, "\n"); sink(); quit(status = 1) }
cat("VERIFICADO\n"); sink()
