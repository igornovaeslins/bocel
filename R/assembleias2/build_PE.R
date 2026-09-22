#!/usr/bin/env Rscript
# build_PE.R — exercicio de mandato dos deputados estaduais de Pernambuco a partir do registro
#   da propria Assembleia Legislativa do Estado de Pernambuco (ALEPE), com pareamento ao Banco
#   de Ocupacao de Cargos Eletivos no Brasil (BOCEL).
#
# Fonte primaria — ATAS das reunioes plenarias publicadas no Diario Oficial do Poder Legislativo
#   (acervo digital da Casa, 2005 em diante; 4.008 edicoes em texto, das quais 2.607 trazem ata).
#   Cada ata abre com a relacao nominal dos deputados presentes e fecha com as ausencias
#   justificadas, as ausencias e os licenciados. A serie dessas relacoes da a composicao em
#   exercicio ao longo da legislatura e o momento em que cada parlamentar deixa de figurar nela,
#   que e o mesmo expediente que rendeu a serie do Parana.
# Fonte complementar 1 — retratos datados do portal guardados no Internet Archive: 126 versoes
#   de /parlamentares/ entre 2015 e 2026 e 30 versoes das quatro paginas alfabeticas de
#   /gabinete/deputados*.html entre 2000 e 2003, estas o unico registro da 14a legislatura, que
#   o acervo do Diario Oficial nao alcanca.
# Fonte complementar 2 — /parlamentares-anteriores/, que publica a relacao da 19a legislatura e
#   marca com asterisco quem entrou como suplente. E a distincao que o portal do Rio tambem
#   trazia entre quem compos a legislatura e quem ocupou a vaga de outro.
# Fonte complementar 3 — texto integral do DO e acervo de noticias da Casa (4.902 materias de
#   1999 em diante), de onde saem os eventos nomeados: efetivacao de suplente, licenca, renuncia,
#   falecimento e cassacao. O ato que efetiva o suplente e a fonte mais direta, porque declara de
#   quem era a cadeira e por que ela vagou.
# Fonte complementar 4 — /eleicoes2002/deputadosEstaduais.html no Internet Archive, apuracao
#   publicada pela propria Casa e unica fonte da ALEPE com votacao nominal (preenche votos_fonte).
#
# Entrada:  data_raw/assembleias2/PE/{atas_presenca,retratos_wayback,portal_fichas,
#             anteriores_19,eventos_texto,eleicoes2002}.csv
#           data/mandatos.csv, data/pessoas.csv, data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/assembleias2/PE.csv (21 colunas canonicas)
#           output/verificacao/asm2pe_*.csv, output/numeros_assinatura.txt
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/build_PE.R
set.seed(20260830)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
raw  <- file.path(root, "data_raw", "assembleias2", "PE")
outd <- file.path(root, "data", "assembleias2")
verd <- file.path(root, "output", "verificacao")
dir.create(outd, showWarnings = FALSE, recursive = TRUE)
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(root, "logs"), showWarnings = FALSE, recursive = TRUE)
script <- file.path(root, "R", "assembleias2", "build_PE.R")
logf   <- file.path(root, "logs", "asm2pe_build_PE.log")
sink(logf, split = TRUE)
cat("build_PE.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(paste0("asm2pe_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")

UF    <- "PE"
HOJE  <- as.IDate("2026-08-30")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
           "nao_tomou_posse", "suplente_efetivado", "assumiu_titular", "outro", "nao_observado")
# A 14a legislatura inaugura o recorte do banco: eleicao de 1998, posse em 1o de fevereiro de 1999.
LEG <- data.table(
  legislatura = 14:20,
  ano_eleicao = seq(1998L, 2022L, 4L),
  leg_inicio  = as.IDate(paste0(seq(1999L, 2023L, 4L), "-02-01")),
  leg_fim     = as.IDate(paste0(seq(2003L, 2027L, 4L), "-01-31")))
# janela de tolerancia no fim da legislatura: as ultimas reunioes ordinarias ocorrem em dezembro,
# e o mandato so termina em 31 de janeiro
JANELA <- 120L
# minimo de reunioes observadas DEPOIS da ultima aparicao para tratar o sumico como saida, e nao
# como buraco do acervo
MIN_DEPOIS <- 6L

norm_nome <- function(x) {
  x <- stri_trans_general(toupper(x), "Latin-ASCII")
  x <- gsub("&[A-Z]+;", " ", x)
  x <- gsub("[^A-Z ]", " ", x)
  gsub(" +", " ", trimws(x))
}
# titulos que a urna carrega e a ata nao (ou o contrario): "MISSIONARIA DILMA LINS" e
# "DILMA LINS" sao a mesma pessoa, e "CORONEL ALBERTO FEITOSA" e "ALBERTO FEITOSA" tambem
TITULOS <- c("PROFESSOR", "PROFESSORA", "PROF", "MISSIONARIA", "MISSIONARIO", "PASTOR",
             "PASTORA", "BISPO", "PADRE", "IRMA", "IRMAO", "DOUTOR", "DOUTORA", "DR", "DRA",
             "CORONEL", "TENENTE", "MAJOR", "CAPITAO", "SARGENTO", "SOLDADO", "CABO",
             "DELEGADO", "DELEGADA", "INSPETOR", "VEREADOR", "VEREADORA", "DEPUTADO",
             "DEPUTADA", "ENFERMEIRA", "ENFERMEIRO", "ADVOGADO", "ADVOGADA", "SENHOR",
             "PRESBITERO", "PRESBITERA", "REVERENDO", "APOSTOLO", "EVANGELISTA", "MISSIONARI",
             "MESTRE", "COMANDANTE", "SUBTENENTE", "GUARDA", "POLICIAL", "BOMBEIRO", "AGENTE",
             "MEDICO", "MEDICA", "DENTISTA", "ENGENHEIRO", "JORNALISTA", "RADIALISTA",
             "TIA", "TIO", "DONA", "SEU", "SARGENTA", "MAJORA")
PARTICULAS <- c("DE", "DA", "DO", "DAS", "DOS", "E")
sem_titulo <- function(x) {
  y <- vapply(strsplit(x, " ", fixed = TRUE), function(v) {
    while (length(v) > 1L && v[1] %in% TITULOS) v <- v[-1]
    paste(v, collapse = " ")
  }, character(1))
  y[is.na(x)] <- NA_character_
  y
}
tokens <- function(x) lapply(strsplit(x, " ", fixed = TRUE),
                             function(v) v[nchar(v) >= 3L & !v %in% PARTICULAS])

le <- function(f, ...) {
  p <- file.path(raw, f)
  if (!file.exists(p) || file.size(p) < 5) return(data.table())
  fread(p, encoding = "UTF-8", na.strings = c("NA", ""), colClasses = "character", ...)
}

## ---------------------------------------------------------------- 1. relacao nominal das atas
atas <- le("atas_presenca.csv")
stopifnot(nrow(atas) > 0)
atas[, `:=`(data_do = as.IDate(data_do), data_reuniao = as.IDate(data_reuniao))]
atas[, data_ref := fifelse(!is.na(data_reuniao) & data_reuniao >= as.IDate("1999-01-01") &
                             data_reuniao <= HOJE, data_reuniao, data_do)]
atas[, nome_norm := norm_nome(nome_ata)]
atas <- atas[nchar(nome_norm) >= 4 & !is.na(data_ref)]
atas[LEG, on = .(data_ref >= leg_inicio, data_ref <= leg_fim),
     `:=`(legislatura = i.legislatura, ano_eleicao = i.ano_eleicao,
          leg_inicio = i.leg_inicio, leg_fim = i.leg_fim)]
atas <- atas[!is.na(legislatura)]
cat("observacoes de ata dentro das legislaturas 14-20:", nrow(atas), "\n")
reg("n_obs_ata", nrow(atas))
reg("n_edicoes_do_com_ata", uniqueN(atas$arquivo))
reg("n_reunioes_distintas_ata", uniqueN(atas[, .(legislatura, data_ref)]))
reg("faixa_datas_ata", paste(format(range(atas$data_ref)), collapse = ".."))

# 'em exercicio' = figura na relacao de presentes, ausentes ou ausencias justificadas.
# 'licenciado' e situacao a parte: o parlamentar existe na Casa mas nao esta no exercicio.
atas[, em_exercicio := tipo_lista %in% c("presente", "ausente", "ausencia_justificada")]
# o calendario de observacao e montado adiante, ja com os retratos do Internet Archive

## ---------------------------------------------------------------- 2. retratos datados
retr <- le("retratos_wayback.csv")
if (nrow(retr)) {
  retr[, data_ref := as.IDate(data_retrato)]
  retr[, nome_norm := norm_nome(nome)]
  retr <- retr[nchar(nome_norm) >= 4 & !is.na(data_ref)]
  retr[LEG, on = .(data_ref >= leg_inicio, data_ref <= leg_fim),
       `:=`(legislatura = i.legislatura, ano_eleicao = i.ano_eleicao,
            leg_inicio = i.leg_inicio, leg_fim = i.leg_fim)]
  retr <- retr[!is.na(legislatura)]
  # o quadro de 2000-2003 sai em quatro paginas alfabeticas: cada uma cobre uma fatia do
  # alfabeto, e por isso o retrato completo tem 12, nao 49 nomes. O piso de nomes por retrato
  # depende do painel, e a comparacao entre retratos so vale dentro do mesmo painel.
  PISO <- c(parlamentares = 20L)
  retr[, piso := fifelse(painel == "parlamentares", 20L, 5L)]
  bons <- retr[, .(n = .N, piso = piso[1]), by = .(painel, timestamp)][n >= piso]
  retr <- retr[bons[, .(painel, timestamp)], on = .(painel, timestamp)]
  retr[, licenciado_portal := grepl("licenciado", slug, fixed = TRUE)]
}
cat("pares (retrato, nome) aproveitados:", nrow(retr), "| retratos:",
    if (nrow(retr)) uniqueN(retr[, .(timestamp)]) else 0L, "\n")
reg("n_obs_retrato_wayback", nrow(retr))
reg("n_retratos_wayback", if (nrow(retr)) uniqueN(retr$timestamp) else 0L)

## ---------------------------------------------------------------- 3. serie por pessoa
obs <- rbindlist(list(
  atas[, .(legislatura, ano_eleicao, leg_inicio, leg_fim, data_ref, nome_norm,
           nome = nome_ata, origem = "ata", painel = "ata", em_exercicio,
           licenciado = tipo_lista == "licenciado",
           ref = pasta, partido_retrato = NA_character_)],
  if (nrow(retr)) retr[, .(legislatura, ano_eleicao, leg_inicio, leg_fim, data_ref, nome_norm,
                           nome, origem = "retrato", painel,
                           em_exercicio = !licenciado_portal,
                           licenciado = licenciado_portal,
                           ref = timestamp, partido_retrato = partido)] else NULL),
  use.names = TRUE, fill = TRUE)

# A mesma pessoa aparece com e sem o titulo que usa na tribuna — "DELEGADO ERICK LESSA" numa
# ata e "ERICK LESSA" noutra. Sem consolidar, a serie se parte em duas e uma das metades fica
# sem par. Dentro de cada legislatura, as variantes que coincidem apos remover o titulo viram a
# forma mais frequente.
consolida <- function(chave_f, rotulo) {
  obs[, chv := chave_f(nome_norm)]
  cn <- obs[, .N, by = .(legislatura, chv, nome_norm)]
  setorder(cn, legislatura, chv, -N)
  cn <- cn[, .(canon = nome_norm[1], variantes = .N), by = .(legislatura, chv)]
  n <- cn[variantes > 1L, sum(variantes - 1L)]
  obs[cn, on = .(legislatura, chv), nome_norm := i.canon]
  obs[, chv := NULL]
  cat(sprintf("  variantes consolidadas por %-22s %d\n", rotulo, n))
  n
}
cat("consolidacao de variantes do mesmo nome:\n")
v1 <- consolida(sem_titulo, "titulo honorifico")
# a extracao do PDF as vezes parte o nome ao meio ("R ODRIGO NOVAES"); a forma sem espaco
# reencontra a mesma pessoa sem afrouxar o criterio, porque as letras sao exatamente as mesmas
v2 <- consolida(function(x) gsub(" ", "", sem_titulo(x), fixed = TRUE), "espacamento do PDF")
reg("n_variantes_de_nome_consolidadas", v1 + v2)

# calendario de observacao por legislatura: as datas em que a fonte disse quem estava na Casa.
# E ele que separa saida de buraco do acervo — sem reuniao publicada depois da ultima aparicao,
# o sumico e do arquivo, nao do parlamentar.
cal <- unique(obs[, .(legislatura, painel, data_ref)])[order(legislatura, painel, data_ref)]
reg("n_datas_observacao", nrow(cal))
reg("n_paineis", uniqueN(cal$painel))

pes <- obs[, .(n_obs = .N,
               n_datas = uniqueN(data_ref),
               n_ata = sum(origem == "ata"),
               n_retrato = sum(origem == "retrato"),
               primeira = min(data_ref),
               ultima = max(data_ref),
               ultima_exerc = suppressWarnings(max(data_ref[em_exercicio])),
               ultima_licenc = suppressWarnings(max(data_ref[licenciado])),
               ref_ultima = ref[which.max(data_ref)],
               origem_ultima = origem[which.max(data_ref)],
               painel_ultima = painel[which.max(data_ref)],
               nome = nome[which.max(nchar(nome))],
               paineis = paste(sort(unique(painel)), collapse = "|"),
               partido_retrato = {
                 v <- partido_retrato[!is.na(partido_retrato) & nzchar(partido_retrato)]
                 if (!length(v)) NA_character_ else v[length(v)]
               },
               leg_inicio = leg_inicio[1], leg_fim = leg_fim[1], ano_eleicao = ano_eleicao[1]),
           by = .(legislatura, nome_norm)]
pes[!is.finite(ultima_exerc), ultima_exerc := NA]
pes[!is.finite(ultima_licenc), ultima_licenc := NA]
cat("pares (legislatura, nome) observados:", nrow(pes), "\n")

## ---------------------------------------------------------------- 4. BOCEL e cadastro do TSE
bocel_m <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = "character")
bocel_p <- fread(file.path(root, "data", "pessoas.csv"), na.strings = "NA", encoding = "UTF-8",
               colClasses = "character")
dep <- bocel_m[cd_cargo == "7" & sg_uf == UF,
             .(id_mandato, id_pessoa, ano_eleicao = as.integer(ano_eleicao), sq_candidato,
               nr_candidato,
               mandato_inicio = as.IDate(mandato_inicio), mandato_fim = as.IDate(mandato_fim))]
dep <- merge(dep, bocel_p[, .(id_pessoa, nome_bocel = nome, dt_nascimento)], by = "id_pessoa",
             all.x = TRUE)
dep[, nome_bocel_norm := norm_nome(nome_bocel)]
cat("mandatos BOCEL de deputado estadual em", UF, ":", nrow(dep), "\n")
stopifnot(nrow(dep) == 343L)

# cadastro de candidaturas do TSE: nome de urna, nome civil, nascimento, sexo, partido, situacao
cands <- rbindlist(lapply(seq(1998L, 2022L, 4L), function(a) {
  f <- file.path(root, "data_raw", "parquet", sprintf("cand_%d.parquet", a))
  if (!file.exists(f)) return(NULL)
  x <- setDT(read_parquet(f, col_select = c("ANO_ELEICAO", "SG_UF", "CD_CARGO", "NR_CANDIDATO",
                                            "SQ_CANDIDATO", "NM_URNA_CANDIDATO", "NM_CANDIDATO",
                                            "DS_SIT_TOT_TURNO", "NM_TIPO_ELEICAO",
                                            "DT_NASCIMENTO", "DS_GENERO", "SG_PARTIDO")))
  x <- x[SG_UF == UF & as.integer(CD_CARGO) == 7L & grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, .(ano_eleicao = as.integer(ANO_ELEICAO), sq_candidato = as.character(SQ_CANDIDATO),
        nome_urna = NM_URNA_CANDIDATO, nome_civil = NM_CANDIDATO, situacao = DS_SIT_TOT_TURNO,
        dt_nasc = DT_NASCIMENTO, genero = DS_GENERO, partido_tse = SG_PARTIDO)]
}))
cands <- unique(cands, by = c("ano_eleicao", "sq_candidato"))
cands[, `:=`(nome_urna_norm = norm_nome(nome_urna), nome_civil_norm = norm_nome(nome_civil))]
cands[, eleito := grepl("^ELEITO", toupper(stri_trans_general(situacao, "Latin-ASCII")))]
cat("candidaturas de deputado estadual em", UF, "1998-2022:", nrow(cands),
    "| eleitas:", cands[eleito == TRUE, .N], "\n")
dep <- merge(dep, cands[, .(ano_eleicao, sq_candidato, nome_urna, nome_urna_norm,
                            nome_civil_tse = nome_civil_norm, dt_nasc_tse = dt_nasc,
                            genero_tse = genero, partido_tse)],
             by = c("ano_eleicao", "sq_candidato"), all.x = TRUE)
dep <- merge(dep, cands[, .(ano_eleicao, sq_candidato, nome_civil_bruto = nome_civil)],
             by = c("ano_eleicao", "sq_candidato"), all.x = TRUE)
reg("n_mandatos_bocel_cd7_PE", nrow(dep))
reg("n_mandatos_bocel_com_nome_urna", dep[!is.na(nome_urna), .N])

## ---------------------------------------------------------------- 5. nome civil das fontes
fichas <- le("portal_fichas.csv")
if (nrow(fichas)) fichas[, nome_norm := norm_nome(nome_parlamentar)]
ante <- le("anteriores_19.csv")
if (nrow(ante)) ante[, `:=`(nome_norm = norm_nome(nome), legislatura = as.integer(legislatura))]

pes[, `:=`(nome_completo = NA_character_, data_nasc_fonte = NA_character_,
           partido_fonte = NA_character_, condicao_portal = NA_character_,
           url_portal = NA_character_)]
if (nrow(fichas)) {
  pes[fichas, on = .(nome_norm), `:=`(nome_completo = i.nome_civil,
                                      data_nasc_fonte = i.data_nascimento,
                                      partido_fonte = i.partido,
                                      url_portal = i.url)]
  pes[legislatura != 20L, `:=`(nome_completo = NA_character_, data_nasc_fonte = NA_character_,
                               partido_fonte = NA_character_, url_portal = NA_character_)]
}
if (nrow(ante)) {
  pes[ante, on = .(legislatura, nome_norm), `:=`(condicao_portal = i.condicao_portal,
                                                 partido_fonte = i.partido,
                                                 url_portal = i.url)]
}
pes[nome_completo == "", nome_completo := NA_character_]
pes[, nome_completo_norm := norm_nome(nome_completo)]
pes[nome_completo_norm == "", nome_completo_norm := NA_character_]

# apuracao de 2002 publicada pela propria Casa: e a unica fonte da ALEPE com votacao nominal.
# A juncao e pelo NUMERO de urna, nao pelo nome — a copia arquivada perdeu os acentos, e o
# numero e chave exata.
ap02 <- le("eleicoes2002.csv")
if (nrow(ap02)) {
  ap02 <- unique(ap02[, .(nr_candidato = numero, votos_alepe = votos,
                          partido_alepe = partido, url_apuracao = url)], by = "nr_candidato")
  dep <- merge(dep, ap02, by = "nr_candidato", all.x = TRUE)
  dep[ano_eleicao != 2002L, `:=`(votos_alepe = NA_character_, partido_alepe = NA_character_,
                                 url_apuracao = NA_character_)]
  cat("mandatos com votacao publicada pela ALEPE (apuracao de 2002):",
      dep[!is.na(votos_alepe), .N], "\n")
  reg("n_votos_da_apuracao_alepe", dep[!is.na(votos_alepe), .N])
} else {
  dep[, `:=`(votos_alepe = NA_character_, partido_alepe = NA_character_,
             url_apuracao = NA_character_)]
}

## ---------------------------------------------------------------- 6. pareamento ao BOCEL
# Mesma ordem de regras de R/13_exercicio_assembleias.R: o nome civil da fonte primeiro, o nome
# parlamentar depois, e cada regra exige unicidade dos dois lados dentro de (ano de eleicao,
# chave). Nome de urna raramente e o nome civil, e nome curto gera falso positivo — a regra so
# fecha o par quando a chave identifica uma pessoa so em cada lado.
pes[, rid := .I]
pes[, `:=`(id_mandato = NA_character_, id_pessoa = NA_character_,
           metodo_pareamento = NA_character_)]
parear <- function(col_pes, col_dep, metodo, min_car = 6L) {
  a <- pes[is.na(id_mandato) & !is.na(get(col_pes)) & nchar(get(col_pes)) >= min_car,
           .(rid, ano_eleicao, chave = get(col_pes))]
  if (!nrow(a)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- dep[!is.na(get(col_dep)) & nchar(get(col_dep)) >= min_car,
           .(id_mandato, id_pessoa, ano_eleicao, chave = get(col_dep))]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  m <- m[!id_mandato %in% pes$id_mandato]
  if (nrow(m)) pes[m, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                                       metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
  nrow(m)
}
cat("pareamento ao BOCEL, por regra:\n")
invisible(parear("nome_completo_norm", "nome_bocel_norm",  "nome_completo_x_nome_bocel"))
invisible(parear("nome_norm",          "nome_urna_norm", "nome_parlamentar_x_urna"))
invisible(parear("nome_norm",          "nome_bocel_norm",  "nome_parlamentar_x_nome_bocel"))
invisible(parear("nome_completo_norm", "nome_urna_norm", "nome_completo_x_urna"))
# titulo honorifico: a urna registra "MISSIONARIA DILMA LINS", a ata escreve "DILMA LINS"
pes[, nome_st := sem_titulo(nome_norm)]
cands[, `:=`(nome_urna_st = sem_titulo(nome_urna_norm))]
dep[, `:=`(urna_st = sem_titulo(nome_urna_norm), bocel_st = sem_titulo(nome_bocel_norm))]
invisible(parear("nome_st", "urna_st", "nome_sem_titulo_x_urna_sem_titulo"))
invisible(parear("nome_st", "bocel_st",  "nome_sem_titulo_x_nome_bocel"))

# subsequencia de tokens: o nome parlamentar e o primeiro nome mais um sobrenome do nome civil
# ("ROMARIO DIAS" dentro de "ROMARIO DE CASTRO DIAS PEREIRA"). A regra exige que o PRIMEIRO
# token coincida e que todos os demais tokens do nome parlamentar estejam no nome civil, e so
# fecha o par quando a correspondencia e unica dos dois lados dentro do ano de eleicao.
parear_subseq <- function(col_alvo, metodo, ancora_inicio = TRUE) {
  a <- pes[is.na(id_mandato) & !is.na(nome_st) & nchar(nome_st) >= 8L,
           .(rid, ano_eleicao, chave = nome_st)]
  if (!nrow(a)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  b <- dep[!is.na(get(col_alvo)) & nchar(get(col_alvo)) >= 8L &
             !id_mandato %in% pes$id_mandato,
           .(id_mandato, id_pessoa, ano_eleicao, alvo = get(col_alvo))]
  if (!nrow(b)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  ta <- tokens(a$chave); tb <- tokens(b$alvo)
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    v <- ta[[i]]
    if (length(v) < 2L) return(NULL)
    if (!ancora_inicio && any(nchar(v) < 4L)) return(NULL)
    j <- which(b$ano_eleicao == a$ano_eleicao[i] &
                 vapply(tb, function(w) {
                   if (length(w) < 2L) return(FALSE)
                   if (ancora_inicio) identical(w[1], v[1]) && all(v[-1] %in% w[-1])
                   else { pos <- match(v, w); !anyNA(pos) && !is.unsorted(pos, strictly = TRUE) }
                 }, logical(1)))
    if (!length(j)) return(NULL)
    data.table(rid = a$rid[i], k = j)
  }))
  if (!nrow(pares)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  pares <- pares[pares[, .N, by = rid][N == 1L], on = "rid"][, .(rid, k)]
  pares <- pares[pares[, .N, by = k][N == 1L], on = "k"][, .(rid, k)]
  if (!nrow(pares)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  pares[, `:=`(id_mandato = b$id_mandato[k], id_pessoa = b$id_pessoa[k])]
  pes[pares, on = "rid", `:=`(id_mandato = i.id_mandato, id_pessoa = i.id_pessoa,
                              metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(pares)))
  nrow(pares)
}
invisible(parear_subseq("nome_bocel_norm", "primeiro_e_sobrenome_x_nome_bocel"))
invisible(parear_subseq("nome_civil_tse", "primeiro_e_sobrenome_x_civil_tse"))
# ultima regra: o nome parlamentar e uma subsequencia ORDENADA do nome civil, sem exigir que
# comece nele — "EDUINO BRITO" dentro de "JOSE EDUINO DE BRITO CAVALCANTI". Cada vocabulo
# precisa de quatro letras, e a correspondencia continua tendo de ser unica dos dois lados.
invisible(parear_subseq("nome_bocel_norm", "subsequencia_x_nome_bocel", ancora_inicio = FALSE))

# quem nao e titular: procura a mesma pessoa entre as candidaturas NAO eleitas do mesmo ano,
# que e o que caracteriza o suplente convocado
cnd <- cands[eleito == FALSE]
pes[, `:=`(sq_cand_suplente = NA_character_, situacao_tse = NA_character_,
           nome_civil_tse = NA_character_, dt_nasc_tse = NA_character_,
           genero_tse = NA_character_, partido_tse = NA_character_)]
achar_cand <- function(col_pes, col_cnd, metodo, min_car = 8L) {
  a <- pes[is.na(id_mandato) & is.na(metodo_pareamento) & !is.na(get(col_pes)) &
             nchar(get(col_pes)) >= min_car, .(rid, ano_eleicao, chave = get(col_pes))]
  if (!nrow(a)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  a <- a[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  b <- cnd[!is.na(get(col_cnd)) & nchar(get(col_cnd)) >= min_car,
           .(sq_candidato, ano_eleicao, chave = get(col_cnd), situacao, nome_civil, dt_nasc,
             genero, partido_tse)]
  b <- b[, if (.N == 1L) .SD, by = .(ano_eleicao, chave)]
  m <- merge(a, b, by = c("ano_eleicao", "chave"))
  if (nrow(m)) pes[m, on = "rid", `:=`(sq_cand_suplente = i.sq_candidato,
                                       situacao_tse = i.situacao, nome_civil_tse = i.nome_civil,
                                       dt_nasc_tse = i.dt_nasc, genero_tse = i.genero,
                                       partido_tse = i.partido_tse, metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(m)))
  nrow(m)
}
cat("identificacao de suplente no cadastro de candidaturas:\n")
invisible(achar_cand("nome_norm", "nome_urna_norm",  "suplente_nome_parlamentar_x_urna"))
invisible(achar_cand("nome_completo_norm", "nome_urna_norm", "suplente_nome_completo_x_urna"))
invisible(achar_cand("nome_norm", "nome_civil_norm", "suplente_nome_parlamentar_x_civil"))
cnd[, `:=`(urna_st = sem_titulo(nome_urna_norm), civil_st = sem_titulo(nome_civil_norm))]
invisible(achar_cand("nome_st", "urna_st", "suplente_sem_titulo_x_urna_sem_titulo"))
# "SEBASTIAO RUFINO" na ata e "CORONEL RUFINO" na urna, mas "SEBASTIAO RUFINO RIBEIRO" no
# registro civil: a mesma regra de subsequencia que fecha o titular fecha o suplente
achar_cand_subseq <- function(col_alvo, metodo) {
  a <- pes[is.na(id_mandato) & is.na(metodo_pareamento) & !is.na(nome_st) & nchar(nome_st) >= 8L,
           .(rid, ano_eleicao, chave = nome_st)]
  if (!nrow(a)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  b <- cnd[!is.na(get(col_alvo)) & nchar(get(col_alvo)) >= 8L,
           .(sq_candidato, ano_eleicao, alvo = get(col_alvo), situacao, nome_civil, dt_nasc,
             genero, partido_tse)]
  ta <- tokens(a$chave); tb <- tokens(b$alvo)
  pares <- rbindlist(lapply(seq_len(nrow(a)), function(i) {
    v <- ta[[i]]
    if (length(v) < 2L) return(NULL)
    j <- which(b$ano_eleicao == a$ano_eleicao[i] &
                 vapply(tb, function(w) length(w) >= 2L && identical(w[1], v[1]) &&
                          all(v[-1] %in% w[-1]), logical(1)))
    if (!length(j)) return(NULL)
    data.table(rid = a$rid[i], k = j)
  }))
  if (!nrow(pares)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  pares <- pares[pares[, .N, by = rid][N == 1L], on = "rid"][, .(rid, k)]
  pares <- pares[pares[, .N, by = k][N == 1L], on = "k"][, .(rid, k)]
  if (!nrow(pares)) { cat(sprintf("  %-34s +0\n", metodo)); return(0L) }
  pes[pares, on = "rid", `:=`(sq_cand_suplente = b$sq_candidato[i.k],
                              situacao_tse = b$situacao[i.k], nome_civil_tse = b$nome_civil[i.k],
                              dt_nasc_tse = b$dt_nasc[i.k], genero_tse = b$genero[i.k],
                              partido_tse = b$partido_tse[i.k], metodo_pareamento = metodo)]
  cat(sprintf("  %-34s +%d\n", metodo, nrow(pares)))
  nrow(pares)
}
invisible(achar_cand_subseq("nome_civil_norm", "suplente_primeiro_e_sobrenome_x_civil"))

# atributos do cadastro para os titulares pareados
pes[dep, on = .(id_mandato), `:=`(
  nome_civil_tse = fifelse(is.na(nome_civil_tse), i.nome_civil_bruto, nome_civil_tse),
  dt_nasc_tse    = fifelse(is.na(dt_nasc_tse),    i.dt_nasc_tse,    dt_nasc_tse),
  genero_tse     = fifelse(is.na(genero_tse),     i.genero_tse,     genero_tse),
  partido_tse    = fifelse(is.na(partido_tse),    i.partido_tse,    partido_tse),
  votos_alepe = i.votos_alepe, partido_alepe = i.partido_alepe,
  mandato_inicio = i.mandato_inicio, mandato_fim = i.mandato_fim)]
if (!"votos_alepe" %in% names(pes)) pes[, `:=`(votos_alepe = NA_character_,
                                               partido_alepe = NA_character_)]

# ruido de leitura de PDF: nome sem nenhuma candidatura correspondente que aparece em poucas
# reunioes. O nome ancorado fica, mesmo com uma aparicao so, porque suplente convocado por
# poucos dias e exatamente isso.
pes[, ancorado := !is.na(id_mandato) | !is.na(sq_cand_suplente)]
pes[, n_vocabulos := lengths(tokens(nome_norm))]
descartes <- pes[ancorado == FALSE & (n_datas < 3L | n_vocabulos < 2L)]
pes <- pes[ancorado == TRUE | (n_datas >= 3L & n_vocabulos >= 2L)]
cat("nomes descartados como ruido de leitura (sem candidatura e com <3 reunioes ou um vocabulo so):",
    nrow(descartes), "\n")
fwrite(descartes[, .(legislatura, nome, n_datas, n_vocabulos, primeira, ultima)],
       file.path(verd, "asm2pe_nomes_descartados.csv"))
reg("n_nomes_descartados_ruido", nrow(descartes))

pes[, condicao := fcase(!is.na(id_mandato), "titular",
                        !is.na(sq_cand_suplente), "suplente",
                        condicao_portal %in% "suplente", "suplente",
                        default = "nao_informado")]

## ---------------------------------------------------------------- 7. saida derivada da estrutura
# A relacao nominal muda de composicao de uma edicao para a outra. Quem figura ate a reta final
# da legislatura cumpriu o mandato; quem deixa de figurar antes disso saiu, e a data da ultima
# aparicao data a saida. Para nao confundir saida com buraco do acervo, so se conclui saida
# quando o acervo registra pelo menos MIN_DEPOIS reunioes posteriores a ultima aparicao.
# quantas datas de observacao o acervo registra DEPOIS da ultima aparicao, contadas so nos
# paineis em que a pessoa foi vista — uma pagina alfabetica nao testemunha sobre quem cai fora
# da sua fatia do alfabeto
cal[, chave := paste(legislatura, painel)]
datas_leg <- split(cal$data_ref, cal$chave)
n_depois <- function(lg, pns, dt) {
  v <- sort(unlist(datas_leg[paste(lg, strsplit(pns, "|", fixed = TRUE)[[1]])],
                   use.names = FALSE))
  if (!length(v)) return(0L)
  length(v) - findInterval(dt, v)
}
pes[, n_depois := mapply(n_depois, legislatura, paineis, ultima)]
pes[, em_curso := leg_fim > HOJE]
# na legislatura em curso a referencia de 'reta final' e hoje, nao o fim do mandato
pes[, ref_fim := fifelse(em_curso, HOJE, leg_fim)]
pes[, chegou_ao_fim := ultima >= (ref_fim - JANELA)]
pes[, ult_licenciado := !is.na(ultima_licenc) &
      (is.na(ultima_exerc) | ultima_licenc >= ultima_exerc)]
pes[, saiu_antes := !chegou_ao_fim & n_depois >= MIN_DEPOIS]

pes[, forma_saida := fcase(
  em_curso & chegou_ao_fim,                    NA_character_,
  chegou_ao_fim & condicao == "suplente",      "suplente_efetivado",
  chegou_ao_fim,                               "fim_regular",
  saiu_antes & condicao == "suplente",         "suplente_efetivado",
  saiu_antes,                                  "outro",
  default = "nao_observado")]
pes[, causa_original := NA_character_]
pes[, data_saida := fifelse(chegou_ao_fim & !em_curso, leg_fim,
                            fifelse(saiu_antes, ultima, as.IDate(NA)))]

# Inicio do exercicio. A primeira aparicao so data a ENTRADA quando o acervo ja estava olhando:
# quem ja figura na primeira relacao disponivel do painel nao entrou naquele dia. Se o acervo
# alcanca a abertura da legislatura, essa pessoa tomou posse na abertura; se o acervo comeca
# depois — o DO da ALEPE so tem edicao de 2005 em diante, e a 15a legislatura abriu em 2003 —
# a data de entrada nao e observada e fica em branco.
ini_painel <- cal[, .(ini = min(data_ref)), by = .(legislatura, painel)]
ini_pessoa <- function(lg, pns) {
  v <- ini_painel[legislatura == lg & painel %in% strsplit(pns, "|", fixed = TRUE)[[1]], ini]
  if (!length(v)) return(as.IDate(NA)) else min(v)
}
pes[, ini_acervo := as.IDate(mapply(ini_pessoa, legislatura, paineis))]
pes[, acervo_alcanca_abertura := !is.na(ini_acervo) & ini_acervo <= (leg_inicio + 45L)]
pes[, ja_estava := !is.na(ini_acervo) & primeira <= (ini_acervo + 30L)]
pes[, data_entrada := fcase(
  !ja_estava, primeira,
  ja_estava & acervo_alcanca_abertura, leg_inicio,
  default = as.IDate(NA))]
reg("n_entrada_observada", pes[ja_estava == FALSE, .N])
reg("n_entrada_na_abertura", pes[ja_estava == TRUE & acervo_alcanca_abertura == TRUE, .N])
reg("n_entrada_nao_observada", pes[is.na(data_entrada), .N])
reg("n_saiu_antes_do_fim", pes[saiu_antes == TRUE, .N])
reg("n_chegou_ao_fim", pes[chegou_ao_fim == TRUE, .N])
reg("n_ultima_como_licenciado", pes[ult_licenciado == TRUE, .N])
pes[, url_evento := NA_character_]
pes[, data_evento := as.IDate(NA)]
pes[, fonte_evento := NA_character_]

## ---------------------------------------------------------------- 8. eventos nomeados
# Renuncia, cassacao, falecimento, afastamento, licenca e posse nao ocorrida sao afirmacoes
# sobre um ato: so entram com o texto da fonte gravado em causa_original. O trecho tem de citar
# o parlamentar a menos de 200 caracteres do gatilho, dentro da janela da legislatura, e nao
# pode estar falando de ex-parlamentar.
ev <- le("eventos_texto.csv")
achou_ev <- data.table()
if (nrow(ev)) {
  ev[, data := as.IDate(data)]
  ev <- ev[!is.na(data)]
  # normalizacao que preserva ponto e virgula: e o ponto que separa o sujeito do ato da frase
  # seguinte, e a virgula que enfileira os nomes de uma relacao. Sem eles nao da para saber se
  # o nome que segue o gatilho e o sujeito ou o comeco de outra oracao.
  norm_ev <- function(x) {
    x <- stri_trans_general(toupper(x), "Latin-ASCII")
    x <- gsub("[^A-Z ,.]", " ", x)
    gsub(" +", " ", trimws(x))
  }
  ev[, trecho_u := norm_ev(trecho)]
  # A proximidade sozinha nao identifica o sujeito do ato: numa homenagem de plenario o nome de
  # meia bancada aparece a poucos caracteres de "falecimento do deputado". O casamento exige
  # que o nome ocupe a posicao do sujeito — logo DEPOIS do gatilho na forma "FALECIMENTO DO
  # DEPUTADO <nome>", ou logo ANTES na forma "<nome> RENUNCIOU AO MANDATO".
  GAT <- list(
    list(k = "falecimento", dir = "pos", n = 45L,
         rx = "FALECIMENTO D[OA] (?:EX )?DEPUTAD[OA]|MORTE D[OA] DEPUTAD[OA]|MORRE O DEPUTADO|MORRE A DEPUTADA|MORRE DEPUTAD[OA]|FALECEU O DEPUTADO|FALECEU A DEPUTADA|NOTA DE PESAR PELO FALECIMENTO DO DEPUTADO"),
    list(k = "falecimento", dir = "pre", n = 60L,
         rx = "VEIO A FALECER|FALECEU NA MADRUGADA|FALECEU NO DIA|FALECEU ONTEM|MORREU NA MADRUGADA|MORREU ONTEM"),
    list(k = "renuncia", dir = "pos", n = 45L,
         rx = "RENUNCIA D[OA] DEPUTAD[OA]|RENUNCIA AO MANDATO D[OA] DEPUTAD[OA]"),
    list(k = "renuncia", dir = "pre", n = 60L,
         rx = "RENUNCIOU AO MANDATO|APRESENTOU RENUNCIA AO MANDATO|APRESENTOU SUA RENUNCIA|RENUNCIA AO MANDATO DE DEPUTAD"),
    list(k = "cassacao", dir = "pos", n = 60L,
         rx = "CASSACAO DO MANDATO D[OA] DEPUTAD[OA]|PERDA DO MANDATO D[OA] DEPUTAD[OA]"),
    list(k = "cassacao", dir = "pre", n = 60L,
         rx = "TEVE O MANDATO CASSADO|TEVE SEU MANDATO CASSADO|TEVE O MANDATO EXTINTO"),
    list(k = "licenca", dir = "pos", n = 45L,
         rx = "LICENCIADO O DEPUTADO|LICENCIADA A DEPUTADA|LICENCIADOS OS DEPUTADOS|LICENCIADAS AS DEPUTADAS|LICENCA D[OA] DEPUTAD[OA]|CONCEDER LICENCA A[OO]? DEPUTAD[OA]"),
    list(k = "licenca", dir = "pre", n = 60L,
         rx = "SE LICENCIOU DO MANDATO|LICENCIOU SE DO MANDATO|PEDIU LICENCA DO MANDATO|ESTA LICENCIAD[OA]"),
    list(k = "afastamento", dir = "pre", n = 90L,
         rx = "PARA ASSUMIR A SECRETARIA|PARA ASSUMIR O CARGO DE SECRETARI|ASSUMIU A SECRETARIA|ASSUME A SECRETARIA|PARA ASSUMIR A PREFEITURA|TOMOU POSSE COMO SECRETARI|FOI NOMEADO SECRETARI|FOI NOMEADA SECRETARI|DEIXA O MANDATO PARA ASSUMIR"),
    # O ato que efetiva o suplente declara de quem e a cadeira e por que ela vagou: "declara-o
    # efetivado na vaga do Deputado Licenciado ISALTINO NASCIMENTO". E a fonte mais direta da
    # forma de saida do titular, e por isso dispensa a confirmacao estrutural da ausencia.
    list(k = "licenca", dir = "pos", n = 45L, forte = TRUE,
         rx = "NA VAGA D[OA] (?:EX )?DEPUTAD[OA] LICENCIAD[OA]|VAGA D[OA] DEPUTAD[OA] LICENCIAD[OA]|EM VIRTUDE DA LICENCA D[OA] DEPUTAD[OA]"),
    list(k = "renuncia", dir = "pos", n = 60L, forte = TRUE,
         rx = "EM VIRTUDE DA RENUNCIA D[OA] DEPUTAD[OA]|NA VAGA DECORRENTE DA RENUNCIA D[OA] DEPUTAD[OA]|NA VAGA D[OA] DEPUTAD[OA] RENUNCIANTE"),
    list(k = "falecimento", dir = "pos", n = 60L, forte = TRUE,
         rx = "EM VIRTUDE DO FALECIMENTO D[OA] DEPUTAD[OA]|NA VAGA DECORRENTE DO FALECIMENTO D[OA] DEPUTAD[OA]|NA VAGA D[OA] DEPUTAD[OA] FALECID[OA]"),
    list(k = "afastamento", dir = "pos", n = 60L, forte = TRUE,
         rx = "NA VAGA D[OA] DEPUTAD[OA] AFASTAD[OA]|EM VIRTUDE DO AFASTAMENTO D[OA] DEPUTAD[OA]"),
    list(k = "cassacao", dir = "pos", n = 60L, forte = TRUE,
         rx = "EM VIRTUDE DA CASSACAO DO MANDATO D[OA] DEPUTAD[OA]|NA VAGA D[OA] DEPUTAD[OA] CASSAD[OA]"))
  alvo <- pes[condicao %in% c("titular", "suplente"),
              .(rid, legislatura, nome_norm, nome, leg_inicio, leg_fim, primeira, ultima)]
  # Entre o gatilho e o sujeito so pode haver a formula parlamentar e os outros nomes da mesma
  # relacao. A legenda de foto do Diario emenda assuntos sem pontuacao — "morte do deputado
  # estadual Waldemar Borges DOSIMETRIA Joao Paulo do PT criticou..." — e sem esta regra o nome
  # do vivo herdaria o obito do morto. Qualquer palavra no vao que nao seja conectivo nem nome
  # de parlamentar desqualifica o par.
  NOMES_RX <- paste0("\\b(",
                     paste(sort(unique(alvo$nome_norm[nchar(alvo$nome_norm) >= 8L]),
                                decreasing = TRUE), collapse = "|"), ")\\b")
  CONECTIVOS <- c("ESTADUAL", "ESTADUAIS", "SENHOR", "SENHORA", "SENHORES", "SENHORAS",
                  "EXCELENTISSIMO", "EXCELENTISSIMA", "SR", "SRA", "EX", "DE", "DA", "DO",
                  "DAS", "DOS", "E", "O", "A", "OS", "AS", "SEU", "SUA", "AO")
  # Gatilho no plural enfileira sujeitos: o vao pode conter outros nomes, desde que so eles e a
  # formula parlamentar. Gatilho no singular tem um sujeito so: qualquer outro nome de
  # parlamentar no vao indica que o ato e do outro, nao deste — e o caso da legenda de foto que
  # emenda o obito de um deputado a fala de outro.
  vao_limpo <- function(v, plural) {
    if (!plural) return(!grepl(NOMES_RX, v))
    v <- gsub(NOMES_RX, " ", v)
    w <- strsplit(gsub("[^A-Z ]", " ", v), " +")[[1]]
    w <- w[nzchar(w)]
    !length(w) || all(w %in% CONECTIVOS)
  }
  res <- list()
  for (g in GAT) {
    g$plural <- grepl("DEPUTAD[OA]S", g$rx)
    if (g$plural) g$n <- max(g$n, 250L)
    sub <- ev[grepl(g$rx, trecho_u)]
    if (!nrow(sub)) next
    for (i in seq_len(nrow(alvo))) {
      nm <- alvo$nome_norm[i]
      if (nchar(nm) < 9L || length(strsplit(nm, " ")[[1]]) < 2L) next
      cd <- sub[data >= alvo$leg_inicio[i] & data <= (alvo$leg_fim[i] + 30L) &
                  grepl(nm, trecho_u, fixed = TRUE)]
      if (!nrow(cd)) next
      # o trecho pode conter varias ocorrencias do gatilho e do nome; basta UM par adjacente na
      # posicao de sujeito, e a posicao desse par e a que centra o texto guardado como evidencia
      pn_l <- gregexpr(nm, cd$trecho_u, fixed = TRUE)
      pg_l <- gregexpr(g$rx, cd$trecho_u)
      plural_g <- isTRUE(g$plural)
      ancora <- vapply(seq_len(nrow(cd)), function(r) {
        pn <- as.integer(pn_l[[r]]); pg <- as.integer(pg_l[[r]])
        if (pg[1] < 0L || pn[1] < 0L) return(c(NA_real_, NA_real_))
        lg <- attr(pg_l[[r]], "match.length")
        tu <- cd$trecho_u[r]
        for (q in seq_along(pg)) {
          for (w in seq_along(pn)) {
            if (g$dir == "pos") {
              d <- pn[w] - (pg[q] + lg[q])
              if (d < -1L || d > g$n) next
              vao <- substr(tu, pg[q] + lg[q], pn[w] - 1L)
            } else {
              d <- pg[q] - (pn[w] + nchar(nm))
              if (d < -1L || d > g$n) next
              vao <- substr(tu, pn[w] + nchar(nm), pg[q] - 1L)
            }
            # entre o gatilho e o sujeito so pode haver outros nomes da mesma relacao: um
            # ponto final fecha a oracao e um novo "DEPUTAD" abre outro sujeito
            if (grepl(".", vao, fixed = TRUE)) next
            if (grepl("DEPUTAD", vao, fixed = TRUE)) next
            if (!vao_limpo(vao, plural_g)) next
            return(c(pg[q], pn[w]))
          }
        }
        c(NA_real_, NA_real_)
      }, numeric(2))
      pos_gat <- ancora[1, ]; pos_nom <- ancora[2, ]
      exd <- grepl(paste0("EX DEPUTAD[OA] ", nm), cd$trecho_u) |
             grepl(paste0("EX PARLAMENTAR ", nm), cd$trecho_u) |
             grepl(paste0("SAUDOSO ", nm), cd$trecho_u)
      manter <- !is.na(pos_gat) & !exd
      cd <- cd[manter]; pos_gat <- pos_gat[manter]; pos_nom <- pos_nom[manter]
      if (!nrow(cd)) next
      # o ato terminal e unico e vale pela primeira mencao; a licenca pode se repetir ao longo
      # do mandato, e o que descreve a saida e a ULTIMA, nao a primeira
      j <- if (g$k %in% c("licenca", "afastamento")) which.max(cd$data) else which.min(cd$data)
      # a evidencia guardada cobre do gatilho ao nome do sujeito, com folga dos dois lados:
      # numa relacao de licenciados o nome pode estar dezenas de caracteres depois do gatilho,
      # e um recorte centrado so no gatilho deixaria de fora justamente quem esta sendo citado
      tx <- cd$trecho[j]
      a0 <- max(1L, as.integer(min(pos_gat[j], pos_nom[j])) - 100L)
      a1 <- min(nchar(tx), as.integer(max(pos_gat[j], pos_nom[j] + nchar(nm))) + 140L)
      res[[length(res) + 1L]] <- data.table(
        rid = alvo$rid[i], legislatura = alvo$legislatura[i], nome = alvo$nome[i], chave = g$k,
        forte = isTRUE(g$forte), data_evento = cd$data[j],
        trecho = substr(tx, a0, a1),
        url = cd$url[j], origem = cd$origem[j], n_trechos = nrow(cd))
    }
  }
  if (length(res)) achou_ev <- rbindlist(res)
}
if (nrow(achou_ev)) {
  # prioridade: o ato que encerra o mandato vale mais que a licenca
  achou_ev <- unique(achou_ev, by = c("rid", "chave", "data_evento", "forte"))
  PRIOR <- c(falecimento = 1L, cassacao = 2L, renuncia = 3L, afastamento = 4L, licenca = 5L)
  # o ato de efetivacao do suplente vem antes de qualquer outro indicio da mesma natureza
  achou_ev[, prior := PRIOR[chave] - fifelse(forte, 10L, 0L)]
  achou_ev[, ordem := fifelse(chave %in% c("licenca", "afastamento"),
                              -as.integer(data_evento), as.integer(data_evento))]
  setorder(achou_ev, rid, prior, ordem)
  achou_ev <- achou_ev[, .SD[1], by = rid]
  # Dois regimes. O ato TERMINAL (falecimento, cassacao, renuncia) encerra o mandato e vale
  # sempre que a serie nao o contradiga — quem volta a figurar na relacao nominal depois da data
  # do ato nao saiu por ele, e o casamento por nome errou. Ja licenca e afastamento interrompem o
  # exercicio sem encerrar o mandato, e so descrevem a saida quando o parlamentar nao volta: a
  # corroboracao exigida e a propria relacao nominal mostrar o licenciamento na ultima aparicao.
  pes[achou_ev, on = "rid", `:=`(ev_chave = i.chave, ev_data = i.data_evento,
                                 ev_trecho = i.trecho, ev_url = i.url, ev_origem = i.origem,
                                 ev_forte = i.forte)]
  TERMINAL <- c("falecimento", "cassacao", "renuncia")
  pes[, ev_contradito := !is.na(ev_chave) & !is.na(ultima_exerc) & ultima_exerc > (ev_data + 30L)]
  reg("n_eventos_contraditos_pela_serie", pes[ev_contradito == TRUE, .N])
  # O ato terminal basta a si mesmo. A licenca e o afastamento, nao: sao episodicos, e so
  # descrevem a saida quando a ESTRUTURA confirma que a pessoa nao voltou — a relacao nominal
  # registra reunioes depois da ultima aparicao dela (saiu_antes) e ela nao reaparece no
  # exercicio ate a reta final. O texto nomeia a causa; a serie confirma que houve saida.
  pes[, chegou_ao_fim_exerc := !is.na(ultima_exerc) & ultima_exerc >= (ref_fim - JANELA)]
  aplica <- pes[!is.na(ev_chave) & ev_contradito == FALSE &
                  (ev_chave %in% TERMINAL | ev_forte %in% TRUE |
                     (ev_chave %in% c("licenca", "afastamento") & saiu_antes == TRUE &
                        chegou_ao_fim_exerc == FALSE))]
  pes[aplica, on = "rid", `:=`(
    forma_saida = i.ev_chave,
    causa_original = trimws(gsub("[[:space:]]+", " ", i.ev_trecho)),
    url_evento = i.ev_url, data_evento = i.ev_data, fonte_evento = i.ev_origem)]
  # a licenca sem saida antecipada observada nao muda a data de fim do exercicio
  pes[!is.na(data_evento) & !is.na(data_saida) & data_evento >= primeira &
        data_evento <= (leg_fim + 30L), data_saida := data_evento]
  pes[is.na(data_saida) & !is.na(data_evento) & data_evento >= primeira &
        data_evento <= (leg_fim + 30L), data_saida := data_evento]
}
cat("eventos nomeados candidatos:", nrow(achou_ev),
    "| aplicados:", pes[!is.na(causa_original), .N], "\n")
if (nrow(achou_ev)) {
  # por que cada candidato entrou ou nao: a auditoria do descarte importa tanto quanto a do uso
  aud <- pes[!is.na(ev_chave), .(legislatura, nome, condicao, ev_chave, ev_forte, ev_data, ev_origem,
                                 ultima_exerc, ult_licenciado, chegou_ao_fim_exerc,
                                 ev_contradito, forma_saida,
                                 aplicado = !is.na(causa_original))]
  aud[, motivo := fcase(
    aplicado == TRUE, "aplicado",
    ev_contradito == TRUE, "serie mostra exercicio depois da data do ato",
    ev_chave %in% c("licenca", "afastamento") & chegou_ao_fim_exerc == TRUE,
      "voltou ao exercicio ate a reta final da legislatura",
    ev_chave %in% c("licenca", "afastamento") & saiu_antes == FALSE,
      "acervo nao registra reuniao depois da ultima aparicao: saida nao confirmada",
    default = "nao aplicado")]
  print(aud[, .N, by = .(ev_chave, motivo)][order(ev_chave, -N)])
  fwrite(aud, file.path(verd, "asm2pe_eventos_auditoria.csv"))
  fwrite(achou_ev[, .(legislatura, nome, chave, data_evento, origem, url, n_trechos)],
         file.path(verd, "asm2pe_eventos_candidatos.csv"))
  reg("n_eventos_descartados_por_contradicao", aud[motivo %like% "^serie", .N])
  reg("n_eventos_descartados_sem_confirmacao", aud[motivo %like% "^acervo", .N])
}
reg("n_eventos_candidatos", nrow(achou_ev))
reg("n_eventos_aplicados", pes[!is.na(causa_original), .N])

## ---------------------------------------------------------------- 9. quem o BOCEL tem e a ata nao
# O titular que nao aparece em nenhuma relacao nominal da sua legislatura pode nao ter tomado
# posse, ou pode estar num periodo que o acervo nao alcanca. So o segundo caso e verificavel
# aqui: sem ato publicado, a linha nao e criada e o mandato fica sem forma observada, que e o
# resultado honesto.
cob_leg <- merge(dep[, .(n_bocel = .N), by = ano_eleicao],
                 pes[!is.na(id_mandato), .(n_pareados = .N), by = ano_eleicao],
                 by = "ano_eleicao", all.x = TRUE)
cob_leg[is.na(n_pareados), n_pareados := 0L]

## ---------------------------------------------------------------- 10. tabela canonica
url_ed <- function(pasta) fifelse(is.na(pasta) | pasta == "", NA_character_,
                                  sprintf("https://www.alepe.pe.gov.br/Flip/pubs/%s/Flip.pdf", pasta))
# o retrato pode vir do quadro atual ou de uma das quatro paginas alfabeticas de 2000-2003;
# a url tem de apontar para a pagina que de fato registrou aquela observacao
url_wb <- function(ts, painel) fifelse(
  is.na(ts) | ts == "", NA_character_,
  fifelse(painel == "parlamentares",
          sprintf("https://web.archive.org/web/%s/https://www.alepe.pe.gov.br/parlamentares/", ts),
          sprintf("https://web.archive.org/web/%s/http://www.alepe.pe.gov.br/gabinete/%s.html",
                  ts, painel)))
pes[, url_base := fifelse(origem_ultima == "ata", url_ed(ref_ultima),
                          url_wb(ref_ultima, painel_ultima))]
pes[, fonte_str := {
  f <- rep("alepe_do_ata", .N)
  f[n_ata == 0L] <- "alepe_retrato_wayback"
  f[n_ata > 0L & n_retrato > 0L] <- "alepe_do_ata+alepe_retrato_wayback"
  f <- fifelse(!is.na(condicao_portal), paste0(f, "+alepe_portal_anteriores"), f)
  f <- fifelse(!is.na(votos_alepe), paste0(f, "+alepe_apuracao_2002"), f)
  f <- fifelse(!is.na(fonte_evento) & fonte_evento == "do", paste0(f, "+alepe_do_ato"),
               fifelse(!is.na(fonte_evento) & fonte_evento == "noticia",
                       paste0(f, "+alepe_noticia"), f))
  f
}]

saida <- pes[, .(
  uf = UF,
  fonte = fonte_str,
  legislatura = as.character(legislatura),
  ano_eleicao = as.integer(ano_eleicao),
  nome,
  nome_normalizado = nome_norm,
  nome_completo = fifelse(!is.na(nome_completo), nome_completo, nome_civil_tse),
  data_nascimento = fcase(
    !is.na(dt_nasc_tse) & grepl("^\\d{2}/\\d{2}/\\d{4}$", dt_nasc_tse),
      format(as.IDate(dt_nasc_tse, format = "%d/%m/%Y")),
    !is.na(dt_nasc_tse) & grepl("^\\d{4}-\\d{2}-\\d{2}$", dt_nasc_tse), dt_nasc_tse,
    !is.na(data_nasc_fonte) & grepl("^\\d{4}-\\d{2}-\\d{2}$", data_nasc_fonte), data_nasc_fonte,
    default = NA_character_),
  # o partido preferido e o que a propria Casa publicou: ficha do portal, relacao de
  # legislaturas anteriores, retrato datado do quadro, apuracao de 2002; o cadastro do TSE
  # so entra onde a ALEPE nao diz nada
  partido = fcase(!is.na(partido_fonte), partido_fonte,
                  !is.na(partido_retrato), partido_retrato,
                  !is.na(partido_alepe), partido_alepe,
                  default = partido_tse),
  condicao,
  data_inicio_exercicio = format(data_entrada),
  data_fim_exercicio = format(data_saida),
  causa_original,
  forma_saida,
  id_pessoa_bocel = id_pessoa,
  id_mandato_bocel = id_mandato,
  metodo_pareamento,
  url = fcase(!is.na(url_evento), url_evento,
              !is.na(url_base), url_base,
              !is.na(url_portal), url_portal,
              default = "https://www.alepe.pe.gov.br/do/"),
  id_fonte = sprintf("PE_leg%02d_%s", legislatura, gsub(" ", "_", nome_norm)),
  votos_fonte = votos_alepe,
  sexo_fonte = fcase(grepl("^MASC", toupper(genero_tse)), "M",
                     grepl("^FEMI", toupper(genero_tse)), "F",
                     default = NA_character_))]
setorder(saida, legislatura, condicao, nome_normalizado, na.last = TRUE)

## ---------------------------------------------------------------- 11. asserts
in_set(saida$forma_saida, VOCAB, nome = "forma_saida")
in_set(saida$condicao, c("titular", "suplente", "nao_informado"), nome = "condicao")
em_faixa(saida$ano_eleicao, 1998, 2022, nome = "ano_eleicao")
checa_unica(as.data.frame(saida), c("uf", "legislatura", "nome_normalizado"))
iso <- function(v) all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v))
stopifnot(iso(saida$data_inicio_exercicio), iso(saida$data_fim_exercicio))
stopifnot(saida[!is.na(data_inicio_exercicio) & !is.na(data_fim_exercicio),
                all(data_inicio_exercicio <= data_fim_exercicio)])
stopifnot(saida[!is.na(data_inicio_exercicio),
                all(as.IDate(data_inicio_exercicio) >= as.IDate("1999-02-01"))])
par <- saida[!is.na(id_mandato_bocel)]
stopifnot(all(par$id_mandato_bocel %in% bocel_m$id_mandato))
ix <- match(par$id_mandato_bocel, bocel_m$id_mandato)
stopifnot(all(bocel_m$cd_cargo[ix] == "7"), all(bocel_m$sg_uf[ix] == UF),
          all(as.integer(bocel_m$ano_eleicao[ix]) == par$ano_eleicao))
stopifnot(uniqueN(par$id_mandato_bocel) == nrow(par))
# ato nomeado exige o texto da fonte; rotulo derivado da estrutura pode nao ter texto
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca", "nao_tomou_posse")
stopifnot(saida[forma_saida %in% TEXTUAL, all(!is.na(causa_original) & nzchar(causa_original))])
stopifnot(saida[(is.na(causa_original) | causa_original == "") &
                  !forma_saida %in% c("nao_observado", "fim_regular") & !is.na(forma_saida),
                all(forma_saida %in% c("suplente_efetivado", "assumiu_titular", "outro"))])
stopifnot(all(!is.na(saida$url) & nzchar(saida$url)))
# fim de exercicio dentro da janela do mandato pareado (tolerancia do integrador)
jan <- par[, {
  k <- match(id_mandato_bocel, bocel_m$id_mandato)
  is.na(data_fim_exercicio) |
    (as.IDate(data_fim_exercicio) >= as.IDate(bocel_m$mandato_inicio[k]) - 60L &
     as.IDate(data_fim_exercicio) <= as.IDate(bocel_m$mandato_fim[k]) + 45L)
}]
cat("linhas pareadas com fim fora da janela do mandato:", sum(!jan), "\n")
reg("n_fim_fora_da_janela", sum(!jan))

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado", "nome_completo",
          "data_nascimento", "partido", "condicao", "data_inicio_exercicio", "data_fim_exercicio",
          "causa_original", "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
setcolorder(saida, COLS)
stopifnot(identical(names(saida), COLS))
fwrite(saida, file.path(outd, "PE.csv"), sep = ",", na = "NA", quote = TRUE, bom = FALSE)
cat("\ndata/assembleias2/PE.csv:", nrow(saida), "linhas x", ncol(saida), "colunas\n")

## ---------------------------------------------------------------- 12. cobertura e numeros
cob <- merge(dep[, .(n_bocel = .N), by = ano_eleicao],
             saida[!is.na(id_mandato_bocel),
                   .(n_pareados = uniqueN(id_mandato_bocel),
                     n_com_forma = uniqueN(id_mandato_bocel[!is.na(forma_saida) &
                                                            forma_saida != "nao_observado"])),
                   by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
cob <- merge(cob, saida[, .(n_linhas = .N,
                            n_titular = sum(condicao == "titular"),
                            n_suplente = sum(condicao == "suplente")),
                        by = ano_eleicao], by = "ano_eleicao", all.x = TRUE)
for (j in setdiff(names(cob), "ano_eleicao")) set(cob, which(is.na(cob[[j]])), j, 0L)
cob[, taxa_pareamento := round(n_pareados / n_bocel, 4)]
cob[, taxa_forma := round(n_com_forma / n_bocel, 4)]
cob[LEG, on = .(ano_eleicao), legislatura := i.legislatura]
setcolorder(cob, c("legislatura", "ano_eleicao"))
setorder(cob, ano_eleicao)
print(cob)
fwrite(cob, file.path(verd, "asm2pe_cobertura_legislatura.csv"))

fs <- saida[, .N, by = .(condicao, forma_saida)][order(condicao, -N)]
cat("\nforma de saida por condicao:\n"); print(fs)
fwrite(fs, file.path(verd, "asm2pe_forma_saida.csv"))
fwrite(saida[!is.na(causa_original), .(legislatura, nome, condicao, forma_saida,
                                       data_fim_exercicio, url, causa_original)],
       file.path(verd, "asm2pe_atos_com_texto.csv"))

n_forma <- uniqueN(saida[!is.na(id_mandato_bocel) & !is.na(forma_saida) &
                           forma_saida != "nao_observado"]$id_mandato_bocel)
reg("n_linhas", nrow(saida))
reg("n_titulares", saida[condicao == "titular", .N])
reg("n_suplentes", saida[condicao == "suplente", .N])
reg("n_condicao_nao_informado", saida[condicao == "nao_informado", .N])
reg("n_pareados_mandato", uniqueN(na.omit(saida$id_mandato_bocel)))
reg("n_mandatos_com_forma", n_forma)
reg("n_com_data_inicio", saida[!is.na(data_inicio_exercicio), .N])
reg("n_com_causa_original", saida[!is.na(causa_original), .N])
# transparencia sobre a origem das colunas descritivas: nome civil, nascimento, sexo e partido
# vem do cadastro de candidaturas do TSE quando a propria Casa nao publica o dado, e so a ficha
# do portal e a relacao de /parlamentares-anteriores/ sao da ALEPE
reg("n_nome_completo_da_alepe", pes[!is.na(nome_completo), .N])
reg("n_nome_completo_do_cadastro_tse",
    saida[!is.na(nome_completo), .N] - pes[!is.na(nome_completo), .N])
reg("n_partido_da_alepe",
    pes[!is.na(partido_fonte) | !is.na(partido_retrato) | !is.na(partido_alepe), .N])
reg("n_votos_fonte_preenchidos", saida[!is.na(votos_fonte), .N])
reg("taxa_pareamento_global", sprintf("%d/%d=%.4f", uniqueN(na.omit(saida$id_mandato_bocel)),
                                      nrow(dep), uniqueN(na.omit(saida$id_mandato_bocel)) / nrow(dep)))
reg("taxa_forma_observada_global", sprintf("%d/%d=%.4f", n_forma, nrow(dep), n_forma / nrow(dep)))
reg("legislaturas_com_linha", paste(sort(unique(as.integer(saida$legislatura))), collapse = ";"))
for (i in seq_len(nrow(cob))) {
  reg(sprintf("pareamento_leg%d", cob$legislatura[i]),
      sprintf("%d/%d=%.4f", cob$n_pareados[i], cob$n_bocel[i], cob$taxa_pareamento[i]))
  reg(sprintf("forma_observada_leg%d", cob$legislatura[i]),
      sprintf("%d/%d=%.4f", cob$n_com_forma[i], cob$n_bocel[i], cob$taxa_forma[i]))
}
for (i in seq_len(nrow(fs)))
  reg(sprintf("forma_%s_%s", fs$condicao[i],
              fifelse(is.na(fs$forma_saida[i]), "em_curso", fs$forma_saida[i])), fs$N[i])
for (m in sort(unique(na.omit(saida$metodo_pareamento))))
  reg(paste0("n_metodo_", m), saida[metodo_pareamento == m, .N])

gravar_relatorio_verificacao(
  alvo = "data/assembleias2/PE.csv", script = script,
  passou = c("21 colunas canonicas na ordem do banco",
             "forma_saida no vocabulario fechado (NA = legislatura em curso)",
             "condicao em {titular, suplente, nao_informado}",
             "ano_eleicao em 1998..2022 com a legislatura correspondente",
             "id_mandato_bocel existe em data/mandatos.csv com cd_cargo 7 e sg_uf PE",
             "um id_mandato_bocel por linha, sem reuso",
             "ato nomeado acompanhado do texto da fonte em causa_original",
             "toda linha com url de origem",
             sprintf("pareamento = %.4f (%d de %d mandatos)",
                     uniqueN(na.omit(saida$id_mandato_bocel)) / nrow(dep),
                     uniqueN(na.omit(saida$id_mandato_bocel)), nrow(dep)),
             sprintf("forma observada = %.4f (%d de %d mandatos)",
                     n_forma / nrow(dep), n_forma, nrow(dep))),
  fora_de_cobertura = c(
    "pertinencia semantica do pareamento por nome: homonimo e apelido nao sao resolvidos por documento",
    "nome_completo, data_nascimento e sexo_fonte vem do cadastro de candidaturas do TSE quando a ALEPE nao publica o dado; so o que a ficha do portal e a relacao de parlamentares anteriores trazem e da propria Casa",
    "completude do acervo digital do DO da ALEPE, que comeca em 2005 e deixa a 14a legislatura sem ata",
    "veracidade do que a Casa publica na ata, no Diario Oficial e no acervo de noticias",
    "presenca na relacao nominal mede exercicio observado, nao a data juridica de posse ou de desligamento",
    "licenca curta entre duas reunioes plenarias nao aparece na serie",
    "o rotulo 'outro' registra saida antecipada sem ato encontrado, e nao afirma a causa"))
cat("\nbuild_PE: concluido —", format(Sys.time()), "\n")
sink()
