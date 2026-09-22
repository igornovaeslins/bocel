# verifica_CE.R — verificador proprio da frente ALECE (assembleias2/CE).
#   Le data/assembleias2/CE.csv DO DISCO, sem reaproveitar objeto em memoria, e confere:
#   esquema e vocabulario fechado; granularidade; faixas; integridade do pareamento contra
#   data/mandatos.csv; que o texto de causa_original existe LITERALMENTE no PDF em cache
#   da fonte citada; e que toda saida antecipada se sustenta na serie de presenca (a pessoa
#   some da relacao nominal e nao volta).
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_CE.R
suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
alvo   <- file.path(root, "data", "assembleias2", "CE.csv")
raw    <- file.path(root, "data_raw", "assembleias2", "CE")
verd   <- file.path(root, "output", "verificacao")
script <- file.path(root, "R", "assembleias2", "verifica_CE.R")
dir.create(verd, showWarnings = FALSE, recursive = TRUE)
logf <- file.path(root, "logs", "asm2ce_verifica_CE.log")
sink(logf, split = TRUE)
cat("verifica_CE.R —", format(Sys.time()), "\n")

passou <- character(); falhou <- character()
ok <- function(msg, cond) {
  cond <- isTRUE(all(cond)) && length(cond) > 0
  if (cond) passou <<- c(passou, msg) else falhou <<- c(falhou, msg)
  cat(sprintf("[%s] %s\n", if (cond) "ok  " else "FALHA", msg))
  invisible(cond)
}
nz <- function(x) !is.na(x) & nzchar(x)

COLS <- c("uf", "fonte", "legislatura", "ano_eleicao", "nome", "nome_normalizado",
          "nome_completo", "data_nascimento", "partido", "condicao",
          "data_inicio_exercicio", "data_fim_exercicio", "causa_original",
          "forma_saida", "id_pessoa_bocel", "id_mandato_bocel", "metodo_pareamento",
          "url", "id_fonte", "votos_fonte", "sexo_fonte")
VOCAB <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento",
           "licenca", "nao_tomou_posse", "suplente_efetivado", "assumiu_titular",
           "outro", "nao_observado")
TEXTUAL <- c("renuncia", "cassacao", "falecimento", "afastamento", "licenca",
             "nao_tomou_posse")
ESTRUTURAL <- c("suplente_efetivado", "assumiu_titular", "outro")
LEG <- data.table(legislatura = as.character(25:31),
                  ano_leg = c(1998L, 2002L, 2006L, 2010L, 2014L, 2018L, 2022L),
                  posse = as.IDate(c("1999-02-01", "2003-02-01", "2007-02-01",
                                     "2011-02-01", "2015-02-01", "2019-02-01",
                                     "2023-02-01")),
                  fim_leg = as.IDate(c("2003-01-31", "2007-01-31", "2011-01-31",
                                       "2015-01-31", "2019-01-31", "2023-01-31",
                                       "2027-01-31")))

ok("data/assembleias2/CE.csv existe", file.exists(alvo))
x <- fread(alvo, encoding = "UTF-8", na.strings = "NA", colClasses = "character")
x[, ano_eleicao := as.integer(ano_eleicao)]

## ------------------------------------------------------------------ esquema
ok("esquema: 21 colunas na ordem exigida", identical(names(x), COLS))
ok("uf == 'CE' em todas as linhas", all(x$uf == "CE"))
ok("nenhuma linha sem nome", all(nz(x$nome)))
ok("nenhuma linha sem url", all(nz(x$url)))
ok("url so de dominio da propria Casa",
   all(grepl("al\\.ce\\.gov\\.br|web\\.archive\\.org", x$url)))
ok("condicao em {titular, suplente}", all(x$condicao %in% c("titular", "suplente")))
ok("forma_saida no vocabulario fechado", all(x$forma_saida %in% VOCAB))
ok("ano_eleicao nos sete pleitos do recorte",
   all(x$ano_eleicao %in% LEG$ano_leg))
ok("as 7 legislaturas estao presentes", uniqueN(x$legislatura) == 7L)
m <- merge(x[, .(legislatura, ano_eleicao)], LEG, by = "legislatura", all.x = TRUE)
ok("legislatura bate com o ano da eleicao", all(m$ano_eleicao == m$ano_leg))
ok("46 titulares por legislatura (bancada da Alece)",
   all(x[condicao == "titular", .N, by = legislatura]$N == 46L))
in_set(x$forma_saida, VOCAB, permitir_na = FALSE, nome = "forma_saida")
in_set(x$condicao, c("titular", "suplente"), permitir_na = FALSE, nome = "condicao")
em_faixa(x$ano_eleicao, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
em_faixa(as.integer(substr(x$data_nascimento, 1, 4)), 1900, 2005, permitir_na = TRUE,
         nome = "ano_nascimento")
em_faixa(as.integer(x$votos_fonte), 0, 500000, permitir_na = TRUE, nome = "votos_fonte")
isod <- function(v) all(is.na(v) | grepl("^\\d{4}-\\d{2}-\\d{2}$", v))
ok("datas em ISO yyyy-mm-dd", isod(x$data_inicio_exercicio) && isod(x$data_fim_exercicio) &&
     isod(x$data_nascimento))
ok("inicio nao posterior ao fim",
   x[nz(data_inicio_exercicio) & nz(data_fim_exercicio),
     all(data_inicio_exercicio <= data_fim_exercicio)])
checa_unica(as.data.frame(x), c("legislatura", "nome_normalizado"))
ok("granularidade: legislatura x nome_normalizado e unica",
   !anyDuplicated(x[, .(legislatura, nome_normalizado)]))

## ------------------------------------- datas dentro da janela de cada legislatura
d <- merge(x, LEG, by = "legislatura")
fora <- d[nz(data_inicio_exercicio) &
            (as.IDate(data_inicio_exercicio) < posse - 30L |
               as.IDate(data_inicio_exercicio) > fim_leg + 1L)]
ok("data de inicio dentro do quadrienio", nrow(fora) == 0L)
fora2 <- d[nz(data_fim_exercicio) &
             (as.IDate(data_fim_exercicio) < posse - 30L |
                as.IDate(data_fim_exercicio) > fim_leg + 45L)]
ok("data de fim dentro do quadrienio", nrow(fora2) == 0L)

## ------------------------------------------------- regime de evidencia por forma
sem_txt <- x[forma_saida %in% TEXTUAL & !nz(causa_original)]
ok("ato nomeado sempre traz o texto da fonte em causa_original", nrow(sem_txt) == 0L)
ok("forma sem texto so nos rotulos derivaveis da estrutura",
   x[!nz(causa_original) & !forma_saida %in% c("nao_observado", "fim_regular"),
     all(forma_saida %in% ESTRUTURAL)])
ok("mandato em curso nao recebe fim_regular",
   x[legislatura == "31", sum(forma_saida == "fim_regular")] == 0L)

## ---------------------------- causa_original existe LITERALMENTE no PDF em cache
# A frase e comparada com o texto da pagina de onde saiu, depois de desfazer a hifenacao de
# fim de linha e de colapsar espaco — que e a unica transformacao que o parse aplica.
cat("\n-- texto de causa_original conferido contra o PDF em cache\n")
lit <- file.path(verd, "asm2ce_causa_literal.csv")
if (file.exists(lit)) file.remove(lit)
sh <- system2("python3", file.path(root, "python", "assembleias2", "confere_causa_CE.py"),
              stdout = TRUE, stderr = TRUE)
cat(paste(sh, collapse = "\n"), "\n")
if (file.exists(lit)) {
  cl <- fread(lit, encoding = "UTF-8")
  ok(sprintf("toda causa_original aparece literalmente no PDF da fonte (%d de %d; %d sem sequer remover a linha-corrente que o PDF intercala)",
             cl[literal_sem_cabecalho == 1L, .N], nrow(cl), cl[literal == 1L, .N]),
     nrow(cl) > 0L && all(cl$literal_sem_cabecalho == 1L))
  ok("toda linha com ato nomeado foi conferida contra o PDF",
     nrow(cl) == x[nz(causa_original), .N])
} else {
  ok("relatorio da conferencia literal foi gravado", FALSE)
}

## ------------------------- saida antecipada se sustenta na serie de presenca
# Duas conferencias independentes da montagem, refeitas do zero a partir das atas em cache:
#   (a) saida antecipada — a pessoa nao pode reaparecer no plenario depois da data de fim
#       gravada (folga de 60 dias, que e o intervalo entre o ato e a ultima sessao a que a
#       fonte ainda pode registrar presenca);
#   (b) fim_regular — a pessoa tem de aparecer em pelo menos uma sessao do trecho final do
#       quadrienio (ultimos 120 dias), e nao apenas "nao ter sido detectada saindo".
cat("\n-- saida e permanencia conferidas contra a serie de presenca\n")
pres <- fread(file.path(raw, "atas_presenca.csv"), encoding = "UTF-8",
              colClasses = list(character = c("id_ata", "nome")))
pres[, `:=`(legislatura = as.character(legislatura), data_sessao = as.IDate(data_sessao))]
resol <- fread(file.path(verd, "asm2ce_resolucao_nomes.csv"), encoding = "UTF-8",
               colClasses = "character")
pr <- merge(pres, resol[nz(sq_candidato), .(legislatura, nome, sq_candidato)],
            by = c("legislatura", "nome"))
mapa <- fread(file.path(verd, "asm2ce_chaves.csv"), encoding = "UTF-8",
              colClasses = "character")
x <- merge(x, mapa[, .(legislatura, nome_normalizado, sq = sq_candidato)],
           by = c("legislatura", "nome_normalizado"), all.x = TRUE)
x <- merge(x, LEG[, .(legislatura, posse, fim_leg)], by = "legislatura")
DIAS_TRECHO <- 120L
sess_fim <- merge(unique(pr[, .(legislatura, id_ata, data_sessao)]),
                  LEG[, .(legislatura, fim_leg)], by = "legislatura")
sess_fim <- sess_fim[data_sessao >= fim_leg - DIAS_TRECHO,
                     .(n_sessoes_trecho = .N), by = legislatura]
pres_fim <- merge(pr, LEG[, .(legislatura, fim_leg)], by = "legislatura")
pres_fim <- pres_fim[data_sessao >= fim_leg - DIAS_TRECHO,
                     .(n_presencas_trecho = .N), by = .(legislatura, sq = sq_candidato)]
tem_serie <- pr[, .(n_presencas = .N, ult_presenca = max(data_sessao)),
                by = .(legislatura, sq = sq_candidato)]
x <- merge(x, tem_serie, by = c("legislatura", "sq"), all.x = TRUE)
x <- merge(x, pres_fim, by = c("legislatura", "sq"), all.x = TRUE)
x <- merge(x, sess_fim, by = "legislatura", all.x = TRUE)
x[is.na(n_presencas_trecho), n_presencas_trecho := 0L]
x[is.na(n_sessoes_trecho), n_sessoes_trecho := 0L]

ANTEC <- c("outro", "renuncia", "cassacao", "falecimento", "licenca", "afastamento")
ant <- x[forma_saida %in% ANTEC]
# reaparecimento depois da data de fim gravada desmente a saida
volta <- merge(ant[nz(data_fim_exercicio), .(legislatura, sq, nome, fim = as.IDate(data_fim_exercicio))],
               pr[, .(legislatura, sq = sq_candidato, data_sessao)],
               by = c("legislatura", "sq"), allow.cartesian = TRUE)
volta <- volta[data_sessao > fim + 60L, .(sessoes_depois = .N), by = .(legislatura, sq)]
ant <- merge(ant, volta, by = c("legislatura", "sq"), all.x = TRUE)
ant[is.na(sessoes_depois), sessoes_depois := 0L]
ant[, sustenta := sessoes_depois == 0L]
cat("saidas antecipadas:", nrow(ant), "| com serie de presenca:",
    ant[!is.na(n_presencas), .N], "| nenhuma volta ao plenario depois do fim gravado:",
    ant[, sum(sustenta)], "\n")
if (ant[sustenta == FALSE, .N])
  print(ant[sustenta == FALSE, .(legislatura, nome, forma_saida, data_fim_exercicio,
                                 ult_presenca, sessoes_depois)])
ok("saida antecipada nao e desmentida pela serie (a pessoa nao volta ao plenario)",
   all(ant$sustenta))
# e a saida por 'outro', que nasce so da serie, precisa de serie que continue depois
so_serie <- ant[forma_saida == "outro" & !is.na(n_presencas)]
ok("saida rotulada 'outro' vem sempre de quem sumiu do trecho final do quadrienio",
   nrow(so_serie) == 0L || all(so_serie$n_presencas_trecho == 0L))
fwrite(ant[, .(legislatura, nome, forma_saida, data_fim_exercicio, ult_presenca,
               n_presencas, n_presencas_trecho, sessoes_depois, sustenta)],
       file.path(verd, "asm2ce_saida_contra_serie.csv"))

fr <- x[forma_saida == "fim_regular"]
fr[, sustenta := n_sessoes_trecho > 0L & n_presencas_trecho > 0L]
cat("fim_regular:", nrow(fr), "| com presenca no trecho final:", sum(fr$sustenta), "\n")
if (fr[sustenta == FALSE, .N])
  print(head(fr[sustenta == FALSE, .(legislatura, nome, ult_presenca, n_presencas_trecho,
                                     n_sessoes_trecho)], 20))
ok("fim_regular so para quem aparece em sessao do trecho final do quadrienio",
   nrow(fr) == 0L || all(fr$sustenta))
fwrite(fr[, .(legislatura, nome, ult_presenca, n_presencas, n_presencas_trecho,
              n_sessoes_trecho, sustenta)],
       file.path(verd, "asm2ce_fim_regular_contra_serie.csv"))

## ------------------------------------------------- pareamento contra o BOCEL
bocel <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8",
             colClasses = list(character = c("sq_candidato", "nr_candidato")))
dep <- bocel[cd_cargo == 7L & sg_uf == "CE", .(id_mandato, ano_bocel = ano_eleicao, cd_cargo,
                                             sg_uf)]
p <- x[nz(id_mandato_bocel)]
j <- merge(p[, .(id_mandato_bocel, ano_eleicao, condicao)], dep, by.x = "id_mandato_bocel",
           by.y = "id_mandato", all.x = TRUE)
ok("todo id_mandato_bocel existe em mandatos.csv com cd_cargo 7 e sg_uf CE",
   nrow(j) == nrow(p) && !anyNA(j$cd_cargo) && all(j$cd_cargo == 7L) && all(j$sg_uf == "CE"))
ok("ano do mandato no BOCEL bate com o ano_eleicao da linha", all(j$ano_bocel == j$ano_eleicao))
ok("id_mandato_bocel nao aponta para dois nomes distintos",
   nrow(p[, .(n = uniqueN(nome_normalizado)), by = id_mandato_bocel][n > 1]) == 0L)
ok("id_mandato_bocel nao se repete na mesma legislatura",
   !anyDuplicated(p[, .(legislatura, id_mandato_bocel)]))
ok("so titular recebe id_mandato_bocel (o BOCEL so guarda os eleitos)",
   all(j$condicao == "titular"))
ok("toda linha pareada tem metodo_pareamento", all(nz(p$metodo_pareamento)))

cob <- merge(dep[, .(n_bocel = .N), by = ano_bocel],
             p[, .(n_par = uniqueN(id_mandato_bocel),
                   n_forma = uniqueN(id_mandato_bocel[forma_saida != "nao_observado"])),
               by = .(ano_bocel = ano_eleicao)], by = "ano_bocel", all.x = TRUE)
cob[is.na(n_par), `:=`(n_par = 0L, n_forma = 0L)]
cob[, `:=`(taxa_par = round(n_par / n_bocel, 4), taxa_forma = round(n_forma / n_bocel, 4))]
setorder(cob, ano_bocel)
cat("\nPareamento e cobertura por ano de eleicao:\n"); print(cob)
fwrite(cob, file.path(verd, "asm2ce_verificacao_pareamento.csv"))
fs <- x[, .N, by = .(condicao, forma_saida)][order(condicao, -N)]
cat("\nForma de saida por condicao:\n"); print(fs)

reg <- function(k, v) registrar_numero(paste0("asm2ce_verif_", k), v, script = script,
                                       out = "output/numeros_assinatura.txt")
reg("n_linhas_lidas_do_disco", nrow(x))
reg("n_checagens_passou", length(passou))
reg("n_checagens_falhou", length(falhou))
reg("n_saidas_antecipadas", nrow(ant))
reg("n_saidas_sustentadas_pela_serie", sum(ant$sustenta))
reg("n_fim_regular", nrow(fr))
reg("n_fim_regular_sustentado", sum(fr$sustenta))
reg("n_linhas_com_serie_de_presenca", x[!is.na(n_presencas), .N])
reg("taxa_forma_observada_recalculada",
    sprintf("%d/%d=%.4f", cob[, sum(n_forma)], cob[, sum(n_bocel)],
            cob[, sum(n_forma) / cob[, sum(n_bocel)]]))
reg("md5_CE_csv", unname(tools::md5sum(alvo)))

gravar_relatorio_verificacao(
  alvo = "data/assembleias2/CE.csv", script = script,
  passou = passou, falhou = falhou,
  fora_de_cobertura = c(
    "presenca em plenario nao e o mesmo que exercicio do mandato: a serie mede comparecimento a sessao, e o limiar de 40 sessoes e 150 dias sem retorno e uma convencao desta frente, nao um criterio da Casa",
    "sensibilidade do rotulo 'outro' ao limiar: quem falta muito e volta perto do limite pode ser lido como saida, e nao ha ata de ausentes nem de licenciados na Alece para desempatar",
    "pertinencia semantica do pareamento por nome onde nao houve sq_candidato do TSE: sem CPF nem titulo nas fontes da Casa, homonimo e grafia divergente ficam por conta do julgamento",
    "veracidade do que a Casa publica: data, partido e causa sao reproduzidos como estao na fonte",
    "completude do acervo de atas: a Casa nao tem ata util para 1999-2002, 2005-2007 e 2010 — em 2005 e 2006 o registro existe por id mas devolve 'Conteudo nao localizado' —, e por isso a 25a legislatura fica inteira sem serie e a 26a so tem 2003 e 2004; ausencia de ata e lacuna, nao evidencia de ausencia do parlamentar",
    "a serie so cobre a sessao plenaria: reuniao de comissao, missao oficial e trabalho de gabinete nao aparecem, e um parlamentar pode estar em exercicio sem comparecer ao plenario",
    "a 31a legislatura (2023-2027) esta em curso: nenhuma linha dela recebe fim_regular",
    "atribuicao da frase do volume do Memorial a uma pessoa: a regra e sintatica (dono da biografia, ou titular citado na oracao 'na vaga de X, que renunciou') e nao interpreta o texto",
    "PDF sem camada de texto fica como lacuna contada: nao ha Tesseract neste ambiente e nada foi lido por OCR"))
cat("\nverifica_CE:", length(passou), "checagens ok,", length(falhou), "falhas —",
    format(Sys.time()), "\n")
sink()
if (length(falhou)) quit(status = 1)
