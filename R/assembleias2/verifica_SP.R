# verifica_SP.R — verificacao independente de data/assembleias2/SP.csv (ALESP).
# Nao reusa objetos do build: le o CSV entregue e os JSON brutos e reconstroi as
# invariantes do zero. Relatorio em output/verificacao/.
# Execucao: cd ~/bocel && Rscript --vanilla R/assembleias2/verifica_SP.R
suppressPackageStartupMessages({ library(data.table); library(stringi); library(jsonlite) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))

root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
raw  <- file.path(root, "data_raw", "assembleias2", "SP")
verd <- file.path(root, "output", "verificacao")
script <- file.path(root, "R", "assembleias2", "verifica_SP.R")
logf <- file.path(root, "logs", "asm2_verifica_SP.log")
sink(logf, split = TRUE)
cat("verifica_SP.R —", format(Sys.time()), "\n")
reg <- function(k, v) registrar_numero(paste0("asm2sp_verif_", k), v, script = script)

passou <- character(); falhou <- character()
ok <- function(cond, msg) {
  if (isTRUE(cond)) { passou <<- c(passou, msg); cat("[ok]   ", msg, "\n") }
  else { falhou <<- c(falhou, msg); cat("[FALHA]", msg, "\n") }
  invisible(cond)
}

f <- file.path(root, "data", "assembleias2", "SP.csv")
ok(file.exists(f), "arquivo data/assembleias2/SP.csv existe")
x <- fread(f, na.strings = "NA", encoding = "UTF-8", colClasses = "character")

COLS <- c("uf","fonte","legislatura","ano_eleicao","nome","nome_normalizado","nome_completo",
          "data_nascimento","partido","condicao","data_inicio_exercicio","data_fim_exercicio",
          "causa_original","forma_saida","id_pessoa_bocel","id_mandato_bocel","metodo_pareamento",
          "url","id_fonte","votos_fonte","sexo_fonte")
ok(identical(names(x), COLS), "esquema: 21 colunas na ordem exigida")

VOCAB <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca",
           "nao_tomou_posse","suplente_efetivado","assumiu_titular","outro","nao_observado")
ok(all(is.na(x$forma_saida) | x$forma_saida %in% VOCAB), "forma_saida dentro do vocabulario fechado")
in_set(x$forma_saida, VOCAB, permitir_na = TRUE, nome = "forma_saida")
ok(all(x$condicao %in% c("titular","suplente","nao_informado")), "condicao em {titular,suplente,nao_informado}")
in_set(x$condicao, c("titular","suplente","nao_informado"), permitir_na = FALSE, nome = "condicao")
ok(all(x$uf == "SP"), "uf constante SP")
ok(all(as.integer(x$ano_eleicao) %in% seq(1998L, 2022L, 4L)), "ano_eleicao nas 7 eleicoes ordinarias")
em_faixa(as.integer(x$ano_eleicao), 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")

# legislatura tem de bater com o ano de eleicao (14a = 1998 ... 20a = 2022)
esp <- 1998L + (as.integer(x$legislatura) - 14L) * 4L
ok(all(esp == as.integer(x$ano_eleicao)), "legislatura coerente com ano_eleicao")

# datas
di <- as.IDate(x$data_inicio_exercicio); df <- as.IDate(x$data_fim_exercicio)
ok(sum(!is.na(di) & !is.na(df) & di > df) == 0L, "data_inicio_exercicio <= data_fim_exercicio")
em_faixa(as.integer(format(di, "%Y")), 1999, 2026, permitir_na = TRUE, nome = "ano_inicio")
em_faixa(as.integer(format(df, "%Y")), 1999, 2027, permitir_na = TRUE, nome = "ano_fim")

# exercicio dentro dos limites da legislatura (a ALESP fecha a 20a em 2027-01-31)
legs <- as.data.table(fromJSON(file.path(raw, "legislaturas.json"), simplifyVector = TRUE))
legs <- legs[nuLegislatura %in% 14:20, .(legislatura = as.character(nuLegislatura),
                                         leg_inicio = as.IDate(dtInicio), leg_fim = as.IDate(dtFim))]
y <- merge(x[, .(legislatura, di = as.IDate(data_inicio_exercicio),
                 df = as.IDate(data_fim_exercicio))], legs, by = "legislatura")
ok(y[!is.na(di) & di < leg_inicio, .N] == 0L, "nenhum inicio de exercicio antes do inicio da legislatura")
ok(y[!is.na(df) & df > leg_fim, .N] == 0L, "nenhum fim de exercicio depois do fim da legislatura")

# granularidade
checa_unica(as.data.frame(x), c("legislatura", "id_fonte", "data_inicio_exercicio"))
ok(TRUE, "granularidade unica em legislatura x id_fonte x data_inicio_exercicio")

# pareamento: id_mandato_bocel tem de existir em mandatos.csv com cd_cargo 7 / sg_uf SP e ano coerente
bocel <- fread(file.path(root, "data", "mandatos.csv"), na.strings = "NA", encoding = "UTF-8")
dep <- bocel[cd_cargo == 7L & sg_uf == "SP", .(id_mandato, ano_bocel = ano_eleicao, id_pessoa_bocel_ref = id_pessoa)]
p <- x[!is.na(id_mandato_bocel)]
m <- merge(p[, .(id_mandato_bocel, ano_eleicao = as.integer(ano_eleicao), id_pessoa_bocel)],
           dep, by.x = "id_mandato_bocel", by.y = "id_mandato")
ok(nrow(m) == nrow(p), "todo id_mandato_bocel existe em mandatos.csv com cd_cargo 7 e sg_uf SP")
ok(all(m$ano_eleicao == m$ano_bocel), "ano_eleicao da linha bate com o ano do mandato no BOCEL")
ok(all(m$id_pessoa_bocel == m$id_pessoa_bocel_ref), "id_pessoa_bocel bate com o titular do mandato no BOCEL")
ok(p[, uniqueN(id_fonte), by = id_mandato_bocel][V1 > 1, .N] == 0L,
   "nenhum id_mandato_bocel apontando para duas pessoas distintas da fonte")

# re-derivacao independente dos numeros a partir do JSON bruto
man <- fromJSON(file.path(raw, "mandatos.json"), simplifyVector = FALSE)
mm <- rbindlist(lapply(man, function(k) data.table(
  leg = as.integer(k$legislatura$nuLegislatura),
  mat = as.character(k$parlamentar$nuMatricula),
  ini = substr(k$dtInicio, 1, 10), fim = substr(k$dtTermino, 1, 10))))
mm <- unique(mm[leg %in% 14:20])
xj <- unique(x[, .(leg = as.integer(legislatura), mat = id_fonte,
                   ini = data_inicio_exercicio, fim = data_fim_exercicio)][!is.na(ini)])
mmu <- unique(mm)
faltam <- fsetdiff(xj, mmu)
ok(nrow(faltam) == 0L,
   sprintf("toda linha datada do CSV reproduz exatamente um registro do JSON bruto (%d de %d)",
           nrow(xj) - nrow(faltam), nrow(xj)))
if (nrow(faltam)) print(head(faltam, 10))
sobra <- fsetdiff(mmu[leg %in% 14:20], xj)
ok(nrow(sobra) == 0L,
   sprintf("nenhum registro de mandato do JSON bruto ficou fora do CSV (%d sobrando)", nrow(sobra)))
if (nrow(sobra)) print(head(sobra, 10))

# composicao: o CSV cobre toda a composicao publicada pela casa
comp <- fromJSON(file.path(raw, "legislaturas_14_20.json"), simplifyVector = FALSE)
nc <- sum(vapply(comp, length, integer(1)))
ok(uniqueN(x[, .(legislatura, id_fonte)]) == nc,
   sprintf("uma linha por parlamentar x legislatura da composicao da casa (%d)", nc))

reg("n_linhas", nrow(x))
reg("n_pareadas", nrow(p))
reg("n_com_data_inicio", x[!is.na(data_inicio_exercicio), .N])
reg("n_com_data_fim", x[!is.na(data_fim_exercicio), .N])
reg("n_com_forma_saida_observada", x[!is.na(forma_saida) & forma_saida != "nao_observado", .N])
reg("n_checks_passou", length(passou))
reg("n_checks_falhou", length(falhou))

FORA <- c(
  "pertinencia semantica do pareamento por nome (homonimo, apelido e nome de urna divergente do civil)",
  "completude do acervo publicado pela ALESP (mandatos ausentes do registro da casa nao aparecem aqui)",
  "veracidade do que a casa publica (datas e situacao sao tomadas como declaradas pela ALESP)",
  "causa da saida antecipada quando a casa nao publica ato ou licenca (fica em forma_saida = outro)",
  "condicao titular/suplente das linhas nao pareadas sem data de inicio (fica nao_informado)")
rel <- gravar_relatorio_verificacao(
  alvo = "data/assembleias2/SP.csv (ALESP, deputado estadual, 1998-2022)",
  script = script, passou = passou, falhou = falhou, fora_de_cobertura = FORA, dir = verd)
cat("\nrelatorio:", rel, "\npassou:", length(passou), "| falhou:", length(falhou), "\n")
if (length(falhou)) quit(status = 1L)
sink()
