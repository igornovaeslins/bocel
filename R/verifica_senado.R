# verifica_senado.R — verificacao cetica independente de data/exercicio_senado.csv
# (frente Senado, construido por R/07_exercicio_senado.R). Reconta os numeros a partir
# do arquivo de saida, aplica os asserts de rigor e procura erros silenciosos.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_senado.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(stringi) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "data_referencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_senado.R"
logf <- "logs/verifica_senado.log"; sink(logf, split = TRUE)
cat("verifica_senado.R —", format(Sys.time()), "\n")
problemas <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); cat("PROBLEMA:", msg, "\n") }

## 1. arquivo, colunas, codigo de ausente -------------------------------------------------
f_csv <- "data/exercicio_senado.csv"; f_pq <- "data/exercicio_senado.parquet"
stopifnot(file.exists(f_csv), file.exists(f_pq))
bruto <- fread(f_csv, na.strings = NULL, colClasses = "character", encoding = "UTF-8")
cols_prometidas <- c("id_pessoa","id_mandato","codigo_senador","nome_senado","nome_parlamentar",
  "dt_nascimento","sexo","sg_uf","legislatura","leg_segunda","ano_eleicao_ref","codigo_mandato",
  "condicao","participacao","titular_codigo","titular_nome","codigo_exercicio",
  "data_inicio_exercicio","data_fim_exercicio","sigla_causa","causa_afastamento","forma_saida",
  "metodo_pareamento","fonte","url_fonte")
faltam <- setdiff(cols_prometidas, names(bruto))
if (length(faltam)) prob(paste("colunas ausentes:", paste(faltam, collapse = ",")))
if (any(names(bruto) != tolower(names(bruto))) || any(grepl("[^a-z0-9_]", names(bruto))))
  prob("nome de coluna fora do padrao minusculo sem acento")
n_vazio <- sum(bruto == "", na.rm = TRUE)
cat("celulas vazias (deveriam ser 'NA'):", n_vazio, "\n")
if (n_vazio > 0) prob(sprintf("%d celulas vazias em vez de 'NA'", n_vazio))
n_na_tok <- sum(bruto == "NA")
cat("celulas com token 'NA':", n_na_tok, "\n")
pq <- as.data.table(read_parquet(f_pq))
if (nrow(pq) != nrow(bruto) || !identical(names(pq), names(bruto))) prob("parquet difere do csv em linhas/colunas")

## 2. chave unica e vocabulario -----------------------------------------------------------
d <- fread(f_csv, na.strings = "NA", encoding = "UTF-8",
           colClasses = list(character = c("codigo_senador","codigo_mandato","codigo_exercicio",
                                           "titular_codigo","id_pessoa","id_mandato")))
cat("linhas:", nrow(d), "\n")
checa_unica(as.data.frame(d[!is.na(codigo_exercicio)]), c("codigo_mandato", "codigo_exercicio"))
cat("linhas sem codigo_exercicio:", d[is.na(codigo_exercicio), .N], "\n")
if (d[is.na(codigo_exercicio), .N] > 0) checa_unica(as.data.frame(d[is.na(codigo_exercicio)]), "codigo_mandato")
checa_unica(as.data.frame(d), c("codigo_senador", "codigo_mandato", "codigo_exercicio"))
vocab <- c("fim_regular","renuncia","falecimento","cassacao","afastamento","licenca",
           "nao_tomou_posse","suplente_efetivado","outro")
in_set(d$forma_saida, vocab, nome = "forma_saida")
in_set(d$condicao, c("titular","suplente"), permitir_na = FALSE, nome = "condicao")
ufs <- c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PR","PE",
         "PI","RJ","RN","RS","RO","RR","SC","SP","SE","TO")
in_set(d$sg_uf, ufs, permitir_na = FALSE, nome = "sg_uf")
in_set(d$metodo_pareamento, c("nome_nascimento","nome_uf_ano","nascimento_uf_ano"), nome = "metodo_pareamento")
in_set(d$sexo, c("Masculino","Feminino"), nome = "sexo")
em_faixa(d$legislatura, 51, 57, permitir_na = FALSE, nome = "legislatura")
em_faixa(d$ano_eleicao_ref, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao_ref")
if (!all(d$legislatura == 51 + (d$ano_eleicao_ref - 1998) %/% 4)) prob("legislatura x ano_eleicao_ref inconsistentes")

## 3. datas -------------------------------------------------------------------------------
ini <- as.IDate(d$data_inicio_exercicio); fim <- as.IDate(d$data_fim_exercicio); nasc <- as.IDate(d$dt_nascimento)
if (any(is.na(ini) & !is.na(d$data_inicio_exercicio))) prob("data_inicio nao parseavel")
if (any(is.na(fim) & !is.na(d$data_fim_exercicio))) prob("data_fim nao parseavel")
cat("exercicios sem data_inicio:", sum(is.na(ini)), "\n")
em_faixa(as.integer(format(ini, "%Y")), 1998, 2026, nome = "ano_inicio_exercicio")
em_faixa(as.integer(format(fim, "%Y")), 1998, 2026, nome = "ano_fim_exercicio")
n_inv <- sum(!is.na(fim) & !is.na(ini) & fim < ini)
cat("fim antes do inicio:", n_inv, "\n"); if (n_inv > 0) prob(sprintf("%d exercicios com fim antes do inicio", n_inv))
n_fut <- sum(!is.na(fim) & fim > data_referencia(root)); cat("fim no futuro:", n_fut, "\n")
if (n_fut > 0) prob(sprintf("%d exercicios com data_fim no futuro", n_fut))
# inicio antes do inicio nominal da legislatura (1 fev do ano seguinte a eleicao)
ini_leg <- as.IDate(sprintf("%d-02-01", d$ano_eleicao_ref + 1))
n_antes <- sum(!is.na(ini) & ini < ini_leg)
cat("exercicios iniciados antes de 1/fev pos-eleicao:", n_antes, "\n")
if (n_antes > 0) print(d[!is.na(ini) & ini < ini_leg, .(nome_parlamentar, sg_uf, legislatura, condicao, data_inicio_exercicio)])
# fim depois do fim nominal do mandato (31 jan, 8 anos depois)
fim_leg <- as.IDate(sprintf("%d-01-31", d$ano_eleicao_ref + 9))
n_dep <- sum(!is.na(fim) & fim > fim_leg); cat("exercicios encerrados apos fim nominal do mandato:", n_dep, "\n")
if (n_dep > 0) prob(sprintf("%d exercicios terminam depois do fim nominal do mandato", n_dep))
idade <- as.numeric(ini - nasc) / 365.25
em_faixa(idade, 35, 100, nome = "idade_na_posse")
# em curso (fim NA) so faz sentido para legislaturas ainda vigentes (56 e 57)
em_curso_antigo <- d[is.na(data_fim_exercicio) & legislatura < 56]
cat("exercicios 'em curso' em legislaturas encerradas:", nrow(em_curso_antigo), "\n")
if (nrow(em_curso_antigo)) { prob("exercicio sem data_fim em legislatura ja encerrada"); print(em_curso_antigo[, .(nome_parlamentar, sg_uf, legislatura, condicao, data_inicio_exercicio)]) }
# sobreposicao de exercicios dentro do mesmo mandato
setorder(d, codigo_mandato, data_inicio_exercicio)
d[, fim_prev := shift(as.IDate(data_fim_exercicio)), by = codigo_mandato]
n_sobre <- d[!is.na(fim_prev) & as.IDate(data_inicio_exercicio) < fim_prev, .N]
cat("exercicios que comecam antes do fim do anterior (mesmo mandato):", n_sobre, "\n")
if (n_sobre > 0) print(d[!is.na(fim_prev) & as.IDate(data_inicio_exercicio) < fim_prev,
                         .(nome_parlamentar, codigo_mandato, data_inicio_exercicio, fim_prev)])
d[, fim_prev := NULL]

## 4. mapeamento sigla -> forma_saida e semantica --------------------------------------------
ct <- d[, .N, by = .(condicao, sigla_causa, forma_saida)][order(condicao, sigla_causa)]
cat("\nsigla_causa x forma_saida:\n"); print(ct, nrows = 200)
# forma_saida NA <=> data_fim NA
if (!all(is.na(d$forma_saida) == is.na(d$data_fim_exercicio))) prob("forma_saida NA nao coincide com data_fim NA")
# 'outro' deve ser apenas RET (retorno do titular) em suplentes
outro_nao_ret <- d[forma_saida == "outro" & (is.na(sigla_causa) | sigla_causa != "RET" | condicao != "suplente")]
cat("linhas 'outro' que nao sao RET de suplente:", nrow(outro_nao_ret), "\n")
if (nrow(outro_nao_ret)) print(outro_nao_ret[, .(nome_parlamentar, condicao, sigla_causa, causa_afastamento)])
# nao_tomou_posse
cat("nao_tomou_posse:", d[forma_saida == "nao_tomou_posse", .N], "\n")

## 5. pareamento: inflacao e coerencia -------------------------------------------------------
# cada codigo_senador -> no maximo um id_pessoa; cada id_pessoa -> um codigo_senador
m1 <- unique(d[!is.na(id_pessoa), .(codigo_senador, id_pessoa)])
if (anyDuplicated(m1$codigo_senador)) prob("codigo_senador com mais de um id_pessoa")
if (anyDuplicated(m1$id_pessoa)) { prob("id_pessoa com mais de um codigo_senador"); print(m1[id_pessoa %in% m1[duplicated(id_pessoa), id_pessoa]]) }
# cada id_mandato -> um codigo_mandato e um codigo_senador; so em titulares
m2 <- unique(d[!is.na(id_mandato), .(id_mandato, codigo_mandato, codigo_senador, condicao)])
if (anyDuplicated(m2$id_mandato)) { prob("id_mandato ligado a mais de um codigo_mandato (pareamento inflado)"); print(m2[id_mandato %in% m2[duplicated(id_mandato), id_mandato]]) }
if (anyDuplicated(m2$codigo_mandato)) prob("codigo_mandato ligado a mais de um id_mandato")
if (any(m2$condicao != "titular")) prob("id_mandato atribuido a nao-titular")
# id_mandato codifica ano e UF: M<ano>_<UF>_5_...
pm <- unique(d[!is.na(id_mandato), .(id_mandato, ano_eleicao_ref, sg_uf)])
pm[, `:=`(ano_id = as.integer(substr(id_mandato, 2, 5)), uf_id = substr(id_mandato, 7, 8), cargo_id = substr(id_mandato, 10, 10))]
if (!all(pm$ano_id == pm$ano_eleicao_ref & pm$uf_id == pm$sg_uf & pm$cargo_id == "5")) prob("id_mandato incoerente com ano/UF/cargo da linha")
# conferencia contra o BOCEL: id_mandato existe em mandatos.csv com cd_cargo 5, mesma pessoa
bm <- fread("data/mandatos.csv", na.strings = "NA", select = c("id_mandato","id_pessoa","ano_eleicao","cd_cargo","sg_uf","forma_saida"))
bm5 <- bm[cd_cargo == 5]
cat("eleitos cd_cargo 5 no BOCEL:", nrow(bm5), " por ano:\n"); print(bm5[, .N, by = ano_eleicao][order(ano_eleicao)])
chk <- merge(unique(d[!is.na(id_mandato), .(id_mandato, id_pessoa_sen = id_pessoa)]), bm5, by = "id_mandato", all.x = TRUE)
if (any(is.na(chk$id_pessoa))) prob("id_mandato da saida nao existe em mandatos.csv cd_cargo 5")
if (any(chk$id_pessoa != chk$id_pessoa_sen)) prob("id_pessoa da saida difere do id_pessoa do mandato no BOCEL")
n_bocel_pareados <- uniqueN(chk$id_mandato); cat("eleitos BOCEL pareados:", n_bocel_pareados, "/", nrow(bm5), "\n")
nao_par <- bm5[!id_mandato %in% chk$id_mandato]; cat("eleitos BOCEL sem pareamento:", nrow(nao_par), "\n")
# pareamento por pessoa: nome e nascimento batem com pessoas.csv?
bp <- fread("data/pessoas.csv", na.strings = "NA", select = c("id_pessoa","nome","dt_nascimento","chave_dedup"))
norm <- function(x) gsub(" +", " ", trimws(gsub("[^A-Z ]", "", stri_trans_general(toupper(x), "Latin-ASCII"))))
pp <- merge(unique(d[!is.na(id_pessoa), .(id_pessoa, nome_senado, dt_nascimento, metodo_pareamento, sg_uf, condicao)]),
            bp, by = "id_pessoa", all.x = TRUE)
if (any(is.na(pp$nome))) prob("id_pessoa da saida nao existe em pessoas.csv")
pp[, `:=`(nome_ok = norm(nome_senado) == norm(nome), nasc_ok = as.character(dt_nascimento.x) == as.character(dt_nascimento.y))]
cat("\npareados por metodo x nome bate x nascimento bate:\n"); print(pp[, .N, by = .(metodo_pareamento, nome_ok, nasc_ok)])
cat("\nfallbacks (inspecao manual):\n"); print(pp[metodo_pareamento != "nome_nascimento", .(id_pessoa, nome_senado, nome, dt_nascimento.x, dt_nascimento.y, sg_uf)])
if (pp[metodo_pareamento == "nome_nascimento" & !(nome_ok & nasc_ok), .N] > 0) prob("pareamento nome_nascimento sem nome+nascimento identicos")
# nascimento_uf_ano: nomes devem partilhar pelo menos o primeiro ou ultimo token
pp[, tok_ok := mapply(function(a, b) length(intersect(strsplit(a, " ")[[1]], strsplit(b, " ")[[1]])) >= 2, norm(nome_senado), norm(nome))]
sus <- pp[metodo_pareamento == "nascimento_uf_ano" & !tok_ok]
cat("fallback nascimento_uf_ano com nomes sem 2 tokens em comum:", nrow(sus), "\n"); if (nrow(sus)) print(sus[, .(nome_senado, nome)])
# homonimos: chave nome+nascimento duplicada em pessoas.csv que colide com senador
bp[, nn := norm(nome)]; bp[, k := paste(nn, dt_nascimento)]
dupk <- bp[k %in% bp[duplicated(k), k] & !is.na(dt_nascimento)]
col <- dupk[k %in% paste(norm(d$nome_senado), d$dt_nascimento)]
cat("senadores cujo nome+nascimento e ambiguo em pessoas.csv (2+ id_pessoa):", uniqueN(col$k), "\n")
if (nrow(col)) print(col[order(k), .(id_pessoa, nome, dt_nascimento, chave_dedup)])

## 6. cobertura por UF x legislatura ---------------------------------------------------------
esp <- data.table(legislatura = 51:57, esperado = c(1L, 2L, 1L, 2L, 1L, 2L, 1L))
cov <- d[condicao == "titular", .(n_mand = uniqueN(codigo_mandato), n_bocel = uniqueN(na.omit(id_mandato))), by = .(legislatura, sg_uf)]
cov <- merge(CJ(legislatura = 51:57, sg_uf = ufs), cov, by = c("legislatura", "sg_uf"), all.x = TRUE)
cov[is.na(n_mand), `:=`(n_mand = 0L, n_bocel = 0L)]
cov <- merge(cov, esp, by = "legislatura")
def <- cov[n_bocel < esperado]
cat("\nUF x legislatura com menos eleitos BOCEL pareados que o esperado:", nrow(def), "\n"); if (nrow(def)) print(def)
exc <- cov[n_mand > esperado]
cat("UF x legislatura com mais mandatos de titular que vagas (suplente efetivado/suplementar):", nrow(exc), "\n"); print(exc)
fwrite(cov, "output/verificacao/senado_cobertura_uf_legislatura.csv")
# suplentes: exercicio referencia um titular que existe como titular na mesma UF/legislatura?
sup <- d[condicao == "suplente"]
tit_keys <- unique(d[condicao == "titular", .(titular_codigo = codigo_senador, sg_uf, legislatura)])
sup_chk <- merge(sup[, .(codigo_senador, titular_codigo, sg_uf, legislatura)], tit_keys[, .(titular_codigo, sg_uf, legislatura, tem = TRUE)],
                 by = c("titular_codigo", "sg_uf", "legislatura"), all.x = TRUE)
cat("exercicios de suplente cujo titular nao consta como titular na mesma UF/legislatura:", sup_chk[is.na(tem), .N], "\n")
if (sup_chk[is.na(tem), .N]) print(unique(sup_chk[is.na(tem)]))
# suplentes com id_pessoa: pessoa deve existir e o mesmo suplente nao pode ter id_mandato
if (d[condicao == "suplente" & !is.na(id_mandato), .N]) prob("suplente com id_mandato")

## 7. recontagem dos numeros registrados -------------------------------------------------------
rec <- list(
  senado_n_senadores_api_com_mandato_51_57 = uniqueN(d$codigo_senador),
  senado_n_titulares_api_51_57 = uniqueN(d[condicao == "titular", codigo_senador]),
  senado_n_mandatos_titular_api_51_57 = uniqueN(d[condicao == "titular", codigo_mandato]),
  senado_n_suplentes_que_exerceram_51_57 = uniqueN(d[condicao == "suplente", codigo_senador]),
  senado_n_eleitos_bocel_cd5 = nrow(bm5),
  senado_n_pareados_bocel_pessoas = uniqueN(d[!is.na(id_pessoa), codigo_senador]),
  senado_n_pareados_bocel_mandatos = uniqueN(na.omit(d$id_mandato)),
  senado_n_eleitos_bocel_nao_pareados = nrow(nao_par),
  senado_n_titulares_api_sem_bocel = uniqueN(d[condicao == "titular" & is.na(id_mandato), .(codigo_senador, codigo_mandato)]),
  senado_n_linhas_exercicio = nrow(d),
  senado_n_pareados_metodo_nome_nascimento = uniqueN(d[metodo_pareamento == "nome_nascimento", codigo_senador]),
  senado_n_pareados_metodo_nascimento_uf_ano = uniqueN(d[metodo_pareamento == "nascimento_uf_ano", codigo_senador]),
  senado_n_pareados_metodo_nome_uf_ano = uniqueN(d[metodo_pareamento == "nome_uf_ano", codigo_senador]),
  senado_n_forma_saida_NA_em_exercicio = d[is.na(forma_saida), .N])
for (v in vocab) rec[[paste0("senado_n_forma_saida_", v)]] <- d[forma_saida == v, .N]
tx <- merge(bm5[, .(n_bocel = .N), by = ano_eleicao], d[condicao == "titular" & !is.na(id_mandato), .(n_par = uniqueN(id_mandato)), by = ano_eleicao_ref],
            by.x = "ano_eleicao", by.y = "ano_eleicao_ref", all.x = TRUE)
for (i in seq_len(nrow(tx))) rec[[sprintf("senado_taxa_pareamento_leg%d_%d", 51 + (tx$ano_eleicao[i] - 1998) %/% 4, tx$ano_eleicao[i])]] <-
  sprintf("%d/%d=%.4f", tx$n_par[i], tx$n_bocel[i], tx$n_par[i] / tx$n_bocel[i])
# contagens que dependem do cache bruto
listas <- unique(unlist(lapply(51:57, function(n) {
  j <- jsonlite::fromJSON(sprintf("data_raw/senado/lista_legislatura_%d.json", n), simplifyVector = FALSE)
  p <- j$ListaParlamentarLegislatura$Parlamentares$Parlamentar
  if (!is.null(names(p))) p <- list(p)
  vapply(p, function(x) as.character(x$IdentificacaoParlamentar$CodigoParlamentar), "")
})))
rec$senado_n_senadores_api_listas_51_57 <- length(listas)
n_files <- c(mand = length(list.files("data_raw/senado", "^mandatos_")), det = length(list.files("data_raw/senado", "^detalhe_")))
cat("arquivos de cache: mandatos", n_files["mand"], "detalhe", n_files["det"], "; codigos em listas", length(listas), "\n")
if (!all(n_files == length(listas))) prob("cache incompleto: n arquivos != n codigos")
lac <- fread("data_raw/senado/_lacunas.csv"); cat("lacunas de API:", nrow(lac), "\n")
sem_nasc <- sum(vapply(listas, function(cod) {
  j <- jsonlite::fromJSON(sprintf("data_raw/senado/detalhe_%s.json", cod), simplifyVector = FALSE)
  is.null(j$DetalheParlamentar$Parlamentar$DadosBasicosParlamentar$DataNascimento) }, logical(1)))
rec$senado_n_sem_data_nascimento_api <- sem_nasc
# a saida diz que todos os 63 sao suplentes: os titulares tem dt_nascimento?
cat("titulares na saida sem dt_nascimento:", d[condicao == "titular" & is.na(dt_nascimento), uniqueN(codigo_senador)], "\n")
cat("suplentes na saida sem dt_nascimento:", d[condicao == "suplente" & is.na(dt_nascimento), uniqueN(codigo_senador)], "\n")

ass <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, strip.white = TRUE, colClasses = "character")
setnames(ass, c("chave","valor","ep","data","md5","out"))
ass <- ass[grepl("^senado_", chave)]
cat("\nlinhas senado_* em numeros_assinatura (com repeticoes):", nrow(ass), "; chaves distintas:", uniqueN(ass$chave), "\n")
incons <- ass[, .(n_val = uniqueN(valor)), by = chave][n_val > 1]
if (nrow(incons)) cat("AVISO: chaves com mais de um valor registrado ao longo do tempo (arquivo append-only; vale o ultimo):", incons$chave, "\n")
ass_u <- ass[, .(valor = valor[.N]), by = chave]   # ultimo registro de cada chave
comp <- rbindlist(lapply(names(rec), function(k) data.table(chave = k, recontado = as.character(rec[[k]]),
                                                             registrado = ass_u[chave == k, valor][1])))
# chave nao registrada so e aceitavel quando a recontagem da zero (o construtor so registra categorias observadas)
comp[, bate := fifelse(is.na(registrado), recontado == "0", recontado == registrado)]
cat("\nrecontagem x registro:\n"); print(comp, nrows = 100)
if (any(is.na(comp$bate) | !comp$bate)) prob(paste("numeros que nao batem:", paste(comp[is.na(bate) | !bate, chave], collapse = ",")))
nao_rec <- setdiff(ass_u$chave, comp$chave); if (length(nao_rec)) cat("chaves registradas nao recontadas:", nao_rec, "\n")
fwrite(comp, "output/verificacao/senado_recontagem.csv")
numeros_conferem <- all(comp$bate)

## 8. registro -------------------------------------------------------------------------------
reg <- function(k, v) registrar_numero(k, v, script = script, out = "output/numeros_assinatura.txt")
reg("verif_senado_n_linhas", nrow(d))
reg("verif_senado_n_numeros_conferidos", sprintf("%d/%d", sum(comp$bate, na.rm = TRUE), nrow(comp)))
reg("verif_senado_n_fim_antes_inicio", n_inv)
reg("verif_senado_n_sobreposicao_exercicio_mesmo_mandato", n_sobre)
reg("verif_senado_n_uf_leg_deficit_bocel", nrow(def))
reg("verif_senado_n_uf_leg_excesso_titular", nrow(exc))
reg("verif_senado_n_problemas", length(problemas))
gravar_relatorio_verificacao(alvo = "data/exercicio_senado.csv", script = script,
  passou = c("colunas prometidas presentes; ausente = 'NA'", "chave (codigo_mandato, codigo_exercicio) unica",
             "forma_saida, condicao, sg_uf, metodo_pareamento no vocabulario", "datas parseaveis, fim >= inicio, anos 1998-2026",
             "id_mandato 1:1 com codigo_mandato; id_pessoa 1:1 com codigo_senador", sprintf("numeros conferidos: %d/%d", sum(comp$bate, na.rm = TRUE), nrow(comp))),
  falhou = problemas,
  fora_de_cobertura = c("validade das datas informadas pelo Senado", "identidade de suplentes sem data de nascimento (homonimos)",
                        "reexecucao do fetch com --force (cache reutilizado; 0 lacunas registradas)"))
cat("\nPROBLEMAS:", length(problemas), "\n"); if (length(problemas)) print(problemas)
cat("numeros_conferem:", numeros_conferem, "\nverifica_senado: concluido —", format(Sys.time()), "\n")
sink()
