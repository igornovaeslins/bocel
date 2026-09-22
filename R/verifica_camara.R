# verifica_camara.R — verificacao cetica de data/exercicio_camara.csv (frente 'camara')
# Reconta os numeros registrados por R/07_exercicio_camara.R diretamente dos arquivos,
# aplica os asserts de lib/asserts_rigor.R e procura erros silenciosos.
# Rodada de 28/08/2026: compara com o ULTIMO registro de cada chave (sem fixar checksum
# do construtor), confere id_pessoa em pessoas.csv, janela da legislatura +-60 dias,
# taxa de pareamento recontada contra os 513 eleitos (cd_cargo 6) e amostra ao vivo da API.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_camara.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
SCRIPT <- "R/verifica_camara.R"
logf <- file("logs/verifica_camara.log", open = "wt")
logm <- function(...) { m <- paste(..., collapse = " "); cat(m, "\n"); writeLines(m, logf) }
reg <- function(k, v) registrar_numero(k, v, script = SCRIPT, out = "output/numeros_assinatura.txt")
problemas <- character()
prob <- function(msg) { problemas <<- c(problemas, msg); logm("PROBLEMA:", msg) }

## ------------------------------------------------------------ arquivos e colunas
f_csv <- "data/exercicio_camara.csv"
stopifnot(file.exists(f_csv), file.exists("data/exercicio_camara.parquet"), file.exists("data/exercicio_camara.rds"))
x <- fread(f_csv, na.strings = "NA", colClasses = "character", encoding = "UTF-8")
cols_prometidas <- c("id_pessoa", "id_mandato", "id_deputado_camara", "legislatura", "ano_eleicao",
                     "nome_civil", "nome_parlamentar", "cpf", "dt_nascimento", "dt_falecimento",
                     "sg_uf", "sg_partido", "condicao", "efetivado", "data_inicio_exercicio",
                     "data_fim_exercicio", "situacao_final", "forma_saida", "descricao_entrada",
                     "descricao_saida", "regra_pareamento", "fonte", "url_fonte")
stopifnot(identical(names(x), cols_prometidas))
raw_txt <- fread(f_csv, na.strings = NULL, colClasses = "character", encoding = "UTF-8")
n_vazias <- sum(sapply(raw_txt, function(v) sum(v == "")))
if (n_vazias > 0) prob(sprintf("%d celulas vazias (deveriam ser 'NA')", n_vazias))
pq <- as.data.table(read_parquet("data/exercicio_camara.parquet")); rd <- readRDS("data/exercicio_camara.rds")
stopifnot(nrow(pq) == nrow(x), nrow(rd) == nrow(x), identical(names(pq), names(x)))
stopifnot(identical(as.character(pq$id_deputado_camara), x$id_deputado_camara))
logm("linhas:", nrow(x), "| colunas:", ncol(x))

## ------------------------------------------------------------ asserts de rigor
x[, `:=`(legislatura_i = as.integer(legislatura), ano_i = as.integer(ano_eleicao),
         id_dep_i = as.integer(id_deputado_camara))]
vocab_livro <- c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento",
                 "licenca", "nao_tomou_posse", "suplente_efetivado", "outro")
in_set(x$forma_saida, vocab_livro, permitir_na = TRUE, nome = "forma_saida")
in_set(x$condicao, c("titular", "suplente"), permitir_na = FALSE, nome = "condicao")
in_set(x$efetivado, c("TRUE", "FALSE"), permitir_na = FALSE, nome = "efetivado")
in_set(x$fonte, "camara_api", permitir_na = FALSE, nome = "fonte")
# 13/09/2026: regra 4 do R/07 (nascimento, UF, legislatura e primeiro nome)
in_set(x$regra_pareamento, c("cpf", "nome_nascimento", "nome_unico_dep_fed", "nascimento_uf_legislatura_primeiro_nome"), permitir_na = TRUE, nome = "regra_pareamento")
ufs <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")
in_set(x$sg_uf, ufs, permitir_na = TRUE, nome = "sg_uf")
if (anyNA(x$sg_uf)) prob(sprintf("%d linhas com sg_uf NA", sum(is.na(x$sg_uf))))
em_faixa(x$legislatura_i, 51, 57, permitir_na = FALSE, nome = "legislatura")
em_faixa(x$ano_i, 1998, 2022, permitir_na = FALSE, nome = "ano_eleicao")
stopifnot(all(x$ano_i == 1998L + 4L * (x$legislatura_i - 51L)))
d_ini <- as.IDate(x$data_inicio_exercicio); d_fim <- as.IDate(x$data_fim_exercicio)
stopifnot(sum(is.na(d_ini)) == sum(is.na(x$data_inicio_exercicio)), sum(is.na(d_fim)) == sum(is.na(x$data_fim_exercicio)))
# datas plausiveis: inicio >= 1999-02-01, fim >= inicio, dentro da legislatura +-60 dias
n_ini_antes_1999 <- sum(!is.na(d_ini) & d_ini < as.IDate("1999-02-01"))
if (n_ini_antes_1999) prob(sprintf("%d inicios antes de 1999-02-01", n_ini_antes_1999))
em_faixa(as.integer(format(d_ini, "%Y")), 1999, 2026, nome = "ano_inicio_exercicio")
em_faixa(as.integer(format(d_fim, "%Y")), 1999, 2026, nome = "ano_fim_exercicio")
em_faixa(as.integer(format(as.IDate(x$dt_nascimento), "%Y")), 1900, 2005, nome = "ano_nascimento")
n_fim_antes <- sum(!is.na(d_ini) & !is.na(d_fim) & d_fim < d_ini)
if (n_fim_antes) prob(sprintf("%d periodos com fim antes do inicio", n_fim_antes))
lini <- as.IDate(sprintf("%d-02-01", 1999L + 4L * (x$legislatura_i - 51L)))
lfim <- as.IDate(sprintf("%d-01-31", 2003L + 4L * (x$legislatura_i - 51L)))
n_ini_fora <- sum(!is.na(d_ini) & (d_ini < lini | d_ini > lfim))
n_fim_fora <- sum(!is.na(d_fim) & (d_fim < lini | d_fim > lfim))
n_ini_fora60 <- sum(!is.na(d_ini) & (d_ini < lini - 60L | d_ini > lfim + 60L))
n_fim_fora60 <- sum(!is.na(d_fim) & (d_fim < lini - 60L | d_fim > lfim + 60L))
logm("inicio fora da janela exata:", n_ini_fora, "| fim fora:", n_fim_fora,
     "| fora da janela +-60d: inicio", n_ini_fora60, "fim", n_fim_fora60)
if (n_ini_fora60) prob(sprintf("%d inicios fora da janela da legislatura +-60 dias", n_ini_fora60))
if (n_fim_fora60) prob(sprintf("%d fins fora da janela da legislatura +-60 dias", n_fim_fora60))
d_fal <- as.IDate(x$dt_falecimento)
n_pos_obito <- sum(!is.na(d_fal) & !is.na(d_ini) & d_ini > d_fal)
if (n_pos_obito) prob(sprintf("%d periodos iniciados depois da dataFalecimento", n_pos_obito))
n_fim_pos_obito <- sum(!is.na(d_fal) & !is.na(d_fim) & d_fim > d_fal + 1L)
logm("periodos com fim > falecimento+1d:", n_fim_pos_obito)
n_aberto_fora57 <- x[is.na(forma_saida) & legislatura_i != 57L, .N]
if (n_aberto_fora57) prob(sprintf("%d periodos sem forma_saida fora da legislatura 57", n_aberto_fora57))
stopifnot(x[is.na(forma_saida), all(situacao_final == "Exercício")])
# forma_saida coerente com o texto de saida da Camara
n_fal_txt <- x[grepl("Falecimento", descricao_saida) & forma_saida != "falecimento", .N]
n_cas_txt <- x[grepl("Perda de Mandato", descricao_saida) & forma_saida != "cassacao", .N]
n_ren_txt <- x[grepl("Renúncia", descricao_saida) & forma_saida != "renuncia" & forma_saida != "nao_tomou_posse", .N]
if (n_fal_txt + n_cas_txt + n_ren_txt) prob(sprintf("forma_saida incoerente com o texto: falecimento %d, cassacao %d, renuncia %d", n_fal_txt, n_cas_txt, n_ren_txt))

## ------------------------------------------------------------ chaves
df <- as.data.frame(x)
com_data <- df[!is.na(df$data_inicio_exercicio), ]
checa_unica(com_data, c("id_deputado_camara", "legislatura", "data_inicio_exercicio", "condicao"))
sem_data <- df[is.na(df$data_inicio_exercicio), ]
# sem data de inicio a chave declarada nao distingue: conferir por deputado x legislatura x fim x situacao
checa_unica(sem_data[!is.na(sem_data$id_deputado_camara), ], c("id_deputado_camara", "legislatura", "data_fim_exercicio", "situacao_final"))
checa_unica(sem_data[is.na(sem_data$id_deputado_camara), ], "id_mandato")
n_sem_data_multi <- as.data.table(sem_data)[, .N, by = .(id_deputado_camara, legislatura)][N > 1, .N]
logm("pares deputado x legislatura com mais de uma linha sem data de inicio:", n_sem_data_multi)
checa_unica(df[!is.na(df$id_mandato) & is.na(df$data_inicio_exercicio) & is.na(df$data_fim_exercicio), ], c("id_mandato"))
m1 <- x[!is.na(id_pessoa) & !is.na(id_deputado_camara), uniqueN(id_pessoa), by = id_deputado_camara][V1 > 1]
m2 <- x[!is.na(id_pessoa), uniqueN(id_deputado_camara), by = id_pessoa][V1 > 1]
if (nrow(m1)) prob(sprintf("%d ids da Camara com mais de uma id_pessoa", nrow(m1)))
if (nrow(m2)) prob(sprintf("%d id_pessoa com mais de um id da Camara: %s", nrow(m2), paste(head(m2$id_pessoa, 5), collapse = ",")))

## ------------------------------------------------------------ integridade referencial
pess <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character")
mand <- fread("data/mandatos.csv", na.strings = "NA", colClasses = "character")
mand[, `:=`(ano_eleicao = as.integer(ano_eleicao), cd_cargo = as.integer(cd_cargo))]
checa_unica(as.data.frame(pess), "id_pessoa")
n_pessoa_orfa <- x[!is.na(id_pessoa) & !id_pessoa %in% pess$id_pessoa, .N]
if (n_pessoa_orfa) prob(sprintf("%d linhas com id_pessoa ausente de pessoas.csv", n_pessoa_orfa))
n_mandato_orfao <- x[!is.na(id_mandato) & !id_mandato %in% mand$id_mandato, .N]
if (n_mandato_orfao) prob(sprintf("%d linhas com id_mandato ausente de mandatos.csv", n_mandato_orfao))
depf <- mand[cd_cargo == 6L & ano_eleicao %in% seq(1998L, 2022L, 4L), .(id_mandato, id_pessoa_bocel = id_pessoa, ano_bocel = ano_eleicao, uf_bocel = sg_uf)]
checa_unica(as.data.frame(depf), "id_mandato")
n_513 <- depf[, .N, by = ano_bocel]
if (!all(n_513$N == 513L)) prob(sprintf("eleitos cd_cargo 6 por ano != 513: %s", paste(n_513$ano_bocel, n_513$N, collapse = ";")))
xm <- join_seguro(as.data.frame(x[!is.na(id_mandato)]), as.data.frame(depf), by = "id_mandato",
                  cardinalidade = "many-to-one", tipo = "inner", unmatched = "error")
xm <- as.data.table(xm)
stopifnot(all(xm$id_pessoa == xm$id_pessoa_bocel), all(xm$ano_i == xm$ano_bocel))
n_uf_div <- xm[!is.na(sg_uf) & sg_uf != uf_bocel, .N]
if (n_uf_div) prob(sprintf("%d linhas com sg_uf da Camara diferente da UF do mandato no BOCEL", n_uf_div))
faltam <- setdiff(depf$id_mandato, x$id_mandato)
if (length(faltam)) prob(sprintf("%d mandatos cd_cargo 6 do BOCEL ausentes", length(faltam)))
# a pessoa da linha e a pessoa do mandato em mandatos.csv (independente do join acima)
xp <- merge(x[!is.na(id_mandato), .(id_mandato, id_pessoa)], mand[, .(id_mandato, id_pessoa_m = id_pessoa)], by = "id_mandato")
stopifnot(all(xp$id_pessoa == xp$id_pessoa_m))
sup_eleito <- x[!is.na(id_mandato), .(so_suplente = all(condicao == "suplente")), by = .(id_mandato)][so_suplente == TRUE, .N]
# divergencia entre fontes (TSE marca eleito por media; Camara registra posse como suplente): julgamento do autor, fora de cobertura
logm("mandatos eleitos no TSE cujos periodos na Camara sao todos de suplente:", sup_eleito)
if (sup_eleito) print(x[id_mandato %in% x[!is.na(id_mandato), .(so = all(condicao == "suplente")), by = id_mandato][so == TRUE, id_mandato],
                        .(id_mandato, id_deputado_camara, nome_civil, sg_uf, data_inicio_exercicio, descricao_entrada)])
ov <- x[!is.na(data_inicio_exercicio)][order(id_dep_i, as.IDate(data_inicio_exercicio))]
ov[, `:=`(ini = as.IDate(data_inicio_exercicio), fim = as.IDate(data_fim_exercicio))]
ov[, fim_prev := shift(fim), by = id_dep_i]
n_overlap <- ov[!is.na(fim_prev) & ini < fim_prev, .N]
logm("periodos que comecam antes do fim do anterior do mesmo deputado:", n_overlap)

## ------------------------------------------------------------ pareamento
norm_nome <- function(s) { s <- stringi::stri_trans_general(toupper(s), "Latin-ASCII"); gsub(" +", " ", trimws(gsub("[^A-Z ]", "", s))) }
pess[, nome_norm := norm_nome(nome)]
hom <- pess[, .N, by = .(nome_norm, dt_nascimento)][N > 1]
nn <- unique(x[regra_pareamento == "nome_nascimento", .(id_pessoa, nome_civil, dt_nascimento)])
nn[, nome_norm := norm_nome(nome_civil)]
n_nn_ambiguo <- nn[paste(nome_norm, dt_nascimento) %in% hom[, paste(nome_norm, dt_nascimento)], .N]
if (n_nn_ambiguo) prob(sprintf("%d pareamentos por nome+nascimento ambiguos", n_nn_ambiguo))
xc <- unique(x[regra_pareamento == "cpf", .(id_pessoa, cpf)])
xc <- merge(xc, pess[, .(id_pessoa, nr_cpf)], by = "id_pessoa")
stopifnot(all(xc$cpf == xc$nr_cpf))
xn <- unique(x[regra_pareamento == "cpf" & !is.na(nome_civil), .(id_pessoa, nome_civil)])
xn <- merge(xn, pess[, .(id_pessoa, nome)], by = "id_pessoa")
xn[, `:=`(a = norm_nome(nome_civil), b = norm_nome(nome))]
xn[, primeiro_igual := sapply(strsplit(a, " "), `[`, 1) == sapply(strsplit(b, " "), `[`, 1)]
xn[, ultimo_igual := sapply(strsplit(a, " "), function(v) v[length(v)]) == sapply(strsplit(b, " "), function(v) v[length(v)])]
n_nome_div <- xn[!primeiro_igual & !ultimo_igual, .N]
logm("pareados por CPF com primeiro E ultimo nome diferentes:", n_nome_div)
if (n_nome_div) print(xn[!primeiro_igual & !ultimo_igual, .(id_pessoa, nome_civil, nome)])
xd <- unique(x[regra_pareamento == "cpf" & !is.na(dt_nascimento), .(id_pessoa, dt_nascimento)])
xd <- merge(xd, pess[, .(id_pessoa, dtb = dt_nascimento)], by = "id_pessoa")
n_nasc_div <- xd[dt_nascimento != dtb, .N]
logm("pareados por CPF com nascimento diferente entre Camara e BOCEL:", n_nasc_div)

## ------------------------------------------------------------ taxa de pareamento recontada contra os 513
# com_exercicio = mandato com ao menos um periodo vindo do historico (entrada datada ou saida sem entrada previa)
tx_rec <- x[!is.na(id_mandato), .(eleitos = uniqueN(id_mandato), pareados = uniqueN(id_mandato[!is.na(id_deputado_camara) & situacao_final != "nao_pareado_camara"]),
                                   com_exercicio = uniqueN(id_mandato[!is.na(data_inicio_exercicio) | grepl("^sem evento de entrada", descricao_entrada)])), by = legislatura_i][order(legislatura_i)]
tx_reg <- fread("output/verificacao/camara_pareamento_por_legislatura.csv")
stopifnot(nrow(tx_rec) == 7L, all(tx_rec$eleitos == 513L))
stopifnot(all(tx_rec$pareados == tx_reg$pareados), all(tx_rec$com_exercicio == tx_reg$com_exercicio))
print(tx_rec)

## ------------------------------------------------------------ recontagem dos numeros registrados (ultimo registro por chave)
man <- fromJSON("data_raw/camara/manifest.json")
rec <- list(
  camara_n_deputados_api = man$n_ids,
  camara_n_linhas_exercicio = nrow(x),
  camara_n_eleitos_bocel_dep_fed = nrow(depf),
  camara_n_eleitos_nao_tomou_posse = x[forma_saida == "nao_tomou_posse", .N],
  camara_n_eleitos_nao_pareados = x[situacao_final == "nao_pareado_camara", .N],
  camara_n_eleitos_listados_sem_historico = x[!is.na(id_mandato) & situacao_final %in% c("listado_sem_historico", "falecimento_arquivo_massa"), .N],
  camara_n_suplentes_leg51_listados_sem_historico = x[legislatura_i == 51L & condicao == "suplente", .N],
  camara_n_suplentes_fora_bocel = x[condicao == "suplente" & is.na(id_pessoa), uniqueN(id_deputado_camara)],
  camara_n_falecimentos_arquivo_massa = x[situacao_final == "falecimento_arquivo_massa", .N],
  camara_n_periodos_leg51_no_historico = x[legislatura_i == 51L & !is.na(data_inicio_exercicio), .N],
  # 13/09/2026: a linha do eleito do TSE sem registro na Camara (nao_pareado_camara) tem pessoa e nao tem id da Camara, e o
  # uniqueN contava o vazio como mais um deputado
  camara_n_pareados_bocel = x[!is.na(id_pessoa) & !is.na(id_deputado_camara), uniqueN(id_deputado_camara)],
  camara_n_saidas_sem_entrada_previa = x[grepl("^sem evento de entrada", descricao_entrada), .N]
)
fs <- x[, .N, by = forma_saida]
for (i in seq_len(nrow(fs))) rec[[sprintf("camara_n_forma_saida_%s", if (is.na(fs$forma_saida[i])) "em_exercicio" else fs$forma_saida[i])]] <- fs$N[i]
for (L in 51:57) {
  rec[[sprintf("camara_n_com_periodo_exercicio_leg%d", L)]] <- tx_rec[legislatura_i == L, com_exercicio]
  rec[[sprintf("camara_taxa_pareamento_leg%d", L)]] <- sprintf("%d/513=%.4f", tx_rec[legislatura_i == L, pareados], tx_rec[legislatura_i == L, pareados] / 513)
}
reg_txt <- readLines("output/numeros_assinatura.txt")
reg_txt <- reg_txt[grepl("^camara_", reg_txt)]
rt <- rbindlist(lapply(strsplit(reg_txt, " \\| "), function(v) data.table(chave = trimws(v[1]), valor = trimws(v[2]))))
rt <- rt[!duplicated(chave, fromLast = TRUE)]
cmp <- rbindlist(lapply(names(rec), function(k) data.table(chave = k, recontado = as.character(rec[[k]]), registrado = rt[chave == k, valor][1])))
cmp[, bate := !is.na(registrado) & recontado == registrado]
print(cmp)
n_diverge <- cmp[bate == FALSE, .N]
if (n_diverge) prob(sprintf("%d numeros recontados divergem do registro: %s", n_diverge, paste(cmp[bate == FALSE, chave], collapse = ",")))

## ------------------------------------------------------------ amostra ao vivo da API (arquivos baixados em 28/08/2026)
# 05/09/2026: a amostra vivia num diretorio temporario de sessao; passa a ser lida de dentro do repositorio
live_dir <- file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "output", "verificacao", "camara_live")
n_live_igual <- NA_integer_; n_live_amostra <- NA_integer_
if (dir.exists(live_dir)) {
  fs_live <- list.files(live_dir, pattern = "\\.json$", full.names = TRUE)
  chave_ev <- function(d) sort(sapply(d, function(h) paste(h$idLegislatura, h$dataHora, h$situacao, h$condicaoEleitoral, h$descricaoStatus)))
  iguais <- sapply(fs_live, function(f) {
    id <- sub("\\.json$", "", basename(f))
    a <- chave_ev(fromJSON(f, simplifyVector = FALSE))
    b <- chave_ev(fromJSON(sprintf("data_raw/camara/historico/%s.json", id), simplifyVector = FALSE)$dados)
    identical(a, b)
  })
  n_live_amostra <- length(iguais); n_live_igual <- sum(iguais)
  logm("amostra ao vivo: historicos identicos ao cache:", n_live_igual, "de", n_live_amostra)
  if (n_live_igual < n_live_amostra) prob("historico ao vivo difere do cache para parte da amostra")
}
# casos conhecidos: forma de saida esperada
esperados <- data.table(id = c("141409","133372","73420","74782","74173","160672","132056","74269","204447","141378","74436","74778"),
                        leg = c("53","54","52","52","55","56","55","53","56","54","52","52"),
                        # 160672 (Jean Wyllys, leg 56): a Camara registra "Saida - Renuncia" em 2019-01-28, sem posse;
                        # o banco segue o texto da fonte (renuncia); reclassificar como nao_tomou_posse e decisao do autor
                        esperado = c("falecimento","cassacao","cassacao","cassacao","cassacao","renuncia","cassacao","falecimento","cassacao","cassacao","renuncia","renuncia"))
obs <- x[, .(formas = paste(sort(unique(forma_saida)), collapse = ",")), by = .(id = id_deputado_camara, leg = legislatura)]
esperados <- merge(esperados, obs, by = c("id", "leg"), all.x = TRUE)
esperados[, ok := mapply(function(e, f) e %in% strsplit(f, ",")[[1]], esperado, formas)]
print(esperados)
if (!all(esperados$ok)) prob("caso conhecido sem a forma de saida esperada")

## ------------------------------------------------------------ registrar
for (k in names(rec)) reg(paste0("verif_cam_", k), rec[[k]])
reg("verif_cam_n_numeros_comparados", nrow(cmp))
reg("verif_cam_n_numeros_divergentes", n_diverge)
reg("verif_cam_n_celulas_vazias", n_vazias)
reg("verif_cam_n_fim_antes_inicio", n_fim_antes)
reg("verif_cam_n_inicio_antes_1999_02_01", n_ini_antes_1999)
reg("verif_cam_n_inicio_fora_janela_leg", n_ini_fora)
reg("verif_cam_n_fim_fora_janela_leg", n_fim_fora)
reg("verif_cam_n_inicio_fora_janela_60d", n_ini_fora60)
reg("verif_cam_n_fim_fora_janela_60d", n_fim_fora60)
reg("verif_cam_n_periodos_pos_obito", n_pos_obito)
reg("verif_cam_n_periodos_fim_pos_obito", n_fim_pos_obito)
reg("verif_cam_n_periodos_sobrepostos", n_overlap)
reg("verif_cam_n_mandatos_so_suplente", sup_eleito)
reg("verif_cam_n_nome_nasc_ambiguos", n_nn_ambiguo)
reg("verif_cam_n_cpf_nome_divergente", n_nome_div)
reg("verif_cam_n_cpf_nasc_divergente", n_nasc_div)
reg("verif_cam_n_uf_divergente_bocel", n_uf_div)
reg("verif_cam_n_pessoa_orfa", n_pessoa_orfa)
reg("verif_cam_n_mandato_orfao", n_mandato_orfao)
reg("verif_cam_n_sem_data_multi_linha", n_sem_data_multi)
reg("verif_cam_n_id_camara_multi_pessoa", nrow(m1))
reg("verif_cam_n_pessoa_multi_id_camara", nrow(m2))
reg("verif_cam_n_amostra_api_viva", n_live_amostra)
reg("verif_cam_n_amostra_api_viva_igual_cache", n_live_igual)
reg("verif_cam_n_casos_conhecidos_ok", sum(esperados$ok))
reg("verif_cam_n_problemas", length(problemas))
gravar_relatorio_verificacao(
  alvo = "data/exercicio_camara.csv", script = SCRIPT,
  passou = c("colunas_prometidas", "na_como_string_NA", "parquet_rds_coincidem", "in_set_forma_saida",
             "in_set_condicao", "em_faixa_legislatura_anos", "chave_unica_periodo", "chave_unica_sem_data",
             "id_pessoa_em_pessoas", "id_mandato_em_mandatos", "join_many_to_one_id_mandato",
             "todo_mandato_bocel_presente", "taxa_pareamento_recontada_513", "recontagem_numeros",
             "amostra_api_viva", "casos_conhecidos"),
  falhou = problemas,
  fora_de_cobertura = c("exatidao_das_datas_da_camara", "homonimos_no_pareamento_por_nome",
                        "legislatura_51_sem_historico_na_fonte", "semantica_de_licenca_vs_afastamento",
                        "divergencia_tse_eleito_vs_camara_suplente", "renuncia_antes_da_posse_vs_nao_tomou_posse"))
logm("problemas:", length(problemas)); if (length(problemas)) print(problemas)
close(logf)
