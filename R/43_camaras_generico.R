# 43_camaras_generico.R — aproveita o HTML de camaras municipais que ja estava em disco
#
# Frente aberta em 30/08/2026 para esgotar o material coletado antes de
# qualquer nova requisicao. O inventario mostrou 1.908 municipios com pagina baixada e nenhuma
# linha extraida; a sondagem (python/parse_camaras_generico.py) mostrou que so 663 tem relacao
# nominal de fato, e que 1.251 nao tem sequer texto util, por serem home vazia ou erro.
#
# Prova de que a pagina e da casa: os nomes casam com os que o TSE registra para AQUELE
# municipio. Sem tres casamentos, o municipio nao entra.
#
# O que a tabela afirma: a pessoa aparece na relacao de vereadores publicada pela casa, o que
# confirma exercicio. O que ela NAO afirma: forma de saida. A pagina e retrato, e nao historico,
# de modo que ausencia de nome nela nao prova saida.
#
# Ambiguidade de legislatura: a pagina raramente diz de que periodo e a relacao. Quando ela traz
# a janela (2021-2024, por exemplo), o exercicio vai para o mandato daquela eleicao. Quando nao
# traz e a pessoa tem um so mandato no municipio, vai para ele. Quando nao traz e a pessoa tem
# mais de um, a linha e DESCARTADA, porque atribuir ao mais recente seria inferencia sem apoio.
#
# Entrada: data_raw/sonda/camaras_generico_bruto.csv (python/sonda_camaras_html.py, depois
#          python/parse_camaras_generico.py; ate 04/09/2026 vivia em /tmp e nao era reproduzivel)
#          data_raw/sonda/tse_nomes_municipais.csv (nomes do TSE por municipio, da mesma sondagem)
#          data_raw/camaras_wordpress/parlamentares_wordpress.csv (python/parse_camaras_wordpress.py;
#          integrado em 05/09/2026 com o mesmo pareamento e as mesmas guardas do bloco generico)
# Saida:   data/exercicio_camaras_generico.csv (coluna sistema distingue generico_html_em_disco de
#          wordpress_html_em_disco)
#
# Reprodutibilidade (05/09/2026): as tres rodadas de 04/09 divergiram (11.927, 10.149, 11.927 linhas
# brutas) porque a segunda usou a variante da sondagem por candidatura, hoje descartada, e porque
# os.walk lia os arquivos na ordem do sistema de arquivos, o que decidia qual pagina (e qual janela
# de anos) ficava na deduplicacao por mandato. A sondagem e o parse passaram a ordenar a leitura, e
# este script registra o md5 de cada insumo (cgen_md5_*), de modo que numero diferente entre
# execucoes so pode vir de data/mandatos.csv ou data/pessoas.csv regravados a montante.
set.seed(20260830)
suppressPackageStartupMessages({library(data.table)})
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root,"lib","proveniencia.R"))
ESTE <- file.path(root,"R","43_camaras_generico.R")
reg <- function(k, v) registrar_numero(k, v, script = ESTE)

bruto <- fread("data_raw/sonda/camaras_generico_bruto.csv", colClasses = c(sg_ue = "character"))
reg("cgen_linhas_brutas", nrow(bruto))
reg("cgen_municipios_sondados", uniqueN(bruto$sg_ue))
# 05/09/2026: assinatura dos insumos, para que a divergencia entre execucoes seja atribuivel
for (f in c("data_raw/sonda/camaras_generico_bruto.csv", "data_raw/sonda/tse_nomes_municipais.csv",
            "data_raw/sonda/sonda_html.json", "data/mandatos.csv", "data/pessoas.csv",
            "data_raw/camaras_wordpress/parlamentares_wordpress.csv"))
  if (file.exists(f)) reg(paste0("cgen_md5_", gsub("[^a-z0-9]+", "_", tolower(basename(f)))), unname(tools::md5sum(f)))

m <- fread("data/mandatos.csv",
           select = c("id_mandato","id_pessoa","cd_cargo","cargo","sg_ue","sg_uf","ano_eleicao",
                      "mandato_inicio","mandato_fim","forma_saida","fonte_forma_saida",
                      "exercicio_confirmado"))
m[, sg_ue := sprintf("%05d", as.integer(sg_ue))]
x <- merge(bruto, m, by.x = c("sg_ue","id_mandato_bocel"), by.y = c("sg_ue","id_mandato"))
setnames(x, "id_mandato_bocel", "id_mandato")

## ---------------------------------------------------- desambiguacao da legislatura
# janela citada na pagina -> eleicao correspondente (a legislatura municipal comeca no ano
# seguinte a eleicao, de modo que a janela 2021-2024 corresponde a eleicao de 2020)
x[, janelas := janelas_na_pagina]
x[, n_mandatos_pessoa_no_municipio := uniqueN(id_mandato), by = .(sg_ue, id_pessoa_bocel)]
casa_janela <- function(janelas, ano) {
  if (is.na(janelas) || janelas == "") return(FALSE)
  ini <- as.integer(sub("-.*$", "", strsplit(janelas, ";")[[1]]))
  any(ini == ano + 1L)
}
x[, janela_bate := mapply(casa_janela, janelas, ano_eleicao)]
x[, criterio := fcase(
  janela_bate == TRUE, "janela_na_pagina",
  n_mandatos_pessoa_no_municipio == 1L, "mandato_unico_no_municipio",
  default = "ambiguo_descartado")]
reg("cgen_criterio_janela", x[criterio == "janela_na_pagina", .N])
reg("cgen_criterio_mandato_unico", x[criterio == "mandato_unico_no_municipio", .N])
reg("cgen_descartados_por_ambiguidade", x[criterio == "ambiguo_descartado", .N])
x <- x[criterio != "ambiguo_descartado"]

## ---------------------------------------------------- esquema padrao
mun <- fread("data/municipios_tse_ibge.csv")
cols_mun <- names(mun)
col_tse <- grep("tse|sg_ue|cd_mun", cols_mun, value = TRUE, ignore.case = TRUE)[1]
col_ibge <- grep("ibge", cols_mun, value = TRUE, ignore.case = TRUE)[1]
mun[, ue := sprintf("%05d", as.integer(get(col_tse)))]
x <- merge(x, unique(mun[, .(ue, id_municipio_ibge = get(col_ibge))]),
           by.x = "sg_ue", by.y = "ue", all.x = TRUE)

out <- x[, .(
  sg_ue, id_municipio_ibge, uf, dominio,
  legislatura_numero = NA_integer_,
  legislatura_inicio = mandato_inicio, legislatura_fim = mandato_fim,
  ano_eleicao_bocel = ano_eleicao,
  nome_fonte, nome_parlamentar = nome_fonte, nome_normalizado = nome_fonte,
  titular = TRUE,
  data_inicio_mandato = NA_character_, data_fim_mandato = NA_character_,
  tipo_afastamento = NA_character_,
  # a pagina prova presenca na relacao da casa, e nao o modo como o mandato terminou
  forma_saida = "nao_observado",
  id_pessoa_bocel, id_mandato_bocel = id_mandato,
  metodo_pareamento = paste0("nome_no_html_", campo_casado, ":", criterio),
  sistema = "generico_html_em_disco",
  so_legislatura_atual = criterio == "mandato_unico_no_municipio",
  url = arquivo,
  exercicio_observado = TRUE
)]
reg("cgen_linhas_so_generico", nrow(out))
reg("cgen_municipios_so_generico", uniqueN(out$sg_ue))

## ---------------------------------------------------- camaras em WordPress (05/09/2026)
# python/parse_camaras_wordpress.py extrai a relacao nominal dos temas WordPress ja baixados e, ate
# 05/09/2026, nenhum script R lia o resultado (1.365 linhas em 94 municipios). O pareamento repete
# o do bloco generico: o nome extraido tem de ser IDENTICO ao nome civil ou ao nome de urna que o
# TSE registra para AQUELE municipio (normalizado, >= 8 caracteres); o municipio so entra com tres
# ou mais nomes casados; a legislatura vem da janela declarada na pagina (ano de inicio = eleicao
# + 1) ou do mandato unico da pessoa no municipio, e o resto e descartado. A pagina prova presenca
# na relacao da casa, nao forma de saida. Linhas que a propria pagina rotula como suplente ficam
# de fora, porque o registro do TSE usado aqui e o dos eleitos titulares. O mandato que o bloco
# generico ja alcancou nao entra de novo (uma linha por mandato, exigida por R/51).
wp_f <- "data_raw/camaras_wordpress/parlamentares_wordpress.csv"
out_wp <- NULL
if (file.exists(wp_f)) {
  wp <- fread(wp_f, colClasses = "character", na.strings = c("", "NA"))
  reg("cwp_linhas_brutas", nrow(wp))
  reg("cwp_municipios_brutos", uniqueN(wp$sg_ue))
  reg("cwp_linhas_suplente_excluidas", wp[papel == "suplente", .N])
  wp <- wp[papel == "titular" & !is.na(nome_normalizado) & nchar(nome_normalizado) >= 8L]
  wp[, sg_ue := sprintf("%05d", as.integer(sg_ue))]
  wp <- unique(wp[, .(uf, sg_ue, dominio, arquivo, legislatura_inicio_ano, nome_fonte, nome_normalizado)])
  tse <- fread("data_raw/sonda/tse_nomes_municipais.csv", colClasses = "character")
  tse_l <- rbindlist(list(
    tse[nchar(nome_n) >= 8L, .(sg_ue, id_mandato_bocel = id_mandato, id_pessoa_bocel = id_pessoa, nome = nome_n, campo_casado = "nome_n")],
    tse[nchar(urna_n) >= 8L, .(sg_ue, id_mandato_bocel = id_mandato, id_pessoa_bocel = id_pessoa, nome = urna_n, campo_casado = "urna_n")]))
  tse_l <- tse_l[!duplicated(tse_l[, .(sg_ue, id_mandato_bocel, nome)])]
  cw <- merge(wp, tse_l, by.x = c("sg_ue", "nome_normalizado"), by.y = c("sg_ue", "nome"), allow.cartesian = TRUE)
  reg("cwp_municipios_com_algum_nome_casado", uniqueN(cw$sg_ue))
  cw[, n_nomes_casados_municipio := uniqueN(nome_normalizado), by = sg_ue]
  cw <- cw[n_nomes_casados_municipio >= 3L]
  reg("cwp_municipios_com_relacao_nominal", uniqueN(cw$sg_ue))
  cw <- merge(cw, m, by.x = c("sg_ue", "id_mandato_bocel"), by.y = c("sg_ue", "id_mandato"))
  cw[, n_mandatos_pessoa_no_municipio := uniqueN(id_mandato_bocel), by = .(sg_ue, id_pessoa_bocel)]
  cw[, janela_bate := !is.na(legislatura_inicio_ano) & suppressWarnings(as.integer(legislatura_inicio_ano)) == ano_eleicao + 1L]
  cw[, criterio := fcase(
    janela_bate == TRUE, "janela_na_pagina",
    n_mandatos_pessoa_no_municipio == 1L, "mandato_unico_no_municipio",
    default = "ambiguo_descartado")]
  reg("cwp_criterio_janela", cw[criterio == "janela_na_pagina", uniqueN(id_mandato_bocel)])
  reg("cwp_criterio_mandato_unico", cw[criterio == "mandato_unico_no_municipio", uniqueN(id_mandato_bocel)])
  reg("cwp_descartados_por_ambiguidade", cw[criterio == "ambiguo_descartado", uniqueN(id_mandato_bocel)])
  cw <- cw[criterio != "ambiguo_descartado"]
  # a mesma pessoa pode aparecer em mais de uma pagina do sitio: uma linha por mandato, a janela vence
  setorder(cw, sg_ue, id_mandato_bocel, criterio, arquivo)
  cw <- cw[!duplicated(id_mandato_bocel)]
  # caminho do arquivo em disco (o parser guarda so o nome; R/51 exige que a origem exista)
  acha_arquivo <- function(uf, sg_ue, arquivo) {
    ds <- file.path(c("data_raw/camaras_sem_sapl", "data_raw/camaras_sem_sapl_2"), uf, sg_ue)
    fs <- unlist(lapply(ds[dir.exists(ds)], function(d) list.files(d, recursive = TRUE, full.names = TRUE)))
    fs <- sort(fs[basename(fs) == arquivo])
    if (length(fs)) fs[1] else NA_character_
  }
  cw[, url := mapply(acha_arquivo, uf, sg_ue, arquivo)]
  reg("cwp_linhas_sem_arquivo_em_disco", cw[is.na(url), .N])
  cw <- cw[!is.na(url)]
  stopifnot(all(cw$uf == cw$sg_uf))
  cw <- merge(cw, unique(mun[, .(ue, id_municipio_ibge = get(col_ibge))]), by.x = "sg_ue", by.y = "ue", all.x = TRUE)
  out_wp <- cw[, .(
    sg_ue, id_municipio_ibge, uf, dominio,
    legislatura_numero = NA_integer_,
    legislatura_inicio = mandato_inicio, legislatura_fim = mandato_fim,
    ano_eleicao_bocel = ano_eleicao,
    nome_fonte, nome_parlamentar = nome_fonte, nome_normalizado,
    titular = TRUE,
    data_inicio_mandato = NA_character_, data_fim_mandato = NA_character_,
    tipo_afastamento = NA_character_,
    forma_saida = "nao_observado",
    id_pessoa_bocel, id_mandato_bocel,
    metodo_pareamento = paste0("nome_no_html_", campo_casado, ":", criterio),
    sistema = "wordpress_html_em_disco",
    so_legislatura_atual = criterio == "mandato_unico_no_municipio",
    url,
    exercicio_observado = TRUE
  )]
  reg("cwp_linhas_pareadas", nrow(out_wp))
  reg("cwp_ja_no_bloco_generico", out_wp[id_mandato_bocel %in% out$id_mandato_bocel, .N])
  out_wp <- out_wp[!id_mandato_bocel %in% out$id_mandato_bocel]
  reg("cwp_linhas_novas", nrow(out_wp))
  reg("cwp_municipios_novos", uniqueN(out_wp$sg_ue))
  cat("  wordpress: linhas novas:", nrow(out_wp), "| municipios:", uniqueN(out_wp$sg_ue), "\n")
  wp_sem_ex <- cw[id_mandato_bocel %in% out_wp$id_mandato_bocel & (is.na(exercicio_confirmado) | exercicio_confirmado == FALSE), .N]
  reg("cwp_mandatos_sem_exercicio_ate_agora", wp_sem_ex)
}
out <- rbindlist(list(out, out_wp), use.names = TRUE)
stopifnot(!anyDuplicated(out$id_mandato_bocel))
stopifnot(all(file.exists(out$url)))
setorder(out, uf, sg_ue, ano_eleicao_bocel)
fwrite(out, "data/exercicio_camaras_generico.csv", quote = TRUE, na = "NA")

# cgen_linhas e o total gravado no arquivo (generico + wordpress), que e o que R/51 e R/52 contam
reg("cgen_linhas", nrow(out))
reg("cgen_municipios", uniqueN(out$sg_ue))
reg("cgen_mandatos", uniqueN(out$id_mandato_bocel))
reg("cgen_ufs", uniqueN(out$uf))
sem_ex <- x[is.na(exercicio_confirmado) | exercicio_confirmado == FALSE, .N]
reg("cgen_mandatos_sem_exercicio_ate_agora", sem_ex)
sem_forma <- x[forma_saida == "nao_observado", .N]
reg("cgen_mandatos_sem_forma_de_saida", sem_forma)

cat("\n43_camaras_generico: concluido\n")
cat("  linhas:", nrow(out), "| municipios:", uniqueN(out$sg_ue), "| UFs:", uniqueN(out$uf), "\n")
cat("  por criterio:\n"); print(out[, .N, by = metodo_pareamento][order(-N)])
cat("  mandatos que ainda nao tinham exercicio confirmado:", sem_ex, "\n")
