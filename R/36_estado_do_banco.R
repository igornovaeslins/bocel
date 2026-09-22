#!/usr/bin/env Rscript
# Regenera as secoes numericas de docs/ESTADO_DO_BANCO.md a partir de data/mandatos.csv.
# Escrito em 29/08/2026 porque as tabelas do documento tinham sido digitadas a mao e
# envelheceram junto com a camada de saida (a fonte tce aparecia com 9.374 e dez UFs,
# quando o numero vinha so de PB e PE).
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente

# o arquivo grava o ausente como NA literal, e ler com na.strings = "" faz a string "NA"
# passar por valor preenchido, o que inflava a contagem de posse e zerava a de exercicio
m <- fread("data/mandatos.csv", na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8")

# 21/09/2026: cabecalho do balanco com a data da versao
cab <- readLines("docs/ESTADO_DO_BANCO.md", encoding = "UTF-8", warn = FALSE, n = 5)
if (length(cab) >= 4 && grepl("^# Estado do Banco", cab[1])) {
  cab[1] <- sub(" — .*$", " — 21 de setembro de 2026", cab[1])
  linhas <- readLines("docs/ESTADO_DO_BANCO.md", encoding = "UTF-8", warn = FALSE)
  linhas[seq_along(cab)] <- cab
  writeLines(linhas, "docs/ESTADO_DO_BANCO.md", useBytes = TRUE)
}
obs <- function(x) !is.na(x) & !(x %in% c("nao_observado", ""))
fmt <- function(x) formatC(x, big.mark = ".", format = "d")
pc  <- function(x) sub("\\.", ",", sprintf("%.1f", 100 * x))

m[, esfera := fifelse(cd_cargo %in% c("1","2","5","6"), "federal",
              fifelse(cd_cargo %in% c("3","4","7","8"), "estadual", "municipal"))]
m[, tem := obs(forma_saida)]

esf <- m[, .(n = .N, k = sum(tem)), by = esfera]
setkey(esf, esfera)
tot_n <- nrow(m); tot_k <- sum(m$tem)
n_posse <- sum(!is.na(m$data_posse))
n_exerc <- sum(!is.na(m$fonte_exercicio))
n_fim <- sum(!is.na(m$data_fim_efetiva))

linha <- function(e) sprintf("| %s | %s | %s (%s%%) |", e, fmt(esf[e]$n), fmt(esf[e]$k), pc(esf[e]$k / esf[e]$n))
tab_esfera <- paste(c(
  "| esfera | mandatos | forma de saída observada |",
  "|---|---|---|",
  linha("federal"), linha("estadual"), linha("municipal"),
  sprintf("| **total** | **%s** | **%s (%s%%)** |", fmt(tot_n), fmt(tot_k), pc(tot_k / tot_n))), collapse = "\n")
# 04/09/2026: a lista de fontes do exercicio deixou de ser digitada na frase e passa a sair da
# propria coluna, porque a lista de presenca do SAPL e a MUNIC ampliada entraram e a frase
# envelheceu no mesmo dia em que a fonte mudou.
# o rotulo ja vem contraido com a preposicao, para a frase sair correta em qualquer combinacao
rot_ex <- c(tse_reeleicao = "pelo registro da candidatura à reeleição no TSE",
            ibge_munic = "pela MUNIC/IBGE", ibge_munic_ampliado = "pela MUNIC/IBGE",
            sapl_presenca = "pela lista de presença em plenário das câmaras municipais",
            receita_cnpj = "pelo cadastro CNPJ da Receita")
tok_ex <- unlist(strsplit(na.omit(mand_fonte_exerc <- m$fonte_exercicio), ";", fixed = TRUE))
ord_ex <- sort(table(tok_ex), decreasing = TRUE)
nomes_ex <- unique(unname(rot_ex[names(ord_ex)]))
nomes_ex <- nomes_ex[!is.na(nomes_ex)]
lista_ex <- if (length(nomes_ex) > 1) {
  paste0(paste(nomes_ex[-length(nomes_ex)], collapse = ", "), " e ", nomes_ex[length(nomes_ex)])
} else nomes_ex
frase_datas <- sprintf(paste0("Além da forma, a camada datou o começo e o encerramento do mandato. ",
  "%s mandatos (%s%%) têm data de posse vinda de fonte, %s (%s%%) têm data de fim efetiva e %s (%s%%) ",
  "têm o titular observado em exercício em alguma data, %s."),
  fmt(n_posse), pc(n_posse / tot_n), fmt(n_fim), pc(n_fim / tot_n), fmt(n_exerc), pc(n_exerc / tot_n),
  lista_ex)

rot <- c("1"="presidente","2"="vice-presidente","3"="governador","4"="vice-governador","5"="senador",
         "6"="deputado federal","7"="deputado estadual","8"="deputado distrital","11"="prefeito",
         "12"="vice-prefeito","13"="vereador")
cg <- m[, .(n = .N, k = sum(tem)), by = cd_cargo][, p := k / n][order(-p)]
cg[, nome := rot[cd_cargo]]
frase_cargo <- paste0("Por cargo, a proporção com forma de saída observada é de ",
  paste0(pc(cg$p), "% em ", cg$nome, collapse = ", "), ".")

if ("ano_eleicao" %in% names(m)) {
  an <- m[cd_cargo %in% c("11","12","13") & !is.na(ano_eleicao),
          .(n = .N, k = sum(tem)), by = ano_eleicao][, p := k / n][order(ano_eleicao)]
  frase_tempo <- sprintf(paste0("A cobertura municipal cresce no tempo, porque a maior parte dos portais mantém ",
    "apenas as legislaturas recentes em linha, e a camada alcança %s%% dos mandatos eleitos em %s, ",
    "%s%% dos de 2012 e %s%% dos de 2020. A eleição de 2024 aparece com pouca saída observada por estar em curso."),
    pc(an[1]$p), an[1]$ano_eleicao, pc(an[ano_eleicao == "2012"]$p), pc(an[ano_eleicao == "2020"]$p))
} else frase_tempo <- ""

nm <- if ("unidade_posicao" %in% names(m)) uniqueN(m[cd_cargo %in% c("11","12","13") & tem]$unidade_posicao) else NA_integer_
nmt <- if ("unidade_posicao" %in% names(m)) uniqueN(m[cd_cargo %in% c("11","12","13")]$unidade_posicao) else NA_integer_
frase_munic <- sprintf("%s dos %s municípios têm ao menos um mandato com saída observada.", fmt(nm), fmt(nmt))

## fontes: contagem real de onde a forma prevaleceu
desc <- c(sapl_municipal = "SAPL das câmaras municipais (Interlegis)|mandatos de vereador com posse, saída e afastamento",
  tce = "Tribunais de Contas (a forma de saída vem de PB e PE; as demais casas dão período e pareamento)|gestores de prefeituras e câmaras com período",
  portal_camara = "Portais de câmaras sem SAPL (duas rodadas de coletores, 15 sistemas)|vereadores por legislatura",
  camara_api = "API da Câmara dos Deputados|posse, licença, afastamento, renúncia, cassação",
  wikipedia = "Wikipédia (governadores, vices, deputados estaduais, prefeitos)|início, fim e causa",
  assembleia_api = "Assembleias com SAPL ou lista por legislatura|deputados estaduais",
  assembleia_portal = "Assembleias por coleta no portal da propria casa (29/08/2026)|posse, fim de exercicio e causa do deputado estadual",
  derivado_titular = "Regra do vice (derivado do titular)|vice assume quando o titular sai",
  wikidata = "Wikidata|mandatos em exercício", wikidata_obito = "Wikidata (óbitos)|morte durante o mandato",
  ibge_munic = "MUNIC/IBGE (2004 e 2005, com nome)|prefeito em exercício diferente do eleito",
  tse_suplementar = "Eleições suplementares do TSE|perda do mandato e posse não ocorrida",
  assembleia_historico = "Assembleias por fontes históricas (memoriais, ALMG)|deputados estaduais",
  sapl_observacao = "SAPL, campo de texto livre da observação (04/09/2026)|o ato que a própria Casa escreveu, com data",
  assembleia_inventario = "Assembleias por fontes administrativas (folha, frequência, transparência)|período de exercício do deputado estadual",
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
  senado_api = "API do Senado|exercícios e causa de afastamento",
  camara_biografia = "Biografia oficial dos deputados na Câmara|legislatura 1999-2003, posse, renúncia, perda de mandato e morte",
  fonte_oficial_curada = "Evento com fonte oficial ou matéria e trecho literal conferido (ref/)|governos, Presidência e casos federais especiais",
# 21/09/2026: regra A1, o evento curado sem fonte oficial leva o rotulo da melhor fonte que tem (lib/tipo_fonte.R)
  base_dhbb_curada = "Verbete do DHBB/CPDOC-FGV conferido (ref/), na falta de fonte oficial|governos e Presidência",
  noticia_orgao_publico_curada = "Notícia publicada por órgão público e conferida (ref/), na falta de ato oficial|governos e Presidência",
  pista_nao_oficial = "Imprensa ou Wikipédia sem fonte oficial, com o mandato em docs/EXCECOES_CONHECIDAS.csv|governos e Presidência",
  diario_oficial = "Diários oficiais (Querido Diário)|renúncia, cassação, licença, posse do vice",
  cargo_incompativel = "Incompatibilidade de cargo, deduzida do proprio banco|mandato encerrado por posse em outro cargo eletivo",
  data_fim_efetiva = "Data de fim efetiva sem fonte de forma|apenas encerramento")
fo <- m[tem == TRUE, .N, by = fonte_forma_saida][order(-N)]
in_set(fo$fonte_forma_saida, names(desc), "fonte_forma_saida com descricao no ESTADO_DO_BANCO")
tab_fontes <- paste(c("| fonte | o que dá | mandatos em que prevaleceu |", "|---|---|---|",
  "| TSE (consulta_cand, votacao_candidato_munzona) | universo de eleitos, partido, votos, coligação | base inteira |",
  sprintf("| %s | %s |", gsub("\\|", " | ", desc[fo$fonte_forma_saida]), fmt(fo$N))), collapse = "\n")

txt <- readLines("docs/ESTADO_DO_BANCO.md", encoding = "UTF-8", warn = FALSE)
i1 <- grep("^## Camada de posse", txt); i2 <- grep("^## De onde vêm os dados", txt); i3 <- grep("^## Para que serve", txt)
stopifnot(length(i1) == 1, length(i2) == 1, length(i3) == 1)
cauda <- txt[(i2 + 1):(i3 - 1)]
cauda <- cauda[cumsum(grepl("^O exercício confirmado", cauda)) > 0]
# A cauda vai ate "Para que serve" e por isso engolia a secao de ocupacao, que e reescrita mais
# abaixo; sem este corte a secao antiga voltava pela cauda e o documento saia com as duas versoes,
# a velha primeiro. Foi o que aconteceu na execucao de 4 de setembro de 2026.
i_co <- grep("^## Ocupação da cadeira", cauda)
if (length(i_co)) cauda <- cauda[seq_len(min(i_co) - 1L)]
## secao do nivel de ocupacao (decisao de 30/08/2026, pendencia 5)
sec_ocu <- character(0)
if (file.exists("data/ocupacoes.csv") && file.exists("data/lista_suplencia.csv")) {
  ocu <- fread("data/ocupacoes.csv", select = c("id_mandato","tipo_ocupante","vinculo_cadeira",
                                                "partido_difere_do_titular","esfera"))
  lsu <- fread("data/lista_suplencia.csv", select = c("id_lista","ordem_suplencia","ordem_suplencia_tse"))
  val <- lsu[!is.na(ordem_suplencia_tse)]
  ft <- function(x) format(x, big.mark = ".", decimal.mark = ",")
  tipos <- ocu[tipo_ocupante != "titular", .N, by = tipo_ocupante][order(-N)]
  sec_ocu <- c(
    "## Ocupação da cadeira", "",
    strwrap(sprintf(paste("A cadeira e o ocupante são níveis distintos desde 30 de agosto de 2026.",
      "O banco mantém %s cadeiras, e sobre elas %s ocupações, das quais %s são de quem não é o",
      "titular eleito. São %s suplentes convocados, %s prefeituras em mãos de interino e %s vices",
      "que assumiram, espalhados por %s cadeiras que passaram por mais de uma pessoa. Em %s dessas",
      "ocupações o partido de quem assumiu difere do partido do titular, o que mede a troca de",
      "legenda dentro da cadeira, sem eleição."),
      ft(ocu[tipo_ocupante == "titular", .N]), ft(nrow(ocu)), ft(ocu[tipo_ocupante != "titular", .N]),
      ft(tipos[tipo_ocupante == "suplente", N][1]), ft(tipos[tipo_ocupante == "interino", N][1]),
      ft(tipos[tipo_ocupante == "vice_assumiu", N][1]),
      ft(ocu[!is.na(id_mandato), .N, by = id_mandato][N > 1, .N]),
      ft(ocu[partido_difere_do_titular %in% TRUE, .N])), 88), "",
    strwrap(sprintf(paste("A fila de suplência de todas as listas está registrada, com %s suplentes",
      "em %s listas partidárias. A ordem vem da votação nominal com desempate pelo mais idoso, e",
      "reproduz a ordem que o TSE publica em 2016 em %s dos %s registros. Em %s ocupações a fonte",
      "não diz qual titular saiu, e a cadeira fica sem identificação, com a ocupação contando como",
      "ocupante da casa."),
      ft(nrow(lsu)), ft(uniqueN(lsu$id_lista)),
      sub(".", ",", sprintf("%.2f%%", 100 * val[ordem_suplencia == ordem_suplencia_tse, .N] / nrow(val)), fixed = TRUE),
      ft(nrow(val)), ft(ocu[vinculo_cadeira == "casa_legislatura", .N])), 88), "")
  registrar_numero("edb_ocupacoes", nrow(ocu))
  registrar_numero("edb_ocupantes_nao_titulares", ocu[tipo_ocupante != "titular", .N])
  registrar_numero("edb_troca_de_partido_na_cadeira", ocu[partido_difere_do_titular %in% TRUE, .N])
}
# a secao e reescrita a cada execucao, e por isso a anterior sai antes
i_ocu <- grep("^## Ocupação da cadeira", txt)
if (length(i_ocu) >= 1) txt <- txt[-(min(i_ocu):(i3 - 1))]
i3 <- grep("^## Para que serve", txt)
novo <- c(txt[1:i1], "", tab_esfera, "", strwrap(frase_datas, 88), "", strwrap(frase_cargo, 88), "", strwrap(frase_tempo, 88), "",
          strwrap(frase_munic, 88), "", txt[i2], "", tab_fontes, "", cauda, sec_ocu, txt[i3:length(txt)])
# 05/09/2026: os dois primeiros itens de "O que falta" eram digitados a mao e envelheceram (2.943
# municipios e 10 TCEs, quando o registro dizia 2.843 e 19). Passam a sair do dado.
n_mun_sem <- 5569L - nm
tce_ufs <- sort(unique(unlist(lapply(c("data/tce_gestores.csv", "data/tce_gestores_b.csv",
                                        "data/tce_gestores_c.csv", "data/tce_gestores_d.csv"), function(f) {
  if (!file.exists(f)) return(character(0))
  x <- fread(f, colClasses = "character", select = intersect(c("uf", "sg_uf"), names(fread(f, nrows = 0))))
  toupper(x[[1]])
}))))
tce_ufs <- tce_ufs[nchar(tce_ufs) == 2]
i_f1 <- grep("^1\\. \\*\\*Vereadores e prefeitos fora dos portais alcançados\\.\\*\\*", novo)
if (length(i_f1) == 1) novo[i_f1] <- sprintf("1. **Vereadores e prefeitos fora dos portais alcançados.** %s municípios sem", fmt(n_mun_sem))
i_f2 <- grep("^2\\. \\*\\*Tribunais de Contas\\.\\*\\*", novo)
if (length(i_f2) == 1) novo[i_f2] <- sprintf("2. **Tribunais de Contas.** %d das 27 casas coletadas (%s); as demais ficaram na fila",
                                             length(tce_ufs), paste(tce_ufs, collapse = ", "))
registrar_numero("edb_municipios_sem_saida", n_mun_sem); registrar_numero("edb_tce_ufs_coletadas", length(tce_ufs))
writeLines(novo, "docs/ESTADO_DO_BANCO.md", useBytes = TRUE)

registrar_numero("edb_mandatos_total", tot_n); registrar_numero("edb_mandatos_com_forma", tot_k)
registrar_numero("edb_pct_com_forma", round(100 * tot_k / tot_n, 2))
for (e in c("federal","estadual","municipal")) {
  registrar_numero(paste0("edb_com_forma_", e), esf[e]$k)
  registrar_numero(paste0("edb_pct_com_forma_", e), round(100 * esf[e]$k / esf[e]$n, 2))
}
for (i in seq_len(nrow(fo))) registrar_numero(paste0("edb_fonte_", fo$fonte_forma_saida[i]), fo$N[i])
registrar_numero("edb_municipios_com_saida", nm); registrar_numero("edb_n_fontes_de_forma", nrow(fo))
registrar_numero("edb_mandatos_com_posse", n_posse); registrar_numero("edb_mandatos_com_fim_efetiva", n_fim)
registrar_numero("edb_mandatos_com_exercicio", n_exerc)
cat("36_estado_do_banco: concluido |", tot_k, "de", tot_n, "\n")
