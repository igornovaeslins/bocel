# auditoria_fontes_e_linguagem.R — tipo de fonte da curadoria e etapas fora do R (19/09/2026)
#
# Duas regras pedem medida antes de decidir o que sai na v1. A decisao na pendencia das Assembleias diz que
# noticia nao pode ser fonte, por ser muito irregular, e a de 19/09 fixou que tudo precisa ser em R e reprodutivel, e que o
# que nao for nao vai ao ar. Este script classifica cada URL das tabelas curadas em ref/ pelo tipo de fonte e lista os
# scripts fora do R que a reconstrucao chama ou que as frentes deixaram. Nao corrige nada.
#
# Saidas: output/verificacao/fontes_curadas_por_tipo.csv (uma linha por URL curada),
#         output/verificacao/fontes_curadas_mandato_sem_fonte_oficial.csv (linhas cuja prova nao tem ato ou pagina oficial),
#         output/verificacao/inventario_nao_R.csv (scripts fora do R), chaves fon_* e ling_* no registro.
# Execucao: cd ~/bocel && Rscript --vanilla R/auditoria_fontes_e_linguagem.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/auditoria_fontes_e_linguagem.R"
reg <- function(k, v) registrar_numero(k, v, script = script)
le <- function(f) if (file.exists(f)) fread(f, colClasses = "character", na.strings = c("", "NA"), encoding = "UTF-8") else NULL

## ---------------------------------------------------------------- classificacao da URL
# Pagina arquivada no Wayback vale pelo endereco original. Dominio de governo, casa legislativa, tribunal ou ministerio
# publico e oficial; dentro dele, caminho de agencia ou noticia e noticia oficial (Agencia Senado, noticia da Assembleia),
# que e a faixa sobre a qual a regra A1 precisa de leitura. O DHBB da FGV e referencia academica. O resto e imprensa.
# A regra do dominio e do caminho mora em lib/tipo_fonte.R (21/09/2026), para a saida dos executivos usar a mesma
# classificacao; tipo_url() aqui so traduz o rotulo novo (oficial, base_dhbb, noticia_orgao_publico, wikipedia,
# imprensa) para o vocabulario que este script ja registrava, sem mudar nenhuma contagem existente.
source(file.path(root, "lib", "tipo_fonte.R"))
VOCAB_ANTIGO_FON <- c(oficial = "oficial", base_dhbb = "referencia_academica", noticia_orgao_publico = "noticia_oficial",
                      wikipedia = "wikipedia", imprensa = "imprensa")
tipo_url <- function(u) {
  t <- tipo_fonte(u)
  if (is.na(t)) return(NA_character_)
  unname(VOCAB_ANTIGO_FON[t])
}
tipo_vec <- function(x) vapply(x, tipo_url, character(1), USE.NAMES = FALSE)

linhas <- list()
# governos e Presidencia: duas fontes por linha
for (f in c("ref/eventos_governos_fonte_oficial.csv", "ref/eventos_presidencia_fonte_oficial.csv")) {
  x <- le(f); if (is.null(x)) next
  x[, `:=`(t1 = tipo_vec(fonte_1), t2 = tipo_vec(fonte_2))]
  linhas[[f]] <- x[, .(tabela = basename(f), id_mandato, sg_uf, ano_eleicao, cargo, nome = nome_tse, forma = forma_saida,
                       url_1 = fonte_1, tipo_1 = t1, url_2 = fonte_2, tipo_2 = t2)]
}
# Assembleias: uma URL por linha, com o tipo declarado pela frente ao lado
for (f in c("ref/eventos_assembleias_fonte_oficial.csv", "ref/composicao_final_assembleias_fonte_oficial.csv")) {
  x <- le(f); if (is.null(x)) next
  nm <- if ("nome_fonte" %chin% names(x)) x$nome_fonte else NA_character_
  fm <- if ("evento" %chin% names(x)) x$evento else paste0("composicao_", x$situacao)
  linhas[[f]] <- data.table(tabela = basename(f), id_mandato = x$id_mandato, sg_uf = x$uf, ano_eleicao = x$ano_eleicao,
                            cargo = "DEPUTADO ESTADUAL/DISTRITAL", nome = nm, forma = fm, url_1 = x$url, tipo_1 = tipo_vec(x$url),
                            url_2 = NA_character_, tipo_2 = NA_character_, tipo_declarado = x$tipo_fonte)
}
fx <- rbindlist(linhas, fill = TRUE)
fx[, tem_oficial := tipo_1 %chin% "oficial" | tipo_2 %chin% "oficial"]
fx[, tem_noticia_oficial := tipo_1 %chin% "noticia_oficial" | tipo_2 %chin% "noticia_oficial"]
fx[, so_imprensa_ou_referencia := !tem_oficial & !tem_noticia_oficial]
# Melhor tipo da linha, na ordem de admissibilidade, para separar DHBB, Wikipedia e imprensa dentro das linhas sem ato.
ORDEM <- c("oficial", "noticia_oficial", "referencia_academica", "wikipedia", "imprensa")
fx[, melhor_tipo := ORDEM[pmin(match(tipo_1, ORDEM, nomatch = 99L), match(tipo_2, ORDEM, nomatch = 99L))]]
fwrite(fx, "output/verificacao/fontes_curadas_por_tipo.csv", na = "NA", quote = TRUE)
fwrite(fx[tem_oficial == FALSE], "output/verificacao/fontes_curadas_mandato_sem_fonte_oficial.csv", na = "NA", quote = TRUE)

stopifnot(nrow(fx) > 0, !anyNA(fx$tabela))
for (tb in unique(fx$tabela)) {
  k <- sub("_fonte_oficial[.]csv$", "", tb)
  y <- fx[tabela == tb]
  reg(sprintf("fon_%s_linhas", k), nrow(y))
  reg(sprintf("fon_%s_sem_fonte_oficial", k), y[tem_oficial == FALSE, .N])
  reg(sprintf("fon_%s_so_noticia_oficial", k), y[tem_oficial == FALSE & tem_noticia_oficial == TRUE, .N])
  reg(sprintf("fon_%s_so_imprensa_ou_referencia", k), y[so_imprensa_ou_referencia == TRUE, .N])
  for (t in c("referencia_academica", "wikipedia", "imprensa")) reg(sprintf("fon_%s_melhor_%s", k, t), y[melhor_tipo %chin% t, .N])
}
stopifnot(fx[tem_oficial == FALSE & tem_noticia_oficial == FALSE, all(melhor_tipo %chin% ORDEM[3:5])])

# contagem no vocabulario novo de lib/tipo_fonte.R (oficial, base_dhbb, noticia_orgao_publico, wikipedia, imprensa),
# so para governos e Presidencia, que sao as tabelas do recorte da v1.0 (pedido de 21/09, alem do que a
# auditoria ja registrava acima)
for (tb in c("eventos_governos_fonte_oficial.csv", "eventos_presidencia_fonte_oficial.csv")) {
  if (!tb %chin% fx$tabela) next
  k <- sub("_fonte_oficial[.]csv$", "", tb)
  y <- fx[tabela == tb]
  y[, tipo_fonte_novo := melhor_tipo_fonte(url_1, url_2)]
  for (t in names(VOCAB_ANTIGO_FON)) reg(sprintf("fon_%s_tipo_fonte_%s", k, t), y[tipo_fonte_novo %chin% t, .N])
}
print(fx[, .N, by = .(tabela, melhor_tipo)][order(tabela, melhor_tipo)])
print(fx[, .(linhas = .N, sem_oficial = sum(!tem_oficial), so_noticia_oficial = sum(!tem_oficial & tem_noticia_oficial),
             so_imprensa_ou_ref = sum(so_imprensa_ou_referencia)), by = tabela])

## ---------------------------------------------------------------- etapas fora do R
rec <- readLines("R/00_reconstruir.sh", warn = FALSE)
rec_ativas <- rec[!grepl("^\\s*#", rec)]
chamados <- unique(unlist(regmatches(rec_ativas, gregexpr("python/[A-Za-z0-9_/]+[.]py", rec_ativas))))
alimenta <- vapply(chamados, function(p) {
  l <- rec_ativas[grepl(p, rec_ativas, fixed = TRUE)][1]
  r <- unlist(regmatches(l, gregexpr("R/[A-Za-z0-9_/]+[.]R", l)))
  if (length(r)) paste(r, collapse = " ") else NA_character_
}, character(1))
todos <- c(list.files("python", pattern = "[.]py$", recursive = TRUE, full.names = TRUE),
           list.files("data_raw/assembleias3", pattern = "[.]py$", recursive = TRUE, full.names = TRUE),
           list.files("zenodo", pattern = "[.]py$", full.names = TRUE),
           list.files("lib", pattern = "[.]py$", full.names = TRUE))
inv <- data.table(arquivo = todos)
inv[, onde := fifelse(startsWith(arquivo, "python/"), "coleta_python",
              fifelse(startsWith(arquivo, "data_raw/assembleias3/"), "frente_fase5",
              fifelse(startsWith(arquivo, "zenodo/"), "deposito", "ferramenta")))]
inv[, chamado_pela_reconstrucao := arquivo %chin% chamados]
inv[, alimenta := unname(alimenta[match(arquivo, chamados)])]
setorder(inv, onde, -chamado_pela_reconstrucao, arquivo)
fwrite(inv, "output/verificacao/inventario_nao_R.csv", na = "NA")
reg("ling_scripts_python_total", nrow(inv))
reg("ling_scripts_python_chamados_pela_reconstrucao", length(chamados))
reg("ling_scripts_python_frentes_fase5", inv[onde == "frente_fase5", .N])
reg("ling_scripts_python_deposito", inv[onde == "deposito", .N])
print(inv[, .N, by = .(onde, chamado_pela_reconstrucao)])
