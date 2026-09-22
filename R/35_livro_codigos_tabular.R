# 35_livro_codigos_tabular.R — livro de codigos em formato tabular, legivel por maquina
# Segue o padrao do material de replicacao de Gelape e Thome (uma aba por arquivo; colunas
# Variavel, Descricao, Tipo e Fonte) e acrescenta nivel de medida, preenchimento e cobertura.
# Entrada: docs/LIVRO_DE_CODIGOS.md (gerado por R/05_documentar.R) e os CSV de data/
# Saida:   docs/LIVRO_DE_CODIGOS.csv e docs/LIVRO_DE_CODIGOS.xlsx (uma aba por arquivo)
# Execucao: cd ~/bocel && Rscript --vanilla R/35_livro_codigos_tabular.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(openxlsx) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
script <- "R/35_livro_codigos_tabular.R"

lc <- readLines("docs/LIVRO_DE_CODIGOS.md", warn = FALSE)
sec <- cumsum(grepl("^## ", lc))
titulos <- lc[grepl("^## ", lc)]
arq_de <- function(t) { m <- regmatches(t, regexpr("[a-z_0-9]+\\.csv", t)); if (length(m)) m else NA_character_ }
linhas <- list()
for (i in seq_along(titulos)) {
  arq <- arq_de(titulos[i]); if (is.na(arq)) next
  bloco <- lc[sec == i]
  vars <- bloco[grepl("^\\| `", bloco)]
  if (!length(vars)) next
  p <- strsplit(sub("^\\|\\s*", "", sub("\\s*\\|$", "", vars)), "\\s*\\|\\s*")
  linhas[[arq]] <- data.table(
    arquivo = arq,
    variavel = gsub("`", "", vapply(p, `[`, character(1), 1)),
    tipo_armazenamento = vapply(p, function(x) if (length(x) > 1) x[2] else NA_character_, character(1)),
    descricao = vapply(p, function(x) if (length(x) > 2) x[3] else NA_character_, character(1)),
    preenchimento = vapply(p, function(x) if (length(x) > 3) x[4] else NA_character_, character(1)))
}
cb <- rbindlist(linhas, use.names = TRUE)
stopifnot(nrow(cb) > 0)

## nivel de medida, na convencao de livro de codigos de ciencias sociais
cb[, nivel_medida := fcase(
  grepl("^inteiro|^numero", tipo_armazenamento) & grepl("^n_|^quantidade|^votos|^idade|^taxa|^pct|^dias|^n$", variavel), "Numerico",
  tipo_armazenamento == "logico", "Binario",
  tipo_armazenamento == "data ISO" | grepl("^data", tipo_armazenamento), "Data",
  grepl("^inteiro|^numero", tipo_armazenamento), "Numerico",
  default = "Categorico")]
cb[grepl("^(ano|ano_eleicao|ano_evento|legislatura)", variavel), nivel_medida := "Ordinal"]

## fonte por variavel: primeiro pelo arquivo, depois pela regra da coluna
fonte_arquivo <- c(
  "mandatos.csv" = "TSE (consulta_cand e votacao_candidato_munzona)",
  "posicoes_ano.csv" = "Autor, a partir de mandatos.csv",
  "pessoas.csv" = "TSE (consulta_cand)",
  "filiacoes.csv" = "TSE (listas de filiacao, via Base dos Dados)",
  "exercicio_camara.csv" = "Camara dos Deputados (API de Dados Abertos)",
  "exercicio_senado.csv" = "Senado Federal (API de Dados Abertos)",
  "exercicio_assembleias.csv" = "Assembleias Legislativas (SAPL e portais)",
  "exercicio_assembleias_lacunas.csv" = "Autor, inventario das fontes das Assembleias",
  "exercicio_assembleias_historico.csv" = "Assembleias Legislativas (memoriais e listas historicas)",
  "exercicio_camaras_municipais.csv" = "Camaras municipais (API SAPL do Interlegis)",
  "exercicio_camaras_municipais_cobertura.csv" = "Autor, a partir de exercicio_camaras_municipais.csv",
  "exercicio_camaras_sem_sapl.csv" = "Camaras municipais (portais proprios)",
  "exercicio_camaras_sem_sapl_cobertura.csv" = "Autor, a partir de exercicio_camaras_sem_sapl.csv",
  "wikidata_mandatos.csv" = "Wikidata (P39 e P6)",
  "wikidata_obitos.csv" = "Wikidata (P570)",
  "wikipedia_estadual.csv" = "Wikipedia em portugues (listas por legislatura)",
  "wikipedia_estadual_cobertura.csv" = "Autor, a partir de wikipedia_estadual.csv",
  "wikipedia_prefeitos.csv" = "Wikipedia em portugues (listas de prefeitos)",
  "wikipedia_prefeitos_cobertura.csv" = "Autor, a partir de wikipedia_prefeitos.csv",
  "eleicoes_suplementares.csv" = "TSE (consulta_cand e votacao, pleitos suplementares)",
  "eleicoes_suplementares_vereador.csv" = "TSE (consulta_cand e votacao, pleitos suplementares)",
  "munic_prefeitos.csv" = "IBGE (MUNIC) e TSE",
  "sinais_tse_exercicio.csv" = "TSE (consulta_cand e DivulgaCand)",
  "auditoria_homonimos.csv" = "Autor, auditoria da deduplicacao",
  "municipios_tse_ibge.csv" = "Base dos Dados (diretorio de municipios) e IBGE",
  "datajud_processos_cassacao.csv" = "CNJ (DataJud, API publica)",
  "datajud_sinal_unidade_eleicao.csv" = "Autor, a partir de datajud_processos_cassacao.csv",
  "tce_gestores.csv" = "Tribunais de Contas estaduais e municipais",
  "tce_gestores_b.csv" = "Tribunais de Contas estaduais e municipais",
  "tce_gestores_c.csv" = "Tribunais de Contas estaduais e municipais",
  "tce_gestores_d.csv" = "Tribunais de Contas estaduais e municipais",
  "tce_gestores_cobertura.csv" = "Autor, a partir de tce_gestores.csv",
  "tce_gestores_b_cobertura.csv" = "Autor, a partir de tce_gestores_b.csv",
  "diarios_eventos.csv" = "Diarios oficiais municipais (Querido Diario, OKBR)",
  "diarios_mandatos_saida.csv" = "Autor, a partir de diarios_eventos.csv",
  "diarios_cobertura_bocel.csv" = "Autor, a partir de diarios_eventos.csv",
  "diarios_taxa_uf_ano.csv" = "Autor, a partir de diarios_eventos.csv",
  "raca_eleitos.csv" = "TSE (consulta_cand, cor ou raca autodeclarada)",
  "migracao_partidaria.csv" = "Autor, a partir de filiacoes.csv, mandatos.csv e sinais_tse_exercicio.csv",
  "migracao_territorial.csv" = "Autor, a partir de mandatos.csv",
  "migracao_territorial_candidaturas.csv" = "Autor, a partir de mandatos.csv e do cadastro de candidaturas do TSE")
cb[, fonte := fonte_arquivo[arquivo]]
cb[is.na(fonte), fonte := "Autor, a partir das tabelas do BOCEL"]
# colunas construidas pelo autor mesmo em tabela de fonte externa
derivadas <- c("id_mandato", "id_pessoa", "esfera", "unidade_posicao", "forma_saida", "fonte_forma_saida",
               "data_posse", "data_fim_efetiva", "exercicio_confirmado", "fonte_exercicio", "antecessor_id",
               "sucessor_id", "via_sucessao", "reeleito_mesma_pessoa", "chave_dedup", "dedup_ponte_nome_nascimento",
               "dedup_auditoria", "regiao", "nome_normalizado", "metodo_pareamento", "tipo_pareamento",
               "id_pessoa_bocel", "id_mandato_bocel", "negra", "troca_efetiva", "origem_canon", "destino_canon", "tipo")
cb[variavel %in% derivadas, fonte := paste0("Autor, a partir de ", sub(" \\(.*", "", fonte))]
cb[variavel %in% c("id_mandato", "id_pessoa") & arquivo %in% c("mandatos.csv", "pessoas.csv", "posicoes_ano.csv"),
   fonte := "Autor, identificador construido na deduplicacao"]

## cobertura temporal por arquivo, no formato da Base dos Dados
cob <- rbindlist(lapply(unique(cb$arquivo), function(a) {
  # 21/09/2026: recorte da v1.0 — o livro de codigos agora documenta as tabelas de data_v1/, e a
  # cobertura temporal vem de la; o caminho antigo (data/) fica so como retaguarda
  f <- if (file.exists(file.path("data_v1", a))) file.path("data_v1", a) else file.path("data", a)
  if (!file.exists(f)) return(data.table(arquivo = a, cobertura_temporal = NA_character_, linhas = NA_integer_))
  x <- fread(f, colClasses = "character", na.strings = "NA", showProgress = FALSE)
  cand <- intersect(c("ano_eleicao", "ano", "ano_evento", "ano_candidatura", "ano_munic", "ano_ajuizamento",
                      "ano_eleicao_bocel", "ano_destino", "legislatura_inicio"), names(x))
  cv <- NA_character_
  if (length(cand)) {
    v <- suppressWarnings(as.integer(substr(x[[cand[1]]], 1, 4)))
    v <- v[!is.na(v) & v > 1900 & v < 2100]
    if (length(v)) cv <- sprintf("%d(1)%d", min(v), max(v))
  }
  data.table(arquivo = a, cobertura_temporal = cv, linhas = nrow(x))
}))
cb <- merge(cb, cob, by = "arquivo", all.x = TRUE, sort = FALSE)
setcolorder(cb, c("arquivo", "variavel", "descricao", "tipo_armazenamento", "nivel_medida", "fonte",
                  "preenchimento", "cobertura_temporal", "linhas"))
fwrite(cb, "docs/LIVRO_DE_CODIGOS.csv", na = "NA", quote = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "indice")
idx <- unique(cb[, .(arquivo, linhas, cobertura_temporal, variaveis = .N), by = arquivo][, .(arquivo, linhas, cobertura_temporal, variaveis)])
writeData(wb, "indice", idx); freezePane(wb, "indice", firstRow = TRUE); setColWidths(wb, "indice", 1:4, "auto")
# 12/09/2026: o Excel limita a aba a 31 caracteres, e dois arquivos com o mesmo prefixo longo colidiam
# (exercicio_assembleias_historico e sua tabela de cobertura); a colisao ganha sufixo numerico
usadas <- "indice"
for (a in unique(cb$arquivo)) {
  ab <- substr(gsub("\\.csv$", "", a), 1, 31)
  k <- 1L
  while (tolower(ab) %in% tolower(usadas)) { k <- k + 1L; ab <- paste0(substr(gsub("\\.csv$", "", a), 1, 28), "_", k) }
  usadas <- c(usadas, ab)
  addWorksheet(wb, ab)
  writeData(wb, ab, cb[arquivo == a, .(variavel, descricao, tipo_armazenamento, nivel_medida, fonte, preenchimento)])
  freezePane(wb, ab, firstRow = TRUE); setColWidths(wb, ab, 1:6, c(28, 90, 16, 14, 46, 16))
}
saveWorkbook(wb, "docs/LIVRO_DE_CODIGOS.xlsx", overwrite = TRUE)

registrar_numero("cb_arquivos_documentados", uniqueN(cb$arquivo), script = script)
registrar_numero("cb_variaveis_documentadas", nrow(cb), script = script)
registrar_numero("cb_variaveis_com_fonte_externa", cb[!grepl("^Autor", fonte), .N], script = script)
registrar_numero("cb_variaveis_derivadas_pelo_autor", cb[grepl("^Autor", fonte), .N], script = script)
cat("35_livro_codigos_tabular: concluido —", uniqueN(cb$arquivo), "arquivos,", nrow(cb), "variaveis\n")
print(cb[, .N, by = fonte][order(-N)][1:12])
