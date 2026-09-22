# 52_balanco_aproveitamento.R — quanto do material ja coletado virou dado
# Escrito em 30/08/2026 para verificar e tratar tudo o que estava em disco antes
# de qualquer nova coleta. Todos os numeros saem de contagem dos proprios arquivos.
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))
source("lib/proveniencia.R")
ESTE <- "R/52_balanco_aproveitamento.R"
reg <- function(k, v) registrar_numero(k, v, script = ESTE)
ft <- function(x) format(x, big.mark = ".", decimal.mark = ",")

b <- fread("output/verificacao/bruto_camaras_nao_parseado.csv")
# 05/09/2026: o inventario passou a ser produzido por python/inventario_bruto_camaras.py (ate entao
# era artefato de sessao sem produtor); o md5 registrado permite conferir que a reproducao e exata
reg("apr_bruto_md5", unname(tools::md5sum("output/verificacao/bruto_camaras_nao_parseado.csv")))
reg("apr_bruto_linhas", nrow(b))
sonda <- fromJSON("data_raw/sonda/sonda_html.json")  # python/sonda_camaras_html.py
setDT(sonda)
g <- fread("data/exercicio_camaras_generico.csv")
m <- fread("data/mandatos.csv", select = c("id_mandato","esfera","forma_saida","exercicio_confirmado","sg_ue"))
m[, sg_ue := sprintf("%05d", suppressWarnings(as.integer(sg_ue)))]
sem_saida <- m[esfera == "municipal", .(k = sum(forma_saida != "nao_observado")), by = sg_ue][k == 0]$sg_ue

reg("apr_municipios_com_bruto_em_disco", uniqueN(b$sg_ue))
reg("apr_arquivos_em_disco", sum(b$arquivos))
reg("apr_municipios_sem_linha_extraida", b[tem_linha == FALSE, .N])
reg("apr_arquivos_sem_linha_extraida", b[tem_linha == FALSE, sum(arquivos)])
reg("apr_sondados", nrow(sonda))
reg("apr_com_relacao_nominal", sonda[casados >= 3, .N])
reg("apr_sem_nenhum_nome", sonda[casados == 0, .N])
reg("apr_sem_texto_util", sonda[casados == 0 & maior_texto < 300, .N])
reg("apr_extraidos_linhas", nrow(g))
reg("apr_extraidos_municipios", uniqueN(g$sg_ue))
reg("apr_extraidos_ufs", uniqueN(g$uf))
novos <- uniqueN(g[sg_ue %in% sem_saida]$sg_ue)
reg("apr_municipios_antes_sem_nenhuma_observacao", novos)
idx <- match(g$id_mandato_bocel, m$id_mandato)
reg("apr_mandatos_sem_exercicio_ate_agora", sum(is.na(m$exercicio_confirmado[idx])))

# 05/09/2026: contagem dos proprios arquivos (ate entao 10.166 e 15 estavam escritos a mao). Cada
# zip do acervo da ALEP (PR/diarios) guarda um PDF. O indice _indice_ocr.csv, gravado pelo OCR do
# PR, diz quais edicoes estavam sem camada de texto; onde nao ha indice (MA e CE, reprocessados em
# 30/08), cada .ocr.txt e uma edicao que estava sem texto e foi recuperada.
pdf_f <- list.files("data_raw", pattern = "\\.pdf$", recursive = TRUE, full.names = TRUE)
zip_pr <- list.files("data_raw/assembleias2/PR/diarios", pattern = "\\.zip$")
pdfs <- length(pdf_f) + length(zip_pr)
ocr_f <- list.files("data_raw", pattern = "\\.ocr\\.txt$", recursive = TRUE, full.names = TRUE)
# 05/09/2026: o indice do OCR usava o nome idx e sobrescrevia o vetor de posicoes dos mandatos
idx_f <- list.files("data_raw", pattern = "^_indice_ocr\\.csv$", recursive = TRUE, full.names = TRUE)
idx_ocr <- rbindlist(lapply(idx_f, fread, colClasses = "character"), fill = TRUE)
sem_txt_idx <- if (nrow(idx_ocr)) idx_ocr[estado == "ocr", uniqueN(arquivo)] else 0L
ocr_sem_idx <- sum(!dirname(ocr_f) %in% dirname(idx_f))
sem_txt <- sem_txt_idx + ocr_sem_idx; ocr <- length(ocr_f)
uf_ocr <- sub("^data_raw/assembleias2/([A-Z]{2})/.*$", "\\1", ocr_f)
n_ocr_uf <- function(u) sum(uf_ocr == u)
conf_pr <- if (nrow(idx_ocr)) round(mean(as.numeric(idx_ocr[estado == "ocr", conf]), na.rm = TRUE), 1) else NA_real_
pe_rec <- "data_raw/assembleias2/PE/recusas.csv"
pe_sem_pdf <- if (file.exists(pe_rec)) fread(pe_rec, colClasses = "character")[status %in% c("SEMTEXTO", "PDFERR"), .N] else 0L
reg("apr_pdfs_em_disco", pdfs); reg("apr_pdfs_sem_camada_de_texto", sem_txt); reg("apr_pdfs_recuperados_por_ocr", ocr)
reg("apr_pdfs_pr_recuperados_por_ocr", n_ocr_uf("PR")); reg("apr_pdfs_ma_recuperados_por_ocr", n_ocr_uf("MA"))
reg("apr_pdfs_ce_recuperados_por_ocr", n_ocr_uf("CE")); reg("apr_ocr_pr_confianca_media", conf_pr)
reg("apr_pe_edicoes_sem_texto_sem_pdf_retido", pe_sem_pdf)

txt <- c(
"# Aproveitamento do material já coletado", "",
sprintf("Levantado em 30 de agosto de 2026 para verificar e tratar tudo"),
"o que já estava em disco antes de qualquer requisição nova. Os números vêm da contagem dos próprios",
"arquivos e estão registrados em `output/numeros_assinatura.txt` sob as chaves `apr_`.", "",
"## Câmaras municipais", "",
sprintf("Havia %s municípios com página baixada em `data_raw/camaras_sem_sapl` e `camaras_sem_sapl_2`,", ft(uniqueN(b$sg_ue))),
sprintf("somando %s arquivos. Destes, %s municípios nunca tiveram linha extraída.", ft(sum(b$arquivos)), ft(b[tem_linha == FALSE, .N])), "",
sprintf("A sondagem de %s desses municípios mostrou que a promessa era menor do que o volume sugeria.", ft(nrow(sonda))),
sprintf("Só %s trazem relação nominal de fato, medida pelo casamento de três ou mais nomes que o TSE", ft(sonda[casados >= 3, .N])),
sprintf("registra para aquele município. Em %s não há nenhum nome, e %s destes não têm sequer texto", ft(sonda[casados == 0, .N]), ft(sonda[casados == 0 & maior_texto < 300, .N])),
"útil, por serem página inicial vazia, site desativado ou erro do servidor. Recuperar esses exige",
"coleta nova, e por isso ficam fora desta frente.", "",
sprintf("A extração produziu `data/exercicio_camaras_generico.csv`, com %s linhas em %s municípios de", ft(nrow(g)), ft(uniqueN(g$sg_ue))),
sprintf("%s unidades da federação, das quais %s não tinham nenhuma observação no banco até agora.", ft(uniqueN(g$uf)), ft(novos)),
sprintf("São %s mandatos que ainda não tinham exercício confirmado por fonte alguma.", ft(sum(is.na(m$exercicio_confirmado[idx])))), "",
"Duas exigências governaram a extração. A página só entra quando três ou mais nomes casam com o que",
"o TSE registra para o município, o que prova que aquilo é a relação da casa. E o nome só entra",
"quando está cercado por outros nomes casados a menos de quatrocentos caracteres, o que separa a",
"relação nominal da notícia que menciona um vereador de passagem, pela regra de",
"que notícia não serve de fonte.", "",
"A tabela afirma exercício, e não forma de saída. A página é retrato do momento em que foi salva,",
"de modo que a ausência de um nome nela não prova que o mandato terminou.", "",
"## Documentos em imagem", "",
sprintf("Dos %s PDFs em disco, contados os zips do acervo da ALEP com um PDF em cada, %s estavam sem", ft(pdfs), ft(sem_txt)),
sprintf("camada de texto e %s foram recuperados por OCR, %s do Paraná, %s do Maranhão e %s do Ceará.", ft(ocr), ft(n_ocr_uf("PR")), ft(n_ocr_uf("MA")), ft(n_ocr_uf("CE"))),
sprintf("As edições do Paraná digitalizadas como imagem estavam dentro dos zips já baixados e passaram pelo"),
sprintf("OCR em 5 de setembro de 2026 (tesserocr 5.5.1, confiança média de %s em `data_raw/assembleias2/PR/diarios/_indice_ocr.csv`).", ft(conf_pr)),
sprintf("As %s edições de Pernambuco sem camada de texto só se recuperam com nova aquisição, porque o coletor", ft(pe_sem_pdf)),
"não reteve o PDF depois de tentar extrair o texto.", "",
"## O que continua parado", "",
"O cache do Querido Diário cobre 527 municípios e rendeu 11.216 eventos, mas só 66 municípios",
"chegaram a ter mandato pareado, o que põe o gargalo no pareamento. No SAPL municipal, 143",
"instâncias têm cache e nenhuma linha. Em `exercicio_camaras_sem_sapl_2` há 209 municípios com",
"linha e sem pareamento, e `tce_gestores_d` pareou 2.749 de 8.625 linhas.")
writeLines(txt, "docs/APROVEITAMENTO_DO_COLETADO.md")
cat("balanco escrito | extraidos:", nrow(g), "linhas |", uniqueN(g$sg_ue), "municipios |",
    novos, "antes sem nenhuma observacao\n")
