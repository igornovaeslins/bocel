# 06_amarracoes_fapesp.R — gera docs/AMARRACOES_FAPESP.md (spec §8) com numeros lidos dos
# arquivos finais e do registro de numeros verificados, nunca digitados a mao.
# Execucao: cd ~/bocel && Rscript --vanilla R/06_amarracoes_fapesp.R
suppressPackageStartupMessages({ library(data.table) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)

# parse robusto do registro (chave | valor | ...): so os dois primeiros campos; vale o ultimo registro por chave
reg_l <- readLines("output/numeros_assinatura.txt", warn = FALSE)
reg <- rbindlist(lapply(strsplit(reg_l[grepl("|", reg_l, fixed = TRUE)], "|", fixed = TRUE),
                        function(p) data.table(chave = trimws(p[1]), valor = trimws(p[2]))))
reg <- reg[, .SD[.N], by = chave]
n <- function(k) reg[chave == k, valor]
fmt <- function(x) format(as.numeric(x), big.mark = ".", decimal.mark = ",")

n_pess <- fmt(n("v1_n_pessoas")); n_mand <- fmt(n("v1_n_mandatos"))
n_pos  <- fmt(n("v1_n_posicoes_ano"))
n_fil  <- if ("v1_n_filiacoes" %in% reg$chave) fmt(n("v1_n_filiacoes")) else NA
pct_fil <- if ("v1_pct_pessoas_com_filiacao" %in% reg$chave)
  sprintf("%.0f%%", 100 * as.numeric(n("v1_pct_pessoas_com_filiacao"))) else NA
info_dep <- tryCatch(jsonlite::fromJSON("zenodo/deposito_info.json"), error = function(e) NULL)
# 21/09/2026: a nota e a sumula citam o DOI-conceito (estavel entre versoes), nao o da v1.0; ele so fica
# confirmado de fato quando a primeira versao publica, e ate la usamos o provisorio ou o da propria versao
doi <- if (!is.null(info_dep$conceptdoi_provisorio)) info_dep$conceptdoi_provisorio else
       if (!is.null(info_dep$doi_reservado)) info_dep$doi_reservado else "[DOI reservado no Zenodo]"

tem <- function(k) k %in% reg$chave
# 21/09/2026: a forma de saida no recorte da v1.0
# (Presidencia, Senado, Camara dos Deputados, governos estaduais) e fechada: todo mandato encerrado
# tem forma de saida com fonte identificada, e o que resta sem ela esta em curso
saida_frase <- if (tem("v1_mandatos_com_saida_observada")) {
  n_obs <- as.numeric(n("v1_mandatos_com_saida_observada")); n_curso <- as.numeric(n("v1_mandatos_em_curso"))
  n_tot <- as.numeric(n("v1_n_mandatos"))
  sprintf(" A forma de saída de cada mandato encerrado (fim regular, renúncia, morte, cassação, afastamento) está observada em %s dos %s mandatos do recorte, e os %s restantes seguem em curso.",
          fmt(n_obs), fmt(n_tot), fmt(n_curso))
} else ""
fil_frase <- if (!is.na(n_fil))
  sprintf(" As listas de filiação partidária do TSE, pareadas pelo título de eleitor, dão o histórico de filiação de %s dessas pessoas, em %s registros.", pct_fil, n_fil) else ""

txt <- sprintf('# Amarrações do BOCEL no projeto FAPESP (spec §8)

Todos os números abaixo vêm de `output/numeros_assinatura.txt` (script 04_verificar.R). DOI %s.

## Nota de rodapé 4 (substitui a versão truncada)

> Parto do Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL, v1.0, DOI %s), que construí a partir dos resultados eleitorais do TSE e que registra %s pessoas em %s mandatos na Presidência, no Senado, na Câmara dos Deputados e nos governos estaduais, nas eleições ordinárias de 1998 a 2024, com identificador estável de pessoa e a sucessão de cada cadeira, com a ordem de suplência de cada mandato.%s%s A unidade de observação é pessoa × cargo × ano (%s posições), o que permite acompanhar permanência, saída e substituição ao longo dos mandatos.

## §2 — resultados esperados (produto entregue)

O BOCEL v1.0, primeiro produto do projeto, está depositado no Zenodo sob licença CC BY 4.0, com livro de códigos, nota de cobertura e script de reconstrução a partir dos brutos do TSE, e cobre %s pessoas e %s mandatos na Presidência, no Senado, na Câmara dos Deputados e nos governos estaduais de 1998 a 2024, com posse, exercício e forma de saída fechados no recorte (a nota 4 dá a cobertura).%s A v1.5, prevista para o segundo ano da bolsa, acrescenta as Assembleias Legislativas e os cargos municipais, com posse, exercício e forma de saída a partir dos diários oficiais das casas legislativas e da Justiça Eleitoral, e a sucessão por cadeira nessas casas, que junto com a saída forma a variável dependente da frente 2.

## §6 — contribuição ao Auxílio 2024/19449-2

O BOCEL entra no Auxílio como infraestrutura de dados compartilhada e citável por DOI, de modo que a equipe da supervisora pode reutilizá-lo no pareamento de vítimas de violência política a trajetórias eleitorais sem repetir a construção.

## Plano de Gestão de Dados

Os dados do projeto derivam de fontes públicas de transparência ativa (TSE) e já estão organizados no BOCEL v1.0 (Zenodo, DOI %s, CC BY 4.0), em CSV UTF-8 e parquet, com livro de códigos por variável, nota de cobertura e script que reconstrói o banco do zero. As versões seguintes entram no mesmo registro por *New version*, sob o mesmo concept DOI, e o histórico de correções fica público no próprio Zenodo.

## Súmula curricular

Lins, Igor Novaes. Banco de Ocupação de Cargos Eletivos no Brasil / Brazilian Elective Office Occupancy Database (BOCEL), 1998–2024. Versão 1.0. Conjunto de dados. Zenodo, 2026. DOI %s.
', doi, doi, n_pess, n_mand, fil_frase, saida_frase, n_pos, n_pess, n_mand, "", doi, doi)
# 12/09/2026: o Auxilio 2024/19449-2 nao financiou o banco, e a secao 6 deixou de dizer que o
# deposito o registra como financiador; a referencia da sumula segue o titulo do registro no Zenodo
writeLines(txt, "docs/AMARRACOES_FAPESP.md")
cat("docs/AMARRACOES_FAPESP.md gerado\n")
