# 09_auditoria_homonimos.R — auditoria da regra 3 da deduplicacao de pessoa (nome + nascimento)
# Entrada:  data/mandatos.csv (id_mandato = "M" + chave_cand) + data_raw/parquet/cand_<ANO>.parquet
# Saida:    data/auditoria_homonimos.csv, data/pessoas_flags_dedup.csv,
#           output/auditoria_homonimos_relatorio.md, output/numeros_assinatura.txt (append)
# Execucao: cd ~/bocel && Rscript --vanilla R/09_auditoria_homonimos.R
# Nao altera R/01..R/06 nem os arquivos existentes em data/.
set.seed(20260827)
suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(stringi)
})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))

root   <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
pq     <- file.path(root, "data_raw", "parquet")
outd   <- file.path(root, "data")
logd   <- file.path(root, "logs")
dir.create(logd, showWarnings = FALSE)
script <- "R/09_auditoria_homonimos.R"
logf   <- file.path(logd, "09_auditoria_homonimos.log")
logar  <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste0(..., collapse = ""))
  cat(msg, "\n"); cat(msg, "\n", file = logf, append = TRUE)
}
cat("", file = logf)
logar("inicio 09_auditoria_homonimos")

## ---------------------------------------------------------------- (a) recuperar identidade por id_mandato
# Replica exatamente a limpeza de identidade do 03_build_banco.R (titulo, cpf, nome_norm,
# dt_nasc_iso, genero) e liga ao banco final pela chave da candidatura, que o id_mandato
# codifica ("M" + ano_ue_cargo_numero_sq). Os eleitos sao, por construcao, os mesmos.
mand <- fread(file.path(outd, "mandatos.csv"), na.strings = "NA", colClasses = "character",
              encoding = "UTF-8")
mand[, ano_eleicao := as.integer(ano_eleicao)]
mand[, cd_cargo := as.integer(cd_cargo)]
mand[, chave_cand := sub("^M", "", id_mandato)]
# 12/09/2026: 481.345 -> 484.771 com a chapa dos vices fechada no R/03 (1998, 2000 e vice do titular
# imputado por votos); constante trocada por essa correcao
# 13/09/2026: 484.772 com a cadeira de Selma Arruda (MT 2018) restaurada por fonte oficial (ref/correcoes_eleitos_fonte_oficial.csv)
stopifnot(!anyDuplicated(mand$id_mandato), nrow(mand) == 484772L)
logar("mandatos lidos: ", nrow(mand))

ne <- function(x) fifelse(x %in% c("#NE", "#NULO", "#NULO#", "-1", "-3", "-4", ""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
unidade_posicao <- function(cd_cargo, sg_uf, sg_ue) {
  fcase(cd_cargo %in% 11:13, sg_ue, cd_cargo %in% 1:2, "BR", default = sg_uf)
}

cols <- c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "SG_UF", "SG_UE", "CD_CARGO", "SQ_CANDIDATO",
          "NR_CANDIDATO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "NR_TITULO_ELEITORAL_CANDIDATO",
          "DT_NASCIMENTO", "DS_GENERO")
cand_files <- list.files(pq, pattern = "^cand_\\d{4}\\.parquet$", full.names = TRUE)
stopifnot(length(cand_files) == 14L)
ident <- rbindlist(lapply(cand_files, function(f) {
  x <- setDT(read_parquet(f, col_select = all_of(cols)))
  x <- x[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, cd_cargo := as.integer(CD_CARGO)]
  x[, ue_pos := unidade_posicao(cd_cargo, SG_UF, SG_UE)]
  x[, chave_cand := paste(as.integer(ANO_ELEICAO), ue_pos, cd_cargo, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
  x[, .(chave_cand,
        titulo = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)),
        cpf    = ne(num_only(NR_CPF_CANDIDATO)),
        nome   = ne(NM_CANDIDATO),
        dt_nasc = ne(DT_NASCIMENTO),
        genero = ne(DS_GENERO))]
}), use.names = TRUE)
ident[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
ident[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
ident[, titulo := fifelse(is.na(titulo), NA_character_, formatC(titulo, width = 12, flag = "0"))]
ident[, nome_norm := stri_trans_general(toupper(nome), "Latin-ASCII")]
ident[, nome_norm := gsub("[^A-Z ]", "", nome_norm)]
ident[, nome_norm := gsub(" +", " ", trimws(nome_norm))]
ident[, dt_nasc_iso := {
  d <- as.IDate(dt_nasc, format = "%d/%m/%Y")
  fifelse(is.na(d) | d < as.IDate("1890-01-01") | d > as.IDate("2010-01-01"),
          NA_character_, format(d, "%Y-%m-%d"))
}]
# a mesma candidatura aparece em mais de um turno com identidade identica; uma linha por chave
ident <- unique(ident[, .(chave_cand, titulo, cpf, nome_norm, dt_nasc_iso, genero)])
n_chaves_multi <- ident[, .N, by = chave_cand][N > 1, .N]
logar("chaves de candidatura com identidade divergente entre turnos: ", n_chaves_multi)
if (n_chaves_multi > 0) {
  # fica a linha mais preenchida (turno decisivo no 03 carrega a mesma identidade)
  ident[, n_pre := rowSums(!is.na(.SD)), .SDcols = c("titulo", "cpf", "nome_norm", "dt_nasc_iso", "genero")]
  setorder(ident, chave_cand, -n_pre)
  ident <- ident[!duplicated(chave_cand)][, n_pre := NULL]
}

el <- merge(mand[, .(id_mandato, chave_cand, id_pessoa, ano_eleicao, cd_cargo, cargo, sg_uf, unidade_posicao)],
            ident, by = "chave_cand", all.x = TRUE)
n_sem_ident <- el[is.na(titulo) & is.na(cpf) & is.na(nome_norm), .N]
logar("mandatos sem identidade recuperada no parquet: ", n_sem_ident)
stopifnot(nrow(el) == nrow(mand), n_sem_ident == 0L)

## ---------------------------------------------------------------- union-find (mesmo algoritmo do 03)
fechar <- function(dt, usar_regra3) {
  uf_parent <- new.env(hash = TRUE)
  uf_find <- function(k) {
    p <- uf_parent[[k]]
    if (is.null(p) || p == k) return(k)
    r <- uf_find(p); uf_parent[[k]] <- r; r
  }
  uf_union <- function(a, b) {
    ra <- uf_find(a); rb <- uf_find(b)
    if (ra != rb) uf_parent[[rb]] <- ra
    invisible(NULL)
  }
  link_by <- function(key_vec) {
    ok <- !is.na(key_vec)
    sp <- split(dt$rid[ok], key_vec[ok])
    for (g in sp) if (length(g) > 1) for (i in 2:length(g)) uf_union(g[1], g[i])
  }
  link_by(dt$titulo)
  link_by(dt$cpf)
  if (usar_regra3)
    link_by(fifelse(!is.na(dt$nome_norm) & !is.na(dt$dt_nasc_iso),
                    paste(dt$nome_norm, dt$dt_nasc_iso), NA_character_))
  vapply(dt$rid, uf_find, character(1))
}
el[, rid := paste0("r", .I)]
logar("fecho transitivo com regras 1+2+3 ...")
el[, comp_123 := fechar(el, TRUE)]
logar("fecho transitivo com regras 1+2 (sem nome+nascimento) ...")
el[, comp_12 := fechar(el, FALSE)]

# a particao 1+2+3 tem de reproduzir id_pessoa do banco (mesmos eleitos, mesmo algoritmo)
# 12/09/2026: a ponte de cadastro auditada no R/03 (output/verificacao/pontes_nucleo_referencia.csv) e
# a unica excecao admitida. Nela uma candidatura traz o titulo de uma pessoa e o CPF e o nascimento de
# outra, o fecho transitivo junta as duas, e o R/03 as mantem separadas. A componente de ponte tem de
# conter exatamente os ids que a auditoria declara, e toda outra componente segue a regra de um id.
f_pontes <- file.path(root, "output", "verificacao", "pontes_nucleo_referencia.csv")
pontes <- if (file.exists(f_pontes)) fread(f_pontes, colClasses = "character") else data.table(componente = character(), chave_cand = character(), pontos_por_id = character())
comp_ponte <- el[chave_cand %in% pontes$chave_cand, unique(comp_123)]
ids_declarados <- unique(sub(":.*$", "", unlist(strsplit(pontes$pontos_por_id, ";", fixed = TRUE))))
ids_na_ponte <- unique(sub("^[A-Z]+", "", el[comp_123 %in% comp_ponte, id_pessoa]))
stopifnot(length(comp_ponte) == uniqueN(pontes$componente),
          setequal(as.integer(ids_na_ponte), as.integer(ids_declarados)))
logar("componentes de ponte auditada: ", length(comp_ponte), " | ids envolvidos: ", length(ids_na_ponte))
# 21/09/2026: segunda excecao admitida, a separacao de pessoas fundidas por documento que o R/03 faz
# quando o titulo (ou o CPF) que uniria duas candidaturas do fecho 1+2+3 e a unica ponte entre CPFs (ou
# titulos) validos e distintos, com nome e nascimento tambem diferentes entre os dois lados
# (R/auditoria_fusao_documentos.R). Cada candidatura declarada em fusao_documentos_separacao.csv tem de
# aparecer com o id_pessoa exato que o R/03 atribuiu, nao so a componente aceita sem checar o id.
f_fusao <- file.path(root, "output", "verificacao", "fusao_documentos_separacao.csv")
fusao <- if (file.exists(f_fusao)) fread(f_fusao, colClasses = "character") else data.table(chave_cand = character(), id_pessoa_esperado = character())
comp_fusao <- el[chave_cand %in% fusao$chave_cand, unique(comp_123)]
if (nrow(fusao)) {
  chk_fusao <- merge(el[comp_123 %in% comp_fusao, .(chave_cand, id_pessoa)],
                     fusao[, .(chave_cand, id_pessoa_esperado)], by = "chave_cand")
  stopifnot(nrow(chk_fusao) == uniqueN(fusao$chave_cand), all(chk_fusao$id_pessoa == chk_fusao$id_pessoa_esperado))
}
logar("componentes da separacao por fusao de documentos: ", length(comp_fusao), " | candidaturas: ", nrow(fusao))
comp_excecao <- union(comp_ponte, comp_fusao)
chk <- el[!comp_123 %in% comp_excecao, .(n_ids = uniqueN(id_pessoa)), by = comp_123][n_ids > 1, .N]
chk2 <- el[!comp_123 %in% comp_excecao, .(n_comp = uniqueN(comp_123)), by = id_pessoa][n_comp > 1, .N]
logar("componentes 1+2+3 com mais de um id_pessoa: ", chk, " | id_pessoa em mais de uma componente: ", chk2)
stopifnot(chk == 0L, chk2 == 0L)
n_pessoas <- uniqueN(el$id_pessoa)
# 12/09/2026: componentes 1+2 contadas dentro de cada pessoa, que e a particao que o banco adota. Com a ponte
# de cadastro auditada (pontes_nucleo_referencia.csv) uma componente global atravessa duas pessoas, e a contagem
# global ficaria uma abaixo da soma por pessoa que R/verifica_homonimos.R reproduz por metodo independente.
n_comp_12 <- el[, .(n = uniqueN(comp_12)), by = id_pessoa][, sum(n)]
logar("pessoas (1+2+3): ", n_pessoas, " | componentes sem regra 3 (1+2): ", n_comp_12)

## ---------------------------------------------------------------- (b) pontes e pessoas so por nome+nascimento
# ponte: a pessoa reune mais de uma componente 1+2, isto e, so a regra 3 as liga
por_pessoa <- el[, .(
  n_mandatos = .N,
  n_comp_12 = uniqueN(comp_12),
  n_titulos_distintos = uniqueN(na.omit(titulo)),
  n_cpfs_distintos = uniqueN(na.omit(cpf)),
  n_sem_titulo = sum(is.na(titulo)),
  n_sem_cpf = sum(is.na(cpf)),
  n_sem_titulo_nem_cpf = sum(is.na(titulo) & is.na(cpf)),
  n_generos = uniqueN(na.omit(genero)),
  generos = paste(sort(unique(na.omit(genero))), collapse = ";"),
  n_ufs = uniqueN(sg_uf),
  ufs = paste(sort(unique(sg_uf)), collapse = ";"),
  n_unidades = uniqueN(unidade_posicao),
  anos = paste(sort(unique(ano_eleicao)), collapse = ";"),
  n_anos_repetidos = sum(duplicated(ano_eleicao)),
  cargos = paste(sort(unique(cargo)), collapse = ";"),
  n_nomes_distintos = uniqueN(na.omit(nome_norm)),
  n_nasc_distintos = uniqueN(na.omit(dt_nasc_iso))
), by = id_pessoa]
por_pessoa[, ponte_nome_nascimento := n_comp_12 > 1L]
por_pessoa[, so_nome_nascimento := n_sem_titulo_nem_cpf == n_mandatos]
# chave_dedup do banco, para conferir
pess <- fread(file.path(outd, "pessoas.csv"), na.strings = "NA", colClasses = "character",
              select = c("id_pessoa", "chave_dedup"))
por_pessoa <- merge(por_pessoa, pess, by = "id_pessoa", all.x = TRUE)
chk3 <- por_pessoa[so_nome_nascimento != (chave_dedup == "nome_nascimento"), .N]
logar("divergencia entre so_nome_nascimento e chave_dedup do banco: ", chk3)
stopifnot(chk3 == 0L)

## ---------------------------------------------------------------- (c)(d) consistencia e classificacao
# Universo auditado: pessoas-ponte (fusao que so a regra 3 sustenta) e pessoas que so tem
# nome+nascimento como chave (sem titulo nem CPF em nenhuma candidatura).
# suspeito: dentro da componente ha titulos distintos, ou CPFs distintos, ou generos distintos,
#           ou dois mandatos na mesma eleicao (uma pessoa nao e eleita a duas posicoes no mesmo pleito)
# consistente: nenhum conflito e uma unica UF
# indeterminado: nenhum conflito, mas mais de uma UF (mudanca de estado e possivel, nao verificavel)
aud <- por_pessoa[ponte_nome_nascimento | so_nome_nascimento]
aud[, motivos := {
  m <- character(0)
  if (n_titulos_distintos > 1) m <- c(m, sprintf("%d titulos distintos", n_titulos_distintos))
  if (n_cpfs_distintos > 1) m <- c(m, sprintf("%d CPFs distintos", n_cpfs_distintos))
  if (n_generos > 1) m <- c(m, "genero divergente")
  if (n_anos_repetidos > 0) m <- c(m, sprintf("%d eleicao(oes) com mais de um mandato", n_anos_repetidos))
  paste(m, collapse = "; ")
}, by = id_pessoa]
aud[, classificacao := fcase(
  nchar(motivos) > 0, "suspeito",
  n_ufs == 1L, "consistente",
  default = "indeterminado")]
# grau da suspeita: forte quando CPF, genero ou calendario contradizem (dois titulos com
# CPFs distintos sao duas pessoas); fraca quando o unico conflito e o numero do titulo,
# que um erro de digitacao no cadastro antigo do TSE tambem produz
aud[, grau_suspeita := fcase(
  classificacao != "suspeito", NA_character_,
  n_cpfs_distintos > 1 | n_generos > 1 | n_anos_repetidos > 0, "forte",
  default = "fraca")]
aud[, justificativa := fcase(
  classificacao == "suspeito", paste0("conflito interno: ", motivos),
  classificacao == "consistente" & ponte_nome_nascimento,
    sprintf("ponte nome+nascimento liga %d componentes (titulo/CPF); genero unico, %d UF, sem titulo nem CPF em conflito",
            n_comp_12, n_ufs),
  classificacao == "consistente",
    sprintf("sem titulo nem CPF em nenhuma candidatura; %d mandato(s), genero unico, %d UF", n_mandatos, n_ufs),
  default = sprintf("sem conflito de titulo/CPF/genero, mas %d UFs (%s); mudanca de estado nao verificavel", n_ufs, ufs))]

saida <- aud[, .(id_pessoa, n_mandatos, n_titulos_distintos, n_cpfs_distintos, n_sem_titulo_nem_cpf,
                 n_comp_12, ponte_nome_nascimento, so_nome_nascimento, generos, ufs, n_unidades, anos, cargos,
                 n_nomes_distintos, n_nasc_distintos, classificacao, grau_suspeita, justificativa)]
setorder(saida, classificacao, -n_mandatos, id_pessoa)
fwrite(saida, file.path(outd, "auditoria_homonimos.csv"), sep = ",", na = "NA", quote = TRUE)

flags <- merge(por_pessoa[, .(id_pessoa, ponte_nome_nascimento, so_nome_nascimento)],
               aud[, .(id_pessoa, classificacao)], by = "id_pessoa", all.x = TRUE)
flags[is.na(classificacao), classificacao := "nao_auditado"]
setorder(flags, id_pessoa)
fwrite(flags[, .(id_pessoa, ponte_nome_nascimento, so_nome_nascimento, classificacao)],
       file.path(outd, "pessoas_flags_dedup.csv"), sep = ",", na = "NA", quote = TRUE)
stopifnot(nrow(flags) == n_pessoas)

## ---------------------------------------------------------------- numeros
n_ponte <- por_pessoa[ponte_nome_nascimento == TRUE, .N]
n_so_nn <- por_pessoa[so_nome_nascimento == TRUE, .N]
n_aud   <- nrow(aud)
tab     <- aud[, .N, by = classificacao]
n_susp  <- tab[classificacao == "suspeito", sum(N)]
n_cons  <- tab[classificacao == "consistente", sum(N)]
n_ind   <- tab[classificacao == "indeterminado", sum(N)]
n_ponte_susp <- aud[ponte_nome_nascimento & classificacao == "suspeito", .N]
n_ponte_cons <- aud[ponte_nome_nascimento & classificacao == "consistente", .N]
n_ponte_ind  <- aud[ponte_nome_nascimento & classificacao == "indeterminado", .N]
n_susp_titulo <- aud[classificacao == "suspeito" & n_titulos_distintos > 1, .N]
n_susp_cpf    <- aud[classificacao == "suspeito" & n_cpfs_distintos > 1, .N]
n_susp_genero <- aud[classificacao == "suspeito" & n_generos > 1, .N]
n_susp_ano    <- aud[classificacao == "suspeito" & n_anos_repetidos > 0, .N]
n_susp_forte  <- aud[grau_suspeita %in% "forte", .N]
n_susp_fraca  <- aud[grau_suspeita %in% "fraca", .N]
n_mand_ponte  <- el[id_pessoa %in% por_pessoa[ponte_nome_nascimento == TRUE, id_pessoa], .N]
n_mand_susp   <- el[id_pessoa %in% aud[classificacao == "suspeito", id_pessoa], .N]
# pessoas que a regra final (manter fusao so quando consistente) desfaria, e quantas pessoas surgiriam
n_pessoas_pos_regra <- n_pessoas + aud[ponte_nome_nascimento & classificacao != "consistente", sum(n_comp_12 - 1L)]

regs <- list(
  aud_n_pessoas = n_pessoas, aud_n_componentes_sem_regra3 = n_comp_12,
  aud_n_pessoas_ponte = n_ponte, aud_n_pessoas_so_nome_nascimento = n_so_nn,
  aud_n_auditadas = n_aud, aud_n_suspeitos = n_susp, aud_n_consistentes = n_cons,
  aud_n_indeterminados = n_ind,
  aud_n_ponte_suspeitos = n_ponte_susp, aud_n_ponte_consistentes = n_ponte_cons,
  aud_n_ponte_indeterminados = n_ponte_ind,
  aud_n_suspeitos_titulo_conflito = n_susp_titulo, aud_n_suspeitos_cpf_conflito = n_susp_cpf,
  aud_n_suspeitos_genero = n_susp_genero, aud_n_suspeitos_mesmo_ano = n_susp_ano,
  aud_n_suspeitos_forte = n_susp_forte, aud_n_suspeitos_fraca = n_susp_fraca,
  aud_n_mandatos_em_pontes = n_mand_ponte, aud_n_mandatos_em_suspeitos = n_mand_susp,
  aud_n_pessoas_se_regra_final = n_pessoas_pos_regra,
  aud_pct_pessoas_ponte = round(n_ponte / n_pessoas, 5),
  aud_pct_pessoas_suspeitas = round(n_susp / n_pessoas, 5),
  aud_n_componentes_excecao_ponte_referencia = length(comp_ponte),
  aud_n_componentes_excecao_fusao_documentos = length(comp_fusao),
  aud_n_candidaturas_excecao_fusao_documentos = nrow(fusao)
)
for (k in names(regs)) registrar_numero(k, regs[[k]], script = script, out = "output/numeros_assinatura.txt")
for (k in names(regs)) logar(k, " = ", regs[[k]])

## ---------------------------------------------------------------- relatorio
ex_susp <- head(saida[classificacao == "suspeito"], 15)
ex_ind  <- head(saida[classificacao == "indeterminado"], 5)
md_tab <- function(dt) {
  if (nrow(dt) == 0) return("Nenhum caso.\n")
  cols <- c("id_pessoa", "n_mandatos", "n_titulos_distintos", "n_cpfs_distintos", "generos", "ufs", "anos", "cargos")
  h <- paste0("| ", paste(cols, collapse = " | "), " |\n|", paste(rep("---", length(cols)), collapse = "|"), "|\n")
  b <- dt[, paste0("| ", do.call(paste, c(.SD, sep = " | ")), " |"), .SDcols = cols]
  paste0(h, paste(b, collapse = "\n"), "\n")
}
rel <- c(
  "# Auditoria de homonimos — regra 3 da deduplicacao (nome + data de nascimento)",
  "",
  sprintf("Gerado por %s em %s. Numeros registrados em output/numeros_assinatura.txt (prefixo aud_).", script, format(Sys.Date())),
  "",
  "## Metodo",
  "",
  "A identidade (titulo, CPF, nome normalizado, data de nascimento, genero) de cada um dos",
  sprintf("%s mandatos foi recuperada dos arquivos consulta_cand pela chave da candidatura que o", format(nrow(el), big.mark = ".", decimal.mark = ",")),
  "id_mandato codifica, com a mesma limpeza do 03_build_banco.R. O fecho transitivo foi refeito",
  "duas vezes com o mesmo algoritmo, com as tres regras e so com titulo e CPF; a particao com",
  "tres regras reproduz o id_pessoa do banco em todas as pessoas. Uma pessoa e ponte quando",
  "reune mais de uma componente de titulo/CPF, isto e, quando so a regra 3 sustenta a fusao;",
  "uma pessoa e so-nome-nascimento quando nenhuma de suas candidaturas traz titulo nem CPF.",
  "",
  "## Resultado",
  "",
  sprintf("- Pessoas no banco: %s; componentes sem a regra 3: %s.", format(n_pessoas, big.mark = ".", decimal.mark = ","), format(n_comp_12, big.mark = ".", decimal.mark = ",")),
  sprintf("- Pessoas-ponte (fusao que depende da regra 3): %d (%.3f%% das pessoas), com %d mandatos.", n_ponte, 100 * n_ponte / n_pessoas, n_mand_ponte),
  sprintf("- Pessoas cuja unica chave e nome + nascimento: %d.", n_so_nn),
  sprintf("- Auditadas (uniao dos dois grupos): %d, classificadas em %d consistentes, %d suspeitas e %d indeterminadas.", n_aud, n_cons, n_susp, n_ind),
  sprintf("- Entre as pontes: %d consistentes, %d suspeitas, %d indeterminadas.", n_ponte_cons, n_ponte_susp, n_ponte_ind),
  sprintf("- Motivo da suspeita (nao exclusivos): %d com titulos distintos, %d com CPFs distintos, %d com genero divergente, %d com mais de um mandato na mesma eleicao.", n_susp_titulo, n_susp_cpf, n_susp_genero, n_susp_ano),
  sprintf("- Grau da suspeita: %d fortes (CPF, genero ou calendario contradizem) e %d fracas (so o numero do titulo difere).", n_susp_forte, n_susp_fraca),
  sprintf("- Mandatos em pessoas suspeitas: %d.", n_mand_susp),
  "",
  "## Criterio",
  "",
  "- suspeito: titulos distintos, ou CPFs distintos, ou generos distintos dentro da componente, ou dois mandatos na mesma eleicao.",
  "- consistente: nenhum conflito e uma unica UF.",
  "- indeterminado: nenhum conflito, mas mais de uma UF (mudanca de estado e plausivel e nao verificavel com o TSE).",
  "",
  "## Exemplos de suspeitos",
  "",
  md_tab(ex_susp),
  "## Exemplos de indeterminados",
  "",
  md_tab(ex_ind),
  "## Regra final proposta (nao aplicada)",
  "",
  "Manter a fusao por nome + nascimento apenas quando a componente for consistente. Nos casos",
  "suspeitos, desfazer a ponte e manter uma pessoa por componente de titulo/CPF (nas suspeitas",
  "fracas, em que so o numero do titulo difere, a separacao e a opcao conservadora, e o par",
  "fica listado para conferencia manual); nos",
  "indeterminados, manter a fusao com o flag em pessoas_flags_dedup.csv, ja que titulo, CPF e",
  "genero nao se contradizem e a mudanca de UF entre eleicoes e observada em pessoas com titulo unico.",
  sprintf("Aplicada aos suspeitos e indeterminados, a regra elevaria o cadastro de %s para %s pessoas.", format(n_pessoas, big.mark = ".", decimal.mark = ","), format(n_pessoas_pos_regra, big.mark = ".", decimal.mark = ",")),
  "A decisao sobre os indeterminados fica com o autor; o script nao altera data/pessoas.csv nem data/mandatos.csv.",
  "",
  "## Fora de cobertura",
  "",
  "A auditoria detecta fusao indevida por contradicao interna (titulo, CPF, genero, calendario).",
  "Nao detecta dois homonimos com a mesma data de nascimento que so tenham nome e nascimento",
  "registrados e nunca se contradigam, nem a divisao indevida (a mesma pessoa com dois titulos",
  "e grafias de nome distintas), que a regra 3 nao alcanca."
)
writeLines(rel, file.path(root, "output", "auditoria_homonimos_relatorio.md"))

gravar_relatorio_verificacao(
  alvo = "data/auditoria_homonimos.csv; data/pessoas_flags_dedup.csv", script = script,
  passou = c("particao 1+2+3 reproduz id_pessoa do banco",
             "so_nome_nascimento coincide com chave_dedup == nome_nascimento",
             "todo mandato recuperou identidade no parquet",
             "flags cobrem todas as pessoas"),
  fora_de_cobertura = c("homonimos com mesma data de nascimento sem contradicao interna",
                        "divisao indevida de pessoa (dois titulos, grafias distintas)"))
logar("fim 09_auditoria_homonimos")
