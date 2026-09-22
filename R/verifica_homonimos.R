# verifica_homonimos.R — verificacao cetica da frente 'homonimos' (R/09_auditoria_homonimos.R)
# Reconta os numeros a partir dos ARQUIVOS DE SAIDA, aplica os asserts de rigor e refaz de forma
# independente (propagacao de rotulo vetorizada, nao union-find) a identificacao das pontes.
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_homonimos.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow); library(stringi); library(dplyr) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_homonimos.R"
logf <- "logs/verifica_homonimos.log"; cat("", file = logf)
logar <- function(...) { m <- paste0(format(Sys.time(), "%H:%M:%S"), " | ", ...); cat(m, "\n"); cat(m, "\n", file = logf, append = TRUE) }
falhas <- character(0); passou <- character(0)
chk <- function(cond, msg) { if (isTRUE(cond)) { passou <<- c(passou, msg); logar("OK   ", msg) } else { falhas <<- c(falhas, msg); logar("FALHA ", msg) } }

## ------------------------------------------------------------ 1. arquivos, colunas, NA, chaves
aud   <- fread("data/auditoria_homonimos.csv", na.strings = "NA", encoding = "UTF-8")
flags <- fread("data/pessoas_flags_dedup.csv", na.strings = "NA", encoding = "UTF-8")
pess  <- fread("data/pessoas.csv", na.strings = "NA", colClasses = "character", encoding = "UTF-8")
mand  <- fread("data/mandatos.csv", na.strings = "NA", colClasses = "character", encoding = "UTF-8")
cols_aud <- c("id_pessoa","n_mandatos","n_titulos_distintos","n_cpfs_distintos","n_sem_titulo_nem_cpf","n_comp_12",
              "ponte_nome_nascimento","so_nome_nascimento","generos","ufs","n_unidades","anos","cargos",
              "n_nomes_distintos","n_nasc_distintos","classificacao","grau_suspeita","justificativa")
cols_flags <- c("id_pessoa","ponte_nome_nascimento","so_nome_nascimento","classificacao")
chk(identical(names(aud), cols_aud), "auditoria_homonimos.csv tem as 18 colunas prometidas, na ordem")
chk(identical(names(flags), cols_flags), "pessoas_flags_dedup.csv tem as 4 colunas prometidas")
chk(all(grepl("^[a-z0-9_]+$", c(names(aud), names(flags)))), "nomes de coluna minusculos sem acento")
# codigo de ausente: nenhuma celula vazia crua nos CSV (so a string NA)
raw_aud <- readLines("data/auditoria_homonimos.csv", encoding = "UTF-8")
raw_flg <- readLines("data/pessoas_flags_dedup.csv", encoding = "UTF-8")
chk(!any(grepl(',,|,$|^,|,""', raw_aud)), "auditoria: sem celula vazia (ausente = string NA)")
chk(!any(grepl(',,|,$|^,|,""', raw_flg)), "flags: sem celula vazia (ausente = string NA)")
checa_unica(as.data.frame(aud), "id_pessoa"); passou <- c(passou, "auditoria: id_pessoa unico")
checa_unica(as.data.frame(flags), "id_pessoa"); passou <- c(passou, "flags: id_pessoa unico")
checa_unica(as.data.frame(pess), "id_pessoa")
checa_unica(as.data.frame(mand), "id_mandato")

## ------------------------------------------------------------ 2. vocabularios e faixas
in_set(aud$classificacao, c("suspeito","consistente","indeterminado"), permitir_na = FALSE, nome = "aud$classificacao")
in_set(flags$classificacao, c("suspeito","consistente","indeterminado","nao_auditado"), permitir_na = FALSE, nome = "flags$classificacao")
in_set(aud$grau_suspeita, c("forte","fraca"), nome = "aud$grau_suspeita")
chk(all(is.na(aud$grau_suspeita) == (aud$classificacao != "suspeito")), "grau_suspeita preenchido sse suspeito")
in_set(aud$generos, c("MASCULINO","FEMININO","NAO DIVULGAVEL","FEMININO;MASCULINO","NÃO DIVULGÁVEL"), nome = "aud$generos")
em_faixa(aud$n_mandatos, 1, 30, permitir_na = FALSE, nome = "n_mandatos")
em_faixa(aud$n_comp_12, 1, 10, permitir_na = FALSE, nome = "n_comp_12")
em_faixa(aud$n_titulos_distintos, 0, 10, permitir_na = FALSE, nome = "n_titulos_distintos")
em_faixa(aud$n_cpfs_distintos, 0, 10, permitir_na = FALSE, nome = "n_cpfs_distintos")
anos <- as.integer(unlist(strsplit(aud$anos, ";")))
em_faixa(anos, 1998, 2024, permitir_na = FALSE, nome = "anos")
chk(all(anos %% 2 == 0), "anos de eleicao pares")
chk(all(aud$ponte_nome_nascimento | aud$so_nome_nascimento), "toda linha auditada e ponte ou so_nome_nascimento")
chk(all(aud$ponte_nome_nascimento == (aud$n_comp_12 > 1)), "ponte <=> n_comp_12 > 1")
chk(all(aud$so_nome_nascimento == (aud$n_sem_titulo_nem_cpf == aud$n_mandatos)), "so_nome_nascimento <=> nenhum mandato com titulo/CPF")
# regra de classificacao reproduzida a partir das colunas
conflito <- aud$n_titulos_distintos > 1 | aud$n_cpfs_distintos > 1 | grepl(";", aud$generos) |
            grepl("eleicao\\(oes\\)", aud$justificativa)
n_ufs <- lengths(strsplit(aud$ufs, ";"))
cls <- fifelse(conflito, "suspeito", fifelse(n_ufs == 1L, "consistente", "indeterminado"))
chk(all(cls == aud$classificacao), "classificacao reproduzida pelas colunas do proprio CSV")

## ------------------------------------------------------------ 3. joins seguros contra o banco
fl <- join_seguro(as.data.frame(flags), as.data.frame(pess[, .(id_pessoa, chave_dedup, dt_nascimento, nr_titulo_eleitoral, nr_cpf)]),
                  by = "id_pessoa", cardinalidade = "one-to-one", tipo = "inner", unmatched = "error")
chk(nrow(fl) == nrow(pess) && nrow(flags) == nrow(pess), "flags cobrem exatamente as pessoas do banco (1:1, sem orfao)")
chk(all(fl$so_nome_nascimento == (fl$chave_dedup == "nome_nascimento")), "so_nome_nascimento == chave_dedup nome_nascimento")
nm <- mand[, .(n_mand_banco = .N, n_anos_rep = sum(duplicated(ano_eleicao)),
               ufs_banco = paste(sort(unique(sg_uf)), collapse = ";")), by = id_pessoa]
aj <- join_seguro(as.data.frame(aud), as.data.frame(nm), by = "id_pessoa", cardinalidade = "one-to-one", tipo = "inner")
chk(all(aj$n_mandatos == aj$n_mand_banco), "n_mandatos das auditadas == contagem em mandatos.csv")
chk(all(aj$ufs == aj$ufs_banco), "ufs das auditadas == UFs em mandatos.csv")
chk(all((aj$n_anos_rep > 0) == grepl("eleicao\\(oes\\)", aj$justificativa)), "mandato repetido na mesma eleicao == mandatos.csv")
# vocabulario fechado da camada de posse/saida (R/10_integrar_exercicio.R)
in_set(mand$forma_saida, c("fim_regular", "renuncia", "falecimento", "cassacao", "afastamento", "licenca",
                           "nao_tomou_posse", "suplente_efetivado", "perda_do_mandato_inferida_por_eleicao_suplementar",
# 13/09/2026: aposentadoria (por invalidez, registrada pela Camara) e impeachment entram no vocabulario fechado
# 13/09/2026: retotalizacao (perda da vaga por nova totalizacao dos votos, sem ato ilicito) entra no vocabulario fechado
                           "substituicao_inferida_munic", "assumiu_titular", "aposentadoria", "impeachment", "retotalizacao", "outro", "nao_observado"),
       permitir_na = FALSE, nome = "mandatos$forma_saida")
chk(all(mand$mandato_inicio < mand$mandato_fim), "mandatos: inicio < fim")

## ------------------------------------------------------------ 4. recontagem independente das pontes
# identidade por candidatura, direto dos parquets (mesma limpeza do 03), ligada ao banco pelo id_mandato
ne <- function(x) fifelse(x %in% c("#NE","#NULO","#NULO#","-1","-3","-4",""), NA_character_, x)
num_only <- function(x) gsub("\\D", "", x)
cols <- c("ANO_ELEICAO","NM_TIPO_ELEICAO","SG_UF","SG_UE","CD_CARGO","SQ_CANDIDATO","NR_CANDIDATO",
          "NR_CPF_CANDIDATO","NR_TITULO_ELEITORAL_CANDIDATO","NM_CANDIDATO","DT_NASCIMENTO","DS_GENERO")
ident <- rbindlist(lapply(list.files("data_raw/parquet", "^cand_\\d{4}\\.parquet$", full.names = TRUE), function(f) {
  x <- setDT(read_parquet(f, col_select = all_of(cols)))[grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]
  x[, cd := as.integer(CD_CARGO)]
  x[, ue := fcase(cd %in% 11:13, SG_UE, cd %in% 1:2, "BR", default = SG_UF)]
  x[, .(chave = paste(as.integer(ANO_ELEICAO), ue, cd, NR_CANDIDATO, SQ_CANDIDATO, sep = "_"),
        titulo = ne(num_only(NR_TITULO_ELEITORAL_CANDIDATO)), cpf = ne(num_only(NR_CPF_CANDIDATO)),
        nome = ne(NM_CANDIDATO), dt = ne(DT_NASCIMENTO), gen = ne(DS_GENERO))]
}))
ident[nchar(titulo) < 4 | grepl("^0+$", titulo), titulo := NA_character_]
ident[nchar(cpf) != 11 | grepl("^0+$", cpf), cpf := NA_character_]
ident[!is.na(titulo), titulo := formatC(titulo, width = 12, flag = "0")]
ident <- unique(ident)
n_multi <- ident[, .N, by = chave][N > 1, .N]
chk(n_multi == 0, "identidade unica por chave de candidatura no parquet (sem fan-out no join)")
mand[, chave := sub("^M", "", id_mandato)]
el <- join_seguro(as.data.frame(mand[, .(id_mandato, id_pessoa, ano_eleicao, chave)]), as.data.frame(ident),
                  by = "chave", cardinalidade = "many-to-one", tipo = "left")
setDT(el)
# 12/09/2026: 481.345 -> 484.771 com a chapa dos vices fechada no R/03; constante trocada por essa correcao
# 13/09/2026: 484.772 com a cadeira de Selma Arruda restaurada
chk(nrow(el) == 484772L, "484.772 mandatos apos o join (sem inflacao)")
chk(el[is.na(titulo) & is.na(cpf) & is.na(nome), .N] == 0, "0 mandatos sem identidade")
# propagacao de rotulo dentro de cada id_pessoa: dois mandatos ficam na mesma componente
# quando compartilham titulo ou CPF (fecho transitivo por iteracao ate convergir)
el[, lab := .I]
repeat {
  old <- copy(el$lab)   # copy: `:=` modifica a coluna por referencia e `old` seria o mesmo vetor
  el[!is.na(titulo), lab := min(lab), by = .(id_pessoa, titulo)]
  el[!is.na(cpf),    lab := min(lab), by = .(id_pessoa, cpf)]
  if (identical(old, el$lab)) break
}
pp <- el[, .(n_comp = uniqueN(lab), n_tit = uniqueN(na.omit(titulo)), n_cpf = uniqueN(na.omit(cpf)),
             n_gen = uniqueN(na.omit(gen)), n_sem = sum(is.na(titulo) & is.na(cpf)), n_mand = .N,
             n_nasc = uniqueN(na.omit(dt))), by = id_pessoa]
pontes_ind <- pp[n_comp > 1, id_pessoa]
so_nn_ind  <- pp[n_sem == n_mand, id_pessoa]
chk(setequal(pontes_ind, aud[ponte_nome_nascimento == TRUE, id_pessoa]), "conjunto das pontes reproduzido por metodo independente")
chk(setequal(so_nn_ind, aud[so_nome_nascimento == TRUE, id_pessoa]), "conjunto so_nome_nascimento reproduzido por metodo independente")
cmp <- merge(aud, pp, by = "id_pessoa")
chk(all(cmp$n_comp_12 == cmp$n_comp & cmp$n_titulos_distintos == cmp$n_tit & cmp$n_cpfs_distintos == cmp$n_cpf &
        cmp$n_nasc_distintos == cmp$n_nasc), "n_comp_12, n_titulos, n_cpfs, n_nasc reproduzidos por metodo independente")
# componentes sem regra 3 no banco inteiro: soma das componentes por pessoa (as pessoas ja particionam os mandatos)
n_comp_12_ind <- pp[, sum(n_comp)]

## ------------------------------------------------------------ 5. recontagem dos numeros x assinatura
# 21/09/2026: as duas excecoes que o R/09 admite no fecho 1+2+3, a ponte de cadastro auditada no R/03 e a
# separacao de pessoas fundidas por documento, recontadas das listas em output/verificacao/. Cada candidatura
# da separacao tem de estar em mandatos.csv com o id_pessoa que a lista declara
pontes_ref <- fread("output/verificacao/pontes_nucleo_referencia.csv", colClasses = "character")
fusao_sep <- fread("output/verificacao/fusao_documentos_separacao.csv", colClasses = "character", na.strings = c("", "NA"))
fus_m <- merge(fusao_sep[, .(chave_cand, id_pessoa_esperado)], mand[, .(chave, id_pessoa)], by.x = "chave_cand", by.y = "chave")
chk(!anyDuplicated(fusao_sep$chave_cand) && nrow(fus_m) == nrow(fusao_sep) && all(fus_m$id_pessoa == fus_m$id_pessoa_esperado),
    sprintf("separacao por fusao de documentos: %d candidaturas em mandatos.csv com o id_pessoa declarado", nrow(fusao_sep)))
rec <- list(
  aud_n_componentes_excecao_ponte_referencia = uniqueN(pontes_ref$componente),
  aud_n_componentes_excecao_fusao_documentos = uniqueN(fusao_sep$comp_raiz),
  aud_n_candidaturas_excecao_fusao_documentos = nrow(fus_m),
  aud_n_pessoas = nrow(pess), aud_n_componentes_sem_regra3 = n_comp_12_ind,
  aud_n_pessoas_ponte = sum(flags$ponte_nome_nascimento), aud_n_pessoas_so_nome_nascimento = sum(flags$so_nome_nascimento),
  aud_n_auditadas = nrow(aud), aud_n_suspeitos = sum(aud$classificacao == "suspeito"),
  aud_n_consistentes = sum(aud$classificacao == "consistente"), aud_n_indeterminados = sum(aud$classificacao == "indeterminado"),
  aud_n_ponte_suspeitos = sum(aud$ponte_nome_nascimento & aud$classificacao == "suspeito"),
  aud_n_ponte_consistentes = sum(aud$ponte_nome_nascimento & aud$classificacao == "consistente"),
  aud_n_ponte_indeterminados = sum(aud$ponte_nome_nascimento & aud$classificacao == "indeterminado"),
  aud_n_suspeitos_titulo_conflito = sum(aud$classificacao == "suspeito" & aud$n_titulos_distintos > 1),
  aud_n_suspeitos_cpf_conflito = sum(aud$classificacao == "suspeito" & aud$n_cpfs_distintos > 1),
  aud_n_suspeitos_genero = sum(aud$classificacao == "suspeito" & grepl(";", aud$generos)),
  aud_n_suspeitos_mesmo_ano = sum(aj$classificacao == "suspeito" & aj$n_anos_rep > 0),
  aud_n_suspeitos_forte = sum(aud$grau_suspeita %in% "forte"), aud_n_suspeitos_fraca = sum(aud$grau_suspeita %in% "fraca"),
  aud_n_mandatos_em_pontes = sum(aud[ponte_nome_nascimento == TRUE, n_mandatos]),
  aud_n_mandatos_em_suspeitos = sum(aud[classificacao == "suspeito", n_mandatos]),
  aud_n_pessoas_se_regra_final = nrow(pess) + aud[ponte_nome_nascimento & classificacao != "consistente", sum(n_comp_12 - 1L)],
  aud_pct_pessoas_ponte = round(sum(flags$ponte_nome_nascimento) / nrow(pess), 5),
  aud_pct_pessoas_suspeitas = round(sum(aud$classificacao == "suspeito") / nrow(pess), 5)
)
ass <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, strip.white = TRUE, colClasses = "character")
setnames(ass, c("chave","valor","ep","data","checksum","out"))
ass <- ass[grepl("^aud_", chave)]
ult <- ass[, .SD[.N], by = chave]   # ultimo registro (append-only)
chk(uniqueN(ult$checksum) == 1L, "ultimos registros aud_ vem de um unico checksum de script")
comp <- data.table(chave = names(rec), recontado = as.character(unlist(rec)))
comp <- merge(comp, ult[, .(chave, assinado = valor)], by = "chave", all = TRUE)
comp[, bate := !is.na(assinado) & as.numeric(recontado) == as.numeric(assinado)]
print(comp)
chk(nrow(comp) == 25L && all(comp$bate), "25 numeros aud_ recontados dos arquivos batem com a assinatura")

## ------------------------------------------------------------ 6. diagnosticos que o construtor nao registrou
n_sonn_sem_nasc <- fl[fl$so_nome_nascimento & is.na(fl$dt_nascimento), ] |> nrow()
n_sonn_sem_nasc_aud <- aud[so_nome_nascimento == TRUE & n_nasc_distintos == 0, .N]
# titulo em conflito fora do universo auditado (componentes ligadas so por CPF, regra 2)
n_cpf_link_tit_conflito <- pp[n_comp == 1 & n_tit > 1, .N]
for (k in names(rec)) registrar_numero(paste0("verif_", k), rec[[k]], script = script, out = "output/numeros_assinatura.txt")
registrar_numero("verif_aud_n_so_nome_nascimento_sem_dt_nascimento", n_sonn_sem_nasc, script = script, out = "output/numeros_assinatura.txt")
registrar_numero("verif_aud_n_nao_auditadas_titulos_distintos_via_cpf", n_cpf_link_tit_conflito, script = script, out = "output/numeros_assinatura.txt")
logar("so_nome_nascimento sem dt_nascimento em pessoas.csv: ", n_sonn_sem_nasc, " (auditoria n_nasc_distintos==0: ", n_sonn_sem_nasc_aud, ")")
logar("pessoas NAO auditadas com titulos distintos ligadas so por CPF: ", n_cpf_link_tit_conflito)

gravar_relatorio_verificacao(alvo = "R/09_auditoria_homonimos.R -> data/auditoria_homonimos.csv; data/pessoas_flags_dedup.csv",
  script = script, passou = passou, falhou = falhas,
  fora_de_cobertura = c("homonimos com mesma data de nascimento sem contradicao interna",
                        "divisao indevida de pessoa (titulos distintos com grafias distintas)",
                        "conflito de titulo em componentes ligadas so por CPF (regra 2) nao e escopo da regra 3"))
logar("falhas: ", length(falhas), " | passou: ", length(passou))
if (length(falhas)) stop("verifica_homonimos: ", length(falhas), " falha(s): ", paste(falhas, collapse = " ; "))
cat("verifica_homonimos: OK\n")
