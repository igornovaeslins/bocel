# auditoria_incompatibilidade_leitura.R — evidencia para ler os casos em que a fonte diz fim regular e a pessoa assumiu
# cargo incompativel no meio do mandato (19/09/2026)
#
# A pendencia 4 do registro de pendencias separa duas causas possiveis para a contradicao listada pelo R/39.
# Ou a relacao da Casa continua imprimindo o nome de quem ja saiu, com o fim convencional da legislatura, ou o banco
# fundiu dois homonimos na mesma pessoa. Este script junta, para cada caso, os documentos do TSE das duas candidaturas
# (CPF, titulo, nascimento, nome), o registro da Casa quando ele vem do SAPL municipal (data de fim, observacao, fim da
# legislatura) e a posse no cargo novo, e separa o que a evidencia ja decide do que pede leitura. Nao corrige nada.
#
# Saidas: output/verificacao/incompatibilidade_contradiz_leitura.csv, chaves incl_*.
# Execucao: cd ~/bocel && Rscript --vanilla R/auditoria_incompatibilidade_leitura.R (depois do R/39)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/auditoria_incompatibilidade_leitura.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

ctr <- fread("output/verificacao/incompatibilidade_contradiz_fonte.csv", colClasses = "character")
inc <- fread("data/saida_cargo_incompativel.csv", colClasses = "character",
             select = c("id_mandato", "id_mandato_incompativel", "data_posse_incompativel"))
ctr <- merge(ctr, inc, by = "id_mandato", all.x = TRUE)
stopifnot(!anyNA(ctr$id_mandato_incompativel))
reg("incl_casos", nrow(ctr))

cols <- c("id_mandato", "ano_eleicao", "sq_candidato", "cd_cargo", "cargo", "sg_ue", "nm_ue", "sg_partido",
          "mandato_inicio", "mandato_fim", "data_posse", "data_fim_efetiva", "forma_saida", "fonte_forma_saida",
          "exercicio_confirmado", "fonte_exercicio")
m <- fread("data/mandatos.csv", colClasses = "character", select = cols, na.strings = c("", "NA"))
m <- m[id_mandato %chin% c(ctr$id_mandato, ctr$id_mandato_incompativel)]

## ---------------------------------------------------------------- documentos do TSE por candidatura
digitos <- function(x) gsub("[^0-9]", "", x)
cpf_ok <- function(x) { d <- digitos(x); fifelse(nchar(d) == 11L & !grepl("^0+$", d), d, NA_character_) }
tit_ok <- function(x) { d <- digitos(x); fifelse(nchar(d) >= 10L & !grepl("^0+$", d), d, NA_character_) }
# Nos anos antigos o SQ_CANDIDATO se repete entre unidades eleitorais, e a chave da candidatura e unidade, cargo e
# sequencial. A primeira versao deste script casava so pelo sequencial e trazia documento de outra pessoa.
doc <- rbindlist(lapply(sort(unique(m$ano_eleicao)), function(a) {
  f <- sprintf("data_raw/parquet/cand_%s.parquet", a)
  sq <- unique(m[ano_eleicao == a, sq_candidato])
  x <- as.data.table(open_dataset(f) |> dplyr::filter(SQ_CANDIDATO %in% sq) |>
                       dplyr::select(SG_UE, CD_CARGO, SQ_CANDIDATO, NM_CANDIDATO, NR_CPF_CANDIDATO,
                                     NR_TITULO_ELEITORAL_CANDIDATO, DT_NASCIMENTO) |> dplyr::collect())
  x[, ano_eleicao := a]
  x[, lapply(.SD, as.character)]
}))
doc[, `:=`(sg_ue = SG_UE, cd_cargo = CD_CARGO, sq_candidato = SQ_CANDIDATO, cpf = cpf_ok(NR_CPF_CANDIDATO),
           titulo = tit_ok(NR_TITULO_ELEITORAL_CANDIDATO), nasc = trimws(DT_NASCIMENTO), nome = toupper(trimws(NM_CANDIDATO)))]
chave <- c("ano_eleicao", "sg_ue", "cd_cargo", "sq_candidato")
# a mesma candidatura aparece uma vez por turno; os documentos nao mudam entre turnos
doc <- unique(doc[, c(chave, "nome", "cpf", "titulo", "nasc"), with = FALSE])
stopifnot("candidatura com documentos diferentes entre turnos" = !anyDuplicated(doc, by = chave))
m[, ano_eleicao := as.character(ano_eleicao)]
m <- merge(m, doc, by = chave, all.x = TRUE)
reg("incl_mandatos_sem_candidatura_no_tse", m[is.na(nome), .N])

a <- m[match(ctr$id_mandato, id_mandato)]
b <- m[match(ctr$id_mandato_incompativel, id_mandato)]
ev <- data.table(ctr[, .(id_mandato, id_pessoa, sg_uf, fonte_atual, data_fim_atual)],
                 cargo_a = a$cargo, municipio_a = a$nm_ue, ano_a = a$ano_eleicao, nome_a = a$nome, nasc_a = a$nasc,
                 cargo_b = b$cargo, municipio_b = b$nm_ue, ano_b = b$ano_eleicao, nome_b = b$nome, nasc_b = b$nasc,
                 id_mandato_b = ctr$id_mandato_incompativel, posse_b = ctr$data_posse_incompativel,
                 data_posse_b = b$data_posse, exercicio_b = b$exercicio_confirmado, fonte_exercicio_b = b$fonte_exercicio,
                 forma_b = b$forma_saida,
                 mesmo_cpf = fifelse(is.na(a$cpf) | is.na(b$cpf), NA, a$cpf == b$cpf),
                 mesmo_titulo = fifelse(is.na(a$titulo) | is.na(b$titulo), NA, a$titulo == b$titulo),
                 mesmo_nasc = fifelse(is.na(a$nasc) | is.na(b$nasc) | !nzchar(a$nasc) | !nzchar(b$nasc), NA, a$nasc == b$nasc),
                 mesmo_nome = a$nome == b$nome)

## ---------------------------------------------------------------- registro da Casa no SAPL municipal
sp <- fread("data/exercicio_camaras_municipais.csv", colClasses = "character", na.strings = c("", "NA"),
            select = c("id_mandato_bocel", "nome_parlamentar", "titular", "legislatura_fim", "data_inicio_mandato",
                       "data_fim_mandato", "tipo_afastamento", "forma_saida", "observacao", "url"))
sp <- sp[id_mandato_bocel %chin% ev$id_mandato]
sp1 <- sp[, .(sapl_registros = .N, sapl_titular = paste(unique(titular), collapse = "|"),
              sapl_fim = paste(unique(data_fim_mandato), collapse = "|"), sapl_legislatura_fim = legislatura_fim[1],
              sapl_afastamento = paste(unique(na.omit(tipo_afastamento)), collapse = "|"),
              sapl_observacao = paste(unique(na.omit(gsub("[[:space:]]+", " ", observacao))), collapse = " || "),
              sapl_url = url[1]), by = .(id_mandato = id_mandato_bocel)]
ev <- merge(ev, sp1, by = "id_mandato", all.x = TRUE)

## ---------------------------------------------------------------- o que a evidencia ja decide
# Documento divergente aponta homonimo fundido, e documento igual confirma a mesma pessoa, caso em que a contradicao e
# da fonte da Casa, que imprime o fim convencional. O CPF decide antes do titulo, porque o cadastro do TSE ja trouxe o
# titulo de uma pessoa na candidatura de outra (Recife 2004, Marau 2008, em auditoria_fusao_documentos.R). Sem documento
# comum, nome e nascimento iguais sustentam a mesma pessoa com menos forca. O resto vai para leitura.
ev[, identidade := fcase(mesmo_cpf %in% TRUE, "documento_igual",
                         mesmo_cpf %in% FALSE, "documento_diverge",
                         mesmo_titulo %in% TRUE, "documento_igual",
                         mesmo_titulo %in% FALSE, "documento_diverge",
                         mesmo_nome %in% TRUE & mesmo_nasc %in% TRUE, "nome_e_nascimento_iguais",
                         default = "sem_prova_de_identidade")]
reg("incl_cpf_igual_titulo_diverge", ev[mesmo_cpf %in% TRUE & mesmo_titulo %in% FALSE, .N])
ev[, fim_da_casa_convencional := fifelse(is.na(sapl_fim), NA,
                                         sapl_fim == sapl_legislatura_fim | sapl_fim == data_fim_atual)]
ev[, leitura := fcase(identidade == "documento_diverge", "homonimo_provavel",
                      identidade %chin% c("documento_igual", "nome_e_nascimento_iguais") & !is.na(data_posse_b),
                        "mesma_pessoa_casa_imprime_fim_convencional",
                      identidade %chin% c("documento_igual", "nome_e_nascimento_iguais"), "mesma_pessoa_posse_no_cargo_novo_sem_data",
                      default = "ler")]
setcolorder(ev, c("leitura", "identidade"))
setorder(ev, leitura, sg_uf, ano_a)
fwrite(ev, "output/verificacao/incompatibilidade_contradiz_leitura.csv", na = "NA", quote = TRUE)
stopifnot(nrow(ev) == nrow(ctr), !anyDuplicated(ev$id_mandato))
for (k in unique(ev$leitura)) reg(paste0("incl_", k), ev[leitura == k, .N])
for (k in unique(ev$identidade)) reg(paste0("incl_identidade_", k), ev[identidade == k, .N])
reg("incl_sapl_fim_convencional", ev[fim_da_casa_convencional %in% TRUE, .N])
print(ev[, .N, by = .(leitura, identidade, fonte_atual)][order(leitura, -N)])
