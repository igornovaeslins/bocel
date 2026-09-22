# verifica_suplementares.R — verificacao cetica da frente 08 (eleicoes suplementares)
# Execucao: cd ~/bocel && Rscript --vanilla R/verifica_suplementares.R
# Le SOMENTE os arquivos de saida do 08 (mais mandatos.csv/pessoas.csv e os parquet para
# reconferir votos) e reconta os numeros contra output/numeros_assinatura.txt.
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table); library(arrow) })
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
script <- "R/verifica_suplementares.R"
reg <- function(chave, valor) registrar_numero(chave, valor, script = script, out = "output/numeros_assinatura.txt")

passou <- character(); falhou <- character()
ok <- function(nome, expr) {
  r <- tryCatch({ force(expr); TRUE }, error = function(e) { message("FALHA: ", nome, " — ", conditionMessage(e)); FALSE })
  if (r) passou <<- c(passou, nome) else falhou <<- c(falhou, nome)
  invisible(r)
}

## ------------------------------------------------------------ carga
pl  <- fread("data/eleicoes_suplementares.csv", colClasses = "character", na.strings = "NA")
fs  <- fread("data/mandatos_forma_saida_suplementar.csv", colClasses = "character", na.strings = "NA")
ver <- fread("data/eleicoes_suplementares_vereador.csv", colClasses = "character", na.strings = "NA")
mand <- fread("data/mandatos.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data/pessoas.csv", colClasses = "character", na.strings = "NA")

## ------------------------------------------------------------ 1. estrutura
cols_pl <- c("id_pleito","ano_arquivo","unidade_posicao","sg_uf","sg_ue","nm_ue","cd_cargo","cargo","esfera",
  "dt_eleicao_suplementar","dt_turno_decisivo","nr_turno","n_candidatos","status_vencedor","vencedor_nome",
  "vencedor_titulo","vencedor_cpf","vencedor_id_pessoa","votos_vencedor","sq_candidato","nr_candidato",
  "sg_partido_vencedor","situacao_totalizacao","fonte_situacao","vice_nome","vice_titulo","vice_cpf",
  "vice_id_pessoa","id_mandato_ordinario_afetado","ocupante_ordinario_id_pessoa","dt_dentro_mandato_ordinario",
  "momento","vencedor_e_o_ocupante_ordinario","mandatos_ordinarios_candidatos","realizado_ate_data_do_arquivo",
  "chave_colide_com_ordinaria","vencedor_chave_colide_com_ordinaria","fonte")
cols_fs <- c("id_mandato_ordinario_afetado","forma_saida","momento","data_fim_inferida","id_pleito_suplementar",
  "sucessor_via_suplementar","sucessor_nome","sucessor_titulo","via_sucessao","id_pleito_sucessor",
  "dt_pleito_sucessor","status_vencedor","n_pleitos_suplementares","vencedor_e_o_ocupante_ordinario","fonte")
cols_ver <- c("id_pleito","ano_arquivo","unidade_posicao","sg_uf","sg_ue","nm_ue","cd_cargo","cargo","esfera",
  "dt_eleicao_suplementar","nr_turno","eleito_id_pessoa","eleito_cpf","eleito_titulo","eleito_nome","votos",
  "sq_candidato","nr_candidato","sg_partido","situacao_totalizacao","fonte_situacao","fonte",
  "n_mandatos_ordinarios_vereador_na_unidade","id_mandato_ordinario_mesma_pessoa")
ok("pleitos: colunas prometidas", stopifnot(identical(names(pl), cols_pl)))
ok("forma_saida: colunas prometidas", stopifnot(identical(names(fs), cols_fs)))
ok("vereador: colunas prometidas", stopifnot(identical(names(ver), cols_ver)))
ok("nomes de coluna minusculos sem acento",
   stopifnot(all(grepl("^[a-z0-9_]+$", c(names(pl), names(fs), names(ver))))))
# codigo de ausente: nenhuma celula vazia ou "NULL"/"#NULO" (o NA ja foi lido como NA)
raw_pl <- fread("data/eleicoes_suplementares.csv", colClasses = "character", na.strings = NULL)
ok("pleitos: ausente so como 'NA' (sem celula vazia)",
   stopifnot(sum(sapply(raw_pl, function(x) sum(x == "" | grepl("^#NULO|^NULL$", x)))) == 0))
raw_fs <- fread("data/mandatos_forma_saida_suplementar.csv", colClasses = "character", na.strings = NULL)
ok("forma_saida: ausente so como 'NA'", stopifnot(sum(sapply(raw_fs, function(x) sum(x == ""))) == 0))
raw_ver <- fread("data/eleicoes_suplementares_vereador.csv", colClasses = "character", na.strings = NULL)
ok("vereador: ausente so como 'NA'", stopifnot(sum(sapply(raw_ver, function(x) sum(x == ""))) == 0))

ok("pleitos: id_pleito unico", checa_unica(as.data.frame(pl), "id_pleito"))
ok("pleitos: (ano_arquivo, unidade_posicao, cd_cargo, dt_eleicao_suplementar) unico",
   checa_unica(as.data.frame(pl), c("ano_arquivo","unidade_posicao","cd_cargo","dt_eleicao_suplementar")))
ok("forma_saida: id_mandato unico", checa_unica(as.data.frame(fs), "id_mandato_ordinario_afetado"))
ok("vereador: (id_pleito, sq_candidato) unico", checa_unica(as.data.frame(ver), c("id_pleito","sq_candidato")))
ok("vereador: (id_pleito, eleito_id_pessoa) unico entre identificados",
   checa_unica(as.data.frame(ver[!is.na(eleito_id_pessoa)]), c("id_pleito","eleito_id_pessoa")))
ok("id_pleito reconstrutivel a partir das colunas",
   stopifnot(all(pl$id_pleito == paste0("S", pl$ano_arquivo, "_", pl$unidade_posicao, "_", pl$cd_cargo, "_",
                                        gsub("-", "", pl$dt_eleicao_suplementar)))))

## ------------------------------------------------------------ 2. vocabularios e faixas
ok("cd_cargo majoritario", in_set(pl$cd_cargo, c("1","3","5","11"), nome = "cd_cargo"))
ok("esfera", in_set(pl$esfera, c("federal","estadual","municipal"), nome = "esfera"))
ok("status_vencedor", in_set(pl$status_vencedor,
   c("vencedor_identificado","pleito_posterior_a_data_do_arquivo","sem_vencedor_marcado_no_tse"), nome = "status_vencedor"))
ok("momento", in_set(pl$momento, c("antes_da_posse","durante_o_mandato","apos_o_fim_do_mandato"), nome = "momento"))
ok("fonte_situacao", in_set(pl$fonte_situacao, c("cadastro","votacao","imputacao_votos"), nome = "fonte_situacao"))
ok("forma_saida: vocabulario fechado", in_set(fs$forma_saida,
   c("perda_do_mandato_inferida_por_eleicao_suplementar",
     "resultado_ordinario_substituido_por_eleicao_suplementar_antes_da_posse"), permitir_na = FALSE, nome = "forma_saida"))
ok("forma_saida: via_sucessao", in_set(fs$via_sucessao, "eleicao_suplementar", permitir_na = FALSE, nome = "via_sucessao"))
ok("forma_saida: momento coerente com forma_saida",
   stopifnot(all((fs$momento == "antes_da_posse") ==
                 (fs$forma_saida == "resultado_ordinario_substituido_por_eleicao_suplementar_antes_da_posse"))))
ok("ano_arquivo em [1998,2024]", em_faixa(as.integer(pl$ano_arquivo), 1998, 2024, permitir_na = FALSE, nome = "ano_arquivo"))
ok("ano_arquivo so em anos eleitorais pares", stopifnot(all(as.integer(pl$ano_arquivo) %% 2 == 0)))
d1 <- as.IDate(pl$dt_eleicao_suplementar); d2 <- as.IDate(pl$dt_turno_decisivo)
ok("datas ISO validas", stopifnot(!anyNA(d1), !anyNA(d2)))
ok("ano do pleito em [1998,2026]", em_faixa(year(d1), 1998, 2026, permitir_na = FALSE, nome = "ano_pleito"))
ok("turno decisivo >= 1o turno", stopifnot(all(d2 >= d1)))
ok("2 turnos <=> data decisiva > 1o turno", stopifnot(all((pl$nr_turno == "2") == (d2 > d1))))
ok("pleito ocorre depois da eleicao ordinaria (out do ano do arquivo)",
   stopifnot(all(d1 > as.IDate(paste0(pl$ano_arquivo, "-10-01")))))
ok("pleito no maximo 4 anos (municipal/estadual) ou 8 (senador) apos o arquivo",
   stopifnot(all(year(d1) - as.integer(pl$ano_arquivo) <= fifelse(pl$cd_cargo == "5", 8L, 4L))))
ok("realizado <=> data <= 2026-08-28",
   stopifnot(all((pl$realizado_ate_data_do_arquivo == "TRUE") == (d1 <= as.IDate("2026-08-28")))))
ok("n_candidatos >= 1", em_faixa(as.integer(pl$n_candidatos), 1, 50, permitir_na = FALSE, nome = "n_candidatos"))
ok("votos_vencedor em faixa", em_faixa(as.numeric(pl$votos_vencedor), 1, 5e6, nome = "votos_vencedor"))
# desde que o 02 separa NM_TIPO_ELEICAO na chave, a colisao de SQ com ordinaria nao contamina
# os votos: o vencedor com chave colidente TEM votos, e eles sao reconferidos na secao 4 contra
# as linhas do parquet rotuladas como suplementar
ok("chave do vencedor colide => votos presentes (02 separa o tipo de eleicao)",
   stopifnot(all(!is.na(pl[vencedor_chave_colide_com_ordinaria == "TRUE", votos_vencedor]))))
ok("vencedor colide => pleito colide", stopifnot(all(pl[vencedor_chave_colide_com_ordinaria == "TRUE", chave_colide_com_ordinaria == "TRUE"])))
# vencedores sem votos: registrar; depois da ligacao por turno divergente (30/10/2022) devem ser 0
sv <- pl[status_vencedor == "vencedor_identificado" & is.na(votos_vencedor)]
reg("verif_sup_n_vencedores_sem_votos", nrow(sv))
reg("verif_sup_datas_vencedores_sem_votos", paste(sort(unique(sv$dt_eleicao_suplementar)), collapse = ";"))
ok("todo vencedor identificado tem votos", stopifnot(nrow(sv) == 0))
ok("vencedor_nome presente <=> status vencedor_identificado",
   stopifnot(all(!is.na(pl$vencedor_nome) == (pl$status_vencedor == "vencedor_identificado"))))
ok("vencedor identificado tem situacao de eleito",
   in_set(pl[status_vencedor == "vencedor_identificado", situacao_totalizacao],
          c("ELEITO","ELEITO (IMPUTADO POR VOTOS)"), permitir_na = FALSE, nome = "situacao_totalizacao"))
ok("pleito futuro sem vencedor", stopifnot(pl[realizado_ate_data_do_arquivo == "FALSE" & !is.na(vencedor_nome), .N] == 0))
ok("vice so quando ha vencedor", stopifnot(pl[is.na(vencedor_nome) & !is.na(vice_nome), .N] == 0))
ok("vice nao e o proprio vencedor",
   stopifnot(pl[!is.na(vice_id_pessoa) & !is.na(vencedor_id_pessoa) & vice_id_pessoa == vencedor_id_pessoa, .N] == 0))

## ------------------------------------------------------------ 3. integridade referencial e pareamentos
ok("vencedor_id_pessoa existe em pessoas", stopifnot(all(na.omit(pl$vencedor_id_pessoa) %in% pess$id_pessoa)))
ok("vice_id_pessoa existe em pessoas", stopifnot(all(na.omit(pl$vice_id_pessoa) %in% pess$id_pessoa)))
ok("eleito_id_pessoa (vereador) existe em pessoas", stopifnot(all(na.omit(ver$eleito_id_pessoa) %in% pess$id_pessoa)))
ok("id_mandato afetado existe em mandatos", stopifnot(all(na.omit(pl$id_mandato_ordinario_afetado) %in% mand$id_mandato)))
ok("forma_saida: id_mandato existe em mandatos", stopifnot(all(fs$id_mandato_ordinario_afetado %in% mand$id_mandato)))
ok("forma_saida: id_pleito existe em pleitos", stopifnot(all(fs$id_pleito_suplementar %in% pl$id_pleito),
                                                            all(fs$id_pleito_sucessor %in% pl$id_pleito)))
ok("forma_saida: sucessor e o vencedor do id_pleito_sucessor",
   stopifnot(all(merge(fs, pl[, .(id_pleito_sucessor = id_pleito, vn = vencedor_nome, vi = vencedor_id_pessoa, sv = status_vencedor)],
                       by = "id_pleito_sucessor")[, identical(vn, sucessor_nome) & identical(vi, sucessor_via_suplementar) & identical(sv, status_vencedor)])))
ok("forma_saida: sucessor presente sempre que algum pleito do mandato teve vencedor",
   stopifnot(all(merge(fs, pl[!is.na(id_mandato_ordinario_afetado), .(alg = any(status_vencedor == "vencedor_identificado")),
                              by = id_mandato_ordinario_afetado], by = "id_mandato_ordinario_afetado")[, alg == !is.na(sucessor_nome)])))
ok("vereador: id_mandato_ordinario_mesma_pessoa existe em mandatos",
   stopifnot(all(na.omit(ver$id_mandato_ordinario_mesma_pessoa) %in% mand$id_mandato)))

# pareamento vencedor -> pessoas refeito de forma independente, por titulo e por CPF. O documento que a
# separacao de pessoas fundidas (R/03) deixou em mais de um id_pessoa nao serve de ponte, e so pode
# aparecer repetido entre as pessoas que essa separacao partiu
sep_fusao <- fread("output/verificacao/fusao_documentos_separacao.csv", colClasses = "character", na.strings = c("", "NA"))
doc_rep <- function(col) pess[!is.na(get(col)), .(n = .N, ids = list(id_pessoa)), by = col][n > 1L]
tit_rep <- doc_rep("nr_titulo_eleitoral"); cpf_rep <- doc_rep("nr_cpf")
ok("titulo ou CPF repetido em pessoas so entre as pessoas separadas por documento",
   stopifnot(all(unlist(tit_rep$ids) %in% sep_fusao$id_pessoa_esperado),
             all(unlist(cpf_rep$ids) %in% sep_fusao$id_pessoa_esperado)))
reg("verif_sup_titulos_repetidos_por_separacao_de_fusao", nrow(tit_rep))
reg("verif_sup_cpfs_repetidos_por_separacao_de_fusao", nrow(cpf_rep))
p_tit <- pess[!is.na(nr_titulo_eleitoral) & !nr_titulo_eleitoral %in% tit_rep$nr_titulo_eleitoral,
              .(nr_titulo_eleitoral, id_t = id_pessoa)]
p_cpf <- pess[!is.na(nr_cpf) & !nr_cpf %in% cpf_rep$nr_cpf, .(nr_cpf, id_c = id_pessoa)]
chk <- as.data.frame(pl[, .(id_pleito, vencedor_titulo, vencedor_cpf, vencedor_id_pessoa)])
chk <- join_seguro(chk, as.data.frame(p_tit), by = c("vencedor_titulo" = "nr_titulo_eleitoral"), cardinalidade = "many-to-one")
chk <- join_seguro(chk, as.data.frame(p_cpf), by = c("vencedor_cpf" = "nr_cpf"), cardinalidade = "many-to-one")
chk$id_ind <- ifelse(!is.na(chk$id_t), chk$id_t, chk$id_c)
ok("vencedor_id_pessoa reproduz o pareamento titulo/CPF",
   stopifnot(identical(chk$id_ind, chk$vencedor_id_pessoa)))
ok("titulo e CPF nao discordam quando ambos pareiam",
   stopifnot(sum(!is.na(chk$id_t) & !is.na(chk$id_c) & chk$id_t != chk$id_c) == 0))

# mandato afetado refeito por join declarado many-to-one
mo <- as.data.frame(mand[cd_cargo %in% c("1","3","11"),
                         .(ano_arquivo = ano_eleicao, unidade_posicao, cd_cargo, id_m = id_mandato,
                           occ = id_pessoa, ini = mandato_inicio, fim = mandato_fim)])
chk2 <- join_seguro(as.data.frame(pl[cd_cargo != "5"]), mo,
                    by = c("ano_arquivo","unidade_posicao","cd_cargo"), cardinalidade = "many-to-one")
ok("id_mandato_ordinario_afetado reproduz o join (prefeito/governador)",
   stopifnot(identical(chk2$id_m, chk2$id_mandato_ordinario_afetado),
             identical(chk2$occ, chk2$ocupante_ordinario_id_pessoa)))
chk2 <- as.data.table(chk2)
ok("momento coerente com mandato_inicio/mandato_fim de mandatos.csv",
   stopifnot(all(chk2[!is.na(ini), fcase(as.IDate(dt_eleicao_suplementar) < as.IDate(ini), "antes_da_posse",
                                          as.IDate(dt_eleicao_suplementar) > as.IDate(fim), "apos_o_fim_do_mandato",
                                          default = "durante_o_mandato") == momento])))
ok("mandatos ordinarios afetados: um por (ano, unidade, cargo) no BOCEL",
   checa_unica(mo, c("ano_arquivo","unidade_posicao","cd_cargo")))
# senador: candidatos listados existem e sao da mesma UF/ano
sen <- pl[cd_cargo == "5"]
sen_ids <- unlist(strsplit(sen$mandatos_ordinarios_candidatos, ";"))
ok("senador: mandatos candidatos existem em mandatos", stopifnot(all(sen_ids %in% mand$id_mandato)))
ok("senador: mandato afetado NA (ambiguidade declarada)", stopifnot(all(is.na(sen$id_mandato_ordinario_afetado))))

# forma_saida reconstruida a partir dos pleitos
fs2 <- pl[!is.na(id_mandato_ordinario_afetado) & realizado_ate_data_do_arquivo == "TRUE"]
setorder(fs2, id_mandato_ordinario_afetado, dt_eleicao_suplementar)
fs2 <- fs2[, { iv <- if (any(status_vencedor == "vencedor_identificado")) which(status_vencedor == "vencedor_identificado")[1] else 1L
  .(data_fim_inferida = dt_eleicao_suplementar[1], id_pleito_suplementar = id_pleito[1],
    sucessor = vencedor_id_pessoa[iv], n = .N, momento = momento[1]) }, by = id_mandato_ordinario_afetado]
cmp <- merge(fs, fs2, by = "id_mandato_ordinario_afetado", all = TRUE)
ok("forma_saida reproduzida a partir de pleitos (mesmos mandatos)", stopifnot(nrow(cmp) == nrow(fs), nrow(cmp) == nrow(fs2)))
ok("forma_saida: data_fim_inferida, pleito, sucessor e n reproduzidos",
   stopifnot(identical(cmp$data_fim_inferida.x, cmp$data_fim_inferida.y),
             identical(cmp$id_pleito_suplementar.x, cmp$id_pleito_suplementar.y),
             identical(cmp$sucessor_via_suplementar, cmp$sucessor),
             identical(as.integer(cmp$n_pleitos_suplementares), cmp$n),
             identical(cmp$momento.x, cmp$momento.y)))
# data_fim_inferida dentro do mandato ordinario (ou antes da posse)
fm <- merge(fs, mand[, .(id_mandato_ordinario_afetado = id_mandato, ini = mandato_inicio, fim = mandato_fim)],
            by = "id_mandato_ordinario_afetado")
ok("forma_saida: data_fim_inferida <= mandato_fim", stopifnot(all(as.IDate(fm$data_fim_inferida) <= as.IDate(fm$fim))))
ok("forma_saida: perda inferida => data >= mandato_inicio",
   stopifnot(all(fm[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar",
                    as.IDate(data_fim_inferida) >= as.IDate(ini)])))
ok("forma_saida: antes_da_posse => data < mandato_inicio",
   stopifnot(all(fm[momento == "antes_da_posse", as.IDate(data_fim_inferida) < as.IDate(ini)])))
# mandatos com 2 pleitos: intervalo entre pleitos (pleito repetido em semanas seria split espurio)
dup <- pl[!is.na(id_mandato_ordinario_afetado)][, if (.N > 1) .(gap = as.numeric(diff(range(as.IDate(dt_eleicao_suplementar))))),
                                                by = id_mandato_ordinario_afetado]
# intervalos curtos (21-63 dias) sao adiamentos em que o TSE manteve as candidaturas na data
# original sem eleito; exige-se que nesses casos o primeiro pleito nao tenha vencedor
curtos <- dup[gap < 90, id_mandato_ordinario_afetado]
ok("mandatos com 2 pleitos em < 90 dias: o primeiro e adiamento (sem vencedor)",
   stopifnot(all(pl[id_mandato_ordinario_afetado %in% curtos][order(dt_eleicao_suplementar),
                    status_vencedor[1] != "vencedor_identificado", by = id_mandato_ordinario_afetado]$V1)))
reg("verif_sup_n_mandatos_2_pleitos_intervalo_menor_90d", length(curtos))
reg("verif_sup_min_gap_dias_entre_pleitos_mesmo_mandato", if (nrow(dup)) min(dup$gap) else NA)

# vereador: id_mandato_ordinario_mesma_pessoa so quando eleito_id_pessoa nao e NA e o mandato e da pessoa
vm <- merge(ver[!is.na(id_mandato_ordinario_mesma_pessoa)],
            mand[, .(id_mandato_ordinario_mesma_pessoa = id_mandato, pm = id_pessoa, ua = unidade_posicao, aa = ano_eleicao, cc = cd_cargo)],
            by = "id_mandato_ordinario_mesma_pessoa")
ok("vereador: mandato ordinario da mesma pessoa e da mesma pessoa/camara/ano",
   stopifnot(all(vm$pm == vm$eleito_id_pessoa), all(vm$ua == vm$unidade_posicao), all(vm$aa == vm$ano_arquivo), all(vm$cc == "13")))
ok("vereador: sem id_pessoa => sem mandato ordinario atribuido",
   stopifnot(ver[is.na(eleito_id_pessoa) & !is.na(id_mandato_ordinario_mesma_pessoa), .N] == 0))
ok("vereador: n_mandatos_ordinarios reproduz mandatos.csv",
   stopifnot(all(merge(ver, mand[cd_cargo == "13", .(n = .N), by = .(ano_arquivo = ano_eleicao, unidade_posicao)],
                       by = c("ano_arquivo","unidade_posicao"))[, as.integer(n_mandatos_ordinarios_vereador_na_unidade) == n])))
ok("vereador: situacao eleito", in_set(ver$situacao_totalizacao,
   c("ELEITO","ELEITO POR QP","ELEITO POR MEDIA","ELEITO POR MÉDIA"), permitir_na = FALSE, nome = "sit_ver"))

## ------------------------------------------------------------ 4. reconferencia contra a fonte bruta (parquet)
pq <- "data_raw/parquet"
cand <- rbindlist(lapply(list.files(pq, "^cand_\\d{4}\\.parquet$", full.names = TRUE), function(f)
  setDT(read_parquet(f))[!grepl("ORDIN", toupper(NM_TIPO_ELEICAO))]), use.names = TRUE)
ok("bruto: 4124 candidaturas suplementares nos parquet", stopifnot(nrow(cand) == 4124))
ok("bruto: anos com suplementar", stopifnot(identical(sort(unique(as.integer(cand$ANO_ELEICAO))),
                                                      c(2004L,2008L,2012L,2014L,2016L,2018L,2020L,2022L,2024L))))
cand[, cc := as.integer(CD_CARGO)]
cand[, ue := fifelse(cc %in% 11:13, SG_UE, SG_UF)]
cand[, cargo_t := fcase(cc == 2L, 1L, cc == 4L, 3L, cc %in% 9:10, 5L, cc == 12L, 11L, default = cc)]
cand[, d := as.IDate(DT_ELEICAO, format = "%d/%m/%Y")]
# pleitos brutos = (ano, unidade, cargo titular, data) no 1o turno, cargos majoritarios
brut <- unique(cand[NR_TURNO == "1" & cargo_t %in% c(1L,3L,5L,11L), .(ANO_ELEICAO, ue, cargo_t, d)])
ok("bruto: n de pleitos majoritarios (1o turno) = 641", stopifnot(nrow(brut) == nrow(pl)))
# eleitos no cadastro por pleito (titular) — nunca mais de um, exceto substituicao declarada
el <- cand[cc %in% c(1L,3L,5L,11L) & toupper(DS_SIT_TOT_TURNO) == "ELEITO",
           .(n = .N, sub = sum(ST_SUBSTITUIDO == "S")), by = .(ANO_ELEICAO, ue, cargo_t, NR_TURNO)]
reg("verif_sup_n_pleitos_turno_com_mais_de_um_eleito_no_cadastro", el[n > 1, .N])
# vencedor do arquivo bate com o cadastro (SQ e nome) para todos os identificados pelo cadastro
vc <- merge(pl[fonte_situacao == "cadastro", .(id_pleito, ano_arquivo, unidade_posicao, cd_cargo, nr_candidato, sq_candidato, vencedor_nome)],
            cand[, .(ano_arquivo = ANO_ELEICAO, unidade_posicao = ue, cd_cargo = as.character(cc), nr_candidato = NR_CANDIDATO,
                     sq_candidato = SQ_CANDIDATO, nm = NM_CANDIDATO, sit = toupper(DS_SIT_TOT_TURNO))],
            by = c("ano_arquivo","unidade_posicao","cd_cargo","nr_candidato","sq_candidato"))
ok("vencedor (cadastro): SQ existe no bruto, mesmo nome e marcado ELEITO em algum turno",
   stopifnot(nrow(vc) >= pl[fonte_situacao == "cadastro", .N],
             all(vc$nm == vc$vencedor_nome), all(vc[, any(sit == "ELEITO"), by = id_pleito]$V1)))
# votos do vencedor reconferidos no parquet de votos para 3 casos conhecidos e para uma amostra
votos <- rbindlist(lapply(list.files(pq, "^votos_\\d{4}\\.parquet$", full.names = TRUE), read_parquet))
ok("parquet de votos traz NM_TIPO_ELEICAO (02 separa o tipo)", stopifnot("NM_TIPO_ELEICAO" %in% names(votos)))
votos[, ue := fifelse(as.integer(CD_CARGO) %in% 11:13, SG_UE, SG_UF)]
votos[, chave := paste(ANO_ELEICAO, ue, as.integer(CD_CARGO), NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
# chaves das candidaturas ORDINARIAS no cadastro bruto (para saber que linha da votacao e ordinaria)
cand_ord <- rbindlist(lapply(list.files(pq, "^cand_\\d{4}\\.parquet$", full.names = TRUE), function(f)
  setDT(read_parquet(f))[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)),
    .(chave = paste(ANO_ELEICAO, fifelse(as.integer(CD_CARGO) %in% 11:13, SG_UE, SG_UF), as.integer(CD_CARGO), NR_CANDIDATO, SQ_CANDIDATO, sep = "_"))]))
# linhas de votacao que pertencem a pleito suplementar: rotulo suplementar/extraordinaria, ou chave sem ordinaria
vs <- votos[grepl("SUPLEMENTAR|EXTRAORDIN", toupper(NM_TIPO_ELEICAO)) | !chave %in% cand_ord$chave]
vv <- vs[, .(v = sum(votos), n_turnos_votacao = uniqueN(NR_TURNO)), by = .(chave, nr_turno = NR_TURNO)]
vv[, n_turnos_votacao := .N, by = chave]
am <- pl[!is.na(votos_vencedor)]
am[, chave := paste(ano_arquivo, unidade_posicao, cd_cargo, nr_candidato, sq_candidato, sep = "_")]
am1 <- merge(am, vv, by = c("chave", "nr_turno"))
# turno divergente (votacao rotula 30/10/2022 como turno 2): a chave so tem um turno na votacao
am2 <- merge(am[!id_pleito %in% am1$id_pleito], vv[n_turnos_votacao == 1L], by = "chave")
reg("verif_sup_n_vencedores_votos_por_turno_divergente", nrow(am2))
reg("verif_sup_datas_vencedores_votos_por_turno_divergente", paste(sort(unique(am2$dt_eleicao_suplementar)), collapse = ";"))
ok("turno divergente so no pleito de 30/10/2022 e so com nr_turno 1 no cadastro",
   stopifnot(all(am2$dt_eleicao_suplementar == "2022-10-30"), all(am2$nr_turno == "1"), all(am2$nr_turno.y == "2")))
am <- rbind(am1[, .(id_pleito, votos_vencedor, v)], am2[, .(id_pleito, votos_vencedor, v)])
ok("todos os vencedores com votos: votos_vencedor = soma das linhas SUPLEMENTARES do parquet de votos",
   stopifnot(nrow(am) == pl[!is.na(votos_vencedor), .N], uniqueN(am$id_pleito) == nrow(am),
             all(as.numeric(am$votos_vencedor) == am$v)))
# os 2 vencedores com chave colidente: o total ordinario da mesma chave e DIFERENTE do suplementar
colv <- pl[vencedor_chave_colide_com_ordinaria == "TRUE"]
colv[, chave := paste(ano_arquivo, unidade_posicao, cd_cargo, nr_candidato, sq_candidato, sep = "_")]
vo <- votos[grepl("ORDIN", toupper(NM_TIPO_ELEICAO)) & chave %in% colv$chave, .(v_ord = sum(votos)), by = chave]
colv <- merge(colv, vo, by = "chave")
ok("vencedores com chave colidente: votos do suplementar nao sao a soma com a ordinaria",
   stopifnot(nrow(colv) == pl[vencedor_chave_colide_com_ordinaria == "TRUE", .N],
             all(as.numeric(colv$votos_vencedor) != colv$v_ord)))
# recontagem independente das candidaturas ligadas por turno divergente (todas as candidaturas
# suplementares do cadastro, qualquer cargo, com um so turno no cadastro, sem linha de votacao no
# proprio turno e com exatamente um turno na votacao)
cand[, chave := paste(ANO_ELEICAO, ue, cc, NR_CANDIDATO, SQ_CANDIDATO, sep = "_")]
cc1 <- cand[, .(n_turnos_cad = .N, turno_cad = NR_TURNO[1]), by = chave][n_turnos_cad == 1L]
cc1 <- merge(cc1, vv[, .(chave, nr_turno, v)], by = "chave", all.x = TRUE)
cc1[, `:=`(tem_proprio = any(nr_turno == turno_cad, na.rm = TRUE), n_t = sum(!is.na(nr_turno))), by = chave]
n_div <- cc1[tem_proprio == FALSE & n_t == 1L, uniqueN(chave)]
reg("verif_sup_n_candidaturas_votos_ligados_por_turno_divergente", n_div)

# amostra de 10 pleitos com vencedor: SQ, nome, numero, partido, situacao, data e UE contra o cadastro bruto
set.seed(20260828)
amostra <- pl[status_vencedor == "vencedor_identificado"][sample(.N, 10)]
amostra[, chave := paste(ano_arquivo, unidade_posicao, cd_cargo, nr_candidato, sq_candidato, sep = "_")]
cb <- cand[, .(chave, NR_TURNO, NM_CANDIDATO, SG_PARTIDO, DS_SIT_TOT_TURNO = toupper(DS_SIT_TOT_TURNO), d, NM_UE, SG_UF, NR_TITULO_ELEITORAL_CANDIDATO)]
amc <- merge(amostra, cb, by = "chave")
amc <- amc[NR_TURNO == nr_turno]
ok("amostra de 10: uma linha do cadastro por pleito no turno decisivo", stopifnot(nrow(amc) == 10, uniqueN(amc$id_pleito) == 10))
ok("amostra de 10: nome, partido, UF, unidade, data do turno decisivo e situacao ELEITO batem com o cadastro bruto",
   stopifnot(all(amc$NM_CANDIDATO == amc$vencedor_nome), all(amc$SG_PARTIDO == amc$sg_partido_vencedor),
             all(amc$SG_UF == amc$sg_uf), all(amc$NM_UE == amc$nm_ue),
             all(format(amc$d, "%Y-%m-%d") == amc$dt_turno_decisivo),
             all(amc$DS_SIT_TOT_TURNO == "ELEITO" | amc$fonte_situacao != "cadastro")))
ok("amostra de 10: titulo do vencedor = titulo do cadastro (12 digitos)",
   stopifnot(all(is.na(amc$vencedor_titulo) | amc$vencedor_titulo == formatC(gsub("\\D", "", amc$NR_TITULO_ELEITORAL_CANDIDATO), width = 12, flag = "0"))))
fwrite(amc[, .(id_pleito, nm_ue, sg_uf, cargo, dt_eleicao_suplementar, dt_turno_decisivo, vencedor_nome, sg_partido_vencedor,
               votos_vencedor, situacao_totalizacao, fonte_situacao, id_mandato_ordinario_afetado, momento)],
       "output/verificacao/suplementares_amostra10.csv")
print(amc[, .(id_pleito, nm_ue, sg_uf, dt_turno_decisivo, vencedor_nome, sg_partido_vencedor, votos_vencedor)])

## ------------------------------------------------------------ 4b. integracao em mandatos.csv (script 10)
if (all(c("forma_saida", "fonte_forma_saida", "sucessor_id", "via_sucessao", "data_fim_efetiva") %in% names(mand))) {
  im <- merge(fs, mand[, .(id_mandato_ordinario_afetado = id_mandato, fs_m = forma_saida, fonte_m = fonte_forma_saida,
                           fim_m = data_fim_efetiva, suc_m = sucessor_id, via_m = via_sucessao)],
              by = "id_mandato_ordinario_afetado")
  ok("integracao: todo mandato afetado esta em mandatos.csv", stopifnot(nrow(im) == nrow(fs)))
  # fontes de prioridade maior (registro institucional, wikidata) podem sobrepor a forma inferida
# 13/09/2026: camara_biografia (biografia oficial da Camara) e fonte_oficial_curada (tabelas curadas em ref/) entram como fontes autoritativas
  # 21/09/2026: rotulos da regra A1 para o evento curado de governo e Presidencia (lib/tipo_fonte.R)
  fontes_sup <- c("camara_api", "senado_api", "camara_biografia", "fonte_oficial_curada", "base_dhbb_curada", "noticia_orgao_publico_curada", "pista_nao_oficial", "tce", "assembleia_historico", "assembleia_api", "sapl_municipal", "portal_camara", "wikipedia", "wikidata", "wikidata_obito", "diario_oficial", "derivado_titular")
  ok("integracao: fonte da forma de saida e tse_suplementar ou fonte de prioridade maior",
     in_set(im$fonte_m, c("tse_suplementar", fontes_sup), permitir_na = FALSE, nome = "fonte_forma_saida"))
  ts <- im[fonte_m == "tse_suplementar"]
  ok("integracao (tse_suplementar): forma_saida = perda inferida (durante) ou nao_tomou_posse (antes da posse)",
     stopifnot(all(ts[momento == "durante_o_mandato", fs_m == "perda_do_mandato_inferida_por_eleicao_suplementar"]),
               all(ts[momento == "antes_da_posse", fs_m == "nao_tomou_posse"])))
  ok("integracao: sucessor_id = sucessor_via_suplementar e via_sucessao = eleicao_suplementar sempre que ha sucessor",
     stopifnot(all(im[!is.na(sucessor_via_suplementar), suc_m == sucessor_via_suplementar & via_m == "eleicao_suplementar"])))
  ok("integracao: via_sucessao eleicao_suplementar em mandatos.csv so nos mandatos afetados com sucessor",
     stopifnot(identical(sort(mand[via_sucessao %in% "eleicao_suplementar", id_mandato]),
                         sort(fs[!is.na(sucessor_via_suplementar), id_mandato_ordinario_afetado]))))
  ok("integracao: perda inferida em mandatos.csv so vem de tse_suplementar e so em mandatos afetados",
     stopifnot(all(mand[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar", fonte_forma_saida == "tse_suplementar"]),
               all(mand[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar", id_mandato] %in% fs$id_mandato_ordinario_afetado)))
  ok("integracao: nao_tomou_posse com fonte tse_suplementar so nos 'antes_da_posse'",
     stopifnot(identical(sort(mand[forma_saida == "nao_tomou_posse" & fonte_forma_saida == "tse_suplementar", id_mandato]),
                         sort(fs[momento == "antes_da_posse", id_mandato_ordinario_afetado]))))
  reg("verif_sup_n_mandatos_forma_saida_sobreposta_por_fonte_maior", im[fonte_m != "tse_suplementar", .N])
  reg("verif_sup_n_mandatos_tse_sup_data_fim_efetiva_diferente_da_inferida", ts[fim_m != data_fim_inferida, .N])
  reg("verif_sup_n_mandatos_csv_perda_inferida", mand[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar", .N])
  reg("verif_sup_n_mandatos_csv_nao_tomou_posse_tse_sup", mand[forma_saida == "nao_tomou_posse" & fonte_forma_saida == "tse_suplementar", .N])
  reg("verif_sup_n_mandatos_csv_via_eleicao_suplementar", mand[via_sucessao %in% "eleicao_suplementar", .N])
}

ama <- pl[grepl("AMAZONINO", vencedor_nome) & ano_arquivo == "2014"]
ok("Amazonino AM 2017: 2 turnos e 782933 votos",
   stopifnot(nrow(ama) == 1, ama$nr_turno == "2", ama$votos_vencedor == "782933"))

## ------------------------------------------------------------ 5. recontagem contra numeros_assinatura.txt
ass <- fread("output/numeros_assinatura.txt", sep = "|", header = FALSE, colClasses = "character", strip.white = TRUE)
setnames(ass, c("chave","valor","ep","data","checksum","out"))
ass <- ass[grepl("^sup_", chave)]
ult <- ass[, .SD[.N], by = chave]        # ultimo valor registrado por chave
ult_ck <- ass[.N, checksum]              # checksum do bloco mais recente
reg("verif_sup_checksum_bloco_mais_recente", ult_ck)
reg("verif_sup_n_blocos_de_checksum_distintos", uniqueN(ass$checksum))
por_cargo <- pl[, .N, by = cargo][order(cargo), paste0(cargo, "=", N)]
rec <- c(
  sup_n_candidaturas_suplementares = nrow(cand),
  sup_anos_arquivo_com_suplementar = paste(sort(unique(cand$ANO_ELEICAO)), collapse = ";"),
  sup_cargos_presentes = paste(sort(unique(cand$cc)), collapse = ";"),
  sup_n_pleitos_majoritarios = nrow(pl),
  sup_n_pleitos_por_cargo = paste(por_cargo, collapse = ";"),
  sup_n_pleitos_2004_prefeito = pl[ano_arquivo == "2004" & cd_cargo == "11", .N],
  sup_n_pleitos_2008_prefeito = pl[ano_arquivo == "2008" & cd_cargo == "11", .N],
  sup_n_pleitos_2012_prefeito = pl[ano_arquivo == "2012" & cd_cargo == "11", .N],
  sup_n_pleitos_2014_governador = pl[ano_arquivo == "2014" & cd_cargo == "3", .N],
  sup_n_pleitos_2016_prefeito = pl[ano_arquivo == "2016" & cd_cargo == "11", .N],
  sup_n_pleitos_2018_senador = pl[ano_arquivo == "2018" & cd_cargo == "5", .N],
  sup_n_pleitos_2020_prefeito = pl[ano_arquivo == "2020" & cd_cargo == "11", .N],
  sup_n_pleitos_2022_governador = pl[ano_arquivo == "2022" & cd_cargo == "3", .N],
  sup_n_pleitos_2024_prefeito = pl[ano_arquivo == "2024" & cd_cargo == "11", .N],
  sup_n_pleitos_2020_vereador = ver[ano_arquivo == "2020", uniqueN(id_pleito)],
  sup_n_pleitos_2024_vereador = ver[ano_arquivo == "2024", uniqueN(id_pleito)],
  sup_n_pleitos_realizados = pl[realizado_ate_data_do_arquivo == "TRUE", .N],
  sup_n_pleitos_posteriores_a_data = pl[realizado_ate_data_do_arquivo == "FALSE", .N],
  sup_n_pleitos_com_vencedor = pl[status_vencedor == "vencedor_identificado", .N],
  sup_n_pleitos_sem_vencedor_marcado = pl[status_vencedor == "sem_vencedor_marcado_no_tse", .N],
  sup_n_pleitos_2_turnos = pl[nr_turno == "2", .N],
  sup_n_vencedores_imputados_por_votos = pl[fonte_situacao == "imputacao_votos", .N],
  sup_n_pleitos_chave_colide_ordinaria = pl[chave_colide_com_ordinaria == "TRUE", .N],
  sup_n_vencedores_com_votos = pl[!is.na(votos_vencedor), .N],
  sup_n_vencedores_sem_votos = pl[status_vencedor == "vencedor_identificado" & is.na(votos_vencedor), .N],
  sup_n_candidaturas_votos_ligados_por_turno_divergente = n_div,
  sup_n_mandatos_afetados = nrow(fs),
  sup_n_mandatos_afetados_forma_saida_perda = fs[forma_saida == "perda_do_mandato_inferida_por_eleicao_suplementar", .N],
  sup_n_mandatos_afetados_forma_saida_antes_posse = fs[momento == "antes_da_posse", .N],
  sup_n_mandatos_afetados_mais_de_um_pleito = fs[as.integer(n_pleitos_suplementares) > 1, .N],
  sup_n_mandatos_afetados_sucessor_de_pleito_posterior = fs[id_pleito_sucessor != id_pleito_suplementar, .N],
  sup_n_vencedores_chave_colide_ordinaria = pl[vencedor_chave_colide_com_ordinaria %in% "TRUE", .N],
  sup_n_pleitos_sem_mandato_ordinario_no_bocel = pl[is.na(id_mandato_ordinario_afetado) & cd_cargo != "5", .N],
  sup_n_pleitos_senador_mandato_ambiguo = pl[cd_cargo == "5" & is.na(id_mandato_ordinario_afetado), .N],
  sup_n_pleitos_antes_da_posse = pl[momento %in% "antes_da_posse", .N],
  sup_n_vencedores_ja_no_bocel = pl[!is.na(vencedor_id_pessoa), .N],
  sup_n_vencedores_sem_id_pessoa = pl[status_vencedor == "vencedor_identificado" & is.na(vencedor_id_pessoa), .N],
  sup_n_vencedor_e_ocupante_ordinario = pl[vencedor_e_o_ocupante_ordinario %in% "TRUE", .N],
  sup_n_vices_identificados = pl[!is.na(vice_nome), .N],
  sup_n_vices_ja_no_bocel = pl[!is.na(vice_id_pessoa), .N],
  sup_n_sucessores_via_suplementar_no_bocel = fs[!is.na(sucessor_via_suplementar), .N],
  sup_n_pleitos_vereador = uniqueN(ver$id_pleito),
  sup_n_eleitos_vereador = nrow(ver),
  sup_n_eleitos_vereador_ja_no_bocel = ver[!is.na(eleito_id_pessoa), .N],
  sup_n_eleitos_vereador_com_mandato_ordinario_mesma_camara = ver[!is.na(id_mandato_ordinario_mesma_pessoa), .N]
)
rec_dt <- data.table(chave = names(rec), recontado = as.character(rec))
cmpn <- merge(rec_dt, ult[, .(chave, registrado = valor)], by = "chave", all.x = TRUE)
cmpn[, bate := recontado == registrado]
fwrite(cmpn, "output/verificacao/suplementares_recontagem.csv")
print(cmpn[bate %in% FALSE | is.na(bate)])
ok("recontagem: todos os numeros sup_* batem com o ultimo registro", stopifnot(all(cmpn$bate)))
reg("verif_sup_n_numeros_recontados", nrow(cmpn))
reg("verif_sup_n_numeros_que_batem", sum(cmpn$bate, na.rm = TRUE))

# taxas de pareamento (registradas, para julgamento)
reg("verif_sup_taxa_vencedor_com_id_pessoa", round(pl[status_vencedor == "vencedor_identificado", mean(!is.na(vencedor_id_pessoa))], 4))
reg("verif_sup_taxa_vencedor_titulo_disponivel", round(pl[status_vencedor == "vencedor_identificado", mean(!is.na(vencedor_titulo))], 4))
reg("verif_sup_taxa_pleito_realizado_com_vencedor", round(pl[realizado_ate_data_do_arquivo == "TRUE", mean(status_vencedor == "vencedor_identificado")], 4))
reg("verif_sup_taxa_pleito_prefeito_gov_com_mandato_afetado", round(pl[cd_cargo != "5", mean(!is.na(id_mandato_ordinario_afetado))], 4))
reg("verif_sup_n_pleitos_sem_vencedor_por_ano", pl[status_vencedor == "sem_vencedor_marcado_no_tse", .N, by = ano_arquivo][order(ano_arquivo), paste0(ano_arquivo, "=", N, collapse = ";")])
reg("verif_sup_n_vencedor_e_ocupante_ordinario_antes_da_posse", pl[vencedor_e_o_ocupante_ordinario %in% "TRUE" & momento == "antes_da_posse", .N])

gravar_relatorio_verificacao(
  alvo = "data/eleicoes_suplementares.csv;data/eleicoes_suplementares_vereador.csv;data/mandatos_forma_saida_suplementar.csv",
  script = script, passou = passou, falhou = falhou,
  fora_de_cobertura = c("causa da vacancia e posse efetiva nao observaveis no TSE",
                        "51 pleitos sem vencedor marcado: lacuna do TSE, nao imputada",
                        "causa da vacancia e posse efetiva nao conferidas contra noticia publica alem da amostra de 10 (julgamento do autor)",
                        "quando fonte de prioridade maior (Wikidata) sobrepoe a forma inferida, data_fim_efetiva pode vir dessa fonte com fonte_forma_saida ainda tse_suplementar (script 10)"))
cat("\nverifica_suplementares: passou", length(passou), "| falhou", length(falhou), "\n")
if (length(falhou)) { cat("FALHAS:\n"); print(falhou) }
