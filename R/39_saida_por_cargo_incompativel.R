#!/usr/bin/env Rscript
# Saida deduzida de incompatibilidade de cargo, sem coleta externa. Quem ocupa mandato eletivo nao
# pode assumir outro cargo eletivo incompativel e permanecer no primeiro, de modo que a posse no
# segundo encerra o primeiro. A evidencia esta dentro do proprio banco: as duas pessoas sao a mesma
# (id_pessoa) e as duas janelas de mandato se sobrepoem.
# Escrito em 30/08/2026, ao medir que 143 mandatos de deputado estadual sem forma de saida pertencem
# a quem assumiu prefeitura ou vice-prefeitura no meio do mandato.
#
# O rotulo e 'outro', e nao 'renuncia': o que o banco estabelece e que o mandato terminou antes do
# fim convencional, nao o ato juridico pelo qual terminou (renuncia, licenca sem retorno, vacancia
# declarada). Promover a renuncia sem o ato seria imputacao.
suppressPackageStartupMessages({library(data.table)})
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "proveniencia.R"))
source(file.path(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")), "lib", "asserts_rigor.R"))
setwd(Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")))  # 05/09/2026: o script lia caminhos relativos e dependia do diretorio corrente

m <- fread("data/mandatos.csv", na.strings = c("NA", ""), colClasses = "character", encoding = "UTF-8")
m[, `:=`(mi = as.IDate(mandato_inicio), mf = as.IDate(mandato_fim))]

# pares de cargos incompativeis entre si: legislativo de uma esfera contra executivo de qualquer
# esfera, e executivo contra executivo. Nao entram os pares do mesmo ciclo eleitoral, porque ali a
# sobreposicao seria erro de dado e nao troca de cargo.
LEG <- c("5", "6", "7", "8", "13")            # senador, dep. federal, estadual, distrital, vereador
EXEC <- c("1", "2", "3", "4", "11", "12")     # presidente, vice, governador, vice, prefeito, vice

alvo <- m[cd_cargo %in% c(LEG, EXEC), .(id_mandato, id_pessoa, cd_cargo, cargo, sg_uf, mi, mf,
                                        forma_saida, fonte_forma_saida, ano_eleicao)]
novo <- m[cd_cargo %in% c(LEG, EXEC), .(id_pessoa, id_mandato_novo = id_mandato, cargo_novo = cargo,
                                        cd_novo = cd_cargo, ini_novo = mi, ano_novo = ano_eleicao)]
j <- merge(alvo, novo, by = "id_pessoa", allow.cartesian = TRUE)
j <- j[id_mandato != id_mandato_novo]
# a posse no cargo novo cai DENTRO da janela do primeiro, com folga de 30 dias das bordas, o que
# exclui a sucessao normal entre um mandato e o seguinte
j <- j[ini_novo > mi + 30L & ini_novo < mf - 30L]
# ciclos distintos: municipal contra geral. Dentro do mesmo ciclo a sobreposicao nao existe por
# construcao do calendario, e se aparecer e defeito de dado, nao troca de cargo.
munic <- function(cd) cd %in% c("11", "12", "13")
j <- j[munic(cd_cargo) != munic(cd_novo)]
if (!nrow(j)) { cat("39_saida_por_cargo_incompativel: nenhum caso\n"); quit(save = "no") }

# uma linha por mandato: a posse mais antiga entre as incompatibilidades encontradas
setorder(j, id_mandato, ini_novo)
out <- j[, .(id_pessoa = id_pessoa[1], cargo = cargo[1], sg_uf = sg_uf[1],
             cargo_incompativel = cargo_novo[1], id_mandato_incompativel = id_mandato_novo[1],
             data_posse_incompativel = as.character(ini_novo[1]),
             data_fim_inferida = as.character(ini_novo[1] - 1L),
             forma_saida = "outro"), by = id_mandato]
checa_unica(as.data.frame(out), "id_mandato")
in_set(out$forma_saida, c("outro"), "forma_saida da incompatibilidade")
fwrite(out, "data/saida_cargo_incompativel.csv", na = "NA")

tem <- function(x) !is.na(x) & x != "nao_observado"
idx <- match(out$id_mandato, m$id_mandato)
cat("\n== casos por cargo encerrado e cargo assumido ==\n")
print(out[, .N, by = .(cargo, cargo_incompativel)][order(-N)], nrows = 20)
cat("\n== ja tem forma hoje:", sum(tem(m$forma_saida[idx])), "| sem forma hoje:", sum(!tem(m$forma_saida[idx])), "\n")
cat("\n== quando ja ha forma, qual e ==\n")
print(data.table(forma = m$forma_saida[idx], fonte = m$fonte_forma_saida[idx])[tem(forma), .N, by = .(forma, fonte)][order(-N)], nrows = 20)

registrar_numero("inc_n_mandatos_encerrados_por_incompatibilidade", nrow(out))
registrar_numero("inc_n_sem_forma_hoje", sum(!tem(m$forma_saida[idx])))
registrar_numero("inc_n_ja_com_forma_hoje", sum(tem(m$forma_saida[idx])))
for (cc in unique(out$cargo)) registrar_numero(paste0("inc_n_", gsub("[^A-Za-z]", "_", cc)), out[cargo == cc, .N])
cat("\n39_saida_por_cargo_incompativel: concluido |", nrow(out), "mandatos\n")

## ---- auditoria: onde a fonte existente contradiz a incompatibilidade
# 'fim_regular' e impossivel para quem assumiu outro cargo eletivo no meio do mandato. A contradicao
# pode estar na relacao da casa, que continua imprimindo o nome, ou no pareamento de pessoa do
# proprio banco, se dois homonimos foram fundidos. Fica como auditoria, e nao como correcao
# automatica, porque as duas causas exigem leitura caso a caso.
aud <- data.table(id_mandato = out$id_mandato, cargo = out$cargo, sg_uf = out$sg_uf,
                  ano_eleicao = m$ano_eleicao[idx], id_pessoa = out$id_pessoa,
                  forma_atual = m$forma_saida[idx], fonte_atual = m$fonte_forma_saida[idx],
                  data_fim_atual = m$data_fim_efetiva[idx],
                  cargo_incompativel = out$cargo_incompativel,
                  posse_incompativel = out$data_posse_incompativel)
contr <- aud[forma_atual %in% "fim_regular"]
fwrite(contr[order(sg_uf, ano_eleicao)], "output/verificacao/incompatibilidade_contradiz_fonte.csv")
cat("\n== contradicoes: fonte diz fim_regular e a pessoa assumiu outro cargo ==\n")
print(contr[, .N, by = .(fonte_atual, cargo, cargo_incompativel)][order(-N)], nrows = 15)
registrar_numero("inc_n_contradiz_fim_regular", nrow(contr))
for (f in unique(contr$fonte_atual)) registrar_numero(paste0("inc_contradiz_", f), contr[fonte_atual == f, .N])
