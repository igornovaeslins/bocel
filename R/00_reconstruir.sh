#!/usr/bin/env bash
# Reconstroi o BOCEL do zero num diretorio limpo.
# Requisitos: R 4.3+ (data.table, arrow, stringi, httr2, jsonlite).
# Uso: bash R/00_reconstruir.sh   (a partir da raiz do repositorio)
#
# 21/09/2026: a cadeia deixou de chamar Python, pela regra sobre o que precisa de script:
# arquivo pronto de fonte oficial (os zips do TSE) nao exige script algum, so a URL, e aqui o download
# e sempre manual porque o CDN do TSE recusa requisicao de R e de curl (so R/coleta/conferir_brutos_tse.R
# confere o que ja foi baixado a mao contra ref/fontes_brutas_tse.csv); banco de dados pronto (a
# filiacao partidaria, pela Base dos Dados no BigQuery) tambem nao exige script, so citar a fonte
# (ref/fonte_filiacao.md); raspagem e coleta de dado primario (Camara, Senado, Wikidata, Wikipedia,
# DivulgaCand) tem coletor em R/coleta/. As camadas fora do recorte da v1.0 (municipios, Assembleias,
# SAPL, TCE, diarios oficiais) ainda so tem coletor em Python e por isso ficam pausadas aqui: os passos
# R que leem o bruto dessas camadas rodam so se o bruto ja estiver em disco de uma coleta anterior, com
# aviso de que a coleta continua na v1.5 (decisao de 21/09/2026).
set -euo pipefail
cd "$(dirname "$0")/.."
mkdir -p data_raw/consulta_cand data_raw/votacao data logs output/verificacao
# 05/09/2026: a raiz e as bibliotecas de verificacao (lib/asserts_rigor.R, lib/proveniencia.R) sao
# resolvidas pela variavel BOCEL_ROOT, que aponta para este diretorio; nada e copiado para fora dele
export BOCEL_ROOT="$(pwd)"

# TSE (consulta_cand e votacao): arquivo pronto dos dados abertos, baixado a mao pelas URLs de
# R/coleta/manifest_consulta_cand.txt e R/coleta/manifest_votacao.txt. Este passo so confere o bruto
# ja em data_raw/ contra ref/fontes_brutas_tse.csv (bytes e md5) e aborta na divergencia; nao baixa nada
Rscript --vanilla R/coleta/conferir_brutos_tse.R

Rscript --vanilla R/01_ingest_cand.R
Rscript --vanilla R/02_ingest_votacao.R
# 12/09/2026: o id de pessoa vem de ref/ids_pessoa_referencia.parquet, que acompanha o deposito; sem o arquivo o
# R/03 numera pela ordem das chaves, e os ids deixam de coincidir com os da versao publicada
Rscript --vanilla R/03_build_banco.R
# filiacoes: banco pronto da Base dos Dados via BigQuery (ref/fonte_filiacao.md), sem coletor proprio;
# roda so se o parquet ja estiver em disco (a consulta ao BigQuery e feita a mao, fora desta cadeia)
[ -f data_raw/filiacao/filiacao_atual.parquet ] && Rscript --vanilla R/03b_filiacoes.R || echo "filiacoes: pulado (falta data_raw/filiacao/filiacao_atual.parquet, ver ref/fonte_filiacao.md)"
# posse, exercicio e forma de saida (fontes complementares; cada uma pula se a fonte falhar)
Rscript --vanilla R/coleta/camara.R && Rscript --vanilla R/07_exercicio_camara.R || echo "camara: pulado"
Rscript --vanilla R/coleta/senado.R && Rscript --vanilla R/07_exercicio_senado.R || echo "senado: pulado"
Rscript --vanilla R/08_suplementares.R || echo "suplementares: pulado"
Rscript --vanilla R/09_auditoria_homonimos.R || echo "homonimos: pulado"
# municipios (prefeito, vice-prefeito, vereador): fora do recorte da v1.0, coletor ainda so em Python
[ -d data_raw/munic ] && Rscript --vanilla R/10_munic_inventario.R && Rscript --vanilla R/11_munic_prefeitos.R || echo "municipios: coleta fica para a v1.5 (bruto ausente em data_raw/munic)"
Rscript --vanilla R/coleta/wikidata.R && Rscript --vanilla R/12_wikidata_mandatos.R || echo "wikidata: pulado"
# deputado estadual e distrital: fora do recorte da v1.0, coletor ainda so em Python
[ -d data_raw/assembleias ] && Rscript --vanilla R/13_exercicio_assembleias.R || echo "assembleias: coleta fica para a v1.5 (bruto ausente em data_raw/assembleias)"
# R/14 roda duas vezes: a primeira grava data_raw/divulgacand/pedidos_reeleicao.csv (SQ dos anos em que
# consulta_cand nao traz ST_REELEICAO); o coletor consulta a API DivulgaCand; a segunda le o cache
Rscript --vanilla R/14_sinais_tse_exercicio.R || echo "sinais tse: pulado"
Rscript --vanilla R/coleta/divulgacand_reeleicao.R && Rscript --vanilla R/14_sinais_tse_exercicio.R || echo "divulgacand: pulado (sinal de reeleicao so dos anos com ST_REELEICAO em consulta_cand)"
# camaras municipais via SAPL: fora do recorte da v1.0, coletor ainda so em Python
[ -d data_raw/sapl_municipal ] && Rscript --vanilla R/15_exercicio_camaras_municipais.R || echo "sapl municipal: coleta fica para a v1.5 (bruto ausente em data_raw/sapl_municipal)"
# Wikipedia: deputado estadual/distrital e prefeitos, as duas camadas fora do recorte da v1.0. O coletor
# R/coleta/wikipedia_listas.R ja existe mas nao roda aqui, porque as duas saidas que alimenta ficam para a v1.5
[ -d data_raw/wikipedia/estadual ] && Rscript --vanilla R/16_wikipedia_estadual.R || echo "wikipedia estadual: coleta fica para a v1.5 (bruto ausente em data_raw/wikipedia/estadual)"
[ -d data_raw/wikipedia/prefeitos ] && Rscript --vanilla R/17_wikipedia_prefeitos.R || echo "wikipedia prefeitos: coleta fica para a v1.5 (bruto ausente em data_raw/wikipedia/prefeitos)"
# DataJud/CNJ (cassacoes eleitorais nos 27 TREs e no TSE): coletor ainda so em Python, sem porte nesta rodada
[ -d data_raw/datajud ] && Rscript --vanilla R/18_datajud_cassacoes.R || echo "datajud: coleta em Python ainda nao portada para R, pulando (bruto ausente em data_raw/datajud)"
# amostra de conferencia ao vivo usada por R/verifica_datajud.R (semente fixa): depende do mesmo coletor
# em Python de data_raw/datajud acima e por isso nao roda aqui; R/verifica_datajud.R pula sozinho sem ela
[ -d data_raw/camaras_sem_sapl ] && Rscript --vanilla R/20_exercicio_camaras_sem_sapl.R || echo "camaras sem sapl: coleta fica para a v1.5 (bruto ausente em data_raw/camaras_sem_sapl)"
# 12/09/2026: R/21 le forma_saida e fonte_forma_saida de mandatos.csv, que so existem depois do R/10; o
# parse roda depois da primeira integracao (bloco abaixo), gated no bruto ja em disco (diarios, fora da v1.0)
[ -d data_raw/assembleias_sem_historico ] && Rscript --vanilla R/22_exercicio_assembleias_historico.R || echo "assembleias historico: coleta fica para a v1.5 (bruto ausente em data_raw/assembleias_sem_historico)"
[ -f data_raw/tce/inventario_tce.csv ] && Rscript --vanilla R/23_tce_gestores.R || echo "tce A: coleta fica para a v1.5 (bruto ausente em data_raw/tce/inventario_tce.csv)"
[ -f data_raw/tce/inventario_tce_b.csv ] && Rscript --vanilla R/24_tce_gestores_b.R || echo "tce B: coleta fica para a v1.5 (bruto ausente em data_raw/tce/inventario_tce_b.csv)"
[ -f data_raw/tce/inventario_tce_c.csv ] && Rscript --vanilla R/33_tce_gestores_c.R || echo "tce C: coleta fica para a v1.5 (bruto ausente em data_raw/tce/inventario_tce_c.csv)"
[ -f data_raw/tce/inventario_tce_d.csv ] && Rscript --vanilla R/34_tce_gestores_d.R || echo "tce D: coleta fica para a v1.5 (bruto ausente em data_raw/tce/inventario_tce_d.csv)"
# 12/09/2026: TCE-AC pelos acordaos de contas anuais (confirma exercicio; R/10 le como fonte_exercicio tce_ac)
[ -f data_raw/tce_ac/indice_acordaos.csv ] && Rscript --vanilla R/57_tce_ac.R || echo "tce AC: coleta fica para a v1.5 (bruto ausente em data_raw/tce_ac/indice_acordaos.csv)"
[ -d data_raw/camaras_sem_sapl_2 ] && Rscript --vanilla R/25_exercicio_camaras_sem_sapl_2.R || echo "camaras 2: coleta fica para a v1.5 (bruto ausente em data_raw/camaras_sem_sapl_2)"
# 13/09/2026: saida do legislativo federal e dos executivos. A biografia oficial da Camara cobre a legislatura 51, que a
# API entrega sem eventos; o R/58 le o historico completo de exercicio e as tabelas curadas com fonte oficial (ref/), e
# o R/60 fecha presidente, vice-presidente, governador e vice-governador; os dois sao autoritativos no R/10
Rscript --vanilla R/coleta/camara_biografia.R && Rscript --vanilla R/59_camara_biografia.R || echo "biografia da camara: pulada"
Rscript --vanilla R/coleta/camara_composicao_atual.R || echo "composicao atual da camara: pulada"
Rscript --vanilla R/58_saida_legislativo_federal.R
Rscript --vanilla R/60_saida_executivos.R
# 21/09/2026: confere as fontes adicionais de presidente/vice-presidente e governador/vice-governador
Rscript --vanilla R/60a_conferir_fontes_executivos.R || echo "conferir fontes executivos: pulado"
# mandato executivo cujo evento curado so tem imprensa ou Wikipedia entra em docs/EXCECOES_CONHECIDAS.csv (idempotente)
Rscript --vanilla R/60e_excecoes_fontes_executivos.R
# 13/09/2026: deputado estadual e distrital pela curadoria com fonte oficial (ref/eventos_assembleias_fonte_oficial.csv e
# ref/composicao_final_assembleias_fonte_oficial.csv, consolidadas pelo R/61a_consolidar_curadoria_assembleias.R a partir
# de data_raw/assembleias3/<UF>/), autoritativa no R/10 nos mandatos que resolve; fora do recorte da v1.0.
# R/61a_consolidar_curadoria_assembleias.R, R/61b_alvos_frentes_assembleias.R (alvos por UF) e
# R/61c_pendentes_frentes_assembleias.R (pendentes da retomada) sao das frentes de curadoria por UF e nao entram nesta cadeia
Rscript --vanilla R/61_saida_assembleias.R
# aproveitamento do que ja esta em disco: recuperacao das instancias SAPL que o coletor deixou pela metade e
# ampliacao da MUNIC por atributos (30/08/2026); as duas camadas ficam fora do recorte da v1.0
Rscript --vanilla R/44_sapl_recuperado.R || echo "sapl recuperado: pulado"
Rscript --vanilla R/49_munic_ampliado.R || echo "munic ampliado: pulado"
Rscript --vanilla R/10_integrar_exercicio.R
# 12/09/2026: os dois produtores que leem a camada integrada rodam sobre a primeira passada, e a integracao
# roda de novo para absorve-los (o R/10 zera a camada ao comecar, de modo que a segunda passada e limpa)
[ -f data_raw/querido_diario/cobertura_cidades.csv ] && Rscript --vanilla R/21_diarios_oficiais.R || echo "diarios oficiais: coleta fica para a v1.5 (bruto ausente em data_raw/querido_diario/cobertura_cidades.csv)"
Rscript --vanilla R/39_saida_por_cargo_incompativel.R || echo "incompatibilidade de cargo: pulada"
# 12/09/2026: R/43, R/50 e R/54 leem forma_saida, fonte_forma_saida, data_fim_efetiva e exercicio_confirmado de
# mandatos.csv, que so existem depois do R/10, e numa reconstrucao do zero paravam no R/43; o R/55 le a saida do R/54.
# O R/10 usa desses arquivos so colunas que nao dependem da integracao, e a segunda passada os absorve.
# 21/09/2026: 05/09/2026: o inventario do bruto (entrada da sondagem e do balanco) e o parse das camaras em
# WordPress que alimentavam o R/43 (data_raw/sonda/camaras_generico_bruto.csv) sao coleta ainda so em Python,
# municipios fora do recorte da v1.0
[ -f data_raw/sonda/camaras_generico_bruto.csv ] && Rscript --vanilla R/43_camaras_generico.R || echo "camaras generico: coleta fica para a v1.5 (bruto ausente em data_raw/sonda/camaras_generico_bruto.csv)"
[ -d data_raw/inventario_assembleias ] && Rscript --vanilla R/50_assembleias_inventario.R || echo "assembleias inventario: coleta fica para a v1.5 (bruto ausente em data_raw/inventario_assembleias)"
# interregno do vereador: o texto livre do SAPL e a lista de presenca em plenario (03-04/09/2026); SAPL fora da v1.0
[ -d data_raw/sapl_municipal ] && Rscript --vanilla R/54_sapl_observacao.R || echo "sapl observacao: coleta fica para a v1.5 (bruto ausente em data_raw/sapl_municipal)"
[ -d data_raw/sapl_presenca ] && Rscript --vanilla R/55_sapl_presenca.R || echo "sapl presenca: coleta fica para a v1.5 (bruto ausente em data_raw/sapl_presenca)"
Rscript --vanilla R/10_integrar_exercicio.R
Rscript --vanilla R/19_levantamento_lacunas.R || echo "levantamento: pulado"
# 06/09/2026: R/53 passou para antes de R/04 porque regrava mandatos.csv (universo_comparavel,
# motivo_sem_saida e chave_tse); antes rodava depois e a versao depositada nao passava pelo
# verificador do nucleo
Rscript --vanilla R/53_universo_comparavel.R || echo "universo comparavel: pulado"
Rscript --vanilla R/04_verificar.R
# nivel de ocupacao: identidade estendida aos suplentes, fila de suplencia e ocupacoes
# (decisao de 30/08/2026; a cadeira continua uma so, o que cresce e o ocupante)
Rscript --vanilla R/40_pessoas_suplentes.R
Rscript --vanilla R/41_lista_suplencia.R
Rscript --vanilla R/42_ocupacoes.R
# 21/09/2026: recorte da v1.0 (presidencia/vice, governador/vice, senador com suplentes, deputado federal,
# ocupacao com suplencia, filiacao), decisao de 21/09/2026; grava data_v1/
Rscript --vanilla R/64_exportar_v1.R
Rscript --vanilla R/51_verifica_tabelas_recuperadas.R || echo "verifica tabelas recuperadas: pulada"
Rscript --vanilla R/52_balanco_aproveitamento.R || echo "balanco do aproveitamento: pulado"
Rscript --vanilla R/26_descritivas_regionais.R || echo "descritivas: pulado"
Rscript --vanilla R/27_idade_genero.R || echo "idade x genero: pulado"
Rscript --vanilla R/28_raca.R || echo "raca: pulado"
Rscript --vanilla R/29_interrupcao.R || echo "interrupcao: pulado"
Rscript --vanilla R/30_migracao_partidaria.R || echo "migracao partidaria: pulado"
Rscript --vanilla R/31_migracao_territorial.R || echo "migracao territorial: pulado"
# 12/09/2026: R/05 passou para depois das descritivas porque documenta raca_eleitos, migracao_partidaria e
# migracao_territorial (linhas e preenchimento), e antes descrevia a versao da rodada anterior; R/35 le o livro
Rscript --vanilla R/05_documentar.R
Rscript --vanilla R/35_livro_codigos_tabular.R || echo "livro tabular: pulado"
Rscript --vanilla R/36_estado_do_banco.R || echo "estado do banco: pulado"
# 05/09/2026: OCR das 226 edicoes do Diario da ALEP digitalizadas como imagem (ja em disco, dentro dos
# zips) e reconstrucao da serie do PR; Assembleias fora do recorte da v1.0, OCR e extracao ainda so em Python.
# O piloto do extrator reprodutivel em R (R/63_extrator_diarios.R, Tocantins) nao entra nesta cadeia
[ -f data_raw/assembleias2/PR/cabecalhos.jsonl ] && Rscript --vanilla R/assembleias2/01_exercicio_PR.R && Rscript --vanilla R/assembleias2/02_build_PR.R && Rscript --vanilla R/assembleias2/verifica_PR.R || echo "serie do PR: coleta/OCR fica para a v1.5 (bruto ausente em data_raw/assembleias2/PR/cabecalhos.jsonl)"
Rscript --vanilla R/38_assembleias_portais.R || echo "assembleias por portal: pulada"
Rscript --vanilla R/37_cobertura_por_uf.R || echo "cobertura por UF: pulada"
Rscript --vanilla R/06_amarracoes_fapesp.R || echo "amarracoes fapesp: pulado"
Rscript --vanilla R/32_checagem_gelape_thome.R || echo "checagem Gelape e Thome: pulada (requer o zip do Dataverse)"
# checagem contra a Base dos Dados no BigQuery (banco pronto, mesma logica da filiacao acima): nao exige
# script de coleta; quem quiser reproduzir a checagem roda a consulta descrita em ref/fonte_filiacao.md a mao
# ancilares de 12/09/2026 (nao entram na reconstrucao): R/congela_referencia_ids.R congela os ids de pessoa da
# versao verificada em ref/ids_pessoa_referencia.parquet; R/sonda_chapa_vices.R e R/sonda_chapa_cascata.R mediram
# a chapa dos vices antes da regra entrar no R/03; R/62a_alvos_frentes_capitais.R e alvo de frente de coleta
# de capitais (v1.5/v2.0, municipios)
# ancilar (nao entra na reconstrucao): R/amostra_diarios_precisao.R mede a precisao da inferencia dos diarios
# R/amostra_divergencia_fontes.R e ancilar: mede a concordancia entre fontes independentes
# R/amostra_sapl_observacao_precisao.R e ancilar: sorteia linhas do texto livre do SAPL para
# leitura manual, e mede a precisao da regra de classificacao por estrato
# sobre a forma de saida (docs/CONCORDANCIA_FONTES.md) e nao entra na reconstrucao.
# ancilares de 19/09/2026 (nao entram na reconstrucao): R/auditoria_fontes_e_linguagem.R classifica por tipo de
# fonte as URLs das tabelas curadas em ref/ e lista o que ainda roda fora do R; R/auditoria_fusao_documentos.R e
# R/auditoria_incompatibilidade_leitura.R sao evidencia de leitura para a pendencia 4 do
# registro de pendencias; R/gera_renv_lock.R escreve o renv.lock a partir do que os scripts usam
echo "BOCEL reconstruido em data/ (nucleo completo) e data_v1/ (recorte publicado da v1.0)"
