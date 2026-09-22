# Livro de códigos — Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL) v1.0

O código de ausente em todos os arquivos CSV é a string `NA`, e nenhuma célula
vazia tem significado. Os códigos de ausência do TSE (`#NE`, `#NULO`, `-1`, `-3`,
`-4`) foram convertidos para `NA` na construção.

## mandatos.csv — pessoa × cargo × mandato (4.253 linhas)

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `ano_eleicao` | inteiro | Ano da eleicao de origem | 100.0% |
| `unidade_posicao` | texto | Unidade em que a posicao e disputada, isto e, codigo TSE do municipio (cargos municipais), UF (estaduais e federais) ou BR (presidente) | 100.0% |
| `id_mandato` | texto | Identificador do mandato, na forma M<ano>_<unidade>_<cd_cargo>_<nr_candidato>_<sq_candidato> | 100.0% |
| `id_pessoa` | texto | Identificador estavel da pessoa (BOCEL + 7 digitos), herdado entre versoes pela referencia ref/ids_pessoa_referencia.parquet; pessoa nova recebe numero acima do maior ja usado | 100.0% |
| `sq_candidato` | texto | SQ_CANDIDATO do TSE (unico nacionalmente so a partir de 2010) | 100.0% |
| `nr_candidato` | texto | Numero do candidato na eleicao de origem | 100.0% |
| `nr_turno_decisivo` | inteiro | Turno em que a eleicao se decidiu (1 ou 2) | 100.0% |
| `dt_eleicao` | data dd/mm/aaaa | Data do pleito (turno decisivo) | 100.0% |
| `cd_cargo` | inteiro | Codigo TSE do cargo | 100.0% |
| `cargo` | texto | Descricao do cargo (PREFEITO, VEREADOR, ...) | 100.0% |
| `esfera` | texto | Esfera (municipal, estadual ou federal) | 100.0% |
| `sg_uf` | texto | UF da posicao (BR para presidente) | 100.0% |
| `sg_ue` | texto | Codigo TSE da unidade eleitoral (municipio ou UF) | 100.0% |
| `nm_ue` | texto | Nome da unidade eleitoral | 100.0% |
| `nr_partido` | inteiro | Numero do partido na eleicao de origem | 100.0% |
| `sg_partido` | texto | Sigla do partido na eleicao de origem | 100.0% |
| `tp_agremiacao` | texto | Tipo de agremiacao (partido isolado, coligacao, federacao) | 100.0% |
| `nm_coligacao` | texto | Nome da coligacao | 100.0% |
| `composicao_coligacao` | texto | Partidos que compoem a coligacao | 100.0% |
| `regra_chapa` | texto | Como a candidatura do vice foi ligada ao titular eleito da mesma chapa, so nos cargos de vice. Valores numero_do_titular (mesmo numero do titular, ou o numero dele seguido de 1 em 1998 e 2000, com situacao vazia no cadastro), numero_titular_imputado (vice de titular imputado por votos, com situacao no cadastro igual a do titular), sq_coligacao (mesma coligacao do titular na unidade, com correspondencia de um para um) e composicao_coligacao (mesma composicao da coligacao escrita por extenso). NA nos titulares e nos demais cargos | 2.6% |
| `reeleicao_declarada` | texto | Candidato declarou concorrer a reeleicao (S/N, TSE) | 43.3% |
| `situacao_totalizacao` | texto | Situacao de totalizacao no turno decisivo (ELEITO, ELEITO POR QP, ELEITO POR MEDIA; ELEITO (IMPUTADO DO TITULAR) para vices; ELEITO (IMPUTADO POR VOTOS) para majoritario sem vencedor marcado pelo TSE) | 100.0% |
| `fonte_situacao` | texto | Origem da situacao, entre cadastro (consulta_cand), votacao (votacao_candidato_munzona), imputacao_titular e imputacao_votos | 100.0% |
| `votos_t1` | inteiro | Votos nominais no 1o turno | 95.4% |
| `votos_turno_decisivo` | inteiro | Votos nominais no turno decisivo | 95.4% |
| `mandato_inicio` | data ISO | Inicio convencional do mandato (1o de fevereiro pos-eleicao para senador e deputados; 1o de janeiro para os demais) | 100.0% |
| `mandato_fim` | data ISO | Fim convencional do mandato (31 de janeiro apos o ultimo ano para senador e deputados; 31 de dezembro para os demais) | 100.0% |
| `forma_saida` | texto | Forma de saida do cargo, vocabulario fechado (fim_regular, renuncia, falecimento, cassacao, afastamento, licenca, nao_tomou_posse, suplente_efetivado, perda_do_mandato_inferida_por_eleicao_suplementar, substituicao_inferida_munic, assumiu_titular, aposentadoria, impeachment, retotalizacao, outro, nao_observado). Em senador, deputado federal, presidente, vice-presidente, governador e vice-governador, licenca e afastamento temporario nao encerram mandato, e o periodo fora do exercicio fica em data/interregnos_legislativo_federal.csv; retotalizacao e a perda da vaga por nova totalizacao dos votos, sem ato ilicito | 100.0% |
| `antecessor_id` | texto | id_pessoa de quem ocupou a mesma posicao no mandato anterior (executivos) | 4.0% |
| `sucessor_id` | texto | id_pessoa de quem ocupou a mesma posicao no mandato seguinte (executivos) | 4.0% |
| `via_sucessao` | texto | Via institucional da sucessao; v1 = nova_eleicao (executivos) | 4.6% |
| `reeleito_mesma_pessoa` | logico | Antecessor imediato e a mesma pessoa (executivos) | 4.0% |
| `data_posse` | data ISO | Inicio do primeiro periodo de exercicio registrado (Camara, Senado, Assembleias, Wikidata) | 99.5% |
| `data_fim_efetiva` | data ISO | Fim do ultimo periodo de exercicio registrado, ou data da saida inferida | 86.1% |
| `precisao_data_fim` | texto | Precisao de data_fim_efetiva, com os valores ato (data do ato registrado pela casa ou por fonte oficial), convencional (fim regular na data constitucional), intervalo (forma confirmada e dia exato fora do alcance das fontes, com os limites na tabela curada). Preenchida nesta versao para senador, deputado federal, presidente, vice-presidente, governador e vice-governador, e vazia nos demais cargos e no mandato sem saida | 86.1% |
| `fonte_forma_saida` | texto | Fonte da forma de saida (camara_api, senado_api, camara_biografia, fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada, pista_nao_oficial, assembleia_api, sapl_municipal, sapl_observacao, portal_camara, tce, assembleia_historico, assembleia_portal, assembleia_inventario, diario_oficial, wikipedia, wikidata, wikidata_obito, datajud, tse_suplementar, ibge_munic, cargo_incompativel, derivado_titular, data_fim_efetiva). sapl_observacao e o ato escrito no campo de texto livre do SAPL, lido por R/54; assembleia_inventario e a folha e a frequencia das casas estaduais; cargo_incompativel e a posse em outro cargo eletivo deduzida do proprio banco. camara_biografia e a biografia oficial do deputado na Camara, fonte da legislatura 1999-2003; fonte_oficial_curada e o evento das tabelas em ref/ com fonte oficial, e nos casos federais especiais tambem com materia e trecho literal conferido. Nos governos e na Presidencia, o evento curado sem fonte oficial leva o rotulo da melhor fonte que tem, base_dhbb_curada para o verbete do DHBB/CPDOC-FGV, noticia_orgao_publico_curada para a noticia publicada por orgao publico e pista_nao_oficial quando so ha imprensa ou Wikipedia, caso em que o mandato consta de docs/EXCECOES_CONHECIDAS.csv. derivado_titular marca o vice que assume quando o titular sai; data_fim_efetiva marca o fim efetivo registrado que iguala ou ultrapassa o fim convencional, promovido a fim_regular | 86.1% |
| `exercicio_confirmado` | data ISO | Data mais recente em que o titular foi observado em exercicio (MUNIC; registro da candidatura a reeleicao no TSE) | 45.0% |
| `fonte_exercicio` | texto | Fonte da confirmacao de exercicio (ibge_munic; ibge_munic_ampliado; tse_reeleicao; receita_cnpj; sapl_presenca; tce_ac), separadas por ponto e virgula quando ha mais de uma. sapl_presenca e a lista de presenca em plenario, na data da ultima presenca; ibge_munic_ampliado e a MUNIC lida por atributos, so no lado que confirma o eleito; tce_ac e o acordao de contas anuais do Tribunal de Contas do Acre, na data de inicio do periodo julgado | 45.0% |
| `universo_comparavel` | logico | A unidade (municipio, UF ou casa nacional) teve varredura por fonte institucional naquela eleicao. Dentro do universo a ausencia de saida observada significa que o mandato terminou; fora dele significa que ninguem olhou. Use esta coluna antes de comparar forma de saida entre esferas, cargos ou anos | 100.0% |
| `motivo_sem_saida` | texto | Motivo da ausencia de forma de saida observada, derivado de universo_comparavel, mandato_fim e forma_saida sem fonte nova. Valores nao_se_aplica (saida observada), em_curso (fim convencional posterior a data de referencia da reconstrucao, output/data_referencia.txt), fim_regular_presumido (dentro do universo comparavel, em que a casa foi varrida) e fonte_ausente (fora dele) | 100.0% |
| `chave_tse` | texto | Chave de juncao com bases derivadas do TSE (ano_eleicao, cd_cargo, sg_ue, nr_candidato), unica em todas as linhas (R/53 reprova duplicata). Preferir ao SQ_CANDIDATO, que so e unico nacionalmente a partir de 2010 | 100.0% |

## posicoes_ano.csv — pessoa × cargo × ano (18.092 linhas)

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_mandato` | texto | Chave para mandatos.csv | 100.0% |
| `id_pessoa` | texto | Identificador estavel da pessoa | 100.0% |
| `cd_cargo` | inteiro | Codigo TSE do cargo | 100.0% |
| `cargo` | texto | Descricao do cargo | 100.0% |
| `esfera` | texto | Esfera (municipal, estadual ou federal) | 100.0% |
| `sg_uf` | texto | UF da posicao | 100.0% |
| `sg_ue` | texto | Codigo TSE da unidade eleitoral | 100.0% |
| `nm_ue` | texto | Nome da unidade eleitoral | 100.0% |
| `ano_eleicao` | inteiro | Ano da eleicao de origem | 100.0% |
| `sg_partido` | texto | Sigla do partido na eleicao de origem | 100.0% |
| `nr_partido` | inteiro | Numero do partido na eleicao de origem | 100.0% |
| `ano` | inteiro | Ano-calendario em que a pessoa ocupa a posicao | 100.0% |

## pessoas.csv — cadastro de pessoas (2.155 linhas)

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Identificador estavel da pessoa | 100.0% |
| `nome` | texto | Nome completo (registro mais recente no TSE) | 100.0% |
| `nome_urna_recente` | texto | Nome de urna mais recente | 99.5% |
| `dt_nascimento` | data ISO | Data de nascimento | 100.0% |
| `genero` | texto | Genero declarado ao TSE | 100.0% |
| `nr_titulo_eleitoral` | texto | Titulo de eleitor (12 digitos, divulgado pelo TSE) | 100.0% |
| `nr_cpf` | texto | CPF (11 digitos, divulgado pelo TSE) | 99.9% |
| `n_mandatos` | inteiro | Numero de mandatos da pessoa no banco | 100.0% |
| `primeiro_ano_eleito` | inteiro | Primeiro ano em que foi eleita | 100.0% |
| `ultimo_ano_eleito` | inteiro | Ultimo ano em que foi eleita | 100.0% |
| `chave_dedup` | texto | Regra que identificou a pessoa (titulo, cpf ou nome_nascimento) | 100.0% |
| `dedup_suspeita_fusao` | texto | Leitura da fusao por documento que R/03 manteve porque nome ou nascimento coincidem entre as candidaturas unidas por um titulo ou CPF (nome_diferente_mesmo_nascimento, mesmo_nome_nascimento_diferente, mesmo_nome_e_nascimento_documento_reemitido); NA quando nao ha suspeita. A lista das fusoes desfeitas esta em output/verificacao/fusao_documentos_separacao.csv | 0.1% |
| `dedup_ponte_nome_nascimento` | logico | Nome e data de nascimento sao a unica chave de identidade desta pessoa, sem titulo nem CPF em nenhuma candidatura (auditoria em data_v1/auditoria_homonimos.csv) | 100.0% |
| `dedup_auditoria` | texto | Classificacao da auditoria de homonimos (consistente, suspeito, indeterminado) para as pessoas auditadas em data_v1/auditoria_homonimos.csv; nao_auditado para as demais | 100.0% |

## filiacoes.csv — pessoa × partido × filiação (5.942 linhas)

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Identificador estavel da pessoa | 100.0% |
| `sigla_partido` | texto | Sigla do partido da filiacao | 100.0% |
| `sg_uf` | texto | UF da zona eleitoral da filiacao | 100.0% |
| `cd_municipio_tse` | texto | Codigo TSE do municipio da zona eleitoral | 100.0% |
| `data_filiacao` | data ISO | Data de filiacao | 99.6% |
| `situacao_registro` | texto | Situacao do registro na lista (REGULAR, CANCELADO, DESFILIADO, ...) | 100.0% |
| `data_desfiliacao` | data ISO | Data de desfiliacao, quando houve | 28.3% |
| `data_cancelamento` | data ISO | Data de cancelamento, quando houve | 44.8% |
| `motivo` | texto | Motivo de desfiliacao ou cancelamento | 42.7% |
| `fonte` | texto | lista_atual (extracao TSE 2026) ou lista_antiga (listas historicas ate 2019) | 100.0% |
| `data_referencia` | data ISO | Data de extracao ou processamento da lista de origem | 29.9% |

Fonte: listas de filiados do TSE (Sistema FILIA), espelhadas pela Base dos Dados (`br_tse_filiacao_partidaria`, tabelas `microdados` e `microdados_antigos`), pareadas às pessoas do banco pelo título de eleitor.

## exercicio_camara.csv — periodos de exercicio na Camara dos Deputados (API dadosabertos.camara.leg.br) (6.329 linhas)

A chave declarada é `id_deputado_camara` × `legislatura` × `data_inicio_exercicio` × `condicao`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Identificador da pessoa no BOCEL (NA para suplente fora do banco) | 94.1% |
| `id_mandato` | texto | Mandato do BOCEL a que o periodo pertence (cd_cargo 6, eleicao que abre a legislatura) | 73.5% |
| `id_deputado_camara` | inteiro | id do deputado na API da Camara | 99.9% |
| `legislatura` | inteiro | Numero da legislatura (51 = 1999-2003 ... 57 = 2023-2027) | 100.0% |
| `ano_eleicao` | inteiro | Ano da eleicao que abriu a legislatura | 100.0% |
| `nome_civil` | texto | Nome civil na Camara | 99.9% |
| `nome_parlamentar` | texto | Nome parlamentar | 99.9% |
| `cpf` | inteiro | CPF (divulgado pela Camara) | 99.9% |
| `dt_nascimento` | data ISO | Data de nascimento (Camara) | 99.9% |
| `dt_falecimento` | data ISO | Data de falecimento (arquivo em massa da Camara) | 9.9% |
| `sg_uf` | texto | UF | 100.0% |
| `sg_partido` | texto | Partido no ultimo status | 90.6% |
| `condicao` | texto | titular ou suplente | 100.0% |
| `efetivado` | logico | Suplente efetivado como titular (logico) | 100.0% |
| `data_inicio_exercicio` | data ISO | Inicio do periodo (evento Entrada no historico) | 86.8% |
| `data_fim_exercicio` | data ISO | Fim do periodo (evento Saida); NA se em curso | 80.3% |
| `situacao_final` | texto | Situacao ao fim do periodo (texto da Camara ou marcador do script) | 100.0% |
| `forma_saida` | texto | Forma de saida no vocabulario fechado | 91.8% |
| `descricao_entrada` | texto | Texto original do evento de entrada | 99.9% |
| `descricao_saida` | texto | Texto original do evento de saida | 80.3% |
| `regra_pareamento` | texto | Regra que pareou com o BOCEL (cpf, nome+nascimento, ...) | 94.0% |
| `fonte` | texto | camara_api | 100.0% |
| `url_fonte` | texto | URL do recurso na API | 99.9% |

## exercicio_senado.csv — exercicios no Senado Federal (API legis.senado.leg.br/dadosabertos) (707 linhas)

A chave declarada é `codigo_exercicio`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Identificador da pessoa no BOCEL (NA para suplente fora do banco) | 70.2% |
| `id_mandato` | texto | Mandato do BOCEL (cd_cargo 5) | 60.1% |
| `codigo_senador` | inteiro | Codigo do parlamentar no Senado | 100.0% |
| `nome_senado` | texto | Nome completo no Senado | 100.0% |
| `nome_parlamentar` | texto | Nome parlamentar | 100.0% |
| `dt_nascimento` | data ISO | Data de nascimento (Senado) | 100.0% |
| `sexo` | texto | Sexo (Senado) | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `legislatura` | inteiro | Primeira legislatura do mandato | 100.0% |
| `leg_segunda` | inteiro | Segunda legislatura do mandato (mandato de 8 anos) | 100.0% |
| `ano_eleicao_ref` | inteiro | Ano da eleicao de referencia | 100.0% |
| `codigo_mandato` | inteiro | Codigo do mandato no Senado | 100.0% |
| `condicao` | texto | titular ou suplente | 100.0% |
| `participacao` | texto | Descricao de participacao (Senado) | 100.0% |
| `titular_codigo` | inteiro | Codigo do titular, para suplentes | 36.8% |
| `titular_nome` | texto | Nome do titular, para suplentes | 36.8% |
| `codigo_exercicio` | inteiro | Codigo do exercicio | 100.0% |
| `data_inicio_exercicio` | data ISO | Inicio do exercicio | 100.0% |
| `data_fim_exercicio` | data ISO | Fim do exercicio; NA se em curso | 88.5% |
| `sigla_causa` | texto | Sigla da causa de afastamento (Senado) | 88.4% |
| `causa_afastamento` | texto | Descricao da causa de afastamento (Senado) | 88.4% |
| `forma_saida` | texto | Forma de saida no vocabulario fechado | 88.5% |
| `metodo_pareamento` | texto | Regra que pareou com o BOCEL | 70.2% |
| `fonte` | texto | senado_api | 100.0% |
| `url_fonte` | texto | URL do recurso na API | 100.0% |

## camara_biografia_eventos.csv — eventos de mandato lidos na biografia oficial do deputado federal na Camara (4.969 linhas)

A chave declarada é `id_deputado_camara` × `secao` × `leg_inicio` × `tipo` × `datas`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_deputado_camara` | inteiro | Id do deputado na API da Camara | 100.0% |
| `secao` | texto | Secao da biografia em que o evento apareceu | 100.0% |
| `leg_inicio` | inteiro | Legislatura a que o evento se refere | 92.9% |
| `tipo` | texto | Tipo do evento (posse, licenca, renuncia, perda_mandato, aposentadoria, afastamento, suplencia, falecimento) | 100.0% |
| `datas` | texto | Datas do evento, no texto da biografia | 98.6% |
| `trecho` | texto | Excerto da pagina que sustenta a leitura | 100.0% |
| `url` | texto | URL da biografia | 100.0% |
| `ano_eleicao` | inteiro | Eleicao do BOCEL correspondente a legislatura | 92.9% |

## camara_biografia_posses.csv — data de posse do deputado federal por legislatura, na biografia oficial da Camara (5.298 linhas)

A chave declarada é `id_deputado_camara` × `leg_inicio`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_deputado_camara` | inteiro | Id do deputado na API da Camara | 100.0% |
| `leg_inicio` | inteiro | Legislatura da posse | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `data_posse` | data ISO | Data de posse na biografia | 100.0% |
| `url` | texto | URL da biografia | 100.0% |
| `ano_eleicao` | inteiro | Eleicao do BOCEL correspondente | 100.0% |

## interregnos_legislativo_federal.csv — periodos fora do exercicio de senador e deputado federal que nao encerram o mandato (licenca, afastamento, suspensao) (1.316 linhas)

A chave declarada é `id_mandato` × `inicio_fora`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_mandato` | texto | Mandato do BOCEL (cd_cargo 5 ou 6) | 100.0% |
| `id_pessoa` | texto | Pessoa | 100.0% |
| `casa` | texto | senado ou camara | 100.0% |
| `cargo` | texto | Cargo | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `ano_eleicao` | inteiro | Eleicao do mandato | 100.0% |
| `tipo` | texto | Causa do afastamento no vocabulario da fonte (licenca, afastamento, suspensao) | 100.0% |
| `causa_original` | texto | Texto original da causa na fonte | 100.0% |
| `inicio_fora` | data ISO | Inicio do periodo fora do exercicio | 100.0% |
| `fim_fora` | data ISO | Fim do periodo, quando a casa registra a volta ou o mandato ja acabou | 98.5% |
| `retorno_observado` | logico | A casa registrou o retorno ao exercicio (logico) | 100.0% |
| `fonte` | texto | senado_api ou camara_api | 100.0% |
| `url_fonte` | texto | URL do recurso na API | 100.0% |

## saida_legislativo_federal.csv — forma de saida de senador e deputado federal pela historia completa de exercicio na propria casa (uma linha por mandato) (3.861 linhas)

A chave declarada é `id_mandato`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_mandato` | texto | Mandato do BOCEL (cd_cargo 5 ou 6) | 100.0% |
| `casa` | texto | senado ou camara | 100.0% |
| `cargo` | texto | Cargo | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `ano_eleicao` | inteiro | Eleicao do mandato | 100.0% |
| `data_posse` | data ISO | Data de posse | 99.7% |
| `data_fim_efetiva` | data ISO | Data do ato que encerrou o mandato, ou do fim convencional | 85.5% |
| `precisao_data_fim` | texto | Precisao da data (ato ou convencional) | 85.5% |
| `forma_saida` | texto | Forma de saida no vocabulario fechado (NA quando em curso) | 85.5% |
| `causa_original` | texto | Texto original da causa na fonte | 85.5% |
| `em_curso` | logico | Mandato em curso na data de referencia (logico) | 100.0% |
| `afastado_no_fim` | logico | O titular estava licenciado ou afastado no fim regular do mandato, sem ter voltado (logico; NA quando nao se aplica) | 72.0% |
| `cobertura` | texto | Fonte que resolveu o mandato, entre historico_da_casa, sem_historico_na_api, biografia_oficial_camara, biografia_sem_posse_na_legislatura e fonte_oficial_curada | 100.0% |
| `fonte` | texto | senado_api, camara_api ou fonte_oficial_curada | 100.0% |
| `url_fonte` | texto | URL da fonte | 100.0% |

## saida_executivos.csv — forma de saida de presidente, vice-presidente, governador e vice-governador (uma linha por mandato) (392 linhas)

A chave declarada é `id_mandato`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_mandato` | texto | Mandato do BOCEL (cd_cargo 1 a 4) | 100.0% |
| `cargo` | texto | Cargo | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `ano_eleicao` | inteiro | Eleicao do mandato | 100.0% |
| `forma_saida` | texto | Forma de saida no vocabulario fechado (NA quando em curso) | 91.3% |
| `data_fim_efetiva` | data ISO | Data da saida ou do fim convencional | 91.3% |
| `precisao_data_fim` | texto | Precisao da data (ato, convencional ou intervalo) | 91.3% |
| `em_curso` | logico | Mandato em curso na data de referencia (logico) | 100.0% |
| `via` | texto | Como a forma foi estabelecida, entre evento_curado (ref/eventos_governos_fonte_oficial.csv e ref/eventos_presidencia_fonte_oficial.csv), listas_wikipedia_e_wikidata, lista_wikipedia, derivado_do_titular_curado, em_curso_sem_evento e sem_confirmacao | 100.0% |
| `confianca` | texto | alta ou media, conforme a via | 91.8% |
| `fonte` | texto | fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada, pista_nao_oficial ou wikipedia (lib/tipo_fonte.R) | 91.8% |
| `url_fonte` | texto | URL da primeira fonte citada no evento curado, ou da pagina da Wikipedia | 91.8% |
| `fonte_2` | texto | URL da segunda fonte citada no evento curado, quando ha | 37.8% |
| `assumiu` | texto | Nome de quem assumiu o cargo no ato, quando o evento curado o registra | 22.4% |
| `observacao` | texto | Nota da curadoria sobre o evento, quando ha | 41.3% |

## ocupantes_legislativo_federal.csv — quem a casa registrou como titular de senado ou camara sem ter mandato correspondente no arquivo do TSE (323 linhas)

A chave declarada é `casa` × `sg_uf` × `legislatura` × `nome_alt` × `data_inicio`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `casa` | texto | senado ou camara | 100.0% |
| `cd_cargo` | inteiro | Codigo do cargo (5 ou 6) | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `legislatura` | inteiro | Legislatura | 100.0% |
| `id_pessoa` | texto | Pessoa no BOCEL, quando identificada | 75.9% |
| `nome_fonte` | texto | Nome civil na fonte | 100.0% |
| `nome_alt` | texto | Nome parlamentar na fonte | 100.0% |
| `efetivado` | logico | Suplente efetivado como titular, na Camara (logico) | 100.0% |
| `data_inicio` | data ISO | Inicio do periodo de exercicio | 95.0% |
| `data_fim` | data ISO | Fim do periodo de exercicio | 89.2% |
| `causa_original` | texto | Causa de saida no texto da fonte | 89.2% |
| `fonte` | texto | senado_api ou camara_api | 100.0% |
| `url_fonte` | texto | URL do recurso na API | 100.0% |
| `tipo_ocupante` | texto | suplente_efetivado, ou o tipo atribuido pela curadoria de ref/ocupantes_federais_fonte_oficial.csv (NA quando a cadeira nao foi identificada) | 100.0% |
| `id_mandato_cadeira` | texto | Cadeira do BOCEL ocupada, quando a curadoria a identifica | 15.2% |
| `fonte_oficial` | texto | Fonte oficial do ato, quando curada | 15.2% |
| `url_oficial` | texto | URL da fonte oficial, quando curada | 0.0% |

## ocupacoes.csv — quem ocupou cada cadeira (titular, suplente convocado, vice que assumiu, interino) (6.287 linhas)

A chave declarada é `id_ocupacao`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_ocupacao` | texto | Identificador da ocupacao | 100.0% |
| `id_mandato` | texto | Cadeira ocupada; NA quando a fonte nao permite identificar qual foi | 74.6% |
| `id_pessoa` | texto | Pessoa que ocupou (cadastro do nucleo ou de suplentes) | 97.7% |
| `tipo_ocupante` | texto | titular, suplente, vice_assumiu ou interino | 100.0% |
| `ordem_ocupacao` | inteiro | 1 no titular, 2 em diante nos demais | 74.6% |
| `id_lista` | texto | Lista partidaria de origem do suplente (coligacao, federacao ou partido) | 8.3% |
| `ano_eleicao` | inteiro | Eleicao que criou a cadeira | 100.0% |
| `cd_cargo` | inteiro | Codigo do cargo (TSE) | 100.0% |
| `cargo` | texto | Cargo | 100.0% |
| `esfera` | texto | federal, estadual ou municipal | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `sg_ue` | texto | Unidade eleitoral nos cargos municipais | 67.6% |
| `nm_ue` | texto | Nome da unidade | 67.6% |
| `unidade_posicao` | texto | Unidade em que a posicao e disputada | 100.0% |
| `sg_partido_ocupante` | texto | Partido de quem ocupou | 91.6% |
| `sg_partido_titular` | texto | Partido do titular eleito para a cadeira | 74.6% |
| `data_inicio` | data ISO | Inicio da ocupacao | 96.3% |
| `data_fim` | data ISO | Fim da ocupacao | 86.1% |
| `origem_data_inicio` | texto | fonte quando ha ato ou registro, convencao quando e a data legal do mandato | 96.3% |
| `forma_saida` | texto | Forma de saida da ocupacao, no vocabulario fechado | 94.4% |
| `fonte_forma_saida` | texto | Fonte da forma de saida | 90.6% |
| `vinculo_cadeira` | texto | Caminho pelo qual a cadeira foi identificada, entre eleicao, chapa_senado, cadeira_unica_da_unidade, lista_vaga_unica, lista_data ou casa_legislatura (nao identificada) | 100.0% |
| `fonte` | texto | Fonte do registro de ocupacao | 100.0% |
| `url` | texto | URL de origem | 32.4% |
| `ordem_suplencia` | inteiro | Posicao do ocupante na fila da lista, quando suplente | 8.3% |
| `regra_pareamento_ocupacao` | texto | Regra que ligou o nome da fonte a pessoa da lista de suplencia | 8.3% |
| `partido_difere_do_titular` | logico | A cadeira trocou de partido por dentro, sem eleicao (logico) | 4.1% |
| `datas_inconsistentes` | logico | A fonte publicou fim anterior ao inicio (logico); a linha nao foi corrigida | 100.0% |

## lista_suplencia.csv — a fila de suplencia de cada lista partidaria (24.706 linhas)

A chave declarada é `id_suplencia`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_suplencia` | texto | Identificador da suplencia | 100.0% |
| `chave_cand` | texto | Chave da candidatura no TSE | 100.0% |
| `id_pessoa` | texto | Pessoa (mesmo espaco de identificador do nucleo) | 100.0% |
| `ano_eleicao` | inteiro | Eleicao | 100.0% |
| `cd_cargo_registro` | inteiro | Cargo do registro no TSE (9 e 10 sao os suplentes da chapa do Senado) | 100.0% |
| `cargo_registro` | texto | Cargo do registro | 100.0% |
| `cd_cargo` | inteiro | Cargo da cadeira que a suplencia alcanca | 100.0% |
| `cargo` | texto | Cargo da cadeira | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `sg_ue` | texto | Unidade eleitoral | 100.0% |
| `nm_ue` | texto | Nome da unidade | 100.0% |
| `unidade_posicao` | texto | Unidade em que a posicao e disputada | 100.0% |
| `id_lista` | texto | Lista partidaria, seja coligacao, federacao ou partido isolado, pelo SQ_COLIGACAO do TSE | 100.0% |
| `nr_partido` | inteiro | Numero do partido | 100.0% |
| `sg_partido` | texto | Partido | 100.0% |
| `tp_agremiacao` | texto | Tipo de agremiacao | 100.0% |
| `nm_coligacao` | texto | Nome da coligacao | 100.0% |
| `composicao_coligacao` | texto | Composicao da coligacao | 100.0% |
| `nome` | texto | Nome do candidato | 100.0% |
| `nome_urna` | texto | Nome de urna | 99.4% |
| `nr_candidato` | inteiro | Numero na urna | 100.0% |
| `sq_candidato` | inteiro | Sequencial da candidatura | 100.0% |
| `votos_nominais` | inteiro | Votacao nominal | 97.9% |
| `ordem_suplencia` | inteiro | Posicao na fila da lista, derivada da votacao com desempate pelo mais idoso | 100.0% |
| `ordem_suplencia_tse` | texto | Ordem publicada pelo TSE, existente so em 2016 | 0.0% |
| `fonte_ordem` | texto | derivada_votacao, tse_e_derivada ou chapa_senado | 100.0% |
| `empate_na_ordem` | logico | Houve empate de votos e nascimento na lista (logico) | 100.0% |
| `pessoa_tambem_eleita` | logico | A pessoa foi eleita em alguma eleicao do banco (logico) | 100.0% |
| `id_mandato_cadeira` | texto | Cadeira que a suplencia alcanca, quando a chapa a nomeia | 2.1% |

## pessoas_suplentes.csv — pessoas que figuram como suplentes e nunca foram eleitas (19.095 linhas)

A chave declarada é `id_pessoa`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Identificador no mesmo espaco de pessoas.csv, sem colisao | 100.0% |
| `nome` | texto | Nome | 100.0% |
| `nome_urna_recente` | texto | Nome de urna mais recente | 96.6% |
| `dt_nascimento` | data ISO | Nascimento | 99.8% |
| `genero` | texto | Genero | 100.0% |
| `nr_titulo_eleitoral` | texto | Titulo de eleitor | 99.8% |
| `nr_cpf` | inteiro | CPF | 99.6% |
| `n_candidaturas_suplente` | inteiro | Candidaturas em que ficou como suplente | 100.0% |
| `primeiro_ano_suplente` | inteiro | Primeira eleicao | 100.0% |
| `ultimo_ano_suplente` | inteiro | Ultima eleicao | 100.0% |
| `chave_dedup` | texto | Chave que identificou a pessoa (titulo, cpf ou nome_nascimento) | 100.0% |
| `condicao_no_banco` | texto | suplente_nao_eleito | 100.0% |

## suplentes_identidade.csv — candidatura de suplente e a pessoa a que pertence (28.038 linhas)

A chave declarada é `chave_cand`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `chave_cand` | texto | Chave da candidatura | 100.0% |
| `id_pessoa` | texto | Pessoa | 100.0% |
| `ano_eleicao` | inteiro | Eleicao | 100.0% |
| `cd_cargo` | inteiro | Cargo | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `sg_ue` | texto | Unidade eleitoral | 100.0% |
| `sg_partido` | texto | Partido | 100.0% |
| `regra_id` | texto | Regra que atribuiu a pessoa, entre nucleo, titulo, cpf, nome_nascimento, componente e novo | 100.0% |
| `pessoa_tambem_eleita` | logico | A pessoa ja existia no cadastro do nucleo (logico) | 100.0% |

## eleicoes_suplementares.csv — pleitos suplementares majoritarios do TSE (novas eleicoes apos cassacao ou anulacao) (5 linhas)

A chave declarada é `id_pleito`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pleito` | texto | Identificador do pleito (S<ano>_<unidade>_<cargo>_<data>) | 100.0% |
| `ano_arquivo` | inteiro | Ano do arquivo do TSE em que o pleito consta | 100.0% |
| `unidade_posicao` | texto | Unidade da posicao | 100.0% |
| `sg_uf` | texto | UF | 100.0% |
| `sg_ue` | texto | Codigo TSE da unidade eleitoral | 100.0% |
| `nm_ue` | texto | Nome da unidade | 100.0% |
| `cd_cargo` | inteiro | Codigo do cargo | 100.0% |
| `cargo` | texto | Cargo | 100.0% |
| `esfera` | texto | Esfera | 100.0% |
| `dt_eleicao_suplementar` | data ISO | Data do 1o turno do pleito | 100.0% |
| `dt_turno_decisivo` | data ISO | Data do turno decisivo | 100.0% |
| `nr_turno` | inteiro | Turno decisivo | 100.0% |
| `n_candidatos` | inteiro | Candidaturas ao cargo titular no pleito | 100.0% |
| `status_vencedor` | texto | vencedor_identificado ou motivo da ausencia | 100.0% |
| `vencedor_nome` | texto | Nome do vencedor | 60.0% |
| `vencedor_titulo` | inteiro | Titulo de eleitor do vencedor | 60.0% |
| `vencedor_cpf` | inteiro | CPF do vencedor | 60.0% |
| `vencedor_id_pessoa` | texto | id_pessoa do vencedor, se ja consta do BOCEL | 60.0% |
| `votos_vencedor` | inteiro | Votos do vencedor no turno decisivo | 60.0% |
| `sq_candidato` | inteiro | SQ_CANDIDATO do vencedor | 60.0% |
| `nr_candidato` | inteiro | Numero do vencedor | 60.0% |
| `sg_partido_vencedor` | texto | Partido do vencedor | 60.0% |
| `situacao_totalizacao` | texto | Situacao de totalizacao do vencedor | 60.0% |
| `fonte_situacao` | texto | Origem da situacao (cadastro ou votacao) | 60.0% |
| `vice_nome` | texto | Nome do vice eleito no pleito | 40.0% |
| `vice_titulo` | inteiro | Titulo do vice | 40.0% |
| `vice_cpf` | inteiro | CPF do vice | 40.0% |
| `vice_id_pessoa` | texto | id_pessoa do vice, se consta do BOCEL | 40.0% |
| `id_mandato_ordinario_afetado` | texto | Mandato ordinario vigente na data do pleito | 60.0% |
| `ocupante_ordinario_id_pessoa` | texto | Pessoa do mandato ordinario afetado | 60.0% |
| `dt_dentro_mandato_ordinario` | logico | Data do pleito cai na janela do mandato ordinario (logico) | 100.0% |
| `momento` | texto | antes_da_posse ou durante_o_mandato | 100.0% |
| `vencedor_e_o_ocupante_ordinario` | logico | O vencedor e a mesma pessoa do mandato ordinario (logico) | 40.0% |
| `mandatos_ordinarios_candidatos` | texto | Mandatos ordinarios candidatos ao pareamento | 40.0% |
| `realizado_ate_data_do_arquivo` | logico | Pleito realizado ate a data de geracao do arquivo do TSE (logico) | 100.0% |
| `chave_colide_com_ordinaria` | logico | Alguma candidatura do pleito tem chave igual a de candidatura ordinaria do mesmo ano (SQ reiniciado ate 2008; logico) | 100.0% |
| `vencedor_chave_colide_com_ordinaria` | logico | A chave do vencedor colide com candidatura ordinaria (logico) | 60.0% |
| `fonte` | texto | tse_consulta_cand+votacao | 100.0% |

## mandatos_forma_saida_suplementar.csv — mandato ordinario afetado por eleicao suplementar, com o sucessor (3 linhas)

A chave declarada é `id_mandato_ordinario_afetado`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_mandato_ordinario_afetado` | texto | Mandato ordinario do BOCEL | 100.0% |
| `forma_saida` | texto | Forma de saida inferida | 100.0% |
| `momento` | texto | durante_o_mandato ou antes_da_posse | 100.0% |
| `data_fim_inferida` | data ISO | Vespera do pleito suplementar | 100.0% |
| `id_pleito_suplementar` | texto | Pleito suplementar (eleicoes_suplementares.csv) | 100.0% |
| `sucessor_via_suplementar` | texto | id_pessoa do vencedor do pleito | 66.7% |
| `sucessor_nome` | texto | Nome do vencedor | 66.7% |
| `sucessor_titulo` | inteiro | Titulo de eleitor do vencedor | 66.7% |
| `via_sucessao` | texto | Via da sucessao | 100.0% |
| `id_pleito_sucessor` | texto | Pleito que deu o sucessor | 100.0% |
| `dt_pleito_sucessor` | data ISO | Data desse pleito | 100.0% |
| `status_vencedor` | texto | vencedor_identificado ou sem_vencedor_marcado_no_tse | 100.0% |
| `n_pleitos_suplementares` | inteiro | Pleitos suplementares na mesma unidade e mandato | 100.0% |
| `vencedor_e_o_ocupante_ordinario` | logico | O vencedor e a mesma pessoa do mandato afetado (logico) | 66.7% |
| `fonte` | texto | Arquivos do TSE usados | 100.0% |

## pessoas_flags_dedup.csv — marcas da auditoria de homonimos sobre a deduplicacao de pessoas (2.155 linhas)

A chave declarada é `id_pessoa`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Pessoa no BOCEL | 100.0% |
| `ponte_nome_nascimento` | logico | A pessoa une componentes so por nome e nascimento (logico) | 100.0% |
| `so_nome_nascimento` | logico | A identificacao depende so de nome e nascimento (logico) | 100.0% |
| `classificacao` | texto | nao_auditado, consistente ou suspeito | 100.0% |

## auditoria_homonimos.csv — pessoas cuja identificacao depende so de nome e nascimento (0 linhas)

A chave declarada é `id_pessoa`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `id_pessoa` | texto | Pessoa | NaN% |
| `n_mandatos` | texto | Mandatos da pessoa | NaN% |
| `n_titulos_distintos` | texto | Titulos de eleitor distintos na componente | NaN% |
| `n_cpfs_distintos` | texto | CPFs distintos na componente | NaN% |
| `n_sem_titulo_nem_cpf` | texto | Candidaturas sem titulo nem CPF | NaN% |
| `n_comp_12` | texto | Componentes que a pessoa teria so com titulo e CPF | NaN% |
| `ponte_nome_nascimento` | texto | A regra nome+nascimento foi a unica ponte (logico) | NaN% |
| `so_nome_nascimento` | texto | A pessoa so tem nome e nascimento como chave (logico) | NaN% |
| `generos` | texto | Generos observados | NaN% |
| `ufs` | texto | UFs observadas | NaN% |
| `n_unidades` | texto | Unidades observadas | NaN% |
| `anos` | texto | Anos observados | NaN% |
| `cargos` | texto | Cargos observados | NaN% |
| `n_nomes_distintos` | texto | Nomes distintos | NaN% |
| `n_nasc_distintos` | texto | Datas de nascimento distintas | NaN% |
| `classificacao` | texto | consistente, suspeito ou indeterminado | NaN% |
| `grau_suspeita` | texto | Grau da suspeita | NaN% |
| `justificativa` | texto | Justificativa da classificacao | NaN% |

## mandatos_lista.csv — cadeira e a lista partidaria que a ganhou (3.591 linhas)

A chave declarada é `id_mandato`.

| variavel | tipo | descricao | preenchimento |
|---|---|---|---|
| `chave_cand` | texto | Chave da candidatura | 100.0% |
| `id_mandato` | texto | Cadeira | 100.0% |
| `id_pessoa` | texto | Titular eleito | 100.0% |
| `cd_cargo` | inteiro | Cargo | 100.0% |
| `ano_eleicao` | inteiro | Eleicao | 100.0% |
| `unidade_posicao` | texto | Unidade | 100.0% |
| `sg_partido` | texto | Partido do titular | 100.0% |
| `forma_saida` | texto | Forma de saida do titular | 100.0% |
| `data_fim_efetiva` | data ISO | Fim efetivo do mandato do titular | 86.5% |
| `data_posse` | data ISO | Posse do titular | 99.7% |
| `mandato_inicio` | data ISO | Inicio convencional | 100.0% |
| `mandato_fim` | data ISO | Fim convencional | 100.0% |
| `id_lista` | texto | Lista partidaria | 100.0% |
| `sq_col` | inteiro | SQ_COLIGACAO do TSE | 83.8% |
| `NR_PARTIDO` | inteiro | Numero do partido | 100.0% |

## Regra de deduplicação de pessoa

Duas candidaturas pertencem à mesma pessoa quando compartilham (1) o mesmo
título de eleitor válido de 12 dígitos, ou (2) o mesmo CPF válido de 11
dígitos, ou (3) o mesmo par nome completo normalizado (maiúsculas, sem acento,
só letras) e data de nascimento. As três regras se encadeiam por fecho
transitivo (union-find), de modo que, se A e B compartilham título e B e C
compartilham CPF, A, B e C são a mesma pessoa. O `id_pessoa` deriva da chave canônica da
componente (menor título; na ausência, menor CPF; na ausência, nome +
nascimento), o que o mantém estável entre versões do banco. A coluna
`chave_dedup` em pessoas.csv informa qual regra identificou cada pessoa;
o pareamento por nome e nascimento em homônimos sem título nem CPF é a fração
com risco residual de fusão indevida, e está quantificada na nota de cobertura.

