# Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL) — v1.2

Ocupação de cargo eletivo no Brasil, 1998–2024, construída a partir dos dados
abertos do Tribunal Superior Eleitoral. A versão 1.2 fecha a Presidência, a
Câmara dos Deputados, o Senado e os governos estaduais, em que todo mandato
encerrado tem forma de saída com a fonte identificada, e traz a ocupação de
cada cadeira com a ordem da suplência e o histórico de filiação partidária de
cada pessoa. As Assembleias Legislativas e os cargos municipais não entram
nesta versão e ficam para a v1.5 e a v2.0. A unidade de observação é pessoa ×
cargo × ano (posicoes_ano.csv), e o banco traz também a tabela de mandatos
(mandatos.csv) e o cadastro de pessoas com identificador estável entre
eleições (pessoas.csv).

### Para que serve

O BOCEL permite acompanhar a trajetória de cada pessoa eleita no Brasil desde
1998, com o cargo que ela ocupou em cada ano, o partido pelo qual se elegeu,
as filiações que teve antes e depois e a forma como deixou cada mandato. Com
ele se reconstitui a carreira de um político, se descobre quem exerceu a
cadeira quando o titular se licenciou, renunciou ou foi cassado, e se liga o
histórico partidário de cada pessoa aos cargos que ocupou.

- **Pessoas:** 2.424
- **Mandatos:** 4.253
- **Posições pessoa × cargo × ano:** 18.092
- **Cobertura:** eleições ordinárias de 1998 a 2022, no recorte federal (ver NOTA_DE_COBERTURA.md)

## Fonte

Os arquivos de candidaturas do TSE (consulta_cand, 1998–2024) vêm do Portal de
Dados Abertos, baixados pelas URLs do manifesto em
R/coleta/manifest_consulta_cand.txt; R/coleta/conferir_brutos_tse.R confere o
hash md5 de cada zip contra ref/fontes_brutas_tse.csv antes da reconstrução
prosseguir, porque o CDN do TSE recusa requisição de R e de curl e o download
é sempre manual, pelo navegador. A filiação partidária vem das listas de
filiados do TSE (Sistema FILIA), lidas na Base dos Dados, nas tabelas
`microdados` e `microdados_antigos` do conjunto `br_tse_filiacao_partidaria`
(https://basedosdados.org/dataset/e68b1cfd-ca29-4bdc-b687-424d9c59760e). A
Base dos Dados pede, na página https://basedosdados.org/faq, a citação pelo
nome do projeto ou pelo artigo abaixo.

> Dahis, Ricardo; Carabetta, João; Scovino, Fernanda; Israel, Frederico;
> Oliveira, Diego. Data Basis: Universalizing Access to High-Quality Data.
> SocArXiv, 5 jul. 2022. DOI: 10.31235/osf.io/r76yg.

A camada de posse, exercício e saída soma sete coletores em R, cada um lendo
uma API ou página pública e gravando o retorno sem alteração de conteúdo.

| coletor | fonte |
|---|---|
| R/coleta/camara.R | API de Dados Abertos da Câmara dos Deputados (dadosabertos.camara.leg.br) |
| R/coleta/camara_biografia.R | Biografia oficial do deputado federal (camara.leg.br/deputados) |
| R/coleta/camara_composicao_atual.R | Composição atual da Câmara (dadosabertos.camara.leg.br/api/v2/deputados) |
| R/coleta/senado.R | API de Dados Abertos do Senado Federal (legis.senado.leg.br/dadosabertos) |
| R/coleta/wikipedia_listas.R | Listas de governadores e vice-governadores na Wikipédia em português (pt.wikipedia.org) |
| R/coleta/wikidata.R | Mandatos dos chefes do Executivo e de seus vices no Wikidata (query.wikidata.org/sparql) |
| R/coleta/divulgacand_reeleicao.R | Situação de candidatura seguinte na API DivulgaCandContas do TSE (divulgacandcontas.tse.jus.br) |

O script de reconstrução acompanha o depósito (reconstruir_banco.zip) e roda
do zero num diretório limpo com R 4.3+ (data.table, arrow, stringi, httr2,
jsonlite, openxlsx). As bibliotecas de verificação (asserts_rigor.R, proveniencia.R)
acompanham o pacote em lib/.

## Arquivos

| arquivo | conteúdo |
|---|---|
| mandatos.csv/.parquet | pessoa × cargo × mandato, com partido, votos e sucessão executiva |
| posicoes_ano.csv/.parquet | painel pessoa × cargo × ano |
| pessoas.csv/.parquet | cadastro de pessoas, identificador estável e regra de deduplicação |
| filiacoes.csv/.parquet | histórico de filiação partidária das pessoas do banco (listas do TSE) |
| ocupacoes.csv/.parquet | quem ocupou cada cadeira e em que período, com o titular, o suplente convocado, o vice que assumiu e o interino |
| lista_suplencia.csv/.parquet | a fila de suplência de cada lista partidária de deputado federal e de senador, com a ordem derivada |
| pessoas_suplentes.csv/.parquet | pessoas que figuram como suplentes e nunca foram eleitas, no mesmo espaço de identificador do cadastro, entre elas os ocupantes de cadeira que não constam de pessoas.csv |
| suplentes_identidade.csv | como cada suplente foi ligado a uma pessoa do banco |
| mandatos_lista.csv | cadeira e a lista partidária que a ganhou |
| saida_executivos.csv | forma de saída de presidente, vice-presidente, governador e vice-governador |
| saida_legislativo_federal.csv | forma de saída de senador e deputado federal pela história completa de exercício na própria casa |
| interregnos_legislativo_federal.csv | licenças, afastamentos e suspensões de senador e deputado federal que não encerram o mandato |
| mandatos_forma_saida_suplementar.csv | mandato ordinário afetado por eleição suplementar e o sucessor |
| eleicoes_suplementares.csv | pleitos suplementares majoritários do TSE (2017–2026), vencedor e mandato ordinário afetado |
| ocupantes_legislativo_federal.csv | quem a Câmara ou o Senado registrou como titular sem mandato correspondente no arquivo do TSE |
| exercicio_camara.csv | períodos de exercício dos deputados federais (API da Câmara), com posse, saída e causa |
| exercicio_senado.csv | exercícios de senadores e suplentes (API do Senado), com causa de afastamento |
| camara_biografia_eventos.csv | eventos de mandato lidos na biografia oficial do deputado federal na Câmara |
| camara_biografia_posses.csv | data de posse do deputado federal por legislatura, na biografia oficial da Câmara |
| auditoria_homonimos.csv | pessoas cuja identificação depende só de nome e nascimento, com classificação |
| pessoas_flags_dedup.csv | marcas da auditoria de homônimos sobre a deduplicação de pessoas |
| ids_pessoa_referencia.parquet | número de id_pessoa atribuído a cada candidatura (chave_cand), que mantém o identificador de uma pessoa quando o banco é refeito; cobre as candidaturas do banco inteiro, inclusive as de fora do recorte da v1.0, porque o mesmo número vale para as versões seguintes |
| LIVRO_DE_CODIGOS.md | nome, tipo, descrição e preenchimento de cada variável |
| LIVRO_DE_CODIGOS.csv / .xlsx | o mesmo livro em formato tabular, com nível de medida e a fonte de cada variável |
| NOTA_DE_COBERTURA.md | o que está completo, parcial e ausente |
| EXCECOES_CONHECIDAS.csv | divergências documentadas em relação às cadeiras esperadas |
| reconstruir_banco.zip | scripts, bibliotecas de verificação e tabelas de referência que reconstroem o banco a partir dos dados brutos |

## Como citar

> Lins, Igor Novaes. (2026). Banco de Ocupação de Cargos Eletivos no Brasil / Brazilian
> Elective Office Occupancy Database (BOCEL), 1998–2024 (v1.2) [Conjunto de dados].
> Zenodo. https://doi.org/10.5281/zenodo.24432652

## Fontes complementares da camada de posse e saída

Câmara dos Deputados (API de Dados Abertos), Senado Federal (API de Dados
Abertos), a biografia oficial do deputado federal na página da Câmara,
Wikidata (CC0) e listas da Wikipédia como cruzamento auxiliar da saída dos
chefes do Executivo e de seus vices, e eleições
suplementares nos arquivos do TSE. Cada mandato identifica a fonte que
preencheu a forma de saída (`fonte_forma_saida`) e a que confirmou o
exercício (`fonte_exercicio`).

## Licença e norma de uso

Os dados estão sob a licença Creative Commons Atribuição 4.0 Internacional
(CC BY 4.0), que permite copiar, redistribuir, adaptar e reutilizar o banco para
qualquer fim, desde que o trabalho que o use cite o autor na forma indicada acima,
na seção de como citar. Os scripts de reconstrução em `reconstruir_banco.zip`
seguem a mesma licença.

Os dados de origem são públicos (transparência ativa do TSE, dados abertos da
Câmara dos Deputados e do Senado Federal, e do Wikidata), e o banco não
acrescenta dado pessoal além do que essas fontes divulgam.

## Versão

v1.2, 22 de novembro de 2026. Correções entram como versões novas no mesmo
registro do Zenodo, sob o mesmo concept DOI (https://doi.org/10.5281/zenodo.22896271).

### Mudanças da v1.2 em relação à v1.1

A v1.2 completa o partido de quem ocupou uma cadeira de deputado federal sem ser o titular eleito e
não muda o recorte. Quando a fonte da ocupação não traz a sigla, `sg_partido_ocupante` passa a
receber o partido em que o cadastro de filiação do TSE mostra a pessoa filiada no início da
ocupação, o que preencheu 242 ocupações, e, quando a filiação não aponta um só
partido, o partido pelo qual a pessoa concorreu na eleição que criou a cadeira, em mais 61. Seguem sem partido 41 ocupações. Duas colunas novas acompanham
a mudança, `origem_partido_ocupante`, que registra de onde veio cada sigla, e
`sg_partido_candidatura`. A marca `partido_difere_do_titular` passou a valer também para o
suplente efetivado e registra troca de partido em 26 deles. Nenhuma outra
coluna e nenhum identificador mudaram, e as demais tabelas seguem iguais às da v1.1.

### Mudanças da v1.1 em relação à v1.0

A v1.1 corrige o recorte da v1.0 sem ampliá-lo. `pessoas.csv` passou a incluir, além dos eleitos no recorte, as
pessoas que só ocuparam uma cadeira nele sem terem sido eleitas ali, com a condição de cada pessoa marcada na coluna
`condicao_no_recorte`, e o identificador de todo ocupante de `ocupacoes.csv` resolve em `pessoas.csv` ou em
`pessoas_suplentes.csv`, que guarda os 304 ocupantes nunca eleitos em nenhuma esfera do banco. `posicoes_ano.csv` ganhou a
coluna `exerceu_no_ano`, que separa o ano em que o titular esteve de fato na cadeira do ano em que só ocupava o cargo no papel.

Em `ocupacoes.csv`, `data_fim` deixou de ultrapassar o fim do próprio mandato, o que reduziu a data de fim de 52 ocupações (todas de vice que assumiu o governo de estado), e a nova coluna `origem_data_fim` registra a
origem de cada data de saída. O vínculo entre o suplente e a cadeira passou a seguir uma regra geral para as cadeiras federais
cujo titular não exerceu o mandato desde o começo da legislatura, que ligou 6 ocupações a cadeira nova (5 em `lista_vaga_inicial` e 1 em `lista_sucessao_vaga`, dois valores
novos de `vinculo_cadeira`). Em AL 1998, as datas de entrada de 2 suplentes e a data de saída de 1 deles vêm da biografia oficial da
Câmara (a saída de 1 deles segue a convenção de fim de legislatura), e `origem_data_inicio` ganhou o valor `biografia_camara` para registrá-lo (o valor `sinal_efetivacao_api` está definido no livro de códigos e não ocorre nesta versão). A primeira ocupação de José Valdevan de Jesus Santos (SE 2018) foi fechada em 27 de abril de 2022 pela mesma fonte (`origem_data_fim` igual a `curadoria_biografia_camara`), e nenhum par de ocupações sobrepostas reúne agora a
mesma pessoa. Os 20 pares de ocupações de suplentes que ainda compartilham mais de 3 dias da mesma cadeira estão documentados. Em 14 deles um suplente em exercício se licenciou e outro cobriu a licença,
e os dados ficam como vieram da fonte. Os outros 6 constam linha a linha em `EXCECOES_CONHECIDAS.csv`, e a troca da
regra de ligação do par de AL 2010 e a curadoria dos 5 pares de fim de legislatura estão previstas para a v1.5.
As correções curadas de Walter Pinheiro e de Almerinda ganharam remissão em `EXCECOES_CONHECIDAS.csv`. NOTA_DE_COBERTURA.md e
LIVRO_DE_CODIGOS.md documentam cada mudança com o número que a mede. O contato do autor passou a trazer o e-mail institucional,
igor.lins@cebrap.org.br, ao lado do pessoal, igornovaeslins@gmail.com, no README, em CITATION.cff e nos metadados do depósito.

## Contato

Igor Novaes Lins, igor.lins@cebrap.org.br (institucional) e igornovaeslins@gmail.com. ORCID 0000-0003-0510-8355

