# Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL) — v1.0

Ocupação de cargo eletivo no Brasil, 1998–2024, construída a partir dos dados
abertos do Tribunal Superior Eleitoral. A versão 1.0 fecha a Presidência, a
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

- **Pessoas:** 2.155
- **Mandatos:** 4.253
- **Posições pessoa × cargo × ano:** 18.092
- **Cobertura:** eleições ordinárias de 1998 a 2024, no recorte federal (ver NOTA_DE_COBERTURA.md)

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
do zero num diretório limpo com R 4.3+ (data.table, arrow, stringi,
jsonlite). As bibliotecas de verificação (asserts_rigor.R, proveniencia.R)
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
| pessoas_suplentes.csv/.parquet | suplentes que nunca ocuparam cadeira, no mesmo espaço de identificador do cadastro |
| suplentes_identidade.csv | como cada suplente foi ligado a uma pessoa do banco |
| mandatos_lista.csv | cadeira e a lista partidária que a ganhou |
| saida_executivos.csv | forma de saída de presidente, vice-presidente, governador e vice-governador |
| saida_legislativo_federal.csv | forma de saída de senador e deputado federal pela história completa de exercício na própria casa |
| interregnos_legislativo_federal.csv | licenças, afastamentos e suspensões de senador e deputado federal que não encerram o mandato |
| mandatos_forma_saida_suplementar.csv | mandato ordinário afetado por eleição suplementar e o sucessor |
| eleicoes_suplementares.csv | pleitos suplementares majoritários do TSE (1998–2024), vencedor e mandato ordinário afetado |
| ocupantes_legislativo_federal.csv | quem a Câmara ou o Senado registrou como titular sem mandato correspondente no arquivo do TSE |
| exercicio_camara.csv | períodos de exercício dos deputados federais (API da Câmara), com posse, saída e causa |
| exercicio_senado.csv | exercícios de senadores e suplentes (API do Senado), com causa de afastamento |
| camara_biografia_eventos.csv | eventos de mandato lidos na biografia oficial do deputado federal na Câmara |
| camara_biografia_posses.csv | data de posse do deputado federal por legislatura, na biografia oficial da Câmara |
| auditoria_homonimos.csv | pessoas cuja identificação depende só de nome e nascimento, com classificação |
| pessoas_flags_dedup.csv | marcas da auditoria de homônimos sobre a deduplicação de pessoas |
| LIVRO_DE_CODIGOS.md | nome, tipo, descrição e preenchimento de cada variável |
| LIVRO_DE_CODIGOS.csv / .xlsx | o mesmo livro em formato tabular, com nível de medida e a fonte de cada variável |
| NOTA_DE_COBERTURA.md | o que está completo, parcial e ausente |
| EXCECOES_CONHECIDAS.csv | divergências documentadas em relação às cadeiras esperadas |
| reconstruir_banco.zip | scripts, bibliotecas de verificação e tabelas de referência que reconstroem o banco a partir dos dados brutos |

## Como citar

> Lins, Igor Novaes. (2026). Banco de Ocupação de Cargos Eletivos no Brasil / Brazilian
> Elective Office Occupancy Database (BOCEL), 1998–2024 (v1.0) [Conjunto de dados].
> Zenodo. DOI atribuído no momento do depósito.

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

v1.0, 21 de setembro de 2026. Correções entram como versões novas no mesmo
registro do Zenodo, sob o mesmo concept DOI.

## Contato

Igor Novaes Lins — igornovaeslins@gmail.com — ORCID 0000-0003-0510-8355

