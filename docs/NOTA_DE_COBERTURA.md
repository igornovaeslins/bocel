# Nota de cobertura — BOCEL v1.0

## O que está completo

O recorte federal das eleições ordinárias de 1998 a 2024, a saber presidente e
vice-presidente, senador, deputado federal, governador e vice-governador, a
partir dos arquivos de candidaturas do TSE regenerados no layout unificado.
Assembleias Legislativas, câmaras e prefeituras municipais ficam para a v1.5
e a v2.0. A tabela abaixo dá a contagem de mandatos por eleição e esfera; a
taxa de pareamento contra as contagens de referência do TSE está em
output/verificacao/taxa_pareamento.csv do repositório de construção.

| ano da eleição | esfera | mandatos |
|---|---|---|
| 1998 | estadual |  54 |
| 1998 | federal | 542 |
| 2002 | estadual |  54 |
| 2002 | federal | 569 |
| 2006 | estadual |  54 |
| 2006 | federal | 542 |
| 2010 | estadual |  54 |
| 2010 | federal | 569 |
| 2014 | estadual |  54 |
| 2014 | federal | 542 |
| 2018 | estadual |  54 |
| 2018 | federal | 569 |
| 2022 | estadual |  54 |
| 2022 | federal | 542 |

## Posse, exercício e forma de saída

As colunas `data_posse`, `data_fim_efetiva`, `forma_saida`, `fonte_forma_saida`, `exercicio_confirmado` e `fonte_exercicio` consolidam, por ordem de prioridade, o registro institucional (API da Câmara dos Deputados, API do Senado Federal), a biografia oficial do deputado federal na página da Câmara e, para presidente, vice-presidente, governador e vice-governador, a curadoria de fonte oficial (ref/eventos_presidencia_fonte_oficial.csv e ref/eventos_governos_fonte_oficial.csv), cruzada com a Wikidata e com listas da Wikipédia quando não há evento localizado. Por esfera, a proporção de mandatos com forma de saída observada, com data de posse e com exercício confirmado é a seguinte.

| esfera | mandatos | forma de saída observada | com data de posse | exercício confirmado |
|---|---|---|---|---|
| federal | 3.875 | 3.314 (85,5%) | 3.863 (99,7%) | 1.822 (47,0%) |
| estadual | 378 | 346 (91,5%) | 369 (97,6%) | 90 (23,8%) |

Distribuição da forma de saída por esfera.

| esfera | forma_saida | mandatos |
|---|---|---|
| estadual | fim_regular |   204 |
| estadual | renuncia |    66 |
| estadual | assumiu_titular |    56 |
| estadual | nao_observado |    32 |
| estadual | cassacao |    15 |
| estadual | falecimento |     4 |
| estadual | impeachment |     1 |
| federal | fim_regular | 2.973 |
| federal | nao_observado |   561 |
| federal | renuncia |   245 |
| federal | falecimento |    50 |
| federal | cassacao |    27 |
| federal | retotalizacao |     7 |
| federal | nao_tomou_posse |     5 |
| federal | aposentadoria |     5 |
| federal | impeachment |     1 |
| federal | assumiu_titular |     1 |

Fonte da forma de saída observada, por esfera.

| esfera | fonte | mandatos |
|---|---|---|
| estadual | wikipedia |   194 |
| estadual | fonte_oficial_curada |    58 |
| estadual | noticia_orgao_publico_curada |    38 |
| estadual | pista_nao_oficial |    36 |
| estadual | base_dhbb_curada |    20 |
| federal | camara_api | 2.581 |
| federal | camara_biografia |   511 |
| federal | senado_api |   197 |
| federal | fonte_oficial_curada |    23 |
| federal | noticia_orgao_publico_curada |     2 |

O sinal de exercício pelo registro de candidatura à reeleição depende do campo ST_REELEICAO do TSE, cujo preenchimento varia por eleição. A tabela dá, por eleição seguinte e esfera, a proporção de candidatos ao mesmo cargo e unidade marcados como reeleição; onde ela fica abaixo de um décimo, o campo está preenchido sem informação e a confirmação de exercício fica subestimada.

| eleição seguinte | esfera | candidatos ao mesmo cargo | marcados S | proporção | fonte |
|---|---|---|---|---|---|
| 2002 | estadual | 825 | 793 | 96,1% | consulta_cand |
| 2002 | federal | 375 | 362 | 96,5% | consulta_cand |
| 2006 | estadual | 820 | 809 | 98,7% | consulta_cand |
| 2006 | federal | 414 | 405 | 97,8% | consulta_cand |
| 2010 | estadual | 818 |   6 | 0,7% | consulta_cand |
| 2010 | federal | 409 |   1 | 0,2% | consulta_cand |
| 2014 | estadual | 794 | 601 | 75,7% | divulgacand_api |
| 2014 | federal | 371 | 229 | 61,7% | divulgacand_api |
| 2018 | estadual | 793 | 781 | 98,5% | divulgacand_api |
| 2018 | federal | 416 | 404 | 97,1% | divulgacand_api |
| 2022 | estadual | 810 | 761 | 94,0% | divulgacand_api |
| 2022 | federal | 438 | 421 | 96,1% | divulgacand_api |

Na Câmara, o histórico da API não traz eventos de posse e saída para a legislatura 51 (1999–2003), e esses mandatos ficam com `forma_saida = outro` e a situação `listado_sem_historico` em exercicio_camara.csv, parte deles resolvida pela biografia oficial (camara_biografia_eventos.csv e camara_biografia_posses.csv); os mandatos da legislatura 57, em curso, ficam sem forma de saída. No Senado, a saída é a do último exercício registrado. As licenças, afastamentos e suspensões que não encerram o mandato de senador e deputado federal ficam em interregnos_legislativo_federal.csv. A inferência por eleição suplementar marca a perda do mandato do titular ordinário na data do pleito novo, sem distinguir cassação de anulação. Para presidente, vice-presidente, governador e vice-governador, a forma de saída vem da curadoria de fonte oficial quando há evento localizado e da Wikipédia quando não há, e a coluna `via` de saida_executivos.csv registra qual delas resolveu cada mandato. As tabelas de origem (exercicio_camara, exercicio_senado, camara_biografia_eventos, camara_biografia_posses, interregnos_legislativo_federal, saida_executivos, saida_legislativo_federal, ocupantes_legislativo_federal, eleicoes_suplementares) acompanham o depósito com os períodos e as causas originais.

## Como a condição de eleito foi estabelecida

A situação de totalização vem do cadastro de candidaturas quando o TSE a
preenche, e dos arquivos de votação nominal quando o cadastro a deixa em
branco, o que ocorre na maior parte dos pleitos de 1998 a 2010. Para vices,
que não têm votação própria, a condição é herdada do titular da chapa. Para
cargos majoritários em que o TSE não marca vencedor em nenhuma candidatura da
unidade, o eleito é o mais votado do turno decisivo. A coluna
`fonte_situacao` em mandatos.csv identifica a origem em cada linha, e a
distribuição está em output/numeros_assinatura.txt do repositório. Quando
a mesma posição e número aparecem em mais de uma candidatura (substituição
de candidato), fica a não substituída, mais votada e deferida. Os casos em
que o banco diverge das cadeiras esperadas por decisão documentada estão em
EXCECOES_CONHECIDAS.csv.

Na construção do banco completo, antes do recorte da v1.0, 205 mandatos majoritários de todo o país foram atribuídos ao mais votado por ausência de marcação do TSE, 894 candidaturas repetidas na mesma posição e número foram reduzidas a uma, e 0 registros eleitos sem nome, título nem CPF foram excluídos. No recorte da v1.0, dos 4.253 mandatos, 4.138 têm a condição de eleito lida do cadastro, 112 herdada do titular da chapa (vices), 1 atribuída pela votação e 2 corrigida pela fonte oficial da casa depois da cassação do registro no TSE.

## O que está parcial

- **Votação obtida.** Agregada dos arquivos de votação nominal por município e
  zona; a taxa de pareamento por esfera e ano acompanha o repositório.
- **Sucessão.** Antecessor e sucessor estão preenchidos para os cargos
  executivos (prefeito, governador, presidente) entre eleições ordinárias
  adjacentes. Para o legislativo, a sucessão por cadeira não é definível a
  partir do resultado eleitoral e fica para a v2.
- **Identificação de pessoa.** Distribuição das regras de deduplicação:

| regra | pessoas | proporção |
|---|---|---|
| titulo | 2.155 | 100,00% |

## O que se sabe que falta

- **Forma de saída fora das fontes acima.** Onde nenhuma fonte cobre o
  mandato, a coluna fica em `nao_observado`; as datas de mandato seguem
  convencionais (1º de fevereiro a 31 de janeiro para senadores e deputados,
  1º de janeiro a 31 de dezembro para os demais) e o eleito sem posse
  observada figura como ocupante. No painel pessoa × cargo × ano, o mês de
  janeiro que encerra a legislatura não gera ano adicional.
- **Eleições suplementares.** A tabela eleicoes_suplementares.csv traz 5 pleitos majoritários (governador, senador) e o vencedor de cada um; o vencedor do pleito suplementar não recebe mandato próprio em mandatos.csv, e o ocupante registrado entre a perda do mandato e a nova eleição ordinária segue sendo o eleito no pleito ordinário anterior, com `forma_saida = perda_do_mandato_inferida_por_eleicao_suplementar`.
- **Precisão do pareamento por nome e nascimento.** No recorte da v1.0, toda pessoa do cadastro tem título ou CPF do TSE, e nenhuma identificação depende só da regra nome + nascimento; auditoria_homonimos.csv fica vazia nesta versão.
- **Filiação partidária longitudinal.** A tabela filiacoes.csv cobre 2.126 das 2.155 pessoas (98,7%), com 5.942 registros de filiação vindos das listas do TSE espelhadas pela Base dos Dados. As pessoas sem título de eleitor válido no cadastro do TSE e as que só constam de listas anteriores ao espelhamento ficam sem histórico; o partido no momento de cada eleição está em mandatos.csv para todas.
- **Suplentes de senador** (cargos 1º e 2º suplente) não constam como
  ocupantes de posição.
- **Vices** constam como ocupantes do cargo de vice. Quando a saída do titular
  da chapa está observada (renúncia, morte, cassação, afastamento, perda do
  mandato ou substituição), o vice recebe `forma_saida = assumiu_titular`, com
  fonte `derivado_titular` e a data de saída do titular; a v1 não observa a
  posse do vice na titularidade nem a saída dele depois disso.


## Ocupação da cadeira e suplência

O banco separa a cadeira de quem a ocupou. A tabela de mandatos guarda uma linha por cadeira
ganha na eleição, e o número de cadeiras por lugar, cargo e eleição continua o mesmo depois da
entrada dos suplentes. Sobre ela, `ocupacoes.csv` registra 6.287 ocupações no recorte federal, das
quais 2.034 são de quem não é o titular eleito, distribuídas por 268 cadeiras que passaram por mais
de uma pessoa. A distinção importa porque a cadeira muda de partido sem eleição quando o
convocado vem de outra legenda da mesma lista, o que se observa em 96 ocupações.

A fila de suplência de deputado federal e de senador está em `lista_suplencia.csv`, com 24.706
suplentes em 1.428 listas. A posição na fila vem da votação nominal dentro da lista, com empate
resolvido pelo candidato mais idoso, que é a regra do Código Eleitoral. O TSE publica essa ordem
já pronta só para vereador, cargo fora do recorte da v1.0, de modo que a fila de deputado federal
e de senador é derivada sem conferência externa contra uma ordem publicada. Os suplentes de
senador entram pela chapa, porque o TSE não os marca como eleitos, do mesmo modo que não marca o
vice do executivo.

O limite da camada está no vínculo entre o suplente e a cadeira que ele ocupou. Onde a chapa
nomeia o titular, onde a unidade tem cadeira única e onde a lista teve uma só vaga aberta, o
vínculo é direto, e assim se resolvem 352 ocupações. Outras 34 são ligadas por inferência, pela
proximidade entre a data de entrada do convocado e a data em que uma cadeira da mesma lista
vagou, exigindo que a vacância anteceda a entrada e que a distância não passe de um ano. Nas
demais 1.599 a casa registra o exercício sem dizer qual titular saiu, de modo que a ocupação fica
com `vinculo_cadeira` igual a `casa_legislatura` e conta como ocupante da casa, sem que nenhuma
cadeira tenha sido criada para acomodá-la. Presidência e governos estaduais somam 49 ocupações
por linha sucessória, eleição indireta pela casa legislativa ou nova totalização de votos, com
`vinculo_cadeira` igual a `fonte_da_casa` e resolvidas pela curadoria de fonte oficial em
saida_executivos.csv, não pela fila de suplência.

A troca de partido dentro da cadeira só é medida onde ela pode ser medida. A sigla do ocupante
entra apenas quando é sigla que existe no banco, porque o texto extraído de enciclopédia trazia
nome de partido por extenso e frases inteiras no lugar dela, e a comparação vale somente para o
suplente convocado, já que vice e interino ocupam por sucessão no Executivo, que é outro
fenômeno.

Quem foi suplente e nunca se elegeu tem identificador de pessoa no mesmo espaço do cadastro
principal, gravado em `pessoas_suplentes.csv`, e nenhum identificador já publicado mudou com a
entrada dessas pessoas.
