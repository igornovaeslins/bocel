# Nota de cobertura — BOCEL v1.2

## O que está completo

O recorte federal das eleições ordinárias de 1998 a 2022, a saber presidente e
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

A auditoria de 24/09/2026 (padrão Jean Wyllys, pendência 12) separou 21 mandatos do recorte em que a forma de saída está preenchida e a data de posse é desconhecida, porque a fonte que registra a saída não registra a entrada. Fiel à fonte, cada um manteve a forma de saída original, inclusive o próprio Jean Wyllys, deputado federal do RJ eleito em 2018, que renunciou sem posse observada; 14 desses mandatos têm ao menos um ano exercido em posicoes_ano.csv antes da saída, o que descarta a leitura de que a ausência de data de posse implica mandato nunca exercido.

A coluna `exerceu_no_ano` de posicoes_ano.csv (DEF-01) marca, ano a ano, se o titular esteve de fato na cadeira, lida da ocupação titular em ocupacoes.csv e da forma de saída do mandato, e não mais só da janela convencional do cargo. Ela vale TRUE em 17.437 das 18.092 linhas do painel, FALSE em 551 e fica NA em 104, reservado ao mandato em curso cujo fim ainda não se conhece e cujo ano consultado é posterior à data de referência da reconstrução (output/data_referencia.txt). 7 mandatos do recorte nunca chegaram a ser exercidos, entre eles Jean Wyllys e um mandato do Piauí eleito em 2022 que renunciou antes da posse. A auditoria de 24/09/2026 contou 276 mandatos e 552 linhas de painel posteriores à saída, comparando a data de fim efetiva com o fim convencional do cargo, e a coluna, que lê a ocupação titular, resulta em 551 linhas FALSE, que pertencem a 278 mandatos, de modo que os dois pares de números medem critérios distintos.

`precisao_data_fim` classifica a data de fim do exercício em ato (477 mandatos, data do ato registrado pela casa ou por fonte oficial), convencional (3.180, fim regular na data constitucional) e intervalo (3, forma de saída confirmada e dia exato fora do alcance das fontes, com os limites na tabela curada). Esse vocabulário foi criado como adaptação ao recorte federal desta versão (pendência 2, opção B), e não cobre as Assembleias Legislativas; a v1.5 leva o painel de exercício das Assembleias a `serie_densa` (fonte com data exata, dia a dia) e `serie_esparsa` (fonte só por legislatura ou por ano), vocabulário próprio que substitui este aqui quando essas casas entrarem.

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
| cpf |     1 | 0,04% |
| titulo | 2.423 | 99,96% |

## O que se sabe que falta

- **Forma de saída fora das fontes acima.** Onde nenhuma fonte cobre o
  mandato, a coluna fica em `nao_observado`; as datas de mandato seguem
  convencionais (1º de fevereiro a 31 de janeiro para senadores e deputados,
  1º de janeiro a 31 de dezembro para os demais) e o eleito sem posse
  observada figura como ocupante. No painel pessoa × cargo × ano, o mês de
  janeiro que encerra a legislatura não gera ano adicional.
- **Eleições suplementares.** A tabela eleicoes_suplementares.csv traz 5 pleitos majoritários (governador, senador) e o vencedor de cada um; o vencedor do pleito suplementar não recebe mandato próprio em mandatos.csv, e o ocupante registrado entre a perda do mandato e a nova eleição ordinária segue sendo o eleito no pleito ordinário anterior, com `forma_saida = perda_do_mandato_inferida_por_eleicao_suplementar`.
- **Precisão do pareamento por nome e nascimento.** No recorte da v1.0, toda pessoa do cadastro tem título ou CPF do TSE, e nenhuma identificação depende só da regra nome + nascimento; auditoria_homonimos.csv fica vazia nesta versão.
- **Filiação partidária longitudinal.** A tabela filiacoes.csv cobre 2.394 das 2.424 pessoas (98,8%), com 6.855 registros de filiação vindos das listas do TSE espelhadas pela Base dos Dados. As pessoas sem título de eleitor válido no cadastro do TSE e as que só constam de listas anteriores ao espelhamento ficam sem histórico; o partido no momento de cada eleição está em mandatos.csv para todas.
- **Filiação repetida entre lista antiga e lista atual (DOC-01).** A fusão das listas histórica e atual do TSE deixou 20 triplas de pessoa, partido e data de filiação repetidas em 40 linhas de filiacoes.csv, sendo 17 de pessoas eleitas no recorte, as únicas que a versão 1.0 trazia, e 3 de ocupantes sem mandato, que a versão 1.1 acrescenta. Cada fonte conta a mesma filiação uma vez. A tabela preserva as duas linhas, e quem contar filiações por pessoa precisa agregar por essa tripla antes de somar.
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
quais 2.034 são de quem não é o titular eleito, distribuídas por 273 cadeiras que passaram por mais
de uma pessoa. A distinção importa porque a cadeira muda de partido sem eleição quando o
convocado vem de outra legenda da mesma lista, o que se observa em 123 ocupações.

A fila de suplência de deputado federal e de senador está em `lista_suplencia.csv`, com 24.706
suplentes em 1.428 listas. A posição na fila vem da votação nominal dentro da lista, com empate
resolvido pelo candidato mais idoso, que é a regra do Código Eleitoral. O TSE publica essa ordem
já pronta só para vereador, cargo fora do recorte da v1.0, de modo que a fila de deputado federal
e de senador é derivada sem conferência externa contra uma ordem publicada. Os suplentes de
senador entram pela chapa, porque o TSE não os marca como eleitos, do mesmo modo que não marca o
vice do executivo.

O limite da camada está no vínculo entre o suplente e a cadeira que ele ocupou, e `vinculo_cadeira`
registra o caminho pelo qual cada ocupação foi ligada. Onde a chapa nomeia o titular, onde a unidade tem
cadeira única e onde a lista teve uma só vaga aberta, o vínculo é direto, e assim se resolvem 352 ocupações. Outras 34 são ligadas por inferência de data
(`lista_data_estrita`), quando várias cadeiras da mesma lista tinham vagado, e vale a que vagou mais perto da
entrada do convocado, com a vacância até 365 dias antes da entrada ou até 30 dias depois dela, margem que absorve o atraso de registro da fonte. Em 49 ocupações (27 de deputado federal e 22 de senador) a cadeira é a que uma tabela
curada com fonte oficial da própria Casa nomeia, o que cobre situações como a cassação do titular, a
retotalização dos votos, a decisão judicial e a eleição suplementar, e o vínculo é `fonte_da_casa`. Outras 6 foram ligadas pela regra geral das vagas permanentes,
descrita adiante. Nas demais 1.593 a casa registra o exercício sem dizer qual titular saiu, de
modo que a ocupação fica com `vinculo_cadeira` igual a `casa_legislatura` e conta como ocupante da casa, sem
que nenhuma cadeira tenha sido criada para acomodá-la. Os 78 vices e interinos que assumiram o governo de um estado ocupam a cadeira única do cargo, e a fila de suplência
não se aplica a eles.

A regra geral (`R/42_ocupacoes.R`, seção 4b) decide o vínculo das cadeiras de deputado federal cujo titular não
exerceu o mandato desde o começo da legislatura. O domínio soma 18 cadeiras, das quais 5 têm titular que não tomou posse, 2 têm titular cuja saída antecede o início e 11 têm titular que tomou posse mais de 60 dias depois do início do mandato. Nas listas dessas cadeiras a regra reconhece 24 vagas permanentes, entre as que existem desde o início
da legislatura e as que se abrem depois com a saída definitiva de um ocupante. Um sinal de efetivação é a data em que um
suplente passa a titular na API de exercício da Câmara, na tabela de ocupantes do legislativo federal ou na seção
de suplências e efetivações da biografia oficial do deputado. A vaga se casa com o suplente cuja efetivação cai na
janela da vaga, que vai de 31 dias antes a 31 dias depois do início do mandato quando a vaga é
inicial, e de 3 dias antes a 31 dias depois da saída quando é de sucessão, e o suplente tem de constar da
lista da cadeira, com ligação única dos dois lados. A saída do suplente que entrou abre a vaga seguinte da mesma cadeira.
Em 1998 a suplência pertence à coligação, e a ordem de suplência das listas foi recalculada pela votação nominal dentro
dela, porque a ordem gravada nas listas daquele ano é a do partido. A regra nunca desfaz um vínculo mais firme. Vêm antes
`fonte_da_casa`, `chapa_senado` e `cadeira_unica_da_unidade`, e `lista_vaga_unica` e `lista_data_estrita` são mantidos,
com a regra apenas registrando se concorda ou diverge.

Aplicada ao banco, a regra firmou 7 pares entre uma vaga e o suplente que a
preencheu (6 de vaga inicial e 1 de sucessão) e alcançou 9 ocupações, contadas as posteriores da mesma
pessoa na legislatura. Dessas, 6 receberam cadeira nova, 5 por `lista_vaga_inicial` e 1 por `lista_sucessao_vaga`, em 5 cadeiras, e 3 já estavam ligadas por `lista_vaga_unica` ou `lista_data_estrita` à mesma cadeira que a regra aponta. Nenhuma
ocupação ligada por esses dois caminhos aponta cadeira diferente da regra, e nenhum par pulou a ordem de suplência da lista. A regra reproduz os 4 casos que a biografia
da Câmara resolve, que são Talvane Albuquerque e José Helenildo Ribeiro em AL 1998, Selma Schons em PR 2002 e Jorge Braz em RJ 2018. As outras 2 ligações da regra, OC00489947 (BOCEL0246485, David Michael Dos Santos Miranda, RJ 2018, saída em 31 de janeiro de 2023) e OC00491741 (BOCEL0100491, Merlong Solano Nogueira, PI 2022, sem data de fim), não têm gabarito independente e
não foram confirmadas por fonte externa à regra. Como a regra foi calibrada nesses casos, a reprodução deles não a valida onde ela decidiu
sozinha, e a pertinência dessas 2 ligações fica fora do que os verificadores do banco atestam e a critério de quem usa o banco. Ficaram sem par 12 vagas de cadeiras do domínio, 11 sem sinal de efetivação na janela e 1 com um entrante efetivado na janela que não consta da lista da vaga. As 12 já tinham a cadeira nomeada por `fonte_da_casa`, e por isso todas as cadeiras do domínio têm ocupante não titular ligado.
O que nenhum caminho decide segue para `casa_legislatura`. A regra infere a vacância quando nenhum ato a publica, e a
validade dessa inferência não foi conferida contra o Diário da Câmara, o que fica a critério de quem usa o banco.

A data ausente na fonte é lida na biografia oficial da Câmara, e só para a ocupação que a fonte não datava.
Em AL 1998 as duas ocupações ligadas à cadeira de um titular que não tomou posse não tinham data na fonte, e as datas
vieram da biografia da Câmara, com `origem_data_inicio` igual a `biografia_camara`. Talvane Albuquerque exerceu de 3 de fevereiro de 1999 a 7 de abril de 1999, data de fim lida na mesma biografia, e
José Helenildo Ribeiro Monteiro assumiu em 8 de abril de 1999 e ficou até 31 de janeiro de 2003, que é o fim da legislatura, com `origem_data_fim` igual a
`convencao_fim_legislatura`, porque a biografia não registra saída definitiva dele antes do fim do mandato. Ao todo a regra deu data a 2 inícios e a 2 fins (1 lido na biografia e 1 pela convenção), e nenhum início veio do sinal de efetivação da API. As biografias oficiais que sustentam
esses casos são as seguintes.

- Helenildo Ribeiro, AL 1998, https://www.camara.leg.br/deputados/73582/biografia, “Assumiu e foi efetivado no mandato de Deputado Federal para a legislatura 1999-2003, em 8 de abril de 1999.”
- Talvane Albuquerque, AL 1998, https://www.camara.leg.br/deputados/74523/biografia, “Empossado e efetivado no mandato de Deputado Federal, na legislatura 1999-2003, exerceu o mandato de 3 de fevereiro a 7 de abril de 1999.”
- Selma Schons, PR 2002, https://www.camara.leg.br/deputados/74403/biografia, “Assumiu e foi efetivada no mandato de Deputada Federal na legislatura 2003-2007, em 1º de fevereiro de 2003.”
- Jorge Braz, RJ 2018, https://www.camara.leg.br/deputados/205550/biografia, “Foi efetivado(a), em 1 de Fevereiro de 2019, assumindo o mandato de Deputado(a) Federal, na Legislatura 2019-2023.”
- José Valdevan de Jesus Santos, SE 2018, https://www.camara.leg.br/deputados/204385/biografia, “Perdeu o mandato de Deputado(a) Federal, na Legislatura 2019-2023, em 27 de Abril de 2022, em virtude de Perda de Mandato por Recontagem de Votos”

A troca de partido dentro da cadeira só é medida onde ela pode ser medida. A sigla do ocupante
entra apenas quando é sigla que existe no banco, porque o texto extraído de enciclopédia trazia
nome de partido por extenso e frases inteiras no lugar dela, e a comparação vale para o
suplente convocado e para o suplente efetivado, já que vice e interino ocupam por sucessão no
Executivo, que é outro fenômeno. Na Câmara, quando a fonte da ocupação não traz a sigla de quem
não é o titular, o partido vem do cadastro de filiação do TSE no início da ocupação, em 242 ocupações, e, sem filiação que aponte um só partido, da candidatura que criou a
cadeira, em 61. Ficam sem partido 41 ocupações, e a coluna
`origem_partido_ocupante` registra o caminho de cada sigla. A marca de troca alcança 26 suplentes efetivados.

Quem foi suplente e nunca se elegeu tem identificador de pessoa no mesmo espaço do cadastro
principal, gravado em `pessoas_suplentes.csv`, e nenhum identificador já publicado mudou com a
entrada dessas pessoas. O identificador de cada ocupante de `ocupacoes.csv` resolve em `pessoas.csv` ou
em `pessoas_suplentes.csv`. Dos 2.728 ocupantes identificados, 573 não foram eleitos no recorte, e desses 269 constam de `pessoas.csv` como ocupantes sem mandato no recorte,
por terem sido eleitos em algum cargo fora dele, enquanto 304 nunca foram eleitos em
nenhuma esfera do banco e constam apenas de `pessoas_suplentes.csv`, onde respondem por 513 ocupações. Quem juntar `ocupacoes.csv` a `pessoas.csv` sem
considerar o segundo arquivo perde essas ocupações.

A sobreposição entre titular e suplente na mesma cadeira, e a repetição da ocupação por ano em
posicoes_ano.csv, decorrem do desenho do banco (OCU-05). Ordenando as ocupações de uma
cadeira pela data de início, 186 pares consecutivos têm a entrada do ocupante seguinte na mesma
data em que termina o anterior ou antes disso, o que 164 cadeiras concentram, pela convenção de
data inclusiva do banco (a saída do suplente cai no mesmo dia da entrada de quem o sucede).
Juntar `ocupacoes.csv` a `posicoes_ano.csv` por `id_mandato` sem agrupar por ano repete cada
ocupação uma vez por ano do mandato, o que soma 20.572 linhas contra 6.287 ocupações e 18.092 posições anuais no painel.

A sobreposição também ocorre entre dois ocupantes que não são o titular. Em deputado federal, 20 pares de ocupações de suplentes compartilham mais de 3 dias da mesma cadeira, em 10 cadeiras, e nenhum deles reúne a mesma pessoa duas vezes. A auditoria de 24/09/2026
separou 14 desses pares como substituição dentro da substituição. Um suplente em exercício se licencia e
outro suplente cobre a licença dentro do período do primeiro, e a biografia da Câmara de ao menos um dos dois registra a
entrada e a saída do substituto temporário. Em 12 desses pares o período de uma das ocupações está contido no da outra, e em 2 os períodos se cruzam. Mantive os dados como vieram da fonte. Os outros 6 pares estão documentados linha a linha em `EXCECOES_CONHECIDAS.csv`. Um deles é uma ligação à cadeira
errada (par 6 da auditoria, AL 2010). Duas cadeiras da mesma lista vagaram em 31 de dezembro de 2014 e em 1 de janeiro de 2015. A ocupação OC00487342, de suplente de ordem 2, foi ligada por `lista_data_estrita` à cadeira que vagou em 1 de janeiro de 2015, que a ocupação OC00487341, de ordem 3, preencheu, e por isso as duas aparecem sobrepostas de 2 de janeiro de 2015 a 31 de janeiro de 2015. Pela proximidade de data, a cadeira provável de OC00487342 é a que vagou por
renúncia do titular em 31 de dezembro de 2014. Mantive a ligação atual e documentei o caso, e a troca da regra
`lista_data_estrita` por um casamento que compare ao mesmo tempo todas as cadeiras vagas da lista fica para a v1.5. Os 5 pares restantes ocorrem em MG 2010 (3 pares), SE 2018 (1 par) e SP 2010 (1 par), sempre no mês final da legislatura, em janeiro de 2015 e janeiro de 2023. Em nenhum deles há segunda vacância confirmada, a biografia da Câmara do ocupante mais graduado
não registra afastamento antes da entrada do seguinte, e a lista não tem outra cadeira vaga compatível. A resposta sobre
qual das convocações prevalece está na ata de posse da Mesa ou no Diário da Câmara do período, que o banco não armazena.
Mantive as duas ocupações de cada par, documentei os casos e deixei a curadoria para a v1.5.

Correções pontuais mudaram datas e identidades antes deste depósito, sem deixar o valor anterior no
arquivo final. `data_fim` não pode ultrapassar o fim do próprio mandato, e 52 ocupações de ocupacoes.csv tiveram a data reduzida a esse limite
(OCU-02), todas de vice que assumiu o governo de estado e cuja fonte secundária dava a saída depois do fim
constitucional do mandato. A mesma regra reduziu a data de fim de 1.371 ocupações de cargos que o recorte não inclui, e por isso o total do banco completo, de 1.423, não descreve este depósito. José Valdevan de Jesus Santos (SE 2018) tinha duas ocupações na mesma cadeira, ambas sem data de fim na
API da Câmara, e a segunda, iniciada em 3 de junho de 2022, ficava sobreposta à primeira. Fechei a
primeira em 27 de abril de 2022, data em que a biografia da Câmara registra a perda do mandato,
com `origem_data_fim` igual a `curadoria_biografia_camara`. A mesma biografia registra outra perda em 14 de junho de 2022, que não entra nesta curadoria, e a segunda ocupação segue como veio da API. A data de início da ocupação de Walter Pinheiro (BA 1998), com
`origem_data_inicio` igual a `curadoria_ocu07`, passou a ser 1 de fevereiro de 1999, o início da legislatura, e a identidade de
Almerinda (RJ 2002) permanece corrigida por curadoria rastreável (OCU-07), com o detalhe de cada caso em EXCECOES_CONHECIDAS.csv. A correção
de Walter Pinheiro alcançou só `ocupacoes.csv`. A `data_posse` do mandato dele, em `mandatos.csv` e em `saida_legislativo_federal.csv`, mantém o valor da fonte, 2 de janeiro de 1999, anterior ao início da legislatura em 1 de fevereiro de 1999, e as duas datas divergem para essa pessoa nessa cadeira.
