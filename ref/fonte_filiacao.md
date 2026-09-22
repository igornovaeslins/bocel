# Fonte da filiação partidária, Base dos Dados (BigQuery)

O histórico de filiação partidária das pessoas do recorte da v1.0 vem das listas de filiados do TSE
(Sistema FILIA), no espelho público que o projeto Base dos Dados mantém no BigQuery. Como se trata de
um banco já publicado, a reconstrução não tem coletor próprio para esse passo, e este documento
registra a consulta que gerou os arquivos brutos lidos pelo `R/03b_filiacoes.R`.

## Conjunto e tabelas

- Projeto de dados no BigQuery: `basedosdados`
- *Dataset*: `br_tse_filiacao_partidaria`
- Tabelas:
  - `microdados`, com as filiações correntes (registro de filiação, título de eleitor, CPF, sigla do
    partido, UF, município do TSE, situação do registro, motivo de desfiliação ou cancelamento,
    indicador de origem, datas de filiação, desfiliação, cancelamento e exclusão, data de extração)
  - `microdados_antigos`, com as filiações do histórico anterior à migração do TSE para o Filiaweb
    (título eleitoral, sigla do partido, UF, município do TSE, situação e tipo de registro, motivo de
    cancelamento, datas de filiação, desfiliação, cancelamento, regularização e processamento)

## Filtro usado

A consulta busca só os títulos de eleitor das pessoas que já estão em `data/pessoas.csv`
(`nr_titulo_eleitoral`, com os 12 dígitos), em lotes de até 30 mil títulos por `IN (...)`, contra as
duas tabelas separadamente. A leitura é incremental, e os títulos já consultados numa rodada anterior
ficam registrados e não voltam a ser pedidos ao BigQuery nas rodadas seguintes.

## Autenticação

A Base dos Dados exige, para consulta por SQL, um projeto de faturamento do Google Cloud em nome de
quem consulta. O identificador do projeto usado na leitura é credencial de conta e fica fora deste
documento e dos scripts.

## Data da leitura

12/09/2026.

## Arquivos que a reconstrução espera

`data_raw/filiacao/filiacao_atual.parquet` (tabela `microdados`) e, no mesmo diretório,
`filiacao_antiga.parquet` (tabela `microdados_antigos`), que o `R/03b_filiacoes.R` lê. Sem esses
arquivos em disco, o `R/00_reconstruir.sh` pula o passo de filiação com aviso, e quem reconstrói o
banco do zero repete a consulta acima no BigQuery antes de rodar o `R/03b_filiacoes.R`. O resultado
desse passo para o recorte da v1.0 está no próprio depósito, em `filiacoes.csv` e `filiacoes.parquet`.
