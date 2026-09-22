#!/usr/bin/env bash
# Roda os verificadores do recorte da v1.0 (presidencia/vice, governador/vice, senador com suplentes,
# deputado federal, a camada de ocupacao/suplencia e a filiacao partidaria) e resume passou/falhou por
# script em logs/verificadores_<tag>/resumo.tsv. Sai com status 1 se qualquer verificador falhar, o
# processo abortar (rc != 0) ou reportar FALHOU/PROBLEMAS > 0 na propria saida.
# Uso: bash R/rodar_verificadores.sh <tag>
set -u
cd "$(dirname "$0")/.."
export BOCEL_ROOT="$(pwd)"
L=logs/verificadores_${1:-rodada}
mkdir -p "$L"; : > "$L/resumo.tsv"

# 21/09/2026: lista explicita dos verificadores do recorte da v1.0 (decisao de 21/09/2026).
# Assembleias e municipios (camaras municipais, SAPL, TCE,
# datajud, wikidata/wikipedia estadual e de prefeitos, diarios, MUNIC) ficam de fora do portao ate a
# v1.5, quando essas esferas voltarem a fazer parte do que o deposito publica; eles continuam rodando
# na cadeia de producao (logs/cadeia_bocel.sh), so nao bloqueiam a publicacao da v1.0.
VERIFICADORES=(
  R/04_verificar.R
  R/verifica_homonimos.R
  R/verifica_consolidacao.R
  R/verifica_reeleicao_tse.R
  R/verifica_executivos.R
  R/verifica_camara.R
  R/verifica_senado.R
  R/verifica_legislativo_federal.R
  R/verifica_suplementares.R
  R/verifica_ocupacoes.R
  R/verifica_documentacao.R
  R/verifica_integracao.R
  R/verifica_integracao_docs.R
  R/verifica_exportacao_v1.R
)

GERAL_RC=0
for s in "${VERIFICADORES[@]}"; do
  if [ ! -f "$s" ]; then
    printf "%s\tAUSENTE\t0\t\n" "$s" | tee -a "$L/resumo.tsv"
    GERAL_RC=1
    continue
  fi
  nome=$(echo "$s" | tr '/' '_')
  t0=$(date +%s)
  Rscript --vanilla "$s" > "$L/$nome.out" 2>&1; rc=$?
  pf=$(grep -hoE "PASSOU: ?[0-9]+ ?\| ?FALHOU: ?[0-9]+|[0-9]+ passaram, [0-9]+ falharam|passou: ?[0-9]+ +falhou: ?[0-9]+|falhas: ?[0-9]+ ?\| ?passou: ?[0-9]+|aprovadas: ?[0-9]+ +reprovadas: ?[0-9]+|[0-9]+ aprovadas, [0-9]+ reprovadas|[0-9]+ checks ok; [0-9]+ falharam|PROBLEMAS: ?[0-9]+|veredito[^,]*" "$L/$nome.out" | tail -1)
  printf "%s\t%s\t%s\t%s\n" "$s" "$rc" "$(( $(date +%s) - t0 ))" "$pf" | tee -a "$L/resumo.tsv"
  if [ "$rc" != 0 ]; then GERAL_RC=1; fi
  if echo "$pf" | grep -qE "FALHOU: ?[1-9]"; then GERAL_RC=1; fi
  if echo "$pf" | grep -qE "PROBLEMAS: ?[1-9]"; then GERAL_RC=1; fi
  if echo "$pf" | grep -qE "^falhas: ?[1-9]"; then GERAL_RC=1; fi
done
echo "FIM (status=$GERAL_RC)"
exit "$GERAL_RC"
