# 05_documentar.R — gera docs/LIVRO_DE_CODIGOS.md, docs/NOTA_DE_COBERTURA.md e docs/README.md
# com percentuais de preenchimento e contagens computados dos arquivos finais.
# Execucao: cd ~/bocel && Rscript --vanilla R/05_documentar.R
set.seed(20260827)
suppressPackageStartupMessages({ library(data.table) })

# 21/09/2026: a v1.0 leva ao Zenodo so o recorte federal (presidencia, governo, senado e camara) mais a
# camada de ocupacao e a filiacao das pessoas do recorte (decisao de 21/09/2026, item 1, opcao A);
# assembleias e municipios ficam para a v1.5/v2.0, e por isso a documentacao le data_v1/, nao data/
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel"))
setwd(root)
mand <- fread("data_v1/mandatos.csv", colClasses = "character", na.strings = "NA")
pos  <- fread("data_v1/posicoes_ano.csv", colClasses = "character", na.strings = "NA")
pess <- fread("data_v1/pessoas.csv", colClasses = "character", na.strings = "NA")

pct <- function(x) sprintf("%.1f%%", 100 * mean(!is.na(x) & x != ""))

dic_mand <- list(
  id_mandato = c("texto", "Identificador do mandato, na forma M<ano>_<unidade>_<cd_cargo>_<nr_candidato>_<sq_candidato>"),
  id_pessoa = c("texto", "Identificador estavel da pessoa (BOCEL + 7 digitos), herdado entre versoes pela referencia ref/ids_pessoa_referencia.parquet; pessoa nova recebe numero acima do maior ja usado"),
  sq_candidato = c("texto", "SQ_CANDIDATO do TSE (unico nacionalmente so a partir de 2010)"),
  nr_candidato = c("texto", "Numero do candidato na eleicao de origem"),
  ano_eleicao = c("inteiro", "Ano da eleicao de origem"),
  nr_turno_decisivo = c("inteiro", "Turno em que a eleicao se decidiu (1 ou 2)"),
  dt_eleicao = c("data dd/mm/aaaa", "Data do pleito (turno decisivo)"),
  cd_cargo = c("inteiro", "Codigo TSE do cargo"),
  cargo = c("texto", "Descricao do cargo (PREFEITO, VEREADOR, ...)"),
  esfera = c("texto", "Esfera (municipal, estadual ou federal)"),
  sg_uf = c("texto", "UF da posicao (BR para presidente)"),
  sg_ue = c("texto", "Codigo TSE da unidade eleitoral (municipio ou UF)"),
  nm_ue = c("texto", "Nome da unidade eleitoral"),
  unidade_posicao = c("texto", "Unidade em que a posicao e disputada, isto e, codigo TSE do municipio (cargos municipais), UF (estaduais e federais) ou BR (presidente)"),
  nr_partido = c("inteiro", "Numero do partido na eleicao de origem"),
  sg_partido = c("texto", "Sigla do partido na eleicao de origem"),
  tp_agremiacao = c("texto", "Tipo de agremiacao (partido isolado, coligacao, federacao)"),
  nm_coligacao = c("texto", "Nome da coligacao"),
  composicao_coligacao = c("texto", "Partidos que compoem a coligacao"),
  regra_chapa = c("texto", "Como a candidatura do vice foi ligada ao titular eleito da mesma chapa, so nos cargos de vice. Valores numero_do_titular (mesmo numero do titular, ou o numero dele seguido de 1 em 1998 e 2000, com situacao vazia no cadastro), numero_titular_imputado (vice de titular imputado por votos, com situacao no cadastro igual a do titular), sq_coligacao (mesma coligacao do titular na unidade, com correspondencia de um para um) e composicao_coligacao (mesma composicao da coligacao escrita por extenso). NA nos titulares e nos demais cargos"),
  reeleicao_declarada = c("texto", "Candidato declarou concorrer a reeleicao (S/N, TSE)"),
  situacao_totalizacao = c("texto", "Situacao de totalizacao no turno decisivo (ELEITO, ELEITO POR QP, ELEITO POR MEDIA; ELEITO (IMPUTADO DO TITULAR) para vices; ELEITO (IMPUTADO POR VOTOS) para majoritario sem vencedor marcado pelo TSE)"),
  fonte_situacao = c("texto", "Origem da situacao, entre cadastro (consulta_cand), votacao (votacao_candidato_munzona), imputacao_titular e imputacao_votos"),
  votos_t1 = c("inteiro", "Votos nominais no 1o turno"),
  votos_turno_decisivo = c("inteiro", "Votos nominais no turno decisivo"),
  mandato_inicio = c("data ISO", "Inicio convencional do mandato (1o de fevereiro pos-eleicao para senador e deputados; 1o de janeiro para os demais)"),
  mandato_fim = c("data ISO", "Fim convencional do mandato (31 de janeiro apos o ultimo ano para senador e deputados; 31 de dezembro para os demais)"),
  forma_saida = c("texto", "Forma de saida do cargo, vocabulario fechado (fim_regular, renuncia, falecimento, cassacao, afastamento, licenca, nao_tomou_posse, suplente_efetivado, perda_do_mandato_inferida_por_eleicao_suplementar, substituicao_inferida_munic, assumiu_titular, aposentadoria, impeachment, retotalizacao, outro, nao_observado). Em senador, deputado federal, presidente, vice-presidente, governador e vice-governador, licenca e afastamento temporario nao encerram mandato, e o periodo fora do exercicio fica em data/interregnos_legislativo_federal.csv; retotalizacao e a perda da vaga por nova totalizacao dos votos, sem ato ilicito"),
  fonte_forma_saida = c("texto", "Fonte da forma de saida (camara_api, senado_api, camara_biografia, fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada, pista_nao_oficial, assembleia_api, sapl_municipal, sapl_observacao, portal_camara, tce, assembleia_historico, assembleia_portal, assembleia_inventario, diario_oficial, wikipedia, wikidata, wikidata_obito, datajud, tse_suplementar, ibge_munic, cargo_incompativel, derivado_titular, data_fim_efetiva). sapl_observacao e o ato escrito no campo de texto livre do SAPL, lido por R/54; assembleia_inventario e a folha e a frequencia das casas estaduais; cargo_incompativel e a posse em outro cargo eletivo deduzida do proprio banco. camara_biografia e a biografia oficial do deputado na Camara, fonte da legislatura 1999-2003; fonte_oficial_curada e o evento das tabelas em ref/ com fonte oficial, e nos casos federais especiais tambem com materia e trecho literal conferido. Nos governos e na Presidencia, o evento curado sem fonte oficial leva o rotulo da melhor fonte que tem, base_dhbb_curada para o verbete do DHBB/CPDOC-FGV, noticia_orgao_publico_curada para a noticia publicada por orgao publico e pista_nao_oficial quando so ha imprensa ou Wikipedia, caso em que o mandato consta de docs/EXCECOES_CONHECIDAS.csv. derivado_titular marca o vice que assume quando o titular sai; data_fim_efetiva marca o fim efetivo registrado que iguala ou ultrapassa o fim convencional, promovido a fim_regular"),
  data_posse = c("data ISO", "Inicio do primeiro periodo de exercicio registrado (Camara, Senado, Assembleias, Wikidata)"),
  data_fim_efetiva = c("data ISO", "Fim do ultimo periodo de exercicio registrado, ou data da saida inferida"),
  precisao_data_fim = c("texto", "Precisao de data_fim_efetiva, com os valores ato (data do ato registrado pela casa ou por fonte oficial), convencional (fim regular na data constitucional), intervalo (forma confirmada e dia exato fora do alcance das fontes, com os limites na tabela curada). Preenchida nesta versao para senador, deputado federal, presidente, vice-presidente, governador e vice-governador, e vazia nos demais cargos e no mandato sem saida"),
  exercicio_confirmado = c("data ISO", "Data mais recente em que o titular foi observado em exercicio (MUNIC; registro da candidatura a reeleicao no TSE)"),
  universo_comparavel = c("logico", "A unidade (municipio, UF ou casa nacional) teve varredura por fonte institucional naquela eleicao. Dentro do universo a ausencia de saida observada significa que o mandato terminou; fora dele significa que ninguem olhou. Use esta coluna antes de comparar forma de saida entre esferas, cargos ou anos"),
  chave_tse = c("texto", "Chave de juncao com bases derivadas do TSE (ano_eleicao, cd_cargo, sg_ue, nr_candidato), unica em todas as linhas (R/53 reprova duplicata). Preferir ao SQ_CANDIDATO, que so e unico nacionalmente a partir de 2010"),
  motivo_sem_saida = c("texto", "Motivo da ausencia de forma de saida observada, derivado de universo_comparavel, mandato_fim e forma_saida sem fonte nova. Valores nao_se_aplica (saida observada), em_curso (fim convencional posterior a data de referencia da reconstrucao, output/data_referencia.txt), fim_regular_presumido (dentro do universo comparavel, em que a casa foi varrida) e fonte_ausente (fora dele)"),
  fonte_exercicio = c("texto", "Fonte da confirmacao de exercicio (ibge_munic; ibge_munic_ampliado; tse_reeleicao; receita_cnpj; sapl_presenca; tce_ac), separadas por ponto e virgula quando ha mais de uma. sapl_presenca e a lista de presenca em plenario, na data da ultima presenca; ibge_munic_ampliado e a MUNIC lida por atributos, so no lado que confirma o eleito; tce_ac e o acordao de contas anuais do Tribunal de Contas do Acre, na data de inicio do periodo julgado"),
  antecessor_id = c("texto", "id_pessoa de quem ocupou a mesma posicao no mandato anterior (executivos)"),
  sucessor_id = c("texto", "id_pessoa de quem ocupou a mesma posicao no mandato seguinte (executivos)"),
  via_sucessao = c("texto", "Via institucional da sucessao; v1 = nova_eleicao (executivos)"),
  reeleito_mesma_pessoa = c("logico", "Antecessor imediato e a mesma pessoa (executivos)")
)
dic_pos <- list(
  id_mandato = c("texto", "Chave para mandatos.csv"),
  id_pessoa = c("texto", "Identificador estavel da pessoa"),
  cd_cargo = c("inteiro", "Codigo TSE do cargo"),
  cargo = c("texto", "Descricao do cargo"),
  esfera = c("texto", "Esfera (municipal, estadual ou federal)"),
  sg_uf = c("texto", "UF da posicao"),
  sg_ue = c("texto", "Codigo TSE da unidade eleitoral"),
  nm_ue = c("texto", "Nome da unidade eleitoral"),
  ano_eleicao = c("inteiro", "Ano da eleicao de origem"),
  sg_partido = c("texto", "Sigla do partido na eleicao de origem"),
  nr_partido = c("inteiro", "Numero do partido na eleicao de origem"),
  ano = c("inteiro", "Ano-calendario em que a pessoa ocupa a posicao")
)
dic_pess <- list(
  id_pessoa = c("texto", "Identificador estavel da pessoa"),
  nome = c("texto", "Nome completo (registro mais recente no TSE)"),
  nome_urna_recente = c("texto", "Nome de urna mais recente"),
  dt_nascimento = c("data ISO", "Data de nascimento"),
  genero = c("texto", "Genero declarado ao TSE"),
  nr_titulo_eleitoral = c("texto", "Titulo de eleitor (12 digitos, divulgado pelo TSE)"),
  nr_cpf = c("texto", "CPF (11 digitos, divulgado pelo TSE)"),
  n_mandatos = c("inteiro", "Numero de mandatos da pessoa no banco"),
  primeiro_ano_eleito = c("inteiro", "Primeiro ano em que foi eleita"),
  ultimo_ano_eleito = c("inteiro", "Ultimo ano em que foi eleita"),
  chave_dedup = c("texto", "Regra que identificou a pessoa (titulo, cpf ou nome_nascimento)"),
  dedup_ponte_nome_nascimento = c("logico", "Nome e data de nascimento sao a unica chave de identidade desta pessoa, sem titulo nem CPF em nenhuma candidatura (auditoria em data_v1/auditoria_homonimos.csv)"),
  dedup_auditoria = c("texto", "Classificacao da auditoria de homonimos (consistente, suspeito, indeterminado) para as pessoas auditadas em data_v1/auditoria_homonimos.csv; nao_auditado para as demais"),
  dedup_suspeita_fusao = c("texto", "Leitura da fusao por documento que R/03 manteve porque nome ou nascimento coincidem entre as candidaturas unidas por um titulo ou CPF (nome_diferente_mesmo_nascimento, mesmo_nome_nascimento_diferente, mesmo_nome_e_nascimento_documento_reemitido); NA quando nao ha suspeita. A lista das fusoes desfeitas esta em output/verificacao/fusao_documentos_separacao.csv")
)

dic_fil <- list(
  id_pessoa = c("texto", "Identificador estavel da pessoa"),
  sigla_partido = c("texto", "Sigla do partido da filiacao"),
  sg_uf = c("texto", "UF da zona eleitoral da filiacao"),
  cd_municipio_tse = c("texto", "Codigo TSE do municipio da zona eleitoral"),
  data_filiacao = c("data ISO", "Data de filiacao"),
  situacao_registro = c("texto", "Situacao do registro na lista (REGULAR, CANCELADO, DESFILIADO, ...)"),
  data_desfiliacao = c("data ISO", "Data de desfiliacao, quando houve"),
  data_cancelamento = c("data ISO", "Data de cancelamento, quando houve"),
  motivo = c("texto", "Motivo de desfiliacao ou cancelamento"),
  fonte = c("texto", "lista_atual (extracao TSE 2026) ou lista_antiga (listas historicas ate 2019)"),
  data_referencia = c("data ISO", "Data de extracao ou processamento da lista de origem")
)
## dicionarios das tabelas auxiliares de posse, exercicio e saida
dic_aux <- list(
  exercicio_camara = list(
    titulo = "exercicio_camara.csv — periodos de exercicio na Camara dos Deputados (API dadosabertos.camara.leg.br)",
    chave = c("id_deputado_camara", "legislatura", "data_inicio_exercicio", "condicao"),
    d = list(
      id_pessoa = "Identificador da pessoa no BOCEL (NA para suplente fora do banco)",
      id_mandato = "Mandato do BOCEL a que o periodo pertence (cd_cargo 6, eleicao que abre a legislatura)",
      id_deputado_camara = "id do deputado na API da Camara", legislatura = "Numero da legislatura (51 = 1999-2003 ... 57 = 2023-2027)",
      ano_eleicao = "Ano da eleicao que abriu a legislatura", nome_civil = "Nome civil na Camara", nome_parlamentar = "Nome parlamentar",
      cpf = "CPF (divulgado pela Camara)", dt_nascimento = "Data de nascimento (Camara)", dt_falecimento = "Data de falecimento (arquivo em massa da Camara)",
      sg_uf = "UF", sg_partido = "Partido no ultimo status", condicao = "titular ou suplente", efetivado = "Suplente efetivado como titular (logico)",
      data_inicio_exercicio = "Inicio do periodo (evento Entrada no historico)", data_fim_exercicio = "Fim do periodo (evento Saida); NA se em curso",
      situacao_final = "Situacao ao fim do periodo (texto da Camara ou marcador do script)", forma_saida = "Forma de saida no vocabulario fechado",
      descricao_entrada = "Texto original do evento de entrada", descricao_saida = "Texto original do evento de saida",
      regra_pareamento = "Regra que pareou com o BOCEL (cpf, nome+nascimento, ...)", fonte = "camara_api", url_fonte = "URL do recurso na API")),
  exercicio_senado = list(
    titulo = "exercicio_senado.csv — exercicios no Senado Federal (API legis.senado.leg.br/dadosabertos)",
    chave = c("codigo_exercicio"),
    d = list(
      id_pessoa = "Identificador da pessoa no BOCEL (NA para suplente fora do banco)", id_mandato = "Mandato do BOCEL (cd_cargo 5)",
      codigo_senador = "Codigo do parlamentar no Senado", nome_senado = "Nome completo no Senado", nome_parlamentar = "Nome parlamentar",
      dt_nascimento = "Data de nascimento (Senado)", sexo = "Sexo (Senado)", sg_uf = "UF", legislatura = "Primeira legislatura do mandato",
      leg_segunda = "Segunda legislatura do mandato (mandato de 8 anos)", ano_eleicao_ref = "Ano da eleicao de referencia",
      codigo_mandato = "Codigo do mandato no Senado", condicao = "titular ou suplente", participacao = "Descricao de participacao (Senado)",
      titular_codigo = "Codigo do titular, para suplentes", titular_nome = "Nome do titular, para suplentes", codigo_exercicio = "Codigo do exercicio",
      data_inicio_exercicio = "Inicio do exercicio", data_fim_exercicio = "Fim do exercicio; NA se em curso", sigla_causa = "Sigla da causa de afastamento (Senado)",
      causa_afastamento = "Descricao da causa de afastamento (Senado)", forma_saida = "Forma de saida no vocabulario fechado",
      metodo_pareamento = "Regra que pareou com o BOCEL", fonte = "senado_api", url_fonte = "URL do recurso na API")),
  exercicio_assembleias = list(
    titulo = "exercicio_assembleias.csv — deputados estaduais por legislatura nas Assembleias com fonte estruturada",
    chave = c("uf", "id_fonte", "legislatura", "data_inicio_exercicio"),
    d = list(
      uf = "UF", fonte = "Tipo de fonte (sapl_api, html_legislatura)", legislatura = "Legislatura na numeracao da casa", ano_eleicao = "Ano da eleicao que abriu a legislatura",
      nome = "Nome na fonte", nome_normalizado = "Nome normalizado (maiusculas, sem acento)", nome_completo = "Nome completo na fonte, quando ha",
      data_nascimento = "Data de nascimento na fonte, quando ha", partido = "Partido na fonte", condicao = "titular ou suplente",
      data_inicio_exercicio = "Inicio do exercicio (NA quando a fonte so lista a legislatura)", data_fim_exercicio = "Fim do exercicio (NA quando a fonte so lista a legislatura)",
      causa_original = "Causa de encerramento no texto da fonte", forma_saida = "Forma de saida no vocabulario fechado (nao_observado quando a fonte nao traz datas)",
      id_pessoa_bocel = "Identificador da pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL (cd_cargo 7 ou 8)", metodo_pareamento = "Regra que pareou com o BOCEL",
      url = "URL da fonte", id_fonte = "Identificador do parlamentar na fonte", votos_fonte = "Votos informados pela fonte, quando ha", sexo_fonte = "Sexo informado pela fonte, quando ha")),
  exercicio_assembleias_2 = list(
    titulo = "exercicio_assembleias_2.csv — deputados estaduais e distritais nas casas coletadas em 29/08/2026, uma frente por UF",
    chave = c("uf", "id_fonte", "legislatura", "data_inicio_exercicio"),
    d = list(
      uf = "UF", fonte = "Tipo de fonte na casa (portal, ficha individual, diario, memorial)", legislatura = "Legislatura na numeracao da casa", ano_eleicao = "Ano da eleicao que abriu a legislatura",
      nome = "Nome na fonte", nome_normalizado = "Nome normalizado (maiusculas, sem acento)", nome_completo = "Nome completo na fonte, quando ha",
      data_nascimento = "Data de nascimento na fonte, quando ha", partido = "Partido na fonte", condicao = "titular, suplente ou nao_informado",
      data_inicio_exercicio = "Inicio do exercicio na fonte", data_fim_exercicio = "Fim do exercicio na fonte",
      causa_original = "Causa de encerramento no texto da fonte", forma_saida = "Forma de saida no vocabulario fechado",
      id_pessoa_bocel = "Identificador da pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL (cd_cargo 7 ou 8)", metodo_pareamento = "Regra que pareou com o BOCEL",
      url = "URL da fonte", id_fonte = "Identificador do parlamentar na fonte", votos_fonte = "Votos informados pela fonte, quando ha", sexo_fonte = "Sexo informado pela fonte, quando ha")),
  saida_cargo_incompativel = list(
    titulo = "saida_cargo_incompativel.csv — mandatos encerrados por posse em outro cargo eletivo, deduzido do proprio banco",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato encerrado", id_pessoa = "Pessoa no BOCEL", cargo = "Cargo encerrado",
             sg_uf = "UF do mandato encerrado", cargo_incompativel = "Cargo assumido, incompativel com o primeiro",
             id_mandato_incompativel = "Mandato do cargo assumido",
             data_posse_incompativel = "Inicio do mandato assumido",
             data_fim_inferida = "Vespera da posse no cargo assumido, tomada como fim do primeiro",
             forma_saida = "Sempre 'outro': a evidencia estabelece o fim antecipado, nao o ato que o produziu")),
  exercicio_assembleias_2_cobertura = list(
    titulo = "exercicio_assembleias_2_cobertura.csv — o que a coleta por portal alcancou em cada casa",
    chave = c("uf"),
    d = list(uf = "UF", linhas = "Linhas coletadas", pareadas = "Linhas com mandato do BOCEL", com_forma = "Mandatos com forma de saida observada")),
  wikidata_mandatos = list(
    titulo = "wikidata_mandatos.csv — mandatos de governador, vice-governador, deputado estadual e prefeito no Wikidata (P39 e P6)",
    chave = c("statement"),
    d = list(
      qid = "QID da pessoa no Wikidata", statement = "Identificador do statement", nome_wikidata = "Rotulo da pessoa", nome_completo = "Nome completo (P1477), quando ha",
      dt_nascimento = "Data de nascimento (P569)", dt_morte = "Data de morte (P570)", cargo = "Cargo na nomenclatura do BOCEL", posicao_wd = "Rotulo da posicao no Wikidata",
      entidade = "UF ou municipio da posicao", ibge_entidade = "Codigo IBGE da entidade (P1585), quando ha", uf = "UF da posicao",
      inicio = "Inicio do mandato (P580)", fim = "Fim do mandato (P582)", causa_fim_original = "Causa do termino (P1534), texto original", forma_saida = "Forma de saida no vocabulario fechado",
      partido_wd = "Partido no statement (P102)", eleicao_wd = "Eleicao no statement (P2715)", substitui_qid = "QID de quem foi substituido (P1365)", substituido_por_qid = "QID do sucessor (P1366)",
      id_pessoa_bocel = "Identificador da pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL pareado", tipo_pareamento = "Regra de pareamento", url = "URL do item")),
  wikidata_obitos = list(
    titulo = "wikidata_obitos.csv — obitos (P570) de pessoas do BOCEL dentro da janela de um mandato",
    chave = c("id_pessoa", "id_mandato"),
    d = list(id_pessoa = "Identificador da pessoa no BOCEL", id_mandato = "Mandato em cuja janela cai a morte", cargo = "Cargo do mandato",
             data_morte = "Data de morte (P570)", dentro_do_mandato = "Sempre TRUE nesta tabela", fonte = "wikidata")),
  eleicoes_suplementares = list(
    titulo = "eleicoes_suplementares.csv — pleitos suplementares majoritarios do TSE (novas eleicoes apos cassacao ou anulacao)",
    chave = c("id_pleito"),
    d = list(id_pleito = "Identificador do pleito (S<ano>_<unidade>_<cargo>_<data>)", ano_arquivo = "Ano do arquivo do TSE em que o pleito consta",
             unidade_posicao = "Unidade da posicao", sg_uf = "UF", sg_ue = "Codigo TSE da unidade eleitoral", nm_ue = "Nome da unidade", cd_cargo = "Codigo do cargo", cargo = "Cargo",
             esfera = "Esfera", dt_eleicao_suplementar = "Data do 1o turno do pleito", dt_turno_decisivo = "Data do turno decisivo", nr_turno = "Turno decisivo",
             n_candidatos = "Candidaturas ao cargo titular no pleito", status_vencedor = "vencedor_identificado ou motivo da ausencia",
             vencedor_nome = "Nome do vencedor", vencedor_titulo = "Titulo de eleitor do vencedor", vencedor_cpf = "CPF do vencedor", vencedor_id_pessoa = "id_pessoa do vencedor, se ja consta do BOCEL",
             votos_vencedor = "Votos do vencedor no turno decisivo", sq_candidato = "SQ_CANDIDATO do vencedor", nr_candidato = "Numero do vencedor", sg_partido_vencedor = "Partido do vencedor",
             situacao_totalizacao = "Situacao de totalizacao do vencedor", fonte_situacao = "Origem da situacao (cadastro ou votacao)",
             vice_nome = "Nome do vice eleito no pleito", vice_titulo = "Titulo do vice", vice_cpf = "CPF do vice", vice_id_pessoa = "id_pessoa do vice, se consta do BOCEL",
             id_mandato_ordinario_afetado = "Mandato ordinario vigente na data do pleito", ocupante_ordinario_id_pessoa = "Pessoa do mandato ordinario afetado",
             dt_dentro_mandato_ordinario = "Data do pleito cai na janela do mandato ordinario (logico)", momento = "antes_da_posse ou durante_o_mandato",
             vencedor_e_o_ocupante_ordinario = "O vencedor e a mesma pessoa do mandato ordinario (logico)", mandatos_ordinarios_candidatos = "Mandatos ordinarios candidatos ao pareamento",
             realizado_atrasado = "Pleito realizado fora do calendario ordinario (logico)",
             realizado_ate_data_do_arquivo = "Pleito realizado ate a data de geracao do arquivo do TSE (logico)",
             chave_colide_com_ordinaria = "Alguma candidatura do pleito tem chave igual a de candidatura ordinaria do mesmo ano (SQ reiniciado ate 2008; logico)",
             vencedor_chave_colide_com_ordinaria = "A chave do vencedor colide com candidatura ordinaria (logico)",
             fonte = "tse_consulta_cand+votacao")),
  eleicoes_suplementares_vereador = list(
    titulo = "eleicoes_suplementares_vereador.csv — eleitos em pleitos suplementares proporcionais (vereador)",
    chave = c("id_pleito", "sq_candidato"),
    d = list(id_pleito = "Identificador do pleito suplementar", ano_arquivo = "Ano do arquivo do TSE", unidade_posicao = "Unidade da posicao (municipio)",
             sg_uf = "UF", sg_ue = "Codigo TSE do municipio", nm_ue = "Nome do municipio", cd_cargo = "Codigo do cargo (13)", cargo = "VEREADOR", esfera = "municipal",
             dt_eleicao_suplementar = "Data do pleito", nr_turno = "Turno", eleito_id_pessoa = "id_pessoa do eleito, se consta do BOCEL", eleito_cpf = "CPF do eleito",
             eleito_titulo = "Titulo de eleitor do eleito", eleito_nome = "Nome do eleito", votos = "Votos do eleito", sq_candidato = "SQ_CANDIDATO", nr_candidato = "Numero do candidato",
             sg_partido = "Partido", situacao_totalizacao = "Situacao de totalizacao", fonte_situacao = "Origem da situacao", fonte = "tse_consulta_cand+votacao",
             n_mandatos_ordinarios_vereador_na_unidade = "Mandatos ordinarios de vereador na unidade e eleicao", id_mandato_ordinario_mesma_pessoa = "Mandato ordinario da mesma pessoa, se houver")),
  exercicio_assembleias_lacunas = list(
    titulo = "exercicio_assembleias_lacunas.csv — inventario das fontes das Assembleias Legislativas por UF",
    chave = c("uf"),
    d = list(uf = "UF", casa = "Sigla da casa legislativa", viavel = "Fonte estruturada encontrada (sim, parcial, nao)", tipo_fonte = "Tipo de fonte (sapl_api, html_legislatura, html_atual, api_atual, nenhum)",
             cobertura_legislaturas = "Legislaturas cobertas pela fonte", coletada = "Fonte coletada e integrada (logico)", motivo = "Descricao da fonte ou da lacuna")),
  municipios_tse_ibge = list(
    titulo = "municipios_tse_ibge.csv — correspondencia entre codigo TSE (sg_ue) e codigo IBGE dos municipios",
    chave = c("sg_ue"),
    d = list(sg_ue = "Codigo TSE do municipio (5 digitos)", sg_uf = "UF", id_municipio_ibge = "Codigo IBGE de 7 digitos", nome_ibge = "Nome do municipio no IBGE",
             origem = "Origem da correspondencia (diretorio Base dos Dados ou nome+UF)", id_municipio_ibge6 = "Codigo IBGE de 6 digitos",
             uf_divergente = "UF do TSE difere da do IBGE (logico)")),
  exercicio_camaras_municipais = list(
    titulo = "exercicio_camaras_municipais.csv — mandatos de vereador nas camaras municipais com SAPL (Interlegis)",
    chave = c("dominio", "id_mandato_sapl"),
    d = list(sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF", dominio = "Dominio da instancia SAPL", legislatura_numero = "Numero da legislatura na casa",
             legislatura_inicio = "Inicio da legislatura (SAPL)", legislatura_fim = "Fim da legislatura (SAPL)", ano_eleicao_bocel = "Eleicao do BOCEL correspondente", nome_fonte = "Nome completo no SAPL",
             nome_parlamentar = "Nome parlamentar no SAPL", nome_normalizado = "Nome normalizado", sexo = "Sexo no SAPL", titular = "Titular (logico); FALSE = suplente",
             data_inicio_mandato = "Inicio do mandato (SAPL)", data_fim_mandato = "Fim do mandato (SAPL)", data_diploma = "Data de expedicao do diploma",
             tipo_afastamento = "Tipo de afastamento registrado no mandato (SAPL)", votos = "Votos recebidos (SAPL)", observacao = "Observacao do mandato (SAPL)",
             forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL (cd_cargo 13)", metodo_pareamento = "Regra de pareamento",
             id_mandato_sapl = "id do mandato na API", url = "URL do registro na API")),
  exercicio_camaras_municipais_cobertura = list(
    titulo = "exercicio_camaras_municipais_cobertura.csv — cobertura do SAPL municipal por UF e eleicao",
    chave = c("uf", "ano_eleicao"),
    d = list(uf = "UF", ano_eleicao = "Eleicao", n_mandatos_bocel = "Mandatos de vereador no BOCEL", n_camaras_com_sapl = "Camaras com SAPL respondendo", n_pareados = "Mandatos pareados", taxa = "Pareados / mandatos do BOCEL")),
  wikipedia_estadual = list(
    titulo = "wikipedia_estadual.csv — governadores, vices e deputados estaduais nas listas da Wikipedia em portugues",
    chave = c("url_pagina", "nome_normalizado", "inicio"),
    d = list(uf = "UF", cargo = "Cargo na nomenclatura do BOCEL", legislatura = "Legislatura ou periodo da lista", ano_eleicao_bocel = "Eleicao do BOCEL correspondente", nome_wiki = "Nome na Wikipedia", nome_normalizado = "Nome normalizado",
             partido_wiki = "Partido na lista", condicao = "titular ou suplente", inicio = "Inicio do mandato na lista", fim = "Fim do mandato na lista", observacao_original = "Observacao da lista (texto)", forma_saida = "Forma de saida no vocabulario fechado",
             sucessor_wiki = "Sucessor indicado na lista", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url_pagina = "URL da pagina", revisao = "Identificador da revisao da pagina")),
  wikipedia_estadual_cobertura = list(
    titulo = "wikipedia_estadual_cobertura.csv — cobertura das listas da Wikipedia por UF, cargo e eleicao",
    chave = c("uf", "cargo", "ano_eleicao"),
    d = list(uf = "UF", cargo = "Cargo", ano_eleicao = "Eleicao", n_bocel = "Mandatos no BOCEL", n_wiki = "Linhas na Wikipedia", n_pareados = "Pareados", taxa = "Pareados / mandatos do BOCEL")),
  wikipedia_prefeitos = list(
    titulo = "wikipedia_prefeitos.csv — prefeitos nas paginas 'Lista de prefeitos de <municipio>' da Wikipedia",
    chave = c("url", "nome_normalizado", "inicio"),
    d = list(sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF", municipio = "Municipio", nome_wiki = "Nome na Wikipedia", nome_normalizado = "Nome normalizado", partido_wiki = "Partido na lista",
             vice_wiki = "Vice-prefeito indicado na lista, quando ha coluna", inicio = "Inicio na lista", fim = "Fim na lista", observacao_original = "Observacao da lista",
             forma_saida = "Forma de saida no vocabulario fechado", condicao = "eleito, vice_em_exercicio ou interino", ano_eleicao_bocel = "Eleicao do BOCEL correspondente",
             id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL (cd_cargo 11 ou 12)", metodo_pareamento = "Regra de pareamento", url = "URL da pagina", revisao = "Identificador da revisao")),
  wikipedia_prefeitos_cobertura = list(
    titulo = "wikipedia_prefeitos_cobertura.csv — cobertura das listas de prefeitos da Wikipedia por eleicao",
    chave = c("ano_eleicao"),
    d = list(ano_eleicao = "Eleicao", n_bocel = "Prefeitos eleitos no BOCEL", n_pareados = "Mandatos pareados", n_municipios = "Municipios com lista pareada", taxa = "Pareados / mandatos do BOCEL")),
  datajud_processos_cassacao = list(
    titulo = "datajud_processos_cassacao.csv — processos eleitorais de cassacao e perda de mandato nos TREs e no TSE (DataJud/CNJ, sem partes)",
    chave = c("tribunal", "numero_processo"),
    d = list(tribunal = "Tribunal (TRE-UF ou TSE)", grau = "Grau de jurisdicao", numero_processo = "Numero unico do processo", classe = "Classe processual (AIJE, AIME, RCED, Representacao)",
             assuntos = "Assuntos separados por ponto e virgula", cargo_assunto = "Cargo citado nos assuntos ('Cargo - Prefeito' etc.)",
             relevante_cassacao = "Classe AIJE/AIME/RCED ou assunto de cassacao, captacao ilicita, perda de mandato ou abuso (logico)",
             data_ajuizamento = "Data de ajuizamento", ano_ajuizamento = "Ano de ajuizamento",
             uf = "UF do tribunal (BR para o TSE)", orgao_julgador = "Orgao julgador", id_municipio_ibge = "Municipio do orgao julgador (IBGE)", sg_ue = "Municipio do orgao julgador (TSE)",
             n_movimentos = "Numero de movimentos", ultimo_movimento = "Ultimo movimento", data_ultimo_movimento = "Data do ultimo movimento",
             indicio_cassacao = "Algum movimento com cassacao, perda de mandato ou procedencia (logico)", data_indicio = "Data do ultimo movimento com indicio", movimento_indicio = "Texto do movimento com indicio")),
  datajud_sinal_unidade_eleicao = list(
    titulo = "datajud_sinal_unidade_eleicao.csv — sinal agregado de litigio de cassacao por municipio do orgao julgador, cargo e eleicao de referencia",
    chave = c("uf", "sg_ue", "cargo_assunto", "eleicao_ref"),
    d = list(uf = "UF", sg_ue = "Municipio (TSE)", id_municipio_ibge = "Municipio (IBGE)", cargo_assunto = "Cargo citado nos assuntos", eleicao_ref = "Eleicao de referencia, isto e, a ultima eleicao do calendario do cargo (municipal ou geral) ate o ano do ajuizamento; sem cargo nos assuntos, ultima eleicao de qualquer tipo ate esse ano",
             n_processos = "Processos", n_aije = "AIJE", n_aime = "AIME", n_rced = "RCED", n_com_indicio_cassacao = "Processos com indicio de cassacao", primeira_data_indicio = "Primeira data de indicio")),
  exercicio_camaras_sem_sapl = list(
    titulo = "exercicio_camaras_sem_sapl.csv — mandatos de vereador em camaras municipais sem SAPL (portais com outros sistemas)",
    chave = c("dominio", "legislatura_numero", "nome_normalizado", "data_inicio_mandato"),
    d = list(sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF", dominio = "Host do portal da camara", legislatura_numero = "Legislatura na casa",
             legislatura_inicio = "Inicio da legislatura", legislatura_fim = "Fim da legislatura", ano_eleicao_bocel = "Eleicao do BOCEL correspondente", nome_fonte = "Nome no portal",
             nome_parlamentar = "Nome parlamentar no portal", nome_normalizado = "Nome normalizado", titular = "Titular (logico)", data_inicio_mandato = "Inicio do mandato no portal",
             data_fim_mandato = "Fim do mandato no portal", tipo_afastamento = "Afastamento registrado", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL",
             id_mandato_bocel = "Mandato do BOCEL (cd_cargo 13)", metodo_pareamento = "Regra de pareamento", sistema = "Sistema do portal (detectado)", url = "URL da origem")),
  exercicio_camaras_sem_sapl_cobertura = list(
    titulo = "exercicio_camaras_sem_sapl_cobertura.csv — cobertura dos portais sem SAPL por UF e eleicao",
    chave = c("uf", "ano_eleicao"),
    d = list(uf = "UF", ano_eleicao = "Eleicao", n_mandatos_bocel = "Mandatos de vereador no BOCEL", n_camaras_coletadas = "Camaras coletadas", n_pareados = "Mandatos pareados", taxa = "Pareados / mandatos do BOCEL")),
  diarios_eventos = list(
    titulo = "diarios_eventos.csv — eventos de posse e saida em diarios oficiais municipais (Querido Diario)",
    chave = c("url_diario", "termo", "id_mandato_bocel"),
    d = list(id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE do municipio", data_diario = "Data do diario", termo = "Termo de busca que localizou o excerto",
             evento_inferido = "Evento no vocabulario fechado", trecho = "Excerto do diario", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL",
             metodo_pareamento = "Regra de pareamento do nome no trecho", confianca = "alta, media ou baixa", url_diario = "URL do diario")),
  diarios_mandatos_saida = list(
    titulo = "diarios_mandatos_saida.csv — saida inferida por mandato a partir do evento de confianca alta mais antigo nos diarios oficiais",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato do BOCEL", id_pessoa = "Pessoa", forma_saida = "Forma de saida no vocabulario fechado", data_fim_inferida = "Data do diario do evento",
             termo = "Termo do evento", confianca = "Confianca do pareamento", url = "URL do diario")),
  exercicio_assembleias_historico = list(
    titulo = "exercicio_assembleias_historico.csv — deputados estaduais das casas sem historico no portal, por fontes historicas (PDFs, memoriais)",
    chave = c("uf", "fonte", "legislatura", "nome_normalizado", "data_inicio_exercicio"),
    d = list(uf = "UF", fonte = "Fonte historica (memorial, PDF, diario da assembleia)", legislatura = "Legislatura na casa", ano_eleicao = "Eleicao que abriu a legislatura",
             nome = "Nome na fonte", nome_normalizado = "Nome normalizado", nome_completo = "Nome completo, quando ha", data_nascimento = "Nascimento, quando ha", partido = "Partido na fonte",
             condicao = "titular ou suplente", data_inicio_exercicio = "Inicio do exercicio", data_fim_exercicio = "Fim do exercicio", causa_original = "Causa no texto da fonte",
             forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL (cd_cargo 7 ou 8)", metodo_pareamento = "Regra de pareamento",
             url = "URL da fonte", id_fonte = "Identificador na fonte", votos_fonte = "Votos na fonte", sexo_fonte = "Sexo na fonte")),
  tce_gestores = list(
    titulo = "tce_gestores.csv — gestores (prefeitos, presidentes de camara) nos cadastros dos Tribunais de Contas, grupo A",
    chave = c("uf", "unidade_gestora", "nome", "data_inicio"),
    d = list(uf = "UF", tribunal = "Tribunal de Contas", unidade_gestora = "Unidade gestora na fonte", tipo_unidade = "prefeitura ou camara", id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE",
             nome = "Nome na fonte", cpf = "CPF na fonte, quando exposto", cargo_fonte = "Cargo na fonte", cargo_bocel = "Cargo na nomenclatura do BOCEL", data_inicio = "Inicio da gestao", data_fim = "Fim da gestao",
             situacao_fonte = "Situacao na fonte", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url = "URL da fonte")),
  tce_gestores_b = list(
    titulo = "tce_gestores_b.csv — gestores nos cadastros dos Tribunais de Contas, grupo B",
    chave = c("uf", "unidade_gestora", "nome", "data_inicio"),
    d = list(uf = "UF", tribunal = "Tribunal de Contas", unidade_gestora = "Unidade gestora na fonte", tipo_unidade = "prefeitura ou camara", id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE",
             nome = "Nome na fonte", cpf = "CPF na fonte, quando exposto", cargo_fonte = "Cargo na fonte", cargo_bocel = "Cargo na nomenclatura do BOCEL", data_inicio = "Inicio da gestao", data_fim = "Fim da gestao",
             situacao_fonte = "Situacao na fonte", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url = "URL da fonte")),
  tce_gestores_cobertura = list(titulo = "tce_gestores_cobertura.csv — cobertura dos cadastros dos TCEs (grupo A) por UF, cargo e eleicao", chave = c("uf", "cargo", "ano_eleicao"),
    d = list(uf = "UF", cargo = "Cargo", ano_eleicao = "Eleicao", n_bocel = "Mandatos no BOCEL", n_pareados = "Pareados", taxa = "Pareados / mandatos do BOCEL")),
  tce_gestores_b_cobertura = list(titulo = "tce_gestores_b_cobertura.csv — cobertura dos cadastros dos TCEs (grupo B) por UF, cargo e eleicao", chave = c("uf", "cargo", "ano_eleicao"),
    d = list(uf = "UF", cargo = "Cargo", ano_eleicao = "Eleicao", n_bocel = "Mandatos no BOCEL", n_pareados = "Pareados", taxa = "Pareados / mandatos do BOCEL")),
  # 12/09/2026: tabelas de fonte que entram no deposito da v1 e nao tinham secao no livro
  tce_gestores_c = list(
    titulo = "tce_gestores_c.csv — gestores nos cadastros e julgamentos de contas dos Tribunais de Contas, grupo C (a lista do TCM-GO publica registro repetido, reproduzido aqui)",
    chave = c("fonte", "sg_ue", "nome", "exercicio", "situacao_fonte", "ordinal da repeticao publicada pela fonte"),
    d = list(uf = "UF", tribunal = "Tribunal de Contas", unidade_gestora = "Unidade gestora na fonte", tipo_unidade = "prefeitura ou camara", id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE",
             nome = "Nome na fonte", cpf = "CPF na fonte, quando exposto", cargo_fonte = "Cargo na fonte", cargo_bocel = "Cargo na nomenclatura do BOCEL", data_inicio = "Inicio da gestao", data_fim = "Fim da gestao",
             situacao_fonte = "Situacao na fonte", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url = "URL da fonte",
             fonte = "Base do tribunal de onde veio a linha", nome_municipio_fonte = "Nome do municipio na fonte", exercicio = "Exercicio financeiro da gestao ou das contas", ano_eleicao_fonte = "Eleicao inferida do exercicio na fonte",
             ano_eleicao = "Eleicao do BOCEL a que a gestao foi atribuida", relacao_chapa_eleita = "titular_eleito, vice_eleito ou indeterminado, quando pareado",
             id_mandato_titular_substituido = "Mandato do titular quando quem responde e o vice")),
  tce_gestores_c_cobertura = list(titulo = "tce_gestores_c_cobertura.csv — cobertura dos Tribunais de Contas do grupo C por UF, cargo e eleicao", chave = c("uf", "cargo", "ano_eleicao"),
    d = list(uf = "UF", cargo = "Cargo", ano_eleicao = "Eleicao", n_bocel = "Mandatos no BOCEL", n_pareados = "Pareados", taxa = "Pareados / mandatos do BOCEL")),
  tce_gestores_d = list(
    titulo = "tce_gestores_d.csv — gestores nos cadastros dos Tribunais de Contas, grupo D (a lista de contas irregulares do TCE-AM publica registro repetido, reproduzido aqui)",
    chave = c("fonte", "sg_ue", "unidade_gestora", "nome", "cargo_fonte", "exercicio", "data_inicio", "situacao_fonte", "ordinal da repeticao publicada pela fonte"),
    d = list(uf = "UF", tribunal = "Tribunal de Contas", unidade_gestora = "Unidade gestora na fonte", tipo_unidade = "prefeitura ou camara", id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE",
             nome = "Nome na fonte", cpf = "CPF na fonte, quando exposto", cargo_fonte = "Cargo na fonte", cargo_bocel = "Cargo na nomenclatura do BOCEL", data_inicio = "Inicio da gestao", data_fim = "Fim da gestao",
             situacao_fonte = "Situacao na fonte", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url = "URL da fonte",
             fonte = "Base do tribunal de onde veio a linha", nome_municipio_fonte = "Nome do municipio na fonte", exercicio = "Exercicio financeiro da gestao ou das contas", ano_eleicao = "Eleicao do BOCEL a que a gestao foi atribuida")),
  tce_gestores_d_cobertura = list(titulo = "tce_gestores_d_cobertura.csv — cobertura dos Tribunais de Contas do grupo D por UF, cargo e eleicao", chave = c("uf", "cargo", "ano_eleicao"),
    d = list(uf = "UF", cargo = "Cargo", ano_eleicao = "Eleicao", n_bocel = "Mandatos no BOCEL", n_pareados = "Pareados", n_com_saida = "Pareados com forma de saida", taxa = "Pareados / mandatos do BOCEL", taxa_saida = "Pareados com saida / mandatos do BOCEL")),
  tce_gestores_e = list(
    titulo = "tce_gestores_e.csv — responsaveis nomeados nos acordaos de contas anuais do TCE-AC (fonte_exercicio tce_ac)",
    chave = c("unidade_gestora", "nome", "exercicio", "url"),
    d = list(uf = "UF", tribunal = "Tribunal de Contas", unidade_gestora = "Unidade gestora na fonte", tipo_unidade = "prefeitura ou camara", id_municipio_ibge = "Codigo IBGE", sg_ue = "Codigo TSE",
             nome = "Nome na fonte", cpf = "CPF na fonte, quando exposto", cargo_fonte = "Cargo na fonte", cargo_bocel = "Cargo na nomenclatura do BOCEL", data_inicio = "Inicio da gestao", data_fim = "Fim da gestao",
             situacao_fonte = "Situacao na fonte", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL", id_mandato_bocel = "Mandato do BOCEL", metodo_pareamento = "Regra de pareamento", url = "URL da fonte",
             fonte = "Base do tribunal de onde veio a linha", nome_municipio_fonte = "Nome do municipio na fonte", exercicio = "Exercicio financeiro da gestao ou das contas", ano_eleicao = "Eleicao do BOCEL a que o exercicio foi atribuido")),
  tce_gestores_e_cobertura = list(titulo = "tce_gestores_e_cobertura.csv — cobertura dos acordaos do TCE-AC por tipo de unidade e eleicao", chave = c("tipo_unidade", "ano_eleicao"),
    d = list(tipo_unidade = "prefeitura ou camara", ano_eleicao = "Eleicao", linhas = "Linhas extraidas dos acordaos", pareadas = "Linhas pareadas a mandato do BOCEL", municipios = "Municipios com ao menos uma linha")),
  diarios_cobertura_bocel = list(
    titulo = "diarios_cobertura_bocel.csv — alcance dos diarios oficiais municipais sobre os mandatos do BOCEL, por cargo",
    chave = c("cd_cargo"),
    d = list(cargo = "Cargo", cd_cargo = "Codigo do cargo no TSE", n_mandatos = "Mandatos no BOCEL", n_em_cidade_coberta = "Mandatos em municipio com diario no Querido Diario",
             n_com_janela_coberta = "Mandatos cuja janela tem diario publicado", n_sem_saida = "Mandatos sem forma de saida observada", n_sem_saida_janela_coberta = "Sem saida observada e com janela coberta")),
  diarios_taxa_uf_ano = list(
    titulo = "diarios_taxa_uf_ano.csv — mandatos nomeados nos diarios oficiais e saida de confianca alta, por UF e eleicao",
    chave = c("sg_uf", "ano_eleicao"),
    d = list(sg_uf = "UF", ano_eleicao = "Eleicao", n_mandatos = "Mandatos no BOCEL", n_sem_saida = "Mandatos sem forma de saida observada", n_mandatos_nomeados = "Mandatos nomeados em algum excerto",
             n_mandatos_saida_alta = "Mandatos com saida de confianca alta", taxa_nomeados = "Nomeados / mandatos", taxa_saida_alta = "Saida de confianca alta / mandatos")),
  exercicio_assembleias_historico_cobertura = list(
    titulo = "exercicio_assembleias_historico_cobertura.csv — cobertura das fontes historicas das Assembleias sem historico no portal, por UF e eleicao",
    chave = c("uf", "ano_eleicao"),
    d = list(uf = "UF", ano_eleicao = "Eleicao", n_bocel = "Mandatos de deputado estadual no BOCEL", n_bocel_sem_saida = "Mandatos sem forma de saida observada", n_fonte = "Linhas na fonte historica",
             n_pareados = "Mandatos pareados", taxa = "Pareados / mandatos do BOCEL", n_pareados_com_saida = "Pareados com forma de saida", n_wiki_pareados = "Mandatos ja pareados pela Wikipedia",
             n_novos_vs_wiki = "Pareados pela fonte historica e nao pela Wikipedia", viavel = "A casa tem fonte historica utilizavel (logico)", fonte = "Descricao da fonte encontrada")),
  exercicio_camaras_sem_sapl_2_cobertura = list(
    titulo = "exercicio_camaras_sem_sapl_2_cobertura.csv — cobertura da segunda rodada de portais sem SAPL por UF e eleicao",
    chave = c("uf", "ano_eleicao"),
    d = list(uf = "UF", ano_eleicao = "Eleicao", n_mandatos_bocel = "Mandatos de vereador no BOCEL", n_municipios_bocel = "Municipios no BOCEL", n_pareados = "Mandatos pareados",
             n_camaras_coletadas = "Camaras coletadas", taxa = "Pareados / mandatos do BOCEL")),
  mandatos_forma_saida_suplementar = list(
    titulo = "mandatos_forma_saida_suplementar.csv — mandato ordinario afetado por eleicao suplementar, com o sucessor",
    chave = c("id_mandato_ordinario_afetado"),
    d = list(id_mandato_ordinario_afetado = "Mandato ordinario do BOCEL", forma_saida = "Forma de saida inferida", momento = "durante_o_mandato ou antes_da_posse",
             data_fim_inferida = "Vespera do pleito suplementar", id_pleito_suplementar = "Pleito suplementar (eleicoes_suplementares.csv)", sucessor_via_suplementar = "id_pessoa do vencedor do pleito",
             sucessor_nome = "Nome do vencedor", sucessor_titulo = "Titulo de eleitor do vencedor", via_sucessao = "Via da sucessao", id_pleito_sucessor = "Pleito que deu o sucessor",
             dt_pleito_sucessor = "Data desse pleito", status_vencedor = "vencedor_identificado ou sem_vencedor_marcado_no_tse", n_pleitos_suplementares = "Pleitos suplementares na mesma unidade e mandato",
             vencedor_e_o_ocupante_ordinario = "O vencedor e a mesma pessoa do mandato afetado (logico)", fonte = "Arquivos do TSE usados")),
  munic_resumo_substituicoes = list(
    titulo = "munic_resumo_substituicoes.csv — o que cada edicao da MUNIC permite afirmar sobre substituicao do prefeito eleito",
    chave = c("ano_munic"),
    d = list(ano_munic = "Edicao da MUNIC", ano_eleicao_bocel = "Eleicao do BOCEL correspondente", n_municipios_munic = "Municipios na edicao", n_pareados_bocel = "Municipios pareados ao BOCEL",
             tem_nome = "A edicao traz o nome do prefeito (logico)", n_com_nome = "Com nome", n_com_sexo = "Com sexo", n_com_idade = "Com idade", n_com_partido = "Com partido",
             n_eleito_em_exercicio = "Eleito em exercicio", n_outro_em_exercicio = "Outra pessoa em exercicio", n_indeterminado = "Indeterminado",
             n_subst_vice = "Substituto e o vice eleito", n_subst_terceiro = "Substituto e terceiro", n_subst_indeterminado = "Substituto indeterminado",
             n_match_nome_exato = "Nome identico ao do eleito", n_match_nome_parcial = "Nome parcialmente igual", n_match_sexo = "Mesmo sexo", n_match_idade = "Idade compativel",
             prop_outro_entre_pareados = "Outra pessoa / pareados", prop_vice_entre_outros = "Vice / outra pessoa")),
  pessoas_flags_dedup = list(
    titulo = "pessoas_flags_dedup.csv — marcas da auditoria de homonimos sobre a deduplicacao de pessoas",
    chave = c("id_pessoa"),
    d = list(id_pessoa = "Pessoa no BOCEL", ponte_nome_nascimento = "A pessoa une componentes so por nome e nascimento (logico)",
             so_nome_nascimento = "A identificacao depende so de nome e nascimento (logico)", classificacao = "nao_auditado, consistente ou suspeito")),
  exercicio_camaras_sem_sapl_2 = list(
    titulo = "exercicio_camaras_sem_sapl_2.csv — mandatos de vereador em portais de camaras sem SAPL, segunda rodada de coletores",
    chave = c("dominio", "legislatura_numero", "nome_normalizado", "data_inicio_mandato"),
    d = list(sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF", dominio = "Host do portal", legislatura_numero = "Legislatura na casa", legislatura_inicio = "Inicio da legislatura",
             legislatura_fim = "Fim da legislatura", ano_eleicao_bocel = "Eleicao do BOCEL", nome_fonte = "Nome no portal", nome_parlamentar = "Nome parlamentar", nome_normalizado = "Nome normalizado", titular = "Titular (logico)",
             data_inicio_mandato = "Inicio do mandato", data_fim_mandato = "Fim do mandato", tipo_afastamento = "Afastamento registrado", forma_saida = "Forma de saida no vocabulario fechado", id_pessoa_bocel = "Pessoa no BOCEL",
             id_mandato_bocel = "Mandato do BOCEL (cd_cargo 13)", metodo_pareamento = "Regra de pareamento", sistema = "Sistema do portal",
             so_legislatura_atual = "O portal so publica a legislatura em curso (logico)", url = "URL da origem")),
  migracao_partidaria = list(
    titulo = "migracao_partidaria.csv — trocas de legenda ao longo da carreira",
    chave = c("id_pessoa", "fonte", "data_evento", "partido_destino"),
    d = list(id_pessoa = "Pessoa", fonte = "Origem do registro (filiacao, mandato_para_mandato, mandato_para_candidatura)",
             partido_origem = "Sigla de origem", partido_destino = "Sigla de destino",
             origem_canon = "Sigla de origem apos unificar mudanca de nome e incorporacao", destino_canon = "Sigla de destino apos a mesma unificacao",
             troca_efetiva = "FALSE quando a mudanca e so de nome ou incorporacao de legenda (logico)",
             data_origem = "Data do vinculo anterior", data_evento = "Data da troca", ano_evento = "Ano da troca",
             dias = "Dias entre o vinculo anterior e a troca", sg_uf = "UF do registro", regiao = "Regiao",
             ano_origem = "Ano da eleicao de origem, quando a fonte e mandato", ano_destino = "Ano da eleicao ou candidatura de destino",
             cargo_origem = "Cargo de origem", cargo_destino = "Cargo de destino")),
  migracao_territorial = list(
    titulo = "migracao_territorial.csv — deslocamento territorial entre mandatos consecutivos",
    chave = c("id_pessoa", "ano_origem", "ano_destino", "unidade_destino"),
    d = list(id_pessoa = "Pessoa", ano_origem = "Eleicao do mandato anterior", ano_destino = "Eleicao do mandato seguinte",
             cargo_origem = "Cargo anterior", cargo_destino = "Cargo seguinte", uf_origem = "UF anterior", uf_destino = "UF seguinte",
             regiao_origem = "Regiao anterior", regiao_destino = "Regiao seguinte", unidade_origem = "Unidade da posicao anterior",
             unidade_destino = "Unidade da posicao seguinte", municipio_origem = "Municipio anterior", municipio_destino = "Municipio seguinte",
             tipo = "mesma unidade, entre municipios da mesma UF, entre UFs da mesma regiao ou entre regioes")),
  migracao_territorial_candidaturas = list(
    titulo = "migracao_territorial_candidaturas.csv — candidatura em UF onde a pessoa nunca teve mandato",
    chave = c("id_pessoa", "ano_candidatura", "uf_candidatura", "cargo_candidatura"),
    d = list(id_pessoa = "Pessoa", ano_candidatura = "Ano da candidatura", uf_candidatura = "UF disputada", cargo_candidatura = "Cargo disputado",
             ufs_com_mandato_ate_entao = "UFs em que a pessoa ja tinha mandato ate aquele ano", ultimo_ano_mandato = "Ano do ultimo mandato antes da candidatura",
             regiao_candidatura = "Regiao da candidatura")),
  raca_eleitos = list(
    titulo = "raca_eleitos.csv — cor ou raca autodeclarada ao TSE por mandato, 2010-2024",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato", id_pessoa = "Pessoa", ano_eleicao = "Eleicao", cargo = "Cargo", sg_uf = "UF", regiao = "Regiao",
             genero = "Genero declarado", cor_raca = "Cor ou raca autodeclarada (preenchida a partir de 2014)",
             negra = "Preta ou parda (logico)", instrucao = "Grau de instrucao declarado", ocupacao = "Ocupacao declarada")),
  munic_prefeitos = list(
    titulo = "munic_prefeitos.csv — prefeito(a) em exercicio segundo a MUNIC/IBGE, pareado ao eleito do BOCEL",
    chave = c("id_municipio_ibge", "ano_munic"),
    d = list(id_municipio_ibge = "Codigo IBGE de 7 digitos", id_municipio_ibge6 = "Codigo IBGE de 6 digitos", ano_munic = "Edicao da MUNIC", data_referencia = "Data ou ano de referencia da edicao",
             sg_ue = "Codigo TSE do municipio", nome_prefeito_munic = "Nome do prefeito na MUNIC (2004 e 2005)", sexo = "Sexo na MUNIC", idade = "Idade na MUNIC", escolaridade = "Escolaridade na MUNIC",
             cor_raca = "Cor ou raca na MUNIC (2021)", partido = "Partido na MUNIC", partido_eleito_munic = "Partido pelo qual foi eleito (MUNIC)", partido_atual_munic = "Partido atual (MUNIC)",
             exercicio_ano_anterior = "Exercia o cargo no ano anterior (MUNIC)", ano_eleicao_bocel = "Eleicao do mandato vigente no BOCEL", id_mandato_bocel = "Mandato do prefeito eleito vigente",
             id_pessoa_bocel = "Pessoa do prefeito eleito vigente", nome_prefeito_bocel = "Nome do eleito no BOCEL", genero_bocel = "Genero no BOCEL", dt_nascimento_bocel = "Nascimento no BOCEL", idade_bocel = "Idade do eleito na data de referencia",
             match_nome = "Nome da MUNIC igual ao do eleito (logico; NA sem nome)", match_nome_parcial = "Nome parcialmente igual (logico)", jw_nome = "Similaridade Jaro-Winkler dos nomes",
             match_sexo = "Sexo compativel (logico)", dif_idade = "Diferenca de idade", match_idade = "Idade compativel +-1 (logico)", match_idade_ampla = "Idade compativel +-3 (logico)",
             status = "eleito_em_exercicio, outro_em_exercicio ou indeterminado", criterio_status = "Criterio que definiu o status", substituto_provavel = "vice, terceiro ou NA",
             criterio_substituto = "Criterio do substituto", id_mandato_vice = "Mandato do vice eleito", id_pessoa_vice = "Pessoa do vice eleito",
             vice_match_nome = "Nome da MUNIC igual ao do vice (logico)", vice_match_sexo = "Sexo compativel com o vice (logico)", vice_match_idade = "Idade compativel com o vice (logico)")),
  sinais_tse_exercicio = list(
    titulo = "sinais_tse_exercicio.csv — candidatura seguinte de cada ocupante e sinal de reeleicao (TSE)",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato do BOCEL", id_pessoa = "Pessoa", ano_eleicao = "Eleicao do mandato", cd_cargo = "Codigo do cargo", cargo = "Cargo", esfera = "Esfera", unidade_posicao = "Unidade da posicao",
             ano_candidatura_seguinte = "Ano da eleicao seguinte em que a pessoa concorreu", candidatura_seguinte = "Concorreu na eleicao seguinte (logico)",
             mesmo_cargo_mesma_unidade = "Concorreu ao mesmo cargo na mesma unidade (logico)", cd_cargo_candidatura_seguinte = "Cargo disputado na eleicao seguinte",
             unidade_candidatura_seguinte = "Unidade disputada na eleicao seguinte", sq_candidatura_seguinte = "SQ_CANDIDATO da candidatura seguinte",
             st_reeleicao = "ST_REELEICAO do TSE na candidatura seguinte (S/N)", fonte_st_reeleicao = "Origem do campo (consulta_cand ou DivulgaCand)",
             exercicio_confirmado_em = "Data em que o TSE registra o titular em exercicio (registro da candidatura a reeleicao)",
             partido_mandato = "Partido no mandato", partido_candidatura_seguinte = "Partido na candidatura seguinte", mudou_partido = "Mudou de partido (logico)",
             situacao_candidatura_seguinte = "Situacao da candidatura seguinte", resultado_candidatura_seguinte = "Resultado da candidatura seguinte")),
  auditoria_homonimos = list(
    titulo = "auditoria_homonimos.csv — pessoas cuja identificacao depende so de nome e nascimento",
    chave = c("id_pessoa"),
    d = list(id_pessoa = "Pessoa", n_mandatos = "Mandatos da pessoa", n_titulos_distintos = "Titulos de eleitor distintos na componente", n_cpfs_distintos = "CPFs distintos na componente",
             n_sem_titulo_nem_cpf = "Candidaturas sem titulo nem CPF", n_comp_12 = "Componentes que a pessoa teria so com titulo e CPF",
             ponte_nome_nascimento = "A regra nome+nascimento foi a unica ponte (logico)", so_nome_nascimento = "A pessoa so tem nome e nascimento como chave (logico)",
             generos = "Generos observados", ufs = "UFs observadas", n_unidades = "Unidades observadas", anos = "Anos observados", cargos = "Cargos observados",
             n_nomes_distintos = "Nomes distintos", n_nasc_distintos = "Datas de nascimento distintas", classificacao = "consistente, suspeito ou indeterminado",
             grau_suspeita = "Grau da suspeita", justificativa = "Justificativa da classificacao")),
  ## nivel de ocupacao (decisao de 30/08/2026): a cadeira e uma so, o que cresce e
  ## a contagem de quem a ocupou
  ocupacoes = list(
    titulo = "ocupacoes.csv — quem ocupou cada cadeira (titular, suplente convocado, vice que assumiu, interino)",
    chave = c("id_ocupacao"),
    d = list(
      id_ocupacao = "Identificador da ocupacao", id_mandato = "Cadeira ocupada; NA quando a fonte nao permite identificar qual foi",
      id_pessoa = "Pessoa que ocupou (cadastro do nucleo ou de suplentes)",
      tipo_ocupante = "titular, suplente, vice_assumiu ou interino", ordem_ocupacao = "1 no titular, 2 em diante nos demais",
      id_lista = "Lista partidaria de origem do suplente (coligacao, federacao ou partido)",
      ano_eleicao = "Eleicao que criou a cadeira", cd_cargo = "Codigo do cargo (TSE)", cargo = "Cargo",
      esfera = "federal, estadual ou municipal", sg_uf = "UF", sg_ue = "Unidade eleitoral nos cargos municipais",
      nm_ue = "Nome da unidade", unidade_posicao = "Unidade em que a posicao e disputada",
      sg_partido_ocupante = "Partido de quem ocupou", sg_partido_titular = "Partido do titular eleito para a cadeira",
      partido_difere_do_titular = "A cadeira trocou de partido por dentro, sem eleicao (logico)",
      data_inicio = "Inicio da ocupacao", data_fim = "Fim da ocupacao",
      origem_data_inicio = "fonte quando ha ato ou registro, convencao quando e a data legal do mandato",
      forma_saida = "Forma de saida da ocupacao, no vocabulario fechado", fonte_forma_saida = "Fonte da forma de saida",
      vinculo_cadeira = "Caminho pelo qual a cadeira foi identificada, entre eleicao, chapa_senado, cadeira_unica_da_unidade, lista_vaga_unica, lista_data ou casa_legislatura (nao identificada)",
      fonte = "Fonte do registro de ocupacao", url = "URL de origem",
      ordem_suplencia = "Posicao do ocupante na fila da lista, quando suplente",
      regra_pareamento_ocupacao = "Regra que ligou o nome da fonte a pessoa da lista de suplencia",
      datas_inconsistentes = "A fonte publicou fim anterior ao inicio (logico); a linha nao foi corrigida")),
  lista_suplencia = list(
    titulo = "lista_suplencia.csv — a fila de suplencia de cada lista partidaria",
    chave = c("id_suplencia"),
    d = list(
      id_suplencia = "Identificador da suplencia", chave_cand = "Chave da candidatura no TSE",
      id_pessoa = "Pessoa (mesmo espaco de identificador do nucleo)", ano_eleicao = "Eleicao",
      cd_cargo_registro = "Cargo do registro no TSE (9 e 10 sao os suplentes da chapa do Senado)",
      cargo_registro = "Cargo do registro", cd_cargo = "Cargo da cadeira que a suplencia alcanca",
      cargo = "Cargo da cadeira", sg_uf = "UF", sg_ue = "Unidade eleitoral", nm_ue = "Nome da unidade",
      unidade_posicao = "Unidade em que a posicao e disputada",
      id_lista = "Lista partidaria, seja coligacao, federacao ou partido isolado, pelo SQ_COLIGACAO do TSE",
      nr_partido = "Numero do partido", sg_partido = "Partido", tp_agremiacao = "Tipo de agremiacao",
      nm_coligacao = "Nome da coligacao", composicao_coligacao = "Composicao da coligacao",
      nome = "Nome do candidato", nome_urna = "Nome de urna", nr_candidato = "Numero na urna",
      sq_candidato = "Sequencial da candidatura", votos_nominais = "Votacao nominal",
      ordem_suplencia = "Posicao na fila da lista, derivada da votacao com desempate pelo mais idoso",
      ordem_suplencia_tse = "Ordem publicada pelo TSE, existente so em 2016",
      fonte_ordem = "derivada_votacao, tse_e_derivada ou chapa_senado",
      empate_na_ordem = "Houve empate de votos e nascimento na lista (logico)",
      pessoa_tambem_eleita = "A pessoa foi eleita em alguma eleicao do banco (logico)",
      id_mandato_cadeira = "Cadeira que a suplencia alcanca, quando a chapa a nomeia")),
  pessoas_suplentes = list(
    titulo = "pessoas_suplentes.csv — pessoas que figuram como suplentes e nunca foram eleitas",
    chave = c("id_pessoa"),
    d = list(
      id_pessoa = "Identificador no mesmo espaco de pessoas.csv, sem colisao", nome = "Nome",
      nome_urna_recente = "Nome de urna mais recente", dt_nascimento = "Nascimento", genero = "Genero",
      nr_titulo_eleitoral = "Titulo de eleitor", nr_cpf = "CPF",
      n_candidaturas_suplente = "Candidaturas em que ficou como suplente",
      primeiro_ano_suplente = "Primeira eleicao", ultimo_ano_suplente = "Ultima eleicao",
      chave_dedup = "Chave que identificou a pessoa (titulo, cpf ou nome_nascimento)",
      condicao_no_banco = "suplente_nao_eleito")),
  suplentes_identidade = list(
    titulo = "suplentes_identidade.csv — candidatura de suplente e a pessoa a que pertence",
    chave = c("chave_cand"),
    d = list(chave_cand = "Chave da candidatura", id_pessoa = "Pessoa", ano_eleicao = "Eleicao",
             cd_cargo = "Cargo", sg_uf = "UF", sg_ue = "Unidade eleitoral", sg_partido = "Partido",
             regra_id = "Regra que atribuiu a pessoa, entre nucleo, titulo, cpf, nome_nascimento, componente e novo",
             pessoa_tambem_eleita = "A pessoa ja existia no cadastro do nucleo (logico)")),
  exercicio_camaras_generico = list(
    titulo = "exercicio_camaras_generico.csv — vereadores na relacao publicada pela casa, extraida do HTML que ja estava em disco",
    chave = c("id_mandato_bocel"),
    d = list(
      sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF",
      dominio = "Dominio do portal da casa", legislatura_numero = "Nao informado por esta fonte",
      legislatura_inicio = "Inicio convencional do mandato", legislatura_fim = "Fim convencional",
      ano_eleicao_bocel = "Eleicao do mandato pareado", nome_fonte = "Nome como aparece na pagina",
      nome_parlamentar = "Idem", nome_normalizado = "Idem, normalizado",
      titular = "Sempre verdadeiro, porque a relacao publicada nao distingue suplente",
      data_inicio_mandato = "Nao informado", data_fim_mandato = "Nao informado",
      tipo_afastamento = "Nao informado",
      forma_saida = "Sempre nao_observado: a pagina e retrato, e nao historico",
      id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato",
      metodo_pareamento = "Campo casado e criterio de desambiguacao da legislatura",
      sistema = "generico_html_em_disco", so_legislatura_atual = "A legislatura veio do criterio de mandato unico",
      url = "Caminho do arquivo em disco que sustenta a linha",
      exercicio_observado = "A pessoa consta da relacao nominal da casa (logico)")),
  exercicio_sapl_recuperado = list(
    titulo = "exercicio_sapl_recuperado.csv — vereadores recuperados de instancias SAPL cujo cache existia sem linha extraida",
    chave = c("id_mandato_bocel"),
    d = list(sg_ue = "Codigo TSE", id_municipio_ibge = "Codigo IBGE", uf = "UF", dominio = "Dominio",
             legislatura_numero = "Legislatura", legislatura_inicio = "Inicio", legislatura_fim = "Fim",
             ano_eleicao_bocel = "Eleicao", nome_fonte = "Nome na fonte", nome_parlamentar = "Nome parlamentar",
             nome_normalizado = "Nome normalizado", titular = "Titular na fonte",
             data_inicio_mandato = "Inicio", data_fim_mandato = "Fim", tipo_afastamento = "Afastamento",
             forma_saida = "Forma de saida", id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato",
             metodo_pareamento = "Regra", sistema = "Sistema", so_legislatura_atual = "Logico", url = "URL")),
  exercicio_assembleias_inventario = list(
    titulo = "exercicio_assembleias_inventario.csv — deputados estaduais nos portais de transparencia ja baixados (folha e frequencia)",
    chave = c("uf", "url", "nome_normalizado"),
    d = list(uf = "UF", fonte = "Fonte na casa", legislatura = "Legislatura", ano_eleicao = "Eleicao",
             nome = "Nome", nome_normalizado = "Nome normalizado", nome_completo = "Nome completo",
             data_nascimento = "Nascimento", partido = "Partido", condicao = "Condicao na fonte",
             data_inicio_exercicio = "Inicio observado", data_fim_exercicio = "Fim observado",
             causa_original = "Texto da fonte, com a nota de rebaixamento quando houve",
             forma_saida = "Sempre nao_observado: folha de pagamento e frequencia sao inferencia, e nao registro de saida",
             id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato", metodo_pareamento = "Regra", url = "URL")),
  sapl_observacao_eventos = list(
    titulo = "sapl_observacao_eventos.csv — o ato que a Casa escreveu no campo de texto livre do SAPL, um por registro de mandato",
    chave = c("chave_sapl"),
    d = list(sg_ue = "Codigo TSE do municipio", id_municipio_ibge = "Codigo IBGE", uf = "UF",
             dominio = "Dominio da instancia SAPL", ano_eleicao = "Eleicao do mandato pareado",
             legislatura_numero = "Legislatura na fonte", legislatura_inicio = "Inicio da legislatura",
             legislatura_fim = "Fim da legislatura", nome_fonte = "Nome como a Casa registra",
             nome_parlamentar = "Nome parlamentar", titular_registro = "Marca de titular no cadastro do SAPL",
             id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato",
             metodo_pareamento = "Regra que ligou o registro ao mandato",
             papel = "Papel que o texto atribui a pessoa, quem saiu ou quem entrou",
             causa = "Causa lida no texto, em vocabulario fechado",
             tipo_evento = "fim_de_mandato, interregno_temporario ou afastamento_sem_retorno_observado",
             retorno_evidenciado = "Evidencia que sustenta o retorno, seja o texto que narra a volta, seja o fim do mandato que alcanca o fim da legislatura",
             forma_saida_causa = "Forma de saida quando o evento encerra o mandato; vazio no interregno",
             data_evento = "Data do ato, quando o texto a traz", data_retorno = "Data da volta, quando o texto a traz",
             precisao_data = "Como a data foi lida do texto",
             titular_nome = "Nome do titular substituido, quando o texto o nomeia",
             id_mandato_titular = "Mandato do titular nomeado, quando o pareamento resolve",
             id_pessoa_titular = "Pessoa do titular nomeado", regra_titular = "Regra que pareou o titular",
             confianca = "alta, media ou baixa, conforme a especificidade do texto e a presenca de data",
             chave_sapl = "dominio#id do registro de mandato no SAPL", id_mandato_sapl = "Id do registro na instancia",
             url = "Endereco do registro", observacao = "Texto original, preservado")),
  sapl_observacao_titular = list(
    titulo = "sapl_observacao_titular.csv — um registro por mandato de vereador do BOCEL alcancado pelo texto livre",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato do BOCEL", tipo_evento = "Tipo do evento lido",
             causa = "Causa em vocabulario fechado", forma_saida_obs = "Forma de saida quando o evento encerra o mandato",
             data_evento = "Data do ato", data_retorno = "Data da volta",
             retorno_evidenciado = "Evidencia do retorno", origem = "De que linha do SAPL o evento veio",
             confianca = "alta, media ou baixa", url = "Endereco do registro",
             trecho = "Trecho do texto que sustenta a leitura", sg_ue = "Municipio", ano_eleicao = "Eleicao",
             nome = "Nome do vereador", sg_partido = "Partido", mandato_inicio = "Inicio convencional",
             mandato_fim = "Fim convencional", forma_saida_atual = "O que o banco ja registrava",
             fonte_atual = "Fonte do que o banco ja registrava", data_fim_atual = "Fim que o banco ja registrava",
             sem_saida_hoje = "O mandato nao tinha saida observada antes desta leitura",
             ganho = "O que esta leitura acrescenta, entre forma nova, forma ja conhecida, interregno e afastamento sem retorno")),
  sapl_presenca_exercicio = list(
    titulo = "sapl_presenca_exercicio.csv — janela de exercicio do vereador vista pela lista de presenca em plenario, uma por pessoa e legislatura",
    chave = c("uf", "sg_ue", "parlamentar", "legislatura"),
    d = list(uf = "UF", sg_ue = "Codigo TSE do municipio", dominio = "Dominio da instancia SAPL",
             legislatura = "Legislatura na instancia", parlamentar = "Id do parlamentar na instancia",
             nome_fonte = "Nome como a Casa registra", nome_parlamentar = "Nome parlamentar",
             ano_eleicao_bocel = "Eleicao do mandato pareado", id_pessoa_bocel = "Pessoa",
             id_mandato_bocel = "Mandato; vazio quando dois parlamentares da Casa apontam para o mesmo mandato",
             titular = "Marca de titular no pareamento", chave_sapl = "dominio#id do registro de mandato",
             n_registros_sapl = "Quantos registros de mandato a Casa abriu para esta pessoa nesta legislatura",
             data_inicio_sapl = "Inicio mais antigo entre esses registros", data_fim_sapl = "Fim mais recente entre eles",
             legislatura_inicio = "Inicio convencional", legislatura_fim = "Fim convencional",
             n_sessoes_leg = "Sessoes da Casa na legislatura, com presenca registrada",
             primeira_sessao = "Data da primeira sessao da legislatura", ultima_sessao = "Data da ultima",
             n_presencas = "Sessoes em que a pessoa consta presente",
             primeira_presenca = "Data da primeira presenca", ultima_presenca = "Data da ultima presenca",
             dias_ate_a_primeira = "Dias entre a primeira sessao da Casa e a primeira presenca da pessoa",
             dias_da_ultima_ao_fim = "Dias entre a ultima presenca e a ultima sessao da Casa",
             sessoes_antes_da_entrada = "Sessoes da Casa anteriores a estreia da pessoa",
             sessoes_depois_da_saida = "Sessoes posteriores a ultima presenca")),
  sapl_presenca_lacunas = list(
    titulo = "sapl_presenca_lacunas.csv — afastamento visto de fora, por sequencia de sessoes perdidas entre duas presencas",
    chave = c("id_lacuna"),
    d = list(uf = "UF", sg_ue = "Municipio", parlamentar = "Id do parlamentar na instancia",
             legislatura = "Legislatura", ultima_presenca_antes = "Ultima presenca antes da lacuna",
             idx = "Posicao dessa sessao na sequencia da Casa",
             primeira_presenca_depois = "Primeira presenca depois da lacuna",
             idx_prox = "Posicao dessa sessao na sequencia", dias = "Duracao da lacuna em dias",
             sessoes_perdidas = "Sessoes da Casa que a pessoa perdeu no intervalo",
             id_lacuna = "Identificador da lacuna",
             n_entrantes = "Pessoas que estreiam na Casa dentro da janela, candidatas a suplente convocado",
             entrantes_na_janela = "Ids dessas pessoas, separados por ponto e virgula",
             dominio = "Dominio da instancia", ano_eleicao_bocel = "Eleicao", nome_fonte = "Nome na fonte",
             id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato", titular = "Marca de titular",
             confianca = "alta quando ha entrante e mandato pareado, media quando ha entrante sem pareamento, baixa quando nao ha entrante")),
  sapl_presenca_cobertura = list(
    titulo = "sapl_presenca_cobertura.csv — o que cada instancia SAPL serviu de sessao e de presenca",
    chave = c("uf", "sg_ue"),
    d = list(uf = "UF", sg_ue = "Codigo TSE do municipio",
             n_sessoes = "Sessoes plenarias devolvidas pela instancia",
             n_presencas = "Registros de presenca devolvidos")),
  cobertura_saida = list(
    titulo = "cobertura_saida.csv — cobertura da forma de saida por esfera, cargo, UF e eleicao, dentro e fora do universo comparavel",
    chave = c("esfera", "cargo", "sg_uf", "ano_eleicao"),
    d = list(esfera = "Esfera da federacao", cargo = "Cargo", sg_uf = "UF", ano_eleicao = "Eleicao",
             cadeiras = "Cadeiras eleitas", no_universo_comparavel = "Cadeiras no universo em que a saida e logicamente observavel",
             com_forma_saida = "Cadeiras com forma de saida observada", com_forma_saida_no_universo = "Idem, dentro do universo comparavel",
             com_data_posse = "Cadeiras com data de posse", com_exercicio = "Cadeiras com exercicio confirmado",
             pct_universo = "Percentual das cadeiras que esta no universo comparavel", pct_saida = "Percentual com forma de saida",
             pct_saida_no_universo = "Percentual com forma de saida dentro do universo comparavel")),
  munic_exercicio_ampliado = list(
    titulo = "munic_exercicio_ampliado.csv — o que cada edicao da MUNIC permite afirmar sobre o prefeito em exercicio",
    chave = c("sg_ue", "ano_munic"),
    d = list(sg_ue = "Codigo TSE", id_municipio_ibge = "Codigo IBGE", uf = "UF", dominio = "Fonte",
             legislatura_numero = "Nao se aplica", legislatura_inicio = "Inicio do mandato",
             legislatura_fim = "Fim do mandato", ano_eleicao_bocel = "Eleicao", nome_fonte = "Nome quando a edicao traz",
             nome_parlamentar = "Idem", nome_normalizado = "Idem normalizado", titular = "Logico",
             data_inicio_mandato = "Inicio", data_fim_mandato = "Fim", tipo_afastamento = "Nao se aplica",
             forma_saida = "Sempre nao_observado", id_pessoa_bocel = "Pessoa", id_mandato_bocel = "Mandato",
             metodo_pareamento = "Regra", sistema = "ibge_munic", so_legislatura_atual = "Logico",
             url = "Fonte", ano_munic = "Edicao da MUNIC", data_referencia = "Data de referencia da edicao",
             afirmacao = "O que a edicao permite afirmar, entre exercicio confirmado por nome, por atributos, outra pessoa em exercicio e indeterminado")),
  mandatos_lista = list(
    titulo = "mandatos_lista.csv — cadeira e a lista partidaria que a ganhou",
    chave = c("id_mandato"),
    d = list(chave_cand = "Chave da candidatura", id_mandato = "Cadeira", id_pessoa = "Titular eleito",
             cd_cargo = "Cargo", ano_eleicao = "Eleicao", unidade_posicao = "Unidade",
             sg_partido = "Partido do titular", forma_saida = "Forma de saida do titular",
             data_fim_efetiva = "Fim efetivo do mandato do titular", data_posse = "Posse do titular",
             mandato_inicio = "Inicio convencional", mandato_fim = "Fim convencional",
             id_lista = "Lista partidaria", sq_col = "SQ_COLIGACAO do TSE", NR_PARTIDO = "Numero do partido")),
  # 21/09/2026: seis tabelas do recorte federal (R/58, R/59 e R/60) sem secao propria no livro ate a v1.0
  camara_biografia_eventos = list(
    titulo = "camara_biografia_eventos.csv — eventos de mandato lidos na biografia oficial do deputado federal na Camara",
    chave = c("id_deputado_camara", "secao", "leg_inicio", "tipo", "datas"),
    d = list(id_deputado_camara = "Id do deputado na API da Camara", secao = "Secao da biografia em que o evento apareceu",
             leg_inicio = "Legislatura a que o evento se refere", tipo = "Tipo do evento (posse, licenca, renuncia, perda_mandato, aposentadoria, afastamento, suplencia, falecimento)",
             datas = "Datas do evento, no texto da biografia", trecho = "Excerto da pagina que sustenta a leitura",
             url = "URL da biografia", ano_eleicao = "Eleicao do BOCEL correspondente a legislatura")),
  camara_biografia_posses = list(
    titulo = "camara_biografia_posses.csv — data de posse do deputado federal por legislatura, na biografia oficial da Camara",
    chave = c("id_deputado_camara", "leg_inicio"),
    d = list(id_deputado_camara = "Id do deputado na API da Camara", leg_inicio = "Legislatura da posse", sg_uf = "UF",
             data_posse = "Data de posse na biografia", url = "URL da biografia", ano_eleicao = "Eleicao do BOCEL correspondente")),
  interregnos_legislativo_federal = list(
    titulo = "interregnos_legislativo_federal.csv — periodos fora do exercicio de senador e deputado federal que nao encerram o mandato (licenca, afastamento, suspensao)",
    chave = c("id_mandato", "inicio_fora"),
    d = list(id_mandato = "Mandato do BOCEL (cd_cargo 5 ou 6)", id_pessoa = "Pessoa", casa = "senado ou camara", cargo = "Cargo",
             sg_uf = "UF", ano_eleicao = "Eleicao do mandato", tipo = "Causa do afastamento no vocabulario da fonte (licenca, afastamento, suspensao)",
             causa_original = "Texto original da causa na fonte", inicio_fora = "Inicio do periodo fora do exercicio",
             fim_fora = "Fim do periodo, quando a casa registra a volta ou o mandato ja acabou", retorno_observado = "A casa registrou o retorno ao exercicio (logico)",
             fonte = "senado_api ou camara_api", url_fonte = "URL do recurso na API")),
  saida_executivos = list(
    titulo = "saida_executivos.csv — forma de saida de presidente, vice-presidente, governador e vice-governador (uma linha por mandato)",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato do BOCEL (cd_cargo 1 a 4)", cargo = "Cargo", sg_uf = "UF", ano_eleicao = "Eleicao do mandato",
             forma_saida = "Forma de saida no vocabulario fechado (NA quando em curso)", data_fim_efetiva = "Data da saida ou do fim convencional",
             precisao_data_fim = "Precisao da data (ato, convencional ou intervalo)", em_curso = "Mandato em curso na data de referencia (logico)",
             via = "Como a forma foi estabelecida, entre evento_curado (ref/eventos_governos_fonte_oficial.csv e ref/eventos_presidencia_fonte_oficial.csv), listas_wikipedia_e_wikidata, lista_wikipedia, derivado_do_titular_curado, em_curso_sem_evento e sem_confirmacao",
             confianca = "alta ou media, conforme a via", fonte = "fonte_oficial_curada, base_dhbb_curada, noticia_orgao_publico_curada, pista_nao_oficial ou wikipedia (lib/tipo_fonte.R)",
             url_fonte = "URL da primeira fonte citada no evento curado, ou da pagina da Wikipedia", fonte_2 = "URL da segunda fonte citada no evento curado, quando ha",
             assumiu = "Nome de quem assumiu o cargo no ato, quando o evento curado o registra", observacao = "Nota da curadoria sobre o evento, quando ha")),
  saida_legislativo_federal = list(
    titulo = "saida_legislativo_federal.csv — forma de saida de senador e deputado federal pela historia completa de exercicio na propria casa (uma linha por mandato)",
    chave = c("id_mandato"),
    d = list(id_mandato = "Mandato do BOCEL (cd_cargo 5 ou 6)", casa = "senado ou camara", cargo = "Cargo", sg_uf = "UF", ano_eleicao = "Eleicao do mandato",
             data_posse = "Data de posse", data_fim_efetiva = "Data do ato que encerrou o mandato, ou do fim convencional",
             precisao_data_fim = "Precisao da data (ato ou convencional)", forma_saida = "Forma de saida no vocabulario fechado (NA quando em curso)",
             causa_original = "Texto original da causa na fonte", em_curso = "Mandato em curso na data de referencia (logico)",
             afastado_no_fim = "O titular estava licenciado ou afastado no fim regular do mandato, sem ter voltado (logico; NA quando nao se aplica)",
             cobertura = "Fonte que resolveu o mandato, entre historico_da_casa, sem_historico_na_api, biografia_oficial_camara, biografia_sem_posse_na_legislatura e fonte_oficial_curada",
             fonte = "senado_api, camara_api ou fonte_oficial_curada", url_fonte = "URL da fonte")),
  ocupantes_legislativo_federal = list(
    titulo = "ocupantes_legislativo_federal.csv — quem a casa registrou como titular de senado ou camara sem ter mandato correspondente no arquivo do TSE",
    chave = c("casa", "sg_uf", "legislatura", "nome_alt", "data_inicio"),
    d = list(casa = "senado ou camara", cd_cargo = "Codigo do cargo (5 ou 6)", sg_uf = "UF", legislatura = "Legislatura", id_pessoa = "Pessoa no BOCEL, quando identificada",
             nome_fonte = "Nome civil na fonte", nome_alt = "Nome parlamentar na fonte", efetivado = "Suplente efetivado como titular, na Camara (logico)",
             data_inicio = "Inicio do periodo de exercicio", data_fim = "Fim do periodo de exercicio", causa_original = "Causa de saida no texto da fonte",
             fonte = "senado_api ou camara_api", url_fonte = "URL do recurso na API",
             tipo_ocupante = "suplente_efetivado, ou o tipo atribuido pela curadoria de ref/ocupantes_federais_fonte_oficial.csv (NA quando a cadeira nao foi identificada)",
             id_mandato_cadeira = "Cadeira do BOCEL ocupada, quando a curadoria a identifica", fonte_oficial = "Fonte oficial do ato, quando curada",
             url_oficial = "URL da fonte oficial, quando curada")))
tipo_col <- function(x) {
  v <- x[!is.na(x) & x != ""]
  if (!length(v)) return("texto")
  if (all(grepl("^-?\\d+$", v))) return("inteiro")
  if (all(grepl("^-?\\d*\\.\\d+$|^-?\\d+$", v))) return("numero")
  if (all(grepl("^\\d{4}-\\d{2}-\\d{2}", v))) return("data ISO")
  v <- iconv(v, from = "UTF-8", to = "UTF-8", sub = "")
  if (all(toupper(v) %in% c("TRUE", "FALSE"))) return("logico")
  "texto"
}
secao_aux <- function(nome) {
  f <- file.path("data_v1", paste0(nome, ".csv"))
  if (!file.exists(f)) return("")
  x <- fread(f, colClasses = "character", na.strings = "NA", encoding = "UTF-8")
  meta <- dic_aux[[nome]]
  linhas <- vapply(names(x), function(v) {
    descr <- if (!is.null(meta$d[[v]])) meta$d[[v]] else paste0("(", gsub("_", " ", v), ")")
    sprintf("| `%s` | %s | %s | %s |", v, tipo_col(x[[v]]), descr, pct(x[[v]]))
  }, character(1))
  sem_desc <- names(x)[vapply(names(x), function(v) is.null(meta$d[[v]]), logical(1))]
  if (length(sem_desc)) warning(sprintf("livro de codigos: %s sem descricao em %s", paste(sem_desc, collapse = ", "), nome))
  paste0("## ", meta$titulo, " (", format(nrow(x), big.mark = ".", decimal.mark = ","), " linhas)\n\nA chave declarada é ",
         paste0("`", meta$chave, "`", collapse = " × "), ".\n\n",
         paste(c("| variavel | tipo | descricao | preenchimento |", "|---|---|---|---|", linhas), collapse = "\n"))
}
## 21/09/2026: a v1.0 leva so o recorte federal + ocupacao (decisao de 21/09, item 1, opcao A);
## as tabelas de assembleias, camaras municipais, MUNIC, diarios oficiais e TCEs ficam para a v1.5/v2.0,
## e o livro desta versao documenta so o que de fato esta em data_v1/
RECORTE_AUX_V1 <- c("exercicio_camara", "exercicio_senado", "camara_biografia_eventos", "camara_biografia_posses",
                    "interregnos_legislativo_federal", "saida_legislativo_federal", "saida_executivos",
                    "ocupantes_legislativo_federal", "ocupacoes", "lista_suplencia", "pessoas_suplentes",
                    "suplentes_identidade", "eleicoes_suplementares", "mandatos_forma_saida_suplementar",
                    "pessoas_flags_dedup", "auditoria_homonimos", "mandatos_lista")
faltam_no_livro <- setdiff(RECORTE_AUX_V1, names(dic_aux))
if (length(faltam_no_livro)) stop("dic_aux sem entrada para: ", paste(faltam_no_livro, collapse = ", "))
aux_txt <- paste(Filter(nzchar, lapply(RECORTE_AUX_V1, secao_aux)), collapse = "\n\n")

tem_fil <- file.exists("data_v1/filiacoes.csv")
fil <- if (tem_fil) fread("data_v1/filiacoes.csv", colClasses = "character", na.strings = "NA") else NULL

tabela_dic <- function(df, dic) {
  # ordem das linhas = ordem das colunas no CSV (correcao 28/08/2026: antes seguia a ordem do dicionario)
  ordem <- c(intersect(names(df), names(dic)), setdiff(names(dic), names(df)))
  linhas <- vapply(ordem, function(v) {
    tipo <- dic[[v]][1]; descr <- dic[[v]][2]
    preench <- if (v %in% names(df)) pct(df[[v]]) else "-"
    sprintf("| `%s` | %s | %s | %s |", v, tipo, descr, preench)
  }, character(1))
  paste(c("| variavel | tipo | descricao | preenchimento |",
          "|---|---|---|---|", linhas), collapse = "\n")
}

## ---------------------------------------------------------- LIVRO DE CODIGOS
lc <- sprintf('# Livro de códigos — Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL) v1.0

O código de ausente em todos os arquivos CSV é a string `NA`, e nenhuma célula
vazia tem significado. Os códigos de ausência do TSE (`#NE`, `#NULO`, `-1`, `-3`,
`-4`) foram convertidos para `NA` na construção.

## mandatos.csv — pessoa × cargo × mandato (%s linhas)

%s

## posicoes_ano.csv — pessoa × cargo × ano (%s linhas)

%s

## pessoas.csv — cadastro de pessoas (%s linhas)

%s

%s

%s

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
',
  format(nrow(mand), big.mark = ".", decimal.mark = ","), tabela_dic(mand, dic_mand),
  format(nrow(pos), big.mark = ".", decimal.mark = ","), tabela_dic(pos, dic_pos),
  format(nrow(pess), big.mark = ".", decimal.mark = ","), tabela_dic(pess, dic_pess),
  if (tem_fil) sprintf("## filiacoes.csv — pessoa × partido × filiação (%s linhas)\n\n%s\n\nFonte: listas de filiados do TSE (Sistema FILIA), espelhadas pela Base dos Dados (`br_tse_filiacao_partidaria`, tabelas `microdados` e `microdados_antigos`), pareadas às pessoas do banco pelo título de eleitor.",
                       format(nrow(fil), big.mark = ".", decimal.mark = ","), tabela_dic(fil, dic_fil)) else "",
  aux_txt)
writeLines(lc, "docs/LIVRO_DE_CODIGOS.md")
# cobertura do livro de codigos: toda coluna dos arquivos principais tem de estar documentada
for (nm in c("mandatos", "posicoes_ano", "pessoas")) {
  cols <- names(get(if (nm == "posicoes_ano") "pos" else if (nm == "pessoas") "pess" else "mand"))
  dic <- get(paste0("dic_", if (nm == "posicoes_ano") "pos" else if (nm == "pessoas") "pess" else "mand"))
  falta <- setdiff(cols, names(dic))
  if (length(falta)) stop(sprintf("livro de codigos: colunas de %s sem entrada: %s", nm, paste(falta, collapse = ", ")))
}
cat("livro de codigos: cobertura de colunas conferida\n")

## ---------------------------------------------------------- NOTA DE COBERTURA
cob <- mand[, .N, by = .(ano_eleicao, esfera)][order(ano_eleicao, esfera)]
cob_tab <- paste(c("| ano da eleição | esfera | mandatos |", "|---|---|---|",
  cob[, sprintf("| %s | %s | %s |", ano_eleicao, esfera, format(N, big.mark = ".", decimal.mark = ","))]),
  collapse = "\n")
pct_dedup <- pess[, .(n = .N), by = chave_dedup][, pct := sub(".", ",", sprintf("%.2f%%", 100*n/sum(n)), fixed = TRUE)][]
dedup_tab <- paste(c("| regra | pessoas | proporção |", "|---|---|---|",
  pct_dedup[, sprintf("| %s | %s | %s |", chave_dedup, format(n, big.mark = ".", decimal.mark = ","), pct)]),
  collapse = "\n")

nc <- sprintf('# Nota de cobertura — BOCEL v1.0

## O que está completo

O recorte federal das eleições ordinárias de 1998 a 2022, a saber presidente e
vice-presidente, senador, deputado federal, governador e vice-governador, a
partir dos arquivos de candidaturas do TSE regenerados no layout unificado.
Assembleias Legislativas, câmaras e prefeituras municipais ficam para a v1.5
e a v2.0. A tabela abaixo dá a contagem de mandatos por eleição e esfera; a
taxa de pareamento contra as contagens de referência do TSE está em
output/verificacao/taxa_pareamento.csv do repositório de construção.

%s

## Posse, exercício e forma de saída

%s

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
EXCECOES_CONHECIDAS.csv%s

%s

## O que está parcial

- **Votação obtida.** Agregada dos arquivos de votação nominal por município e
  zona; a taxa de pareamento por esfera e ano acompanha o repositório.
- **Sucessão.** Antecessor e sucessor estão preenchidos para os cargos
  executivos (prefeito, governador, presidente) entre eleições ordinárias
  adjacentes. Para o legislativo, a sucessão por cadeira não é definível a
  partir do resultado eleitoral e fica para a v2.
- **Identificação de pessoa.** Distribuição das regras de deduplicação:

%s

## O que se sabe que falta

- **Forma de saída fora das fontes acima.** Onde nenhuma fonte cobre o
  mandato, a coluna fica em `nao_observado`; as datas de mandato seguem
  convencionais (1º de fevereiro a 31 de janeiro para senadores e deputados,
  1º de janeiro a 31 de dezembro para os demais) e o eleito sem posse
  observada figura como ocupante. No painel pessoa × cargo × ano, o mês de
  janeiro que encerra a legislatura não gera ano adicional.
%s
%s
- **Suplentes de senador** (cargos 1º e 2º suplente) não constam como
  ocupantes de posição.
- **Vices** constam como ocupantes do cargo de vice. Quando a saída do titular
  da chapa está observada (renúncia, morte, cassação, afastamento, perda do
  mandato ou substituição), o vice recebe `forma_saida = assumiu_titular`, com
  fonte `derivado_titular` e a data de saída do titular; a v1 não observa a
  posse do vice na titularidade nem a saída dele depois disso.
', cob_tab, {
  # secao de posse/exercicio/saida: gerada das colunas integradas pelo script 10
  if (!"fonte_forma_saida" %in% names(mand)) {
    "A v1.0 não observa posse, exercício nem forma de saída."
  } else {
    fs <- mand[, .N, by = .(esfera, forma_saida)][order(esfera, -N)]
    ft <- mand[forma_saida != "nao_observado", .N, by = .(esfera, fonte_forma_saida)][order(esfera, -N)]
    obs <- mand[, .(n = .N, obs = sum(forma_saida != "nao_observado"),
                    posse = sum(!is.na(data_posse) & data_posse != "NA"),
                    exerc = sum(!is.na(exercicio_confirmado) & exercicio_confirmado != "NA")), by = esfera]
    paste0(
      "As colunas `data_posse`, `data_fim_efetiva`, `forma_saida`, `fonte_forma_saida`, ",
      "`exercicio_confirmado` e `fonte_exercicio` consolidam, por ordem de prioridade, o registro ",
      "institucional (API da Câmara dos Deputados, API do Senado Federal), a biografia oficial do ",
      "deputado federal na página da Câmara e, para presidente, vice-presidente, governador e ",
      "vice-governador, a curadoria de fonte oficial (ref/eventos_presidencia_fonte_oficial.csv e ",
      "ref/eventos_governos_fonte_oficial.csv), cruzada com a Wikidata e com listas da Wikipédia quando ",
      "não há evento localizado. ",
      "Por esfera, a proporção de mandatos com forma de saída observada, com data de posse e com ",
      "exercício confirmado é a seguinte.\n\n",
      "| esfera | mandatos | forma de saída observada | com data de posse | exercício confirmado |\n|---|---|---|---|---|\n",
      paste(obs[, sprintf("| %s | %s | %s (%s) | %s (%s) | %s (%s) |", esfera,
                          trimws(format(n, big.mark = ".", decimal.mark = ",")), trimws(format(obs, big.mark = ".", decimal.mark = ",")), sub(".", ",", sprintf("%.1f%%", 100 * obs / n), fixed = TRUE),
                          trimws(format(posse, big.mark = ".", decimal.mark = ",")), sub(".", ",", sprintf("%.1f%%", 100 * posse / n), fixed = TRUE),
                          trimws(format(exerc, big.mark = ".", decimal.mark = ",")), sub(".", ",", sprintf("%.1f%%", 100 * exerc / n), fixed = TRUE))], collapse = "\n"),
      "\n\nDistribuição da forma de saída por esfera.\n\n| esfera | forma_saida | mandatos |\n|---|---|---|\n",
      paste(fs[, sprintf("| %s | %s | %s |", esfera, forma_saida, format(N, big.mark = ".", decimal.mark = ","))], collapse = "\n"),
      "\n\nFonte da forma de saída observada, por esfera.\n\n| esfera | fonte | mandatos |\n|---|---|---|\n",
      paste(ft[, sprintf("| %s | %s | %s |", esfera, fonte_forma_saida, format(N, big.mark = ".", decimal.mark = ","))], collapse = "\n"),
      if (file.exists("output/verificacao/sinais_st_reeleicao_prop_S_ano_esfera.csv")) {
        sr <- fread("output/verificacao/sinais_st_reeleicao_prop_S_ano_esfera.csv")[esfera %in% c("federal", "estadual")]
        paste0("\n\nO sinal de exercício pelo registro de candidatura à reeleição depende do campo ST_REELEICAO ",
               "do TSE, cujo preenchimento varia por eleição. A tabela dá, por eleição seguinte e esfera, a ",
               "proporção de candidatos ao mesmo cargo e unidade marcados como reeleição; onde ela fica abaixo ",
               "de um décimo, o campo está preenchido sem informação e a confirmação de exercício fica subestimada.\n\n",
               "| eleição seguinte | esfera | candidatos ao mesmo cargo | marcados S | proporção | fonte |\n|---|---|---|---|---|---|\n",
               paste(sr[, sprintf("| %s | %s | %s | %s | %s | %s |", ano_candidatura, esfera,
                                  format(n_mesmo_cargo, big.mark = ".", decimal.mark = ","), format(n_S, big.mark = ".", decimal.mark = ","),
                                  sub(".", ",", sprintf("%.1f%%", 100 * prop_S), fixed = TRUE), fonte)], collapse = "\n"))
      } else "",
      "\n\nNa Câmara, o histórico da API não traz eventos de posse e saída para a legislatura 51 ",
      "(1999–2003), e esses mandatos ficam com `forma_saida = outro` e a situação `listado_sem_historico` ",
      "em exercicio_camara.csv, parte deles resolvida pela biografia oficial (camara_biografia_eventos.csv ",
      "e camara_biografia_posses.csv); os mandatos da legislatura 57, em curso, ficam sem forma de saída. ",
      "No Senado, a saída é a do último exercício registrado. As licenças, afastamentos e suspensões que ",
      "não encerram o mandato de senador e deputado federal ficam em interregnos_legislativo_federal.csv. ",
      "A inferência por eleição suplementar marca a perda do mandato do titular ordinário na data ",
      "do pleito novo, sem distinguir cassação de anulação. Para presidente, vice-presidente, governador ",
      "e vice-governador, a forma de saída vem da curadoria de fonte oficial quando há evento localizado ",
      "e da Wikipédia quando não há, e a coluna `via` de saida_executivos.csv registra qual delas resolveu ",
      "cada mandato. As tabelas de origem (exercicio_camara, exercicio_senado, camara_biografia_eventos, ",
      "camara_biografia_posses, interregnos_legislativo_federal, saida_executivos, saida_legislativo_federal, ",
      "ocupantes_legislativo_federal, eleicoes_suplementares) acompanham o depósito com os períodos e as ",
      "causas originais.")
  }
}, {
  # divergencias de cadeiras nomeadas no texto (verifica_integracao_docs.R, 28/08/2026: a nota so remetia ao CSV
  # e nao dizia que MT 2018 tem 53 senadores)
  exc <- fread("docs/EXCECOES_CONHECIDAS.csv", colClasses = "character", na.strings = "NA")
  ex_n <- exc[!is.na(delta_esperado)]
  if (nrow(ex_n)) {
    n_cad <- function(a, c) mand[ano_eleicao == a & cargo == c, .N]
    paste0(", a saber, ", paste(ex_n[, sprintf("%s em %s (%s, %s cadeira a %s do esperado, %s no banco)", tolower(cargo), ano_eleicao, sg_uf,
                                                abs(as.integer(delta_esperado)), fifelse(as.integer(delta_esperado) < 0, "menos", "mais"),
                                                mapply(n_cad, ano_eleicao, cargo))], collapse = "; "), ".")
  } else "."
}, {
  cc <- fread("output/construcao_contagens.csv")
  fs <- mand[, .N, by = fonte_situacao][order(-N)]
  g <- function(k) trimws(format(cc[chave == k, valor], big.mark = ".", decimal.mark = ","))
  h <- function(k) trimws(format(fs[fonte_situacao == k, N], big.mark = ".", decimal.mark = ","))
  sprintf(paste0("Na construção do banco completo, antes do recorte da v1.0, %s mandatos majoritários ",
                 "de todo o país foram atribuídos ao mais votado por ausência de marcação do TSE, %s ",
                 "candidaturas repetidas na mesma posição e número foram reduzidas a uma, e %s registros ",
                 "eleitos sem nome, título nem CPF foram excluídos. No recorte da v1.0, dos %s mandatos, ",
                 "%s têm a condição de eleito lida do cadastro, %s herdada do titular da chapa (vices), ",
                 "%s atribuída pela votação e %s corrigida pela fonte oficial da casa depois da cassação ",
                 "do registro no TSE."),
          g("n_imputados_por_votos"), g("n_duplicatas_posicao_removidas"),
          g("n_eleitos_sem_identidade_excluidos"),
          trimws(format(nrow(mand), big.mark = ".", decimal.mark = ",")),
          h("cadastro"), h("imputacao_titular"), h("imputacao_votos"), h("fonte_oficial_casa"))
  }, dedup_tab, {
  # lacunas recontadas das tabelas auxiliares (correcao 28/08/2026: o texto anterior dizia que os pleitos
  # suplementares nao entravam na v1, o que contradiz a camada de forma de saida)
  fm <- function(n) format(n, big.mark = ".", decimal.mark = ",", trim = TRUE)
  b_sup <- if (file.exists("data_v1/eleicoes_suplementares.csv")) {
    su <- fread("data_v1/eleicoes_suplementares.csv", colClasses = "character", na.strings = "NA")
    sprintf(paste0("- **Eleições suplementares.** A tabela eleicoes_suplementares.csv traz %s pleitos majoritários (%s) ",
                   "e o vencedor de cada um; o vencedor do pleito suplementar não recebe mandato próprio em mandatos.csv, ",
                   "e o ocupante registrado entre a perda do mandato e a nova eleição ordinária segue sendo o eleito no ",
                   "pleito ordinário anterior, com `forma_saida = perda_do_mandato_inferida_por_eleicao_suplementar`."),
            fm(nrow(su)), paste(tolower(sort(unique(su$cargo))), collapse = ", "))
  } else "- **Eleições suplementares.** Pleitos suplementares não entram na v1."
  b_hom <- if (file.exists("data_v1/auditoria_homonimos.csv")) {
    au <- fread("data_v1/auditoria_homonimos.csv", colClasses = "character", na.strings = "NA")
    if (nrow(au) == 0) {
      "- **Precisão do pareamento por nome e nascimento.** No recorte da v1.0, toda pessoa do cadastro tem título ou CPF do TSE, e nenhuma identificação depende só da regra nome + nascimento; auditoria_homonimos.csv fica vazia nesta versão."
    } else {
      cl <- au[, .N, by = classificacao]
      n_cl <- function(k) if (k %in% cl$classificacao) cl[classificacao == k, N] else 0L
      n_so <- au[so_nome_nascimento == "TRUE", .N]; n_ponte <- au[ponte_nome_nascimento == "TRUE", .N]
      sprintf(paste0("- **Precisão do pareamento por nome e nascimento.** A identificação de %s pessoas (%s do cadastro) ",
                     "depende da regra nome + nascimento, seja porque a pessoa não tem título nem CPF no TSE (%s), seja porque ",
                     "essa regra foi a única ponte entre candidaturas com títulos distintos (%s). A auditoria em ",
                     "auditoria_homonimos.csv classifica %s como consistentes, %s como suspeitas e %s como indeterminadas, ",
                     "e pessoas.csv repete a classificação em `dedup_auditoria`."),
              fm(nrow(au)), sub(".", ",", sprintf("%.2f%%", 100 * nrow(au) / nrow(pess)), fixed = TRUE),
              fm(n_so), fm(n_ponte), n_cl("consistente"), n_cl("suspeito"), n_cl("indeterminado"))
    }
  } else ""
  paste(Filter(nzchar, c(b_sup, b_hom)), collapse = "\n")
  },
  if (tem_fil) sprintf("- **Filiação partidária longitudinal.** A tabela filiacoes.csv cobre %s das %s pessoas (%s), com %s registros de filiação vindos das listas do TSE espelhadas pela Base dos Dados. As pessoas sem título de eleitor válido no cadastro do TSE e as que só constam de listas anteriores ao espelhamento ficam sem histórico; o partido no momento de cada eleição está em mandatos.csv para todas.",
    format(uniqueN(fil$id_pessoa), big.mark = ".", decimal.mark = ","), format(nrow(pess), big.mark = ".", decimal.mark = ","),
    sub(".", ",", sprintf("%.1f%%", 100 * uniqueN(fil$id_pessoa) / nrow(pess)), fixed = TRUE), format(nrow(fil), big.mark = ".", decimal.mark = ","))
  else "- **Filiação partidária longitudinal.** A v1 traz o partido no momento de cada eleição; o histórico de filiação com datas entra na v2.")
## bloco da camada de ocupacao (decisao de 30/08/2026, pendencia 5)
if (file.exists("data_v1/ocupacoes.csv") && file.exists("data_v1/lista_suplencia.csv")) {
  ocu <- fread("data_v1/ocupacoes.csv", select = c("id_mandato","tipo_ocupante","vinculo_cadeira",
                                                "partido_difere_do_titular"))
  lsu <- fread("data_v1/lista_suplencia.csv", select = c("id_lista","ordem_suplencia",
                                                      "ordem_suplencia_tse","fonte_ordem"))
  vinc_direto <- ocu[vinculo_cadeira %in% c("chapa_senado","cadeira_unica_da_unidade",
                                            "lista_vaga_unica"), .N]
  vinc_inferido <- ocu[vinculo_cadeira == "lista_data_estrita", .N]
  vinc_executivo <- ocu[vinculo_cadeira == "fonte_da_casa", .N]
  nc <- c(nc, sprintf(
"
## Ocupação da cadeira e suplência

O banco separa a cadeira de quem a ocupou. A tabela de mandatos guarda uma linha por cadeira
ganha na eleição, e o número de cadeiras por lugar, cargo e eleição continua o mesmo depois da
entrada dos suplentes. Sobre ela, `ocupacoes.csv` registra %s ocupações no recorte federal, das
quais %s são de quem não é o titular eleito, distribuídas por %s cadeiras que passaram por mais
de uma pessoa. A distinção importa porque a cadeira muda de partido sem eleição quando o
convocado vem de outra legenda da mesma lista, o que se observa em %s ocupações.

A fila de suplência de deputado federal e de senador está em `lista_suplencia.csv`, com %s
suplentes em %s listas. A posição na fila vem da votação nominal dentro da lista, com empate
resolvido pelo candidato mais idoso, que é a regra do Código Eleitoral. O TSE publica essa ordem
já pronta só para vereador, cargo fora do recorte da v1.0, de modo que a fila de deputado federal
e de senador é derivada sem conferência externa contra uma ordem publicada. Os suplentes de
senador entram pela chapa, porque o TSE não os marca como eleitos, do mesmo modo que não marca o
vice do executivo.

O limite da camada está no vínculo entre o suplente e a cadeira que ele ocupou. Onde a chapa
nomeia o titular, onde a unidade tem cadeira única e onde a lista teve uma só vaga aberta, o
vínculo é direto, e assim se resolvem %s ocupações. Outras %s são ligadas por inferência, pela
proximidade entre a data de entrada do convocado e a data em que uma cadeira da mesma lista
vagou, exigindo que a vacância anteceda a entrada e que a distância não passe de um ano. Nas
demais %s a casa registra o exercício sem dizer qual titular saiu, de modo que a ocupação fica
com `vinculo_cadeira` igual a `casa_legislatura` e conta como ocupante da casa, sem que nenhuma
cadeira tenha sido criada para acomodá-la. Presidência e governos estaduais somam %s ocupações
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
entrada dessas pessoas.",
    fm(nrow(ocu)), fm(ocu[tipo_ocupante != "titular", .N]),
    fm(ocu[!is.na(id_mandato), .N, by = id_mandato][N > 1, .N]),
    fm(ocu[partido_difere_do_titular %in% TRUE, .N]),
    fm(nrow(lsu)), fm(uniqueN(lsu$id_lista)),
    fm(vinc_direto), fm(vinc_inferido),
    fm(ocu[vinculo_cadeira == "casa_legislatura", .N]), fm(vinc_executivo)))
}
writeLines(nc, "docs/NOTA_DE_COBERTURA.md")

## ---------------------------------------------------------- README
doi_info <- tryCatch(jsonlite::fromJSON("zenodo/deposito_info.json"),
                     error = function(e) NULL)
# 22/09/2026: o zenodo/deposit.py grava o DOI da versao em doi_reservado, e o campo conceptdoi que se lia aqui
# nunca existiu, o que deixava o README e o CITATION.cff sem DOI. A citacao da v1.0 usa o DOI da versao, que o
# Zenodo reserva no rascunho; o concept DOI so se confirma quando a primeira versao publica
doi_txt <- if (!is.null(doi_info)) doi_info$doi_reservado else NULL

# 04/09/2026: o formato passou de 8.192 bytes, limite do sprintf, quando a tabela de arquivos ganhou
# as 18 linhas do nivel de ocupacao e do interregno; o README e montado em tres pedacos.
rd_cab <- sprintf('# Banco de Ocupação de Cargos Eletivos no Brasil (BOCEL) — v1.0

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

- **Pessoas:** %s
- **Mandatos:** %s
- **Posições pessoa × cargo × ano:** %s
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
jsonlite). As bibliotecas de verificação (asserts_rigor.R, proveniencia.R)
acompanham o pacote em lib/.

## Arquivos

| arquivo | conteúdo |
|---|---|
',
  format(nrow(pess), big.mark = ".", decimal.mark = ","), format(nrow(mand), big.mark = ".", decimal.mark = ","),
  format(nrow(pos), big.mark = ".", decimal.mark = ","))
rd_tab <- '| mandatos.csv/.parquet | pessoa × cargo × mandato, com partido, votos e sucessão executiva |
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
| ids_pessoa_referencia.parquet | número de id_pessoa atribuído a cada candidatura (chave_cand), que mantém o identificador de uma pessoa quando o banco é refeito; cobre as candidaturas do banco inteiro, inclusive as de fora do recorte da v1.0, porque o mesmo número vale para as versões seguintes |
| LIVRO_DE_CODIGOS.md | nome, tipo, descrição e preenchimento de cada variável |
| LIVRO_DE_CODIGOS.csv / .xlsx | o mesmo livro em formato tabular, com nível de medida e a fonte de cada variável |
| NOTA_DE_COBERTURA.md | o que está completo, parcial e ausente |
| EXCECOES_CONHECIDAS.csv | divergências documentadas em relação às cadeiras esperadas |
| reconstruir_banco.zip | scripts, bibliotecas de verificação e tabelas de referência que reconstroem o banco a partir dos dados brutos |
'
doi_linha <- if (!is.null(doi_txt)) sprintf("> Zenodo. https://doi.org/%s", doi_txt) else "> Zenodo. DOI atribuído no momento do depósito."
rd_cauda <- sprintf('
## Como citar

> Lins, Igor Novaes. (2026). Banco de Ocupação de Cargos Eletivos no Brasil / Brazilian
> Elective Office Occupancy Database (BOCEL), 1998–2024 (v1.0) [Conjunto de dados].
%s

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

v1.0, 22 de setembro de 2026. Correções entram como versões novas no mesmo
registro do Zenodo, sob o mesmo concept DOI.

## Contato

Igor Novaes Lins — igornovaeslins@gmail.com — ORCID 0000-0003-0510-8355
', doi_linha)
rd <- paste0(rd_cab, rd_tab, rd_cauda)
writeLines(rd, "docs/README.md")

## ---------------------------------------------------------- CITATION.cff
## 21/09/2026: metadados iguais aos de zenodo/deposit.py (creators, license, version), para o
## GitHub oferecer a mesma citacao de quem le so o repositorio, sem passar pelo Zenodo
cff_doi <- if (!is.null(doi_txt)) sprintf('
identifiers:
  - type: doi
    value: "%s"
    description: "DOI da versao v1.0 no Zenodo"', doi_txt) else ""
cff <- sprintf('cff-version: 1.2.0
message: "Ao usar este banco, cite-o conforme os metadados abaixo."
type: dataset
title: "Banco de Ocupação de Cargos Eletivos no Brasil / Brazilian Elective Office Occupancy Database (BOCEL), 1998–2024"
authors:
  - family-names: "Lins"
    given-names: "Igor Novaes"
    orcid: "https://orcid.org/0000-0003-0510-8355"
    affiliation: "Centro Brasileiro de Análise e Planejamento (CEBRAP)"
version: "v1.0"
date-released: "2026-09-22"
license: "CC-BY-4.0"
repository-code: "https://github.com/igornovaeslins/bocel"%s
', cff_doi)
writeLines(cff, "CITATION.cff")

cat("docs gerados: LIVRO_DE_CODIGOS.md, NOTA_DE_COBERTURA.md, README.md, CITATION.cff\n")
