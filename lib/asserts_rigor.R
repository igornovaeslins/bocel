# =============================================================================
# asserts_rigor.R  —  Biblioteca de asserções de rigor
# =============================================================================
#
# O QUE É
# -------
# Biblioteca de asserções declarativas para pesquisa quantitativa sensível a
# erro (DiD/TWFE, microdados, joins, dados administrativos BR, georreferência).
# Cada função torna BARULHENTO um erro que normalmente é SILENCIOSO: o código
# roda, o número sai plausível, e ninguém percebe que o join duplicou linhas,
# que o NA virou zero, que a longitude entrou no lugar da latitude.
#
# COMO FUNCIONA
# -------------
# Toda asserção falha ALTO (stop) com mensagem clara. Antes de parar, anexa a
# violação ao arquivo indicado pela variável de ambiente ASSERTS_PENDENCIA,
# quando ela está definida, e a pendência só sai quando a asserção re-roda e
# passa. Só vale o artefato gravado por script que rodou.
#
# PRINCÍPIO DE PROJETO
# --------------------
# Verificação INDEPENDENTE e DETERMINÍSTICA: recontar linhas, comparar nobs,
# bater faixa, re-rodar o modelo com linhas reordenadas, sempre por controle
# externo por re-execução. Estas funções são o critério da verificação e não se
# editam para fazer uma checagem passar.
#
# COMO USAR
# ---------
#   source("lib/asserts_rigor.R")
#   df <- checa_unica(municipios, c("cod_ibge", "ano"))
#   painel <- join_seguro(painel, pib, by = "cod_ibge",
#                         cardinalidade = "many-to-one")
#   em_faixa(df$taxa_homicidio, 0, 200)            # por 100 mil hab
#   in_set(df$uf, c("AC","AL", ..., "TO"))
#   dentro_do_bbox(pontos_rj, bbox_rj)
# Veja o BLOCO DE EXEMPLO no fim do arquivo.
#
# COBERTURA (honestidade): estas funções cobrem cardinalidade, schema, unidade,
# faixa, encoding, geometria e duas invariâncias metamórficas. NÃO cobrem
# validade de identificação causal, escolha de denominador sem contrato, nem
# pertinência de citação — isso é defesa humana, fora daqui.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. Infraestrutura interna: registro de pendência + detecção de pacote
# -----------------------------------------------------------------------------

# Arquivo de pendência de dados, lido da variável de ambiente ASSERTS_PENDENCIA. Vazia, a
# falha só interrompe o script e nada se grava fora do diretório de trabalho.
.ARQUIVO_PENDENCIA_DADOS <- Sys.getenv("ASSERTS_PENDENCIA", unset = "")

#' registra_pendencia(msg)
#' Anexa uma linha de violação ao arquivo de pendência (ASSERTS_PENDENCIA).
#' Formato: ISO8601 | funcao | mensagem  — uma linha por violação, append.
#' Chamada SEMPRE antes do stop(), para que a violação fique gravada mesmo que
#' o erro suba e interrompa o script. O arquivo só se limpa depois que a
#' asserção volta a passar.
registra_pendencia <- function(msg, funcao = NA_character_) {
  if (!nzchar(.ARQUIVO_PENDENCIA_DADOS)) return(invisible(FALSE))
  if (is.na(funcao)) {
    # tenta inferir a função chamadora para dar rastro no log
    chamada <- sys.call(-1)
    funcao <- if (!is.null(chamada)) deparse(chamada[[1]])[1] else "asserts_rigor"
  }
  linha <- sprintf("%s | %s | %s",
                   format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                   funcao,
                   gsub("[\r\n]+", " ", msg))
  con <- tryCatch(file(.ARQUIVO_PENDENCIA_DADOS, open = "a", encoding = "UTF-8"),
                  error = function(e) NULL)
  if (is.null(con)) {
    warning("Não consegui gravar pendência em ", .ARQUIVO_PENDENCIA_DADOS,
            " — a violação não ficará registrada.")
    return(invisible(FALSE))
  }
  on.exit(close(con))
  writeLines(linha, con)
  invisible(TRUE)
}

# Falha padronizada: registra a pendência E levanta o erro. Centralizar aqui
# garante que NENHUMA asserção pare sem antes deixar rastro no arquivo de pendência.
.falha <- function(msg, funcao = NA_character_) {
  if (is.na(funcao)) {
    chamada <- sys.call(-1)
    funcao <- if (!is.null(chamada)) deparse(chamada[[1]])[1] else NA_character_
  }
  registra_pendencia(msg, funcao = funcao)
  stop(msg, call. = FALSE)
}

# Detecção de pacote sem carregar tudo — usamos assertr/pointblank quando há,
# com fallback em base R quando não há. O comportamento (o que é erro) é o
# mesmo nos dois caminhos; muda só o motor.
.tem <- function(pkg) requireNamespace(pkg, quietly = TRUE)


# -----------------------------------------------------------------------------
# 1. join_seguro — o join é a fábrica número um de erro silencioso
# -----------------------------------------------------------------------------
# Um left_join numa chave não-única do lado direito MULTIPLICA linhas e infla N
# sem nenhum aviso. join_seguro exige a cardinalidade declarada de antemão
# (clarify-before-commit: você diz "many-to-one" ANTES), usa relationship e
# unmatched = "error" do dplyr quando disponíveis, e compara nrow e
# n_distinct(chave) antes/depois, reportando a conta.
#
# cardinalidade ∈ {"one-to-one","one-to-many","many-to-one","many-to-many"}.
# many-to-many é permitido mas EXIGE confirmar = TRUE (é quase sempre engano).
#' @param x,y data.frames a juntar
#' @param by  vetor de chaves (nomeado se os nomes diferem, igual a dplyr)
#' @param cardinalidade relação esperada x:y
#' @param tipo "left","inner","right","full"
#' @param unmatched "error" (default) faz inner/right errarem se houver órfão
#' @param confirmar TRUE só para liberar many-to-many conscientemente
join_seguro <- function(x, y, by, cardinalidade,
                        tipo = c("left", "inner", "right", "full"),
                        unmatched = "error",
                        confirmar = FALSE) {
  tipo <- match.arg(tipo)
  card_ok <- c("one-to-one", "one-to-many", "many-to-one", "many-to-many")
  if (!cardinalidade %in% card_ok) {
    .falha(sprintf("cardinalidade '%s' inválida; use uma de: %s",
                   cardinalidade, paste(card_ok, collapse = ", ")),
           funcao = "join_seguro")
  }
  if (cardinalidade == "many-to-many" && !confirmar) {
    .falha(paste0("join_seguro recusou many-to-many sem confirmar=TRUE. ",
                  "many-to-many quase sempre é chave errada e produz produto ",
                  "cartesiano. Se for mesmo intencional, passe confirmar=TRUE."),
           funcao = "join_seguro")
  }

  if (!.tem("dplyr")) {
    .falha("join_seguro exige o pacote dplyr instalado.", funcao = "join_seguro")
  }

  # chaves do lado x e do lado y (by pode ser nomeado: c("a" = "b"))
  by_x <- if (is.null(names(by))) by else ifelse(names(by) == "", by, names(by))
  by_y <- as.character(by)

  faltam_x <- setdiff(by_x, names(x))
  faltam_y <- setdiff(by_y, names(y))
  if (length(faltam_x) || length(faltam_y)) {
    .falha(sprintf("chave ausente — em x: {%s}; em y: {%s}",
                   paste(faltam_x, collapse = ","),
                   paste(faltam_y, collapse = ",")),
           funcao = "join_seguro")
  }

  # mede ANTES
  nrow_x   <- nrow(x)
  ndist_x  <- dplyr::n_distinct(x[by_x])
  ndist_y  <- dplyr::n_distinct(y[by_y])

  # mapeia cardinalidade -> relationship do dplyr (>= 1.1.0)
  relationship <- switch(cardinalidade,
    "one-to-one"   = "one-to-one",
    "one-to-many"  = "one-to-many",
    "many-to-one"  = "many-to-one",
    "many-to-many" = "many-to-many")

  join_fun <- switch(tipo,
    left  = dplyr::left_join,
    inner = dplyr::inner_join,
    right = dplyr::right_join,
    full  = dplyr::full_join)

  # unmatched só vale para inner/right/full (left mantém os de x por definição)
  args <- list(x = x, y = y, by = by)
  # relationship existe no dplyr >= 1.1.0; tenta usar, cai pro modo manual se não.
  tem_relationship <- "relationship" %in% names(formals(dplyr::left_join))
  if (tem_relationship) {
    args$relationship <- relationship
    if (tipo %in% c("inner", "right", "full")) args$unmatched <- unmatched
  }

  out <- tryCatch(
    do.call(join_fun, args),
    error = function(e) {
      .falha(sprintf("dplyr rejeitou o join (%s, esperado %s): %s",
                     tipo, cardinalidade, conditionMessage(e)),
             funcao = "join_seguro")
    }
  )

  # mede DEPOIS e checa a invariante de cardinalidade explicitamente
  # (cinto e suspensório: vale mesmo em dplyr antigo sem relationship).
  nrow_out <- nrow(out)
  if (cardinalidade %in% c("one-to-one", "many-to-one") && nrow_out > nrow_x) {
    .falha(sprintf(paste0("join INFLOU linhas: esperado %s (%d linhas em x), ",
                          "saiu com %d. O lado direito não é único na chave ",
                          "{%s} — n_distinct(y)=%d. Houve fan-out."),
                   cardinalidade, nrow_x, nrow_out,
                   paste(by_y, collapse = ","), ndist_y),
           funcao = "join_seguro")
  }

  # relatório determinístico (vai para o stdout)
  message(sprintf(
    "[join_seguro] %s %s | x: %d linhas / %d chaves distintas; y: %d chaves distintas; saida: %d linhas",
    tipo, cardinalidade, nrow_x, ndist_x, ndist_y, nrow_out))

  out
}


# -----------------------------------------------------------------------------
# 2. checa_unica — a chave é mesmo a granularidade que você acha que é?
# -----------------------------------------------------------------------------
# Erro se houver duplicata na chave. Antes de juntar, agregar ou tratar um
# data.frame como "um por município-ano", confirme. Devolve o df (invisível)
# para encadear em pipe.
#' @param df    data.frame
#' @param chave vetor de nomes de coluna que deveriam ser únicos juntos
checa_unica <- function(df, chave) {
  faltam <- setdiff(chave, names(df))
  if (length(faltam)) {
    .falha(sprintf("checa_unica: coluna(s) ausente(s) na chave: %s",
                   paste(faltam, collapse = ", ")),
           funcao = "checa_unica")
  }
  sub <- df[, chave, drop = FALSE]
  n_total <- nrow(sub)
  n_unico <- nrow(unique(sub))
  if (n_total != n_unico) {
    dup <- n_total - n_unico
    # mostra alguns exemplos de chave duplicada para o diagnóstico ser acionável
    dups_idx <- duplicated(sub) | duplicated(sub, fromLast = TRUE)
    exemplos <- utils::head(unique(sub[dups_idx, , drop = FALSE]), 5)
    exemplos_txt <- paste(apply(exemplos, 1, function(r)
                            paste(r, collapse = "/")), collapse = "; ")
    .falha(sprintf(paste0("checa_unica: chave {%s} NÃO é única — %d linhas, ",
                          "%d combinações distintas (%d duplicadas). ",
                          "Exemplos: %s. Defina a granularidade antes de seguir."),
                   paste(chave, collapse = ","), n_total, n_unico, dup,
                   exemplos_txt),
           funcao = "checa_unica")
  }
  message(sprintf("[checa_unica] chave {%s} única em %d linhas.",
                  paste(chave, collapse = ","), n_total))
  invisible(df)
}


# -----------------------------------------------------------------------------
# 3. na_nao_e_zero — NA não é zero, e silenciar NA com 0 é decisão substantiva
# -----------------------------------------------------------------------------
# ANTI-PADRÃO que esta função combate:
#     df$x <- replace_na(df$x, 0)        # NA virou 0 sem justificativa
#     df$x <- coalesce(df$x, 0)          # idem
#     mean(df$x, na.rm = TRUE)           # média sobre denominador encolhido
# Trocar NA por 0 muda a SUBSTÂNCIA: "não houve homicídio" (0) é diferente de
# "não sei se houve" (NA). Um vira denominador, o outro não. Isso é um erro de
# pesquisa, não de tipo — por isso a função CHECA e EXIGE decisão explícita.
#
# Uso: na_nao_e_zero(df, "homicidios", decisao = "manter_na") em pontos onde um
# leitor poderia ter zerado NA na cabeça. Sem decisao válida, ela PARA e te
# obriga a declarar o que fazer com o NA (clarify-before-commit).
#' @param df  data.frame
#' @param col nome da coluna numérica sob suspeita
#' @param decisao uma de: NA (default, força você a decidir e PARA),
#'        "manter_na" (NA é informativo, ok),
#'        "zero_e_justificado" (você assume que NA==0 com razão registrada).
#' @param justificativa texto obrigatório se decisao=="zero_e_justificado"
na_nao_e_zero <- function(df, col, decisao = NA_character_, justificativa = NULL) {
  if (!col %in% names(df)) {
    .falha(sprintf("na_nao_e_zero: coluna '%s' não existe.", col),
           funcao = "na_nao_e_zero")
  }
  x <- df[[col]]
  n_na   <- sum(is.na(x))
  n_zero <- sum(x == 0, na.rm = TRUE)

  if (n_na == 0) {
    message(sprintf("[na_nao_e_zero] '%s' sem NA; nada a decidir.", col))
    return(invisible(df))
  }

  opcoes <- c("manter_na", "zero_e_justificado")
  if (is.na(decisao) || !decisao %in% opcoes) {
    .falha(sprintf(paste0("na_nao_e_zero: '%s' tem %d NA (e %d zeros reais). ",
                          "Você precisa DECIDIR explicitamente, não deixar o NA ",
                          "virar 0 por acidente. Passe decisao='manter_na' (NA é ",
                          "informativo) ou decisao='zero_e_justificado' com ",
                          "justificativa. NA não é zero."),
                   col, n_na, n_zero),
           funcao = "na_nao_e_zero")
  }

  if (decisao == "zero_e_justificado") {
    if (is.null(justificativa) || !nzchar(trimws(justificativa))) {
      .falha(sprintf(paste0("na_nao_e_zero: para zerar %d NA em '%s' você deve ",
                            "passar justificativa não-vazia (ela fica no log da ",
                            "decisão substantiva)."), n_na, col),
             funcao = "na_nao_e_zero")
    }
    message(sprintf("[na_nao_e_zero] '%s': %d NA serão tratados como 0. Motivo: %s",
                    col, n_na, justificativa))
    df[[col]][is.na(df[[col]])] <- 0
    return(invisible(df))
  }

  # decisao == "manter_na"
  message(sprintf("[na_nao_e_zero] '%s': %d NA MANTIDOS como NA (informativos).",
                  col, n_na))
  invisible(df)
}


# -----------------------------------------------------------------------------
# 4. n_modelo_estavel — por que essa especificação tem N diferente?
# -----------------------------------------------------------------------------
# Em TWFE/DiD, trocar de especificação muda N silenciosamente quando uma
# covariável tem NA (listwise deletion) ou quando muda a amostra. Comparar
# coeficientes entre modelos com N diferente é comparar maçãs e laranjas.
# Esta função compara nobs() entre todas as especificações e PARA se variar —
# a menos que você sinalize a mudança de amostra de propósito (flag_mudanca).
#' @param modelos lista nomeada de objetos com método nobs() (lm, fixest, glm…)
#' @param flag_mudanca TRUE libera N diferente (você sabe que a amostra mudou)
#' @param tolerancia diferença absoluta de N tolerada (default 0 = idêntico)
n_modelo_estavel <- function(modelos, flag_mudanca = FALSE, tolerancia = 0L) {
  if (!is.list(modelos) || length(modelos) < 2) {
    .falha("n_modelo_estavel: passe uma lista (nomeada) com 2+ modelos.",
           funcao = "n_modelo_estavel")
  }
  nomes <- names(modelos)
  if (is.null(nomes)) nomes <- paste0("modelo_", seq_along(modelos))

  ns <- vapply(modelos, function(m) {
    n <- tryCatch(stats::nobs(m), error = function(e) NA_integer_)
    as.integer(n)
  }, integer(1))

  if (anyNA(ns)) {
    quais <- nomes[is.na(ns)]
    .falha(sprintf("n_modelo_estavel: nobs() falhou para: %s. O objeto tem método nobs()?",
                   paste(quais, collapse = ", ")),
           funcao = "n_modelo_estavel")
  }

  amplitude <- max(ns) - min(ns)
  relatorio <- paste(sprintf("%s=%d", nomes, ns), collapse = "; ")

  if (amplitude > tolerancia && !flag_mudanca) {
    .falha(sprintf(paste0("n_modelo_estavel: N MUDOU entre especificações sem ",
                          "flag. [%s] (amplitude %d > tolerância %d). ",
                          "Provável NA em covariável fazendo listwise deletion ",
                          "ou amostra diferente. Se a mudança é intencional, ",
                          "passe flag_mudanca=TRUE; senão, alinhe a amostra."),
                   relatorio, amplitude, tolerancia),
           funcao = "n_modelo_estavel")
  }

  message(sprintf("[n_modelo_estavel] N %s entre modelos: %s",
                  if (amplitude == 0) "idêntico" else "dentro da tolerância",
                  relatorio))
  invisible(ns)
}


# -----------------------------------------------------------------------------
# 5. em_faixa — todo número tem uma faixa fisicamente plausível
# -----------------------------------------------------------------------------
# Pega erro de unidade/escala: taxa por 100 mil que aparece como 0,0003 (foi
# como proporção), idade 250, percentual 4500 (foi multiplicado por 100 duas
# vezes). Usa assertr::within_bounds quando disponível, base R caso contrário.
#' @param var vetor numérico
#' @param lo,hi limites plausíveis inclusivos
#' @param permitir_na TRUE ignora NA; FALSE trata NA como violação
#' @param nome rótulo da variável para a mensagem
em_faixa <- function(var, lo, hi, permitir_na = TRUE, nome = deparse(substitute(var))) {
  if (!is.numeric(var)) {
    .falha(sprintf("em_faixa: '%s' não é numérico (é %s).", nome, class(var)[1]),
           funcao = "em_faixa")
  }
  if (lo > hi) {
    .falha(sprintf("em_faixa: limites invertidos (lo=%g > hi=%g) para '%s'.",
                   lo, hi, nome), funcao = "em_faixa")
  }

  n_na <- sum(is.na(var))
  if (n_na > 0 && !permitir_na) {
    .falha(sprintf("em_faixa: '%s' tem %d NA e permitir_na=FALSE.", nome, n_na),
           funcao = "em_faixa")
  }

  fora <- !is.na(var) & (var < lo | var > hi)
  n_fora <- sum(fora)

  if (.tem("assertr")) {
    # caminho assertr: within_bounds devolve um predicado; verify levanta erro.
    ok <- tryCatch({
      d <- data.frame(.v = var)
      assertr::assert(d, assertr::within_bounds(lo, hi,
                        allow.na = permitir_na), .v)
      TRUE
    }, error = function(e) FALSE)
    if (!ok && n_fora == 0 && n_na > 0 && !permitir_na) {
      # assertr pegou NA; deixa a mensagem base abaixo cuidar de forma uniforme
    }
  }

  if (n_fora > 0) {
    exemplos <- utils::head(sort(unique(var[fora])), 5)
    .falha(sprintf(paste0("em_faixa: '%s' tem %d valor(es) FORA de [%g, %g]. ",
                          "Exemplos: %s. Cheque unidade/escala (proporção vs %%, ",
                          "por 100 mil, log)."),
                   nome, n_fora, lo, hi,
                   paste(format(exemplos), collapse = ", ")),
           funcao = "em_faixa")
  }

  message(sprintf("[em_faixa] '%s': todos os %d valores em [%g, %g]%s.",
                  nome, sum(!is.na(var)), lo, hi,
                  if (n_na > 0) sprintf(" (%d NA ignorados)", n_na) else ""))
  invisible(var)
}


# -----------------------------------------------------------------------------
# 6. in_set — a categoria existe no dicionário canônico?
# -----------------------------------------------------------------------------
# Pega encoding/fragmentação: "RJ" vs "Rio de Janeiro" vs "rj " com espaço,
# "M"/"F"/"masculino", UF inválida que entrou por OCR. Compara contra o
# conjunto canônico e PARA listando os intrusos.
#' @param cat vetor (caractere/fator) de categorias observadas
#' @param conjunto vetor com os valores canônicos permitidos
#' @param permitir_na TRUE ignora NA
#' @param nome rótulo para a mensagem
in_set <- function(cat, conjunto, permitir_na = TRUE,
                   nome = deparse(substitute(cat))) {
  cat_chr <- as.character(cat)
  conj_chr <- as.character(conjunto)

  n_na <- sum(is.na(cat_chr))
  if (n_na > 0 && !permitir_na) {
    .falha(sprintf("in_set: '%s' tem %d NA e permitir_na=FALSE.", nome, n_na),
           funcao = "in_set")
  }

  observados <- cat_chr[!is.na(cat_chr)]
  intrusos <- setdiff(unique(observados), conj_chr)

  if (length(intrusos) > 0) {
    # heurística amigável: aponta colisões prováveis por trim/case
    pista <- ""
    norm_intrusos <- tolower(trimws(intrusos))
    norm_canon    <- tolower(trimws(conj_chr))
    colisoes <- intrusos[norm_intrusos %in% norm_canon]
    if (length(colisoes) > 0) {
      pista <- sprintf(" Possível espaço/maiúscula: %s.",
                       paste(sprintf("'%s'", utils::head(colisoes, 5)),
                             collapse = ", "))
    }
    .falha(sprintf(paste0("in_set: '%s' tem %d categoria(s) FORA do conjunto ",
                          "canônico: %s.%s"),
                   nome, length(intrusos),
                   paste(sprintf("'%s'", utils::head(intrusos, 8)), collapse = ", "),
                   pista),
           funcao = "in_set")
  }

  message(sprintf("[in_set] '%s': todas as categorias dentro do conjunto (%d valores canônicos).",
                  nome, length(conj_chr)))
  invisible(cat)
}


# -----------------------------------------------------------------------------
# 7. dentro_do_bbox — lat/long trocado e CRS errado são erros silenciosos
# -----------------------------------------------------------------------------
# Ponto que cai fora do bounding-box do polígono de referência denuncia
# lat/long invertido (clássico no Brasil: long negativa virou positiva),
# unidade errada (graus vs metros, EPSG:4326 vs 31983/3857), ou ponto fora da
# área de estudo. Usa sf quando disponível; senão compara coordenadas direto.
#' @param pontos  data.frame com colunas de coordenada OU objeto sf de pontos
#' @param poligono objeto sf (polígono) OU bbox c(xmin, ymin, xmax, ymax)
#' @param coords  nomes das colunas (x/long, y/lat) quando pontos é data.frame
#' @param nome rótulo para a mensagem
dentro_do_bbox <- function(pontos, poligono,
                           coords = c("lon", "lat"),
                           nome = "pontos") {
  # extrai o bbox de referência (xmin, ymin, xmax, ymax)
  if (is.numeric(poligono) && length(poligono) == 4) {
    bb <- stats::setNames(as.numeric(poligono),
                          c("xmin", "ymin", "xmax", "ymax"))
  } else if (.tem("sf")) {
    bb <- tryCatch(sf::st_bbox(poligono),
                   error = function(e)
                     .falha(sprintf("dentro_do_bbox: não consegui obter bbox do polígono: %s",
                                    conditionMessage(e)), funcao = "dentro_do_bbox"))
  } else {
    .falha("dentro_do_bbox: 'poligono' não é bbox numérico de 4 e o pacote sf não está instalado.",
           funcao = "dentro_do_bbox")
  }

  # extrai coordenadas dos pontos
  if (.tem("sf") && inherits(pontos, "sf")) {
    xy <- sf::st_coordinates(pontos)
    x <- xy[, 1]; y <- xy[, 2]
  } else if (is.data.frame(pontos)) {
    if (!all(coords %in% names(pontos))) {
      .falha(sprintf("dentro_do_bbox: colunas de coordenada {%s} ausentes em '%s'.",
                     paste(coords, collapse = ","), nome),
             funcao = "dentro_do_bbox")
    }
    x <- pontos[[coords[1]]]; y <- pontos[[coords[2]]]
  } else {
    .falha("dentro_do_bbox: 'pontos' deve ser data.frame com coords ou objeto sf.",
           funcao = "dentro_do_bbox")
  }

  fora <- !is.na(x) & !is.na(y) &
          (x < bb["xmin"] | x > bb["xmax"] | y < bb["ymin"] | y > bb["ymax"])
  n_fora <- sum(fora)

  if (n_fora > 0) {
    # diagnóstico de troca: o ponto entra se invertermos x e y?
    cabe_invertido <- !is.na(x) & !is.na(y) &
      (y >= bb["xmin"] & y <= bb["xmax"] & x >= bb["ymin"] & x <= bb["ymax"])
    pista <- if (any(cabe_invertido[fora]))
      " Vários pontos CABEM se lat/long forem trocados — provável inversão de eixo." else
      " Cheque CRS/EPSG (graus vs metros) ou se o ponto é da área de estudo."
    i <- which(fora)[1]
    .falha(sprintf(paste0("dentro_do_bbox: %d ponto(s) de '%s' FORA do bbox ",
                          "[x %g..%g, y %g..%g]. Ex.: (x=%g, y=%g).%s"),
                   n_fora, nome, bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"],
                   x[i], y[i], pista),
           funcao = "dentro_do_bbox")
  }

  message(sprintf("[dentro_do_bbox] '%s': todos os %d pontos dentro do bbox.",
                  nome, sum(!is.na(x) & !is.na(y))))
  invisible(pontos)
}


# -----------------------------------------------------------------------------
# 8. invariante_reordenacao — teste metamórfico: a ordem das linhas é irrelevante
# -----------------------------------------------------------------------------
# Um coeficiente de regressão NÃO pode depender da ordem das linhas. Se mudar
# quando reembaralhamos as linhas, há vazamento de estado: índice usado como
# variável, lag/lead sem ordenar por grupo, seed presa à ordem, merge posicional.
# Reordena os dados, re-ajusta e confere que os coeficientes batem.
#' @param fit_fun função dados -> modelo (objeto com coef()) OU já o vetor de
#'        coeficientes nomeado. Aceita os dois: \(d) lm(y~x, d) ou
#'        \(d) coef(lm(y~x, d)).
#' @param dados   data.frame de entrada
#' @param tol     tolerância numérica (default 1e-8)
#' @param semente para o embaralhamento ser determinístico
invariante_reordenacao <- function(fit_fun, dados, tol = 1e-8, semente = 1L) {
  if (!is.function(fit_fun)) {
    .falha("invariante_reordenacao: fit_fun deve ser função dados->modelo.",
           funcao = "invariante_reordenacao")
  }
  # extrai coeficientes seja qual for o retorno: modelo (chama coef) ou já
  # o vetor numérico nomeado (usa direto, sem coef() duplo que quebra).
  .coef_de <- function(obj) {
    if (is.numeric(obj) && is.null(dim(obj))) return(obj)
    stats::coef(obj)
  }
  coef_orig <- tryCatch(.coef_de(fit_fun(dados)),
    error = function(e) .falha(sprintf("fit_fun falhou nos dados originais: %s",
                                       conditionMessage(e)),
                               funcao = "invariante_reordenacao"))

  set.seed(semente)
  ordem <- sample(nrow(dados))
  coef_perm <- tryCatch(.coef_de(fit_fun(dados[ordem, , drop = FALSE])),
    error = function(e) .falha(sprintf("fit_fun falhou nos dados reordenados: %s",
                                       conditionMessage(e)),
                               funcao = "invariante_reordenacao"))

  # alinha por nome quando possível (coeficientes podem sair em outra ordem)
  if (!is.null(names(coef_orig)) && !is.null(names(coef_perm))) {
    comuns <- intersect(names(coef_orig), names(coef_perm))
    if (length(comuns) != length(coef_orig)) {
      .falha(sprintf("invariante_reordenacao: conjunto de coeficientes mudou ao reordenar (%d vs %d).",
                     length(coef_orig), length(coef_perm)),
             funcao = "invariante_reordenacao")
    }
    coef_orig <- coef_orig[comuns]; coef_perm <- coef_perm[comuns]
  }

  difs <- abs(coef_orig - coef_perm)
  pior <- max(difs, na.rm = TRUE)
  if (pior > tol) {
    qual <- names(which.max(difs))
    .falha(sprintf(paste0("invariante_reordenacao QUEBROU: coeficiente muda com ",
                          "a ordem das linhas (maior dif=%.3g em '%s', tol=%.1g). ",
                          "Vazamento de estado: índice como variável, lag sem ",
                          "order(), seed presa à ordem ou merge posicional."),
                   pior, if (is.null(qual)) "?" else qual, tol),
           funcao = "invariante_reordenacao")
  }

  message(sprintf("[invariante_reordenacao] OK: coeficientes idênticos sob reordenação (maior dif=%.2g).",
                  pior))
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# 9. permuta_rotulo_zera — placebo: sob H0, tratamento embaralhado não tem efeito
# -----------------------------------------------------------------------------
# Permuta o rótulo de tratamento muitas vezes e confere que o efeito estimado
# fica perto de zero NA MÉDIA das permutações. Se o "efeito placebo" for grande,
# o pipeline está fabricando efeito (especificação que sempre acha algo, fuga de
# graus de liberdade, vazamento do resultado para o tratamento). É verificação
# externa por re-execução.
#' @param fit_fun função (dados) -> efeito numérico do tratamento (1 número),
#'        lendo a coluna de tratamento pelo nome `tratamento`
#' @param dados      data.frame
#' @param tratamento nome da coluna de tratamento a permutar
#' @param n_perm     número de permutações (default 200)
#' @param tol_media  |média dos efeitos permutados| tolerada. Default NULL =
#'        4 * erro-padrão da média (4 * sd/sqrt(n_perm)): sob H0 a média das
#'        permutações é ruído amostral em torno de 0 com esse desvio, então o
#'        limite acompanha a escala E o número de permutações em vez de um
#'        chute fixo que dispara falso-positivo.
#' @param semente    para reprodutibilidade
permuta_rotulo_zera <- function(fit_fun, dados, tratamento,
                                n_perm = 200L, tol_media = NULL, semente = 1L) {
  if (!is.function(fit_fun)) {
    .falha("permuta_rotulo_zera: fit_fun deve ser função dados->efeito numérico.",
           funcao = "permuta_rotulo_zera")
  }
  if (!tratamento %in% names(dados)) {
    .falha(sprintf("permuta_rotulo_zera: coluna de tratamento '%s' não existe.",
                   tratamento), funcao = "permuta_rotulo_zera")
  }

  efeito_real <- tryCatch(as.numeric(fit_fun(dados)),
    error = function(e) .falha(sprintf("fit_fun falhou nos dados reais: %s",
                                       conditionMessage(e)),
                               funcao = "permuta_rotulo_zera"))
  if (length(efeito_real) != 1 || is.na(efeito_real)) {
    .falha("permuta_rotulo_zera: fit_fun deve devolver UM número (o efeito).",
           funcao = "permuta_rotulo_zera")
  }

  set.seed(semente)
  efeitos <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    d <- dados
    d[[tratamento]] <- sample(d[[tratamento]])   # permuta sob H0
    efeitos[i] <- tryCatch(as.numeric(fit_fun(d)),
      error = function(e) NA_real_)
  }
  if (anyNA(efeitos)) {
    .falha(sprintf("permuta_rotulo_zera: fit_fun falhou em %d das %d permutações.",
                   sum(is.na(efeitos)), n_perm),
           funcao = "permuta_rotulo_zera")
  }

  media_perm <- mean(efeitos)
  sd_perm    <- stats::sd(efeitos)
  # tolerância = 4 erros-padrão da média sob H0 (sd/sqrt(n_perm)). Acompanha a
  # escala do efeito e o nº de permutações; evita o falso-positivo de um limite
  # fixo proporcional ao sd (que confunde ruído amostral normal com viés).
  if (is.null(tol_media)) tol_media <- 4 * sd_perm / sqrt(n_perm)

  # p-valor de permutação bicaudal: quão extremo é o efeito real?
  p_perm <- mean(abs(efeitos) >= abs(efeito_real))

  if (abs(media_perm) > tol_media) {
    .falha(sprintf(paste0("permuta_rotulo_zera QUEBROU: efeito placebo médio = ",
                          "%.4g (tol=%.4g, sd das permutações=%.4g). Sob H0 o ",
                          "efeito deveria ser ~0. O pipeline fabrica efeito ",
                          "(especificação enviesada, vazamento ou df-hacking)."),
                   media_perm, tol_media, sd_perm),
           funcao = "permuta_rotulo_zera")
  }

  message(sprintf(paste0("[permuta_rotulo_zera] OK: placebo médio=%.4g ~0 ",
                         "(sd=%.4g); efeito real=%.4g, p_perm=%.3f."),
                  media_perm, sd_perm, efeito_real, p_perm))
  invisible(list(efeito_real = efeito_real, media_perm = media_perm,
                 sd_perm = sd_perm, p_perm = p_perm, efeitos = efeitos))
}


# =============================================================================
# BLOCO DE EXEMPLO DE USO  (comentado — descomente para rodar à mão)
# =============================================================================
# source("lib/asserts_rigor.R")
#
# # --- dados fictícios: painel município-ano ---
# set.seed(42)
# muni <- data.frame(
#   cod_ibge = rep(c(3304557, 3550308, 5300108), each = 3),  # RJ, SP, DF
#   ano      = rep(2018:2020, times = 3),
#   uf       = rep(c("RJ", "SP", "DF"), each = 3),
#   homicidios = c(120, 110, NA, 900, 880, 870, 40, 35, 33)
# )
# pib <- data.frame(cod_ibge = c(3304557, 3550308, 5300108),
#                   pib = c(360, 700, 250))   # um por município => many-to-one
#
# # 1) a chave município-ano é única?
# checa_unica(muni, c("cod_ibge", "ano"))
#
# # 2) NA em homicídios: decida, não deixe virar 0 por acidente
# muni <- na_nao_e_zero(muni, "homicidios", decisao = "manter_na")
#
# # 3) join seguro (painel:pib é muitos-para-um); inflar linhas => stop
# painel <- join_seguro(muni, pib, by = "cod_ibge", cardinalidade = "many-to-one")
#
# # 4) faixa e conjunto canônico
# em_faixa(painel$homicidios, 0, 5000, nome = "homicidios")
# in_set(painel$uf, c("AC","AL","AP","AM","BA","CE","DF","ES","GO","MA","MT",
#                     "MS","MG","PA","PB","PR","PE","PI","RJ","RN","RS","RO",
#                     "RR","SC","SP","SE","TO"), nome = "uf")
#
# # 5) geometria: ponto no RJ não pode cair fora do bbox do estado
# bbox_rj <- c(xmin = -44.9, ymin = -23.4, xmax = -40.9, ymax = -20.7)
# pts <- data.frame(lon = c(-43.2, -43.4), lat = c(-22.9, -22.8))
# dentro_do_bbox(pts, bbox_rj, coords = c("lon", "lat"), nome = "ocorrencias")
#
# # 6) testes metamórficos sobre um ajuste
# dados <- data.frame(y = rnorm(200), x = rnorm(200), trat = rbinom(200, 1, .5))
# fit_lm   <- function(d) lm(y ~ x + trat, data = d)
# efeito_t <- function(d) coef(lm(y ~ x + trat, data = d))[["trat"]]
#
# invariante_reordenacao(fit_lm, dados)            # ordem não pode mudar coef
# permuta_rotulo_zera(efeito_t, dados, "trat")     # placebo deve dar ~0
#
# # 7) estabilidade de N entre especificações (com fixest/lm/glm)
# m1 <- lm(y ~ x, dados); m2 <- lm(y ~ x + trat, dados)
# n_modelo_estavel(list(base = m1, completo = m2))
# =============================================================================
