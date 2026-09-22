# =============================================================================
# proveniencia.R — registro de proveniência e de números verificados.
# Generaliza o padrão registrar_fonte()/baixar() do projeto Belém (00_config.R).
# Critério da verificação, não se edita para fazer uma checagem passar. Base R, sem dependências.
# Princípio: nada de número narrado — todo valor que vai para texto nasce de um
# script que rodou e foi registrado aqui (chave, data, checksum do script).
# =============================================================================

# checksum de arquivo (md5 via base R; honesto sobre ser md5, não sha256)
.prov_checksum <- function(path) {
  if (!is.null(path) && !is.na(path) && file.exists(path)) unname(tools::md5sum(path)) else ""
}

# registra a coleta de uma fonte de dados (espelha registrar_fonte do Belém)
registrar_fonte <- function(arquivo, fonte, url = NA,
                            log = "output/proveniencia_fontes.csv") {
  dir.create(dirname(log), recursive = TRUE, showWarnings = FALSE)
  novo <- data.frame(
    data_iso = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    arquivo  = arquivo,
    fonte    = fonte,
    url      = ifelse(is.na(url), "", url),
    checksum_md5 = .prov_checksum(arquivo),
    bytes    = if (file.exists(arquivo)) file.info(arquivo)$size else NA_integer_,
    stringsAsFactors = FALSE
  )
  write.table(novo, log, sep = ",", append = file.exists(log),
              col.names = !file.exists(log), row.names = FALSE, qmethod = "double")
  invisible(novo)
}

# registra UM número verificado: a unidade de "registrar, nunca narrar"
registrar_numero <- function(chave, valor, ep = NA, script = NA,
                             out = "output/numeros_assinatura.txt") {
  # 12/09/2026: valor de comprimento diferente de 1 gravava linha vazia ou quebrada, e a leitura do registro por
  # fread parava nela, fazendo verificador conferir contra valor antigo
  if (length(valor) != 1L) stop(sprintf("registrar_numero: a chave '%s' recebeu %d valores", chave, length(valor)))
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  linha <- sprintf("%s | %s | %s | %s | %s | %s",
                   chave, valor, ifelse(is.na(ep), "", ep),
                   format(Sys.Date()), .prov_checksum(script), out)
  cat(linha, "\n", file = out, append = TRUE, sep = "")
  invisible(linha)
}

# grava o relatório de evidência da verificação
gravar_relatorio_verificacao <- function(alvo, script, passou = character(),
                                         falhou = character(),
                                         fora_de_cobertura = character(),
                                         dir = "output/verificacao") {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  ts <- format(Sys.time(), "%Y%m%dT%H%M%S")
  f <- file.path(dir, sprintf("relatorio_verificacao_%s.json", ts))
  esc <- function(v) paste0('"', gsub('"', '\\\\"', v), '"')
  arr <- function(x) if (length(x)) paste0("[", paste(esc(x), collapse=","), "]") else "[]"
  veredito <- if (length(falhou)) "reprovado" else "verificado"
  cat(sprintf('{\n  "alvo": %s,\n  "data_iso": %s,\n  "checksum_script": %s,\n  "passou": %s,\n  "falhou": %s,\n  "fora_de_cobertura": %s,\n  "veredito": %s\n}\n',
              esc(alvo), esc(format(Sys.time(),"%Y-%m-%dT%H:%M:%S")),
              esc(.prov_checksum(script)), arr(passou), arr(falhou),
              arr(fora_de_cobertura), esc(veredito)),
      file = f)
  invisible(f)
}
