# gera_renv_lock.R — renv.lock do BOCEL a partir dos pacotes que os scripts usam (19/09/2026)
#
# A reconstrucao precisa fixar as versoes dos pacotes (decisao de 19/09, tudo em R e reprodutivel). O renv nao
# esta instalado nesta maquina, e inicializa-lo ativaria uma biblioteca de projeto vazia que quebraria a cadeia. Este
# script escreve o renv.lock no formato do renv sem depender dele: le library(), require(), requireNamespace() e pkg::
# nos .R de R/ e lib/, fecha as dependencias (Depends, Imports, LinkingTo) pela biblioteca instalada e grava versao e
# origem de cada pacote. Quem reconstroi roda renv::restore() na raiz e recebe as mesmas versoes.
#
# Saidas: renv.lock, output/verificacao/renv_pacotes.csv (pacote, versao, origem, se e usado direto ou so dependencia,
#         scripts que o chamam), output/verificacao/renv_pacotes_ausentes.csv (citados e nao instalados), chaves renv_*.
# Execucao: cd ~/bocel && Rscript --vanilla R/gera_renv_lock.R
suppressPackageStartupMessages({ library(data.table); library(jsonlite) })
root <- Sys.getenv("BOCEL_ROOT", unset = path.expand("~/bocel")); setwd(root)
source(file.path(root, "lib", "proveniencia.R"))
script <- "R/gera_renv_lock.R"
reg <- function(k, v) registrar_numero(k, v, script = script)

## ---------------------------------------------------------------- pacotes citados nos scripts
arqs <- c(list.files("R", pattern = "[.]R$", recursive = TRUE, full.names = TRUE),
          list.files("lib", pattern = "[.]R$", full.names = TRUE))
arqs <- arqs[!grepl("^R/arquivo/", arqs)]
# library() e require() aceitam nome sem aspas; requireNamespace() e loadNamespace() so contam com nome entre aspas, porque
# com variavel (requireNamespace(pkg)) o nome nao esta no texto. O prefixo pkg:: nao pode vir colado a hifen, que e o
# eixo do XPath ("following-sibling::span").
PAT <- c("(?:library|require)\\(\\s*[\"']?([A-Za-z][A-Za-z0-9.]*)[\"']?\\s*[,)]",
         "(?:requireNamespace|loadNamespace)\\(\\s*[\"']([A-Za-z][A-Za-z0-9.]*)[\"']",
         "(?<![-A-Za-z0-9._])([A-Za-z][A-Za-z0-9.]*):::?[A-Za-z._]")
# Pacote usado so quando instalado, com caminho em R base na falta dele (lib/asserts_rigor.R, l. 93 e 367). Fica fora do
# lock e do conteudo de ausentes.
OPCIONAL <- c("assertr", "pointblank")
cita <- rbindlist(lapply(arqs, function(f) {
  l <- readLines(f, warn = FALSE, encoding = "UTF-8")
  l <- sub("#.*$", "", l)  # comentario fora; pode cortar '#' dentro de string, o que so perde citacao, nunca inventa
  p <- unlist(lapply(PAT, function(pt) {
    m <- regmatches(l, gregexpr(pt, l, perl = TRUE))
    unlist(lapply(m, function(x) sub(pt, "\\1", x, perl = TRUE)))
  }))
  if (length(p)) data.table(pacote = unique(p), arquivo = f) else NULL
}))
BASE <- rownames(installed.packages(priority = "base"))
cita <- cita[!pacote %chin% c(BASE, "R", OPCIONAL)]
instalados <- installed.packages(fields = c("Repository", "RemoteType", "RemoteUsername", "RemoteRepo", "RemoteRef", "RemoteSha"))
ausentes <- cita[!pacote %chin% rownames(instalados)]
fwrite(ausentes[order(pacote, arquivo)], "output/verificacao/renv_pacotes_ausentes.csv")
diretos <- sort(unique(cita[pacote %chin% rownames(instalados), pacote]))

## ---------------------------------------------------------------- fecho de dependencias
dep <- tools::package_dependencies(diretos, db = instalados, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)
todos <- sort(setdiff(unique(c(diretos, unlist(dep))), c(BASE, "R")))
faltam_dep <- setdiff(todos, rownames(instalados))
stopifnot("dependencia de pacote usado nao instalada" = length(faltam_dep) == 0)

campo <- function(p, f) { v <- instalados[p, f]; if (is.na(v) || !nzchar(v)) NA_character_ else unname(v) }
req <- function(p) {
  d <- tools::package_dependencies(p, db = instalados, which = c("Depends", "Imports", "LinkingTo"))[[1]]
  sort(setdiff(d, c(BASE, "R")))
}
entrada <- function(p) {
  rt <- campo(p, "RemoteType"); repo <- campo(p, "Repository")
  e <- list(Package = p, Version = unname(instalados[p, "Version"]))
  if (!is.na(rt) && rt == "github") {
    e <- c(e, list(Source = "GitHub", RemoteType = "github", RemoteUsername = campo(p, "RemoteUsername"),
                   RemoteRepo = campo(p, "RemoteRepo"), RemoteRef = campo(p, "RemoteRef"), RemoteSha = campo(p, "RemoteSha")))
  } else {
    e <- c(e, list(Source = "Repository", Repository = if (is.na(repo)) "CRAN" else repo))
  }
  r <- req(p); if (length(r)) e$Requirements <- r
  e
}
pk <- setNames(lapply(todos, entrada), todos)
lock <- list(R = list(Version = paste(R.version$major, R.version$minor, sep = "."),
                      Repositories = list(list(Name = "CRAN", URL = "https://cloud.r-project.org"))),
             Packages = pk)
writeLines(toJSON(lock, auto_unbox = TRUE, pretty = TRUE, null = "null"), "renv.lock")

## ---------------------------------------------------------------- relatorio e conferencia
tab <- data.table(pacote = todos, versao = vapply(todos, function(p) unname(instalados[p, "Version"]), ""),
                  origem = vapply(pk, `[[`, "", "Source"), uso = fifelse(todos %chin% diretos, "direto", "dependencia"))
tab[, scripts := vapply(pacote, function(p) paste(sort(unique(cita[pacote == p, arquivo])), collapse = " "), "")]
fwrite(tab, "output/verificacao/renv_pacotes.csv")
volta <- fromJSON("renv.lock", simplifyVector = FALSE)
stopifnot(identical(sort(names(volta$Packages)), todos),
          all(vapply(volta$Packages, function(x) identical(x$Version, unname(instalados[x$Package, "Version"])), TRUE)),
          identical(volta$R$Version, paste(R.version$major, R.version$minor, sep = ".")))
reg("renv_pacotes_diretos", length(diretos))
reg("renv_pacotes_total_com_dependencias", length(todos))
reg("renv_pacotes_fora_do_cran", tab[origem != "Repository", .N])
reg("renv_pacotes_citados_nao_instalados", uniqueN(ausentes$pacote))
print(tab[, .N, by = .(uso, origem)])
if (nrow(ausentes)) print(ausentes[, .(scripts = .N), by = pacote])
cat("renv.lock com", length(todos), "pacotes, R", volta$R$Version, "\n")
