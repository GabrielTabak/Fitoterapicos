args_script <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("^--file=", "", args_script[grepl("^--file=", args_script)])
if (length(arquivo_script)) {
  setwd(dirname(normalizePath(gsub("~+~", " ", arquivo_script, fixed = TRUE))))
}

# install.packages(c("florabr", "stringi"))

library(florabr)
library(stringi)
library(dplyr)

# 1) Carregar Flora do Brasil

bf <- load_florabr(data_dir = "florabr_data", type = "complete")

# 2) Funções auxiliares

normalizar <- function(x) {
  x <- tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- gsub("\\s+", " ", trimws(x))
  x
}

quebrar_termos <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x) || trimws(x) == "") {
    return(character(0))
  }

  # Mantém "/" como separador principal, mas também aceita ";" e "|"
  termos <- unlist(strsplit(as.character(x), "\\s*/\\s*|\\s*;\\s*|\\s*\\|\\s*", perl = TRUE))
  termos <- unique(trimws(termos))
  termos <- termos[termos != ""]
  termos
}

deduplicar_normalizado <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & x != ""]

  if (length(x) == 0) {
    return(character(0))
  }

  x[!duplicated(normalizar(x))]
}

colar_unico <- function(x) {
  x <- deduplicar_normalizado(x)
  if (length(x) == 0) {
    return("")
  }
  paste(x, collapse = "; ")
}

extrair_populares <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(character(0))
  }

  nomes <- unlist(strsplit(paste(x, collapse = ","), ","))
  nomes <- trimws(nomes)
  nomes <- nomes[!is.na(nomes) & nomes != ""]
  deduplicar_normalizado(nomes)
}

pegar_sinonimos <- function(aceitos) {
  aceitos <- deduplicar_normalizado(aceitos)

  if (length(aceitos) == 0) {
    return(character(0))
  }

  sins <- tryCatch(
    get_synonym(data = bf, species = aceitos),
    error = function(e) NULL
  )

  if (is.null(sins) || nrow(sins) == 0 || !"synonym" %in% names(sins)) {
    return(character(0))
  }

  deduplicar_normalizado(sins$synonym)
}

pegar_populares_por_especie <- function(aceitos) {
  aceitos <- deduplicar_normalizado(aceitos)

  if (length(aceitos) == 0) {
    return(character(0))
  }

  idx <- rep(FALSE, nrow(bf))

  if ("species" %in% names(bf)) {
    idx <- idx | bf$species %in% aceitos
  }

  if ("acceptedName" %in% names(bf)) {
    idx <- idx | bf$acceptedName %in% aceitos
  }

  if (!"vernacularName" %in% names(bf)) {
    return(character(0))
  }

  extrair_populares(bf$vernacularName[idx])
}

# 3) Busca por nome popular

buscar_popular_detalhado <- function(nome_pop_original) {
  termos_originais <- quebrar_termos(nome_pop_original)
  termos_busca <- unique(normalizar(termos_originais))
  termos_busca <- termos_busca[termos_busca != ""]

  todos_hits <- list()

  for (v in termos_busca) {
    h <- tryCatch(
      select_by_vernacular(data = bf, names = v, exact = TRUE),
      error = function(e) NULL
    )

    if (!is.null(h) && nrow(h) > 0) {
      todos_hits[[v]] <- h
    }
  }

  if (length(todos_hits) == 0) {
    return(list(
      termos_buscados = termos_busca,
      termos_com_match = character(0),
      aceitos = character(0),
      sinonimos = character(0),
      populares = character(0)
    ))
  }

  hits <- do.call(rbind, todos_hits)

  if ("species" %in% names(hits)) {
    hits <- hits[!duplicated(hits$species), ]
  }

  aceitos <- ifelse(
    is.na(hits$acceptedName) | hits$acceptedName == "",
    hits$species,
    hits$acceptedName
  )

  aceitos <- deduplicar_normalizado(aceitos)
  sinonimos <- pegar_sinonimos(aceitos)

  populares <- character(0)

  if ("vernacularName" %in% names(hits)) {
    populares <- extrair_populares(hits$vernacularName)
  }

  list(
    termos_buscados = termos_busca,
    termos_com_match = names(todos_hits),
    aceitos = aceitos,
    sinonimos = sinonimos,
    populares = populares
  )
}

# 4) Busca por nome científico

buscar_cientifico_detalhado <- function(nome_cient_original) {
  termos_originais <- quebrar_termos(nome_cient_original)

  if (length(termos_originais) == 0) {
    return(list(
      termos_buscados = character(0),
      termos_com_match = character(0),
      aceitos = character(0),
      sinonimos = character(0),
      populares = character(0),
      status = character(0)
    ))
  }

  binomios <- tryCatch(
    get_binomial(
      termos_originais,
      include_subspecies = TRUE,
      include_variety = TRUE
    ),
    error = function(e) character(0)
  )

  binomios <- unique(trimws(binomios))
  binomios <- binomios[!is.na(binomios) & binomios != ""]

  # Caso get_binomial não consiga extrair algo, usa o termo original
  if (length(binomios) == 0) {
    binomios <- termos_originais
  }

  todas_checagens <- list()

  for (b in binomios) {
    ch <- tryCatch(
      check_names(data = bf, species = b, max_distance = 0.1),
      error = function(e) NULL
    )

    if (!is.null(ch) && nrow(ch) > 0) {
      if ("acceptedName" %in% names(ch)) {
        tem_match <- any(!is.na(ch$acceptedName) & ch$acceptedName != "")
      } else {
        tem_match <- TRUE
      }

      if (tem_match) {
        todas_checagens[[b]] <- ch
      }
    }
  }

  if (length(todas_checagens) == 0) {
    return(list(
      termos_buscados = binomios,
      termos_com_match = character(0),
      aceitos = character(0),
      sinonimos = character(0),
      populares = character(0),
      status = "sem_match"
    ))
  }

  checagem <- do.call(rbind, todas_checagens)

  aceitos <- character(0)

  if ("acceptedName" %in% names(checagem)) {
    aceitos <- c(aceitos, checagem$acceptedName)
  }

  if ("species" %in% names(checagem)) {
    aceitos <- c(aceitos, checagem$species)
  }

  aceitos <- deduplicar_normalizado(aceitos)
  sinonimos <- pegar_sinonimos(aceitos)
  populares <- pegar_populares_por_especie(aceitos)

  status <- character(0)

  if ("Spelling" %in% names(checagem)) {
    status <- unique(checagem$Spelling)
  }

  list(
    termos_buscados = binomios,
    termos_com_match = names(todas_checagens),
    aceitos = aceitos,
    sinonimos = sinonimos,
    populares = populares,
    status = status
  )
}

# 5) Função final: junta nome popular + nome científico

buscar_linha_consolidada <- function(nome_popular, nome_cientifico) {
  termos_originais <- quebrar_termos(nome_cientifico)
  termos_busca_cient <- unique(normalizar(termos_originais))
  termos_busca_cient <- termos_busca_cient[termos_busca_cient != ""]

  termos_originais <- quebrar_termos(nome_popular)
  termos_busca_pop <- unique(normalizar(termos_originais))
  termos_busca_pop <- termos_busca_pop[termos_busca_pop != ""]

  res_pop <- buscar_popular_detalhado(nome_popular)
  res_cient <- buscar_cientifico_detalhado(nome_cientifico)

  variacoes_populares <- deduplicar_normalizado(c(
    res_pop$populares,
    res_cient$populares,
    termos_busca_pop
  ))

  variacoes_cientificas <- deduplicar_normalizado(c(
    res_pop$aceitos,
    res_pop$sinonimos,
    res_cient$aceitos,
    res_cient$sinonimos,
    termos_busca_cient
  ))

  variacoes_populares <- setdiff(variacoes_populares, "NA")
  variacoes_cientificas <- setdiff(variacoes_cientificas, "NA")

  n_pop_encontrados <- length(res_pop$termos_com_match)
  n_cient_encontrados <- length(res_cient$termos_com_match)

  data.frame(
    Nome_popular_buscado = ifelse(is.na(nome_popular), "", nome_popular),
    Nome_cientifico_buscado = ifelse(is.na(nome_cientifico), "", nome_cientifico),
    Variacoes_nome_popular_encontradas = colar_unico(variacoes_populares),
    Variacoes_nome_cientifico_encontradas = colar_unico(variacoes_cientificas),
    Termos_populares_com_match = colar_unico(res_pop$termos_com_match),
    Termos_cientificos_com_match = colar_unico(res_cient$termos_com_match),
    n_termos_populares_encontrados = n_pop_encontrados,
    n_termos_cientificos_encontrados = n_cient_encontrados,
    n_termos_encontrados_total = n_pop_encontrados + n_cient_encontrados,
    n_variacoes_populares_encontradas = length(variacoes_populares),
    n_variacoes_cientificas_encontradas = length(variacoes_cientificas),
    status_busca_cientifica = colar_unico(res_cient$status),
    stringsAsFactors = FALSE
  )
}

buscar_linha_consolidada_2 <- function(nome_popular, nome_cientifico) {
  termos_originais <- quebrar_termos(nome_cientifico)
  termos_busca_cient <- unique(normalizar(termos_originais))
  termos_busca_cient <- termos_busca_cient[termos_busca_cient != ""]

  termos_originais <- quebrar_termos(nome_popular)
  termos_busca_pop <- unique(normalizar(termos_originais))
  termos_busca_pop <- termos_busca_pop[termos_busca_pop != ""]

  res_pop <- buscar_popular_detalhado(nome_popular)
  res_cient <- buscar_cientifico_detalhado(nome_cientifico)

  res_pop$sinonimos <- deduplicar_normalizado(sub(
    "\\s+\\S+\\..*$",
    "",
    res_pop$sinonimos
  ))

  res_pop$aceitos <- deduplicar_normalizado(sub(
    "\\s+\\S+\\..*$",
    "",
    res_pop$aceitos
  ))

  res_cient$sinonimos <- deduplicar_normalizado(sub(
    "\\s+\\S+\\..*$",
    "",
    res_cient$sinonimos
  ))

  res_cient$aceitos <- deduplicar_normalizado(sub(
    "\\s+\\S+\\..*$",
    "",
    res_cient$aceitos
  ))

  if (length(res_pop$populares) > 11 | length(res_pop$aceitos) > 11 |
    length(res_pop$sinonimos) > 11) {
    if (length(res_cient$sinonimos) > 20) {
      variacoes_populares <- deduplicar_normalizado(c(
        res_cient$populares,
        termos_busca_pop
      ))

      variacoes_cientificas <- deduplicar_normalizado(c(
        res_cient$aceitos,
        termos_busca_cient
      ))
    } else {
      variacoes_populares <- deduplicar_normalizado(c(
        res_cient$populares,
        termos_busca_pop
      ))

      variacoes_cientificas <- deduplicar_normalizado(c(
        res_cient$aceitos,
        res_cient$sinonimos,
        termos_busca_cient
      ))
    }
  } else if (length(res_cient$sinonimos) > 20) {
    variacoes_populares <- deduplicar_normalizado(c(
      res_pop$populares,
      res_cient$populares,
      termos_busca_pop
    ))

    variacoes_cientificas <- deduplicar_normalizado(c(
      res_pop$aceitos,
      res_pop$sinonimos,
      res_cient$aceitos,
      termos_busca_cient
    ))
  } else {
    variacoes_populares <- deduplicar_normalizado(c(
      res_pop$populares,
      res_cient$populares,
      termos_busca_pop
    ))

    variacoes_cientificas <- deduplicar_normalizado(c(
      res_pop$aceitos,
      res_pop$sinonimos,
      res_cient$aceitos,
      res_cient$sinonimos,
      termos_busca_cient
    ))
  }

  #  if(length(variacoes_cientificas)>40){

  #  }

  variacoes_populares <- setdiff(variacoes_populares, "NA")
  variacoes_cientificas <- setdiff(variacoes_cientificas, "NA")

  n_pop_encontrados <- length(res_pop$termos_com_match)
  n_cient_encontrados <- length(res_cient$termos_com_match)

  data.frame(
    Nome_popular_buscado = ifelse(is.na(nome_popular), "", nome_popular),
    Nome_cientifico_buscado = ifelse(is.na(nome_cientifico), "", nome_cientifico),
    Variacoes_nome_popular_encontradas = colar_unico(variacoes_populares),
    Variacoes_nome_cientifico_encontradas = colar_unico(variacoes_cientificas),
    Termos_populares_com_match = colar_unico(res_pop$termos_com_match),
    Termos_cientificos_com_match = colar_unico(res_cient$termos_com_match),
    n_termos_populares_encontrados = n_pop_encontrados,
    n_termos_cientificos_encontrados = n_cient_encontrados,
    n_termos_encontrados_total = n_pop_encontrados + n_cient_encontrados,
    n_variacoes_populares_encontradas = length(variacoes_populares),
    n_variacoes_cientificas_encontradas = length(variacoes_cientificas),
    status_busca_cientifica = colar_unico(res_cient$status),
    stringsAsFactors = FALSE
  )
}

# 6) Rodar na base completa e salvar um único CSV

plantas <- read.csv(
  "dados/plantas_fontes.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE,
  fileEncoding = "UTF-8",
  sep = ";"
)

# Identifica automaticamente as colunas de nome popular e científico
col_popular <- grep("popular", names(plantas), ignore.case = TRUE, value = TRUE)[1]
col_cientifico <- grep("cient", names(plantas), ignore.case = TRUE, value = TRUE)[1]

lista_resultados <- lapply(seq_len(nrow(plantas)), function(i) {
  buscar_linha_consolidada(
    nome_popular = plantas[[col_popular]][i],
    nome_cientifico = plantas[[col_cientifico]][i]
  )
})

resultado_buscas <- do.call(rbind, lista_resultados)

# Checagem de segurança
cat("Linhas em plantas:", nrow(plantas), "\n")
cat("Linhas em resultado_buscas:", nrow(resultado_buscas), "\n")

# Caso queira testar alguns em particular
# buscar_cientifico_detalhado("Citrus limon")
# buscar_popular_detalhado("Jalapa")

# Salvar aqueles com variacoes exageradas
resultado_buscas %>%
  filter(n_variacoes_populares_encontradas >= 10 |
    n_variacoes_cientificas_encontradas >= 10) %>%
  select(
    Nome_popular_buscado, Nome_cientifico_buscado,
    n_variacoes_populares_encontradas,
    n_variacoes_cientificas_encontradas
  ) %>% # View()
  write.csv2("dados/variacoes_para_revisao.csv")

### Resultado final - pos termos com muitas buscas populares
# Regra:
# Se nome popular tiver match com mais de 10 nomes, então
# descartamos a busca por nome popular, e mantem apenas
# o nome popular original (que foi buscado) + variações dos matchs com cientifico
# Se nome cientifico sinonimos tiver mais que 20 matchs,
# usamos apenas os aceitos, e excluimos os sinonimos.

lista_resultados_2 <- lapply(seq_len(nrow(plantas)), function(i) {
  buscar_linha_consolidada_2(
    nome_popular = plantas[[col_popular]][i],
    nome_cientifico = plantas[[col_cientifico]][i]
  )
})

resultado_buscas_2 <- do.call(rbind, lista_resultados_2)

# Comparando quantos eram inicialmente, e quantos
# restam pós filtros
sum(resultado_buscas$n_variacoes_populares_encontradas) +
  sum(resultado_buscas$n_variacoes_cientificas_encontradas)

sum(resultado_buscas_2$n_variacoes_populares_encontradas) +
  sum(resultado_buscas_2$n_variacoes_cientificas_encontradas)

resultado_final <- cbind(resultado_buscas_2[, 1:2], plantas[, 3:8], resultado_buscas_2[, c(3, 4, 10, 11)])

write.csv2(
  resultado_final,
  "dados/plantas_busca_geradas.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Parte de remover pontuação que ficou devendo antes

library(stringr)
df <- read.csv2("dados/plantas_busca_geradas.csv")

for (i in 1:nrow(df)) {
  df$Variacoes_nome_popular_encontradas[i] <- str_remove_all(df$Variacoes_nome_popular_encontradas[i], "'")
  df$Variacoes_nome_popular_encontradas[i] <- str_remove_all(df$Variacoes_nome_popular_encontradas[i], "~")
  df$Variacoes_nome_popular_encontradas[i] <- str_remove_all(df$Variacoes_nome_popular_encontradas[i], fixed("^"))
}

## Por alguma razão o ^ não some mesmo assim, então tive que tirar na mão

write.csv2(
  df,
  "dados/plantas_busca_geradas.csv",
  row.names = FALSE,
  na = "",
  fileEncoding = "UTF-8"
)

# Tambem removemos: (planta), china. Na mão. Por serem termos
# que não nos interessam.

mean(resultado_buscas_2$n_variacoes_populares_encontradas + resultado_buscas_2$n_variacoes_cientificas_encontradas)
median(resultado_buscas_2$n_variacoes_populares_encontradas + resultado_buscas_2$n_variacoes_cientificas_encontradas)
max(resultado_buscas_2$n_variacoes_populares_encontradas + resultado_buscas_2$n_variacoes_cientificas_encontradas)
