args_script <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("^--file=", "", args_script[grepl("^--file=", args_script)])
if (length(arquivo_script)) {
  setwd(dirname(normalizePath(gsub("~+~", " ", arquivo_script, fixed = TRUE))))
}

# Gera dois infográficos A4 a partir da base de medicamentos ativos.

suppressPackageStartupMessages({
  library(dplyr)
  library(grid)
  library(readr)
  library(ragg)
  library(scales)
  library(stringr)
  library(svglite)
})

arquivo_base <- file.path(
  "ResultadosFinal",
  "Bases",
  "base_produto_planta.csv"
)

dir_saida <- file.path(
  "ResultadosFinal",
  "Infograficos"
)

dir.create(dir_saida, recursive = TRUE, showWarnings = FALSE)

largura_a4 <- 8.2677165
altura_a4 <- 11.692913

familia_texto <- "sans"
familia_display <- "sans"

paleta <- list(
  verde = "#075B4B",
  verde_escuro = "#063F36",
  verde_claro = "#DCEAD8",
  lima = "#B9D43A",
  laranja = "#F4A340",
  coral = "#E75B4B",
  azul = "#1D7285",
  creme = "#F6F0E3",
  creme_claro = "#FCFAF4",
  tinta = "#18312B",
  cinza = "#61706C",
  linha = "#D9D8CE",
  branco = "#FFFFFF"
)

nome_curto_planta <- function(x) {
  str_squish(str_split_fixed(as.character(x), "/", 2)[, 1])
}

base <- read_csv2(
  arquivo_base,
  show_col_types = FALSE,
  progress = FALSE
)

base_ativos <- base %>%
  filter(situacao == "Ativo")

dados_medicamentos <- base_ativos %>%
  distinct(id_planta, nome_planta, chave_produto) %>%
  count(id_planta, nome_planta, name = "valor") %>%
  mutate(
    planta = nome_curto_planta(nome_planta),
    metrica = "medicamentos ativos"
  ) %>%
  arrange(desc(valor), planta) %>%
  mutate(
    posicao = min_rank(desc(valor)),
    ordem = row_number(),
    percentual = valor / sum(valor)
  )

dados_empresas <- base_ativos %>%
  filter(
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  distinct(
    id_planta,
    nome_planta,
    empresa_detentora_da_regularizacao
  ) %>%
  count(id_planta, nome_planta, name = "valor") %>%
  mutate(
    planta = nome_curto_planta(nome_planta),
    metrica = "empresas"
  ) %>%
  arrange(desc(valor), planta) %>%
  mutate(
    posicao = min_rank(desc(valor)),
    ordem = row_number(),
    percentual = valor / sum(valor)
  )

marca_empresa <- function(x) {
  case_when(
    str_detect(x, "^AIRELA ") ~ "AIRELA",
    str_detect(x, "^EMS ") ~ "EMS",
    str_detect(x, "^ACHÉ ") ~ "ACHÉ",
    str_detect(x, "^BIONATUS ") ~ "BIONATUS",
    str_detect(x, "^KLEY HERTZ ") ~ "KLEY HERTZ",
    str_detect(x, "^MAKROFARMA ") ~ "MAKROFARMA",
    str_detect(x, "^HERBARIUM ") ~ "HERBARIUM",
    str_detect(x, "^ZYDUS NIKKHO ") ~ "ZYDUS NIKKHO",
    TRUE ~ str_to_title(
      word(
        str_remove(x, " - [0-9]{2}[.].*$"),
        1,
        3
      )
    )
  )
}

ids_lideres_empresas <- dados_empresas %>%
  slice_head(n = 3) %>%
  pull(id_planta)

empresas_das_lideres <- base_ativos %>%
  filter(
    id_planta %in% ids_lideres_empresas,
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  distinct(
    id_planta,
    nome_planta,
    empresa_detentora_da_regularizacao,
    chave_produto
  ) %>%
  count(
    id_planta,
    nome_planta,
    empresa_detentora_da_regularizacao,
    name = "n_medicamentos_ativos"
  ) %>%
  mutate(
    planta = nome_curto_planta(nome_planta),
    empresa = str_remove(
      empresa_detentora_da_regularizacao,
      " - [0-9]{2}[.].*$"
    ),
    marca = marca_empresa(empresa_detentora_da_regularizacao)
  ) %>%
  group_by(id_planta) %>%
  arrange(
    desc(n_medicamentos_ativos),
    empresa_detentora_da_regularizacao,
    .by_group = TRUE
  ) %>%
  slice_head(n = 3) %>%
  ungroup()

stopifnot(
  nrow(dados_medicamentos) > 0,
  nrow(dados_empresas) > 0,
  all(dados_medicamentos$valor > 0),
  all(dados_empresas$valor > 0)
)

npc <- function(x) unit(x, "npc")

cor_alpha <- function(cor, alpha) {
  adjustcolor(cor, alpha.f = alpha)
}

retangulo <- function(
  x,
  y,
  largura,
  altura,
  preenchimento,
  borda = NA,
  raio = 0
) {
  if (raio > 0) {
    grid.roundrect(
      x = npc(x),
      y = npc(y),
      width = npc(largura),
      height = npc(altura),
      just = c("left", "bottom"),
      r = npc(raio),
      gp = gpar(
        fill = preenchimento,
        col = borda,
        lwd = ifelse(is.na(borda), 0, 0.8)
      )
    )
  } else {
    grid.rect(
      x = npc(x),
      y = npc(y),
      width = npc(largura),
      height = npc(altura),
      just = c("left", "bottom"),
      gp = gpar(
        fill = preenchimento,
        col = borda,
        lwd = ifelse(is.na(borda), 0, 0.8)
      )
    )
  }
}

texto <- function(
  rotulo,
  x,
  y,
  tamanho = 10,
  cor = paleta$tinta,
  negrito = FALSE,
  familia = familia_texto,
  just = c("left", "center"),
  rot = 0,
  lineheight = 0.95
) {
  grid.text(
    label = rotulo,
    x = npc(x),
    y = npc(y),
    just = just,
    rot = rot,
    gp = gpar(
      fontsize = tamanho,
      col = cor,
      fontface = ifelse(negrito, "bold", "plain"),
      fontfamily = familia,
      lineheight = lineheight
    )
  )
}

linha <- function(
  x0,
  y0,
  x1,
  y1,
  cor = paleta$linha,
  largura = 1,
  ponta = NULL,
  tipo = 1
) {
  grid.segments(
    x0 = npc(x0),
    y0 = npc(y0),
    x1 = npc(x1),
    y1 = npc(y1),
    arrow = ponta,
    gp = gpar(
      col = cor,
      lwd = largura,
      lty = tipo,
      lineend = "round"
    )
  )
}

circulo <- function(
  x,
  y,
  raio,
  preenchimento,
  borda = NA,
  largura = 0
) {
  grid.circle(
    x = npc(x),
    y = npc(y),
    r = npc(raio),
    gp = gpar(
      fill = preenchimento,
      col = borda,
      lwd = largura
    )
  )
}

folha <- function(
  x,
  y,
  comprimento,
  largura,
  angulo,
  cor,
  alpha = 1
) {
  t <- seq(0, 1, length.out = 30)
  x_local <- c(
    -comprimento / 2 + comprimento * t,
    rev(-comprimento / 2 + comprimento * t)
  )
  y_local <- c(
    largura / 2 * sin(pi * t),
    rev(-largura / 2 * sin(pi * t))
  )

  a <- angulo * pi / 180
  xr <- x_local * cos(a) - y_local * sin(a) + x
  yr <- x_local * sin(a) + y_local * cos(a) + y

  grid.polygon(
    x = npc(xr),
    y = npc(yr),
    gp = gpar(
      fill = cor_alpha(cor, alpha),
      col = NA
    )
  )

  linha(
    x - comprimento / 2 * cos(a),
    y - comprimento / 2 * sin(a),
    x + comprimento / 2 * cos(a),
    y + comprimento / 2 * sin(a),
    cor = cor_alpha(paleta$branco, alpha * 0.45),
    largura = 0.55
  )
}

ramo_decorativo <- function(x, y, escala = 1, cor = paleta$lima) {
  linha(
    x - 0.075 * escala,
    y - 0.045 * escala,
    x + 0.065 * escala,
    y + 0.05 * escala,
    cor = cor_alpha(cor, 0.7),
    largura = 2.2
  )
  folha(x - 0.045 * escala, y - 0.017 * escala, 0.050 * escala, 0.020 * escala, 45, cor, 0.95)
  folha(x - 0.005 * escala, y + 0.008 * escala, 0.057 * escala, 0.022 * escala, 145, cor, 0.90)
  folha(x + 0.032 * escala, y + 0.034 * escala, 0.052 * escala, 0.020 * escala, 35, cor, 0.85)
  folha(x + 0.058 * escala, y + 0.052 * escala, 0.040 * escala, 0.017 * escala, 150, cor, 0.75)
}

desenhar_cabecalho <- function(
  titulo,
  subtitulo
) {
  retangulo(0, 0, 1, 1, paleta$creme_claro)
  retangulo(0, 0.905, 1, 0.095, paleta$verde_escuro)

  texto(
    "GUIA VISUAL • FITOTERÁPICOS COM REGISTRO ATIVO",
    0.055,
    0.961,
    tamanho = 8.3,
    cor = paleta$lima,
    negrito = TRUE,
    familia = familia_display
  )

  ramo_decorativo(0.88, 0.948, escala = 1.25, cor = paleta$lima)

  texto(
    titulo,
    0.055,
    0.862,
    tamanho = 23,
    cor = paleta$tinta,
    negrito = TRUE,
    familia = familia_display,
    lineheight = 0.88
  )

  texto(
    subtitulo,
    0.055,
    0.793,
    tamanho = 8.3,
    cor = paleta$cinza,
    lineheight = 1.05
  )
}

desenhar_ranking <- function(dados, cor_acento, rotulo_valor) {
  lideres <- dados %>%
    slice_head(n = 12)

  texto(
    "AS 12 LÍDERES",
    0.055,
    0.755,
    tamanho = 9.2,
    cor = paleta$verde,
    negrito = TRUE,
    familia = familia_display
  )

  texto(
    paste0("número de ", rotulo_valor),
    0.62,
    0.755,
    tamanho = 6.4,
    cor = paleta$cinza,
    just = c("right", "center")
  )

  y_topo <- 0.728
  passo <- 0.0186
  x_barra <- 0.265
  largura_maxima <- 0.305
  maximo <- max(lideres$valor)

  for (i in seq_len(nrow(lideres))) {
    y <- y_topo - (i - 1) * passo
    valor <- lideres$valor[i]
    proporcao <- valor / maximo
    posicao <- lideres$posicao[i]
    recebe_enfase <- posicao <= 3

    cor_barra <- case_when(
      posicao == 1 ~ cor_acento,
      posicao == 2 ~ paleta$lima,
      posicao == 3 ~ paleta$laranja,
      TRUE ~ paleta$verde
    )

    texto(
      sprintf("%02d", posicao),
      0.055,
      y,
      tamanho = 6.2,
      cor = ifelse(recebe_enfase, cor_barra, paleta$cinza),
      negrito = TRUE,
      familia = familia_display
    )

    texto(
      lideres$planta[i],
      0.085,
      y,
      tamanho = 7.0,
      cor = paleta$tinta,
      negrito = recebe_enfase
    )

    retangulo(
      x_barra,
      y - 0.004,
      largura_maxima,
      0.008,
      cor_alpha(paleta$verde, 0.10),
      raio = 0.004
    )

    retangulo(
      x_barra,
      y - 0.004,
      largura_maxima * proporcao,
      0.008,
      cor_alpha(cor_barra, ifelse(recebe_enfase, 1, 0.72)),
      raio = 0.004
    )

    circulo(
      0.602,
      y,
      0.012,
      ifelse(recebe_enfase, cor_barra, paleta$creme),
      borda = ifelse(recebe_enfase, NA, paleta$linha),
      largura = 0.7
    )

    texto(
      as.character(valor),
      0.602,
      y,
      tamanho = 6.1,
      cor = ifelse(recebe_enfase, paleta$branco, paleta$tinta),
      negrito = TRUE,
      familia = familia_display,
      just = c("center", "center")
    )
  }
}

desenhar_leitura_rapida <- function(
  dados,
  tipo = c("medicamentos", "empresas"),
  cor_acento
) {
  tipo <- match.arg(tipo)
  participacao_top10 <- sum(head(dados$valor, 10)) / sum(dados$valor)

  retangulo(
    0.66,
    0.505,
    0.285,
    0.242,
    paleta$creme,
    borda = paleta$linha,
    raio = 0.014
  )

  texto(
    "LEITURA RÁPIDA",
    0.685,
    0.720,
    tamanho = 8.7,
    cor = paleta$verde,
    negrito = TRUE,
    familia = familia_display
  )

  desenhar_item <- function(numero, rotulo, y, cor_numero) {
    texto(
      numero,
      0.685,
      y,
      tamanho = if (nchar(numero) > 4) 11 else 16,
      cor = cor_numero,
      negrito = TRUE,
      familia = familia_display
    )
    texto(
      rotulo,
      0.765,
      y,
      tamanho = 6.8,
      cor = paleta$tinta,
      negrito = TRUE,
      lineheight = 0.98
    )
  }

  if (tipo == "medicamentos") {
    desenhar_item(
      as.character(dados$valor[1]),
      paste0(
        dados$planta[1],
        " lidera o ranking de\nmedicamentos ativos."
      ),
      0.672,
      cor_acento
    )

    desenhar_item(
      paste0(dados$valor[2], " / ", dados$valor[3]),
      paste0(
        dados$planta[2],
        " e ",
        dados$planta[3],
        "\nvêm logo depois."
      ),
      0.616,
      paleta$laranja
    )

    desenhar_item(
      percent(
        participacao_top10,
        accuracy = 0.1,
        decimal.mark = ","
      ),
      "das associações estão\nnas 10 primeiras plantas.",
      0.553,
      paleta$verde
    )
  } else {
    valores_distintos <- sort(unique(dados$valor), decreasing = TRUE)
    valor_terceiro <- valores_distintos[3]
    plantas_terceiro <- dados %>%
      filter(valor == valor_terceiro) %>%
      pull(planta)

    desenhar_item(
      as.character(dados$valor[1]),
      paste0(
        dados$planta[1],
        " reúne o maior\nnúmero de empresas."
      ),
      0.672,
      cor_acento
    )

    desenhar_item(
      as.character(dados$valor[2]),
      paste0(
        dados$planta[2],
        " ocupa a\nsegunda posição."
      ),
      0.616,
      paleta$lima
    )

    desenhar_item(
      as.character(valor_terceiro),
      paste0(
        paste(plantas_terceiro, collapse = ", "),
        "\nempatam na 3ª posição."
      ),
      0.553,
      paleta$laranja
    )
  }
}

desenhar_faixa_pratica <- function(tipo = c("medicamentos", "empresas")) {
  tipo <- match.arg(tipo)

  if (tipo == "medicamentos") {
    return(invisible(NULL))
  } else {
    texto(
      "EXEMPLOS DE EMPRESAS NAS PLANTAS LÍDERES",
      0.055,
      0.478,
      tamanho = 8.4,
      cor = paleta$verde,
      negrito = TRUE,
      familia = familia_display
    )

    texto(
      "empresas com mais medicamentos ativos associados; número entre parênteses",
      0.945,
      0.478,
      tamanho = 5.8,
      cor = paleta$cinza,
      just = c("right", "center")
    )

    lideres <- dados_empresas %>%
      slice_head(n = 3)

    for (i in seq_len(nrow(lideres))) {
      x <- 0.055 + (i - 1) * 0.305
      cor_card <- c(paleta$coral, paleta$laranja, paleta$azul)[i]
      empresas_planta <- empresas_das_lideres %>%
        filter(id_planta == lideres$id_planta[i])

      lista_empresas <- paste0(
        empresas_planta$marca,
        " (",
        empresas_planta$n_medicamentos_ativos,
        ")",
        collapse = "  •  "
      )

      retangulo(
        x,
        0.397,
        0.285,
        0.066,
        cor_alpha(cor_card, 0.12),
        borda = cor_alpha(cor_card, 0.35),
        raio = 0.011
      )

      texto(
        str_to_upper(lideres$planta[i]),
        x + 0.015,
        0.444,
        tamanho = 7.3,
        cor = cor_card,
        negrito = TRUE,
        familia = familia_display
      )

      texto(
        str_wrap(lista_empresas, width = 30),
        x + 0.015,
        0.419,
        tamanho = 6.0,
        cor = paleta$tinta,
        negrito = TRUE,
        lineheight = 0.95
      )
    }
  }
}

desenhar_mapa_completo <- function(
  dados,
  rotulo_valor,
  cor_acento,
  y_separador,
  y_cabecalho,
  y_topo,
  passo
) {
  restantes <- dados %>%
    slice(-(1:12))

  linha(
    0.055,
    y_separador,
    0.945,
    y_separador,
    cor = cor_alpha(paleta$verde, 0.42),
    largura = 1.2,
    tipo = "24"
  )

  texto(
    "MAPA COMPLETO",
    0.055,
    y_cabecalho,
    tamanho = 8.6,
    cor = paleta$verde,
    negrito = TRUE,
    familia = familia_display
  )

  texto(
    paste0("posição • planta • nº de ", rotulo_valor),
    0.945,
    y_cabecalho,
    tamanho = 5.9,
    cor = paleta$cinza,
    just = c("right", "center")
  )

  n_colunas <- 4
  n_linhas <- ceiling(nrow(restantes) / n_colunas)
  largura_coluna <- 0.2225
  fonte_nome <- ifelse(passo >= 0.017, 6.1, 5.65)
  fonte_posicao <- ifelse(passo >= 0.017, 5.6, 5.25)
  fonte_valor <- ifelse(passo >= 0.017, 5.25, 4.9)
  raio_valor <- ifelse(passo >= 0.017, 0.0090, 0.0082)
  altura_faixa <- min(0.0124, passo * 0.88)

  for (i in seq_len(nrow(restantes))) {
    coluna <- (i - 1) %/% n_linhas
    linha_indice <- (i - 1) %% n_linhas
    x <- 0.055 + coluna * largura_coluna
    y <- y_topo - linha_indice * passo
    valor <- restantes$valor[i]

    if (linha_indice %% 2 == 0) {
      retangulo(
        x,
        y - altura_faixa / 2,
        largura_coluna - 0.007,
        altura_faixa,
        cor_alpha(paleta$creme, 0.48),
        raio = 0.003
      )
    }

    texto(
      sprintf("%02d", restantes$posicao[i]),
      x + 0.006,
      y,
      tamanho = fonte_posicao,
      cor = cor_alpha(cor_acento, 0.85),
      negrito = TRUE,
      familia = familia_display
    )

    texto(
      restantes$planta[i],
      x + 0.035,
      y,
      tamanho = fonte_nome,
      cor = paleta$tinta,
      negrito = valor >= 5
    )

    circulo(
      x + 0.202,
      y,
      raio_valor,
      case_when(
        valor >= 8 ~ cor_acento,
        valor >= 4 ~ paleta$lima,
        valor >= 2 ~ paleta$laranja,
        TRUE ~ paleta$creme
      ),
      borda = ifelse(valor == 1, paleta$linha, NA),
      largura = 0.6
    )

    texto(
      as.character(valor),
      x + 0.202,
      y,
      tamanho = fonte_valor,
      cor = ifelse(valor == 1, paleta$tinta, paleta$verde_escuro),
      negrito = TRUE,
      familia = familia_display,
      just = c("center", "center")
    )
  }
}

desenhar_rodape <- function() {
  linha(0.055, 0.062, 0.945, 0.062, cor = paleta$linha, largura = 0.8)

  texto(
    "Fonte: base do projeto construída a partir de registros da Anvisa • filtro: situação “Ativo”",
    0.055,
    0.044,
    tamanho = 5.65,
    cor = paleta$cinza
  )

  texto(
    "Cada medicamento ou empresa é contado uma vez em cada planta à qual está associado.",
    0.055,
    0.025,
    tamanho = 5.4,
    cor = paleta$cinza
  )
}

desenhar_infografico_medicamentos <- function() {
  grid.newpage()

  desenhar_cabecalho(
    titulo = "Quais plantas já aparecem\ncom força nos medicamentos?",
    subtitulo = paste0(
      nrow(dados_medicamentos),
      " plantas com medicamentos ativos • cada número representa uma associação única planta–medicamento"
    )
  )

  desenhar_ranking(
    dados_medicamentos,
    cor_acento = paleta$coral,
    rotulo_valor = "medicamentos ativos"
  )

  desenhar_leitura_rapida(
    dados_medicamentos,
    tipo = "medicamentos",
    cor_acento = paleta$coral
  )

  desenhar_mapa_completo(
    dados_medicamentos,
    rotulo_valor = "medicamentos",
    cor_acento = paleta$coral,
    y_separador = 0.475,
    y_cabecalho = 0.447,
    y_topo = 0.418,
    passo = 0.0181
  )

  desenhar_rodape()
}

desenhar_infografico_empresas <- function() {
  grid.newpage()

  desenhar_cabecalho(
    titulo = "Onde há mais empresas\natuando com essas plantas?",
    subtitulo = paste0(
      nrow(dados_empresas),
      " plantas com empresas ligadas a medicamentos ativos"
    )
  )

  desenhar_ranking(
    dados_empresas,
    cor_acento = paleta$azul,
    rotulo_valor = "empresas"
  )

  desenhar_leitura_rapida(
    dados_empresas,
    tipo = "empresas",
    cor_acento = paleta$azul
  )

  desenhar_mapa_completo(
    dados_empresas,
    rotulo_valor = "empresas",
    cor_acento = paleta$azul,
    y_separador = 0.475,
    y_cabecalho = 0.447,
    y_topo = 0.418,
    passo = 0.0181
  )

  desenhar_rodape()
}

abrir_pdf <- function(file, width, height, family, bg) {
  if (capabilities("aqua")) {
    grDevices::quartz(
      type = "pdf", file = file, width = width,
      height = height, family = family, bg = bg
    )
  } else if (capabilities("cairo")) {
    grDevices::cairo_pdf(
      file = file, width = width, height = height,
      family = family, bg = bg
    )
  } else {
    stop("A geração de PDF requer Cairo (Linux) ou Quartz (macOS).")
  }
}

exportar_pagina <- function(funcao_desenho, nome_base) {
  caminho_png <- file.path(dir_saida, paste0(nome_base, ".png"))
  caminho_pdf <- file.path(dir_saida, paste0(nome_base, ".pdf"))
  caminho_svg <- file.path(dir_saida, paste0(nome_base, ".svg"))

  agg_png(
    filename = caminho_png,
    width = 2480,
    height = 3508,
    units = "px",
    res = 300,
    background = paleta$creme_claro
  )
  funcao_desenho()
  dev.off()

  abrir_pdf(
    file = caminho_pdf,
    width = largura_a4,
    height = altura_a4,
    family = familia_texto,
    bg = paleta$creme_claro
  )
  funcao_desenho()
  dev.off()

  svglite(
    file = caminho_svg,
    width = largura_a4,
    height = altura_a4,
    bg = paleta$creme_claro,
    system_fonts = list(
      sans = familia_texto,
      display = familia_display
    )
  )
  funcao_desenho()
  dev.off()

  stopifnot(file.exists(caminho_png), file.exists(caminho_pdf), file.exists(caminho_svg))
  invisible(
    c(
      png = caminho_png,
      pdf = caminho_pdf,
      svg = caminho_svg
    )
  )
}

arquivos_14 <- exportar_pagina(
  desenhar_infografico_medicamentos,
  "14_infografico_medicamentos_ativos_por_planta"
)

arquivos_15 <- exportar_pagina(
  desenhar_infografico_empresas,
  "15_infografico_empresas_por_planta_ativos"
)

caminho_pdf_conjunto <- file.path(
  dir_saida,
  "14_15_infograficos_plantas_mercado.pdf"
)

abrir_pdf(
  file = caminho_pdf_conjunto,
  width = largura_a4,
  height = altura_a4,
  family = familia_texto,
  bg = paleta$creme_claro
)
desenhar_infografico_medicamentos()
desenhar_infografico_empresas()
dev.off()

writexl::write_xlsx(list(
  medicamentos = dados_medicamentos,
  empresas = dados_empresas,
  empresas_lideres = empresas_das_lideres
), file.path(dir_saida, "dados_infograficos.xlsx"))

manifesto <- c(
  "INFOGRAFICOS 14 E 15 — PACOTE PARA AGRICULTORES",
  "",
  paste0("Gerado em: ", Sys.Date()),
  paste0("Script novo: ", normalizePath("gerar_infograficos.R")),
  paste0("Fonte de dados: ", normalizePath(arquivo_base)),
  "",
  "CRITERIOS",
  "- Foram mantidos apenas registros com situacao == \"Ativo\".",
  "- Medicamentos: produto unico (chave_produto) contado uma vez por planta.",
  "- Empresas: empresa unica contada uma vez por planta.",
  "- Os nomes das plantas usam o primeiro nome antes da barra, como nos graficos originais.",
  "- Posicoes empatadas recebem o mesmo numero e a mesma enfase visual.",
  "- As empresas em destaque sao ordenadas pelo numero de medicamentos ativos associados a cada planta.",
  "",
  "ARQUIVOS",
  paste0("- ", basename(arquivos_14)),
  paste0("- ", basename(arquivos_15)),
  paste0("- ", basename(caminho_pdf_conjunto)),
  "- dados_infograficos.xlsx",
  "",
  "NOTA DE INTERPRETACAO",
  "Os numeros retratam presenca em registros ativos da Anvisa.",
  "Nao devem ser interpretados isoladamente como preco, demanda futura ou garantia de compra."
)

writeLines(
  manifesto,
  file.path(dir_saida, "MANIFESTO_infograficos.txt"),
  useBytes = TRUE
)

cat("\nInfograficos criados em: ", dir_saida, "\n", sep = "")
cat("- ", arquivos_14[["pdf"]], "\n", sep = "")
cat("- ", arquivos_15[["pdf"]], "\n", sep = "")
cat("- ", caminho_pdf_conjunto, "\n", sep = "")
