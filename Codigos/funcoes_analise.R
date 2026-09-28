# Leitura da fonte final e funções compartilhadas pelas análises.

pacotes <- c(
  "dplyr", "tidyr", "stringr", "lubridate", "ggplot2",
  "scales", "forcats", "purrr", "readr", "rlang", "readxl", "writexl"
)
faltando <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltando)) stop("Instale os pacotes: ", paste(faltando, collapse = ", "))
suppressPackageStartupMessages(invisible(lapply(pacotes, library, character.only = TRUE)))

options(scipen = 999)
theme_set(theme_minimal(base_size = 12))

CORES <- list(
  principal = "#2C7FB8",
  destaque = "#E6550D",
  substancia = "#3182BD",
  empresa = "#8856A7",
  planta = "#41AB5D",
  situacao = c("Ativo" = "#1A9850", "Inativo" = "#D73027", "Não informado" = "grey70")
)

salvar_fig <- function(plot, nome, dir_fig, largura = 9, altura = 6) {
  dir.create(dir_fig, recursive = TRUE, showWarnings = FALSE)
  args <- list(
    filename = file.path(dir_fig, paste0(nome, ".png")), plot = plot,
    width = largura, height = altura, dpi = 150, bg = "white"
  )
  do.call(ggplot2::ggsave, args)
}

slug <- function(x) str_replace_all(str_to_lower(x), "[^a-z0-9]+", "_")

normaliza_situacao <- function(x) {
  x <- toupper(str_squish(as.character(x)))
  dplyr::case_when(
    x %in% c("ATIVO", "VÁLIDO", "VALIDO") ~ "Ativo",
    x %in% c("INATIVO", "INVÁLIDO", "INVALIDO") ~ "Inativo",
    is.na(x) | x %in% c("", "NA") ~ "Não informado",
    TRUE ~ str_to_title(x)
  )
}

# Harmoniza as seis abas em memória; a planilha original permanece intacta.
ler_produtos_por_nome <- function(arquivo) {
  categorias <- c(
    alimentos = "Alimentos",
    cosmeticos_registrados = "Cosméticos Registrados",
    cosmeticos_regularizados = "Cosméticos Isentos de Registro",
    medicamentos = "Medicamentos por nome",
    saneantes = "Saneantes",
    produtos_saude = "Produtos para Saúde"
  )
  abas <- readxl::excel_sheets(arquivo)
  if (length(setdiff(abas, names(categorias)))) {
    stop("Abas não reconhecidas: ", paste(setdiff(abas, names(categorias)), collapse = ", "))
  }
  campo <- function(df, candidatos) {
    valor <- rep(NA_character_, nrow(df))
    for (nome in intersect(candidatos, names(df))) {
      novo <- na_if(str_squish(as.character(df[[nome]])), "")
      valor <- coalesce(valor, novo)
    }
    valor
  }
  ano <- function(x) {
    case_when(
      str_detect(x, "^[12][0-9]{3}-") ~ as.integer(substr(x, 1, 4)),
      str_detect(x, "^[0-9]{2}/[12][0-9]{3}$") ~ as.integer(substr(x, 4, 7)),
      str_detect(x, "^[0-9]{2}[12][0-9]{3}$") ~ as.integer(substr(x, 3, 6)),
      TRUE ~ NA_integer_
    )
  }
  map_dfr(abas, function(aba) {
    d <- readxl::read_excel(arquivo, sheet = aba, col_types = "text")
    if (!all(c("_search_term", "_category") %in% names(d))) {
      stop("Aba sem termo/categoria de origem: ", aba)
    }
    if (any(is.na(d$`_category`) | d$`_category` != aba)) stop("Categoria inconsistente na aba: ", aba)
    data_reg <- campo(d, c("processo.dataRegularizacao", "produto.dataRegistro", "dataInicioVigencia"))
    data_venc <- campo(d, c(
      "vencimento", "produto.dataVencimentoRegistro", "produto.vencimento",
      "vencimento.data", "dataVencimento"
    ))
    tibble(
      categoria = unname(categorias[aba]),
      termo_busca = str_to_lower(str_squish(d$`_search_term`)),
      empresa = campo(d, c("detentorRegistro.razaoSocial", "nomeEmpresa", "empresa.razaoSocial")),
      produto = campo(d, c("produto.descricao", "nomeProduto", "produto.nome", "produto")),
      cnpj = str_pad(str_remove_all(campo(d, c("detentorRegistro.cnpj", "cnpjEmpresa", "empresa.cnpj")), "[^0-9]"), 14, pad = "0"),
      registro = campo(d, c("produto.numeroRegistroOuNotificacao", "registro", "produto.numeroRegistro")),
      processo = campo(d, c("processo.numero", "processo")),
      situacao = normaliza_situacao(campo(d, c("produto.situacaoRegistro", "situacaoProdutoFormatado", "produto.situacaoApresentacao", "situacao"))),
      ano_reg = suppressWarnings(ano(data_reg)),
      ano_venc = suppressWarnings(ano(data_venc)),
      ano_ref = coalesce(ano_reg, ano_venc)
    )
  })
}

recorte_produtos_por_nome <- function(base, apenas_ativos = TRUE) {
  base <- base %>%
    filter(categoria %in% c("Alimentos", "Cosméticos Registrados", "Cosméticos Isentos de Registro", "Saneantes")) %>%
    filter(is.na(ano_ref) | ano_ref >= 2000, !termo_busca %in% c("china", "barata"))
  if (apenas_ativos) base <- filter(base, situacao == "Ativo")
  base
}

fmt_n <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)

# Barras horizontais com os n maiores. `cat` e `val` sao nomes de coluna (sem aspas).
barras_top <- function(df, cat, val, titulo, cor = CORES$principal,
                       n = 15, trunc = 45, ylab = "Nº de registros") {
  cat <- ensym(cat)
  val <- ensym(val)
  df %>%
    slice_max(!!val, n = n, with_ties = FALSE) %>%
    mutate(.rotulo = str_trunc(as.character(!!cat), trunc)) %>%
    ggplot(aes(reorder(.rotulo, !!val), !!val)) +
    geom_col(fill = cor) +
    geom_text(aes(label = fmt_n(!!val)), hjust = -0.1, size = 3.2) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.13))) +
    labs(title = titulo, x = NULL, y = ylab)
}

# Heatmap de contagens. Espera um data frame longo com as colunas
# `linha`, `coluna` e `n`. Ordena ambos os eixos pelo total.
heatmap_contagem <- function(df_long, titulo, lab_x, lab_y,
                             cor_alta = CORES$principal, rotular = FALSE) {
  d <- df_long %>%
    mutate(
      linha = fct_reorder(factor(linha), n, .fun = sum),
      coluna = fct_reorder(factor(coluna), n, .fun = sum)
    )
  g <- ggplot(d, aes(coluna, linha, fill = n)) +
    geom_tile(color = "white", linewidth = 0.3) +
    scale_fill_gradient(
      low = "#EEF3F7", high = cor_alta, trans = "sqrt",
      name = "Nº reg.", labels = label_number(accuracy = 1)
    ) +
    labs(title = titulo, x = lab_x, y = lab_y) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y = element_text(size = 8),
      panel.grid = element_blank()
    )
  if (rotular) {
    g <- g + geom_text(aes(label = ifelse(n > 0, n, "")), size = 2.4, color = "grey20")
  }
  g
}
