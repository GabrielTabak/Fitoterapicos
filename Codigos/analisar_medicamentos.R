args_script <- commandArgs(trailingOnly = FALSE)
arquivo_script <- sub("^--file=", "", args_script[grepl("^--file=", args_script)])
if (length(arquivo_script)) {
  setwd(dirname(normalizePath(gsub("~+~", " ", arquivo_script, fixed = TRUE))))
}

# Consolida a coleta por princípio ativo e gera as figuras finais.
# Expressões completas evitam falsos matches; exceções estão declaradas abaixo.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(readxl)
  library(stringr)
  library(stringi)
  library(janitor)
  library(ggplot2)
  library(forcats)
  library(scales)
  library(writexl)
  library(tibble)
})

argumentos <- commandArgs(trailingOnly = TRUE)
exportar_auditoria <- "--auditoria" %in% argumentos
exportar_latex <- "--tabelas-relatorio" %in% argumentos
figuras_relatorio <- c(
  "01_top_plantas_situacao", "04_distribuicao_medicamentos_por_planta",
  "13_proporcao_medicamentos_ativos_por_planta", "08_heatmap_empresas_plantas_ativos",
  "10_rede_coocorrencia_plantas_ativos", "06_medicamentos_por_ano_processo",
  "12_vencimentos_medicamentos_ativos", "14_pizza_medicamentos_ativos_por_planta",
  "15_pizza_empresas_por_planta_ativos", "02b_top_plantas_por_opcao_clicada_ativos",
  "05c_empresas_por_planta_ativos"
)

arquivo_plantas <- file.path("dados/plantas_busca.csv")
dir_downloads <- file.path("Downloads")
arquivo_progresso <- file.path(dir_downloads, "progresso_anvisa.csv")
dir_saida <- file.path("ResultadosFinal")
dir_bases <- file.path("ResultadosFinal/Bases")
dir_tabelas <- file.path("ResultadosFinal/Tabelas")
dir_figuras <- file.path("ResultadosFinal/Figuras")
dir_mapas_data <- file.path("../Codigos - Mapas/Data")

# Permite reconstruir o projeto em uma pasta sem resultados preexistentes.
for (diretorio in c(dir_saida, dir_bases, dir_tabelas, dir_figuras)) {
  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
}

options(scipen = 999)

normalizar_texto <- function(x) {
  x <- ifelse(is.na(x), NA_character_, as.character(x))
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^a-z0-9]+", " ")
  str_squish(x)
}

contem_expressao <- function(texto, expressao) {
  texto <- normalizar_texto(texto)
  expressao <- normalizar_texto(expressao)
  !is.na(texto) & !is.na(expressao) & expressao != "" &
    str_detect(paste0(" ", texto, " "), fixed(paste0(" ", expressao, " ")))
}

primeiro_nao_vazio <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

colar_unicos <- function(x) {
  x <- sort(unique(as.character(x[!is.na(x) & str_squish(x) != ""])))
  if (length(x) == 0) NA_character_ else paste(x, collapse = " | ")
}

normalizar_situacao <- function(x) {
  z <- normalizar_texto(x)
  case_when(
    z %in% c("ativo", "valido") ~ "Ativo",
    z %in% c("inativo", "invalido", "cancelado", "caducado") ~ "Inativo",
    is.na(z) | z == "" ~ "Não informado",
    TRUE ~ str_to_title(z)
  )
}

nome_curto_planta <- function(x) {
  str_squish(str_split_fixed(as.character(x), "/", 2)[, 1])
}

salvar_csv <- function(x, caminho) {
  essenciais <- c("base_produto_planta.csv", "medicamentos_ativos_empresas.csv")
  if (exportar_auditoria || basename(caminho) %in% essenciais) {
    readr::write_csv2(x, caminho, na = "")
  }
}

salvar_tex <- function(x, nome, legenda, rotulo, alinhamento,
                       digitos = 2, larguras = NULL, tabela_longa = FALSE) {
  if (!exportar_latex) {
    return(invisible(NULL))
  }
  tabela <- knitr::kable(
    x,
    format = "latex",
    booktabs = TRUE,
    longtable = tabela_longa,
    linesep = "",
    caption = legenda,
    label = str_remove(rotulo, "^tab:"),
    align = alinhamento,
    digits = digitos,
    escape = TRUE
  ) %>%
    kableExtra::kable_styling(
      latex_options = if (tabela_longa) "repeat_header" else "hold_position",
      repeat_header_text = "\\textit{(continuação)}",
      full_width = FALSE
    )

  if (!is.null(larguras)) {
    for (i in seq_along(larguras)) {
      if (!is.na(larguras[i])) {
        tabela <- kableExtra::column_spec(tabela, i, width = larguras[i])
      }
    }
  }

  writeLines(
    as.character(tabela),
    file.path(dir_tabelas, nome),
    useBytes = TRUE
  )
}

salvar_figura <- function(grafico, nome, largura = 10, altura = 6) {
  if (!nome %in% figuras_relatorio) {
    return(invisible(NULL))
  }
  caminho_png <- file.path(dir_figuras, paste0(nome, ".png"))
  caminho_pdf <- file.path(dir_figuras, paste0(nome, ".pdf"))
  ggsave(caminho_png, grafico, width = largura, height = altura, dpi = 300, bg = "white")
  ggsave(caminho_pdf, grafico, width = largura, height = altura, bg = "white")
}

tema_final <- theme_classic(base_family = "serif", base_size = 11) +
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_text(face = "bold", size = 12, color = "#202020"),
    plot.subtitle = element_text(size = 10, color = "#4A4A4A", margin = margin(b = 8)),
    plot.caption = element_text(size = 8, color = "#555555", hjust = 0, margin = margin(t = 8)),
    axis.title = element_text(size = 10, color = "#222222"),
    axis.text = element_text(size = 9, color = "#222222"),
    axis.line = element_line(color = "#333333", linewidth = 0.35),
    axis.ticks = element_line(color = "#555555", linewidth = 0.3),
    panel.grid.major = element_line(color = "#E3E3E3", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.key = element_blank(),
    plot.margin = margin(10, 14, 8, 10)
  )

cores_situacao <- c(
  "Ativo" = "#526A61",
  "Inativo" = "#8A6268",
  "Não informado" = "#A6A6A6"
)

cores_opcoes <- c(
  "1" = "#3F5364",
  "2" = "#657786",
  "3" = "#8797A3",
  "4" = "#A8B3BA",
  "5" = "#C2C9CD",
  "6" = "#D7DBDD"
)

# Paleta mais viva para a figura de medicamentos ativos. As cores mantêm bom
# contraste entre segmentos e continuam distinguiveis para daltonismo comum.
cores_opcoes_ativos <- c(
  "1" = "#4477AA",
  "2" = "#EE6677",
  "3" = "#228833",
  "4" = "#CCBB44",
  "5" = "#66CCEE",
  "6" = "#AA3377"
)

cores_rotulos_opcoes_ativos <- c(
  "1" = "white",
  "2" = "#202020",
  "3" = "white",
  "4" = "#202020",
  "5" = "#202020",
  "6" = "white"
)

plantas <- readr::read_csv2(
  arquivo_plantas,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) %>%
  mutate(
    id_planta = row_number(),
    nome_planta = str_squish(Nome_popular_buscado),
    nome_planta_norm = normalizar_texto(nome_planta)
  )

colunas_alias <- c(
  "Nome_popular_buscado",
  "Nome_cientifico_buscado",
  "Variacoes_nome_popular_encontradas",
  "Variacoes_nome_cientifico_encontradas"
)

dicionario_alias <- plantas %>%
  select(id_planta, nome_planta, nome_planta_norm, all_of(colunas_alias)) %>%
  pivot_longer(
    cols = all_of(colunas_alias),
    names_to = "origem_alias",
    values_to = "alias"
  ) %>%
  filter(!is.na(alias), str_squish(alias) != "") %>%
  separate_rows(alias, sep = "\\s*[;/]\\s*") %>%
  mutate(
    alias = str_squish(alias),
    alias_norm = normalizar_texto(alias),
    tipo_alias = if_else(str_detect(origem_alias, "popular"), "popular", "cientifico"),
    alias_principal = str_detect(origem_alias, "_buscado$")
  ) %>%
  filter(!is.na(alias_norm), alias_norm != "") %>%
  distinct(id_planta, alias_norm, .keep_all = TRUE)

alias_por_planta <- dicionario_alias %>%
  group_by(id_planta, nome_planta, nome_planta_norm) %>%
  summarise(
    aliases = list(sort(unique(alias_norm))),
    aliases_populares = list(sort(unique(alias_norm[tipo_alias == "popular"]))),
    .groups = "drop"
  )

termo_para_planta <- dicionario_alias %>%
  distinct(id_planta, nome_planta, nome_planta_norm, termo_norm = alias_norm)

progresso <- readr::read_csv(
  arquivo_progresso,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) %>%
  mutate(
    id_linha_progresso = row_number(),
    termo = str_squish(as.character(termo)),
    opcao = str_squish(as.character(opcao)),
    termo_norm = normalizar_texto(termo),
    opcao_norm = normalizar_texto(opcao)
  )

status_com_arquivo <- c(
  "excel_baixado", "arquivo_existente", "arquivo_legado", "legado_existente"
)

arquivos_progresso <- progresso %>%
  filter(status %in% status_com_arquivo, !is.na(arquivo), arquivo != "") %>%
  mutate(
    nome_arquivo = basename(arquivo),
    caminho_registrado = arquivo,
    caminho_downloads = file.path(dir_downloads, nome_arquivo),
    caminho_arquivo = case_when(
      file.exists(caminho_downloads) ~ caminho_downloads,
      file.exists(caminho_registrado) ~ caminho_registrado,
      TRUE ~ caminho_downloads
    )
  ) %>%
  arrange(id_linha_progresso) %>%
  group_by(nome_arquivo) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(id_arquivo = row_number())

colunas_anvisa <- c(
  "nome_do_produto",
  "complemento_da_marca",
  "principio_ativo_ou_descricao_do_medicamento_notificado",
  "tipo_de_regularizacao",
  "numero_da_regularizacao",
  "numero_do_processo",
  "empresa_detentora_da_regularizacao",
  "situacao_da_regularizacao",
  "vencimento_da_regularizacao"
)

ler_arquivo_anvisa <- function(id_arquivo, caminho_arquivo) {
  if (!file.exists(caminho_arquivo)) {
    return(list(
      dados = tibble(),
      auditoria = tibble(
        id_arquivo = id_arquivo,
        arquivo_encontrado = FALSE,
        leitura_ok = FALSE,
        n_linhas = NA_integer_,
        erro_leitura = "Arquivo nao encontrado"
      )
    ))
  }

  tryCatch(
    {
      # Embora tenham extensao .xlsx, os arquivos exportados pela ANVISA sao XLS.
      assinatura <- readBin(caminho_arquivo, "raw", n = 2)
      leitor <- if (identical(assinatura, charToRaw("PK"))) readxl::read_xlsx else readxl::read_xls
      x <- suppressMessages(leitor(caminho_arquivo, col_types = "text")) %>%
        janitor::clean_names()

      ausentes <- setdiff(colunas_anvisa, names(x))
      if (length(ausentes) > 0) x[ausentes] <- NA_character_

      x <- x %>%
        select(all_of(colunas_anvisa)) %>%
        mutate(across(everything(), as.character)) %>%
        mutate(id_arquivo = id_arquivo, .before = 1)

      list(
        dados = x,
        auditoria = tibble(
          id_arquivo = id_arquivo,
          arquivo_encontrado = TRUE,
          leitura_ok = TRUE,
          n_linhas = nrow(x),
          erro_leitura = NA_character_
        )
      )
    },
    error = function(e) {
      list(
        dados = tibble(),
        auditoria = tibble(
          id_arquivo = id_arquivo,
          arquivo_encontrado = TRUE,
          leitura_ok = FALSE,
          n_linhas = NA_integer_,
          erro_leitura = conditionMessage(e)
        )
      )
    }
  )
}

leituras <- map2(
  arquivos_progresso$id_arquivo,
  arquivos_progresso$caminho_arquivo,
  ler_arquivo_anvisa
)

auditoria_arquivos <- map_dfr(leituras, "auditoria") %>%
  left_join(
    arquivos_progresso %>%
      select(id_arquivo, termo, opcao, status, nome_arquivo, caminho_arquivo),
    by = "id_arquivo"
  ) %>%
  select(
    id_arquivo, termo, opcao, status, nome_arquivo, caminho_arquivo,
    arquivo_encontrado, leitura_ok, n_linhas, erro_leitura
  )

base_anvisa_bruta <- map_dfr(leituras, "dados") %>%
  left_join(
    arquivos_progresso %>% select(id_arquivo, termo, opcao, nome_arquivo),
    by = "id_arquivo"
  ) %>%
  relocate(id_arquivo, nome_arquivo, termo, opcao)

correspondencias <- arquivos_progresso %>%
  select(id_arquivo, nome_arquivo, termo, termo_norm, opcao, opcao_norm) %>%
  left_join(termo_para_planta, by = "termo_norm", relationship = "many-to-many") %>%
  left_join(
    alias_por_planta %>% select(id_planta, aliases, aliases_populares),
    by = "id_planta"
  )

encontrar_alias <- function(opcao, aliases) {
  if (is.na(opcao) || length(aliases) == 0 || all(is.na(aliases))) {
    return(NA_character_)
  }
  encontrados <- aliases[vapply(aliases, function(a) contem_expressao(opcao, a), logical(1))]
  encontrados <- encontrados[order(nchar(encontrados), decreasing = TRUE)]
  if (length(encontrados) == 0) NA_character_ else encontrados[1]
}

correspondencias <- correspondencias %>%
  mutate(
    alias_encontrado = map2_chr(opcao_norm, aliases, encontrar_alias),
    match_alias_completo = !is.na(alias_encontrado),
    termo_aparece_completo = contem_expressao(opcao_norm, termo_norm)
  )

# Excecoes pequenas, explicitas e revisaveis. Elas corrigem apenas variacoes
# morfologicas legitimas que a regra de expressao completa nao reconhece.
excecoes_inclusao <- tribble(
  ~nome_planta_norm, ~termo_norm, ~opcao_norm, ~justificativa,
  "babosa aloe", "aloe", "tintura de aloes", "Plural de aloe; forma farmaceutica da mesma planta",
  "eucalipto", "eucalipto", "eucaliptol", "Eucaliptol; derivado diretamente associado ao eucalipto"
)

correspondencias <- correspondencias %>%
  left_join(
    excecoes_inclusao %>% mutate(excecao_inclusao = TRUE),
    by = c("nome_planta_norm", "termo_norm", "opcao_norm")
  ) %>%
  mutate(
    excecao_inclusao = coalesce(excecao_inclusao, FALSE),
    decisao = case_when(
      is.na(id_planta) ~ "excluido_termo_sem_planta",
      excecao_inclusao ~ "incluido_excecao",
      match_alias_completo ~ "incluido_alias_completo",
      TRUE ~ "excluido_sem_alias_completo"
    ),
    incluir = str_starts(decisao, "incluido"),
    motivo = case_when(
      decisao == "incluido_excecao" ~ justificativa,
      decisao == "incluido_alias_completo" ~ paste0("Opcao contem o nome completo: ", alias_encontrado),
      decisao == "excluido_termo_sem_planta" ~ "Termo do progresso nao encontrado no dicionario de plantas",
      TRUE ~ "Opcao nao contem nenhum nome completo da planta; coincidencia apenas parcial ou resultado alheio"
    )
  ) %>%
  left_join(
    auditoria_arquivos %>% select(id_arquivo, leitura_ok, n_linhas, erro_leitura),
    by = "id_arquivo"
  ) %>%
  select(
    id_arquivo, nome_arquivo, termo, opcao, id_planta, nome_planta,
    alias_encontrado, termo_aparece_completo, match_alias_completo,
    excecao_inclusao, decisao, incluir, motivo, leitura_ok, n_linhas, erro_leitura
  )

correspondencias_validas <- correspondencias %>%
  filter(incluir, leitura_ok, !is.na(id_planta)) %>%
  select(
    id_arquivo, id_planta, nome_planta, termo, opcao,
    alias_encontrado,
    decisao_correspondencia = decisao
  )

base_anvisa_validada <- base_anvisa_bruta %>%
  select(-termo, -opcao) %>%
  inner_join(
    correspondencias_validas,
    by = "id_arquivo",
    relationship = "many-to-many"
  ) %>%
  mutate(
    situacao = normalizar_situacao(situacao_da_regularizacao),
    registro_norm = normalizar_texto(numero_da_regularizacao),
    processo_norm = normalizar_texto(numero_do_processo),
    produto_norm = normalizar_texto(nome_do_produto),
    empresa_norm = normalizar_texto(empresa_detentora_da_regularizacao),
    principio_ativo_norm = normalizar_texto(
      principio_ativo_ou_descricao_do_medicamento_notificado
    ),
    chave_produto = case_when(
      !is.na(registro_norm) & registro_norm != "" ~ paste0("REG:", registro_norm),
      !is.na(processo_norm) & processo_norm != "" ~ paste0("PROC:", processo_norm),
      TRUE ~ paste(
        "FALLBACK", coalesce(produto_norm, ""), coalesce(empresa_norm, ""),
        coalesce(principio_ativo_norm, ""),
        sep = ":"
      )
    ),
    ano_processo = suppressWarnings(as.integer(
      str_match(numero_do_processo, "/([12][0-9]{3})")[, 2]
    )),
    fonte_termo_na_opcao = contem_expressao(opcao, termo)
  ) %>%
  relocate(id_planta, nome_planta, chave_produto, termo, opcao)

# Uma fonte canonica por par planta-produto. Isso evita que o mesmo registro,
# encontrado por sinonimos diferentes, seja contado varias vezes nos graficos.
fontes_por_produto <- base_anvisa_validada %>%
  group_by(id_planta, chave_produto) %>%
  summarise(
    n_arquivos_origem = n_distinct(id_arquivo),
    termos_origem = colar_unicos(termo),
    opcoes_origem = colar_unicos(opcao),
    .groups = "drop"
  )

base_produto_planta <- base_anvisa_validada %>%
  arrange(
    id_planta,
    chave_produto,
    desc(fonte_termo_na_opcao),
    nchar(opcao),
    opcao,
    termo,
    id_arquivo
  ) %>%
  distinct(id_planta, chave_produto, .keep_all = TRUE) %>%
  left_join(fontes_por_produto, by = c("id_planta", "chave_produto"))

resumo_plantas <- base_produto_planta %>%
  group_by(id_planta, nome_planta) %>%
  summarise(
    n_medicamentos = n_distinct(chave_produto),
    n_ativos = n_distinct(chave_produto[situacao == "Ativo"]),
    n_inativos = n_distinct(chave_produto[situacao == "Inativo"]),
    n_situacao_nao_informada = n_distinct(chave_produto[situacao == "Não informado"]),
    n_empresas = n_distinct(empresa_detentora_da_regularizacao, na.rm = TRUE),
    n_principios_ativos = n_distinct(
      principio_ativo_ou_descricao_do_medicamento_notificado,
      na.rm = TRUE
    ),
    n_termos_origem = n_distinct(termo),
    n_opcoes_origem = n_distinct(opcao),
    .groups = "drop"
  ) %>%
  right_join(
    plantas %>% select(id_planta, nome_planta),
    by = c("id_planta", "nome_planta")
  ) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0L))) %>%
  arrange(desc(n_medicamentos), nome_planta)

resumo_opcoes <- base_produto_planta %>%
  count(id_planta, nome_planta, opcao, name = "n_medicamentos", sort = TRUE)

resumo_empresas <- base_produto_planta %>%
  filter(
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  group_by(empresa = empresa_detentora_da_regularizacao) %>%
  summarise(
    n_medicamentos = n_distinct(chave_produto),
    n_plantas = n_distinct(id_planta),
    .groups = "drop"
  ) %>%
  arrange(desc(n_medicamentos), empresa)

# Base para os mapas: somente medicamentos ativos, resumidos por empresa.
empresas_medicamentos_ativos <- base_produto_planta %>%
  filter(
    situacao == "Ativo",
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  group_by(empresa = empresa_detentora_da_regularizacao) %>%
  summarise(
    n_medicamentos_ativos = n_distinct(chave_produto),
    n_plantas = n_distinct(id_planta),
    plantas = colar_unicos(nome_planta),
    .groups = "drop"
  ) %>%
  arrange(desc(n_medicamentos_ativos), empresa)

resumo_principios_ativos <- base_produto_planta %>%
  filter(
    !is.na(principio_ativo_ou_descricao_do_medicamento_notificado),
    str_squish(principio_ativo_ou_descricao_do_medicamento_notificado) != ""
  ) %>%
  group_by(principio_ativo = principio_ativo_ou_descricao_do_medicamento_notificado) %>%
  summarise(
    n_medicamentos = n_distinct(chave_produto),
    n_plantas = n_distinct(id_planta),
    .groups = "drop"
  ) %>%
  arrange(desc(n_medicamentos), principio_ativo)

serie_ano <- base_produto_planta %>%
  filter(!is.na(ano_processo), between(ano_processo, 1900L, as.integer(format(Sys.Date(), "%Y")))) %>%
  count(ano_processo, name = "n_medicamentos") %>%
  arrange(ano_processo)

resumo_status_progresso <- progresso %>%
  count(status, name = "n_registros", sort = TRUE)

auditoria_excluidos <- correspondencias %>%
  filter(!incluir) %>%
  count(termo, opcao, decisao, motivo, name = "n_correspondencias", sort = TRUE)

indicadores <- tibble(
  indicador = c(
    "Plantas na base de referencia",
    "Termos distintos registrados no progresso",
    "Arquivos de resultado no progresso",
    "Arquivos encontrados",
    "Arquivos lidos sem erro",
    "Arquivos com ao menos uma linha",
    "Correspondencias planta-arquivo incluidas",
    "Correspondencias planta-arquivo excluidas",
    "Linhas brutas importadas",
    "Linhas apos validacao das correspondencias",
    "Pares unicos planta-medicamento",
    "Medicamentos unicos",
    "Plantas com ao menos um medicamento",
    "Empresas distintas"
  ),
  valor = c(
    n_distinct(plantas$id_planta),
    n_distinct(progresso$termo[!is.na(progresso$termo)]),
    n_distinct(arquivos_progresso$id_arquivo),
    sum(auditoria_arquivos$arquivo_encontrado),
    sum(auditoria_arquivos$leitura_ok),
    sum(auditoria_arquivos$leitura_ok & auditoria_arquivos$n_linhas > 0, na.rm = TRUE),
    sum(correspondencias$incluir, na.rm = TRUE),
    sum(!correspondencias$incluir, na.rm = TRUE),
    nrow(base_anvisa_bruta),
    nrow(base_anvisa_validada),
    nrow(base_produto_planta),
    n_distinct(base_produto_planta$chave_produto),
    n_distinct(base_produto_planta$id_planta),
    n_distinct(base_produto_planta$empresa_norm[base_produto_planta$empresa_norm != ""], na.rm = TRUE)
  )
)

# Coeficiente de Gini para a distribuicao de medicamentos entre as plantas.
gini <- function(x) {
  x <- sort(as.numeric(x[is.finite(x) & x >= 0]))
  if (length(x) == 0 || sum(x) == 0) {
    return(NA_real_)
  }
  n <- length(x)
  sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
}

indicadores <- bind_rows(
  indicadores,
  tibble(
    indicador = "Gini da distribuicao de medicamentos entre plantas",
    valor = gini(resumo_plantas$n_medicamentos)
  )
)

dir.create(dir_mapas_data, recursive = TRUE, showWarnings = FALSE)

salvar_csv(base_anvisa_bruta, file.path(dir_bases, "base_anvisa_bruta.csv"))
salvar_csv(base_anvisa_validada, file.path(dir_bases, "base_anvisa_validada.csv"))
salvar_csv(base_produto_planta, file.path(dir_bases, "base_produto_planta.csv"))
salvar_csv(auditoria_arquivos, file.path(dir_tabelas, "auditoria_arquivos.csv"))
salvar_csv(correspondencias, file.path(dir_tabelas, "auditoria_correspondencias.csv"))
salvar_csv(auditoria_excluidos, file.path(dir_tabelas, "correspondencias_excluidas.csv"))
salvar_csv(indicadores, file.path(dir_tabelas, "indicadores_gerais.csv"))
salvar_csv(resumo_plantas, file.path(dir_tabelas, "resumo_plantas.csv"))
salvar_csv(resumo_opcoes, file.path(dir_tabelas, "resumo_opcoes_por_planta.csv"))
salvar_csv(resumo_empresas, file.path(dir_tabelas, "resumo_empresas.csv"))
salvar_csv(
  empresas_medicamentos_ativos,
  file.path(dir_tabelas, "empresas_medicamentos_ativos.csv")
)
salvar_csv(
  empresas_medicamentos_ativos,
  file.path(dir_mapas_data, "medicamentos_ativos_empresas.csv")
)
salvar_csv(resumo_principios_ativos, file.path(dir_tabelas, "resumo_principios_ativos.csv"))
salvar_csv(serie_ano, file.path(dir_tabelas, "serie_ano_processo.csv"))
salvar_csv(resumo_status_progresso, file.path(dir_tabelas, "resumo_status_progresso.csv"))

# Tabelas LaTeX usadas no texto.
base_tex <- base_produto_planta %>%
  mutate(
    planta_tex = nome_curto_planta(nome_planta),
    empresa_tex = str_squish(str_remove(
      empresa_detentora_da_regularizacao,
      "\\s*-\\s*[0-9./-]+\\s*$"
    ))
  )

produtos_tex <- base_tex %>%
  group_by(chave_produto) %>%
  summarise(
    situacao = primeiro_nao_vazio(situacao),
    n_plantas = n_distinct(id_planta),
    .groups = "drop"
  )

tab_kpis_produtos <- tibble(
  Indicador = c(
    "Pares produto-planta (linhas)",
    "Produtos (registros) únicos",
    "Produtos ativos",
    "Produtos inativos",
    "Plantas com >=1 medicamento (cobertura)",
    "Plantas na base botânica de referência",
    "Empresas detentoras distintas",
    "Substâncias (termos encontrados) distintas"
  ),
  Valor = c(
    nrow(base_tex),
    nrow(produtos_tex),
    sum(produtos_tex$situacao == "Ativo"),
    sum(produtos_tex$situacao == "Inativo"),
    n_distinct(base_tex$id_planta),
    n_distinct(plantas$id_planta),
    n_distinct(base_tex$empresa_tex[base_tex$empresa_tex != ""], na.rm = TRUE),
    n_distinct(base_tex$opcao, na.rm = TRUE)
  )
)

salvar_tex(
  tab_kpis_produtos,
  "tab_kpis_produtos.tex",
  "Indicadores gerais da base de produtos (rota princípio ativo / medicamentos).",
  "tab:kpis-produtos",
  "lr",
  digitos = 0
)

tab_top_plantas <- resumo_plantas %>%
  filter(n_medicamentos > 0) %>%
  slice_head(n = 20) %>%
  transmute(
    Planta = nome_curto_planta(nome_planta),
    Produtos = n_medicamentos,
    Ativos = n_ativos,
    Inativos = n_inativos,
    Empresas = n_empresas
  )

salvar_tex(
  tab_top_plantas,
  "tab_top_plantas.tex",
  "Vinte plantas com maior número de medicamentos associados por princípio ativo.",
  "tab:top-plantas",
  "lrrrr",
  digitos = 0
)

ambiguidades_termos <- correspondencias_validas %>%
  mutate(
    opcao_norm = normalizar_texto(opcao),
    planta = nome_curto_planta(nome_planta)
  ) %>%
  filter(!is.na(opcao_norm), opcao_norm != "") %>%
  distinct(opcao_norm, opcao, id_planta, planta) %>%
  group_by(opcao_norm) %>%
  filter(n_distinct(id_planta) > 1) %>%
  summarise(
    `Termo encontrado` = primeiro_nao_vazio(opcao),
    `Plantas associadas` = paste(sort(unique(planta)), collapse = "; "),
    `N plantas` = n_distinct(id_planta),
    .groups = "drop"
  )

tab_ambiguidades <- ambiguidades_termos %>%
  group_by(`Plantas associadas`, `N plantas`) %>%
  summarise(
    `N termos ambíguos` = n_distinct(opcao_norm),
    `Termos encontrados` = paste(
      sort(unique(`Termo encontrado`)),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(`N termos ambíguos`),
    desc(`N plantas`),
    `Plantas associadas`
  )

salvar_tex(
  tab_ambiguidades,
  "tab_ambiguidades.tex",
  "Ambiguidades entre plantas: grupos que compartilham termos encontrados.",
  "tab:ambiguidades",
  "lrrl",
  digitos = 0,
  larguras = c("4cm", NA, NA, "7cm")
)

tab_eficiencia <- resumo_plantas %>%
  filter(n_medicamentos > 0) %>%
  slice_head(n = 20) %>%
  transmute(
    Planta = nome_curto_planta(nome_planta),
    Produtos = n_medicamentos,
    Substâncias = n_opcoes_origem,
    `Termos de busca` = n_termos_origem,
    `Prod./subst.` = round(n_medicamentos / n_opcoes_origem, 2),
    `Prod./termo` = round(n_medicamentos / n_termos_origem, 2)
  )

salvar_tex(
  tab_eficiencia,
  "tab_eficiencia_casamento.tex",
  paste(
    "Eficiência do casamento por planta: produtos recuperados por",
    "substância e por termo de busca (top 20 em volume)."
  ),
  "tab:eficiencia-casamento",
  "lrrrrr"
)

tab_exclusivo_compartilhado <- base_tex %>%
  distinct(chave_produto, id_planta, planta_tex) %>%
  left_join(
    produtos_tex %>% select(chave_produto, n_plantas),
    by = "chave_produto"
  ) %>%
  mutate(tipo = if_else(n_plantas >= 2, "Compartilhado", "Exclusivo")) %>%
  count(planta_tex, tipo, name = "n") %>%
  pivot_wider(names_from = tipo, values_from = n, values_fill = 0) %>%
  mutate(
    Total = Exclusivo + Compartilhado,
    `% compartilhado` = round(100 * Compartilhado / Total, 1)
  ) %>%
  arrange(desc(Total), planta_tex) %>%
  slice_head(n = 20) %>%
  transmute(
    Planta = planta_tex,
    Exclusivo,
    Compartilhado,
    Total,
    `% compartilhado`
  )

salvar_tex(
  tab_exclusivo_compartilhado,
  "tab_exclusivo_compartilhado.tex",
  paste(
    "Medicamentos por planta segundo composição: ingrediente exclusivo",
    "vs. fórmulas multi-ingrediente (top 20 em volume)."
  ),
  "tab:exclusivo-compartilhado",
  "lrrrr"
)

tab_multi_ingrediente <- base_tex %>%
  filter(!is.na(produto_norm), produto_norm != "") %>%
  group_by(produto_norm) %>%
  summarise(
    Produto = primeiro_nao_vazio(nome_do_produto),
    `N plantas` = n_distinct(id_planta),
    Plantas = paste(sort(unique(planta_tex)), collapse = " | "),
    .groups = "drop"
  ) %>%
  filter(`N plantas` >= 2) %>%
  arrange(desc(`N plantas`), Produto) %>%
  slice_head(n = 15) %>%
  select(Produto, `N plantas`, Plantas)

salvar_tex(
  tab_multi_ingrediente,
  "tab_multi_ingrediente.tex",
  "Produtos multi-ingrediente com maior número de plantas associadas.",
  "tab:multi-ingrediente",
  "lll",
  digitos = 0,
  larguras = c("3.2cm", NA, "9cm")
)

tab_top_empresas <- base_tex %>%
  filter(!is.na(empresa_tex), empresa_tex != "") %>%
  group_by(Empresa = empresa_tex) %>%
  summarise(
    Produtos = n_distinct(chave_produto),
    Ativos = n_distinct(chave_produto[situacao == "Ativo"]),
    `Plantas distintas` = n_distinct(id_planta),
    .groups = "drop"
  ) %>%
  arrange(desc(Produtos), Empresa) %>%
  slice_head(n = 15)

salvar_tex(
  tab_top_empresas,
  "tab_top_empresas.tex",
  "Quinze empresas com maior número de medicamentos registrados.",
  "tab:top-empresas",
  "lrrr",
  digitos = 0,
  larguras = c("7cm", NA, NA, NA)
)

writexl::write_xlsx(
  list(
    indicadores = indicadores,
    plantas = resumo_plantas,
    opcoes_por_planta = resumo_opcoes,
    empresas = resumo_empresas,
    principios_ativos = resumo_principios_ativos,
    auditoria_arquivos = auditoria_arquivos,
    auditoria_matches = correspondencias,
    excluidos = auditoria_excluidos
  ),
  file.path(dir_saida, "Resultados_ANVISA.xlsx")
)

source("graficos_medicamentos.R")

cat("\nConstrucao concluida.\n")
cat("Saida: ", dir_saida, "\n", sep = "")
cat("Arquivos de resultado considerados: ", nrow(arquivos_progresso), "\n", sep = "")
cat("Arquivos lidos sem erro: ", sum(auditoria_arquivos$leitura_ok), "\n", sep = "")
cat("Correspondencias incluidas: ", sum(correspondencias$incluir, na.rm = TRUE), "\n", sep = "")
cat("Correspondencias excluidas: ", sum(!correspondencias$incluir, na.rm = TRUE), "\n", sep = "")
cat("Pares planta-medicamento: ", nrow(base_produto_planta), "\n", sep = "")
cat("Medicamentos unicos: ", n_distinct(base_produto_planta$chave_produto), "\n", sep = "")
cat("Plantas com medicamento: ", n_distinct(base_produto_planta$id_planta), "\n", sep = "")
