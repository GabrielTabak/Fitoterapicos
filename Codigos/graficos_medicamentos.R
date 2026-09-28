# Chamado por analisar_medicamentos.R, usando os dados já consolidados.

# 1. Plantas com mais medicamentos, por situacao do registro.
top_plantas <- resumo_plantas %>%
  filter(n_medicamentos > 0) %>%
  slice_max(n_medicamentos, n = 15, with_ties = FALSE)

top_situacao <- base_produto_planta %>%
  semi_join(top_plantas, by = c("id_planta", "nome_planta")) %>%
  count(id_planta, nome_planta, situacao, name = "n_medicamentos") %>%
  mutate(
    nome_planta_exibicao = nome_curto_planta(nome_planta),
    nome_planta_exibicao = fct_reorder(
      nome_planta_exibicao, n_medicamentos,
      .fun = sum
    )
  )

totais_top_situacao <- top_situacao %>%
  group_by(nome_planta_exibicao) %>%
  summarise(total = sum(n_medicamentos), .groups = "drop")

g_top_situacao <- ggplot(
  top_situacao,
  aes(nome_planta_exibicao, n_medicamentos, fill = situacao)
) +
  geom_col() +
  geom_text(
    data = totais_top_situacao,
    aes(nome_planta_exibicao, total, label = total),
    inherit.aes = FALSE,
    hjust = -0.35,
    size = 3,
    family = "serif",
    color = "#222222"
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_situacao, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Plantas com mais medicamentos associados",
    subtitle = "Produtos únicos por planta, deduplicados por registro ou processo",
    x = NULL, y = "Número de medicamentos", fill = "Situação"
  ) +
  tema_final +
  theme(panel.grid.major.y = element_blank())

salvar_figura(g_top_situacao, "01_top_plantas_situacao", 10, 7)

# Opções de princípio ativo nos medicamentos ativos.
resumo_opcoes_ativos <- base_produto_planta %>%
  filter(situacao == "Ativo") %>%
  count(id_planta, nome_planta, opcao, name = "n_medicamentos", sort = TRUE)

top_ids_ativos <- resumo_opcoes_ativos %>%
  group_by(id_planta) %>%
  summarise(total_planta = sum(n_medicamentos), .groups = "drop") %>%
  slice_max(total_planta, n = 10, with_ties = FALSE) %>%
  pull(id_planta)

opcoes_top_ativos <- resumo_opcoes_ativos %>%
  filter(id_planta %in% top_ids_ativos) %>%
  mutate(nome_planta_exibicao = nome_curto_planta(nome_planta)) %>%
  group_by(id_planta, nome_planta, nome_planta_exibicao) %>%
  arrange(desc(n_medicamentos), opcao, .by_group = TRUE) %>%
  mutate(
    total_planta = sum(n_medicamentos),
    ordem_opcao = row_number(),
    ordem_cor = factor(pmin(ordem_opcao, 6L), levels = 1:6),
    opcao_exibida = str_trunc(str_to_sentence(opcao), width = 27),
    cabe_rotulo =
      n_medicamentos >= pmax(5, ceiling(nchar(opcao_exibida) * 0.38)) |
        (
          nome_planta_exibicao %in% c("Garra-do-diabo", "Ginseng", "Cardo-mariano") &
            ordem_opcao == 1
        ),
    rotulo_segmento = if_else(
      cabe_rotulo,
      paste0(opcao_exibida, " (", n_medicamentos, ")"),
      ""
    ),
    cor_rotulo = unname(cores_rotulos_opcoes_ativos[as.character(ordem_cor)]),
    posicao_rotulo = cumsum(n_medicamentos) - n_medicamentos / 2
  ) %>%
  ungroup() %>%
  mutate(nome_planta_exibicao = fct_reorder(nome_planta_exibicao, total_planta))

totais_opcoes_top_ativos <- opcoes_top_ativos %>%
  distinct(id_planta, nome_planta_exibicao, total_planta)

g_opcoes_ativos <- ggplot(
  opcoes_top_ativos,
  aes(nome_planta_exibicao, n_medicamentos, fill = ordem_cor)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.68,
    color = "white",
    linewidth = 0.25,
    show.legend = FALSE
  ) +
  geom_text(
    aes(y = posicao_rotulo, label = rotulo_segmento, color = cor_rotulo),
    family = "serif",
    size = 2.45,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  geom_text(
    data = totais_opcoes_top_ativos,
    aes(nome_planta_exibicao, total_planta, label = total_planta),
    inherit.aes = FALSE,
    hjust = -0.35,
    family = "serif",
    size = 3,
    color = "#202020"
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_opcoes_ativos, drop = FALSE) +
  scale_color_identity() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Composição dos medicamentos ativos por opção selecionada",
    subtitle = "Cada barra representa uma planta; os segmentos correspondem às opções encontradas",
    caption = paste(
      "Nota: o total aparece à direita de cada barra.",
      "Opções pequenas permanecem como segmentos, mas são omitidas dos rótulos para preservar a legibilidade."
    ),
    x = NULL,
    y = "Número de medicamentos ativos"
  ) +
  tema_final +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 9)
  )

salvar_figura(
  g_opcoes_ativos,
  "02b_top_plantas_por_opcao_clicada_ativos",
  13,
  7.5
)


# 4. Distribuicao de medicamentos ativos por planta.
distribuicao_faixas <- resumo_plantas %>%
  filter(n_ativos > 0) %>%
  mutate(
    limite_inferior = floor((n_ativos - 1) / 5) * 5 + 1,
    faixa_medicamentos = paste0(limite_inferior, "-", limite_inferior + 4)
  ) %>%
  count(limite_inferior, faixa_medicamentos, name = "n_plantas") %>%
  arrange(limite_inferior) %>%
  mutate(
    percentual_plantas = n_plantas / sum(n_plantas),
    rotulo = paste0(
      n_plantas,
      " (",
      percent(percentual_plantas, accuracy = 0.1, decimal.mark = ","),
      ")"
    ),
    faixa_medicamentos = factor(
      faixa_medicamentos,
      levels = rev(faixa_medicamentos)
    ),
    faixa_mais_frequente = n_plantas == max(n_plantas)
  )

g_distribuicao <- ggplot(
  distribuicao_faixas,
  aes(faixa_medicamentos, n_plantas, fill = faixa_mais_frequente)
) +
  geom_col(
    width = 0.72, color = "white", linewidth = 0.3, show.legend = FALSE
  ) +
  geom_text(
    aes(label = rotulo),
    hjust = -0.18,
    size = 3,
    family = "serif",
    color = "#202020"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#526A61", "FALSE" = "#84939D")) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title = "Plantas por faixa de medicamentos ativos associados",
    subtitle = "Os rótulos mostram o número e o percentual de plantas em cada faixa",
    caption = paste(
      "Nota: são consideradas apenas plantas com ao menos um medicamento ativo.",
      "Faixas sem nenhuma planta não são exibidas."
    ),
    x = "Medicamentos ativos associados a cada planta",
    y = "Número de plantas"
  ) +
  tema_final +
  theme(panel.grid.major.y = element_blank())

salvar_figura(g_distribuicao, "04_distribuicao_medicamentos_por_planta", 10, 7)

# 5b. Empresas com mais medicamentos ativos associados as plantas.
resumo_empresas_ativos <- empresas_medicamentos_ativos %>%
  transmute(
    empresa,
    n_medicamentos = n_medicamentos_ativos,
    n_plantas
  )

# 5c. Composicao das empresas segundo as plantas associadas.
associacoes_empresa_planta_ativos <- base_produto_planta %>%
  filter(
    situacao == "Ativo",
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  distinct(
    empresa = empresa_detentora_da_regularizacao,
    id_planta,
    nome_planta,
    chave_produto
  ) %>%
  count(empresa, id_planta, nome_planta, name = "n_associacoes")

empresas_destaque_05c <- resumo_empresas_ativos %>%
  slice_max(n_medicamentos, n = 12, with_ties = FALSE) %>%
  pull(empresa)

plantas_destaque_05c <- associacoes_empresa_planta_ativos %>%
  filter(empresa %in% empresas_destaque_05c) %>%
  group_by(id_planta, nome_planta) %>%
  summarise(n_associacoes = sum(n_associacoes), .groups = "drop") %>%
  slice_max(n_associacoes, n = 8, with_ties = FALSE) %>%
  arrange(desc(n_associacoes), nome_planta) %>%
  mutate(planta_exibicao = str_trunc(nome_curto_planta(nome_planta), 24))

niveis_plantas_05c <- c(
  plantas_destaque_05c$planta_exibicao,
  "Outras plantas"
)

cores_plantas_05c <- setNames(
  c(
    grDevices::hcl.colors(
      nrow(plantas_destaque_05c),
      palette = "Dark 3"
    ),
    "#C9CED1"
  ),
  niveis_plantas_05c
)

dados_empresas_plantas_05c <- associacoes_empresa_planta_ativos %>%
  filter(empresa %in% empresas_destaque_05c) %>%
  left_join(
    plantas_destaque_05c %>%
      select(id_planta, planta_destaque = planta_exibicao),
    by = "id_planta"
  ) %>%
  mutate(
    planta_exibicao = coalesce(planta_destaque, "Outras plantas"),
    planta_exibicao = factor(
      planta_exibicao,
      levels = niveis_plantas_05c
    )
  ) %>%
  group_by(empresa, planta_exibicao) %>%
  summarise(n_associacoes = sum(n_associacoes), .groups = "drop") %>%
  group_by(empresa) %>%
  mutate(total_associacoes = sum(n_associacoes)) %>%
  ungroup() %>%
  mutate(
    empresa_exibicao = str_squish(str_remove(
      empresa,
      "\\s*-\\s*[0-9./-]+\\s*$"
    )),
    empresa_exibicao = str_trunc(empresa_exibicao, 43),
    empresa_exibicao = fct_reorder(
      empresa_exibicao,
      total_associacoes
    ),
    rotulo_segmento = if_else(
      n_associacoes >= 2,
      as.character(n_associacoes),
      ""
    )
  )

totais_empresas_plantas_05c <- dados_empresas_plantas_05c %>%
  distinct(empresa, empresa_exibicao, total_associacoes)

g_empresas_plantas_ativos <- ggplot(
  dados_empresas_plantas_05c,
  aes(empresa_exibicao, n_associacoes, fill = planta_exibicao)
) +
  geom_col(
    width = 0.7,
    color = "white",
    linewidth = 0.25
  ) +
  geom_text(
    aes(label = rotulo_segmento),
    position = position_stack(vjust = 0.5),
    size = 2.6,
    family = "serif",
    color = "#202020"
  ) +
  geom_text(
    data = totais_empresas_plantas_05c,
    aes(
      empresa_exibicao,
      total_associacoes,
      label = total_associacoes
    ),
    inherit.aes = FALSE,
    hjust = -0.25,
    size = 3,
    family = "serif",
    color = "#202020"
  ) +
  coord_flip() +
  scale_fill_manual(values = cores_plantas_05c, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Composição das empresas por plantas associadas",
    subtitle = "Medicamentos ativos distintos em cada par empresa-planta",
    caption = paste(
      "Nota: o total representa associações planta-medicamento ativo.",
      "Um medicamento ligado a mais de uma planta contribui para mais de um segmento;",
      "segmentos com uma associação não recebem rótulo."
    ),
    x = NULL,
    y = "Associações planta-medicamento ativo",
    fill = "Planta"
  ) +
  tema_final +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    legend.key.width = grid::unit(0.8, "cm")
  ) +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))

salvar_figura(
  g_empresas_plantas_ativos,
  "05c_empresas_por_planta_ativos",
  13,
  8
)

# 6. Evolucao dos processos, quando o ano esta disponivel.
if (nrow(serie_ano) > 0) {
  g_ano <- ggplot(serie_ano, aes(ano_processo, n_medicamentos)) +
    geom_col(fill = "#5E7169", width = 0.78) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(
      title = "Medicamentos associados por ano do processo",
      subtitle = "Ano extraído do número do processo informado pela ANVISA",
      x = "Ano", y = "Número de medicamentos"
    ) +
    tema_final
  salvar_figura(g_ano, "06_medicamentos_por_ano_processo", 10, 6)
}

# Base comum das figuras relacionais: um registro por par planta-medicamento.
base_ativos <- base_produto_planta %>%
  filter(situacao == "Ativo")

# 8. Heatmap das conexoes entre empresas e plantas.
top_empresas_heatmap <- base_ativos %>%
  filter(
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  group_by(empresa = empresa_detentora_da_regularizacao) %>%
  summarise(
    n_medicamentos = n_distinct(chave_produto),
    .groups = "drop"
  ) %>%
  arrange(desc(n_medicamentos), empresa) %>%
  slice_head(n = 20) %>%
  mutate(
    empresa_exibicao = str_squish(str_remove(
      empresa,
      "\\s*-\\s*[0-9./-]+\\s*$"
    )),
    empresa_exibicao = str_trunc(empresa_exibicao, 34)
  )

top_plantas_heatmap <- base_ativos %>%
  group_by(id_planta, nome_planta) %>%
  summarise(
    n_medicamentos = n_distinct(chave_produto),
    .groups = "drop"
  ) %>%
  arrange(desc(n_medicamentos), nome_planta) %>%
  slice_head(n = 20) %>%
  mutate(
    planta_exibicao = str_trunc(
      nome_curto_planta(nome_planta),
      24
    )
  )

contagens_heatmap <- base_ativos %>%
  filter(
    empresa_detentora_da_regularizacao %in%
      top_empresas_heatmap$empresa,
    id_planta %in% top_plantas_heatmap$id_planta
  ) %>%
  distinct(
    empresa = empresa_detentora_da_regularizacao,
    id_planta,
    chave_produto
  ) %>%
  count(empresa, id_planta, name = "n_medicamentos")

dados_heatmap <- expand_grid(
  empresa = top_empresas_heatmap$empresa,
  id_planta = top_plantas_heatmap$id_planta
) %>%
  left_join(contagens_heatmap, by = c("empresa", "id_planta")) %>%
  mutate(n_medicamentos = replace_na(n_medicamentos, 0L)) %>%
  left_join(
    top_empresas_heatmap %>%
      select(empresa, empresa_exibicao),
    by = "empresa"
  ) %>%
  left_join(
    top_plantas_heatmap %>%
      select(id_planta, planta_exibicao),
    by = "id_planta"
  ) %>%
  mutate(
    empresa_exibicao = factor(
      empresa_exibicao,
      levels = rev(top_empresas_heatmap$empresa_exibicao)
    ),
    planta_exibicao = factor(
      planta_exibicao,
      levels = top_plantas_heatmap$planta_exibicao
    ),
    rotulo = if_else(
      n_medicamentos > 0,
      as.character(n_medicamentos),
      ""
    ),
    cor_rotulo = if_else(n_medicamentos >= 3, "white", "#202020")
  )

g_heatmap_empresas_plantas <- ggplot(
  dados_heatmap,
  aes(planta_exibicao, empresa_exibicao, fill = n_medicamentos)
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = rotulo, color = cor_rotulo),
    size = 2.8,
    family = "serif",
    show.legend = FALSE
  ) +
  scale_fill_gradient(
    low = "#F1F4F3",
    high = "#3F6258",
    breaks = pretty_breaks(n = 5),
    name = "Medicamentos\nativos"
  ) +
  scale_color_identity() +
  labs(
    title = "Medicamentos ativos por empresa e planta",
    subtitle = "Número de medicamentos distintos em cada combinação entre as 20 principais empresas e plantas",
    caption = "Células sem medicamentos ativos são exibidas sem número.",
    x = "Planta",
    y = "Empresa"
  ) +
  tema_final +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 8
    ),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

salvar_figura(
  g_heatmap_empresas_plantas,
  "08_heatmap_empresas_plantas_ativos",
  15,
  11
)

# 10. Rede de coocorrencia de plantas em medicamentos ativos.
plantas_compartilhadas_por_medicamento <- base_ativos %>%
  distinct(chave_produto, id_planta) %>%
  group_by(chave_produto) %>%
  summarise(
    ids_plantas = list(sort(unique(id_planta))),
    .groups = "drop"
  ) %>%
  filter(lengths(ids_plantas) >= 2)

pares_plantas_ativos <- map_dfr(
  plantas_compartilhadas_por_medicamento$ids_plantas,
  function(ids) {
    as_tibble(
      t(combn(ids, 2)),
      .name_repair = ~ c("planta_a", "planta_b")
    )
  }
) %>%
  count(planta_a, planta_b, name = "peso", sort = TRUE)

pares_rede <- pares_plantas_ativos %>%
  filter(peso >= 2)

if (
  nrow(pares_rede) > 0 &&
    requireNamespace("igraph", quietly = TRUE) &&
    requireNamespace("ggraph", quietly = TRUE)
) {
  ids_rede <- unique(c(pares_rede$planta_a, pares_rede$planta_b))

  nos_rede <- base_ativos %>%
    distinct(id_planta, nome_planta) %>%
    filter(id_planta %in% ids_rede) %>%
    transmute(
      name = as.character(id_planta),
      rotulo = str_trunc(nome_curto_planta(nome_planta), 24)
    )

  grafo_rede <- igraph::graph_from_data_frame(
    pares_rede %>%
      transmute(
        from = as.character(planta_a),
        to = as.character(planta_b),
        peso
      ),
    directed = FALSE,
    vertices = nos_rede
  )

  grafo_rede <- igraph::set_vertex_attr(
    grafo_rede,
    name = "forca",
    value = igraph::strength(
      grafo_rede,
      weights = igraph::edge_attr(grafo_rede, "peso")
    )
  )

  set.seed(42)
  layout_rede <- igraph::layout_components(
    grafo_rede,
    layout = igraph::layout_with_fr
  )

  g_rede_plantas_ativos <- ggraph::ggraph(
    grafo_rede,
    layout = "manual",
    x = layout_rede[, 1],
    y = layout_rede[, 2]
  ) +
    ggraph::geom_edge_link(
      aes(edge_width = peso),
      color = "#A6ADB1",
      alpha = 0.55
    ) +
    ggraph::geom_node_point(
      aes(size = forca),
      color = "#405F57",
      alpha = 0.95
    ) +
    ggraph::geom_node_text(
      aes(label = rotulo),
      repel = TRUE,
      size = 3,
      family = "serif",
      color = "#202020",
      box.padding = 0.4,
      point.padding = 0.3
    ) +
    ggraph::scale_edge_width(
      range = c(0.5, 3),
      name = "Medicamentos\ncompartilhados"
    ) +
    scale_size_continuous(range = c(3, 10), guide = "none") +
    labs(
      title = "Rede de plantas conectadas por medicamentos ativos",
      subtitle = "Cada ligação indica pelo menos dois medicamentos ativos compartilhados",
      caption = paste(
        "A espessura da ligação representa o número de medicamentos compartilhados;",
        "o tamanho do nó representa a intensidade total das conexões."
      )
    ) +
    theme_void(base_family = "serif", base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 12,
        color = "#202020"
      ),
      plot.subtitle = element_text(
        size = 10,
        color = "#4A4A4A",
        margin = margin(b = 8)
      ),
      plot.caption = element_text(
        size = 8,
        color = "#555555",
        hjust = 0,
        margin = margin(t = 8)
      ),
      legend.position = "bottom",
      plot.margin = margin(10, 14, 8, 10)
    )

  salvar_figura(
    g_rede_plantas_ativos,
    "10_rede_coocorrencia_plantas_ativos",
    11,
    9
  )
} else {
  warning(
    "Figura 10 nao gerada: rede vazia ou pacotes igraph/ggraph indisponiveis."
  )
}

# 12. Ano de vencimento informado para medicamentos ativos.
vencimentos_ativos <- base_ativos %>%
  distinct(chave_produto, .keep_all = TRUE) %>%
  mutate(
    ano_vencimento = suppressWarnings(as.integer(str_extract(
      vencimento_da_regularizacao,
      "[12][0-9]{3}"
    ))),
    ano_exibicao = if_else(
      is.na(ano_vencimento),
      "Não informado",
      as.character(ano_vencimento)
    )
  ) %>%
  count(ano_vencimento, ano_exibicao, name = "n_medicamentos") %>%
  arrange(is.na(ano_vencimento), ano_vencimento) %>%
  mutate(
    ano_exibicao = factor(
      ano_exibicao,
      levels = ano_exibicao
    )
  )

g_vencimentos_ativos <- ggplot(
  vencimentos_ativos,
  aes(ano_exibicao, n_medicamentos)
) +
  geom_col(fill = "#65786F", width = 0.72) +
  geom_text(
    aes(label = n_medicamentos),
    vjust = -0.25,
    size = 3,
    family = "serif",
    color = "#202020"
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Ano de vencimento dos medicamentos ativos",
    subtitle = "Medicamentos únicos segundo o vencimento da regularização informado pela ANVISA",
    caption = "A situação regulatória e o vencimento correspondem aos campos disponíveis na base consultada.",
    x = "Ano de vencimento",
    y = "Número de medicamentos ativos únicos"
  ) +
  tema_final +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

salvar_figura(
  g_vencimentos_ativos,
  "12_vencimentos_medicamentos_ativos",
  11,
  6
)

# 13. Proporcao de medicamentos ativos dentro do total de cada planta.
proporcao_ativos_por_planta <- resumo_plantas %>%
  filter(n_medicamentos >= 5) %>%
  mutate(
    proporcao_ativos = n_ativos / n_medicamentos,
    nome_planta_exibicao = nome_curto_planta(nome_planta),
    nome_planta_exibicao = fct_reorder(
      nome_planta_exibicao,
      proporcao_ativos
    ),
    rotulo_proporcao = percent(
      proporcao_ativos,
      accuracy = 0.1,
      decimal.mark = ","
    )
  )

g_proporcao_ativos_por_planta <- ggplot(
  proporcao_ativos_por_planta,
  aes(proporcao_ativos, nome_planta_exibicao)
) +
  geom_segment(
    aes(
      x = 0,
      xend = proporcao_ativos,
      yend = nome_planta_exibicao
    ),
    color = "#C4C8C6",
    linewidth = 0.45
  ) +
  geom_point(
    aes(size = n_medicamentos),
    color = "#526A61",
    alpha = 0.9
  ) +
  geom_text(
    aes(label = rotulo_proporcao),
    hjust = -0.3,
    size = 2.5,
    family = "serif",
    color = "#202020"
  ) +
  scale_x_continuous(
    labels = percent_format(
      accuracy = 1,
      decimal.mark = ","
    ),
    breaks = seq(0, 0.7, by = 0.1),
    limits = c(0, 0.7),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_size_continuous(
    range = c(2.2, 7),
    breaks = pretty_breaks(n = 4),
    name = "Medicamentos\ntotais"
  ) +
  labs(
    title = "Proporção de medicamentos ativos por planta",
    subtitle = "Plantas com pelo menos cinco medicamentos; o tamanho do ponto representa o volume total",
    caption = paste(
      "A proporção corresponde aos medicamentos ativos únicos divididos pelo total",
      "de medicamentos únicos associados a cada planta."
    ),
    x = "Proporção de medicamentos ativos",
    y = NULL
  ) +
  tema_final +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 8),
    legend.position = "right"
  )

salvar_figura(
  g_proporcao_ativos_por_planta,
  "13_proporcao_medicamentos_ativos_por_planta",
  11.5,
  19
)

# 14 e 15. Pizzas resumidas das associacoes por planta.
# Todas as plantas permanecem representadas; as dez maiores recebem fatias
# proprias e as demais sao reunidas em "Outras".
preparar_dados_pizza <- function(dados, n_destaques = 10L) {
  dados_ordenados <- dados %>%
    arrange(desc(n), nome_planta)

  ids_destaque <- dados_ordenados %>%
    slice_head(n = n_destaques) %>%
    pull(id_planta)

  dados_ordenados %>%
    mutate(
      destaque = id_planta %in% ids_destaque,
      categoria = if_else(
        destaque,
        nome_curto_planta(nome_planta),
        "Outras"
      ),
      ordem = if_else(
        destaque,
        match(id_planta, ids_destaque),
        n_destaques + 1L
      )
    ) %>%
    group_by(categoria, ordem) %>%
    summarise(
      n = sum(n),
      n_plantas = n_distinct(id_planta),
      .groups = "drop"
    ) %>%
    arrange(ordem) %>%
    mutate(
      percentual = n / sum(n),
      posicao_rotulo = sum(n) - cumsum(n) + n / 2,
      categoria_legenda = if_else(
        categoria == "Outras",
        paste0(
          "Outras (", n_plantas, " plantas): ", n, " (",
          percent(percentual, accuracy = 0.1, decimal.mark = ","), ")"
        ),
        paste0(
          categoria, ": ", n, " (",
          percent(percentual, accuracy = 0.1, decimal.mark = ","), ")"
        )
      ),
      categoria_legenda = factor(
        categoria_legenda,
        levels = categoria_legenda
      ),
      rotulo_fatia = if_else(
        ordem <= 3 | categoria == "Outras",
        if_else(
          categoria == "Outras",
          paste0(
            "Outras\n", n_plantas, " plantas\n", n, " (",
            percent(percentual, accuracy = 0.1, decimal.mark = ","), ")"
          ),
          paste0(
            categoria, "\n", n, " (",
            percent(percentual, accuracy = 0.1, decimal.mark = ","), ")"
          )
        ),
        ""
      ),
      cor_rotulo = if_else(
        categoria == "Outras",
        "#202020",
        "white"
      )
    )
}

cores_pizza <- c(
  "#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
  "#56B4E9", "#332288", "#117733", "#AA4499", "#882255",
  "#B8B8B8"
)

# 14. Distribuicao das associacoes planta-medicamento ativo.
medicamentos_ativos_por_planta_pizza <- base_ativos %>%
  distinct(id_planta, nome_planta, chave_produto) %>%
  count(id_planta, nome_planta, name = "n", sort = TRUE) %>%
  preparar_dados_pizza()

n_outras_plantas_medicamentos <- medicamentos_ativos_por_planta_pizza %>%
  filter(categoria == "Outras") %>%
  pull(n_plantas)

cores_medicamentos_pizza <- setNames(
  cores_pizza[seq_len(nrow(medicamentos_ativos_por_planta_pizza))],
  levels(medicamentos_ativos_por_planta_pizza$categoria_legenda)
)

g_medicamentos_ativos_pizza <- ggplot(
  medicamentos_ativos_por_planta_pizza,
  aes(
    x = 2,
    y = n,
    fill = categoria_legenda
  )
) +
  geom_col(
    width = 0.9,
    color = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      y = posicao_rotulo,
      label = rotulo_fatia,
      color = cor_rotulo
    ),
    family = "serif",
    fontface = "bold",
    size = 2.8,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  coord_polar(
    theta = "y",
    start = -pi / 2
  ) +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    values = cores_medicamentos_pizza,
    drop = FALSE
  ) +
  scale_color_identity() +
  labs(
    title = "Medicamentos ativos associados por planta",
    subtitle = paste0(
      "As 10 maiores plantas são destacadas; as outras ",
      n_outras_plantas_medicamentos,
      " são reunidas em uma única fatia"
    ),
    caption = paste(
      "Cada medicamento é contado uma vez em cada planta a que está associado.",
      "Os percentuais referem-se às associações planta-medicamento ativo."
    ),
    fill = NULL
  ) +
  theme_void(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      color = "#202020"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "#4A4A4A",
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      size = 8,
      color = "#555555",
      hjust = 0,
      margin = margin(t = 8)
    ),
    legend.position = "left",
    legend.justification = "center",
    legend.text = element_text(size = 9),
    legend.key.size = grid::unit(0.5, "cm"),
    legend.spacing.y = grid::unit(0.12, "cm"),
    plot.margin = margin(10, 14, 8, 14)
  ) +
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = TRUE
    )
  )

salvar_figura(
  g_medicamentos_ativos_pizza,
  "14_pizza_medicamentos_ativos_por_planta",
  11,
  9
)

# 15. Distribuicao das associacoes planta-empresa com medicamento ativo.
empresas_por_planta_pizza <- base_ativos %>%
  filter(
    !is.na(empresa_detentora_da_regularizacao),
    str_squish(empresa_detentora_da_regularizacao) != ""
  ) %>%
  distinct(
    id_planta,
    nome_planta,
    empresa_detentora_da_regularizacao
  ) %>%
  count(id_planta, nome_planta, name = "n", sort = TRUE) %>%
  preparar_dados_pizza()

n_outras_plantas_empresas <- empresas_por_planta_pizza %>%
  filter(categoria == "Outras") %>%
  pull(n_plantas)

cores_empresas_pizza <- setNames(
  cores_pizza[seq_len(nrow(empresas_por_planta_pizza))],
  levels(empresas_por_planta_pizza$categoria_legenda)
)

g_empresas_por_planta_pizza <- ggplot(
  empresas_por_planta_pizza,
  aes(
    x = 2,
    y = n,
    fill = categoria_legenda
  )
) +
  geom_col(
    width = 0.9,
    color = "white",
    linewidth = 0.6
  ) +
  geom_text(
    aes(
      y = posicao_rotulo,
      label = rotulo_fatia,
      color = cor_rotulo
    ),
    family = "serif",
    fontface = "bold",
    size = 2.8,
    lineheight = 0.9,
    show.legend = FALSE
  ) +
  coord_polar(
    theta = "y",
    start = -pi / 2
  ) +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    values = cores_empresas_pizza,
    drop = FALSE
  ) +
  scale_color_identity() +
  labs(
    title = "Empresas com medicamentos ativos por planta",
    subtitle = paste0(
      "As 10 maiores plantas são destacadas; as outras ",
      n_outras_plantas_empresas,
      " são reunidas em uma única fatia"
    ),
    caption = paste(
      "Cada empresa é contada uma vez em cada planta para a qual possui medicamento ativo.",
      "Os percentuais referem-se às associações planta-empresa."
    ),
    fill = NULL
  ) +
  theme_void(base_family = "serif", base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 12,
      color = "#202020"
    ),
    plot.subtitle = element_text(
      size = 10,
      color = "#4A4A4A",
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      size = 8,
      color = "#555555",
      hjust = 0,
      margin = margin(t = 8)
    ),
    legend.position = "left",
    legend.justification = "center",
    legend.text = element_text(size = 9),
    legend.key.size = grid::unit(0.5, "cm"),
    legend.spacing.y = grid::unit(0.12, "cm"),
    plot.margin = margin(10, 14, 8, 14)
  ) +
  guides(
    fill = guide_legend(
      ncol = 1,
      byrow = TRUE
    )
  )

salvar_figura(
  g_empresas_por_planta_pizza,
  "15_pizza_empresas_por_planta_ativos",
  11,
  9
)
