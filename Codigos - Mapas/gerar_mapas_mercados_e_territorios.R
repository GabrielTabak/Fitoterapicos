# Mapas gerais por mercado e recortes dos territórios de plantio.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(sf)
  library(ggplot2)
  library(leaflet)
})
f <- sub("^--file=", "", commandArgs(FALSE)[grepl("^--file=", commandArgs(FALSE))])
HERE <- dirname(normalizePath(gsub("~+~", " ", f, fixed = TRUE)))
ROOT <- dirname(HERE)
OUT <- file.path(HERE, "MapasFinais")
for (d in c("gerais", "territorios", "interativos")) dir.create(file.path(OUT, d), recursive = TRUE, showWarnings = FALSE)
rd <- function(p) read_csv2(p, col_types = cols(.default = col_character()), show_col_types = FALSE)
norm <- function(x) str_squish(str_replace_all(stri_trans_general(str_to_lower(coalesce(x, "")), "Latin-ASCII"), "[^a-z0-9]+", " "))
slug <- function(x) str_replace_all(norm(x), " ", "_")
unique_text <- function(x) paste(sort(unique(x[!is.na(x) & x != ""])), collapse = " | ")
has <- function(text, aliases) {
  a <- strsplit(coalesce(aliases, ""), "|", fixed = TRUE)[[1]]
  a <- a[nzchar(a)]
  if (!length(a)) {
    return(rep(FALSE, length(text)))
  }
  Reduce(`|`, lapply(a, function(z) str_detect(paste0(" ", text, " "), fixed(paste0(" ", norm(z), " ")))))
}
num <- function(x) as.numeric(str_replace(x, ",", "."))
cores <- c("Medicamentos" = "#177D65", "Alimentos" = "#C57514", "Cosméticos Registrados" = "#5766B1", "Cosméticos Isentos de Registro" = "#AC568C", "Saneantes" = "#187FA0")
curtos <- c("Medicamentos", "Alimentos", "Cosméticos registrados", "Cosméticos isentos", "Saneantes")
names(curtos) <- names(cores)

catalogo <- rd(file.path(ROOT, "Relatorios/Mapas Plantas Grupo Bioeconomia/config/especies.csv"))
nomes <- rd(file.path(HERE, "config/nomes_produtos_plantas.csv"))
stopifnot(setequal(catalogo$id, nomes$id), !anyDuplicated(nomes$id))
catalogo <- left_join(catalogo, nomes, by = "id", relationship = "one-to-one")
territorios <- catalogo |>
  select(id, territorios) |>
  separate_rows(territorios, sep = ";\\s*") |>
  rename(territorio = territorios)

med <- rd(file.path(ROOT, "Codigos/ResultadosFinal/Bases/base_produto_planta.csv")) |>
  filter(situacao == "Ativo") |>
  mutate(cnpj = str_remove_all(str_extract(empresa_detentora_da_regularizacao, "[0-9]{2}\\.[0-9]{3}\\.[0-9]{3}/[0-9]{4}-[0-9]{2}"), "[^0-9]"))
gmed <- rd(file.path(HERE, "Data/Receita/medicamentos_empresas_geocodificadas.csv")) |>
  transmute(
    cnpj = cnpj_numero, empresa, municipio, uf, latitude = num(latitude), longitude = num(longitude),
    precisao = "Coordenada cadastral por CEP; não comprova localização da fábrica", mapeavel = coordenada_ok == "TRUE"
  )
proxy <- rd(file.path(HERE, "Data/OutrosMercados/proxy_com_cnpj.csv")) |>
  mutate(chave = coalesce(na_if(processo, ""), na_if(registro, ""), produto))
geo <- rd(file.path(HERE, "Data/OutrosMercados/empresas_georreferenciadas.csv")) |>
  transmute(categoria, cnpj,
    empresa = empresas_proxy, municipio, uf, latitude = num(latitude), longitude = num(longitude),
    precisao = precisao_geografica, mapeavel = mapeavel == "TRUE"
  )
stopifnot(!anyDuplicated(gmed$cnpj), !anyDuplicated(geo[c("categoria", "cnpj")]))

# Os mapas gerais incluem todas as empresas das bases ativas, sem recorte de plantas.
geral_med <- med |>
  group_by(cnpj) |>
  summarise(plantas = unique_text(nome_planta), produtos = n_distinct(chave_produto), .groups = "drop") |>
  left_join(gmed, by = "cnpj", relationship = "many-to-one") |>
  mutate(categoria = "Medicamentos", evidencia = "Associação da base de princípios ativos")
geral_outros <- proxy |>
  group_by(categoria, cnpj) |>
  summarise(plantas = unique_text(termo_busca), produtos = n_distinct(chave), .groups = "drop") |>
  left_join(geo, by = c("categoria", "cnpj"), relationship = "many-to-one") |>
  mutate(evidencia = "Termos da busca por nome; composição não confirmada")
gerais <- bind_rows(geral_med, geral_outros)

# Correspondências por espécie; nomes populares ficam explicitamente como indícios.
texto_med <- norm(do.call(paste, c(med[c("opcao", "opcoes_origem", "alias_encontrado", "principio_ativo_ou_descricao_do_medicamento_notificado")], sep = " | ")))
texto_proxy <- norm(paste(proxy$termo_busca, proxy$produto))
termos_proxy <- norm(proxy$termo_busca)
associacoes <- bind_rows(lapply(seq_len(nrow(catalogo)), function(i) {
  s <- catalogo[i, ]
  a <- med[has(texto_med, s$aliases), ] |>
    distinct(cnpj, chave_produto) |>
    group_by(cnpj) |>
    summarise(produtos = n(), .groups = "drop") |>
    left_join(gmed, by = "cnpj", relationship = "many-to-one") |>
    mutate(categoria = "Medicamentos", evidencia = "Princípio ativo: correspondência científica")
  cientifico <- has(texto_proxy, s$aliases)
  populares <- termos_proxy %in% norm(strsplit(coalesce(s$nomes_busca, ""), "|", fixed = TRUE)[[1]])
  excluir <- has(texto_proxy, s$excluir)
  b <- proxy[(cientifico | populares) & !excluir, ] |>
    mutate(evidencia = if_else(cientifico[(cientifico | populares) & !excluir],
      "Nome científico na busca/produto; composição não confirmada",
      "Nome popular na busca; espécie e composição não confirmadas"
    )) |>
    group_by(categoria, cnpj, evidencia) |>
    summarise(produtos = n_distinct(chave), .groups = "drop") |>
    left_join(geo, by = c("categoria", "cnpj"), relationship = "many-to-one")
  bind_rows(a, b) |> mutate(id = s$id, planta = s$planta, especie = s$especie)
}))
validos <- function(d) d |> filter(mapeavel %in% TRUE, is.finite(longitude), is.finite(latitude), between(longitude, -74, -28), between(latitude, -35, 6))

# Limites estaduais ou municipais conforme a escala declarada na lista original.
estados <- st_read(file.path(HERE, "Data/Geografia/BR_UF_2024/BR_UF_2024.shp"), quiet = TRUE) |>
  st_transform(5880) |>
  st_make_valid()
municipios <- st_read(file.path(HERE, "Data/OutrosMercados/fontes/municipios_ibge.geojson"), quiet = TRUE)
municipios <- municipios[municipios$codarea %in% c("4108403", "4100509", "3503208", "3521705"), ] |>
  st_transform(5880) |>
  st_make_valid()
ids_municipais <- c("Francisco Beltrão/PR" = "4108403", "Altônia/PR" = "4100509", "Araraquara/SP" = "3503208", "Itaberá/SP" = "3521705")
message("Bases preparadas; construindo territórios.")
regioes <- lapply(unique(territorios$territorio), function(t) {
  g <- if (t %in% names(ids_municipais)) municipios[municipios$codarea == ids_municipais[[t]], ] else estados[estados$NM_UF == t, ]
  if (nrow(g) != 1) stop("Limite geográfico não identificado: ", t)
  st_sf(territorio = t, geometry = st_geometry(g))
}) |> bind_rows()
stopifnot(nrow(catalogo) == 23, nrow(territorios) == 59, nrow(regioes) == 12)

# Hachura recortada pelo polígono real, sem depender de ggpattern.
hachura <- function(regiao) {
  bb <- st_bbox(regiao)
  passo <- max(bb$xmax - bb$xmin, bb$ymax - bb$ymin) / 32
  y <- seq(bb$ymin - (bb$xmax - bb$xmin), bb$ymax, by = passo)
  linhas <- st_sfc(lapply(y, function(v) st_linestring(matrix(c(bb$xmin, v, bb$xmax, v + bb$xmax - bb$xmin), ncol = 2, byrow = TRUE))), crs = 5880)
  suppressWarnings(st_intersection(linhas, st_union(regiao)))
}
base_tema <- theme_void(base_size = 12) + theme(
  plot.background = element_rect(fill = "#FAFAF7", color = NA),
  plot.title = element_text(face = "bold", size = 23, color = "#174F45"), plot.subtitle = element_text(size = 11, lineheight = 1.2),
  plot.caption = element_text(size = 9, hjust = 0, lineheight = 1.2), legend.position = "bottom", plot.margin = margin(18, 18, 18, 18),
  strip.text = element_text(face = "bold", size = 11)
)
pontos <- function(d) {
  d |>
    group_by(categoria, longitude, latitude) |>
    summarise(empresas = n_distinct(cnpj), .groups = "drop") |>
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
    st_transform(5880)
}
salvar <- function(p, caminho) ggsave(file.path(OUT, caminho), p, width = 13, height = 10, dpi = 180, bg = "#FAFAF7")

# Popups preservam todas as empresas de uma coordenada, sem deslocar os pontos.
escape <- function(x) as.character(htmltools::htmlEscape(coalesce(as.character(x), "")))
mapa_html <- function(d, titulo, nome, regiao = NULL, linhas = NULL, plantas = "") {
  e <- st_transform(st_simplify(estados, dTolerance = 1200), 4326)
  m <- leaflet(options = leafletOptions(preferCanvas = FALSE, minZoom = 3, fadeAnimation = FALSE)) |>
    addPolygons(data = e, fillColor = "#ECEDE7", fillOpacity = 1, color = "#FFFFFF", weight = 1, label = ~NM_UF) |>
    addControl(htmltools::HTML(paste0(
      "<div style='max-width:360px;max-height:150px;overflow:auto'><strong>", escape(titulo), "</strong><br>", escape(plantas),
      "<br>Selecione os mercados à direita. Clique nos pontos para ver empresas e plantas.<br>Use − para ver empresas fora do recorte inicial.<br><small>Pontos cadastrais, sem comprovação de fábricas ou demanda. Outros mercados: associação por nome.</small></div>"
    )), position = "topleft")
  if (!is.null(regiao)) {
    m <- m |>
      addPolygons(data = st_transform(regiao, 4326), fillColor = "#F5D988", fillOpacity = 0.3, color = "#A16D11", weight = 2) |>
      addPolylines(data = st_transform(linhas, 4326), color = "#A16D11", weight = 1, opacity = 0.8)
  }
  for (cat in names(cores)) {
    dd <- d |> filter(categoria == cat)
    if (!nrow(dd)) next
    popup <- dd |>
      group_by(longitude, latitude) |>
      summarise(
        n = n_distinct(cnpj), local = first(paste(municipio, uf, sep = " / ")),
        html = paste0("<b>", escape(empresa), "</b><br>CNPJ: ", escape(cnpj), "<br>Plantas/termos: ", escape(plantas),
          "<br>Evidência: ", escape(evidencia), "<br>Localização: ", escape(precisao),
          collapse = "<hr>"
        ), .groups = "drop"
      )
    m <- m |> addCircleMarkers(
      data = popup, lng = ~longitude, lat = ~latitude, group = cat,
      radius = ~ pmin(13, 4 + sqrt(n)), color = "white", weight = 1, fillColor = cores[[cat]], fillOpacity = 0.85,
      label = ~ paste(local, "—", n, "empresa(s) —", cat),
      popup = paste0("<b>", escape(cat), " · ", escape(popup$local), "</b><div style='max-height:300px;overflow:auto'>", popup$html, "</div>"),
      popupOptions = popupOptions(maxWidth = 470, autoPanPaddingTopLeft = c(10, 250), autoPanPaddingBottomRight = c(10, 140))
    )
  }
  m <- m |>
    addLayersControl(overlayGroups = names(cores), options = layersControlOptions(collapsed = TRUE)) |>
    addLegend(colors = unname(cores), labels = unname(curtos), position = "bottomright")
  bb <- st_bbox(st_transform(if (is.null(regiao)) estados else st_as_sfc(st_bbox(regiao) + c(-200000, -200000, 200000, 200000)), 4326))
  m <- m |> fitBounds(unname(bb$xmin), unname(bb$ymin), unname(bb$xmax), unname(bb$ymax))
  m <- htmlwidgets::onRender(m, "function(el, x) {
    var map = this;
    function focusable() {
      map.eachLayer(function(layer) {
        if (!layer.getTooltip || !layer.getTooltip() || !layer.getElement) return;
        var node = layer.getElement(); if (!node) return;
        node.setAttribute('tabindex', '0'); node.setAttribute('role', 'button');
        node.setAttribute('aria-label', layer.getTooltip().getContent());
        node.onfocus = function(){layer.openTooltip();};
        node.onblur = function(){layer.closeTooltip();};
        node.onkeydown = function(e){if(e.key==='Enter' || e.key===' '){e.preventDefault();layer.openPopup();}};
      });
    }
    map.on('overlayadd', focusable); focusable();
  }")
  htmlwidgets::saveWidget(m, file.path(OUT, "interativos", paste0(nome, ".html")), selfcontained = FALSE, libdir = "bibliotecas", title = titulo)
}

# Cinco mapas nacionais comparáveis, com a mesma escala de tamanho.
max_n <- max(pontos(validos(gerais))$empresas)
for (cat in names(cores)) {
  message("Mapa geral: ", cat)
  d <- validos(gerais) |> filter(categoria == cat)
  p <- ggplot() +
    geom_sf(data = estados, fill = "#E9ECE6", color = "white", linewidth = 0.3) +
    geom_sf(data = pontos(d), aes(size = empresas), color = cores[[cat]], alpha = 0.7) +
    scale_size_area(max_size = 11, limits = c(0, max_n), name = "Empresas na coordenada") +
    labs(
      title = paste("Empresas ·", curtos[[cat]]), subtitle = paste(n_distinct(d$cnpj), "CNPJs mapeados · registros ativos da base local"),
      caption = "Medicamentos: coordenadas cadastrais por CEP. Demais categorias: ponto representativo do município.\nFontes: bases locais Anvisa, cadastros de empresas e limites IBGE. Uma empresa pode aparecer em mais de uma categoria."
    ) +
    base_tema
  salvar(p, paste0("gerais/", slug(cat), ".png"))
}
mapa_html(validos(gerais), "Empresas por mercado · Brasil", "brasil")

resumos <- list()
for (i in seq_len(nrow(regioes))) {
  reg <- regioes[i, ]
  t <- reg$territorio
  message("Território: ", t)
  ids <- territorios$id[territorios$territorio == t]
  d <- validos(associacoes) |> filter(id %in% ids)
  # O detalhe fica no popup e na planilha, sem ocultar empresas em rótulos truncados.
  empresas <- d |>
    group_by(categoria, cnpj, empresa, municipio, uf, latitude, longitude, precisao) |>
    summarise(plantas = unique_text(paste0(planta, " [", especie, "]")), evidencia = unique_text(evidencia), .groups = "drop")
  bb <- st_bbox(reg) + c(-200000, -200000, 200000, 200000)
  pts <- pontos(empresas)
  dentro <- lengths(st_intersects(pts, st_as_sfc(bb))) > 0
  visiveis <- pts[dentro, ]
  h <- hachura(reg)
  # Uma faceta por mercado evita que pontos coincidentes escondam categorias.
  fundo <- estados[rep(seq_len(nrow(estados)), length(cores)), ]
  fundo$mercado <- rep(unname(curtos), each = nrow(estados))
  visiveis$mercado <- unname(curtos[visiveis$categoria])
  nomes_plantas <- unique_text(catalogo$planta[catalogo$id %in% ids])
  p <- ggplot() +
    geom_sf(data = fundo, fill = "#ECEEE8", color = "white", linewidth = 0.25) +
    geom_sf(data = reg, fill = "#F8E4AA", color = "#A16D11", linewidth = 0.5) +
    geom_sf(data = h, color = "#A16D11", linewidth = 0.25) +
    geom_sf(data = visiveis, aes(size = empresas, color = categoria), alpha = 0.8) +
    scale_color_manual(values = cores, guide = "none") +
    scale_size_area(max_size = 7, name = "Empresas na coordenada") +
    facet_wrap(~ factor(mercado, levels = unname(curtos)), ncol = 3, drop = FALSE) +
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], expand = FALSE, datum = NA) +
    labs(
      title = paste("Plantas de interesse ·", t), subtitle = str_wrap(paste("Plantas:", nomes_plantas), 130),
      caption = paste0(
        "Hachura: território indicado na lista de plantio. Enquadramento: limite + 200 km em cada direção (não é uma área de captação).\n",
        "Pontos somente de empresas associadas às plantas acima. No Brasil: ", n_distinct(d$cnpj), " CNPJs; consulte os detalhes e o mapa nacional no HTML.\n",
        "Medicamentos: correspondência científica. Outros mercados: indícios pelo nome, sem comprovação de composição ou espécie.\n",
        "Coordenadas cadastrais/municipais, não localização de fábricas. Ausência de pontos não significa ausência de mercado."
      )
    ) +
    base_tema
  salvar(p, paste0("territorios/", slug(t), ".png"))
  mapa_html(empresas, paste("Plantas de interesse ·", t), slug(t), reg, h, nomes_plantas)
  resumos[[i]] <- tibble(
    territorio = t, plantas_selecionadas = length(ids), empresas_associadas_brasil = n_distinct(d$cnpj),
    pontos_categoria_no_enquadramento = nrow(visiveis)
  )
}
cobertura <- gerais |>
  group_by(categoria) |>
  summarise(
    empresas = n_distinct(cnpj, na.rm = TRUE),
    empresas_mapeadas = n_distinct(cnpj[mapeavel %in% TRUE & is.finite(latitude) & is.finite(longitude)], na.rm = TRUE), .groups = "drop"
  )
por_planta <- catalogo |>
  select(id, planta, especie) |>
  left_join(associacoes |> group_by(id, categoria) |> summarise(empresas = n_distinct(cnpj), .groups = "drop"), by = "id")
writexl::write_xlsx(list(
  cobertura = cobertura, plantas = por_planta, territorios = bind_rows(resumos),
  empresas_gerais = gerais, associacoes_plantas = associacoes, escolhas_territoriais = territorios,
  pendencias_geograficas = gerais |> filter(!(mapeavel %in% TRUE) | !is.finite(latitude) | !is.finite(longitude))
), file.path(OUT, "dados_dos_mapas.xlsx"))
links <- paste0("<li><a href='interativos/", slug(regioes$territorio), ".html'>", escape(regioes$territorio), "</a></li>", collapse = "\n")
writeLines(paste0(
  "<!doctype html><html lang='pt-BR'><meta charset='utf-8'><title>Mapas de mercados e territórios</title><style>body{font:18px system-ui;max-width:850px;margin:50px auto;padding:20px;color:#174F45}li{margin:12px 0}a{color:#176D91}</style><h1>Empresas, plantas e territórios</h1><p>Registros ativos das bases locais. Selecione um mapa e clique nos pontos para consultar empresas, mercados e plantas associadas.</p><h2>Brasil por mercado</h2><a href='interativos/brasil.html'>Abrir mapa geral</a><h2>Territórios da lista de plantio</h2><ul>", links,
  "</ul><p>Hachura: área de interesse. Outros mercados usam correspondência por nome; isso não comprova a composição dos produtos. Não há localização individual de produtores.</p><p><a href='dados_dos_mapas.xlsx'>Planilha de dados e cobertura</a></p></html>"
), file.path(OUT, "index.html"))
print(cobertura)
message("Concluído: 5 mapas gerais, 12 territoriais, 13 interativos e uma planilha em ", OUT)
