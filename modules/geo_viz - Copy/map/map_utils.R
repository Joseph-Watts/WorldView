# modules/geo_viz/map/map_utils.R

# ---- null-coalescing helper ----
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


# ---- palette choices ----
map_palette_choices <- function() {
  c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
}

map_palette_option <- function(option) {
  choices <- map_palette_choices()
  if (is.null(option) || length(option) != 1 || !(option %in% choices)) {
    return("viridis")
  }
  option
}

map_palette_cols <- function(n = 256, option = "viridis") {
  # Strip the alpha channel from viridisLite colours so CSS gradients render
  # consistently across browsers.
  substr(viridisLite::viridis(n, option = map_palette_option(option)), 1, 7)
}

# ---- join data ----
join_world_data <- function(world_shape, wvs_country, country_phylogeny) {
  out <- world_shape %>%
    dplyr::left_join(wvs_country, by = c("iso_a3" = "B_COUNTRY_ALPHA")) %>%
    dplyr::left_join(
      country_phylogeny %>% dplyr::distinct(iso3166alpha3, .keep_all = TRUE),
      by = c("iso_a3" = "iso3166alpha3")
    )
  
  if (!"name" %in% names(out) && "name_long" %in% names(out)) {
    out$name <- out$name_long
  }
  out
}

# ---- display name for a WVS variable ----
map_var_display <- function(var, codebook_data = NULL) {
  if (is.null(var) || length(var) == 0 || is.na(var)) {
    return("")
  }
  
  if (!is.null(codebook_data) && exists("wvs_var_display", mode = "function")) {
    return(wvs_var_display(var, codebook_data))
  }
  
  var
}

# ---- build per-country hover label ----
build_country_labels <- function(world_sf, var = NULL, digits = 3, codebook_data = NULL) {
  n <- nrow(world_sf)
  
  nm   <- world_sf$name %||% world_sf$iso_a3
  iso3 <- world_sf$iso_a3
  
  fmt_num <- function(x) {
    ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))
  }
  
  var_label <- map_var_display(var, codebook_data)
  
  vapply(seq_len(n), function(i) {
    parts <- c(
      paste0("<b>", htmltools::htmlEscape(nm[i]), "</b>"),
      paste0("ISO3: ", htmltools::htmlEscape(iso3[i]))
    )
    
    if (!is.null(var) && length(var) == 1 && var %in% names(world_sf)) {
      parts <- c(
        parts,
        "<hr style='margin:6px 0;'/>",
        paste0(
          htmltools::htmlEscape(var_label), ": ",
          htmltools::htmlEscape(fmt_num(world_sf[[var]][i]))
        )
      )
    }
    
    paste(parts, collapse = "<br/>")
  }, character(1))
}

# ---- numeric palette for selected variable ----
make_variable_palette <- function(x, palette_option = "viridis") {
  rng <- range(x, na.rm = TRUE)
  if (!all(is.finite(rng)) || rng[1] == rng[2]) {
    rng <- c(0, 1)
  }
  leaflet::colorNumeric(
    palette  = map_palette_cols(256, palette_option),
    domain   = rng,
    na.color = "#CCCCCC"
  )
}

# ---- country polygons coloured by selected variable ----
add_variable_polygons <- function(map, sf1, var, codebook_data = NULL, palette_option = "viridis") {
  if (is.null(var) || length(var) != 1 || !(var %in% names(sf1))) {
    return(
      map %>% leaflet::addPolygons(
        data        = sf1,
        fillColor   = "#CCCCCC",
        fillOpacity = 0.35,
        weight      = 1,
        color       = "white",
        label       = lapply(build_country_labels(sf1), htmltools::HTML),
        highlightOptions = leaflet::highlightOptions(
          weight = 2,
          color = "#444444",
          bringToFront = TRUE
        )
      )
    )
  }
  
  sf1$.map_value <- suppressWarnings(as.numeric(sf1[[var]]))
  pal <- make_variable_palette(sf1$.map_value, palette_option = palette_option)
  label <- build_country_labels(sf1, var = var, codebook_data = codebook_data)
  map %>%
    leaflet::addPolygons(
      data        = sf1,
      fillColor   = ~pal(.map_value),
      fillOpacity = 0.75,
      weight      = 1,
      color       = "white",
      label       = lapply(label, htmltools::HTML),
      highlightOptions = leaflet::highlightOptions(
        weight = 2,
        color = "#444444",
        bringToFront = TRUE
      )
    )
}

# ---- external continuous legend below the leaflet map ----
build_map_legend_ui <- function(world_sf, var, codebook_data = NULL, palette_option = "viridis") {
  if (is.null(var) || length(var) != 1 || !(var %in% names(world_sf))) {
    return(NULL)
  }
  
  values <- suppressWarnings(as.numeric(world_sf[[var]]))
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(NULL)
  }
  
  rng <- range(values, na.rm = TRUE)
  if (!all(is.finite(rng))) {
    return(NULL)
  }
  
  legend_title <- map_var_display(var, codebook_data)
  fmt <- function(x) format(round(x, 3), nsmall = 3, trim = TRUE)
  
  if (rng[1] == rng[2]) {
    tick_vals <- rng[1]
    tick_labels <- fmt(tick_vals)
  } else {
    tick_vals <- unique(pretty(rng, n = 5))
    tick_vals <- tick_vals[tick_vals >= rng[1] & tick_vals <= rng[2]]
    if (length(tick_vals) < 2) {
      tick_vals <- rng
    }
    tick_labels <- fmt(tick_vals)
  }
  
  cols <- map_palette_cols(9, palette_option)
  stops <- paste0(cols, " ", seq(0, 100, length.out = length(cols)), "%")
  gradient_css <- paste0("linear-gradient(to right, ", paste(stops, collapse = ", "), ")")
  
  tick_items <- lapply(seq_along(tick_labels), function(i) {
    htmltools::tags$span(tick_labels[[i]])
  })
  
  htmltools::tags$div(
    class = "map-legend-below",
    style = paste0(
      "max-width:720px; margin:12px auto 0 auto; padding:0 10px;",
      "font-size:12px; color:#333;"
    ),
    htmltools::tags$div(
      style = "font-weight:600; text-align:center; margin-bottom:5px;",
      legend_title
    ),
    htmltools::tags$div(
      style = paste0(
        "height:14px; border-radius:3px; border:1px solid rgba(0,0,0,.25); ",
        "background:", gradient_css, ";"
      )
    ),
    htmltools::tags$div(
      style = paste0(
        "display:flex; justify-content:space-between; gap:8px; ",
        "margin-top:4px; font-variant-numeric: tabular-nums;"
      ),
      tick_items
    )
  )
}

# ---- static map for image download ----
build_static_map_plot <- function(world_sf, var, codebook_data = NULL, palette_option = "viridis") {
  if (is.null(var) || length(var) != 1 || !(var %in% names(world_sf))) {
    stop("No valid map variable selected.", call. = FALSE)
  }
  
  sf1 <- world_sf
  sf1$.map_value <- suppressWarnings(as.numeric(sf1[[var]]))
  legend_title <- map_var_display(var, codebook_data)
  values <- sf1$.map_value[is.finite(sf1$.map_value)]
  if (length(values) == 0) {
    stop("The selected map variable has no finite values to plot.", call. = FALSE)
  }
  
  ggplot2::ggplot(sf1) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$.map_value), color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradientn(
      colours = map_palette_cols(256, palette_option),
      na.value = "#CCCCCC",
      name = legend_title,
      guide = ggplot2::guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        barwidth = grid::unit(5.5, "in"),
        barheight = grid::unit(0.18, "in")
      )
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 11, face = "bold"),
      legend.text = ggplot2::element_text(size = 9),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
