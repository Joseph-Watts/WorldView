# modules/geo_viz/map/map_utils.R

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

# ---- build per-country hover label (vectorized) ----
build_country_labels <- function(world_sf, vars = character(), digits = 3) {
  n <- nrow(world_sf)
  
  nm   <- world_sf$name %||% world_sf$iso_a3
  iso3 <- world_sf$iso_a3
  
  glottocode           <- world_sf$glottocode %||% rep(NA_character_, n)
  language_name        <- world_sf$language_name %||% rep(NA_character_, n)
  family_glottocode    <- world_sf$family_glottocode %||% rep(NA_character_, n)
  family_language_name <- world_sf$family_language_name %||% rep(NA_character_, n)
  
  fmt_num <- function(x) {
    ifelse(is.na(x), "NA", format(round(x, digits), nsmall = digits))
  }
  
  out <- vapply(seq_len(n), function(i) {
    parts <- c(
      paste0("<b>", htmltools::htmlEscape(nm[i]), "</b>"),
      paste0("ISO3: ", htmltools::htmlEscape(iso3[i])),
      paste0("glottocode: ", htmltools::htmlEscape(glottocode[i])),
      paste0("language: ", htmltools::htmlEscape(language_name[i])),
      paste0("family_glottocode: ", htmltools::htmlEscape(family_glottocode[i])),
      paste0("family: ", htmltools::htmlEscape(family_language_name[i]))
    )
    
    if (length(vars) > 0) {
      vlines <- character()
      for (v in vars) {
        if (v %in% names(world_sf)) {
          vlines <- c(vlines, paste0(v, ": ", fmt_num(world_sf[[v]][i])))
        }
      }
      if (length(vlines)) {
        parts <- c(parts, "<hr style='margin:6px 0;'/>", paste(vlines, collapse = "<br/>"))
      }
    }
    
    paste(parts, collapse = "<br/>")
  }, character(1))
  
  out
}

# ---- background palette (discrete) ----
make_bg_palette <- function(x) {
  lev <- sort(unique(stats::na.omit(as.character(x))))
  pal_cols <- if (length(lev) <= 10) {
    RColorBrewer::brewer.pal(max(3, length(lev)), "Set3")
  } else {
    grDevices::rainbow(length(lev))
  }
  leaflet::colorFactor(palette = pal_cols[seq_along(lev)], domain = lev, na.color = "#CCCCCC")
}

# ---- background polygons (NO leaflet legend here) ----
add_background_polygons <- function(map, sf1, bg_mode) {
  if (is.null(bg_mode) || bg_mode == "none" || !(bg_mode %in% names(sf1))) {
    # Outline only
    return(
      map %>% leaflet::addPolygons(
        data        = sf1,
        fillColor   = "transparent",
        fillOpacity = 0,
        weight      = 1,
        color       = "white"
      )
    )
  }
  
  sf1$.bg_val <- sf1[[bg_mode]]
  pal <- make_bg_palette(sf1$.bg_val)
  
  map %>% leaflet::addPolygons(
    data        = sf1,
    fillColor   = ~pal(.bg_val),
    fillOpacity = 0.65,
    weight      = 1,
    color       = "white"
  )
}

# ---- background legend UI (full list, scrollable; placed in Data control) ----
build_bg_legend_full_ui <- function(world_sf, bg_mode) {
  if (is.null(bg_mode) || bg_mode == "none" || !(bg_mode %in% names(world_sf))) {
    return(HTML("<i>No background colouring (outline only).</i>"))
  }
  
  x <- as.character(world_sf[[bg_mode]])
  x[is.na(x) | x == ""] <- "NA"
  
  tab <- sort(table(x), decreasing = TRUE)
  
  items <- lapply(names(tab), function(k) {
    tags$div(
      style = "display:flex; justify-content:space-between; gap:8px; margin:2px 0;",
      tags$span(htmltools::htmlEscape(k)),
      tags$span(style = "opacity:.75;", as.integer(tab[[k]]))
    )
  })
  
  div(
    style = paste0(
      "max-height:150px; overflow-y:auto; overflow-x:hidden; ",
      "border:1px solid rgba(0,0,0,.1); border-radius:6px; padding:8px;"
    ),
    tags$div(style = "font-size:12px; opacity:.8; margin-bottom:6px;",
             "All categories (count of countries):"),
    items
  )
}

# ---- d3.schemeCategory10 (hard-coded; stable) ----
palette_category10 <- function() {
  c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )
}

# ---- scaling for charts ----
scale_01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    return(rep(0, length(x)))
  }
  (x - rng[1]) / (rng[2] - rng[1])
}

# ---- prepare minicharts data (no attribute warning) ----
prepare_minicharts_data <- function(world_sf, vars) {
  # Use geometry only -> avoids:
  # "st_point_on_surface assumes attributes are constant over geometries"
  sf_proj  <- sf::st_transform(world_sf, 3857)
  pts_proj <- sf::st_point_on_surface(sf::st_geometry(sf_proj))  # sfc only
  
  pts_ll <- sf::st_transform(pts_proj, 4326)
  coords <- sf::st_coordinates(pts_ll)
  
  sf_ll <- sf::st_transform(world_sf, 4326)
  chart_mat <- sapply(vars, function(v) scale_01(sf_ll[[v]]))
  chart_mat <- as.matrix(chart_mat)
  
  list(
    lng       = coords[, 1],
    lat       = coords[, 2],
    chart_mat = chart_mat
  )
}

# ---- legend df for charts (outside map) ----
make_chart_legend_df <- function(world_sf, vars, colors) {
  k <- length(vars)
  cols <- rep(colors, length.out = k)
  
  rngs <- lapply(vars, function(v) {
    x <- world_sf[[v]]
    r <- range(x, na.rm = TRUE)
    c(min = ifelse(is.finite(r[1]), r[1], NA_real_),
      max = ifelse(is.finite(r[2]), r[2], NA_real_))
  })
  rngs <- do.call(rbind, rngs)
  
  data.frame(
    var  = vars,
    col  = cols,
    vmin = rngs[, "min"],
    vmax = rngs[, "max"],
    stringsAsFactors = FALSE
  )
}
