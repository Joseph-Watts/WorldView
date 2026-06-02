# modules/geo_viz/map/map_server.R

geo_viz_map_server <- function(id,
                             wvs_country,
                             codebook_data,
                             world_shape,
                             country_phylogeny) {
  
  moduleServer(id, function(input, output, session) {
    
    # --- 1) numeric variable choices ---
    observe({
      num_vars <- names(wvs_country)[vapply(wvs_country, is.numeric, logical(1))]
      disp <- vapply(num_vars, function(v) wvs_var_display(v, codebook_data), FUN.VALUE = character(1))
      # names = display text, values = real col_id (input$map_vars still returns col_id)
      choices_named <- stats::setNames(num_vars, disp)
      updateSelectizeInput(
        session, "map_vars",
        choices  = choices_named,
        selected = head(num_vars, 2),
        server   = TRUE
      )
    })
    
    # --- 2) join once ---
    world_sf <- reactive({
      join_world_data(world_shape, wvs_country, country_phylogeny)
    })
    
    # --- 3) Update button: only update after click ---
    settings <- eventReactive(input$btn_update, {
      list(
        vars        = input$map_vars %||% character(),
        bg_mode     = input$bg_mode %||% "none",
        show_charts = isTRUE(input$show_charts),
        chart_type  = input$chart_type %||% "bar",
        chart_size  = input$chart_size %||% 38
      )
    }, ignoreNULL = FALSE)
    
    # --- 4) background legend in Data control (full list, scrollable) ---
    output$bg_legend_full <- renderUI({
      req(world_sf())
      s <- settings()
      build_bg_legend_full_ui(world_sf(), s$bg_mode)
    })
    
    # --- 5) Variable description ---
    output$var_description <- renderUI({
      vars <- input$map_vars %||% character(0)
      
      if (length(vars) == 0) return(HTML("<i>No variables selected.</i>"))
      
      disp <- vapply(
        vars,
        function(v) wvs_var_display(v, codebook_data),
        FUN.VALUE = character(1)
      )
      
      items <- lapply(seq_along(vars), function(i) {
        tags$li(tags$b(htmltools::htmlEscape(disp[i])))
      })
      
      tagList(
        tags$div(
          style = "margin-bottom:6px;",
          tags$small(
            style = "opacity:.8;",
            "Hover a country to see ISO3, glottocode, language/family and selected values."
          )
        ),
        tags$ul(items)
      )
    })
    
    # --- 6) chart legend plot (outside map), always Category10 ---
    output$charts_legend <- renderPlot({
      s <- settings()
      req(world_sf())
      
      if (!isTRUE(s$show_charts) || length(s$vars) == 0) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "Charts hidden or no variables selected.")
        return(invisible())
      }
      
      cols <- rep(palette_category10(), length.out = length(s$vars))
      df <- make_chart_legend_df(world_sf(), s$vars, cols)
      
      op <- par(mar = c(3, 2, 2, 1))
      on.exit(par(op), add = TRUE)
      
      plot.new()
      plot.window(xlim = c(0, 1), ylim = c(0, 1))
      title(main = "Chart legend (Category10 + value ranges)", cex.main = 0.9)
      
      y0 <- seq(0.85, 0.15, length.out = nrow(df))
      for (i in seq_len(nrow(df))) {
        rect(0.03, y0[i] - 0.03, 0.08, y0[i] + 0.03, col = df$col[i], border = "grey40")
        txt <- sprintf("%s  (min=%.3f, max=%.3f)", df$var[i], df$vmin[i], df$vmax[i])
        text(0.10, y0[i], labels = txt, adj = 0, cex = 0.85)
      }
      
      # mtext("Note: chart values are scaled per variable (0–1) for comparability.", side = 1, cex = 0.75)
    })
    
    # --- 7) map ---
    output$map <- renderLeaflet({
      req(world_sf())
      s <- settings()
      sf0 <- world_sf()
      
      sf0$label <- build_country_labels(sf0, vars = s$vars, digits = 3, codebook_data = codebook_data)
      
      m <- leaflet::leaflet(sf0) %>%
        leaflet::addProviderTiles("CartoDB.Positron")
      
      # background polygons (no map legend; legend is in Data control)
      m <- add_background_polygons(m, sf0, s$bg_mode)
      
      # transparent overlay for hover/highlight (keeps background visible)
      m <- m %>% leaflet::addPolygons(
        data        = sf0,
        fillColor   = "transparent",
        fillOpacity = 0,
        weight      = 1,
        color       = "white",
        label       = ~lapply(label, htmltools::HTML),
        highlight   = leaflet::highlightOptions(
          weight = 2,
          color  = "#666",
          bringToFront = TRUE
        )
      )
      
      # charts (Category10 fixed)
      if (isTRUE(s$show_charts) && length(s$vars) > 0) {
        prep <- prepare_minicharts_data(sf0, s$vars)
        pal_vec <- palette_category10()
        
        m <- m %>%
          leaflet.minicharts::addMinicharts(
            lng          = prep$lng,
            lat          = prep$lat,
            chartdata    = prep$chart_mat,
            type         = s$chart_type,
            colorPalette = pal_vec,
            width        = s$chart_size,
            height       = s$chart_size,
            opacity      = 0.95,
            showLabels   = FALSE,
            legend       = FALSE
          )
      }
      
      m
    })
  })
}
