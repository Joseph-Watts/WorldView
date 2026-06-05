# modules/geo_viz/map/map_server.R

geo_viz_map_server <- function(id, wvs_country, codebook_data, world_shape, country_phylogeny,
                               grouped_vars = NULL) {
  moduleServer(id, function(input, output, session) {
    world_data <- shiny::reactive({
      join_world_data(world_shape, wvs_country, country_phylogeny)
    })
    
    numeric_vars <- shiny::reactive({
      sf1 <- world_data()
      cols <- names(sf1)[vapply(sf1, is.numeric, logical(1))]
      cols <- setdiff(cols, c("scalerank", "labelrank", "pop_est", "gdp_md", "pop_year", "lastcensus"))
      cols[vapply(sf1[cols], function(x) any(!is.na(x)), logical(1))]
    })
    
    make_grouped_map_choices <- function(grouped_vars, world_sf, codebook_data) {
      # Fallback: use all numeric variables if no grouped variable list is supplied.
      if (is.null(grouped_vars)) {
        vars <- numeric_vars()
        return(stats::setNames(
          vars,
          vapply(vars, map_var_display, character(1), codebook_data = codebook_data)
        ))
      }
      
      # grouped_minus_ignored is expected to be a list, usually grouped by section.
      # Each element may contain Col_ID values, ColLab labels, or a mix of both.
      choices <- lapply(grouped_vars, function(x) {
        x <- as.character(x)
        
        ids <- ifelse(
          x %in% codebook_data$Col_ID,
          x,
          codebook_data$Col_ID[match(x, codebook_data$ColLab)]
        )
        
        ids <- stats::na.omit(ids)
        ids <- ids[ids %in% names(world_sf)]
        ids <- ids[vapply(world_sf[ids], is.numeric, logical(1))]
        ids <- ids[vapply(world_sf[ids], function(z) any(!is.na(z)), logical(1))]
        ids <- unique(ids)
        
        stats::setNames(
          ids,
          vapply(ids, map_var_display, character(1), codebook_data = codebook_data)
        )
      })
      
      choices <- choices[lengths(choices) > 0]
      
      # If none of the grouped variables are available in the joined map data,
      # fall back to all numeric country-level variables so the selector is not empty.
      if (length(choices) == 0) {
        vars <- numeric_vars()
        choices <- stats::setNames(
          vars,
          vapply(vars, map_var_display, character(1), codebook_data = codebook_data)
        )
      }
      
      choices
    }
    
    shiny::observe({
      sf1 <- world_data()
      choices <- make_grouped_map_choices(
        grouped_vars  = grouped_vars,
        world_sf      = sf1,
        codebook_data = codebook_data
      )
      
      first_choice <- unname(unlist(choices, recursive = FALSE, use.names = FALSE))[1]
      if (is.na(first_choice)) {
        first_choice <- character(0)
      }
      
      shiny::updateSelectizeInput(
        session,
        "map_var",
        choices = choices,
        selected = first_choice,
        server = TRUE
      )
    })
    
    output$var_description <- shiny::renderUI({
      req(input$map_var)
      selected_var_ui <- htmltools::tags$div(
        htmltools::tags$span("Selected variable: "),
        htmltools::tags$b(map_var_display(input$map_var, codebook_data))
      )
      
      if (!is.null(codebook_data) && all(c("Col_ID", "Question") %in% names(codebook_data))) {
        row <- codebook_data[codebook_data$Col_ID == input$map_var, , drop = FALSE]
        if (nrow(row) > 0) {
          return(htmltools::tagList(
            selected_var_ui,
            htmltools::tags$p(row$Question[[1]])
          ))
        }
      }
      
      selected_var_ui
    })
    
    output$map <- leaflet::renderLeaflet({
      sf1 <- world_data()
      req(nrow(sf1) > 0)
      
      leaflet::leaflet(sf1, options = leaflet::leafletOptions(worldCopyJump = TRUE)) %>%
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
        add_variable_polygons(
          sf1 = sf1,
          var = input$map_var,
          codebook_data = codebook_data,
          palette_option = input$map_palette %||% "viridis"
        ) %>%
        leaflet::setView(lng = 0, lat = 20, zoom = 2)
    })
    

    output$map_legend <- shiny::renderUI({
      sf1 <- world_data()
      req(nrow(sf1) > 0, input$map_var)
      legend_ui <- build_map_legend_ui(
        world_sf = sf1,
        var = input$map_var,
        codebook_data = codebook_data,
        palette_option = input$map_palette %||% "viridis"
      )
      shiny::validate(shiny::need(!is.null(legend_ui), "No finite values available for the selected variable."))
      legend_ui
    })
    
    output$download_map <- shiny::downloadHandler(
      filename = function() {
        var_stub <- gsub("[^A-Za-z0-9_-]+", "_", input$map_var %||% "map")
        paste0("world_map_", var_stub, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        sf1 <- world_data()
        req(nrow(sf1) > 0, input$map_var)
        p <- build_static_map_plot(
          world_sf = sf1,
          var = input$map_var,
          codebook_data = codebook_data,
          palette_option = input$map_palette %||% "viridis"
        )
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = 12,
          height = 7,
          dpi = 300,
          bg = "white"
        )
      }
    )

    shiny::observeEvent(list(input$map_var, input$map_palette), {
      sf1 <- world_data()
      req(nrow(sf1) > 0)
      
      leaflet::leafletProxy("map", session = session, data = sf1) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        add_variable_polygons(
          sf1 = sf1,
          var = input$map_var,
          codebook_data = codebook_data,
          palette_option = input$map_palette %||% "viridis"
        )
    }, ignoreInit = TRUE)
  })
}
