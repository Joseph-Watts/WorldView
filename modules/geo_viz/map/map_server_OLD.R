# modules/geo_viz/map/map_server.R

geo_viz_map_server <- function(id, wvs_country, 
                               codebook_data, 
                               world_shape, 
                               country_phylogeny,
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
    
    shiny::observe({
      vars <- numeric_vars()
      choices <- stats::setNames(
        vars,
        vapply(vars, map_var_display, character(1), codebook_data = codebook_data)
      )
      
      shiny::updateSelectizeInput(
        session,
        "map_var",
        choices = choices,
        selected = if (length(vars) > 0) vars[[1]] else character(0),
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
          codebook_data = codebook_data
        ) %>%
        leaflet::setView(lng = 0, lat = 20, zoom = 2)
    })
    
    shiny::observeEvent(input$map_var, {
      sf1 <- world_data()
      req(nrow(sf1) > 0)
      
      leaflet::leafletProxy("map", session = session, data = sf1) %>%
        leaflet::clearShapes() %>%
        leaflet::clearControls() %>%
        add_variable_polygons(
          sf1 = sf1,
          var = input$map_var,
          codebook_data = codebook_data
        )
    }, ignoreInit = TRUE)
  })
}
