# modules/geo_viz/map/map_server.R

geo_viz_map_server <- function(id, indiv_data, codebook_data, world_shape, country_phylogeny,
                               grouped_vars = NULL) {
  moduleServer(id, function(input, output, session) {
    map_variable_ids <- shiny::reactive({
      ids <- names(indiv_data)
      ids <- setdiff(ids, c("B_COUNTRY", "B_COUNTRY_ALPHA", "S007"))

      ids <- ids[vapply(indiv_data[ids], function(x) {
        is.numeric(x) || is.integer(x) || is.ordered(x) || is.factor(x)
      }, logical(1))]

      ids[vapply(indiv_data[ids], function(x) any(!is.na(x)), logical(1))]
    })

    make_grouped_map_choices <- function(grouped_vars, variable_ids, codebook_data) {
      # Fallback: use all map-compatible individual-level variables if no grouped
      # variable list is supplied.
      if (is.null(grouped_vars)) {
        return(stats::setNames(
          variable_ids,
          vapply(variable_ids, map_var_display, character(1), codebook_data = codebook_data)
        ))
      }

      # grouped_vars is usually a list grouped by codebook section. Each element
      # may contain Col_ID values, ColLab labels, or a mix of both.
      choices <- lapply(grouped_vars, function(x) {
        x <- as.character(x)

        ids <- ifelse(
          x %in% codebook_data$Col_ID,
          x,
          codebook_data$Col_ID[match(x, codebook_data$ColLab)]
        )

        ids <- stats::na.omit(ids)
        ids <- ids[ids %in% variable_ids]
        ids <- unique(ids)

        stats::setNames(
          ids,
          vapply(ids, map_var_display, character(1), codebook_data = codebook_data)
        )
      })

      choices <- choices[lengths(choices) > 0]

      # If none of the grouped variables are available in the individual data,
      # fall back to all map-compatible individual-level variables so the selector
      # is not empty.
      if (length(choices) == 0) {
        choices <- stats::setNames(
          variable_ids,
          vapply(variable_ids, map_var_display, character(1), codebook_data = codebook_data)
        )
      }

      choices
    }

    selected_var_type <- shiny::reactive({
      req(input$map_var)
      map_variable_type(indiv_data[[input$map_var]])
    })

    selected_factor_levels <- shiny::reactive({
      req(input$map_var)
      x <- indiv_data[[input$map_var]]
      if (!is.factor(x) || is.ordered(x)) {
        return(character(0))
      }
      levs <- levels(x)
      levs[vapply(levs, function(level) any(!is.na(x) & x == level), logical(1))]
    })

    selected_factor_level <- shiny::reactive({
      if (!identical(selected_var_type(), "factor")) {
        return(NULL)
      }
      levels <- selected_factor_levels()
      req(length(levels) > 0)

      level <- input$map_factor_level
      if (is.null(level) || length(level) != 1 || is.na(level) || !(level %in% levels)) {
        return(levels[[1]])
      }
      level
    })

    world_data <- shiny::reactive({
      req(input$map_var)
      join_world_data_from_individuals(
        world_shape       = world_shape,
        indiv_data        = indiv_data,
        country_phylogeny = country_phylogeny,
        var               = input$map_var,
        factor_level      = selected_factor_level(),
        codebook_data     = codebook_data
      )
    })

    shiny::observe({
      variable_ids <- map_variable_ids()
      choices <- make_grouped_map_choices(
        grouped_vars  = grouped_vars,
        variable_ids   = variable_ids,
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

    shiny::observeEvent(input$map_var, {
      levels <- selected_factor_levels()
      selected <- if (length(levels) > 0) levels[[1]] else character(0)
      shiny::updateSelectizeInput(
        session,
        "map_factor_level",
        choices = levels,
        selected = selected,
        server = TRUE
      )
    }, ignoreInit = FALSE)

    output$map_factor_level_ui <- shiny::renderUI({
      req(input$map_var)
      if (!identical(selected_var_type(), "factor")) {
        return(NULL)
      }

      selectizeInput(
        session$ns("map_factor_level"),
        "Factor level to show:",
        choices = selected_factor_levels(),
        selected = selected_factor_level(),
        multiple = FALSE,
        options = list(placeholder = "Select one response level")
      )
    })

    output$var_description <- shiny::renderUI({
      req(input$map_var)
      selected_var_ui <- htmltools::tags$div(
        htmltools::tags$span("Selected variable: "),
        htmltools::tags$b(map_var_display(input$map_var, codebook_data))
      )

      measure_ui <- switch(
        selected_var_type(),
        factor = htmltools::tags$p(
          "Map value: proportion of non-missing responses equal to ",
          htmltools::tags$b(selected_factor_level()),
          "."
        ),
        ordered = htmltools::tags$p("Map value: country mean of the ordered response scale."),
        numeric = htmltools::tags$p("Map value: country mean of the numeric response."),
        NULL
      )

      if (!is.null(codebook_data) && all(c("Col_ID", "Question") %in% names(codebook_data))) {
        row <- codebook_data[codebook_data$Col_ID == input$map_var, , drop = FALSE]
        if (nrow(row) > 0) {
          return(htmltools::tagList(
            selected_var_ui,
            htmltools::tags$p(row$Question[[1]]),
            measure_ui
          ))
        }
      }

      htmltools::tagList(selected_var_ui, measure_ui)
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
        level_stub <- if (identical(selected_var_type(), "factor")) {
          paste0("_", gsub("[^A-Za-z0-9_-]+", "_", selected_factor_level()))
        } else {
          ""
        }
        var_stub <- gsub("[^A-Za-z0-9_-]+", "_", input$map_var %||% "map")
        paste0("world_map_", var_stub, level_stub, "_", Sys.Date(), ".png")
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

    shiny::observeEvent(list(input$map_var, input$map_factor_level, input$map_palette), {
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
