# modules/phylo_viz/tree/tree_server.R
phylo_viz_tree_server <- function(id,
                                  wvs_data,
                                  codebook_data,
                                  lang_tree,
                                  lang_country_map,
                                  grouped_vars = NULL) {
  moduleServer(id, function(input, output, session) {

    # Download image size reactives. Keep values capped so exporting does not
    # become too slow on the Shiny server. These values are used only for the
    # downloaded PNG, not for the on-page plot.
    download_width <- reactive({
      min(max(input$download_width %||% 1400, 800), 2400)
    })

    download_height <- reactive({
      min(max(input$download_height %||% 1200, 600), 3000)
    })

    # Build grouped WVS variable choices.
    # The selector returns real column IDs from wvs_data, while labels remain readable.
    make_grouped_tree_choices <- function(grouped_vars, wvs_data, codebook_data) {
      numeric_vars <- names(wvs_data)[vapply(wvs_data, is.numeric, logical(1))]
      numeric_vars <- setdiff(numeric_vars, "geometry")

      # Fallback: all numeric variables in the country-level WVS data
      if (is.null(grouped_vars)) {
        disp <- vapply(
          numeric_vars,
          function(v) wvs_var_display(v, codebook_data),
          FUN.VALUE = character(1)
        )
        return(stats::setNames(numeric_vars, disp))
      }

      out <- lapply(grouped_vars, function(x) {
        raw_values <- unname(as.character(x))
        raw_names  <- names(x)
        if (is.null(raw_names)) raw_names <- rep(NA_character_, length(raw_values))

        ids <- vapply(seq_along(raw_values), function(i) {
          value_i <- raw_values[[i]]
          name_i  <- raw_names[[i]]

          # Case 1: grouped item already stores the column ID as its value
          if (!is.na(value_i) && value_i %in% codebook_data$Col_ID) {
            return(value_i)
          }

          # Case 2: grouped item stores the display label/ColLab as its value
          idx_value_collab <- match(value_i, codebook_data$ColLab)
          if (!is.na(idx_value_collab)) {
            return(codebook_data$Col_ID[[idx_value_collab]])
          }

          # Case 3: grouped item is a named vector where the name is a column ID
          if (!is.na(name_i) && name_i %in% codebook_data$Col_ID) {
            return(name_i)
          }

          # Case 4: grouped item is a named vector where the name is the display label/ColLab
          idx_name_collab <- match(name_i, codebook_data$ColLab)
          if (!is.na(idx_name_collab)) {
            return(codebook_data$Col_ID[[idx_name_collab]])
          }

          NA_character_
        }, FUN.VALUE = character(1))

        ids <- stats::na.omit(ids)
        ids <- unique(ids)
        ids <- ids[ids %in% numeric_vars]

        if (length(ids) == 0) {
          return(NULL)
        }

        stats::setNames(
          ids,
          vapply(
            ids,
            function(v) wvs_var_display(v, codebook_data),
            FUN.VALUE = character(1)
          )
        )
      })

      out <- out[!vapply(out, is.null, logical(1))]

      # If grouped_minus_ignored does not match the country-level columns for any
      # reason, fall back to all available numeric WVS variables so the selector
      # is never empty.
      if (length(out) == 0) {
        disp <- vapply(
          numeric_vars,
          function(v) wvs_var_display(v, codebook_data),
          FUN.VALUE = character(1)
        )
        return(stats::setNames(numeric_vars, disp))
      }

      out
    }

    # Populate WVS variable list from grouped_minus_ignored-style grouped choices
    observe({
      var_choices <- make_grouped_tree_choices(
        grouped_vars  = grouped_vars,
        wvs_data      = wvs_data,
        codebook_data = codebook_data
      )

      # Do not pre-select all three slots. If maxItems is already full, selectize
      # will not open the dropdown until a variable is removed, which makes it
      # look like there are no alternative options. Start with one selected item
      # so users can immediately open the menu and add/change variables.
      first_choice <- unname(unlist(var_choices, recursive = FALSE, use.names = FALSE))[1]
      if (is.na(first_choice)) {
        first_choice <- character(0)
      }

      updateSelectizeInput(
        session,
        "outcome_vars",
        choices = var_choices,
        selected = first_choice,
        server = TRUE
      )
    })

    # Dynamic palette dropdowns for each selected variable (max 3)
    output$var_palette_ui <- renderUI({
      vars <- input$outcome_vars
      if (is.null(vars) || length(vars) == 0) return(NULL)

      if (length(vars) > 3) vars <- vars[1:3]

      palette_choices <- default_palette_sequence()

      tagList(lapply(vars, function(v) {
        selectInput(
          session$ns(paste0("palette__", v)),
          label = paste0("Palette for ", wvs_var_display(v, codebook_data), ":"),
          choices = palette_choices,
          selected = default_palette_for_var(v, vars)
        )
      }))
    })

    # Build plot only when Generate Plot is clicked (keeps app responsive)
    tree_plot_obj <- eventReactive(input$update_plot, {
      req(lang_tree, lang_country_map)

      vars <- input$outcome_vars
      req(vars)
      if (length(vars) == 0) stop("Please select at least one WVS variable.")
      if (length(vars) > 3) vars <- vars[1:3]

      # Collect per-variable palette choices (defaulting by position)
      palettes_by_var <- stats::setNames(vector("list", length(vars)), vars)
      for (v in vars) {
        key <- paste0("palette__", v)
        palettes_by_var[[v]] <- input[[key]] %||% default_palette_for_var(v, vars)
      }

      build_ggtree_multi_bar_plot(
        tree = lang_tree,
        wvs_data = wvs_data,
        lang_country_map = lang_country_map,
        outcome_vars = vars,
        palettes_by_var = palettes_by_var,
        layout = input$tree_layout,
        show_tip_labels = input$show_tip_labels,
        tip_label_fields = input$tip_label_fields,
        tip_label_sep = input$tip_label_sep,
        tip_label_size = input$tip_label_size,
        bar_panel_width = input$bar_panel_width,
        bar_panel_gap = input$bar_panel_gap,
        show_legends = input$show_legends
      )
    })

    # Render the on-page plot at the previous fixed display scale. The download
    # sliders below the plot affect only the exported PNG dimensions.
    output$tree_plot <- renderPlot({
      tree_plot_obj()
    }, height = 1100)

    # Download the current generated plot as a PNG image
    output$download_tree_image <- downloadHandler(
      filename = function() {
        paste0("phylogeny-tree-", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- tree_plot_obj()

        # Export at the same pixel canvas size selected in the UI. A moderate
        # DPI keeps file sizes and server time reasonable while preserving detail.
        export_dpi <- 150
        plot_width_in <- download_width() / export_dpi
        plot_height_in <- download_height() / export_dpi

        ggplot2::ggsave(
          filename = file,
          plot = p,
          device = "png",
          width = plot_width_in,
          height = plot_height_in,
          units = "in",
          dpi = export_dpi,
          bg = "white",
          limitsize = FALSE
        )
      }
    )
  })
}
