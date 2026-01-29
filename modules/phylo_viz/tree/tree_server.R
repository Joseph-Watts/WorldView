# modules/phylo_viz/tree/tree_server.R
phylo_viz_tree_server <- function(id,
                              wvs_data,
                              codebook_data,
                              lang_tree,
                              lang_country_map) {
  moduleServer(id, function(input, output, session) {
    
    # Plot height reactive
    plot_height <- reactive(input$plot_height)
    
    # Populate WVS variable list (numeric only)
    observe({
      if (input$data_type == "wvs") {
        num_vars <- names(wvs_data)[vapply(wvs_data, is.numeric, logical(1))]
        disp <- vapply(num_vars, function(v) wvs_var_display(v, codebook_data), FUN.VALUE = character(1))
        
        # names = display text, values = real col_id (input$map_vars still returns col_id)
        choices_named <- stats::setNames(num_vars, disp)
        
        updateSelectizeInput(session, "outcome_vars", 
                             choices = choices_named, 
                             selected = head(num_vars, 2),
                             server = TRUE)
      } else {
        updateSelectizeInput(
          session, "outcome_vars",
          choices = character(0),
          selected = character(0),
          server = TRUE
        )
      }
    })
    
    # Dynamic palette dropdowns for each selected variable (max 8)
    output$var_palette_ui <- renderUI({
      if (input$data_type != "wvs") return(NULL)
      
      vars <- input$outcome_vars
      if (is.null(vars) || length(vars) == 0) return(NULL)
      
      # UI should enforce maxItems=5; keep a safety cap anyway
      if (length(vars) > 5) vars <- vars[1:5]
      
      palette_choices <- default_palette_sequence()
      
      tagList(lapply(vars, function(v) {
        selectInput(
          session$ns(paste0("palette__", v)),
          label = paste0("Palette for ", v, ":"),
          choices = palette_choices,
          selected = default_palette_for_var(v, vars)
        )
      }))
    })
    
    # Variable description panel (multi-variable)
    output$var_description <- renderUI({
      if (input$data_type != "wvs") {
        return(HTML("<b>Base tree:</b> no WVS variables selected."))
      }
      
      vars <- input$outcome_vars
      if (is.null(vars) || length(vars) == 0) {
        return(HTML("Select up to 5 WVS variables to show bars on the right."))
      }
      if (length(vars) > 5) vars <- vars[1:5]
      
      disp <- vapply(
        vars,
        function(v) wvs_var_display(v, codebook_data),
        FUN.VALUE = character(1)
      )
      
      items <- paste0(
        "<li><b>", htmltools::htmlEscape(disp), "</b></li>",
        collapse = ""
      )
      
      HTML(paste0("<ul>", items, "</ul>"))
    })
    
    # Build plot only when Update Plot is clicked (keeps app responsive)
    tree_plot_obj <- eventReactive(input$update_plot, {
      req(lang_tree, lang_country_map)
      
      # Base tree (no bars)
      if (input$data_type == "blank") {
        return(
          build_ggtree_multi_bar_plot(
            tree = lang_tree,
            wvs_data = wvs_data,
            lang_country_map = lang_country_map,
            outcome_vars = character(0),
            palettes_by_var = list(),
            layout = input$tree_layout,
            show_tip_labels = input$show_tip_labels,
            tip_label_fields = input$tip_label_fields,
            tip_label_sep = input$tip_label_sep,
            tip_label_size = input$tip_label_size,
            bar_panel_width = input$bar_panel_width,
            bar_panel_gap = input$bar_panel_gap,
            show_legends = input$show_legends
          )
        )
      }
      
      # WVS mode: require at least 1 variable
      vars <- input$outcome_vars
      req(vars)
      if (length(vars) == 0) stop("Please select at least one WVS variable.")
      if (length(vars) > 5) vars <- vars[1:5]  # safety
      
      # Collect per-variable palette choices (defaulting by position)
      palettes_by_var <- setNames(vector("list", length(vars)), vars)
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
    
    # Render plot
    output$tree_plot <- renderPlot({
      tree_plot_obj()
    }, height = plot_height)
  })
}
