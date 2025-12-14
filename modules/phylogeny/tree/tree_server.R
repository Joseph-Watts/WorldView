# modules/phylogeny/tree/tree_server.R
phylo_tree_server <- function(id,
                              wvs_data,
                              codebook_data,
                              lang_tree,
                              lang_country_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive expression for plot height
    plot_height <- reactive({
      input$plot_height
    })
    
    # Update variable choices based on selected data type
    observe({
      if (input$data_type == "wvs") {
        num_vars <- names(wvs_data)[sapply(wvs_data, is.numeric)]
        updateSelectInput(session, "outcome_var", choices = num_vars)
      } else {
        updateSelectInput(session, "outcome_var", choices = character(0))
      }
    })
    
    # Render variable description
    output$var_description <- renderUI({
      render_variable_description(
        data_type = input$data_type,
        outcome_var = input$outcome_var,
        codebook_data = codebook_data
      )
    })
    
    # Create a reactive values object to store current settings
    current_settings <- reactiveValues(
      data_type = "wvs",
      outcome_var = NULL,
      language_format = "iso639P3",
      country_format = "iso3166alpha3",
      tree_layout = "phylogram",
      tip_label_size = 1,
      color_scheme = "viridis",
      show_legend = TRUE
    )
    
    # Update settings only when update button is clicked
    observeEvent(input$update_plot, {
      current_settings$data_type <- input$data_type
      current_settings$outcome_var <- input$outcome_var
      current_settings$language_format <- input$language_format
      current_settings$country_format <- input$country_format
      current_settings$tree_layout <- input$tree_layout
      current_settings$tip_label_size <- input$tip_label_size
      current_settings$color_scheme <- input$color_scheme
      current_settings$show_legend <- input$show_legend
    })
    
    # Main tree plotting function - depends on current_settings
    tree_plot <- eventReactive(input$update_plot, {
      req(lang_tree, lang_country_map)
      
      # Use current settings instead of direct input values
      if (current_settings$data_type == "blank") {
        # Plot blank tree
        plot_blank_tree(
          tree = lang_tree,
          layout_type = current_settings$tree_layout,
          tip_label_size = current_settings$tip_label_size,
          lang_country_map = lang_country_map,
          language_format = current_settings$language_format,
          country_format = current_settings$country_format
        )
      } else {
        # Plot tree with WVS7 data
        req(current_settings$outcome_var)
        
        # Prepare tree data with WVS7 data
        tree_data <- prepare_wvs_tree_data(
          tree = lang_tree,
          wvs_data = wvs_data,
          lang_country_map = lang_country_map,
          outcome_var = current_settings$outcome_var,
          language_format = current_settings$language_format,
          country_format = current_settings$country_format
        )
        
        # Generate the plot
        plot_wvs_tree(
          tree_data = tree_data,
          layout_type = current_settings$tree_layout,
          tip_label_size = current_settings$tip_label_size,
          color_scheme = current_settings$color_scheme,
          show_legend = current_settings$show_legend,
          outcome_var = current_settings$outcome_var
        )
      }
    })
    
    # Render the plot with dynamic height
    output$tree_plot <- renderPlot({
      tree_plot()
    }, height = plot_height)
  })
}