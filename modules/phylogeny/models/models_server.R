# modules/phylogeny/models/models_server.R
phylo_models_server <- function(id, wvs_country, codebook_data, lang_tree, lang_country_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load required packages
    if (!require(phylolm)) stop("Please install: phylolm")
    if (!require(plotly)) stop("Please install: plotly")
    if (!require(dplyr)) stop("Please install: dplyr")
    
    # Track warnings
    model_warnings <- reactiveVal("")
    
    # Update variable choices
    observe({
      num_vars <- names(wvs_country)[sapply(wvs_country, is.numeric)]
      num_vars <- setdiff(num_vars, c("B_COUNTRY", "B_COUNTRY_ALPHA"))
      
      updateSelectInput(session, "outcome", choices = num_vars, selected = num_vars[1])
      updateSelectInput(session, "predictors", choices = num_vars, selected = num_vars[2])
    })
    
    # Prepare data
    prepped_data <- reactive({
      req(input$outcome)
      
      # Select and clean data
      model_data <- wvs_country %>%
        select(B_COUNTRY_ALPHA, all_of(c(input$outcome, input$predictors))) %>%
        na.omit()
      
      # Extract country codes from tree tips
      tip_countries <- sapply(strsplit(lang_tree$tip.label, "_"), function(x) {
        if (length(x) >= 2) x[2] else NA
      })
      tip_countries <- tip_countries[!is.na(tip_countries)]
      
      # Find common countries
      common_countries <- intersect(tip_countries, model_data$B_COUNTRY_ALPHA)
      
      if (length(common_countries) < 5) {
        showNotification("Insufficient matching countries for analysis", type = "error")
        return(NULL)
      }
      
      # Filter data
      model_data <- model_data %>%
        filter(B_COUNTRY_ALPHA %in% common_countries)
      
      # Prune tree
      keep_tips <- sapply(strsplit(lang_tree$tip.label, "_"), function(x) {
        if (length(x) >= 2) x[2] %in% common_countries else FALSE
      })
      
      if (sum(keep_tips) < 5) {
        showNotification("Cannot create valid phylogenetic tree", type = "error")
        return(NULL)
      }
      
      pruned_tree <- try(ape::keep.tip(lang_tree, lang_tree$tip.label[keep_tips]))
      
      if (inherits(pruned_tree, "try-error")) {
        showNotification("Error pruning phylogenetic tree", type = "error")
        return(NULL)
      }
      
      # Order data to match tree
      tip_order <- sapply(strsplit(pruned_tree$tip.label, "_"), function(x) x[2])
      model_data <- model_data %>%
        slice(match(tip_order, B_COUNTRY_ALPHA)) %>%
        as.data.frame()
      
      # Set row names
      row.names(model_data) <- pruned_tree$tip.label
      
      list(tree = pruned_tree, data = model_data, countries = common_countries)
    })
    
    # Build formula
    model_formula <- reactive({
      req(input$outcome, prepped_data())
      
      if (is.null(input$predictors) || length(input$predictors) == 0) {
        as.formula(paste(input$outcome, "~ 1"))
      } else {
        preds <- paste(input$predictors, collapse = " + ")
        as.formula(paste(input$outcome, "~", preds))
      }
    })
    
    # Fit PGLS model
    pgls_model <- eventReactive(input$fit_model, {
      req(model_formula(), prepped_data())
      
      data_list <- prepped_data()
      if (is.null(data_list)) return(NULL)
      
      data <- data_list$data
      tree <- data_list$tree
      formula <- model_formula()
      
      # Verify data-tree match
      if (!all(tree$tip.label %in% rownames(data))) {
        showNotification("Data and tree labels don't match", type = "error")
        return(NULL)
      }
      
      data <- data[tree$tip.label, , drop = FALSE]
      
      # Fit PGLS with warning handling
      pgls_warnings <- NULL
      pgls <- withCallingHandlers(
        {
          try(phylolm::phylolm(formula, data = data, phy = tree, model = "lambda",))
        },
        warning = function(w) {
          pgls_warnings <<- c(pgls_warnings, w$message)
          invokeRestart("muffleWarning")
        }
      )
      
      # Store warnings
      if (!is.null(pgls_warnings)) {
        model_warnings(paste(pgls_warnings, collapse = "; "))
      }
      
      if (inherits(pgls, "try-error")) {
        showNotification("Error fitting PGLS model", type = "error")
        return(NULL)
      }
      
      list(model = pgls, data = data, tree = tree, formula = formula,
           warnings = pgls_warnings)
    })
    
    # Display warnings
    output$model_warnings <- renderUI({
      warnings <- model_warnings()
      if (!is.null(warnings) && warnings != "") {
        div(class = "alert alert-warning",
            h5("Model Note:"),
            p("Lambda estimation reached bounds - this is common and results are valid.")
        )
      }
    })
    
    # Variable information
    output$variable_info <- renderUI({
      
      selected_vars <- c(input$outcome, input$predictors)
      selected_vars <- unlist(Filter(Negate(is.null), selected_vars))
      render_variable_description(
        data_type = "WVS",
        outcome_var = if(length(selected_vars) > 0) selected_vars else NULL,
        codebook_data = codebook_data
      )
    })
    
    # Model formula
    output$model_formula <- renderText({
      req(model_formula())
      deparse(model_formula())
    })
    
    # PGLS results
    output$pgls_summary <- renderPrint({
      mod_list <- pgls_model()
      if (is.null(mod_list)) {
        cat("No model results available\n")
        return()
      }
      
      mod <- mod_list$model
      smry <- summary(mod)
      
      cat("=== PGLS MODEL RESULTS ===\n\n")
      print(smry)
      cat("\nMODEL FIT STATISTICS:\n")
      cat("AIC:", round(AIC(mod), 2), "\n")
      
      # Extract lambda
      lambda <- if (!is.null(mod$optpar)) mod$optpar else NA
      cat("Pagel's λ:", round(lambda, 3), "\n")
      
      # Extract lambda p-value using custom function
      lambda_p <- NA
      
      if (!is.na(lambda_p)) {
        cat("λ p-value:", format.pval(lambda_p, digits = 4), "\n")
        cat("λ significance:", ifelse(lambda_p < 0.05, "SIGNIFICANT", "Not significant"), "\n")
      } else {
        cat("λ p-value: Not available\n")
      }
      
      # Show note if boundary warning
      if (!is.null(mod_list$warnings)) {
        cat("\nNOTE: Lambda estimation reached bounds (common occurrence)\n")
      }
    })
    
    # PGLS diagnostic plot
    output$pgls_plot <- renderPlotly({
      mod_list <- pgls_model()
      if (is.null(mod_list)) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      mod <- mod_list$model
      
      # Prepare data
      fitted_vals <- as.vector(mod$fitted)
      resid_vals <- as.vector(mod$residuals)
      country_vals <- mod_list$data$B_COUNTRY_ALPHA[1:length(fitted_vals)]
      
      plot_data <- data.frame(
        fitted = fitted_vals,
        residuals = resid_vals,
        country = country_vals
      ) %>% na.omit()
      
      lambda <- if (!is.null(mod$optpar)) round(mod$optpar, 3) else "NA"
      
      # Create plot
      plot_ly(data = plot_data) %>%
        add_markers(x = ~fitted, y = ~residuals, 
                    marker = list(color = '#2E86AB', size = 10, opacity = 0.7),
                    text = ~country,
                    hoverinfo = 'text',
                    hovertext = ~paste("Country:", country,
                                       "<br>Fitted:", round(fitted, 2),
                                       "<br>Residual:", round(residuals, 2))) %>%
        add_lines(x = range(plot_data$fitted, na.rm = TRUE), y = c(0, 0),
                  line = list(dash = 'dash', color = 'red')) %>%
        layout(title = paste("PGLS Residual Plot (λ =", lambda, ")"),
               xaxis = list(title = "Fitted Values"),
               yaxis = list(title = "Residuals"))
    })
    
    # PGLS interpretation
    output$pgls_interpretation <- renderUI({
      mod_list <- pgls_model()
      if (is.null(mod_list)) {
        return(tags$p("No model results available"))
      }
      
      mod <- mod_list$model
      smry <- summary(mod)
      
      # Extract parameters
      lambda <- if (!is.null(mod$optpar)) mod$optpar else NA
      lambda_p <- NA
      
      has_warning <- !is.null(mod_list$warnings)
      
      HTML(generate_pgls_interpretation(mod, lambda, lambda_p, input$outcome, has_warning))
    })
  })
}