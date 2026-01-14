# modules/models/phylo_lm/phylo_lm_server.R
# Phylogenetic linear regression (phylolm) module server

models_phylo_lm_server <- function(id, wvs_country, codebook_data, lang_tree, lang_country_map) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # ----------------------------
    # Prepare tree+data once
    # ----------------------------
    prep <- phylo_lm_prepare_tree_and_data(
      wvs_country = wvs_country,
      lang_tree = lang_tree,
      lang_country_map = lang_country_map,
      iso3_col = "B_COUNTRY_ALPHA"
    )
    
    tree_sub <- prep$tree
    data_sub <- prep$data
    
    # ----------------------------
    # Populate choices (numeric variables only)
    # ----------------------------
    numeric_vars <- phylo_lm_list_numeric_vars(data_sub)
    choices <- phylo_lm_make_choices(codebook_data, numeric_vars)
    
    shiny::updateSelectInput(session, "dv", choices = choices)
    shiny::observeEvent(input$dv, {
      req(input$dv)
      dv <- input$dv

      choices_x <- phylo_lm_make_choices(codebook_data, setdiff(numeric_vars, dv))
      
      for (id in c("x_vars", "controls", "step_candidates")) {
        shiny::updateSelectizeInput(
          session, id,
          choices  = choices_x,
          selected = setdiff(phylo_lm_null(input[[id]], character(0)), dv),
          server   = TRUE
        )
      }
    }, ignoreInit = TRUE)
    
    # Focal variable choices depend on X selection
    shiny::observeEvent(input$x_vars, {
      x <- phylo_lm_null(input$x_vars, character(0))
      if (length(x) == 0) {
        shiny::updateSelectInput(session, "focal_var", choices = character(0))
        return()
      }
      focal_choices <- phylo_lm_make_choices(codebook_data, x)
      shiny::updateSelectInput(session, "focal_var", choices = focal_choices, selected = x[[1]])
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Reactive storage (manual model)
    # ----------------------------
    model_rv <- shiny::reactiveVal(NULL)
    info_rv <- shiny::reactiveVal(NULL)
    coef_rv <- shiny::reactiveVal(NULL)
    analysis_data_rv <- shiny::reactiveVal(NULL)
    analysis_tree_rv <- shiny::reactiveVal(NULL)
    
    # ----------------------------
    # Manual run
    # ----------------------------
    shiny::observeEvent(input$run, {
      req(input$dv)
      
      dv <- input$dv
      x_vars <- phylo_lm_null(input$x_vars, character(0))
      formula <- phylo_lm_build_formula(dv, x_vars = x_vars, controls = character(0))  # X can be empty -> Y~1
      
      vars_used <- unique(c(dv, x_vars))
      d <- data_sub
      
      if (isTRUE(input$drop_missing)) d <- phylo_lm_drop_missing(d, vars_used)
      
      # prune tree after NA drop
      keep_tips <- rownames(d)
      tree_fit <- ape::keep.tip(tree_sub, keep_tips)
      d <- d[tree_fit$tip.label, , drop = FALSE]
      
      if (isTRUE(input$standardize)) d <- phylo_lm_standardize_numeric(d, x_vars)
      
      mod <- tryCatch(phylo_lm_fit(tree_fit, d, formula), error = function(e) e)
      
      if (inherits(mod, "error")) {
        model_rv(NULL); info_rv(NULL); coef_rv(NULL)
        analysis_data_rv(NULL); analysis_tree_rv(NULL)
        
        output$model_summary_text <- shiny::renderUI({
          phylo_lm_callout("danger", shiny::tags$p(paste("Model failed:", mod$message)))
        })
        return()
      }
      
      model_rv(mod)
      info_rv(phylo_lm_glance(mod))
      coef_rv(phylo_lm_tidy_coef(mod))
      analysis_data_rv(d)
      analysis_tree_rv(tree_fit)
      
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Stepwise storage
    # ----------------------------
    step_rv <- shiny::reactiveVal(NULL)
    
    shiny::observeEvent(input$run_step, {
      req(isTRUE(input$enable_step))
      req(input$dv)
      
      dv <- input$dv
      candidates <- phylo_lm_null(input$step_candidates, character(0))
      controls <- phylo_lm_null(input$controls, character(0))
      
      if (length(candidates) == 0) {
        step_rv(list(status = "Please select candidate predictors for stepwise.", final_model = NULL, step_log = character(0)))
        return()
      }
      
      vars_used <- unique(c(dv, candidates, controls))
      d <- data_sub
      if (isTRUE(input$drop_missing)) d <- phylo_lm_drop_missing(d, vars_used)
      
      keep_tips <- rownames(d)
      tree_fit <- ape::keep.tip(tree_sub, keep_tips)
      d <- d[tree_fit$tip.label, , drop = FALSE]
      
      if (isTRUE(input$standardize)) d <- phylo_lm_standardize_numeric(d, unique(c(candidates, controls)))
      
      res <- phylo_lm_run_stepwise(
        tree = tree_fit,
        data = d,
        dv = dv,
        candidates = candidates,
        controls = controls,
        direction = input$step_direction,
        k = input$step_k,
        trace_level = 1
      )
      
      step_rv(res)
    }, ignoreInit = TRUE)
    
    # ----------------------------
    # Active model for TOP metrics (manual unless stepwise has final_model)
    # ----------------------------
    active_model <- shiny::reactive({
      s <- step_rv()
      if (isTRUE(input$enable_step) && !is.null(s) && !is.null(s$final_model)) {
        return(list(model = s$final_model, source = "Stepwise"))
      }
      m <- model_rv()
      if (!is.null(m)) return(list(model = m, source = "Manual"))
      list(model = NULL, source = "—")
    })
    
    active_info <- shiny::reactive({
      am <- active_model()
      if (is.null(am$model)) return(NULL)
      phylo_lm_glance(am$model)
    })
    
    # ----------------------------
    # Value boxes (use active model)
    # ----------------------------
    output$vb_n <- shinydashboard::renderValueBox({
      inf <- active_info()
      src <- active_model()$source
      val <- if (is.null(inf)) "—" else inf$n
      shinydashboard::valueBox(val, subtitle = paste("Countries used (n) •", src), icon = shiny::icon("flag"), color = "aqua")
    })
    
    output$vb_lambda <- shinydashboard::renderValueBox({
      inf <- active_info()
      src <- active_model()$source
      lam_txt <- if (is.null(inf)) "—" else phylo_lm_format_lambda(inf$lambda)
      
      strength <- "—"
      if (!is.null(inf) && !is.na(inf$lambda)) {
        strength <- if (inf$lambda < 0.2) "Weak" else if (inf$lambda < 0.6) "Moderate" else "Strong"
      }
      
      shinydashboard::valueBox(
        lam_txt,
        subtitle = paste("Lambda (λ):", strength, "•", src),
        icon = shiny::icon("sitemap"),
        color = "purple"
      )
    })
    
    output$vb_aic <- shinydashboard::renderValueBox({
      inf <- active_info()
      src <- active_model()$source
      aic <- if (is.null(inf) || is.na(inf$AIC)) "—" else round(inf$AIC, 2)
      shinydashboard::valueBox(aic, subtitle = paste("AIC •", src), icon = shiny::icon("chart-line"), color = "yellow")
    })
    
    output$vb_r2 <- shinydashboard::renderValueBox({
      inf <- active_info()
      src <- active_model()$source
      r2 <- if (is.null(inf) || is.na(inf$r2)) "—" else round(inf$r2, 3)
      shinydashboard::valueBox(r2, subtitle = paste("R² (approx) •", src), icon = shiny::icon("percent"), color = "green")
    })
    
    # ----------------------------
    # Summary callout (manual model context)
    # ----------------------------
    output$model_summary_text <- shiny::renderUI({
      mod <- model_rv()
      inf <- info_rv()
      if (is.null(mod) || is.null(inf)) return(shiny::helpText("Choose variables and click 'Run model'."))
      
      dv_text <- phylo_lm_choice_label(codebook_data, input$dv)
      lam <- inf$lambda
      
      msg <- if (is.na(lam)) {
        sprintf("Outcome: %s. Model fitted successfully.", dv_text)
      } else {
        strength <- if (lam < 0.2) "weak" else if (lam < 0.6) "moderate" else "strong"
        sprintf(
          "Outcome: %s. Estimated phylogenetic signal λ = %.3g (%s). Coefficients below are adjusted for language relatedness.",
          dv_text, lam, strength
        )
      }
      
      warns <- phylo_lm_null(inf$warnings, character(0))
      hit_bound <- any(grepl("lambda matches the upper/lower bound", warns, fixed = TRUE))
      
      shiny::tags$div(
        phylo_lm_callout("info", shiny::tags$p(msg)),
        if (hit_bound) phylo_lm_callout(
          "warning",
          shiny::tags$p("Note: λ hit the search boundary. This often means the phylogenetic signal is extremely weak (≈0) or extremely strong (≈1).")
        ) else NULL
      )
    })
    
    # ----------------------------
    # Printed summary (manual model)
    # ----------------------------
    output$model_summary_print <- shiny::renderText({
      mod <- model_rv()
      if (is.null(mod)) return("")
      paste(capture.output(print(summary(mod))), collapse = "\n")
    })
    
    # ----------------------------
    # Forest plot (manual model)
    # ----------------------------
    output$plot_forest <- plotly::renderPlotly({
      mod <- model_rv()
      coef_df <- coef_rv()
      req(mod, coef_df)
      
      term_labels <- vapply(coef_df$term, function(v) phylo_lm_choice_label(codebook_data, v), FUN.VALUE = character(1))
      names(term_labels) <- coef_df$term
      
      p <- phylo_lm_plot_forest_gg(coef_df, term_labels = term_labels)
      plotly::ggplotly(p, tooltip = c("y", "x"))
    })
    
    # ----------------------------
    # Marginal effect (manual model)
    # ----------------------------
    output$plot_effect <- plotly::renderPlotly({
      mod <- model_rv()
      d <- analysis_data_rv()
      req(mod, d)
      focal <- input$focal_var
      req(focal)
      
      eff <- phylo_lm_make_effect_df(mod, d, focal_var = focal, grid_n = 60)
      if (is.null(eff)) return(NULL)
      
      xlab <- phylo_lm_choice_label(codebook_data, focal)
      ylab <- paste("Predicted", phylo_lm_choice_label(codebook_data, input$dv))
      
      p <- phylo_lm_plot_effect_gg(eff, focal_label = xlab, y_label = ylab)
      plotly::ggplotly(p)
    })
    
    # ----------------------------
    # Diagnostics (manual model) with hover
    # ----------------------------
    output$plot_ovp <- plotly::renderPlotly({
      mod <- model_rv()
      d <- analysis_data_rv()
      req(mod, d)
      
      dv_lab <- phylo_lm_choice_label(codebook_data, input$dv)
      p <- phylo_lm_plot_ovp_gg(mod, meta_df = d, dv_label = dv_lab)
      plotly::ggplotly(p, tooltip = "text")
    })
    
    output$plot_resid_dist <- plotly::renderPlotly({
      mod <- model_rv()
      d <- analysis_data_rv()
      tree_fit <- analysis_tree_rv()
      req(mod, d, tree_fit)
      
      df <- phylo_lm_make_resid_distance_df(mod, tree_fit, meta_df = d, bins = 12)
      p <- phylo_lm_plot_resid_dist_gg(df)
      plotly::ggplotly(p, tooltip = "text")
    })
    
    # ----------------------------
    # Stepwise UI outputs
    # ----------------------------
    output$step_status <- shiny::renderUI({
      s <- step_rv()
      if (is.null(s)) return(shiny::helpText("Run stepwise to see results."))
      phylo_lm_callout("warning", shiny::tags$p(phylo_lm_null(s$status, "—")))
    })
    
    output$step_model_summary_print <- shiny::renderText({
      s <- step_rv()
      if (is.null(s) || is.null(s$final_model)) return("")
      paste(capture.output(print(summary(s$final_model))), collapse = "\n")
    })
    
    output$step_path_tbl <- shiny::renderTable({
      s <- step_rv()
      if (is.null(s) || is.null(s$step_log)) return(NULL)
      # Step log as a table
      data.frame(
        step = seq_along(s$step_log),
        message = s$step_log,
        stringsAsFactors = FALSE
      )
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
    
    output$plot_step_forest <- plotly::renderPlotly({
      s <- step_rv()
      if (is.null(s) || is.null(s$final_model)) return(NULL)
      
      mod <- s$final_model
      coef_df <- phylo_lm_tidy_coef(mod)
      
      term_labels <- vapply(coef_df$term, function(v) phylo_lm_choice_label(codebook_data, v), FUN.VALUE = character(1))
      names(term_labels) <- coef_df$term
      
      p <- phylo_lm_plot_forest_gg(coef_df, term_labels = term_labels)
      plotly::ggplotly(p, tooltip = c("y", "x"))
    })
    
  })
}
