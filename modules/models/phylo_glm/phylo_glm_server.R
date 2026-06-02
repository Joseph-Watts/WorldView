# modules/models/phylo_glm/phylo_glm_server.R

models_phylo_glm_server <- function(id, wvs_country, codebook_data, lang_tree, lang_country_map) {
  shiny::moduleServer(id, function(input, output, session) {
    
    prep <- phylo_glm_prepare_tree_and_data(
      wvs_country = wvs_country,
      lang_tree = lang_tree,
      lang_country_map = lang_country_map,
      iso3_col = "B_COUNTRY_ALPHA"
    )
    tree_sub <- prep$tree
    data_sub <- prep$data
    
    num_vars <- phylo_glm_list_numeric_vars(data_sub)
    dv_choices <- phylo_glm_make_choices(codebook_data, num_vars)
    x_choices  <- phylo_glm_make_choices(codebook_data, num_vars)
    
    shiny::updateSelectInput(session, "dv", choices = dv_choices)
    shiny::observeEvent(input$dv, {
      req(input$dv)
      dv <- input$dv
      
      # predictors cannot include dv
      x_choices <- phylo_glm_make_choices(codebook_data, setdiff(num_vars, dv))
      
      for (id in c("x_vars", "controls", "step_candidates")) {
        shiny::updateSelectizeInput(
          session, id,
          choices  = x_choices,
          selected = setdiff(phylo_glm_null(input[[id]], character(0)), dv),
          server   = TRUE
        )
      }
    }, ignoreInit = TRUE)
    
    shiny::observeEvent(input$x_vars, {
      x <- phylo_glm_null(input$x_vars, character(0))
      if (length(x) == 0) {
        shiny::updateSelectInput(session, "focal_var", choices = character(0))
        return()
      }
      shiny::updateSelectInput(session, "focal_var", choices = phylo_glm_make_choices(codebook_data, x), selected = x[[1]])
    }, ignoreInit = TRUE)
    
    model_rv <- shiny::reactiveVal(NULL)
    info_rv <- shiny::reactiveVal(NULL)
    coef_rv <- shiny::reactiveVal(NULL)
    analysis_data_rv <- shiny::reactiveVal(NULL)
    analysis_tree_rv <- shiny::reactiveVal(NULL)
    dv_note_rv <- shiny::reactiveVal(NULL)
    
    step_rv <- shiny::reactiveVal(NULL)
    
    dv_bin_name <- ".Ybin"
    
    # ---- Manual run
    shiny::observeEvent(input$run, {
      req(input$dv)
      
      dv <- input$dv
      x <- phylo_glm_null(input$x_vars, character(0))
      d <- data_sub
      
      yres <- tryCatch(phylo_glm_make_binary_y(d, dv = dv, thresh_text = input$dv_threshold), error = function(e) e)
      if (inherits(yres, "error")) {
        model_rv(NULL); info_rv(NULL); coef_rv(NULL)
        analysis_data_rv(NULL); analysis_tree_rv(NULL)
        dv_note_rv(NULL)
        output$model_summary_text <- shiny::renderUI({
          phylo_glm_callout("danger", shiny::tags$p(paste("Y binarization failed:", yres$message)))
        })
        return()
      }
      
      d[[dv_bin_name]] <- yres$y
      dv_note_rv(yres$note)
      
      fml <- phylo_glm_build_formula(dv_bin_name, x_vars = x, controls = character(0))
      
      vars_used <- unique(c(dv_bin_name, x))
      if (isTRUE(input$drop_missing)) d <- phylo_glm_drop_missing(d, vars_used)
      
      keep_tips <- as.character(d$.tip_label)
      keep_tips <- keep_tips[!is.na(keep_tips)]
      tree_fit <- ape::keep.tip(tree_sub, keep_tips)
      
      idx <- match(tree_fit$tip.label, d$.tip_label)
      d <- d[idx, , drop = FALSE]
      rownames(d) <- d$.tip_label
      
      if (isTRUE(input$standardize)) d <- phylo_glm_standardize_numeric(d, x)
      
      mod <- tryCatch(phylo_glm_fit(tree_fit, d, fml), error = function(e) e)
      if (inherits(mod, "error")) {
        model_rv(NULL); info_rv(NULL); coef_rv(NULL)
        analysis_data_rv(NULL); analysis_tree_rv(NULL)
        output$model_summary_text <- shiny::renderUI({
          phylo_glm_callout("danger", shiny::tags$p(paste("Model failed:", mod$message)))
        })
        return()
      }
      
      model_rv(mod)
      coef_rv(phylo_glm_tidy_coef(mod))
      analysis_data_rv(d)
      analysis_tree_rv(tree_fit)
      info_rv(phylo_glm_glance(mod, d, dv_bin_name))
    }, ignoreInit = TRUE)
    
    # ---- Stepwise (FIXED: remove keeping.formula)
    shiny::observeEvent(input$run_step, {
      req(isTRUE(input$enable_step))
      req(input$dv)
      
      dv <- input$dv
      candidates <- phylo_glm_null(input$step_candidates, character(0))
      controls <- phylo_glm_null(input$controls, character(0))
      
      if (length(candidates) == 0) {
        step_rv(list(status = "Please select candidate predictors for stepwise.", final_model = NULL, step_log = character(0), info = NULL, note = NULL))
        return()
      }
      
      d <- data_sub
      yres <- tryCatch(phylo_glm_make_binary_y(d, dv = dv, thresh_text = input$dv_threshold), error = function(e) e)
      if (inherits(yres, "error")) {
        step_rv(list(status = paste("Y binarization failed:", yres$message), final_model = NULL, step_log = character(0), info = NULL, note = NULL))
        return()
      }
      d[[dv_bin_name]] <- yres$y
      
      vars_used <- unique(c(dv_bin_name, candidates, controls))
      if (isTRUE(input$drop_missing)) d <- phylo_glm_drop_missing(d, vars_used)
      
      keep_tips <- as.character(d$.tip_label)
      keep_tips <- keep_tips[!is.na(keep_tips)]
      tree_fit <- ape::keep.tip(tree_sub, keep_tips)
      
      idx <- match(tree_fit$tip.label, d$.tip_label)
      d <- d[idx, , drop = FALSE]
      rownames(d) <- d$.tip_label
      
      if (isTRUE(input$standardize)) d <- phylo_glm_standardize_numeric(d, unique(c(candidates, controls)))
      
      res <- phylo_glm_run_stepwise(
        tree = tree_fit,
        data = d,
        dv_bin_name = dv_bin_name,
        candidates = candidates,
        controls = controls,
        direction = input$step_direction,
        k = input$step_k,
        trace_level = 1,
        method = "logistic_MPLE"
      )
      
      if (!is.null(res$final_model)) {
        res$info <- tryCatch(phylo_glm_glance(res$final_model, d, dv_bin_name), error = function(e) NULL)
      } else {
        res$info <- NULL
      }
      
      step_rv(res)
    }, ignoreInit = TRUE)
    
    # ---- top boxes: prefer stepwise if available
    active_model <- shiny::reactive({
      s <- step_rv()
      if (isTRUE(input$enable_step) && !is.null(s) && !is.null(s$final_model)) {
        return(list(model = s$final_model, info = s$info, source = "Stepwise", note = s$note))
      }
      m <- model_rv()
      if (!is.null(m)) return(list(model = m, info = info_rv(), source = "Manual", note = NULL))
      list(model = NULL, info = NULL, source = "—", note = NULL)
    })
    
    output$vb_n <- shinydashboard::renderValueBox({
      am <- active_model()
      val <- if (is.null(am$info)) "—" else am$info$n
      shinydashboard::valueBox(val, subtitle = paste("Countries used (n) •", am$source), icon = shiny::icon("flag"), color = "aqua")
    })
    
    output$vb_alpha <- shinydashboard::renderValueBox({
      am <- active_model()
      a_txt <- if (is.null(am$info)) "—" else phylo_glm_format_alpha(am$info$alpha)
      shinydashboard::valueBox(a_txt, subtitle = paste("Alpha (α) •", am$source), icon = shiny::icon("sitemap"), color = "purple")
    })
    
    output$vb_aic <- shinydashboard::renderValueBox({
      am <- active_model()
      aic <- if (is.null(am$info) || is.na(am$info$AIC)) "—" else round(am$info$AIC, 2)
      shinydashboard::valueBox(aic, subtitle = paste("AIC •", am$source), icon = shiny::icon("chart-line"), color = "yellow")
    })
    
    output$vb_auc <- shinydashboard::renderValueBox({
      am <- active_model()
      if (is.null(am$info) || is.na(am$info$auc)) {
        return(shinydashboard::valueBox("—", subtitle = paste("AUC •", am$source), icon = shiny::icon("chart-area"), color = "green"))
      }
      auc <- round(am$info$auc, 3)
      shinydashboard::valueBox(auc, subtitle = paste("AUC •", am$source), icon = shiny::icon("chart-area"), color = "green")
    })
    
    output$model_summary_text <- shiny::renderUI({
      mod <- model_rv()
      inf <- info_rv()
      if (is.null(mod) || is.null(inf)) return(shiny::helpText("Choose Y/X and click 'Run model'."))
      
      dv_text <- phylo_glm_choice_label(codebook_data, input$dv)
      note <- phylo_glm_null(dv_note_rv(), "")
      auc_txt <- if (is.na(inf$auc)) "AUC: —" else paste0("AUC: ", round(inf$auc, 3))
      phylo_glm_callout("info", shiny::tags$p(paste0("Outcome: ", dv_text, ". ", note, " ", auc_txt, ".")))
    })
    
    output$model_summary_print <- shiny::renderText({
      mod <- model_rv()
      if (is.null(mod)) return("")
      paste(capture.output(print(summary(mod))), collapse = "\n")
    })
    
    output$plot_forest <- plotly::renderPlotly({
      mod <- model_rv()
      cd <- coef_rv()
      req(mod, cd)
      
      term_labels <- vapply(cd$term, function(v) phylo_glm_choice_label(codebook_data, v), FUN.VALUE = character(1))
      names(term_labels) <- cd$term
      
      p <- phylo_glm_plot_forest_gg(cd, term_labels = term_labels, show_or = isTRUE(input$show_or))
      plotly::ggplotly(p, tooltip = c("y", "x"))
    })
    
    # ---- Marginal effect (FIXED in utils)
    output$plot_effect <- plotly::renderPlotly({
      mod <- model_rv()
      d <- analysis_data_rv()
      req(mod, d)
      req(input$focal_var)
      
      eff <- phylo_glm_make_effect_df(mod, d, focal_var = input$focal_var, grid_n = 60)
      if (is.null(eff)) return(NULL)
      
      xlab <- phylo_glm_choice_label(codebook_data, input$focal_var)
      plotly::ggplotly(phylo_glm_plot_effect_gg(eff, focal_label = xlab))
    })
    
    output$plot_ovp <- plotly::renderPlotly({
      mod <- model_rv()
      d <- analysis_data_rv()
      req(mod, d)
      plotly::ggplotly(phylo_glm_plot_ovp_gg(mod, d, dv_bin_name), tooltip = "text")
    })
    
    output$auc_text <- shiny::renderUI({
      inf <- info_rv()
      if (is.null(inf)) return(NULL)
      if (!is.null(inf$auc_note) && nzchar(inf$auc_note)) return(phylo_glm_callout("warning", shiny::tags$p(inf$auc_note)))
      if (is.na(inf$auc)) return(phylo_glm_callout("warning", shiny::tags$p("AUC is not available (need both 0 and 1 classes).")))
      phylo_glm_callout("info", shiny::tags$p(paste0("AUC = ", round(inf$auc, 3))))
    })
    
    output$plot_roc <- plotly::renderPlotly({
      inf <- info_rv()
      req(inf)
      if (is.null(inf$roc)) return(NULL)
      plotly::ggplotly(phylo_glm_plot_roc_gg(phylo_glm_roc_df(inf$roc)))
    })
    
    # ---- stepwise outputs
    output$step_status <- shiny::renderUI({
      s <- step_rv()
      if (is.null(s)) return(shiny::helpText("Run stepwise to see results."))
      msg <- s$status
      if (!is.null(s$note) && nzchar(s$note)) msg <- paste0(msg, " ", s$note)
      phylo_glm_callout("warning", shiny::tags$p(msg))
    })
    
    output$step_model_summary_print <- shiny::renderText({
      s <- step_rv()
      if (is.null(s) || is.null(s$final_model)) return("")
      paste(capture.output(print(summary(s$final_model))), collapse = "\n")
    })
    
    output$step_path_tbl <- shiny::renderTable({
      s <- step_rv()
      if (is.null(s) || is.null(s$step_log)) return(NULL)
      data.frame(step = seq_along(s$step_log), message = s$step_log, stringsAsFactors = FALSE)
    }, striped = TRUE, bordered = TRUE, spacing = "s", rownames = FALSE)
    
    output$plot_step_forest <- plotly::renderPlotly({
      s <- step_rv()
      if (is.null(s) || is.null(s$final_model)) return(NULL)
      
      cd <- phylo_glm_tidy_coef(s$final_model)
      term_labels <- vapply(cd$term, function(v) phylo_glm_choice_label(codebook_data, v), FUN.VALUE = character(1))
      names(term_labels) <- cd$term
      
      p <- phylo_glm_plot_forest_gg(cd, term_labels = term_labels, show_or = isTRUE(input$show_or))
      plotly::ggplotly(p, tooltip = c("y", "x"))
    })
  })
}
