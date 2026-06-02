# modules/models/phylo_lm/phylo_lm_ui.R
# Phylogenetic linear regression (phylolm) module UI

models_phylo_lm_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "phylo_lm",
    shiny::fluidRow(
      
      # ----------------------------
      # Left: controls
      # ----------------------------
      shinydashboard::box(
        width = 4,
        title = "Model setup (phylolm)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        shiny::helpText(
          "Use this page when the outcome is numeric (country-level mean/index).",
          "The model accounts for language relatedness using Pagel's lambda (λ)."
        ),
        
        shiny::selectInput(ns("dv"), "Outcome (Y)", choices = NULL),
        
        shiny::selectizeInput(
          ns("x_vars"),
          "Predictors (X) (max 5; can be empty → Y ~ 1)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 5, placeholder = "Select up to 5 predictors (or none)")
        ),
        
        shiny::checkboxInput(ns("standardize"), "Standardize numeric predictors (recommended)", value = TRUE),
        shiny::checkboxInput(ns("drop_missing"), "Drop rows with missing values in selected variables", value = TRUE),
        
        shiny::actionButton(ns("run"), "Run model", icon = shiny::icon("play")),
        
        shiny::hr(),
        
        shiny::tags$details(
          shiny::tags$summary("Advanced: Stepwise selection (phylostep)"),
          
          shiny::checkboxInput(ns("enable_step"), "Enable stepwise selection (exploratory)", value = FALSE),
          
          shiny::selectizeInput(
            ns("controls"),
            "Always include (controls, optional) (max 3)",
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 3, placeholder = "Controls always kept in stepwise")
          ),
          
          shiny::helpText("Stepwise uses AIC to add/drop predictors. Exploratory only (not a proof)."),
          
          shiny::selectizeInput(
            ns("step_candidates"),
            "Candidate predictors for stepwise (max 10)",
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 10, placeholder = "Select candidate predictors")
          ),
          
          shiny::selectInput(ns("step_direction"), "Direction", choices = c("both", "backward", "forward"), selected = "both"),
          
          shiny::numericInput(ns("step_k"), "Penalty k (k=2 is AIC)", value = 2, min = 1, step = 0.5),
          
          shiny::actionButton(ns("run_step"), "Run stepwise", icon = shiny::icon("wand-magic-sparkles"))
        ),
        
        shiny::hr(),
        shiny::tags$strong("References"),
        shiny::tags$small(
          shiny::tags$ul(
            shiny::tags$li("CRAN phylolm: Phylogenetic Linear Regression."),
            shiny::tags$li("Ho LST, Ane C (2014). A linear-time algorithm for Gaussian and non-Gaussian trait evolution models. Systematic Biology, 63, 397–408.")
          )
        )
      ),
      
      # ----------------------------
      # Right: results
      # ----------------------------
      shiny::column(
        width = 8,
        
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(ns("vb_n"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_lambda"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_aic"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_r2"), width = 3)
        ),
        
        shinydashboard::box(
          width = 12,
          title = "Results",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          
          shiny::tabsetPanel(
            id = ns("tabs"),
            type = "tabs",
            
            shiny::tabPanel(
              "Summary",
              shiny::uiOutput(ns("model_summary_text")),
              shiny::hr(),
              shiny::h4("Model summary (printed)"),
              shiny::verbatimTextOutput(ns("model_summary_print")),
              shiny::hr(),
              shiny::h4("Coefficient plot (forest plot)"),
              plotly::plotlyOutput(ns("plot_forest"), height = "380px")
            ),
            
            shiny::tabPanel(
              "Marginal effect",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::selectInput(ns("focal_var"), "Show effect of", choices = NULL),
                  shiny::helpText("We vary one predictor, holding others fixed (mean / most common).")
                ),
                shiny::column(width = 8, plotly::plotlyOutput(ns("plot_effect"), height = "420px"))
              )
            ),
            
            shiny::tabPanel(
              "Diagnostics",
              shiny::h4("Observed vs Predicted"),
              shiny::helpText("Hover on a point to see country + language + family."),
              plotly::plotlyOutput(ns("plot_ovp"), height = "450px"),
              shiny::hr(),
              shiny::tags$details(
                shiny::tags$summary("Advanced: Residual similarity vs phylogenetic distance"),
                shiny::helpText("Exploratory: Are residuals more similar for countries closer on the language tree?"),
                plotly::plotlyOutput(ns("plot_resid_dist"), height = "450px")
              )
            ),
            
            shiny::tabPanel(
              "Stepwise (Advanced)",
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("enable_step")),
                shiny::h4("Stepwise results"),
                shiny::uiOutput(ns("step_status")),
                shiny::hr(),
                shiny::h4("Final model summary (printed)"),
                shiny::verbatimTextOutput(ns("step_model_summary_print")),
                shiny::hr(),
                shiny::h4("Step path (Step log)"),
                shiny::tableOutput(ns("step_path_tbl")),
                shiny::hr(),
                shiny::h4("Final model coefficients"),
                plotly::plotlyOutput(ns("plot_step_forest"), height = "380px")
              ),
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == false", ns("enable_step")),
                shiny::helpText("Enable stepwise in the left panel to view results here.")
              )
            )
          )
        )
      )
    )
  )
}
