# modules/models/phylo_glm/phylo_glm_ui.R

models_phylo_glm_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shinydashboard::tabItem(
    tabName = "phylo_glm",
    shiny::fluidRow(
      shinydashboard::box(
        width = 4,
        title = "Model setup (phyloglm)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        shiny::helpText("Binary outcome is created from a numeric country-level variable, then we fit a phylogenetic logistic regression."),
        
        shiny::selectInput(ns("dv"), "Outcome (Y) (numeric survey variables)", choices = NULL),
        
        shiny::textInput(
          ns("dv_threshold"),
          "Y threshold (default q50)",
          value = "q50",
          placeholder = "e.g., q50 (median) or 1.6"
        ),
        shiny::helpText("Format: qxx percentile (e.g., q50) OR a numeric value (e.g., 1.6). Above threshold = 1."),
        
        shiny::selectizeInput(
          ns("x_vars"),
          "Predictors (X) (max 5; can be empty → Y ~ 1)",
          choices = NULL,
          multiple = TRUE,
          options = list(maxItems = 5, placeholder = "Select up to 5 predictors (or none)")
        ),
        
        shiny::checkboxInput(ns("standardize"), "Standardize numeric predictors (recommended)", value = TRUE),
        shiny::checkboxInput(ns("drop_missing"), "Drop rows with missing values in selected variables", value = TRUE),
        shiny::checkboxInput(ns("show_or"), "Show Odds Ratio (exp(β)) in coefficient plot", value = TRUE),
        
        shiny::actionButton(ns("run"), "Run model", icon = shiny::icon("play")),
        
        shiny::hr(),
        
        shiny::tags$details(
          shiny::tags$summary("Advanced: Stepwise selection (phyloglmstep)"),
          shiny::checkboxInput(ns("enable_step"), "Enable stepwise selection (exploratory)", value = FALSE),
          
          shiny::selectizeInput(
            ns("controls"),
            "Start with (optional) controls (max 3)",
            choices = NULL, multiple = TRUE,
            options = list(maxItems = 3, placeholder = "Used as starting.formula")
          ),
          
          shiny::selectizeInput(
            ns("step_candidates"),
            "Candidate predictors for stepwise (max 10)",
            choices = NULL, multiple = TRUE,
            options = list(maxItems = 10, placeholder = "Full model candidates")
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
      
      shiny::column(
        width = 8,
        
        shiny::fluidRow(
          shinydashboard::valueBoxOutput(ns("vb_n"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_alpha"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_aic"), width = 3),
          shinydashboard::valueBoxOutput(ns("vb_auc"), width = 3)
        ),
        
        shinydashboard::box(
          width = 12,
          title = "Results",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          
          shiny::tabsetPanel(
            id = ns("tabs"),
            
            shiny::tabPanel(
              "Summary",
              shiny::uiOutput(ns("model_summary_text")),
              shiny::hr(),
              shiny::h4("Model summary (printed)"),
              shiny::verbatimTextOutput(ns("model_summary_print")),
              shiny::hr(),
              shiny::h4("Coefficient plot"),
              plotly::plotlyOutput(ns("plot_forest"), height = "380px")
            ),
            
            shiny::tabPanel(
              "Marginal effect",
              shiny::fluidRow(
                shiny::column(
                  width = 4,
                  shiny::selectInput(ns("focal_var"), "Show effect of", choices = NULL)
                ),
                shiny::column(width = 8, plotly::plotlyOutput(ns("plot_effect"), height = "420px"))
              )
            ),
            
            shiny::tabPanel(
              "Diagnostics",
              shiny::h4("Observed vs Predicted probability"),
              plotly::plotlyOutput(ns("plot_ovp"), height = "340px"),
              shiny::hr(),
              shiny::h4("ROC curve"),
              shiny::uiOutput(ns("auc_text")),
              plotly::plotlyOutput(ns("plot_roc"), height = "320px")
            ),
            
            shiny::tabPanel(
              "Stepwise (Advanced)",
              shiny::conditionalPanel(
                condition = sprintf("input['%s'] == true", ns("enable_step")),
                shiny::uiOutput(ns("step_status")),
                shiny::hr(),
                shiny::h4("Final model summary (printed)"),
                shiny::verbatimTextOutput(ns("step_model_summary_print")),
                shiny::hr(),
                shiny::h4("Step log"),
                shiny::tableOutput(ns("step_path_tbl")),
                shiny::hr(),
                shiny::h4("Final model coefficients"),
                plotly::plotlyOutput(ns("plot_step_forest"), height = "380px")
              )
            )
          )
        )
      )
    )
  )
}
