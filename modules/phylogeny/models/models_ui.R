# modules/phylogeny/models/models_ui.R
phylo_models_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Warning display
    uiOutput(ns("model_warnings")),
    
    # Row 1: Control Panel
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("PGLS Model Settings"),
          selectInput(
            ns("outcome"),
            "Dependent Variable (Y):",
            choices = NULL
          ),
          selectInput(
            ns("predictors"), 
            "Independent Variables (X):",
            choices = NULL,
            multiple = TRUE
          ),
          actionButton(ns("fit_model"), "Run PGLS Analysis", class = "btn-primary")
          
        )
      ),
      
      # Column 2: Variable Info
      column(
        width = 4,
        wellPanel(
          h4("Data Summary"),
          uiOutput(ns("variable_info")),
          br(),
          h5("Model Formula:"),
          verbatimTextOutput(ns("model_formula"))
        )
      ),
      
      # Column 3: Quick Guide
      column(
        width = 4,
        wellPanel(
          h4("Interpretation Guide"),
          h5("λ Values:"),
          tags$ul(
            tags$li("0.7-1.0: Strong signal"),
            tags$li("0.3-0.7: Moderate signal"), 
            tags$li("0.0-0.3: Weak signal")
          ),
          h5("Significance:"),
          tags$ul(
            tags$li("p < 0.05: Significant"),
            tags$li("p ≥ 0.05: Not significant")
          )
        )
      )
    ),
    
    # Row 2: Model Results
    fluidRow(
      column(
        width = 8,
        wellPanel(
          h4("PGLS Model Results"),
          verbatimTextOutput(ns("pgls_summary"))
        )
      ),
      column(
        width = 4,
        wellPanel(
          h4("Model Interpretation"),
          uiOutput(ns("pgls_interpretation"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        wellPanel(
          h4("Residual Diagnostics"),
          plotlyOutput(ns("pgls_plot"), height = "300px")
        )
      )
    )
  )
}