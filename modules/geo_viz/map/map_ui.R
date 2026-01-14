# modules/geo_viz/map/map_ui.R

geo_viz_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        .wv-box-scroll {
          height: 350px;
          overflow-y: auto;
          overflow-x: hidden;
          padding-right: 6px;
        }
      "))
    ),
    
    fluidRow(
      # --- Data control (width 4) ---
      box(
        width       = 4,
        title       = "Data control",
        status      = "warning",
        solidHeader = TRUE,
        tags$hr(),
        
        div(
          class = "wv-box-scroll",
          selectizeInput(
            ns("map_vars"),
            "Variables to show (max 5):",
            choices  = NULL,
            selected = NULL,
            multiple = TRUE,
            options  = list(
              placeholder = "Select 1–5 numeric variables",
              maxItems = 5
            )
          ),  
          selectInput(
            ns("bg_mode"),
            "Country fill (background):",
            choices  = c(
              "None (outline only)" = "none",
              "Main language"       = "language_name",
              "Language family"     = "family_language_name"
            ),
            selected = "family_language_name"
          ),
          
          tags$div(style = "margin-top:6px;",
                   tags$b("Background legend (full list):")),
          uiOutput(ns("bg_legend_full"))
          
          
        )
      ),
      
      # --- Chart control (width 4) ---
      box(
        width       = 4,
        title       = "Chart control",
        status      = "info",
        solidHeader = TRUE,
        div(
          class = "wv-box-scroll",
          
          checkboxInput(ns("show_charts"), "Show charts", TRUE),
          
          selectInput(
            ns("chart_type"),
            "Chart type:",
            choices  = c(
              "bar"          = "bar",
              "pie"          = "pie",
              "polar-area"   = "polar-area",
              "polar-radius" = "polar-radius"
            ),
            selected = "bar"
          ),
          
          sliderInput(
            ns("chart_size"),
            "Chart size:",
            min = 20, max = 90, value = 38, step = 2
          ),
          
          tags$hr(),
          tags$div(tags$b("Chart legend (outside map):")),
          plotOutput(ns("charts_legend"), height = 230)
        )
      ),
      
      # --- Variable Description (width 4, last) ---
      box(
        width       = 4,
        title       = "Variable Description",
        status      = "success",
        solidHeader = TRUE,
        
        div(
          actionButton(ns("btn_update"), "Update", icon = icon("sync"))
        ),
        
        div(
          class = "wv-box-scroll",
          htmlOutput(ns("var_description"))
        )
      )
    ),
    
    fluidRow(
      box(
        width       = 12,
        title       = "World map",
        status      = "primary",
        solidHeader = TRUE,
        leafletOutput(ns("map"), height = 560)
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "References",
        status = "info",
        solidHeader = TRUE,
        tags$small(
          tags$ul(
            tags$li("Cheng J, Schloerke B, Karambelkar B, Xie Y, Aden-Buie G (2025). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package version 2.2.3.9000 (GitHub: https://github.com/rstudio/leaflet)."),
            tags$li("leaflet.minicharts: Mini Charts for Interactive Maps. https://doi.org/10.32614/CRAN.package.leaflet.minicharts")
          )
        )
      )
    )
  )
}
