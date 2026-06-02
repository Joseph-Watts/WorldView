# modules/geo_viz/map/map_ui.R

geo_viz_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width       = 12,
        title       = "Map variable",
        status      = "warning",
        solidHeader = TRUE,
        selectizeInput(
          ns("map_var"),
          "Variable to show:",
          choices  = NULL,
          selected = NULL,
          multiple = FALSE,
          options  = list(
            placeholder = "Select one numeric variable"
          )
        ),
        selectInput(
          ns("map_palette"),
          "Palette:",
          choices = map_palette_choices(),
          selected = "viridis"
        )
      )
    ),
    
    fluidRow(
      box(
        width       = 12,
        title       = "World map",
        status      = "primary",
        solidHeader = TRUE,
        leafletOutput(ns("map"), height = 560),
        tags$div(
          style = "margin-top:12px;",
          uiOutput(ns("map_legend"))
        ),
        tags$div(
          style = paste(
            "margin-top:12px;",
            "display:flex;",
            "justify-content:center;",
            "align-items:center;"
          ),
          downloadButton(ns("download_map"), "Download map image")
        )
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
            tags$li("Cheng J, Schloerke B, Karambelkar B, Xie Y, Aden-Buie G (2025). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package version 2.2.3.9000 (GitHub: https://github.com/rstudio/leaflet).")
          )
        )
      )
    )
  )
}
