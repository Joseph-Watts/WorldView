# modules/phylogeny/map/map_ui.R

phylo_map_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        width       = 4,
        title       = "Data controls",
        status      = "warning",
        solidHeader = TRUE,
        selectInput(
          ns("map_var"),
          "Variable to map:",
          choices = NULL
        )
      ),
      # Variable description box
      box(
        width = 8,
        title = "Variable Description",
        status = "success",
        solidHeader = TRUE,
        htmlOutput(ns("var_description"))
      )
    ),
    
    # Map
    fluidRow(
      box(
        width       = 12,
        title       = "World map",
        status      = "info",
        solidHeader = TRUE,
        leafletOutput(ns("map"), height = 500)
      )
    )
  )
}