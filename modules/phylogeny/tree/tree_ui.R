# modules/phylogeny/tree/tree_ui.R
phylo_tree_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # Data control panel
      box(
        width = 4,
        title = "Data Controls",
        status = "warning",
        solidHeader = TRUE,
        
        # Data type selection
        selectInput(
          ns("data_type"),
          "Data Type:",
          choices = c("WVS7 Data" = "wvs", "Base Tree" = "blank"),
          selected = "wvs"
        ),
        
        # Variable selection (dynamic)
        selectInput(
          ns("outcome_var"),
          "WVS7 Variable:",
          choices = NULL
        ),
        
        # Tip label formatting - Language part
        selectInput(
          ns("language_format"),
          "Language Label Format:",
          choices = c(
            "Glottocode" = "glottocode",
            "ISO639P3" = "iso639P3", 
            "Language Name" = "language_name"
          ),
          selected = "iso639P3"
        ),
        
        # Tip label formatting - Country part
        selectInput(
          ns("country_format"),
          "Country Label Format:",
          choices = c(
            "ISO3166 Alpha-3" = "iso3166alpha3",
            "ISO3166 Alpha-2" = "iso3166alpha2",
            "Country Name" = "country_name"
          ),
          selected = "iso3166alpha3"
        )
      ),

      # Variable description box
      box(
        width = 4,
        title = "Variable Description",
        status = "success",
        solidHeader = TRUE,
        htmlOutput(ns("var_description"))
      ),
      
      # Tree control panel
      box(
        width = 4,
        title = "Tree Controls",
        status = "info",
        solidHeader = TRUE,
        
        # Visualization controls
        sliderInput(
          ns("plot_height"),
          "Plot Height (px):",
          min = 500,
          max = 5000,
          value = 1000,
          step = 100
        ),
        
        # Extended tree layout options
        selectInput(
          ns("tree_layout"),
          "Tree Layout:",
          choices = c(
            "Phylogram" = "phylogram",
            "Cladogram" = "cladogram", 
            "Fan" = "fan",
            "Unrooted" = "unrooted",
            "Radial" = "radial",
            "Tidy" = "tidy"
          ),
          selected = "phylogram"
        ),
        
        # Tip label size control
        sliderInput(
          ns("tip_label_size"),
          "Tip Label Size:",
          min = 0.5,
          max = 1.5,
          value = 1,
          step = 0.1
        ),
        
        # Color controls
        selectInput(
          ns("color_scheme"),
          "Color Scheme:",
          choices = c("viridis", "plasma", "inferno", "magma", "cividis"),
          selected = "viridis"
        ),
        
        checkboxInput(
          ns("show_legend"),
          "Show Color Legend",
          value = TRUE
        ),
        
        actionButton(ns("update_plot"), "Update Plot", class = "btn-primary")
      )
    ),
    
    # Main plot area
    fluidRow(
      box(
        width = 12,
        title = "Language Phylogeny Tree",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("tree_plot"), height = "auto")
      )
    )
  )
}