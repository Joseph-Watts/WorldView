# modules/phylo_viz/tree/tree_ui.R
phylo_viz_tree_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- shared CSS for scrollable boxes ---
    tags$style(HTML("
      .wv-box-scroll {
        max-height: 420px;     /* unified max height */
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 6px;    /* avoid scrollbar covering content */
      }
      .wv-update-row {
        display: flex;
        justify-content: flex-start;
        align-items: center;
        margin-bottom: 8px;
      }
    ")),
    
    fluidRow(
      # -------------------------
      # Left: Data Controls
      # -------------------------
      box(
        width = 4,
        title = "Data Controls",
        status = "warning",
        solidHeader = TRUE,
        
        div(
          class = "wv-box-scroll",
          
          selectInput(
            ns("data_type"),
            "Data Type:",
            choices = c("WVS7 Data" = "wvs", "Base Tree" = "blank"),
            selected = "wvs"
          ),
          # Select up to 5 numeric WVS variables
          selectizeInput(
            ns("outcome_vars"),
            "WVS7 Variables (max 5):",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select up to 5 numeric variables",
              maxItems = 5
            )
          ),
          
          # Per-variable palette selectors (generated dynamically)
          uiOutput(ns("var_palette_ui")),
          
          # Tip label fields (max 3)
          selectizeInput(
            ns("tip_label_fields"),
            "Tip label fields (max 3):",
            choices = c(
              "Country name"  = "country_name",
              "ISO3"          = "iso3166alpha3",
              "ISO2"          = "iso3166alpha2",
              "Language name" = "language_name",
              "ISO639P3"      = "iso639P3",
              "Glottocode"    = "glottocode"
            ),
            selected = c("country_name"),
            multiple = TRUE,
            options = list(maxItems = 3)
          ),
          
          textInput(ns("tip_label_sep"), "Tip label separator:", value = " | ")
        )
      ),
      
      # -------------------------
      # Middle: Tree Controls
      # -------------------------
      box(
        width = 4,
        title = "Tree Controls",
        status = "info",
        solidHeader = TRUE,
        
        div(
          class = "wv-box-scroll",
          
          sliderInput(
            ns("plot_height"),
            "Plot Height (px):",
            min = 500,
            max = 5000,
            value = 1100,
            step = 100
          ),
          
          # Remove "unrooted" (not needed)
          selectInput(
            ns("tree_layout"),
            "Tree Layout:",
            choices = c(
              "Rectangular" = "rectangular",
              "Slanted"     = "slanted",
              "Circular"    = "circular",
              "Fan"         = "fan",
              "Radial"      = "radial"
            ),
            selected = "rectangular"
          ),
          
          checkboxInput(ns("show_tip_labels"), "Show tip labels", value = TRUE),
          
          sliderInput(
            ns("tip_label_size"),
            "Tip Label Size:",
            min = 1,
            max = 6,
            value = 3,
            step = 0.5
          ),
          
          sliderInput(
            ns("bar_panel_width"),
            "Each bar panel width:",
            min = 0.3,
            max = 1.2,
            value = 0.6,
            step = 0.1
          ),
          
          sliderInput(
            ns("bar_panel_gap"),
            "Gap between panels:",
            min = 0.01,
            max = 0.2,
            value = 0.06,
            step = 0.01
          ),
          
          checkboxInput(ns("show_legends"), "Show legends", value = TRUE)
        )
      ),
      
      # -------------------------
      # Right: Variable Description
      # (Update Plot button is here, first row)
      # -------------------------
      box(
        width = 4,
        title = "Variable Description",
        status = "success",
        solidHeader = TRUE,
        
        div(
          class = "wv-box-scroll",
          
          div(
            class = "wv-update-row",
            actionButton(ns("update_plot"), "Update Plot", class = "btn-primary")
          ),
          
          htmlOutput(ns("var_description"))
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Language Phylogeny Tree",
        status = "primary",
        solidHeader = TRUE,
        plotOutput(ns("tree_plot"), height = "auto")
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
            tags$li("Xu, S., Li, L., Luo, X., Chen, M., Tang, W., Zhan, L., Dai, Z., Lam, T.T., Guan, Y., & Yu, G. (2022). ggtree: A serialized data object for visualization of a phylogenetic tree and annotation data. iMeta, 1(4), e56."),
            tags$li("Xu, S., Dai, Z., Guo, P., Fu, X., Liu, S., Zhou, L., Tang, W., Feng, T., Chen, M., Zhan, L., Wu, T., Hu, E., Jiang, Y., Bo, X., & Yu, G. (2021). ggtreeExtra: Compact visualization of richly annotated phylogenetic data. Molecular Biology and Evolution, 38(9), 4039–4042.")
          )
        )
      )
    )
  )
}
