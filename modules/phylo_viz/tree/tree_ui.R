# modules/phylo_viz/tree/tree_ui.R
phylo_viz_tree_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- shared CSS for scrollable boxes ---
    tags$style(HTML("
      .wv-box-scroll {
        max-height: 420px;
        overflow-y: auto;
        overflow-x: hidden;
        padding-right: 6px;
      }
      .wv-action-row {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 10px;
        margin-top: 12px;
        margin-bottom: 8px;
      }
    ")),
    
    fluidRow(
      box(
        width = 12,
        title = "Options",
        status = "warning",
        solidHeader = TRUE,
        
        div(
          class = "wv-box-scroll",
          
          # Select up to 3 numeric WVS variables
          selectizeInput(
            ns("outcome_vars"),
            "WVS7 Variables (max 3):",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select up to 3 numeric variables",
              maxItems = 3,
              plugins = list("remove_button")
            )
          ),
          
          # Per-variable palette selectors (generated dynamically)
          uiOutput(ns("var_palette_ui")),
          
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
            options = list(
              maxItems = 3,
              plugins = list("remove_button")
            )
          ),
          
          textInput(ns("tip_label_sep"), "Tip label separator:", value = " | "),
          
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
        ),
        
        div(
          class = "wv-action-row",
          actionButton(
            ns("update_plot"),
            "Generate Plot",
            class = "btn-primary btn-block",
            style = "width: 100%;"
          )
        )
      )
    ),
    
    fluidRow(
      box(
        width = 12,
        title = "Language Phylogeny Tree",
        status = "primary",
        solidHeader = TRUE,
        div(
          style = "width:100%; overflow-x:auto; text-align:center;",
          plotOutput(ns("tree_plot"), width = "100%", height = "auto")
        )
      )
    ),

    fluidRow(
      box(
        width = 12,
        title = "Download",
        status = "success",
        solidHeader = TRUE,
        fluidRow(
          column(
            width = 12,
            div(
              class = "wv-action-row",
              downloadButton(ns("download_tree_image"), "Download image", class = "btn-success")
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            sliderInput(
              ns("download_width"),
              "Download image width (px):",
              min = 800,
              max = 2400,
              value = 1400,
              step = 100
            )
          ),
          column(
            width = 6,
            sliderInput(
              ns("download_height"),
              "Download image height (px):",
              min = 600,
              max = 3000,
              value = 1200,
              step = 100
            )
          )
        ),
        tags$small(
          style = "display:block; margin-top:4px; color:#666;",
          "Image width is capped at 2400 px and image height at 3000 px to keep rendering responsive."
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
            tags$li("Xu, S., Li, L., Luo, X., Chen, M., Tang, W., Zhan, L., Dai, Z., Lam, T.T., Guan, Y., & Yu, G. (2022). ggtree: A serialized data object for visualization of a phylogenetic tree and annotation data. iMeta, 1(4), e56."),
            tags$li("Xu, S., Dai, Z., Guo, P., Fu, X., Liu, S., Zhou, L., Tang, W., Feng, T., Chen, M., Zhan, L., Wu, T., Hu, E., Jiang, Y., Bo, X., & Yu, G. (2021). ggtreeExtra: Compact visualization of richly annotated phylogenetic data. Molecular Biology and Evolution, 38(9), 4039–4042."),
            tags$li("Bouckaert, R., Redding, D., Sheehan, O., Kyritsis, T., Gray, R., Jones, K. E., & Atkinson, Q. (2022, 1 July). Global language diversification is linked to socio-ecology and threat status. doi:10.31235/osf.io/f8tr6")
          )
        )
      )
    )
  )
}
