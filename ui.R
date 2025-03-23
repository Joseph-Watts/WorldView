#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

#' Some things still needing done:
#' - Add basic table where users can look through the participant and country
#' level data
#' 
#' - Add world map plots for the country level data page
#' 
#' - Consider adding a section to the website where users can test whether
#' two countries differ in respect to one variable
#' 
#' - More informative variable names needed. Probably need to add a column to
#' "WVS_Dataset/Codebook manual coded index.xlsx" with display names?
#' 
#' - Also there needs to be more information on the variables on the website.
#' Potentially a page with a list or something like that?
#' 
#' - Statistics and visualizations need to developed to matched the particular 
#' kinds of variables being compared. 
#' 
#' - Make things look and work better
#' 
#' -----------
#' Things that would be nice to add in the future:
#' 
#' - Integrate data from outside the WVS with the global analyses
#' 
#' - Individual level world plots using the latitude and longitude coordinates 
#' provided in the WVS
#' 
#' - Plot country level data on a language phylogeny
#' 
#' - Perform more sophisticated statistical models
#'      

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }",
    HTML(
      "
      .blue-button {
                    background-color: #33aaff !important;
                    color: white !important;
                    border-color: #33aaff !important;
      }

      .green-button {
                    background-color: #00a65a !important;
                    color: white !important;
                    border-color: #00a65a !important;
      }

      .nav-pills .nav-link.active {
                    background-color: #00a65a !important;
                    color: white !important;
      }
    "
    )
  ),
  
  dashboardPage(
    
    skin = "green",
    
    dashboardHeader(title = "World Values Survey", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(
                         paste0(
                           "<br>",
                           "<a href='https://www.worldvaluessurvey.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logoWVS215crop.png' width = '186'></a>",
                           "<br>",
                           "<p style = 'text-align: center;'><small>Data visualisation tool for <br> PSYC382: Culture and Cognition</small></p>",
                           "<br>"
                         )
                       ),
                       
                       menuItem(
                         "Home",
                         tabName = "home",
                         icon = icon("home")
                       ),
                       
                       menuItem(
                         "Master Survey Questionnaire",
                         tabName = "pdfview",
                         icon = icon("comment", lib = "glyphicon")
                       ),
                       
                       menuItem(
                         "Questionnaire Codebook",
                         tabName = "codebookview",
                         icon = icon("book", lib = "glyphicon")
                       ),
                       
                       menuItem(
                         "Data Tables",
                         tabName = "dummy",
                         icon = icon('list', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "WVS7 Data Table",
                           tabName = "EDA",
                           icon = icon("th", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Missing Data Visualizations",
                           tabName = "vis_miss",
                           icon = icon("warning-sign", lib = "glyphicon")
                         )
                       ),
                       
                       menuItem(
                         "Hypothesis Analysis & Testing",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "World Map view",
                           tabName = "map",
                           icon = icon("globe", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Within Country",
                           tabName = "withinCountry",
                           icon = icon("save", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Between Countries",
                           tabName = "betweenCountries",
                           icon = icon("random", lib = "glyphicon")
                         )
                       ),
                       
                       menuItem(
                         "FAQ",
                         tabName = "faq",
                         icon = icon("question-sign", lib = "glyphicon")
                       ),
                       
                       menuItem(
                         "About the Team",
                         tabName = "team",
                         icon = icon("user", lib = "glyphicon")
                       )
                       
                     ) # end sidebarMenu
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "dummy"
                # INTENTIONALLY EMPTY
        ),
        
        tabItem(tabName = "home",
                includeMarkdown("www/home.md")
        ),
        
        tabItem(tabName = "pdfview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("pdfview"))))
        ),
        
        tabItem(tabName = "codebookview",
                fluidRow(column(12, shinycssloaders::withSpinner(uiOutput("codebookview"))))
        ),
        
        tabItem(tabName = "EDA",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                # bsButton(
                #   "q1",
                #   label = "",
                #   icon = icon("question"),
                #   style = "info",
                #   class = "green-button",
                #   size = "extra-small"
                # ),
                fluidRow(column(6, uiOutput("DTchoice"))),
                fluidRow(column(12, DT::dataTableOutput(outputId = "Table"))),
                # bsPopover(
                #   id = "q1",
                #   title = "Tidy data",
                #   content = paste0(
                #     "You should read the ",
                #     a("tidy data paper", href = "http://vita.had.co.nz/papers/tidy-data.pdf", target =
                #         "_blank")
                #   ),
                #   placement = "top",
                #   trigger = "focus",
                #   options = list(container = "body")
                # )
        ),
        
        tabItem(tabName = "vis_miss",
                includeMarkdown("www/miss_vars.md"),
                
                tabsetPanel(id = "MissingViews", type = "pills",
                            tabPanel("Top 15 Missing", value = "top_miss",
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_indiv", height = "60vh")))),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Top_miss_country", height = "60vh"))))
                            ),
                            
                            tabPanel("Missing data from Individual Responses", value = "indiv_miss",
                                     checkboxInput(inputId = "cluster_indiv",
                                                   label = "Cluster missingness",
                                                   value = FALSE),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Indiv_missing_with_ratio", height = "85vh"))))
                            ),
                            
                            tabPanel("Missing data from Countries responses", value = "countries_miss",
                                     checkboxInput(inputId = "cluster_ctry",
                                                   label = "Cluster missingness",
                                                   value = FALSE),
                                     fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("Missing", height = "85vh"))))
                            )
                )
        ),
        
        tabItem(tabName = "map",
                includeMarkdown("www/choropleth.md"),
                
                tabsetPanel(id = "mapTabs", type = "pills",
                    tabPanel("Map view", value = "map_view",
                         fluidRow(
                           column(3, uiOutput("pickRegion"))
                         ),
                         fluidRow(
                           column(12, shinycssloaders::withSpinner(leafletOutput("worldMap", height = "60vh", width = "100%")))
                         ),
                         fluidRow(
                           column(12, div(style = "float:right",
                                          actionButton("next1", "Next", class = "green-button"))))
                    ),
                    tabPanel("Correlations", value = "correlations",
                         fluidRow(
                           column(3, uiOutput("pickTopic")),
                           column(3, uiOutput("pickQuestion"))
                         ),
                         fluidRow(
                           column(10, shinycssloaders::withSpinner(plotOutput("corrChart", height = "70vh", width = "100%"))),
                           column(2,
                                  # materialSwitch(inputId = "tglHeatmap", label = "Toggle heatmap",status = "danger"),
                                  # prettyToggle(
                                  #   inputId = "tglHeatmap",
                                  #   label_on = "Heatmap",
                                  #   label_off = "Ellipse",
                                  #   value = FALSE,
                                  #   shape = "curve"
                                  # )
                                  # Radio buttons for method
                                  radioButtons("corr_method", "Method:", choices = c("ellipse", "color"), selected = "ellipse"),

                                  # Radio buttons for order
                                  radioButtons("corr_order", "Order:", choices = c("FPC", "alphabet", "AOE", "hclust"), selected = "FPC"),

                                  # Slider for text size
                                  sliderInput("corr_tl_cex", "Text Size:", min = 0.5, max = 2, value = 0.8, step = 0.1),

                                  # Radio buttons for plot type
                                  radioButtons("corr_type", "Type:", choices = c("full", "upper"), selected = "upper"),

                                  # Checkbox for diagonal
                                  checkboxInput("corr_diag", "Show Diagonal", value = FALSE),

                                  # Checkbox for adding coefficient colors
                                  checkboxInput("corr_addCoef_col", "Show Coefficients", value = TRUE),

                                  # Dropdown for coefficient color
                                  selectInput("corr_coef_color", "Coefficient Color:", choices = c("black", "blue", "red"), selected = "black"),

                                  # Slider for text rotation
                                  sliderInput("corr_tl_srt", "Text Rotation:", min = 0, max = 90, value = 45),

                                  # Radio buttons for background color
                                  radioButtons("corr_bg", "Background Color:", choices = c("darkgrey", "white"), selected = "white"),

                                  # Download button to export plot
                                  downloadButton("corr_downloadPlot", "Download Plot")
                                  )
                         ),
                         fluidRow(
                           column(12, div(style = "float:right",
                                          actionButton("prev1", "Previous", class = "green-button"),
                                          actionButton("next2", "Next", class = "green-button"))))
                    ),
                    tabPanel("ANOVA", value = "anova",
                             fluidRow(
                               column(10, shinycssloaders::withSpinner(plotOutput("anovaBoxplot", height = "70vh", width = "100%"))),
                               column(2, checkboxInput("bxplt_notch", "Show Notches", value = FALSE))
                             ),
                             # fluidRow(
                             #   column(12, shinycssloaders::withSpinner(plotOutput("anovaChart")))
                             # ),
                             fluidRow(
                               column(12, shinycssloaders::withSpinner(verbatimTextOutput("modelSummary")))
                             ),
                             fluidRow(
                               column(12, shinycssloaders::withSpinner(verbatimTextOutput("anovaSummary")))
                             ),
                             fluidRow(
                               column(12, div(style = "float:right",
                                              actionButton("prev2", "Previous", class = "green-button"))))
                    )
                )
        ),
        
        tabItem(tabName ="withinCountry",
                fluidRow(
                  column(6, uiOutput("wc_country_sel"))
                  ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(tableOutput("c_total_obs")))
                  ),
                fluidRow(
                  column(6, uiOutput("wc_qA")),
                  column(6, uiOutput("wc_qB"))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_wc_qA"))),
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_wc_qB")))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qA_levels"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qB_levels")))
                ),
                tags$br(),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qA_prop"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_wc_qB_prop")))
                ),
                tags$br(),
                fluidRow(column(12, shinycssloaders::withSpinner(plotOutput("test_output_plot")))),
                fluidRow(column(12, shinycssloaders::withSpinner(tableOutput("test_output_table")))),
                fluidRow(column(12, textOutput("test_output_stats")))
                
        ),
        
        tabItem(tabName = "betweenCountries",
                fluidRow(column(6, uiOutput("bc_question"))),
                fluidRow(
                  column(6, uiOutput("bc_countryA")),
                  column(6, uiOutput("bc_countryB"))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(tableOutput("cA_total_obs"))),
                  column(6, shinycssloaders::withSpinner(tableOutput("cB_total_obs")))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_bc_cA"))),
                  column(6, shinycssloaders::withSpinner(tableOutput("stats_bc_cB")))
                ),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qcA_levels"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qcB_levels")))
                ),
                tags$br(),
                fluidRow(
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qA_prop"))),
                  column(6, shinycssloaders::withSpinner(plotOutput("plot_bc_qB_prop")))
                )
        ),
        
        tabItem(tabName = "faq",
                includeMarkdown("www/faq.md")
        ),
        
        tabItem(tabName = "team",
                includeMarkdown("www/team.md")
        )
        
      ) #end tabItems
    ) # end dashboardBody
    
  ) #end dashboardPage
  
) #end fluidPage
) #end Server