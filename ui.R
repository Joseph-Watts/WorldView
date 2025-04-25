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
                           "<a href='https://www.worldvaluessurvey.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='images/logo_v1.png' width = '186'></a>",
                           "<br>",
                           "<p style = 'text-align: center;'><small>Data visualisation tool for <br> PSYC382: Culture and Cognition</small></p>",
                           "<br>"
                         ) # sidebar bg color -> #222d32
                       ),
                       
                       ####################### HOME #######################
                       menuItem(
                         "Home",
                         tabName = "home",
                         icon = icon("home")
                       ),
                       ####################### HOME #######################
                       
                       
                       ############## VARIABLE DOCUMENTATION ##############
                       menuItem(
                         "Variable Documentation",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Master Survey Questionnaire",
                           tabName = "pdfview",
                           icon = icon("comment", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Questionnaire Codebook",
                           tabName = "codebookview",
                           icon = icon("book", lib = "glyphicon")
                         )
                       ),
                       ############## VARIABLE DOCUMENTATION ##############
                       
                       
                       ################## RAW DATA TABLES #################
                       menuItem(
                         "Raw Data Tables",
                         tabName = "dummy",
                         icon = icon('list', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Participant-level Data",
                           tabName = "EDA_indiv",
                           icon = icon("user", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Country-level Data",
                           tabName = "EDA_country",
                           icon = icon("globe", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Missing Data Visualizations",
                           tabName = "vis_miss",
                           icon = icon("warning-sign", lib = "glyphicon")
                         )
                       ),
                       ################## RAW DATA TABLES #################
                       
                       
                       ################ SUMMARY STATISTICS ################
                       menuItem(
                         "Summary Statistics",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Individual-level",
                           tabName = "withinCountry",
                           icon = icon("user", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Country-level",
                           tabName = "betweenCountries", ## placeholder
                           icon = icon("globe", lib = "glyphicon")
                         )
                       ),
                       ################ SUMMARY STATISTICS ################
                       
                       
                       ################## VISUALISATIONS ##################
                       menuItem(
                         "Visualisations",
                         tabName = "dummy",
                         icon = icon('picture', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Map View",
                           tabName = "map", ## placeholder
                           icon = icon("map-marker", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Scatterplot - Participants",
                           tabName = "dummy", ## placeholder
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         menuSubItem(
                           "Scatterplot - Countries",
                           tabName = "dummy", ## placeholder
                           icon = icon("equalizer", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Bar Graph",
                           tabName = "dummy", ## placeholder
                           icon = icon("stats", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Heatmap",
                           tabName = "dummy", ## placeholder
                           icon = icon("fire", lib = "glyphicon")
                         )
                       ),
                       ################## VISUALISATIONS ##################
                       
                       
                       ###################### MODELS ######################
                       menuItem(
                         "Models",
                         tabName = "dummy",
                         icon = icon('screenshot', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Kendall's Rank Correlations",
                           tabName = "dummy", ## placeholder
                           icon = icon("sort-by-attributes-alt", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "ANOVA",
                           tabName = "dummy", ## placeholder
                           icon = icon("th", lib = "glyphicon")
                         )
                       ),
                       ###################### MODELS ######################
                       
                       
                       ######################## FAQ #######################
                       menuItem(
                         "FAQ",
                         tabName = "faq",
                         icon = icon("question-sign", lib = "glyphicon")
                       ),
                       ######################## FAQ #######################
                       
                       
                       ###################### ABOUT #######################
                       menuItem(
                         "About the Team",
                         tabName = "team",
                         icon = icon("user", lib = "glyphicon")
                       )
                       ###################### ABOUT #######################
                       
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
        
        tabItem(tabName = "EDA_indiv",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, DT::dataTableOutput(outputId = "Table_indiv"))),

        ),
        tabItem(tabName = "EDA_country",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                fluidRow(column(12, DT::dataTableOutput(outputId = "Table_country"))),
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
                           column(3, uiOutput("pickQuestion")),
                           column(3, uiOutput("menuCorrPlot"))
                           
                         ),
                         fluidRow(
                           column(12, plotOutput("corrChart", height = "70vh", width = "100%"))
                           # column(2, uiOutput("menuCorrPlot"))
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