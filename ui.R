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

# library(leaflet)
# library(shinydashboard)
# library(collapsibleTree)
# library(shinycssloaders)
# library(DT)
# library(tidyverse)
# library(tigris)
# library(gtsummary)
# library(gt)
# library(shinyWidgets)
# library(shinyBS)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }",
    HTML(
      ".blue-button {
                    background-color: #33aaff !important;
                    color: white !important;
                    border-color: #33aaff !important;
      }
      
      .green-button {
                    background-color: #00a65a !important;
                    color: black !important;
                    border-color: #00a65a !important;
      }"
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
                         "Master Survery Questionnaire",
                         tabName = "pdfview",
                         icon = icon("stats", lib = "glyphicon")
                       ),
                       
                       menuItem(
                         "Data Tables",
                         tabName = "dummy",
                         icon = icon('list-alt', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "WVS7 Data Table",
                           tabName = "EDA",
                           icon = icon("th", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Missing Data Visualization",
                           tabName = "vis_miss",
                           icon = icon("warning-sign", lib = "glyphicon")
                         ),
                         
                         menuSubItem(
                           "Missing Data Visualization 2",
                           tabName = "dummy",
                           icon = icon("warning-sign", lib = "glyphicon")
                         )
                       ),
                       
                       menuItem(
                         "Intelligence",
                         tabName = "dummy",
                         icon = icon('info-sign', lib = "glyphicon"),
                         startExpanded = F,
                         
                         menuSubItem(
                           "Choropleth",
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
                       )
                       
                     ) # end sidebarMenu
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "dummy"
                # INTENTIONALLY EMPTY
        ),
        
        tabItem(tabName = "home",
                # home section
                includeMarkdown("www/home.md")
        ),
        
        tabItem(tabName = "map",
                includeMarkdown("www/choropleth.md"),
                
                # top row
                fluidRow(
                  column(6, uiOutput("pickRegion")),
                  column(6, uiOutput("selectAspect")),
                ),
                
                # bottom row
                fluidRow(
                  # left side
                  column(6, leafletOutput("worldMap", height = "80vh", width = "100%")),
                  
                  # right side
                  column(6, 
                         # first row inside the right side
                         fluidRow(
                           column(6, uiOutput("global_varA")),
                           column(6, uiOutput("global_varB"))
                         ),
                         # second row inside the right side
                         fluidRow(
                           column(12, plotOutput("global_p1", height = "600px"))
                         )
                  )
                ),
        ),
        
        tabItem(tabName = "pdfview",
                fluidRow(column(12, uiOutput("pdfview")))
        ),
        
        tabItem(tabName = "EDA",
                includeMarkdown("www/DTable.md"),
                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                bsButton(
                  "q1",
                  label = "",
                  icon = icon("question"),
                  style = "info",
                  class = "green-button",
                  size = "extra-small"
                ),
                fluidRow(column(6,uiOutput("DTchoice"))),
                fluidRow(column(12,DT::dataTableOutput(outputId = "Table"))),
                bsPopover(
                  id = "q1",
                  title = "Tidy data",
                  content = paste0(
                    "You should read the ",
                    a("tidy data paper", href = "http://vita.had.co.nz/papers/tidy-data.pdf", target =
                        "_blank")
                  ),
                  placement = "top",
                  trigger = "focus",
                  options = list(container = "body")
                )
        ),
        
        tabItem(tabName = "vis_miss",
                checkboxInput(inputId = "cluster",
                              label = "Cluster missingness",
                              value = FALSE),
                fluidRow(column(12,plotOutput("Missing", height = "90vh")))
                
        ),
        
        tabItem(tabName ="withinCountry",
                fluidRow(column(6, uiOutput("wc_country_sel"))),
                fluidRow(column(6, tableOutput("c_total_obs"))),
                fluidRow(
                  column(6, uiOutput("wc_qA")),
                  column(6, uiOutput("wc_qB"))
                ),
                fluidRow(
                  column(6, tableOutput("stats_wc_qA")),
                  column(6, tableOutput("stats_wc_qB"))
                ),
                fluidRow(
                  column(6,plotOutput("plot_wc_qA_levels")),
                  column(6,plotOutput("plot_wc_qB_levels"))
                ),
                tags$br(),
                fluidRow(
                  column(6,plotOutput("plot_wc_qA_prop")),
                  column(6,plotOutput("plot_wc_qB_prop"))
                ),
                tags$br(),
                fluidRow(column(12,plotOutput("test_output_plot"))),
                fluidRow(column(12,tableOutput("test_output_table"))),
                fluidRow(column(12,textOutput("test_output_stats")))
                
        ),
        
        tabItem(tabName = "betweenCountries",
                fluidRow(column(6,uiOutput("bc_question"))),
                fluidRow(
                  column(6,uiOutput("bc_countryA")),
                  column(6,uiOutput("bc_countryB"))
                ),
                fluidRow(
                  column(6,tableOutput("cA_total_obs")),
                  column(6,tableOutput("cB_total_obs"))
                ),
                fluidRow(
                  column(6,tableOutput("stats_bc_cA")),
                  column(6,tableOutput("stats_bc_cB"))
                ),
                fluidRow(
                  column(6,plotOutput("plot_bc_qcA_levels")),
                  column(6,plotOutput("plot_bc_qcB_levels"))
                ),
                tags$br(),
                fluidRow(
                  column(6,plotOutput("plot_bc_qA_prop")),
                  column(6,plotOutput("plot_bc_qB_prop"))
                )
        ),
        
        tabItem(tabName = "faq",
                includeMarkdown("www/faq.md")
        )
        
      ) #end tabItems
    ) # end dashboardBody
    
  ) #end dashboardPage
  
) #end fluidPage
) #end Server