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

library(leaflet)
library(shinydashboard)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tidyverse)
library(tigris)
library(gtsummary)
library(gt)

###########
# LOAD UI #
###########

shinyUI(fluidPage(
  
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  # load google analytics script
  tags$head(includeScript("www/google-analytics-bioNPS.js")),
  
  # remove shiny "red" warning messages on GUI
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  dashboardPage(
    
    skin = "green",
    
    dashboardHeader(title="**World Values Survey", titleWidth = 300),
    
    dashboardSidebar(width = 300,
                     sidebarMenu(
                       HTML(paste0(
                         "<br>",
                         "<a href='https://www.worldvaluessurvey.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='logoWVS215crop.png' width = '186'></a>",
                         "<br>",
                         "<p style = 'text-align: center;'><small>Data visualisation tool for <br> PSYC382: Culture and Cognition</small></p>",
                         "<br>"
                       )),
                       menuItem("Home",
                                tabName = "home",
                                icon = icon("home")),
                       menuItem( "dropdown test", tabName = "dummy", icon = icon('list'), startExpanded = F,
                                 menuSubItem("Exports", tabName = "dummy", icon = icon('export', lib = 'glyphicon')),
                                 menuSubItem("Imports", tabName = "dummy", icon = icon('import', lib = 'glyphicon')),
                                 menuSubItem("Intelligence by HS code", tabName = "dummy", icon = icon("bolt"))),
                       menuItem("Master Survery Questionnaire",
                                tabName="pdfview",
                                icon = icon("stats",lib = "glyphicon")),
                       menuItem("WVS7 Data",
                                tabName = "EDA",
                                icon = icon("stats",lib = "glyphicon")),
                       menuItem("Within Country",
                                tabName = "withinCountry",
                                icon = icon("stats",lib = "glyphicon")),
                       menuItem("Between Countries",
                                tabName="betweenCountries",
                                icon = icon("stats",lib = "glyphicon")),
                       menuItem("Global",
                                tabName = "global",
                                icon = icon("stats",lib = "glyphicon")),
                       menuItem("FAQ",
                                tabName = "faq",
                                icon = icon("question-sign",lib = "glyphicon"))
                     )
    ),
    
    dashboardBody(
      
      tabItems(
        tabItem(tabName = "dummy"
                # INTENTIONALLY EMPTY
        ),
        tabItem(tabName = "home",
                # home section
                includeMarkdown("www/home.md")
        ),
        tabItem(tabName = "pdfview",
                fluidRow(column(12, uiOutput("pdfview")))
        ),
        tabItem(tabName = "EDA",
                includeMarkdown("www/DTable.md"),
                fluidRow(column(6,uiOutput("DTchoice"))),
                fluidRow(column(12,DT::dataTableOutput(outputId = "Table")))
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
        tabItem(tabName = "global",
                fluidRow(
                  column(6,uiOutput("global_varA")),
                  column(6,uiOutput("global_varB"))
                ),
                fluidRow(
                  column(12,plotOutput("global_p1"))
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