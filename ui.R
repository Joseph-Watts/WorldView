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
  
  # load page layout
  dashboardPage(
    
    skin = "green",
      
    dashboardHeader(title="World Values Survey", titleWidth = 300),
    
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

        menuItem("Within Country", 
                 tabName = "within_country", 
                 icon = icon("stats", 
                             lib = "glyphicon")),
        
        menuItem("Global", 
                 tabName = "global", 
                 icon = icon("stats", 
                             lib = "glyphicon")) 
        
      )
      
    ), # end dashboardSidebar
    
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "home",
          
          # home section
          includeMarkdown("www/home.md")
          
        ),
        
        tabItem(tabName = "within_country",
                includeMarkdown("www/within_country.md"),
                fluidRow(column(3, uiOutput("Within_Country_Select"))),
                fluidRow(
                  column(6, uiOutput("Within_Var_1_Select")),
                  column(6, uiOutput("Within_Var_2_Select"))
                  ),
                fluidRow(
                  column(6, plotOutput("within_country_p_var_1")),
                  column(6, plotOutput("within_country_p_var_2"))
                )
                #' Add in a plot and more details about the relationship
                #' between the two variables here?
                
                
        ),
        
        #' To add in a between county page here?
        
        tabItem(tabName = "global",
                includeMarkdown("www/global.md"),
                fluidRow(
                  column(6, uiOutput("Global_Var_1_Select")),
                  column(6, uiOutput("Global_Var_2_Select"))
                  ),
                fluidRow(
                  column(12, plotOutput("global_p1") %>% 
                           withSpinner(color = "green"))
                )
                
        )
              
      )
    
    ) # end dashboardBody
  
  )# end dashboardPage

))