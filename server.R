#' Work in progress
#' 
#' Code based on: https://shiny.posit.co/r/gallery/life-sciences/biodiversity-national-parks/
#' WVS data source: https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

library(shiny)
library(tidyverse)
library(gtsummary)
library(gt)
library(leaflet.extras)
library(rvest)
library(readxl)
library(vcd)
library(DT)

#####################
# SUPPORT FUNCTIONS #
#####################

source("Functions.R")

# ##################
# # DATA WRANGLING #
# ##################

#' Variable information
# var_info <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")
# var_info$Variable_Display_Logical <- as.logical(var_info$Variable_Display_Logical)
# var_options <- var_info$Col_ID[var_info$Variable_Display_Logical]

var_info <- read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
var_info$Variable_Display_Logical <- as.logical(var_info$Variable_Display_Logical)
var_options <- var_info$ColLab[var_info$Variable_Display_Logical]


#' Country level data
d_country <- read_rds("WVS_Dataset/WVS7_Country.rds")

#' Individual level data
d_ind <- read_rds("WVS_Dataset/WVS7_Individual.rds")
ind_country_options <- unique(d_ind$B_COUNTRY)

#' Grouping Questions by section for input options
sections <- as.list(unique(var_info$Section))
sections_ord <- factor(var_info$Section, ordered=TRUE, levels = sections)

testDD <- data.frame(group=sections_ord,
                     qvar=var_info$ColLab)
choicesQ <- split(testDD$qvar,testDD$group, lex.order=FALSE)
choicesQ <- choicesQ[-1]

  

################
# SERVER LOGIC #
################

shinyServer(
  
  function(input, output, session) {
    session$onSessionEnded(function() {
      stopApp()
    })
    
 # DataTable
    
    output$DTchoice <- renderUI ({
      radioButtons("DTdata", "Choose Dataset", choices = c("Country", "Individuals"), selected = "Country")
    }) 
     
    output$Table <- DT::renderDataTable({
      if (input$DTdata == "Country"){
        DTdata<-d_country
      }else if (input$DTdata == "Individuals"){
        DTdata<-d_ind
      }
      DT::datatable(data = DTdata, options = list(scrollX=TRUE))
    })  
    
 #' ---
 #' Within Country
 #' 
 #' This page is set up to look at the relationship between two variables
 #' within a single country

 output$Within_Var_1_Select <- renderUI({
   selectInput("Selected_Within_Var_1",
               "Select variable 1:", 
               choicesQ,
               selected = "Q1-Important in life: Family")
 })
 
 output$Within_Var_2_Select <- renderUI({
   selectInput("Selected_Within_Var_2",
               "Select variable 2:", 
               choicesQ,
               selected = "Q2-Important in life: Friends")
 })
 
 output$Within_Country_Select <- renderUI({
   selectInput("Selected_Within_Country",
               "Select a country:", 
               ind_country_options,
               selected="New Zealand")
 })
 
 d_within_country <- reactive({
   
   d_ind[d_ind$B_COUNTRY == input$Selected_Within_Country, ]
   
 })
 
 q1var <- reactive({
   q1_lookup <- var_info[which(var_info$ColLab == input$Selected_Within_Var_1),]
   q1input <- q1_lookup$Col_ID
  
 })
 
 q2var <- reactive({
   q2_lookup <- var_info[which(var_info$ColLab == input$Selected_Within_Var_2),]
   q2input <- q2_lookup$Col_ID
   
 })
 
 mosaicvarwithin<- reactive({
   m_var <- as.character(c(q1var(), q2var()))
   m_var
 })
   
 output$within_country_p_var_1 <- renderPlot({
 
   ggplot(d_within_country(), 
#          aes_string(x = input$Selected_Within_Var_1)) +
           aes(x = .data[[q1var()]])) +
     geom_bar() +
     theme_minimal()
   
 })
 
 output$within_country_p_var_2 <- renderPlot({
   
   ggplot(d_within_country(), 
#          aes_string(x = input$Selected_Within_Var_2)) +
          aes(x = .data[[q2var()]])) +
     geom_bar() +
     theme_minimal()
   
 })
 
 # Mosaic code template
 # output$Mosaic <- renderPlot({
 #   req(length(input$Discrete) > 0)
 #   form <- as.formula(paste0("~", paste(input$Discrete, collapse = "+")))
 #   vcd::mosaic(form, data = getData(), main = "Discrete novelties", sub = "Novelty shows in red", legend = TRUE, shade = TRUE, )
 # }) 
 
 #Add explanation and tidy up labelling
 output$within_country_mosaic <- renderPlot({
   form <- as.formula(paste0("~", paste(mosaicvarwithin(), collapse="+")))
   vcd::mosaic(form, data=d_within_country(), main = "Comparison between questions", legend = TRUE, shade = TRUE, labeling = labeling_border(rot_labels = c(25,0),just_labels = c("left","right")))
 })
 
 #' ---
 #' Between Country
 #' 
 #' Would like to add something like this in where two countries can be
 #' directly compared. 
 #' This would be a separate page where you tested and visualized 
 #' whether two countries varied on a single variable.
 #' To make comparisons (at least as plots) the data needs to be converted 
 #' to proportions rather than absolute values because countries differ in 
 #' the number of people they have
 #' 
 output$Between_Var_Select <- renderUI({
   selectInput("Selected_Between_Variable",
               "Select a variable:",
               choicesQ
               )
 })
 
 betweenQvar <- reactive({
   betweenQ_lookup <- var_info[which(var_info$ColLab == input$Selected_Between_Variable),]
   betweenQinput <- betweenQ_lookup$Col_ID
 })

 output$Between_Country_1_Select <- renderUI({
   selectInput("Selected_Between_Country_1",
               "Select a country:",
               ind_country_options,
               selected = "New Zealand")
 })

 output$Between_Country_2_Select <- renderUI({
   selectInput("Selected_Between_Country_2",
               "Select a country:",
               ind_country_options)
 })

# d_within_country <- reactive({
# d_between_country <- reactive({
#    d_ind[d_ind$B_COUNTRY == c(input$Selected_Between_Country_1,
#                               input$Selected_Between_Country_2), ]
# 
#  })

 d_between_country1 <- reactive({
    d_ind[d_ind$B_COUNTRY == input$Selected_Between_Country_1, ]

  })
 
 d_between_country2 <- reactive({
   d_ind[d_ind$B_COUNTRY == input$Selected_Between_Country_2, ]
   
 })

 output$between_country_p1 <- renderPlot({

   ggplot(d_between_country1(),
          aes(x = .data[[betweenQvar()]])) +
     geom_bar() +
     theme_minimal()

 })
 
 output$between_country_p2 <- renderPlot({
   
   ggplot(d_between_country2(),
          aes(x = .data[[betweenQvar()]])) +
     geom_bar() +
     theme_minimal()
   
 })
 
 #### How to do between mosaic plot? Problem: It's a subtlely different way of slicing the dataset, so Same question from 2 countries has varying number of responses.
 


 #' ---
 #' Global
 #' 
 #' This section looks at the global relationship between two variables. 
 #' 
 #' The country level dataset is very simple in that it provides country level 
 #' mean or proportions. Variation within countries has not been accounted for.
 #' 
 #' This is probably ok for the current simple purposes of the app. 
 #' More sophistication could be added in later.
 #' 
 output$Global_Var_1_Select <- renderUI({
   selectInput("Selected_Global_Var_1",
               "Select variable 1:",
               choicesQ,
               selected = "Q1-Important in life: Family")
 })

 output$Global_Var_2_Select <- renderUI({
   selectInput("Selected_Global_Var_2",
               "Select variable 2:",
               choicesQ,
               selected = "Q2-Important in life: Friends")
 })
 
 gblvar1 <- reactive({
   gblvar1_lookup <- var_info[which(var_info$ColLab == input$Selected_Global_Var_1),]
   gv1input <- gblvar1_lookup$Col_ID
 })
 
 gblvar2 <- reactive({
   gblvar2_lookup <- var_info[which(var_info$ColLab == input$Selected_Global_Var_2),]
   gv2input <- gblvar2_lookup$Col_ID
 })
 

 #' This reactions sections doesn't seem necessary.
 #' Something will need to change here to determine the correct plot type
 #' based on the 
 d_global <- reactive({

   d_country[ , c(gblvar1(),
                  gblvar2())]

 })

 #' Here it would be good to add:
 #' 1. Global plot for variable 1
 #' 2. Global plot for variable 2
 #' 3. Summary information for each variable
 #' 4. Basic statistical test of the relationship between the variables
 
#' This will need adapted to handle the different possible formats of the
#' selected variables. Currently variables can be:
#' Factor (responses at the country level have been split into different rows)
#' Ordinal Factor (treated as integer and then mean)
#' Integer (mean)
 
 output$global_p1 <- renderPlot({

   ggplot(d_global(),
          aes(x=.data[[gblvar1()]],
              y=.data[[gblvar2()]])) +
     geom_point() +
     geom_smooth(method="lm", se=FALSE)+
     theme_minimal()

 })
 
})
 
