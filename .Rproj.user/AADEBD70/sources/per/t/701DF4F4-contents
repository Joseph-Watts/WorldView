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

#####################
# SUPPORT FUNCTIONS #
#####################

#' Currently none


# ##################
# # DATA WRANGLING #
# ##################

#' Variable information
var_info <- read_xlsx("WVS_Dataset/Codebook manual coded index.xlsx")
var_info$Variable_Display_Logical <- as.logical(var_info$Variable_Display_Logical)
var_options <- var_info$Col_ID[var_info$Variable_Display_Logical]

#' Country level data
d_country <- read_rds("WVS_Dataset/WVS7_Country.rds")

#' Individual level data
d_ind <- read_rds("WVS_Dataset/WVS7_Individual.rds")
ind_country_options <- unique(d_ind$B_COUNTRY)


################
# SERVER LOGIC #
################

shinyServer(
  
  function(input, output) {

 #' ---
 #' Within Country
 #' 
 #' This page is set up to look at the relationship between two variables
 #' within a single country

 output$Within_Var_1_Select <- renderUI({
   selectInput("Selected_Within_Var_1",
               "Select variable 1:", 
               var_options)
 })
 
 output$Within_Var_2_Select <- renderUI({
   selectInput("Selected_Within_Var_2",
               "Select variable 2:", 
               var_options)
 })
 
 output$Within_Country_Select <- renderUI({
   selectInput("Selected_Within_Country",
               "Select a country:", 
               ind_country_options)
 })
 
 d_within_country <- reactive({
   
   d_ind[d_ind$B_COUNTRY == input$Selected_Within_Country, ]
   
 })
   
 output$within_country_p_var_1 <- renderPlot({
   
   ggplot(d_within_country(), 
          aes_string(x = input$Selected_Within_Var_1)) +
     geom_bar() +
     theme_minimal()
   
 })
 
 output$within_country_p_var_2 <- renderPlot({
   
   ggplot(d_within_country(), 
          aes_string(x = input$Selected_Within_Var_2)) +
     geom_bar() +
     theme_minimal()
   
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
 # output$Between_Var_Select <- renderUI({
 #   selectInput("Selected_Between_Variable",
 #               "Select a variable:", 
 #               var_options)
 # })
 # 
 # output$Between_Country_1_Select <- renderUI({
 #   selectInput("Selected_Between_Country_1",
 #               "Select a country:", 
 #               ind_country_options)
 # })
 # 
 # output$Between_Country_2_Select <- renderUI({
 #   selectInput("Selected_Between_Country_2",
 #               "Select a country:", 
 #               ind_country_options)
 # })
 # 
 # d_within_country <- reactive({
 #   
 #   d_ind[d_ind$B_COUNTRY == c(input$Selected_Between_Country_1,
 #                              input$Selected_Between_Country_2), ]
 #   
 # })
 # 
 # output$between_country_p1 <- renderPlot({
 #   
 #   ggplot(d_within_country(), 
 #          aes_string(x = input$Selected_Between_Variable)) +
 #     geom_bar() +
 #     theme_minimal()
 #   
 # })
 
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
               var_options)
 })

 output$Global_Var_2_Select <- renderUI({
   selectInput("Selected_Global_Var_2",
               "Select variable 2:",
               var_options)
 })

 #' This reactions sections doesn't seem necessary.
 #' Something will need to change here to determine the correct plot type
 #' based on the 
 d_global <- reactive({

   d_country[ , c(input$Selected_Global_Var_1,
                  input$Selected_Global_Var_2)]

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
          aes_string(x=input$Selected_Global_Var_1, 
              y=input$Selected_Global_Var_2)) + 
     geom_point() +
     theme_minimal()

 })
 
})