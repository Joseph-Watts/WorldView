# ######################
# # PACKAGE COLLECTION #
# ######################

library(shiny)

required_packages <- c('collapsibleTree', 'DT', 'ggplot2', 'gt', 'gtsummary', 'leaflet', 'leaflet.extras', 'naniar',
                       'readxl', 'rnaturalearth', 'rvest', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
                       'shinyWidgets', 'tidyverse', 'tigris', 'vcd')

for (packageName in required_packages){
  if (!requireNamespace(packageName, quietly = TRUE)) {
    install.packages(packageName)
  }
}


library(collapsibleTree)
library(DT)
library(ggplot2)
library(gt)
library(gtsummary)
library(leaflet)
library(leaflet.extras)
library(naniar)
library(readxl)
library(rnaturalearth)
library(rvest)
library(sf)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(tigris)
library(vcd)


# ###############################################################
# # WAVE 7 WRANGLING CODE BLOCK                                 #
# #                                                             #
# # Other waves may have different data wrangling requirements, #
# # but as they are similar, just copy this block and change it #
# # accordingly                                                 #
# ###############################################################
  
  
  # ##################
  # # DATA WRANGLING #
  # ##################
  
  
  # ##################
  # # DATA WRANGLING #
  # ##################
  
  
  # ##################
  # # DATA WRANGLING #
  # ##################

indv_part1 <- readRDS("WVS_Dataset/WVS7_Individual.rds")

vis_miss(indv_part1[6001:8000, ])