# ######################
# # PACKAGE COLLECTION #
# ######################
library(shiny)

required_packages <- c('collapsibleTree', 'DT', 'ggplot2', 'gt', 'gtsummary', 'leaflet', 'leaflet.extras', 'naniar',
                       'readxl', 'rnaturalearth', 'rnaturalearthdata', 'rvest', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
                       'shinyWidgets', 'tidyverse', 'tigris', 'vcd', 'dplyr', 'recipes', 'GGally', 'corrgram', 'corrplot',
                       'ggpubr', 'rstatix', 'broom', 'AICcmodavg', 'viridis', 'scales', 'colorspace', 'plotly')

for (packageName in required_packages) {
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
library(vcd) # double check to exclude
library(dplyr)
library(recipes) # double check to exclude
library(GGally)
library(corrgram)
library(corrplot)
library(ggpubr)
library(rstatix)
library(broom)
library(AICcmodavg)
library(viridis)
library(scales)
library(colorspace)
library(plotly)


# ####################################
# # SETTING SEED FOR REPRODUCIBILITY #
# ####################################
set.seed(20241211)


# #############################################
# # RUN THIS LINE ON THE VERY FIRST EXECUTION #
# #############################################
# source(file.path("Support_Files/WVS_Wave7_Setup.R"), local = TRUE)


# ###########################
# # wave 7 - DATA WRANGLING #
# ###########################
source(file.path("Support_Files/WVS_Wave7_Wrangling.R"), local = TRUE)


# #####################
# # SUPPORT FUNCTIONS #
# #####################
source(file.path("Support_Files/functions.R"), local = TRUE)



# Create global variable
grouped_questions <- get_groupedQs_I()

