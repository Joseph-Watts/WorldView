############################-
#### PACKAGE COLLECTION ####
############################-
library(shiny)

required_packages <- c('DT', 'ggplot2', 'gtsummary', 'leaflet', 'naniar', 'readxl', 'rnaturalearth', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
                       'shinyWidgets', 'tidyverse', 'corrplot', 'rstatix', 'broom', 'viridis', 'scales', 'plotly')


for (packageName in required_packages) {
  if (!requireNamespace(packageName, quietly = TRUE)) {
    install.packages(packageName)
  }
}

library(DT)
library(ggplot2)
library(gtsummary)
library(leaflet)
library(naniar)
library(readxl)
library(rnaturalearth)
library(sf)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(dplyr)
library(corrplot)
library(rstatix)
library(broom)
library(viridis)
library(scales)
library(plotly)


##########################################-
#### SETTING SEED FOR REPRODUCIBILITY ####
##########################################-
set.seed(20241211)


###################################################-
#### RUN THIS LINE ON THE VERY FIRST EXECUTION ####
###################################################-
# source(file.path("Support_Files/WVS_Wave7_Setup.R"), local = TRUE)


#################################-
#### WAVE 7 - DATA WRANGLING ####
#################################-
source(file.path("Support_Files/WVS_Wave7_Wrangling.R"), local = TRUE)


###########################-
#### SUPPORT FUNCTIONS ####
###########################-
source(file.path("Support_Files/functions.R"), local = TRUE)



# Create global variable
grouped_questions <- get_groupedQs_I()

