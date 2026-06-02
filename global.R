###########################-
#### REQUIRED PACKAGES ####
###########################-

required_packages <- c("shiny", "markdown", "haven", "here", "labelled", 
                       "sjlabelled", "DT", "ggplot2", "naniar",
                       "readxl", "writexl", "tm", "shinyBS", 
                       "shinycssloaders", "shinydashboard", "shinyWidgets",
                       "tidyverse", "corrplot", "broom", "viridis", 
                       "plotly", "psych", "car", "randomForest",
                       "leaflet", "rnaturalearth", "rnaturalearthdata",
                       "ape", "phylolm", "pROC", "phytools")

for (packageName in required_packages) {
  if (!requireNamespace(packageName)) {
    install.packages(packageName)
  }
}


###########################-
#### SETUP.R LIBRARIES ####
###########################-
library(haven)
library(here)
library(labelled)
library(sjlabelled)
library(readxl)
library(writexl)
library(tm)
library(plyr)


############################-
#### LIBRARY COLLECTION ####
############################-
library(shiny)
library(markdown)
library(DT)
library(ggplot2) #covered by tidyverse, remove later, maybe?
library(naniar)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(corrplot)
library(broom)
library(viridis)
library(plotly)
library(psych)
library(car)
library(randomForest)

# library(gtsummary)
# library(leaflet)
# library(rnaturalearth)
# library(sf)
# library(dplyr)
# library(rstatix)
# library(scales)
# library(skimr)
# library(visdat)


##########################################-
#### SETTING SEED FOR REPRODUCIBILITY ####
##########################################-
set.seed(20241211)


##########################################################-
#### RUN THIS LINE ON THE VERY FIRST EXECUTION        ####
#### IF NO FILES OTHER THE RDS's ARE SAVED            ####
#### INTO THE FOLDER 'WDS_Dataset'.                   ####
#### THIS CODE WILL CREATE EVERY OTHER NECESSARY FILE ####
##########################################################-
# source(file.path("Support_Files/WVS_Wave7_Setup.R"), local = TRUE)

#' JW TO DO: check whether there have been changes or whether there is data and run above if needed



#################################-
#### WAVE 7 - DATA WRANGLING ####
#################################-

# load processed data
indiv_data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
country_data <- readRDS("WVS_Dataset/WVS7_Country.rds")
codebook_data <- readxl::read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
orig_UNSD_data <- readxl::read_excel("WVS_Dataset/UNSD — Methodology.xlsx")
picker_country_list <- read_rds("WVS_Dataset/picker_country_list.rds")


###########################-
#### SUPPORT FUNCTIONS ####
###########################-
source(file.path("Support_Files/functions.R"), 
       local = TRUE)


# Create global list of questions to select from
grouped_questions <- get_groupedQs_I(colnames(indiv_data))

#### modules ####
# source("modules/phylogeny/phylogeny_global.R")
source("modules/phylo_viz/phylo_viz_global.R")
source("modules/geo_viz/geo_viz_global.R")
source("modules/models/models_global.R")
