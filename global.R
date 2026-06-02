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
# source(file.path("Support_Files/WVS_Wave7_Wrangling.R"), 
#        local = TRUE)


# load processed data
orig_indiv_data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
orig_country_data <- readRDS("WVS_Dataset/WVS7_Country.rds")
orig_codebook_data <- readxl::read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
orig_UNSD_data <- readxl::read_excel("WVS_Dataset/UNSD — Methodology.xlsx")

picker_country_list <- read_rds("WVS_Dataset/picker_country_list.rds")

# # Ignored questions (given the number of factors they have or any other condition)
ignored_questions <- c("Q223", # political parties for each country - almost 1000 different factors
                       "Q266", # birth place - basically all countries ~ 200 factors
                       "Q267", # birth place - basically all countries ~ 200 factors
                       "Q268", # birth place - basically all countries ~ 200 factors
                       "Q272", # language groupings - # different factors
                       "Q290") # ethnic groupings - # different factors

indiv_ordinal <- readRDS("WVS_Dataset/WVS7_Individual.rds")

###########################-
#### SUPPORT FUNCTIONS ####
###########################-
source(file.path("Support_Files/functions.R"), 
       local = TRUE)


# Create global variable
grouped_questions <- get_groupedQs_I()

not_diplayed <- orig_codebook_data$Col_ID[
  orig_codebook_data$Variable_Display_Logical == "F"
  ]

ignored_not_diplayed_questions <- c(ignored_questions, 
                                    not_diplayed) %>%
  unique()

# list of questions grouped by their category minus ignored questions
grouped_minus_ignored <- lapply(grouped_questions, 
                                function(x)
  x[!grepl(paste0("\\b(", paste(ignored_not_diplayed_questions, 
                                collapse = "|"), 
                  ")\\b"), x)])



#### modules ####
# source("modules/phylogeny/phylogeny_global.R")
source("modules/phylo_viz/phylo_viz_global.R")
source("modules/geo_viz/geo_viz_global.R")
source("modules/models/models_global.R")
