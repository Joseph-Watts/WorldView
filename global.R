###########################-
#### REQUIRED PACKAGES ####
###########################-

required_packages <- c("shiny", "markdown", "haven", "here", "labelled", "sjlabelled", "DT", "ggplot2", "naniar",
                       "readxl", "writexl", "tm", "shinyBS", "shinycssloaders", "shinydashboard", "shinyWidgets",
                       "tidyverse", "corrplot", "broom", "viridis", "plotly", "psych", "car")

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


# list of questions grouped by their category minus ignored questions
grouped_minus_ignored <- lapply(grouped_questions, function(x)
  x[!grepl(paste0("\\b(", paste(ignored_questions, collapse = "|"), ")\\b"), x)])