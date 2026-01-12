###########################-
#### REQUIRED PACKAGES ####
###########################-



required_packages <- c("shiny", "markdown", "haven", "here", "labelled", "sjlabelled", "DT", "ggplot2", "naniar",
                       "readxl", "writexl", "tm", "shinyBS", "shinycssloaders", "shinydashboard", "shinyWidgets",
                       "tidyverse", "corrplot", "broom", "viridis", "plotly", "psych", "car", "randomForest")

# for (packageName in required_packages) {
#   if (!requireNamespace(packageName)) {
#     install.packages(packageName)
#   }
# }

invisible(lapply(required_packages, require, character.only = TRUE))



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
#library(plyr)


############################-
#### LIBRARY COLLECTION ####
############################-
library(shiny)
library(markdown)
library(DT)   #duplicate
#library(ggplot2) #covered by tidyverse, remove later, maybe? #duplicate
library(naniar)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(corrplot)
library(broom)
library(viridis)
library(plotly)    #duplicate
library(psych)
library(car)
library(randomForest)

# library(gtsummary)
# library(leaflet)
# library(rnaturalearth)
# library(sf)
library(dplyr)
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






################################################################################
                      ###   global.R for HDRs ### 
################################################################################

# Load libraries
#library(shiny)
#library(dplyr)     #data manipulation
#library(readxl)
#library(ggplot2)  
#library(plotly)
#library(DT)
#install.packages("countrycode")  #add country code 
#library(stringdist)   # for checking match
library(openxlsx)
library(janitor)
library(sf)        # spatial data
library(rnaturalearth)
library(leaflet)
library(countrycode)
library(purrr)   # for map()
library(tidyr)




# ============================================
# Load MASTER dataset (HDR + WVS, classified)
# Country-level dataset used by the Shiny app
# ============================================
MASTER_COUNTRY_DATA <- readRDS(
  here::here("Support_Files/MASTER_HDR_WVS7_CLASSIFIED.rds"
             )
 )




#Load "MASTER_HDR_WVS7_CLASSIFIED""
MASTER_HDR_WVS7_CLASSIFIED <- MASTER_COUNTRY_DATA


# =============================
# Load WVS7 country-level data
# =============================
wvs7_country <- readRDS(
  here::here("WVS_Dataset", "WVS7_Country.rds")
)



# ================================
# Load variable dictionary (HDR + WVS7)
# ================================
FULL_VARIABLE_DICTIONARY <- readRDS(
  here::here("Support_Files", "FULL_VARIABLE_DICTIONARY.rds")
)

#View(FULL_VARIABLE_DICTIONARY)

# ADD A COL FOR READABLE VARIABLES NAMES 
FULL_VARIABLE_DICTIONARY <- FULL_VARIABLE_DICTIONARY %>%
  dplyr::mutate(
    # Short label: take text before " is "
    short_label = stringr::str_trim(
      stringr::str_remove(definition, " is .*")
    )
  )








# ================================
# Load HDR_GROUP_BENCHMARK
# ================================
HDR_GROUP_BENCHMARKS<- readRDS("Support_Files/HDR_GROUP_BENCHMARKS.rds")


# ======================================
# Load variables in build_HDR_tables.R
# ======================================
source("HDR_files/build_HDR_tables.R")


# ======================================
# Load variables in build_HDR_tables.R
# ======================================
#source("Support_Files/Build_WVS7_tables.R")
source("Support_Files/HDR_WVS7_dropdown_choices.R")



# =========================================
# Load variables in Build_master_dataset.R
# =========================================
MASTER_HDR_WVS7_CLASSIFIED <- MASTER_COUNTRY_DATA


# ======================================
# Load HDR_GROUP_LOOKUP 
# ======================================
HDR_GROUP_LOOKUP <- readRDS("Support_Files/HDR_GROUP_LOOKUP.rds")
View(HDR_GROUP_LOOKUP)


# ======================================
# Load HDR_AREA_LOOKUP 
# ======================================
HDR_AREA_LOOKUP <- readRDS("Support_Files/HDR_AREA_LOOKUP.rds")





# ==========================================================
# HDR variable choices for GROUP-LEVEL scatter 
# ==========================================================
# Identify All numeric benchmark variables actually available for plotting
HDR_BENCHMARK_VARS <- HDR_GROUP_BENCHMARKS %>%
  dplyr::select(where(is.numeric)) %>%
  names()





# ======================================
# Load HDR_AREA_LOOKUP 
# ======================================
HDR_LABELS <- readRDS("Support_Files/HDR_LABELS.rds")  



# ==========================================================
# HDR variable choices for GROUP-LEVEL scatter
# ==========================================================
HDR_GROUP_VAR_CHOICES <- HDR_GROUP_BENCHMARKS %>%
  dplyr::distinct(variable) %>%
  dplyr::filter(variable %in% names(HDR_LABELS)) %>%
  dplyr::mutate(
    label = HDR_LABELS[variable]
  ) %>%
  dplyr::arrange(label) %>%
  {
    setNames(.$variable, .$label)
  }


 

# ==================================================================
# Load WVS7_country_variable_dictionary for WVS7 readable dropdwn
# ==================================================================
WVS7_COUNTRY_DICT <- readRDS(
  "Support_Files/WVS7_COUNTRY_DICT.rds"
)



# ==========================================================================
# Load FULL_COUNTRY_VAR_DICT which has readble var names for WVS7 and HDRs 
# ==========================================================================
FULL_COUNTRY_VAR_DICT <- readRDS(
  "Support_Files/FULL_COUNTRY_VAR_DICT.rds"
)



# ==========================================================================
# Load country-level dataset for LMM
# ==========================================================================
hdr_table2_ctry_long<- readRDS(here::here("Support_Files/hdr_table2_ctry_long.rds"))


countries_table2 <- readRDS(here::here("Support_Files/countries_table2.rds"))




# ==========================================================
# Load group level dataset for LMM
# ==========================================================
hdr_table2_aggregates_long <- readRDS("Support_Files/hdr_table2_aggregates_long.rds")



# ============================================================
# HDR country-level outcome choices (UI-safe)
# ============================================================
HDR_var_choices <- FULL_COUNTRY_VAR_DICT %>%
  
  # Keep HDR variables only
  dplyr::filter(source == "HDR") %>%
  
  # Exclude identifiers / non-outcomes
  dplyr::filter(!var_code %in% c("country", "iso3")) %>%
  
  # Build named choices for selectInput()
  dplyr::arrange(label) %>%
  {
    setNames(.$var_code, .$label)
  }












