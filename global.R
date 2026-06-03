###########################-
#### REQUIRED PACKAGES ####
###########################-

# Packages should be installed in the deployment environment, not during app
# startup. This check fails fast with a clear message instead of trying to run
# install.packages() or BiocManager::install() while users are waiting.
required_packages <- c(
  "shiny", "markdown", "DT", "ggplot2", "dplyr", "tidyr", "readr", "readxl",
  "sjlabelled", "shinycssloaders", "shinydashboard", "shinyWidgets",
  "broom", "viridis", "viridisLite", "plotly", "psych", "car",
  "leaflet", "sf", "rnaturalearth", "htmltools", "ape", "phylolm", "pROC",
  "ggtree", "ggtreeExtra", "ggnewscale"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    ". Install these before launching the app."
  )
}


############################-
#### LIBRARY COLLECTION ####
############################-
# Keep attached packages lean. Most calls in the app use package::function()
# already, which reduces namespace conflicts and startup overhead.
library(shiny)
library(markdown)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(broom)
library(viridis)
library(plotly)
library(psych)
library(car)

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
codebook_data$Variable_Display_Logical <- as.logical(codebook_data$Variable_Display_Logical)

# Fast lookup vectors used throughout the app. These avoid repeatedly scanning
# the codebook for every plot/model render.
question_id_by_label <- stats::setNames(codebook_data$Col_ID, codebook_data$ColLab)
question_label_by_id <- stats::setNames(codebook_data$ColLab, codebook_data$Col_ID)

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
