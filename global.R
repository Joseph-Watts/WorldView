# ######################
# # PACKAGE COLLECTION #
# ######################

library(shiny)

required_packages <- c('collapsibleTree', 'DT', 'ggplot2', 'gt', 'gtsummary', 'leaflet', 'leaflet.extras', 'naniar',
                       'readxl', 'rnaturalearth', 'rvest', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
                       'shinyWidgets', 'tidyverse', 'tigris', 'vcd', 'dplyr')

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
library(vcd)
library(dplyr)


# ###############################################################
# # WAVE 7 WRANGLING CODE BLOCK                                 #
# #                                                             #
# # Other waves may have different data wrangling requirements, #
# # but as they are similar, just copy this block and change it #
# # accordingly                                                 #
# ###############################################################

# setting seed for reproducibility
set.seed(20241211)


# load original data
orig_indiv_data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
orig_country_data <- readRDS("WVS_Dataset/WVS7_Country.rds")
orig_codebook_data <- read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
orig_UNSD_data <- read_excel("WVS_Dataset/UNSD — Methodology.xlsx")


# load intermediaries
WVS7_part_countries <- orig_country_data[c(1:2)]
UNSD_countries_list <- orig_UNSD_data[c(3:12)]

# transform into ordinal
indiv_ordinal <- orig_indiv_data
indiv_ordinal <- as.data.frame(lapply(indiv_ordinal, function(col) {
  if (is.ordered(col)) {
    as.numeric(col)
  } else {
    col
  }
}))

# transformation of non-ordinal data into numerical











# picker list
WVS7_part_countries <- WVS7_part_countries %>%
  left_join(
    UNSD_countries_list %>%
      select(`ISO-alpha3 Code`, `Region Name`),
    by = c("B_COUNTRY_ALPHA" = "ISO-alpha3 Code")
  ) %>%
  mutate(`Region Name` = ifelse(is.na(`Region Name`), "Not defined", `Region Name`))

picker_country_list <- WVS7_part_countries %>%
  arrange(`B_COUNTRY`) %>%
  group_by(`Region Name`) %>%
  summarise(Countries = list(`B_COUNTRY`), .groups = "drop") %>%
  deframe()


# ##################
# # DATA WRANGLING #
# ##################


# ##################
# # DATA WRANGLING #
# ##################


# ##################
# # DATA WRANGLING #
# ##################



