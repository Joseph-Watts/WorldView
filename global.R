# ######################
# # PACKAGE COLLECTION #
# ######################

library(shiny)

required_packages <- c('collapsibleTree', 'DT', 'ggplot2', 'gt', 'gtsummary', 'leaflet', 'leaflet.extras', 'naniar',
                       'readxl', 'rnaturalearth', 'rvest', 'sf', 'shinyBS', 'shinycssloaders', 'shinydashboard',
                       'shinyWidgets', 'tidyverse', 'tigris', 'vcd', 'dplyr', 'recipes', 'corrgram', 'corrplot')

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
library(recipes)
library(GGally)
library(corrgram)
library(corrplot)


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

# Ignored questions (given the number of factors they have or any other condition)
# Q223, Q266, Q267, Q268, Q272 and Q290
ignored_questions <- c("Q223", "Q266", "Q267", "Q268", "Q272", "Q290")

# Questions that doesn't fit the translation between nominal to numeric



# transformation of non-ordinal data into numerical
indiv_ordinal <- indiv_ordinal[, setdiff(names(indiv_ordinal), ignored_questions)] %>%
  dplyr::mutate(
    Q56 = case_when(
      Q56 == "Better off" ~ 1,
      Q56 == "Worse off" ~ -1,
      Q56 == "Or about the same" ~ 0,
      TRUE ~ NA_real_  # Keep NA as is
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q57 = case_when(
      Q57 == "Most people can be trusted" ~ 1,
      Q57 == "Need to be very careful" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(Q91 = case_when(
    Q91 == "France" ~ 2,
    Q91 == "China" ~ 1,
    Q91 == "India" ~ 0,
    TRUE ~ NA_real_
  )) %>% ######################################  ######################################
  dplyr::mutate(Q92 = case_when(
    Q92 == "Washington DC" ~ 2,
    Q92 == "London" ~ 1,
    Q92 == "Geneva" ~ 0,
    TRUE ~ NA_real_
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q93 = case_when(
      Q93 == "Climate change" ~ 2,
      Q93 == "Human rights" ~ 1,
      Q93 == "Destruction of historic monuments" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q94, Q95, Q96, Q97, Q98, Q99, Q100, Q101, Q102, Q103, Q104, Q105),
    ~ case_when(
      . == "Active member" ~ 1,
      . == "Inactive member" ~ -1,
      . == "Don't belong" | . == "Not a member" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q111 = case_when(
      Q111 == "Protecting environment" ~ 2,
      Q111 == "Economy growth and creating jobs" ~ 1,
      Q111 == "Other answer" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q139, Q140, Q141, Q144, Q145, Q151, Q165, Q166, Q167, Q168, Q269, Q285),
    ~ case_when(. == "Yes" ~ 1, . == "No" ~ 0, TRUE ~ NA_real_)
  )) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q149, Q150),
    ~ case_when(. == "Freedom" ~ 1, . == "Equality" |
                  . == "Security" ~ 0, TRUE ~ NA_real_)
  )) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q152, Q153),
    ~ case_when(
      . == "A high level of economic growth" ~ 3,
      . == "Making sure this country has strong defence forces" ~ 2,
      . == "Seeing that people have more say about how  are done at their jobs and in their communities" ~ 1,
      . == "Trying to make our cities and countryside more beautiful" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q154, Q155),
    ~ case_when(
      . == "Maintaining order in the nation" ~ 3,
      . == "Giving people more say in important government decisions" ~ 2,
      . == "Fighting rising prices" ~ 1,
      . == "Protecting freedom of speech" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q156, Q157),
    ~ case_when(
      . == "A stable economy" ~ 3,
      . == "Progress toward a less impersonal and more humane society" ~ 2,
      . == "Progress toward a society in which Ideas count more than money" ~ 1,
      . == "The fight against crime" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q173 = case_when(
      Q173 == "A religious person" ~ 1,
      Q173 == "Not a religious person" ~ 0,
      Q173 == "An atheist" ~ -1,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q174 = case_when(
      Q174 == "Follow religious norms and ceremonies" ~ 1,
      Q174 == "Do good to other people" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q175 = case_when(
      Q175 == "Make sense of life after death" ~ 1,
      Q175 == "Make sense of life in this world" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q260 = case_when(
      Q260 == "Male" ~ 1,
      Q260 == "Female" ~ 0,
      TRUE ~ NA_real_
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q263 = case_when(
      Q263 == "I am born in this country" ~ 1,
      Q263 == "I am an immigrant to this country (born outside this country)" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q264, Q265),
    ~ case_when(. == "Immigrant" ~ 1, . == "Not an immigrant" ~ 0, TRUE ~ NA_real_)
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q271 = case_when(
      Q271 == "Yes, both own parent(s) and parent(s) in law" ~ 3,
      Q271 == "Yes, own parent(s)" ~ 2,
      Q271 == "Yes, parent(s) in law" ~ 1,
      Q271 == "No" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q273 = case_when(
      Q273 == "Married" ~ 5,
      Q273 == "Living together as married" ~ 4,
      Q273 == "Divorced" ~ 3,
      Q273 == "Separated" ~ 2,
      Q273 == "Widowed" ~ 1,
      Q273 == "Single" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q279, Q280),
    ~ case_when(
      . == "Full time (30 hours a week or more)" ~ 9,
      . == "Part time (less than 30 hours a week)" ~ 8,
      . == "Self employed" ~ 7,
      . == "Military Service (EVS)" ~ 6,
      . == "Homemaker not otherwise employed" ~ 5,
      . == "Student" ~ 4,
      . == "Retired/pensioned" ~ 3,
      . == "Disabled (EVS)" ~ 2,
      . == "Unemployed" ~ 1,
      . == "Other" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(across(
    c(Q281, Q282, Q283),
    ~ case_when(
      . == "Higher administrative (for example: banker, executive in big business, high government official, union official)" ~ 11,
      . == "Professional and technical (for example: doctor, teacher, engineer, artist, accountant, nurse)" ~ 10,
      . == "Skilled worker (for example: foreman, motor mechanic, printer, seamstress, tool and die maker, electrician)" ~ 9,
      . == "Clerical (for example: secretary, clerk, office manager, civil servant, bookkeeper" ~ 8,
      . == "Sales (for example: sales manager, shop owner, shop assistant, insurance agent, buyer)" ~ 7,
      . == "Service (for example: restaurant owner, police officer, waitress, barber, caretaker)" ~ 6,
      . == "Semi-skilled worker (for example: bricklayer, bus driver, cannery worker, carpenter, sheet metal worker, baker)" ~ 5,
      . == "Farm owner, farm manager" ~ 4,
      . == "Farm worker (for example: farm labourer, tractor driver)" ~ 3,
      . == "Unskilled worker (for example: labourer, porter, unskilled factory worker, cleaner)" ~ 2,
      . == "Never had a job" ~ 1,
      . == "JP,KG,TJ: Other" ~ 0,
      TRUE ~ NA_real_
    )
  )) %>% ######################################  ######################################
  dplyr::mutate(
    Q284 = case_when(
      Q284 == "Private business or industry" ~ 2,
      Q284 == "Private non-profit organization" ~ 1,
      Q284 == "Government or public institution" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    Q289 = case_when(
      Q289 == "Do not belong to a denomination" ~ 9,
      Q289 == "Catholic (Roman/Greek/etc)" ~ 8,
      Q289 == "Protestant" ~ 7,
      Q289 == "Orthodox (Russian/Greek/etc.)" ~ 6,
      Q289 == "Jew" ~ 5,
      Q289 == "Muslim" ~ 4,
      Q289 == "Hindu" ~ 3,
      Q289 == "Buddhist" ~ 2,
      Q289 == "Other Christian (Jehova withness...)" ~ 1,
      Q289 == "Other" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    E1_LITERACY = case_when(
      E1_LITERACY == "Literate" ~ 1,
      E1_LITERACY == "Illiterate" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    F_INTPRIVACY = case_when(
      F_INTPRIVACY == "There were other people around who could follow the interview" ~ 1,
      F_INTPRIVACY == "There were no other people around who could follow the interview" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(
    H_SETTLEMENT = case_when(
      H_SETTLEMENT == "Capital city" ~ 4,
      H_SETTLEMENT == "Regional center" ~ 3,
      H_SETTLEMENT == "District center" ~ 2,
      H_SETTLEMENT == "Another city, town (not a regional or district center)" ~ 1,
      H_SETTLEMENT == "Village" ~ 0,
      TRUE ~ NA_real_
    )
  ) %>% ######################################  ######################################
  dplyr::mutate(H_URBRURAL = case_when(H_URBRURAL == "Urban" ~ 1, H_URBRURAL == "Rural" ~ 0, TRUE ~ NA_real_))



# #########################
# # END OF WAVE 7         #
# # DATA WRANGLING BLOCK  #
# #########################



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


#####################
# SUPPORT FUNCTIONS #
#####################

# Identify factor variables and print the number of levels for each
factor_info <- sapply(indiv_ordinal, function(x) {
  if (is.factor(x)) {
    return(length(levels(x)))
  } else {
    return(NA)  # NA for non-factor variables
  }
})

# # Filter and print only factor variables
# factor_info <- factor_info[!is.na(factor_info)]
# print(factor_info)
# 
# length(factor_info)



print_factor_levels <- function(data) {
  # Loop through columns of the data frame
  factor_columns <- names(data)[sapply(data, is.factor)]  # Select only factor columns
  
  # Iterate through factor columns and check the number of levels
  for (col_name in factor_columns) {
    column <- data[[col_name]]
    
    if (length(levels(column)) < 15) {  # Check for fewer than 15 levels
      cat(sprintf("Variable '%s' has %d levels:\n", col_name, length(levels(column))))
      print(levels(column))
      cat("\n")  # Add a blank line for readability
    }
  }
}

# print_factor_levels(orig_indiv_data)
# print_factor_levels(indiv_ordinal)



# ##################
# # DATA WRANGLING #
# ##################

