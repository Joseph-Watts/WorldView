###############################################################################
# SCRIPT NAME: HDR_WVS7_dropdown_choices.R
# -----------------------------------------------------------------------------
# PURPOSE
# -----------------------------------------------------------------------------
# This script defines and builds all human-readable labels and dropdown choices
# used for Human Development Report (HDR) and World Values Survey (WVS7)
# variables in the HDR–WVS Shiny application.
#
# Its main purpose is to separate user-facing labels and variable metadata
# from the underlying data, ensuring that dropdown menus, plots, and tables
# display clear, consistent, and authoritative descriptions rather than
# internal variable codes.
#
# -----------------------------------------------------------------------------
# CONTENTS
# -----------------------------------------------------------------------------
# This script performs the following tasks:
#
# 1. Defines a named vector (HDR_LABELS) that maps HDR variable codes to
#    simplified, user-friendly labels for all HDR tables.
#
# 2. Saves HDR_LABELS as a reusable .rds object for loading in global.R.
#
# 3. Defines clean, human-readable labels for each HDR table to be used in
#    group-level visualisations and UI elements.
#
# 4. Builds a country-level WVS7 variable dictionary from the original WVS
#    codebook, keeping only variables flagged for display.
#
# 5. Combines HDR and WVS7 country-level variables into a unified dictionary
#    (FULL_COUNTRY_VAR_DICT) that supports mixed HDR–WVS analyses.
#
# -----------------------------------------------------------------------------
# OUTPUT
# -----------------------------------------------------------------------------
# This script creates and saves the following objects:
#
# - HDR_LABELS.rds
#   Named vector mapping HDR variable codes to user-friendly labels.
#
# - WVS7_COUNTRY_DICT.rds / .xlsx
#   Country-level variable dictionary for WVS7 indicators.
#
# - FULL_COUNTRY_VAR_DICT.rds / .xlsx
#   Unified dictionary of all country-level HDR and WVS7 variables used in
#   dropdown menus and visualisations.
#
# -----------------------------------------------------------------------------
# USAGE
# -----------------------------------------------------------------------------
# This script is executed during the data preparation stage and is not run
# at Shiny runtime. The saved objects are loaded in global.R and used to
# populate dropdowns, legends, and labels dynamically throughout the app.
###############################################################################


# ==========================================================
# 1. Simplified HDR dropdown labels (authoritative)
# ==========================================================
HDR_LABELS <- c(
  # ------------------------------------------------------------------
  # TABLE 1 - Human Development Index (HDI) & Components
  # ------------------------------------------------------------------
  # ---- Identification ----
  "hdi_rank" = "HDI rank",
  "country"  = "Country",
  
  # ---- Core HDI ----
  "hdi_2023" = "Human Development Index (HDI), 2023",
  
  "life_expect_birth_2023" = "Life expectancy at birth, 2023",
  
  "expected_yrs_schooling_2023" = "Expected years of schooling, 2023",
  "mean_yrs_schooling_2023" = "Mean years of schooling (adults 25+), 2023",
  
  "gni_per_capita_2023" = "Gross national income per capita (PPP), 2023",
  "gni_minus_hdi_rank_2023" = "GNI per capita rank minus HDI rank, 2023",
  
  "hdi_rank_2022" = "HDI rank, 2022", 
  

# --------------------------------------------------------------------
# TABLE 2 - HDI TRENDS 1990-2023
# --------------------------------------------------------------------  
  # ---- Identification ----
  "hdi_rank"  = "HDI rank, 2023",
  "country"   = "Country",
  
  # ---- HDI levels by year ----
  "hdi_1990"  = "Human Development Index (HDI), 1990",
  "hdi_2000"  = "Human Development Index (HDI), 2000",
  "hdi_2010"  = "Human Development Index (HDI), 2010",
  "hdi_2015"  = "Human Development Index (HDI), 2015",
  "hdi_2020"  = "Human Development Index (HDI), 2020",
  "hdi_2021"  = "Human Development Index (HDI), 2021",
  "hdi_2022"  = "Human Development Index (HDI), 2022",
  "hdi_2023"  = "Human Development Index (HDI), 2023",
  
  # ---- Rank change ----
  "change_hdi_rank_2015_2023" = "Change in HDI rank, 2015–2023",
  
  # ---- Average annual HDI growth rates ----
  "avg_annual_hdi_growth_perct_1990_2000" = "Average annual HDI growth (%), 1990–2000",
  
  "avg_annual_hdi_growth_perct_2000_2010" = "Average annual HDI growth (%), 2000–2010",
  
  "avg_annual_hdi_growth_perct_2010_2023" = "Average annual HDI growth (%), 2010–2023",
  
  "avg_annual_hdi_growth_perct_1990_2023" = "Average annual HDI growth (%), 1990–2023",
  
  
  # ==========================================================
  # Table 3 — Inequality-adjusted HDI
  # ==========================================================
    # ---- Identification ----
    "hdi_rank"  = "HDI rank, 2023",
    "country"   = "Country",
    
    # ---- Core indices ----
    "hdi_2023"  = "Human Development Index (HDI), 2023",
    "ihdi_2023" = "Inequality-adjusted HDI (IHDI), 2023",
    "ihdi_loss_perct_2023" = "Overall loss due to inequality (%), 2023",
    "ihdi_diff_hdirank_2023" = "Difference from HDI rank after inequality adjustment, 2023",
    
    "coef_human_inequality_2023" = "Coefficient of human inequality, 2023",
    
    "ineq_life_exp_perct_2023" = "Inequality in life expectancy (%), 2023",
    "ineq_adj_life_exp_index_2023" = "Inequality-adjusted life expectancy index, 2023",
    
    "ineq_educ_perct_2023" = "Inequality in education (%), 2023",
    "ineq_adj_educ_index_2023" = "Inequality-adjusted education index, 2023",
    
    "ineq_income_perct_2022" = "Inequality in income distribution (%), 2022",
    "ineq_adj_income_index_2022" = "Inequality-adjusted income index, 2022",
    
    "inc_shares_poor40perct_2010_2023" = "Income share of poorest 40%, 2010–2023",
    "inc_shares_rich10perct_2010_2023" = "Income share of richest 10%, 2010–2023",
    "inc_shares_rich1perct_2010_2023" = "Income share of richest 1%, 2010–2023",
    
    "gini_coef_2010_2023" = "Gini coefficient, 2010–2023",
    
    
    # ==========================================================
    # Table 4 — Gender Development Index (GDI)
    # ==========================================================
      # ---- Identification ----
      "hdi_rank" = "HDI rank, 2023",
      "country"  = "Country",
      
      # ---- Core GDI indicators ----
      "gdi_2023" = "Gender Development Index (GDI), 2023",
      "gdi_group_2023" = "GDI group classification, 2023",
      
      "hdi_female_2023" = "Female HDI value, 2023",
      "hdi_male_2023" = "Male HDI value, 2023",
      
      "life_expect_birth_female_2023" = "Female life expectancy at birth, 2023",
      "life_expect_birth_male_2023" = "Male life expectancy at birth, 2023",
      
      "expected_yrs_school_female_2023" = "Expected years of schooling (female), 2023",
      "expected_yrs_school_male_2023" = "Expected years of schooling (male), 2023",
      
      "mean_yrs_school_female_2023" = "Mean years of schooling (women 25+), 2023",
      "mean_yrs_school_male_2023" = "Mean years of schooling (men 25+), 2023",
      
      "gross_nat_inc_capita_female_2023" = "Gross national income per capita (female), 2023",
      "gross_nat_inc_capita_male_2023" = "Gross national income per capita (male), 2023",
    
    
    # ==========================================================
    # Table 5 — Gender Inequality Index (GII)
    # ==========================================================
      # ---- Identification ----
      "hdi_rank" = "HDI rank, 2023",
      "country"  = "Country",
      
      # ---- Core GII indicators ----
      "gii_2023" = "Gender Inequality Index (GII), 2023",
      "gii_rank_2023" = "GII rank, 2023",
      
      "mater_mortal_ratio_2020" = "Maternal mortality ratio (per 100,000 births), 2020",
      "ado_birth_rate_2023" = "Adolescent birth rate (ages 15–19), 2023",
      
      "parliament_women_perct_2023" = "Women’s share of parliamentary seats (%), 2023",
      
      "female_secondary_educ_perct_2023" = "Women with at least secondary education (%), 2023",
      "male_secondary_educ_perct_2023" = "Men with at least secondary education (%), 2023",
      
      "female_labourforce_rate_perct_2023" = "Female labour force participation rate (%), 2023",
      "male_labourforce_rate_perct_2023" = "Male labour force participation rate (%), 2023",


    # ==========================================================
    # Table 7 — Planetary Pressures–Adjusted HDI (PHDI)
    # ==========================================================
      # ---- Identification ----
      "hdi_rank" = "HDI rank, 2023",
      "country"  = "Country",
      
      # ---- Core indices ----
      "hdi_2023"  = "Human Development Index (HDI), 2023",
      "phdi_2023" = "Planetary Pressures–Adjusted HDI (PHDI), 2023",
      
      "phdi_diff_hdi_perct_2023" = "Difference from HDI value (%), 2023",
      "phdi_diff_hdi_rank_2023" = "Difference from HDI rank, 2023",
      
      "adj_factor_planet_press_2023" = "Adjustment factor for planetary pressures, 2023",
      
      "co2_emissions_per_capita_tonnes_2023" = "CO₂ emissions per capita (tonnes), 2023",
      "co2_emissions_index_2023" = "CO₂ emissions index, 2023",
      
      "material_footprint_per_capita_tonnes_2023" = "Material footprint per capita (tonnes), 2023",
      "material_footprint_index_2023" = "Material footprint index, 2023"
    )
    

#Save it as .rds object
saveRDS(HDR_LABELS, "Support_Files/HDR_LABELS.rds")   
    



############## BUILD MASTER WVS&&HDR READABLE VARIABLES DICTIONARY##############
# ==========================================================
# 2. Build WVS7 country-level variable dictionary
# ==========================================================
# `orig_country_data` is built in `WVS_Wave7_Wrangling.R` from the line 
#orig_country_data <- readRDS("WVS_Dataset/WVS7_Country.rds")
#------------------------------------------------------------------
WVS7_COUNTRY_DICT <- orig_codebook_data %>%
  
  # Keep only variables flagged for display
  dplyr::filter(Variable_Display_Logical == "T") %>%
  
  dplyr::transmute(
    var_code = Col_ID,
    label    = ColLab,
    source   = "WVS7",
    section  = Section
  ) %>%
  
  dplyr::distinct(var_code, .keep_all = TRUE) %>%
  dplyr::arrange(var_code)


#Save files in .rds and .xlsx format
saveRDS(
  WVS7_COUNTRY_DICT,
  file = "Support_Files/WVS7_COUNTRY_DICT.rds"
)

writexl::write_xlsx(
  WVS7_COUNTRY_DICT,
  path = "Support_Files/WVS7_COUNTRY_DICT.xlsx"
)


#View(WVS7_COUNTRY_DICT)

# ==========================================================
# Create Country-level variable dictionary (HDR + WVS7)
# ==========================================================
FULL_COUNTRY_VAR_DICT <- dplyr::bind_rows(
  # ---- HDR country-level variables ----
  tibble::tibble(
    var_code = names(HDR_LABELS),
    label    = unname(HDR_LABELS),
    source   = "HDR",
    section  = "HDR"
  ),
  # ---- WVS7 country-level variables ----
  WVS7_COUNTRY_DICT
) %>%
  dplyr::distinct(var_code, .keep_all = TRUE)

#View(FULL_COUNTRY_VAR_DICT)

# Save files in .rds and .xlsx format
saveRDS(
  FULL_COUNTRY_VAR_DICT,
  file = "Support_Files/FULL_COUNTRY_VAR_DICT.rds"
)
writexl::write_xlsx(
  FULL_COUNTRY_VAR_DICT,
  path = "Support_Files/FULL_COUNTRY_VAR_DICT.xlsx"
)







# ============================================================
# 3. WVS7 Country-Level Variables Extraction
# ============================================================
# Purpose:
#   • Load the WVS Wave 7 country-level dataset
#   • Extract all available WVS indicators (questions)
#   • Remove identifier / metadata columns
#   • Produce a clean vector of variables for UI dropdowns
#
# Output:
#   • ALL_VARS_WVS7 — character vector of WVS indicator names
#
# Notes:
#   • This file does NOT modify data values
#   • It only prepares variable names for selection in the app
# ============================================================


# ------------------------------------------------------------
# Load WVS7 country-level dataset
# ------------------------------------------------------------
wvs7_ctry <- readRDS("WVS_Dataset/WVS7_Country.rds")


# ------------------------------------------------------------
# Extract all column names from the dataset
# ------------------------------------------------------------
wvs7_vars <- colnames(wvs7_ctry)
# print(wvs7_vars)   #  view all raw column names


# ------------------------------------------------------------
# Define identifier / metadata columns to exclude
# (These are not selectable indicators)
# ------------------------------------------------------------
wvs7_ID_vars <- c(
  "country",
  "B_COUNTRY",
  "B_COUNTRY_ALPHA"
)


# ------------------------------------------------------------
# Remove identifier columns to keep WVS indicators only
# ------------------------------------------------------------
# ALL_VARS_WVS7 <- setdiff(
#   wvs7_vars,
#   wvs7_ID_vars
# )
# 
# Check: inspect final list of WVS indicators
# print(ALL_VARS_WVS7)

WVS7_COUNTRY_VARS <- setdiff(wvs7_vars, wvs7_ID_vars)

#Check: inspect final list of WVS indicators
#print(WVS7_COUNTRY_VARS)








