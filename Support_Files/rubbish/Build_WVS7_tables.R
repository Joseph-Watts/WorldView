# ============================================================
# WVS7 Country-Level Variables Extraction
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
print(WVS7_COUNTRY_VARS)







