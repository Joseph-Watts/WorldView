###############################################################################
# SCRIPT NAME: Create_full_variable_dictionary.R
# -----------------------------------------------------------------------------
# PURPOSE
# -----------------------------------------------------------------------------
# This script builds a unified, structured variable dictionary that combined 
# variable definitions of both datasets, the Human Development Reports (HDRs) and 
# the World Values Survey (WVS7).
# The resulting dictionary links each variable code to a human-readable
# label, data source, originating table/section, and (where available)
# a textual definition. 
#This object is used throughout the Shiny app to:
#  - populate dropdown menus, 
#  - display labels and tooltips
# -----------------------------------------------------------------------------
# CONTENTS
# -----------------------------------------------------------------------------
# This script performs the following steps:
#
# 1. Organises all cleaned HDR tables into a structured list (HDR_DATA),
#    separating country-level data from aggregate groups, regions, and
#    special groups.
#
# 2. Constructs an initial HDR variable dictionary (HDR_DICT) containing:
#    - variable codes
#    - human-readable labels
#    - data source (HDR)
#    - placeholder fields for table and definition
#
# 3. Assigns each HDR variable to its corresponding HDR table by inspecting
#    country-level columns in HDR_DATA.
#
# 4. Enriches the HDR dictionary with formal variable definitions by joining
#    an external HDR codebook (Excel).
#
# 5. Aligns the WVS7 country-level variable dictionary to the same schema
#    (var_code, label, source, table, definition).
#
# 6. Removes duplicate variables shared across HDR tables (e.g. country,
#    HDI rank) to ensure uniqueness.
#
# 7. Combines the HDR and WVS7 dictionaries into a single master dictionary
#    (FULL_VARIABLE_DICTIONARY).
#
# -----------------------------------------------------------------------------
# OUTPUT
# -----------------------------------------------------------------------------
# The script produces and saves the following object:
#
# - FULL_VARIABLE_DICTIONARY.rds
#   A unified variable dictionary containing metadata for all HDR and WVS7
#   country-level variables used in the project.
#
# -----------------------------------------------------------------------------
# USAGE
# -----------------------------------------------------------------------------
# This script is executed during the data preparation stage (not at Shiny
# runtime). The saved dictionary is later loaded in global.R and used to
# drive dynamic UI elements, variable selection, and consistent labeling
# across the application.
#
###############################################################################
library(readxl)





#==============================================================================
# 1. STRUCTURED LIST FOR ALL HDRs TABLES
#==============================================================================
HDR_DATA <- list(
  "Table 1 - HDI & Components" = list(
    groups    = hdi_groups_table1,
    regions   = regions_table1,
    special   = special_groups_table1,
    countries = countries_table1
  ),
  
  "Table 2 - HDI Trends, 1990-2023" = list(
    groups    = hdi_groups_table2,
    regions   = regions_table2,
    special   = special_groups_table2,
    countries = countries_table2
  ),
  
  "Table 3 - Inequality-adjusted HDI" = list(
    groups    = hdi_groups_table3,
    regions   = regions_table3,
    special   = special_groups_table3,
    countries = countries_table3
  ),
  
  
  "Table 4 - Gender Development Index" = list(
    groups    = hdi_groups_table4,
    regions   = regions_table4,
    special   = special_groups_table4,
    countries = countries_table4
  ),
  
  
  "Table 5 - Gender Inequality Index" = list(
    groups    = hdi_groups_table5,
    regions   = regions_table5,
    special   = special_groups_table5,
    countries = countries_table5
  ),
  
  
  "Table 7 - Planetary Pressures-adjusted HDI" = list(
    groups    = hdi_groups_table7,
    regions   = regions_table7,
    special   = special_groups_table7,
    countries = countries_table7
  )
)


#View(HDR_DATA)


#===================================================================================
# 2.1. Create dataframe to store var_code, label, source, table and definition of HDR vars
#====================================================================================
#Note: HDR_LABELS is created in 'HDR_WVS7_dropdown_choices.R' 
HDR_DICT <- tibble::tibble(
  var_code   = names(HDR_LABELS),
  label      = unname(HDR_LABELS),
  source     = "HDR",
  definition = NA_character_,
  table      = NA_character_
)

#View(HDR_DICT)

# ------------------------------------------------------------
# 2.2 Assign HDR table names to HDR_DICT$table
# ------------------------------------------------------------
for (tbl_name in names(HDR_DATA)) {
  
  # Extract the country-level data frame for this HDR table
  df_countries <- HDR_DATA[[tbl_name]]$countries
  
  # Get variable names from the table
  # (exclude identifier columns)
  vars_in_table <- setdiff(
    names(df_countries),
    c("country")  #exclude country from indicators
  )
  
  # Assign the table name to matching variables in HDR_DICT
  HDR_DICT$table[HDR_DICT$var_code %in% vars_in_table] <- tbl_name
}
#View(HDR_DICT)



#Sanity check:
# How many variables got a table assigned?
#table(HDR_DICT$table, useNA = "ifany")

# Which HDR variables did NOT get a table?
#HDR_DICT %>%
#  dplyr::filter(is.na(table)) %>%
#  dplyr::select(var_code, label)



# ---------------------------------------------------------------------
# 2.3. Fill the "definition" col of HDR_DICT using the Excel codebook
# ---------------------------------------------------------------------
#read codebook
hdr_codebook <- readxl::read_excel(
  "Support_Files/HDR variables codebook.xlsx"
)


#Fill definition col using left-join
HDR_DICT <- HDR_DICT %>%
dplyr::left_join(
  hdr_codebook %>%
    dplyr::select(var_code, definition),
  by = "var_code",
  suffix = c("", "_xl")
) %>%
  dplyr::mutate(
    definition = dplyr::coalesce(definition_xl, definition)
  ) %>%
  dplyr::select(-definition_xl)

#View(HDR_DICT)



#==============================================================================
# 3. Match WVS7_DICT col names to HDR_DICT col names 
#==============================================================================
# WVS7_COUNTRY_DICT is built in the R script 'HDR_WVS_dropdown_choices.R'
WVS7_DICT <- WVS7_COUNTRY_DICT %>%
  dplyr::transmute(
    var_code   = var_code,
    label      = label,
    source     = source,      # already "WVS7"
    table      = section,     # section becomes table
    definition = NA_character_
  )

#View(WVS7_DICT)


#Sanity check
# WVS7_DICT %>%
#   dplyr::count(var_code) %>%
#   dplyr::filter(n > 1)

#names(WVS7_DICT)
# should be: var_code label source table definition
#setdiff(names(WVS7_DICT), names(HDR_DICT))
#setdiff(names(HDR_DICT), names(WVS7_DICT))
#==============================================================================
# Drop duplicate vars (hdi_rank, country). 
#Must be done before combining HDR+WVS7
#==============================================================================
HDR_DICT <- HDR_DICT %>%
  dplyr::group_by(source, var_code) %>%
  dplyr::summarise(
    label      = dplyr::first(label),
    table      = dplyr::first(na.omit(table)),
    definition = dplyr::first(na.omit(definition)),
    .groups = "drop"
  )


#==============================================================================
# 4. Combine HDR + WVS7 dictionaries
#==============================================================================
FULL_VARIABLE_DICTIONARY <- dplyr::bind_rows(
  HDR_DICT,
  WVS7_DICT
)

#Sanity check 
#View(FULL_VARIABLE_DICTIONARY)

#Final validation: source, var_code must be unique
# FULL_VARIABLE_DICTIONARY %>%
#   dplyr::count(source, var_code) %>%
#   dplyr::filter(n > 1)
#Quick overview
#table(FULL_VARIABLE_DICTIONARY$source)



# Save the MASTER_VARIABLE_DEFINITION
saveRDS(
FULL_VARIABLE_DICTIONARY,
"Support_Files/FULL_VARIABLE_DICTIONARY.rds"
)






