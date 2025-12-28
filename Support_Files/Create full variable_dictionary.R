#==============================================================================
# STRUCTURED LIST FOR ALL HDRs TABLES
#==============================================================================
HDR_DATA <- list(
  "Table 1 - HDI & Components" = list(
    groups    = hdi_groups_table1,
    regions   = regions_table1,
    special   = special_groups_table1,
    countries = countries_table1
  ),
  
  "Table 2 - HDI Trends" = list(
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
  
  
  "Table 4 - GDI" = list(
    groups    = hdi_groups_table4,
    regions   = regions_table4,
    special   = special_groups_table4,
    countries = countries_table4
  ),
  
  
  "Table 5 - GII" = list(
    groups    = hdi_groups_table5,
    regions   = regions_table5,
    special   = special_groups_table5,
    countries = countries_table5
  ),
  
  
  "Table 7 - PHDI" = list(
    groups    = hdi_groups_table7,
    regions   = regions_table7,
    special   = special_groups_table7,
    countries = countries_table7
  )
)


#print(HDR_DATA)


#===================================================================================
# Create dataframe to store var_code, label, source, table and definition of HDR vars
#====================================================================================
HDR_DICT <- tibble::tibble(
  var_code   = names(HDR_LABELS),
  label      = unname(HDR_LABELS),
  source     = "HDR",
  definition = NA_character_,
  table      = NA_character_
)

View(HDR_DICT)

# ------------------------------------------------------------
# STEP 2: Assign HDR table names to HDR_DICT$table
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



# #==============================================================================
# # Extract var_code from HDR tables
# #==============================================================================
# # Extract variable codes as a vector (in dataframe order)
# vars <- HDR_DICT$var_code
# #Check
# length(vars)
# head(vars)
# tail(vars)
# # Print one variable per line (Excel-friendly)
# cat(vars, sep = "\n")



# ---------------------------------------------------------------------
# Fill the "definition" col of HDR_DICT using the Excel codebook
# ---------------------------------------------------------------------
library(readxl)
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
# Match WVS7_DICT col names to HDR_DICT col names 
#==============================================================================
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
WVS7_DICT %>%
  dplyr::count(var_code) %>%
  dplyr::filter(n > 1)

names(WVS7_DICT)
# should be: var_code label source table definition
setdiff(names(WVS7_DICT), names(HDR_DICT))
setdiff(names(HDR_DICT), names(WVS7_DICT))


#==============================================================================
# Drop duplicate vars (hdi_rank, hdi_2023, country). 
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
# Combine HDR + WVS7 dictionaries
#==============================================================================
FULL_VARIABLE_DICTIONARY <- dplyr::bind_rows(
  HDR_DICT,
  WVS7_DICT
)

#Sanity check 
View(FULL_VARIABLE_DICTIONARY)
#Final validation: source, var_code must be unique
FULL_VARIABLE_DICTIONARY %>%
  dplyr::count(source, var_code) %>%
  dplyr::filter(n > 1)
#Quick overview
table(FULL_VARIABLE_DICTIONARY$source)



# Save the MASTER_VARIABLE_DEFINITION
saveRDS(
FULL_VARIABLE_DICTIONARY,
"Support_Files/FULL_VARIABLE_DICTIONARY.rds"
)





