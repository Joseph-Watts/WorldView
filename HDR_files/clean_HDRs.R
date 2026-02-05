library(readxl)
library(dplyr)
library(janitor)
library(writexl)




################ HELPER FUNCTIONS ####################################

# # 1. HARMONISE PLACEHOLDER FOR MISSING VALUE-----------------------------------
# fix_missing <- function(x) {
#   x <- trimws(x)  # remove leading/trailing spaces
#   
#   x[x %in% c("—", "-", "..", "...", "", "n/a", "N/A", "NA")] <- NA
#   return(x)
# }


#CLEAN COLUMN
clean_columns <- function(df,
                          remove_first = TRUE,
                          remove_all_dash = TRUE,
                          remove_all_na = TRUE) {
  
  # Remove first column if requested
  if (remove_first && ncol(df) > 1) {
    df <- df[, -1, drop = FALSE]
  }
  
  # Convert all "—" to NA BEFORE testing
  df[df == "—"] <- NA
  
  # Remove columns where ALL values were "—"
  if (remove_all_dash) {
    dash_cols <- sapply(df, function(col) all(is.na(col)))
    df <- df[, !dash_cols, drop = FALSE]
  }
  
  # Remove columns where ALL values are NA
  if (remove_all_na) {
    na_cols <- sapply(df, function(col) all(is.na(col)))
    df <- df[, !na_cols, drop = FALSE]
  }
  
  return(df)
}




#CONVERT DATA TYPES -----------------------------------------------------------
convert_types <- function(df,
                          numeric_cols = NULL,
                          categorical_cols = NULL,
                          country_as_factor = TRUE) {
  
  #Convert numeric columns 
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.numeric(df[[col]])
      }
    }
  }
  
  #Convert categorical/text columns 
  if (!is.null(categorical_cols)) {
    for (col in categorical_cols) {
      if (col %in% names(df)) {
        df[[col]] <- as.factor(df[[col]])
      }
    }
  }
  
  #Convert country column to factor (optional) 
  if ("country" %in% names(df) && country_as_factor) {
    df$country <- as.factor(df$country)
  }
  
  return(df)
}




# 4. DROP ROWS THAT ARE COMPLETY EMPTY (all NA)---------------------------------
#drop_empty_rows <- function(df) df[rowSums(is.na(df)) != ncol(df), ]




# 5. STANDARDISE MISSING_VALUES PLACE HOLDERS
#Replace all missing values placeholders with NAs
fix_miss_val <- function(x) {
  
  #Remove leading/trailing spaces so " .. " becomes ".."
  x <- trimws(x)
  
  #Replace known placeholders with NA. These values appear in HDR Excel files to mean "no data"
  missing_values <- c("—", "-", "..", "...", "", "n/a", "N/A", "NA")
  
  # Select elements of x that match the placeholders
  x[x %in% missing_values] <- NA
  
  return(x) #Return cleaned vector
}



# 6. Remove rows above col names (a.k.a. header)--------------------------------

remove_rows_above_header <- function(df, header_keyword = "HDI", ignore_case = TRUE) {
  #identify the header row by fuzzy matching
  #search for ANY cell containing the header keyword (default: "HDI")
  header_row <- which(
    apply(df, 1, function(r) any(grepl(header_keyword, r, ignore.case = ignore_case)))
  )[1]
  
  #Error if no matching header row is found
  if (is.na(header_row)) {
    stop(paste0("No header row found containing keyword: '", header_keyword, "'"))
  }
  
  #Keep the header row AND all rows below it
  df_clean <- df[header_row:nrow(df), , drop = FALSE]
  
  return(df_clean) #Return cleaned table
}



# 7. APPLY NEW COL NANES ----------------------------------------------------
# apply_new_colnames <- function(df, new_names, header_keyword = NULL) {
#   
#   #Assign new column names
#   if (length(new_names) != ncol(df)) {
#     stop("new_names must match the number of columns in df.")
#   }
#   colnames(df) <- new_names
#   
#   #If user wants to remove header row
#   if (!is.null(header_keyword)) {
#     df <- df[df[[1]] != header_keyword, ]
#   }
#   
#   return(df)
# }


################# GLOBAL VARIABLES ##################################
#----------- HEADERS INSIDE `country`
# Define section headers inside 'Country' (will be removed) 
section_headers <- c(
  "Other countries or territories",
  "Human development groups",
  "Regions"
)

#---------- CATEGORY LABELS
# Define HDI labels
# hdi_labels_hdr <- c(
#   "Very High Human Development",
#   "High Human Development",
#   "Medium Human Development",
#   "Low Human Development"
# )

#USED ONLY for grouping variables downstream
hdi_labels <- c(
  "Very High Human Development",
  "High Human Development",
  "Medium Human Development",
  "Low Human Development"
)


#USED ONLY to extract HDR group rows
# Define HDI labels 
hdi_labels_hdr <- c(
  "Very high human development",
  "High human development",
  "Medium human development",
  "Low human development"
)




# Define Regions labels 
region_labels <- c(
  "Arab States",
  "East Asia and the Pacific",
  "Europe and Central Asia",
  "Latin America and the Caribbean",
  "South Asia",
  "Sub-Saharan Africa"
)

# Define Special groups labels
specialgroup_labels <- c(
  "Least developed countries",
  "Developing countries",
  "Small island developing states",
  "Organisation for Economic Co-operation and Development",
  "World")


#-------------LOOK UP TABLES 
# look up table for HUMAN DEVELOPMENT GROUPS
hdi_group_lookup <- tibble::tribble(
  ~category_label,                    ~hdi_group,
  "Very High human Development",      "Very high",
  "High Human Development",           "High",
  "Medium Human Development",         "Medium",
  "Low Human Development",            "Low"
)


#look up table for REGIONS
region_lookup <- tibble::tribble(
  ~region_label,                    ~region_name,
  "Arab States",                    "Arab States",
  "East Asia and the Pacific",      "East Asia and the Pacific",
  "Europe and Central Asia",        "Europe and Central Asia",
  "Latin America and the Caribbean", "Latin America and the Caribbean",
  "South Asia",                     "South Asia",
  "Sub-Saharan Africa",             "Sub-Saharan Africa"
)



#####################CLEANING HDRs_TABLE 1 ####################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
#Read raw table
table1_raw<- readxl::read_excel("HDR_RawData/Table1_HDR25_Statistical_Annex_HDI.xlsx", col_names = FALSE)
#table1_raw
# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table1_clean <- table1_raw[
  ( 
    # Keep columns that are not fully empty
    colSums(!is.na(table1_raw)) > 0  &
      
      # Keep columns that contain at least one number
      sapply(table1_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table1_raw) == "country"
]

#View(table1_clean)


# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table1_clean <- table1_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table1_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table1_clean <- table1_clean[
  rowSums(!is.na(table1_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table1_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table1_clean <- remove_rows_above_header(table1_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table1_clean <- table1_clean[-c(1, 3), ]

#View(table1_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb1 <- c(
  "hdi_rank",
  "country",
  "hdi_2023",
  "life_expect_birth_2023",
  "expected_yrs_schooling_2023",
  "mean_yrs_schooling_2023",
  "gni_per_capita_2023",
  "gni_minus_hdi_rank_2023",
  "hdi_rank_2022"
)

# Apply the new cleaned column names
colnames(table1_clean) <- new_colnames_tb1
#View(table1_clean)

#Remove first 2 rows (it contains the original dirty header)
table1_clean <- table1_clean[-2, ]

#View(table1_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb1 <- which(table1_clean[[2]] == "Notes")

# Keep everything above the Notes row, remove Notes and below
table1_clean <- table1_clean[1:(notes_row_tb1 - 1), ]

#View(table1_clean)





# #Human development groups
# ## OPTIONAL: explicitly remove HDI group rows from country table
# hdi_groups_table1 <- table1_clean[
#   table1_clean$country %in% hdi_labels_hdr,
# ]




# --------------------- CONVERT DATATYPES -------------------------------------
# Convert to Numeric 
#list cols to convert into numeric
numeric_cols_tb1 <- setdiff(names(table1_clean), "country")
str(table1_clean)

#convert cols into numeric
table1_clean <- convert_types(
  df = table1_clean,
  numeric_cols = numeric_cols_tb1, # without "country"
  categorical_cols = NULL,  #no categorical conversions
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
#sapply(table1_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of 
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table1_clean <- table1_clean[
  
  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table1_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table1_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS --------
#Human development groups
hdi_groups_table1 <- table1_clean[
  table1_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table1 <- table1_clean[
  table1_clean$country %in% region_labels,
]

#Special groups
special_groups_table1 <- table1_clean[
  table1_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table1 <- table1_clean[
  !(table1_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


# Drop empty cols (`HDI rank`, `pphdi_2023`)
## countries_table1<- clean_columns(countries_table1)
## hdi_groups_table1<- clean_columns(hdi_groups_table1)
## regions_table1<- clean_columns(regions_table1)
## special_groups_table1<- clean_columns(special_groups_table1)


#View category tables
# View(countries_table1)
# View(hdi_groups_table1)
# View(regions_table1)
# View(special_groups_table1)


#Save each object as .rds
saveRDS(countries_table1, "Support_Files/countries_table1.rds")
saveRDS(hdi_groups_table1, "Support_Files/hdi_groups_table1.rds")
saveRDS(regions_table1, "Support_Files/regions_table1.rds")
saveRDS(special_groups_table1, "Support_Files/special_groups_table1.rds")





#####################CLEANING HDRs_TABLE 2 ############################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
table2_raw<- readxl::read_excel("HDR_RawData/Table2_HDR25_Statistical_Annex_HDI_Trends.xlsx", col_names = FALSE)

# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table2_clean <- table2_raw[
  (
    # Keep columns that are not fully empty
    colSums(!is.na(table2_raw)) > 0  &

      # Keep columns that contain at least one number
      sapply(table2_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table2_raw) == "country"
]

#View(table2_clean)


# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table2_clean <- table2_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table2_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table2_clean <- table2_clean[
  rowSums(!is.na(table2_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table2_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table2_clean <- remove_rows_above_header(table2_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table2_clean <- table2_clean[-c(1, 2), ]

#View(table2_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb2 <- c(
  "hdi_rank",
  "country",
  "hdi_1990",
  "hdi_2000",
  "hdi_2010",
  "hdi_2015",
  "hdi_2020",
  "hdi_2021",
  "hdi_2022",
  "hdi_2023",
  "change_hdi_rank_2015_2023",
  "avg_annual_hdi_growth_perct_1990_2000",
  "avg_annual_hdi_growth_perct_2000_2010",
  "avg_annual_hdi_growth_perct_2010_2023",
  "avg_annual_hdi_growth_perct_1990_2023"
)


# Apply the new cleaned column names
colnames(table2_clean) <- new_colnames_tb2
#View(table2_clean)

#Remove first row (it contains the original dirty header)
table2_clean <- table2_clean[-1, ]

#View(table2_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb2 <- which(table2_clean[[2]] == "Notes")

# Keep everything above the Notes row, remove Notes and below
table2_clean <- table2_clean[1:(notes_row_tb2 - 1), ]

#View(table2_clean)


# --------------------- CONVERT DATATYPES --------------------------------------
# Convert to Numeric
numeric_cols_tb2 <- setdiff(names(table2_clean), "country")
str(table2_clean)

#convert cols into numeric
table2_clean <- convert_types(
  df = table2_clean,
  numeric_cols = numeric_cols_tb2, # without "country"
  categorical_cols = NULL,  #no categorical conversions
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
#sapply(table2_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table2_clean <- table2_clean[

  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table2_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table2_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS ------
#Human development groups
hdi_groups_table2 <- table2_clean[
  table2_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table2 <- table2_clean[
  table2_clean$country %in% region_labels,
]

#Special groups
special_groups_table2 <- table2_clean[
  table2_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table2 <- table2_clean[
  !(table2_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


##Drop empty cols (`HDI rank`, `change_hdi_rank_2015_2023`)
## countries_table2<- clean_columns(countries_table2)
## hdi_groups_table2<- clean_columns(hdi_groups_table2)
## regions_table2<- clean_columns(regions_table2)
## special_groups_table2<- clean_columns(special_groups_table2)


# View category tables
View(countries_table2)
View(hdi_groups_table2)
View(regions_table2)
View(special_groups_table2)


#Save each object as .rds
saveRDS(countries_table2, "Support_Files/countries_table2.rds")
saveRDS(hdi_groups_table2, "Support_Files/hdi_groups_table2.rds")
saveRDS(regions_table2, "Support_Files/regions_table2.rds")
saveRDS(special_groups_table2, "Support_Files/special_groups_table2.rds")


#save countries_table2 in excel file 
writexl::write_xlsx(countries_table2, "Support_Files/countries_table2.xlsx")




#####################CLEANING HDRs_TABLE 3 ####################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
table3_raw<- readxl::read_excel("HDR_RawData/Table3_HDR25_Statistical_Annex_IHDI.xlsx", col_names = FALSE)

# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table3_clean <- table3_raw[
  (
    # Keep columns that are not fully empty
    colSums(!is.na(table3_raw)) > 0  &

      # Keep columns that contain at least one number
      sapply(table3_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table3_raw) == "country"
]

#View(table3_clean)


# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table3_clean <- table3_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table3_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table3_clean <- table3_clean[
  rowSums(!is.na(table3_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table3_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table3_clean <- remove_rows_above_header(table3_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table3_clean <- table3_clean[-c(1, 2), ]

#View(table3_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb3 <- c(
  "hdi_rank",
  "country",
  "hdi_2023",
  "ihdi_2023",
  "ihdi_loss_perct_2023",
  "ihdi_diff_hdirank_2023",
  "coef_human_inequality_2023",
  "ineq_life_exp_perct_2023",
  "ineq_adj_life_exp_index_2023",
  "ineq_educ_perct_2023",
  "ineq_adj_educ_index_2023",
  "ineq_income_perct_2022",
  "ineq_adj_income_index_2022",
  "inc_shares_poor40perct_2010_2023",
  "inc_shares_rich10perct_2010_2023",
  "inc_shares_rich1perct_2010_2023",
  "gini_coef_2010_2023")

# Apply the new cleaned column names
colnames(table3_clean) <- new_colnames_tb3
#View(table3_clean)

#Remove first row (it contains the original dirty header)
table3_clean <- table3_clean[-1, ]

#View(table3_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb3 <- which(table3_clean[[2]] == "Notes")

# Keep everything above the Notes row, remove Notes and below
table3_clean <- table3_clean[1:(notes_row_tb3 - 1), ]

#View(table3_clean)



# --------------------- CONVERT DATATYPES -------------------------------------
# Convert to Numeric
#list cols to convert into numeric
numeric_cols_tb3 <- setdiff(names(table3_clean), "country")
#str(table3_clean)

#convert cols into numeric
table3_clean <- convert_types(
  df = table3_clean,
  numeric_cols = numeric_cols_tb3, # without "country"
  categorical_cols = NULL,  #no categorical conversions
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
sapply(table3_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table3_clean <- table3_clean[

  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table3_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table3_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS --------
#Human development groups
hdi_groups_table3 <- table3_clean[
  table3_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table3 <- table3_clean[
  table3_clean$country %in% region_labels,
]

#Special groups
special_groups_table3 <- table3_clean[
  table3_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table3 <- table3_clean[
  !(table3_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


#Drop empty cols (`HDI rank`, `change_hdi_rank_2015_2023`)
## countries_table3<- clean_columns(countries_table3)
## hdi_groups_table3<- clean_columns(hdi_groups_table3)
## regions_table3<- clean_columns(regions_table3)
## special_groups_table3<- clean_columns(special_groups_table3)


#View category tables
# View(countries_table3)
# View(hdi_groups_table3)
# View(regions_table3)
# View(special_groups_table3)



#Save each object as .rds
saveRDS(countries_table3, "Support_Files/countries_table3.rds")
saveRDS(hdi_groups_table3, "Support_Files/hdi_groups_table3.rds")
saveRDS(regions_table3, "Support_Files/regions_table3.rds")
saveRDS(special_groups_table3, "Support_Files/special_groups_table3.rds")




#####################CLEANING HDRs_TABLE 4 ####################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
table4_raw<- readxl::read_excel("HDR_RawData/Table4_HDR25_Statistical_Annex_GDI.xlsx", col_names = FALSE)

# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table4_clean <- table4_raw[
  ( 
    # Keep columns that are not fully empty
    colSums(!is.na(table4_raw)) > 0  &
      
      # Keep columns that contain at least one number
      sapply(table4_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table4_raw) == "country"
]

#View(table4_clean)


# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table4_clean <- table4_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table4_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table4_clean <- table4_clean[
  rowSums(!is.na(table4_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table4_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table4_clean <- remove_rows_above_header(table4_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table4_clean <- table4_clean[-c(1, 2), ]

#View(table5_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb4 <- c(
  "hdi_rank",
  "country",
  "gdi_2023",
  "gdi_group_2023",
  "hdi_female_2023",
  "hdi_male_2023",
  "life_expect_birth_female_2023",
  "life_expect_birth_male_2023",
  "expected_yrs_school_female_2023",
  "expected_yrs_school_male_2023",
  "mean_yrs_school_female_2023",
  "mean_yrs_school_male_2023",
  "gross_nat_inc_capita_female_2023",
  "gross_nat_inc_capita_male_2023")


# Apply the new cleaned column names
colnames(table4_clean) <- new_colnames_tb4
#View(table4_clean)

#Remove first row (it contains the original dirty header)
table4_clean <- table4_clean[-1, ]

#View(table4_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb4 <- which(table4_clean[[2]] == "Notes")

# Keep everything above the Notes row, remove Notes and below
table4_clean <- table4_clean[1:(notes_row_tb4 - 1), ]

#View(table4_clean)



# --------------------- CONVERT DATATYPES -------------------------------------
# Convert to Numeric 
#list cols to convert into numeric
numeric_cols_tb4 <- setdiff(names(table4_clean), "country")
str(table4_clean)

#convert cols into numeric
table4_clean <- convert_types(
  df = table4_clean,
  numeric_cols = numeric_cols_tb4, # without "country"
  categorical_cols = "gdi_group_2023",  #categorical conversion
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
sapply(table4_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of 
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table4_clean <- table4_clean[
  
  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table4_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table4_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS --------
#Human development groups
hdi_groups_table4 <- table4_clean[
  table4_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table4 <- table4_clean[
  table4_clean$country %in% region_labels,
]

#Special groups
special_groups_table4 <- table4_clean[
  table4_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table4 <- table4_clean[
  !(table4_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


#Drop empty cols (`HDI rank`, `gdi_group_2023`)
## countries_table4<- clean_columns(countries_table4)
## hdi_groups_table4<- clean_columns(hdi_groups_table4)
## regions_table4<- clean_columns(regions_table4)
## special_groups_table4<- clean_columns(special_groups_table4)


#View category tables
# View(countries_table4)
# View(hdi_groups_table4)
# View(regions_table4)
# View(special_groups_table4)



#Save each object as .rds
saveRDS(countries_table4, "Support_Files/countries_table4.rds")
saveRDS(hdi_groups_table4, "Support_Files/hdi_groups_table4.rds")
saveRDS(regions_table4, "Support_Files/regions_table4.rds")
saveRDS(special_groups_table4, "Support_Files/special_groups_table4.rds")

#####################CLEANING HDRs_TABLE 5 ####################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
table5_raw<- readxl::read_excel("HDR_RawData/Table5_HDR25_Statistical_Annex_GII.xlsx", col_names = FALSE)

# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table5_clean <- table5_raw[
  ( 
    # Keep columns that are not fully empty
    colSums(!is.na(table5_raw)) > 0  &
      
      # Keep columns that contain at least one number
      sapply(table5_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table5_raw) == "country"
]

#View(table5_clean)

# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table5_clean <- table5_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table5_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table5_clean <- table5_clean[
  rowSums(!is.na(table5_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table5_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table5_clean <- remove_rows_above_header(table5_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table5_clean <- table5_clean[-c(1, 2), ]

#View(table5_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb5 <- c(
  "hdi_rank",
  "country",
  "gii_2023",
  "gii_rank_2023",
  "mater_mortal_ratio_2020",
  "ado_birth_rate_2023",
  "parliament_women_perct_2023",
  "female_secondary_educ_perct_2023",
  "male_secondary_educ_perct_2023",
  "female_labourforce_rate_perct_2023",
  "male_labourforce_rate_perct_2023")


# Apply the new cleaned column names
colnames(table5_clean) <- new_colnames_tb5
#View(table5_clean)

#Remove first row (it only has the sub header very high human development)
table5_clean <- table5_clean[-1, ]

#View(table5_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb5 <- which(table5_clean[[2]] == "Notes")

# Keep everything above the Notes row, remove Notes and below
table5_clean <- table5_clean[1:(notes_row_tb5 - 1), ]

#View(table5_clean)



# --------------------- CONVERT DATATYPES -------------------------------------
# Convert to Numeric 
#list cols to convert into numeric
numeric_cols_tb5 <- setdiff(names(table5_clean), "country")
#str(table5_clean)

#convert cols into numeric
table5_clean <- convert_types(
  df = table5_clean,
  numeric_cols = numeric_cols_tb5, # without "country"
  categorical_cols = NULL,  #no categorical conversions
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
#sapply(table5_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of 
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table5_clean <- table5_clean[
  
  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table5_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table5_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS --------
#Human development groups
hdi_groups_table5 <- table5_clean[
  table5_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table5 <- table5_clean[
  table5_clean$country %in% region_labels,
]

#Special groups
special_groups_table5 <- table5_clean[
  table5_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table5 <- table5_clean[
  !(table5_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


#Drop empty cols (`HDI rank`, `gii_rank_2023`)
## countries_table5<- clean_columns(countries_table5)
## hdi_groups_table5<- clean_columns(hdi_groups_table5)
## regions_table5<- clean_columns(regions_table5)
## special_groups_table5<- clean_columns(special_groups_table5)


# View category tables
# View(countries_table5)
# View(hdi_groups_table5)
# View(regions_table5)
# View(special_groups_table5)



#Save each object as .rds
saveRDS(countries_table5, "Support_Files/countries_table5.rds")
saveRDS(hdi_groups_table5, "Support_Files/hdi_groups_table5.rds")
saveRDS(regions_table5, "Support_Files/regions_table5.rds")
saveRDS(special_groups_table5, "Support_Files/special_groups_table5.rds")


#####################CLEANING HDRs_TABLE 7 ####################################
# ------------- LOAD DATA WITHOUT COL NAME -------------------------------------
table7_raw<- readxl::read_excel("HDR_RawData/Table7_HDR25_Statistical_Annex_PHDI.xlsx", col_names = FALSE)

# -------------- REMOVE FULLY EMPTY COLS & NON-NUMERIC COLS --------------------
table7_clean <- table7_raw[
  ( 
    # Keep columns that are not fully empty
    colSums(!is.na(table7_raw)) > 0  &
      
      # Keep columns that contain at least one number
      sapply(table7_raw, function(col) any(grepl("[0-9]", col)))
  )
  # ALSO keep the Country column even if it has no numbers
  | names(table7_raw) == "country"
]

#View(table7_clean)


# --------------------- STANDARDISE MISSING VALUES PLACEHOLDERS ----------------
# Apply the fix_miss_val() function to EVERY column of table3_clean
table7_clean <- table7_clean %>%
  dplyr::mutate(across(everything(), fix_miss_val))

#View(table7_clean)

# --------------------- REMOVE FULLY EMPTY ROWS -------------------------------
table7_clean <- table7_clean[
  rowSums(!is.na(table7_clean)) > 0,   # keep only rows with at least one non-NA value
]

#View(table7_clean)
# --------------------- REMOVE ROWS ABOVE THE TRUE HEADER ---------------------
# Find and remove the row where the real table header starts ("HDI rank" in the first column)
table7_clean <- remove_rows_above_header(table7_clean, header_keyword = "HDI rank")

#Remove manually the 2 rows above header which are not removed by the function
#`remove_rows_above_header` because they have merged cells
table7_clean <- table7_clean[-c(1, 2), ]

#View(table7_clean)

# --------------------- RENNAME COLUMNS  ---------------------------------------
# list new col names
new_colnames_tb7 <- c(
  "hdi_rank",                                  # HDI rank
  "country",                                   # Country name
  "hdi_2023",                                  # HDI value 2023
  "phdi_2023",                                 # PHDI value 2023
  "phdi_diff_hdi_perct_2023",                  # % difference from HDI value
  "phdi_diff_hdi_rank_2023",                   # rank difference from HDI rank
  "adj_factor_planet_press_2023",              # Adjustment factor for planetary pressures
  "co2_emissions_per_capita_tonnes_2023",      # CO2 per capita (tonnes)
  "co2_emissions_index_2023",                  # CO2 index
  "material_footprint_per_capita_tonnes_2023", # Material footprint per capita (tonnes)
  "material_footprint_index_2023")              # Material footprint index


# Apply the new cleaned column names
colnames(table7_clean) <- new_colnames_tb7
#View(table7_clean)

#Remove first row (it contains the original dirty header)
table7_clean <- table7_clean[-1, ]

#View(table7_clean)

# --------------------- DROP BAD ROWS AT THE BOTTOM ----------------------------
# HDR tables often include notes or text blocks at the bottom.
# These rows have non-numeric values in hdi_rank (e.g., "Notes:", "HDI: ...").
# Remove footer rows starting at "Notes"
notes_row_tb7 <- which(table7_clean[[2]] == "Footnotes")

# Keep everything above the Notes row, remove Notes and below
table7_clean <- table7_clean[1:(notes_row_tb7 - 1), ]

#View(table7_clean)



# --------------------- CONVERT DATATYPES -------------------------------------
# Convert to Numeric 
#list cols to convert into numeric
numeric_cols_tb7 <- setdiff(names(table7_clean), "country")
str(table7_clean)

#convert cols into numeric
table7_clean <- convert_types(
  df = table7_clean,
  numeric_cols = numeric_cols_tb7, # without "country"
  categorical_cols = NULL,  #no categorical conversions
  country_as_factor = FALSE   # keep country as character
)

#check datatypes
#sapply(table7_clean, class)


# -----------------  REMOVE EMBEDDED SUBCATEGORY HEADERS  ---------------------
# Remove rows where ALL columns (except the country column) are NA => it remove the header rows of 
#the categories "very high human development", " High Human development", "Medium Human development",
#"Low Human Development", Other countries or territories", ", Human development groups", "Regions"
table7_clean <- table7_clean[
  
  # keep rows where NOT all non-country columns are NA
  !apply(
    is.na(table7_clean[, -2]), 1, #select all columns except column 2 (the country name)
    #apply the function row-by-row (1 = rows)
    all ),]                    #check if ALL values in the row are TRUE (i.e., all NA)

#View(table7_clean)


# ------------- SPLIT `country` INTO CATEGORY TABLES & DROP EMPTY COLS --------
#Human development groups
hdi_groups_table7 <- table7_clean[
  table7_clean$country %in% hdi_labels_hdr,
]

#Regions
regions_table7 <- table7_clean[
  table7_clean$country %in% region_labels,
]

#Special groups
special_groups_table7 <- table7_clean[
  table7_clean$country %in% specialgroup_labels,
]


#Countries only
countries_table7 <- table7_clean[
  !(table7_clean[[2]] %in% c(
    hdi_labels_hdr,
    region_labels,
    specialgroup_labels,
    section_headers
  )),
]


# Drop empty cols (`HDI rank`, `pphdi_2023`)
## countries_table7<- clean_columns(countries_table7)
## hdi_groups_table7<- clean_columns(hdi_groups_table7)
## regions_table7<- clean_columns(regions_table7)
## special_groups_table7<- clean_columns(special_groups_table7)


#View category tables
# View(countries_table7)
# View(hdi_groups_table7)
# View(regions_table7)
# View(special_groups_table7)




#Save each object as .rds
saveRDS(countries_table7, "Support_Files/countries_table7.rds")
saveRDS(hdi_groups_table7, "Support_Files/hdi_groups_table7.rds")
saveRDS(regions_table7, "Support_Files/regions_table7.rds")
saveRDS(special_groups_table7, "Support_Files/special_groups_table7.rds")








###SAVE CLEAN HDRs TABLES
saveRDS(table1_clean, "Support_Files/table1_clean.rds")
saveRDS(table2_clean, "Support_Files/table2_clean.rds")
saveRDS(table3_clean, "Support_Files/table3_clean.rds")
saveRDS(table4_clean, "Support_Files/table4_clean.rds")
saveRDS(table5_clean, "Support_Files/table5_clean.rds")
saveRDS(table7_clean, "Support_Files/table7_clean.rds")









###############################################################################
##################### create ?????          ####################################
########################################## ####################################
#1. Official HDR aggregate labels (EXTRACTION ONLY)
hdi_labels_extract <- c(
  "Very High Human Development",
  "High Human Development",
  "Medium Human Development",
  "Low Human Development"
)


#USED ONLY to extract HDR group rows
# Define HDI labels 
hdi_labels_hdr_extract <- c(
  "Very high human development",
  "High human development",
  "Medium human development",
  "Low human development"
)




# Define Regions labels 
region_labels_extract <- c(
  "Arab States",
  "East Asia and the Pacific",
  "Europe and Central Asia",
  "Latin America and the Caribbean",
  "South Asia",
  "Sub-Saharan Africa"
)

# Define Special groups labels
specialgroup_labels_extract <- c(
  "Least developed countries",
  "Developing countries",
  "Small island developing states",
  "Organisation for Economic Co-operation and Development",
  "World")







###########################################################################################################


HDR_LABELS <- readRDS("Support_files/HDR_LABELS.rds")
View(HDR_LABELS)

HDR_GROUP_BENCHMARKS <-readRDS("Support_files/HDR_GROUP_BENCHMARKS.rds")
#View(HDR_GROUP_BENCHMARKS)

HDR_GROUP_LOOKUP<- readRDS("Support_files/HDR_GROUP_LOOKUP.rds")
#View(HDR_GROUP_LOOKUP)


FULL_COUNTRY_VAR_DICT<- readRDS("Support_Files/FULL_COUNTRY_VAR_DICT.rds")
#View(FULL_COUNTRY_VAR_DICT)


FULL_VARIABLE_DICTIONARY<- readRDS("Support_Files/FULL_VARIABLE_DICTIONARY.rds")
#View(FULL_VARIABLE_DICTIONARY)



MASTER_HDR_WVS7_CLASSIFIED<- readRDS("Support_Files/MASTER_HDR_WVS7_CLASSIFIED.rds")
View(MASTER_HDR_WVS7_CLASSIFIED)

MASTER_HDR_WVS7_CLASSIFIED<- readRDS("Support_Files/MASTER_HDR_WVS7_CLASSIFIED.rds")
#str(MASTER_HDR_WVS7_CLASSIFIED)


MASTER_COUNTRY_DATA<- readRDS("Support_Files/MASTER_COUNTRY_DATA.rds")
#View(MASTER_COUNTRY_DATA)


HDR_GROUP_LOOKUP<- readRDS("Support_Files/HDR_GROUP_LOOKUP.rds")
#View(HDR_GROUP_LOOKUP)





table2_clean <- readRDS("Support_Files/table2_clean.rds")
#View(table2_clean)



hdr_table2_aggregates_long <- readRDS("Support_Files/hdr_table2_aggregates_long.rds")
#View(hdr_table2_aggregates_long)
#unique(hdr_table2_aggregates_long$group_type)


hdr_table2_ctry_long <- readRDS("Support_Files/hdr_table2_ctry_long.rds")
#View(hdr_table2_ctry_long)





m <- lmm_group_model()

#head(predict(m))
#summary(predict(m))
#anyNA(predict(m))












