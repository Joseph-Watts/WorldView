###############################################################################
# SCRIPT NAME: Build_master_dataset.R
# -----------------------------------------------------------------------------
# PURPOSE
# -----------------------------------------------------------------------------
# This script builds the final country-level master dataset used in the
# HDR–WVS Shiny application by integrating cleaned Human Development Report
# (HDR) indicators with World Values Survey Wave 7 (WVS7) country-level data.
#
# HDR data are used as the reference framework, and WVS7 variables are joined
# using ISO-3 country codes. 

#It has 2 parts:
  # PART I: build the authoritative lookup tables used to classify countries
  # in the HDR–WVS master dataset. These lookup tables assign each country to
  # Human Development Index (HDI) groups, geographic regions, and special HDR
  # analytical groups (e.g. OECD, Small Island Developing States, Least Developed
  # Countries).

  # PART II: Used the group lookup tables to build the final classified master
# dataset (MASTER_HDR_WVS7_CLASSIFIED) and for grouping, filtering, and
# benchmarking within the Shiny application.
###############################################################################







###############################################################################
# -----------------------------------------------------------------------------
# PART I CONTENTS
# -----------------------------------------------------------------------------
# This script performs the following steps:
#
# 1. Reads an Excel file containing country-to-area mappings, with one sheet
#    per classification type (HDI groups, regions, and special groups).
#
# 2. Combines these mappings into a unified lookup table with ISO-3 country
#    codes for validation and inspection.
#
# 3. Constructs three separate, purpose-specific lookup tables:
#    - HDR_HDI_GROUP_LOOKUP: categorical HDI group membership
#    - HDR_REGION_LOOKUP: geographic region classification
#    - HDR_SPECIAL_LOOKUP: special group membership
#
# 4. Applies targeted fixes for known ISO-3 coding inconsistencies (Rep of Korea) 
# to ensure reliable joins with HDR and WVS datasets.
###############################################################################
# =============================================================================
#       I.1. BUILD GROUP LOOK UP TABLES & ADD ISO3 COUNTRY CODES
# =============================================================================
###############################################################################
# Path to the Excel file that contains an area per sheet
#Note:`Area country list.xlsx` was manually created using official UNDP sources 
path <- "HDR_files/Area country list.xlsx"

# Names of the sheets to import from the file
selected_sheets <- c("hdi_groups", "regions", "special_groups")

# Read each sheet into a named list of data frames
area_files <- selected_sheets %>%
  set_names() %>%                     # Use sheet names as list element names
  purrr::map(~ read_excel(path, sheet = .x)) # Read each sheet into a data frame
#View(area_files$special_groups)




# Combine all area files into one unified look up table and clean it
HDR_AREA_LOOKUP <- bind_rows(area_files) %>%     # bind all data frames vertically into on long dataframe  `area_files`
  mutate(
    country = trimws(as.character(country)), # ensure "country" is character and remove leading/trailing spaces
    area    = trimws(as.character(area)),   # ensure "area" is character and remove leading/trailing spaces
    
    # Convert country names -> ISO3 codes using countrycode()
    iso3    = countrycode(country, "country.name", "iso3c")
  ) %>%
  
  # --- FIX KNOWN ISO3 CODE ISSUES ---
  mutate(
    iso3 = case_when(
      country == "Korea (Republic of)" ~ "KOR",  # Manually write ISO3 for "Korea (Republic of)" because it is mis-detected
      TRUE ~ iso3                                 # otherwise keep the automatically detected ISO3 code
    )
  )
#View(HDR_AREA_LOOKUP)

saveRDS(HDR_AREA_LOOKUP, "Support_Files/HDR_AREA_LOOKUP.rds")



#HDI GROUP lookup (pure categorical, no thresholds)
HDR_HDI_GROUP_LOOKUP <- area_files$hdi_groups %>%
  mutate(
    country = trimws(as.character(country)),
    hdr_group = trimws(as.character(area)),
    iso3 = countrycode(country, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3 = if_else(country == "Korea (Republic of)", "KOR", iso3)
  ) %>%
  select(iso3, hdr_group) %>%
  distinct()

#View(HDR_HDI_GROUP_LOOKUP)



#REGION lookup (one-to-one, complete)
HDR_REGION_LOOKUP <- area_files$regions %>%
  mutate(
    country = trimws(as.character(country)),
    hdr_region = trimws(as.character(area)),
    iso3 = countrycode(country, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3 = if_else(country == "Korea (Republic of)", "KOR", iso3)
  ) %>%
  select(iso3, hdr_region) %>%
  distinct()
#View(HDR_REGION_LOOKUP)



#SPECIAL GROUPS lookup (membership-based)
HDR_SPECIAL_LOOKUP <- area_files$special_groups %>%
  mutate(
    country = trimws(as.character(country)),
    special_group = trimws(as.character(area)),
    iso3 = countrycode(country, "country.name", "iso3c")
  ) %>%
  mutate(
    iso3 = if_else(country == "Korea (Republic of)", "KOR", iso3)
  ) %>%
  select(iso3, special_group) %>%
  distinct()
#View(HDR_SPECIAL_LOOKUP)









###############################################################################
# PART II CONTENTS
# -----------------------------------------------------------------------------
# This script performs the following steps:
#
# 1. Loads the WVS7 country-level dataset and aligns it with HDR data using
#    ISO-3 country codes.
#
# 2. Joins WVS7 country-level variables onto the cleaned HDR master dataset,
#    ensuring that all HDR countries are retained.
#
# 3. Adds the group classification lookup tables, including:
#    - Human Development Index (HDI) group membership,
#    - Geographic region,
#    - Special group indicators (OECD, Small Island Developing States, and
#      Least Developed Countries).
# -----------------------------------------------------------------------------
# OUTPUT
# -----------------------------------------------------------------------------
# The script produces and saves the following dataset:
#
# - MASTER_HDR_WVS7_CLASSIFIED.rds
#   A fully integrated, country-level dataset combining HDR indicators,
#   WVS7 variables, and HDR classification metadata. This dataset is the
#   primary data source used throughout the Shiny application.
# -----------------------------------------------------------------------------
# USAGE
# -----------------------------------------------------------------------------
# This script is executed during the data preparation stage and is not run
# at Shiny runtime. The saved master dataset is loaded in global.R and used
# for analysis, visualisation, and model fitting within the application.
###############################################################################

# ============================================
# II.1. Load WVS7 Country-level data
# ============================================

# Load WVS7 country dataset (country-level metadata)
wvs7_country <- readRDS("WVS_Dataset/WVS7_Country.rds")

# Name ISO3 column from WVS alpha-3 country code
# This ensures compatibility with HDR ISO3 codes
wvs7_country <- wvs7_country %>%
  mutate(iso3 = as.character(B_COUNTRY_ALPHA))

# ============================================
# II.2. Build HDR–WVS master (HDR as reference table)
# ============================================

# Join WVS country-level data onto HDR master table
# LEFT JOIN ensures ALL HDR countries are kept
master_HDR_WVS7_data <- HDRs_master_clean %>%
  left_join(wvs7_country, by = "iso3")

# CHECKPOINT: overlap between HDR and WVS
# Number of countries present in BOTH HDR and WVS
#length(intersect(HDRs_master_clean$iso3, wvs7_country$iso3))
# Expected: ~62 countries
#view(master_HDR_WVS7_data)

# =======================================================================
# II.3. Join Human development group classification (authoritative)
# ============================================
# Join HDI group from UNDP annex lookup
# Countries not classified by UNDP (e.g. PRK, Monaco) will retain NA
MASTER_HDR_WVS7_HDI <- master_HDR_WVS7_data %>%
  left_join(HDR_HDI_GROUP_LOOKUP, by = "iso3")

# Check HDI group distribution
#count(MASTER_HDR_WVS7_HDI, hdr_group, sort = TRUE)


# ============================================
# II.4. Join HDR region (one-to-one classification)
# ============================================
# Add geographic region for each country
MASTER_HDR_WVS7_HDI_REGION <- MASTER_HDR_WVS7_HDI %>%
  left_join(HDR_REGION_LOOKUP, by = "iso3")


# ============================================
# II.5. Add special group flags (OECD, SIDS, LDC)
# ============================================
# Create binary indicators for special HDR groups
MASTER_HDR_WVS7_HDI_REGION_SPECIAL <- MASTER_HDR_WVS7_HDI_REGION %>%
  mutate(
    is_oecd = iso3 %in% HDR_SPECIAL_LOOKUP$iso3[
      HDR_SPECIAL_LOOKUP$special_group == "OECD"
    ],
    is_sids = iso3 %in% HDR_SPECIAL_LOOKUP$iso3[
      HDR_SPECIAL_LOOKUP$special_group == "SIDS"
    ],
    is_dc  = iso3 %in% HDR_SPECIAL_LOOKUP$iso3[
      HDR_SPECIAL_LOOKUP$special_group == "DCs"
    ],
    is_ldc  = iso3 %in% HDR_SPECIAL_LOOKUP$iso3[
      HDR_SPECIAL_LOOKUP$special_group == "LCDs"
    ]
  )

#View(MASTER_HDR_WVS7_HDI_REGION_SPECIAL)
# ============================================
# II.6. Final classified master dataset
# ============================================
MASTER_HDR_WVS7_CLASSIFIED <- MASTER_HDR_WVS7_HDI_REGION_SPECIAL

# Final check
count(MASTER_HDR_WVS7_CLASSIFIED, hdr_group, sort = TRUE)
#Checking the 2 NAs in the count
# MASTER_HDR_WVS7_CLASSIFIED %>%
#   filter(is.na(hdr_group)) %>%
#   select(country, iso3)
# output
# 1 Korea (Democratic People's Rep. of) PRK  
# 2 Monaco                              MCO  


# Reorder columns so that classification variables appear immediately after iso3
# This improves readability and makes the dataset easier to inspect and use in Shiny
MASTER_HDR_WVS7_CLASSIFIED <- MASTER_HDR_WVS7_CLASSIFIED %>%
  relocate(
    # Classification columns to move
    hdr_group,
    hdr_region,
    is_oecd,
    is_sids,
    is_ldc,
    is_dc,
    # Place them directly after the ISO3 country code
    .after = iso3
  )



# Save the final classified master dataset to disk
# Save using here() to guarantee correct location
saveRDS(
  MASTER_HDR_WVS7_CLASSIFIED,
  here::here("Support_Files", "MASTER_HDR_WVS7_CLASSIFIED.rds")
)

#View(MASTER_HDR_WVS7_CLASSIFIED)






