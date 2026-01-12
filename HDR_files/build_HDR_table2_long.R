# ==========================================================
# build_HDR_table2_country_long.R
# ----------------------------------------------------------
# Purpose:
#   - Construct a clean longitudinal (long-format) dataset
#     from HDR Table 2 (HDI trends over time)
#
# Output:
#   - One row per country-year
#   - Suitable for Linear Mixed Models (LMM)
#
# Output file:
#   Support_Files/HDR_TABLE_2_LONG.rds
# ==========================================================


library(dplyr)
library(tidyr)
library(readxl)
library(janitor)
library(here)
library(stringr)






# ----------------------------------------------------------
# Load HDR Table 2 (raw)
# ----------------------------------------------------------
countries_table2 <- readxl::read_excel(
  here::here("Support_Files", "countries_table2.xlsx")
)

#View(countries_table2)
# ----------------------------------------------------------
# Pivot wide table into long table2
# ----------------------------------------------------------
hdr_table2_ctry_long <- countries_table2 %>%
  pivot_longer(
    cols = matches("^hdi_\\d{4}$"), #pivot ONLY columns that are HDI-by-year
    names_to = "year",
    values_to = "hdi"
  ) %>%
  mutate(
    year = as.integer(sub("hdi_", "", year))
  ) %>%
  filter(!is.na(hdi))

#View(hdr_table2_ctry_long)


# ----------------------------------------------------------
# Drop unnecessary columns
# ----------------------------------------------------------
hdr_table2_ctry_long <- hdr_table2_ctry_long %>%
  dplyr::select(
    -starts_with("avg_annual_hdi_growth"),
    -starts_with("change_hdi_rank"),
    -hdi_rank
  )


#View(hdr_table2_ctry_long)



# ----------------------------------------------------------
# Join country-level classifications onto Table 2 long
# ----------------------------------------------------------
hdr_table2_ctry_long <- hdr_table2_ctry_long %>%
  dplyr::left_join(
    MASTER_HDR_WVS7_CLASSIFIED %>%
      dplyr::select(
        country,
        iso3,
        hdr_group,
        hdr_region,
        is_oecd,
        is_sids,
        is_ldc
      ),
    by = "country"
  )



# ----------------------------------------------------------
# Center time variable (default behaviour)
# ----------------------------------------------------------
# Compute the mean observed year across all countries
mean_year <- mean(hdr_table2_ctry_long$year, na.rm = TRUE)

# Add centered time variable
hdr_table2_ctry_long <- hdr_table2_ctry_long %>%
  dplyr::mutate(
    year_c = year - mean_year
  )

# Optional: store the reference year as an attribute
attr(hdr_table2_ctry_long, "year_center_reference") <- mean_year

View(hdr_table2_ctry_long)


saveRDS(hdr_table2_ctry_long, "Support_Files/hdr_table2_ctry_long.rds")
write.xlsx(hdr_table2_ctry_long, "Support_Files/hdr_table2_ctry_long.xlsx")









# ==========================================================
# Build HDR Table 2 – Aggregate groups (long format)
# ----------------------------------------------------------
# Purpose:
#   • Extract aggregate rows from table2_clean.rds
#   • Create a clean long-format dataset for group-level LMMs
#   • Separate aggregates from country-level data by design
#
# Output:
#   • hdr_table2_aggregates_long
# ==========================================================


# ----------------------------------------------------------
# Load cleaned Table 2
# ----------------------------------------------------------
table2_clean <- readRDS("Support_Files/table2_clean.rds")
View(table2_clean)

# ----------------------------------------------------------
# Keep AGGREGATE rows only
#    (HDR aggregates have hdi_rank == NA)
# ----------------------------------------------------------
table2_aggregates <- table2_clean %>%
  dplyr::filter(is.na(hdi_rank))
View(table2_aggregates)

# ----------------------------------------------------------
# Define group types explicitly
# ----------------------------------------------------------
table2_aggregates <- table2_aggregates %>%
  dplyr::mutate(
    group_type = case_when(
      
      # ------------------------------
      # Human development groups
      # ------------------------------
      country %in% c(
        "Very high human development",
        "High human development",
        "Medium human development",
        "Low human development"
      ) ~ "hd_group",
      
      # ------------------------------
      # Regions
      # ------------------------------
      country %in% c(
        "Arab States",
        "East Asia and the Pacific",
        "Europe and Central Asia",
        "Latin America and the Caribbean",
        "South Asia",
        "Sub-Saharan Africa"
      ) ~ "region",
      
      # ------------------------------
      # International reference groups
      # ------------------------------
      country %in% c(
        "Developing countries",
        "Least developed countries",
        "Small island developing states",
        "Organisation for Economic Co-operation and Development",
        "World"
      ) ~ "reference_group",
      
      # ------------------------------
      # Safety fallback
      # ------------------------------
      TRUE ~ NA_character_
    )
  )

# Safety check: all aggregate rows must be classified
stopifnot(!any(is.na(table2_aggregates$group_type)))

#View(table2_aggregates)
# ----------------------------------------------------------
# Rename country -> group
# ----------------------------------------------------------
table2_aggregates <- table2_aggregates %>%
  dplyr::rename(group = country)

#View(table2_aggregates)

# ----------------------------------------------------------
# Drop unnecessary columns
# ----------------------------------------------------------
table2_aggregates <- table2_aggregates %>%
  dplyr::select(
    -starts_with("avg_annual_hdi_growth"),
    -starts_with("change_hdi_rank"),
    -hdi_rank
  )
View(table2_aggregates)

# ----------------------------------------------------------
# Pivot HDI columns to long format
# ----------------------------------------------------------
hdr_table2_aggregates_long <- table2_aggregates %>%
  tidyr::pivot_longer(
    cols = starts_with("hdi_"),
    names_to  = "year",
    values_to = "hdi"
  ) %>%
  dplyr::mutate(
    year = as.integer(str_remove(year, "hdi_"))
  )




# ----------------------------------------------------------
# Create centered year variable
# ----------------------------------------------------------
hdr_table2_aggregates_long <- hdr_table2_aggregates_long %>%
  dplyr::mutate(
    year_c = year - mean(year, na.rm = TRUE)
  )
View(hdr_table2_aggregates_long)


# ----------------------------------------------------------
# Final sanity checks
# ----------------------------------------------------------
stopifnot(
  all(c("group", "group_type", "year", "year_c", "hdi") %in%
        names(hdr_table2_aggregates_long))
)

# ----------------------------------------------------------
# Save output
# ----------------------------------------------------------
saveRDS(
  hdr_table2_aggregates_long,
  "Support_Files/hdr_table2_aggregates_long.rds")


View(hdr_table2_aggregates_long)

