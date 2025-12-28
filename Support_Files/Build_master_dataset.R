#Build_master_dataset.R

#Purpose: join datasets

#Deals with:
#  HDR + WVS merges

  # ISO3 joins

  #classifications



# ============================================
# Load WVS7 Country-level data
# ============================================

# 1. Load WVS7 country dataset (country-level metadata)
wvs7_country <- readRDS("WVS_Dataset/WVS7_Country.rds")

# 2. Create ISO3 column from WVS alpha-3 country code
# This ensures compatibility with HDR ISO3 codes
wvs7_country <- wvs7_country %>%
  mutate(iso3 = as.character(B_COUNTRY_ALPHA))

# ============================================
# Build HDR–WVS master (HDR as reference table)
# ============================================

# Join WVS country-level data onto HDR master table
# LEFT JOIN ensures ALL HDR countries are kept
master_HDR_WVS7_data <- HDRs_master_clean %>%
  left_join(wvs7_country, by = "iso3")

# --------------------------------------------
# CHECKPOINT: overlap between HDR and WVS
# --------------------------------------------

# Number of countries present in BOTH HDR and WVS
length(intersect(HDRs_master_clean$iso3, wvs7_country$iso3))
# Expected: ~62 countries

# ============================================
# Join HDI group classification (authoritative)
# ============================================

# Join HDI group from UNDP annex lookup
# Countries not classified by UNDP (e.g. PRK, Monaco) will retain NA
MASTER_HDR_WVS7_HDI <- master_HDR_WVS7_data %>%
  left_join(HDR_HDI_GROUP_LOOKUP, by = "iso3")

# Check HDI group distribution
count(MASTER_HDR_WVS7_HDI, hdr_group, sort = TRUE)

# ============================================
# Join HDR region (one-to-one classification)
# ============================================

# Add geographic region for each country
MASTER_HDR_WVS7_HDI_REGION <- MASTER_HDR_WVS7_HDI %>%
  left_join(HDR_REGION_LOOKUP, by = "iso3")

# ============================================
# Add special group flags (OECD, SIDS, LDC)
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
    is_ldc  = iso3 %in% HDR_SPECIAL_LOOKUP$iso3[
      HDR_SPECIAL_LOOKUP$special_group == "Least developed countries"
    ]
  )

# ============================================
# Final classified master dataset
# ============================================

MASTER_HDR_WVS7_CLASSIFIED <- MASTER_HDR_WVS7_HDI_REGION_SPECIAL

# Final check
count(MASTER_HDR_WVS7_CLASSIFIED, hdr_group, sort = TRUE)


# Save the final classified master dataset to disk
# Save using here() to guarantee correct location
saveRDS(
  MASTER_HDR_WVS7_CLASSIFIED,
  here::here("Support_Files", "MASTER_HDR_WVS7_CLASSIFIED.rds")
)


