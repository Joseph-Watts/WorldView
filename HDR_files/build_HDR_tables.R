# ==============================================================================
# Script name : build_HDR_tables.R
# Purpose
# ------------------------------------------------------------------------------
# This script assembles and harmonises cleaned Human Development Report (HDR)
# tables into analysis-ready master datasets.
#
# It integrates country-level data and aggregate benchmark tables (HDI groups,
# regions, and special groups), enriches datasets with ISO-3 country codes,
# constructs group-level benchmark datasets, and generates lookup tables and
# metadata used throughout the Shiny application.

# Inputs
# ------------------------------------------------------------------------------
# - Cleaned HDR tables (.rds) produced by clean_HDRs.R:
#   table1_clean, table2_clean, table3_clean, table4_clean,
#   table5_clean, table7_clean

# Outputs
# ------------------------------------------------------------------------------
# The following datasets are created and saved for downstream use:
# 1. Structured HDR table collection:
#    - HDR_DATA
#      • Nested list containing, for each HDR table:
#        - countries_tableX
#        - hdi_groups_tableX
#        - regions_tableX
#        - special_groups_tableX

# 2. Group-level benchmark dataset:
#    - HDR_GROUP_BENCHMARKS
#      • Long-format dataset combining HDI groups, regions,
#        and special groups across all HDR tables

# 3. Group and area lookup tables:MASTER_HDR_WVS7_CLASSIFIED
#    - HDR_GROUP_LOOKUP
#    - HDR_AREA_LOOKUP
#    - HDR_HDI_GROUP_LOOKUP
#    - HDR_REGION_LOOKUP
#    - HDR_SPECIAL_LOOKUP

# 4. Indicator metadata and dictionaries:
#    - TABLEX_INDICATORS, 
#    - ALL_HDR_INDICATORS
#    - HDR_VARIABLE_DEFINITIONS

# 5. Country-level master datasets with ISO-3 codes:
#    - HDRs_master
#    - HDRs_master_clean
#
# Usage
# ------------------------------------------------------------------------------
# Run once during data preparation. All outputs are saved as .rds files and
# loaded by the Shiny application; this script is not executed at runtime.
# ==============================================================================




#==============================================================================
# 1a. LOAD ALL HDRs CLEAN TABLES
#==============================================================================
table1_clean <- readRDS("Support_Files/table1_clean.rds")
table2_clean <- readRDS("Support_Files/table2_clean.rds")
table3_clean <- readRDS("Support_Files/table3_clean.rds")
table4_clean <- readRDS("Support_Files/table4_clean.rds")
table5_clean <- readRDS("Support_Files/table5_clean.rds")
table7_clean <- readRDS("Support_Files/table7_clean.rds")
#View(table1_clean)

#==============================================================================
# 1b. Load all groups table (needed downstream)
#==============================================================================
countries_table1<- readRDS("Support_Files/countries_table1.rds")
hdi_groups_table1 <- readRDS("Support_Files/hdi_groups_table1.rds")
regions_table1<-readRDS("Support_Files/regions_table1.rds")
special_groups_table1 <- readRDS("Support_Files/special_groups_table1.rds")


countries_table2<- readRDS("Support_Files/countries_table2.rds")
hdi_groups_table2 <- readRDS("Support_Files/hdi_groups_table2.rds")
regions_table2 <-readRDS("Support_Files/regions_table2.rds")
special_groups_table2 <- readRDS("Support_Files/special_groups_table2.rds")


countries_table3<- readRDS("Support_Files/countries_table3.rds")
hdi_groups_table3 <- readRDS("Support_Files/hdi_groups_table3.rds")
regions_table3<-readRDS("Support_Files/regions_table3.rds")
special_groups_table3 <- readRDS("Support_Files/special_groups_table3.rds")


countries_table4<- readRDS("Support_Files/countries_table4.rds")
hdi_groups_table4 <- readRDS("Support_Files/hdi_groups_table4.rds")
regions_table4<-readRDS("Support_Files/regions_table4.rds")
special_groups_table4 <- readRDS("Support_Files/special_groups_table4.rds")


countries_table5<- readRDS("Support_Files/countries_table5.rds")
hdi_groups_table5 <- readRDS("Support_Files/hdi_groups_table5.rds")
regions_table5<-readRDS("Support_Files/regions_table5.rds")
special_groups_table5 <- readRDS("Support_Files/special_groups_table5.rds")



hdi_groups_table7 <- readRDS("Support_Files/hdi_groups_table7.rds")
regions_table7<- readRDS("Support_Files/regions_table7.rds")
countries_table7<- readRDS("Support_Files/countries_table7.rds")
special_groups_table7 <- readRDS("Support_Files/special_groups_table7.rds")
#View(special_groups_table1)

#==============================================================================
# 2.          STRUCTURED LIST FOR ALL HDRs TABLES
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


#save 
saveRDS(HDR_DATA,"Support_Files/HDR_DATA.rds")

#View(HDR_DATA)




###############################################################################
# =============================================================================
#                3. ADD ISO3 CODE TO COUNTRIES
# =============================================================================
###############################################################################
# Create a version of Table 1 (countries only) with ISO-3 code added
tab1_Iso3 <- HDR_DATA$`Table 1 - HDI & Components`$countries %>% 
  mutate(
    # Add a new column 'iso3' by converting country names
    # ISO-3 codes (e.g., NZL, AUS, USA) are required for joining
    # with world shapefiles and building maps
    iso3 = countrycode(
      country,                  # input: country names
      origin = "country.name",  # tells the function countrycode() the input format
      destination = "iso3c"     # tells the function the output format: 3-letter ISO-3 country code
    )
  )

#View(tab1_Iso3)

# Create Table 2 with ISO-3 code 
tab2_Iso3 <- HDR_DATA$`Table 2 - HDI Trends`$countries%>%
  mutate(
    iso3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )



# Create Table 3 with ISO-3 code
tab3_Iso3 <- HDR_DATA$`Table 3 - Inequality-adjusted HDI`$countries%>%
  mutate(
    iso3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )



# Create Table 4 with ISO-3 code 
tab4_Iso3 <- HDR_DATA$`Table 4 - GDI`$countries%>%
  mutate(
    iso3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )



# Create Table 5 with ISO-3 code 
tab5_Iso3 <- HDR_DATA$`Table 5 - GII`$countries%>%
  mutate(
    iso3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )



# Create Table 5 with ISO-3 code
tab7_Iso3 <- HDR_DATA$`Table 7 - PHDI`$countries%>%
  mutate(
    iso3 = countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )
  )

#CHECKPOINT: view if col `IsO3` correctly added to the table
# View(tab1_Iso3)
# View(tab2_Iso3)
# View(tab3_Iso3)
# View(tab4_Iso3)
# View(tab5_Iso3)
# View(tab7_Iso3)



#--CHECKPOINT: Identify countries where ISO3 code conversion failed
tb1_mismatches <- tab1_Iso3 %>%
  filter(is.na(iso3)) %>%      # keep rows where Iso3 is missing
  select(country)              # show only the country names with missing Iso3 col
# Print the list of unmatched countries
#View(tb1_mismatches)

tb2_mismatches <- tab2_Iso3 %>%
  filter(is.na(iso3)) %>%     
  select(country)            
#View(tb2_mismatches)

tb3_mismatches <- tab3_Iso3 %>%
  filter(is.na(iso3)) %>%     
  select(country)             
#View(tb3_mismatches)

tb4_mismatches <- tab4_Iso3 %>%
  filter(is.na(iso3)) %>%      
  select(country)              
#View(tb4_mismatches)

tb5_mismatches <- tab5_Iso3 %>%
  filter(is.na(iso3)) %>%      
  select(country)              
#View(tb5_mismatches)         

tb7_mismatches <- tab7_Iso3 %>%
  filter(is.na(iso3)) %>%      
  select(country)              
#View(tb7_mismatches)    




###############################################################################
# =============================================================================
#               4.BUILD MASTER HDR DATASETS WITH ISO3 CODES
# =============================================================================
###############################################################################
#CREATE MASTER HDR DATASETS
HDRs_master<- tab1_Iso3 %>%
  left_join(tab2_Iso3, by= "iso3")%>%
  left_join(tab3_Iso3, by= "iso3")%>%
  left_join(tab4_Iso3, by= "iso3")%>%
  left_join(tab5_Iso3, by= "iso3")%>%
  left_join(tab7_Iso3, by= "iso3")

#View(HDRs_master)
#str(HDRs_master)

#REMOVE DUPLICATE columns (i.e.`country`, and `hdi_2023`)----------
#Rename `country.x` into 'country' to keep one col `country`
HDRs_master_clean <- HDRs_master %>%
  rename(                   #preserve `hdi_rank`, `country` and `hdi_2023`
    hdi_rank = hdi_rank.x,   
    country = country.x,
    hdi_2023 = hdi_2023.x)%>%
  select(                    # Remove duplicate columns from joins
    -matches("\\.x$"),
    -matches("\\.y$"),
    -matches("\\.x\\.x$"),
    -matches("\\.y\\.y$"),
    -matches("\\.x\\.x\\.x$"),
    -matches("\\.y\\.y\\.y$")
  )


#Save and view
saveRDS(HDRs_master_clean, "Support_files/HDRs_master_clean.rds")
#View(HDRs_master_clean)


#Move the col iso3 right after the col `country`
HDRs_master_clean<- HDRs_master_clean%>%
  relocate(iso3, .after=country)
#View(HDRs_master_clean)
#str(HDRs_master_clean)


#CHECKPOINT: Check which HDR country did NOT match Natural Earth ISO3
#is there  1 or more country in HDRs_master_clean that do not have iso3 code.
# This test tells if a country was dropped during the joins that created HDRs_master_clean()
HDRs_country_missingIso3<- HDRs_master_clean %>%
  filter(is.na(iso3)) %>%
  select(country)
#View(HDRs_country_missingIso3)   #there is no missing iso3 in HDRs_master_clean after join




#########################################################################################
#########################################################################################
#########################################################################################





















###############################################################################
# =============================================================================
#             8. CREATE CHOROPLETH: WORLD MAP SHAPEFILE
# =============================================================================
###############################################################################

#Load the world map shapefile => gives world map with ISO country codes
world_shape <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#View(world_shape)

#CHECKPOINT: View countries in Natural Earth where ISO3 is missing ("-99")
#Natural Earth uses "-99" ISO code for: countries with unresolved or special-status,
#or for countries with multiple separate geographical lands such as France, Norway, etc. 
NaturalEarth_countries_missingIso3<- world_shape %>%
                                    filter(is.na(iso_a3) | iso_a3 == "-99") %>%
                                    select(name, iso_a3)
#View(NaturalEarth_countries_missingIso3)
# "France" and "Norway" are the only 2 countries in HDRs that do not have a iso3 code 
#in Natural Earth dataset because France territory include overseas departments, 
# and Nowrway is a composites country => Their Iso3 is indefined (-99). 




#FIX ISO3 CODE FOR NORWAY AND FRANCE in world_shape
world_shape <- world_shape %>%
  mutate(
    name_clean = trimws(name),
    iso_a3 = case_when(
      name_clean %in% c("France","French Guiana","Guadeloupe","Martinique","Réunion","Mayotte") ~ "FRA",
      name_clean == "Norway" ~ "NOR",
      TRUE ~ iso_a3
    )
  )


#CHECKPOINT: if France and Norway have now an iso3 
# world_shape %>%
#   filter(is.na(iso_a3) | iso_a3 == "-99") %>%
#   select(name, iso_a3)    #Output does not show France and Norway anymore





###############################################################################
# =============================================================================
#                5. MASTER LIST OF ALL INDICATORS
# =============================================================================
##############################################################################
# Build a unified master list of ALL indicators from ALL HDR tables
ALL_HDR_INDICATORS <- unique(unlist(
  
  lapply(HDR_DATA, function(tbl) {
    
    # Extract column names from the "countries" sub-table
    # (this sub-table always contains the full indicator list for each HDR table)
    names(tbl$countries)
    
  })
))

# Remove the 'country' column because it's an ID, not an indicator
ALL_HDR_INDICATORS <- setdiff(ALL_HDR_INDICATORS, "country")

#print(ALL_HDR_INDICATORS)







###############################################################################
#==============================================================================
# 9. ENRICHED VARIABLE DEFINITIONS FOR ALL HDRs Tables
#==============================================================================
###############################################################################
HDR_VARIABLE_DEFINITIONS <- list(
  # ------------------------------------------------------------------
  # TABLE 1 - Human Development Index (HDI) & Components
  # ------------------------------------------------------------------
  "Table 1 - HDI & Components" = list(
    
    "hdi_rank" = paste(
      "HDI Rank is the country position in the Human Development Index (HDI).",
      "A lower rank means higher human development."
    ),
    
    "country" = "Country name.",
    
    "hdi_value_2023" = paste(
      "'Human Development Index value (HDI)' is a composite index measuring ",
      "average achievement in three dimensions:",
      "1) Long and healthy life,",
      "2) Knowledge,",
      "3) Decent standard of living."
    ),
    
    "life_expect_birth_2022" = paste(
      "'Life expectancy at birth in 2022' is the number of years a newborn infant can",
      "expect to live assuming age-specific mortality rates remain constant."
    ),
    
    "expected_yrs_schooling_2023" = paste(
      "'Expected years of schooling in 2023' is the number of years of schooling a child",
      "entering school can expect to receive if enrollment patterns remain constant."
    ),
    
    "mean_yrs_schooling_2023" = paste(
      "'Mean years of schooling' is the average number of years of education",
      "received by adults aged 25+ based on officially reported educational",
      " attainment data."
    ),
    
    "gni_per_capita_2023" = paste(
      "'Gross National Income (GNI) per capita in 2023' is the aggregate income generated",
      " by the economy, including overseas income converted to international dollars",
      " using PPP rates and divided by population."
    ),
    
    "gni_minus_hdi_rank_2023" = paste(
      "'GNI per capita rank minus HDI rank' is the difference between a country’s GNI rank",
      " and its HDI rank. A negative value means the country is better ranked",
      "by GNI than by HDI."
    ),
    
    "hdi_rank_2022" = paste(
      "'HDI rank for 2022' is the HDI ranking calculated for 2022 using ",
      " the same revised data as the 2023 HDI."
    )
  ),
  
  # --------------------------------------------------------------------
  # TABLE 2 - HDI TRENDS 1990-2023
  # --------------------------------------------------------------------
  "Table 2 - HDI Trends" = list(
    
    "hdi_rank" = paste(
      "HDI Rank:",
      "Country's rank based on HDI in 2023."
    ),
    
    "country" = "Country name.",
    
    "hdi_1990" = "HDI value for year 1990.",
    "hdi_2000" = "HDI value for year 2000.",
    "hdi_2010" = "HDI value for year 2010.",
    "hdi_2015" = "HDI value for year 2015.",
    "hdi_2020" = "HDI value for year 2020.",
    "hdi_2021" = "HDI value for year 2021.",
    "hdi_2022" = "HDI value for year 2022.",
    "hdi_2023" = "HDI value for year 2023.",
    
    "change_hdi_rank_2015_2023" = paste(
      "'Change in HDI rank (2015–2023)'is the ",
      "difference between a country's HDI rank in 2015 and 2023."
    ),
    
    # Generic definition for all 4 growth variables
    "avg_annual_hdi_growth_perct_1990_2000, _2010_2023, _2000_2010,
    or _1990_2023" = "'Average annual HDI growth' is the annualized compound 
    growth rate of the HDI for the indicated period."
  ),
  
  # --------------------------------------------------------------------
  # TABLE 3 - Inequality-Adjusted HDI (IHDI)
  # --------------------------------------------------------------------
  "Table 3 - Inequality-adjusted HDI" = list(
    
    "hdi_rank" = "Country's HDI rank in 2023.",
    "country" = "Country name.",
    "hdi_2023" = "HDI value in 2023.",
    
    "ihdi_2023" = paste(
      "'Inequality-adjusted HDI (IHDI)' is the HDI value adjusted for",
      "inequality in health, education, and income in 2023."
    ),
    
    "ihdi_loss_perct_2023" = paste(
      "'Inequality HDI Overall loss' is the percentage difference between the IHDI value",
      " and the HDI value in 2023. Higher losses indicate greater inequality."
    ), 
    
    "ihdi_diff_hdirank_2023" = paste(
      "'Difference from HDI rank' is the difference in ranks between the IHDI ",
      "and the HDI, Positive values means worse ranking after adjusting for inequality."
    ),
    
    "coef_human_inequality_2023" = paste(
      "'Coefficient of human inequality' is the average inequality",
      " across the three HDI dimensions (health, education and income) in 2023."
    ),
    
    "ineq_life_exp_perct_2023" = paste(
      "'Inequality in life expectancy' is inequality in distribution of",
      " expected life years,based on Atkinson inequality index in 2023."
    ), 
    
    "ineq_adj_life_exp_index_2023" = paste(
      "'Inequality-adjusted life expectancy index' is the HDI life ",
      "expectancy index adjusted for inequality in life expectancy in 2023."
    ),
    
    "ineq_educ_perct_2023" = paste(
      "'Inequality in education' is the inequality in distribution of years of",
      "schooling in 2023 using Atkinson index estimates from household surveys."
    ),
    
    "ineq_adj_educ_index_2023" = paste(
      "'Inequality-adjusted education index' is the education index adjusted for",
      " inequality in distribution of years of schooling, in 2023."
    ),
    
    "ineq_income_perct_2022" = paste(
      "'Inequality in income distribution' is the income distribution",
      " inequality estimated using the Atkinson index, in 2022."
    ),
    
    "ineq_adj_income_index_2022" = paste(
      "'Inequality-adjusted income index' is the ",
      "income index adjusted for inequality of income distribution, in 2022."
    ),
    
    "inc_shares_poor40perct_2010_2023" = paste(
      "'Income share held by poorest 40%' is the percentage of national income",
      " (or consumption) accruing to the bottom 40% of the population, between 2010 and 2023."
    ),
    
    "inc_shares_rich10perct_2010_2023" = paste(
      "'Income share held by richest 10% 'is the ",
      "percentage of national income (or consumption) accruing to the top 10%."
    ),
    
    "inc_shares_rich1perct_2010_2023" = paste(
      "'Income share held by richest 1%' is the ",
      "share of pretax national income held by the richest 1% of thr population."
    ),
    
    "gini_coef_2010_2023" = paste(
      "'Gini coefficient' is the measure of income inequality, i.e. the", 
      "measure of the deviation of the distribution of income among individuals", 
      "or households in a country from a perfectly  equal distribution. ",
      "0 = perfect equality, 100 = perfect inequality."
    )
  ),
  
  # --------------------------------------------------------------------
  # TABLE 4 - Gender Development Index (GDI)
  # --------------------------------------------------------------------
  "Table 4 - GDI" = list(
    
    "hdi_rank" = "HDI rank of the country.",
    "country" = "Country name.",
    
    "gdi_2023" = paste(
      "'Gender Development Index (GDI)' is the ratio of female to male HDI values ",
      "in 2023"
    ),
    
    "gdi_group_2023" = paste(
      "GDI group classification' in 2023'.",
      "Countries grouped based on deviation from gender parity in HDI:",
      "Group 1: High equality (<2.5%).",
      "Group 2: Medium-high equality (2.5-5%).",
      "Group 3: Medium equality (5-7.5%).",
      "Group 4: Medium-low equality (7.5-10%).",
      "Group 5: Low equality (>10%)."
    ),
    
    "hdi_female_2023" = "Female HDI value in 2023.",
    "hdi_male_2023" = "Male HDI value in 2023.",
    
    "life_expect_birth_female_2023" = "'Female life expectancy at birth in 2023'.",
    "life_expect_birth_male_2023" = "'Male life expectancy at birth', in 2023.",
    
    "expected_yrs_school_female_2023" = "'Expected years of schooling for girls in 2023'.",
    "It is the number of years of schooling that a child of school entrance age can",
    "expect to receive if prevailing patterns of age-specific enrolment rates",
    "persist throughout the child’s life.",
    
    "expected_yrs_school_male_2023" = "'Expected years of schooling for boys in 2023'.",
    
    "mean_yrs_school_female_2023" = "'Mean years of schooling for women aged",
    "25+ in 2023'. It is the average number of years of education received by",
    "people ages 25 and older, converted from educational attainment levels ",
    "using official durations of each level.",
    
    "mean_yrs_school_male_2023" = "Mean years of schooling for men aged 25+ in 2025.",
    
    "gross_nat_inc_capita_female_2023" = paste(
      "'Female gross national income (GNI) per capita in 2023'",
      " is estimated using female/male wage ratios and labour shares."
    ),
    
    "gross_nat_inc_capita_male_2023" = paste(
      "'Male GNI per capita in 2023' is",
      "estimated using male/female wage ratios and labour shares."
    )
  ),
  
  # --------------------------------------------------------------------
  # TABLE 5 - Gender Inequality Index (GII)
  # --------------------------------------------------------------------
  "Table 5 - GII" = list(
    
    "hdi_rank" = "HDI rank of the country.",
    "country" = "Country name.",
    
    "gii_2023" = paste(
      "'Gender Inequality Index (GII) in 2023' is the composite measure of inequality",
      "in reproductive health, empowerment, and labour market participation.",
      ""
    ),
    
    "gii_rank_2023" = "Country ranking based on GII.",
    
    "mater_mortal_ratio_2020" = paste(
      "'Maternal mortality ratio in 2020' is the number of deaths due to",
      "pregnancy-related causes per 100,000 live births."
    ),
    
    "ado_birth_rate_2023" = paste(
      "'Adolescent birth rate in 2023' is the",
      "number of births per 1,000 women ages 15–19."
    ),
    
    "parliament_women_perct_2023" = paste(
      "'Share of seats in parliament by women in 2023' is the ",
      "percentage of parliamentary seats held by women."
    ),
    
    "female_secondary_educ_perct_2023" = paste(
      "'Female secondary education in 2023' is the percentage of women",
      " aged 25+ with at least some secondary education."
    ),
    
    "male_secondary_educ_perct_2023" = paste(
      "'Male secondary education in 2023'is the percentage of men aged 25+",
      "with at least some secondary education."
    ),
    
    "female_labourforce_rate_perct_2023" = paste(
      "'Female labour force participation rate in 2023' is the ",
      "percentage of working-age women (ages 15 and older) who work or seek work."
    ),
    
    "male_labourforce_rate_perct_2023" = paste(
      "'Male labour force participation rate in 2023' is the",
      "percentage of working-age men (ages 15 and older) who work or seek work."
    )
  ),
  
  # --------------------------------------------------------------------
  # TABLE 7 - Planetary Pressures–Adjusted HDI (PHDI)
  # --------------------------------------------------------------------
  "Table 7 - PHDI" = list(
    
    "hdi_rank" = "Country's HDI rank.",
    "country" = "Country name.",
    "hdi_2023" = "HDI value in 2023.",
    
    "phdi_2023" = paste(
      "'Planetary Pressures–Adjusted HDI in 2023' is the HDI adjusted for",
      "Carbon dioxide emissions and material footprint per capita."
    ),
    
    "phdi_diff_hdi_perct_2023" = paste(
      "'Difference from HDI value in 2023' is the",
      "percentage difference between PHDI and HDI."
    ),
    
    "phdi_diff_hdi_rank_2023" = paste(
      "'Difference from HDI rank in 2023' is the ",
      "difference in ranks on the PHDI and the HDI, calculated only for",
      "countries for which a PHDI value is calculated."
    ),
    
    "adj_factor_planet_press_2023" = paste(
      "'Adjustment factor for planetary pressures in 2023' is the average of ",
      "carbon dioxide emissions index and material footprint index.",
      "A high value implies less pressure on the planet."
    ),
    
    "CO2_emissions_per_capita_tonnes_2023" = paste(
      "'CO2 emissions per capita in 2023' is the CO2 emissions produced",
      "as a consequence of human activities (tonnes) divided by midyear population."
    ),
    
    "CO2_emissions_index_2023" = paste(
      "'CO2 emissions index in 2023' is the carbon dioxide emissions per capita",
      "(production-based) expressed as an index using a minimum value of 0",
      " and a maximum value of 76.61 tonnes per capita.",
      "A high value on this index implies less pressure on the planet",
      "i.e lower emissions per capita."
    ),
    
    "material_footprint_per_capita_tonnes_2023" = paste(
      "'Material footprint per capita in 2023' is the total material footprint",
      "(the sum of the material footprint for biomass, fossil fuels,",
      "metal ores and nonmetal ores). This indicator is calculated as the raw",
      "material equivalent of imports plus domestic extraction minus raw",
      " material equivalents of exports.",
      "It describes the average material use for final demand."
    ),
    
    "material_footprint_index_2023" = paste(
      "'Material footprint index in 2023' is material footprint per capita",
      "expressed as an index using a minimum value of 0 and a maximum value",
      "of 90.27 tonnes per capita",
      "Higher values indicate less pressure on the planet."
    )
  )
)






###############################################################################
# =============================================================================
#         BUILD LIST OF COUNTRIES PER AREA & LOOK UP TABLE FOR ALL AREAS
# =============================================================================
###############################################################################
# Path to the Excel file that contains an area per sheet
path <- "HDR_files/Area country list.xlsx"

# Names of the sheets to import from the file
selected_sheets <- c("hdi_groups", "regions", "special_groups")

# Read each sheet into a named list of data frames
area_files <- selected_sheets %>%
  set_names() %>%                     # Use sheet names as list element names
  purrr::map(~ read_excel(path, sheet = .x)) # Read each sheet into a data frame
#View(area_files)




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





###############################################################################
# =============================================================================
#         CREATE A CANONICAL GROUP MAPPING TABLE
# =============================================================================
###############################################################################
HDR_GROUP_NAME_MAP <- tibble::tribble(
  ~benchmark_group,                                              ~lookup_group,
  "very high human development",                                 "very high human development",
  "high human development",                                      "high human development",
  "medium human development",                                    "medium human development",
  "low human development",                                       "low human development",
  "arab states",                                                  "arab states",
  "east asia and the pacific",                                    "east asia and the pacific",
  "europe and central asia",                                      "europe and central asia",
  "latin america and the caribbean",                              "latin america and the caribbean",
  "south asia",                                                   "south asia",
  "sub-saharan africa",                                           "sub-saharan africa",
  
  "least developed countries",                                   "lcds",
  "organisation for economic co-operation and development",      "oecd",
  
  # Derived Aggregates
  "developing countries",                                        "developing countries",
  "world",                                                       "world"
)


#View(HDR_GROUP_NAME_MAP)



# ==========================================================
# 4. BUILD HDR_GROUP_LOOKUP TABLE 
# ==========================================================

ref_table <- "Table 1 - HDI & Components"

# ---- Sanity checks ----
stopifnot(ref_table %in% names(HDR_DATA))
stopifnot(all(c("groups", "regions", "special") %in% names(HDR_DATA[[ref_table]])))
stopifnot("country" %in% names(HDR_DATA[[ref_table]]$groups))
stopifnot("country" %in% names(HDR_DATA[[ref_table]]$regions))
stopifnot("country" %in% names(HDR_DATA[[ref_table]]$special))


# ---- Extract group names from "country" column ----
HDR_GROUP_LOOKUP <- list(
  
  groups = sort(unique(
    as.character(HDR_DATA[[ref_table]]$groups$country)
  )),
  
  regions = sort(unique(
    as.character(HDR_DATA[[ref_table]]$regions$country)
  )),
  
  special = sort(unique(
    as.character(HDR_DATA[[ref_table]]$special$country)
  ))
)

# ---- Final validation ----
stopifnot(length(HDR_GROUP_LOOKUP$groups)  > 0)
stopifnot(length(HDR_GROUP_LOOKUP$regions) > 0)
stopifnot(length(HDR_GROUP_LOOKUP$special) > 0)
stopifnot(length(HDR_GROUP_LOOKUP$special) > 0)


# ---- Save lookup ----
saveRDS(
  HDR_GROUP_LOOKUP,
  "Support_Files/HDR_GROUP_LOOKUP.rds"
)




###############################################################################
#==============================================================================
# 3. Construct the HDR group-level benckmarks master dataset
#==============================================================================
##############################################################################
HDR_GROUP_BENCHMARKS <- purrr::map_dfr(
  names(HDR_DATA),
  function(table_name) {
    
    bind_rows(
      # ------------------------------
      # HDI groups
      # ------------------------------
      HDR_DATA[[table_name]]$groups %>%
        dplyr::mutate(group_type = "HDI group"),
      
      # ------------------------------
      # Regions
      # ------------------------------
      HDR_DATA[[table_name]]$regions %>%
        dplyr::mutate(group_type = "Region"),
      
      # ------------------------------
      # Special groups (OECD, SIDS…)
      # ------------------------------
      HDR_DATA[[table_name]]$special %>%
        dplyr::mutate(group_type = "Special group")
    ) %>%
      # ------------------------------
    # Clean + reshape
    # ------------------------------
    dplyr::select(
      group_type,
      country,
      where(is.numeric),
      -contains("rank")   #drop hdi_rank and "change in hdi rank" because type factor
    ) %>%
      dplyr::rename(group = country) %>%
      tidyr::pivot_longer(
        cols = -c(group, group_type),
        names_to  = "variable",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        table  = table_name,
        source = "HDR"
      )
  }
)


saveRDS(HDR_GROUP_BENCHMARKS, "Support_Files/HDR_GROUP_BENCHMARKS.rds")
#HDR_GROUP_BENCHMARKS <- readRDS("Support_files/HDR_GROUP_BENCHMARKS.rds")
#View(HDR_GROUP_BENCHMARKS)







###############################################################################
# =============================================================================
#        BUILD LABELLED HDR INDICATOR CHOICES (FOR SHINY UI)
# =============================================================================
###############################################################################

HDR_INDICATOR_CHOICES <- FULL_VARIABLE_DICTIONARY %>%
  dplyr::filter(
    source == "HDR",
    var_code %in% ALL_HDR_INDICATORS
  ) %>%
  dplyr::select(var_code, label) %>%
  dplyr::distinct() %>%
  { setNames(.$var_code, .$label) }


saveRDS(
  HDR_INDICATOR_CHOICES,
  "Support_Files/HDR_INDICATOR_CHOICES.rds"
)





















