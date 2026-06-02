#################################################################-
#### WAVE 7 DATA WRANGLING CODE BLOCK                        ####
# #                                                             #
# # Other waves may have different data wrangling requirements, #
# # but as they are similar, just copy this block and change it #
# # accordingly                                                 #
#################################################################-

# load original data
orig_indiv_data <- readRDS("WVS_Dataset/WVS7_Individual.rds")
orig_country_data <- readRDS("WVS_Dataset/WVS7_Country.rds")
orig_codebook_data <- readxl::read_xlsx("WVS_Dataset/WVS7_Codebook_updated_labels.xlsx")
orig_UNSD_data <- readxl::read_excel("WVS_Dataset/UNSD — Methodology.xlsx")

variables_to_process <- orig_codebook_data$Variable_Display_Logical %>% as.logical()

# stop("Need to update this code so it only runs for the variables included in variables_to_process")

# load intermediaries
WVS7_part_countries <- orig_country_data[c(1:2)]
UNSD_countries_list <- orig_UNSD_data[,c(4,6,8,12)]

# countries and questions simple lists
WVS7_countries_list <- levels(orig_country_data$B_COUNTRY)
WVS7_iso_list <- levels(orig_country_data$B_COUNTRY_ALPHA)

indiv_ordinal <- orig_indiv_data

# # Ignored questions (given the number of factors they have or any other condition)
ignored_questions <- c("Q223", # political parties for each country - almost 1000 different factors
                       "Q266", # birth place - basically all countries ~ 200 factors
                       "Q267", # birth place - basically all countries ~ 200 factors
                       "Q268", # birth place - basically all countries ~ 200 factors
                       "Q272", # language groupings - # different factors
                       "Q290") # ethnic groupings - # different factors

# picker lists
WVS7_part_countries <- WVS7_part_countries %>%
  dplyr::left_join(
    UNSD_countries_list %>%
      dplyr::select(`ISO-alpha3 Code`, `Region Name`),
    by = c("B_COUNTRY_ALPHA" = "ISO-alpha3 Code")
  ) %>%
  dplyr::mutate(`Region Name` = coalesce(`Region Name`, 
                                         "Not defined"))


picker_country_list <- WVS7_part_countries %>%
  dplyr::arrange('Region Name', 'B_COUNTRY') %>%
  dplyr::group_by('Region Name') %>%
  dplyr::summarise(
    Countries = list(stats::setNames(B_COUNTRY_ALPHA, 
                                     B_COUNTRY)),
    .groups = "drop") %>%
  tibble::deframe()


###########################-
#### END OF WAVE 7     ####
# # DATA WRANGLING BLOCK  #
###########################-