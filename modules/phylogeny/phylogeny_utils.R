# modules/phylogeny/phylogeny_utils.R

## check phylogeny dataset exist or not, and initial phylogeny dataset
phylo_init_dataset <- function(path="WVS_Dataset/phylogeny"){
  if(!dir.exists(path)){
    dir.create(path)
  }
  
  cat(paste0(path," folder exist!\n"))
  
  file_phylo_country <- paste0(path,"/country_phylogeny.csv")
  if(!file.exists(file_phylo_country)){
    
    file_langFamily <- paste0(path,"/langFamily.xlsx")
    if(!file.exists(file_langFamily)){
      cat("langFamily not exist, find file in the github (ScottClaessens/crossNationalCorrelations).")
      
      url_langFamily <- "https://github.com/ScottClaessens/crossNationalCorrelations/raw/refs/heads/master/data/countryData/langFamily.xlsx"
      tryCatch(
        download.file(url_langFamily, file_langFamily, mode = "wb"),
        error = function(e) stop("langFamily.xlsx download fail!", e$message)
      )
    }
    phylo_langFamily <- tibble(read.xlsx(file_langFamily, na.strings = ""))
    
    file_languoid <- paste0(path,"/languoid.csv")
    if(!file.exists(file_languoid)){
      cat("languoid mapping not exist, find file in the website( https://glottolog.org/meta/downloads ).")
      
      url_languoid <- "https://cdstar.eva.mpg.de//bitstreams/EAEA0-2198-D710-AA36-0/glottolog_languoid.csv.zip"
      zip_languoid <- paste0(path,"/glottolog_languoid.csv.zip")
      tryCatch(
        download.file(url_languoid, zip_languoid, mode = "wb"),
        error = function(e) stop("glottolog_languoid.csv.zip download fail!: ", e$message)
      )
      unzip(zip_languoid,files = "languoid.csv",exdir = path)
      
    }
    phylo_languoid <- tibble(read.csv(file_languoid, na.strings = ""))
    
    file_iso3166 <- paste0(path,"/iso3166.csv")
    if(!file.exists(file_iso3166)){
      cat("country_iso_code not exist, find file in the github( zonicdoe/ISO-3166-Country-codes ).")
      
      url_coutry_iso <- "https://raw.githubusercontent.com/ipregistry/iso3166/refs/heads/main/countries.csv"
      tryCatch(
        download.file(url_coutry_iso, file_iso3166, mode = "wb"),
        error = function(e) stop("countries.csv fail!: ", e$message)
      )
    }
    iso3166 <- tibble(read.csv(file_iso3166, na.strings = ""))
    
    ## standard iso3166 with 249 Country or Region
    phylo_country <- iso3166 %>% select(country_name=name_short, 
                                        iso3166alpha2=X.country_code_alpha2,
                                        iso3166alpha3=country_code_alpha3) %>% 
      left_join(phylo_langFamily %>% select(iso, langCode),
                by = c("iso3166alpha2" = "iso"), na_matches = "never") %>% 
      mutate(iso639P3 = case_when(
        iso3166alpha3 == "ATA" ~ "eng",  # Antarctica - eng
        iso3166alpha3 == "BVT" ~ "nor",  # Bouvet Island - nor
        iso3166alpha3 == "ATF" ~ "fra",  # French Southern Territories - fra
        iso3166alpha3 == "HMD" ~ "eng",  # Heard Island and McDonald Islands - eng
        iso3166alpha3 == "SGS" ~ "eng",  # South Georgia and the South Sandwich Islands - eng
        iso3166alpha3 == "SJM" ~ "nor",  # Svalbard and Jan Mayen - nor
        iso3166alpha3 == "UMI" ~ "eng",  # United States Minor Outlying Islands - eng
        TRUE ~ langCode), langCode=NULL) %>% arrange(iso3166alpha3) %>%
      left_join(phylo_languoid %>% select(iso639P3code, glottocode = id,language_name=name, family_glottocode=family_id),
                by = c("iso639P3" = "iso639P3code"), na_matches = "never") %>% 
      left_join(phylo_languoid %>% select(family_glottocode = id,family_language_name=name),
                by = c("family_glottocode" = "family_glottocode"), na_matches = "never")
    
    phylo_country$family_glottocode <- as.factor(phylo_country$family_glottocode)
    phylo_country$family_language_name <- as.factor(phylo_country$family_language_name)
    write_excel_csv(phylo_country,file=file_phylo_country)
    # saveRDS(phylo_country,file=file_phylo_country)
    
  }
  
  phylo_country <- read_csv(file_phylo_country,na = "")
  cat(paste0("\n language number:",length(unique(phylo_country$glottocode))))
  cat(paste0("\n coutry number:",nrow(phylo_country)))
  
  file_country_phylogeny_tree <- paste0(path, "/country_phylogeny_tree.tree")
  if(!file.exists(file_country_phylogeny_tree)){
    file_global_lang_tree <- paste0(path,"/global_language_tree.tree")
    if(!file.exists(file_global_lang_tree)){
      tgz_global_lang_tree <- paste0(path,"/global-language-tree.gz")
      if(!file.exists(tgz_global_lang_tree)){
        cat("Full language tree not exist, find file in the github( rbouckaert/global-language-tree-pipeline ).")

        url_global_lang_tree <- "https://github.com/rbouckaert/global-language-tree-pipeline/releases/download/v1.0.0/global-language-tree-MCC-labelled.tree.gz"
        tryCatch(
          download.file(url_global_lang_tree, tgz_global_lang_tree, mode = "wb"),
          error = function(e) stop("global_language_tree download fail!: ", e$message)
        )
      }
      
      writeLines(readLines(gzfile(tgz_global_lang_tree, "rt")), file_global_lang_tree)
    }
    
    global_lang_tree <- read.nexus(file_global_lang_tree)
    
    
    # Create country-glottocode mapping from WVS data
    country_glotto_map <- phylo_country %>%
      select(glottocode, language_name, iso639P3, iso3166alpha3, country_name) %>%
      distinct()
    
    # Extract glottocodes from tree tip labels
    tip_labels <- global_lang_tree$tip.label
    tip_glottocodes <- sapply(strsplit(tip_labels, "_"), function(x) x[1])
    
    # Get unique glottocodes present in WVS data
    glottocodes_to_keep <- unique(phylo_country$glottocode)
    
    # Identify base tips to keep (first occurrence of each glottocode)
    tips_to_keep <- tip_labels[tip_glottocodes %in% glottocodes_to_keep]
    
    # Perform initial pruning to keep only relevant language tips
    base_tree <- keep.tip(global_lang_tree, tips_to_keep)
    
    cat("Base tree after initial pruning:", length(base_tree$tip.label), "tips\n")
    
    # Create country-specific tree by adding branches for each country
    country_tree <- base_tree
    
    # pdf(paste0(path,"/language_tree.pdf"), width = 20, height = 30)
    # plot(country_tree)
    # dev.off()
    
    branch_operations <- list()
      
    for (glotto in glottocodes_to_keep) {
      countries <- country_glotto_map %>% filter(glottocode == glotto) %>% 
        arrange(iso3166alpha3) %>% pull(iso3166alpha3)
      
      if (length(countries) >= 1) {
        branch_operations[[paste0(glotto, "_label")]] <- list(
          type = "label",
          glotto = glotto,
          country = countries[1]
        )
        
        # record branches
        if (length(countries) > 1) {
          for (i in 2:length(countries)) {
            branch_operations[[paste0(glotto, "_branch_", i)]] <- list(
              type = "branch",
              glotto = glotto,
              country = countries[i]
            )
          }
        }
      }
    }
    
    # update all branches
    final_tree <- base_tree
    
    for (op_name in names(branch_operations)) {
      op <- branch_operations[[op_name]]
      
      if (op$type == "label") {
        # update label
        tip_glottocodes <- sapply(strsplit(final_tree$tip.label, "_"), function(x) x[1])
        tip_index <- which(tip_glottocodes == op$glotto)[1]
        if (!is.na(tip_index)) {
          final_tree$tip.label[tip_index] <- paste0(op$glotto, "_", op$country)
        }
      } else if (op$type == "branch") {
        # add branches
        tip_glottocodes <- sapply(strsplit(final_tree$tip.label, "_"), function(x) x[1])
        tip_index <- which(tip_glottocodes == op$glotto)[1]
        if (!is.na(tip_index)) {
          final_tree <- bind.tip(
            tree = final_tree,
            tip.label = paste0(op$glotto, "_", op$country),
            where = tip_index,
            position = 0.001
          )
        }
      }
    }
    
    country_phylogeny_tree <- final_tree
    
    # pdf(paste0(path,"/country_language_tree.pdf"), width = 20, height = 50)
    # plot(country_phylogeny_tree)
    # dev.off()
    
    write.tree(country_phylogeny_tree, file = file_country_phylogeny_tree)
    
  }
  country_phylogeny_tree <- read.tree(file_country_phylogeny_tree)
  cat(paste0("\n country_phylogeny_tree coutry number:",Ntip(country_phylogeny_tree)))
  cat("\n phylogeny dataset initial success!")
  
}


#' Render variable description UI
#' 
#' @param data_type Selected data type ("wvs" or "blank")
#' @param outcome_var Selected outcome variable(s) - can be single or multiple
#' @param codebook_data Codebook data for variable descriptions
#' @return UI elements for variable description
render_variable_description <- function(data_type, outcome_var, codebook_data) {
  # Handle blank tree mode
  if (data_type == "blank") {
    return(
      tagList(
        h4("Blank"),
        p("No variable selected.")
      )
    )
  }
  
  # Handle no variable selected
  if (is.null(outcome_var) || length(outcome_var) == 0 || (length(outcome_var) == 1 && outcome_var == "")) {
    return(
      tagList(
        h4("No Variable Selected"),
        p("Please select a variable from the WVS7 dataset.")
      )
    )
  }
  
  # Extract base variable names (remove everything after dot if present)
  # For variables like "Q56.Better_off", we only need "Q56" to match codebook
  base_vars <- unique(sapply(outcome_var, function(var) {
    ifelse(grepl("\\.", var), sub("\\..*", "", var), var)
  }))
  
  # Get variable descriptions from codebook
  var_info <- codebook_data %>% 
    filter(Col_ID %in% base_vars)
  
  # Handle case where no variables found in codebook
  if (nrow(var_info) == 0) {
    return(
      tagList(
        h4("Variable Not Found"),
        p("No description available for the selected variable(s).")
      )
    )
  }
  
  # Single variable - show detailed description
  if (length(outcome_var) == 1) {
    var_info <- var_info[1, ]  # Take first match
    
    # Check if this is a sub-variable of a base variable
    is_sub_var <- base_vars[1] != outcome_var
    
    return(
      tagList(
        h4(var_info$ColLab),
        if (!is.na(var_info$Section) && var_info$Section != "") {
          p(strong("Section:"), var_info$Section)
        },
        if (!is.na(var_info$`Variable Text`) && var_info$`Variable Text` != "") {
          p(strong("Variable Text:"), br(), var_info$`Variable Text`)
        },
        if (!is.na(var_info$`Additional Text`) && var_info$`Additional Text` != "") {
          p(strong("Additional Text:"), br(), var_info$`Additional Text`)
        },
        # Add note if this is a sub-variable
        if (is_sub_var) {
          p(em("Note: This is a sub-variable of", base_vars[1]))
        }
      )
    )
  }
  
  # Multiple variables - show concise descriptions only
  descriptions <- lapply(base_vars, function(base_var) {
    info <- var_info %>% filter(Col_ID == base_var)
    if (nrow(info) == 0) return(NULL)
    
    # Find all sub-variables for this base variable
    sub_vars <- outcome_var[grepl(paste0("^", base_var, "(\\.|$)"), outcome_var)]
    
    tagList(
      h5(info$ColLab[1])
    )
  })
  
  # Remove NULL elements
  descriptions <- descriptions[!sapply(descriptions, is.null)]
  
  # Return multiple variable description
  tagList(
    descriptions
  )
}