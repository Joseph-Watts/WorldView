# modules/phylo_viz/phylo_viz_utils.R

## check phylogeny dataset exist or not, and initial phylogeny dataset
phylo_init_dataset <- function(path="data/phylogeny"){
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
        ##  country/area missing language, find a proper language for each
        iso3166alpha3 == "ATA" ~ "eng",  # Antarctica - eng
        iso3166alpha3 == "BVT" ~ "nor",  # Bouvet Island - nor
        iso3166alpha3 == "ATF" ~ "fra",  # French Southern Territories - fra
        iso3166alpha3 == "HMD" ~ "eng",  # Heard Island and McDonald Islands - eng
        iso3166alpha3 == "SGS" ~ "eng",  # South Georgia and the South Sandwich Islands - eng
        iso3166alpha3 == "SJM" ~ "nor",  # Svalbard and Jan Mayen - nor
        iso3166alpha3 == "UMI" ~ "eng",  # United States Minor Outlying Islands - eng
        
        ## country/area language level is dialet, find the language level iso639P3, 
        ## find in languoid dataset, all those 4 country/area parent language is 
        ## east2821(Eastern Herzegovinian Shtokavian , dialect) and nearest language level language is 
        ## east2821 -> news1236 -> shto1241 -> sout1528(hbs, Serbian-Croatian-Bosnian, language)
        iso3166alpha3 == "BIH" ~ "hbs", 
        iso3166alpha3 == "HRV" ~ "hbs",
        iso3166alpha3 == "MNE" ~ "hbs",
        iso3166alpha3 == "SRB" ~ "hbs",
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


# ----------------------------
# WVS variable display helper
# - exact match in codebook:   "Q1.Important in life: Family" (sep configurable)
# - split option columns:      "Q56.Standard of living comparing with your parents--Better_off"
# ----------------------------
wvs_var_display <- function(col_id, codebook_data, label_sep = ".", choice_sep = "--") {
  stopifnot(!missing(codebook_data))
  if (is.null(col_id) || length(col_id) != 1 || is.na(col_id)) return(as.character(col_id))
  
  # 1) exact match in codebook (e.g., Q1)
  idx <- match(col_id, codebook_data$Col_ID)
  if (!is.na(idx)) {
    lab <- codebook_data$Col_Label[[idx]]
    if (!is.na(lab) && nzchar(lab)) return(paste0(col_id, label_sep, lab))
    return(col_id)
  }
  
  # 2) derived split columns (e.g., Q56.Better_off)
  if (grepl("\\.", col_id)) {
    base <- sub("\\..*$", "", col_id)
    opt  <- sub("^[^.]*\\.", "", col_id)
    
    idx2 <- match(base, codebook_data$Col_ID)
    if (!is.na(idx2)) {
      lab2 <- codebook_data$Col_Label[[idx2]]
      if (!is.na(lab2) && nzchar(lab2)) return(paste0(base, label_sep, lab2, choice_sep, opt))
    }
  }
  
  # 3) fallback
  col_id
}

