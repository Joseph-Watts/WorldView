# modules/phylo_viz/phylo_viz_global.R

## common use for rendering color
library("viridis")

## packages
library("ape")

phylo_viz_required <- c("ggtree", "ggtreeExtra", "ggnewscale")
phylo_viz_missing <- phylo_viz_required[
  !vapply(phylo_viz_required, requireNamespace, logical(1), quietly = TRUE)
]
if (length(phylo_viz_missing) > 0) {
  stop(
    "Missing phylogenetic visualisation packages: ",
    paste(phylo_viz_missing, collapse = ", "),
    ". Install these before launching the app."
  )
}

library("ggtree")
library("ggtreeExtra")
library("ggnewscale")

## common functions in phylo
source("modules/phylo_viz/phylo_viz_utils.R")

## Phylogenetic Visualisation sub modules
source("modules/phylo_viz/tree/tree_utils.R")
source("modules/phylo_viz/tree/tree_ui.R")
source("modules/phylo_viz/tree/tree_server.R")

## data load

path_phylogeny_dataset <- "data/phylogeny"

# run one time when missing data/phylogeny folder .
# phylo_init_dataset(path_phylogeny_dataset)

country_phylogeny <- readr::read_csv(paste0(path_phylogeny_dataset,"/country_phylogeny.csv"), na = "", show_col_types = FALSE)

country_phylogeny_tree <- ape::read.tree(paste0(path_phylogeny_dataset, "/country_phylogeny_tree.tree"))


wvs_country2 <- country_data

merge_NIR_to_GBR <- TRUE
# merge_NIR_to_GBR <- FALSE
if(merge_NIR_to_GBR){
  wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="GBR", 3:ncol(wvs_country2)] <-
    (447*wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="NIR", 3:ncol(wvs_country2)] +
       2609*wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="GBR", 3:ncol(wvs_country2)]) / (447+2609)
  wvs_country2 <- dplyr::filter(wvs_country2, B_COUNTRY_ALPHA!="NIR")
}

# Cache the static tree tip-to-country/language join once at startup. Plot
# renders only need to join the selected WVS variables on top of this.
country_phylogeny_tip_keys <- extract_tip_keys(country_phylogeny_tree)
country_phylogeny_base_tip_anno <- dplyr::left_join(
  country_phylogeny_tip_keys,
  country_phylogeny,
  by = c("country_code" = "iso3166alpha3"),
  keep = TRUE
)

