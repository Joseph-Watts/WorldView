# modules/phylo_viz/phylo_viz_global.R

## common use for rendering color
library("viridis")

## packages
library("ape")
library("phytools")

if (!requireNamespace("ggtree")) {
  cat("ggtree not installed!\n")
  if (!requireNamespace("BiocManager")) {
    cat("BiocManager not installed!\n")
    install.packages("BiocManager")
    cat("BiocManager is installed!\n")
  }
  library("BiocManager")
  BiocManager::install("ggtree")
  BiocManager::install("ggtreeExtra")
  cat("ggtree is installed!\n")
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

path_phylogeny_dataset <- "WVS_Dataset/phylogeny"

# run one time when missing WVS_Dataset/phylogeny folder .
# phylo_init_dataset(path_phylogeny_dataset)

country_phylogeny <- read_csv(paste0(path_phylogeny_dataset,"/country_phylogeny.csv"),na="")

country_phylogeny_tree <- read.tree(paste0(path_phylogeny_dataset, "/country_phylogeny_tree.tree"))


wvs_country2 <- country_data

merge_NIR_to_GBR <- TRUE
# merge_NIR_to_GBR <- FALSE
if(merge_NIR_to_GBR){
  wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="GBR", 3:ncol(wvs_country2)] <-
    (447*wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="NIR", 3:ncol(wvs_country2)] +
       2609*wvs_country2[wvs_country2$B_COUNTRY_ALPHA=="GBR", 3:ncol(wvs_country2)]) / (447+2609)
  wvs_country2 <- dplyr::filter(wvs_country2, B_COUNTRY_ALPHA!="NIR")
}

