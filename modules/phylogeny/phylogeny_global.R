# modules/phylogeny/phylogeny_global.R

# use pacman to auto load packages in local environment
# install.packages("pacman")
# library("pacman")
# pacman::p_load("viridis","ape","phytools","leaflet","sf","rnaturalearth","phylolm","plotly")

# use "library" for the auto detection by distribution on shinyapp.io cloud server 
# (auto prase and install by the keyword of "library")

## common use for rendering color
library("viridis")

## tree
library("ape")
library("phytools")

## world map
library("leaflet")
library("sf")
library("rnaturalearth")

## models
library("phylolm")
library("plotly")


## Phylogeny utils
source("modules/phylogeny/phylogeny_utils.R")

## Phylogeny sub modules
source("modules/phylogeny/tree/tree_utils.R")
source("modules/phylogeny/tree/tree_ui.R")
source("modules/phylogeny/tree/tree_server.R")

source("modules/phylogeny/map/map_utils.R")
source("modules/phylogeny/map/map_ui.R")
source("modules/phylogeny/map/map_server.R")

source("modules/phylogeny/models/models_utils.R")
source("modules/phylogeny/models/models_ui.R")
source("modules/phylogeny/models/models_server.R")

path_phylogeny_dataset <- "WVS_Dataset/phylogeny"

# run one time when missing WVS_Dataset/phylogeny folder.
# phylo_init_dataset(path_phylogeny_dataset)

country_phylogeny <- read_csv(paste0(path_phylogeny_dataset,"/country_phylogeny.csv"),na="")

country_phylogeny_tree <- read.tree(paste0(path_phylogeny_dataset, "/country_phylogeny_tree.tree"))

world_shape <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
) %>% dplyr::filter(iso_a3 %in% orig_country_data$B_COUNTRY_ALPHA)