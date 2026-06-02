# modules/geo_viz/geo_viz_global.R

## packages
library("leaflet")
library("sf")
library("rnaturalearth")
library("viridisLite")
library("htmltools")

## Geographic Visualisation sub modules
source("modules/geo_viz/map/map_utils.R")
source("modules/geo_viz/map/map_ui.R")
source("modules/geo_viz/map/map_server.R")

## data load
## phylo_viz_global.R already loaded

## data pre-process
world_shape <- rnaturalearth::ne_countries(
  scale = "medium", returnclass = "sf"
) %>% dplyr::filter(iso_a3 %in% country_data$B_COUNTRY_ALPHA)
