# modules/phylogeny/map/map_utils.R

# join WVS data
join_world_wvs <- function(world_shape,
                           country_data,
                           var) {
  world_shape %>%
    left_join(country_data,
              by = c("iso_a3" = "B_COUNTRY_ALPHA"))
}

# use leaflet to plot world map
plot_wvs_world_map <- function(world_sf, var,
                               palette   = "Viridis",
                               log_scale = FALSE) {
  
  # 1. get value
  vals <- world_sf[[var]]
  
  if (log_scale) {
    vals <- log1p(vals)
  }
  
  # 2. color function
  pal_fun <- colorNumeric(
    palette = if (palette == "Viridis") "viridis" else palette,
    domain  = vals,
    na.color = "#CCCCCC"
  )
  
  # 3. pre mapping value and label to world_sf
  world_sf$plot_val <- vals
  
  if (!"name" %in% names(world_sf) && "name_long" %in% names(world_sf)) {
    world_sf$name <- world_sf$name_long
  }
  
  world_sf$label <- paste0(
    world_sf$name, ": ",
    round(world_sf[[var]], 3)
  )
  
  # 4. plot map
  leaflet::leaflet(world_sf) %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
      fillColor   = ~pal_fun(plot_val),  # col name: plot_val
      weight      = 1,
      color       = "white",
      fillOpacity = 0.8,
      label       = ~label,              # use label col
      highlight   = highlightOptions(
        weight = 2,
        color  = "#666",
        bringToFront = TRUE
      )
    ) %>% addLegend(
      pal = pal_fun,           
      values = ~plot_val,      
      opacity = 0.8,           
      title = var,             
      position = "bottomright" 
    )
}
