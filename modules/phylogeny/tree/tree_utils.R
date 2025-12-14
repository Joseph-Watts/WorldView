# modules/phylogeny/tree/tree_utils.R

#' Prepare tree data with WVS7 data, pruning tree to match available countries
#' 
#' @param tree Phylogenetic tree object
#' @param wvs_data WVS7 country-level data
#' @param lang_country_map Mapping between countries and languages
#' @param outcome_var Variable name to use for coloring
#' @param language_format Format for language part of tip labels
#' @param country_format Format for country part of tip labels
#' @return Processed tree data for plotting
prepare_wvs_tree_data <- function(tree, wvs_data, lang_country_map, outcome_var,
                                  language_format = "glottocode", 
                                  country_format = "iso3166alpha3") {
  
  # Extract country codes from tree tip labels (format: glottocode_ISO3)
  tip_country_codes <- sapply(strsplit(tree$tip.label, "_"), function(x) x[length(x)])
  
  # Create base tip data frame
  tip_data <- data.frame(
    original_tip_label = tree$tip.label,
    country_code = tip_country_codes,
    stringsAsFactors = FALSE
  )
  
  # Merge with language-country mapping - keep original country_code
  tip_data <- tip_data %>%
    left_join(lang_country_map, by = c("country_code" = "iso3166alpha3"), keep = TRUE)
  
  # Merge with WVS7 data using B_COUNTRY_ALPHA (ISO3 codes)
  tip_data <- tip_data %>%
    left_join(
      wvs_data %>% 
        select(B_COUNTRY_ALPHA, all_of(outcome_var)),
      by = c("country_code" = "B_COUNTRY_ALPHA")
    )
  
  # Rename outcome variable for consistency
  if (outcome_var %in% names(tip_data)) {
    tip_data$outcome_value <- tip_data[[outcome_var]]
  } else {
    tip_data$outcome_value <- NA
  }
  
  # Generate formatted tip labels
  tip_data$formatted_tip_label <- generate_formatted_tip_labels(
    tip_data, language_format, country_format
  )
  
  
  # Prune tree to only include tips with WVS7 data
  pruned_result <- prune_tree_to_wvs_data(tree, tip_data)
  
  # Return processed data
  list(
    tree = pruned_result$tree,
    tip_data = pruned_result$tip_data,
    outcome_var = outcome_var
  )
}

#' Generate formatted tip labels based on user selection
#' 
#' @param tip_data Data frame with tip information
#' @param language_format Format for language part
#' @param country_format Format for country part
#' @return Character vector of formatted labels
generate_formatted_tip_labels <- function(tip_data, language_format, country_format) {
  # Get language part based on format
  language_part <- switch(language_format,
                          "glottocode" = tip_data$glottocode,
                          "iso639P3" = tip_data$iso639P3,
                          "language_name" = tip_data$language_name)
  
  # Get country part based on format
  country_part <- switch(country_format,
                         "iso3166alpha3" = tip_data$iso3166alpha3,
                         "iso3166alpha2" = tip_data$iso3166alpha2,
                         "country_name" = tip_data$country_name)
  
  # Combine with underscore separator
  paste(language_part, country_part, round(tip_data$outcome_value,3), sep = ", ")
}

#' Prune tree to include only tips with WVS7 data
#' 
#' @param tree Original phylogenetic tree
#' @param tip_data Tip data with outcome values
#' @return List with pruned tree and corresponding tip data
prune_tree_to_wvs_data <- function(tree, tip_data) {
  # Identify tips with valid WVS7 data
  valid_tips <- !is.na(tip_data$outcome_value)
  
  if (sum(valid_tips) == 0) {
    stop("No matching WVS7 data found for any countries in the tree")
  }
  
  if (sum(valid_tips) < length(valid_tips)) {
    # Some tips missing data - prune tree to only WVS7 countries
    tips_to_keep <- tip_data$original_tip_label[valid_tips]
    pruned_tree <- ape::keep.tip(tree, tips_to_keep)
    pruned_tip_data <- tip_data[valid_tips, ]
    
    # Update tip labels in pruned tree
    pruned_tree$tip.label <- pruned_tip_data$formatted_tip_label
    
    # message("Pruned tree from ", length(tree$tip.label), " to ", 
    #         length(pruned_tree$tip.label), " tips (WVS7 countries only)")
    
    return(list(tree = pruned_tree, tip_data = pruned_tip_data))
  }
  
  # All tips have data - update tip labels in original tree
  tree$tip.label <- tip_data$formatted_tip_label
  return(list(tree = tree, tip_data = tip_data))
}

#' Plot blank tree (no data coloring)
#' 
#' @param tree Phylogenetic tree
#' @param layout_type Tree layout
#' @param tip_label_size Size of tip labels
#' @param lang_country_map Language-country mapping for label formatting
#' @param language_format Format for language part of labels
#' @param country_format Format for country part of labels
plot_blank_tree <- function(tree, layout_type = "fan", tip_label_size = 0.7, 
                            lang_country_map, language_format = "glottocode", 
                            country_format = "iso3166alpha3") {
  
  # Extract country codes and prepare formatted labels
  tip_country_codes <- sapply(strsplit(tree$tip.label, "_"), function(x) x[length(x)])
  
  tip_data <- data.frame(
    original_tip_label = tree$tip.label,
    country_code = tip_country_codes,
    stringsAsFactors = FALSE
  )
  
  # Merge with language-country mapping - keep original country_code
  tip_data <- tip_data %>%
    left_join(lang_country_map, by = c("country_code" = "iso3166alpha3"), keep = TRUE)
  
  # Generate formatted labels
  tree$tip.label <- generate_formatted_tip_labels(
    tip_data, language_format, country_format
  )
  
  # Plot the tree
  ape::plot.phylo(
    tree,
    type = layout_type,
    tip.color = "black",
    show.tip.label = TRUE,  # Always show labels
    cex = tip_label_size,
    edge.color = "gray50",
    edge.width = 1.5,
    main = "Language Phylogeny Tree (Base)"
  )
}

#' Plot tree with WVS7 data coloring
#' 
#' @param tree_data Processed tree data from prepare_wvs_tree_data()
#' @param layout_type Tree layout type
#' @param tip_label_size Size of tip labels
#' @param color_scheme Color scheme for plotting
#' @param show_legend Whether to show color legend
#' @param outcome_var Name of the outcome variable for title
#' @return Base R plot
plot_wvs_tree <- function(tree_data, layout_type = "fan", tip_label_size = 0.7,
                          color_scheme = "viridis", show_legend = TRUE, 
                          outcome_var = NULL) {
  
  tree <- tree_data$tree
  tip_data <- tree_data$tip_data
  
  outcome_values <- tip_data$outcome_value
  valid_data <- !is.na(outcome_values)
  
  if (sum(valid_data) == 0) {
    stop("No valid data available for plotting")
  }
  
  # Create color mapping for valid data
  colors <- create_color_mapping(outcome_values[valid_data], color_scheme)
  
  # Apply colors
  tip_colors <- colors$colors[match(1:length(outcome_values), which(valid_data))]
  tip_colors[!valid_data] <- "gray80"  # Should not happen due to pruning, but safety
  
  # Plot the tree
  ape::plot.phylo(
    tree,
    type = layout_type,
    tip.color = tip_colors,
    show.tip.label = TRUE,  # Always show labels
    cex = tip_label_size,
    edge.color = "gray50",
    edge.width = 1.5
  )
  
  # Add title
  if (!is.null(outcome_var)) {
    title(main = paste("Language Phylogeny - Colored by:", outcome_var))
  }
  
  # Add legend if requested
  if (show_legend && sum(valid_data) > 0) {
    add_color_legend(colors, position = "bottomleft")
  }
}

#' Create color mapping for continuous values
#' 
#' @param values Numeric values to map to colors
#' @param scheme Color scheme name
#' @return List with colors and value range
create_color_mapping <- function(values, scheme = "viridis") {
  # Normalize values to [0, 1] range
  val_range <- range(values, na.rm = TRUE)
  normalized <- (values - val_range[1]) / (val_range[2] - val_range[1])
  
  # Get color palette
  palette <- switch(scheme,
                    "viridis" = viridis::viridis(100),
                    "plasma" = viridis::plasma(100),
                    "inferno" = viridis::inferno(100),
                    "magma" = viridis::magma(100),
                    "cividis" = viridis::cividis(100))
  
  # Map values to colors
  color_indices <- round(normalized * 99) + 1
  colors <- palette[color_indices]
  
  list(colors = colors, range = val_range, palette = palette)
}

#' Add color legend to plot
#' 
#' @param color_data Color mapping data from create_color_mapping()
#' @param position Legend position
add_color_legend <- function(color_data, position = "bottomleft") {
  # Create gradient legend
  legend_colors <- color_data$palette
  val_range <- color_data$range
  
  # Calculate position based on layout
  usr <- par("usr")
  x_left <- usr[1] + 0.02 * (usr[2] - usr[1])
  x_right <- x_left + 0.03 * (usr[2] - usr[1])
  y_bottom <- usr[3] + 0.05 * (usr[4] - usr[3])
  y_top <- y_bottom + 0.2 * (usr[4] - usr[3])
  
  # Draw color gradient
  rasterImage(as.raster(matrix(rev(legend_colors), ncol = 1)), 
              x_left, y_bottom, x_right, y_top)
  
  # Add value labels
  text(x_right + 0.01 * (usr[2] - usr[1]), 
       c(y_bottom, (y_bottom + y_top)/2, y_top),
       labels = round(c(val_range[1], mean(val_range), val_range[2]), 2),
       pos = 4, cex = 0.8)
}