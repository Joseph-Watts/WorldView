# modules/phylo_viz/tree/tree_utils.R

# ------------------------------------------------------------------------------
# Palettes
# ------------------------------------------------------------------------------

# Default palette order for up to 8 variables
default_palette_sequence <- function() {
  c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
}

# Pick default palette based on variable position in selection
default_palette_for_var <- function(var_name, selected_vars) {
  seq <- default_palette_sequence()
  idx <- match(var_name, selected_vars)
  
  if (is.na(idx) || idx < 1) return("viridis")
  if (idx > length(seq)) return(tail(seq, 1))
  seq[[idx]]
}

# ------------------------------------------------------------------------------
# Small helpers
# ------------------------------------------------------------------------------

`%||%` <- function(x, y) if (is.null(x)) y else x

assert_tree_deps <- function() {
  pkgs <- c("ggplot2", "dplyr", "tidyr", "viridis", "ape", "ggtree", "ggtreeExtra", "ggnewscale")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "))
  }
}

# ------------------------------------------------------------------------------
# Tip parsing + joins
# ------------------------------------------------------------------------------

# Tree tip.label format: "glottocode_ISO3"
extract_tip_keys <- function(tree) {
  tip_country_codes <- vapply(strsplit(tree$tip.label, "_"), function(x) x[length(x)], character(1))
  tip_glottocodes   <- vapply(strsplit(tree$tip.label, "_"), function(x) x[1], character(1))
  
  data.frame(
    label = tree$tip.label,              # Must match ggtree tip key
    country_code = tip_country_codes,    # ISO3
    glottocode_from_tip = tip_glottocodes,
    stringsAsFactors = FALSE
  )
}

# Build tip annotation table: (tip keys) + (language/country map) + (WVS variables)
make_tip_annotation <- function(tree, wvs_data, lang_country_map, outcome_vars) {
  tip_keys <- extract_tip_keys(tree)
  
  # Join mapping by ISO3 (country_code)
  tip_anno <- dplyr::left_join(
    tip_keys,
    lang_country_map,
    by = c("country_code" = "iso3166alpha3"),
    keep = TRUE
  )
  
  # Join WVS by ISO3 (B_COUNTRY_ALPHA)
  if (length(outcome_vars) > 0) {
    wvs_small <- dplyr::select(wvs_data, B_COUNTRY_ALPHA, dplyr::all_of(outcome_vars))
    tip_anno <- dplyr::left_join(
      tip_anno,
      wvs_small,
      by = c("country_code" = "B_COUNTRY_ALPHA")
    )
  }
  
  tip_anno
}

# Create a readable tip label from selected fields
build_tip_label <- function(tip_anno, fields = c("country_name"), sep = " | ") {
  fields <- unique(fields)
  fields <- fields[fields %in% names(tip_anno)]
  if (length(fields) == 0) return(tip_anno$label)
  
  parts <- lapply(fields, function(f) {
    x <- tip_anno[[f]]
    x[is.na(x)] <- ""
    as.character(x)
  })
  
  out <- do.call(paste, c(parts, list(sep = sep)))
  out <- trimws(gsub(paste0("(^\\Q", sep, "\\E\\s*|\\s*\\Q", sep, "\\E$)"), "", out))
  ifelse(out == "", tip_anno$label, out)
}

# Prune tips that have no data for ALL selected variables (keep if ANY variable is available)
prune_tree_for_selected_vars <- function(tree, tip_anno, outcome_vars) {
  if (length(outcome_vars) == 0) {
    return(list(tree = tree, tip_anno = tip_anno))
  }
  
  keep <- rep(FALSE, nrow(tip_anno))
  for (v in outcome_vars) {
    keep <- keep | !is.na(tip_anno[[v]])
  }
  
  if (sum(keep) == 0) {
    stop("No matching WVS data for selected variables on this tree.")
  }
  
  kept_labels <- tip_anno$label[keep]
  pruned_tree <- ape::keep.tip(tree, kept_labels)
  pruned_tip_anno <- tip_anno[keep, , drop = FALSE]
  
  list(tree = pruned_tree, tip_anno = pruned_tip_anno)
}

# ------------------------------------------------------------------------------
# Plot helpers
# ------------------------------------------------------------------------------

viridis_scale_for_option <- function(option, name, legend_order = 1, show_legend = TRUE) {
  ggplot2::scale_fill_viridis_c(
    option = option,
    name = name,
    na.value = "grey90",
    guide = if (show_legend) ggplot2::guide_colorbar(order = legend_order) else "none"
  )
}

make_long_bar_data <- function(tip_anno, outcome_vars) {
  dplyr::select(tip_anno, label, dplyr::all_of(outcome_vars)) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(outcome_vars),
      names_to = "variable",
      values_to = "value"
    )
}

# ------------------------------------------------------------------------------
# Main plot builder (ggtree + multiple bar panels)
# ------------------------------------------------------------------------------

build_ggtree_multi_bar_plot <- function(
    tree,
    wvs_data,
    lang_country_map,
    outcome_vars,
    palettes_by_var,
    layout = "rectangular",
    show_tip_labels = TRUE,
    tip_label_fields = c("country_name"),
    tip_label_sep = " | ",
    tip_label_size = 3,
    bar_panel_width = 0.6,
    bar_panel_gap = 0.06,
    show_legends = TRUE
) {
  assert_tree_deps()
  
  # geom_fruit requires a bare geom name (avoid ggplot2::geom_col in the call)
  geom_col  <- ggplot2::geom_col
  geom_text <- ggplot2::geom_text
  
  # Prepare tip annotation
  tip_anno <- make_tip_annotation(tree, wvs_data, lang_country_map, outcome_vars)
  tip_anno$formatted_label <- build_tip_label(tip_anno, tip_label_fields, tip_label_sep)
  
  # Prune tree for selected variables
  if (length(outcome_vars) > 0) {
    pruned <- prune_tree_for_selected_vars(tree, tip_anno, outcome_vars)
    tree <- pruned$tree
    tip_anno <- pruned$tip_anno
  }
  
  tip_anno_tree <- dplyr::select(tip_anno, -formatted_label)
  
  # Base tree
  p <- ggtree::ggtree(tree, layout = layout) %<+% tip_anno_tree +
    ggtree::theme_tree2()
  
  # Tip labels aligned right (works best for rectangular/slanted)
  # if (show_tip_labels) {
  #   p <- p + ggtree::geom_tiplab(
  #     ggplot2::aes(label = formatted_label),
  #     align = TRUE,
  #     linesize = 0.25,
  #     size = tip_label_size
  #   )
  # }
  
  offset <- bar_panel_gap
  
  if(length(outcome_vars) > 0){
    # Multi-variable bars: one panel per variable, each with its own fill scale + legend
    bar_long <- make_long_bar_data(tip_anno, outcome_vars)
    
    for (i in seq_along(outcome_vars)) {
      var <- outcome_vars[i]
      pal <- palettes_by_var[[var]] %||% "viridis"
      
      df_var <- bar_long[bar_long$variable == var, , drop = FALSE]
      
      p <- p +
        ggtreeExtra::geom_fruit(
          data = df_var,
          geom = geom_col,
          mapping = ggplot2::aes(
            y = label,     # y is tip key
            x = value,
            fill = value   # fill mapped to value => per-variable continuous legend
          ),
          orientation = "y",
          offset = offset,
          pwidth = bar_panel_width,
          axis.params = list(
            axis = "x",
            text.size = 2.5,
            title = var,
            title.size = 3
          ),
          grid.params = list()
        ) +
        viridis_scale_for_option(
          option = pal,
          name = var,
          legend_order = i,      # <-- keep legend order same as variable order
          show_legend = show_legends
        )
      
      # Separate legends/scales for next variable
      if (i < length(outcome_vars)) {
        p <- p + ggnewscale::new_scale_fill()
      }
      
      offset <- offset + bar_panel_gap
    }
  }

  # ---- Add a final text panel for tip labels (prevents overlap with bars) ----
  if (show_tip_labels) {
    p <- p +
      ggtreeExtra::geom_fruit(
        data = tip_anno,
        geom = geom_text,
        mapping = ggplot2::aes(
          y = label,                 # tip key
          x = 0,                     # constant x within this panel
          label = formatted_label    # IMPORTANT: use column name, not tip_anno$...
        ),
        orientation = "y",
        offset = offset,             # after the last bar panel
        pwidth = 1.0,
        # hjust = 0,                   # left align text
        size = tip_label_size,
        axis.params = list(axis = "none"),
        grid.params = NULL
      ) +
      ggplot2::theme(
        plot.margin = ggplot2::margin(5.5, 60, 5.5, 5.5)  # extra right margin for long labels
      )

    # IMPORTANT:
    # coord_cartesian() will overwrite the circular/fan/radial coordinate system
    # so only use it for rectangular/slanted layouts.
    if (layout %in% c("rectangular", "slanted")) {
      p <- p + ggplot2::coord_cartesian(clip = "off")
    }
  }
  
  # Blank mode (no variables)
  if (length(outcome_vars) == 0) {
    return(p + ggplot2::ggtitle("Language Phylogeny (Base)"))
  }
  p + ggplot2::ggtitle("Language Phylogeny (Multi-variable bars)")
}
