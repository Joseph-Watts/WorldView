# modules/models/phylo_lm/phylo_lm_utils.R

phylo_lm_callout <- function(type = c("info", "warning", "danger", "success"), ...) {
  type <- match.arg(type)
  shiny::tags$div(class = paste("callout", paste0("callout-", type)), ...)
}

phylo_lm_null <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}



# ----------------------------
# Labels / choices (include question ID like Q1)
# ----------------------------
phylo_lm_get_label <- function(codebook_data, col_id) {
  hit <- codebook_data[codebook_data$Col_ID == col_id, , drop = FALSE]
  if (nrow(hit) == 0) return(col_id)
  lab <- hit$Col_Label[[1]]
  if (is.na(lab) || !nzchar(lab)) col_id else lab
}

phylo_lm_choice_label <- function(codebook_data, col_id) {
  paste0(col_id, " — ", phylo_lm_get_label(codebook_data, col_id))
}

phylo_lm_make_choices <- function(codebook_data, vars) {
  labs <- vapply(vars, function(v) wvs_var_display(v, codebook_data), character(1))
  stats::setNames(vars, labs)
}

phylo_lm_list_numeric_vars <- function(df, exclude = c("B_COUNTRY", "B_COUNTRY_ALPHA")) {
  is_num <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  vars <- names(df)[is_num]
  setdiff(vars, exclude)
}

# ----------------------------
# Tree + data alignment + language metadata
# ----------------------------
phylo_lm_extract_iso3_from_tip <- function(tip_label) sub("^.*_", "", tip_label)

phylo_lm_make_tip_map <- function(tree) {
  iso3 <- vapply(tree$tip.label, phylo_lm_extract_iso3_from_tip, FUN.VALUE = character(1))
  data.frame(tip_label = tree$tip.label, iso3166alpha3 = iso3, stringsAsFactors = FALSE)
}

phylo_lm_prepare_tree_and_data <- function(wvs_country, lang_tree, lang_country_map = NULL, iso3_col = "B_COUNTRY_ALPHA") {
  stopifnot(iso3_col %in% names(wvs_country))
  
  tip_map <- phylo_lm_make_tip_map(lang_tree)
  tip_map_unique <- tip_map[!duplicated(tip_map$iso3166alpha3), , drop = FALSE]
  
  iso3_data <- as.character(wvs_country[[iso3_col]])
  iso3_keep <- intersect(iso3_data, tip_map_unique$iso3166alpha3)
  
  d_sub <- wvs_country[iso3_data %in% iso3_keep, , drop = FALSE]
  iso3_sub <- as.character(d_sub[[iso3_col]])
  
  tip_label_sub <- tip_map_unique$tip_label[match(iso3_sub, tip_map_unique$iso3166alpha3)]
  d_sub$.tip_label <- tip_label_sub
  
  # Attach display fields for hover
  d_sub$._country_name <- if ("B_COUNTRY" %in% names(d_sub)) as.character(d_sub$B_COUNTRY) else iso3_sub
  
  if (!is.null(lang_country_map)) {
    lmap <- lang_country_map[!duplicated(lang_country_map$iso3166alpha3), , drop = FALSE]
    meta <- lmap[, c("iso3166alpha3", "language_name", "family_language_name"), drop = FALSE]
    m <- meta[match(iso3_sub, meta$iso3166alpha3), , drop = FALSE]
    d_sub$._language_name <- m$language_name
    d_sub$._family_name <- m$family_language_name
  } else {
    d_sub$._language_name <- NA_character_
    d_sub$._family_name <- NA_character_
  }
  
  tree_sub <- ape::keep.tip(lang_tree, tip_label_sub)
  idx <- match(tree_sub$tip.label, d_sub$.tip_label)
  d_sub <- d_sub[idx, , drop = FALSE]
  rownames(d_sub) <- d_sub$.tip_label
  
  list(tree = tree_sub, data = d_sub, tip_map = tip_map_unique)
}

# ----------------------------
# Formula & preprocessing
# ----------------------------
phylo_lm_build_formula <- function(dv, x_vars, controls = character(0)) {
  rhs <- c(x_vars, controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  if (length(rhs) == 0) stats::as.formula(paste(dv, "~ 1"))
  else stats::as.formula(paste(dv, "~", paste(rhs, collapse = " + ")))
}

phylo_lm_drop_missing <- function(df, vars) {
  keep <- stats::complete.cases(df[, vars, drop = FALSE])
  df[keep, , drop = FALSE]
}

phylo_lm_standardize_numeric <- function(df, vars) {
  for (v in vars) {
    if (v %in% names(df) && is.numeric(df[[v]])) {
      sdv <- stats::sd(df[[v]], na.rm = TRUE)
      if (is.finite(sdv) && sdv > 0) df[[v]] <- as.numeric(scale(df[[v]]))
    }
  }
  df
}

# ----------------------------
# Extract lambda robustly (avoid always "-")
# ----------------------------
phylo_lm_extract_lambda <- function(model) {
  if (!is.null(model$optpar)) {
    if ("lambda" %in% names(model$optpar)) return(as.numeric(model$optpar[["lambda"]]))
    if (length(model$optpar) == 1) return(as.numeric(model$optpar[[1]]))
  }
  if (!is.null(model$lambda)) return(as.numeric(model$lambda))
  if (!is.null(model$param) && "lambda" %in% names(model$param)) return(as.numeric(model$param[["lambda"]]))
  NA_real_
}

phylo_lm_format_lambda <- function(lam) {
  if (is.na(lam)) return("—")
  # scientific if very small
  if (abs(lam) < 1e-3) return(format(lam, scientific = TRUE, digits = 3))
  format(round(lam, 3), nsmall = 3)
}

# ----------------------------
# Model fit (capture warnings; do NOT print)
# ----------------------------
phylo_lm_fit <- function(tree, data, formula, lower.bound = 1e-7, upper.bound = 1) {
  warn_msgs <- character(0)
  
  mod <- withCallingHandlers(
    phylolm::phylolm(
      formula = formula,
      data = data,
      phy = tree,
      model = "lambda",
      lower.bound = lower.bound,
      upper.bound = upper.bound
    ),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")  # suppress console warning
    }
  )
  
  attr(mod, "fit_warnings") <- warn_msgs
  mod
}

phylo_lm_r2 <- function(model) {
  # Approx R^2 based on observed vs fitted
  obs <- stats::model.response(stats::model.frame(model))
  fit <- as.numeric(model$fitted.values)
  sse <- sum((obs - fit)^2, na.rm = TRUE)
  sst <- sum((obs - mean(obs, na.rm = TRUE))^2, na.rm = TRUE)
  if (!is.finite(sst) || sst <= 0) return(NA_real_)
  1 - (sse / sst)
}

phylo_lm_glance <- function(model) {
  ll <- tryCatch(as.numeric(stats::logLik(model)), error = function(e) NA_real_)
  aic <- tryCatch(stats::AIC(model), error = function(e) NA_real_)
  lam <- phylo_lm_extract_lambda(model)
  r2 <- tryCatch(phylo_lm_r2(model), error = function(e) NA_real_)
  
  list(
    n = length(model$fitted.values),
    lambda = lam,
    logLik = ll,
    AIC = aic,
    r2 = r2,
    warnings = attr(model, "fit_warnings")
  )
}

phylo_lm_tidy_coef <- function(model) {
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  se <- sqrt(diag(V))
  ci_low <- beta - 1.96 * se
  ci_high <- beta + 1.96 * se
  
  data.frame(
    term = names(beta),
    estimate = as.numeric(beta),
    se = as.numeric(se),
    conf_low = as.numeric(ci_low),
    conf_high = as.numeric(ci_high),
    stringsAsFactors = FALSE
  )
}

# ----------------------------
# Plots
# ----------------------------
phylo_lm_plot_forest_gg <- function(coef_df, term_labels = NULL) {
  df <- coef_df
  df <- df[df$term != "(Intercept)", , drop = FALSE]
  
  if (nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotate("text", x = 0, y = 0,
                          label = "No predictors selected (model is Y ~ 1).",
                          size = 4)
    )
  }
  
  df$label <- df$term
  if (!is.null(term_labels)) df$label <- ifelse(df$term %in% names(term_labels), term_labels[df$term], df$term)
  df$label <- factor(df$label, levels = df$label[order(df$estimate)])
  
  ggplot2::ggplot(df, ggplot2::aes(x = estimate, y = label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = conf_low, xmax = conf_high), height = 0.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = "Coefficient (β)", y = NULL) +
    ggplot2::theme_minimal()
}

phylo_lm_make_effect_df <- function(model, data, focal_var, grid_n = 60) {
  mf <- stats::model.frame(model)
  vars_in_model <- names(mf)
  vars_in_model <- setdiff(vars_in_model, all.vars(stats::formula(model))[1]) # remove DV
  
  if (!focal_var %in% vars_in_model) return(NULL)
  x <- data[[focal_var]]
  if (!is.numeric(x)) return(NULL)
  
  # baseline: mean/mode
  base <- data[1, , drop = FALSE]
  for (v in vars_in_model) {
    if (!v %in% names(data)) next
    if (is.numeric(data[[v]])) {
      base[[v]] <- mean(data[[v]], na.rm = TRUE)
    } else {
      vv <- data[[v]]
      vv <- vv[!is.na(vv)]
      if (length(vv) > 0) base[[v]] <- names(sort(table(vv), decreasing = TRUE))[1]
    }
  }
  
  xseq <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid_n)
  newdata <- base[rep(1, grid_n), , drop = FALSE]
  newdata[[focal_var]] <- xseq
  
  X <- stats::model.matrix(stats::formula(model), newdata)
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  
  fit <- as.numeric(X %*% beta)
  se <- sqrt(pmax(0, diag(X %*% V %*% t(X))))
  
  data.frame(x = xseq, fit = fit, low = fit - 1.96 * se, high = fit + 1.96 * se)
}

phylo_lm_plot_effect_gg <- function(effect_df, focal_label = "X", y_label = "Predicted Y") {
  if (is.null(effect_df)) return(NULL)
  ggplot2::ggplot(effect_df, ggplot2::aes(x = x, y = fit)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), alpha = 0.2) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(x = focal_label, y = y_label) +
    ggplot2::theme_minimal()
}

phylo_lm_plot_ovp_gg <- function(model, meta_df, dv_label) {
  obs <- stats::model.response(stats::model.frame(model))
  pred <- as.numeric(model$fitted.values)
  
  tips <- names(model$fitted.values)
  m <- meta_df[tips, , drop = FALSE]
  
  tooltip <- paste0(
    "Country: ", m$._country_name,
    "<br>Language: ", m$._language_name,
    "<br>Family: ", m$._family_name,
    "<br>Observed: ", round(obs, 3),
    "<br>Predicted: ", round(pred, 3)
  )
  
  df <- data.frame(observed = as.numeric(obs), predicted = pred, tooltip = tooltip)
  
  ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = observed, text = tooltip)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_point(size = 2, alpha = 0.85) +
    ggplot2::labs(x = "Predicted", y = dv_label) +
    ggplot2::theme_minimal()
}

phylo_lm_make_resid_distance_df <- function(model, tree, meta_df, bins = 12) {
  res <- as.numeric(stats::residuals(model))
  names(res) <- tree$tip.label
  D <- ape::cophenetic.phylo(tree)
  
  idx <- which(upper.tri(D), arr.ind = TRUE)
  dist_vals <- D[idx]
  resid_diff <- abs(res[idx[, 1]] - res[idx[, 2]])
  
  tip_i <- rownames(D)[idx[, 1]]
  tip_j <- colnames(D)[idx[, 2]]
  
  mi <- meta_df[tip_i, , drop = FALSE]
  mj <- meta_df[tip_j, , drop = FALSE]
  
  raw <- data.frame(
    dist = as.numeric(dist_vals),
    resid_diff = as.numeric(resid_diff),
    stringsAsFactors = FALSE
  )
  
  breaks <- pretty(raw$dist, n = bins)
  raw$bin <- cut(raw$dist, breaks = breaks, include.lowest = TRUE)
  
  agg <- stats::aggregate(resid_diff ~ bin, data = raw, FUN = function(x) c(mean = mean(x), n = length(x)))
  
  out <- data.frame(
    bin = agg$bin,
    mean_resid_diff = agg$resid_diff[, "mean"],
    n_pairs = agg$resid_diff[, "n"],
    stringsAsFactors = FALSE
  )
  out$bin_mid <- seq_len(nrow(out))
  out$tooltip <- paste0("Bin: ", out$bin, "<br>Mean |Δresid|: ", round(out$mean_resid_diff, 3), "<br>Pairs: ", out$n_pairs)
  out
}

phylo_lm_plot_resid_dist_gg <- function(resid_dist_df) {
  if (is.null(resid_dist_df) || nrow(resid_dist_df) == 0) return(NULL)
  ggplot2::ggplot(resid_dist_df, ggplot2::aes(x = bin_mid, y = mean_resid_diff, text = tooltip)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Distance bins (closer → farther)", y = "Mean |residual_i − residual_j|") +
    ggplot2::theme_minimal()
}

# ----------------------------
# Stepwise: use keeping.formula + Step log (capture.output(trace))
# ----------------------------
phylo_lm_run_stepwise <- function(tree, data, dv, candidates, controls = character(0),
                                  direction = "both", k = 2, trace_level = 1) {
  
  full_rhs <- unique(c(controls, candidates))
  full_formula <- phylo_lm_build_formula(dv, x_vars = full_rhs, controls = character(0))
  
  # Controls must be kept (never dropped)
  keep_formula <- if (length(controls) == 0) {
    stats::as.formula(paste(dv, "~ 1"))
  } else {
    stats::as.formula(paste(dv, "~", paste(controls, collapse = " + ")))
  }
  
  step_obj <- NULL
  
  # Capture printed trace as step log
  step_log <- capture.output({
    step_obj <- tryCatch(
      phylolm::phylostep(
        formula = full_formula,
        keeping.formula = keep_formula,
        data = data,
        phy = tree,
        model = "lambda",
        direction = direction,
        k = k,
        trace = trace_level
      ),
      error = function(e) e
    )
  })
  
  # phylostep typically returns a phylolm object (best model)
  if (inherits(step_obj, "error") || is.null(step_obj)) {
    status <- paste("Stepwise failed:", if (inherits(step_obj, "error")) step_obj$message else "unknown error")
    return(list(
      status = status,
      final_model = NULL,
      step_log = step_log
    ))
  }
  
  list(
    status = "Stepwise completed (exploratory).",
    final_model = step_obj,
    step_log = step_log
  )
}
