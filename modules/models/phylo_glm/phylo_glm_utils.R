# modules/models/phylo_glm/phylo_glm_utils.R

phylo_glm_callout <- function(type = c("info", "warning", "danger", "success"), ...) {
  type <- match.arg(type)
  shiny::tags$div(class = paste("callout", paste0("callout-", type)), ...)
}

phylo_glm_null <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ---- labels/choices
phylo_glm_get_label <- function(codebook_data, col_id) {
  hit <- codebook_data[codebook_data$Col_ID == col_id, , drop = FALSE]
  if (nrow(hit) == 0) return(col_id)
  lab <- hit$Col_Label[[1]]
  if (is.na(lab) || !nzchar(lab)) col_id else lab
}

phylo_glm_choice_label <- function(codebook_data, col_id) paste0(col_id, " — ", phylo_glm_get_label(codebook_data, col_id))


phylo_glm_make_choices <- function(codebook_data, vars) {
  labs <- vapply(vars, function(v) wvs_var_display(v, codebook_data), character(1))
  stats::setNames(vars, labs)
}


# ---- numeric variables only
phylo_glm_list_numeric_vars <- function(df, exclude = c("B_COUNTRY", "B_COUNTRY_ALPHA", ".tip_label", "._country_name", "._language_name", "._family_name")) {
  is_num <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  setdiff(names(df)[is_num], exclude)
}

# ---- tree/data alignment
phylo_glm_extract_iso3_from_tip <- function(tip_label) sub("^.*_", "", tip_label)

phylo_glm_make_tip_map <- function(tree) {
  iso3 <- vapply(tree$tip.label, phylo_glm_extract_iso3_from_tip, FUN.VALUE = character(1))
  data.frame(tip_label = tree$tip.label, iso3166alpha3 = iso3, stringsAsFactors = FALSE)
}

phylo_glm_prepare_tree_and_data <- function(wvs_country, lang_tree, lang_country_map = NULL, iso3_col = "B_COUNTRY_ALPHA") {
  stopifnot(iso3_col %in% names(wvs_country))
  
  tip_map <- phylo_glm_make_tip_map(lang_tree)
  tip_map_unique <- tip_map[!duplicated(tip_map$iso3166alpha3), , drop = FALSE]
  
  iso3_data <- as.character(wvs_country[[iso3_col]])
  iso3_keep <- intersect(iso3_data, tip_map_unique$iso3166alpha3)
  
  d_sub <- wvs_country[iso3_data %in% iso3_keep, , drop = FALSE]
  iso3_sub <- as.character(d_sub[[iso3_col]])
  
  d_sub$.tip_label <- tip_map_unique$tip_label[match(iso3_sub, tip_map_unique$iso3166alpha3)]
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
  
  keep <- d_sub$.tip_label
  keep <- keep[!is.na(keep)]
  tree_sub <- ape::keep.tip(lang_tree, keep)
  
  idx <- match(tree_sub$tip.label, d_sub$.tip_label)
  d_sub <- d_sub[idx, , drop = FALSE]
  rownames(d_sub) <- d_sub$.tip_label
  
  list(tree = tree_sub, data = d_sub)
}

# ---- threshold & binary Y (numeric only)
phylo_glm_parse_threshold <- function(thresh_text) {
  txt <- trimws(thresh_text)
  if (!nzchar(txt)) txt <- "q50"
  
  if (grepl("^[qQ][0-9]{1,3}$", txt)) {
    p <- as.numeric(sub("^[qQ]", "", txt))
    if (!is.finite(p) || p < 0 || p > 100) stop("Percentile must be between 0 and 100, e.g., q50.")
    return(list(type = "quantile", p = p / 100, raw = txt))
  }
  
  val <- suppressWarnings(as.numeric(txt))
  if (is.finite(val)) return(list(type = "numeric", value = val, raw = txt))
  
  stop("Invalid threshold. Use qxx (e.g., q50) or a numeric value (e.g., 1.6).")
}

phylo_glm_make_binary_y <- function(df, dv, thresh_text) {
  if (!dv %in% names(df)) stop("DV not found in data.")
  
  x <- suppressWarnings(as.numeric(df[[dv]]))
  if (!is.numeric(x) || all(is.na(x))) stop("Selected Y must be numeric.")
  
  thr <- phylo_glm_parse_threshold(thresh_text)
  
  if (thr$type == "quantile") {
    tval <- stats::quantile(x, probs = thr$p, na.rm = TRUE, names = FALSE, type = 7)
    y <- ifelse(is.na(x), NA_integer_, ifelse(x > tval, 1L, 0L))
    note <- paste0("Binary Y: ", dv, " > ", thr$raw, " (value=", signif(tval, 4), ") → 1.")
    return(list(y = y, note = note, threshold = as.numeric(tval)))
  } else {
    tval <- thr$value
    y <- ifelse(is.na(x), NA_integer_, ifelse(x > tval, 1L, 0L))
    note <- paste0("Binary Y: ", dv, " > ", thr$raw, " → 1.")
    return(list(y = y, note = note, threshold = as.numeric(tval)))
  }
}

# ---- formula & preprocessing
phylo_glm_build_formula <- function(dv_bin_name, x_vars, controls = character(0)) {
  rhs <- c(x_vars, controls)
  rhs <- rhs[!is.na(rhs) & nzchar(rhs)]
  if (length(rhs) == 0) stats::as.formula(paste(dv_bin_name, "~ 1"))
  else stats::as.formula(paste(dv_bin_name, "~", paste(rhs, collapse = " + ")))
}

phylo_glm_drop_missing <- function(df, vars) df[stats::complete.cases(df[, vars, drop = FALSE]), , drop = FALSE]

phylo_glm_standardize_numeric <- function(df, vars) {
  for (v in vars) {
    if (v %in% names(df) && is.numeric(df[[v]])) {
      sdv <- stats::sd(df[[v]], na.rm = TRUE)
      if (is.finite(sdv) && sdv > 0) df[[v]] <- as.numeric(scale(df[[v]]))
    }
  }
  df
}

# ---- alpha formatting
phylo_glm_extract_alpha <- function(model) {
  if (!is.null(model$optpar)) {
    if ("alpha" %in% names(model$optpar)) return(as.numeric(model$optpar[["alpha"]]))
    if (length(model$optpar) == 1) return(as.numeric(model$optpar[[1]]))
  }
  if (!is.null(model$alpha)) return(as.numeric(model$alpha))
  NA_real_
}
phylo_glm_format_alpha <- function(a) {
  if (is.na(a)) return("—")
  if (abs(a) < 1e-3) return(format(a, scientific = TRUE, digits = 3))
  format(round(a, 3), nsmall = 3)
}

# ---- fit (capture warnings)
phylo_glm_fit <- function(tree, data, formula, method = "logistic_MPLE") {
  warn_msgs <- character(0)
  mod <- withCallingHandlers(
    phylolm::phyloglm(formula = formula, data = data, phy = tree, method = method),
    warning = function(w) {
      warn_msgs <<- c(warn_msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  attr(mod, "fit_warnings") <- warn_msgs
  mod
}

# ---- y/p from stored data (avoid model.frame(model))
phylo_glm_get_y_p_from_data <- function(model, data, dv_bin_name) {
  tips <- names(model$fitted.values)
  y <- as.numeric(data[tips, dv_bin_name])
  p <- as.numeric(model$fitted.values)
  list(y = y, p = p, tips = tips)
}

# ---- AUC/ROC via pROC (from data)
phylo_glm_auc <- function(model, data, dv_bin_name) {
  yp <- phylo_glm_get_y_p_from_data(model, data, dv_bin_name)
  y <- yp$y; p <- yp$p
  ok <- is.finite(y) & is.finite(p)
  y <- y[ok]; p <- p[ok]
  
  if (length(unique(y)) < 2) {
    return(list(auc = NA_real_, ci = c(NA, NA, NA), roc = NULL, note = "AUC needs both 0 and 1 classes."))
  }
  
  roc_obj <- pROC::roc(response = y, predictor = p, quiet = TRUE, direction = "<")
  auc_val <- as.numeric(pROC::auc(roc_obj))
  ci <- tryCatch(as.numeric(pROC::ci.auc(roc_obj)), error = function(e) c(NA, NA, NA))
  list(auc = auc_val, ci = ci, roc = roc_obj, note = NULL)
}

phylo_glm_roc_df <- function(roc_obj) {
  if (is.null(roc_obj)) return(NULL)
  df <- data.frame(fpr = 1 - roc_obj$specificities, tpr = roc_obj$sensitivities)
  df <- df[order(df$fpr, df$tpr), , drop = FALSE]
  df
}

phylo_glm_plot_roc_gg <- function(roc_df) {
  if (is.null(roc_df) || nrow(roc_df) == 0) return(NULL)
  ggplot2::ggplot(roc_df, ggplot2::aes(x = fpr, y = tpr)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(x = "False Positive Rate (1 - Specificity)", y = "True Positive Rate (Sensitivity)") +
    ggplot2::theme_minimal()
}

phylo_glm_glance <- function(model, data, dv_bin_name) {
  aic <- tryCatch(stats::AIC(model), error = function(e) NA_real_)
  alpha <- phylo_glm_extract_alpha(model)
  auc_res <- tryCatch(phylo_glm_auc(model, data, dv_bin_name), error = function(e) list(auc = NA, ci = c(NA, NA, NA), roc = NULL, note = e$message))
  list(
    n = length(model$fitted.values),
    alpha = alpha,
    AIC = aic,
    auc = auc_res$auc,
    auc_ci = auc_res$ci,
    roc = auc_res$roc,
    auc_note = auc_res$note,
    warnings = attr(model, "fit_warnings")
  )
}

# ---- tidy coefs + forest plot
phylo_glm_tidy_coef <- function(model) {
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  se <- sqrt(diag(V))
  data.frame(
    term = names(beta),
    estimate = as.numeric(beta),
    se = as.numeric(se),
    conf_low = as.numeric(beta - 1.96 * se),
    conf_high = as.numeric(beta + 1.96 * se),
    stringsAsFactors = FALSE
  )
}

phylo_glm_plot_forest_gg <- function(coef_df, term_labels = NULL, show_or = TRUE) {
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
  
  if (isTRUE(show_or)) {
    df$x <- exp(df$estimate); df$lo <- exp(df$conf_low); df$hi <- exp(df$conf_high)
    xlab <- "Odds Ratio (exp(β))"; vline <- 1
  } else {
    df$x <- df$estimate; df$lo <- df$conf_low; df$hi <- df$conf_high
    xlab <- "Log-odds coefficient (β)"; vline <- 0
  }
  
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = label)) +
    ggplot2::geom_vline(xintercept = vline, linetype = "dashed") +
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = lo, xmax = hi), height = 0.2) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(x = xlab, y = NULL) +
    ggplot2::theme_minimal()
}

# =========================================================
# FIX 2: Marginal effect (do NOT call terms(model))
# Use formula stored in model$call$formula instead.
# =========================================================
phylo_glm_get_model_formula <- function(model) {
  # phyloglm stores call; safest is to eval it in parent frame
  fml <- NULL
  fml <- tryCatch(model$formula, error = function(e) NULL)
  if (is.null(fml)) {
    fml <- tryCatch(eval(model$call$formula, envir = parent.frame()), error = function(e) NULL)
  }
  if (is.null(fml)) stop("Cannot extract model formula from phyloglm object.")
  fml
}

phylo_glm_make_effect_df <- function(model, data, focal_var, grid_n = 60) {
  fml <- phylo_glm_get_model_formula(model)
  terms_obj <- stats::terms(fml)  # terms from formula, not from model object
  dv_name <- all.vars(fml)[1]
  pred_vars <- attr(terms_obj, "term.labels")
  
  if (length(pred_vars) == 0) return(NULL)
  if (!focal_var %in% pred_vars) return(NULL)
  if (!is.numeric(data[[focal_var]])) return(NULL)
  
  # baseline: mean for numeric, mode for factor
  base <- data[1, , drop = FALSE]
  for (v in pred_vars) {
    if (is.numeric(data[[v]])) base[[v]] <- mean(data[[v]], na.rm = TRUE)
    else {
      vv <- data[[v]]; vv <- vv[!is.na(vv)]
      if (length(vv) > 0) base[[v]] <- names(sort(table(vv), decreasing = TRUE))[1]
    }
  }
  
  x <- data[[focal_var]]
  xseq <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = grid_n)
  newdata <- base[rep(1, grid_n), , drop = FALSE]
  newdata[[focal_var]] <- xseq
  
  # design matrix from predictors only
  X <- stats::model.matrix(stats::delete.response(terms_obj), newdata)
  beta <- stats::coef(model)
  V <- stats::vcov(model)
  
  eta <- as.numeric(X %*% beta)
  se_eta <- sqrt(pmax(0, diag(X %*% V %*% t(X))))
  
  p <- stats::plogis(eta)
  lo <- stats::plogis(eta - 1.96 * se_eta)
  hi <- stats::plogis(eta + 1.96 * se_eta)
  
  data.frame(x = xseq, p = p, low = lo, high = hi, dv = dv_name)
}

phylo_glm_plot_effect_gg <- function(effect_df, focal_label = "X") {
  if (is.null(effect_df)) return(NULL)
  ggplot2::ggplot(effect_df, ggplot2::aes(x = x, y = p)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = low, ymax = high), alpha = 0.2) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::labs(x = focal_label, y = "Predicted probability") +
    ggplot2::theme_minimal()
}

# ---- Observed vs Predicted (from data)
phylo_glm_plot_ovp_gg <- function(model, data, dv_bin_name) {
  yp <- phylo_glm_get_y_p_from_data(model, data, dv_bin_name)
  tips <- yp$tips
  y <- yp$y
  p <- yp$p
  
  m <- data[tips, , drop = FALSE]
  tooltip <- paste0(
    "Country: ", m$._country_name,
    "<br>Language: ", m$._language_name,
    "<br>Family: ", m$._family_name,
    "<br>Y: ", y,
    "<br>Pred prob: ", round(p, 3)
  )
  
  df <- data.frame(p = p, y = y, tooltip = tooltip)
  df$y_j <- df$y + stats::runif(nrow(df), -0.06, 0.06)
  
  ggplot2::ggplot(df, ggplot2::aes(x = p, y = y_j, text = tooltip)) +
    ggplot2::geom_point(alpha = 0.85, size = 2) +
    ggplot2::labs(x = "Predicted probability", y = "Observed (0/1, jittered)") +
    ggplot2::theme_minimal()
}

# =========================================================
# FIX 1: Stepwise (phyloglmstep) — remove keeping.formula
# Use full formula + optional starting.formula only.
# =========================================================
phylo_glm_run_stepwise <- function(tree, data, dv_bin_name, candidates,
                                   controls = character(0),
                                   direction = "both", k = 2, trace_level = 1,
                                   method = "logistic_MPLE") {
  
  # Full model: controls + candidates
  full_rhs <- unique(c(controls, candidates))
  full_formula <- phylo_glm_build_formula(dv_bin_name, x_vars = full_rhs, controls = character(0))
  
  # Starting model (optional): controls only (or intercept)
  starting_formula <- NULL
  if (length(controls) > 0) {
    starting_formula <- stats::as.formula(paste(dv_bin_name, "~", paste(controls, collapse = " + ")))
  } else {
    starting_formula <- stats::as.formula(paste(dv_bin_name, "~ 1"))
  }
  
  note <- NULL
  if (length(controls) > 0 && direction != "forward") {
    # Not an error, just clarify behavior
    note <- "Note: controls are only used as starting model; they may be dropped unless direction='forward'."
  }
  
  step_obj <- NULL
  step_log <- capture.output({
    step_obj <- tryCatch(
      phylolm::phyloglmstep(
        formula = full_formula,
        starting.formula = starting_formula,
        data = data,
        phy = tree,
        method = method,
        direction = direction,
        trace = trace_level,
        k = k
      ),
      error = function(e) e
    )
  })
  
  if (inherits(step_obj, "error") || is.null(step_obj)) {
    status <- paste("Stepwise failed:", if (inherits(step_obj, "error")) step_obj$message else "unknown error")
    return(list(status = status, final_model = NULL, step_log = step_log, note = note))
  }
  
  list(status = "Stepwise completed (exploratory).", final_model = step_obj, step_log = step_log, note = note)
}
