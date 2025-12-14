# modules/phylogeny/models/models_utils.R

# Generate PGLS interpretation
generate_pgls_interpretation <- function(model, lambda, lambda_p, outcome_var, has_warning = FALSE) {
  aic <- round(AIC(model), 2)
  
  # Start interpretation
  interpretation <- "<h4>Phylogenetic Signal Analysis</h4>"
  
  # Add warning note if present
  if (has_warning) {
    interpretation <- paste0(interpretation,
                             "<div class='alert alert-info' style='padding: 10px;'>",
                             "<strong>Technical Note:</strong> Lambda estimation reached parameter bounds. ",
                             "This is common and does not affect result validity.",
                             "</div>")
  }
  
  # Phylogenetic signal results
  if (!is.na(lambda)) {
    interpretation <- paste0(interpretation,
                             "<h5>Signal Strength (λ = ", round(lambda, 3), ")</h5>")
    
    # Signal strength classification
    if (lambda >= 0.7) {
      strength <- "strong"
      icon <- "✅"
    } else if (lambda >= 0.3) {
      strength <- "moderate" 
      icon <- "⚠️"
    } else {
      strength <- "weak"
      icon <- "ℹ️"
    }
    
    interpretation <- paste0(interpretation,
                             "<p>", icon, " <strong>", strength, " phylogenetic signal detected</strong></p>",
                             "<p>This suggests that ", outcome_var, " is ", strength, "ly influenced by ",
                             "evolutionary relationships between countries.</p>")
    
    # Significance with proper p-value formatting
    if (!is.na(lambda_p)) {
      # Format p-value for display
      if (lambda_p < 0.001) {
        p_display <- "< 0.001"
      } else {
        p_display <- round(lambda_p, 4)
      }
      
      if (lambda_p < 0.05) {
        interpretation <- paste0(interpretation,
                                 "<p><strong>Statistical significance:</strong> The phylogenetic signal is ",
                                 "<span style='color: green; font-weight: bold;'>statistically significant</span> ",
                                 "(p = ", p_display, ").</p>")
      } else {
        interpretation <- paste0(interpretation,
                                 "<p><strong>Statistical significance:</strong> The phylogenetic signal is ",
                                 "<span style='color: orange;'>not statistically significant</span> ",
                                 "(p = ", p_display, ").</p>")
        
        # Add explanation for non-significant result
        if (lambda > 0.5) {
          interpretation <- paste0(interpretation,
                                   "<p><em>Note: Despite high lambda value, the signal is not statistically significant. ",
                                   "This may be due to small sample size or high variability in the data.</em></p>")
        }
      }
    } else {
      interpretation <- paste0(interpretation,
                               "<p><strong>Statistical significance:</strong> Cannot be determined</p>")
    }
  } else {
    interpretation <- paste0(interpretation,
                             "<p>⚠️ <strong>Could not estimate phylogenetic signal</strong></p>")
  }
  
  # Model fit
  interpretation <- paste0(interpretation,
                           "<h5>Model Fit</h5>",
                           "<p><strong>AIC:</strong> ", aic, " (lower values indicate better fit)</p>")
  
  # Practical implications
  interpretation <- paste0(interpretation,
                           "<h5>Research Implications</h5>")
  
  if (!is.na(lambda)) {
    if (!is.na(lambda_p) && lambda_p < 0.05) {
      # Statistically significant signal
      if (lambda >= 0.5) {
        interpretation <- paste0(interpretation,
                                 "<p>✅ <strong>PGLS is strongly recommended</strong> for analyzing ", outcome_var, ".</p>",
                                 "<ul>",
                                 "<li>Strong and significant phylogenetic signal detected</li>",
                                 "<li>Language relationships strongly influence this variable</li>", 
                                 "<li>Standard methods may give biased results</li>",
                                 "<li>PGLS accounts for evolutionary relationships</li>",
                                 "</ul>")
      } else {
        interpretation <- paste0(interpretation,
                                 "<p>⚠️ <strong>PGLS is recommended</strong> for analyzing ", outcome_var, ".</p>",
                                 "<ul>",
                                 "<li>Significant phylogenetic signal detected</li>",
                                 "<li>PGLS provides more reliable estimates</li>",
                                 "</ul>")
      }
    } else {
      # Not statistically significant
      if (lambda >= 0.7) {
        interpretation <- paste0(interpretation,
                                 "<p>ℹ️ <strong>Consider using PGLS</strong> for analyzing ", outcome_var, ".</p>",
                                 "<ul>",
                                 "<li>High lambda value suggests potential phylogenetic signal</li>",
                                 "<li>Statistical significance may be limited by sample size</li>",
                                 "<li>PGLS may still provide more accurate results</li>",
                                 "</ul>")
      } else if (lambda >= 0.3) {
        interpretation <- paste0(interpretation,
                                 "<p>ℹ️ <strong>Standard methods may be sufficient</strong> for analyzing ", outcome_var, ".</p>",
                                 "<ul>",
                                 "<li>Moderate phylogenetic signal detected</li>",
                                 "<li>Signal is not statistically significant</li>",
                                 "<li>Both PGLS and standard methods should give similar results</li>",
                                 "</ul>")
      } else {
        interpretation <- paste0(interpretation,
                                 "<p>ℹ️ <strong>Standard methods are appropriate</strong> for analyzing ", outcome_var, ".</p>",
                                 "<ul>",
                                 "<li>Weak phylogenetic signal detected</li>",
                                 "<li>Language relationships have minimal influence</li>",
                                 "<li>PGLS and standard methods will give very similar results</li>",
                                 "</ul>")
      }
    }
  } else {
    interpretation <- paste0(interpretation,
                             "<p>⚠️ <strong>Cannot determine appropriate method</strong> due to estimation issues.</p>")
  }
  
  return(interpretation)
}