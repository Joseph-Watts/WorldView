test_data <- indiv_ordinal[indiv_ordinal$B_COUNTRY %in% c("Brazil", "New Zealand", "Canada", "China", "Australia", "India"), c("B_COUNTRY", "Q36", "Q49", "Q127", "Q172", "Q176", "Q192", "Q201", "Q221", "Q250", "Q251")]

bxplt_test_data <- test_data |>
  pivot_longer(cols = starts_with(c("Q", "E", "F_", "G_", "H_")),
               names_to = "question",
               values_to = "response")


ggplot(bxplt_test_data, aes(x = question, y = response, fill = B_COUNTRY)) +
  geom_boxplot(notch = TRUE) +
  labs(x = "Question", y = "Responses") +
  # theme_minimal() +
  theme(legend.position = "none")


anov_test <- aov(B_COUNTRY ~ ., data = test_data)

ctry_test <- aov(Q36 ~ B_COUNTRY, data = test_data)

summary(ctry_test)
tidy(ctry_test)
class(ctry_tes)

par(mfrow = c(2, 2))
plot(ctry_test)


summary(anov_test)

par(mfrow = c(2, 2))
plot(anov_test)
print(anov_test)

par(mfrow = c(1, 1))





pairwise.t.test(test_data[, 2],
                interaction(test_data$B_COUNTRY, test_data[, c(3, 4)]),
                p.adjust.method = "bonferroni")

aov(test_data[-1] ~ B_COUNTRY, data = test_data)


manova(cbind(test_data[, -1]) ~ B_COUNTRY, data = test_data)




# library(ggpubr)
# library(rstatix)
# library(datarium)

# 
# 
# 
# # boxplot ou violin plot
# # H0 and Ha
# 

# 
# set.seed(123)
# data("jobsatisfaction", package = "datarium")
# jobsatisfaction %>% sample_n_by(gender, education_level, size = 1)
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   get_summary_stats(score, type = "mean_sd")
# 
# 
# bxp <- ggboxplot(
#   jobsatisfaction, x = "gender", y = "score",
#   color = "education_level", palette = "jco"
# )
# bxp
# 
# 
# 
# 
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   identify_outliers(score)
# 
# # Build the linear model
# model  <- lm(score ~ gender*education_level,
#              data = jobsatisfaction)
# # Create a QQ plot of residuals
# ggqqplot(residuals(model))
# 
# # Compute Shapiro-Wilk test of normality
# shapiro_test(residuals(model))
# 
# 
# jobsatisfaction %>%
#   group_by(gender, education_level) %>%
#   shapiro_test(score)
# 
# ggqqplot(jobsatisfaction, "score", ggtheme = theme_bw()) +
#   facet_grid(gender ~ education_level)
# 
# jobsatisfaction %>% levene_test(score ~ gender*education_level)
# 
# 
# res.aov <- jobsatisfaction %>% anova_test(score ~ gender * education_level)
# res.aov
# 
# 
# # Group the data by gender and fit  anova
# model <- lm(score ~ gender * education_level, data = jobsatisfaction)
# jobsatisfaction %>%
#   group_by(gender) %>%
#   anova_test(score ~ education_level, error = model)
# 
# 
# # pairwise comparisons
# library(emmeans)
# pwc <- jobsatisfaction %>% 
#   group_by(gender) %>%
#   emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 
# pwc
# 
# res.aov
# 
# 
# jobsatisfaction %>%
#   pairwise_t_test(
#     score ~ education_level, 
#     p.adjust.method = "bonferroni"
#   )
# 
# model <- lm(score ~ gender * education_level, data = jobsatisfaction)
# jobsatisfaction %>% 
#   emmeans_test(
#     score ~ education_level, p.adjust.method = "bonferroni",
#     model = model
#   )
# 
# # Visualization: box plots with p-values
# pwc <- pwc %>% add_xy_position(x = "gender")
# bxp +
#   stat_pvalue_manual(pwc) +
#   labs(
#     subtitle = get_test_label(res.aov, detailed = TRUE),
#     caption = get_pwc_label(pwc)
#   )








# Check original missingness
original_missingness <- miss_var_summary(orig_indiv_data)
print(original_missingness)


sample_with_missing_ratio <- function(data, sample_size) {
  # Get the total number of rows in the dataset
  total_rows <- nrow(data)
  
  # Calculate the sampled dataset
  sampled_data <- data %>%
    # For each column, sample missing and non-missing rows proportionally
    reframe(across(everything(), ~ {
      missing_indices <- which(is.na(.))
      non_missing_indices <- which(!is.na(.))
      
      # Number of missing and non-missing rows to sample
      num_missing <- round(sample_size * length(missing_indices) / total_rows)
      num_non_missing <- round(sample_size * length(non_missing_indices) / total_rows)
      
      # Sample indices for missing and non-missing values
      sampled_missing <- sample(missing_indices, size = num_missing, replace = FALSE)
      sampled_non_missing <- sample(non_missing_indices, size = num_non_missing, replace = FALSE)
      
      # Combine the sampled values
      combined_indices <- sort(c(sampled_missing, sampled_non_missing))
      .[combined_indices] # Return the sampled values
    }))
  
  return(sampled_data)
}

sampled_data <- sample_with_missing_ratio(orig_indiv_data, sample_size = 2500)
vis_miss(sampled_data)






















