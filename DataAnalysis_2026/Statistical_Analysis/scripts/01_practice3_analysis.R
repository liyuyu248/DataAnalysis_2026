# Practice 3 - Statistical Analysis
# Course: Data Analysis
# Dataset: data_for_analysis.csv
#
# This script produces:
# 1) Descriptive statistics by outcome group for all hormone variables
# 2) Distribution fit table by group
# 3) Shapiro-Wilk and Levene tests
# 4) Brunner-Munzel, t.test, and wilcox.test for two independent groups
# 5) Histograms, Q-Q plots, and correlation heatmaps by outcome group

# -----------------------------
# 0. Packages
# -----------------------------
packages <- c(
  "tidyverse", "car", "brunnermunzel", "fitdistrplus",
  "broom", "moments", "ggplot2", "scales"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))

# -----------------------------
# 1. Paths
# -----------------------------
root_dir <- normalizePath(file.path(dirname(sys.frame(1)$ofile), ".."), mustWork = FALSE)
data_path <- file.path(root_dir, "data", "data_for_analysis.csv")
output_dir <- file.path(root_dir, "outputs")
table_dir <- file.path(output_dir, "tables")
figure_dir <- file.path(output_dir, "figures")
hist_dir <- file.path(figure_dir, "histograms")
qq_dir <- file.path(figure_dir, "qqplots")
heatmap_dir <- file.path(figure_dir, "heatmaps")

dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(hist_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qq_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(heatmap_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------------
# 2. Read and prepare data
# -----------------------------
data_for_analysis <- read.csv(data_path, check.names = FALSE)

# Hormone variables for Task 3
hormone_vars <- names(data_for_analysis)[startsWith(names(data_for_analysis), "hormone")]

# Remove the one record with missing outcome for group comparisons
analysis_data <- data_for_analysis %>%
  filter(!is.na(outcome)) %>%
  mutate(outcome = as.factor(outcome))

# Save basic data information
sink(file.path(output_dir, "session_info.txt"))
cat("Dataset dimensions:\n")
print(dim(data_for_analysis))
cat("\nOutcome counts:\n")
print(table(data_for_analysis$outcome, useNA = "ifany"))
cat("\nHormone variables:\n")
print(hormone_vars)
cat("\n\nR session information:\n")
print(sessionInfo())
sink()

# -----------------------------
# 3. Descriptive statistics by group
# -----------------------------
descriptive_statistics <- analysis_data %>%
  select(outcome, all_of(hormone_vars)) %>%
  pivot_longer(cols = all_of(hormone_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable, outcome) %>%
  summarise(
    n = sum(!is.na(value)),
    missing = sum(is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = quantile(value, 0.25, na.rm = TRUE),
    q3 = quantile(value, 0.75, na.rm = TRUE),
    iqr = IQR(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    skewness = moments::skewness(value, na.rm = TRUE),
    kurtosis = moments::kurtosis(value, na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  descriptive_statistics,
  file.path(table_dir, "descriptive_statistics_by_outcome.csv"),
  row.names = FALSE
)

# -----------------------------
# 4. Distribution fit by group
# Candidate distributions: normal, lognormal, gamma
# A small offset is added only for lognormal/gamma fitting when a variable contains 0.
# -----------------------------
safe_fitdist <- function(x, dist_name) {
  tryCatch(
    fitdistrplus::fitdist(x, dist_name),
    error = function(e) NULL
  )
}

fit_distribution_one <- function(x) {
  x <- x[!is.na(x)]
  positive_x <- x
  offset <- 0

  if (any(positive_x <= 0)) {
    positive_min <- min(positive_x[positive_x > 0], na.rm = TRUE)
    offset <- positive_min / 2
    positive_x <- positive_x + offset
  }

  fit_norm <- safe_fitdist(x, "norm")
  fit_lnorm <- safe_fitdist(positive_x, "lnorm")
  fit_gamma <- safe_fitdist(positive_x, "gamma")

  fit_list <- list(normal = fit_norm, lognormal = fit_lnorm, gamma = fit_gamma)
  fit_list <- fit_list[!sapply(fit_list, is.null)]

  aic_table <- tibble(
    distribution = names(fit_list),
    aic = sapply(fit_list, function(fit) fit$aic)
  ) %>%
    arrange(aic)

  best_name <- aic_table$distribution[1]
  best_fit <- fit_list[[best_name]]
  estimates <- best_fit$estimate

  tibble(
    best_distribution = best_name,
    best_aic = aic_table$aic[1],
    parameter_1 = names(estimates)[1],
    estimate_1 = unname(estimates[1]),
    parameter_2 = ifelse(length(estimates) >= 2, names(estimates)[2], NA),
    estimate_2 = ifelse(length(estimates) >= 2, unname(estimates[2]), NA),
    offset_added_for_positive_distributions = offset,
    aic_normal = aic_table$aic[match("normal", aic_table$distribution)],
    aic_lognormal = aic_table$aic[match("lognormal", aic_table$distribution)],
    aic_gamma = aic_table$aic[match("gamma", aic_table$distribution)]
  )
}

distribution_fit <- analysis_data %>%
  select(outcome, all_of(hormone_vars)) %>%
  pivot_longer(cols = all_of(hormone_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable, outcome) %>%
  summarise(fit = list(fit_distribution_one(value)), .groups = "drop") %>%
  unnest(fit)

write.csv(
  distribution_fit,
  file.path(table_dir, "distribution_fit_by_outcome.csv"),
  row.names = FALSE
)

# -----------------------------
# 5. Shapiro-Wilk normality test by group
# -----------------------------
shapiro_results <- analysis_data %>%
  select(outcome, all_of(hormone_vars)) %>%
  pivot_longer(cols = all_of(hormone_vars), names_to = "variable", values_to = "value") %>%
  group_by(variable, outcome) %>%
  summarise(
    n = sum(!is.na(value)),
    shapiro_W = shapiro.test(value)$statistic,
    shapiro_p = shapiro.test(value)$p.value,
    normal_at_alpha_0.05 = shapiro_p >= 0.05,
    .groups = "drop"
  )

write.csv(
  shapiro_results,
  file.path(table_dir, "shapiro_wilk_by_outcome.csv"),
  row.names = FALSE
)

# -----------------------------
# 6. Levene, t.test, wilcox.test, Brunner-Munzel
# -----------------------------
run_tests_one_variable <- function(var_name) {
  temp <- analysis_data %>%
    select(outcome, value = all_of(var_name)) %>%
    filter(!is.na(value))

  x0 <- temp %>% filter(outcome == "0") %>% pull(value)
  x1 <- temp %>% filter(outcome == "1") %>% pull(value)

  shapiro_0 <- shapiro.test(x0)$p.value
  shapiro_1 <- shapiro.test(x1)$p.value
  levene_obj <- car::leveneTest(value ~ outcome, data = temp, center = median)

  student_t <- t.test(value ~ outcome, data = temp, var.equal = TRUE)
  welch_t <- t.test(value ~ outcome, data = temp, var.equal = FALSE)
  wilcox_obj <- wilcox.test(value ~ outcome, data = temp, exact = FALSE)
  bm_obj <- tryCatch(
    brunnermunzel::brunnermunzel.test(x0, x1),
    error = function(e) NULL
  )

  normal_both <- shapiro_0 >= 0.05 & shapiro_1 >= 0.05
  equal_variance <- levene_obj$`Pr(>F)`[1] >= 0.05

  recommended_test <- case_when(
    normal_both & equal_variance ~ "Student t-test",
    normal_both & !equal_variance ~ "Welch t-test",
    !normal_both & equal_variance ~ "Wilcoxon rank-sum / Mann-Whitney U",
    TRUE ~ "Brunner-Munzel"
  )

  recommendation_reason <- case_when(
    normal_both & equal_variance ~ "Both groups passed Shapiro-Wilk normality and Levene's test did not reject equal variances.",
    normal_both & !equal_variance ~ "Both groups passed Shapiro-Wilk normality, but Levene's test rejected equal variances.",
    !normal_both & equal_variance ~ "At least one group failed Shapiro-Wilk normality, while Levene's test did not reject equal variances.",
    TRUE ~ "At least one group failed Shapiro-Wilk normality and Levene's test rejected equal variances."
  )

  tibble(
    variable = var_name,
    n_outcome0 = length(x0),
    n_outcome1 = length(x1),
    shapiro_p_outcome0 = shapiro_0,
    shapiro_p_outcome1 = shapiro_1,
    levene_F = levene_obj$`F value`[1],
    levene_p = levene_obj$`Pr(>F)`[1],
    student_t_stat = unname(student_t$statistic),
    student_t_p = student_t$p.value,
    welch_t_stat = unname(welch_t$statistic),
    welch_t_p = welch_t$p.value,
    wilcox_W_stat = unname(wilcox_obj$statistic),
    wilcox_p = wilcox_obj$p.value,
    brunner_munzel_stat = ifelse(is.null(bm_obj), NA, unname(bm_obj$statistic)),
    brunner_munzel_p = ifelse(is.null(bm_obj), NA, bm_obj$p.value),
    recommended_test = recommended_test,
    recommendation_reason = recommendation_reason
  )
}

statistical_tests <- map_dfr(hormone_vars, run_tests_one_variable)

write.csv(
  statistical_tests,
  file.path(table_dir, "statistical_tests_all_hormones.csv"),
  row.names = FALSE
)

main_result_summary <- statistical_tests %>%
  transmute(
    variable,
    wilcox_p,
    brunner_munzel_p,
    recommended_test,
    wilcox_significant = wilcox_p < 0.05,
    brunner_munzel_significant = brunner_munzel_p < 0.05
  )

write.csv(
  main_result_summary,
  file.path(table_dir, "main_result_summary.csv"),
  row.names = FALSE
)

# -----------------------------
# 7. Histograms and Q-Q plots
# -----------------------------
for (v in hormone_vars) {
  p_hist <- ggplot(analysis_data, aes(x = .data[[v]])) +
    geom_histogram(bins = 30, color = "black", fill = "grey75") +
    facet_wrap(~ outcome, scales = "free_y") +
    labs(
      title = paste("Histogram of", v, "by outcome"),
      x = v,
      y = "Frequency"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(hist_dir, paste0(v, "_histogram_by_outcome.png")),
    plot = p_hist,
    width = 10,
    height = 4,
    dpi = 300
  )

  p_qq <- ggplot(analysis_data, aes(sample = .data[[v]])) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ outcome, scales = "free") +
    labs(
      title = paste("Q-Q plot of", v, "by outcome"),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    theme_minimal()

  ggsave(
    filename = file.path(qq_dir, paste0(v, "_qqplot_by_outcome.png")),
    plot = p_qq,
    width = 10,
    height = 4,
    dpi = 300
  )
}

# -----------------------------
# 8. Correlation heatmaps by group
# Method: Pearson only when all hormone variables in a group pass normality;
# otherwise Spearman.
# -----------------------------
make_correlation_heatmap <- function(group_value) {
  group_shapiro <- shapiro_results %>%
    filter(outcome == group_value)

  method <- ifelse(all(group_shapiro$normal_at_alpha_0.05), "pearson", "spearman")

  group_data <- analysis_data %>%
    filter(outcome == group_value) %>%
    select(all_of(hormone_vars))

  corr_mat <- cor(group_data, use = "pairwise.complete.obs", method = method)

  write.csv(
    corr_mat,
    file.path(table_dir, paste0("correlation_matrix_outcome_", group_value, "_", method, ".csv"))
  )

  corr_long <- as.data.frame(corr_mat) %>%
    rownames_to_column("var1") %>%
    pivot_longer(-var1, names_to = "var2", values_to = "correlation")

  p_heat <- ggplot(corr_long, aes(x = var1, y = var2, fill = correlation)) +
    geom_tile() +
    scale_fill_gradient2(limits = c(-1, 1), low = "blue", mid = "white", high = "red") +
    coord_equal() +
    labs(
      title = paste0("Hormone correlation heatmap: outcome = ", group_value),
      subtitle = paste("Method:", method),
      x = "",
      y = "",
      fill = "Correlation"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = file.path(heatmap_dir, paste0("correlation_heatmap_outcome_", group_value, "_", method, ".png")),
    plot = p_heat,
    width = 8,
    height = 7,
    dpi = 300
  )
}

walk(levels(analysis_data$outcome), make_correlation_heatmap)

# -----------------------------
# 9. Short written conclusion
# -----------------------------
significant_hormones <- main_result_summary %>%
  filter(wilcox_significant) %>%
  pull(variable)

summary_text <- c(
  "# Practice 3 Result Summary",
  "",
  paste0("Dataset rows: ", nrow(data_for_analysis)),
  paste0("Dataset variables: ", ncol(data_for_analysis)),
  paste0("Rows used after removing missing outcome: ", nrow(analysis_data)),
  "",
  "Main conclusion:",
  "All hormone variables are non-normal in at least one group according to the Shapiro-Wilk test at alpha = 0.05.",
  "Levene's test does not reject homogeneity of variance for the hormone variables at alpha = 0.05.",
  "Therefore, the recommended two-independent-group test is Wilcoxon rank-sum / Mann-Whitney U.",
  "Brunner-Munzel is also reported as a robust non-parametric alternative; t-tests are included for comparison.",
  "",
  "Significant hormones according to the recommended Wilcoxon test:",
  ifelse(length(significant_hormones) == 0, "None", paste(significant_hormones, collapse = ", ")),
  "",
  "Correlation method:",
  "Spearman correlation is used for both outcome groups because the hormone variables do not satisfy normality."
)

writeLines(summary_text, file.path(output_dir, "answer_summary.md"))

cat("Analysis complete. Results saved in the outputs folder.\n")
