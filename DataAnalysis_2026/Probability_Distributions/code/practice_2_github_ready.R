# ============================================================
# Practice 2 - GitHub-ready version
# Course: Data Analysis
# Task: Probability distributions by outcome group
# Main task: exclude lipids5
# Extra task: fix missing lipids5 and repeat
# ============================================================

# -------------------- packages ------------------------------
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# -------------------- folders -------------------------------
dir.create("output", showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# -------------------- read data -----------------------------
example_df <- read.csv("data/distribution.csv", header = TRUE, dec = ",", sep = ";")
factor_df  <- read.csv("data/factor_data.csv")
imputed_df <- read.csv("data/imputed_data.csv")

# -------------------- merge data ----------------------------
data_for_analysis <- merge(
  factor_df,
  imputed_df,
  by = "record_id",
  all = FALSE
)

write.csv(data_for_analysis, "data/data_for_analysis.csv", row.names = FALSE)

# -------------------- helper functions ----------------------
safe_fit <- function(x, dist_name) {
  x <- x[!is.na(x)]
  if (length(x) < 5) return(NULL)

  if (dist_name == "normal") {
    return(tryCatch(fitdistr(x, densfun = "normal"), error = function(e) NULL))
  }

  if (dist_name == "lognormal") {
    if (any(x <= 0)) return(NULL)
    return(tryCatch(fitdistr(x, densfun = "lognormal"), error = function(e) NULL))
  }

  if (dist_name == "exponential") {
    if (any(x < 0)) return(NULL)
    return(tryCatch(fitdistr(x, densfun = "exponential"), error = function(e) NULL))
  }

  return(NULL)
}

fit_variable <- function(x) {
  fits <- list(
    normal = safe_fit(x, "normal"),
    lognormal = safe_fit(x, "lognormal"),
    exponential = safe_fit(x, "exponential")
  )

  fits <- fits[!sapply(fits, is.null)]
  if (length(fits) == 0) return(NULL)

  bic_values <- sapply(fits, BIC)
  best_name <- names(bic_values)[which.min(bic_values)]
  best_fit  <- fits[[best_name]]

  param_names <- names(best_fit$estimate)
  param_values <- unname(best_fit$estimate)

  result <- list(
    selected_distribution = best_name,
    selected_bic = unname(min(bic_values)),
    param1_name = ifelse(length(param_names) >= 1, param_names[1], NA),
    param1_value = ifelse(length(param_values) >= 1, param_values[1], NA),
    param2_name = ifelse(length(param_names) >= 2, param_names[2], NA),
    param2_value = ifelse(length(param_values) >= 2, param_values[2], NA)
  )

  fit_table <- data.frame(
    distribution = names(bic_values),
    bic = as.numeric(bic_values)
  )

  return(list(summary = result, fit_table = fit_table))
}

summarize_variable <- function(df, variable_name, group_value) {
  x <- df[df$outcome == group_value, variable_name]
  x_no_na <- x[!is.na(x)]

  model <- fit_variable(x_no_na)
  if (is.null(model)) return(NULL)

  summary_row <- data.frame(
    outcome = group_value,
    variable = variable_name,
    n_non_missing = length(x_no_na),
    n_missing = sum(is.na(x)),
    mean = mean(x_no_na),
    sd = sd(x_no_na),
    median = median(x_no_na),
    p25 = unname(quantile(x_no_na, 0.25)),
    p75 = unname(quantile(x_no_na, 0.75)),
    min = min(x_no_na),
    max = max(x_no_na),
    selected_distribution = model$summary$selected_distribution,
    selected_bic = model$summary$selected_bic,
    param1_name = model$summary$param1_name,
    param1_value = model$summary$param1_value,
    param2_name = model$summary$param2_name,
    param2_value = model$summary$param2_value
  )

  fit_table <- model$fit_table
  fit_table$outcome <- group_value
  fit_table$variable <- variable_name

  return(list(summary_row = summary_row, fit_table = fit_table))
}

run_analysis <- function(df, variable_names, summary_file, fit_file) {
  summary_list <- list()
  fit_list <- list()

  group_values <- sort(unique(df$outcome[!is.na(df$outcome)]))

  for (g in group_values) {
    for (v in variable_names) {
      result <- summarize_variable(df, v, g)
      if (!is.null(result)) {
        summary_list[[length(summary_list) + 1]] <- result$summary_row
        fit_list[[length(fit_list) + 1]] <- result$fit_table
      }
    }
  }

  summary_table <- do.call(rbind, summary_list)
  fit_table_all <- do.call(rbind, fit_list)

  write.csv(summary_table, summary_file, row.names = FALSE)
  write.csv(fit_table_all, fit_file, row.names = FALSE)

  return(list(summary_table = summary_table, fit_table = fit_table_all))
}

plot_histograms <- function(df, variable_names, group_value, output_file) {
  png(output_file, width = 1800, height = 2200, res = 160)
  par(mfrow = c(6, 4), mar = c(3, 3, 3, 1), oma = c(0, 0, 3, 0))

  for (v in variable_names) {
    x <- df[df$outcome == group_value, v]
    hist(x[!is.na(x)], main = v, xlab = "", col = "grey80", border = "white")
  }

  mtext(paste("Histograms by outcome =", group_value, "(excluding lipids5)"), outer = TRUE, cex = 1.4)
  dev.off()
}

# -------------------- main task -----------------------------
analysis_df <- data_for_analysis[!is.na(data_for_analysis$outcome), ]

factor_columns <- c("record_id", "outcome", "factor_eth", "factor_h", "factor_pcos", "factor_prl")
continuous_variables_main <- setdiff(names(analysis_df), c(factor_columns, "lipids5"))

main_results <- run_analysis(
  df = analysis_df,
  variable_names = continuous_variables_main,
  summary_file = "output/tables/descriptive_statistics_by_outcome_excluding_lipids5.csv",
  fit_file = "output/tables/distribution_fits_by_outcome_excluding_lipids5.csv"
)

plot_histograms(analysis_df, continuous_variables_main, 0, "output/figures/histograms_outcome_0_excluding_lipids5.png")
plot_histograms(analysis_df, continuous_variables_main, 1, "output/figures/histograms_outcome_1_excluding_lipids5.png")

# -------------------- extra task ----------------------------
extra_df <- analysis_df

imputation_log <- data.frame(
  outcome = integer(),
  imputation_method = character(),
  imputed_value = numeric(),
  n_imputed = integer()
)

for (g in sort(unique(extra_df$outcome))) {
  group_median <- median(extra_df[extra_df$outcome == g, "lipids5"], na.rm = TRUE)
  missing_index <- which(extra_df$outcome == g & is.na(extra_df$lipids5))

  if (length(missing_index) > 0) {
    extra_df$lipids5[missing_index] <- group_median
  }

  imputation_log <- rbind(
    imputation_log,
    data.frame(
      outcome = g,
      imputation_method = "median within outcome group",
      imputed_value = group_median,
      n_imputed = length(missing_index)
    )
  )
}

write.csv(extra_df, "data/data_for_analysis_lipids5_fixed.csv", row.names = FALSE)
write.csv(imputation_log, "output/tables/lipids5_imputation_log.csv", row.names = FALSE)

continuous_variables_extra <- setdiff(names(extra_df), factor_columns)

extra_results <- run_analysis(
  df = extra_df,
  variable_names = continuous_variables_extra,
  summary_file = "output/tables/descriptive_statistics_by_outcome_with_lipids5_fixed.csv",
  fit_file = "output/tables/distribution_fits_by_outcome_with_lipids5_fixed.csv"
)

cat("Done. Files were written to output/tables and output/figures.\n")
