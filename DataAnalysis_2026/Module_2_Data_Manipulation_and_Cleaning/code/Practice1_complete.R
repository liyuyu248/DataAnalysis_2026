# Practice 1 - Data Manipulation and Cleaning
# Full reproducible script for the uploaded dataset

# ===================== 0. Packages =====================
required_packages <- c(
  "dplyr", "skimr", "visdat", "naniar", "mice",
  "ggplot2", "tidyr", "dbscan"
)

to_install <- required_packages[!required_packages %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(skimr)
library(visdat)
library(naniar)
library(mice)
library(ggplot2)
library(tidyr)
library(dbscan)

# For Little's MCAR test
if (getRversion() < "4.0.0") {
  if (!"BaylorEdPsych" %in% installed.packages()[, "Package"]) install.packages("BaylorEdPsych")
  library(BaylorEdPsych)
}

# ===================== 1. Read data =====================
# Put the CSV in the same project folder or edit the path below
data_path <- "DataSet_No_Details.csv"
df <- read.csv(data_path)

str(df)
skim(df)

# ===================== 2. Data preparation =====================
cols_to_remove <- c(
  "h_index_34", "h_index_56", "hormone10_1", "hormone10_2",
  "an_index_23", "outcome", "factor_eth", "factor_h",
  "factor_pcos", "factor_prl"
)

MD_df <- df %>% select(-any_of(cols_to_remove))
factor_df <- df %>% select(record_id, outcome, factor_eth, factor_h, factor_pcos, factor_prl)

str(MD_df)
summary(factor_df)

# Missing data summary
sum(is.na(MD_df))
na_counts <- colSums(is.na(MD_df))
na_stats <- colMeans(is.na(MD_df)) * 100

na_summary <- data.frame(
  Column = names(na_counts),
  NA_Count = as.numeric(na_counts),
  NA_Percent = as.numeric(na_stats)
)

write.csv(na_summary, "na_summary_MD_df.csv", row.names = FALSE)

# Keep columns with <= 35% missing
na_stats_filtered <- na_stats[na_stats <= 35]
na_stats_removed  <- na_stats[na_stats > 35]

write.csv(
  data.frame(Column = names(na_stats_filtered), NA_Percent = as.numeric(na_stats_filtered)),
  "na_columns_kept.csv", row.names = FALSE
)

write.csv(
  data.frame(Column = names(na_stats_removed), NA_Percent = as.numeric(na_stats_removed)),
  "na_columns_removed.csv", row.names = FALSE
)

# Visualize missingness
png("vis_miss_MD_df.png", width = 1200, height = 800)
vis_miss(MD_df)
dev.off()

png("gg_miss_var_MD_df.png", width = 1200, height = 800)
gg_miss_var(MD_df)
dev.off()

# ===================== 3. Create handle_MD_df =====================
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13", "hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))
str(handle_MD_df)

# ===================== 4. Little's MCAR test =====================
# H0: data is MCAR
# H1: data is not MCAR

if (getRversion() < "4.0.0") {
  mcar_test <- LittleMCAR(handle_MD_df)
  print(mcar_test)
  mcar_result <- data.frame(
    chi_square = mcar_test$chi.square,
    df = mcar_test$df,
    p_value = mcar_test$p.value,
    missing_patterns = mcar_test$missing.patterns
  )
} else {
  # For R 4.0+, the simplest reproducible option is still BaylorEdPsych if available
  if (!"BaylorEdPsych" %in% installed.packages()[, "Package"]) install.packages("BaylorEdPsych")
  library(BaylorEdPsych)
  mcar_test <- LittleMCAR(handle_MD_df)
  print(mcar_test)
  mcar_result <- data.frame(
    chi_square = mcar_test$chi.square,
    df = mcar_test$df,
    p_value = mcar_test$p.value,
    missing_patterns = mcar_test$missing.patterns
  )
}

write.csv(mcar_result, "little_mcar_result.csv", row.names = FALSE)

# ===================== 5. Imputation: PMM vs RF =====================
set.seed(42)

# PMM
imp_pmm <- mice(handle_MD_df, m = 5, method = "pmm", printFlag = FALSE, seed = 42)
imputed_handle_MD_df_final_pmm <- complete(imp_pmm, 1)

# Random forest
imp_rf <- mice(handle_MD_df, m = 5, method = "rf", printFlag = FALSE, seed = 42)
imputed_handle_MD_df_final_rf <- complete(imp_rf, 1)

write.csv(imputed_handle_MD_df_final_pmm, "imputed_handle_MD_df_final_pmm.csv", row.names = FALSE)
write.csv(imputed_handle_MD_df_final_rf,  "imputed_handle_MD_df_final_rf.csv",  row.names = FALSE)

# Compare distributions for hormone10_generated
comparison_df <- bind_rows(
  data.frame(dataset = "Original observed", value = handle_MD_df$hormone10_generated[!is.na(handle_MD_df$hormone10_generated)]),
  data.frame(dataset = "PMM imputed complete", value = imputed_handle_MD_df_final_pmm$hormone10_generated),
  data.frame(dataset = "RF imputed complete", value = imputed_handle_MD_df_final_rf$hormone10_generated)
)

png("hormone10_density_compare.png", width = 1200, height = 800)
ggplot(comparison_df, aes(x = value, fill = dataset)) +
  geom_density(alpha = 0.35) +
  coord_cartesian(xlim = c(0, 20)) +
  labs(
    title = "Original vs PMM vs RF for hormone10_generated",
    x = "hormone10_generated",
    y = "Density"
  ) +
  theme_minimal()
dev.off()

imputation_summary <- data.frame(
  dataset = c("Original observed", "PMM imputed complete", "RF imputed complete"),
  mean = c(
    mean(handle_MD_df$hormone10_generated, na.rm = TRUE),
    mean(imputed_handle_MD_df_final_pmm$hormone10_generated, na.rm = TRUE),
    mean(imputed_handle_MD_df_final_rf$hormone10_generated, na.rm = TRUE)
  ),
  sd = c(
    sd(handle_MD_df$hormone10_generated, na.rm = TRUE),
    sd(imputed_handle_MD_df_final_pmm$hormone10_generated, na.rm = TRUE),
    sd(imputed_handle_MD_df_final_rf$hormone10_generated, na.rm = TRUE)
  ),
  min = c(
    min(handle_MD_df$hormone10_generated, na.rm = TRUE),
    min(imputed_handle_MD_df_final_pmm$hormone10_generated, na.rm = TRUE),
    min(imputed_handle_MD_df_final_rf$hormone10_generated, na.rm = TRUE)
  ),
  max = c(
    max(handle_MD_df$hormone10_generated, na.rm = TRUE),
    max(imputed_handle_MD_df_final_pmm$hormone10_generated, na.rm = TRUE),
    max(imputed_handle_MD_df_final_rf$hormone10_generated, na.rm = TRUE)
  )
)

write.csv(imputation_summary, "imputation_comparison_hormone10_generated.csv", row.names = FALSE)

# ===================== 6. Choose final dataset =====================
# PMM is usually preferable for continuous biomedical variables because it preserves plausible observed values.
imputed_handle_MD_df_final <- imputed_handle_MD_df_final_pmm
write.csv(imputed_handle_MD_df_final, "imputed_handle_MD_df_final.csv", row.names = FALSE)

# ===================== 7. Outlier detection with LOF =====================
lof_input <- imputed_handle_MD_df_final %>%
  select(-record_id) %>%
  mutate(across(everything(), as.numeric)) %>%
  scale()

lof_model <- lof(lof_input, minPts = 20)
lof_scores <- as.numeric(lof_model)

lof_results <- data.frame(
  record_id = imputed_handle_MD_df_final$record_id,
  LOF_score = lof_scores
) %>%
  arrange(desc(LOF_score))

# Common screening rule: LOF > 1.5
lof_results$is_outlier <- lof_results$LOF_score > 1.5

write.csv(lof_results, "lof_results.csv", row.names = FALSE)

png("lof_histogram.png", width = 1200, height = 800)
hist(lof_scores, breaks = 40, main = "Histogram of LOF scores", xlab = "LOF score")
dev.off()

# Bivariate scatterplot using lipids2 vs lipids4
plot_df <- imputed_handle_MD_df_final %>%
  select(record_id, lipids2, lipids4) %>%
  left_join(lof_results %>% select(record_id, is_outlier), by = "record_id")

png("lof_scatter_lipids2_lipids4.png", width = 1200, height = 800)
ggplot(plot_df, aes(x = lipids2, y = lipids4, color = is_outlier)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "LOF outliers: lipids2 vs lipids4",
    x = "lipids2",
    y = "lipids4"
  ) +
  theme_minimal()
dev.off()

# ===================== 8. Final conclusion =====================
# - If p-value <= 0.05, missingness is not MCAR
# - PMM is safer for continuous data if RF creates overly extreme imputations
# - Review LOF outliers before downstream modeling
