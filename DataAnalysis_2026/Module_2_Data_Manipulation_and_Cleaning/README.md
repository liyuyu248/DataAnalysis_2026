# Practice 1 — Data Manipulation and Cleaning

## Assignment
This repository contains the results for **Practical class 1: Missing Data and Outliers**.  
According to the assignment slides, the required tasks are:
1. Create `handle_MD_df`
2. Perform **Little's MCAR test**
3. Impute missing values using **PMM** and **Random Forest**
4. Compare imputed vs original distributions
5. Detect outliers with **Local Outlier Factor (LOF)** and visualize the results.  

## Dataset
- **Name:** Hormonal and Biochemical Profile in Patients
- **Format:** CSV
- **Rows × Columns:** 1148 × 41
- **Source:** Internal company database
- **Practical use:** Missing data analysis and outlier detection

## Environment
- **Language:** R
- **Recommended version:** R 4.0+
- **Main packages:** dplyr, skimr, visdat, naniar, mice, ggplot2, tidyr, dbscan, BaylorEdPsych (for R 3.6.x)

## Data preparation
Following the provided script, the following columns were removed to build `MD_df`:
`h_index_34`, `h_index_56`, `hormone10_1`, `hormone10_2`, `an_index_23`, `outcome`, `factor_eth`, `factor_h`, `factor_pcos`, `factor_prl`

Then the following high-missing columns were removed to create `handle_MD_df`:
`hormone9`, `hormone11`, `hormone12`, `hormone13`, `hormone14`

## Key results
- **Little's MCAR test:** chi-square = 1809.585, df = 1102, p-value < 0.001  
  **Conclusion:** reject H0, so the missing data is **not MCAR**.
- For `hormone10_generated`, PMM produced more conservative imputations than RF:
  - PMM imputed-only mean = 0.510, max = 2.971
  - RF imputed-only mean = 1.047, max = 18.139
- **Chosen final dataset:** PMM-imputed dataset (better for continuous variables and produced more plausible values).
- **LOF outliers:** 58 rows flagged (~5.1% of the dataset).

## Files included
- `DataSet_No_Details.csv` — source data
- `Practice1_complete.R` — full reproducible R script
- `na_summary_MD_df.csv` — missing-value summary
- `little_mcar_result.csv` — MCAR test result
- `imputation_comparison_hormone10_generated.csv` — original vs PMM vs RF comparison
- `imputed_only_comparison_hormone10_generated.csv` — imputed-row comparison
- `lof_top_outliers.csv` — largest LOF scores
- `hormone10_density_compare.png` — density comparison plot
- `lof_histogram.png` — LOF score histogram
- `lof_scatter_pca.png` — scatterplot of detected outliers

## Conclusion
The missingness pattern is **systematic rather than completely random**.  
Because the data is not MCAR and the variables are continuous, **PMM** is the safer choice for the final imputed dataset.  
The LOF analysis identified a small subset of multivariate outliers that should be reviewed before further modeling.
