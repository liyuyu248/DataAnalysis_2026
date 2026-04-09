# Module 02 – Probability Distributions

## Assignment summary
Main task:
1. Estimate the distribution of continuous variables in `data_for_analysis` by `outcome` group (excluding `lipids5`).
2. Create a table with descriptive statistics and specify the parameters of the selected distribution by group.

Extra task:
1. Find and fix the error in the data (`lipids5` missing data).
2. Repeat the distribution analysis by `outcome` groups.
3. Create the descriptive statistics table again after the fix.

## Files in this folder

### Data
- `data/distribution.csv` – example distribution dataset from the practical class
- `data/factor_data.csv` – factor variables
- `data/imputed_data.csv` – imputed biomarker variables from the previous task
- `data/data_for_analysis.csv` – merged dataset created from `factor_data.csv` and `imputed_data.csv`
- `data/data_for_analysis_lipids5_fixed.csv` – optional extra-point version with `lipids5` imputed by group median

### Code
- `code/practice_2_original.R` – original uploaded script
- `code/practice_2_github_ready.R` – cleaned version with relative paths and output export
- `code/practice_2_analysis_notes.txt` – short note about the prepared outputs

### Output
#### Main task
- `output/tables/descriptive_statistics_by_outcome_excluding_lipids5.csv`
- `output/tables/distribution_fits_by_outcome_excluding_lipids5.csv`

#### Extra task
- `output/tables/lipids5_imputation_log.csv`
- `output/tables/descriptive_statistics_by_outcome_with_lipids5_fixed.csv`
- `output/tables/distribution_fits_by_outcome_with_lipids5_fixed.csv`

#### Figures
- `output/figures/histograms_outcome_0_excluding_lipids5.png`
- `output/figures/histograms_outcome_1_excluding_lipids5.png`
- `output/figures/selected_distribution_counts.png`

## Data description
The merged analysis dataset contains factor variables and continuous laboratory variables. The PDF describes it as a dataset with 1148 observations and 31 variables. For grouped analysis by `outcome`, one row with missing `outcome` was excluded from the exported result tables.

## R environment
The uploaded materials do not state the original R version. The cleaned script is prepared for:
- R 4.3+
- MASS

## How to run
Open this module folder as your working directory in R/RStudio and run:

```r
source("code/practice_2_github_ready.R")
```

## Notes on the extra task
The source files clearly show that `lipids5` still has missing values. Since the exact repair rule is not stated in the uploaded files, the extra-point version in this package uses a transparent rule:
- impute missing `lipids5` values with the median within each `outcome` group

If your instructor expects a different imputation rule, update the script before submission.
