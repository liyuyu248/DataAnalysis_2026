# DataAnalysis_2026

Course assignment repository for **Data Analysis**.

> Replace `YOUR_REAL_NAME`, `YOUR_GITHUB_USERNAME`, and any student/course information below before uploading if needed.

## Author

- Name: `YOUR_REAL_NAME`
- GitHub account: `YOUR_GITHUB_USERNAME`
- Repository: `YOUR_GITHUB_USERNAME/DataAnalysis_2026`

## Module

### Practice 3: Statistical Analysis

This folder contains the completed homework for Practical Class 3: Statistical Analysis.

The task uses the dataset `data_for_analysis.csv` and performs:

1. Descriptive statistics by `outcome` group for all hormone variables.
2. Distribution fitting and parameter estimation by group.
3. Shapiro-Wilk normality tests and Levene's homogeneity of variance tests.
4. Brunner-Munzel test, `t.test`, and `wilcox.test` for two independent groups, repeated for all hormone variables.
5. Correlation heatmaps for hormone variables separately in the two outcome groups, using Pearson or Spearman correlation depending on normality.

## Dataset

- File: `Practice_3_Statistical_Analysis/data/data_for_analysis.csv`
- Source: internal company database, as specified in the course materials.
- Raw size: 1,148 observations and 31 variables.
- Outcome groups used in analysis:
  - `outcome = 0`: 987 observations
  - `outcome = 1`: 160 observations
  - 1 row with missing `outcome` is removed from group comparisons.
- Hormone variables analyzed:
  - `hormone1`
  - `hormone2`
  - `hormone3`
  - `hormone4`
  - `hormone5`
  - `hormone6`
  - `hormone7`
  - `hormone8`
  - `hormone10_generated`

## Repository structure

```text
DataAnalysis_2026/
└── Practice_3_Statistical_Analysis/
    ├── README.md
    ├── data/
    │   └── data_for_analysis.csv
    ├── scripts/
    │   └── 01_practice3_analysis.R
    └── outputs/
        ├── answer_summary.md
        ├── session_info.txt
        ├── tables/
        │   ├── descriptive_statistics_by_outcome.csv
        │   ├── distribution_fit_by_outcome.csv
        │   ├── shapiro_wilk_by_outcome.csv
        │   ├── statistical_tests_all_hormones.csv
        │   ├── main_result_summary.csv
        │   ├── correlation_matrix_outcome_0_spearman.csv
        │   └── correlation_matrix_outcome_1_spearman.csv
        └── figures/
            ├── histograms/
            ├── qqplots/
            └── heatmaps/
```

## R version and packages

The analysis script is written for R 4.x. The script automatically installs missing packages if needed.

Main packages:

- `tidyverse`
- `car`
- `brunnermunzel`
- `fitdistrplus`
- `broom`
- `moments`
- `ggplot2`
- `scales`

The exact R session information is saved to:

```text
Practice_3_Statistical_Analysis/outputs/session_info.txt
```

## How to reproduce the analysis

Open R or RStudio from the repository root and run:

```r
source("Practice_3_Statistical_Analysis/scripts/01_practice3_analysis.R")
```

The script will read:

```text
Practice_3_Statistical_Analysis/data/data_for_analysis.csv
```

and create/update all result tables and figures in:

```text
Practice_3_Statistical_Analysis/outputs/
```

## Main conclusion

After removing the one row with missing `outcome`, the comparison used 1,147 observations.

The Shapiro-Wilk test rejected normality for all hormone variables in both outcome groups at `alpha = 0.05`. Levene's test did not reject homogeneity of variance for the hormone variables at `alpha = 0.05`.

Therefore, the most applicable main test for comparing the two independent groups is:

**Wilcoxon rank-sum test / Mann-Whitney U test**

The Brunner-Munzel test is also reported as a robust non-parametric alternative. The t-tests are included for comparison, but they are not the primary interpretation because the normality assumption is not satisfied.

Using the recommended Wilcoxon test, the hormone variables with statistically significant differences between outcome groups are:

- `hormone2`
- `hormone5`
- `hormone8`

Because normality was rejected for the hormone variables, the correlation heatmaps use **Spearman correlation** in both outcome groups.

## Files to check before submission

Before submitting the GitHub link, check that these files are visible in the public repository:

- `Practice_3_Statistical_Analysis/README.md`
- `Practice_3_Statistical_Analysis/data/data_for_analysis.csv`
- `Practice_3_Statistical_Analysis/scripts/01_practice3_analysis.R`
- `Practice_3_Statistical_Analysis/outputs/tables/statistical_tests_all_hormones.csv`
- `Practice_3_Statistical_Analysis/outputs/figures/heatmaps/correlation_heatmap_outcome_0_spearman.png`
- `Practice_3_Statistical_Analysis/outputs/figures/heatmaps/correlation_heatmap_outcome_1_spearman.png`
