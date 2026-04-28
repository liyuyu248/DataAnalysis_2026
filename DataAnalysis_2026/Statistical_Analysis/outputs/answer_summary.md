# Practice 3 Result Summary

Dataset: `data_for_analysis.csv`

Rows in raw data: 1148  
Variables in raw data: 31  
Rows used for group comparison after removing missing `outcome`: 1147

Group sizes:

| outcome | n |
|---:|---:|
| 0 | 987 |
| 1 | 160 |

Hormone variables analyzed:

hormone1, hormone2, hormone3, hormone4, hormone5, hormone6, hormone7, hormone8, hormone10_generated

## Main conclusion

All hormone variables failed the Shapiro-Wilk normality test in at least one outcome group, and in this dataset the Shapiro-Wilk p-values were below 0.05 for every hormone in both groups. Levene's test did not reject homogeneity of variance for the hormone variables at α = 0.05.

Therefore, the main applicable two-independent-group test for these data is the **Wilcoxon rank-sum test / Mann-Whitney U test**. The Brunner-Munzel test is also reported as a robust non-parametric alternative. The t-tests are reported for comparison, but they are not the primary choice because the normality assumption is not satisfied.

## Hormones with statistically significant between-group differences using the recommended Wilcoxon test

hormone2, hormone5, hormone8

## Correlation method

Since normality was rejected for the hormone variables in both groups, the correlation heatmaps use **Spearman correlation** for outcome 0 and outcome 1.

See:

- `outputs/tables/descriptive_statistics_by_outcome.csv`
- `outputs/tables/shapiro_wilk_by_outcome.csv`
- `outputs/tables/statistical_tests_all_hormones.csv`
- `outputs/tables/distribution_fit_by_outcome.csv`
- `outputs/tables/correlation_matrix_outcome_0_spearman.csv`
- `outputs/tables/correlation_matrix_outcome_1_spearman.csv`
- `outputs/figures/`
