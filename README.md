# TernTablesR
**TernTablesR** is a lightweight R package for generating clean summary tables with appropriate statistical tests. It supports two-level and three-level group comparisons for binary, continuous, and ordinal variables, and includes options for exporting tables to Word and Excel.

## 🚀 Installation

You can install the development version of TernTablesR from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTablesR")
```

## 📦 Functions

### `ternG()`

Generates summary tables for either **binary** or **3-level categorical** grouping variables. Automatically applies appropriate statistical tests based on variable type and number of groups:

- **Continuous variables**:
  - **2 groups**: Welch’s *t*-test or Wilcoxon rank-sum (based on Shapiro-Wilk test for normality)
  - **3+ groups**: ANOVA or Kruskal-Wallis, based on both normality (Shapiro-Wilk) and homogeneity of variance (Levene’s test)

- **Categorical variables**:
  - Chi-squared test or Fisher’s exact test (based on expected cell counts)

- **Ordinal variables** (defined via `force_ordinal`):
  - Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups)

## 📝 Examples

### Two-level comparison

```r
library(TernTablesR)
ternG(
  data = your_data,
  group_var = "treatment_group",  # binary variable
  exclude_vars = c("ID"),
  force_ordinal = c("severity_score", "stage"),
  group_order = c("Control", "Treatment"),  # Optional custom order
  output_xlsx = "summary_2v.xlsx",
  output_docx = "summary_2v.docx",
  OR_col = TRUE,  # Adds odds ratios for 2x2 categorical comparisons
  consider_normality = TRUE # Runs normality test to choose test to compare means (default)
)
```

### Three-level comparison

```r
library(TernTablesR)
ternG(
  data = your_data,
  group_var = "grade",  # 3-level variable (e.g., 3, 4, 5)
  exclude_vars = c("ID"),
  force_ordinal = c("ISS", "GCS"),
  group_order = c(3, 4, 5),
  output_xlsx = "summary_3v.xlsx",
  output_docx = "summary_3v.docx"
)
```

---

### `ternD()`

Generates **descriptive-only** summary tables without group comparisons. Useful for baseline cohort description or single-group studies.

- **Continuous variables**: Mean ± SD  
- **Ordinal variables** (defined via `force_ordinal`): Median [IQR]  
- **Categorical variables**: Counts (%)

## 📝 Example

```r
ternD(
  data = your_data,
  exclude_vars = c("ID"),
  force_ordinal = c("severity_score", "stage"),
  output_xlsx = "summary_descriptive.xlsx",
  output_docx = "summary_descriptive.docx"
)
```

---

## 📤 Output

- Returns a tibble with:
  - Variable name  
  - Summary statistics (per group if using `ternG()`, single overall row if using `ternD()`)  
  - p-value and test name (`ternG()` only)
  - Odds ratios with 95% confidence intervals (for Chi squared and Fisher's) (Optional via OR_col argument)
- Optionally exports to `.xlsx` and `.docx` files.


## 📄 License

[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

This project is licensed under the MIT License.

---

**Developed and maintained by Josh Preston and Helen Abadiotakis**  
Feedback and contributions are welcome!
