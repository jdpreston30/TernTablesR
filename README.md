# TernTablesR
**TernTablesR** is a lightweight R package for generating clean summary tables with appropriate statistical tests. It supports two-level and three-level group comparisons for binary, continuous, and ordinal variables, and includes options for exporting tables to Word and Excel.

## 🚀 Installation

You can install the development version of TernTablesR from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTablesR")
```

## 📦 Functions

### `Tern2v()`
Generates summary tables for a **binary grouping variable**. Applies:
- **Welch’s t-test** or **Wilcoxon rank-sum test** for continuous variables (based on Shapiro-Wilk normality test).
- **Chi-squared** or **Fisher’s exact test** for categorical variables.
- **Wilcoxon test** for ordinal variables explicitly specified.

### `Tern3v()`
Generates summary tables for a **3-level grouping variable**. Applies:
- **ANOVA** or **Kruskal-Wallis** for continuous and ordinal variables.
- **Chi-squared** or **Fisher’s exact test** for categorical variables.

## 📝 Examples

### Two-level comparison

```r
Tern2v(
  data = your_data,
  group_var = "treatment_group",  # binary variable
  exclude_vars = c("ID"),
  force_ordinal = c("severity_score", "stage"),
  output_xlsx = "summary_2v.xlsx",
  output_docx = "summary_2v.docx"
)
```

### Three-level comparison

```r
Tern3v(
  data = your_data,
  group_var = "grade",  # 3-level variable (e.g., 3, 4, 5)
  exclude_vars = c("ID"),
  force_ordinal = c("ISS", "GCS"),
  group_order = c(3, 4, 5),
  output_xlsx = "summary_3v.xlsx",
  output_docx = "summary_3v.docx"
)
```

## 📤 Output

- Returns a tibble with:
  - Variable name
  - Group-wise summary statistics (n (%), mean ± SD, median [IQR], etc.)
  - p-value
  - Statistical test used
- Optionally exports to `.xlsx` and `.docx`

## 📄 License

MIT License

---

**Developed and maintained by Josh Preston and Helen Abadiotakis.**  
Feedback and contributions are welcome!
