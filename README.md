# TernTables

[![CRAN status](https://www.r-pkg.org/badges/version/TernTables)](https://cran.r-project.org/package=TernTables)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/TernTables)](https://cran.r-project.org/package=TernTables)
[![CRAN total downloads](https://cranlogs.r-pkg.org/badges/grand-total/TernTables)](https://cran.r-project.org/package=TernTables)
[![Web App](https://img.shields.io/badge/Web%20App-tern--tables.com-blue?logo=data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHZpZXdCb3g9IjAgMCAyNCAyNCI+PHBhdGggZmlsbD0id2hpdGUiIGQ9Ik0xMiAyQzYuNDggMiAyIDYuNDggMiAxMnM0LjQ4IDEwIDEwIDEwIDEwLTQuNDggMTAtMTBTMTcuNTIgMiAxMiAyem0tMSAxNy45M1Y0LjA3YzMuOTUuNDkgNyAzLjg1IDcgNy45M3MtMy4wNSA3LjQ0LTcgNy45M3oiLz48L3N2Zz4=)](https://tern-tables.com/)
[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/jdpreston30/TernTables/blob/main/LICENSE)
[![DOI](https://img.shields.io/badge/DOI-10.64898%2F2026.04.15.717241-blue)](https://doi.org/10.64898/2026.04.15.717241)

> **If you use TernTables, please cite:**
> Preston JD, Abadiotakis H, Tang A, Rust CJ, Halkos ME, Daneshmand MA, Chan JL (2026). *TernTables: A Statistical Analysis and Table Generation Web Interface for Clinical and Biomedical Research.* bioRxiv 2026.04.15.717241. <https://doi.org/10.64898/2026.04.15.717241>

Clinical researchers spend a disproportionate amount of time on the mechanics of generating summary tables: choosing the right test, formatting *P* values, cleaning variable names, writing the methods paragraph, and wrestling with Word. For descriptive statistics and standard univariate comparisons, this work is largely procedural — it follows well-established rules — yet it consistently becomes a bottleneck that slows down manuscript preparation.

**TernTables** handles that procedural layer automatically. Variable type detection, statistical test selection, *P* value formatting, Word document export, and methods text generation are all handled in a single function call. The output tables are ready to paste directly into a submission.

The time savings come entirely from automating the administrative work (formatting, test selection bookkeeping, and methods writing), not from bypassing statistical rigour. Every decision follows established criteria: normality is assessed with the Shapiro-Wilk test applied per group; the Fisher's exact / Chi-squared switch follows the Cochran (1954) expected-cell criterion; odds ratios are unadjusted, with the first factor level of the grouping variable as the reference; and the auto-generated methods paragraph covers the statistical approach used, ready to serve as a starting draft for a manuscript's statistical methods section. The output is appropriate for submission to peer-reviewed clinical journals.

For analyses that go beyond descriptive statistics and standard group comparisons, a biostatistician remains the right resource. TernTables is designed to free up that time for the work that actually requires it.

Raw data from CSV or XLSX files can be standardized with `ternP()` before analysis — string NA values, whitespace, blank rows, capitalization inconsistencies, and PHI column names are all handled automatically with full preprocessing feedback.

Descriptive summaries (Table 1), two-group comparisons (with optional odds
ratios), and three-group comparisons are all supported, for continuous,
binary, and categorical variables. Numeric variables can be designated as
ordinal via `force_ordinal`, bypassing normality testing in favour of
median/IQR and nonparametric tests.

---

## Try It Without Writing Code

TernTables is available as a **free, point-and-click web application** at **[tern-tables.com](https://tern-tables.com/)** — no R installation required. Upload a CSV or XLSX file, configure your analysis through a simple interface, and download a publication-ready Word table in minutes.

The web application is powered directly by this R package. The statistical methods, test selection logic (including ROBUST normality routing), and Word output format are identical to calling `ternG()`, `ternD()`, and `ternP()` in R.

The web app is transparent by design: a built-in side panel displays the exact R commands being executed in the background as you work, and the full script can be downloaded at the end of your session. Every analysis produced on the web app is therefore fully auditable and reproducible — the downloaded script runs as-is in R and produces identical output. This makes it suitable for sharing with statistical reviewers, co-authors, or IRB documentation, and serves as a practical entry point for researchers who want to transition to scripted R workflows.

For batch processing, custom formatting, or programmatic integration with other analyses, the R package (this repository) is the canonical reference.

---

## Installation

Install the released version from CRAN:

```r
install.packages("TernTables")
```

Or install the development version directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("jdpreston30/TernTables", build_vignettes = TRUE)
```

A full walkthrough is available in the package vignette (use `build_vignettes = TRUE` with the GitHub install; built automatically when installing from CRAN):

```r
vignette("getting-started", package = "TernTables")
```

## Example Data

The bundled `tern_colon` dataset is derived from `survival::colon`
([Moertel et al., 1990](https://doi.org/10.1056/NEJM199002083220602)). It is a reformatted subset
restricted to the recurrence endpoint (`etype == 1`), providing one row per
patient (n = 929) with clinically labelled factor levels and descriptive column
names. See `?tern_colon` and `data-raw/tern_colon.R` for the full
pre-processing pipeline.

```r
library(TernTables)
data(tern_colon)
```

## Functions

### `ternP()` — Preprocess raw data

Cleans a raw CSV or XLSX file before passing it to `ternG()` or `ternD()`. Converts string NA values (`"NA"`, `"na"`, `"Na"`, `"unk"`), trims whitespace, drops 100% empty columns, removes blank rows, and normalizes capitalization inconsistencies. Hard stops with a descriptive error if any column name matches a PHI pattern (e.g. `MRN`, `DOB`, `FirstName`) or if any unnamed column contains data.

```r
raw    <- readr::read_csv("my_data.csv", show_col_types = FALSE)
result <- ternP(raw)

# Prints a cleaning summary automatically:
# ✔ No transformations required. Data passed through unchanged.
# ─────────────────────────────────────────────────────────────
# ℹ Cleaned data: 929 rows × 14 columns.

result$clean_data    # analysis-ready tibble → pass to ternG() or ternD()
result$sparse_rows   # rows with >50% missing, retained but flagged
result$feedback      # named list of what changed (NULL = no action)
```

### `write_cleaning_doc()` — Data cleaning audit document

Writes a Word document recording every transformation applied by `ternP()`. Only paragraphs for triggered transformations are included; if the data was already clean, a single sentence stating that is written instead. Suitable for data management logs, IRB documentation, or supplemental materials.

```r
write_cleaning_doc(result, filename = "cleaning_summary.docx")
```

### `ternD()` — Descriptive summary table

Generates a single-column summary of all variables without group comparisons.

```r
tbl_descriptive <- ternD(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  output_docx        = "descriptive.docx"
)
```

- Continuous variables: mean ± SD or median [IQR] based on Shapiro-Wilk test
- Binary / categorical: n (%) with optional hierarchical sub-rows
- Use `force_ordinal` to treat specific numeric variables as ordinal

### `ternG()` — Grouped comparison table

> **Independence assumption:** all tests applied by `ternG()` assume each row represents a distinct, unrelated subject. TernTables is not designed for repeated-measures, longitudinal, or clustered data (e.g. pre/post measurements, matched pairs, or patients nested within sites), where the independence assumption is violated and *P* values would be invalid.

Use `ternG()` to compare variables between two or more groups. Set `OR_col = TRUE` to add unadjusted odds ratios with 95% CI for any two-level variable in two-group comparisons — including binary variables (Y/N, 0/1) and two-level categoricals such as Male/Female or Present/Absent. The reference level (factor level 1, or alphabetical first for non-factors) shows `1.00 (ref.)`; the non-reference level shows the computed OR with 95% CI. Fisher's exact or Wald method is chosen automatically based on expected cell counts (Cochran criterion). Unadjusted odds ratios are not available for 3+ groups.

Set `p_adjust = TRUE` to apply Benjamini-Hochberg (BH) false discovery rate (FDR) correction to all omnibus *P* values after testing (Benjamini & Hochberg, 1995). The correction pool is one *P* value per variable; post-hoc pairwise *P* values are excluded. Use `p_adjust_display = "fdr_only"` (default) to show only FDR-corrected values — the *P* value column is renamed to `"P value (FDR corrected)"`. Use `p_adjust_display = "both"` to show original and corrected values side by side. The auto-generated methods paragraph is updated automatically when `p_adjust = TRUE`.

**Two-group comparison:**

```r
tbl_2group <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  group_var          = "Recurrence",
  consider_normality = TRUE,
  OR_col             = TRUE,
  output_docx        = "two_group.docx"
)
```

**Three-group comparison with post-hoc testing:**

```r
tbl_3group <- ternG(
  data               = tern_colon,
  exclude_vars       = c("ID"),
  group_var          = "Treatment_Arm",
  group_order        = c("Observation", "Levamisole", "Levamisole + 5FU"),
  consider_normality = TRUE,
  post_hoc           = TRUE,
  output_docx        = "three_group.docx"
)
```

**Two-group comparison with BH FDR correction:**

```r
tbl_fdr <- ternG(
  data             = tern_colon,
  exclude_vars     = c("ID"),
  group_var        = "Recurrence",
  p_adjust         = TRUE,
  p_adjust_display = "fdr_only"   # or "both" to show raw + corrected
)
```

Omnibus *P* values are reported for 3+ group comparisons. When `post_hoc = TRUE` and the omnibus *P* < 0.05, pairwise post-hoc tests are run automatically for continuous and ordinal variables: Games-Howell following Welch ANOVA, and Dunn's test with Holm correction following Kruskal-Wallis. Results appear as compact letter display (CLD) superscripts appended to each cell value — groups sharing a letter are not significantly different. Categorical variables never receive post-hoc testing. Unadjusted odds ratios are not available for 3+ groups.

Statistical tests applied automatically:

| Variable type | 2 groups | 3+ groups | Post-hoc (3+ groups, `post_hoc = TRUE`, omnibus *p* < 0.05) |
|---|---|---|---|
| Binary / Categorical | Fisher's exact or Chi-squared | Fisher's exact or Chi-squared | — |
| Numeric, normal | Welch's *t*-test | Welch ANOVA | Games-Howell |
| Numeric, non-normal† | Wilcoxon rank-sum | Kruskal-Wallis | Dunn's + Holm |

†Includes variables designated as ordinal via `force_ordinal`, which bypass normality testing and always use non-parametric methods. Variables overridden to parametric via `force_normal` always appear in the normal row.

Fisher's exact is used when any expected cell count is < 5 (Cochran criterion). If the exact algorithm cannot complete (workspace limit exceeded for large tables), Fisher's exact with Monte Carlo simulation (B = 10,000; seed fixed via `getOption("TernTables.seed")`, default 42) is used automatically.

Normality routing uses `consider_normality = "ROBUST"` (default) — a four-gate
decision: (1) any group n < 3 → non-parametric (conservative fail-safe);
(2) absolute skewness > 2 or excess kurtosis > 7 in any group → non-parametric;
(3) all groups
n ≥ 30 → parametric via the Central Limit Theorem; (4) otherwise Shapiro-Wilk
p > 0.05 in all groups → parametric.
Set `consider_normality = TRUE` to use Shapiro-Wilk alone (original behaviour).

| Gate | Condition checked | Routes to | Note |
|---|---|---|---|
| 1 | Any group n < 3 | Non-parametric | Conservative fail-safe; insufficient data to assess |
| 2 | \|skewness\| > 2 **or** \|excess kurtosis\| > 7 in any group | Non-parametric | Distribution shape precludes parametric assumptions regardless of n |
| 3 | All groups n ≥ 30 | Parametric | Central Limit Theorem |
| 4 | Shapiro-Wilk p > 0.05 in **all** groups | Parametric (pass) / Non-parametric (fail) | Valid only when 3 ≤ n ≤ 5,000; n outside this range routes non-parametric |

> **Note:** This routing algorithm is a pragmatic heuristic for automated clinical reporting, not a formal distributional inference. Individual variables can be overridden to non-parametric via `force_ordinal`, to parametric via `force_normal`, or globally via `consider_normality`.

BH FDR correction (Benjamini & Hochberg, 1995) is available via `p_adjust = TRUE`.

**Selected additional `ternG()` and `ternD()` parameters:**

| Parameter | What it does |
|---|---|
| `percentage_compute = "row"` | Row percentages — each level's % distributed across groups. Total column auto-suppressed. |
| `show_p = FALSE` | Suppress *P* value, OR, test, and normality columns (descriptive-only grouped table). |
| `show_missing = TRUE` | Append `Missing: n (%)` sub-rows beneath each variable. |
| `zero_to_dash = TRUE` | Replace `"0 (0%)"` and `"0 (NaN%)"` cells with `"-"`. |
| `font_family` | Word output font. Default: `getOption("TernTables.font_family", "Arial")`. Set once with `options(TernTables.font_family = "Times New Roman")`. |
| `plain_header = TRUE` | White first-column header (no dark background). |
| `force_normal` | Character vector: variables always routed to parametric (mean ± SD, Welch tests). |
| `force_continuous` | Character vector: `{0, 1}` columns always treated as continuous, not binary. |
| `round_decimal` | Integer decimal places for continuous summaries (default 1). |
| `variable_footnote` | Named character vector assigning `*`, `†`, `‡`… superscripts to named variables, with definitions appended below the table. |

### `word_export()` — Format and export to Word

Formats any TernTables result tibble as a flextable and writes to `.docx`.
Use `category_start` to insert bold section-header rows between variable groups.

```r
word_export(
  tbl      = tbl_2group,
  filename = "two_group.docx",
  category_start = c(
    "Patient Demographics"  = "Age (yr)",
    "Surgical Findings"     = "Colonic Obstruction",
    "Tumor Characteristics" = "Positive Lymph Nodes (n)"
  )
)
```

### `write_methods_doc()` — Auto-generated methods paragraph

Inspects the results tibble and writes a Word document containing a standard
statistical methods paragraph covering the normality assessment approach,
test types used, significance threshold, and (when odds ratios are reported)
the estimation method and reference group. Suitable as a starting draft for
a manuscript methods section.

```r
write_methods_doc(
  tbl      = tbl_2group,
  filename = "methods.docx"
)
```

### `ternStyle()` — Format a custom tibble in TernTables Word style

Applies full TernTables Word formatting to any user-supplied tibble — useful for manually assembled tables or custom summaries that need to match the style of `ternG()` / `ternD()` output. The returned tibble carries a `ternB_meta` attribute so it can be passed to `ternB()` for inclusion in combined documents.

```r
custom <- tibble::tibble(
  Variable = c("N", "Median follow-up (months)"),
  Value    = c("47", "28.3 [18.1, 40.2]")
)
ternStyle(
  tbl      = custom,
  filename = "custom_table.docx"
)
```

Use the `spanner` argument to add a decked (two-level) header — a labelled row above the column names that groups related columns:

```r
results <- tibble::tibble(
  Variable          = c("Age (yr)", "BMI"),
  `Uni HR (95% CI)` = c("1.02 [0.98–1.06]", "1.11 [1.03–1.19]"),
  `Uni p`           = c("0.31", "0.006"),
  `Multi HR (95% CI)` = c("1.01 [0.97–1.05]", "1.08 [1.00–1.17]"),
  `Multi p`           = c("0.64", "0.047")
)
ternStyle(
  tbl      = results,
  filename = "regression_table.docx",
  spanner  = list(
    "Univariate"   = c("Uni HR (95% CI)", "Uni p"),
    "Multivariate" = c("Multi HR (95% CI)", "Multi p")
  )
)
```

When the spanner already provides the group label, use a **named** inner vector to strip the redundant prefix from the column-header row (`names` = display label, `values` = actual column name):

```r
cor_tbl <- tibble::tibble(
  Variable    = c("Gene A", "Gene B"),
  `SLAM r`    = c("0.42", "-0.18"),
  `SLAM p`    = c("0.031", "0.214"),
  `Control r` = c("0.11", "0.05"),
  `Control p` = c("0.401", "0.712")
)
ternStyle(
  tbl      = cor_tbl,
  filename = "correlation_table.docx",
  spanner  = list(
    "SLAM"    = c("r" = "SLAM r",    "p" = "SLAM p"),
    "Control" = c("r" = "Control r", "p" = "Control p")
  )
)
# Rendered header: SLAM | Control  (spanner row)
#                  r  p | r  p     (column-names row)
```

### `classify_normality()` — Inspect normality routing per variable

Returns a tidy tibble with per-variable × per-group statistics used by `ternG()` and `ternD()` to route normality: n, skewness, excess kurtosis, Shapiro-Wilk *p*, the decision gate (1–4), a plain-language `gate_reason`, and the final `routing` outcome (`"parametric"` / `"non-parametric"`). Designed for auditing test selection or addressing reviewer questions.

```r
norm_tbl <- classify_normality(tern_colon, group_var = "Recurrence")
print(norm_tbl)
```

### `val_p_format()` / `val_format()` — Formatting utilities

```r
val_p_format(0.0432)       # "0.043"
val_p_format(0.000012)     # "1E-5"
val_format(72.4, 8.1)  # "72.4 ± 8.1"
```

## Output

Every `ternD()` and `ternG()` call returns a tibble and, when `output_docx` is specified, writes a publication-ready `.docx` file directly. The Word
output uses Arial, consistent padding, bold significant *P* values, and optional
bold category-section headers. No post-processing is required before pasting
into a manuscript.

The tibble can also be:

- Passed to `word_export()` for additional formatting control (e.g. `category_start`)
- Given a caption in the Word output via `table_caption = "Table 1. Patient demographics."`
- Given a footnote below the table via `table_footnote = "† P values from Wilcoxon rank-sum test."` (accepts a character vector for multiple lines)
- Combined with other tables into one document using `ternB(list(T1, T2, T3), output_docx = "tables.docx")`
- Written to Excel with `writexl::write_xlsx()`
- Inspected or further manipulated in R

## Citation

If you use TernTables in your research, please cite the preprint:

> Preston JD, Abadiotakis H, Tang A, Rust CJ, Halkos ME, Daneshmand MA, Chan JL (2026).
> *TernTables: A Statistical Analysis and Table Generation Web Interface for Clinical and Biomedical Research.*
> bioRxiv 2026.04.15.717241. <https://doi.org/10.64898/2026.04.15.717241>

Or from R:

```r
citation("TernTables")
```

---

## License

[![License: MIT](https://img.shields.io/badge/license-MIT-green.svg)](https://github.com/jdpreston30/TernTables/blob/main/LICENSE)

This project is licensed under the MIT License.

---

**Web Application:** [tern-tables.com](https://tern-tables.com/) — the full TernTables workflow in a point-and-click interface, powered by this package.

---

**Developed and maintained by:**

- [Joshua D. Preston](https://orcid.org/0000-0001-9834-3017) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--9834--3017-brightgreen?logo=orcid)](https://orcid.org/0000-0001-9834-3017)
- [Helen Abadiotakis](https://orcid.org/0009-0002-8268-927X) [![ORCID](https://img.shields.io/badge/ORCID-0009--0002--8268--927X-brightgreen?logo=orcid)](https://orcid.org/0009-0002-8268-927X)
- [Ailin Tang](https://orcid.org/0009-0007-8715-1678) [![ORCID](https://img.shields.io/badge/ORCID-0009--0007--8715--1678-brightgreen?logo=orcid)](https://orcid.org/0009-0007-8715-1678)
- [Clayton J. Rust](https://orcid.org/0000-0001-5929-0733) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--5929--0733-brightgreen?logo=orcid)](https://orcid.org/0000-0001-5929-0733)
- [Joshua L. Chan](https://orcid.org/0000-0001-7220-561X) [![ORCID](https://img.shields.io/badge/ORCID-0000--0001--7220--561X-brightgreen?logo=orcid)](https://orcid.org/0000-0001-7220-561X)

Feedback and contributions are welcome.
