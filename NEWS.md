# TernTables (development)

## New parameters

**`ternStyle()` / `word_export()`**

* `spanner` — Named list for column spanner (decked) headers. Each element name
  is the spanner label; the value is a character vector of column names that fall
  under that label. Columns not listed receive an empty spanner cell. The spanner
  row is rendered above the normal column-header row, inheriting the same grey
  background, bold font, and bottom-border treatment. Adjacent labeled groups are
  visually separated by a narrow spacer column. Stored in `ternB_meta` so bundled
  tables produced via `ternB()` preserve their spanners automatically.

  Inner vectors may be **named** to override the display label in the column-header
  row without renaming the underlying tibble column — useful when column names carry
  a group prefix that becomes redundant once the spanner provides context:
  ```r
  spanner = list(
    "SLAM"    = c("r" = "SLAM r",    "p" = "SLAM p"),
    "Control" = c("r" = "Control r", "p" = "Control p")
  )
  # renders: r  p | r  p  under SLAM / Control spanners
  ```

# TernTables 1.7.2

## New exported functions

* **`ternStyle()`**: Applies full TernTables 'Word' formatting to any
  user-supplied tibble — font, header shading, borders, caption, footnote,
  and citation footer — without running it through the `ternG()`/`ternD()`
  pipeline. Supports `subheader_rows`, `bold_rows`, `italic_rows`,
  `bold_cols`, `italic_cols`, `bold_sig` (cell-level p-value bolding for
  non-standard column names), and `header_format_follow`. Returns a tibble
  with a `ternB_meta` attribute for direct use in `ternB()`.

* **`classify_normality()`**: Exposes the internal ROBUST normality routing
  algorithm as an exported function. Returns a tidy tibble with per-variable
  × per-group statistics (n, skewness, excess kurtosis, Shapiro-Wilk p), the
  gate that triggered the routing decision, a plain-language `gate_reason`,
  and the final parametric/non-parametric routing outcome. Intended for
  manuscript auditing and responding to reviewer questions about normality
  assessment.

## New parameters

**`ternG()`**

* `force_normal` — Bypass all normality assessment for listed variables;
  always route to mean ± SD and Welch tests. Per-variable counterpart to
  `force_ordinal`. `force_ordinal` takes priority when a variable appears in
  both.
* `force_continuous` — Bypass automatic binary 0/1 detection for listed
  variables so they are analysed as continuous rather than converted to Y/N
  categorical.
* `show_p` — When `FALSE`, suppresses the P value column and all associated
  columns (OR, test, normality), producing a descriptive-only grouped table.
* `percentage_compute` — `"column"` (default) or `"row"`. Controls the
  denominator for categorical percentages. When `"row"`, the Total column is
  automatically suppressed.
* `categorical_posthoc` — For three-or-more-group comparisons, computes
  Haberman's adjusted standardized residuals following a significant omnibus
  test (p < 0.05). Cells exceeding ±1.96 are annotated with `*`. The
  ±1.96 threshold derives from the omnibus chi-squared distribution; no
  additional multiple-comparisons correction is required.
* `p_adjust` — When `TRUE`, applies Benjamini-Hochberg (BH) false discovery
  rate correction to all omnibus P values.
* `p_adjust_display` — `"fdr_only"` (default) replaces the P column with
  BH-corrected values; `"both"` retains raw values alongside the corrected
  column.
* `show_missing` — When `TRUE`, appends a `Missing: n (%)` sub-row beneath
  each variable showing per-group missing counts. Footnote added
  automatically.
* `show_missingness` — `FALSE` (default), `"total"`, or `"group"`. Appends
  dedicated missingness percentage column(s): one overall column (`"total"`)
  or one interleaved column per group (`"group"`). Counts both `NA` and
  string representations of missing data.
* `missing_indicators` — Custom character vector that replaces the built-in
  string-NA list when computing missingness columns (requires
  `show_missingness` to be active). Matching is case-insensitive.
* `zero_to_dash` — When `TRUE`, replaces `"0 (0%)"` cells with `"-"` in
  categorical output.
* `round_decimal` — Integer; overrides the default 1 decimal place for all
  continuous summary values. Ignored when `round_intg = TRUE`.
* `font_family` — Font used throughout all 'Word' output. Defaults to
  `getOption("TernTables.font_family", "Arial")`; set package-wide with
  `options(TernTables.font_family = "Times New Roman")`.
* `citation` — When `TRUE` (default), embeds a full package citation as a
  page footer in every exported `.docx` and at the end of the methods
  document.
* `open_doc` — When `TRUE` (default), opens the written `.docx` in the
  system default application after saving. Set `FALSE` to suppress (useful
  in scripted or server-side workflows).
* `plain_header` — Named character vector (same interface as
  `category_start`). Inserts a label-only row with underline formatting and
  no bold, merge, or border treatments. Also accepted by `word_export()`.
* `abbreviation_footnote`, `variable_footnote`, `index_style` — Structured
  footnote system. `abbreviation_footnote` prints first. `variable_footnote`
  (named character vector) auto-assigns `*`, `†`, `‡` … as superscripts to
  named variables in column 1 and appends definitions below the table.
  `index_style = "alphabet"` uses Unicode superscript letters instead. Pipe-
  separated keys (`"Var A|Var B"`) assign one shared symbol and footnote to
  multiple variables.

**`ternD()`**: `force_normal`, `force_continuous`, `plain_header`,
`show_missing`,
`show_missingness`, `missing_indicators`, `zero_to_dash`, `round_decimal`,
`font_family`, `citation`, `open_doc` — all behave identically to the
`ternG()` descriptions above. `show_missingness = "group"` raises an
informative error directing the user to `ternG()`.

**`ternP()`**

* `mode` — `"auto"` (default) or `"manual"`. In `"manual"` mode, the PHI
  column-name hard-stop is skipped and a prominent warning is emitted; all
  other cleaning steps still run. Intended for datasets that trigger
  false-positive PHI flags (e.g. a column named `"Address"` that contains a
  non-PHI clinical field). Use `drop_cols` to explicitly remove identifier
  columns in this mode.
* `extra_na` — Character vector of additional strings to treat as `NA`,
  appended to the built-in list. Example:
  `extra_na = c("9999", "Not Done", "PENDING")`.
* `drop_cols` — Character vector of column names to drop before cleaning
  begins. Names not found in the data are silently ignored.

The preprocessing summary now includes a **Missingness Summary** section
listing every column with at least one missing value (count and percentage),
computed on the fully cleaned data. The section is suppressed when no
missing values exist.

**`word_export()`, `ternB()`, `ternStyle()`**: `font_family`, `round_decimal`,
`citation` (as described for `ternG()`). `word_export()` and `ternStyle()`
additionally accept `bold_sig` (named list for cell-level p-value bolding
of custom tibbles with non-standard p-value column names).

**`write_cleaning_doc()`**: `open_doc` and `citation` added for consistency
with all other Word-output functions.

**`write_methods_doc()`** redesigned: generates a single dynamic paragraph
tailored to the actual run (descriptive, two-group, or three-or-more-group)
rather than a fixed three-section boilerplate. `boilerplate = TRUE` writes a
comprehensive reference document covering all five standard configurations.
`ternB(methods_doc = TRUE)` generates one labeled methods section per table,
deduplicating tables with identical configurations.

## Bug fixes

* **`fisher.test()` segfault** (`ternG`): Tables larger than 2×2 now route
  directly to Monte Carlo simulation (`simulate.p.value = TRUE`,
  B = 10,000). Previously the C-level exact algorithm could segfault on
  tables with many levels, killing the R session — a condition that
  `tryCatch` cannot intercept.
* **Degenerate single-level categorical crash** (`ternG`): Variables reduced
  to one non-`NA` level after string-NA removal now display
  `NA (insufficient variation)` for the p-value instead of crashing the
  entire `ternG()` call.
* **`methods_filename = NULL` crash** (`ternG`, `ternD`,
  `write_methods_doc`): Passing `NULL` explicitly now resolves to the
  default `"TernTables_methods.docx"` rather than throwing a
  `dirname(NULL)` error.
* **`categorical_posthoc` crashes** (`ternG`): Fixed three independent
  issues — (1) `stdres` returning a vector instead of a matrix caused an
  `if()` condition to error; (2) `NA` factor level names bypassed `%in%`
  guards causing subscript-out-of-bounds; (3) `cat_posthoc_fisher_display`
  was referenced before being extracted from `.ternG_env`.
* **Blank-string factor levels** (`ternG`): Factor levels named `""`
  (common in UNOS/SRTR `haven_labelled` data) produced `NA (NA%)` because
  `as.data.frame.matrix()` silently renames `""` to `"X"` via
  `make.names()`. Fixed by saving column names before conversion and
  restoring them explicitly; all 2D cell lookups now use integer indexing
  via `match()`.
* **Blank page between `ternB()` tables**: Stripped the default blank
  paragraph from each temp document, eliminating spurious blank pages in
  combined output.
* **Citation footer bleed in `ternB()`**: Page footer from one table's temp
  document no longer carries over into subsequent tables in the combined
  file.
* **CLD dependency resolution**: `rstatix` and `multcompView` moved to
  `Imports` (were `Suggests`), ensuring compact letter display superscripts
  always work without a separate install step.
* **Wide-table page overflow** (`word_export`): `fit_to_width(6.5)` is now
  applied after `autofit()` only when the table exceeds Letter page width
  (6.5 in), preventing column truncation in 'Word' and PDF exports.
* **Name-cleaning false positives**: Fixed `.apply_cleaning_rules()` patterns
  that incorrectly transformed variable names containing medical
  abbreviations and unit suffixes (e.g. `"Gy"` in radiation dose variables).
* **`bold_sig` column-name mismatch** (`word_export`, `ternStyle`):
  `word_export()` renames columns internally (e.g. `"P"` → `"P value"`,
  spaces → `\n` when `line_break_header = TRUE`). The `bold_sig` lookup was
  comparing caller-supplied names against already-renamed columns, so HR
  bolding was silently skipped. Fixed by building an original-to-renamed name
  map before the lookup.

## Internal changes

* **CRAN compliance — `<<-` eliminated**: All accumulator patterns using
  `<<-` inside nested closures in `ternG.R`, `ternD.R`, and `ternP.R`
  replaced with `new.env(parent = emptyenv())`-based counters. No
  user-visible change.
* **`set.seed()` replaced with `withr::with_seed()`**: RNG state is now
  scoped locally within the Monte Carlo Fisher's exact fallback, restoring
  the caller's state after the call. `withr` added to `Imports`.
* **ROBUST Gate 2 — kurtosis added**: Excess kurtosis (|excess kurtosis| > 7)
  now triggers non-parametric routing alongside absolute skewness > 2.
  Normality decision logic extracted to `utils_normality.R`.
* **CLD letter ordering**: Removed center-based letter re-mapping;
  `multcompLetters()` default alphabetical ordering is now used directly,
  aligning with standard CLD conventions.
* **PHI detection tightened**: `patient_id`, `subject_id`,
  `participant_id`, and clinical-event date patterns removed from the flag
  list; only personal-identity patterns (DOB, DOD) remain.
* **Shared missing-value helpers**: `.tern_missing_strings()` and
  `.is_missing_value()` extracted to `utils_preprocess.R` as a single
  source of truth used by both `ternP()` and the `show_missingness` columns
  in `ternG()`/`ternD()`.
* **Independence-of-observations scope**: Explicit notes added to the
  package help page, `ternG()` description, vignette, and README stating
  that all tests assume independent observations.

---


# TernTables 1.6.4

* CRAN resubmission addressing two reviewer comments on v1.6.3.
* Eliminated all uses of `<<-` in `R/ternG.R`, `R/ternD.R`, and `R/ternP.R`.
  Accumulator patterns replaced with `new.env(parent = emptyenv())`-based
  counters; the `dplyr::across()` lambda in `ternP.R` replaced with an
  explicit `for` loop. No change to user-visible behavior.
* Replaced bare `set.seed()` with `withr::with_seed()` in the Monte Carlo
  Fisher's exact fallback, scoping the seed locally and restoring the
  caller's RNG state. `withr` added to `Imports`.
* Removed Maria V. Aslam from `Authors@R` at her request.

---

# TernTables 1.6.3

* CRAN resubmission addressing reviewer comments on v1.3.1.
* All `\dontrun{}` blocks replaced with `\donttest{}` across all exported
  functions. Examples that write files to disk use `\donttest{}` because the
  'Word' export operations may exceed 5 seconds on slower machines; no missing
  software or API keys are required.
* Software and package names now quoted in the `Description` field of
  `DESCRIPTION` per CRAN policy: 'Word', 'Excel', 'tibble', 'officer',
  'flextable', 'writexl', 'rstatix'.
* Package-level documentation (`?TernTables`) updated: normality routing
  description corrected to four-gate algorithm; `.onLoad` now explicitly
  initialises `TernTables.line_break_header` option.
* `val_format()` now uses the Unicode plus-minus symbol (±) for consistency
  with `ternG()` and `ternD()` output.
* `utils_naming.R`: single-word variable names (e.g. `age`, `sex`, `race`)
  now correctly flow through the abbreviation map and capitalisation rules
  in `.apply_cleaning_rules()`.

# TernTables 1.6.2

* Added odds ratio (OR) support for two-level categorical variables in
  `ternG()` (e.g. Male/Female shown with `1.00 (ref.)` and computed OR with
  95% CI). Previously only Y/N and 0/1 binary variables received ORs.
* Added `factor_order = "mixed"` option to `ternG()` and `ternD()`.
* Post-hoc compact letter display (CLD) superscripts implemented in
  `utils_posthoc.R` with center-based letter ordering (highest center = "a").
* Fallback to Monte Carlo simulation for Fisher's exact test when the exact
  algorithm cannot complete due to workspace limits (B = 10,000; seed fixed
  via `getOption("TernTables.seed")`).
* Liberalized PHI column name detection in `utils_preprocess.R` to reduce
  false positives on common research variable names.

# TernTables 1.6.1

* Integrated tern-tables.com web app into all public-facing documentation:
  README, vignette, and package help page (`?TernTables`).
* Added `URL:` and `BugReports:` fields to `DESCRIPTION`.

# TernTables 1.6.0

* Added `ternP()`: preprocessing function for raw CSV/XLSX data. Handles
  string NA conversion, whitespace trimming, empty column and blank row
  removal, and case normalisation. Hard-stops on PHI column name patterns
  and unnamed columns with data.
* Added `write_cleaning_doc()`: writes a 'Word' audit document recording every
  transformation applied by `ternP()`.
* Added bundled example data `tern_colon_messy.csv` to `inst/extdata/csv/` for
  use in `ternP()` examples.
* `line_break_header` parameter refined: improved behaviour for edge-case
  column widths in `ternG()`, `ternD()`, and `word_export()`.

# TernTables 1.5.0

* Added ROBUST normality routing (`consider_normality = "ROBUST"`, now the
  default). Four-gate algorithm applied per group: (1) n < 3 fail-safe to
  non-parametric; (2) |skewness| > 2 to non-parametric; (3) all groups
  n ≥ 30 to parametric via CLT; (4) Shapiro-Wilk p > 0.05 to parametric.
  Implemented in both `ternG()` and `ternD()`.

# TernTables 1.4.0

* Added `ternB()`: combines multiple TernTables result tibbles into a single
  formatted 'Word' document.
* Added `table_caption` parameter to `ternG()`, `ternD()`, and `word_export()`:
  places a bold caption above the table in the 'Word' output.
* Added `table_footnote` parameter to `ternG()`, `ternD()`, and `word_export()`:
  adds a merged footer row below the table.
* Added Welch ANOVA for 3+ group continuous comparisons (parametric path);
  previously only Kruskal-Wallis was available for 3+ groups.
* Added `line_break_header` parameter to `ternG()`, `ternD()`, and
  `word_export()`: wraps long column headers onto two lines in 'Word' output.
* `write_methods_doc()` now generates a methods paragraph tailored to `ternB()`
  multi-table output.
* Fixed bug where P values were not displaying in three-group comparisons.
* Fixed dynamic bolding/non-bolding of table captions.

# TernTables 1.3.1

* Initial CRAN submission.
* `ternG()`: grouped comparison table for 2- and 3-level group variables,
  with optional odds ratios (`OR_col`), normality testing, and post-hoc
  test framework.
* `ternD()`: descriptive summary table with no group comparisons.
* `word_export()`: exports any TernTables tibble to a formatted 'Word' document.
* `write_methods_doc()`: generates a boilerplate statistical methods paragraph.
* `val_format()` and `val_p_format()`: formatting utilities for publication-ready
  numeric and P value display.

