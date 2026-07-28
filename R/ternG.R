#' Generate grouped summary table with appropriate statistical tests
#'
#' Creates a grouped summary table with optional statistical testing for group
#' comparisons. Supports numeric and categorical variables; numeric variables
#' can be treated as ordinal via \code{force_ordinal}. Includes options to
#' calculate P values and odds ratios. For descriptive
#' (ungrouped) tables, use \code{ternD}.
#'
#' \strong{Independence assumption:} all statistical tests applied by this
#' function (Welch's \emph{t}-test, Wilcoxon rank-sum, Welch ANOVA,
#' Kruskal-Wallis, chi-squared, and Fisher's exact) assume that observations
#' are independent — each row must represent a distinct, unrelated subject.
#' \code{ternG} is not appropriate for repeated-measures, longitudinal, or
#' clustered data (e.g. pre/post measurements, matched pairs, or patients
#' nested within sites).
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{group_var} and \code{exclude_vars}.
#' @param exclude_vars Character vector of variable(s) to exclude. \code{group_var} is automatically excluded.
#' @param group_var Character, the grouping variable (factor or character with >=2 levels).
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use medians/IQR and nonparametric tests).
#' @param force_normal Character vector of variable names to treat as normally distributed, bypassing all
#'   normality assessment (Gates 1--4 under \code{"ROBUST"}, or Shapiro-Wilk under \code{TRUE}). Listed
#'   variables are summarized as mean \eqn{\pm} SD and compared with Welch tests regardless of the
#'   \code{consider_normality} setting. Takes priority over \code{consider_normality} but not over
#'   \code{force_ordinal} (if a variable appears in both, \code{force_ordinal} wins). Default is \code{NULL}.
#' @param force_continuous Character vector of variables to force treatment as continuous (mean \eqn{\pm} SD and
#'   parametric tests), bypassing the automatic binary \code{0/1} detection that would otherwise convert
#'   them to categorical Y/N. Useful when a numeric variable with only two unique values (e.g. \code{0}/\code{1}
#'   dose levels) should be analysed as a continuous measurement rather than a dichotomous category.
#'   Takes priority over automatic type detection but does not override \code{force_ordinal} (if a variable
#'   appears in both, \code{force_ordinal} wins). Default is \code{NULL}.
#' @param group_order Optional character vector to specify a custom group level order.
#' @param output_xlsx Optional filename to export the table as an Excel file.
#' @param output_docx Optional filename to export the table as a Word document.
#' @param OR_col Logical; if \code{TRUE}, adds unadjusted odds ratios with 95\% CI for binary categorical variables
#'   (Y/N, YES/NO, or numeric 0/1) and two-level categorical variables (e.g. Male/Female). For
#'   two-level categoricals displayed with sub-rows, the reference level (factor level 1, or
#'   alphabetical first for non-factors) shows \code{"1.00 (ref.)"}; the non-reference level
#'   shows the computed OR with 95\% CI. Variables with three or more levels show \code{"-"}.
#'   Only valid when \code{group_var} has exactly 2 levels; an error is raised for 3+ group comparisons.
#'   Default is \code{FALSE}.
#' @param OR_method Character; controls how odds ratios are calculated when \code{OR_col = TRUE}.
#'   If \code{"dynamic"} (default), uses Fisher's exact method when any expected cell count is < 5
#'   (Cochran criterion), otherwise uses the Wald method. If \code{"wald"}, forces the Wald method
#'   regardless of expected cell counts.
#' @param consider_normality Character or logical; controls how continuous variables are routed to
#'   parametric vs. non-parametric tests.
#'   \code{"ROBUST"} (default) applies a four-gate decision consistent with standard biostatistical
#'   practice: (1) any group n < 3 is a conservative fail-safe to non-parametric; (2) absolute skewness
#'   > 2 or excess kurtosis > 7 in any group routes to non-parametric regardless of sample size
#'   (catches heavy-tailed distributions and LOS/count-style skews in which the CLT guarantee is
#'   compromised); (3) all groups n \eqn{\geq} 30 routes to parametric via the Central Limit Theorem;
#'   (4) otherwise Shapiro-Wilk p > 0.05 in all groups routes to parametric. Normal variables use mean \eqn{\pm} SD
#'   and Welch t-test (2 groups) or Welch ANOVA (3+ groups); non-normal variables use median [IQR] and
#'   Wilcoxon rank-sum (2 groups) or Kruskal-Wallis (3+ groups).
#'   If \code{TRUE}, uses Shapiro-Wilk alone (p > 0.05 in all groups = normal). Conservative at large n.
#'   If \code{FALSE}, all numeric variables are treated as normally distributed regardless of distribution.
#'   If \code{"FORCE"}, all numeric variables are treated as non-normal (median [IQR], nonparametric tests).
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk P values in the output. Default is \code{FALSE}.
#' @param show_test Logical; if \code{TRUE}, includes the statistical test name as a column in the output. Default is \code{FALSE}.
#' @param p_digits Integer; number of decimal places for P values (default 3).
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param round_decimal Integer or \code{NULL}; number of decimal places for all continuous summary values
#'   (means, SDs, medians, IQRs). Overrides the default of 1 decimal place when set. Ignored when
#'   \code{round_intg = TRUE}. Default is \code{NULL} (1 decimal place).
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and subheadings for publication-ready output using built-in rule-based pattern matching for common medical abbreviations and prefixes. Default is \code{TRUE}.
#' @param insert_subheads Logical; if \code{TRUE} (default), creates a hierarchical structure with a header row and
#'   indented sub-category rows for categorical variables with 3 or more levels. Binary variables
#'   (Y/N, YES/NO, or numeric 1/0 -- which are auto-detected and treated as Y/N) are always displayed
#'   as a single row showing the positive/yes count regardless of this setting. Two-level categorical
#'   variables whose values are not Y/N, YES/NO, or 1/0 (e.g. Male/Female) use the hierarchical
#'   sub-row format, showing both levels as indented rows.
#'   If \code{FALSE}, all categorical variables use a single-row flat format. Default is \code{TRUE}.
#' @param factor_order Character; controls the ordering of factor levels in the output.
#'   \code{"mixed"} (default) applies level-aware ordering for two-level categorical variables and
#'   frequency ordering for variables with three or more levels: for any factor, factor level order
#'   is always respected regardless of the number of levels; for non-factor two-level variables
#'   (e.g. Male/Female), levels are sorted alphabetically; for non-factor variables with three or
#'   more levels, levels are sorted by decreasing frequency.
#'   \code{"levels"} respects the original factor level ordering for all variables; if the variable
#'   is not a factor, falls back to frequency ordering.
#'   \code{"frequency"} orders all levels by decreasing frequency (most common first).
#' @param table_font_size Numeric; font size for Word document output tables. Default is 9.
#' @param methods_doc Logical; if \code{TRUE} (default), generates a methods document describing the statistical tests used.
#' @param methods_filename Character; filename for the methods document. Default is \code{"TernTables_methods.docx"}.
#' @param category_start Named character vector specifying where to insert category headers.
#'   Names are the header label text to display; values are the anchor variable -- either the
#'   original column name (e.g. \code{"Age_Years"}) or the cleaned display name
#'   (e.g. \code{"Age (yr)"}). Both forms are accepted.
#'   Example: \code{c("Demographics" = "Age_Years", "Clinical" = "bmi")}.
#'   Default is \code{NULL} (no category headers).
#' @param plain_header Named character vector, same interface as \code{category_start}. Names are
#'   the label text; values are the anchor variable to insert before. Inserts a label-only row
#'   with underline formatting and no bold, merge, or border treatments. Default \code{NULL}.
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) that should be
#'   formatted as italicized and indented in Word output -- matching the appearance of factor sub-category
#'   rows. Has no effect on the returned tibble; only applies when \code{output_docx} is specified or when
#'   the tibble is passed to \code{word_export}.
#' @param manual_underline Character vector of display variable names (post-cleaning) that should be
#'   formatted as underlined in Word output -- matching the appearance of multi-category variable headers.
#'   Has no effect on the returned tibble; only applies when \code{output_docx} is specified or when
#'   the tibble is passed to \code{word_export}.
#' @param show_total Logical; if \code{TRUE}, adds a "Total" column showing the aggregate summary statistic across all groups (e.g., for a publication Table 1 that includes both per-group and overall columns). Default is \code{TRUE}.
#' @param table_caption Optional character string for a table caption to display above the table in
#'   the Word document. Rendered as size 11 Arial bold, single-spaced with a small gap before the table.
#'   Default is \code{NULL} (no caption).
#'   Example: \code{"Table 2. Comparison of recurrence vs. no recurrence."}
#' @param table_footnote Optional character string for a footnote to display below the table in the
#'   Word document. Rendered as size 6 Arial italic with a double-bar border above and below.
#'   Default is \code{NULL} (no footnote).
#' @param abbreviation_footnote Optional character string listing abbreviations. Always printed
#'   first in the footnote block. Default \code{NULL}.
#' @param variable_footnote Optional named character vector. Names are display variable names
#'   (case-insensitive); values are the footnote definition text. Each variable gets the next
#'   symbol appended to its name in the table, and the footnote block lists each definition
#'   below the abbreviation line. To share one footnote between multiple variables, separate
#'   their names with a pipe: \code{c("Var A|Var B" = "Shared note text.")}. Default \code{NULL}.
#' @param index_style Character; \code{"symbols"} (default) uses *, dagger, double-dagger ...
#'   \code{"alphabet"} uses Unicode superscript letters. See \code{word_export} for details.
#' @param line_break_header Logical; if \code{TRUE} (default), column headers are wrapped with
#'   \code{\\n} -- group names break on spaces, sample size counts move to a second line, and
#'   the first column header reads \code{"Category / Variable"}. Set to \code{FALSE} to suppress
#'   all header line breaks. Can also be set package-wide via
#'   \code{options(TernTables.line_break_header = FALSE)}.
#' @param post_hoc Logical; if \code{TRUE}, runs pairwise post-hoc tests for continuous and ordinal
#'   variables in three or more group comparisons and annotates each group column value with a compact
#'   letter display (CLD) superscript. Groups sharing a letter are not significantly different at
#'   \eqn{\alpha = 0.05}. For normally distributed variables (Welch ANOVA path), Games-Howell
#'   pairwise tests are used. For non-normal and ordinal variables (Kruskal-Wallis path), Dunn's test
#'   with Holm correction is used. Post-hoc testing is never applied to categorical variables.
#'   Only valid when \code{group_var} has three or more levels; silently ignored for two-group
#'   comparisons. Requires the \code{rstatix} package. Default is \code{FALSE}.
#' @param indent_info_column Logical; if \code{FALSE} (default), the internal \code{.indent} helper column
#'   is dropped from the returned tibble. Set to \code{TRUE} to retain it -- this is necessary when you
#'   intend to post-process the tibble and later pass it to \code{word_export} directly, as
#'   \code{word_export} uses the \code{.indent} column to apply correct indentation and italic formatting
#'   in the Word table.
#' @param p_adjust Logical; if \code{TRUE}, applies the Benjamini-Hochberg (BH) false discovery rate
#'   correction to all omnibus P values after testing. The correction pool is one P value per
#'   variable — sub-rows of multi-level categoricals share the parent P value and are not
#'   double-counted. Post-hoc pairwise P values (which already carry within-variable Holm
#'   correction) are excluded from the correction pool. Default is \code{FALSE}.
#' @param p_adjust_display Character; controls how BH-corrected P values appear in the output
#'   when \code{p_adjust = TRUE}. \code{"fdr_only"} (default) replaces the standard P value
#'   column with the corrected values, renaming the column to \code{"P value (FDR corrected)"}.
#'   \code{"both"} retains the original P values in a column named \code{"P value"} and adds
#'   FDR-corrected values immediately to its right in a column named
#'   \code{"P value (FDR corrected)"}. Ignored when \code{p_adjust = FALSE}.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens the written Word document
#'   in the system default application after saving. Set to \code{FALSE} to suppress.
#'   Has no effect when \code{output_docx} is \code{NULL}.
#' @param citation Logical; if \code{TRUE} (default), appends a citation line at the bottom
#'   of the table footnote block and at the end of the methods document: package version, authors,
#'   and links to the GitHub repository and web interface. Set to \code{FALSE} to suppress.
#' @param font_family Character; font family name used for all Word output (table,
#'   captions, footnotes, methods document). Any font installed on the system that
#'   renders the document may be used. Popular options include \code{"Arial"},
#'   \code{"Helvetica"}, \code{"Times New Roman"}, \code{"Garamond"}, and
#'   \code{"Calibri"}. Defaults to \code{getOption("TernTables.font_family", "Arial")}.
#' @param show_missing Logical; if \code{TRUE}, appends a \code{"Missing"} row after each
#'   variable's data rows showing the count and percentage of missing observations per group
#'   (denominator is each group's total N). Missing rows display \code{"-"} in the P and OR
#'   columns. A footnote is automatically appended noting that missing values are reported.
#'   Default is \code{FALSE}.
#' @param zero_to_dash Logical; if \code{TRUE}, replaces any categorical cell that would display
#'   \code{"0 (0\%)"} with \code{"-"} in the output table. Useful when zero counts in a group are
#'   not meaningful to report numerically (e.g. no patients with a condition in one arm).
#'   Default is \code{FALSE}.
#' @param percentage_compute Character; controls the denominator used when computing percentages
#'   for categorical variables. \code{"column"} (default) divides each cell count by the column
#'   (group) total, so percentages describe the composition of each group -- the standard
#'   Table 1 interpretation (e.g. "60\% of the Recurrence group is Male"). \code{"row"} divides
#'   each cell count by the row total (the number of subjects with that category level across all
#'   groups), so percentages describe how each category level is distributed across groups
#'   (e.g. "30\% of Males had Recurrence"). When \code{"row"}, the Total column is automatically
#'   suppressed (a Total column would show 100\% for every level, which is uninformative).
#'   Applies to both binary and multinomial categorical variables
#'   in both two- and three-group comparisons.
#' @param show_p Logical; if \code{TRUE} (default), the P value column is included in the
#'   output and Excel/Word exports. Set to \code{FALSE} to produce a descriptive-only grouped
#'   table — the output will contain only the Variable column, one column per group level, and
#'   the Total column (if \code{show_total = TRUE}). When \code{FALSE}, \code{OR_col},
#'   \code{show_test}, \code{print_normality}, \code{post_hoc}, \code{categorical_posthoc},
#'   and \code{p_adjust} are all suppressed automatically.
#' @param categorical_posthoc Logical; if \code{TRUE}, computes adjusted standardized residuals
#'   from the global contingency table for categorical variables following a significant omnibus
#'   test (p < 0.05). Cells whose adjusted standardized residual exceeds \eqn{\pm 1.96}
#'   are marked with an asterisk (\code{*}), indicating a significant deviation from expected
#'   frequencies (\eqn{\alpha = 0.05}). This method is equivalent to Haberman's adjusted
#'   residuals and does not require a separate multiple-comparisons correction, as the \eqn{\pm 1.96}
#'   threshold is derived directly from the omnibus test distribution. Only applied when
#'   \code{group_var} has three or more levels; silently ignored for two-group comparisons.
#'   Default is \code{FALSE}.
#' @param show_missingness Controls whether a column of missing-value percentages is appended
#'   to the table. Options:\cr
#'   \code{FALSE} (default) — no missingness columns added.\cr
#'   \code{"total"} — one column (\code{"Missing, n (\%)"}) appended at the far right showing
#'   the number and percentage of missing observations across \emph{all} rows for each variable.\cr
#'   \code{"group"} — one column per group level (\code{"Miss. [level]"}) interleaved immediately
#'   after each group's data column, showing per-group missingness for each variable.\cr
#'   Missingness is computed on the \emph{raw} data column (before ternG's internal NA filtering),
#'   so both \code{NA} values and string representations of missing data (e.g., \code{"Unknown"},
#'   \code{"N/A"}) are counted. See \code{missing_indicators} to customise which strings count.
#' @param missing_indicators Optional character vector of string values to treat as missing
#'   in addition to (or instead of) the built-in ternP defaults. When \code{NULL} (default),
#'   the ternP canonical list is used (\code{"na"}, \code{"n/a"}, \code{"unknown"}, etc.).
#'   When supplied, the custom list \strong{replaces} the ternP defaults — only the values
#'   in \code{missing_indicators} (plus true \code{NA}) are counted as missing. Matching is
#'   always case-insensitive and ignores leading/trailing whitespace.
#' @param plain_tibble Logical; if \code{TRUE}, returns the tidy per-variable statistics frame
#'   (see \code{\link{tern_stats}}) instead of the formatted display table — one un-indented row
#'   per variable with raw numeric statistics and their formatted counterparts in adjacent
#'   columns. Word/Excel exports and the methods document are still written if requested.
#'   Setting this is never required to reach the tidy data: the same frame is always attached
#'   to the normal return value as the \code{"tern_stats"} attribute, so
#'   \code{tern_stats(ternG(...))} yields both the pretty table and the tidy data from one call.
#'   Default is \code{FALSE}.
#'
#' @return A tibble with one row per variable (multi-row for multi-level factors), showing summary statistics by group,
#' P values, test type, and optionally odds ratios and total summary column.
#' Two tidy, machine-readable frames are always attached as attributes:
#' \code{"tern_stats"} (one row per variable — test, statistic, df, raw numeric P value,
#' normality decision, odds ratio) and \code{"tern_estimates"} (long format: one row per
#' variable x group x level, with the underlying counts, percentages, means, SDs, medians and
#' quartiles). Access them with \code{\link{tern_stats}} and \code{\link{tern_estimates}}
#' rather than parsing the display table.
#'
#' @seealso \code{\link{tern_stats}}, \code{\link{tern_estimates}}
#'
#' @examples
#' data(tern_colon)
#'
#' # 2-group comparison
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       methods_doc = FALSE)
#'
#' # 2-group comparison with odds ratios
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       OR_col = TRUE, methods_doc = FALSE)
#'
#' # 3-group comparison
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Treatment_Arm",
#'       group_order = c("Observation", "Levamisole", "Levamisole + 5FU"),
#'       methods_doc = FALSE)
#'
#' # 2-group comparison with BH FDR correction (fdr_only — default display)
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       p_adjust = TRUE, methods_doc = FALSE)
#'
#' # 2-group comparison with BH FDR correction (show raw + corrected side by side)
#' ternG(tern_colon, exclude_vars = c("ID"), group_var = "Recurrence",
#'       p_adjust = TRUE, p_adjust_display = "both", methods_doc = FALSE)
#'
#' # Export to Word (writes a file to tempdir)
#' \donttest{
#' ternG(tern_colon,
#'       exclude_vars   = c("ID"),
#'       group_var      = "Recurrence",
#'       OR_col         = TRUE,
#'       methods_doc    = FALSE,
#'       open_doc       = FALSE,
#'       output_docx    = file.path(tempdir(), "comparison.docx"),
#'       category_start = c("Patient Demographics"  = "Age (yr)",
#'                          "Tumor Characteristics" = "Positive Lymph Nodes (n)"))
#' }
#'
#' @export
ternG <- function(data,
                  vars = NULL,
                  exclude_vars = NULL,
                  group_var,
                  force_ordinal = NULL,
                  force_normal = NULL,
                  force_continuous = NULL,
                  group_order = NULL,
                  output_xlsx = NULL,
                  output_docx = NULL,
                  OR_col = FALSE,
                  OR_method = "dynamic",
                  consider_normality = "ROBUST",
                  print_normality = FALSE,
                  show_test = FALSE,
                  p_digits = 3,
                  round_intg = FALSE,
                  round_decimal = NULL,
                  smart_rename = TRUE,
                  insert_subheads = TRUE,
                  factor_order = "mixed",
                  table_font_size = 9,
                  methods_doc = TRUE,
                  methods_filename = "TernTables_methods.docx",
                  category_start = NULL,
                  plain_header = NULL,
                  manual_italic_indent = NULL,
                  manual_underline = NULL,
                  indent_info_column = FALSE,
                  show_total = TRUE,
                  table_caption = NULL, table_footnote = NULL,
                  abbreviation_footnote = NULL, variable_footnote = NULL,
                  index_style = "symbols",
                  line_break_header = getOption("TernTables.line_break_header", TRUE),
                  post_hoc = FALSE,
                  p_adjust = FALSE,
                  p_adjust_display = "fdr_only",
                  open_doc = TRUE, citation = TRUE,
                  font_family = getOption("TernTables.font_family", "Arial"),
                  show_missing = FALSE,
                  show_p = TRUE,
                  zero_to_dash = FALSE,
                  percentage_compute = "column",
                  categorical_posthoc = FALSE,
                  show_missingness = FALSE,
                  missing_indicators = NULL,
                  plain_tibble = FALSE) {

  # Helper function for proper rounding (0.5 always rounds up)
  round_up_half <- function(x, digits = 0) {
    if (digits == 0) {
      floor(x + 0.5)
    } else {
      factor <- 10^digits
      floor(x * factor + 0.5) / factor
    }
  }

  if (is.null(vars)) {
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  }

  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }
  if (!is.null(group_order)) {
    data[[group_var]] <- factor(data[[group_var]], levels = group_order)
  }

  data <- data %>% filter(!is.na(.data[[group_var]]))
  n_levels <- length(unique(data[[group_var]]))

  # ── Validate percentage_compute ───────────────────────────────────────────
  percentage_compute <- match.arg(percentage_compute, c("column", "row"))

  # ── row %: auto-suppress Total (n (100%) per level is uninformative) ────────
  if (percentage_compute == "row") show_total <- FALSE

  # ── show_p = FALSE: suppress all stat-output flags early ──────────────────
  if (!isTRUE(show_p)) {
    OR_col              <- FALSE
    show_test           <- FALSE
    print_normality     <- FALSE
    post_hoc            <- FALSE
    categorical_posthoc <- FALSE
    p_adjust            <- FALSE
  }

  # ── Validate show_missingness ───────────────────────────────────────────
  if (!isFALSE(show_missingness) &&
      !(is.character(show_missingness) && show_missingness %in% c("total", "group"))) {
    stop('`show_missingness` must be FALSE, "total", or "group".', call. = FALSE)
  }

  if (isTRUE(OR_col) && n_levels != 2) {
    stop("`OR_col = TRUE` is only valid for 2-group comparisons. ",
         group_var, " has ", n_levels, " levels. Set `OR_col = FALSE` or use a binary group variable.")
  }

  group_counts <- data %>% count(.data[[group_var]]) %>% deframe()
  group_levels <- names(group_counts)
  group_labels <- setNames(
    paste0(group_levels, " (n = ", group_counts, ")"),
    group_levels
  )
  
  # Calculate total N for the Total column header
  total_n <- nrow(data)

  # Initialize normality tracking variables
  normality_results <- list()
  # Use an environment instead of <<- so closures can accumulate without
  # modifying the global environment (CRAN policy compliance).
  .ternG_env <- new.env(parent = emptyenv())
  .ternG_env$numeric_vars_tested <- 0L
  .ternG_env$numeric_vars_failed <- 0L
  # Track which variables actually had post-hoc tests run (omnibus significant)
  .ternG_env$posthoc_ran_display <- character(0)
  # Track which categorical variables had adjusted residuals applied
  .ternG_env$cat_posthoc_ran_display <- character(0)
  # Track which variables had categorical_posthoc run while omnibus was Fisher's exact
  .ternG_env$cat_posthoc_fisher_display <- character(0)
  # Track which variables used Monte Carlo Fisher's exact (workspace limit fallback)
  .ternG_env$fisher_sim_display  <- character(0)
  # ── Tidy statistics side-channel (see R/utils_stats_tidy.R) ────────────────
  # Accumulated from the same objects that populate the display cells, so the
  # tidy frames can never drift from the rendered table.
  .ternG_env$stats_records <- list()
  .ternG_env$est_records   <- list()
  # Carries the normality verdict across the non-normal recursion below, where
  # .summarize_var_internal() re-enters itself via the force_ordinal path.
  .ternG_env$pending_norm  <- NULL

  .record_stats <- function(...) {
    .ternG_env$stats_records <- c(.ternG_env$stats_records, list(.tern_stat_row(...)))
    invisible(NULL)
  }
  .record_est <- function(...) {
    .ternG_env$est_records <- c(.ternG_env$est_records, list(.tern_est_row(...)))
    invisible(NULL)
  }
  # Retrieve (and clear) the normality verdict stashed before the recursion.
  .take_pending_norm <- function(var) {
    pn <- .ternG_env$pending_norm
    .ternG_env$pending_norm <- NULL
    if (is.null(pn) || !identical(pn$var, var)) {
      return(list(is_normal = NA, sw_p = NA_real_))
    }
    list(is_normal = pn$is_normal, sw_p = .tern_min_sw(pn$sw_p))
  }

  .summarize_var_internal <- function(df, var, force_ordinal = NULL, force_continuous = NULL, show_test = FALSE, round_intg = FALSE, round_decimal = NULL, show_total = FALSE) {

    # Rounding helper: integer mode, custom decimal, or default 1dp
    rd <- function(x) {
      if (round_intg) round_up_half(x, 0)
      else round(x, if (!is.null(round_decimal)) as.integer(round_decimal) else 1L)
    }

    # Count missings per group BEFORE the NA filter below (used when show_missing = TRUE)
    .missing_row <- function(result_tbl) {
      if (!isTRUE(show_missing)) return(result_tbl)
      miss_n   <- sapply(group_levels, function(g_lvl) {
        grp_data <- df[!is.na(df[[group_var]]) & df[[group_var]] == g_lvl, ]
        sum(is.na(grp_data[[var]]))
      })
      grp_n    <- sapply(group_levels, function(g_lvl) {
        sum(!is.na(df[[group_var]]) & df[[group_var]] == g_lvl)
      })
      miss_pct <- round(miss_n / grp_n * 100)
      # Only emit the row when at least one group has missing values
      if (!any(miss_n > 0)) return(result_tbl)
      mrow <- tibble(Variable = "Missing", .indent = 6L)
      for (g_lvl in group_levels) {
        mrow[[group_labels[g_lvl]]] <- paste0(miss_n[g_lvl], " (", miss_pct[g_lvl], "%)")
      }
      mrow$P      <- "-"
      mrow$.p_raw <- NA_real_
      if (show_test)  mrow$test      <- "-"
      if (OR_col)     mrow$OR        <- "-"
      if (show_test && OR_col) mrow$OR_method <- "-"
      if (show_total) {
        total_miss <- sum(miss_n)
        total_grp  <- sum(grp_n)
        mrow$Total <- paste0(total_miss, " (", round(total_miss / total_grp * 100), "%)")
      }
      bind_rows(result_tbl, mrow)
    }

    # ── Missingness column helper ─────────────────────────────────────────────
    # Appends missingness % column(s) to result_tbl using the raw (unfiltered)
    # data so that NA + string-NA values in df[[var]] are counted correctly.
    .add_miss_cols <- function(result_tbl) {
      if (isFALSE(show_missingness)) return(result_tbl)
      raw_vec <- df[[var]]
      is_miss <- .is_missing_value(raw_vec, missing_indicators)
      n_total <- length(is_miss)
      if (show_missingness == "total") {
        n_miss   <- sum(is_miss)
        pct      <- round(n_miss / n_total * 100)
        miss_str <- paste0(n_miss, " (", pct, "%)")
        result_tbl[["Missing, n (%)"]] <- ifelse(result_tbl$.indent == 6L, "-", miss_str)
      } else if (show_missingness == "group") {
        for (g_lvl in group_levels) {
          grp_mask    <- !is.na(df[[group_var]]) & df[[group_var]] == g_lvl
          is_miss_grp <- .is_missing_value(df[[var]][grp_mask], missing_indicators)
          n_miss_grp  <- sum(is_miss_grp)
          n_grp       <- sum(grp_mask)
          pct_grp     <- if (n_grp > 0L) round(n_miss_grp / n_grp * 100) else 0L
          miss_str_grp <- paste0(n_miss_grp, " (", pct_grp, "%)")
          result_tbl[[paste0("Miss. [", g_lvl, "]")]] <- ifelse(
            result_tbl$.indent == 6L, "-", miss_str_grp
          )
        }
      }
      result_tbl
    }

    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) return(NULL)
    v <- g[[var]]

    # Auto-detect binary numeric (0/1) as categorical Y/N
    # Skipped when the variable is listed in force_continuous
    if (is.numeric(v) && length(unique(stats::na.omit(v))) == 2 && all(stats::na.omit(v) %in% c(0, 1)) &&
        !(var %in% force_continuous)) {
      v <- factor(v, levels = c(0, 1), labels = c("N", "Y"))
      g[[var]] <- v
    }

    # ----- Categorical -----
    if (is.character(v) || is.factor(v)) {
      g[[var]] <- factor(g[[var]])
      tab <- table(g[[group_var]], g[[var]])
      # margin=1: % within each group (column %). margin=2: % within each level (row %).
      # We restore colnames explicitly from colnames(tab) because as.data.frame.matrix
      # uses its own `make.names` parameter (not `check.names`), so `check.names=FALSE`
      # is silently ignored and blank/special factor level names (e.g. "") get mangled
      # to "X" or "V1" by make.names(). Restoring preserves exact original names.
      .tab_col_names <- colnames(tab)
      if (percentage_compute == "row") {
        tab_pct <- as.data.frame.matrix(round(prop.table(tab, 2) * 100))
      } else {
        tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      }
      colnames(tab_pct) <- .tab_col_names
      tab_n   <- as.data.frame.matrix(tab)
      colnames(tab_n) <- .tab_col_names
      tab_total_n   <- colSums(tab)
      if (percentage_compute == "row") {
        # Row percentages: Total column is always 100% per level
        tab_total_pct <- setNames(rep(100L, length(tab_total_n)), names(tab_total_n))
      } else {
        tab_total_pct <- round(prop.table(tab_total_n) * 100)
      }

      # Cochran (1954) criterion: use Fisher's exact when any *expected* cell count < 5
      # Wrapped in tryCatch: chisq.test() errors on degenerate tables (e.g. single-level
      # variable after NA removal). Default TRUE (Fisher) so the test_result block below
      # handles it gracefully and returns "NA (insufficient variation)".
      fisher_flag <- tryCatch(
        any(suppressWarnings(stats::chisq.test(tab)$expected) < 5),
        error = function(e) TRUE
      )
      test_result <- tryCatch({
        if (fisher_flag) {
          # Fisher's exact is only safe for 2×2 tables. For larger tables the C-level
          # workspace algorithm can segfault (not catchable by tryCatch) when the table
          # has many levels or large marginal counts. Go directly to Monte Carlo simulation
          # for any table larger than 2×2; for 2×2, try exact first then fall back.
          is_large_table <- nrow(tab) > 2L || ncol(tab) > 2L
          ft_wrap <- if (is_large_table) {
            list(
              result = withr::with_seed(
                getOption("TernTables.seed", 42L),
                stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000L)
              ),
              simulated = TRUE
            )
          } else {
            tryCatch(
              list(result = stats::fisher.test(tab), simulated = FALSE),
              error = function(e) {
                list(
                  result = withr::with_seed(
                    getOption("TernTables.seed", 42L),
                    stats::fisher.test(tab, simulate.p.value = TRUE, B = 10000L)
                  ),
                  simulated = TRUE
                )
              }
            )
          }
          # Fisher's exact reports no test statistic or df.
          list(p_value = ft_wrap$result$p.value,
               statistic = NA_real_, df = NA_real_,
               test_name = if (ft_wrap$simulated) "Fisher exact (simulated)" else "Fisher exact",
               error = NULL)
        } else {
          cs <- stats::chisq.test(tab)
          list(p_value = cs$p.value, statistic = cs$statistic, df = cs$parameter,
               test_name = "Chi-squared", error = NULL)
        }
      }, error = function(e) {
        # Determine the reason for failure
        if (nrow(tab) < 2 || ncol(tab) < 2) {
          reason <- "insufficient variation"
        } else if (sum(tab) == 0) {
          reason <- "no observations"
        } else if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
          reason <- "empty cells"
        } else {
          reason <- "test failure"
        }
        list(p_value = NA_real_, statistic = NA_real_, df = NA_real_,
             test_name = if (fisher_flag) "Fisher exact" else "Chi-squared",
             error = reason)
      })
      
      
      # Raw odds-ratio record for the tidy side-channel. Populated from the same
      # objects that build the formatted OR string below (never recomputed).
      or_rec <- list(value = NA_real_, lcl = NA_real_, ucl = NA_real_,
                     fmt = NA_character_, method = NA_character_)

      # Determine if this should use simple format or hierarchical subheads
      # Always use simple format for Y/N variables or when insert_subheads is FALSE
      # Otherwise use hierarchical format for categorical variables
      upper_cols     <- toupper(colnames(tab))
      is_yes_no      <- all(c("Y", "N")   %in% upper_cols)
      is_yes_no_full <- all(c("YES", "NO") %in% upper_cols)
      is_binary <- is_yes_no || is_yes_no_full
      use_simple_format <- is_binary || !insert_subheads
      
      if (use_simple_format) {
        # Simple format: single row showing the positive/yes level
        if (is_yes_no) {
          ref_level <- colnames(tab)[toupper(colnames(tab)) == "Y"]
        } else if (is_yes_no_full) {
          ref_level <- colnames(tab)[toupper(colnames(tab)) == "YES"]
        } else {
          if ((factor_order == "levels" || factor_order == "mixed") && is.factor(g[[var]])) {
            # Factor: use first level that actually appears in the data
            available_levels <- levels(g[[var]])[levels(g[[var]]) %in% colnames(tab)]
            ref_level <- available_levels[1]
          } else if (factor_order == "mixed") {
            # Non-factor: 2-level → alphabetical first; 3+ → most frequent
            if (length(colnames(tab)) == 2) {
              ref_level <- sort(colnames(tab))[1]
            } else {
              ref_level <- names(sort(colSums(tab), decreasing = TRUE))[1]
            }
          } else {
            # "levels" non-factor or "frequency": use most frequent level
            ref_level <- names(sort(colSums(tab), decreasing = TRUE))[1]
          }
        }
        result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
        # Use integer indices to safely handle any level name (including blank "")
        ci_ref <- match(ref_level, colnames(tab_n))
        for (g_lvl in group_levels) {
          ri <- match(g_lvl, rownames(tab_n))
          result[[group_labels[g_lvl]]] <- paste0(
            tab_n[ri, ci_ref], " (", tab_pct[ri, ci_ref], "%)"
          )
        }

        if (!is.null(test_result$error)) {
          result$P      <- paste0("NA (", test_result$error, ")")
          result$.p_raw <- NA_real_
        } else {
          result$P      <- val_p_format(test_result$p_value, p_digits)
          result$.p_raw <- test_result$p_value
        }
        if (show_test) {
          result$test <- test_result$test_name
        }

        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          use_fisher_or <- (OR_method == "dynamic" && fisher_flag)
          if (OR_method == "dynamic" || OR_method == "wald") {
            or_ok <- FALSE
            if (use_fisher_or) {
              fisher_obj <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
              if (!is.null(fisher_obj)) {
                or_rec$value <- unname(fisher_obj$estimate)
                or_rec$lcl   <- fisher_obj$conf.int[1]
                or_rec$ucl   <- fisher_obj$conf.int[2]
                or_ok        <- TRUE
              }
              or_rec$method <- "Fisher"
            } else {
              or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
              if (!is.null(or_obj)) {
                or_rec$value <- or_obj[2, 1]
                or_rec$lcl   <- or_obj[2, 2]
                or_rec$ucl   <- or_obj[2, 3]
                or_ok        <- TRUE
              }
              or_rec$method <- "Wald"
            }
            or_rec$fmt <- if (or_ok) {
              sprintf("%.2f [%.2f\u2013%.2f]", or_rec$value, or_rec$lcl, or_rec$ucl)
            } else {
              "NA (calculation failed)"
            }
            result$OR <- or_rec$fmt
            if (show_test) result$OR_method <- or_rec$method
          }
        } else if (OR_col) {
          result$OR <- "-"
          if (show_test) result$OR_method <- "-"
        }
        if (show_total) {
          ti_ref <- match(ref_level, names(tab_total_n))
          result$Total <- paste0(tab_total_n[ti_ref], " (", tab_total_pct[ti_ref], "%)")
        }

      } else {
        # Hierarchical format: header + indented sub-categories
        # tab_total_n is already a vector of total counts per level
        if ((factor_order == "levels" || factor_order == "mixed") && is.factor(g[[var]])) {
          # Factor: always respect original factor level ordering
          sorted_levels <- levels(g[[var]])
          sorted_levels <- sorted_levels[sorted_levels %in% names(tab_total_n)]
        } else if (factor_order == "mixed") {
          # Non-factor: 2-level → alphabetical; 3+ → frequency
          available <- names(tab_total_n)
          if (length(available) == 2) {
            sorted_levels <- sort(available)
          } else {
            sorted_levels <- names(sort(tab_total_n, decreasing = TRUE))
          }
        } else {
          # "levels" non-factor fallback or "frequency": sort by frequency
          sorted_levels <- names(sort(tab_total_n, decreasing = TRUE))
        }
        
        # Create header row for the main variable (no data, just variable name)
        header_row <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
        for (g_lvl in group_levels) {
          header_row[[group_labels[g_lvl]]] <- ""
        }
        header_row$P      <- ""
        header_row$.p_raw <- NA_real_
        if (show_test) {
          header_row$test <- ""
        }
        if (OR_col) {
          header_row$OR <- ""
          if (show_test) header_row$OR_method <- ""
        }
        if (show_total) {
          header_row$Total <- ""
        }
        
        # For two-level categorical variables, compute OR once before the row loop.
        # Reference level: factor level 1, or alphabetical first for non-factors.
        hier_or_vals <- NULL
        if (OR_col && length(sorted_levels) == 2) {
          ref_level_hier <- if (is.factor(g[[var]])) {
            lvls <- levels(g[[var]])
            lvls[lvls %in% sorted_levels][1]
          } else {
            sort(sorted_levels)[1]
          }
          non_ref_level_hier <- sorted_levels[sorted_levels != ref_level_hier]
          # Reorder columns so reference level is first (groups stay as rows)
          tab_hier2 <- tab[, c(ref_level_hier, non_ref_level_hier), drop = FALSE]
          if (nrow(tab_hier2) == 2 && ncol(tab_hier2) == 2) {
            # Compute the raw estimate/CI first, then format from it, so the
            # tidy side-channel and the displayed string share one computation.
            or_num_hier <- tryCatch({
              if (OR_method == "dynamic" && fisher_flag) {
                fisher_obj2 <- stats::fisher.test(tab_hier2)
                c(unname(fisher_obj2$estimate), fisher_obj2$conf.int[1], fisher_obj2$conf.int[2])
              } else if (OR_method == "dynamic" || OR_method == "wald") {
                or_obj2 <- epitools::oddsratio(tab_hier2, method = "wald")$measure
                c(or_obj2[2, 1], or_obj2[2, 2], or_obj2[2, 3])
              } else {
                NULL
              }
            }, error = function(e) NA_real_)
            or_string_hier <- if (is.null(or_num_hier)) {
              "NA"
            } else if (length(or_num_hier) != 3L) {
              "NA (calculation failed)"
            } else {
              sprintf("%.2f [%.2f\u2013%.2f]", or_num_hier[1], or_num_hier[2], or_num_hier[3])
            }
            if (length(or_num_hier) == 3L) {
              or_rec$value <- or_num_hier[1]
              or_rec$lcl   <- or_num_hier[2]
              or_rec$ucl   <- or_num_hier[3]
            }
            or_rec$fmt    <- or_string_hier
            or_rec$method <- if (fisher_flag) "Fisher" else "Wald"
            hier_or_vals <- list(
              ref_level = ref_level_hier,
              or_string  = or_string_hier,
              or_method  = if (fisher_flag) "Fisher" else "Wald"
            )
          }
        }

        # Create sub-category rows (indented)
        sub_rows <- lapply(sorted_levels, function(level) {
          out <- tibble(Variable = level, .indent = 6)
          # Use integer indices to safely handle any level name (including blank "")
          ci <- match(level, colnames(tab_n))
          for (g_lvl in group_levels) {
            ri <- match(g_lvl, rownames(tab_n))
            val <- if (!is.na(ri) && !is.na(ci)) {
              paste0(tab_n[ri, ci], " (", tab_pct[ri, ci], "%)")
            } else {
              "NA (NA%)"
            }
            out[[group_labels[g_lvl]]] <- val
          }
          
          # Only first sub-row gets P value, others get "-"
          if (level == sorted_levels[1]) {
            if (!is.null(test_result$error)) {
              out$P      <- paste0("NA (", test_result$error, ")")
              out$.p_raw <- NA_real_
            } else {
              out$P      <- val_p_format(test_result$p_value, p_digits)
              out$.p_raw <- test_result$p_value
            }
            if (show_test) {
              out$test <- test_result$test_name
            }
          } else {
            out$P      <- "-"
            out$.p_raw <- NA_real_
            if (show_test) {
              out$test <- "-"
            }
          }
          
          if (OR_col) {
            if (!is.null(hier_or_vals)) {
              # Two-level categorical: reference row shows "1.00 (ref.)", other row shows computed OR
              out$OR <- if (level == hier_or_vals$ref_level) "1.00 (ref.)" else hier_or_vals$or_string
              if (show_test) out$OR_method <- hier_or_vals$or_method
            } else {
              out$OR <- "-"
              if (show_test) out$OR_method <- "-"
            }
          }
          if (show_total) {
            ti <- match(level, names(tab_total_n))
            out$Total <- paste0(tab_total_n[ti], " (", tab_total_pct[ti], "%)")
          }
          out
        })
        
        # Combine header and sub-rows
        result <- bind_rows(list(header_row), sub_rows)
      }

      # ── Adjusted standardized residuals (categorical post-hoc) ─────────────
      # Applied when categorical_posthoc = TRUE, 3+ groups, and omnibus p < 0.05.
      # Haberman's adjusted residuals follow N(0,1); |z| > 1.96 ↔ α = 0.05.
      # No additional multiple-comparisons correction is needed — the ±1.96
      # threshold is derived directly from the omnibus chi-sq distribution.
      if (categorical_posthoc && n_levels >= 3L && is.null(test_result$error) &&
          !is.na(test_result$p_value) && test_result$p_value < 0.05) {
        stdres <- tryCatch(
          suppressWarnings(stats::chisq.test(tab)$stdres),
          error = function(e) NULL
        )
        if (!is.null(stdres) && is.matrix(stdres) && !anyNA(dim(stdres)) && nrow(stdres) >= 2L) {
          if (use_simple_format) {
            # Single-row display: annotate the ref_level cell for each group
            for (g_lvl in group_levels) {
              if (is.na(g_lvl) || !g_lvl %in% rownames(stdres) || is.na(ref_level) || !ref_level %in% colnames(stdres)) next
              val <- tryCatch(stdres[g_lvl, ref_level], error = function(e) NA_real_)
              if (!is.na(val) && abs(val) > 1.96)
                result[[group_labels[g_lvl]]] <- paste0(result[[group_labels[g_lvl]]], "*")
            }
          } else {
            # Hierarchical display: annotate each group × level cell (skip header row)
            for (i in seq_len(nrow(result))) {
              if (!isTRUE(result$.indent[i] == 6L)) next  # header rows have .indent == 2
              level <- result$Variable[i]
              if (is.na(level) || !level %in% colnames(stdres)) next
              for (g_lvl in group_levels) {
                if (is.na(g_lvl) || !g_lvl %in% rownames(stdres)) next
                val <- tryCatch(stdres[g_lvl, level], error = function(e) NA_real_)
                if (!is.na(val) && abs(val) > 1.96)
                  result[[group_labels[g_lvl]]][i] <- paste0(result[[group_labels[g_lvl]]][i], "*")
              }
            }
          }
          .ternG_env$cat_posthoc_ran_display <- c(
            .ternG_env$cat_posthoc_ran_display, result$Variable[1]
          )
          if (grepl("Fisher", test_result$test_name, ignore.case = TRUE))
            .ternG_env$cat_posthoc_fisher_display <- c(
              .ternG_env$cat_posthoc_fisher_display, result$Variable[1]
            )
        }
      }

      if (grepl("simulated", test_result$test_name, fixed = FALSE))
        .ternG_env$fisher_sim_display <- c(.ternG_env$fisher_sim_display, result$Variable[1])

      # ── Tidy side-channel: categorical ──────────────────────────────────────
      .record_stats(
        variable  = var,
        label     = result$Variable[1],
        type      = "categorical",
        stat_type = "n_pct",
        n         = nrow(g),
        n_missing = nrow(df) - nrow(g),
        n_levels  = ncol(tab),
        test      = test_result$test_name,
        statistic = test_result$statistic,
        df        = test_result$df,
        p_value   = test_result$p_value,
        p_fmt     = if (!is.null(test_result$error)) {
          paste0("NA (", test_result$error, ")")
        } else {
          val_p_format(test_result$p_value, p_digits)
        },
        test_note = if (is.null(test_result$error)) NA_character_ else test_result$error,
        or_value  = or_rec$value,
        or_lcl    = or_rec$lcl,
        or_ucl    = or_rec$ucl,
        or_fmt    = or_rec$fmt,
        or_method = or_rec$method
      )
      # Every level is emitted here, including levels the display collapses away
      # (a binary Y/N variable renders only the "Y" row).
      for (g_lvl in rownames(tab)) {
        for (lvl in colnames(tab)) {
          ri <- match(g_lvl, rownames(tab_n)); ci <- match(lvl, colnames(tab_n))
          .record_est(
            variable = var, label = result$Variable[1], group = g_lvl, level = lvl,
            n = tab_n[ri, ci], pct = tab_pct[ri, ci],
            value_fmt = paste0(tab_n[ri, ci], " (", tab_pct[ri, ci], "%)")
          )
        }
      }
      for (lvl in names(tab_total_n)) {
        ti <- match(lvl, names(tab_total_n))
        .record_est(
          variable = var, label = result$Variable[1], group = "Total", level = lvl,
          n = tab_total_n[[ti]], pct = tab_total_pct[[ti]],
          value_fmt = paste0(tab_total_n[ti], " (", tab_total_pct[ti], "%)")
        )
      }
      return(.add_miss_cols(.missing_row(result)))
    }

    # ----- Force ordinal -----
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      # Raw quantiles are kept alongside the rounded display values so the tidy
      # side-channel can report un-rounded statistics without recomputing them.
      stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
        Q1_raw  = quantile(.data[[var]], 0.25, na.rm = TRUE),
        med_raw = median(.data[[var]], na.rm = TRUE),
        Q3_raw  = quantile(.data[[var]], 0.75, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(Q1 = rd(.data$Q1_raw), med = rd(.data$med_raw), Q3 = rd(.data$Q3_raw))
      result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
          paste0(val$med, " [", val$Q1, "\u2013", val$Q3, "]")
        } else {
          "NA [NA\u2013NA]"
        }
      }
      
      test_result <- tryCatch({
        if (n_levels == 2) {
          ht <- stats::wilcox.test(g[[var]] ~ g[[group_var]])
          # Wilcoxon reports W but no degrees of freedom.
          list(p_value = ht$p.value, statistic = ht$statistic, df = NA_real_,
               test_name = "Wilcoxon rank-sum", error = NULL)
        } else {
          ht <- stats::kruskal.test(g[[var]] ~ g[[group_var]])
          list(p_value = ht$p.value, statistic = ht$statistic, df = ht$parameter,
               test_name = "Kruskal-Wallis", error = NULL)
        }
      }, error = function(e) {
        # Determine reason for test failure
        group_sizes <- table(g[[group_var]])
        if (any(group_sizes < 2)) {
          reason <- "insufficient group sizes"
        } else if (length(unique(g[[var]])) < 2) {
          reason <- "no variation in values"
        } else {
          reason <- "test failure"
        }
        test_name <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
        list(p_value = NA_real_, statistic = NA_real_, df = NA_real_,
             test_name = test_name, error = reason)
      })
      
      if (!is.null(test_result$error)) {
        result$P      <- paste0("NA (", test_result$error, ")")
        result$.p_raw <- NA_real_
      } else {
        result$P      <- val_p_format(test_result$p_value, p_digits)
        result$.p_raw <- test_result$p_value
      }
      if (show_test) {
        result$test <- test_result$test_name
      }
      
      if (OR_col) result$OR <- "-"
      # Computed unconditionally so the display cell and the tidy Total record
      # come from a single computation.
      val_total <- g %>% dplyr::summarise(
        Q1_raw  = quantile(.data[[var]], 0.25, na.rm = TRUE),
        med_raw = median(.data[[var]], na.rm = TRUE),
        Q3_raw  = quantile(.data[[var]], 0.75, na.rm = TRUE)) %>%
        dplyr::mutate(Q1 = rd(.data$Q1_raw), med = rd(.data$med_raw), Q3 = rd(.data$Q3_raw))
      if (show_total) {
        result$Total <- paste0(val_total$med, " [", val_total$Q1, "\u2013", val_total$Q3, "]")
      }
      if (print_normality) {
        for (g_lvl in group_levels) {
          sw_p <- tryCatch({
            x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
            if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          }, error = function(e) NA_real_)
          result[[paste0("SW_p_", g_lvl)]] <- formatC(sw_p, format = "f", digits = 4)
        }
      }
      # Post-hoc pairwise testing for 3+ groups: Dunn's test with Holm correction
      # Only runs when omnibus (Kruskal-Wallis) is significant at alpha = 0.05
      if (post_hoc && n_levels >= 3L && is.null(test_result$error) &&
          !is.na(test_result$p_value) && test_result$p_value < 0.05) {
        # Sanitize column names for rstatix (spaced/special names break its formula parser)
          safe_var      <- make.names(var)
          safe_group    <- make.names(group_var)
          g_safe        <- g
          names(g_safe)[names(g_safe) == var]       <- safe_var
          names(g_safe)[names(g_safe) == group_var] <- safe_group
          ph_formula <- stats::as.formula(paste0("`", safe_var, "` ~ `", safe_group, "`"))
          ph_res <- tryCatch(
            rstatix::dunn_test(g_safe, ph_formula, p.adjust.method = "holm"),
            error = function(e) NULL
          )
          if (!is.null(ph_res)) {
            centers_df  <- g %>% dplyr::group_by(.data[[group_var]]) %>%
              dplyr::summarise(ctr = median(.data[[var]], na.rm = TRUE), .groups = "drop")
            centers_vec <- setNames(centers_df$ctr, as.character(centers_df[[group_var]]))
            ph_df <- data.frame(
              group1 = as.character(ph_res$group1),
              group2 = as.character(ph_res$group2),
              p.adj  = ph_res$p.adj,
              stringsAsFactors = FALSE
            )
            cld <- .compute_cld(as.character(group_levels), ph_df)
            for (g_lvl in group_levels) {
              sup <- .superscript_letters(cld[as.character(g_lvl)])
              if (!is.null(sup) && nzchar(sup))
                result[[group_labels[g_lvl]]] <- paste0(result[[group_labels[g_lvl]]], sup)
            }
            .ternG_env$posthoc_ran_display <- c(.ternG_env$posthoc_ran_display, result$Variable[1])
          }
      }
      if (grepl("simulated", test_result$test_name, fixed = FALSE))
        .ternG_env$fisher_sim_display <- c(.ternG_env$fisher_sim_display, result$Variable[1])

      # ── Tidy side-channel: ordinal / non-normal continuous ──────────────────
      # `pending` carries the normality verdict from the caller when this branch
      # was reached by the non-normal recursion; it is empty for force_ordinal
      # variables, whose distribution is never assessed.
      pending <- .take_pending_norm(var)
      .record_stats(
        variable  = var,
        label     = result$Variable[1],
        type      = "continuous",
        stat_type = "median_iqr",
        n         = nrow(g),
        n_missing = nrow(df) - nrow(g),
        test      = test_result$test_name,
        statistic = test_result$statistic,
        df        = test_result$df,
        p_value   = test_result$p_value,
        p_fmt     = if (!is.null(test_result$error)) {
          paste0("NA (", test_result$error, ")")
        } else {
          val_p_format(test_result$p_value, p_digits)
        },
        test_note = if (is.null(test_result$error)) NA_character_ else test_result$error,
        is_normal = pending$is_normal,
        sw_p      = pending$sw_p
      )
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        if (nrow(val) != 1) next
        .record_est(
          variable = var, label = result$Variable[1], group = g_lvl,
          n = sum(g[[group_var]] == g_lvl, na.rm = TRUE),
          median = val$med_raw, q1 = val$Q1_raw, q3 = val$Q3_raw,
          value_fmt = paste0(val$med, " [", val$Q1, "\u2013", val$Q3, "]")
        )
      }
      .record_est(
        variable = var, label = result$Variable[1], group = "Total",
        n = nrow(g), median = val_total$med_raw, q1 = val_total$Q1_raw, q3 = val_total$Q3_raw,
        value_fmt = paste0(val_total$med, " [", val_total$Q1, "\u2013", val_total$Q3, "]")
      )
      return(.add_miss_cols(.missing_row(result)))
    }

    # ----- Normality assessment -----
    sw_p_all <- list()
    is_normal <- TRUE

    # force_normal: bypass all normality assessment for listed variables
    # force_ordinal takes priority if a variable appears in both
    if (!is.null(force_normal) && var %in% force_normal &&
        (is.null(force_ordinal) || !var %in% force_ordinal)) {
      .ternG_env$numeric_vars_tested <- .ternG_env$numeric_vars_tested + 1L
      is_normal <- TRUE
    # Handle different consider_normality options
    } else if (consider_normality == "FORCE") {
      # Test normality for baseline statistics but force all to be treated as ordinal
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      baseline_normal <- all(sw_p_all > 0.05, na.rm = TRUE)
      
      # Track baseline normality results for reporting
      .ternG_env$numeric_vars_tested <- .ternG_env$numeric_vars_tested + 1L
      if (!baseline_normal) {
        .ternG_env$numeric_vars_failed <- .ternG_env$numeric_vars_failed + 1L
      }
      
      # Force all to be treated as ordinal regardless of normality
      is_normal <- FALSE
    } else if (consider_normality == "ROBUST") {
      # ROBUST: four-gate decision tree — see R/utils_normality.R
      group_vals <- lapply(group_levels, function(g_lvl) {
        g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
      })
      names(group_vals) <- group_levels

      .ternG_env$numeric_vars_tested <- .ternG_env$numeric_vars_tested + 1L

      robust_result <- .robust_normality(group_vals)
      is_normal      <- robust_result$is_normal
      if (!is.null(robust_result$sw_pvalues)) sw_p_all <- robust_result$sw_pvalues
      if (!is_normal) .ternG_env$numeric_vars_failed <- .ternG_env$numeric_vars_failed + 1L
    } else if (isTRUE(consider_normality)) {
      # TRUE: Shapiro-Wilk only (conservative at large n)
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      is_normal <- all(!is.na(sw_p_all) & sw_p_all > 0.05)

      .ternG_env$numeric_vars_tested <- .ternG_env$numeric_vars_tested + 1L
      if (!is_normal) {
        .ternG_env$numeric_vars_failed <- .ternG_env$numeric_vars_failed + 1L
      }
    } else {
      # consider_normality = FALSE: still run Shapiro-Wilk for informational reporting,
      # but always treat variables as normally distributed for display.
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA_real_, n_levels))
      .ternG_env$numeric_vars_tested <- .ternG_env$numeric_vars_tested + 1L
      if (!all(sw_p_all > 0.05, na.rm = TRUE)) .ternG_env$numeric_vars_failed <- .ternG_env$numeric_vars_failed + 1L
    }

    if (!is_normal) {
      # Stash the normality verdict so the ordinal branch below (re-entered via
      # this recursion) can record it in the tidy frame — it is not otherwise
      # recoverable once we hand off to the force_ordinal path.
      .ternG_env$pending_norm <- list(var = var, is_normal = FALSE, sw_p = sw_p_all)
      # force_continuous must be forwarded: without it the recursion re-enters the
      # automatic binary 0/1 detection and converts the variable to categorical
      # Y/N, which is precisely what force_continuous exists to prevent.
      return(.summarize_var_internal(df, var = var, force_ordinal = var, force_continuous = force_continuous, show_test = show_test, round_intg = round_intg, round_decimal = round_decimal, show_total = show_total))
    }

    # ----- Normally distributed numeric -----
    result <- tibble(Variable = .clean_variable_name_for_header(var), .indent = 2)
    stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE), .groups = "drop")

    for (g_lvl in group_levels) {
      val <- stats %>% filter(.data[[group_var]] == g_lvl)
      result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
        paste0(rd(val$mean), " \u00b1 ", rd(val$sd))
      } else {
        "NA \u00b1 NA"
      }
    }

    test_result <- tryCatch({
      if (n_levels == 2) {
        ht <- stats::t.test(g[[var]] ~ g[[group_var]], var.equal = FALSE)
        list(p_value = ht$p.value, statistic = ht$statistic, df = ht$parameter,
             test_name = "Welch t-test", error = NULL)
      } else {
        ht <- stats::oneway.test(g[[var]] ~ g[[group_var]], var.equal = FALSE)
        # oneway.test reports numerator and denominator df.
        list(p_value = ht$p.value, statistic = ht$statistic,
             df = ht$parameter[1], df2 = ht$parameter[2],
             test_name = "Welch ANOVA", error = NULL)
      }
    }, error = function(e) {
      # Determine reason for test failure
      group_sizes <- table(g[[group_var]])
      if (any(group_sizes < 2)) {
        reason <- "insufficient group sizes"
      } else if (all(is.na(g[[var]]))) {
        reason <- "all values missing"
      } else if (stats::var(g[[var]], na.rm = TRUE) == 0) {
        reason <- "no variation in values"
      } else {
        reason <- paste0("test failure: ", conditionMessage(e))
      }
      test_name <- if (n_levels == 2) "Welch t-test" else "Welch ANOVA"
      list(p_value = NA_real_, statistic = NA_real_, df = NA_real_,
           test_name = test_name, error = reason)
    })
    
    if (!is.null(test_result$error)) {
      result$P      <- paste0("NA (", test_result$error, ")")
      result$.p_raw <- NA_real_
    } else {
      result$P      <- val_p_format(test_result$p_value, p_digits)
      result$.p_raw <- test_result$p_value
    }
    if (show_test) {
      result$test <- test_result$test_name
    }
    
    if (OR_col) result$OR <- "-"
    # Computed unconditionally so the display cell and the tidy Total record
    # come from a single computation.
    val_total <- g %>% dplyr::summarise(mean = mean(.data[[var]], na.rm = TRUE), sd = sd(.data[[var]], na.rm = TRUE))
    if (show_total) {
      result$Total <- paste0(rd(val_total$mean), " \u00b1 ", rd(val_total$sd))
    }

    if (print_normality && length(sw_p_all) > 0) {
      for (nm in names(sw_p_all)) {
        result[[nm]] <- formatC(sw_p_all[[nm]], format = "f", digits = 4)
      }
    }
    # Post-hoc pairwise testing for 3+ groups: Games-Howell
    # Only runs when omnibus (Welch ANOVA) is significant at alpha = 0.05
    if (post_hoc && n_levels >= 3L && is.null(test_result$error) &&
        !is.na(test_result$p_value) && test_result$p_value < 0.05) {
        # Sanitize column names for rstatix (spaced/special names break its formula parser)
        safe_var      <- make.names(var)
        safe_group    <- make.names(group_var)
        g_safe        <- g
        names(g_safe)[names(g_safe) == var]       <- safe_var
        names(g_safe)[names(g_safe) == group_var] <- safe_group
        gh_formula <- stats::as.formula(paste0("`", safe_var, "` ~ `", safe_group, "`"))
        ph_res <- tryCatch(
          rstatix::games_howell_test(g_safe, gh_formula),
          error = function(e) NULL
        )
        if (!is.null(ph_res)) {
          centers_vec <- setNames(stats$mean, as.character(stats[[group_var]]))
          ph_df <- data.frame(
            group1 = as.character(ph_res$group1),
            group2 = as.character(ph_res$group2),
            p.adj  = ph_res$p.adj,
            stringsAsFactors = FALSE
          )
          cld <- .compute_cld(as.character(group_levels), ph_df)
          for (g_lvl in group_levels) {
            sup <- .superscript_letters(cld[as.character(g_lvl)])
            if (!is.null(sup) && nzchar(sup))
              result[[group_labels[g_lvl]]] <- paste0(result[[group_labels[g_lvl]]], sup)
          }
          .ternG_env$posthoc_ran_display <- c(.ternG_env$posthoc_ran_display, result$Variable[1])
        }
    }

    # ── Tidy side-channel: normally-distributed continuous ────────────────────
    .record_stats(
      variable  = var,
      label     = result$Variable[1],
      type      = "continuous",
      stat_type = "mean_sd",
      n         = nrow(g),
      n_missing = nrow(df) - nrow(g),
      test      = test_result$test_name,
      statistic = test_result$statistic,
      df        = test_result$df,
      df2       = test_result$df2,
      p_value   = test_result$p_value,
      p_fmt     = if (!is.null(test_result$error)) {
        paste0("NA (", test_result$error, ")")
      } else {
        val_p_format(test_result$p_value, p_digits)
      },
      test_note = if (is.null(test_result$error)) NA_character_ else test_result$error,
      is_normal = TRUE,
      sw_p      = .tern_min_sw(sw_p_all)
    )
    for (g_lvl in group_levels) {
      val <- stats %>% filter(.data[[group_var]] == g_lvl)
      if (nrow(val) != 1) next
      .record_est(
        variable = var, label = result$Variable[1], group = g_lvl,
        n = sum(g[[group_var]] == g_lvl, na.rm = TRUE),
        mean = val$mean, sd = val$sd,
        value_fmt = paste0(rd(val$mean), " \u00b1 ", rd(val$sd))
      )
    }
    .record_est(
      variable = var, label = result$Variable[1], group = "Total",
      n = nrow(g), mean = val_total$mean, sd = val_total$sd,
      value_fmt = paste0(rd(val_total$mean), " \u00b1 ", rd(val_total$sd))
    )
    return(.add_miss_cols(.missing_row(result)))
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) .summarize_var_internal(data, v, force_ordinal, force_continuous, show_test, round_intg, round_decimal, show_total)))
    cli::cli_alert_info("Multi-level categorical variables occupy more than one row in the output table.")
    result
  })

  # ── Assemble the tidy side-channel frames ─────────────────────────────────
  # Records were appended in `vars` order, one per summarized variable, so these
  # frames stay row-aligned with the display table's variable sequence.
  stats_tbl <- .tern_bind_stats(.ternG_env$stats_records)
  est_tbl   <- .tern_bind_est(.ternG_env$est_records)


  # -- Standardize group headers and enforce final column order (level-agnostic)
  # Keep the group column names with "n = x" format as requested

  # Collect any normality cols
  normality_cols <- grep("^SW_p_", names(out_tbl), value = TRUE)

  # The group columns should already have the "n = x" format from group_labels
  existing_group_cols <- intersect(unname(group_labels), names(out_tbl))

  # For "group" missingness mode interleave miss columns after each group column
  if (identical(show_missingness, "group") && n_levels >= 2L) {
    miss_col_names <- paste0("Miss. [", group_levels, "]")
    group_section  <- as.vector(rbind(existing_group_cols, miss_col_names))
  } else {
    group_section <- existing_group_cols
  }
  miss_total_col <- if (identical(show_missingness, "total")) "Missing, n (%)" else character(0)

  # Desired column order (keeping group columns with n = x format)
  if (show_test) {
    desired <- c("Variable", group_section, "Total", "OR", "p", "test", "OR_method", normality_cols)
  } else {
    desired <- c("Variable", group_section, "Total", "OR", "p", normality_cols)
    # Remove test and OR_method columns if they exist
    if ("test" %in% names(out_tbl)) {
      out_tbl <- dplyr::select(out_tbl, -test)
    }
    if ("OR_method" %in% names(out_tbl)) {
      out_tbl <- dplyr::select(out_tbl, -OR_method)
    }
  }

  # Reorder: put desired first (when they exist), then everything else
  out_tbl <- dplyr::select(out_tbl, dplyr::any_of(desired), dplyr::everything())
  
  # Rename Total column to include N with uppercase N and line break
  if ("Total" %in% names(out_tbl)) {
    names(out_tbl)[names(out_tbl) == "Total"] <- paste0("Total\n(N = ", total_n, ")")
  }

  # ── Benjamini-Hochberg FDR correction ─────────────────────────────────────
  if (isTRUE(p_adjust) && ".p_raw" %in% names(out_tbl)) {
    raw_vals <- out_tbl[[".p_raw"]]
    adj_idx  <- which(!is.na(raw_vals))
    if (length(adj_idx) > 0) {
      if (length(adj_idx) < 2) {
        cli::cli_alert_info(
          "p_adjust = TRUE: only {length(adj_idx)} P value found in the correction pool. \\
           BH adjustment requires multiple tests to be meaningful; \\
           the reported value is unchanged from the raw P value."
        )
      }
      fdr_vals <- p.adjust(raw_vals[adj_idx], method = "BH")
      fdr_fmt  <- vapply(fdr_vals, val_p_format, character(1), digits = p_digits)
      # Same correction pool, one computation: `.p_raw` is non-NA exactly once
      # per variable, matching the tidy frame's one-row-per-variable p_value.
      stat_idx <- which(!is.na(stats_tbl$p_value))
      if (length(stat_idx) == length(fdr_vals)) {
        stats_tbl$p_adjusted[stat_idx]     <- fdr_vals
        stats_tbl$p_adjusted_fmt[stat_idx] <- fdr_fmt
      }
      # Template from P column: preserves "-" (sub-rows) and "" (header rows) as-is
      fdr_col         <- out_tbl[["P"]]
      fdr_col[adj_idx] <- fdr_fmt
      if (p_adjust_display == "fdr_only") {
        out_tbl[["P"]] <- fdr_col
        names(out_tbl)[names(out_tbl) == "P"] <- "P value (FDR corrected)"
      } else {
        names(out_tbl)[names(out_tbl) == "P"] <- "P value"
        out_tbl <- dplyr::mutate(out_tbl, `P value (FDR corrected)` = fdr_col,
                                 .after = dplyr::all_of("P value"))
      }
    }
  }
  out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".p_raw"))

  # ── Drop P and associated columns when show_p = FALSE ─────────────────────
  if (!isTRUE(show_p)) {
    out_tbl <- dplyr::select(
      out_tbl,
      -dplyr::any_of(c("P", "P value", "P value (FDR corrected)", "OR", "OR_method")),
      -dplyr::starts_with("SW_p_")
    )
  }

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)

  # Extract accumulator values from environment for reporting
  numeric_vars_tested        <- .ternG_env$numeric_vars_tested
  numeric_vars_failed        <- .ternG_env$numeric_vars_failed
  posthoc_ran_display        <- .ternG_env$posthoc_ran_display
  cat_posthoc_ran_display    <- .ternG_env$cat_posthoc_ran_display
  cat_posthoc_fisher_display <- .ternG_env$cat_posthoc_fisher_display
  fisher_sim_display         <- .ternG_env$fisher_sim_display

  # Write methods document if requested
  if (methods_doc) {
    write_methods_doc(out_tbl, methods_filename, n_levels = n_levels, OR_col = OR_col, OR_method = OR_method, source = "ternG", post_hoc = post_hoc, categorical_posthoc = categorical_posthoc, cat_posthoc_fisher_vars = cat_posthoc_fisher_display, show_missingness = show_missingness, missing_indicators = missing_indicators, p_adjust = p_adjust, open_doc = open_doc, citation = citation, font_family = font_family)
  }

  # -- Report normality test results -----------------------------------------
  if (numeric_vars_tested > 0) {
    numeric_vars_passed <- numeric_vars_tested - numeric_vars_failed
    passed_pct  <- round((numeric_vars_passed / numeric_vars_tested) * 100, 1)
    param_test    <- if (n_levels == 2) "Welch's independent samples t-test" else "Welch's one-way ANOVA"
    nonparam_test <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"

    cli::cli_rule(left = "Normality Assessment (Shapiro-Wilk) \u2014 ternG")
    if (consider_normality == "FORCE") {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = 'FORCE': all continuous variables \u2192 median [IQR], tested with {nonparam_test}")
    } else if (consider_normality == "ROBUST") {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} routed to parametric ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Routing: skewness>2 \u2192 non-param; all-n\u226530 \u2192 CLT parametric; else Shapiro-Wilk",
        ">" = "Parametric   \u2192 mean \u00b1 SD, tested with {param_test}",
        ">" = "Non-parametric \u2192 median [IQR], tested with {nonparam_test}"
      ))
    } else if (isTRUE(consider_normality)) {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Normally distributed \u2192 mean \u00b1 SD, tested with {param_test}",
        ">" = "Non-normal           \u2192 median [IQR], tested with {nonparam_test}"
      ))
    } else {
      cli::cli_alert_info("{numeric_vars_passed} of {numeric_vars_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = FALSE: all continuous variables \u2192 mean \u00b1 SD, tested with {param_test}")
    }
  }

  # ── Missingness column report ─────────────────────────────────────────────
  if (!isFALSE(show_missingness)) {
    mode_desc <- if (show_missingness == "total") {
      "one column appended at far right"
    } else {
      paste0(n_levels, " columns interleaved after each group column")
    }
    miss_vals <- if (!is.null(missing_indicators)) missing_indicators else .tern_missing_strings()
    miss_str_list <- paste0('"', paste(miss_vals, collapse = '", "'), '"')
    indicator_note <- if (!is.null(missing_indicators)) " (custom list)" else " (ternP defaults)"
    cli::cli_rule(left = "Missingness columns added \u2014 ternG")
    cli::cli_alert_info("Mode: \"{show_missingness}\" \u2014 {mode_desc}")
    cli::cli_alert_info("Values treated as missing: NA + {miss_str_list}{indicator_note}")
  }

  # ── Categorical post-hoc + Fisher's exact warning ────────────────────────
  if (length(cat_posthoc_fisher_display) > 0L) {
    fisher_cat_vars <- paste(unique(cat_posthoc_fisher_display), collapse = ", ")
    cli::cli_rule(left = "Categorical Post-hoc Note \u2014 ternG")
    cli::cli_alert_warning("Fisher\u2019s exact was the omnibus test for: {fisher_cat_vars}.")
    cli::cli_alert_warning(paste0(
      "Adjusted standardized residuals (Haberman\u2019s method) were derived from the ",
      "global chi-squared contingency table \u2014 no Fisher\u2019s exact equivalent exists. ",
      "Results may be less reliable when expected cell counts are very small."
    ))
  }

  # Insert category header rows if specified
  # Replace "0 (NaN%)" with "-" for structurally impossible cells
  # (e.g. a subgroup that cannot logically have any observations in a given column)
  # Also replace "0 (0%)" when zero_to_dash = TRUE.
  out_tbl <- out_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ {
      x <- gsub("0 \\(NaN%\\)", "-", .x)
      if (isTRUE(zero_to_dash)) gsub("0 \\(0%\\)", "-", x) else x
    }))

  # Apply smart variable name cleaning if requested
  if (smart_rename) {
    for (i in seq_len(nrow(out_tbl))) {
      # Factor level sub-rows (.indent == 6) are user data — never run through
      # name-cleaning rules or their casing/content will be corrupted.
      if (out_tbl$.indent[i] == 6) next
      current_var <- out_tbl$Variable[i]
      if (grepl("^\\s+", current_var)) {
        padding <- stringr::str_extract(current_var, "^\\s+")
        trimmed_var <- trimws(current_var)
        if (grepl(": [A-Za-z0-9]+$", trimmed_var)) {
          parts <- strsplit(trimmed_var, ": ")[[1]]
          cleaned_var <- paste0(.apply_cleaning_rules(parts[1]), ": ", parts[2])
        } else {
          cleaned_var <- .apply_cleaning_rules(trimmed_var)
        }
        out_tbl$Variable[i] <- paste0(padding, cleaned_var)
      } else {
        out_tbl$Variable[i] <- .apply_cleaning_rules(current_var)
      }
    }
    # Keep the tidy frames' display labels in step with the rendered table.
    # Variable header rows never carry leading padding, so the simple branch of
    # the loop above is the only one that applies here.
    if (nrow(stats_tbl) > 0L)
      stats_tbl$label <- vapply(stats_tbl$label, .apply_cleaning_rules, character(1), USE.NAMES = FALSE)
    if (nrow(est_tbl) > 0L)
      est_tbl$label <- vapply(est_tbl$label, .apply_cleaning_rules, character(1), USE.NAMES = FALSE)
  }

  # Save with .indent intact for ternB multi-table export metadata
  out_tbl_with_indent <- out_tbl

  # Export to Word AFTER smart_rename so docx gets clean names
  # Auto-prepend post-hoc footnote only when at least one variable actually ran post-hoc
  # CLD note: positioned after abbreviations and before symbols in the footnote block
  posthoc_note <- NULL
  if (length(posthoc_ran_display) > 0L) {
    vars_listed <- paste(unique(posthoc_ran_display), collapse = ", ")
    posthoc_note <- paste0(
      "Superscript letters indicate pairwise post-hoc comparisons (",
      vars_listed,
      "; \u03b1\u00a0=\u00a00.05); groups sharing a letter are not significantly different."
    )
  }
  # Categorical residuals note: only shown when residuals were actually applied
  cat_posthoc_note <- NULL
  if (length(cat_posthoc_ran_display) > 0L) {
    cat_vars_listed <- paste(unique(cat_posthoc_ran_display), collapse = ", ")
    cat_posthoc_note <- paste0(
      "Asterisk (*) indicates an adjusted standardized residual exceeding \u00b11.96 (",
      cat_vars_listed,
      "; \u03b1\u00a0=\u00a00.05) \u2014 a significant deviation from expected frequencies following a significant omnibus test."
    )
  }
  # Merge continuous CLD note and categorical residuals note for the posthoc_footnote slot
  combined_posthoc_note <- paste(
    Filter(Negate(is.null), list(posthoc_note, cat_posthoc_note)),
    collapse = " "
  )
  combined_posthoc_note <- if (nchar(trimws(combined_posthoc_note)) == 0L) NULL else combined_posthoc_note

  # Other auto-notes (Fisher simulation, show_missing) go into table_footnote slot
  effective_footnote <- table_footnote
  notes <- character(0)
  if (length(fisher_sim_display) > 0L) {
    sim_vars <- paste(unique(fisher_sim_display), collapse = ", ")
    notes <- c(notes, paste0(
      "Fisher\u2019s exact test with Monte Carlo simulation (B\u00a0=\u00a010,000; seed\u00a0",
      getOption("TernTables.seed", 42L),
      ") was used where exact enumeration was computationally infeasible: ",
      sim_vars, "."
    ))
  }
  if (isTRUE(show_missing)) {
    notes <- c(notes, "Missing: n (%) of missing observations per group.")
  }
  if (length(notes) > 0L)
    effective_footnote <- if (is.null(table_footnote)) notes else c(notes, table_footnote)

  # Auto-append bold-convention note to abbreviation_footnote
  # The p-value column is named "P" in out_tbl (renamed to "P value" only inside word_export).
  # FDR-corrected variant is "P value (FDR corrected)".
  has_p_col  <- any(c("P", "P value (FDR corrected)") %in% colnames(out_tbl))
  has_or_col <- isTRUE(OR_col) && "OR" %in% colnames(out_tbl)
  bold_note  <- if (has_p_col && has_or_col) {
    "Bold values indicate statistical significance (p\u00a0<\u00a00.05); bold OR indicates 95% CI excludes 1."
  } else if (has_p_col) {
    "Bold p-values indicate statistical significance (p\u00a0<\u00a00.05)."
  } else {
    NULL
  }
  effective_abbr <- if (!is.null(bold_note)) {
    if (is.null(abbreviation_footnote)) bold_note else c(abbreviation_footnote, bold_note)
  } else {
    abbreviation_footnote
  }

  if (!is.null(output_docx)) word_export(out_tbl, output_docx, round_intg = round_intg, round_decimal = round_decimal, font_size = table_font_size, category_start = category_start, plain_header = plain_header, manual_italic_indent = manual_italic_indent, manual_underline = manual_underline, table_caption = table_caption, table_footnote = effective_footnote, abbreviation_footnote = effective_abbr, posthoc_footnote = combined_posthoc_note, variable_footnote = variable_footnote, index_style = index_style, line_break_header = line_break_header, open_doc = open_doc, citation = citation, font_family = font_family)

  if (!indent_info_column) out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".indent"))

  # Attach word-export metadata so ternB() can reproduce this table in a combined document
  attr(out_tbl, "ternB_meta") <- list(
    tbl                   = out_tbl_with_indent,
    round_intg            = round_intg,
    round_decimal         = round_decimal,
    font_size             = table_font_size,
    category_start        = category_start,
    plain_header          = plain_header,
    manual_italic_indent  = manual_italic_indent,
    manual_underline      = manual_underline,
    table_caption         = table_caption,
    table_footnote        = effective_footnote,
    abbreviation_footnote = effective_abbr,
    posthoc_footnote      = combined_posthoc_note,
    variable_footnote     = variable_footnote,
    index_style           = index_style,
    line_break_header     = line_break_header,
    source                = "ternG",
    n_levels              = n_levels,
    OR_col                = OR_col,
    OR_method             = OR_method,
    post_hoc              = post_hoc,
    categorical_posthoc   = categorical_posthoc,
    p_adjust              = p_adjust,
    p_adjust_display      = p_adjust_display,
    citation              = citation,
    font_family           = font_family,
    force_continuous      = force_continuous,
    force_normal          = force_normal,
    show_missing          = show_missing,
    show_p                = show_p,
    zero_to_dash          = zero_to_dash,
    percentage_compute    = percentage_compute,
    show_missingness      = show_missingness,
    missing_indicators    = missing_indicators
  )

  # Attach the tidy side-channel. Always attached (it is cheap) so a single call
  # yields the formatted table plus machine-readable results — see tern_stats().
  attr(out_tbl, "tern_stats")     <- stats_tbl
  attr(out_tbl, "tern_estimates") <- est_tbl

  if (isTRUE(plain_tibble)) {
    attr(stats_tbl, "tern_stats")     <- stats_tbl
    attr(stats_tbl, "tern_estimates") <- est_tbl
    return(stats_tbl)
  }

  return(out_tbl)
}
