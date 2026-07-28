#' Generate descriptive summary table (optionally normality-aware)
#'
#' Creates a descriptive summary table with a single "Total" column format.
#' By default (\code{consider_normality = "ROBUST"}), continuous variables are shown
#' as mean +/- SD or median [IQR] based on a four-gate decision (n < 3 fail-safe, skewness/kurtosis, CLT, and Shapiro-Wilk).
#' This can be overridden via \code{consider_normality} and \code{force_ordinal}.
#'
#' @param data Tibble with variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{exclude_vars}.
#' @param exclude_vars Character vector to exclude from the summary.
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use median [IQR]) 
#'   regardless of the \code{consider_normality} setting. This parameter takes priority over 
#'   normality testing when \code{consider_normality = "ROBUST"} or \code{TRUE}.
#' @param force_normal Character vector of variable names to treat as normally distributed, bypassing all
#'   normality assessment. Listed variables are summarized as mean \eqn{\pm} SD regardless of the
#'   \code{consider_normality} setting. Takes priority over \code{consider_normality} but not over
#'   \code{force_ordinal} (if a variable appears in both, \code{force_ordinal} wins). Default is \code{NULL}.
#' @param force_continuous Character vector of variables to force treatment as continuous (mean \eqn{\pm} SD),
#'   bypassing the automatic binary \code{0/1} detection that would otherwise convert them to categorical Y/N.
#'   Useful when a numeric variable with only two unique values (e.g. \code{0}/\code{1} dose levels) should
#'   be analysed as a continuous measurement rather than a dichotomous category. Default is \code{NULL}.
#' @param output_xlsx Optional Excel filename to export the table.
#' @param output_docx Optional Word filename to export the table.
#' @param consider_normality Character or logical; controls routing of continuous variables to
#'   mean \eqn{\pm} SD vs median [IQR].
#'   \code{"ROBUST"} (default) applies a four-gate decision: (1) n < 3 \eqn{\rightarrow} non-parametric
#'   (conservative fail-safe); (2) absolute skewness > 2 or excess kurtosis > 7
#'   \eqn{\rightarrow} non-parametric regardless of n;
#'   (3) n \eqn{\geq} 30 \eqn{\rightarrow} parametric via the Central Limit Theorem;
#'   (4) otherwise Shapiro-Wilk p > 0.05 \eqn{\rightarrow} parametric.
#'   If \code{TRUE}, uses Shapiro-Wilk alone (can be over-sensitive at large n).
#'   If \code{FALSE}, defaults to mean \eqn{\pm} SD for all numeric variables unless specified in
#'   \code{force_ordinal}.
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk P values as an
#'   additional column in the output. Default is \code{FALSE}.
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard 
#'   deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param round_decimal Integer or \code{NULL}; number of decimal places for all continuous summary
#'   values (means, SDs, medians, IQRs). Overrides the default of 1 decimal place when set.
#'   Ignored when \code{round_intg = TRUE}. Default is \code{NULL} (1 decimal place).
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and
#'   subheadings for publication-ready output using built-in rule-based pattern matching for
#'   common medical abbreviations and prefixes. Default is \code{TRUE}.
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
#'   is always respected regardless of the number of levels; for non-factor two-level variables,
#'   levels are sorted alphabetically; for non-factor variables with three or more levels, levels
#'   are sorted by decreasing frequency.
#'   \code{"levels"} respects the original factor level ordering for all variables; if the variable
#'   is not a factor, falls back to frequency ordering.
#'   \code{"frequency"} orders all levels by decreasing frequency (most common first).
#' @param methods_doc Logical; if \code{TRUE} (default), generates a methods document
#'   describing the statistical presentation used. The document contains boilerplate
#'   text for all three table types so the relevant section can be copied directly
#'   into a manuscript.
#' @param methods_filename Character; filename for the methods document.
#'   Default is \code{"TernTables_methods.docx"}.
#' @param category_start Named character vector specifying where to insert category headers.
#'   Names are the header label text to display; values are the anchor variable -- either the
#'   original column name (e.g. \code{"Age_Years"}) or the cleaned display name
#'   (e.g. \code{"Age (yr)"}). Both forms are accepted.
#'   Example: \code{c("Demographics" = "Age_Years", "Clinical Measures" = "bmi")}.
#'   Default is \code{NULL} (no category headers).
#' @param plain_header Named character vector, same interface as \code{category_start}. Names are
#'   the label text; values are the anchor variable to insert before. Inserts a label-only row
#'   with underline formatting and no bold, merge, or border treatments. Default \code{NULL}.
#' @param table_font_size Numeric; font size for Word document output tables. Default is 9.
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) that should be
#'   formatted as italicized and indented in Word output -- matching the appearance of factor sub-category
#'   rows. Has no effect on the returned tibble; only applies when \code{output_docx} is specified.
#'   Default is \code{NULL}.
#' @param manual_underline Character vector of display variable names (post-cleaning) that should be
#'   formatted as underlined in Word output -- matching the appearance of multi-category variable headers.
#'   Has no effect on the returned tibble; only applies when \code{output_docx} is specified.
#'   Default is \code{NULL}.
#' @param table_caption Optional character string for a table caption to display above the table in
#'   the Word document. Rendered as size 11 Arial bold, single-spaced with a small gap before the table.
#'   Default is \code{NULL} (no caption).
#'   Example: \code{"Table 1. Patient demographics."}
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
#'   \code{\\n} -- the first column header includes a category hierarchy label, and the sample
#'   size appears on a second line. Set to \code{FALSE} to suppress all header line breaks.
#'   Can also be set package-wide via \code{options(TernTables.line_break_header = FALSE)}.
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
#'   variable's data rows showing the count and percentage of missing observations
#'   (denominator is the total N). Only emitted when at least one observation is missing.
#'   A footnote is automatically appended noting that missing values are reported.
#'   Default is \code{FALSE}.
#' @param zero_to_dash Logical; if \code{TRUE}, replaces any categorical cell that would display
#'   \code{"0 (0\%)"} with \code{"-"} in the output table. Useful when zero counts are not
#'   meaningful to report numerically. Default is \code{FALSE}.
#' @param show_missingness Controls whether a \code{"Missing, n (\%)"} column is appended to
#'   the table after the \code{Total} column. Options:\cr
#'   \code{FALSE} (default) — no missingness column added.\cr
#'   \code{"total"} — one column appended showing the count and percentage of missing
#'   observations across all rows for each variable.\cr
#'   \code{"group"} is not supported by \code{ternD()} (which has no group structure); use
#'   \code{ternG()} with \code{show_missingness = "group"} instead. Missingness is computed on
#'   the raw data column so both \code{NA} values and string representations of missing data
#'   (e.g., \code{"Unknown"}, \code{"N/A"}) are counted. See \code{missing_indicators}.
#' @param missing_indicators Optional character vector of string values to treat as missing
#'   in addition to (or instead of) the built-in ternP defaults. When \code{NULL} (default),
#'   the ternP canonical list is used. When supplied, the custom list \strong{replaces} the
#'   ternP defaults. Matching is case-insensitive and trims whitespace.
#' @param plain_tibble Logical; if \code{TRUE}, returns the tidy per-variable statistics frame
#'   (see \code{\link{tern_stats}}) instead of the formatted display table — one un-indented row
#'   per variable with raw numeric statistics and their formatted counterparts in adjacent
#'   columns. Word/Excel exports and the methods document are still written if requested.
#'   Setting this is never required to reach the tidy data: the same frame is always attached
#'   to the normal return value as the \code{"tern_stats"} attribute, so
#'   \code{tern_stats(ternD(...))} yields both the pretty table and the tidy data from one call.
#'   Since \code{ternD()} performs no group comparison, the test-related columns
#'   (\code{test}, \code{statistic}, \code{df}, \code{p_value}) are \code{NA}.
#'   Default is \code{FALSE}.
#'
#' @details
#' The function always returns a tibble with a single \code{Total (N = n)} column format, regardless of the
#' \code{consider_normality} setting. The behavior for numeric variables follows this priority:
#' \enumerate{
#'   \item Variables in \code{force_ordinal}: Always use median [IQR]
#'   \item When \code{consider_normality = "ROBUST"}: Four-gate decision (n<3 fail-safe, skewness/kurtosis, CLT, Shapiro-Wilk)
#'   \item When \code{consider_normality = TRUE}: Use Shapiro-Wilk test to choose format
#'   \item When \code{consider_normality = FALSE}: Default to mean +/- SD
#' }
#'
#' For categorical variables, the function shows frequencies and percentages. When
#' \code{insert_subheads = TRUE}, categorical variables with 3 or more levels are displayed with
#' hierarchical formatting (main variable as header, levels as indented sub-rows). Binary variables
#' (Y/N, YES/NO, or numeric 1/0 auto-detected as Y/N) always use a single-row format showing
#' only the positive/yes count, regardless of this setting. Two-level categorical variables whose
#' values are not Y/N, YES/NO, or 1/0 (e.g. Male/Female) also use the hierarchical sub-row format.
#'
#' @return A tibble with one row per variable (multi-row for factors), containing:
#' \describe{
#'   \item{Variable}{Variable names with appropriate indentation}
#'   \item{Total (N = n)}{Summary statistics (mean +/- SD, median [IQR], or n (\%) as appropriate)}
#'   \item{SW_p}{Shapiro-Wilk P values (only if \code{print_normality = TRUE})}
#' }
#' Two tidy, machine-readable frames are always attached as attributes:
#' \code{"tern_stats"} (one row per variable — summary type, n, normality decision,
#' Shapiro-Wilk P) and \code{"tern_estimates"} (long format: the underlying counts,
#' percentages, means, SDs, medians and quartiles behind each displayed cell). Access them
#' with \code{\link{tern_stats}} and \code{\link{tern_estimates}} rather than parsing the
#' display table.
#'
#' @seealso \code{\link{tern_stats}}, \code{\link{tern_estimates}}
#'
#' @examples
#' data(tern_colon)
#'
#' # Basic descriptive summary
#' ternD(tern_colon, exclude_vars = c("ID"), methods_doc = FALSE)
#'
#' # With normality-aware formatting and category section headers
#' ternD(tern_colon, exclude_vars = c("ID"), methods_doc = FALSE,
#'       category_start = c("Patient Demographics"  = "Age (yr)",
#'                          "Tumor Characteristics" = "Positive Lymph Nodes (n)"))
#'
#' # Force specific variables to ordinal (median [IQR]) display
#' ternD(tern_colon, exclude_vars = c("ID"), methods_doc = FALSE,
#'       force_ordinal = c("Positive_Lymph_Nodes_n"))
#'
#' # Export to Word (writes a file to tempdir)
#' \donttest{
#' ternD(tern_colon,
#'       exclude_vars     = c("ID"),
#'       methods_doc      = FALSE,
#'       open_doc         = FALSE,
#'       output_docx      = file.path(tempdir(), "descriptive.docx"),
#'       category_start   = c("Patient Demographics"  = "Age (yr)",
#'                            "Surgical Findings"     = "Colonic Obstruction",
#'                            "Tumor Characteristics" = "Positive Lymph Nodes (n)",
#'                            "Outcomes"              = "Recurrence"))
#' }
#' @export
ternD <- function(data, vars = NULL, exclude_vars = NULL, force_ordinal = NULL,
                  force_normal = NULL,
                  force_continuous = NULL,
                  output_xlsx = NULL, output_docx = NULL,
                  consider_normality = "ROBUST", print_normality = FALSE,
                  round_intg = FALSE, round_decimal = NULL, smart_rename = TRUE, insert_subheads = TRUE,
                  factor_order = "mixed", methods_doc = TRUE,
                  methods_filename = "TernTables_methods.docx", category_start = NULL,
                  plain_header = NULL,
                  table_font_size = 9, manual_italic_indent = NULL, manual_underline = NULL,
                  table_caption = NULL, table_footnote = NULL,
                  abbreviation_footnote = NULL, variable_footnote = NULL,
                  index_style = "symbols",
                  line_break_header = getOption("TernTables.line_break_header", TRUE),
                  open_doc = TRUE, citation = TRUE,
                  font_family = getOption("TernTables.font_family", "Arial"),
                  show_missing = FALSE,
                  zero_to_dash = FALSE,
                  show_missingness = FALSE,
                  missing_indicators = NULL,
                  plain_tibble = FALSE) {
  stopifnot(is.data.frame(data))

  # ── Validate show_missingness ───────────────────────────────────────────
  if (!isFALSE(show_missingness) &&
      !(is.character(show_missingness) && show_missingness %in% c("total", "group"))) {
    stop('`show_missingness` must be FALSE, "total", or "group".', call. = FALSE)
  }
  if (identical(show_missingness, "group")) {
    stop(
      '`show_missingness = "group"` requires a grouped comparison. ',
      'Use `show_missingness = "total"` with `ternD()`, ',
      'or use `ternG()` for per-group missingness columns.',
      call. = FALSE
    )
  }
  
  # Store total N for column header
  total_n <- nrow(data)

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
    vars <- setdiff(names(data), exclude_vars)
  }

  # Both formatters return the raw statistics alongside the display string, so
  # the tidy side-channel (see R/utils_stats_tidy.R) reports un-rounded values
  # without ever recomputing them.
  calc_mean_sd <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    dp <- if (!is.null(round_decimal)) as.integer(round_decimal) else 1L
    fmt <- if (round_intg) {
      paste0(round_up_half(m, 0), " \u00b1 ", round_up_half(s, 0))
    } else {
      paste0(round(m, dp), " \u00b1 ", round(s, dp))
    }
    list(mean = m, sd = s, fmt = fmt)
  }

  calc_median_iqr <- function(x) {
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    dp <- if (!is.null(round_decimal)) as.integer(round_decimal) else 1L
    fmt <- if (round_intg) {
      paste0(round_up_half(q[2], 0), " [", round_up_half(q[1], 0), "\u2013", round_up_half(q[3], 0), "]")
    } else {
      paste0(round(q[2], dp), " [", round(q[1], dp), "\u2013", round(q[3], dp), "]")
    }
    list(median = q[2], q1 = q[1], q3 = q[3], fmt = fmt)
  }

  shapiro_p <- function(x) {
    x <- x[!is.na(x)]
    # Shapiro requires 3 <= n <= 5000 and not all equal
    if (length(x) < 3 || length(x) > 5000 || stats::var(x) == 0) {
      return(NA_real_)
    }
    out <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    out
  }

  summarize_variable <- function(df, var) {
    v <- df[[var]]

    # Build a "Missing" row to append when show_missing = TRUE
    .make_missing_row <- function() {
      n_miss <- sum(is.na(v))
      if (!isTRUE(show_missing) || n_miss == 0) return(NULL)
      pct <- round(n_miss / total_n * 100)
      row <- tibble::tibble(Variable = "Missing", .indent = 6L,
                            Summary  = paste0(n_miss, " (", pct, "%)"))
      if (print_normality) row$SW_p <- NA_real_
      row
    }
    # ── Missingness column helper ─────────────────────────────────────────────
    # Appends a "Missing, n (%)" column using raw (unfiltered) data so both
    # true NAs and string-NA values are counted.
    .add_miss_col_d <- function(result_tbl) {
      if (isFALSE(show_missingness)) return(result_tbl)
      raw_vec <- df[[var]]
      is_miss <- .is_missing_value(raw_vec, missing_indicators)
      n_miss  <- sum(is_miss)
      n_total <- length(is_miss)
      pct     <- round(n_miss / n_total * 100)
      miss_str <- paste0(n_miss, " (", pct, "%)")
      result_tbl[["Missing, n (%)"]] <- ifelse(result_tbl$.indent == 6L, "-", miss_str)
      result_tbl
    }
    # Auto-detect binary numeric (0/1) as categorical Y/N
    # Skipped when the variable is listed in force_continuous
    if (is.numeric(v) && length(unique(stats::na.omit(v))) == 2 && all(stats::na.omit(v) %in% c(0, 1)) &&
        !(var %in% force_continuous)) {
      v <- factor(v, levels = c(0, 1), labels = c("N", "Y"))
    }

    # ---------- CATEGORICAL ----------
    if (is.factor(v) || is.character(v) || is.logical(v)) {
      v <- factor(v) # ensure levels
      tab <- table(v, useNA = "no")
      if (length(tab) == 0) {
        # all missing
        out <- tibble::tibble(Variable = .clean_variable_name_for_header(var), .indent = 2, Summary = "0 (0%)")
        if (print_normality) out$SW_p <- NA_real_
        .record_stats(variable = var, label = out$Variable[1], type = "categorical",
                      stat_type = "n_pct", n = 0L, n_missing = length(v), n_levels = 0L)
        return(.add_miss_col_d(dplyr::bind_rows(out, .make_missing_row())))
      }
      pct <- round(100 * prop.table(tab))
      
      # Sort levels: respect factor levels, frequency, or mixed (alphabetical for 2-level, frequency for 3+)
      if ((factor_order == "levels" || factor_order == "mixed") && is.factor(v)) {
        # Factor: always respect original factor level ordering
        sorted_levels <- levels(v)
        sorted_levels <- sorted_levels[sorted_levels %in% names(tab)]
      } else if (factor_order == "mixed") {
        # Non-factor: 2-level → alphabetical; 3+ → frequency
        available <- names(tab)
        if (length(available) == 2) {
          sorted_levels <- sort(available)
        } else {
          sorted_levels <- names(sort(tab, decreasing = TRUE))
        }
      } else {
        # "levels" non-factor fallback or "frequency": sort by frequency
        sorted_levels <- names(sort(tab, decreasing = TRUE))
      }
      
      # Determine if this should use simple format or hierarchical subheads
      # Always use simple format for Y/N variables or when insert_subheads is FALSE
      # Otherwise use hierarchical format for multi-level categorical variables
      upper_levels   <- toupper(sorted_levels)
      is_yes_no      <- all(c("Y", "N")   %in% upper_levels)
      is_yes_no_full <- all(c("YES", "NO") %in% upper_levels)
      is_binary <- is_yes_no || is_yes_no_full
      use_hierarchical <- !is_binary && insert_subheads && length(sorted_levels) > 1
      
      if (use_hierarchical) {
        # Create header row for the main variable
        header_row <- tibble::tibble(
          Variable = .clean_variable_name_for_header(var),
          .indent  = 2L,
          Summary  = ""
        )
        if (print_normality) header_row$SW_p <- NA_real_
        
        # Create sub-category rows (indented)
        sub_rows <- lapply(sorted_levels, function(level) {
          n <- as.integer(tab[[level]])
          p <- pct[[level]]
          row <- tibble::tibble(
            Variable = level,
            .indent  = 6L,
            Summary  = paste0(n, " (", p, "%)"))
          if (print_normality) row$SW_p <- NA_real_
          return(row)
        })
        
        # Combine header and sub-rows
        out <- dplyr::bind_rows(list(header_row), sub_rows)
      } else {
        # For Y/N variables or when insert_subheads=FALSE, use simple format with 2-space indentation
        # For Y/N, show only the "Y" level; for Yes/No, show only "Yes"; for others, show most common level
        if (is_yes_no) {
          # Y/N variables (any case): show only the positive level
          ref_level <- sorted_levels[toupper(sorted_levels) == "Y"]
        } else if (is_yes_no_full) {
          # Yes/No variables (any case): show only the positive level
          ref_level <- sorted_levels[toupper(sorted_levels) == "YES"]
        } else {
          # Other variables: show most common level or first level based on factor_order
          if (factor_order == "levels" && is.factor(v)) {
            # Use the first level that actually appears in the data
            available_levels <- levels(v)[levels(v) %in% names(tab)]
            ref_level <- available_levels[1]
          } else {
            # Default: show most common level
            ref_level <- sorted_levels[1]  # Already sorted by frequency
          }
        }
        
        row <- tibble::tibble(
          Variable = .clean_variable_name_for_header(var),
          .indent  = 2L,
          Summary  = paste0(as.integer(tab[[ref_level]]), " (", pct[[ref_level]], "%)")
        )
        if (print_normality) row$SW_p <- NA_real_
        rows <- list(row)
        out <- dplyr::bind_rows(rows)
      }

      # ── Tidy side-channel: categorical ──────────────────────────────────────
      .record_stats(
        variable  = var,
        label     = out$Variable[1],
        type      = "categorical",
        stat_type = "n_pct",
        n         = sum(tab),
        n_missing = sum(is.na(v)),
        n_levels  = length(tab)
      )
      # Every level is emitted, including levels the display collapses away
      # (a binary Y/N variable renders only the "Y" row).
      for (lvl in names(tab)) {
        .record_est(
          variable = var, label = out$Variable[1], group = "Total", level = lvl,
          n = as.integer(tab[[lvl]]), pct = pct[[lvl]],
          value_fmt = paste0(as.integer(tab[[lvl]]), " (", pct[[lvl]], "%)")
        )
      }
      return(.add_miss_col_d(dplyr::bind_rows(out, .make_missing_row())))
    }

    # ---------- NUMERIC ----------
    x <- suppressWarnings(as.numeric(v))
    # Always run Shapiro-Wilk (used for print_normality column and TRUE/ROBUST Gate 4)
    sw <- shapiro_p(x)

    # `norm_verdict` records the normality decision that actually drove the
    # display: NA where the variable was forced and its distribution never
    # decided the format.
    norm_verdict <- NA
    # Check if variable is forced to be ordinal
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      # Force ordinal: use median/IQR regardless of consider_normality setting
      .ternD_env$norm_tested <- .ternD_env$norm_tested + 1L
      .ternD_env$norm_failed <- .ternD_env$norm_failed + 1L
      summary_vals <- calc_median_iqr(x)
    } else if (!is.null(force_normal) && var %in% force_normal) {
      # Force parametric: use mean/SD regardless of consider_normality setting
      .ternD_env$norm_tested <- .ternD_env$norm_tested + 1L
      summary_vals <- calc_mean_sd(x)
    } else if (consider_normality == "ROBUST") {
      # ROBUST: four-gate decision tree — see R/utils_normality.R
      .ternD_env$norm_tested <- .ternD_env$norm_tested + 1L
      robust_result <- .robust_normality(list(x))
      norm_verdict  <- isTRUE(robust_result$is_normal)
      if (!robust_result$is_normal) {
        .ternD_env$norm_failed <- .ternD_env$norm_failed + 1L
        summary_vals <- calc_median_iqr(x)
      } else {
        summary_vals <- calc_mean_sd(x)
      }
    } else if (isTRUE(consider_normality)) {
      .ternD_env$norm_tested <- .ternD_env$norm_tested + 1L
      if (is.na(sw) || sw < 0.05) .ternD_env$norm_failed <- .ternD_env$norm_failed + 1L
      # choose mean +- SD if normal; else median [IQR]
      norm_verdict <- !is.na(sw) && sw > 0.05
      if (norm_verdict) {
        summary_vals <- calc_mean_sd(x)
      } else {
        summary_vals <- calc_median_iqr(x)
      }
    } else {
      .ternD_env$norm_tested <- .ternD_env$norm_tested + 1L
      if (is.na(sw) || sw < 0.05) .ternD_env$norm_failed <- .ternD_env$norm_failed + 1L
      # Default behavior when consider_normality = FALSE: use mean +/- SD
      summary_vals <- calc_mean_sd(x)
    }
    summary_str <- summary_vals$fmt

    out <- tibble::tibble(
      Variable = .clean_variable_name_for_header(var),
      .indent  = 2L,
      Summary  = summary_str
    )
    if (print_normality) out$SW_p <- sw

    # ── Tidy side-channel: continuous ─────────────────────────────────────────
    is_mean_sd <- !is.null(summary_vals$mean)
    .record_stats(
      variable  = var,
      label     = out$Variable[1],
      type      = "continuous",
      stat_type = if (is_mean_sd) "mean_sd" else "median_iqr",
      n         = sum(!is.na(x)),
      n_missing = sum(is.na(x)),
      is_normal = norm_verdict,
      sw_p      = sw
    )
    .record_est(
      variable  = var, label = out$Variable[1], group = "Total",
      n         = sum(!is.na(x)),
      mean      = if (is_mean_sd) summary_vals$mean else NA_real_,
      sd        = if (is_mean_sd) summary_vals$sd   else NA_real_,
      median    = if (is_mean_sd) NA_real_ else summary_vals$median,
      q1        = if (is_mean_sd) NA_real_ else summary_vals$q1,
      q3        = if (is_mean_sd) NA_real_ else summary_vals$q3,
      value_fmt = summary_str
    )
    return(.add_miss_col_d(dplyr::bind_rows(out, .make_missing_row())))
  }

  .ternD_env <- new.env(parent = emptyenv())
  .ternD_env$norm_tested <- 0L
  .ternD_env$norm_failed <- 0L
  # ── Tidy statistics side-channel (see R/utils_stats_tidy.R) ────────────────
  .ternD_env$stats_records <- list()
  .ternD_env$est_records   <- list()
  .record_stats <- function(...) {
    .ternD_env$stats_records <- c(.ternD_env$stats_records, list(.tern_stat_row(...)))
    invisible(NULL)
  }
  .record_est <- function(...) {
    .ternD_env$est_records <- c(.ternD_env$est_records, list(.tern_est_row(...)))
    invisible(NULL)
  }

  out_tbl <- dplyr::bind_rows(lapply(vars, function(v) summarize_variable(data, v)))

  stats_tbl <- .tern_bind_stats(.ternD_env$stats_records)
  est_tbl   <- .tern_bind_est(.ternD_env$est_records)

  # Extract accumulator values from environment for reporting
  norm_tested <- .ternD_env$norm_tested
  norm_failed <- .ternD_env$norm_failed

  # -- Report normality results -----------------------------------------------
  if (norm_tested > 0) {
    norm_passed <- norm_tested - norm_failed
    passed_pct  <- round((norm_passed / norm_tested) * 100, 1)
    cli::cli_rule(left = "Normality Assessment (Shapiro-Wilk) \u2014 ternD")
    if (consider_normality == "ROBUST") {
      cli::cli_alert_info("{norm_passed} of {norm_tested} continuous variable{?s} routed to parametric ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Routing: skewness>2 \u2192 median [IQR]; all-n\u226530 (CLT) \u2192 mean \u00b1 SD; else Shapiro-Wilk"
      ))
    } else if (isTRUE(consider_normality)) {
      cli::cli_alert_info("{norm_passed} of {norm_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_bullets(c(
        ">" = "Normally distributed \u2192 mean \u00b1 SD",
        ">" = "Non-normal           \u2192 median [IQR]"
      ))
    } else {
      cli::cli_alert_info("{norm_passed} of {norm_tested} continuous variable{?s} normally distributed ({passed_pct}%)")
      cli::cli_alert_warning("consider_normality = FALSE: all continuous variables displayed as mean \u00b1 SD")
    }
  }

  # ── Missingness column report ─────────────────────────────────────────────
  if (!isFALSE(show_missingness)) {
    miss_vals <- if (!is.null(missing_indicators)) missing_indicators else .tern_missing_strings()
    miss_str_list <- paste0('"', paste(miss_vals, collapse = '", "'), '"')
    indicator_note <- if (!is.null(missing_indicators)) " (custom list)" else " (ternP defaults)"
    cli::cli_rule(left = "Missingness columns added \u2014 ternD")
    cli::cli_alert_info("Mode: \"total\" \u2014 one column appended at far right")
    cli::cli_alert_info("Values treated as missing: NA + {miss_str_list}{indicator_note}")
  }

  # Rename Summary column to match ternG Total column format
  names(out_tbl)[names(out_tbl) == "Summary"] <- paste0("Total\n(N = ", total_n, ")")
  
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
    if (nrow(stats_tbl) > 0L)
      stats_tbl$label <- vapply(stats_tbl$label, .apply_cleaning_rules, character(1), USE.NAMES = FALSE)
    if (nrow(est_tbl) > 0L)
      est_tbl$label <- vapply(est_tbl$label, .apply_cleaning_rules, character(1), USE.NAMES = FALSE)
  }

  # Replace "0 (NaN%)" with "-" for structurally impossible cells
  # (e.g. a subgroup that cannot logically have any observations in a given column)
  # Also replace "0 (0%)" when zero_to_dash = TRUE.
  out_tbl <- out_tbl %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ {
      x <- gsub("0 \\(NaN%\\)", "-", .x)
      if (isTRUE(zero_to_dash)) gsub("0 \\(0%\\)", "-", x) else x
    }))

  # Save with .indent intact for ternB multi-table export metadata
  out_tbl_with_indent <- out_tbl

  # Auto-footnote when show_missing is active
  effective_footnote <- table_footnote
  if (isTRUE(show_missing)) {
    missing_note <- "Missing: n (%) of missing observations."
    effective_footnote <- if (is.null(table_footnote)) missing_note else c(missing_note, table_footnote)
  }

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) word_export(out_tbl, output_docx, font_size = table_font_size,
                                         round_decimal         = round_decimal,
                                         category_start        = category_start,
                                         plain_header          = plain_header,
                                         manual_italic_indent  = manual_italic_indent,
                                         manual_underline      = manual_underline,
                                         table_caption         = table_caption,
                                         table_footnote        = effective_footnote,
                                         abbreviation_footnote = abbreviation_footnote,
                                         variable_footnote     = variable_footnote,
                                         index_style           = index_style,
                                         line_break_header     = line_break_header,
                                         open_doc              = open_doc,
                                         citation              = citation,
                                         font_family           = font_family)
  if (methods_doc) write_methods_doc(out_tbl, methods_filename, source = "ternD", show_missingness = show_missingness, missing_indicators = missing_indicators, open_doc = open_doc, citation = citation, font_family = font_family)

  out_tbl <- dplyr::select(out_tbl, -dplyr::any_of(".indent"))

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
    abbreviation_footnote = abbreviation_footnote,
    variable_footnote     = variable_footnote,
    index_style           = index_style,
    line_break_header     = line_break_header,
    source                = "ternD",
    n_levels              = 1L,
    OR_col                = FALSE,
    OR_method             = "dynamic",
    post_hoc              = FALSE,
    citation              = citation,
    font_family           = font_family,
    force_continuous      = force_continuous,
    force_normal      = force_normal,
    show_missing          = show_missing,
    zero_to_dash          = zero_to_dash,
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

  out_tbl
}
