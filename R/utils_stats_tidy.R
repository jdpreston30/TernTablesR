# Internal constructors and public accessors for the tidy statistics side-channel
# attached to every ternG()/ternD() result.
#
# Design contract (see ?tern_stats):
#   * Every value emitted here comes from the SAME object that populated the
#     rendered display cell — nothing is recomputed. This is what guarantees the
#     tidy frame can never drift from the formatted table.
#   * Raw statistics stay numeric and un-rounded; the formatted display string
#     lives in an adjacent `*_fmt` column.
#   * `variable` is the ORIGINAL data column name, so downstream code never has
#     to reverse-engineer smart_rename()'s display-name cleaning to match rows.

# Coerce anything to a single unnamed double (NULL / length-0 -> NA_real_).
.tern_num1 <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  suppressWarnings(unname(as.numeric(x[[1]])))
}

# Minimum Shapiro-Wilk P across groups; NA when normality was never assessed.
.tern_min_sw <- function(x) {
  if (is.null(x) || length(x) == 0L) return(NA_real_)
  v <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)))
  if (length(v) == 0L || all(is.na(v))) return(NA_real_)
  min(v, na.rm = TRUE)
}

# One row of the per-variable tidy frame. All columns are always present and
# always the same type, so records bind cleanly regardless of variable type.
.tern_stat_row <- function(variable,
                           label,
                           type,
                           stat_type,
                           n,
                           n_missing,
                           n_levels  = NA_integer_,
                           test      = NA_character_,
                           statistic = NA_real_,
                           df        = NA_real_,
                           df2       = NA_real_,
                           p_value   = NA_real_,
                           p_fmt     = NA_character_,
                           test_note = NA_character_,
                           is_normal = NA,
                           sw_p      = NA_real_,
                           or_value  = NA_real_,
                           or_lcl    = NA_real_,
                           or_ucl    = NA_real_,
                           or_fmt    = NA_character_,
                           or_method = NA_character_) {
  tibble::tibble(
    variable       = as.character(variable),
    label          = as.character(label),
    type           = as.character(type),
    stat_type      = as.character(stat_type),
    n              = as.integer(n),
    n_missing      = as.integer(n_missing),
    n_levels       = as.integer(n_levels),
    test           = as.character(test),
    statistic      = .tern_num1(statistic),
    df             = .tern_num1(df),
    df2            = .tern_num1(df2),
    p_value        = .tern_num1(p_value),
    p_fmt          = as.character(p_fmt),
    p_adjusted     = NA_real_,
    p_adjusted_fmt = NA_character_,
    test_note      = as.character(test_note),
    is_normal      = as.logical(is_normal),
    sw_p           = .tern_num1(sw_p),
    or_value       = .tern_num1(or_value),
    or_lcl         = .tern_num1(or_lcl),
    or_ucl         = .tern_num1(or_ucl),
    or_fmt         = as.character(or_fmt),
    or_method      = as.character(or_method)
  )
}

# One row of the long-format estimates frame (variable x group x level).
.tern_est_row <- function(variable,
                          label,
                          group,
                          level     = NA_character_,
                          n         = NA_integer_,
                          pct       = NA_real_,
                          mean      = NA_real_,
                          sd        = NA_real_,
                          median    = NA_real_,
                          q1        = NA_real_,
                          q3        = NA_real_,
                          value_fmt = NA_character_) {
  tibble::tibble(
    variable  = as.character(variable),
    label     = as.character(label),
    group     = as.character(group),
    level     = as.character(level),
    n         = as.integer(n),
    pct       = .tern_num1(pct),
    mean      = .tern_num1(mean),
    sd        = .tern_num1(sd),
    median    = .tern_num1(median),
    q1        = .tern_num1(q1),
    q3        = .tern_num1(q3),
    value_fmt = as.character(value_fmt)
  )
}

# Bind accumulated records into a tibble, returning a correctly-typed 0-row
# tibble when nothing was recorded.
.tern_bind_stats <- function(records) {
  if (length(records) == 0L) {
    return(.tern_stat_row(character(0), character(0), character(0), character(0),
                          integer(0), integer(0))[0, ])
  }
  dplyr::bind_rows(records)
}

.tern_bind_est <- function(records) {
  if (length(records) == 0L) {
    return(.tern_est_row(character(0), character(0), character(0))[0, ])
  }
  dplyr::bind_rows(records)
}


#' Extract the tidy per-variable statistics from a TernTables result
#'
#' Returns the machine-readable record of everything \code{\link{ternG}} or
#' \code{\link{ternD}} computed while building the display table: one row per
#' variable, with raw un-rounded statistics in numeric columns and the matching
#' publication-formatted strings alongside them.
#'
#' This is the supported way to consume TernTables results programmatically.
#' It removes any need to parse the rendered table — no walking the internal
#' \code{.indent} column, no allowing for the fact that continuous and
#' categorical variables park their test results on differently-shaped rows, and
#' no fuzzy-matching around \code{smart_rename}'s display-name cleaning (rows are
#' keyed by \code{variable}, the original data column name).
#'
#' The frame is attached to every \code{ternG()} / \code{ternD()} result as the
#' \code{"tern_stats"} attribute, so a single call yields both the formatted
#' table and the tidy data. Values are never recomputed: each row is populated
#' from the same test object that produced the corresponding display cell, so
#' the tidy frame cannot drift from the rendered table.
#'
#' @param x A tibble returned by \code{\link{ternG}} or \code{\link{ternD}}.
#'
#' @return A tibble with one row per summarized variable and the following
#'   stable columns:
#'   \describe{
#'     \item{variable}{Original column name in \code{data} (the join key).}
#'     \item{label}{Display name as it appears in the rendered table, after
#'       \code{smart_rename} cleaning when enabled.}
#'     \item{type}{\code{"continuous"} or \code{"categorical"}.}
#'     \item{stat_type}{How the variable was summarized: \code{"mean_sd"},
#'       \code{"median_iqr"}, or \code{"n_pct"}.}
#'     \item{n, n_missing}{Observations used by the test, and observations
#'       dropped as missing.}
#'     \item{n_levels}{Number of levels for categorical variables; \code{NA}
#'       for continuous.}
#'     \item{test}{Name of the statistical test applied (\code{NA} for
#'       \code{ternD}, which performs no group comparison).}
#'     \item{statistic, df, df2}{Test statistic and degrees of freedom, un-rounded.
#'       \code{df2} holds the denominator df for Welch ANOVA. \code{NA} where the
#'       test reports none (e.g. Fisher's exact).}
#'     \item{p_value}{The raw numeric P value, un-rounded.}
#'     \item{p_fmt}{The same P value formatted exactly as displayed.}
#'     \item{p_adjusted, p_adjusted_fmt}{Benjamini-Hochberg corrected P values,
#'       populated only when \code{p_adjust = TRUE}.}
#'     \item{test_note}{Reason the test could not be run (e.g.
#'       \code{"insufficient variation"}), otherwise \code{NA}.}
#'     \item{is_normal}{Normality routing decision for continuous variables;
#'       \code{NA} for categorical variables and for variables whose distribution
#'       was never assessed (e.g. \code{force_ordinal}).}
#'     \item{sw_p}{Minimum Shapiro-Wilk P across groups, where computed.}
#'     \item{or_value, or_lcl, or_ucl}{Odds ratio and 95\% CI bounds, populated
#'       when \code{OR_col = TRUE}.}
#'     \item{or_fmt, or_method}{Formatted OR string and the method used
#'       (\code{"Fisher"} or \code{"Wald"}).}
#'   }
#'   Statistical fields are populated whenever the underlying test ran, including
#'   when \code{show_p = FALSE} or \code{show_test = FALSE} suppressed them from
#'   the display table.
#'
#' @seealso \code{\link{tern_estimates}} for the per-group, per-level summary
#'   values; \code{\link{ternG}} and \code{\link{ternD}}, whose
#'   \code{plain_tibble} argument returns this frame directly.
#'
#' @examples
#' data(tern_colon)
#'
#' tbl <- ternG(tern_colon, exclude_vars = "ID", group_var = "Recurrence",
#'              methods_doc = FALSE)
#'
#' # Pretty table returned as always; tidy data one accessor away
#' stats <- tern_stats(tbl)
#' stats[, c("variable", "test", "p_value", "p_fmt")]
#'
#' # Harvest specific variables without touching the rendered layout
#' stats[stats$variable %in% c("Age_Years", "Sex"), ]
#'
#' @export
tern_stats <- function(x) {
  out <- attr(x, "tern_stats", exact = TRUE)
  if (is.null(out)) {
    stop("`x` has no \"tern_stats\" attribute. ",
         "Pass a tibble returned directly by ternG() or ternD() \u2014 ",
         "the attribute is dropped by some downstream operations.",
         call. = FALSE)
  }
  out
}


#' Extract the tidy per-group summary estimates from a TernTables result
#'
#' Long-format companion to \code{\link{tern_stats}}: one row per variable x
#' group x level, holding the raw numeric summary values (counts, percentages,
#' means, SDs, medians, quartiles) behind each displayed cell, plus the exact
#' formatted string that was rendered.
#'
#' For categorical variables, every level is reported — including levels the
#' display collapses away (a binary Y/N variable shows only the "Y" row in the
#' table, but both levels appear here).
#'
#' @param x A tibble returned by \code{\link{ternG}} or \code{\link{ternD}}.
#'
#' @return A tibble with columns \code{variable}, \code{label}, \code{group}
#'   (the group level, or \code{"Total"} for the aggregate column), \code{level}
#'   (factor level for categorical variables, \code{NA} for continuous),
#'   \code{n}, \code{pct}, \code{mean}, \code{sd}, \code{median}, \code{q1},
#'   \code{q3}, and \code{value_fmt}. Columns not applicable to a given variable
#'   type are \code{NA}.
#'
#' @seealso \code{\link{tern_stats}}
#'
#' @examples
#' data(tern_colon)
#'
#' tbl <- ternG(tern_colon, exclude_vars = "ID", group_var = "Recurrence",
#'              methods_doc = FALSE)
#' est <- tern_estimates(tbl)
#' est[est$variable == "Age_Years", ]
#'
#' @export
tern_estimates <- function(x) {
  out <- attr(x, "tern_estimates", exact = TRUE)
  if (is.null(out)) {
    stop("`x` has no \"tern_estimates\" attribute. ",
         "Pass a tibble returned directly by ternG() or ternD() \u2014 ",
         "the attribute is dropped by some downstream operations.",
         call. = FALSE)
  }
  out
}
