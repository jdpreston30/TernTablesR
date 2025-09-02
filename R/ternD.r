#' Generate descriptive summary table (optionally normality-aware)
#'
#' @param data Tibble with variables.
#' @param vars Character vector of variables to summarize. Defaults to all.
#' @param exclude_vars Character vector to exclude.
#' @param output_xlsx Optional Excel filename.
#' @param output_docx Optional Word filename.
#' @param consider_normality Logical; if TRUE choose mean±SD vs median [IQR].
#' @param print_normality Logical; include Shapiro–Wilk p-values if TRUE.
#' @return Tibble; one row per variable (multi-row for factors).
#' @examples
#' # ternD(mtcars, consider_normality = TRUE, print_normality = TRUE)
#' @export
ternD <- function(data, vars = NULL, exclude_vars = NULL,
                  output_xlsx = NULL, output_docx = NULL,
                  consider_normality = FALSE, print_normality = FALSE) {
  stopifnot(is.data.frame(data))

  if (is.null(vars)) {
    vars <- setdiff(names(data), exclude_vars)
  }

  fmt_mean_sd <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    paste0(round(m, 1), " ± ", round(s, 1))
  }

  fmt_median_iqr <- function(x) {
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    paste0(round(q[2], 1), " [", round(q[1], 1), "–", round(q[3], 1), "]")
  }

  shapiro_p <- function(x) {
    x <- x[!is.na(x)]
    # Shapiro requires n >= 3 and not all equal
    if (length(x) < 3 || stats::var(x) == 0) {
      return(NA_real_)
    }
    out <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    out
  }

  summarize_variable <- function(df, var) {
    v <- df[[var]]

    # ---------- CATEGORICAL ----------
    if (is.factor(v) || is.character(v) || is.logical(v)) {
      v <- factor(v) # ensure levels
      tab <- table(v, useNA = "no")
      if (length(tab) == 0) {
        # all missing
        if (isTRUE(consider_normality)) {
          out <- tibble::tibble(Variable = var, Summary = "0 (0%)")
          if (print_normality) out$SW_p <- NA_real_
          return(out)
        } else {
          out <- tibble::tibble(
            Variable = var,
            n_pct = "0 (0%)",
            Mean_SD = NA_character_,
            Median_IQR = NA_character_
          )
          return(out)
        }
      }
      pct <- round(100 * prop.table(tab))
      rows <- lapply(names(tab), function(level) {
        n <- as.integer(tab[[level]])
        p <- pct[[level]]
        if (isTRUE(consider_normality)) {
          tibble::tibble(
            Variable = paste0(var, ": ", level),
            Summary  = paste0(n, " (", p, "%)")
          )
        } else {
          tibble::tibble(
            Variable = paste0(var, ": ", level),
            n_pct = paste0(n, " (", p, "%)"),
            Mean_SD = NA_character_,
            Median_IQR = NA_character_
          )
        }
      })
      out <- dplyr::bind_rows(rows)
      if (print_normality) {
        # Not applicable for categorical
        out$SW_p <- NA_real_
      }
      return(out)
    }

    # ---------- NUMERIC ----------
    x <- suppressWarnings(as.numeric(v))
    # compute normality p (even if consider_normality = FALSE, we may print it)
    sw <- if (print_normality || consider_normality) shapiro_p(x) else NA_real_

    if (isTRUE(consider_normality)) {
      # choose mean±SD if normal; else median [IQR]
      if (!is.na(sw) && sw >= 0.05) {
        summary_str <- fmt_mean_sd(x)
      } else {
        summary_str <- fmt_median_iqr(x)
      }
      out <- tibble::tibble(
        Variable = var,
        Summary  = summary_str
      )
      if (print_normality) out$SW_p <- sw
      return(out)
    } else {
      # old behavior: keep separate columns
      out <- tibble::tibble(
        Variable = var,
        n_pct = NA_character_,
        Mean_SD = fmt_mean_sd(x),
        Median_IQR = fmt_median_iqr(x)
      )
      if (print_normality) out$SW_p <- sw
      return(out)
    }
  }

  out_tbl <- dplyr::bind_rows(lapply(vars, function(v) summarize_variable(data, v)))

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  out_tbl
}
