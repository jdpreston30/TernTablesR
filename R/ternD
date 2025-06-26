#' Generate descriptive summary table without grouping
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all.
#' @param exclude_vars Character vector of variable(s) to exclude.
#' @param output_xlsx Optional filename for Excel export.
#' @param output_docx Optional filename for Word export.
#' @return A tibble with one row per variable (multi-row for multi-level factors), summary statistics, NA p-value and test type as "Descriptive".
#' @export
ternD <- function(data,
                  vars = NULL,
                  exclude_vars = NULL,
                  output_xlsx = NULL,
                  output_docx = NULL) {
  if (is.null(vars)) {
    vars <- setdiff(names(data), exclude_vars)
  }

  summarize_variable <- function(df, var) {
    v <- df[[var]]
    if (is.factor(v) || is.character(v)) {
      tab <- table(v)
      tab_pct <- round(100 * prop.table(tab))
      rows <- lapply(names(tab), function(level) {
        tibble(
          Variable = paste0(var, ": ", level),
          Summary = paste0(tab[[level]], " (", tab_pct[[level]], "%)"),
          p = NA_real_,
          test = "Descriptive"
        )
      })
      bind_rows(rows)
    } else {
      stats <- df %>% filter(!is.na(.data[[var]])) %>% summarise(
        mean = mean(.data[[var]], na.rm = TRUE),
        sd = sd(.data[[var]], na.rm = TRUE),
        Q1 = quantile(.data[[var]], 0.25),
        med = median(.data[[var]]),
        Q3 = quantile(.data[[var]], 0.75)
      )
      tibble(
        Variable = var,
        Summary = paste0(
          round(stats$mean, 1), " ± ", round(stats$sd, 1),
          " | Median [IQR]: ", round(stats$med, 1), " [",
          round(stats$Q1, 1), "-", round(stats$Q3, 1), "]"
        ),
        p = NA_real_,
        test = "Descriptive"
      )
    }
  }

  out_tbl <- bind_rows(lapply(vars, function(v) summarize_variable(data, v)))

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  return(out_tbl)
}
