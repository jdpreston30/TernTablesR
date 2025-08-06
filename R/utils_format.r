#' Format a p-value for reporting
#'
#' @param p Numeric p-value
#' @return Formatted string with 2 decimals, scientific if < 0.001
#' @export
fmt_p <- function(p) {
  if (is.na(p)) {
    return(NA_character_)
  }

  if (p < 0.001) {
  base <- signif(p, 1)
  sci <- format(base, scientific = TRUE, digits = 1)
  return(sci)
  }

  if (p >= 0.001 && p < 0.1) {
    return(sprintf("%.3f", round(p, 3)))
  }

  if (p >= 0.94 && p < 1) {
    return(sprintf("%.3f", round(p, 3)))
  }

  if (p == 1) {
    return(">0.99")
  }

  return(sprintf("%.2f", round(p, 2)))
}

#' Format a mean ± SD string
#'
#' @param mean Mean value
#' @param sd Standard deviation
#' @return Formatted string "mean ± sd"
#' @export
format_val <- function(mean, sd) {
  paste0(
    formatC(mean, format = "f", digits = 1),
    " ± ",
    formatC(sd, format = "f", digits = 1)
  )
}

export_to_excel <- function(tbl, filename) {
  writexl::write_xlsx(tbl, path = filename)
}
