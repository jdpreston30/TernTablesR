#' Format a p-value for reporting
#'
#' @param p Numeric p-value
#' @param digits Number of decimal places to round to (default 3)
#' @return Formatted string with specified decimals, scientific if < 0.001
#' @export
fmt_p <- function(p, digits = 3) {
  if (is.na(p)) {
    return(NA_character_)
  }

  # Use proper rounding (5 rounds up, not banker's rounding)
  p_rounded <- round(p + .Machine$double.eps, digits)

  if (p < 0.001) {
    base <- signif(p, 1)
    sci <- format(base, scientific = TRUE, digits = 1)
    return(sci)
  }

  if (p >= 0.001 && p < 0.1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  if (p >= 0.94 && p < 1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  if (p == 1) {
    return(paste0(">0.", paste(rep("9", digits), collapse = "")))
  }

  # For all other values, use the specified digits
  return(sprintf(paste0("%.", digits, "f"), p_rounded))
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
