#' Format a P value for reporting
#'
#' @param p Numeric P value in the range [0, 1]. \code{NA} values are returned as \code{NA_character_}.
#'   Values >= 1 (or rounding to >= 1) are returned as e.g. \code{">0.999"}.
#' @param digits Integer; number of decimal places for reported P values. Default is 3.
#'   Note: for p < 0.001, the value is reported in scientific notation with 1 significant figure
#'   regardless of \code{digits} (e.g., \code{8E-4}).
#' @return A character string. Values < 0.001 are formatted in scientific notation with 1 significant
#'   figure (e.g., \code{"8E-4"}). All other values use fixed-point notation rounded to \code{digits}
#'   decimal places.
#' @export
val_p_format <- function(p, digits = 3) {
  if (is.na(p)) {
    return(NA_character_)
  }

  # Handle p-values >= 1 or very close to 1 (due to rounding/floating point issues)
  if (p >= 1.0 || round(p, digits) >= 1.0) {
    return(paste0(">0.", paste(rep("9", digits), collapse = "")))
  }

  # Use proper rounding (5 rounds up, not banker's rounding)
  p_rounded <- round(p + .Machine$double.eps, digits)

  if (p < 0.001) {
    base <- signif(p, 1)
    sci <- format(base, scientific = TRUE, digits = 1)
    # Capitalize E and remove leading zeros from exponent
    sci <- gsub("e", "E", sci)
    sci <- gsub("E-0+", "E-", sci)
    sci <- gsub("E\\+0+", "E+", sci)
    return(sci)
  }

  if (p >= 0.001 && p < 0.1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  if (p >= 0.94 && p < 1) {
    return(sprintf(paste0("%.", digits, "f"), p_rounded))
  }

  # For all other values, use the specified digits
  return(sprintf(paste0("%.", digits, "f"), p_rounded))
}

#' Format a mean +/- SD string
#'
#' @param mean Numeric mean value. Formatted to 1 decimal place.
#' @param sd Numeric standard deviation. Formatted to 1 decimal place.
#' @return A character string of the form \code{"X.X \u00b1 Y.Y"} where both values are
#'   rendered to 1 decimal place using fixed-point notation.
#' @export
val_format <- function(mean, sd) {
  paste0(
    formatC(mean, format = "f", digits = 1),
    " \u00b1 ",
    formatC(sd, format = "f", digits = 1)
  )
}

export_to_excel <- function(tbl, filename) {
  # Simple Excel export without formatting
  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(tbl, path = filename)
}

# Internal helper: generate n footnote symbols in standard academic order.
# style = "symbols": *, \u2020, \u2021, \u00a7, \u00b6, \u2225, then doubles (**, \u2020\u2020, ...)
# style = "alphabet": Unicode superscript Latin letters (\u1d43=\u1d43, \u1d47=b, \u1d9c=c, ...)
.footnote_symbol_seq <- function(n, style = "symbols") {
  if (style == "alphabet") {
    base <- c("\u1d43", "\u1d47", "\u1d9c", "\u1d48", "\u1d49", "\u1da0",
              "\u1d4d", "\u02b0", "\u2071", "\u02b2", "\u1d4f", "\u02e1",
              "\u1d50", "\u207f", "\u1d52", "\u1d56", "\u02b3", "\u02e2",
              "\u1d57", "\u1d58", "\u1d5b", "\u02b7", "\u02e3", "\u02b8",
              "\u1dbb")
    all_syms <- c(base, paste0(base, base))
  } else {
    base     <- c("*", "\u2020", "\u2021", "\u00a7", "\u00b6", "\u2225")
    doubled  <- unlist(lapply(base, function(s) paste0(s, s)))
    all_syms <- c(base, doubled)
  }
  all_syms[seq_len(min(n, length(all_syms)))]
}

# Internal helper: open a file with the OS-level default application,
# bypassing IDE shell interception (e.g. VS Code's browseURL handler).
.open_docx <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)
  sysname <- Sys.info()[["sysname"]]
  if (sysname == "Darwin") {
    system2("open", shQuote(path), wait = FALSE)
  } else if (sysname == "Windows") {
    shell.exec(path)
  } else {
    system2("xdg-open", shQuote(path), wait = FALSE)
  }
  invisible(NULL)
}

# Internal helper: returns the 3-part public release version of TernTables,
# stripping any development tag (e.g. .9008) so footers always show the
# public release number (e.g. "1.6.3").
.tern_pkg_version <- function() {
  raw <- unclass(utils::packageVersion("TernTables"))[[1]]
  paste(raw[seq_len(min(3L, length(raw)))], collapse = ".")
}

# Internal helper: returns the full citation line used in Word document
# footers when citation = TRUE. Version is resolved dynamically.
.tern_citation_line <- function() {
  ver <- .tern_pkg_version()
  paste0(
    "Created with TernTables v", ver, ". Please cite: ",
    "Preston JD, Abadiotakis H, Tang A, Rust CJ, Halkos ME, Daneshmand MA, Chan JL. ",
    "TernTables: A Statistical Analysis and Table Generation Web Interface for Clinical and Biomedical Research. ",
    "bioRxiv 2026.04.15.717241. ",
    "https://doi.org/10.64898/2026.04.15.717241"
  )
}
