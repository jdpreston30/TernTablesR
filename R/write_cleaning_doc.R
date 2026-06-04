#' Write a cleaning summary document for ternP output
#'
#' Generates a Word document summarising the preprocessing transformations
#' applied by \code{\link{ternP}}. Only sections for triggered transformations
#' are written; if the data required no preprocessing, a single sentence
#' stating that is produced instead. The document can be attached to a
#' data-management log or supplemental materials.
#'
#' @param result A \code{ternP_result} object returned by \code{\link{ternP}}.
#' @param filename Output file path ending in \code{.docx}.
#'   Default is \code{"cleaning_summary.docx"} in the current working directory.
#' @param font_family Character; font family for the Word document. Default \code{"Arial"}.
#'   Can also be set via \code{options(TernTables.font_family = ...)}.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens the written Word
#'   document in the system default application after saving. Set to \code{FALSE} to suppress.
#' @param citation Logical; if \code{TRUE} (default), appends the TernTables citation as a
#'   page footer in the written document. Set to \code{FALSE} to suppress.
#' @return Invisibly returns the path to the written Word file.
#' @seealso \code{\link{ternP}}, \code{\link{write_methods_doc}}
#' @examples
#' \donttest{
#' path   <- system.file("extdata/csv", "tern_colon_messy.csv",
#'                       package = "TernTables")
#' raw    <- read.csv(path, stringsAsFactors = FALSE)
#' result <- ternP(raw)
#' write_cleaning_doc(result, filename = file.path(tempdir(), "cleaning_summary.docx"),
#'                   open_doc = FALSE)
#' }
#' @export
write_cleaning_doc <- function(result,
                               filename = "cleaning_summary.docx",
                               font_family = getOption("TernTables.font_family", "Arial"),
                               open_doc = TRUE,
                               citation = TRUE) {

  if (!inherits(result, "ternP_result")) {
    stop("`result` must be an object returned by `ternP()`.", call. = FALSE)
  }

  fb      <- result$feedback
  n_clean <- nrow(result$clean_data)
  n_cols  <- ncol(result$clean_data)

  # ── officer text / paragraph properties ─────────────────────────────────────
  head_props   <- fp_text(font.size = 11, font.family = font_family, bold = TRUE)
  body_props   <- fp_text(font.size = 11, font.family = font_family)
  bullet_par   <- fp_par(padding.left = 14)

  make_heading <- function(txt) fpar(ftext(txt, prop = head_props))
  make_body    <- function(txt) fpar(ftext(txt, prop = body_props))
  make_bullet  <- function(txt) fpar(ftext(paste0("\u2022  ", txt), prop = body_props),
                                     fp_p = bullet_par)

  # ── Initialise document ──────────────────────────────────────────────────────
  doc <- read_docx() |>
    body_add_fpar(make_heading("Data Preprocessing Summary (ternP)")) |>
    body_add_par("", style = "Normal")

  # ── Body: clean vs transformed ───────────────────────────────────────────────
  clean_flag <- all(vapply(fb, is.null, logical(1)))

  if (clean_flag) {
    doc <- doc |>
      body_add_fpar(make_body(paste0(
        "No transformations were required. The input data passed all validation ",
        "checks and preprocessing steps without modification."
      )))
  } else {

    doc <- doc |>
      body_add_fpar(make_body("The following transformations were applied:")) |>
      body_add_par("", style = "Normal")

    if (!is.null(fb$string_na_converted)) {
      sna      <- fb$string_na_converted
      n_tot    <- sna$total
      n_cols_s <- length(sna$cols)
      col_list <- paste(sna$cols, collapse = ", ")
      doc <- doc |>
        body_add_fpar(make_bullet(paste0(
          n_tot, if (n_tot == 1L) " string NA value (" else " string NA values (",
          "\u201cNA\u201d, \u201cna\u201d, \u201cNa\u201d, \u201cunk\u201d) were detected across ",
          n_cols_s, if (n_cols_s == 1L) " column" else " columns",
          " and converted to true NA: ", col_list, "."
        )))
    }

    if (!is.null(fb$dropped_empty_cols)) {
      n_d      <- length(fb$dropped_empty_cols)
      col_list <- paste(fb$dropped_empty_cols, collapse = ", ")
      doc <- doc |>
        body_add_fpar(make_bullet(paste0(
          n_d, if (n_d == 1L) " column was" else " columns were",
          " dropped because all values were missing: ", col_list, "."
        )))
    }

    if (!is.null(fb$blank_rows_removed)) {
      br      <- fb$blank_rows_removed
      idx_str <- paste(br$row_indices, collapse = ", ")
      doc <- doc |>
        body_add_fpar(make_bullet(paste0(
          br$count,
          if (br$count == 1L) " completely blank row was" else " completely blank rows were",
          " removed from the dataset (original row ",
          if (br$count == 1L) "position" else "positions",
          ": ", idx_str, ")."
        )))
    }

    if (!is.null(fb$case_normalized_vars)) {
      cn           <- fb$case_normalized_vars
      n_n          <- length(cn$cols)
      detail_lines <- vapply(cn$cols, function(col_nm) {
        d     <- cn$detail[[col_nm]]
        pairs <- paste0(
          "\u201c", d$changed_from, "\u201d \u2192 \u201c", d$changed_to, "\u201d",
          collapse = ", "
        )
        paste0(col_nm, ": ", pairs)
      }, character(1))
      detail_str <- paste(detail_lines, collapse = "; ")
      doc <- doc |>
        body_add_fpar(make_bullet(paste0(
          "Capitalization inconsistencies were detected and standardized to title case in ",
          n_n, if (n_n == 1L) " column" else " columns",
          ". ", detail_str, "."
        )))
    }

    if (!is.null(fb$sparse_rows_flagged)) {
      sp      <- fb$sparse_rows_flagged
      idx_str <- paste(sp$row_indices, collapse = ", ")
      doc <- doc |>
        body_add_fpar(make_bullet(paste0(
          sp$count,
          if (sp$count == 1L) " row was" else " rows were",
          " flagged as sparse (more than 50% of values missing) and retained in ",
          "the cleaned dataset (row ",
          if (sp$count == 1L) "position" else "positions",
          " in cleaned data: ", idx_str, "). ",
          "These rows are available via result$sparse_rows for review or export."
        )))
    }

  }

  # ── Footer: final dimensions ─────────────────────────────────────────────────
  doc <- doc |>
    body_add_par("", style = "Normal") |>
    body_add_fpar(make_body(paste0(
      "Final cleaned data: ",
      n_clean, if (n_clean == 1L) " row" else " rows",
      " \u00d7 ",
      n_cols,  if (n_cols  == 1L) " column" else " columns",
      "."
    )))

  if (isTRUE(citation)) {
    cit_props <- fp_text(font.family = font_family, font.size = 7,
                         bold = TRUE, italic = TRUE, color = "black")
    doc <- body_set_default_section(
      doc,
      value = prop_section(
        footer_default = block_list(
          fpar(ftext(.tern_citation_line(), prop = cit_props))
        )
      )
    )
  }

  dir.create(dirname(filename), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = filename)
  if (isTRUE(open_doc)) .open_docx(filename)
  cli::cli_alert_success("Cleaning summary written to: {filename}")
  invisible(filename)
}
