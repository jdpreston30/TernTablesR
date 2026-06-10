#' Export a custom tibble to Word with TernTables formatting
#'
#' \code{ternStyle()} renders any user-built tibble into a Word document with
#' the exact same visual style as tables produced by \code{ternG()},
#' \code{ternD()}, and \code{word_export()} -- Arial font, grey header,
#' double-bar footer, caption/footnote block, and citation footer.
#'
#' Use this function when you have pre-computed summary statistics in a tibble
#' (e.g. a custom cross-tab or manually assembled output table) and want it to
#' match the rest of your TernTables document without running it through the full
#' \code{ternG}/\code{ternD} pipeline.
#'
#' @param tbl A data frame or tibble. The first column is used as the row-label
#'   column (rendered as "Variable" unless renamed via \code{col1_name}).
#'   All columns are coerced to character before rendering; \code{NA} values
#'   become empty strings.
#' @param filename Output file path ending in \code{.docx}. Pass \code{NULL}
#'   (default) to write to a temporary file and suppress auto-opening -- useful
#'   when the result will be passed directly to \code{\link{ternB}} for bundling.
#' @param col1_name Optional character string. If supplied, the first column is
#'   renamed to this label in the rendered table. The column need not be named
#'   \code{"Variable"} in the input; any name is accepted and renamed here.
#'   Default \code{NULL} (use the tibble's existing first column name).
#' @param subheader_rows Character vector of labels that already exist as rows
#'   in \code{tbl} and should be formatted as full section-header rows: cells
#'   merged across all columns, bold, with a bottom border line -- identical to
#'   the treatment applied by \code{category_start}. No row is inserted; the
#'   matching existing row is formatted in place. Matching is case-insensitive.
#'   Default \code{NULL}.
#' @param bold_rows Integer vector of body row indices (1-based, final rendered
#'   table) to bold across every column. Applied after all structural formatting
#'   so it always wins. Default \code{NULL}.
#' @param bold_sig Optional named list for cell-level p-value-based bolding.
#'   Use this when your tibble has pre-formatted p-value strings in columns that
#'   are not named \code{"P value"} (e.g. \code{"Uni p"}, \code{"Multi p"}).
#'   Supply a list with:
#'   \itemize{
#'     \item \code{p_cols} — character vector of column names containing p-value strings.
#'     \item \code{hr_cols} — optional character vector of column names (same length and
#'       order as \code{p_cols}) to also bold when the paired p-value is significant
#'       (e.g. the corresponding HR or coefficient column). Pass \code{NULL} or omit
#'       to skip paired-column bolding.
#'     \item \code{threshold} — numeric significance threshold. Default \code{0.05}.
#'   }
#'   The Variable column is never modified by \code{bold_sig}; use \code{bold_rows} to
#'   bold entire rows (e.g., predictor-level rows where the p-value represents an
#'   omnibus LRT). Example:
#'   \preformatted{bold_sig = list(
#'     p_cols    = c("Uni p", "Multi p"),
#'     hr_cols   = c("Uni HR (95\% CI)", "Multi HR (95\% CI)"),
#'     threshold = 0.05
#'   )}  Default \code{NULL}.
#' @param italic_rows Integer vector of body row indices to italicize across
#'   every column. Default \code{NULL}.
#' @param bold_cols Integer vector of column indices (1-based) to bold across
#'   all body rows. Default \code{NULL}.
#' @param italic_cols Integer vector of column indices to italicize across all
#'   body rows. Default \code{NULL}.
#' @param header_format_follow Logical; if \code{TRUE}, columns listed in
#'   \code{bold_cols} or \code{italic_cols} also have their header cell bolded
#'   or italicized. Default \code{FALSE}.
#' @param round_intg Logical; passed to \code{word_export}. Default \code{FALSE}.
#' @param round_decimal Integer or NULL; if provided, rounds all numeric values in the
#'   table to this many decimal places before rendering. Passed to \code{word_export}.
#'   Default \code{NULL} (no rounding).
#' @param font_size Numeric; font size for table body. Default \code{9}.
#' @param category_start Named character vector; same as in \code{word_export}.
#'   Insert new section-header rows at anchor variable positions, in addition to
#'   any rows already in the tibble. Default \code{NULL}.
#' @param plain_header Named character vector; same as in \code{word_export}.
#'   Insert underline-only (no bold, no merge) label rows at anchor positions.
#'   Default \code{NULL}.
#' @param manual_italic_indent Character vector of row labels to italicize and
#'   indent (sub-item appearance). Default \code{NULL}.
#' @param manual_underline Character vector of row labels to underline (multi-
#'   category header appearance without the full subheader treatment). Default
#'   \code{NULL}.
#' @param table_caption Optional character string for the caption above the
#'   table. Default \code{NULL}.
#' @param table_footnote Optional character string for a footnote below the
#'   table. Default \code{NULL}.
#' @param abbreviation_footnote Optional character string (or character vector)
#'   of abbreviations. Always printed first in the footnote block. Default
#'   \code{NULL}.
#' @param variable_footnote Optional named character vector of per-variable
#'   footnote definitions (case-insensitive name match). To share one footnote
#'   symbol between multiple variables, separate their names with a pipe:
#'   \code{c("Var A|Var B" = "Shared note text.")}. Default \code{NULL}.
#' @param index_style Character; \code{"symbols"} (default) or
#'   \code{"alphabet"}. Controls the footnote symbol sequence. See
#'   \code{word_export} for details.
#' @param col1_header Optional character string. Overrides the top-left header
#'   cell. When \code{NULL} (default), the standard \code{"Category\\n   Variable"}
#'   label is used. Example: \code{"Variable\\n   Index Management Strategy"}.
#' @param line_break_header Logical; if \code{TRUE}, column headers are wrapped
#'   with \code{\\n} and the first column header shows the two-line
#'   \code{"Category / Variable"} label. For custom tibbles the column names are
#'   typically already formatted, so this defaults to \code{FALSE} here (unlike
#'   \code{word_export} where it defaults to \code{TRUE}).
#' @param open_doc Logical; if \code{TRUE} (default), opens the written
#'   document after saving. Default \code{TRUE}.
#' @param citation Logical; if \code{TRUE} (default), appends the TernTables
#'   citation line in the page footer. Default \code{TRUE}.
#' @param font_family Character; font family name used for all Word output.
#'   Defaults to \code{getOption("TernTables.font_family", "Arial")}.
#'   See \code{\link{word_export}} for details.
#' @param col1_spanner_pos Character; \code{"top"} (default) places the first-column header
#'   label in the spanner row for visual prominence; \code{"bottom"} keeps the classic layout
#'   with the label in the column-names row. Has no effect when \code{spanner} is \code{NULL}.
#' @param spanner Optional named list of column spanners (decked header). Each element name
#'   is the spanner label; the value is a character vector of column names from \code{tbl}
#'   that fall under that spanner. Columns not listed receive an empty spanner cell. The spanner
#'   row sits above the column-header row and shares the same grey background and bold styling.
#'   Supply the column names exactly as they appear in \code{tbl}.
#'
#'   To strip redundant group prefixes from column headers (e.g. when the spanner already
#'   provides the group label), use a \emph{named} inner vector where the \strong{names} are
#'   the display labels and the \strong{values} are the actual column names in \code{tbl}:
#'   \preformatted{spanner = list(
#'     "SLAM"    = c("r" = "SLAM r",    "p" = "SLAM p"),
#'     "Control" = c("r" = "Control r", "p" = "Control p"),
#'     "NMN"     = c("r" = "NMN r",     "p" = "NMN p")
#'   )}
#'   Unnamed inner vectors leave column headers unchanged:
#'   \preformatted{spanner = list(
#'     "Univariate"   = c("Uni HR (95\% CI)", "Uni p"),
#'     "Multivariate" = c("Multi HR (95\% CI)", "Multi p")
#'   )}
#'   Default \code{NULL} (no spanner row).
#' @return Invisibly returns the input tibble (after renaming and coercion)
#'   with a \code{"ternB_meta"} attribute attached. This makes the result
#'   directly passable to \code{\link{ternB}} for bundling with other tables
#'   into a combined Word document. Any \code{spanner} definition is stored in
#'   the metadata and replayed automatically by \code{ternB()}.
#' @examples
#' \donttest{
#' library(tibble)
#' # Basic usage with section headers
#' my_tbl <- tibble(
#'   Variable      = c("Section A", "Row 1", "Row 2", "Section B", "Row 3"),
#'   `Group 1`     = c("",          "12 (40%)", "18 (60%)", "", "9 (30%)"),
#'   `Group 2`     = c("",          "15 (50%)", "15 (50%)", "", "21 (70%)")
#' )
#' ternStyle(
#'   tbl             = my_tbl,
#'   filename        = file.path(tempdir(), "custom_table.docx"),
#'   subheader_rows  = c("Section A", "Section B"),
#'   open_doc        = FALSE,
#'   citation        = FALSE
#' )
#'
#' # Spanner (decked) header grouping related columns
#' reg_tbl <- tibble(
#'   Variable            = c("Age (yr)", "BMI"),
#'   `Uni HR (95% CI)`   = c("1.02 [0.98-1.06]", "1.11 [1.03-1.19]"),
#'   `Uni p`             = c("0.31", "0.006"),
#'   `Multi HR (95% CI)` = c("1.01 [0.97-1.05]", "1.08 [1.00-1.17]"),
#'   `Multi p`           = c("0.64", "0.047")
#' )
#' ternStyle(
#'   tbl         = reg_tbl,
#'   filename    = file.path(tempdir(), "regression_table.docx"),
#'   col1_header = "Variable",
#'   spanner     = list(
#'     "Univariate"   = c("Uni HR (95% CI)", "Uni p"),
#'     "Multivariate" = c("Multi HR (95% CI)", "Multi p")
#'   ),
#'   open_doc    = FALSE,
#'   citation    = FALSE
#' )
#' }
#' @export
ternStyle <- function(
    tbl,
    filename              = NULL,
    col1_name             = NULL,
    subheader_rows        = NULL,
    bold_rows             = NULL,
    bold_sig              = NULL,
    italic_rows           = NULL,
    bold_cols             = NULL,
    italic_cols           = NULL,
    header_format_follow  = FALSE,
    round_intg            = FALSE,
    round_decimal         = NULL,
    font_size             = 9,
    category_start        = NULL,
    plain_header          = NULL,
    manual_italic_indent  = NULL,
    manual_underline      = NULL,
    table_caption         = NULL,
    table_footnote        = NULL,
    abbreviation_footnote = NULL,
    variable_footnote     = NULL,
    index_style           = "symbols",
    col1_header           = NULL,
    line_break_header     = FALSE,
    open_doc              = TRUE,
    citation              = TRUE,
    font_family           = getOption("TernTables.font_family", "Arial"),
    spanner               = NULL,
    col1_spanner_pos      = c("top", "bottom")
) {
  stopifnot(is.data.frame(tbl))
  tbl <- tibble::as_tibble(tbl)

  # ── Rename first column ───────────────────────────────────────────────────
  if (!is.null(col1_name)) {
    colnames(tbl)[1] <- as.character(col1_name)
  }
  # word_export expects the first column to be named "Variable" for indent logic
  colnames(tbl)[1] <- "Variable"

  # ── Coerce all columns to character ──────────────────────────────────────
  for (j in seq_len(ncol(tbl))) {
    if (!is.character(tbl[[j]])) tbl[[j]] <- as.character(tbl[[j]])
    tbl[[j]][is.na(tbl[[j]])] <- ""
  }

  # ── Assign .indent: 2L (normal body row) for all rows by default ─────────
  # subheader_rows get 0L so the per-row formatting loop treats them as
  # indent_level == 0; the subheader_rows → category_rows pass in word_export
  # will then apply the full merge/bold/border treatment on top.
  tbl[[".indent"]] <- 2L
  if (!is.null(subheader_rows) && length(subheader_rows) > 0) {
    for (sr in subheader_rows) {
      idx <- which(trimws(tbl[["Variable"]]) == sr)
      if (length(idx) == 0)
        idx <- which(tolower(trimws(tbl[["Variable"]])) == tolower(sr))
      if (length(idx) > 0) tbl[[".indent"]][idx] <- 0L
    }
  }

  # ── Resolve filename ────────────────────────────────────────────────────
  if (is.null(filename)) {
    filename <- tempfile(fileext = ".docx")
    open_doc <- FALSE
  }

  word_export(
    tbl                   = tbl,
    filename              = filename,
    round_intg            = round_intg,
    round_decimal         = round_decimal,
    font_size             = font_size,
    category_start        = category_start,
    plain_header          = plain_header,
    subheader_rows        = subheader_rows,
    bold_rows             = bold_rows,
    bold_sig              = bold_sig,
    italic_rows           = italic_rows,
    bold_cols             = bold_cols,
    italic_cols           = italic_cols,
    header_format_follow  = header_format_follow,
    manual_italic_indent  = manual_italic_indent,
    manual_underline      = manual_underline,
    table_caption         = table_caption,
    table_footnote        = table_footnote,
    abbreviation_footnote = abbreviation_footnote,
    posthoc_footnote      = NULL,
    variable_footnote     = variable_footnote,
    index_style           = index_style,
    col1_header           = col1_header,
    line_break_header     = line_break_header,
    open_doc              = open_doc,
    citation              = citation,
    font_family           = font_family,
    spanner               = spanner,
    col1_spanner_pos      = col1_spanner_pos
  )

  # ── Attach ternB_meta so this table can be passed to ternB() ─────────────
  # tbl at this point already has the .indent column set (see above), which is
  # exactly what word_export() / ternB() needs to replay the rendering.
  # ternStyle tables have no statistical test machinery, so OR_col, post_hoc,
  # p_adjust, etc. are set to their safe no-op defaults (matching ternD).
  attr(tbl, "ternB_meta") <- list(
    tbl                   = tbl,
    round_intg            = round_intg,
    round_decimal         = round_decimal,
    font_size             = font_size,
    category_start        = category_start,
    plain_header          = plain_header,
    manual_italic_indent  = manual_italic_indent,
    manual_underline      = manual_underline,
    table_caption         = table_caption,
    table_footnote        = table_footnote,
    abbreviation_footnote = abbreviation_footnote,
    posthoc_footnote      = NULL,
    variable_footnote     = variable_footnote,
    index_style           = index_style,
    line_break_header     = line_break_header,
    subheader_rows        = subheader_rows,
    bold_rows             = bold_rows,
    bold_sig              = bold_sig,
    italic_rows           = italic_rows,
    bold_cols             = bold_cols,
    italic_cols           = italic_cols,
    header_format_follow  = header_format_follow,
    col1_header           = col1_header,
    spanner               = spanner,
    col1_spanner_pos      = col1_spanner_pos,
    source                = "ternStyle",
    n_levels              = 1L,
    OR_col                = FALSE,
    OR_method             = "dynamic",
    post_hoc              = FALSE,
    p_adjust              = FALSE,
    p_adjust_display      = "replace",
    citation              = citation,
    font_family           = font_family
  )

  invisible(tbl)
}
