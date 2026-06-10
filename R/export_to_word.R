#' Export TernTables output to a formatted Word document
#'
#' @param tbl A tibble created by ternG or ternD
#' @param filename Output file path ending in .docx
#' @param round_intg Logical; if TRUE, adds note about integer rounding. Default is FALSE.
#' @param round_decimal Integer or NULL; if provided, rounds all numeric values in the table
#'   to this many decimal places before rendering. Default is \code{NULL} (no rounding).
#' @param font_size Numeric; font size for table body. Default is 9.
#' @param category_start Named character vector specifying category headers. Names are header
#'   label text; values are anchor variable names -- either the original column name or the
#'   cleaned display name (both forms accepted).
#' @param plain_header Named character vector, same interface as \code{category_start}. Names are
#'   the label text to display; values are the anchor variable to insert before. The inserted row
#'   has text only in column 1 (all other cells blank) and receives \emph{underline} formatting --
#'   identical to \code{manual_underline} -- but no bold, merge, or border treatments.
#'   Default is \code{NULL} (none).
#' @param subheader_rows Character vector of labels that already exist as rows in the table and
#'   should be formatted as full category section headers (merged across all columns, bold, with a
#'   bottom border line). Unlike \code{category_start}, no new row is inserted -- the matching
#'   existing row is formatted in place. Intended for use with \code{ternStyle()} where section-
#'   divider rows are pre-built into the tibble. Case-insensitive match. Default \code{NULL}.
#' @param bold_rows Integer vector of body row indices (1-based, in the final rendered table) to
#'   bold across every column. Applied as the last formatting pass so it overrides structural
#'   formatting. Default \code{NULL}.
#' @param bold_sig Optional named list for cell-level conditional bolding based on parsed p-values.
#'   Intended for use with \code{ternStyle()} when columns contain pre-formatted p-values that
#'   are not named \code{"P value"} (the name \code{ternG} uses internally). Supply a list with:
#'   \itemize{
#'     \item \code{p_cols} — character vector of column names containing p-value strings.
#'     \item \code{hr_cols} — optional character vector of column names to also bold when the
#'       paired p-value is significant (must be the same length and order as \code{p_cols},
#'       or \code{NULL} to skip paired HR bolding).
#'     \item \code{threshold} — numeric significance threshold; default \code{0.05}.
#'   }
#'   For each p-value cell where the parsed numeric value is below \code{threshold}, that cell
#'   is bolded. If \code{hr_cols} is supplied, the corresponding HR cell in the same row is also
#'   bolded. The Variable column is never touched by this argument — use \code{bold_rows} to bold
#'   entire rows (e.g., predictor header rows). Default \code{NULL}.
#' @param italic_rows Integer vector of body row indices to italicize across every column.
#'   Default \code{NULL}.
#' @param bold_cols Integer vector of column indices (1-based) to bold across all body rows.
#'   Default \code{NULL}.
#' @param italic_cols Integer vector of column indices to italicize across all body rows.
#'   Default \code{NULL}.
#' @param header_format_follow Logical; if \code{TRUE}, any columns listed in \code{bold_cols}
#'   or \code{italic_cols} also have their header cell bolded or italicized, respectively.
#'   Default \code{FALSE}.
#' @param manual_italic_indent Character vector of display variable names (post-cleaning) to force into
#'   italicized and indented formatting, matching the appearance of factor sub-category rows (e.g., levels
#'   of a multi-category variable). Use this for rows that should visually appear as sub-items but are not
#'   automatically detected as such.
#' @param manual_underline Character vector of display variable names (post-cleaning) to force into
#'   underlined formatting, matching the appearance of multi-category variable header rows. Use this for
#'   rows that should visually appear as section headers but are not automatically detected as such.
#' @param table_caption Optional character string to display as a caption above the table in the Word
#'   document. Rendered as size 11 Arial bold, single-spaced with a small gap before the table.
#'   Default is \code{NULL} (no caption).
#' @param table_footnote Optional character string to display as a footnote below the table in the Word
#'   document. Rendered as size 6 Arial italic. A double-bar border is applied above and below the
#'   footnote row. Default is \code{NULL} (no footnote).
#' @param abbreviation_footnote Optional character string (or character vector, which will be
#'   collapsed with spaces) listing abbreviations to display at the top of the footnote block.
#'   Always printed first, before any variable-specific footnote lines. Default \code{NULL}.
#' @param posthoc_footnote Optional character string describing post-hoc CLD superscript
#'   conventions. When supplied by \code{ternG()}, it is inserted after the abbreviations
#'   footnote and before the variable symbol footnotes. Default \code{NULL}.
#' @param variable_footnote Optional named character vector. Names are display variable names as
#'   they appear in the table (case-insensitive match); values are the footnote definition text
#'   for that variable. Each entry is assigned the next symbol in the sequence (*, dagger,
#'   double-dagger, ...) and the symbol is appended to the variable name in column 1.
#'   The footnote block lists each as \code{"* Definition text."} below the abbreviations.
#'   To map multiple variables to the same footnote symbol and note, separate the variable
#'   names with a pipe character in the key: \code{c("Var A|Var B" = "Shared note text.")}.
#'   Default \code{NULL}.
#' @param index_style Character; controls the footnote symbol sequence. \code{"symbols"} (default)
#'   uses *, dagger, double-dagger, section, pilcrow, double-vertical-bar, then doubled forms.
#'   \code{"*"} is appended as plain text; all others are rendered as true Word superscripts.
#'   \code{"alphabet"} uses Unicode superscript letters (a, b, c, ...) which render as raised
#'   glyphs without explicit superscript formatting.
#' @param page_break_after Logical; if \code{TRUE}, a page break is appended at the end of the
#'   Word document after the table. Used internally by \code{ternB()} to embed page breaks inside
#'   each table's temp file rather than injecting them into the combined document body, which
#'   avoids double-break artifacts when tables do not fill the page. Default is \code{FALSE}.
#' @param col1_header Optional character string. Overrides the top-left header cell text.
#'   When \code{NULL} (default), the cell shows \code{"Category\\n   Variable"} (the standard
#'   two-line label). Supply any string, including \code{"\\n"} line breaks, to customise.
#'   Example: \code{"Variable\\n   Index Management Strategy"}.
#' @param line_break_header Logical; if \code{TRUE} (default), column headers are wrapped with
#'   \code{\\n} -- group names break on spaces, sample size counts move to a second line, and
#'   the first column header includes a category hierarchy label. Set to \code{FALSE} to suppress
#'   all header line breaks. Can also be set package-wide via
#'   \code{options(TernTables.line_break_header = FALSE)}.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens the written Word
#'   document in the system default application after saving. Set to \code{FALSE} to suppress.
#' @param citation Logical; if \code{TRUE} (default), appends a citation line at the bottom
#'   of the table footnote block: package version, authors, and links to the GitHub repository
#'   and web interface. Set to \code{FALSE} to suppress.
#' @param font_family Character; font family used for the entire Word table and its caption,
#'   footnote, and citation. Any font name accepted by the rendering system is valid (Word
#'   will fall back to its default if the font is not installed). Can also be set package-wide
#'   via \code{options(TernTables.font_family = "Garamond")}.
#'   Default is \code{"Arial"}.
#' @param col1_spanner_pos Character; controls where the first-column header label appears
#'   when \code{spanner} is active. \code{"top"} (default) places the label in the spanner
#'   row (top-left cell) and leaves the column-names row cell below it empty, giving the
#'   label more visual prominence. \code{"bottom"} keeps the classic layout where the label
#'   stays in the column-names row and the spanner row cell above it is blank.
#'   Has no effect when \code{spanner} is \code{NULL}.
#' @param spanner Optional named list of column spanners (decked header). Each element name
#'   is the spanner label text; the value is a character vector of column names from \code{tbl}
#'   that fall under that spanner. Columns not listed receive an empty spanner cell. The spanner
#'   row is rendered above the normal column-header row, inheriting the same grey background,
#'   bold font, and border treatment. Supply either the original column names or their post-rename
#'   equivalents -- both are resolved.
#'
#'   To override the display label shown in the column-header row (e.g. to strip a
#'   redundant group prefix when the spanner already provides context), use a \emph{named}
#'   inner vector where the \strong{names} are the display labels and the \strong{values} are
#'   the actual column names in \code{tbl}:
#'   \preformatted{spanner = list(
#'     "SLAM"    = c("r" = "SLAM r",    "p" = "SLAM p"),
#'     "Control" = c("r" = "Control r", "p" = "Control p"),
#'     "NMN"     = c("r" = "NMN r",     "p" = "NMN p")
#'   )}
#'   Unnamed inner vectors (the simple form) leave column headers unchanged:
#'   \preformatted{spanner = list(
#'     "Univariate"    = c("Uni HR (95\% CI)", "Uni p"),
#'     "Multivariate"  = c("Multi HR (95\% CI)", "Multi p")
#'   )}
#'   Default \code{NULL} (no spanner row).
#' @return Invisibly returns the path to the written Word file.
#' @examples
#' \donttest{
#' data(tern_colon)
#' tbl <- ternD(tern_colon, exclude_vars = c("ID"), methods_doc = FALSE, open_doc = FALSE)
#' word_export(
#'   tbl      = tbl,
#'   filename = file.path(tempdir(), "descriptive.docx"),
#'   open_doc = FALSE,
#'   category_start = c(
#'     "Patient Demographics"  = "Age (yr)",
#'     "Tumor Characteristics" = "Positive Lymph Nodes (n)"
#'   )
#' )
#' }
#' @export
word_export <- function(tbl, filename, round_intg = FALSE, round_decimal = NULL, font_size = 9, category_start = NULL, plain_header = NULL, subheader_rows = NULL, bold_rows = NULL, bold_sig = NULL, italic_rows = NULL, bold_cols = NULL, italic_cols = NULL, header_format_follow = FALSE, manual_italic_indent = NULL, manual_underline = NULL, table_caption = NULL, table_footnote = NULL, abbreviation_footnote = NULL, posthoc_footnote = NULL, variable_footnote = NULL, index_style = "symbols", page_break_after = FALSE, col1_header = NULL, line_break_header = getOption("TernTables.line_break_header", TRUE), open_doc = TRUE, citation = TRUE, font_family = getOption("TernTables.font_family", "Arial"), spanner = NULL, col1_spanner_pos = c("top", "bottom")) {
  col1_spanner_pos <- match.arg(col1_spanner_pos)

  # Keep the table as-is
  modified_tbl <- tbl

  # Insert category header rows before extracting .indent
  if (!is.null(category_start) && length(category_start) > 0) {
    for (header_label in names(category_start)) {
      var_name <- category_start[[header_label]]
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      var_matches <- which(trimmed_vars == var_name)

      # Fallback 1: case-insensitive exact match (handles smart_rename sentence-case output)
      if (length(var_matches) == 0) {
        var_matches <- which(tolower(trimmed_vars) == tolower(var_name))
      }

      # Fallback 2: try matching the cleaned display form (allows raw column names as anchors,
      # e.g. "Age_Years" → "Age (yr)")
      if (length(var_matches) == 0) {
        cleaned_anchor <- .clean_variable_name_for_header(var_name)
        var_matches <- which(trimmed_vars == cleaned_anchor)
        # Fallback 2b: case-insensitive cleaned form
        if (length(var_matches) == 0) {
          var_matches <- which(tolower(trimmed_vars) == tolower(cleaned_anchor))
        }
      }

      if (length(var_matches) > 1) {
        second_col <- modified_tbl[[2]]
        header_match <- var_matches[which(second_col[var_matches] == "" | is.na(second_col[var_matches]))]
        var_idx <- if (length(header_match) > 0) header_match[1] else var_matches[1]
      } else if (length(var_matches) == 1) {
        var_idx <- var_matches[1]
      } else {
        next
      }

      cat_row <- modified_tbl[1, ]
      cat_row[[1]][1] <- header_label
      for (j in 2:ncol(cat_row)) {
        if (is.numeric(cat_row[[j]])) cat_row[[j]][1] <- NA_real_
        else cat_row[[j]][1] <- ""
      }
      if (".indent" %in% colnames(cat_row)) cat_row[[".indent"]][1] <- 0L

      if (var_idx == 1) {
        modified_tbl <- rbind(cat_row, modified_tbl)
      } else {
        modified_tbl <- rbind(
          modified_tbl[1:(var_idx - 1), ],
          cat_row,
          modified_tbl[var_idx:nrow(modified_tbl), ]
        )
      }
    }
  }

  # Insert plain header rows (underlined label-only rows, no subheader styling)
  if (!is.null(plain_header) && length(plain_header) > 0) {
    for (ph_label in names(plain_header)) {
      var_name <- plain_header[[ph_label]]
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      var_matches <- which(trimmed_vars == var_name)
      if (length(var_matches) == 0)
        var_matches <- which(tolower(trimmed_vars) == tolower(var_name))
      if (length(var_matches) == 0) {
        cleaned_anchor <- .clean_variable_name_for_header(var_name)
        var_matches <- which(trimmed_vars == cleaned_anchor)
        if (length(var_matches) == 0)
          var_matches <- which(tolower(trimmed_vars) == tolower(cleaned_anchor))
      }
      if (length(var_matches) > 1) {
        second_col <- modified_tbl[[2]]
        header_match <- var_matches[which(second_col[var_matches] == "" | is.na(second_col[var_matches]))]
        var_idx <- if (length(header_match) > 0) header_match[1] else var_matches[1]
      } else if (length(var_matches) == 1) {
        var_idx <- var_matches[1]
      } else {
        next
      }
      ph_row <- modified_tbl[1, ]
      ph_row[[1]][1] <- ph_label
      for (j in 2:ncol(ph_row)) {
        if (is.numeric(ph_row[[j]])) ph_row[[j]][1] <- NA_real_
        else ph_row[[j]][1] <- ""
      }
      if (".indent" %in% colnames(ph_row)) ph_row[[".indent"]][1] <- 0L
      if (var_idx == 1) {
        modified_tbl <- rbind(ph_row, modified_tbl)
      } else {
        modified_tbl <- rbind(
          modified_tbl[1:(var_idx - 1), ],
          ph_row,
          modified_tbl[var_idx:nrow(modified_tbl), ]
        )
      }
    }
  }

  # Store .indent column for later use, then remove it from display
  indent_col <- if (".indent" %in% colnames(modified_tbl)) modified_tbl[[".indent"]] else NULL
  if (!is.null(indent_col)) {
    modified_tbl <- modified_tbl %>% select(-dplyr::any_of(".indent"))
  }

  # Track which rows are category headers for formatting
  category_rows <- NULL
  if (!is.null(category_start) && length(category_start) > 0) {
    for (category_label in names(category_start)) {
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      cat_idx <- which(trimmed_vars == category_label)
      if (length(cat_idx) > 0) category_rows <- c(category_rows, cat_idx)
    }
  }

  # Track which rows are plain headers for formatting
  plain_header_rows <- NULL
  if (!is.null(plain_header) && length(plain_header) > 0) {
    for (ph_label in names(plain_header)) {
      trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
      ph_idx <- which(trimmed_vars == ph_label)
      if (length(ph_idx) > 0) plain_header_rows <- c(plain_header_rows, ph_idx)
    }
  }

  # Treat subheader_rows labels as full category headers (format-only, no insertion)
  if (!is.null(subheader_rows) && length(subheader_rows) > 0) {
    trimmed_vars <- sapply(modified_tbl[[1]], function(x) trimws(x, which = "both"))
    for (sr_label in subheader_rows) {
      sr_idx <- which(trimmed_vars == sr_label)
      if (length(sr_idx) == 0) sr_idx <- which(tolower(trimmed_vars) == tolower(sr_label))
      if (length(sr_idx) > 0) category_rows <- c(category_rows, sr_idx)
    }
  }

  # ── Variable footnote: assign symbols and inject into column 1 ────────────
  # Names may be pipe-separated ("Var A|Var B") to map multiple variables to
  # the same footnote symbol and note text.
  vf_row_info <- list()
  if (!is.null(variable_footnote) && length(variable_footnote) > 0) {
    vf_names        <- names(variable_footnote)
    symbols         <- .footnote_symbol_seq(length(vf_names), index_style)
    trimmed_col1    <- trimws(modified_tbl[[1]])
    vf_row_counter  <- 0L
    for (k in seq_along(vf_names)) {
      vnames_k <- trimws(strsplit(vf_names[k], "|", fixed = TRUE)[[1]])
      sym      <- symbols[k]
      for (vname in vnames_k) {
        row_matches <- which(trimmed_col1 == vname)
        if (length(row_matches) == 0)
          row_matches <- which(tolower(trimmed_col1) == tolower(vname))
        if (length(row_matches) == 0) next
        row_idx   <- row_matches[1]
        il        <- if (!is.null(indent_col) && row_idx <= length(indent_col)) indent_col[row_idx] else 2L
        is_italic <- isTRUE(il == 6L) ||
                     (!is.null(manual_italic_indent) && vname %in% manual_italic_indent)
        # symbols: superscript all except "*"; alphabet: never (glyphs inherently raised)
        needs_super <- (index_style == "symbols") && (sym != "*")
        modified_tbl[[1]][row_idx] <- paste0(trimmed_col1[row_idx], sym)
        trimmed_col1[row_idx]      <- paste0(trimmed_col1[row_idx], sym)
        vf_row_counter <- vf_row_counter + 1L
        vf_row_info[[vf_row_counter]] <- list(row_idx = row_idx, symbol = sym,
                                              needs_super = needs_super, is_italic = is_italic)
      }
    }
  }

  # Modify column headers to add symbols and line breaks
  original_colnames <- colnames(modified_tbl)
  new_colnames <- original_colnames

  if (line_break_header) {
    # Replace first column header with category hierarchy
    new_colnames[1] <- if (!is.null(col1_header)) col1_header else "Category\n   Variable"

    # Add line breaks for sample sizes and multi-word group names
    for (i in 2:length(new_colnames)) {
      if (!new_colnames[i] %in% c("P", "test", "OR", "OR_method") && !grepl("^Total", new_colnames[i])) {
        col <- new_colnames[i]
        # Insert \n before the (n = ...) count suffix
        col <- gsub(" \\(n = ", "\n(n = ", col)
        # For multi-word group names, replace any remaining spaces in the label
        # portion (before the count suffix) with \n so they wrap automatically
        if (grepl("\n", col, fixed = TRUE)) {
          parts <- strsplit(col, "\n", fixed = TRUE)[[1]]
          # Keep "+ word" together: only break spaces in the portion before " + "
          if (grepl(" \\+ ", parts[1])) {
            plus_pos <- regexpr(" \\+ ", parts[1])
            pre  <- substr(parts[1], 1, plus_pos - 1)
            post <- paste0("+ ", substr(parts[1], plus_pos + 3, nchar(parts[1])))
            parts[1] <- paste0(gsub(" ", "\n", pre), "\n", post)
          } else {
            parts[1] <- gsub(" ", "\n", parts[1])
          }
          col <- paste(parts, collapse = "\n")
        } else {
          # Keep "+ word" together: only break spaces before " + "
          if (grepl(" \\+ ", col)) {
            plus_pos <- regexpr(" \\+ ", col)
            pre  <- substr(col, 1, plus_pos - 1)
            post <- paste0("+ ", substr(col, plus_pos + 3, nchar(col)))
            col  <- paste0(gsub(" ", "\n", pre), "\n", post)
          } else {
            col <- gsub(" ", "\n", col)
          }
        }
        new_colnames[i] <- col
      } else if (new_colnames[i] == "P") {
        new_colnames[i] <- "P value"
      } else if (grepl("^Total", new_colnames[i])) {
        # Total column already has line break from ternG, no change needed
      }
    }
  } else {
    # line_break_header = FALSE: keep group names as-is (no word-splitting),
    # but still move (n = ...) count onto its own line and rename P column.
    # The top-left header always uses the two-line label regardless.
    new_colnames[1] <- if (!is.null(col1_header)) col1_header else "Category\n   Variable"
    for (i in 2:length(new_colnames)) {
      if (!new_colnames[i] %in% c("P", "test", "OR", "OR_method") && !grepl("^Total", new_colnames[i])) {
        new_colnames[i] <- gsub(" \\(n = ", "\n(n = ", new_colnames[i])
      } else if (new_colnames[i] == "P") {
        new_colnames[i] <- "P value"
      }
    }
  }
  colnames(modified_tbl) <- new_colnames

  # Save post-rename colnames before any spanner spacer columns are inserted;
  # used below by bold_sig name resolution so lengths stay consistent.
  pre_spanner_colnames <- colnames(modified_tbl)

  # Detect which statistical tests were actually used
  has_test_column <- "test" %in% colnames(tbl)
  tests_used <- character(0)

  if (has_test_column) {
    tests_used <- unique(tbl$test)
    tests_used <- tests_used[!is.na(tests_used) & tests_used != "" & tests_used != "-"]
  }

  # ── Spanner (column group header) row ────────────────────────────────────
  # Preparation happens BEFORE flextable() so spacer columns can be inserted
  # into modified_tbl; sp_values / sp_colwidths are hoisted for the post-bold
  # border pass.
  spanner_added      <- FALSE
  sp_values          <- NULL
  sp_colwidths       <- NULL
  spacer_cols        <- integer(0)
  spanner_col_display <- list()  # actual_colname -> display label overrides
  if (!is.null(spanner) && is.list(spanner) && length(spanner) > 0) {
    n_cols <- ncol(modified_tbl)
    col_spanner_label <- rep("", n_cols)

    for (sp_label in names(spanner)) {
      inner         <- spanner[[sp_label]]
      display_names <- names(inner)   # NULL when inner vector is unnamed
      for (i in seq_along(inner)) {
        cname <- inner[[i]]
        # Resolve against post-rename colnames first, then original colnames
        pos <- which(colnames(modified_tbl) == cname)
        if (length(pos) == 0L) pos <- which(original_colnames == cname)
        if (length(pos) > 0L) {
          col_spanner_label[pos[1L]] <- sp_label
          # Capture display-label override when the inner vector is named
          if (!is.null(display_names) && nzchar(display_names[i])) {
            spanner_col_display[[ colnames(modified_tbl)[pos[1L]] ]] <- display_names[i]
          }
        }
      }
    }

    # Collapse consecutive same-label columns into merged spanner cells
    sp_values    <- character(0)
    sp_colwidths <- integer(0)
    k <- 1L
    while (k <= n_cols) {
      lbl <- col_spanner_label[k]
      m   <- k
      while (m < n_cols && col_spanner_label[m + 1L] == lbl) m <- m + 1L
      sp_values    <- c(sp_values,    lbl)
      sp_colwidths <- c(sp_colwidths, m - k + 1L)
      k <- m + 1L
    }

    # Insert a narrow empty spacer column between every pair of adjacent labeled
    # groups so the spanner underlines have a visible gap between them.
    # NOTE: new_sp_colwidths already accumulates spacer widths (1L each) as gaps
    # are inserted, so sum(new_sp_colwidths) is always the correct 1-based column
    # index of the end of the current group in the growing modified_tbl.
    new_sp_values    <- character(0)
    new_sp_colwidths <- integer(0)

    for (s in seq_along(sp_values)) {
      new_sp_values    <- c(new_sp_values,    sp_values[s])
      new_sp_colwidths <- c(new_sp_colwidths, sp_colwidths[s])

      need_spacer <- s < length(sp_values) &&
                     nzchar(sp_values[s]) &&
                     nzchar(sp_values[s + 1L])
      if (need_spacer) {
        # sum(new_sp_colwidths) is already adjusted for all previously-inserted
        # spacers because their widths (1L each) are appended to new_sp_colwidths.
        insert_at <- sum(new_sp_colwidths)

        empty_col        <- rep("", nrow(modified_tbl))
        modified_tbl     <- dplyr::bind_cols(
          modified_tbl[, seq_len(insert_at),               drop = FALSE],
          tibble::tibble(.sp_gap = empty_col),
          modified_tbl[, seq(insert_at + 1L, ncol(modified_tbl)), drop = FALSE]
        )
        spacer_col_idx   <- insert_at + 1L
        spacer_cols      <- c(spacer_cols, spacer_col_idx)

        new_sp_values    <- c(new_sp_values,    "")
        new_sp_colwidths <- c(new_sp_colwidths, 1L)
      }
    }

    sp_values    <- new_sp_values
    sp_colwidths <- new_sp_colwidths
    spanner_added <- TRUE
  }

  # Create flextable (after modified_tbl has been updated with any spacer cols)
  ft <- flextable(modified_tbl)

  if (spanner_added) {
    ft <- add_header_row(ft, values = sp_values, colwidths = sp_colwidths, top = TRUE)
    # Optionally move the col1 header label into the spanner row (top) or
    # leave it in the column-names row (bottom, classic behaviour).
    if (col1_spanner_pos == "top") {
      col1_display <- colnames(modified_tbl)[1]
      ft <- compose(ft, i = 1, j = 1, part = "header",
                    value = as_paragraph(as_chunk(col1_display)))
      ft <- compose(ft, i = 2, j = 1, part = "header",
                    value = as_paragraph(as_chunk("")))
    }
    # Blank out spacer column headers in the column-names row (row 2)
    if (length(spacer_cols) > 0L) {
      spacer_label_args <- setNames(
        as.list(rep("", length(spacer_cols))),
        colnames(modified_tbl)[spacer_cols]
      )
      ft <- do.call(set_header_labels, c(list(x = ft), spacer_label_args))
    }
    # Apply display-label overrides (named inner vectors in spanner)
    if (length(spanner_col_display) > 0L) {
      ft <- do.call(set_header_labels, c(list(x = ft), spanner_col_display))
    }
  }

  ft <- ft %>%
    font(fontname = font_family, part = "all") %>%
    fontsize(size = font_size, part = "all") %>%
    bg(bg = "#cdcdcd", part = "header") %>%
    # Set body alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "body") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "body") %>%
    # Set header alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "header") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "header") %>%
    border_remove() %>%
    border(border.bottom = fp_border(color = "black", width = 0.75), part = "header") %>%
    padding(padding.top = 0, padding.bottom = 1, part = "body") %>%
    padding(padding.left = 0, padding.right = 6, part = "body") %>%
    padding(padding.left = 3, padding.right = 6, part = "header")
  
  # Apply variable formatting (use modified_tbl which has category headers inserted)
  # Use the .indent column to determine padding (or spaces if .indent doesn't exist for backwards compat)
  variable_col <- modified_tbl[[1]]
  
  for (i in seq_along(variable_col)) {
    # Skip category header rows - they're formatted separately
    if (!is.null(category_rows) && i %in% category_rows) {
      next
    }
    
    # Determine indentation level
    if (!is.null(indent_col) && i <= length(indent_col) && !is.na(indent_col[i])) {
      indent_level <- indent_col[i]
    } else {
      # Fallback: count spaces (backwards compatibility)
      var_name <- variable_col[i]
      indent_level <- nchar(var_name) - nchar(trimws(var_name, which = "left"))
    }
    
    # Check for manual formatting overrides
    trimmed_var <- trimws(variable_col[i], which = "both")
    manual_italic <- !is.null(manual_italic_indent) && trimmed_var %in% manual_italic_indent
    manual_under <- !is.null(manual_underline) && trimmed_var %in% manual_underline
    
    # Apply formatting based on indentation level
    # Add universal base padding of 3 to all rows
    base_padding <- 3
    
    # Manual formatting overrides natural formatting
    if (manual_italic) {
      # Manually specified to be indented and italicized (like level 6)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 12 + base_padding, part = "body") %>%
        italic(i = i, j = 1, part = "body")
    } else if (manual_under) {
      # Manually specified to be underlined (like multi-category header)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body") %>%
        style(i = i, j = 1, pr_t = fp_text(underlined = TRUE, font.family = font_family, font.size = font_size), part = "body")
    } else if (indent_level == 0) {
      # Category headers from tibble - just base padding
      ft <- ft %>% padding(i = i, j = 1, padding.left = base_padding, part = "body")
    } else if (indent_level == 2) {
      # Regular variables
      is_empty <- length(modified_tbl) >= 2 && (modified_tbl[[2]][i] == "" || is.na(modified_tbl[[2]][i]))
      if (is_empty) {
        # Multi-category header (underlined)
        ft <- ft %>% 
          padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body") %>%
          style(i = i, j = 1, pr_t = fp_text(underlined = TRUE, font.family = font_family, font.size = font_size), part = "body")
      } else {
        # Regular variable
        ft <- ft %>% padding(i = i, j = 1, padding.left = 6 + base_padding, part = "body")
      }
    } else if (indent_level == 6) {
      # Stratified variables (italicized)
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 12 + base_padding, part = "body") %>%
        italic(i = i, j = 1, part = "body")
    }
  }
  
  # Bold header row - don't change alignment here
  ft <- ft %>% bold(part = "header")

  # Center-align the spanner row and apply selective bottom borders.
  # Strip all borders from the spanner row, then re-add only under labeled cells.
  # Spacer cells (empty label) naturally get no border, creating the visual gap.
  if (spanner_added) {
    ft <- align(ft, i = 1, align = "center", part = "header")
    # When col1 label sits in the spanner row, left-align it (it's a label, not a group name)
    if (col1_spanner_pos == "top") {
      ft <- align(ft, i = 1, j = 1, align = "left", part = "header")
    }
    ft <- border(ft, i = 1,
                 border.bottom = fp_border(width = 0),
                 part = "header")
    # Bottom border only under labeled spanner group cells (col1 gets none)
    col_start <- 1L
    for (s in seq_along(sp_values)) {
      if (nzchar(sp_values[s])) {
        ft <- border(ft, i = 1, j = col_start,
                     border.bottom = fp_border(color = "black", width = 0.75),
                     part = "header")
      }
      col_start <- col_start + sp_colwidths[s]
    }
  }

  # Format category header rows if they exist
  if (!is.null(category_rows) && length(category_rows) > 0) {
    base_padding <- 3
    for (cat_row_idx in category_rows) {
      ft <- ft %>%
        merge_at(i = cat_row_idx, j = 1:ncol(modified_tbl), part = "body") %>%
        bold(i = cat_row_idx, part = "body") %>%
        bg(i = cat_row_idx, bg = "white", part = "body") %>%
        border(i = cat_row_idx, border.bottom = fp_border(color = "black", width = 0.5), part = "body") %>%
        align(i = cat_row_idx, align = "left", part = "body") %>%
        padding(i = cat_row_idx, j = 1, padding.left = base_padding, padding.top = 2, padding.bottom = 2, part = "body")
    }
  }

  # Format plain header rows: underlined label, same indent as a normal variable row, no other treatments
  if (!is.null(plain_header_rows) && length(plain_header_rows) > 0) {
    base_padding <- 3
    for (ph_row_idx in plain_header_rows) {
      ft <- ft %>%
        padding(i = ph_row_idx, j = 1, padding.left = 6 + base_padding, part = "body") %>%
        style(i = ph_row_idx, j = 1, pr_t = fp_text(underlined = TRUE, font.family = font_family, font.size = font_size), part = "body")
    }
  }
  
  # P value header is already set in new_colnames, no special formatting needed
  
  # Bold significant p-values (use modified_tbl which has category headers)
  # Note: column is now called "P value" after renaming
  if ("P value" %in% colnames(modified_tbl)) {
    p_col_index <- which(colnames(modified_tbl) == "P value")
    sig_rows <- which(sapply(modified_tbl[[p_col_index]], function(p_val) {
      if (is.na(p_val) || is.null(p_val) || p_val == "" || p_val == "-") return(FALSE)
      if (grepl("E-", p_val)) return(TRUE)  # Scientific notation with negative exponent
      p_numeric <- suppressWarnings(as.numeric(p_val))
      if (!is.na(p_numeric) && p_numeric < 0.05) return(TRUE)
      return(FALSE)
    }))
    if (length(sig_rows) > 0) {
      ft <- bold(ft, i = sig_rows, j = p_col_index, part = "body")
    }
  }
  
  # Bold significant odds ratios (95% CI excludes 1) - use modified_tbl
  if ("OR" %in% colnames(modified_tbl)) {
    or_col_index <- which(colnames(modified_tbl) == "OR")
    sig_or_rows <- which(sapply(modified_tbl$OR, function(or_val) {
      if (is.na(or_val) || is.null(or_val) || or_val == "" || or_val == "-" || grepl("NA", or_val)) return(FALSE)
      # Parse format: "1.23 [0.45-2.67]"
      match <- regmatches(or_val, regexec("([0-9.]+)\\s*\\[([0-9.]+)\u2013([0-9.]+)\\]", or_val))
      if (length(match[[1]]) == 4) {
        lower_ci <- as.numeric(match[[1]][3])
        upper_ci <- as.numeric(match[[1]][4])
        # Significant if CI doesn't include 1
        return(!is.na(lower_ci) && !is.na(upper_ci) && (lower_ci > 1 || upper_ci < 1))
      }
      return(FALSE)
    }))
    if (length(sig_or_rows) > 0) {
      ft <- bold(ft, i = sig_or_rows, j = or_col_index, part = "body")
    }
  }

  # ── bold_sig: cell-level significance bolding for caller-specified p-value columns ────
  # Used by ternStyle() for custom tibbles where p-value columns are not named "P value".
  # For each p_col/hr_col pair, parse the p-value string and bold the cell (and its
  # paired HR cell) when the value is below the threshold. The Variable column is never
  # touched here — caller uses bold_rows for that.
  #
  # IMPORTANT: by the time this block runs, colnames(modified_tbl) reflect the renamed
  # headers (e.g. "P" → "P value"; spaces → "\n" when line_break_header = TRUE). We
  # build a reverse map from original → renamed name so caller-supplied names always
  # resolve correctly regardless of whether line_break_header is TRUE or FALSE.
  if (!is.null(bold_sig) && is.list(bold_sig) && length(bold_sig$p_cols) > 0) {
    bsig_threshold <- if (!is.null(bold_sig$threshold)) as.numeric(bold_sig$threshold) else 0.05
    bsig_hr_cols   <- bold_sig$hr_cols  # may be NULL

    # Build map: original column name → renamed column name
    bsig_name_map  <- setNames(pre_spanner_colnames, original_colnames)

    # Resolve a user-supplied column name to its renamed equivalent:
    # 1. If the name appears directly in modified_tbl columns, use it as-is.
    # 2. Otherwise try the original→renamed map.
    # 3. Fall back to the supplied name (will be caught by the %in% check below).
    resolve_colname <- function(nm) {
      if (nm %in% colnames(modified_tbl)) return(nm)
      if (nm %in% names(bsig_name_map))   return(bsig_name_map[[nm]])
      nm
    }

    for (pi in seq_along(bold_sig$p_cols)) {
      pcol_name <- resolve_colname(bold_sig$p_cols[[pi]])
      if (!pcol_name %in% colnames(modified_tbl)) next
      pcol_idx  <- which(colnames(modified_tbl) == pcol_name)
      sig_rows_bs <- which(sapply(modified_tbl[[pcol_idx]], function(pv) {
        if (is.na(pv) || pv == "" || pv == "-") return(FALSE)
        pv_clean <- trimws(gsub("<\\s*", "", pv))  # handle "< 0.001" and "<0.001"
        p_num <- suppressWarnings(as.numeric(pv_clean))
        if (!is.na(p_num) && p_num < bsig_threshold) return(TRUE)
        if (grepl("E-", pv, ignore.case = TRUE)) return(TRUE)  # scientific notation
        FALSE
      }))
      if (length(sig_rows_bs) > 0) {
        ft <- bold(ft, i = sig_rows_bs, j = pcol_idx, part = "body")
        # Bold paired HR/effect column if supplied
        if (!is.null(bsig_hr_cols) && pi <= length(bsig_hr_cols)) {
          hcol_name <- resolve_colname(bsig_hr_cols[[pi]])
          if (nzchar(hcol_name) && hcol_name %in% colnames(modified_tbl)) {
            hcol_idx <- which(colnames(modified_tbl) == hcol_name)
            ft <- bold(ft, i = sig_rows_bs, j = hcol_idx, part = "body")
          }
        }
      }
    }
  }

  # ── Manual bold/italic overrides ──────────────────────────────────────────────
  # Applied last so they unconditionally override all structural formatting above.
  if (!is.null(bold_rows)   && length(bold_rows)   > 0) ft <- bold(ft,   i = bold_rows,   part = "body")
  if (!is.null(italic_rows) && length(italic_rows) > 0) ft <- italic(ft, i = italic_rows, part = "body")
  if (!is.null(bold_cols)   && length(bold_cols)   > 0) ft <- bold(ft,   j = bold_cols,   part = "body")
  if (!is.null(italic_cols) && length(italic_cols) > 0) ft <- italic(ft, j = italic_cols, part = "body")
  if (isTRUE(header_format_follow)) {
    if (!is.null(bold_cols)   && length(bold_cols)   > 0) ft <- bold(ft,   j = bold_cols,   part = "header")
    if (!is.null(italic_cols) && length(italic_cols) > 0) ft <- italic(ft, j = italic_cols, part = "header")
  }

  # Shrink all columns to fit their content, then lock row heights exactly.
  # height() and hrule() must come AFTER autofit() -- autofit resets row heights
  # as a side effect of its column-width calculation.
  ft <- autofit(ft)
  # If the autofit width exceeds the usable page width (6.5 in = Letter - 1in
  # margins each side), scale the table down so it fits on the page without
  # overflowing. This prevents cut-off columns in Word / PDF / the preview PNG.
  ft_dims <- flextable::flextable_dim(ft)
  page_width_in <- 6.5
  if (!is.null(ft_dims$widths) && sum(ft_dims$widths) > page_width_in) {
    ft <- flextable::fit_to_width(ft, max_width = page_width_in)
  }
  # Force spacer columns to stay narrow after autofit/fit_to_width
  if (length(spacer_cols) > 0L) {
    ft <- width(ft, j = spacer_cols, width = 0.07)
  }
  ft <- ft %>%
    height(height = font_size / 72 * 1.5, part = "body") %>%
    flextable::hrule(rule = "exact", part = "body")

  # Re-apply category header row heights after the blanket lock
  if (!is.null(category_rows) && length(category_rows) > 0) {
    for (cat_row_idx in category_rows) {
      ft <- ft %>% height(i = cat_row_idx, height = font_size / 72 * 2.05, part = "body")
    }
  }

  # ── Superscript pass: rebuild cells for non-"*" symbols ──────────────────
  for (info in vf_row_info) {
    if (!isTRUE(info$needs_super)) next
    row_idx    <- info$row_idx
    sym        <- info$symbol
    cell_text  <- modified_tbl[[1]][row_idx]
    main_text  <- substr(cell_text, 1, nchar(cell_text) - nchar(sym))
    is_underlined <- !is.null(plain_header_rows) && row_idx %in% plain_header_rows
    main_props  <- fp_text(font.family = font_family, font.size = font_size,
                           italic = isTRUE(info$is_italic),
                           underlined = is_underlined)
    super_props <- fp_text(font.family = font_family,
                           font.size = max(4L, as.integer(font_size) - 2L),
                           vertical.align = "superscript")
    ft <- compose(ft, i = row_idx, j = 1,
      value = as_paragraph(
        as_chunk(main_text, props = main_props),
        as_chunk(sym,       props = super_props)
      ),
      part = "body"
    )
  }

  # ── Assemble footnote: abbreviations → posthoc CLD → variable definitions → table_footnote ─
  # Each group is a character vector of tightly-spaced lines; groups are then
  # separated by a blank line (\n\n) so the sections are visually distinct.
  {
    footnote_sections <- list()
    if (!is.null(abbreviation_footnote) && length(abbreviation_footnote) > 0) {
      abbr_text <- trimws(paste(abbreviation_footnote, collapse = " "))
      if (nchar(abbr_text) > 0) footnote_sections[[length(footnote_sections) + 1L]] <- abbr_text
    }
    if (!is.null(posthoc_footnote) && nchar(trimws(posthoc_footnote)) > 0) {
      footnote_sections[[length(footnote_sections) + 1L]] <- trimws(posthoc_footnote)
    }
    if (!is.null(variable_footnote) && length(variable_footnote) > 0) {
      vf_syms <- .footnote_symbol_seq(length(variable_footnote), index_style)
      vf_lines <- character(length(variable_footnote))
      for (k in seq_along(variable_footnote)) {
        vf_lines[k] <- paste0(vf_syms[k], " ", variable_footnote[[k]])
      }
      footnote_sections[[length(footnote_sections) + 1L]] <- paste(vf_lines, collapse = "\n")
    }
    if (!is.null(table_footnote) && length(table_footnote) > 0) {
      tf_clean <- table_footnote[nchar(trimws(table_footnote)) > 0]
      if (length(tf_clean) > 0)
        footnote_sections[[length(footnote_sections) + 1L]] <- paste(tf_clean, collapse = "\n")
    }
    if (length(footnote_sections) > 0) {
      dbl_border    <- fp_border(color = "black", width = 0.5, style = "double")
      footnote_text <- paste(footnote_sections, collapse = "\n\n")
      ft <- ft %>%
        add_footer_lines(values = footnote_text) %>%
        font(fontname = font_family, part = "footer") %>%
        fontsize(size = 6, part = "footer") %>%
        italic(part = "footer") %>%
        align(align = "left", part = "footer") %>%
        hline_top(border = dbl_border, part = "footer") %>%
        hline_bottom(border = dbl_border, part = "footer")
    }
  }

  # Create Word document
  doc <- read_docx()
  # Remove the default blank paragraph officer inserts so temp files used by
  # ternB() don't carry a leading empty paragraph that can displace captions
  # onto a separate page when assembled into a combined document.
  doc <- officer::cursor_begin(doc)
  suppressWarnings(doc <- officer::body_remove(doc))
  if (!is.null(table_caption) && nchar(trimws(table_caption)) > 0) {
    cap <- trimws(table_caption)

    # Split into sentences on period + whitespace boundaries
    sentences <- strsplit(cap, "(?<=\\.)\\s+", perl = TRUE)[[1]]

    bold_prop  <- fp_text(font.size = 11, font.family = font_family, bold = TRUE,  italic = FALSE)
    plain_prop <- fp_text(font.size = 11, font.family = font_family, bold = FALSE, italic = FALSE)

    # Rule: if caption starts with "Table <n>." and has at least two sentences,
    # sentences 1-2 are bold and the remainder is plain weight.
    # Otherwise the whole caption is bold. Italic is never applied.
    use_split <- length(sentences) >= 2 && grepl("^Table\\s*\\d", sentences[1])

    if (use_split) {
      bold_text  <- paste(sentences[1:2], collapse = " ")
      plain_text <- if (length(sentences) > 2) paste(sentences[3:length(sentences)], collapse = " ") else NULL

      caption_text <- if (!is.null(plain_text)) {
        fpar(
          ftext(bold_text,              prop = bold_prop),
          ftext(paste0(" ", plain_text), prop = plain_prop),
          fp_p = fp_par(line_spacing = 1, padding.bottom = 6)
        )
      } else {
        fpar(
          ftext(bold_text, prop = bold_prop),
          fp_p = fp_par(line_spacing = 1, padding.bottom = 6)
        )
      }
    } else {
      caption_text <- fpar(
        ftext(cap, prop = bold_prop),
        fp_p = fp_par(line_spacing = 1, padding.bottom = 6)
      )
    }
    doc <- doc %>% body_add_fpar(caption_text)
  }
  doc <- doc %>% body_add_flextable(ft)

  if (isTRUE(page_break_after)) {
    doc <- doc %>% body_add_break()
  }

  # ── Word document page footer (citation line) ─────────────────────────────
  if (isTRUE(citation)) {
    cit_props <- fp_text(font.family = font_family, font.size = 8,
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
  invisible(filename)
}