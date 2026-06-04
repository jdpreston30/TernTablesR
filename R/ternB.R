#' Combine multiple ternD/ternG tables into a single Word document
#'
#' Takes a list of tibbles previously created by \code{ternD()} or \code{ternG()}
#' and writes them all into one \code{.docx} file, one table per page, preserving
#' the exact formatting settings that were used when each table was built.
#'
#' @param tables A \strong{list} of tibbles created by \code{ternD()} or \code{ternG()}.
#'   Must be constructed with \code{list()}, not \code{c()} (e.g.
#'   \code{list(T1, T2, T3)}).  Each tibble must have been produced in the
#'   \emph{current R session}; the metadata is stored in memory, not in the
#'   tibble columns.
#' @param output_docx Output file path ending in \code{.docx}.
#' @param page_break Logical; if \code{TRUE} (default), inserts a page break
#'   between each consecutive table.
#' @param methods_doc Logical; if \code{TRUE}, writes a single methods section
#'   Word document that covers all tables in the list. Statistical test details
#'   are pooled across all tables. Default is \code{FALSE}.
#' @param methods_filename Output file path for the methods document. Defaults
#'   to \code{"TernTables_methods.docx"} in the working directory.
#' @param open_doc Logical; if \code{TRUE} (default), automatically opens each
#'   written \code{.docx} in the system default application after saving.
#'   Set to \code{FALSE} to suppress.
#' @param citation Logical; if \code{TRUE} (default), appends a citation line at the bottom
#'   of each table footnote block and methods document: package version, authors, and links
#'   to the GitHub repository and web interface. Set to \code{FALSE} to suppress.
#' @param font_family Character; font family for all Word output. Any font name accepted by
#'   the rendering system is valid. Can also be set via
#'   \code{options(TernTables.font_family = "Garamond")}. Default \code{"Arial"}.
#'
#' @details
#' \code{ternB()} works by replaying the exact \code{word_export()} call that
#' \code{ternD()} / \code{ternG()} would have made -- using stored metadata
#' attached as an attribute to each returned tibble -- but directing all output
#' into a single combined document instead of separate files.
#'
#' Table captions (\code{table_caption}) and footnotes (\code{table_footnote}) specified in the original
#' \code{ternD()} / \code{ternG()} call are reproduced automatically.  You can
#' override them by modifying the \code{"ternB_meta"} attribute before calling
#' \code{ternB()}, though in practice it is easier to set captions and footnotes when you
#' first build each table.
#'
#' @return Invisibly returns the path to the written Word file.
#'
#' @examples
#' \donttest{
#' data(tern_colon)
#'
#' T1 <- ternD(tern_colon,
#'             exclude_vars  = "ID",
#'             table_caption = "Table 1. Overall patient characteristics.",
#'             methods_doc   = FALSE,
#'             open_doc      = FALSE)
#'
#' T2 <- ternG(tern_colon,
#'             group_var     = "Recurrence",
#'             exclude_vars  = "ID",
#'             table_caption = "Table 2. Characteristics by recurrence status.",
#'             methods_doc   = FALSE,
#'             open_doc      = FALSE)
#'
#' ternB(list(T1, T2),
#'       output_docx = file.path(tempdir(), "combined_tables.docx"),
#'       open_doc    = FALSE)
#' }
#' @export
ternB <- function(tables, output_docx, page_break = TRUE,
                  methods_doc = FALSE,
                  methods_filename = "TernTables_methods.docx",
                  open_doc = TRUE, citation = TRUE,
                  font_family = getOption("TernTables.font_family", "Arial")) {

  # ── Input validation ──────────────────────────────────────────────────────
  if (!is.list(tables) || inherits(tables, "data.frame")) {
    stop(
      "'tables' must be a list of ternD/ternG output tibbles.\n",
      "Use list(T1, T2, T3) rather than c(T1, T2, T3).",
      call. = FALSE
    )
  }
  if (length(tables) == 0) {
    stop("'tables' is empty -- nothing to export.", call. = FALSE)
  }
  if (!grepl("\\.docx$", output_docx, ignore.case = TRUE)) {
    stop("'output_docx' must end with .docx", call. = FALSE)
  }

  # ── Write each table to its own temp file ─────────────────────────────────
  temp_files <- character(length(tables))
  on.exit(unlink(temp_files[nchar(temp_files) > 0]), add = TRUE)

  for (i in seq_along(tables)) {
    meta <- attr(tables[[i]], "ternB_meta")

    if (is.null(meta)) {
      stop(
        "Table ", i, " does not carry ternB metadata.\n",
        "Make sure it was created by ternD() or ternG() in the current R session.",
        call. = FALSE
      )
    }

    temp_files[i] <- tempfile(fileext = ".docx")

    tryCatch(
      word_export(
        tbl                   = meta$tbl,
        filename              = temp_files[i],
        round_intg            = meta$round_intg,
        round_decimal         = meta$round_decimal,
        font_size             = meta$font_size,
        category_start        = meta$category_start,
        plain_header          = meta$plain_header,
        manual_italic_indent  = meta$manual_italic_indent,
        manual_underline      = meta$manual_underline,
        table_caption         = meta$table_caption,
        table_footnote        = meta$table_footnote,
        abbreviation_footnote = meta$abbreviation_footnote,
        posthoc_footnote      = meta$posthoc_footnote,
        variable_footnote     = meta$variable_footnote,
        index_style           = if (is.null(meta$index_style)) "symbols" else meta$index_style,
        line_break_header     = if (is.null(meta$line_break_header)) getOption("TernTables.line_break_header", TRUE) else meta$line_break_header,
        subheader_rows        = meta$subheader_rows,
        bold_rows             = meta$bold_rows,
        bold_sig              = meta$bold_sig,
        italic_rows           = meta$italic_rows,
        bold_cols             = meta$bold_cols,
        italic_cols           = meta$italic_cols,
        header_format_follow  = if (is.null(meta$header_format_follow)) FALSE else meta$header_format_follow,
        col1_header           = meta$col1_header,
        page_break_after      = (i < length(temp_files)) && isTRUE(page_break),
        open_doc              = FALSE,
        citation              = FALSE,   # temp files only; prevents section-property bleed into combined doc
        font_family           = font_family
      ),
      error = function(e) {
        stop(
          "ternB(): failed to render table ", i,
          if (!is.null(meta$table_caption)) paste0(" (\"", substr(meta$table_caption, 1, 60), "\")") else "",
          "\n", conditionMessage(e),
          call. = FALSE
        )
      }
    )
  }

  # ── Assemble the combined document ────────────────────────────────────────
  doc <- read_docx()
  # Strip the default blank paragraph that officer inserts on read_docx() so
  # the first table starts at the top of page 1 with no leading empty line.
  doc <- officer::cursor_begin(doc)
  suppressWarnings(doc <- officer::body_remove(doc))

  for (i in seq_along(temp_files)) {
    doc <- doc %>% body_add_docx(src = temp_files[i])
  }

  dir.create(dirname(output_docx), recursive = TRUE, showWarnings = FALSE)
  print(doc, target = output_docx)
  if (isTRUE(open_doc)) .open_docx(output_docx)

  # ── Optional unified methods document ─────────────────────────────────────
  if (methods_doc) {
    all_metas <- lapply(tables, function(t) attr(t, "ternB_meta"))

    # Generate a methods paragraph for each table individually, capturing the
    # returned text string (write_methods_doc returns invisible(methods_body)).
    # Each call writes to a discarded tempfile; only the text is kept.
    # ternStyle tables carry no statistical test information and are skipped
    # (NA is returned for them; they are filtered out before building sections).
    para_texts <- vapply(seq_along(all_metas), function(i) {
      m <- all_metas[[i]]
      src <- if (is.null(m$source)) "ternG" else m$source
      if (src == "ternStyle") return(NA_character_)
      suppressMessages(
        write_methods_doc(
          tbl                 = m$tbl,
          filename            = tempfile(fileext = ".docx"),
          n_levels            = if (is.null(m$n_levels))            2L        else m$n_levels,
          OR_col              = if (is.null(m$OR_col))              FALSE     else m$OR_col,
          OR_method           = if (is.null(m$OR_method))           "dynamic" else m$OR_method,
          source              = src,
          post_hoc            = if (is.null(m$post_hoc))            FALSE     else m$post_hoc,
          categorical_posthoc = if (is.null(m$categorical_posthoc)) FALSE     else m$categorical_posthoc,
          p_adjust            = if (is.null(m$p_adjust))            FALSE     else m$p_adjust,
          open_doc            = FALSE,
          citation            = citation
        )
      )
    }, character(1))

    # Drop ternStyle tables (NA) before deduplication / section building
    keep_idx   <- which(!is.na(para_texts))
    para_texts <- para_texts[keep_idx]

    # Derive a short label for each table: extract just "Table N" from the
    # start of the caption. Fall back to "Table i" if no match.
    labels <- vapply(seq_along(all_metas), function(i) {
      cap <- all_metas[[i]]$table_caption
      if (!is.null(cap) && nchar(trimws(cap)) > 0) {
        m <- regmatches(cap, regexpr("^Table\\s*\\d+", cap, ignore.case = TRUE))
        if (length(m) > 0 && nchar(m) > 0) m else paste0("Table ", i)
      } else {
        paste0("Table ", i)
      }
    }, character(1))

    # Keep only the labels for tables that produced a methods paragraph
    labels <- labels[keep_idx]

    # Group tables that produced identical paragraph text to avoid redundancy.
    # Result: list of (heading, paragraph) pairs in first-occurrence order.
    seen      <- character(0)
    sections  <- list()
    for (i in seq_along(para_texts)) {
      txt <- para_texts[[i]]
      if (txt %in% seen) {
        # Append this table's label to the existing section heading
        idx <- which(vapply(sections, function(s) identical(s$txt, txt), logical(1)))
        sections[[idx]]$labels <- c(sections[[idx]]$labels, labels[[i]])
      } else {
        seen <- c(seen, txt)
        sections <- c(sections, list(list(labels = labels[[i]], txt = txt)))
      }
    }

    # Build the Word document
    hp <- fp_text(font.size = 11, font.family = font_family, bold = TRUE)
    bp <- fp_text(font.size = 11, font.family = font_family)
    fp <- fp_text(font.size = 9,  font.family = font_family, italic = TRUE,
                  color = "#555555")

    pkg_ver <- .tern_pkg_version()

    doc <- read_docx()
    for (sec in sections) {
      heading_txt <- paste(sec$labels, collapse = " / ")
      heading_txt <- paste0(heading_txt, " Statistical Methods")
      doc <- doc |>
        body_add_fpar(fpar(ftext(heading_txt, prop = hp))) |>
        body_add_par("", style = "Normal") |>
        body_add_fpar(fpar(ftext(sec$txt, prop = bp))) |>
        body_add_par("", style = "Normal")
    }

    footer_txt <- paste0(
      "Generated by TernTables v", pkg_ver, ". ",
      "Each section reflects the specific configuration of the corresponding table(s). ",
      "Sections with identical configurations are consolidated to avoid redundancy."
    )
    doc <- doc |>
      body_add_fpar(fpar(ftext(footer_txt, prop = fp)))

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

    dir.create(dirname(methods_filename), recursive = TRUE, showWarnings = FALSE)
    print(doc, target = methods_filename)
    if (isTRUE(open_doc)) .open_docx(methods_filename)
    cli::cli_alert_success("Methods document written to: {methods_filename}")
  }

  invisible(output_docx)
}
