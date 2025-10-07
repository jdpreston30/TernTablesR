#' Export TernTablesR output to a formatted Word document
#'
#' @param tbl A tibble created by ternG or ternD
#' @param filename Output file path ending in .docx
#' @export
export_to_word <- function(tbl, filename) {
  library(officer)
  library(flextable)

  ft <- flextable(tbl) %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    bold(part = "header") %>%
    italic(part = "header") %>%
    bg(bg = "#cdcdcd", part = "header") %>%
    align(align = "left", j = 1, part = "all") %>%
    align(align = "center", j = 2:ncol(tbl), part = "all") %>%
    # Remove all borders first
    border_remove() %>%
    # Add only bottom border to header row
    border(i = 1, border.bottom = fp_border(color = "black", width = 0.5), part = "header") %>%
    # Add only bottom border to the very last row
    border(i = nrow(tbl), border.bottom = fp_border(color = "black", width = 0.5), part = "body")
  
  # Bold significant p-values if p column exists
  if ("p" %in% names(tbl)) {
    p_col_index <- which(names(tbl) == "p")
    
    # Find rows with significant p-values
    sig_rows <- which(sapply(tbl$p, function(p_val) {
      if (is.na(p_val) || is.null(p_val) || p_val == "") return(FALSE)
      
      # Check if it's in scientific notation (definitely significant)
      if (grepl("e-", p_val, ignore.case = TRUE)) return(TRUE)
      
      # Try to convert to numeric and check if < 0.05
      p_numeric <- suppressWarnings(as.numeric(p_val))
      if (!is.na(p_numeric) && p_numeric < 0.05) return(TRUE)
      
      return(FALSE)
    }))
    
    # Apply bold formatting to significant p-values
    if (length(sig_rows) > 0) {
      ft <- bold(ft, i = sig_rows, j = p_col_index)
    }
  }

    # Full methods description
  footer_note <- fpar(
    ftext(
      "Statistical Methods: For continuous variables, normality was assessed using the Shapiro–Wilk test (when enabled). ",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    ),
    ftext(
      "Normally distributed variables were summarized as mean ± SD and compared using Welch's t-test for two-group comparisons or one-way ANOVA for three-group comparisons. ",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    ),
    ftext(
      "Non-normally distributed or user-specified ordinal variables were summarized as median [IQR] and compared using the Wilcoxon rank-sum test (two groups) or Kruskal–Wallis test (three groups). ",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    ),
    ftext(
      "Categorical variables were summarized as n (%) and compared using Chi-squared tests or Fisher's exact tests when expected counts were <5. ",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    ),
    ftext(
      "For binary categorical variables, odds ratios with 95% confidence intervals were computed using Fisher's exact or Wald methods, as appropriate. ",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    ),
    ftext(
      "Approximate p-values were used for non-parametric tests with tied data.",
      fp_text(font.size = 8, italic = TRUE, font.family = "Arial")
    )
  )
  
  doc <- read_docx() %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal") %>%
    body_add_fpar(footer_note)

  print(doc, target = filename)
}
