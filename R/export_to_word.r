#' Export TernTablesR output to a formatted Word document
#'
#' @param tbl A tibble created by Tern2v or Tern3v
#' @param filename Output file path ending in .docx
#' @export
export_to_word <- function(tbl, filename) {
  library(officer)
  library(flextable)

  ft <- flextable(tbl) %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    fontsize(size = 10, part = "all") %>%
    bold(part = "header") %>%
    italic(part = "header") %>%
    align(align = "left", j = 1, part = "all") %>%
    align(align = "center", j = 2:ncol(tbl), part = "all")

  # Full methods description
  footer_note <- fpar(
    ftext(
      "Statistical Methods: For continuous variables, normality was assessed using the Shapiro-Wilk test. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "For 2-group comparisons, normally distributed variables were compared using Welch’s t-test; non-normal distributions were compared using the Wilcoxon rank-sum test. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "For 3-group comparisons, normally distributed variables were compared using one-way ANOVA; non-normal or ordinal variables specified by the user were compared using the Kruskal-Wallis test. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Categorical variables were compared using Chi-squared tests or Fisher’s exact tests if expected counts were <5. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Ordinal variables were summarized as medians [IQR] and compared using appropriate non-parametric tests. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Note: Approximate p-values were used for non-parametric tests with tied data.",
      fp_text(font.size = 8, italic = TRUE)
    )
  )
  
  doc <- read_docx() %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal") %>%
    body_add_fpar(footer_note)

  print(doc, target = filename)
}
