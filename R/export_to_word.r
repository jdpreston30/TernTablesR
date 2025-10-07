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
      "Statistical Methods: For continuous variables, normality was assessed using the Shapiro–Wilk test (when enabled). ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Normally distributed variables were summarized as mean ± SD and compared using Welch’s t-test for two-group comparisons or one-way ANOVA for three-group comparisons. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Non-normally distributed or user-specified ordinal variables were summarized as median [IQR] and compared using the Wilcoxon rank-sum test (two groups) or Kruskal–Wallis test (three groups). ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Categorical variables were summarized as n (%) and compared using Chi-squared tests or Fisher’s exact tests when expected counts were <5. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "For binary categorical variables, odds ratios with 95% confidence intervals were computed using Fisher’s exact or Wald methods, as appropriate. ",
      fp_text(font.size = 8, italic = TRUE)
    ),
    ftext(
      "Approximate p-values were used for non-parametric tests with tied data.",
      fp_text(font.size = 8, italic = TRUE)
    )
  )
  
  doc <- read_docx() %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal") %>%
    body_add_fpar(footer_note)

  print(doc, target = filename)
}
