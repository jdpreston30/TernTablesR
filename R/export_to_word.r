#' Export TernTablesR output to a formatted Word document
#'
#' @param tbl A tibble created by ternG or ternD
#' @param filename Output file path ending in .docx
#' @param round_intg Logical; if TRUE, adds note about integer rounding. Default is FALSE.
#' @export
export_to_word <- function(tbl, filename, round_intg = FALSE) {
  library(officer)
  library(flextable)

  # Apply hierarchical formatting to variable names
  modified_tbl <- tbl
  
  for (i in seq_along(tbl[[1]])) {
    var_name <- tbl[[1]][i]
    modified_tbl[[1]][i] <- trimws(var_name, which = "left")
  }
  
  # Modify column headers to add symbols and line breaks
  original_colnames <- colnames(modified_tbl)
  new_colnames <- original_colnames
  
  # Add asterisk to group columns and line breaks for sample sizes
  for (i in 2:length(new_colnames)) {
    if (!new_colnames[i] %in% c("p", "test", "OR", "OR_method", "Total")) {
      new_colnames[i] <- gsub(" \\(n = ", "*\n(n = ", new_colnames[i])
    } else if (new_colnames[i] == "p") {
      new_colnames[i] <- "p†"
    } else if (new_colnames[i] == "Total") {
      new_colnames[i] <- gsub(" \\(n = ", "*\n(n = ", new_colnames[i])
    }
  }
  colnames(modified_tbl) <- new_colnames
  
  # Update the Variable column header
  colnames(modified_tbl)[1] <- "Variable\n   Stratified Variable"
  
  # Create the footer text with conditional rounding note
  footer_text <- "Statistical Methods: For continuous variables, normality was assessed using the Shapiro–Wilk test (when enabled). Normally distributed variables were summarized as mean ± SD and compared using Welch's t-test for two-group comparisons or one-way ANOVA for three-group comparisons. Non-normally distributed or user-specified ordinal variables were summarized as median [IQR] and compared using the Wilcoxon rank-sum test (two groups) or Kruskal–Wallis test (three groups). Categorical variables were summarized as n (%) and compared using Chi-squared tests or Fisher's exact tests when expected counts were <5. For binary categorical variables, odds ratios with 95% confidence intervals were computed using Fisher's exact or Wald methods, as appropriate. Approximate p-values were used for non-parametric tests with tied data.\n\n*- All values are presented as mean ± SD for normally distributed continuous variables, median [IQR] for non-normally distributed continuous or ordinal variables, and n (%) for categorical variables."
  
  # Add rounding note if round_intg is TRUE
  if (round_intg) {
    footer_text <- paste0(footer_text, " All means, medians, SDs, and IQRs are rounded to the nearest integer value.")
  }
  
  # Add the final dagger note
  footer_text <- paste0(footer_text, "\n†- Welch's t-test was used to compute p-values for normally distributed continuous variables. Wilcoxon Rank Sum test was used to compute p-values for non-normally distributed continuous or ordinal variables. Fisher's exact or chi-squared test was used to compute p-values for categorical variables, as appropriate. p-values are bolded for p ≤ 0.05.")
  
  # Add footer row
  footer_row <- modified_tbl[1,]
  footer_row[1,] <- ""
  footer_row[[1]][1] <- footer_text
  modified_tbl <- rbind(modified_tbl, footer_row)
  footer_row_index <- nrow(modified_tbl)
  
  # Create flextable
  ft <- flextable(modified_tbl) %>%
    set_table_properties(width = 1, layout = "autofit") %>%
    font(fontname = "Arial", part = "all") %>%
    fontsize(size = 9, part = "all") %>%
    fontsize(size = 8, part = "header") %>%
    bg(bg = "#cdcdcd", part = "header") %>%
    # Set body alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "body") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "body") %>%
    # Set header alignment: Variable column left, all others center
    align(align = "left", j = 1, part = "header") %>%
    align(align = "center", j = 2:ncol(modified_tbl), part = "header") %>%
    border_remove() %>%
    border(i = 1, border.bottom = fp_border(color = "black", width = 0.5), part = "header") %>%
    border(i = footer_row_index - 1, border.bottom = fp_border(color = "black", width = 0.5), part = "body") %>%
    padding(padding.top = 0, padding.bottom = 0, part = "body") %>%
    padding(padding.left = 0, padding.right = 6, part = "body") %>%
    height(height = 0.4, part = "header") %>%
    height(height = 0.1, part = "body") %>%
    merge_at(i = footer_row_index, j = 1:ncol(modified_tbl), part = "body") %>%
    border(i = footer_row_index, border.top = fp_border(color = "black", width = 0.5), part = "body") %>%
    border(i = footer_row_index, border.bottom = fp_border(color = "black", width = 0.5), part = "body") %>%
    height(i = footer_row_index, height = 0.8, part = "body") %>%
    padding(i = footer_row_index, padding.top = 6, padding.bottom = 6, padding.left = 6, padding.right = 6, part = "body") %>%
    fontsize(i = footer_row_index, j = 1, size = 6, part = "body") %>%
    italic(i = footer_row_index, j = 1, part = "body")
  
  # Apply variable formatting
  variable_col <- tbl[[1]]
  for (i in seq_along(variable_col)) {
    var_name <- variable_col[i]
    spaces <- nchar(var_name) - nchar(trimws(var_name, which = "left"))
    
    if (spaces == 2) {
      if (length(tbl) >= 2 && (tbl[[2]][i] == "" || is.na(tbl[[2]][i]))) {
        ft <- ft %>% 
          padding(i = i, j = 1, padding.left = 12, part = "body") %>%
          style(i = i, j = 1, pr_t = fp_text(underlined = TRUE, font.family = "Arial", font.size = 9), part = "body")
      } else {
        ft <- ft %>% padding(i = i, j = 1, padding.left = 12, part = "body")
      }
    } else if (spaces == 6) {
      ft <- ft %>% 
        padding(i = i, j = 1, padding.left = 20, part = "body") %>%
        italic(i = i, j = 1, part = "body")
    }
  }
  
  # Bold header row - don't change alignment here
  ft <- ft %>% bold(part = "header")
  
  # Make the dagger superscript in p† header (if it exists)
  p_col_names <- colnames(modified_tbl)
  dagger_col <- which(grepl("p†", p_col_names))
  if (length(dagger_col) > 0) {
    # Replace p† with p and superscript dagger
    tryCatch({
      ft <- ft %>%
        compose(i = 1, j = dagger_col, 
               value = as_paragraph(
                 ftext("p", prop = fp_text(bold = TRUE, font.family = "Arial", font.size = 8)),
                 ftext("†", prop = fp_text(bold = TRUE, font.family = "Arial", font.size = 6, vertical.align = "superscript"))
               ), 
               part = "header")
    }, error = function(e) {
      # If compose fails, just keep the regular p† formatting
      message("Note: Could not apply superscript formatting to dagger symbol")
    })
  }
  
  # Bold significant p-values
  if ("p" %in% names(tbl)) {
    p_col_index <- which(names(tbl) == "p")
    sig_rows <- which(sapply(tbl$p, function(p_val) {
      if (is.na(p_val) || is.null(p_val) || p_val == "") return(FALSE)
      if (grepl("e-", p_val, ignore.case = TRUE)) return(TRUE)
      p_numeric <- suppressWarnings(as.numeric(p_val))
      if (!is.na(p_numeric) && p_numeric <= 0.05) return(TRUE)
      return(FALSE)
    }))
    if (length(sig_rows) > 0) {
      ft <- bold(ft, i = sig_rows, j = p_col_index)
    }
  }

  # Create Word document
  doc <- read_docx() %>% body_add_flextable(ft)
  print(doc, target = filename)
}