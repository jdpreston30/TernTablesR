#' Generate descriptive summary table (optionally normality-aware)
#'
#' Creates a descriptive summary table with a single "Summary" column format.
#' For numeric variables, the default behavior is to show mean ± SD, but this
#' can be modified using the \code{consider_normality} and \code{force_ordinal} parameters.
#'
#' @param data Tibble with variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{exclude_vars}.
#' @param exclude_vars Character vector to exclude from the summary.
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use median [IQR]) 
#'   regardless of the \code{consider_normality} setting. This parameter takes priority over 
#'   normality testing when \code{consider_normality = TRUE}.
#' @param output_xlsx Optional Excel filename to export the table.
#' @param output_docx Optional Word filename to export the table.
#' @param consider_normality Logical; if \code{TRUE}, uses Shapiro-Wilk test to choose between 
#'   mean ± SD (for normal data) vs median [IQR] (for non-normal data) for numeric variables. 
#'   If \code{FALSE}, defaults to mean ± SD for all numeric variables unless specified in 
#'   \code{force_ordinal}.
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk p-values as an 
#'   additional column in the output.
#' @param round_intg Logical; if \code{TRUE}, rounds all means, medians, IQRs, and standard 
#'   deviations to nearest integer (0.5 rounds up). Default is \code{FALSE}.
#' @param smart_rename Logical; if \code{TRUE}, automatically cleans variable names and 
#'   subheadings for publication-ready output using hybrid AI+rules cleaning. Uses rule-based 
#'   cleaning for known medical terms, falls back to AI for complex cases. Default is \code{FALSE}.
#' @param insert_subheads Logical; if \code{TRUE}, creates hierarchical structure with headers 
#'   and indented sub-categories for multi-level categorical variables (except Y/N). If \code{FALSE}, 
#'   uses simple flat format. Default is \code{TRUE}.
#'
#' @details
#' The function always returns a tibble with a single "Summary" column format, regardless of the
#' \code{consider_normality} setting. The behavior for numeric variables follows this priority:
#' \enumerate{
#'   \item Variables in \code{force_ordinal}: Always use median [IQR]
#'   \item When \code{consider_normality = TRUE}: Use Shapiro-Wilk test to choose format
#'   \item When \code{consider_normality = FALSE}: Default to mean ± SD
#' }
#'
#' For categorical variables, the function shows frequencies and percentages. When 
#' \code{insert_subheads = TRUE}, multi-level categorical variables are displayed with 
#' hierarchical formatting (main variable as header, levels as indented sub-rows), except 
#' for binary Y/N variables which use simple formatting.
#'
#' @return A tibble with one row per variable (multi-row for factors), containing:
#' \describe{
#'   \item{Variable}{Variable names with appropriate indentation}
#'   \item{Summary}{Summary statistics (mean ± SD, median [IQR], or n (\%) as appropriate)}
#'   \item{SW_p}{Shapiro-Wilk p-values (only if \code{print_normality = TRUE})}
#' }
#'
#' @examples
#' # Basic usage with default mean ± SD for numeric variables
#' ternD(mtcars)
#' 
#' # Use normality testing to choose summary format
#' ternD(mtcars, consider_normality = TRUE)
#' 
#' # Force specific variables to use median [IQR]
#' ternD(mtcars, force_ordinal = c("hp", "qsec"), consider_normality = TRUE)
#' 
#' # Export to Word document
#' ternD(mtcars, output_docx = "descriptive_table.docx", consider_normality = TRUE)
#' 
#' @export
ternD <- function(data, vars = NULL, exclude_vars = NULL, force_ordinal = NULL,
                  output_xlsx = NULL, output_docx = NULL,
                  consider_normality = FALSE, print_normality = FALSE, 
                  round_intg = FALSE, smart_rename = FALSE, insert_subheads = TRUE) {
  stopifnot(is.data.frame(data))

  # Helper function for proper rounding (0.5 always rounds up)
  round_up_half <- function(x, digits = 0) {
    if (digits == 0) {
      floor(x + 0.5)
    } else {
      factor <- 10^digits
      floor(x * factor + 0.5) / factor
    }
  }

  if (is.null(vars)) {
    vars <- setdiff(names(data), exclude_vars)
  }

  fmt_mean_sd <- function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- stats::sd(x, na.rm = TRUE)
    if (round_intg) {
      paste0(round_up_half(m, 0), " ± ", round_up_half(s, 0))
    } else {
      paste0(round(m, 1), " ± ", round(s, 1))
    }
  }

  fmt_median_iqr <- function(x) {
    q <- stats::quantile(x, probs = c(0.25, 0.5, 0.75), na.rm = TRUE, names = FALSE)
    if (round_intg) {
      paste0(round_up_half(q[2], 0), " [", round_up_half(q[1], 0), "–", round_up_half(q[3], 0), "]")
    } else {
      paste0(round(q[2], 1), " [", round(q[1], 1), "–", round(q[3], 1), "]")
    }
  }

  shapiro_p <- function(x) {
    x <- x[!is.na(x)]
    # Shapiro requires n >= 3 and not all equal
    if (length(x) < 3 || stats::var(x) == 0) {
      return(NA_real_)
    }
    out <- tryCatch(stats::shapiro.test(x)$p.value, error = function(e) NA_real_)
    out
  }

  summarize_variable <- function(df, var) {
    v <- df[[var]]
    
    # Helper function to clean variable names for headers
    .clean_variable_name_for_header <- function(var_name) {
      # Remove common suffixes and clean up for display
      clean_name <- var_name
      clean_name <- gsub("_simplified$", "", clean_name)
      clean_name <- gsub("_calc$", "", clean_name)
      clean_name <- gsub("_tpx$", "", clean_name)
      clean_name <- gsub("_", " ", clean_name)
      clean_name <- tools::toTitleCase(clean_name)
      
      # Handle common abbreviations
      clean_name <- gsub("\\bCod\\b", "Cause of Death", clean_name)
      clean_name <- gsub("\\bDbd Dcd\\b", "Mode of Organ Donation", clean_name)
      clean_name <- gsub("\\bPhm\\b", "Predicted Heart Mass", clean_name)
      clean_name <- gsub("\\bPhs\\b", "PHS", clean_name)
      clean_name <- gsub("\\bLvef\\b", "LVEF", clean_name)
      
      return(clean_name)
    }

    # ---------- CATEGORICAL ----------
    if (is.factor(v) || is.character(v) || is.logical(v)) {
      v <- factor(v) # ensure levels
      tab <- table(v, useNA = "no")
      if (length(tab) == 0) {
        # all missing
        out <- tibble::tibble(Variable = paste0("  ", var), Summary = "0 (0%)")
        if (print_normality) out$SW_p <- NA_real_
        return(out)
      }
      pct <- round(100 * prop.table(tab))
      
      # Sort levels by frequency (descending order - most common first)
      sorted_levels <- names(sort(tab, decreasing = TRUE))
      
      # Determine if this should use simple format or hierarchical subheads
      # Always use simple format for Y/N variables or when insert_subheads is FALSE
      # Otherwise use hierarchical format for multi-level categorical variables
      is_yes_no <- all(c("Y", "N") %in% sorted_levels)
      is_yes_no_full <- all(c("Yes", "No") %in% sorted_levels)
      is_binary <- is_yes_no || is_yes_no_full
      use_hierarchical <- !is_binary && insert_subheads && length(sorted_levels) > 1
      
      if (use_hierarchical) {
        # Create header row for the main variable
        header_row <- tibble::tibble(
          Variable = paste0("  ", .clean_variable_name_for_header(var)),
          Summary = ""
        )
        if (print_normality) header_row$SW_p <- NA_real_
        
        # Create sub-category rows (indented)
        sub_rows <- lapply(sorted_levels, function(level) {
          n <- as.integer(tab[[level]])
          p <- pct[[level]]
          row <- tibble::tibble(
            Variable = paste0("      ", level),
            Summary  = paste0(n, " (", p, "%)")
          )
          if (print_normality) row$SW_p <- NA_real_
          return(row)
        })
        
        # Combine header and sub-rows
        out <- dplyr::bind_rows(list(header_row), sub_rows)
      } else {
        # For Y/N variables or when insert_subheads=FALSE, use simple format with 2-space indentation
        # For Y/N, show only the "Y" level; for Yes/No, show only "Yes"; for others, show most common level
        if (is_yes_no) {
          # Y/N variables: show only Y
          ref_level <- "Y"
        } else if (is_yes_no_full) {
          # Yes/No variables: show only Yes
          ref_level <- "Yes"
        } else {
          # Other variables: show most common level
          ref_level <- sorted_levels[1]  # Already sorted by frequency
        }
        
        row <- tibble::tibble(
          Variable = paste0("  ", var, ": ", ref_level),
          Summary = paste0(as.integer(tab[[ref_level]]), " (", pct[[ref_level]], "%)")
        )
        if (print_normality) row$SW_p <- NA_real_
        rows <- list(row)
        out <- dplyr::bind_rows(rows)
      }
      return(out)
    }

    # ---------- NUMERIC ----------
    x <- suppressWarnings(as.numeric(v))
    # compute normality p (even if consider_normality = FALSE, we may print it)
    sw <- if (print_normality || consider_normality) shapiro_p(x) else NA_real_

    # Check if variable is forced to be ordinal
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      # Force ordinal: use median/IQR regardless of consider_normality setting
      summary_str <- fmt_median_iqr(x)
    } else if (isTRUE(consider_normality)) {
      # choose mean +- SD if normal; else median [IQR]
      if (!is.na(sw) && sw >= 0.05) {
        summary_str <- fmt_mean_sd(x)
      } else {
        summary_str <- fmt_median_iqr(x)
      }
    } else {
      # Default behavior when consider_normality = FALSE: use mean ± SD
      summary_str <- fmt_mean_sd(x)
    }
    
    out <- tibble::tibble(
      Variable = paste0("  ", var),
      Summary  = summary_str
    )
    if (print_normality) out$SW_p <- sw
    return(out)
  }

  out_tbl <- dplyr::bind_rows(lapply(vars, function(v) summarize_variable(data, v)))
  
  # Apply smart variable name cleaning if requested
  if (smart_rename) {
    # Source the cleaning function if not already loaded
    if (!exists("clean_variable_names")) {
      source(system.file("R", "clean_variable_names.r", package = "TernTablesR"))
    }
    
    # Clean variable names - handle both main variables and subheadings
    for (i in seq_len(nrow(out_tbl))) {
      current_var <- out_tbl$Variable[i]
      
      # Check if this is a subheading (has leading spaces)
      if (grepl("^\\s+", current_var)) {
        # Extract padding and clean the variable name part
        padding <- stringr::str_extract(current_var, "^\\s+")
        trimmed_var <- trimws(current_var)
        
        # For categorical variables with suffixes, clean only the base name
        if (grepl(": [A-Za-z0-9]+$", trimmed_var)) {
          # Split variable name and suffix
          parts <- strsplit(trimmed_var, ": ")[[1]]
          base_name <- parts[1]
          suffix <- parts[2]
          
          # Clean the base name only
          cleaned_base <- clean_variable_names(base_name, method = "hybrid")
          cleaned_var <- paste0(cleaned_base, ": ", suffix)
        } else {
          # Clean the entire variable name
          cleaned_var <- clean_variable_names(trimmed_var, method = "hybrid")
        }
        
        # Restore original padding
        out_tbl$Variable[i] <- paste0(padding, cleaned_var)
      } else {
        # This shouldn't happen with insert_subheads = TRUE, but handle it just in case
        out_tbl$Variable[i] <- clean_variable_names(current_var, method = "rules")
      }
    }
  }

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  out_tbl
}
