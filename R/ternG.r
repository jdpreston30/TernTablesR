#' Generate grouped or descriptive summary table with appropriate statistical tests
#'
#' Creates a grouped or ungrouped summary table, with optional statistical testing
#' for group comparisons. Supports numeric, ordinal, and categorical variables, and
#' includes options to calculate p-values and odds ratios.
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all except \code{group_var} and \code{exclude_vars}.
#' @param exclude_vars Character vector of variable(s) to exclude. \code{group_var} is automatically excluded.
#' @param group_var Character, the grouping variable (factor or character with ≥2 levels).
#' @param force_ordinal Character vector of variables to treat as ordinal (i.e., use medians/IQR and nonparametric tests).
#' @param group_order Optional character vector to specify a custom group level order.
#' @param descriptive Logical; if \code{TRUE}, suppresses statistical tests and returns descriptive statistics only.
#' @param output_xlsx Optional filename to export the table as an Excel file.
#' @param output_docx Optional filename to export the table as a Word document.
#' @param OR_col Logical; if \code{TRUE}, adds odds ratios for 2-level categorical variables.
#' @param OR_method Character; if \code{"dynamic"}, uses Fisher/Wald based on test type. If \code{"wald"}, forces Wald method.
#' @param consider_normality Logical or character; if \code{TRUE}, uses Shapiro-Wilk to choose t-test vs. Wilcoxon for numeric vars. If \code{FALSE}, uses variable type and force_ordinal. If \code{"FORCE"}, treats all numeric variables as ordinal (median/IQR, nonparametric tests).
#' @param print_normality Logical; if \code{TRUE}, includes Shapiro-Wilk p-values in the output.
#' @param p_digits Integer; number of decimal places for p-values (default 3).
#'
#' @return A tibble with one row per variable (multi-row for multi-level factors), showing summary statistics by group,
#' p-values, test type, and optionally odds ratios and total summary column.
#'
#' @export
ternG <- function(data,
                  vars = NULL,
                  exclude_vars = NULL,
                  group_var,
                  force_ordinal = NULL,
                  group_order = NULL,
                  descriptive = NULL,
                  output_xlsx = NULL,
                  output_docx = NULL,
                  OR_col = FALSE,
                  OR_method = "dynamic",
                  consider_normality = TRUE,
                  print_normality = FALSE,
                  p_digits = 3) {

  if (is.null(vars)) {
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  }

  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }
  if (!is.null(group_order)) {
    data[[group_var]] <- factor(data[[group_var]], levels = group_order)
  }

  data <- data %>% filter(!is.na(.data[[group_var]]))
  n_levels <- length(unique(data[[group_var]]))
  if (is.null(descriptive)) descriptive <- n_levels < 2

  group_counts <- data %>% count(.data[[group_var]]) %>% deframe()
  group_levels <- names(group_counts)
  group_labels <- setNames(
    paste0(group_levels, " (n = ", group_counts, ")"),
    group_levels
  )

  # Initialize normality tracking variables
  normality_results <- list()
  numeric_vars_tested <- 0
  numeric_vars_failed <- 0

  .summarize_var_internal <- function(df, var, force_ordinal = NULL) {
    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) return(NULL)
    v <- g[[var]]

    # ----- Categorical -----
    if (is.character(v) || is.factor(v)) {
      g[[var]] <- factor(g[[var]])
      tab <- table(g[[group_var]], g[[var]])
      tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      tab_n   <- as.data.frame.matrix(tab)
      tab_total_n   <- colSums(tab)
      tab_total_pct <- round(prop.table(tab_total_n) * 100)

      # Calculate p-value once for the entire contingency table (not per level)
      fisher_flag <- any(tab < 5)
      test_result <- tryCatch({
        if (fisher_flag) {
          list(p_value = stats::fisher.test(tab)$p.value, test_name = "Fisher exact", error = NULL)
        } else {
          list(p_value = stats::chisq.test(tab)$p.value, test_name = "Chi-squared", error = NULL)
        }
      }, error = function(e) {
        # Determine the reason for failure
        if (nrow(tab) < 2 || ncol(tab) < 2) {
          reason <- "insufficient variation"
        } else if (sum(tab) == 0) {
          reason <- "no observations"
        } else if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) {
          reason <- "empty cells"
        } else {
          reason <- "test failure"
        }
        list(p_value = NA_real_, test_name = if (fisher_flag) "Fisher exact" else "Chi-squared", 
             error = reason)
      })
      
      if (ncol(tab) == 2) {
        ref_level <- if (all(c("Y", "N") %in% colnames(tab))) "Y" else names(sort(colSums(tab), decreasing = TRUE))[1]
        result <- tibble(Variable = paste0(var, ": ", ref_level))
        for (g_lvl in group_levels) {
          result[[group_labels[g_lvl]]] <- paste0(
            tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)"
          )
        }

        if (!is.null(test_result$error)) {
          result$p <- paste0("NA (", test_result$error, ")")
        } else {
          result$p <- fmt_p(test_result$p_value, p_digits)
        }
        result$test <- test_result$test_name

        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          if (OR_method == "dynamic") {
            if (fisher_flag) {
              fisher_obj <- tryCatch(stats::fisher.test(tab), error = function(e) NULL)
              if (!is.null(fisher_obj)) {
                result$OR <- sprintf("%.2f [%.2f–%.2f]", fisher_obj$estimate, fisher_obj$conf.int[1], fisher_obj$conf.int[2])
                result$OR_method <- "Fisher"
              } else {
                result$OR <- "NA (calculation failed)"
                result$OR_method <- "Fisher"
              }
            } else {
              or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
              result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
              result$OR_method <- "Wald"
            }
          } else if (OR_method == "wald") {
            or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
            result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else "NA (calculation failed)"
            result$OR_method <- "Wald"
          }
        } else if (OR_col) {
          result$OR <- NA_character_
          result$OR_method <- NA_character_
        }

        if (descriptive) {
          result$Total <- paste0(tab_total_n[ref_level], " (", tab_total_pct[ref_level], "%)")
        }
      } else {
        # For multinomial variables (>2 levels), share the same p-value across all levels
        rows <- lapply(colnames(tab), function(level) {
          out <- tibble(Variable = paste0(var, ": ", level))
          for (g_lvl in group_levels) {
            val <- if (g_lvl %in% rownames(tab_n) && level %in% colnames(tab_n)) {
              paste0(tab_n[g_lvl, level], " (", tab_pct[g_lvl, level], "%)")
            } else {
              "NA (NA%)"
            }
            out[[group_labels[g_lvl]]] <- val
          }
          
          if (!is.null(test_result$error)) {
            out$p <- paste0("NA (", test_result$error, ")")
          } else {
            out$p <- fmt_p(test_result$p_value, p_digits)
          }
          out$test <- test_result$test_name
          
          if (OR_col) {
            out$OR <- NA_character_
            out$OR_method <- NA_character_
          }
          if (descriptive) {
            out$Total <- paste0(tab_total_n[level], " (", tab_total_pct[level], "%)")
          }
          out
        })
        result <- bind_rows(rows)
      }
      return(result)
    }

    # ----- Force ordinal -----
    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
        Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 1),
        med = round(median(.data[[var]], na.rm = TRUE), 1),
        Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 1), .groups = "drop")
      result <- tibble(Variable = var)
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
          paste0(val$med, " [", val$Q1, "–", val$Q3, "]")
        } else {
          "NA [NA–NA]"
        }
      }
      
      test_result <- tryCatch({
        if (n_levels == 2) {
          p_val <- stats::wilcox.test(g[[var]] ~ g[[group_var]])$p.value
          list(p_value = p_val, test_name = "Wilcoxon rank-sum", error = NULL)
        } else {
          p_val <- stats::kruskal.test(g[[var]] ~ g[[group_var]])$p.value
          list(p_value = p_val, test_name = "Kruskal-Wallis", error = NULL)
        }
      }, error = function(e) {
        # Determine reason for test failure
        group_sizes <- table(g[[group_var]])
        if (any(group_sizes < 2)) {
          reason <- "insufficient group sizes"
        } else if (length(unique(g[[var]])) < 2) {
          reason <- "no variation in values"
        } else {
          reason <- "test failure"
        }
        test_name <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
        list(p_value = NA_real_, test_name = test_name, error = reason)
      })
      
      if (!is.null(test_result$error)) {
        result$p <- paste0("NA (", test_result$error, ")")
      } else {
        result$p <- fmt_p(test_result$p_value, p_digits)
      }
      result$test <- test_result$test_name
      
      if (OR_col) result$OR <- NA_character_
      if (descriptive) {
        val_total <- g %>% summarise(Q1 = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 1),
                                     med = round(median(.data[[var]], na.rm = TRUE), 1),
                                     Q3 = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 1))
        result$Total <- paste0(val_total$med, " [", val_total$Q1, "–", val_total$Q3, "]")
      }
      if (print_normality) {
        for (g_lvl in group_levels) {
          sw_p <- tryCatch({
            x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
            if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
          }, error = function(e) NA_real_)
          result[[paste0("SW_p_", g_lvl)]] <- formatC(sw_p, format = "f", digits = 4)
        }
      }
      return(result)
    }

    # ----- Normality assessment -----
    sw_p_all <- list()
    is_normal <- TRUE
    
    # Handle different consider_normality options
    if (consider_normality == "FORCE") {
      # Force all numeric variables to be treated as ordinal
      is_normal <- FALSE
      numeric_vars_tested <<- numeric_vars_tested + 1
      numeric_vars_failed <<- numeric_vars_failed + 1
    } else if (isTRUE(consider_normality)) {
      # Test normality and track results
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3) stats::shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      is_normal <- all(sw_p_all > 0.05, na.rm = TRUE)
      
      # Track normality results for reporting
      numeric_vars_tested <<- numeric_vars_tested + 1
      if (!is_normal) {
        numeric_vars_failed <<- numeric_vars_failed + 1
      }
    }
    # If consider_normality is FALSE, we don't test normality and proceed with normal distribution assumption

    if (!is_normal) {
      return(.summarize_var_internal(df, var = var, force_ordinal = var))
    }

    # ----- Normally distributed numeric -----
    stats <- g %>% group_by(.data[[group_var]]) %>% summarise(
      mean = mean(.data[[var]], na.rm = TRUE),
      sd = sd(.data[[var]], na.rm = TRUE), .groups = "drop")

    result <- tibble(Variable = var)
    for (g_lvl in group_levels) {
      val <- stats %>% filter(.data[[group_var]] == g_lvl)
      result[[group_labels[g_lvl]]] <- if (nrow(val) == 1) {
        paste0(round(val$mean, 1), " ± ", round(val$sd, 1))
      } else {
        "NA ± NA"
      }
    }

    test_result <- tryCatch({
      if (n_levels == 2) {
        p_val <- stats::t.test(g[[var]] ~ g[[group_var]])$p.value
        list(p_value = p_val, test_name = "t-test", error = NULL)
      } else {
        p_val <- stats::aov(g[[var]] ~ g[[group_var]]) %>% summary() %>% .[[1]] %>% .["Pr(>F)"][[1]][1]
        list(p_value = p_val, test_name = "ANOVA", error = NULL)
      }
    }, error = function(e) {
      # Determine reason for test failure
      group_sizes <- table(g[[group_var]])
      if (any(group_sizes < 2)) {
        reason <- "insufficient group sizes"
      } else if (all(is.na(g[[var]]))) {
        reason <- "all values missing"
      } else if (var(g[[var]], na.rm = TRUE) == 0) {
        reason <- "no variation in values"
      } else {
        reason <- "test failure"
      }
      test_name <- if (n_levels == 2) "t-test" else "ANOVA"
      list(p_value = NA_real_, test_name = test_name, error = reason)
    })
    
    if (!is.null(test_result$error)) {
      result$p <- paste0("NA (", test_result$error, ")")
    } else {
      result$p <- fmt_p(test_result$p_value, p_digits)
    }
    result$test <- test_result$test_name
    
    if (OR_col) result$OR <- NA_character_
    if (descriptive) {
      val_total <- g %>% summarise(mean = mean(.data[[var]], na.rm = TRUE), sd = sd(.data[[var]], na.rm = TRUE))
      result$Total <- paste0(round(val_total$mean, 1), " ± ", round(val_total$sd, 1))
    }
    if (print_normality && length(sw_p_all) > 0) {
      for (nm in names(sw_p_all)) {
        result[[nm]] <- formatC(sw_p_all[[nm]], format = "f", digits = 4)
      }
    }
    return(result)
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) .summarize_var_internal(data, v, force_ordinal)))
    message("Note: Categorical variables with >2 levels return multiple rows.")
    result
  })
                        
  # -- Standardize group headers and enforce final column order (level-agnostic)
  # Keep the group column names with "n = x" format as requested

  # Collect any normality cols
  normality_cols <- grep("^SW_p_", names(out_tbl), value = TRUE)

  # The group columns should already have the "n = x" format from group_labels
  existing_group_cols <- intersect(unname(group_labels), names(out_tbl))

  # Desired column order (keeping group columns with n = x format)
  desired <- c("Variable", existing_group_cols, "Total", "p", "OR", "test", "OR_method", normality_cols)

  # Reorder: put desired first (when they exist), then everything else
  out_tbl <- dplyr::select(out_tbl, dplyr::any_of(desired), dplyr::everything())
                        

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  # -- Report normality test results -----------------------------------------
  if (numeric_vars_tested > 0 && (isTRUE(consider_normality) || consider_normality == "FORCE")) {
    failed_pct <- round((numeric_vars_failed / numeric_vars_tested) * 100, 1)

    if (consider_normality == "FORCE") {
      message(sprintf(
        "%d of %d numerical variables failed normality tests at baseline (%s%%).",
        numeric_vars_failed, numeric_vars_tested, failed_pct
      ))
      message("Note: consider_normality = 'FORCE' used; all numeric variables were displayed as median [IQR] and tested using nonparametric methods (Wilcoxon rank-sum for two groups or Kruskal–Wallis for ≥3 groups).")
    } else {
      message(sprintf(
        "%d of %d numerical variables in your table failed normality tests (%s%%).",
        numeric_vars_failed, numeric_vars_tested, failed_pct
      ))

      if (failed_pct > 50) {
        message("Consider running with consider_normality = 'FORCE' if a majority of your variables fail normality for stylistic consistency.")
      } else {
        message("Consider running with consider_normality = TRUE if a minority of your variables would fail normality for stylistic consistency.")
      }
    }
  }

  return(out_tbl)
}
