#' Generate 3-level grouped summary table with appropriate statistical tests
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all variables except `group_var` and `exclude_vars`.
#' @param exclude_vars Character vector of variable(s) to exclude from summary. `group_var` is automatically excluded.
#' @param group_var Character, the grouping variable with 3 levels (can be numeric or factor).
#' @param force_ordinal Character vector of variable names to treat as ordinal, using median [IQR] and Kruskal-Wallis test.
#' @param group_order Optional character or numeric vector to specify custom order of group_var levels (e.g., c(3, 4, 5)).
#' @param output_xlsx Optional filename to export the summary table to Excel (.xlsx).
#' @param output_docx Optional filename to export the summary table to Word (.docx).
#'
#' @return A tibble with one row per variable, grouped summary statistics across the 3 levels, test type, and p-value.
#' @export
TernTables3v <- function(data,
                         vars = NULL,
                         exclude_vars = NULL,
                         group_var,
                         force_ordinal = NULL,
                         group_order = NULL,
                         output_xlsx = NULL,
                         output_docx = NULL) {
  if (is.null(vars)) {
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  }

  data <- data %>% filter(!is.na(.data[[group_var]]))

  if (!is.factor(data[[group_var]])) {
    data[[group_var]] <- factor(data[[group_var]])
  }
  if (!is.null(group_order)) {
    data[[group_var]] <- factor(data[[group_var]], levels = group_order)
  }

  group_counts <- data %>%
    count(.data[[group_var]]) %>%
    deframe()
  group_labels <- setNames(
    paste0(names(group_counts), " (n = ", group_counts, ")"),
    names(group_counts)
  )

  summarize_variable <- function(df, var) {
    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) {
      return(NULL)
    }
    v <- g[[var]]

    g <- g %>% mutate(group_temp = factor(.data[[group_var]], labels = paste0("G", seq_along(levels(.data[[group_var]])))))

    if (is.factor(v) || is.character(v)) {
      tab <- table(g[[group_var]], v)
      test_type <- if (any(tab < 5)) "Fisher exact" else "Chi-squared"
      p <- tryCatch(
        if (test_type == "Fisher exact") fisher.test(tab)$p.value else chisq.test(tab)$p.value,
        error = function(e) NA_real_
      )

      tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      tab_n <- as.data.frame.matrix(tab)
      ref_level <- if (all(c("Y", "N") %in% colnames(tab))) "Y" else names(sort(colSums(tab), decreasing = TRUE))[1]

      result <- tibble(Variable = paste0(var, ": ", ref_level))
      for (g_lvl in names(group_labels)) {
        val <- if (g_lvl %in% rownames(tab_n) && ref_level %in% colnames(tab_n)) {
          paste0(tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)")
        } else {
          "NA (NA%)"
        }
        result[[group_labels[g_lvl]]] <- val
      }
      result$p <- fmt_p(p)
      result$test <- test_type
      return(result)
    } else if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          Q1 = quantile(.data[[var]], 0.25),
          med = median(.data[[var]]),
          Q3 = quantile(.data[[var]], 0.75),
          .groups = "drop"
        )

      result <- tibble(Variable = var)
      for (g_lvl in names(group_labels)) {
        row_vals <- stats %>% filter(.data[[group_var]] == g_lvl)
        val <- if (nrow(row_vals) == 1) paste0(row_vals$med, " [", row_vals$Q1, "–", row_vals$Q3, "]") else "NA [NA–NA]"
        result[[group_labels[g_lvl]]] <- val
      }

      p <- tryCatch(
        {
          pval <- kruskal.test(g[[var]] ~ as.character(g[[group_var]]))$p.value
          if (length(pval) > 0) pval[1] else NA_real_
        },
        error = function(e) NA_real_
      )
      result$p <- fmt_p(p)
      result$test <- "Kruskal-Wallis"
      return(result)
    } else {
      stats <- g %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          mean = mean(.data[[var]], na.rm = TRUE),
          sd = sd(.data[[var]], na.rm = TRUE),
          .groups = "drop"
        )

      result <- tibble(Variable = var)
      for (g_lvl in names(group_labels)) {
        row_vals <- stats %>% filter(.data[[group_var]] == g_lvl)
        val <- if (nrow(row_vals) == 1) format_val(row_vals$mean, row_vals$sd) else "NA ± NA"
        result[[group_labels[g_lvl]]] <- val
      }

      p <- tryCatch(
        {
          pval <- summary(aov(g[[var]] ~ as.character(g[[group_var]])))[[1]][["Pr(>F)"]]
          if (length(pval) > 0) pval[1] else NA_real_
        },
        error = function(e) NA_real_
      )
      result$p <- fmt_p(p)
      result$test <- "ANOVA"
      return(result)
    }
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) summarize_variable(data, v)))
    message("Note: Kruskal-Wallis test is used for 3-level ordinal/non-normal data.")
    result
  })

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  return(out_tbl)
}
