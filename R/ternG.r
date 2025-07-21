#' Generate grouped or descriptive summary table with appropriate statistical tests
#'
#' @param data Tibble containing all variables.
#' @param vars Character vector of variables to summarize. Defaults to all except `group_var` and `exclude_vars`.
#' @param exclude_vars Character vector of variable(s) to exclude. `group_var` is automatically excluded.
#' @param group_var Character, the grouping variable (factor or character with >=2 levels).
#' @param force_ordinal Character vector of variables to treat as ordinal.
#' @param group_order Optional vector to specify custom group level order.
#' @param descriptive Logical; if TRUE, adds a Total column with population-level summaries.
#' @param output_xlsx Optional filename for Excel export.
#' @param output_docx Optional filename for Word export.
#' @param OR_col Logical; if TRUE, adds Odds Ratios for 2-level categorical variables.
#' @return A tibble with one row per variable (multi-row for multi-level factors), summary statistics by group, and optionally p-value, test type, odds ratio, and total.
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
                  OR_col = FALSE) {
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
  if (is.null(descriptive)) {
    descriptive <- n_levels < 2
  }

  group_counts <- data %>% count(.data[[group_var]]) %>% deframe()
  group_levels <- names(group_counts)
  group_labels <- setNames(
    paste0(group_levels, " (n = ", group_counts, ")"),
    group_levels
  )

  summarize_variable <- function(df, var) {
    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) return(NULL)
    v <- g[[var]]

    if (is.factor(v) || is.character(v)) {
      tab <- table(g[[group_var]], v)
      tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      tab_n <- as.data.frame.matrix(tab)

      tab_total_n <- colSums(tab)
      tab_total_pct <- round(prop.table(tab_total_n) * 100)

      if (ncol(tab) == 2) {
        ref_level <- if (all(c("Y", "N") %in% colnames(tab))) "Y" else names(sort(colSums(tab), decreasing = TRUE))[1]
        result <- tibble(Variable = paste0(var, ": ", ref_level))
        for (g_lvl in group_levels) {
          result[[group_labels[g_lvl]]] <- paste0(tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)")
        }
        result$p <- fmt_p(tryCatch(if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value, error = function(e) NA_real_))
        result$test <- if (any(tab < 5)) "Fisher exact" else "Chi-squared"
        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
          result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2, 1], or_obj[2, 2], or_obj[2, 3]) else NA_character_
        } else if (OR_col) {
          result$OR <- NA_character_
        }
        if (descriptive) {
          result$Total <- paste0(tab_total_n[ref_level], " (", tab_total_pct[ref_level], "%)")
        }
      } else {
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
          out$p <- fmt_p(tryCatch(if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value, error = function(e) NA_real_))
          out$test <- if (any(tab < 5)) "Fisher exact" else "Chi-squared"
          if (OR_col) out$OR <- NA_character_
          if (descriptive) {
            out$Total <- paste0(tab_total_n[level], " (", tab_total_pct[level], "%)")
          }
          out
        })
        result <- bind_rows(rows)
      }
      return(result)
    }

    if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          Q1  = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 0),
          med = round(median(.data[[var]], na.rm = TRUE), 0),
          Q3  = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 0),
          .groups = "drop"
        )
    
      result <- tibble(Variable = var)
      for (g_lvl in group_levels) {
        val <- stats %>% filter(.data[[group_var]] == g_lvl)
        result[[group_labels[g_lvl]]] <- if (nrow(val) == 1)
          paste0(val$med, " [", val$Q1, "–", val$Q3, "]") else "NA [NA–NA]"
      }
    
      p <- tryCatch(
        if (n_levels == 2) wilcox.test(g[[var]] ~ g[[group_var]])$p.value
        else kruskal.test(g[[var]] ~ g[[group_var]])$p.value,
        error = function(e) NA_real_
      )
    
      result$p <- fmt_p(p)
      result$test <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
      if (OR_col) result$OR <- NA_character_
    
      if (descriptive) {
        val_total <- g %>%
          summarise(
            Q1  = round(quantile(.data[[var]], 0.25, na.rm = TRUE), 0),
            med = round(median(.data[[var]], na.rm = TRUE), 0),
            Q3  = round(quantile(.data[[var]], 0.75, na.rm = TRUE), 0)
          )
        result$Total <- paste0(val_total$med, " [", val_total$Q1, "–", val_total$Q3, "]")
      }
      return(result)
    }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) summarize_variable(data, v)))
    message("Note: Categorical variables with >2 levels return multiple rows.")
    result
  })

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  return(out_tbl)
}
