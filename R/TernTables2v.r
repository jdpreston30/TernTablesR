#' Generate 2-level grouped summary table with appropriate statistical tests
#'
#' @param data Tibble containing all variables
#' @param vars Variables to analyze (default: all except group_var & exclude_vars)
#' @param exclude_vars Character vector of variable(s) to exclude. Automatically includes group_var.
#' @param group_var Variable to group by (must be binary)
#' @param force_ordinal Character vector of variables to treat as ordinal and make median [IQR]
#' @param output_xlsx Optional filename to write XLSX output
#' @param output_docx Optional filename to write Word document output
#' @return A tibble with formatted summary statistics and p-values.
#' @export
Tern2v <- function(data,
                         vars = NULL,
                         exclude_vars = NULL,
                         group_var,
                         force_ordinal = NULL,
                         output_xlsx = NULL,
                         output_docx = NULL) {
  if (is.null(vars)) {
    vars <- setdiff(names(data), unique(c(exclude_vars, group_var)))
  }

  data <- data %>% filter(.data[[group_var]] %in% c("Nonoperative", "Operative"))

  group_counts <- data %>%
    filter(!is.na(.data[[group_var]])) %>%
    count(.data[[group_var]]) %>%
    deframe()

  group_labels <- c(
    Nonoperative = paste0("Nonoperative (n = ", max(group_counts[names(group_counts) == "Nonoperative"], na.rm = TRUE), ")"),
    Operative = paste0("Operative (n = ", max(group_counts[names(group_counts) == "Operative"], na.rm = TRUE), ")")
  )

  summarize_variable <- function(df, var) {
    g <- df %>% filter(!is.na(.data[[var]]), !is.na(.data[[group_var]]))
    if (nrow(g) == 0) {
      return(NULL)
    }
    v <- g[[var]]

    if (is.factor(v) || is.character(v)) {
      tab <- table(g[[group_var]], v)
      if (!all(c("Nonoperative", "Operative") %in% rownames(tab))) {
        return(NULL)
      }
      test_type <- if (any(tab < 5)) "Fisher exact" else "Chi-squared"
      p <- tryCatch(if (test_type == "Fisher exact") fisher.test(tab)$p.value else chisq.test(tab)$p.value, error = function(e) NA_real_)
      tab_pct <- as.data.frame.matrix(round(prop.table(tab, 1) * 100))
      tab_n <- as.data.frame.matrix(tab)
      ref_level <- if (all(c("Y", "N") %in% colnames(tab))) "Y" else names(sort(colSums(tab), decreasing = TRUE))[1]

      tibble(
        Variable = paste0(var, ": ", ref_level),
        !!group_labels["Nonoperative"] := paste0(tab_n["Nonoperative", ref_level], " (", tab_pct["Nonoperative", ref_level], "%)"),
        !!group_labels["Operative"] := paste0(tab_n["Operative", ref_level], " (", tab_pct["Operative", ref_level], "%)"),
        p = fmt_p(p),
        test = test_type
      )
    } else if (!is.null(force_ordinal) && var %in% force_ordinal) {
      stats <- g %>%
        group_by(.data[[group_var]]) %>%
        summarise(
          Q1 = quantile(.data[[var]], 0.25),
          med = median(.data[[var]]),
          Q3 = quantile(.data[[var]], 0.75),
          .groups = "drop"
        )
      if (!all(c("Nonoperative", "Operative") %in% stats[[group_var]])) {
        return(NULL)
      }
      p <- suppressWarnings(wilcox.test(as.formula(paste(var, "~", group_var)), data = g)$p.value)

      tibble(
        Variable = var,
        !!group_labels["Nonoperative"] := paste0(stats$med[stats[[group_var]] == "Nonoperative"], " [", stats$Q1[stats[[group_var]] == "Nonoperative"], "–", stats$Q3[stats[[group_var]] == "Nonoperative"], "]"),
        !!group_labels["Operative"] := paste0(stats$med[stats[[group_var]] == "Operative"], " [", stats$Q1[stats[[group_var]] == "Operative"], "–", stats$Q3[stats[[group_var]] == "Operative"], "]"),
        p = fmt_p(p),
        test = "Wilcoxon rank-sum"
      )
    } else {
      vals <- split(g[[var]], g[[group_var]])
      norm_check <- sapply(vals, function(x) if (length(unique(x)) > 2) shapiro.test(x)$p.value else 0)
      normal <- all(norm_check > 0.05)
      stats <- g %>%
        group_by(.data[[group_var]]) %>%
        summarise(mean = mean(.data[[var]]), sd = sd(.data[[var]]), .groups = "drop")
      if (!all(c("Nonoperative", "Operative") %in% stats[[group_var]])) {
        return(NULL)
      }

      test_type <- if (normal) "Welch t-test" else "Wilcoxon rank-sum"
      p <- tryCatch(
        {
          if (normal) {
            t.test(as.formula(paste(var, "~", group_var)), data = g)$p.value
          } else {
            wilcox.test(as.formula(paste(var, "~", group_var)), data = g)$p.value
          }
        },
        error = function(e) NA_real_
      )

      tibble(
        Variable = var,
        !!group_labels["Nonoperative"] := format_val(stats$mean[stats[[group_var]] == "Nonoperative"], stats$sd[stats[[group_var]] == "Nonoperative"]),
        !!group_labels["Operative"] := format_val(stats$mean[stats[[group_var]] == "Operative"], stats$sd[stats[[group_var]] == "Operative"]),
        p = fmt_p(p),
        test = test_type
      )
    }
  }

  out_tbl <- suppressWarnings({
    result <- bind_rows(lapply(vars, function(v) summarize_variable(data, v)))
    message("Note: Approximate p-value used for Wilcoxon test due to ties. This is expected.")
    result
  })

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)

  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  return(out_tbl)
}
