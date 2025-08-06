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
                  print_normality = FALSE) {

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

      if (ncol(tab) == 2) {
        ref_level <- if (all(c("Y", "N") %in% colnames(tab))) "Y" else names(sort(colSums(tab), decreasing = TRUE))[1]
        result <- tibble(Variable = paste0(var, ": ", ref_level))
        for (g_lvl in group_levels) {
          result[[group_labels[g_lvl]]] <- paste0(
            tab_n[g_lvl, ref_level], " (", tab_pct[g_lvl, ref_level], "%)"
          )
        }

        fisher_flag <- any(tab < 5)
        result$p <- fmt_p(tryCatch(
          if (fisher_flag) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
          error = function(e) NA_real_
        ))
        result$test <- if (fisher_flag) "Fisher exact" else "Chi-squared"

        if (OR_col && ncol(tab) == 2 && nrow(tab) == 2) {
          if (OR_method == "dynamic") {
            if (fisher_flag) {
              fisher_obj <- fisher.test(tab)
              result$OR <- sprintf("%.2f [%.2f–%.2f]", fisher_obj$estimate, fisher_obj$conf.int[1], fisher_obj$conf.int[2])
              result$OR_method <- "Fisher"
            } else {
              or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
              result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else NA_character_
              result$OR_method <- "Wald"
            }
          } else if (OR_method == "wald") {
            or_obj <- tryCatch(epitools::oddsratio(tab, method = "wald")$measure, error = function(e) NULL)
            result$OR <- if (!is.null(or_obj)) sprintf("%.2f [%.2f–%.2f]", or_obj[2,1], or_obj[2,2], or_obj[2,3]) else NA_character_
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
          out$p <- fmt_p(tryCatch(
            if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
            error = function(e) NA_real_
          ))
          out$test <- if (any(tab < 5)) "Fisher exact" else "Chi-squared"
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
      p <- tryCatch(
        if (n_levels == 2) wilcox.test(g[[var]] ~ g[[group_var]])$p.value
        else kruskal.test(g[[var]] ~ g[[group_var]])$p.value,
        error = function(e) NA_real_
      )
      result$p <- fmt_p(p)
      result$test <- if (n_levels == 2) "Wilcoxon rank-sum" else "Kruskal-Wallis"
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
            if (length(x) >= 3) shapiro.test(x)$p.value else NA_real_
          }, error = function(e) NA_real_)
          result[[paste0("SW_p_", g_lvl)]] <- formatC(sw_p, format = "f", digits = 4)
        }
      }
      return(result)
    }

    # ----- Normality assessment -----
    sw_p_all <- list()
    is_normal <- TRUE
    if (isTRUE(consider_normality)) {
      sw_p_all <- tryCatch({
        out <- lapply(group_levels, function(g_lvl) {
          x <- g %>% filter(.data[[group_var]] == g_lvl) %>% pull(.data[[var]])
          pval <- if (length(x) >= 3) shapiro.test(x)$p.value else NA_real_
          setNames(pval, paste0("SW_p_", g_lvl))
        })
        do.call(c, out)
      }, error = function(e) rep(NA, n_levels))
      is_normal <- all(sw_p_all > 0.05, na.rm = TRUE)
    }

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

    p <- tryCatch(
      if (n_levels == 2) t.test(g[[var]] ~ g[[group_var]])$p.value
      else aov(g[[var]] ~ g[[group_var]]) %>% summary() %>% .[[1]] %>% .["Pr(>F)"][[1]][1],
      error = function(e) NA_real_
    )

    result$p <- fmt_p(p)
    result$test <- if (n_levels == 2) "t-test" else "ANOVA"
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

  if (!is.null(output_xlsx)) export_to_excel(out_tbl, output_xlsx)
  if (!is.null(output_docx)) export_to_word(out_tbl, output_docx)

  return(out_tbl)
}
