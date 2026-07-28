# Tests for the tidy statistics side-channel (tern_stats / tern_estimates).
#
# The central guarantee under test is that the tidy frame agrees with the
# rendered display table in every case. Several tests therefore harvest the
# display table the hard way — walking .indent and filling variable names down —
# and assert the accessor returns the same answer. That fragile walk is exactly
# what the accessor exists to replace; here it serves as an independent oracle.

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# Compare frame contents without the side-channel attributes they carry.
bare <- function(x) {
  attr(x, "tern_stats") <- NULL
  attr(x, "tern_estimates") <- NULL
  attr(x, "ternB_meta") <- NULL
  as.data.frame(x)
}

# Independent oracle: recover test/P from the rendered layout without the API.
harvest_display <- function(tg) {
  h <- tg
  h$.var <- ifelse(h$.indent == 2, h$Variable, NA_character_)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$.var[i]) && i > 1) h$.var[i] <- h$.var[i - 1]
  }
  h <- h[!is.na(h$P) & trimws(h$P) != "" & trimws(h$P) != "-", ]
  h[!duplicated(h$.var), ]
}

make_data <- function() {
  d <- TernTables::tern_colon
  set.seed(11)
  d$Age_Years[sample(nrow(d), 15)] <- NA
  d
}


test_that("tern_stats() is attached to ternG output and has one row per variable", {
  d <- make_data()
  vars <- c("Age_Years", "Sex", "Positive_Lymph_Nodes_n", "Tumor_Differentiation")
  tg <- quiet(ternG(d, vars = vars, group_var = "Recurrence",
                    methods_doc = FALSE, citation = FALSE))

  s <- tern_stats(tg)
  expect_s3_class(s, "tbl_df")
  expect_identical(s$variable, vars)          # original names, in requested order
  expect_false(anyDuplicated(s$variable) > 0) # exactly one row per variable
  expect_true(all(c("variable", "label", "type", "stat_type", "n", "n_missing",
                    "test", "statistic", "df", "p_value", "p_fmt", "is_normal",
                    "sw_p", "or_value", "or_fmt") %in% names(s)))
})


test_that("tidy p-values and test names match the rendered table exactly", {
  d <- make_data()
  tg <- quiet(ternG(d, exclude_vars = "ID", group_var = "Recurrence",
                    show_test = TRUE, indent_info_column = TRUE,
                    methods_doc = FALSE, citation = FALSE))
  s <- tern_stats(tg)
  h <- harvest_display(tg)

  # Same variables, same order, same displayed P string and test name.
  expect_identical(s$label, h$.var)
  expect_identical(s$p_fmt, h$P)
  expect_identical(s$test,  h$test)

  # The numeric p_value is the un-rounded source of the displayed string.
  expect_identical(
    vapply(s$p_value, val_p_format, character(1), digits = 3),
    s$p_fmt
  )
})


test_that("tidy p-values match independent base-R recomputation", {
  d <- make_data()
  vars <- c("Age_Years", "Sex", "Positive_Lymph_Nodes_n")
  tg <- quiet(ternG(d, vars = vars, group_var = "Recurrence",
                    methods_doc = FALSE, citation = FALSE))
  s <- tern_stats(tg)

  g <- d[!is.na(d$Recurrence), ]

  # Age: routed to parametric (all groups n >= 30 -> CLT gate) -> Welch t-test
  age <- g[!is.na(g$Age_Years), ]
  expect_identical(s$test[s$variable == "Age_Years"], "Welch t-test")
  expect_equal(s$p_value[s$variable == "Age_Years"],
               stats::t.test(age$Age_Years ~ age$Recurrence, var.equal = FALSE)$p.value,
               tolerance = 1e-12)

  # Sex: categorical, all expected counts >= 5 -> chi-squared
  sx <- g[!is.na(g$Sex), ]
  expect_identical(s$test[s$variable == "Sex"], "Chi-squared")
  expect_equal(s$p_value[s$variable == "Sex"],
               stats::chisq.test(table(sx$Recurrence, sx$Sex))$p.value,
               tolerance = 1e-12)

  # Positive lymph nodes: heavily skewed -> non-parametric
  ln <- g[!is.na(g$Positive_Lymph_Nodes_n), ]
  expect_identical(s$test[s$variable == "Positive_Lymph_Nodes_n"], "Wilcoxon rank-sum")
  expect_equal(s$p_value[s$variable == "Positive_Lymph_Nodes_n"],
               quiet(stats::wilcox.test(ln$Positive_Lymph_Nodes_n ~ ln$Recurrence)$p.value),
               tolerance = 1e-12)
})


test_that("test statistics and degrees of freedom are captured", {
  d <- make_data()
  s2 <- tern_stats(quiet(ternG(d, vars = c("Age_Years", "Sex"), group_var = "Recurrence",
                               methods_doc = FALSE, citation = FALSE)))
  # Welch t-test: statistic and fractional df
  expect_true(is.finite(s2$statistic[s2$variable == "Age_Years"]))
  expect_true(s2$df[s2$variable == "Age_Years"] %% 1 != 0)  # Welch df is fractional
  # Chi-squared on a 2x2 table: df = 1
  expect_equal(s2$df[s2$variable == "Sex"], 1)

  # Welch ANOVA reports numerator and denominator df
  s3 <- tern_stats(quiet(ternG(d, vars = "Age_Years", group_var = "Treatment_Arm",
                               methods_doc = FALSE, citation = FALSE)))
  expect_identical(s3$test, "Welch ANOVA")
  expect_equal(s3$df, 2)              # 3 groups -> numerator df 2
  expect_true(is.finite(s3$df2))
})


test_that("normality routing decision is exposed", {
  d <- make_data()
  s <- tern_stats(quiet(ternG(d, vars = c("Age_Years", "Positive_Lymph_Nodes_n", "Sex"),
                              group_var = "Recurrence",
                              methods_doc = FALSE, citation = FALSE)))

  # mean +/- SD <=> is_normal TRUE; median [IQR] via the ROBUST gates <=> FALSE
  expect_true(s$is_normal[s$variable == "Age_Years"])
  expect_identical(s$stat_type[s$variable == "Age_Years"], "mean_sd")
  expect_false(s$is_normal[s$variable == "Positive_Lymph_Nodes_n"])
  expect_identical(s$stat_type[s$variable == "Positive_Lymph_Nodes_n"], "median_iqr")

  # Categorical variables are never normality-assessed
  expect_true(is.na(s$is_normal[s$variable == "Sex"]))
  expect_identical(s$type[s$variable == "Sex"], "categorical")

  # force_ordinal bypasses assessment entirely -> no verdict to report
  sf <- tern_stats(quiet(ternG(d, vars = "Age_Years", group_var = "Recurrence",
                               force_ordinal = "Age_Years",
                               methods_doc = FALSE, citation = FALSE)))
  expect_true(is.na(sf$is_normal))
  expect_identical(sf$stat_type, "median_iqr")
})


test_that("odds ratios are exposed as numerics matching the formatted string", {
  d <- make_data()
  tg <- quiet(ternG(d, vars = c("Sex", "Colonic_Obstruction", "Age_Years"),
                    group_var = "Recurrence", OR_col = TRUE, show_test = TRUE,
                    methods_doc = FALSE, citation = FALSE))
  s <- tern_stats(tg)

  binary <- s[!is.na(s$or_value), ]
  expect_gt(nrow(binary), 0)
  expect_identical(
    sprintf("%.2f [%.2f–%.2f]", binary$or_value, binary$or_lcl, binary$or_ucl),
    binary$or_fmt
  )
  expect_true(all(binary$or_lcl <= binary$or_value & binary$or_value <= binary$or_ucl))
  expect_true(all(binary$or_method %in% c("Fisher", "Wald")))

  # Continuous variables get no odds ratio
  expect_true(is.na(s$or_value[s$variable == "Age_Years"]))
})


test_that("BH-corrected p-values are recorded and match the display column", {
  d <- make_data()
  tg <- quiet(ternG(d, exclude_vars = "ID", group_var = "Recurrence",
                    p_adjust = TRUE, p_adjust_display = "both",
                    indent_info_column = TRUE, methods_doc = FALSE, citation = FALSE))
  s <- tern_stats(tg)

  h <- tg
  h$.var <- ifelse(h$.indent == 2, h$Variable, NA_character_)
  for (i in seq_len(nrow(h))) if (is.na(h$.var[i]) && i > 1) h$.var[i] <- h$.var[i - 1]
  h <- h[!is.na(h$`P value`) & trimws(h$`P value`) != "" & trimws(h$`P value`) != "-", ]
  h <- h[!duplicated(h$.var), ]

  expect_identical(s$p_adjusted_fmt, h$`P value (FDR corrected)`)
  expect_equal(s$p_adjusted,
               stats::p.adjust(s$p_value, method = "BH"),
               tolerance = 1e-12)
  # Correction never decreases a p-value
  expect_true(all(s$p_adjusted >= s$p_value - 1e-12, na.rm = TRUE))
})


test_that("statistics are recorded even when suppressed from the display", {
  d <- make_data()
  tg <- quiet(ternG(d, vars = c("Age_Years", "Sex"), group_var = "Recurrence",
                    show_p = FALSE, methods_doc = FALSE, citation = FALSE))
  expect_false("P" %in% names(tg))          # suppressed in the rendered table

  s <- tern_stats(tg)
  expect_true(all(!is.na(s$p_value)))       # still available programmatically
  expect_true(all(!is.na(s$test)))
})


test_that("tern_estimates() reports the values behind each displayed cell", {
  d <- make_data()
  tg <- quiet(ternG(d, vars = c("Age_Years", "Sex"), group_var = "Recurrence",
                    indent_info_column = TRUE, methods_doc = FALSE, citation = FALSE))
  est <- tern_estimates(tg)
  grp_col <- grep("^No Recurrence", names(tg), value = TRUE)

  # Continuous: raw mean/sd are un-rounded and reproduce the displayed string
  age <- est[est$variable == "Age_Years" & est$group == "No Recurrence", ]
  expect_equal(nrow(age), 1)
  expect_identical(age$value_fmt, tg[[grp_col]][tg$Variable == "Age (yr)"])
  expect_true(age$mean %% 1 != 0)  # un-rounded, unlike the displayed value
  raw <- d[!is.na(d$Recurrence) & d$Recurrence == "No Recurrence" & !is.na(d$Age_Years), ]
  expect_equal(age$mean, mean(raw$Age_Years), tolerance = 1e-12)
  expect_equal(age$sd, stats::sd(raw$Age_Years), tolerance = 1e-12)

  # Categorical: every level present, matching the rendered sub-rows
  sx <- est[est$variable == "Sex" & est$group == "No Recurrence", ]
  expect_setequal(sx$level, c("Female", "Male"))
  expect_identical(
    sx$value_fmt[sx$level == "Male"],
    tg[[grp_col]][tg$Variable == "Male"]
  )

  # A Total row is always emitted for each variable
  expect_true(all(c("Age_Years", "Sex") %in% est$variable[est$group == "Total"]))
})


test_that("tern_estimates() reports levels the display collapses away", {
  d <- make_data()
  # Colonic_Obstruction is binary Y/N: the table shows only the "Y" row
  tg <- quiet(ternG(d, vars = "Colonic_Obstruction", group_var = "Recurrence",
                    methods_doc = FALSE, citation = FALSE))
  expect_equal(nrow(tg), 1L)

  est <- tern_estimates(tg)
  expect_setequal(unique(est$level), c("Y", "N"))
})


test_that("labels track smart_rename and variable keys never do", {
  d <- make_data()
  on_  <- tern_stats(quiet(ternG(d, vars = "Age_Years", group_var = "Recurrence",
                                 smart_rename = TRUE, methods_doc = FALSE, citation = FALSE)))
  off_ <- tern_stats(quiet(ternG(d, vars = "Age_Years", group_var = "Recurrence",
                                 smart_rename = FALSE, methods_doc = FALSE, citation = FALSE)))

  # The join key is the raw column name regardless of display-name cleaning
  expect_identical(on_$variable, "Age_Years")
  expect_identical(off_$variable, "Age_Years")
  # ... while the label mirrors whatever the table rendered
  expect_identical(on_$label, "Age (yr)")
})


test_that("plain_tibble = TRUE returns the tidy frame directly", {
  d <- make_data()
  vars <- c("Age_Years", "Sex")
  tg <- quiet(ternG(d, vars = vars, group_var = "Recurrence",
                    methods_doc = FALSE, citation = FALSE))
  pt <- quiet(ternG(d, vars = vars, group_var = "Recurrence", plain_tibble = TRUE,
                    methods_doc = FALSE, citation = FALSE))

  expect_identical(bare(pt), bare(tern_stats(tg)))
  expect_identical(bare(tern_estimates(pt)), bare(tern_estimates(tg)))
  # No indentation column, no multi-line headers, one row per variable
  expect_false(".indent" %in% names(pt))
  expect_false(any(grepl("\n", names(pt))))
  expect_identical(nrow(pt), length(vars))
})


test_that("ternD attaches the same tidy contract", {
  d <- make_data()
  vars <- c("Age_Years", "Sex", "Positive_Lymph_Nodes_n")
  td <- quiet(ternD(d, vars = vars, methods_doc = FALSE, citation = FALSE))
  s  <- tern_stats(td)

  expect_identical(s$variable, vars)
  expect_true(all(is.na(s$test)))        # ternD makes no group comparison
  expect_true(all(is.na(s$p_value)))
  expect_identical(s$type, c("continuous", "categorical", "continuous"))
  expect_equal(s$n_missing[s$variable == "Age_Years"], 15L)

  est <- tern_estimates(td)
  expect_identical(unique(est$group), "Total")
  total_col <- grep("^Total", names(td), value = TRUE)
  expect_identical(
    est$value_fmt[est$variable == "Age_Years"],
    td[[total_col]][td$Variable == "Age (yr)"]
  )

  pt <- quiet(ternD(d, vars = vars, plain_tibble = TRUE,
                    methods_doc = FALSE, citation = FALSE))
  expect_identical(bare(pt), bare(s))
})


test_that("failed tests are reported as a note rather than silently dropped", {
  d <- make_data()
  d$AllSame <- 1L
  s <- tern_stats(quiet(ternG(d, vars = "AllSame", group_var = "Recurrence",
                              methods_doc = FALSE, citation = FALSE)))

  expect_equal(nrow(s), 1L)
  expect_true(is.na(s$p_value))
  expect_false(is.na(s$test_note))
  expect_match(s$p_fmt, "^NA \\(")
})


test_that("accessors give an actionable error on a plain tibble", {
  expect_error(tern_stats(tibble::tibble(a = 1)), "tern_stats")
  expect_error(tern_estimates(tibble::tibble(a = 1)), "tern_estimates")
})
