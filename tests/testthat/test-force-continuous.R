# Tests for force_continuous surviving the non-normal recursion.
#
# When a continuous variable fails the normality gates, ternG() re-enters
# .summarize_var_internal() via the force_ordinal path. That recursion must carry
# force_continuous with it — otherwise the automatic binary 0/1 detection at the
# top of the function fires on the second pass and converts the variable to
# categorical Y/N, silently producing the exact treatment force_continuous exists
# to prevent.
#
# The three ROBUST gates that can route a 0/1 variable are covered separately,
# because only one of them (the CLT gate) avoids the recursion altogether.

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

run_fc <- function(d, ...) {
  quiet(ternG(d, vars = "Dose01", group_var = "Grp", show_test = TRUE,
              force_continuous = "Dose01", methods_doc = FALSE, citation = FALSE, ...))
}

# Continuous renders as "m ± sd" (normal) or "med [Q1–Q3]" (non-normal);
# categorical renders as "n (p%)". The distinction under test is continuous vs.
# categorical, not parametric vs. non-parametric.
expect_continuous <- function(tbl) {
  grp <- grep("^A \\(n", names(tbl), value = TRUE)
  cell <- tbl[[grp]][1]
  expect_match(cell, "±|\\[", info = "expected mean ± SD or median [IQR]")
  expect_false(grepl("%\\)$", cell), info = "rendered as a count — treated as categorical")
  expect_equal(nrow(tbl), 1L)
}
expect_categorical <- function(tbl) {
  grp <- grep("^A \\(n", names(tbl), value = TRUE)
  expect_match(tbl[[grp]][1], "%\\)$")
}


test_that("gate 3 (CLT): large balanced 0/1 stays continuous", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 60), Dose01 = stats::rbinom(120, 1, 0.5))
  tbl <- run_fc(d)
  # This path never recursed even before the fix, so it is the control case.
  expect_continuous(tbl)
  expect_identical(tbl$test[1], "Welch t-test")
})


test_that("gate 2 (skewness): rare 0/1 stays continuous through the recursion", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 60), Dose01 = stats::rbinom(120, 1, 0.05))
  tbl <- run_fc(d)
  expect_continuous(tbl)
  # Non-normal continuous -> median [IQR] and the rank-based test, NOT Fisher/chi-sq
  expect_identical(tbl$test[1], "Wilcoxon rank-sum")
  expect_false(grepl("Fisher|Chi", tbl$test[1]))

  s <- tern_stats(tbl)
  expect_identical(s$type, "continuous")
  expect_identical(s$stat_type, "median_iqr")
  expect_false(s$is_normal)      # verdict survives the recursion
})


test_that("gate 4 (Shapiro-Wilk): small-n 0/1 stays continuous through the recursion", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 12), Dose01 = stats::rbinom(24, 1, 0.5))
  tbl <- run_fc(d)
  expect_continuous(tbl)
  expect_identical(tbl$test[1], "Wilcoxon rank-sum")

  s <- tern_stats(tbl)
  expect_identical(s$type, "continuous")
  expect_identical(s$n_levels, NA_integer_)   # not treated as a factor
})


test_that("three or more groups recurse correctly too", {
  set.seed(7)
  d <- data.frame(Grp = rep(c("A", "B", "C"), each = 10),
                  Dose01 = stats::rbinom(30, 1, 0.5))
  tbl <- run_fc(d)
  expect_match(tbl[[grep("^A \\(n", names(tbl), value = TRUE)]][1], "±|\\[")
  expect_identical(tbl$test[1], "Kruskal-Wallis")
})


test_that("the P value is the rank-based test, not the contingency-table test", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 12), Dose01 = stats::rbinom(24, 1, 0.5))
  s <- tern_stats(run_fc(d))

  expect_equal(s$p_value,
               quiet(stats::wilcox.test(d$Dose01 ~ d$Grp)$p.value),
               tolerance = 1e-12)
  # ... and demonstrably different from what the pre-fix output reported
  expect_false(isTRUE(all.equal(s$p_value,
                                stats::fisher.test(table(d$Grp, d$Dose01))$p.value)))
})


test_that("force_ordinal still wins over force_continuous", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 60), Dose01 = stats::rbinom(120, 1, 0.5))
  tbl <- quiet(ternG(d, vars = "Dose01", group_var = "Grp", show_test = TRUE,
                     force_continuous = "Dose01", force_ordinal = "Dose01",
                     methods_doc = FALSE, citation = FALSE))
  expect_continuous(tbl)
  expect_identical(tern_stats(tbl)$stat_type, "median_iqr")
})


test_that("force_normal short-circuits the recursion entirely", {
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 12), Dose01 = stats::rbinom(24, 1, 0.5))
  tbl <- quiet(ternG(d, vars = "Dose01", group_var = "Grp", show_test = TRUE,
                     force_continuous = "Dose01", force_normal = "Dose01",
                     methods_doc = FALSE, citation = FALSE))
  expect_continuous(tbl)
  expect_identical(tbl$test[1], "Welch t-test")
})


test_that("without force_continuous, 0/1 is still auto-detected as categorical", {
  # The fix must not weaken the default behaviour: this is the no-flag control.
  set.seed(3)
  d <- data.frame(Grp = rep(c("A", "B"), each = 12), Dose01 = stats::rbinom(24, 1, 0.5))
  tbl <- quiet(ternG(d, vars = "Dose01", group_var = "Grp", show_test = TRUE,
                     methods_doc = FALSE, citation = FALSE))
  expect_categorical(tbl)
  expect_identical(tern_stats(tbl)$type, "categorical")
})


test_that("non-0/1 continuous variables are unaffected by the forwarded argument", {
  set.seed(5)
  d <- data.frame(Grp = rep(c("A", "B"), each = 15),
                  Skewed = stats::rexp(30, 0.1),
                  Dose01 = stats::rbinom(30, 1, 0.5))
  # Skewed is non-normal and not binary: it recurses but force_continuous is
  # irrelevant to it, so its output must not depend on the flag.
  a <- quiet(ternG(d, vars = "Skewed", group_var = "Grp", show_test = TRUE,
                   methods_doc = FALSE, citation = FALSE))
  b <- quiet(ternG(d, vars = "Skewed", group_var = "Grp", show_test = TRUE,
                   force_continuous = "Dose01", methods_doc = FALSE, citation = FALSE))
  # Compare rendered content; ternB_meta legitimately records the argument.
  expect_identical(as.data.frame(unclass(a)[names(a)]),
                   as.data.frame(unclass(b)[names(b)]))
  expect_identical(tern_stats(a)$p_value, tern_stats(b)$p_value)
})
