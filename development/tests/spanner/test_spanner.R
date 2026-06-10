# Manual tests for spanner and col1_header behaviour.
# Run from the package root:
#   devtools::load_all(".")
#   source("development/tests/spanner/test_spanner.R")

library(tibble)

out <- "development/tests/spanner"

# ── Test 1: basic two-group spanner ──────────────────────────────────────────
# Expected: grey spanner row above the column headers with "Control" spanning
# columns 2-3 and "Treatment" spanning columns 4-5; column 1 has an empty
# spanner cell.
tbl1 <- tibble(
  Variable     = c("Age (yr)", "BMI", "Weight (kg)"),
  `Ctrl Mean`  = c("45.2", "27.1", "72.1"),
  `Ctrl SD`    = c("8.1",  "3.4",  "12.3"),
  `Tx Mean`    = c("43.8", "26.8", "71.5"),
  `Tx SD`      = c("7.9",  "3.1",  "11.8")
)

ternStyle(
  tbl      = tbl1,
  filename = file.path(out, "test1_basic_spanner.docx"),
  spanner  = list(
    "Control"   = c("Ctrl Mean", "Ctrl SD"),
    "Treatment" = c("Tx Mean",   "Tx SD")
  ),
  open_doc = FALSE,
  citation = FALSE
)
message("Test 1 written: test1_basic_spanner.docx")


# ── Test 2: regression-style spanner with col1_header ────────────────────────
# Expected: spanner row with "Univariate" over cols 2-3 and "Multivariate"
# over cols 4-5. Top-left header cell shows just "Variable" (no Category label)
# because col1_header is set explicitly.
tbl2 <- tibble(
  Variable            = c("Age (yr)", "BMI", "Smoking"),
  `Uni HR (95% CI)`   = c("1.02 [0.98-1.06]", "1.11 [1.03-1.19]", "2.14 [1.45-3.16]"),
  `Uni p`             = c("0.31",  "0.006",  "< 0.001"),
  `Multi HR (95% CI)` = c("1.01 [0.97-1.05]", "1.08 [1.00-1.17]", "1.87 [1.23-2.84]"),
  `Multi p`           = c("0.64",  "0.047",  "0.003")
)

ternStyle(
  tbl               = tbl2,
  filename          = file.path(out, "test2_regression_spanner.docx"),
  col1_header       = "Variable",   # <-- just "Variable", no "Category\n   Variable"
  line_break_header = FALSE,
  spanner           = list(
    "Univariate"   = c("Uni HR (95% CI)", "Uni p"),
    "Multivariate" = c("Multi HR (95% CI)", "Multi p")
  ),
  table_caption = "Table 2. Multivariable regression results.",
  open_doc = FALSE,
  citation = FALSE
)
message("Test 2 written: test2_regression_spanner.docx")


# ── Test 3: col1_name + line_break_header = FALSE, NO col1_header ────────────
# This demonstrates the BUG / gotcha:
#   col1_name = "Variable" does NOT change the header cell.
#   The top-left still reads "Category\n   Variable" because col1_header is NULL.
# Use this to visually confirm the behaviour before explaining it to a user.
tbl3 <- tibble(
  Predictor  = c("Age (yr)", "BMI"),
  `Group A`  = c("45.2", "27.1"),
  `Group B`  = c("43.8", "26.8")
)

ternStyle(
  tbl               = tbl3,
  filename          = file.path(out, "test3_col1name_no_col1header.docx"),
  col1_name         = "Variable",   # renames data column label only
  line_break_header = FALSE,        # does NOT suppress "Category\n   Variable" header
  open_doc = FALSE,
  citation = FALSE
)
message("Test 3 written: test3_col1name_no_col1header.docx  [expect: top-left still shows 'Category / Variable']")


# ── Test 4: correct way to suppress "Category / Variable" ────────────────────
# Pass col1_header = "Variable" to override the top-left cell explicitly.
ternStyle(
  tbl               = tbl3,
  filename          = file.path(out, "test4_col1header_variable.docx"),
  col1_header       = "Variable",   # this is the correct way
  line_break_header = FALSE,
  open_doc = FALSE,
  citation = FALSE
)
message("Test 4 written: test4_col1header_variable.docx  [expect: top-left shows just 'Variable']")


# ── Test 5: spanner + ternB bundling ─────────────────────────────────────────
# Confirms that spanner metadata survives the ternB() round-trip.
t5a <- ternStyle(
  tbl      = tbl1,
  spanner  = list("Control" = c("Ctrl Mean", "Ctrl SD"),
                  "Treatment" = c("Tx Mean", "Tx SD")),
  table_caption = "Table A. Descriptive statistics.",
  open_doc = FALSE,
  citation = FALSE
)

t5b <- ternStyle(
  tbl               = tbl2,
  col1_header       = "Variable",
  line_break_header = FALSE,
  spanner           = list("Univariate"   = c("Uni HR (95% CI)", "Uni p"),
                           "Multivariate" = c("Multi HR (95% CI)", "Multi p")),
  table_caption     = "Table B. Multivariable regression.",
  open_doc = FALSE,
  citation = FALSE
)

ternB(
  tables      = list(t5a, t5b),
  output_docx = file.path(out, "test5_ternB_bundle.docx"),
  open_doc    = FALSE,
  citation    = TRUE
)
message("Test 5 written: test5_ternB_bundle.docx  [expect: both tables retain their spanner rows]")

# ── Test 6: named inner vectors — strip redundant group prefix ────────────────
# Tibble has columns "SLAM ρ", "SLAM p", etc.  The spanner labels each group
# and the named inner vectors override the column-header row so it shows just
# "ρ" and "p" under each group rather than the full prefixed names.
# Expected: spanner row shows SLAM | Control | NMN; column-names row shows ρ p | ρ p | ρ p
tbl6 <- tibble(
  Variable    = c("Gene A", "Gene B", "Gene C"),
  `SLAM ρ`    = c("0.42",  "−0.18",  "0.67"),
  `SLAM p`    = c("0.031", "0.214",  "< 0.001"),
  `Control ρ` = c("0.11",  "0.05",   "0.38"),
  `Control p` = c("0.401", "0.712",  "0.044"),
  `NMN ρ`     = c("0.58",  "−0.31",  "0.72"),
  `NMN p`     = c("0.008", "0.089",  "< 0.001")
)

ternStyle(
  tbl         = tbl6,
  filename    = file.path(out, "test6_named_spanner.docx"),
  col1_header = "Gene",
  spanner     = list(
    "SLAM"    = c("ρ" = "SLAM ρ",    "p" = "SLAM p"),
    "Control" = c("ρ" = "Control ρ", "p" = "Control p"),
    "NMN"     = c("ρ" = "NMN ρ",     "p" = "NMN p")
  ),
  open_doc = FALSE,
  citation = FALSE
)
message("Test 6 written: test6_named_spanner.docx  [expect: col headers show ρ p ρ p ρ p under SLAM/Control/NMN spanners]")

message("\nAll tests complete. Open the .docx files in: ", out)
