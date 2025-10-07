# Example usage of Tern2v and Tern3v functions
# Requires synthetic_data.csv located in inst/examples/
library(dplyr)
library(readr)
library(tibble)
library(stats)
library(writexl)
library(officer)
library(flextable)

# If testing locally
source("R/ternG.r")
source("R/export_to_word.r")
source("R/utils_format.r")
# Import and structure data
demographics_i <- read_csv("inst/examples/synthetic_data.csv")  %>%
    mutate(
      survived = factor(survived),
      index_group = factor(index_group),
      renal_pres = factor(renal_pres),
      AKI = if_else(survived == "N", NA_character_, AKI),
      AKI = factor(AKI),
      return_ed_30d = if_else(survived == "N", NA_character_, return_ed_30d),
      return_ed_30d = factor(return_ed_30d),
      grade = as.integer(grade),
      GCS = as.integer(GCS),
      ISS = as.integer(ISS),
      surv_ICU_LOS = as.integer(surv_ICU_LOS),
      vent_LOS = as.integer(vent_LOS),
      surv_hosp_LOS = as.integer(surv_hosp_LOS),
      Age = as.integer(Age)
    )
# Run TernG (2-level comparison using index_operative)
Tern2v <- ternG(
  data = demographics_i,
  exclude_vars = c("ID"),
  group_var = "index_operative",
  force_ordinal = c("ISS", "GCS"),
  group_order = c("Nonoperative", "Operative"),
  output_xlsx = "Outputs/summary_oper_vs_nonop.xlsx",
  output_docx = "Outputs/summary_oper_vs_nonop.docx",
  OR_col = TRUE,
  consider_normality = "FORCE",
  show_test = FALSE
)

# Run TernG (3-level comparison using grade)
Tern3v <- ternG(
  data = demographics_i,
  exclude_vars = c("ID"),
  group_var = "grade",
  force_ordinal = c("ISS", "Age"),
  group_order = c(3, 4, 5),
  output_xlsx = "tern_summary_3grade.xlsx",
  output_docx = "tern_summary_3grade.docx",
  OR_col = FALSE,
  consider_normality = "FORCE",
  show_test = FALSE
)
