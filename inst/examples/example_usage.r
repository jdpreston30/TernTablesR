# TernTablesR Example Usage
# 
# First install the package fresh
if ("TernTablesR" %in% rownames(installed.packages())) {
  remove.packages("TernTablesR")
}

# Install fresh version
system("R CMD INSTALL .", wait = TRUE)

# Load the freshly installed package
library(TernTablesR)
library(dplyr)
library(readr)

# Import and structure data
demographics_i <- read_csv("inst/examples/synthetic_data.csv") %>%
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
cat("Running ternG with insert_subheads = TRUE and smart_rename = TRUE...\n")
Tern2v <- ternG(
  data = demographics_i,
  exclude_vars = c("ID"),
  group_var = "index_operative",
  force_ordinal = c("ISS", "GCS"),
  group_order = c("Nonoperative", "Operative"),
  output_xlsx = "inst/examples/Outputs/summary_oper_vs_nonop.xlsx",
  output_docx = "inst/examples/Outputs/summary_oper_vs_nonop.docx",
  OR_col = FALSE,
  consider_normality = "FORCE",
  show_test = FALSE,
  insert_subheads = TRUE,
  smart_rename = TRUE
)

print(Tern2v)

# Run TernG (3-level comparison using grade)
cat("\nRunning ternG with 3-level comparison and smart_rename = TRUE...\n")
Tern3v <- ternG(
  data = demographics_i,
  exclude_vars = c("ID"),
  group_var = "grade",
  force_ordinal = c("ISS", "GCS"),
  OR_col = FALSE,
  consider_normality = "FORCE",
  show_test = FALSE,
  insert_subheads = TRUE,
  smart_rename = TRUE
)

print(Tern3v)

cat("\n✅ Examples completed successfully!\n")
cat("Generated files:\n")
cat("- inst/examples/Outputs/summary_oper_vs_nonop.xlsx\n")
cat("- inst/examples/Outputs/summary_oper_vs_nonop.docx\n")
