# Example usage of Tern2v and Tern3v functions
# Requires synthetic_data.csv located in inst/examples/

# Import and structure data
demographics_i <- read_csv("synthetic_data.csv")  %>%
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

# Run Tern2v (2-level comparison using index_operative)
Tern2v <- tern(
  data = demographics_i,
  group_var = "index_operative",
  group_order = c("Nonoperative", "Operative"),
  exclude_vars = c("MRN", "some_other_column"),
  force_ordinal = c("ISS", "GCS"),
  output_xlsx = "summary_oper_vs_nonop.xlsx"
)


# Run Tern3v (3-level comparison using grade)
Tern3v <- tern(
  data = demographics_i,
  group_var = "grade",
  group_order = c(3, 4, 5),
  exclude_vars = c("MRN", "AAST_raw", "extra_notes"),
  force_ordinal = c("ISS", "Age"),
  output_docx = "tern_summary_3grade.docx"
)
