# Example usage of TernTables2v and TernTables3v functions
# Requires synthetic_data.csv located in inst/examples/

# Import and structure data
demographics_i <- read_csv("synthetic_data.csv")  %>%
    mutate(
      survived = factor(survived),
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

demographics <- TernTables2v(
  data = demographics_i,
  exclude_vars = "ID",
  group_var = "index_operative", # replace with your binary grouping variable
  force_ordinal = c("grade"), # replace as needed
  output_xlsx = "output.xlsx",
  output_docx = "output.docx",
)

# Run TernTables3v (3-level comparison using grade)
demographics_by_grade <- TernTables3v(
  data = demographics_i,
  exclude_vars = "ID",
  group_var = "grade", # grade must be 3-level numeric (e.g., 3, 4, 5)
  force_ordinal = c("ISS", "GCS"), # add other ordinal vars here if needed
  output_xlsx = "output_by_grade.xlsx",
  output_docx = "output_by_grade.docx"
)
