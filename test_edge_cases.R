# Test edge cases that should produce NA with explanations
library(dplyr)

# Load the functions
source("R/utils_format.r")
source("R/ternG.r")

print("Testing edge cases that should produce explanatory NAs...")

# Test 1: Variable with no variation
cat("\n=== Test 1: No variation ===\n")
no_var_data <- tibble(
  constant_var = rep("A", 100),  # All the same value
  group = sample(c("Group1", "Group2"), 100, replace = TRUE)
)
result1 <- ternG(no_var_data, group_var = "group", vars = "constant_var")
print(result1)

# Test 2: All missing values in variable
cat("\n=== Test 2: All missing ===\n")
missing_data <- tibble(
  missing_var = rep(NA_character_, 100),
  group = sample(c("Group1", "Group2"), 100, replace = TRUE)
)
result2 <- ternG(missing_data, group_var = "group", vars = "missing_var")
print(result2)

# Test 3: Only one group has data
cat("\n=== Test 3: Data only in one group ===\n")
one_group_data <- tibble(
  var = c(rep("Y", 50), rep(NA_character_, 50)),  # All data in first 50
  group = c(rep("HasData", 50), rep("NoData", 50))
)
result3 <- ternG(one_group_data, group_var = "group", vars = "var")
print(result3)

# Test 4: Numeric variable with no variation
cat("\n=== Test 4: Numeric no variation ===\n")
numeric_no_var <- tibble(
  constant_num = rep(5, 100),  # All the same number
  group = sample(c("Group1", "Group2"), 100, replace = TRUE)
)
result4 <- ternG(numeric_no_var, group_var = "group", vars = "constant_num")
print(result4)

# Test 5: Groups too small
cat("\n=== Test 5: Very small groups ===\n")
small_groups <- tibble(
  var = c("Y", "N", "Y", "N"),
  group = c("A", "A", "B", "B")  # Only 2 observations per group
)
result5 <- ternG(small_groups, group_var = "group", vars = "var")
print(result5)

cat("\nEdge case tests completed!\n")
