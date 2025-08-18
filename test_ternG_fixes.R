# Test script to validate ternG fixes
library(dplyr)

# Load the functions
source("R/utils_format.r")
source("R/ternG.r")

# Create synthetic test data that mimics the issues described
set.seed(123)
test_data <- tibble(
  # Binary variables that should work
  binary_standard = sample(c("Y", "N"), 100, replace = TRUE, prob = c(0.3, 0.7)),
  
  # Binary variables with non-standard levels
  binary_nonstandard = sample(c("Male", "Female"), 100, replace = TRUE, prob = c(0.8, 0.2)),
  
  # Multinomial variables
  multi_category = sample(c("Black", "White", "Other"), 100, replace = TRUE, prob = c(0.8, 0.15, 0.05)),
  
  # Variable with very sparse data (should cause test failures)
  sparse_binary = c(rep("Y", 2), rep("N", 98)),
  
  # Variable with zero cases in one group
  zero_cases = c(rep("A", 50), rep("B", 50)),  # We'll manipulate this
  
  # Numeric variable
  numeric_var = rnorm(100, mean = 50, sd = 10),
  
  # Grouping variable
  group = sample(c("Treatment", "Control"), 100, replace = TRUE)
)

# Manipulate zero_cases to have zero in one group for some levels
test_data$zero_cases[test_data$group == "Treatment"] <- "A"

print("Testing ternG with problematic data...")

# Test 1: Standard run
cat("\n=== Test 1: Standard run ===\n")
result1 <- ternG(test_data, group_var = "group")
print(head(result1, 10))

# Test 2: Focus on sparse data
cat("\n=== Test 2: Sparse data test ===\n")
sparse_data <- tibble(
  sparse_var = c(rep("Y", 1), rep("N", 99)),
  group = sample(c("A", "B"), 100, replace = TRUE)
)
result2 <- ternG(sparse_data, group_var = "group", vars = "sparse_var")
print(result2)

# Test 3: Zero cases in a group
cat("\n=== Test 3: Zero cases test ===\n")
zero_data <- tibble(
  zero_var = c(rep("Y", 50), rep("N", 50)),
  group = c(rep("A", 50), rep("B", 50))
)
# Make all Y's in group A, all N's in group B
zero_data$zero_var[zero_data$group == "A"] <- "Y"
zero_data$zero_var[zero_data$group == "B"] <- "N"
result3 <- ternG(zero_data, group_var = "group", vars = "zero_var")
print(result3)

cat("\nTests completed!\n")




