# Diagnostic script to check what's happening with the contingency tables
library(dplyr)

# Create the same test data
set.seed(123)
test_data <- tibble(
  binary_standard = sample(c("Y", "N"), 100, replace = TRUE, prob = c(0.3, 0.7)),
  binary_nonstandard = sample(c("Male", "Female"), 100, replace = TRUE, prob = c(0.8, 0.2)),
  group = sample(c("Treatment", "Control"), 100, replace = TRUE)
)

# Test the contingency table creation manually
print("=== Diagnostic: Binary Standard ===")
g1 <- test_data %>% filter(!is.na(binary_standard), !is.na(group))
print("Filtered data dimensions:")
print(dim(g1))
g1$binary_standard <- factor(g1$binary_standard)
tab1 <- table(g1$group, g1$binary_standard)
print("Contingency table:")
print(tab1)
print("Any cells < 5:")
print(any(tab1 < 5))
print("Row sums:")
print(rowSums(tab1))
print("Col sums:")
print(colSums(tab1))

# Try Fisher test manually
print("Manual Fisher test:")
tryCatch({
  fisher_result <- fisher.test(tab1)
  print(paste("Fisher p-value:", fisher_result$p.value))
}, error = function(e) {
  print(paste("Fisher test error:", e$message))
})

# Try Chi-squared test manually
print("Manual Chi-squared test:")
tryCatch({
  chisq_result <- chisq.test(tab1)
  print(paste("Chi-squared p-value:", chisq_result$p.value))
}, error = function(e) {
  print(paste("Chi-squared test error:", e$message))
})

print("=== Diagnostic: Binary Nonstandard ===")
g2 <- test_data %>% filter(!is.na(binary_nonstandard), !is.na(group))
g2$binary_nonstandard <- factor(g2$binary_nonstandard)
tab2 <- table(g2$group, g2$binary_nonstandard)
print("Contingency table:")
print(tab2)
print("Any cells < 5:")
print(any(tab2 < 5))

# Try Fisher test manually
tryCatch({
  fisher_result <- fisher.test(tab2)
  print(paste("Fisher p-value:", fisher_result$p.value))
}, error = function(e) {
  print(paste("Fisher test error:", e$message))
})
