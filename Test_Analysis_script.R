# Prepare data for analysis (remove "Other" category)
population_data_main <- population_data[population_data$Continent != "Other", ]
population_data_main$Continent <- droplevels(as.factor(population_data_main$Continent))

# Create final contingency table
final_table <- table(population_data_main$Continent, population_data_main$Growth_Status)
cat("Final Contingency Table for Analysis:\n")
print(final_table)

cat("\n--- Statistical Test Selection ---\n")
cat("Test selected: Chi-square Test of Independence\n")
cat("Reason: Both variables are nominal (categorical):\n")
cat("  - Independent variable: Continent (nominal, >2 categories)\n")
cat("  - Dependent variable: Growth Status (nominal, 2 categories)\n")
cat("  - Purpose: Test if proportions are independent of groups\n")

# Perform Chi-square test
cat("\n--- Chi-square Test Output ---\n")
chi_test <- chisq.test(final_table)

# Display detailed test results
cat("Chi-square Test Results:\n")
cat(sprintf("  χ² statistic: %.3f\n", chi_test$statistic))
cat(sprintf("  Degrees of freedom: %d\n", chi_test$parameter))
cat(sprintf("  P-value: %.6f\n", chi_test$p.value))
cat(sprintf("  Significance level (α): 0.05\n"))

# Calculate and display expected frequencies
cat("\nExpected Frequencies (if no association):\n")
print(round(chi_test$expected, 2))

# Calculate effect size (Cramer's V)
n <- sum(final_table)
k <- min(dim(final_table)) - 1
cramers_v <- sqrt(chi_test$statistic / (n * k))
cat(sprintf("\nEffect Size (Cramer's V): %.3f\n", cramers_v))

# Interpretation of effect size
cat("Effect size interpretation: ")
if(cramers_v >= 0.5) {
  cat("Large effect\n")
} else if(cramers_v >= 0.3) {
  cat("Medium effect\n")
} else if(cramers_v >= 0.1) {
  cat("Small effect\n")
} else {
  cat("Negligible effect\n")
}


# The null hypothesis rejected/not rejected

# Restate the hypotheses
cat("Research Question:", 
    "'Is there a difference in the proportion of countries with growing populations among different continents?'\n\n")

cat("Null Hypothesis (H₀):", 
    "'There is NO difference in the proportion of countries with growing populations among different continents.'\n\n")

cat("Alternative Hypothesis (H₁):", 
    "'There IS a difference in the proportion of countries with growing populations among different continents.'\n\n")

# Make the statistical decision
alpha <- 0.05
cat("--- Statistical Decision ---\n")
cat(sprintf("P-value: %.6f\n", chi_test$p.value))
cat(sprintf("Alpha level: %.3f\n", alpha))

if(chi_test$p.value < alpha) {
  cat("\n✅ DECISION: REJECT the null hypothesis (H₀)\n")
  cat("   Reason: p-value (", format(chi_test$p.value, scientific = TRUE), 
      ") < α (", alpha, ")\n", sep = "")
  cat("   Conclusion: There IS a statistically significant difference in the\n")
  cat("   proportion of growing populations among different continents.\n")
  
  # Identify which continents contribute most to the difference
  cat("\n--- Post-hoc Analysis: Standardized Residuals ---\n")
  cat("(Values > |2| indicate significant departure from expected)\n")
  std_res <- round(chi_test$stdres, 2)
  print(std_res)
  
} else {
  cat("\n✅ DECISION: FAIL TO REJECT the null hypothesis (H₀)\n")
  cat("   Reason: p-value (", format(chi_test$p.value, scientific = TRUE), 
      ") ≥ α (", alpha, ")\n", sep = "")
  cat("   Conclusion: There is NO statistically significant difference in the\n")
  cat("   proportion of growing populations among different continents.\n")
}

cat(" Statistical test: Chi-square test of independence\n")
cat(sprintf(" Test results: χ²(%d) = %.2f, p = %.4f\n", 
            chi_test$parameter, chi_test$statistic, chi_test$p.value))
cat(sprintf(" Effect size: Cramer's V = %.3f\n", cramers_v))

if(chi_test$p.value < 0.05) {
  cat(" Decision: Reject the null hypothesis (H₀)\n")
} else {
  cat(" Decision: Fail to reject the null hypothesis (H₀)\n")
}

