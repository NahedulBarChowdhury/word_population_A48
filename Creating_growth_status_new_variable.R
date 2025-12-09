cat("=== CHECKING YEARLY CHANGE VALUES ===\n")
print(head(population_data$Yearly.Change, 10))
# Converting Yearly Change to numeric (handle special minus sign)
# Replacing the special minus sign with regular minus
population_data$Yearly_Change_Numeric <- gsub("−", "-", population_data$Yearly.Change)
# Removing the % sign
population_data$Yearly_Change_Numeric <- gsub("%", "", population_data$Yearly_Change_Numeric)
# Converting to numeric
population_data$Yearly_Change_Numeric <- as.numeric(population_data$Yearly_Change_Numeric)

# Creating Growth Status
population_data$Growth_Status <- ifelse(
       population_data$Yearly_Change_Numeric > 0, 
        "Growing", 
        "Declining")
table(population_data$Growth_Status)

# Testing with the first few rows
test_rows <- population_data[1:10, ]
test_rows$Growth_Status <- ifelse(
       as.numeric(gsub("%", "", gsub("−", "-", test_rows$Yearly.Change))) > 0,
      "Growing",
      "Declining")


