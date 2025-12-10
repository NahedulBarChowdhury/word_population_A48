install.packages("tidyverse")  # For data manipulation
install.packages("countrycode") # For country-to-continent mapping
install.packages("ggplot2")     # For visualizations
install.packages("dplyr")
install.packages("viridis")

# Load libraries
library(tidyverse)
library(countrycode)
library(ggplot2)
library(dplyr)
library(viridis)

cat("✅ Libraries loaded successfully!\n")

population_data <- read.csv("population_data.csv")

# Basic dataset information
cat("=== DATASET OVERVIEW ===\n")
cat("Total countries:", nrow(population_data), "\n")
cat("Variables available:\n")
print(names(population_data))

# View structure of the dataset
str(population_data)
head(population_data, 3)

# Check for missing values
missing_values <- sum(is.na(population_data))
cat("\nTotal missing values:", missing_values, "\n")

