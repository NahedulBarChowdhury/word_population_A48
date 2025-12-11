# Here we were setting up the environment for the project. We installed and
# loaded the libraries, imported the population dataset, and checked the key
# details such as the number of countries, the variable names, the structure,
# and any missing values. This was simply to confirm that everything was clean
# and ready before moving on to the analysis.


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

