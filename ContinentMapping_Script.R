# Here we were assigning each country to its continent using the countrycode 
# package. We checked for any countries that could not be matched, labelled 
# them as Other, and then printed out the distribution to confirm that the 
# mapping looked correct before moving on with the analysis.

# First, let's create the continent mapping using countrycode
population_data$Continent <- countrycode(
  sourcevar = population_data$Country..or.dependency.,
  origin = "country.name",
  destination = "continent",
  warn = FALSE
)

# Handle any countries that couldn't be mapped
na_continent <- sum(is.na(population_data$Continent))
if(na_continent > 0) {
  cat("Countries without continent assignment:", na_continent, "\n")
  # Assign "Other" for unmapped countries
  population_data$Continent[is.na(population_data$Continent)] <- "Other"
}

# Check continent distribution
cat("\n=== CONTINENT DISTRIBUTION ===\n")
continent_table <- table(population_data$Continent)
print(continent_table)
