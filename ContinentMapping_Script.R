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
