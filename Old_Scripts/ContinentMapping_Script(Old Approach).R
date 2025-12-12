# Read the data
data <- read.csv("population_data.csv", stringsAsFactors = FALSE)

# Create function to assign continents
assign_continent <- function(country) {
  country_lower <- tolower(country)
  
  # Asia
  if (country_lower %in% c("india", "china", "indonesia", "pakistan", "bangladesh", 
                           "japan", "philippines", "vietnam", "iran", "turkey", 
                           "thailand", "myanmar", "south korea", "iraq", "afghanistan",
                           "yemen", "uzbekistan", "malaysia", "saudi arabia", "nepal",
                           "north korea", "syria", "sri lanka", "taiwan", "kazakhstan",
                           "kyrgyzstan", "tajikistan", "turkmenistan", "hong kong",
                           "israel", "jordan", "united arab emirates", "lebanon", 
                           "state of palestine", "oman", "kuwait", "qatar", "bahrain",
                           "mongolia", "bhutan", "macao", "singapore", "timor-leste",
                           "cyprus", "brunei", "maldives", "cambodia", "laos")) {
    return("Asia")
  }
  
  # Africa
  else if (country_lower %in% c("nigeria", "ethiopia", "egypt", "dr congo", "tanzania", 
                                "south africa", "kenya", "sudan", "uganda", "algeria", 
                                "morocco", "ghana", "mozambique", "madagascar", 
                                "côte d'ivoire", "cameroon", "niger", "mali", 
                                "burkina faso", "malawi", "zambia", "chad", "somalia",
                                "senegal", "guinea", "benin", "rwanda", "burundi",
                                "tunisia", "south sudan", "togo", "sierra leone",
                                "libya", "congo", "liberia", "central african republic",
                                "mauritania", "namibia", "botswana", "lesotho", 
                                "gambia", "gabon", "guinea-bissau", "equatorial guinea",
                                "mauritius", "eswatini", "djibouti", "comoros", 
                                "eritrea", "cape verde", "são tomé & principe", 
                                "seychelles", "angola", "zimbabwe", "south sudan",
                                "cabo verde")) {
    return("Africa")
  }
  
  # Europe
  else if (country_lower %in% c("united kingdom", "france", "italy", "spain", "ukraine",
                                "poland", "germany", "netherlands", "belgium", "greece",
                                "portugal", "sweden", "czech republic (czechia)", 
                                "hungary", "austria", "belarus", "switzerland", 
                                "romania", "denmark", "finland", "norway", "slovakia",
                                "ireland", "croatia", "georgia", "bosnia and herzegovina",
                                "moldova", "armenia", "lithuania", "albania", "slovenia",
                                "latvia", "north macedonia", "estonia", "montenegro",
                                "luxembourg", "malta", "iceland", "andorra", 
                                "liechtenstein", "monaco", "san marino", "vatican",
                                "faroe islands", "gibraltar", "isle of man", "serbia",
                                "bulgaria")) {
    return("Europe")
  }
  
  # North America
  else if (country_lower %in% c("united states", "canada", "mexico", "guatemala", 
                                "honduras", "cuba", "haiti", "dominican republic",
                                "el salvador", "nicaragua", "costa rica", "panama",
                                "jamaica", "puerto rico", "trinidad and tobago",
                                "bahamas", "belize", "barbados", "saint lucia",
                                "grenada", "antigua and barbuda", "dominica",
                                "saint kitts & nevis", "saint vincent & grenadines")) {
    return("North America")
  }
  
  # South America
  else if (country_lower %in% c("brazil", "colombia", "argentina", "peru", "venezuela",
                                "chile", "ecuador", "bolivia", "paraguay", "uruguay",
                                "guyana", "suriname", "french guiana", "falkland islands")) {
    return("South America")
  }
  
  # Oceania
  else if (country_lower %in% c("australia", "papua new guinea", "new zealand", "fiji",
                                "solomon islands", "vanuatu", "samoa", "kiribati",
                                "micronesia", "tonga", "marshall islands", "palau",
                                "nauru", "tuvalu", "cook islands", "niue", "tokelau",
                                "guam", "new caledonia", "french polynesia", 
                                "american samoa", "northern mariana islands",
                                "wallis & futuna")) {
    return("Oceania")
  }
  
  # Default
  else {
    return("Other")
  }
}

# Add continent column
data$Continent <- sapply(data$Country..or.dependency., assign_continent)

# Show output summary
cat("Data now has", ncol(data), "columns\n")
cat("New column 'Continent' added\n")

cat("\nFirst 20 countries with their continents:\n")
print(data[1:20, c("Country..or.dependency.", "Continent")])

# Count countries per continent
cat("\nNumber of countries per continent:\n")
print(table(data$Continent))
