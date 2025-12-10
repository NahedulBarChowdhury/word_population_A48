# Create contingency table for the visualization
cat("Contingency Table (Continent × Growth Status):\n")
contingency_table <- table(population_data$Continent, population_data$Growth_Status)
print(contingency_table)

# Calculate proportions
cat("\nProportions (% by Continent):\n")
prop_table <- round(prop.table(contingency_table, margin = 1) * 100, 1)
print(prop_table)

# Create THE MAIN VISUALIZATION: Stacked Bar Chart for Proportions
cat("\n--- Creating Main Visualization: Stacked Bar Chart ---\n")

stacked_bar_main <- ggplot(population_data, aes(x = Continent, fill = Growth_Status)) +
  geom_bar(position = "fill", width = 0.7) +
  labs(
    title = "Proportion of Growing vs Declining Countries by Continent",
    subtitle = "Research Question: Is there a difference in proportions among continents?",
    x = "Continent",
    y = "Proportion of Countries",
    fill = "Population Growth"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Growing" = "#2E8B57", "Declining" = "#B22222")) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  # Add percentage labels on bars
  geom_text(
    aes(label = paste0(round(..count../tapply(..count.., ..x.., sum)[..x..] * 100, 0), "%")),
    stat = "count",
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  )

# Display and save the main visualization
print(stacked_bar_main)
ggsave("RQ_visualization_stacked_bar.png", stacked_bar_main, 
       width = 10, height = 7, dpi = 300, bg = "white")
cat("1. Uses stacked bar chart for comparing PROPORTIONS (matching our RQ)\n")
cat("2. Shows clear visual differences between continents\n")
cat("3. Includes percentage labels for clarity\n")
cat("4. Directly addresses the research question\n")

# 3.2 Additional info to understand the data 
# Create a summary table
cat("Summary Statistics by Continent:\n")
summary_stats <- population_data %>%
  group_by(Continent) %>%
  summarise(
    Total_Countries = n(),
    Growing = sum(Growth_Status == "Growing"),
    Declining = sum(Growth_Status == "Declining"),
    Percent_Growing = round(Growing/Total_Countries * 100, 1)
  ) %>%
  arrange(desc(Percent_Growing))

print(as.data.frame(summary_stats))

# Create an additional histogram for Yearly Change
cat("\n--- Additional Visualization: Distribution of Yearly Change ---\n")

histogram_yearly_change <- ggplot(population_data, aes(x = Yearly_Change_Numeric)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Yearly Population Change Across Countries",
    subtitle = "Red line indicates 0% change (boundary between growing/declining)",
    x = "Yearly Population Change (%)",
    y = "Number of Countries"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )

print(histogram_yearly_change)
ggsave("additional_histogram_yearly_change.png", histogram_yearly_change,
       width = 10, height = 6, dpi = 300, bg = "white")
