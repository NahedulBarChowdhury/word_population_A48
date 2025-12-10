# Story: "Which continents are above/below average in growth?"

# Calculate average growth percentage globally
avg_growing <- mean(population_data$Growth_Status == "Growing") * 100

# Create data for diverging bars
diverging_data <- population_data %>%
  group_by(Continent) %>%
  summarise(
    Total = n(),
    Growing = sum(Growth_Status == "Growing"),
    Percent_Growing = Growing / Total * 100,
    Above_Average = Percent_Growing > avg_growing
  )

diverging_colors <- c("#E74C3C", "#3498DB") # Red-Blue gradient for above/below

diverging_bar <- ggplot(diverging_data, aes(x = Continent)) +
  # Bars for percent growing
  geom_bar(
    aes(y = Percent_Growing, fill = Above_Average),
    stat = "identity",
    width = 0.7
  ) +
  # Reference line for average
  geom_hline(
    yintercept = avg_growing,
    color = "#2C3E50",
    linetype = "dashed",
    size = 1,
    alpha = 0.7
  ) +
  # Label for average line
  annotate(
    "text",
    x = length(unique(diverging_data$Continent)) - 0.5,
    y = avg_growing + 2,
    label = paste0("Global Average: ", round(avg_growing, 1), "%"),
    color = "#2C3E50",
    fontface = "bold",
    size = 3.5
  ) +
  scale_fill_manual(
    values = diverging_colors,
    labels = c("Below Average", "Above Average"),
    name = "Compared to Global Average"
  ) +
  labs(
    title = "CONTINENTS VS GLOBAL AVERAGE",
    subtitle = "Shows which continents have above/below average growth percentages\nRed-Blue gradient indicates performance",
    x = "Continent",
    y = "% of Countries Growing",
    caption = "Story: Continental performance compared to global average"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D", margin = margin(b=10)),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray95"),
    panel.grid.minor = element_blank()
  ) +
  # Add value labels
  geom_text(
    aes(y = Percent_Growing, label = paste0(round(Percent_Growing, 1), "%")),
    vjust = -0.5,
    color = "#2C3E50",
    fontface = "bold",
    size = 4
  )

print(diverging_bar)
ggsave("diverging_bar_gradient.png", diverging_bar, width = 10, height = 7)




  
  # Shows the balance/imbalance within each continent
  
  # Calculate balance scores
  story2_data <- population_data %>%
  group_by(Continent) %>%
  summarise(
    Growing = sum(Growth_Status == "Growing"),
    Declining = sum(Growth_Status == "Declining"),
    Total = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Balance_Ratio = Growing / Declining,
    Balance_Status = case_when(
      Balance_Ratio > 1.5 ~ "Strongly Growing",
      Balance_Ratio > 1 ~ "Moderately Growing",
      Balance_Ratio == 1 ~ "Perfectly Balanced",
      Balance_Ratio > 0.5 ~ "Moderately Declining",
      TRUE ~ "Strongly Declining"
    )
  )

# Create a diverging color gradient from green (growing) to red (declining)
balance_colors <- c("Strongly Growing" = "#1B5E20",
                    "Moderately Growing" = "#4CAF50",
                    "Perfectly Balanced" = "#FFC107",
                    "Moderately Declining" = "#F44336",
                    "Strongly Declining" = "#B71C1C")

story2_plot <- ggplot(story2_data, 
                      aes(x = reorder(Continent, Balance_Ratio))) +
  
  # Growing side (positive)
  geom_col(aes(y = Growing), fill = "#4CAF50", alpha = 0.8, width = 0.6) +
  
  # Declining side (negative, shown as mirror)
  geom_col(aes(y = -Declining), fill = "#F44336", alpha = 0.8, width = 0.6) +
  
  # Zero line
  geom_hline(yintercept = 0, color = "#2C3E50", size = 1) +
  
  # Balance indicator points
  geom_point(aes(y = (Growing - Declining), 
                 color = Balance_Status, 
                 size = abs(Growing - Declining)),
             show.legend = FALSE) +
  
  scale_color_manual(values = balance_colors) +
  scale_size_continuous(range = c(5, 15)) +
  
  # Labels for growing side
  geom_text(
    aes(y = Growing, label = paste0("↑", Growing)),
    vjust = -0.5,
    color = "#1B5E20",
    fontface = "bold",
    size = 4
  ) +
  
  # Labels for declining side
  geom_text(
    aes(y = -Declining, label = paste0("↓", Declining)),
    vjust = 1.5,
    color = "#B71C1C",
    fontface = "bold",
    size = 4
  ) +
  
  # Balance ratio labels
  geom_text(
    aes(y = (Growing - Declining)/2, 
        label = paste0("Ratio: ", round(Balance_Ratio, 2))),
    vjust = -0.5,
    color = "#2C3E50",
    fontface = "bold",
    size = 3.5
  ) +
  
  # Titles and labels
  labs(
    title = "GROWTH-DECLINE CONTINENTAL BALANCE",
    subtitle = "Positive values (green) = Growing countries\nNegative values (red) = Declining countries\nSize of dots indicates imbalance magnitude",
    x = "Continent (Ordered by Growth:Decline Ratio)",
    y = "Number of Countries\n(Growing ↑ / Declining ↓)",
    color = "Balance Status",
    caption = "Story: Shows growth-decline balance/imbalance within each continent"
  ) +
  
  # Custom y-axis labels
  scale_y_continuous(
    breaks = seq(-50, 50, by = 10),
    labels = function(x) ifelse(x < 0, paste0("↓", abs(x)), 
                                ifelse(x > 0, paste0("↑", x), "0"))
  ) +
  
  # Theme
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D", margin = margin(b=15)),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  coord_flip()  # Flip for better readability

print(story2_plot)
ggsave("story2_growth_balance_gradient.png", story2_plot, width = 12, height = 8, dpi = 300)

