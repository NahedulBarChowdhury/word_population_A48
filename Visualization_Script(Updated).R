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





# Story: "Side-by-side comparison of growing vs declining"

# Prepare data for pyramid
pyramid_data <- population_data %>%
  mutate(
    # For pyramid, we need growing on positive side, declining on negative
    Count = ifelse(Growth_Status == "Growing", 1, -1)
  ) %>%
  group_by(Continent, Growth_Status) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  mutate(
    Count = ifelse(Growth_Status == "Declining", -Count, Count)
  )

pyramid_colors <- c("Growing" = "#9B59B6", "Declining" = "#F39C12") # Purple-Orange gradient

pyramid_bar <- ggplot(pyramid_data, aes(x = Continent, y = Count, fill = Growth_Status)) +
  geom_bar(stat = "identity", position = "identity") +
  # Flip coordinates for pyramid effect
  coord_flip() +
  # Custom y-axis to show positive and negative
  scale_y_continuous(
    breaks = seq(-50, 50, by = 10),
    labels = function(x) ifelse(x < 0, paste0(abs(x), " Declining"), 
                                ifelse(x > 0, paste0(x, " Growing"), "0"))
  ) +
  scale_fill_manual(values = pyramid_colors) +
  labs(
    title = "PYRAMID COMPARISON",
    subtitle = "Growing countries on right (positive), Declining on left (negative)\nPurple-Orange gradient for visual separation",
    x = "Continent",
    y = "Number of Countries",
    fill = "Growth Status",
    caption = "Story: Direct side-by-side comparison of growing vs declining counts"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#7F8C8D", margin = margin(b=10)),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = element_line(color = "gray95"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F8F9FA", color = NA)
  ) +
  # Add value labels
  geom_text(
    aes(
      label = ifelse(Count > 0, Count, -Count),
      hjust = ifelse(Count > 0, -0.2, 1.2)
    ),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  # Center line
  geom_hline(yintercept = 0, color = "#2C3E50", size = 1)

print(pyramid_bar)
ggsave("pyramid_stacked_gradient.png", pyramid_bar, width = 10, height = 7)


