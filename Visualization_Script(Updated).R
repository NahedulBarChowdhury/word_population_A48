# Here we were creating a series of visual stories to explore how population 
# growth patterns differed across continents. We calculated averages, ratios, 
# hierarchies and distribution patterns, then used several types of plots to 
# show which continents were above or below the global trend, how balanced 
# they were between growth and decline, and how strong their growth levels 
# were overall. The goal was to break the data into clear, interpretable 
# insights through visuals.

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
  # global average reference line
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
    y = avg_growing + 2, # small offset so label sits above dashed line
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
    Balance_Ratio = Growing / Declining,  # >1 means more growing than declining
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






# Shows the hierarchy of growth dominance across continents


# Calculate growth hierarchy and remove "Other" category
story3_data <- population_data %>%
  filter(Continent != "Other") %>%  # Remove "Other" category
  group_by(Continent) %>%
  summarise(
    Total = n(),
    Growing = sum(Growth_Status == "Growing"),
    Declining = sum(Growth_Status == "Declining"),
    .groups = 'drop'
  ) %>%
  mutate(
    Growth_Ratio = Growing / (Growing + Declining),
    # Create hierarchy levels
    Hierarchy_Level = case_when(
      Growth_Ratio >= 0.8 ~ "Very High Growth",
      Growth_Ratio >= 0.6 ~ "High Growth",
      Growth_Ratio >= 0.4 ~ "Moderate Growth",
      Growth_Ratio >= 0.2 ~ "Low Growth",
      TRUE ~ "Very Low Growth"
    )
  ) %>%
  arrange(desc(Growth_Ratio))

print("Filtered Data (without 'Other' category):")
print(story3_data)

# Create a vibrant rainbow gradient for the hierarchy levels
hierarchy_colors <- c(
  "Very High Growth" = "#FF0000",    # Red (hottest)
  "High Growth" = "#FF8000",         # Orange
  "Moderate Growth" = "#FFFF00",     # Yellow
  "Low Growth" = "#00FF00",          # Green
  "Very Low Growth" = "#0000FF"      # Blue (coldest)
)


story3_plot <- ggplot(story3_data, 
                      aes(x = reorder(Continent, -Growth_Ratio), 
                          y = Total)) +
  
  # Background bar (total countries) - use subtle gradient
  geom_col(fill = "#F0F0F0", alpha = 0.3, width = 0.7) +
  
  # Growing countries (stacked on top) with vibrant gradient
  geom_col(aes(y = Growing, fill = Hierarchy_Level), 
           width = 0.7, alpha = 0.9) +
  
  # Add subtle gradient overlay for more vibrant effect
  geom_col(aes(y = Growing), 
           fill = "white", 
           alpha = 0.1, 
           width = 0.7) +
  
  # VIBRANT RAINBOW GRADIENT SCALE
  scale_fill_manual(
    values = hierarchy_colors,
    name = "Growth Level",
    guide = guide_legend(reverse = TRUE)  # Reverse legend to match bar order
  ) +
  
  # Add labels for growing countries - bold black for contrast
  geom_text(
    aes(y = Growing, label = paste0(Growing, "/", Total)),
    vjust = -0.5,
    color = "black",
    fontface = "bold",
    size = 4.5
  ) +
  
  # Add percentage labels inside bars - white for readability
  geom_text(
    aes(y = Growing/2, 
        label = paste0(round(Growth_Ratio * 100, 0), "%")),
    color = "white",
    fontface = "bold",
    size = 5,
    alpha = 0.9
  ) +
  
  # Titles and labels
  labs(
    title = "CONTINENTAL GROWTH HIERARCHY",
    subtitle = "Vibrant rainbow gradient shows growth intensity across continents\nRed (Very High) → Orange → Yellow → Green → Blue (Very Low)",
    x = "Continent (Ranked by Growth Percentage)",
    y = "Number of Countries",
    caption = "Data Analysis: Population Growth Status by Continent"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold", 
      size = 20, 
      hjust = 0.5, 
      color = "#333333",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 13, 
      hjust = 0.5, 
      color = "#666666", 
      margin = margin(b = 20)
    ),
    axis.title = element_text(
      face = "bold", 
      size = 13, 
      color = "#333333"
    ),
    axis.text.x = element_text(
      angle = 45, 
      hjust = 1, 
      vjust = 1, 
      face = "bold", 
      size = 12, 
      color = "#333333"
    ),
    axis.text.y = element_text(
      size = 11, 
      color = "#333333"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold", 
      size = 12, 
      color = "#333333"
    ),
    legend.text = element_text(
      size = 11, 
      color = "#333333"
    ),
    legend.key.size = unit(1, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "#E0E0E0", size = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  
  # Add subtle glow effect to bars
  geom_col(
    aes(y = Growing),
    fill = NA,
    color = "white",
    size = 1,
    width = 0.71,
    alpha = 0.3
  ) +
  
  # Ensure y-axis shows proper range
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    breaks = seq(0, 60, by = 10),
    limits = c(0, max(story3_data$Total) * 1.15)
  )

print(story3_plot)
ggsave("vibrant_growth_hierarchy_gradient.png", story3_plot, width = 12, height = 8, dpi = 300)


# Shows different distribution patterns across continents

# First, let's create population categories (but we won't use population data)
# We'll just use the counts to create interesting patterns

story4_data <- population_data %>%
  group_by(Continent) %>%
  summarise(
    Growing = sum(Growth_Status == "Growing"),
    Declining = sum(Growth_Status == "Declining"),
    Total = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    # Create pattern categories based on distribution
    Pattern_Type = case_when(
      Growing >= Declining * 2 ~ "Heavily Growing",
      Growing > Declining ~ "Moderately Growing",
      Growing == Declining ~ "Balanced",
      Declining > Growing ~ "Moderately Declining",
      Declining >= Growing * 2 ~ "Heavily Declining"
    )
  ) %>%
  arrange(desc(Growing))

# Create pattern gradient colors
pattern_colors <- c(
  "Heavily Growing" = "#00441B",     # Very dark green
  "Moderately Growing" = "#238B45",  # Medium green
  "Balanced" = "#FFD700",            # Gold
  "Moderately Declining" = "#CB181D", # Medium red
  "Heavily Declining" = "#67000D"    # Very dark red
)

# Create a stacked bar with pattern overlay effect
story4_plot <- ggplot(story4_data) +
  
  # Base bars (Total)
  geom_col(aes(x = reorder(Continent, -Total), y = Total),
           fill = "#F0F0F0", width = 0.7, alpha = 0.5) +
  
  # Declining countries (bottom stack)
  geom_col(aes(x = reorder(Continent, -Total), y = Declining),
           fill = "#FF6B6B", width = 0.7, alpha = 0.7) +
  
  # Growing countries (top stack)
  geom_col(aes(x = reorder(Continent, -Total), y = Growing),
           fill = "#4ECDC4", width = 0.7, alpha = 0.7,
           position = position_nudge(y = story4_data$Declining)) +
  # nudge is used to stack Growing *above* Declining manually
  
  # Pattern overlay based on Pattern_Type
  geom_tile(
    aes(x = as.numeric(reorder(Continent, -Total)),
        y = Total/2,
        width = 0.5,
        height = Total,
        fill = Pattern_Type),
    alpha = 0.2
  ) +
  
  scale_fill_manual(values = pattern_colors, name = "Distribution Pattern") +
  
  # Add connecting lines to show the split
  geom_segment(
    aes(x = as.numeric(reorder(Continent, -Total)) - 0.35,
        xend = as.numeric(reorder(Continent, -Total)) + 0.35,
        y = Declining,
        yend = Declining),
    color = "gray40",
    linetype = "dashed",
    size = 0.5
  ) +
  
  # Labels for growing
  geom_text(
    aes(x = reorder(Continent, -Total),
        y = Declining + Growing/2,
        label = paste0("G:", Growing)),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  
  # Labels for declining
  geom_text(
    aes(x = reorder(Continent, -Total),
        y = Declining/2,
        label = paste0("D:", Declining)),
    color = "white",
    fontface = "bold",
    size = 4
  ) +
  
  # Pattern type labels
  geom_text(
    aes(x = reorder(Continent, -Total),
        y = Total + max(story4_data$Total) * 0.05,
        label = Pattern_Type),
    color = "#2C3E50",
    fontface = "bold",
    size = 3.5
  ) +
  
  # Titles and labels
  labs(
    title = "GROWTH DISTRIBUTION PATTERNS",
    subtitle = "Shows different distribution patterns of growth across continents\nBlue = Growing, Red = Declining, Pattern overlay shows distribution type",
    x = "Continent (Ordered by Total Countries)",
    y = "Number of Countries",
    caption = "Story: Different patterns of growth distribution across continents"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2C3E50"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#7F8C8D", margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, face = "bold", size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)))

print(story4_plot)
ggsave("story4_growth_patterns_gradient.png", story4_plot, width = 11, height = 8, dpi = 300)


