library(europepmc)
library(tidyverse)
library(viridis)

# Fetching trend data for different diets using europepmc package
trend_veg <- europepmc::epmc_hits_trend(query = "vegetarian diet",
                                        period = 1995:2023, synonym = FALSE)
trend_keto <- europepmc::epmc_hits_trend(query = "low carbohydrate diet",
                                         period = 1995:2023, synonym = FALSE)
trend_inter <- europepmc::epmc_hits_trend(query = "intermittent fasting",
                                          period = 1995:2023, synonym = FALSE)
trend_glut <- europepmc::epmc_hits_trend(query = "gluten free diet",
                                         period = 1995:2023, synonym = FALSE)

# Combining all diet trends into a single dataframe
combined_data <- rbind(
  mutate(trend_veg, diet = "Vegetarian"),
  mutate(trend_keto, diet = "Low Carb"),
  mutate(trend_inter, diet = "Intermittent Fasting"),
  mutate(trend_glut, diet = "Gluten Free")
)

# Creating a combined plot using ggplot2
combined_plot <- ggplot(combined_data, aes(x = factor(year), y = (query_hits / all_hits * 100), fill = diet)) +
  geom_col(width = 0.6, alpha = 0.9) +  # Creating bar plots
  theme_minimal() +  # Setting a minimal theme
  labs(x = "Year", y = "% of all published articles") +  # Labeling axes
  ggtitle("Interest of scientists in studying different diets") +  # Adding a title
  ylim(0, 0.85) +  # Setting y-axis limits
  scale_fill_viridis_d() +  # Using viridis color palette for better distinction
  facet_wrap(~diet, ncol = 2) +  # Creating separate panels for each diet
  theme(
    legend.position = "none",  # Removing legend
    axis.text.x = element_text(size = 7, angle = 60, hjust = 1),  # Rotating x-axis labels
    plot.title = element_text(hjust = 0.5),  # Centering plot title
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),  # Removing major grid lines
    panel.grid.minor = element_blank()  # Removing minor grid lines
  ) +
  scale_x_discrete(labels = function(x) ifelse(seq_along(x) %% 2 == 1, x, ""))  # Showing every other year on x-axis

# Display the plot
combined_plot

# Save the plot as an SVG file
ggsave("combined_plot.svg", plot = combined_plot, device = "svg")

# Note: SVG format can be used for further editing in other software
