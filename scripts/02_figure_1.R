# Creating my first figure for my repository

library(tidyverse)

# Load the clean master dataset
mercer_data <- read_csv("data/clean_mercer_master.csv", show_col_types = FALSE)

# Build a clear horizontal bar chart sorted from fastest to slowest
fig1 <- ggplot(mercer_data, aes(x = total_train_commute_min, y = reorder(town, -total_train_commute_min))) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = total_train_commute_min), hjust = -0.2) +
  geom_vline(aes(xintercept = mercer_median_train_min), linetype = "dashed", color = "red") +
  xlim(0, 100) +
  labs(
    title = "Door-to-Door Train Commute to Philadelphia by Town",
    subtitle = "Sorted fastest to slowest (Red dashed line = Mercer County median)",
    x = "Total One-Way Commute Time (Minutes)",
    y = "Municipality",
    caption = "Sources: SEPTA (2026), NJGIN, OpenStreetMap"
  ) +
  theme_minimal()

# View in RStudio and save to my repository
print(fig1)
dir.create("figures", showWarnings = FALSE)
ggsave("figures/fig1_train_commute_by_town.png", plot = fig1, width = 8, height = 5)