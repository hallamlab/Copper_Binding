# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# 1. Create the data frame from the image
df <- data.frame(
  Protein = c("CB2A", "RsaA", "Cu_BP", "Azurin"),
  Supernatant = c(0.827, 0.060, 0.110, 0.371),
  SD_Supernatant = c(0.452, 0.291, 0.190, 0.353),
  Pellet = c(2.22, 0.452, 0.443, 1.81),
  SD_Pellet = c(0.661, 0.387, 0.077, 0.463)
)

# 2. Reshape and calculate cumulative positions for error bars
df_long_stacked <- df %>%
  pivot_longer(
    cols = c(Supernatant, Pellet),
    names_to = "Fraction",
    values_to = "Mean_Value"
  ) %>%
  mutate(
    SD_Value = case_when(
      Fraction == "Supernatant" ~ df$SD_Supernatant[match(Protein, df$Protein)],
      Fraction == "Pellet" ~ df$SD_Pellet[match(Protein, df$Protein)]
    )
  ) %>%
  # Crucial step for stacking: Calculate cumulative sum
  arrange(Protein, desc(Fraction)) %>% 
  group_by(Protein) %>%
  mutate(cum_y = cumsum(Mean_Value)) %>%
  # *** NEW: Explicitly set the factor levels for the X-axis order ***
  mutate(Protein = factor(Protein, levels = c("CB2A", "RsaA", "Cu_BP", "Azurin")))


# 3. Create the stacked bar plot with error bars
ggplot(df_long_stacked, aes(x = Protein, y = Mean_Value, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack", width = 0.45) + 
  geom_errorbar(
    aes(
      ymin = cum_y - SD_Value,
      ymax = cum_y + SD_Value
    ),
    width = 0.1, 
    color = "black"
  ) +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black") # This adds the x and y axis lines
  ) +
  labs(
    title = "Cu²⁺ incubation with 10 ppm, Proteinase K-treated, n=3",
    y = "Cu²⁺ ppm",
    x = ""
  ) +
  # Use scale_fill_manual to define specific colors
  scale_fill_manual(values = c("Pellet" = "#ED7D31", "Supernatant" = "#4472C4"))
 
