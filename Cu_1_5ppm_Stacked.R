library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------
# Combine datasets
# ------------------------------
df_all <- bind_rows(df_5, df_2.5, df_1)

# ------------------------------
# Convert to long format (NO NORMALIZATION)
# ------------------------------
df_long <- df_all %>%
  pivot_longer(
    cols = c(Supernatant, Pellet),
    names_to = "Fraction",
    values_to = "Value"
  )

# ------------------------------
# Reorder facets so 5 ppm is on top
# ------------------------------
df_long$Condition <- factor(df_long$Condition,
                            levels = c("5 ppm", "2.5 ppm", "1 ppm"))

# Optional: control stacking order
df_long$Fraction <- factor(df_long$Fraction,
                           levels = c("Pellet", "Supernatant"))

# ------------------------------
# Stacked bar plot (ppm scale)
# ------------------------------
p <- ggplot(df_long, aes(x = Strain, y = Value, fill = Fraction)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(
    title = "Cu²⁺ Incubation Comparison (1 ppm, 2.5 ppm, 5 ppm)",
    x = "",
    y = "Cu²⁺ (ppm)"
  ) +
  facet_wrap(~ Condition, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold")
  )

# ------------------------------
# Export PNG at 300 dpi
# ------------------------------

print (p)

ggsave("Cu_1_5ppm_noNorm_faceted.png", p, width = 10, height = 8, dpi = 300)
