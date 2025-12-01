library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Combine all datasets ----
df_all <- bind_rows(df_50, df_25, df_10, df_5, df_2.5, df_1)

# ---- Convert to long format ----
df_long <- df_all %>%
  pivot_longer(
    cols = c(Supernatant, Pellet),
    names_to = "Fraction",
    values_to = "Value"
  )

# ---- Order conditions for faceting ----
df_long$Condition <- factor(
  df_long$Condition,
  levels = c("50 ppm", "25 ppm", "10 ppm", "5 ppm", "2.5 ppm", "1 ppm")
)

# ---- Supernatant then Pellet ----
df_long$Fraction <- factor(df_long$Fraction, levels = c("Supernatant", "Pellet"))


# ---- Plot: Stacked by strain ----
p <- ggplot(df_long, aes(x = Fraction, y = Value, fill = Strain)) +
  geom_bar(stat = "identity", width = 0.45) +
  labs(
    title = "Cu²⁺ Incubation – Stacked by Strain",
    x = "",
    y = "Cu²⁺ (ppm)"
  ) +
  facet_wrap(~ Condition, ncol = 1, scales = "free_y") +
  theme_bw(base_size = 12) +
  theme(
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold")
  )

print (p)

