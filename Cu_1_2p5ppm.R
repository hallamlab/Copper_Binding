library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------
# 2.5 ppm dataset
# ------------------------------
df_2.5 <- data.frame(
  Strain = c(
    "CB2A",
    "CB2A_p4A_RsaA",
    "CB2A_p4A_RsaA_LanM",
    "CB2A_p4A_RsaA_LBT",
    "CB2A_p4A_RsaA_CuBP",
    "CB2A_p4A_RsaA_Azurin"
  ),
  Supernatant = c(1.007, 2.174, 0.941, 1.634, 1.275, 0.832),
  Pellet      = c(1.097, 1.450, 1.059, 0.985, 1.052, 1.011),
  Condition   = "2.5 ppm"
)

# ------------------------------
# 1 ppm dataset
# ------------------------------
df_1 <- data.frame(
  Strain = c(
    "CB2A",
    "CB2A_p4A_RsaA",
    "CB2A_p4A_RsaA_LanM",
    "CB2A_p4A_RsaA_LBT",
    "CB2A_p4A_RsaA_CuBP",
    "CB2A_p4A_RsaA_Azurin"
  ),
  Supernatant = c(0.95, 1.262, 0.924, 0.955, 0.994, 1.040),
  Pellet      = c(-0.127, -0.083, 0.006, 0.000, 0.115, 0.081),
  Condition   = "1 ppm"
)

# ------------------------------
# Combine datasets
# ------------------------------
df_all <- bind_rows(df_2.5, df_1)

# ------------------------------
# Convert to long format + normalize
# ------------------------------
df_long <- df_all %>%
  pivot_longer(
    cols = c(Supernatant, Pellet),
    names_to = "Fraction",
    values_to = "Value"
  ) %>%
  group_by(Strain, Condition) %>%
  mutate(Percent = Value / sum(Value))

# ------------------------------
# Reorder facets so 2.5 ppm is on top
# ------------------------------
df_long$Condition <- factor(df_long$Condition, levels = c("2.5 ppm", "1 ppm"))

# ------------------------------
# Plot
# ------------------------------
p <- ggplot(df_long, aes(x = Strain, y = Percent, fill = Fraction)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ Condition, ncol = 1, scales = "free_y") +
  labs(
    title = "Cu²⁺ Incubation Comparison (1 ppm, 2.5 ppm)",
    x = "",
    y = "Percentage"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 14, face = "bold")
  )

# ------------------------------
# Export PNG at 300 dpi
# ------------------------------
ggsave("Cu_incubation_faceted.png", p, width = 10, height = 8, dpi = 300)
