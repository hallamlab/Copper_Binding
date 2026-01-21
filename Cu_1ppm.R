# ------------------------------
# Data
# ------------------------------
df <- data.frame(
  Strain = c(
    "CB2A",
    "CB2A_p4A_RsaA",
    "CB2A_p4A_RsaA_LanM",
    "CB2A_p4A_RsaA_LBT",
    "CB2A_p4A_RsaA_CuBP",
    "CB2A_p4A_RsaA_Azurin"
  ),
  Supernatant = c(0.95, 1.262, 0.924, 0.955, 0.994, 1.04),
  Pellet      = c(-0.127, -0.083, 0.006, 0, 0.115, 0.081)
)

# ------------------------------
# Convert to tidy format
# ------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

df_long <- df %>%
  pivot_longer(cols = c(Supernatant, Pellet),
               names_to = "Fraction",
               values_to = "Value")

# ------------------------------
# Normalize to % stacked bars
# ------------------------------
df_long <- df_long %>%
  group_by(Strain) %>%
  mutate(Percent = Value / sum(Value))

# ------------------------------
# Plot
# ------------------------------
ggplot(df_long, aes(x = Strain, y = Percent, fill = Fraction)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "1 ppm Cu²⁺ Incubation",
    y = "Percentage",
    x = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
P
