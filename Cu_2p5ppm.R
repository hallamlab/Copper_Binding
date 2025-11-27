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
  Supernatant = c(1.007, 2.174, 0.941, 1.634, 1.275, 0.832),
  Pellet      = c(1.097, 1.450, 1.059, 0.985, 1.052, 1.011)
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
    title = "2.5 ppm Cu²⁺ Incubation",
    y = "Percentage",
    x = ""
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
