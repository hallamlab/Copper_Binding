library(dplyr)
library(tidyr)
library(ggplot2)

# Load data
df <- read.csv("Cu_10ppm_Heat_CtrlCorr_13Jan2026.csv")

# Convert to long format

df_long <- df %>%
  pivot_longer(
    cols = c(Supernatant, Pellet),
    names_to = "Fraction",
    values_to = "Value"
  ) %>%
  mutate(Value = as.numeric(Value))

# Set order of columns

df_long$Strain <- factor(
  df_long$Strain,
  levels = c(
    "CB2A_Ctrl", "CB2A",
    "RsaA_Ctrl", "RsaA",
    "LanM_Ctrl", "LanM",
    "LBT_Ctrl", "LBT",
    "CuBP_Ctrl", "CuBP",
    "Azuron_Ctrl", "Azurin"
  )
)

# Set stack order: Supernatant bottom, Pellet top
df_long$Fraction <- factor(
  df_long$Fraction,
  levels = c("Pellet", "Supernatant")
)

# Plot
p <- ggplot(df_long, aes(x = Strain, y = Value, fill = Fraction)) +
  geom_col(width = 0.55) +
  scale_fill_manual(
    values = c(
      "Supernatant" = "#4472C4",
      "Pellet" = "#ED7D31"      
    )
  ) +
  scale_y_continuous(
    limits = c(-1, 4),
    breaks = seq(0, 6, 1),
    expand = c(0, 0)
  ) +
  labs(
    title = "10 ppm Cu²⁺, Ctrl-corrected, cells boiled",
    x = NULL,
    y = "Cu²⁺ (ppm)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line.y = element_line(color = "black"),
    axis.line.x = element_line(color = "black"),
    panel.grid.major.y = element_line(color = "grey85"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5)
  )

# Show plot
p

ggsave("Cu_10ppm_CellsBoiled.png", p, width = 10, height = 6, dpi = 300)
