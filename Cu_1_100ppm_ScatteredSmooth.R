library(tidyverse)

# Read CSV
df_raw <- read.csv("Cu_1_100ppm_06Dec2025.csv", header = FALSE, stringsAsFactors = FALSE)

# ------------------------------
# Extract headings / axes labels
# ------------------------------
x_axis_title <- df_raw[2, 1]      # A2
y_axis_title <- df_raw[1, 2]      # B1

# ------------------------------
# Extract X-axis values (A3:A9)
# ------------------------------
x_vals <- df_raw[3:9, 1]          # A3:A9
x_vals <- as.numeric(x_vals)

# ------------------------------
# Extract Y-axis data (B3:M9)
# ------------------------------
y_vals <- df_raw[3:9, 2:13]       # B3:M9
colnames(y_vals) <- df_raw[2, 2:13]   # row 2 gives column names for each Clones

# Combine into tidy format
df_long <- cbind(x_vals, y_vals) %>%
  pivot_longer(cols = -x_vals,
               names_to = "Clones",
               values_to = "Cu_measured_ppm")

# Convert types
df_long$x_vals <- as.numeric(df_long$x_vals)
df_long$Cu_measured_ppm <- as.numeric(df_long$Cu_measured_ppm)

# ------------------------------
# Plot: Scatter + LOESS Smoothing
# ------------------------------
ggplot(df_long, aes(x = x_vals, 
                    y = Cu_measured_ppm,
                    color = Clones)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  labs(
    title = "Cu2+ Binding in CB2A Clones",
    x = x_axis_title,
    y = y_axis_title,
    color = "Clones"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ggsave("Cu_1_100ppm_ScatteredSmooth.png", p, width = 10, height = 8, dpi = 300)
