library(tidyverse)

# Read raw CSV exactly as laid out
df_raw <- read.csv("Cu_1_100ppm_06Dec2025.csv",
                   header = FALSE, stringsAsFactors = FALSE)

# ---------------------------------------------------
# Extract axis labels
# ---------------------------------------------------
x_axis_title <- df_raw[2, 1]     # A2
y_axis_title <- df_raw[1, 2]     # B1

# ---------------------------------------------------
# Extract X values (A3:A9)
# ---------------------------------------------------
x_vals <- as.numeric(df_raw[3:9, 1])

# ---------------------------------------------------
# Extract Y values (B3:M9)
# Column names for B–M come from row 2
# ---------------------------------------------------
y_vals <- df_raw[3:9, 2:13]
colnames(y_vals) <- df_raw[2, 2:13]

# Build tidy data frame
df_long <- cbind(x_vals, y_vals) %>%
  pivot_longer(cols = -x_vals,
               names_to = "Clones",
               values_to = "Cu_measured_ppm") %>%
  mutate(
    Cu_measured_ppm = as.numeric(Cu_measured_ppm),
    Clones = factor(Clones)
  )

# ---------------------------------------------------
# Plot: STRAIGHT LINES + DIFFERENT MARKERS
# ---------------------------------------------------
p3 <- ggplot(df_long, aes(x = x_vals,
                    y = Cu_measured_ppm,
                    color = Clones,
                    shape = Clones)) +
  geom_point(size = 2.5, stroke = 1.1) +
  geom_line(linewidth = 1) +
  scale_shape_manual(values = 0:25) +   # many marker types
  labs(
    title = "Cu2+ Binding in CB2A Clones",
    x = x_axis_title,
    y = y_axis_title,
    color = "Clones",
    shape = "Clones"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "right"
  )

print (p3)

ggsave("Cu_1_100ppm_ScatteredLines.png", p3, width = 15, height = 8, dpi = 300)
