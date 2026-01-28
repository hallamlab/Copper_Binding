library(tidyverse)

# Load raw CSV
df_raw <- read.csv("Cu_1_100ppm_06Dec2025.csv",
                   header = FALSE,
                   stringsAsFactors = FALSE,
                   check.names = FALSE)

# Remove fully empty columns
df_raw <- df_raw[, colSums(!is.na(df_raw) & df_raw != "") > 0]

# Extract labels
x_axis_title <- as.character(df_raw[2, 1])     # A2
x_values     <- as.character(df_raw[3:9, 1])   # A3:A9

y_axis_title <- as.character(df_raw[1, 2])     # B1

subgroups <- as.character(unlist(df_raw[2, 2:ncol(df_raw)]))

# Extract numeric block B3:M9
value_matrix <- df_raw[3:9, 2:ncol(df_raw)]
value_matrix <- as.data.frame(lapply(value_matrix, as.numeric))

# Build long dataframe
df <- data.frame(X = x_values, value_matrix)
colnames(df) <- c(x_axis_title, subgroups)

df_long <- df %>%
  pivot_longer(
    cols = all_of(subgroups),
    names_to = "Group",
    values_to = "Value"
  )

# Use tidy-eval (!!sym()) so labels can contain symbols safely
ggplot(df_long, aes(x = !!sym(x_axis_title), y = Value, color = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = x_axis_title,
    y = y_axis_title,
    color = "Subgroup"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
