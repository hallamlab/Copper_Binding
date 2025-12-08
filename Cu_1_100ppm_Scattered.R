library(tidyverse)

# ---- Load CSV ----
df_raw <- read.csv("Cu_1_100ppm_06Dec2025.csv",
                   header = FALSE,
                   stringsAsFactors = FALSE,
                   check.names = FALSE)

# Remove empty columns
df_raw <- df_raw[, colSums(!is.na(df_raw) & df_raw != "") > 0]

# ---- Extract metadata ----
x_axis_title <- as.character(df_raw[2, 1])     # A2
x_values     <- as.character(df_raw[3:9, 1])   # A3:A9
y_axis_title <- as.character(df_raw[1, 2])     # B1

subgroups <- as.character(unlist(df_raw[2, 2:ncol(df_raw)]))

# CLEAN subgroup names to safe R names (but keep readable labels)
safe_subgroups <- make.names(subgroups, unique = TRUE)

# Extract and convert numeric block
value_matrix <- df_raw[3:9, 2:ncol(df_raw)]
value_matrix <- as.data.frame(lapply(value_matrix, as.numeric))
colnames(value_matrix) <- safe_subgroups

# Build full data frame
df <- cbind(data.frame(X = x_values), value_matrix)
colnames(df)[1] <- "X"

# Long format
df_long <- df %>%
  pivot_longer(
    cols = all_of(safe_subgroups),
    names_to = "SafeGroup",
    values_to = "Value"
  ) %>%
  mutate(Group = subgroups[match(SafeGroup, safe_subgroups)])

# ---- Plot ----
ggplot(df_long, aes(x = X, y = Value, color = Group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    x = x_axis_title,
    y = y_axis_title,
    color = "Subgroup"
  ) +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
