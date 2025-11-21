library(ggplot2)
library(dplyr)
library(tidyr)


#PLOT SENSITIVITY
df_st <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(3.0, 3.05, 3.0, 1.75, 2.2),
  RMSE_AR = c(2.9, 3.2, 3.1, 2.0, 2.3),
  RMSE_Naive = c(3.0, 3.1, 3.0, 2.11, 2.5),
  MAE_VAR = c(2.4, 2.5, 2.3, 1.4, 1.6),
  MAE_AR = c(2.3, 2.5, 2.3, 1.55, 1.8),
  MAE_Naive = c(2.50, 2.40, 2.3, 1.65, 1.87)
)

#PLOT X2
PLOT_SMOOTHING_SENSITIVITY_x2(df_st)


#PLOT SENSITIVITY
plots_sens <- PLOT_SMOOTHING_SENSITIVITY(df_st)

#RMSE
plots_sens$RMSE
#MAE
plots_sens$MAE



#******************
df <- tribble(
  ~Jurisdiction, ~VAR_RMSE, ~Naive_RMSE, ~Improve_RMSE, ~VAR_MAPE, ~Naive_MAPE, ~Improve_MAPE, ~VAR_Bias, ~Naive_Bias, ~Improve_Bias,
  "New York City", 1.30, 1.74, 25.57, 1.13, 1.54, 26.83,  0.28, -1.22, 122.86,
  "Texas", 2.00, 2.30, 13.03, 1.80, 1.89, 4.67, -0.20, -0.42, 52.03,
  "L.A. county", 1.93, 2.65, 27.19, 1.68, 2.18, 23.27, -0.56, -1.33, 58.04,
  "Florida", 2.49, 1.66, -50.33, 2.00, 1.32, -51.28, -0.85, -0.43, -96.98,
  "Illinois", 2.26, 2.81, 19.59, 2.10, 2.42, 13.22, -1.50, -0.69, -116.02,
  "Georgia", 1.31, 1.17, -11.58, 0.83, 0.99, 16.35, -0.40, -0.60, 33.78,
  "San Diego county", 1.37, 1.50, 9.04, 1.13, 1.11, -1.08, -0.16, -0.60, 72.74,
  "Washington state", 1.11, 1.85, 40.06, 0.84, 1.16, 27.66, -0.54, -1.01, 46.73
)

plot_metric_bars <- function(metric = "RMSE") {
  
  metric_cols <- c(
    VAR = paste0("VAR_", metric),
    Naive = paste0("Naive_", metric),
    Improve = paste0("Improve_", metric)
  )
  
  df_long <- df %>%
    select(Jurisdiction, all_of(metric_cols)) %>%
    pivot_longer(-Jurisdiction, names_to = "Model", values_to = "Value") %>%
    mutate(Model = recode(Model,
                          !!metric_cols["VAR"] := "VAR",
                          !!metric_cols["Naive"] := "Naive",
                          !!metric_cols["Improve"] := "% Improve"))
  
  ggplot(df_long, aes(x = Model, y = Value, fill = Model)) +
    geom_col(position = "dodge", width = 0.7) +
    facet_wrap(~ Jurisdiction, nrow = 2, ncol = 4, scales = "free_y") +
    scale_fill_manual(values = c("Naive" = "red", "VAR" = "blue", "% Improve" = "black")) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "top",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(
      y = metric,
      fill = "Model",
      title = paste0("Naive vs VAR vs % Improvement (", metric, ")")
    )
}




# Your data ----
df <- tribble(
  ~Jurisdiction, ~VAR_RMSE, ~Naive_RMSE, ~Improve_RMSE, ~VAR_MAPE, ~Naive_MAPE, ~Improve_MAPE, ~VAR_Bias, ~Naive_Bias, ~Improve_Bias,
  "New York City", 1.30, 1.74, 25.57, 1.13, 1.54, 26.83,  0.28, -1.22, 122.86,
  "Texas", 2.00, 2.30, 13.03, 1.80, 1.89, 4.67, -0.20, -0.42, 52.03,
  "L.A. county", 1.93, 2.65, 27.19, 1.68, 2.18, 23.27, -0.56, -1.33, 58.04,
  "Florida", 2.49, 1.66, -50.33, 2.00, 1.32, -51.28, -0.85, -0.43, -96.98,
  "Illinois", 2.26, 2.81, 19.59, 2.10, 2.42, 13.22, -1.50, -0.69, -116.02,
  "Georgia", 1.31, 1.17, -11.58, 0.83, 0.99, 16.35, -0.40, -0.60, 33.78,
  "San Diego county", 1.37, 1.50, 9.04, 1.13, 1.11, -1.08, -0.16, -0.60, 72.74,
  "Washington state", 1.11, 1.85, 40.06, 0.84, 1.16, 27.66, -0.54, -1.01, 46.73
)

# Function ----
plot_metric_bars <- function(metric = "RMSE") {
  
  metric_cols <- c(
    VAR = paste0("VAR_", metric),
    Naive = paste0("Naive_", metric),
    Improve = paste0("Improve_", metric)
  )
  
  df_long <- df %>%
    select(Jurisdiction, all_of(metric_cols)) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = unique(df$Jurisdiction))) %>%  # keep order!
    pivot_longer(-Jurisdiction, names_to = "Model", values_to = "Value") %>%
    mutate(Model = recode(Model,
                          !!metric_cols["VAR"] := "VAR",
                          !!metric_cols["Naive"] := "Naive",
                          !!metric_cols["Improve"] := "% Improve"))
  
  ggplot(df_long, aes(x = Model, y = Value, fill = Model)) +
    geom_col(width = 0.7) +
    facet_wrap(~ Jurisdiction, nrow = 2, ncol = 4, scales = "free_y") +
    scale_fill_manual(values = c("Naive" = "red", "VAR" = "blue", "% Improve" = "black")) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "top",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    ) +
    labs(
      y = metric,
      fill = "Model",
      title = paste0("Naive vs VAR vs % Improvement (", metric, ")")
    )
}


plot_metric_bars("RMSE")
plot_metric_bars("MAPE")
plot_metric_bars("Bias")


#******************************
#PLOT FOR PRESENTATION
library(dplyr)

# Create the summary df directly
df_summary <- data.frame(
  Predictor = c(
    "Illinois_L24", "Illinois_L23", "LA_L12", 
    "NYC_L13", "WA_L3"
  ),
  avg_coefficient = c(0.43, 0.235, 0.13, 0.12, 0.10)
)

# Clean up predictor names for x-axis
df_summary <- df_summary %>%
  mutate(
    Predictor = case_when(
      grepl("Illinois_L24", Predictor) ~ "Illinois - Lag 24 weeks",
      grepl("Illinois_L23", Predictor) ~ "Illinois - Lag 23 weeks",
      grepl("LA_L12", Predictor) ~ "LA - Lag 12 weeks",
      grepl("NYC_L13", Predictor) ~ "NYC - Lag 13 weeks",
      grepl("WA_L3", Predictor) ~ "Washington state - Lag 3 weeks",
      TRUE ~ Predictor
    ),
    Predictor = factor(Predictor, levels = Predictor[order(-avg_coefficient)])
  )

df_summary

ggplot(df_summary, aes(x = Predictor, y = avg_coefficient, fill = avg_coefficient)) +
  geom_col() +
  scale_fill_gradientn(
    colors = c("#ffffb2", "#fecc5c", "#fd8d3c", "#f03b20", "#bd0026")
  ) +
  labs(
    title = "Top 5 Coefficients for San Diego county, 2024 forecasts",
    x = "Coefficient (Lag Weeks)",
    y = "Average Coefficient",
    fill = "Avg Coefficient"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

#******************************************************
#* PLOTS

cols <- c(
  "LA" = "#C6C648",         # LA County 
  "Illinois" = "#47B7D6",   # Illinois 
  "NewYorkCity" = "#D55ACB",# New York City 
  "Washington" = "#C44E76"  # Washington State 
)


df_summary <- df_summary %>%
  mutate(
    Predictor = case_when(
      grepl("Illinois_L24", Predictor) ~ "Illinois - Lag 24 weeks",
      grepl("Illinois_L23", Predictor) ~ "Illinois - Lag 23 weeks",
      grepl("LA_L12", Predictor) ~ "LA County - Lag 12 weeks",
      grepl("NYC_L13", Predictor) ~ "New York City - Lag 13 weeks",
      grepl("WA_L3", Predictor) ~ "Washington - Lag 3 weeks",
      TRUE ~ Predictor
    ),
    Location = case_when(  
      grepl("Illinois", Predictor) ~ "Illinois",
      grepl("LA", Predictor) ~ "LA",  # <- fixed: matches “LA County”
      grepl("New York City|NYC", Predictor) ~ "NewYorkCity",
      grepl("Washington", Predictor) ~ "Washington",
      TRUE ~ NA_character_
    ),
    Predictor = factor(Predictor, levels = Predictor[order(-avg_coefficient)])
  )

ggplot(df_summary, aes(x = Predictor, y = avg_coefficient, fill = Location)) +
  geom_col() +
  scale_fill_manual(
    values = cols,
    breaks = c("Illinois", "LA", "NewYorkCity", "Washington"),
    labels = c("Illinois", "LA County", "New York City", "Washington")
  ) +
  labs(
    title = "Top 5 Coefficients for San Diego County, 2024 Forecasts",
    x = "Coefficient (Lag Weeks)",
    y = "Average Coefficient",
    fill = "Location"
  ) +
  theme_minimal(base_size = 20) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))


#**************
#* SENSITIVITY ANALYSIS
#* 
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Create data
df <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(4.85, 3.80, 3.00, 1.84, 2.00),
  RMSE_Naive = c(3.29, 3.10, 2.90, 2.08, 2.34),
  MAE_VAR = c(2.90, 2.82, 2.35, 1.33, 1.52),
  MAE_Naive = c(2.50, 2.40, 2.23, 1.39, 1.87)
)

# Convert to long format for ggplot
df_long <- df %>%
  pivot_longer(cols = -Smoothing,
               names_to = c("Metric", "Model"),
               names_sep = "_",
               values_to = "Value")

# Order x-axis properly
df_long$Smoothing <- factor(df_long$Smoothing,
                            levels = c("No smoothing", "2", "3", "4", "5"))

# Custom colors
cols <- c("VAR" = "blue", "Naïve" = "red")

# Plot 1: RMSE
p_rmse <- df_long %>%
  filter(Metric == "RMSE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "Positive slope-weighted RMSE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 11),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = cols)

# Plot 2: MAE
p_mae <- df_long %>%
  filter(Metric == "MAE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(y = "Positive slope-weighted MAE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  ) +
  scale_color_manual(values = cols)

# Combine plots side by side
p_rmse + p_mae +
  plot_annotation(tag_levels = 'A')


#*************************************
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# Create data
df <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(2.9, 3.9, 3.0, 1.75, 2.2),
  RMSE_AR = c(2.8, 3.2, 3.1, 2.0, 2.3),
  RMSE_Naive = c(3.0, 3.1, 3.0, 2.11, 2.5),
  MAE_VAR = c(2.4, 2.9, 2.3, 1.4, 1.6),
  MAE_AR = c(2.3, 2.5, 2.3, 1.5, 1.8),
  MAE_Naive = c(2.50, 2.40, 2.3, 1.65, 1.87)
)

# df <- data.frame(
#   Smoothing = c("No smoothing", "2", "3", "4", "5"),
#   RMSE_VAR = c(4.85, 3.80, 3.00, 1.84, 2.00),
#   RMSE_Naive = c(3.29, 3.10, 2.90, 2.08, 2.34),
#   MAE_VAR = c(2.90, 2.82, 2.35, 1.33, 1.52),
#   MAE_Naive = c(2.50, 2.40, 2.23, 1.39, 1.87)
# )

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = -Smoothing,
               names_to = c("Metric", "Model"),
               names_sep = "_",
               values_to = "Value") %>%
  mutate(Model = ifelse(Model == "Naive", "Naive", "VAR"))  # standardize names

# Order x-axis properly
df_long$Smoothing <- factor(df_long$Smoothing,
                            levels = c("No smoothing", "2", "3", "4", "5"))

# Custom colors
cols <- c("VAR" = "blue", "Naive" = "red")

# Identify the highlight point
highlight <- df_long %>%
  filter(Model == "VAR", Smoothing == "4")

# Plot 1: RMSE with highlight
p_rmse <- df_long %>%
  filter(Metric == "RMSE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  # highlight VAR at window 4
  geom_point(data = highlight, aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5) +
  labs(y = "Positive slope-weighted RMSE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 11),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = cols)

# Plot 2: MAE with highlight
highlight_mae <- df_long %>%
  filter(Model == "VAR", Smoothing == "4", Metric == "MAE")

p_mae <- df_long %>%
  filter(Metric == "MAE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = highlight_mae, aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5) +
  labs(y = "Positive slope-weighted MAE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  ) +
  scale_color_manual(values = cols)

# Combine side by side
p_rmse + p_mae + plot_annotation(tag_levels = 'A')

###########

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# -----------------------------
# Create data
# -----------------------------

df <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(2.9, 3.9, 3.0, 1.75, 2.2),
  RMSE_AR = c(2.8, 3.2, 3.1, 2.0, 2.3),
  RMSE_Naive = c(3.0, 3.1, 3.0, 2.11, 2.5),
  MAE_VAR = c(2.4, 2.9, 2.3, 1.4, 1.6),
  MAE_AR = c(2.3, 2.5, 2.3, 1.5, 1.8),
  MAE_Naive = c(2.50, 2.40, 2.3, 1.65, 1.87)
)

# df <- data.frame(
#   Smoothing = c("No smoothing", "2", "3", "4", "5"),
#   RMSE_VAR = c(4.85, 3.80, 3.00, 1.84, 2.00),
#   RMSE_Naive = c(3.29, 3.10, 2.90, 2.08, 2.34),
#   MAE_VAR = c(2.90, 2.82, 2.35, 1.33, 1.52),
#   MAE_Naive = c(2.50, 2.40, 2.23, 1.39, 1.87)
# )

# -----------------------------
# Convert to long format
# -----------------------------
df_long <- df %>%
  pivot_longer(cols = -Smoothing,
               names_to = c("Metric", "Model"),
               names_sep = "_",
               values_to = "Value") %>%
  mutate(Model = ifelse(Model == "Naive", "Naive", "VAR"))  # standardize names

# Order x-axis
df_long$Smoothing <- factor(df_long$Smoothing,
                            levels = c("No smoothing", "2", "3", "4", "5"))

# -----------------------------
# Highlight VAR at window 4
# -----------------------------
highlight_rmse <- df_long %>% filter(Metric == "RMSE", Model == "VAR", Smoothing == "4")
highlight_mae  <- df_long %>% filter(Metric == "MAE", Model == "VAR", Smoothing == "4")

# -----------------------------
# Custom colors
# -----------------------------
cols <- c("VAR" = "blue", "Naive" = "red")

# -----------------------------
# Plot RMSE
# -----------------------------
p_rmse <- df_long %>%
  filter(Metric == "RMSE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = highlight_rmse, 
             aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5,
             inherit.aes = FALSE) +
  labs(y = "Positive slope-weighted RMSE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 14),  
    legend.title = element_blank()
  ) +
  scale_color_manual(values = cols)

# -----------------------------
# Plot MAE
# -----------------------------
p_mae <- df_long %>%
  filter(Metric == "MAE") %>%
  ggplot(aes(x = Smoothing, y = Value, group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = highlight_mae, 
             aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5,
             inherit.aes = FALSE) +
  labs(y = "Positive slope-weighted MAE", x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 14)  
  ) +
  scale_color_manual(values = cols)

# -----------------------------
# Combine plots side by side
# -----------------------------
p_rmse + p_mae + plot_annotation(tag_levels = 'A')


#***********************************************
#SENSITIVITY ANALYSIS WITH VAR, AR, NAIVE

library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# -----------------------------
# Create data
# -----------------------------

df <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(2.9, 3.9, 3.0, 1.75, 2.2),
  RMSE_AR = c(2.8, 3.2, 3.1, 2.0, 2.3),
  RMSE_Naive = c(3.0, 3.1, 3.0, 2.11, 2.5),
  MAE_VAR = c(2.4, 2.9, 2.3, 1.4, 1.6),
  MAE_AR = c(2.3, 2.5, 2.3, 1.5, 1.8),
  MAE_Naive = c(2.50, 2.40, 2.3, 1.65, 1.87)
)

# -----------------------------
# Convert to long format
# -----------------------------
df_long <- df %>%
  pivot_longer(
    cols = -Smoothing,
    names_to = c("Metric", "Model"),
    names_sep = "_",
    values_to = "Value"
  ) %>%
  mutate(Model = factor(Model, levels = c("VAR", "AR", "Naive")))

# Order x-axis
df_long$Smoothing <- factor(df_long$Smoothing,
                            levels = c("No smoothing", "2", "3", "4", "5"))

# -----------------------------
# Highlight VAR at window 4
# -----------------------------
highlight_rmse <- df_long %>% 
  filter(Metric == "RMSE", Model == "VAR", Smoothing == "4")

highlight_mae  <- df_long %>%
  filter(Metric == "MAE", Model == "VAR", Smoothing == "4")

# -----------------------------
# Custom colors
# -----------------------------
cols <- c("VAR" = "blue", "AR" = "darkgreen", "Naive" = "red")

# -----------------------------
# Plot RMSE
# -----------------------------
p_rmse <- df_long %>%
  filter(Metric == "RMSE") %>%
  ggplot(aes(x = Smoothing, y = Value, 
             group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = highlight_rmse, 
             aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5,
             inherit.aes = FALSE) +
  labs(y = "Positive slope-weighted RMSE", 
       x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    axis.text.x = element_text(size = 11),
    legend.text = element_text(size = 14),
    legend.title = element_blank()
  ) +
  scale_color_manual(values = cols)

# -----------------------------
# Plot MAE
# -----------------------------
p_mae <- df_long %>%
  filter(Metric == "MAE") %>%
  ggplot(aes(x = Smoothing, y = Value, 
             group = Model, color = Model, shape = Model)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_point(data = highlight_mae, 
             aes(x = Smoothing, y = Value),
             color = "green", size = 5, shape = 1, stroke = 1.5,
             inherit.aes = FALSE) +
  labs(y = "Positive slope-weighted MAE", 
       x = "Average moving window (weeks)") +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 11)
  ) +
  scale_color_manual(values = cols)

# -----------------------------
# Combine plots side by side
# -----------------------------
p_rmse + p_mae + plot_annotation(tag_levels = 'A')

