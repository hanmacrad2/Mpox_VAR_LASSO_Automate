
#PLOT FUNCTIONS FOR THE PAPER


#**************
# ---- Define the plotting function ----
PLOT_GLOBAL_METRICS <- function(df) {
  
  # Replace column names with nicer labels
  df <- df %>%
    rename(
      `VAR Lasso`    = VAR_Lasso,
      `AR Lasso`     = AR_Lasso,
      `Naive`        = Naive,
      `Improve VA %` = ImproveVA,
      `Improve VN %` = ImproveVN
    )
  
  # Pivot longer
  df_long <- df %>%
    pivot_longer(
      cols = c(`VAR Lasso`, `AR Lasso`, `Naive`, `Improve VA %`, `Improve VN %`),
      names_to = "Model",
      values_to = "Value"
    )
  
  # Set x-axis order
  df_long$Model <- factor(
    df_long$Model,
    levels = c("VAR Lasso", "AR Lasso", "Naive", "Improve VA %", "Improve VN %")
  )
  
  # Fill colors (light alpha for improvements)
  fill_colors <- c(
    "VAR Lasso"    = "blue",
    "AR Lasso"     = "darkgreen",
    "Naive"        = "red",
    "Improve VA %" = alpha("darkgreen", 0.3),
    "Improve VN %" = alpha("red", 0.3)
  )
  
  # Outline colors
  line_colors <- c(
    "VAR Lasso"    = "blue",
    "AR Lasso"     = "darkgreen",
    "Naive"        = "red",
    "Improve VA %" = alpha("darkgreen", 0.5),
    "Improve VN %" = alpha("red", 0.5)
  )
  
  # Plot builder
  make_plot <- function(metric_name) {
    df_sub <- df_long %>% filter(Metric == metric_name)
    
    p <- ggplot(df_sub,
                aes(x = Model, y = Value, fill = Model, color = Model)) +
      geom_col(width = 0.75, linewidth = 1.1) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = line_colors) +
      labs(title = metric_name, x = NULL, y = NULL) +
      theme_bw(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 13, angle = 30, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
      )
    
    # Explicit y-axis ticks for RMSE & MAE
    if (metric_name %in% c("Slope-weighted RMSE", "Slope-weighted MAE")) {
      p <- p + scale_y_continuous(limits = c(0, 16),
                                  breaks = c(0, 5, 10, 15))
    }
    
    return(p)
  }
  
  p_rmse <- make_plot("Slope-weighted RMSE")
  p_mae  <- make_plot("Slope-weighted MAE")
  p_bias <- make_plot("Slope-weighted Bias")
  
  # Combine horizontally
  patchwork::wrap_plots(p_rmse, p_mae, p_bias, ncol = 3)
}

#***********************
#* PLOT_GLOBAL_IMPROVEMENTS
# ---- Define plotting function ----
PLOT_GLOBAL_IMPROVEMENTS <- function(df) {
  
  # Rename columns
  df <- df %>%
    rename(
      `% Improve VAR-AR` = ImproveVA,
      `% Improve VAR-Naive` = ImproveVN
    )
  
  # Pivot to long format
  df_long <- df %>%
    pivot_longer(
      cols = c(`% Improve VAR-AR`, `% Improve VAR-Naive`),
      names_to = "Model",
      values_to = "Value"
    )
  
  # Factor order
  df_long$Model <- factor(
    df_long$Model,
    levels = c("% Improve VAR-AR", "% Improve VAR-Naive")
  )
  
  # Colors
  fill_colors <- c(
    "% Improve VAR-AR" = alpha("darkgreen", 0.4),
    "% Improve VAR-Naive" = alpha("red", 0.4)
  )
  line_colors <- c(
    "% Improve VAR-AR" = "darkgreen",
    "% Improve VAR-Naive" = "red"
  )
  
  # Plot builder
  make_plot <- function(metric_name) {
    df_sub <- df_long %>% filter(Metric == metric_name)
    
    ggplot(df_sub, aes(x = Model, y = Value, fill = Model, color = Model)) +
      geom_col(width = 0.7, linewidth = 1.1) +
      scale_fill_manual(values = fill_colors) +
      scale_color_manual(values = line_colors) +
      labs(title = metric_name, x = NULL, y = "% Improvement") +
      theme_bw(base_size = 14) +
      theme(
        axis.text.x = element_text(size = 13, angle = 30, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 15, face = "bold")
      ) +
      scale_y_continuous(limits = c(0, max(df_sub$Value) * 1.2),
                         breaks = scales::pretty_breaks(n = 4))
  }
  
  p_rmse <- make_plot("Slope-weighted RMSE")
  p_mae  <- make_plot("Slope-weighted MAE")
  p_bias <- make_plot("Slope-weighted Bias")
  
  patchwork::wrap_plots(p_rmse, p_mae, p_bias, ncol = 3)
}


PLOT_JURISDICTION_METRICS <- function(df, alpha_value = 0.3, axis_tick_size = 12,
                                      axis_label_size = 14) {
  
  # ---- Pivot to long format ----
  df_long <- df %>%
    pivot_longer(-Jurisdiction, names_to = "variable", values_to = "Value") %>%
    tidyr::separate(variable, into = c("Model","Metric"), sep = "_") %>%
    mutate(
      Metric = factor(case_when(
        Metric == "RMSE" ~ "Slope-weighted RMSE",
        Metric == "MAE"  ~ "Slope-weighted MAE"
      ), levels = c("Slope-weighted RMSE","Slope-weighted MAE")),
      Model = factor(case_when(
        Model == "VAR"        ~ "VAR Lasso",
        Model == "AR"         ~ "AR Lasso",
        Model == "Naive"      ~ "Naive",
        Model == "ImproveVA"  ~ "Improve VA %",
        Model == "ImproveVN"  ~ "Improve VN %"
      ), levels = c("VAR Lasso","AR Lasso","Naive","Improve VA %","Improve VN %")),
      # Keep levels for order, labels for display
      Jurisdiction = factor(Jurisdiction,
                            levels = c("NYC","Texas","LA","Florida","Illinois","Georgia","San Diego","Washington"),
                            labels = c("New York City","Texas","Los Angeles county","Florida","Illinois","Georgia","San Diego county","Washington (state)"))
    )
  
  # ---- Fill and outline colors ----
  fill_colors <- c(
    "VAR Lasso"    = "blue",
    "AR Lasso"     = "darkgreen",
    "Naive"        = "red",
    "Improve VA %" = alpha("darkgreen", alpha_value),
    "Improve VN %" = alpha("red", alpha_value)
  )
  
  line_colors <- c(
    "VAR Lasso"    = "blue",
    "AR Lasso"     = "darkgreen",
    "Naive"        = "red",
    "Improve VA %" = alpha("darkgreen", alpha_value + 0.2),
    "Improve VN %" = alpha("red", alpha_value + 0.2)
  )
  
  # ---- Plot using facet_grid ----
  p <- ggplot(df_long, aes(x = Model, y = Value, fill = Model, color = Model)) +
    geom_col(width = 0.75, linewidth = 1.1) +
    scale_fill_manual(values = fill_colors) +
    scale_color_manual(values = line_colors) +
    facet_grid(Metric ~ Jurisdiction, scales = "free_y") +
    theme_bw(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = axis_tick_size),
      axis.title.x = element_text(size = axis_label_size, face = "bold"),
      axis.text.y = element_text(size = axis_tick_size),
      legend.position = "none",
      strip.text.x = element_text(size = axis_label_size, face = "bold"),  # full jurisdiction names
      strip.text.y = element_text(size = axis_label_size, face = "bold"),
      plot.title = element_text(size = axis_label_size, face = "bold"),
      panel.spacing = unit(0.5, "lines"),
      plot.margin = margin(t = 5, r = 5, b = 5, l = 25)  # increase left margin for long labels
    ) +
    labs(x = "Model", y = NULL) +
    coord_cartesian(clip = "off")  # prevent cutting off labels
  
  return(p)
}


