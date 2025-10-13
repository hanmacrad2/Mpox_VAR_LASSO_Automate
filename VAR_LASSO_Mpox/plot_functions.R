#*******************************
#* PLOT FUNCTIONS
#******************************
library(patchwork)

PLOT_DATES_TRUE_FORECAST <- function(data_ts, df_preds, list_ordered_jur,
                                     title_plot, n_col_plot = 2, predicted_col = "red") {
  
  #FORMAT
  data_ts <- data_ts %>%
    mutate(date_week_start = as.Date(date_week_start))
  df_preds <- df_preds %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  # Step 1: Format true cases from full dataset
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # Step 2: Prepare predictions long format (already has date_week_start)
  df_pred_long <- df_preds %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # Step 3: Combine and filter
  df_plot <- bind_rows(df_true_long, df_pred_long) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur)
    )
  
  # Step 4: Use actual dates and select every 4th week for axis ticks
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  # Every 4th date to reduce label clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Make sure last date is included (e.g. November)
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Step 5: Plot
  ggplot(df_plot, aes(x = date_week_start, y = Cases, color = Source)) +
    geom_line(data = filter(df_plot, Source == "True"), size = 0.9) +
    geom_point(data = filter(df_plot, Source == "True"), size = 1.75) +
    geom_line(data = filter(df_plot, Source == "Predicted"), size = 0.6) +
    geom_point(data = filter(df_plot, Source == "Predicted"), size = 1.75) +
    scale_color_manual(values = c("True" = "black", "Predicted" = predicted_col)) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%m/%d/%y"
    ) +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
    labs(x = "Date (week start date of reported cases)", y = "Cases", title = title_plot) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12)
    )
}

PLOT_DATES_TRUE_FORECAST_SD <- function(data_sd, title_plot) {
  
  # Ensure Date column exists as date_week_start or week_start_date (rename if needed)
  # Assume data_sd has a column named 'date_week_start' for actual dates
  
  data_sd <- data_sd %>%
    mutate(date_week_start = as.Date(date_week_start))
  
  data_sd <- data_sd %>%
    rename(Date = date_week_start) %>%
    dplyr::select(Week_Number, Date, Predicted, Actual) %>%
    pivot_longer(cols = c("Predicted", "Actual"), 
                 names_to = "Source", 
                 values_to = "Cases")
  
  # Get all unique dates in ascending order
  date_seq <- data_sd %>%
    distinct(Date) %>%
    arrange(Date) %>%
    pull(Date)
  
  # Pick every 4th date to reduce clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Ensure last date included
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(data_sd, aes(x = Date, y = Cases, color = Source)) +
    geom_line(size = 0.7) +
    geom_point(size = 3.0) +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    scale_y_continuous(
      breaks = 0:10,
      limits = c(0, 10),
      expand = expansion(mult = c(0.02, 0))
    ) + 
    labs(x = "Date (week start date of reported cases)", y = "Cases", title = title_plot) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_line(),
      axis.ticks.length = unit(5, "pt")
    )
}