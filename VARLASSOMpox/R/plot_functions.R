#*******************************
#* PLOT FUNCTIONS
#******************************
library(patchwork)
library(ggh4x)  # for facet_wrap2

PLOT_DATES_TRUE_FORECAST <- function(data_ts, 
                                     df_preds_var, df_preds_ar, df_preds_naive,
                                     list_ordered_jur, title_plot, 
                                     n_col_plot = 2,
                                     col_var = "blue", col_ar = "darkgreen",
                                     col_naive = "red", 
                                     three_figures = TRUE, point_size = 2.0) {
  
  # --- Step 0: Jurisdiction labels ---
  jur_labels = GET_JUR_LABELS()
  
  # --- Step 1: Ensure date format ---
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start),
                                          Method = "VAR")
  df_preds_ar <- df_preds_ar %>% mutate(date_week_start = as.Date(date_week_start),
                                        Method = "AR")
  df_preds_naive <- df_preds_naive %>% mutate(date_week_start = as.Date(date_week_start),
                                              Method = "Naive")
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Combine predictions ---
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # --- Step 4: Combine true + predicted, set factor labels ---
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction_orig = Jurisdiction,  # keep original for filtering
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur]),
      Method = factor(Method,
                      levels = c("VAR", "AR", "Naive"),
                      labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
    )
  
  # --- Step 5: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  #if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # --- Step 6: Plot helper ---
  plot_subset <- function(subset_jur) {
    ggplot() +
      # Reported cases
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                 aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      # Predicted cases
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                aes(x = date_week_start, y = Cases, color = Method),
                size = 0.7) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                 aes(x = date_week_start, y = Cases, color = Method),
                 size = point_size) +
      
      # Scales
      scale_color_manual(name = "",
                         values = c("VAR-Lasso" = col_var,
                                    "AR-Lasso" = col_ar,
                                    "Naive" = col_naive)) +
      scale_linetype_manual(name = "",
                            values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "",
                         values = c("Reported cases" = 16)) +
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      
      labs(
        x = "Date (week start date of reported cases)",
        y = "Cases",
        title = title_plot
      ) +
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  # --- Step 7: Multi-panel mode ---
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    # Split into chunks of 4 for 2x2 layout
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
      
      # Pad last panel if <4 jurisdictions
      if (length(subset_jur) < 4) {
        missing <- 4 - length(subset_jur)
        placeholders <- paste0("placeholder_", seq_len(missing))
        
        df_dummy <- tibble(
          date_week_start = as.Date(NA),
          Week_Number = NA,
          Jurisdiction_orig = placeholders,
          Jurisdiction = factor(placeholders, levels = placeholders),
          Cases = NA,
          Source = "True",
          Method = "VAR-Lasso"
        )
        df_plot <<- bind_rows(df_plot, df_dummy)
        subset_jur <- c(subset_jur, placeholders)
      }
      
      p <- plot_subset(subset_jur)
      print(p)
    })
    
    invisible(plot_list)
  }
}



PLOT_DATES_TRUE_FORECAST2 <- function(data_ts, 
                                     df_preds_var, df_preds_ar, df_preds_naive,
                                     list_ordered_jur, title_plot, 
                                     n_col_plot = 2,
                                     col_var = "blue", col_ar = "darkgreen",
                                     col_naive = "red", 
                                     three_figures = TRUE, point_size = 2.0) {
  
  # --- Step 1: Ensure date format ---
  jur_labels = GET_JUR_LABELS()
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start),
                                          Method = "VAR")
  df_preds_ar <- df_preds_ar %>% mutate(date_week_start = as.Date(date_week_start),
                                        Method = "AR")
  df_preds_naive <- df_preds_naive %>% mutate(date_week_start = as.Date(date_week_start),
                                              Method = "Naive")
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Combine predictions ---
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # --- Step 4: Combine true + predicted, set factor labels ---
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      # Set Jurisdiction as factor with nice labels
      Jurisdiction = factor(Jurisdiction, levels = names(jur_labels), labels = jur_labels[names(jur_labels)]),
      Method = factor(Method,
                      levels = c("VAR", "AR", "Naive"),
                      labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
    )
  
  # df_plot <- bind_rows(df_true_long, df_preds_all) %>%
  #   filter(Jurisdiction %in% list_ordered_jur) %>%
  #   mutate(
  #     Source = factor(Source, levels = c("True", "Predicted")),
  #     Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur, labels = jur_labels[names(jur_labels)]),
  #     Method = factor(Method,
  #                     levels = c("VAR", "AR", "Naive"),
  #                     labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
  #   )
  
  # --- Step 5: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # --- Step 6: Plot helper ---
  plot_subset <- function(subset_jur) {
    ggplot() +
      
      # Reported (True) cases — add to legend using linetype + shape
      geom_line(data = df_plot %>% filter(Source == "True", Jurisdiction %in% subset_jur),
                aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Source == "True", Jurisdiction %in% subset_jur),
                 aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      # Predicted cases
      geom_line(data = df_plot %>% filter(Source == "Predicted", Jurisdiction %in% subset_jur),
                aes(x = date_week_start, y = Cases, color = Method),
                size = 0.7) +
      geom_point(data = df_plot %>% filter(Source == "Predicted", Jurisdiction %in% subset_jur),
                 aes(x = date_week_start, y = Cases, color = Method),
                 size = point_size) +
      
      # Manual scales for legend
      scale_color_manual(name = "",
                         values = c("VAR-Lasso" = col_var,
                                    "AR-Lasso" = col_ar,
                                    "Naive" = col_naive)
      ) +
      scale_linetype_manual(name = "",
                            values = c("Reported cases" = "solid")
      ) +
      scale_shape_manual(name = "",
                         values = c("Reported cases" = 16)
      ) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      
      labs(
        x = "Date (week start date of reported cases)",
        y = "Cases",
        title = title_plot
      ) +
      
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  # --- Step 7: Multi-panel mode ---
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    # split into chunks of 4
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
      
      # pad last panel for 2x2 layout
      if (length(subset_jur) < 4) {
        missing <- 4 - length(subset_jur)
        placeholders <- paste0("placeholder_", seq_len(missing))
        
        df_dummy <- tibble(
          date_week_start = as.Date(NA),
          Week_Number = NA,
          Jurisdiction = placeholders,
          Cases = NA,
          Source = "True",
          Method = "VAR-Lasso"
        )
        
        df_plot <<- bind_rows(df_plot, df_dummy) %>%
          mutate(Jurisdiction = factor(Jurisdiction,
                                       levels = c(levels(df_plot$Jurisdiction), placeholders)))
        
        subset_jur <- c(subset_jur, placeholders)
      }
      
      p <- plot_subset(subset_jur)
      print(p)
    })
    
    invisible(plot_list)
  }
}


PLOT_DATES_TRUE_FORECAST2 <- function(data_ts, 
                                     df_preds_var, df_preds_ar, df_preds_naive,
                                     list_ordered_jur, title_plot, 
                                     n_col_plot = 2,
                                     col_var = "blue", col_ar = "darkgreen",
                                     col_naive = "red", 
                                     three_figures = TRUE) {
  
  # --- Step 1: Ensure date format ---
  data_ts <- data_ts %>%
    mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>%
    mutate(date_week_start = as.Date(date_week_start), Method = "VAR")
  df_preds_ar <- df_preds_ar %>%
    mutate(date_week_start = as.Date(date_week_start), Method = "AR")
  df_preds_naive <- df_preds_naive %>%
    mutate(date_week_start = as.Date(date_week_start), Method = "Naive")
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Combine all predictions ---
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # --- Step 4: Combine true + preds ---
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur),
      Method = factor(Method, levels = c("VAR", "AR", "Naive"))
    )
  
  # --- Step 5: Date breaks ---
  date_seq <- df_plot %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # --- Step 6: Define plotting helper ---
  plot_subset <- function(subset_jur) {
    ggplot() +
      geom_line(data = df_plot %>% filter(Source == "True", Jurisdiction %in% subset_jur),
                aes(x = date_week_start, y = Cases, group = 1),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Source == "True", Jurisdiction %in% subset_jur),
                 aes(x = date_week_start, y = Cases),
                 color = "black", size = 1.75) +
      geom_line(data = df_plot %>% filter(Source == "Predicted", Jurisdiction %in% subset_jur),
                aes(x = date_week_start, y = Cases, color = Method),
                size = 0.7) +
      geom_point(data = df_plot %>% filter(Source == "Predicted", Jurisdiction %in% subset_jur),
                 aes(x = date_week_start, y = Cases, color = Method),
                 size = 1.75) +
      scale_color_manual(
        name = "Model",
        values = c("VAR" = col_var, "AR" = col_ar, "Naive" = col_naive)
      ) +
      scale_x_date(
        breaks = x_breaks,
        date_labels = "%m/%d/%y"
      ) +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      labs(
        x = "Date (week start date of reported cases)",
        y = "Cases",
        title = title_plot
      ) +
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),      # legend labels
        legend.title = element_text(size = 16),  
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19)
      )
  }
  
  # --- Step 7: Either single figure or multiple ---
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    # Split into chunks of 4
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
      
      # Pad last subset with empty placeholders to preserve 2x2 layout
      if (length(subset_jur) < 4) {
        missing <- 4 - length(subset_jur)
        placeholders <- paste0("placeholder_", seq_len(missing))
        
        # Add dummy rows so facet_wrap2 keeps grid size
        df_dummy <- tibble(
          date_week_start = as.Date(NA),
          Week_Number = NA,
          Jurisdiction = placeholders,
          Cases = NA,
          Source = "True",
          Method = "VAR"
        )
        
        df_plot <<- bind_rows(df_plot, df_dummy) %>%
          mutate(Jurisdiction = factor(Jurisdiction, 
                                       levels = c(levels(df_plot$Jurisdiction), placeholders)))
        subset_jur <- c(subset_jur, placeholders)
      }
      
      p <- plot_subset(subset_jur) + 
        labs(title = title_plot)
      print(p)
    })
    
    invisible(plot_list)
  }
}



PLOT_DATES_TRUE_FORECAST_JUR <- function(df_preds_var, df_preds_ar, df_preds_naive,
                                         title_plot,
                                         col_var = "blue", col_ar = "darkgreen", col_naive = "red") {
  
  # Standardize columns and add method labels
  df_var <- df_preds_var %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "VAR") %>%
    dplyr::select(Week_Number, Date = date_week_start, Predicted, Actual, Method)
  
  df_ar <- df_preds_ar %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "AR") %>%
    dplyr::select(Week_Number, Date = date_week_start, Predicted, Actual, Method)
  
  df_naive <- df_preds_naive %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "Naive") %>%
    dplyr::select(Week_Number, Date = date_week_start, Predicted, Actual, Method)
  
  # Combine all
  df_all <- bind_rows(df_var, df_ar, df_naive)
  
  # Pivot to long format for ggplot
  df_long <- df_all %>%
    pivot_longer(cols = c("Predicted", "Actual"), 
                 names_to = "Source", 
                 values_to = "Cases") %>%
    mutate(Method = factor(Method, levels = c("VAR", "AR", "Naive")))
  # df_long <- df_all %>%
  #   pivot_longer(cols = c("Predicted", "Actual"), 
  #                names_to = "Source", 
  #                values_to = "Cases")
  
  # Get all unique dates in ascending order
  date_seq <- df_long %>%
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
  ggplot() +
    # Actual (single black line)
    geom_line(data = df_long %>% filter(Source == "Actual"),
              aes(x = Date, y = Cases, group = 1),
              color = "black", size = 1) +
    geom_point(data = df_long %>% filter(Source == "Actual"),
               aes(x = Date, y = Cases),
               color = "black", size = 3) +
    # Predicted (colored by method)
    geom_line(data = df_long %>% filter(Source == "Predicted"),
              aes(x = Date, y = Cases, color = Method),
              size = 0.9) +
    geom_point(data = df_long %>% filter(Source == "Predicted"),
               aes(x = Date, y = Cases, color = Method),
               size = 2.5) +
    scale_color_manual(
      name = "Model",
      values = c("VAR" = col_var, "AR" = col_ar, "Naive" = col_naive)
    ) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    labs(x = "Date (week start date of reported cases)",
         y = "Cases",
         title = title_plot) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks.x = element_line(),
      axis.ticks.length = unit(5, "pt")
    )
}


#PLOT
plot_jur_data <- function(data_sd, data_other, label_other, colour_jur, month_label = TRUE) {
  
  # Ensure consistent date column names
  if (!"date_week_start" %in% names(data_sd)) stop("data_sd must have a column named 'date_week_start'")
  if (!"date_week_start" %in% names(data_other)) stop("data_other must have a column named 'date_week_start'")
  
  # Add jurisdiction labels
  data_sd <- data_sd %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Jurisdiction = "San Diego"
    )
  
  data_other <- data_other %>%
    mutate(
      date_week_start = as.Date(date_week_start),
      Jurisdiction = label_other
    )
  
  # Combine both datasets
  data_combined <- bind_rows(data_sd, data_other)
  
  # Get all unique dates for x-axis
  date_seq <- data_combined %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  # Pick every 4th date to reduce clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Ensure last date is included
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Color mapping
  jur_colors <- setNames(c("#A6D74E", colour_jur), c("San Diego", label_other))
  
  # x-axis breaks / labels
  if(month_label) {
    x_scale <- scale_x_date(date_breaks = "1 month", date_labels = "%b %y")
  } else {
    x_scale <- scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y")
  }
  
  # Plot
  ggplot(data_combined, aes(x = date_week_start, y = Cases, group = Jurisdiction)) +
    geom_line(aes(color = Jurisdiction), size = 1) +
    geom_point(aes(fill = Jurisdiction),
               shape = 21,
               color = "black",
               size = 2.8,
               stroke = 0.8) +
    scale_color_manual(values = jur_colors) +
    scale_fill_manual(values = jur_colors) +
    x_scale +
    labs(
      x = ifelse(month_label, "Month", "Week start date"),
      y = "Cases",
      title = ifelse(month_label,
                     paste("Mpox weekly reported cases:", label_other, "vs San Diego, 2023–2024"),
                     paste("Mpox weekly reported cases:", label_other, "vs San Diego, 2023–2024")),
      color = "Jurisdiction",
      fill = "Jurisdiction"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}


#PLOT
PLOT_REPORTED_CASES <- function(data_ts, 
                                list_ordered_jur, 
                                title_plot, 
                                n_col_plot = 2,
                                point_size = 2.0,
                                three_figures = TRUE) {
  
  # --- Step 0: Jurisdiction labels ---
  jur_labels = GET_JUR_LABELS()
  
  # --- Step 1: Ensure date format ---
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Filter + add readable labels ---
  df_plot <- df_true_long %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur])
    )
  
  # --- Step 4: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 8)]
  #if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # --- Step 5: Plot helper ---
  plot_subset <- function(subset_jur) {
    ggplot(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur)) +
      geom_line(aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      
      labs(
        x = "Date (week start date of reported cases)",
        y = "Cases",
        title = title_plot
      ) +
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  # --- Step 6: Multi-panel mode ---
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
      
      # pad last panel if <4
      if (length(subset_jur) < 4) {
        missing <- 4 - length(subset_jur)
        placeholders <- paste0("placeholder_", seq_len(missing))
        
        df_dummy <- tibble(
          date_week_start = as.Date(NA),
          Week_Number = NA,
          Jurisdiction_orig = placeholders,
          Jurisdiction = factor(placeholders, levels = placeholders),
          Cases = NA,
          Source = "True"
        )
        
        df_plot <<- bind_rows(df_plot, df_dummy)
        subset_jur <- c(subset_jur, placeholders)
      }
      
      p <- plot_subset(subset_jur)
      print(p)
    })
    
    invisible(plot_list)
  }
}

PLOT_REPORTED_CASES2 <- function(data_ts, 
                                list_ordered_jur, 
                                title_plot, 
                                n_col_plot = 2,
                                point_size = 2.0,
                                three_figures = TRUE) {
  
  # --- Step 0: Jurisdiction labels ---
  jur_labels <- c(
    "NewYorkCity" = "New York City",
    "Texas"      = "Texas",
    "LA"         = "Los Angeles County",
    "Florida"    = "Florida",
    "Illinois"   = "Illinois",
    "Georgia"    = "Georgia",
    "SanDiego"   = "San Diego County",
    "Washington" = "Washington State"
  )
  
  # --- Step 1: Ensure date format ---
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Filter + add readable labels ---
  df_plot <- df_true_long %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur])
    )
  
  # --- Step 4: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 8)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # --- Step 5: Plot helper ---
  plot_subset <- function(subset_jur) {
    ggplot(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur)) +
      geom_line(aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      
      labs(
        x = "Date (week start date of reported cases)",
        y = "Cases",
        title = title_plot
      ) +
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  # --- Step 6: Multi-panel mode ---
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur) / 4))
    
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
      
      # pad last panel if <4
      if (length(subset_jur) < 4) {
        missing <- 4 - length(subset_jur)
        placeholders <- paste0("placeholder_", seq_len(missing))
        
        df_dummy <- tibble(
          date_week_start = as.Date(NA),
          Week_Number = NA,
          Jurisdiction_orig = placeholders,
          Jurisdiction = factor(placeholders, levels = placeholders),
          Cases = NA,
          Source = "True"
        )
        
        df_plot <<- bind_rows(df_plot, df_dummy)
        subset_jur <- c(subset_jur, placeholders)
      }
      
      p <- plot_subset(subset_jur)
      print(p)
    })
    
    invisible(plot_list)
  }
}



PLOT_REPORTED_CASES_2x4 <- function(data_ts, 
                                    list_ordered_jur, 
                                    title_plot, 
                                    point_size = 2.0) {
  
  # --- Step 1: Ensure date format ---
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # --- Step 2: Format true cases ---
  df_plot <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(Jurisdiction = factor(Jurisdiction, levels = list_ordered_jur))
  
  # --- Step 3: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, tail(date_seq, 1))
  
  # --- Step 4: Plot ---
  ggplot(df_plot) +
    geom_line(aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
              color = "black", size = 0.9) +
    geom_point(aes(x = date_week_start, y = Cases, shape = "Reported cases"),
               color = "black", size = point_size) +
    scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
    scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", nrow = 4, ncol = 2) +
    labs(
      x = "Date (week start date of reported cases)",
      y = "Cases",
      title = title_plot
    ) +
    theme_minimal(base_size = 17) +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 19),
      legend.key.size = unit(1.2, "lines"),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.8, "cm")
    )
}

#JUR LABELS
GET_JUR_LABELS <- function(){
  
  jur_labels <- c(
    "NewYorkCity" = "New York City",
    "Texas"      = "Texas",
    "LA"         = "Los Angeles County",
    "Florida"    = "Florida",
    "Illinois"   = "Illinois",
    "Georgia"    = "Georgia",
    "SanDiego"   = "San Diego County",
    "Washington" = "Washington State"
  )
  
  return(jur_labels)
  
}
