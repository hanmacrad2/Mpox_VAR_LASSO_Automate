#*******************************
#* PLOT FUNCTIONS
#******************************

PLOT_CASES_FORECASTS <- function(data_ts, 
                                     df_preds_var, df_preds_ar, df_preds_naive,
                                     list_ordered_jur, title_plot, 
                                     n_col_plot = 2, ymax = 17,
                                     col_var = "blue", col_ar = "darkgreen",
                                     col_naive = "magenta", 
                                     three_figures = FALSE, point_size = 2.0) {
  
  jur_labels <- GET_JUR_LABELS()
  
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start), Method = "VAR")
  df_preds_ar <- df_preds_ar %>% mutate(date_week_start = as.Date(date_week_start), Method = "AR", Lower_CI = NA, Upper_CI = NA)
  df_preds_naive <- df_preds_naive %>% mutate(date_week_start = as.Date(date_week_start), Method = "Naive", Lower_CI = NA, Upper_CI = NA)
  
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method, Lower_CI, Upper_CI) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur]),
      Method = factor(Method, levels = c("VAR", "AR", "Naive"), labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
    )
  
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  plot_subset <- function(subset_jur) {
    ggplot() +
      # Ribbon for VAR-Lasso
      geom_ribbon(
        data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Method == "VAR-Lasso"),
        aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI),
        fill = col_var, alpha = 0.2
      ) +
      
      # Reported cases
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                 aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      # Predicted lines/points
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                aes(x = date_week_start, y = Cases, color = Method), size = 0.7) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                 aes(x = date_week_start, y = Cases, color = Method), size = point_size) +
      
      # Scales
      scale_color_manual(name = "", values = c("VAR-Lasso" = col_var, "AR-Lasso" = col_ar, "Naive" = col_naive)) +
      scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      labs(x = "Month", y = "Case counts", title = title_plot) +
      coord_cartesian(ylim = c(0, ymax)) +
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
  
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur)/4))
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
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

#***************
#PLOT WITH CI
PLOT_CASES_FORECASTS_CI_LEGEND <- function(data_ts, 
                                               df_preds_var, df_preds_ar, df_preds_naive,
                                               list_ordered_jur, title_plot, 
                                               n_col_plot = 2, ymax = 17,
                                               col_var = "blue", col_ar = "darkgreen",
                                               col_naive = "magenta", 
                                               three_figures = TRUE, point_size = 2.0) {
  
  jur_labels <- GET_JUR_LABELS()
  
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start), Method = "VAR")
  df_preds_ar <- df_preds_ar %>% mutate(date_week_start = as.Date(date_week_start), Method = "AR")
  df_preds_naive <- df_preds_naive %>% mutate(date_week_start = as.Date(date_week_start), Method = "Naive", Lower_CI = NA, Upper_CI = NA)
  
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method, Lower_CI, Upper_CI) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur]),
      Method = factor(Method, levels = c("VAR", "AR", "Naive"), labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
    )
  
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  plot_subset <- function(subset_jur) {
    ggplot() +
      # CI ribbons
      geom_ribbon(
        data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Method == "VAR-Lasso", !is.na(Lower_CI)),
        aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI, fill = "VAR CI"), alpha = 0.3
      ) +
      geom_ribbon(
        data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Method == "AR-Lasso", !is.na(Lower_CI)),
        aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI, fill = "AR CI"), alpha = 0.3
      ) +
      
      # Predicted lines/points
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                aes(x = date_week_start, y = Cases, color = Method), size = 0.7) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                 aes(x = date_week_start, y = Cases, color = Method), size = point_size) +
      
      # Reported cases
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                 aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      # Scales
      scale_color_manual(name = "", values = c("VAR-Lasso" = col_var, "AR-Lasso" = col_ar, "Naive" = col_naive)) +
      scale_fill_manual(name = "", values = c("VAR CI" = "lightblue", "AR CI" = "lightgreen")) +
      scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
      
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      labs(x = "Month", y = "Case counts", title = title_plot) +
      coord_cartesian(ylim = c(0, ymax)) +
      theme_minimal(base_size = 17) +
      theme(
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 19),
        legend.key.size = unit(1.2, "lines"),
        legend.key.width = unit(2, "cm"),
        legend.key.height = unit(0.8, "cm")
      )
  }
  
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur)/4))
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
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
          Method = "VAR-Lasso",
          Lower_CI = NA,
          Upper_CI = NA
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




PLOT_DATES_TRUE_FORECAST_JUR <- function(df_preds_var, df_preds_ar, df_preds_naive,
                                         title_plot,
                                         col_var = "blue", col_ar = "darkgreen", col_naive = "magenta") {
  
  # Standardize columns and add method labels
  df_var <- df_preds_var %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "VAR-Lasso") %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_ar <- df_preds_ar %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "AR-Lasso") %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  df_naive <- df_preds_naive %>%
    mutate(date_week_start = as.Date(date_week_start),
           Method = "Naive") %>%
    dplyr::select(Week_Number, date_week_start, Predicted, Actual, Method)
  
  # Combine all
  df_all <- bind_rows(df_var, df_ar, df_naive)
  
  # Pivot to long format and create a Legend variable
  df_long <- df_all %>%
    pivot_longer(cols = c("Predicted", "Actual"), 
                 names_to = "Source", 
                 values_to = "Cases") %>%
    mutate(Legend = case_when(
      Source == "Actual" ~ "Reported Cases",
      Method == "VAR-Lasso" ~ "VAR-Lasso",
      Method == "AR-Lasso" ~ "AR-Lasso",
      Method == "Naive" ~ "Naive"
    ),
    Legend = factor(Legend, levels = c("Reported Cases", "VAR-Lasso", "AR-Lasso", "Naive")))
  
  # Get all unique dates in ascending order
  date_seq <- df_long %>%
    distinct(date_week_start) %>%
    arrange(date_week_start) %>%
    pull(date_week_start)
  
  # Pick every 4th date to reduce clutter
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  # Ensure last date included
  if (tail(date_seq, 1) != tail(x_breaks, 1)) {
    x_breaks <- c(x_breaks, tail(date_seq, 1))
  }
  
  # Plot
  ggplot(df_long, aes(x = date_week_start, y = Cases, color = Legend)) +
    geom_line(aes(group = Legend, size = Legend)) +
    geom_point(size = 2.5) +
    scale_color_manual(
      name = NULL, 
      values = c(
        "Reported Cases" = "black",
        "VAR-Lasso" = col_var,
        "AR-Lasso" = col_ar,
        "Naive" = col_naive
      )
    ) +
    scale_size_manual(
      values = c(
        "Reported Cases" = 1.2,
        "VAR-Lasso" = 0.9,
        "AR-Lasso" = 0.9,
        "Naive" = 0.9
      ),
      guide = "none"
    ) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    labs(x = "Date (week start date of reported cases)",
         y = "Case counts",
         title = title_plot) +
    theme_minimal(base_size = 17) +
    theme(
      plot.title = element_text(size = 22, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 18),
      strip.text = element_text(size = 19),
      legend.key.size = unit(1.2, "lines"),
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.8, "cm")
    )
}


#PLOT
PLOT_REPORTED_CASES <- function(data_ts, 
                                list_ordered_jur, 
                                title_plot,
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

#PLOT TOP JUR
PLOT_REPORTED_CASES_JUR <- function(data_mpox, list_jur, start_year_interest){
  
  #DATA
  data_mpox_jur = data_mpox %>% filter(Jurisdiction %in% list_jur)
  data_mpox_jur_2023_24 = data_mpox_jur %>% filter(Year >= start_year_interest)
  
  weekly_jurisdiction <- data_mpox_jur_2023_24 %>%
    group_by(Jurisdiction, date_week_start) %>%
    summarise(total_cases = sum(Cases, na.rm = TRUE), .groups = "drop")
  
  # Custom colors
  custom_colors <- c(
    "California" = "orange",      # Pine green
    "NYC" = "purple",                # vivid red
    "Texas" = "red",
    "Florida" = "blue",
    "LA" = "yellow",
    "Georgia" = "darkgreen",            # Fuchsia pink
    "Illinois" = "lightblue",
    "San Diego" = "greenyellow"
  )
  
  # Add colors for any others
  other_jurisdictions <- setdiff(unique(weekly_jurisdiction$Jurisdiction), names(custom_colors))
  if (length(other_jurisdictions) > 0) {
    custom_colors <- c(custom_colors, setNames(rainbow(length(other_jurisdictions)), other_jurisdictions))
  }
  
  # Plot: thinner lines, filled points, larger axis labels
  ggplot(weekly_jurisdiction, aes(x = date_week_start, y = total_cases, color = Jurisdiction, fill = Jurisdiction)) +
    geom_line(linewidth = 0.7) +                 # slim lines
    geom_point(size = 2.5, shape = 21) +           # filled circles
    scale_color_manual(values = custom_colors) +
    scale_fill_manual(values = custom_colors) +
    labs(
      title = "Mpox Reported Cases across U.S jurisdictions, 2022-2025",
      x = "Year",
      y = "Weekly Case count"
    ) +
    theme_minimal(base_size = 16) +              # larger base font
    theme(
      axis.title.x = element_text(size = 16, face = 'bold'), 
      axis.title.y = element_text(size = 16, face = 'bold'),
      axis.text = element_text(size = 16, face = 'bold'),
      axis.text.x = element_text(size = 16, face = 'bold'), 
      axis.text.Y = element_text(size = 16, face = 'bold'), 
      plot.title = element_text(size = 20, hjust = 0.5, face = "bold"),
      legend.position = "none")
  
}

PLOT_REPORTED_CASES_2x4 <- function(data_ts, 
                                    list_ordered_jur, 
                                    title_plot, ymax, title_size = 19, sub_title_size = 16, 
                                    font_base_size = 15,
                                    point_size = 1.5, line_size = 0.75, n_row_plot = 4,
                                    n_col_plot = 2) {
  
  # --- Step 1: Ensure date format ---
  jur_labels = GET_JUR_LABELS()
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  
  # --- Step 2: Format true cases ---
  df_plot <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur])
    )
  
  # --- Step 3: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 8)]
  #if (tail(date_seq, 1) != tail(x_breaks, 1)) x_breaks <- c(x_breaks, (tail(date_seq, 1)+10))
  
  # --- Step 4: Plot ---
  ggplot(df_plot) +
    geom_line(aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
              color = "black", size = line_size) +
    geom_point(aes(x = date_week_start, y = Cases, shape = "Reported cases"),
               color = "black", size = point_size) +
    ylim(0, ymax) +
    scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
    scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", nrow = n_row_plot, ncol = n_col_plot) +
    labs(
      x = "Date (week start date of reported cases)",
      y = "Case Counts",
      title = title_plot
    ) +
    theme_minimal(base_size = font_base_size) +
    theme(
      plot.title = element_text(size = title_size, hjust = 0.5),
      legend.position = "none",
      #legend.text = element_text(size = 16),
      #legend.title = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = sub_title_size),
      panel.spacing.y = unit(0.2, "lines") 
      #legend.key.size = unit(1.2, "lines"),
      #legend.key.width = unit(2, "cm"),
      #legend.key.height = unit(0.8, "cm")
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


#SENSITIVITY PLOTS
PLOT_SMOOTHING_SENSITIVITY <- function(df,
                                       col_var = "blue", col_ar = "darkgreen",
                                       col_naive = "magenta",
                                       base_size = 15, title_size = 16,
                                       legend_size = 16, axis_label_size = 15, axis_tick_size = 14,
                                       point_size = 3, highlight_size = 8,
                                       line_size = 1,
                                       legend_x = 0.75, legend_y = 0.85) {
  # -----------------------------
  # Convert to long format safely
  # -----------------------------
  df_long <- df %>%
    pivot_longer(
      cols = -Smoothing,
      names_to = c("Metric", "Model"),
      names_pattern = "(RMSE|MAE)_(.*)",
      values_to = "Value"
    ) %>%
    mutate(Model = factor(Model, levels = c("VAR", "AR", "Naive")))
  
  # Order x-axis
  df_long$Smoothing <- factor(df_long$Smoothing,
                              levels = c("No smoothing", "2", "3", "4", "5"))
  
  # Highlight VAR at smoothing window 4
  highlight_rmse <- df_long %>%
    filter(Metric == "RMSE", Model == "VAR", Smoothing == "4")
  
  highlight_mae <- df_long %>%
    filter(Metric == "MAE", Model == "VAR", Smoothing == "4")
  
  # Colors
  cols <- c("VAR" = col_var, "AR" = col_ar, "Naive" = col_naive)
  
  # Legend labels
  labels <- c("VAR" = "VAR-Lasso",
              "AR" = "AR-Lasso",
              "Naive" = "Naive")
  
  # --------------------------------
  # Helper theme with legend inside
  # --------------------------------
  theme_inside <- function() {
    theme_minimal(base_size = base_size) +
      theme(
        legend.position = c(legend_x, legend_y),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.key = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.text = element_text(size = legend_size),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = title_size),
        axis.text.x = element_text(size = axis_tick_size),
        axis.text.y = element_text(size = axis_tick_size),
        axis.title.x = element_text(size = axis_label_size,
                                    margin = margin(t = 15)),
        axis.title.y = element_text(size = axis_label_size,
                                    margin = margin(r = 15))
      )
  }
  
  # -----------------------------
  # RMSE PLOT
  # -----------------------------
  p_rmse <- df_long %>%
    filter(Metric == "RMSE") %>%
    ggplot(aes(x = Smoothing, y = Value,
               color = Model, shape = Model, group = Model)) +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    geom_point(data = highlight_rmse,
               aes(x = Smoothing, y = Value),
               inherit.aes = FALSE,
               color = "orange", shape = 1, stroke = 1.5,
               size = highlight_size) +
    scale_color_manual(values = cols, labels = labels) +
    scale_shape_manual(values = c(16, 17, 15), labels = labels) +
    labs(
      x = "Average moving window (weeks)",
      y = "Positive slope-weighted RMSE",
      title = 'Sensitivity Analysis - Slope-weighted RMSE'
    ) +
    theme_inside()
  
  # -----------------------------
  # MAE PLOT
  # -----------------------------
  p_mae <- df_long %>%
    filter(Metric == "MAE") %>%
    ggplot(aes(x = Smoothing, y = Value,
               color = Model, shape = Model, group = Model)) +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    geom_point(data = highlight_mae,
               aes(x = Smoothing, y = Value),
               inherit.aes = FALSE,
               color = "orange", shape = 1, stroke = 1.5,
               size = highlight_size) +
    scale_color_manual(values = cols, labels = labels) +
    scale_shape_manual(values = c(16, 17, 15), labels = labels) +
    labs(
      x = "Average moving window (weeks)",
      y = "Positive slope-weighted MAE",
      title = 'Sensitivity Analysis - Slope-weighted MAE'
    ) +
    theme_inside()
  
  return(list(RMSE = p_rmse, MAE = p_mae))
}

#*********************
# METRICS

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
PLOT_GLOBAL_IMPROVEMENTS <- function(df, col_ar = 'darkgreen', col_naive = 'magenta',
                                     axis_label_size = 17) {
  
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
    "% Improve VAR-AR" = alpha(col_ar, 0.4),
    "% Improve VAR-Naive" = alpha(col_naive, 0.4)
  )
  line_colors <- c(
    "% Improve VAR-AR" = col_ar,
    "% Improve VAR-Naive" = col_naive
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
        axis.text.x = element_text(size = axis_label_size, angle = 30, hjust = 1),
        axis.text.y = element_text(size = axis_label_size),
        axis.title.x = element_text(size = axis_label_size),
        axis.title.y = element_text(size = axis_label_size),
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

# -----------------------------
# SENSITIVITY PLOTS FUNCTION
# -----------------------------
PLOT_SMOOTHING_SENSITIVITY_x2 <- function(df,
                                          col_var = "blue", col_ar = "darkgreen",
                                          col_naive = "magenta",
                                          base_size = 17, title_size = 18,
                                          legend_size = 16, axis_label_size = 16, axis_tick_size = 13,
                                          point_size = 3, highlight_size = 8,
                                          line_size = 1,
                                          legend_x = 0.75, legend_y = 0.85) {
  
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
  
  # Highlight VAR at smoothing window 4
  highlight_rmse <- df_long %>%
    filter(Metric == "RMSE", Model == "VAR", Smoothing == "4")
  
  highlight_mae <- df_long %>%
    filter(Metric == "MAE", Model == "VAR", Smoothing == "4")
  
  # Colors
  cols <- c("VAR" = col_var, "AR" = col_ar, "Naive" = col_naive)
  
  # --------------------------------
  # Helper theme with legend inside
  # --------------------------------
  theme_inside <- function() {
    theme_minimal(base_size = base_size) +
      theme(
        legend.position = c(legend_x, legend_y),
        legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.key = element_rect(fill = alpha("white", 0.7), color = NA),
        legend.text = element_text(size = legend_size),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, size = title_size),
        axis.title.x = element_text(size = axis_label_size,
                                    margin = margin(t = 15)),  # space above x-axis label
        axis.title.y = element_text(size = axis_label_size,
                                    margin = margin(r = 15)),  # space to the right of y-axis label
        axis.text.x = element_text(size = axis_tick_size),
        axis.text.y = element_text(size = axis_tick_size)
      )
  }
  
  # -----------------------------
  # RMSE PLOT
  # -----------------------------
  p_rmse <- df_long %>%
    filter(Metric == "RMSE") %>%
    ggplot(aes(x = Smoothing, y = Value,
               color = Model, shape = Model, group = Model)) +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    geom_point(data = highlight_rmse,
               aes(x = Smoothing, y = Value),
               inherit.aes = FALSE,
               color = "orange", shape = 1, stroke = 1.5,
               size = highlight_size) +
    scale_color_manual(values = cols) +
    scale_y_continuous(breaks = c(1.75, 2, 2.25, 2.5, 2.75, 3, 3.25)) +  # <-- set RMSE y-axis ticks
    labs(
      x = "Average moving window (weeks)",
      y = "Positive slope-weighted RMSE",
      title = 'A). Sensitivity Analysis - Slope-weighted RMSE'
    ) +
    theme_inside() +
    coord_cartesian(ylim = c(1.75, 3.25)) #xlim = c(1, 5), 
  
  # -----------------------------
  # MAE PLOT
  # -----------------------------
  p_mae <- df_long %>%
    filter(Metric == "MAE") %>%
    ggplot(aes(x = Smoothing, y = Value,
               color = Model, shape = Model, group = Model)) +
    geom_line(size = line_size) +
    geom_point(size = point_size) +
    geom_point(data = highlight_mae,
               aes(x = Smoothing, y = Value),
               inherit.aes = FALSE,
               color = "orange", shape = 1, stroke = 1.5,
               size = highlight_size) +
    scale_color_manual(values = cols) +
    scale_y_continuous(breaks = c(1.25, 1.5, 1.75, 2, 2.25, 2.5)) +  # <-- set MAE y-axis ticks
    labs(
      x = "Average moving window (weeks)",
      y = "Positive slope-weighted MAE",
      title = 'B). Sensitivity Analysis - Slope-weighted MAE'
    ) +
    theme_inside() +
    theme(legend.position = "none") +   # hide second legend 
    coord_cartesian(ylim = c(1.25, 2.5))# hide second legend xlim = c(1, 5), 
  
  # -----------------------------
  # Combine plots into 2-row, 1-column layout
  # -----------------------------
  combined_plot <- (p_rmse + theme(plot.margin = margin(b = 20))) /
    (p_mae  + theme(plot.margin = margin(t = 20))) +
    plot_layout(ncol = 1)
  
  #combined_plot <- p_rmse / p_mae + plot_layout(ncol = 1, heights = c(1, 1))
  
  print(combined_plot)
  #return(combined_plot)
}

PLOT_CASES_FORECASTS_CI <- function(data_ts, 
                                        df_preds_var, df_preds_ar, df_preds_naive,
                                        list_ordered_jur, title_plot, 
                                        n_col_plot = 2, ymax = 17,
                                        col_var = "blue", col_ar = "darkgreen",
                                        col_naive = "magenta", 
                                        three_figures = TRUE, point_size = 2.0) {
  
  jur_labels <- GET_JUR_LABELS()
  
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start), Method = "VAR")
  df_preds_ar <- df_preds_ar %>% mutate(date_week_start = as.Date(date_week_start), Method = "AR")
  df_preds_naive <- df_preds_naive %>% mutate(date_week_start = as.Date(date_week_start), Method = "Naive", Lower_CI = NA, Upper_CI = NA)
  
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  df_preds_all <- bind_rows(df_preds_var, df_preds_ar, df_preds_naive) %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Method, Lower_CI, Upper_CI) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  df_plot <- bind_rows(df_true_long, df_preds_all) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_labels[list_ordered_jur]),
      Method = factor(Method, levels = c("VAR", "AR", "Naive"), labels = c("VAR-Lasso", "AR-Lasso", "Naive"))
    )
  
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 4)]
  
  plot_subset <- function(subset_jur) {
    ggplot() +
      
      # Ribbon for AR-Lasso (light green)
      geom_ribbon(
        data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Method == "AR-Lasso", !is.na(Lower_CI)),
        aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI),
        fill = "lightgreen", alpha = 0.3
      ) +
      
      # Ribbon for VAR-Lasso (light blue)
      geom_ribbon(
        data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Method == "VAR-Lasso", !is.na(Lower_CI)),
        aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI),
        fill = "cyan") + #, alpha = 0.3 +
      
      # Reported cases
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
                color = "black", size = 0.9) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "True"),
                 aes(x = date_week_start, y = Cases, shape = "Reported cases"),
                 color = "black", size = point_size) +
      
      # Predicted lines/points
      geom_line(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                aes(x = date_week_start, y = Cases, color = Method), size = 0.7) +
      geom_point(data = df_plot %>% filter(Jurisdiction_orig %in% subset_jur, Source == "Predicted"),
                 aes(x = date_week_start, y = Cases, color = Method), size = point_size) +
      
      # Scales
      scale_color_manual(name = "", values = c("VAR-Lasso" = col_var, "AR-Lasso" = col_ar, "Naive" = col_naive)) +
      scale_linetype_manual(name = "", values = c("Reported cases" = "solid")) +
      scale_shape_manual(name = "", values = c("Reported cases" = 16)) +
      scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
      
      facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", ncol = n_col_plot) +
      labs(x = "Month", y = "Case counts", title = title_plot) +
      coord_cartesian(ylim = c(0, ymax)) +
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
  
  if (!three_figures) {
    return(plot_subset(list_ordered_jur))
  } else {
    jur_chunks <- split(list_ordered_jur, ceiling(seq_along(list_ordered_jur)/4))
    plot_list <- lapply(seq_along(jur_chunks), function(i) {
      subset_jur <- jur_chunks[[i]]
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
          Method = "VAR-Lasso",
          Lower_CI = NA,
          Upper_CI = NA
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


#**********************************
#* PLOT CI
PLOT_VAR_LASSO_FORECAST_CI_2x4 <- function(data_ts,  df_preds_var, list_ordered_jur, title_plot, 
                                           ymax, title_size = 19, sub_title_size = 16, 
                                           font_base_size = 15, point_size = 1.5, 
                                           line_size = 0.75, n_row_plot = 4,
                                           n_col_plot = 2, col_var = "blue") {
  
  # --- Step 1: Ensure date format ---
  jur_labels <- GET_JUR_LABELS()
  data_ts <- data_ts %>% mutate(date_week_start = as.Date(date_week_start))
  df_preds_var <- df_preds_var %>% mutate(date_week_start = as.Date(date_week_start))
  
  jur_levels <- list_ordered_jur # jur_labels[unlist(list_ordered_jur)]
  
  
  # --- Step 2: Format true cases ---
  df_true_long <- data_ts %>%
    pivot_longer(cols = -c(Week_Number, date_week_start), 
                 names_to = "Jurisdiction", values_to = "Cases") %>%
    mutate(Source = "True")
  
  # --- Step 3: Format VAR predictions ---
  df_preds_long <- df_preds_var %>%
    dplyr::select(date_week_start, Week_Number, Jurisdiction, Predicted, Lower_CI, Upper_CI) %>%
    rename(Cases = Predicted) %>%
    mutate(Source = "Predicted")
  
  # --- Step 4: Combine and filter ---
  df_plot <- bind_rows(df_true_long, df_preds_long) %>%
    filter(Jurisdiction %in% list_ordered_jur) %>%
    mutate(
      Source = factor(Source, levels = c("True", "Predicted")),
      Jurisdiction_orig = Jurisdiction,
      Jurisdiction = factor(jur_labels[Jurisdiction], levels = jur_levels)
    )
  
  # --- Step 5: Date breaks ---
  date_seq <- df_plot %>% distinct(date_week_start) %>% arrange(date_week_start) %>% pull(date_week_start)
  x_breaks <- date_seq[seq(1, length(date_seq), by = 8)]
  
  # --- Step 6: Plot ---
  ggplot() +
    # Ribbon for VAR-Lasso CI
    geom_ribbon(
      data = df_plot %>% filter(Source == "Predicted", !is.na(Lower_CI)),
      aes(x = date_week_start, ymin = Lower_CI, ymax = Upper_CI),
      fill = col_var, alpha = 0.2
    ) +
    
    # Reported cases
    geom_line(
      data = df_plot %>% filter(Source == "True"),
      aes(x = date_week_start, y = Cases, linetype = "Reported cases"),
      color = "black", size = line_size
    ) +
    geom_point(
      data = df_plot %>% filter(Source == "True"),
      aes(x = date_week_start, y = Cases, shape = "Reported cases"),
      color = "black", size = point_size
    ) +
    
    # Predicted cases
    geom_line(
      data = df_plot %>% filter(Source == "Predicted"),
      aes(x = date_week_start, y = Cases, linetype = "VAR-Lasso forecast"),
      color = col_var, size = line_size
    ) +
    geom_point(
      data = df_plot %>% filter(Source == "Predicted"),
      aes(x = date_week_start, y = Cases, shape = "VAR-Lasso forecast"),
      color = col_var, size = point_size
    ) +
    
    ylim(0, ymax) +
    scale_linetype_manual(
      name = "", 
      values = c("Reported cases" = "solid", "VAR-Lasso forecast" = "dashed")
    ) +
    scale_shape_manual(
      name = "", 
      values = c("Reported cases" = 16, "VAR-Lasso forecast" = 17)
    ) +
    scale_x_date(breaks = x_breaks, date_labels = "%m/%d/%y") +
    facet_wrap2(~Jurisdiction, scales = "free_y", axes = "all", nrow = n_row_plot, ncol = n_col_plot) +
    labs(
      x = "Date (week start date of reported cases)",
      y = "Case Counts",
      title = title_plot
    ) +
    theme_minimal(base_size = font_base_size) +
    theme(
      plot.title = element_text(size = title_size, hjust = 0.5),
      legend.position = "bottom",
      legend.text = element_text(size = font_base_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = sub_title_size),
      panel.spacing.y = unit(0.2, "lines"),
      legend.key.width = unit(2, "cm")
    )
}

