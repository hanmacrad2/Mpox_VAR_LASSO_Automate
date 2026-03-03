#' Output Utilities for ResilientSims Integration
#'
#' Functions to write results in ResilientSims-compatible format.

source("file_utils.R")

#' Write forecast CSV for portal
#'
#' Creates mpox_forecasts.csv with historical observations and forecast predictions
#' for a single jurisdiction.
#'
#' @param data_long Original long-format case data
#' @param forecasts Forecast results from run_all_forecasts()
#' @param portal_jurisdiction Jurisdiction to include in output
#' @param output_dir Output directory
#'
#' @return Path to the output file
write_forecasts_csv <- function(data_long, forecasts, portal_jurisdiction, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- get_output_dir(default = getwd())
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Historical observations for this jurisdiction
  jur_data <- data_long[data_long$Jurisdiction == portal_jurisdiction, ]
  jur_data <- jur_data[order(jur_data$date_week_start), ]

  obs_df <- data.frame(
    epiweek_start = as.Date(jur_data$date_week_start),
    cases = jur_data$Cases,
    type = "observation",
    method = NA_character_,
    predicted = NA_real_,
    lower_ci = NA_real_,
    upper_ci = NA_real_,
    stringsAsFactors = FALSE
  )

  # Forecasts for this jurisdiction
  jur_forecasts <- forecasts[forecasts$jurisdiction == portal_jurisdiction, ]

  if (nrow(jur_forecasts) > 0) {
    forecast_df <- data.frame(
      epiweek_start = as.Date(jur_forecasts$date_week_start),
      cases = NA_real_,
      type = "forecast",
      method = jur_forecasts$method,
      predicted = round(jur_forecasts$predicted, 2),
      lower_ci = round(jur_forecasts$lower_ci, 2),
      upper_ci = round(jur_forecasts$upper_ci, 2),
      stringsAsFactors = FALSE
    )
    output_df <- rbind(obs_df, forecast_df)
  } else {
    output_df <- obs_df
  }

  output_file <- file.path(output_dir, "mpox_forecasts.csv")
  write.csv(output_df, file = output_file, row.names = FALSE, na = "")

  n_obs <- sum(output_df$type == "observation")
  n_forecasts <- sum(output_df$type == "forecast")
  message(sprintf("Saved forecasts to: %s", output_file))
  message(sprintf("  %d observations, %d forecast rows", n_obs, n_forecasts))

  output_file
}


#' Write results JSON for ResilientSims
#'
#' Creates results.json with summary metadata and output manifest.
#'
#' @param forecast_data Prepared forecast data from prepare_forecast_data()
#' @param portal_jurisdiction Jurisdiction included in output
#' @param output_dir Output directory
#'
#' @return Path to the output file
write_results_json <- function(forecast_data, portal_jurisdiction, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- get_output_dir(default = getwd())
  }

  results <- list(
    summary = list(
      forecast_origin_date = as.character(forecast_data$last_data_date),
      forecast_date = as.character(forecast_data$forecast_date),
      jurisdiction = portal_jurisdiction,
      methods = c("VAR_LASSO", "AR_LASSO", "NAIVE")
    ),
    plots = list(),
    output_manifest = list("mpox_forecasts.csv"),
    history = list()
  )

  output_file <- file.path(output_dir, "results.json")
  jsonlite::write_json(results, output_file, auto_unbox = TRUE, pretty = TRUE)

  message(sprintf("Saved results.json to: %s", output_file))
  output_file
}


#' Write all outputs
#'
#' Convenience function to write both CSV and JSON outputs.
#'
#' @param data_long Original long-format case data
#' @param forecasts Forecast results from run_all_forecasts()
#' @param forecast_data Prepared forecast data from prepare_forecast_data()
#' @param portal_jurisdiction Jurisdiction for output
#' @param output_dir Output directory
#'
#' @return List with paths to both output files
write_outputs <- function(data_long, forecasts, forecast_data, portal_jurisdiction, output_dir = NULL) {
  csv_path <- write_forecasts_csv(data_long, forecasts, portal_jurisdiction, output_dir)
  json_path <- write_results_json(forecast_data, portal_jurisdiction, output_dir)

  list(
    csv = csv_path,
    json = json_path
  )
}


#' Check portal plot
#'
#' Test function to load and visualize the portal CSV output.
#' Plots historical observations and forecasts with confidence intervals.
#'
#' @param csv_path Path to mpox_forecasts.csv (default: output_dir/mpox_forecasts.csv)
#' @param n_obs Number of recent observations to plot (default: NA, plots all)
#' @param show_var Show VAR-Lasso forecasts (default: TRUE)
#' @param show_ar Show AR-Lasso forecasts (default: TRUE)
#' @param show_naive Show Naive forecasts (default: TRUE)
#'
#' @return ggplot object
check_portal_plot <- function(csv_path = NULL,
                               n_obs = NA,
                               show_var = TRUE,
                               show_ar = TRUE,
                               show_naive = TRUE) {
  if (is.null(csv_path)) {
    csv_path <- file.path(get_output_dir(default = getwd()), "mpox_forecasts.csv")
  }

  if (!file.exists(csv_path)) {
    stop(sprintf("CSV file not found: %s", csv_path))
  }

  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  df$epiweek_start <- as.Date(df$epiweek_start)

  # Split observations and forecasts
  obs_df <- df[df$type == "observation", ]
  forecast_df <- df[df$type == "forecast", ]

  # Restrict observations if n_obs specified
 if (!is.na(n_obs) && nrow(obs_df) > n_obs) {
    obs_df <- tail(obs_df, n_obs)
  }

  # Filter forecast methods
  methods_to_show <- c()
  if (show_var) methods_to_show <- c(methods_to_show, "VAR_LASSO")
  if (show_ar) methods_to_show <- c(methods_to_show, "AR_LASSO")
  if (show_naive) methods_to_show <- c(methods_to_show, "NAIVE")

  forecast_df <- forecast_df[forecast_df$method %in% methods_to_show, ]

  # Colors
  method_colors <- c(
    "VAR_LASSO" = "blue",
    "AR_LASSO" = "darkgreen",
    "NAIVE" = "magenta"
  )

  # Build plot
  p <- ggplot2::ggplot() +
    # Observations
    ggplot2::geom_line(
      data = obs_df,
      ggplot2::aes(x = epiweek_start, y = cases),
      color = "black", linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = obs_df,
      ggplot2::aes(x = epiweek_start, y = cases),
      color = "black", size = 2
    )

  # Add forecast ribbons (CI) for VAR and AR
  for (m in intersect(methods_to_show, c("VAR_LASSO", "AR_LASSO"))) {
    m_df <- forecast_df[forecast_df$method == m & !is.na(forecast_df$lower_ci), ]
    if (nrow(m_df) > 0) {
      p <- p + ggplot2::geom_ribbon(
        data = m_df,
        ggplot2::aes(x = epiweek_start, ymin = lower_ci, ymax = upper_ci),
        fill = method_colors[m], alpha = 0.2
      )
    }
  }

  # Add forecast lines and points
  if (nrow(forecast_df) > 0) {
    p <- p +
      ggplot2::geom_line(
        data = forecast_df,
        ggplot2::aes(x = epiweek_start, y = predicted, color = method),
        linewidth = 0.8, linetype = "dashed"
      ) +
      ggplot2::geom_point(
        data = forecast_df,
        ggplot2::aes(x = epiweek_start, y = predicted, color = method),
        size = 2.5
      ) +
      ggplot2::scale_color_manual(values = method_colors, name = "Forecast")
  }

  p <- p +
    ggplot2::labs(
      x = "Week",
      y = "Cases",
      title = "Mpox Forecasts"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  print(p)
  invisible(p)
}
