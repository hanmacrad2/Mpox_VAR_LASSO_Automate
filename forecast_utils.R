#' Forecast Utilities for Mpox VAR-Lasso Pipeline
#'
#' Functions to prepare data and run forecast models.

#' Prepare data for forecasting
#'
#' Smooths the input data, converts to wide format, fills missing weeks with
#' zeros, and creates placeholder future data for the forecast functions.
#'
#' @param data_long Long-format data with Jurisdiction, date_week_start, Cases
#' @param roll_window Rolling mean window for smoothing
#' @param missing_threshold Warn if jurisdiction has more than this fraction missing (default: 0.1)
#'
#' @return List containing:
#'   \itemize{
#'     \item \code{train_data_ts} - Wide-format smoothed training data (NAs filled with 0)
#'     \item \code{future_data_ts} - Placeholder rows for forecast functions
#'     \item \code{future_real_ts} - Placeholder rows (NAs for actuals we don't have)
#'     \item \code{last_data_date} - Last date in training data
#'     \item \code{forecast_date} - Target forecast date (2 weeks ahead)
#'   }
prepare_forecast_data <- function(data_long, roll_window, missing_threshold = 0.1) {
  df_smooth <- GET_SMOOTH_DATA(data_long, roll_window)
  data_ts_smooth <- GET_TS_DATA(df_smooth)

  jurisdiction_cols <- setdiff(names(data_ts_smooth), c("Week_Number", "date_week_start"))
  total_weeks <- nrow(data_ts_smooth)

  # Check for and report missing data, then fill with zeros
  missing_counts <- sapply(jurisdiction_cols, function(jur) {
    sum(is.na(data_ts_smooth[[jur]]))
  })

  if (any(missing_counts > 0)) {
    total_missing <- sum(missing_counts)
    message(sprintf("Filling %d missing week-jurisdiction values with 0", total_missing))

    # Warn about jurisdictions with significant missing data
    significant_missing <- missing_counts[missing_counts > total_weeks * missing_threshold]
    if (length(significant_missing) > 0) {
      warning("Jurisdictions with >", round(missing_threshold * 100), "% missing weeks:")
      for (jur in names(significant_missing)) {
        pct <- round(significant_missing[jur] / total_weeks * 100, 1)
        warning(sprintf("  %s: %d/%d weeks missing (%.1f%%)",
                        jur, significant_missing[jur], total_weeks, pct))
      }
    }

    # Fill NAs with 0
    for (jur in jurisdiction_cols) {
      data_ts_smooth[[jur]][is.na(data_ts_smooth[[jur]])] <- 0
    }
  }

  last_date <- max(as.Date(data_ts_smooth$date_week_start))
  last_week <- max(data_ts_smooth$Week_Number)

  # The core functions produce 2-week-ahead forecasts (t+2).
  # We need 2 rows in future_data_ts: the functions use n_forecasts = nrow - 1,
  # and output the prediction for row 2 (the t+2 date).
  forecast_date <- last_date + 14  # 2 weeks ahead
  future_weeks <- c(last_week + 1, last_week + 2)
  future_dates <- c(last_date + 7, last_date + 14)

  # Future placeholder for training extension (zeros, since we use smoothed data)
  future_data <- data.frame(
    Week_Number = future_weeks,
    date_week_start = future_dates
  )
  for (jur in jurisdiction_cols) {
    future_data[[jur]] <- 0
  }

  # Future placeholder for actuals (NAs, since we don't have real values)
  future_real <- data.frame(
    Week_Number = future_weeks,
    date_week_start = future_dates
  )
  for (jur in jurisdiction_cols) {
    future_real[[jur]] <- NA_real_
  }

  list(
    train_data_ts = data_ts_smooth,
    future_data_ts = future_data,
    future_real_ts = future_real,
    last_data_date = last_date,
    forecast_date = forecast_date,  # Single date, 2 weeks ahead
    jurisdictions = jurisdiction_cols
  )
}


#' Run all forecast methods
#'
#' Runs VAR-Lasso, AR-Lasso, and Naive forecast methods, each producing
#' 2-week-ahead predictions.
#'
#' @param forecast_data List from prepare_forecast_data()
#' @param roll_window Rolling window used for smoothing
#' @param n_lags Maximum lag order for models
#'
#' @return Data frame with columns: date_week_start, Jurisdiction, method,
#'   predicted, lower_ci, upper_ci
run_all_forecasts <- function(forecast_data, roll_window, n_lags) {
  train <- forecast_data$train_data_ts
  future <- forecast_data$future_data_ts
  future_real <- forecast_data$future_real_ts
  jurisdictions <- forecast_data$jurisdictions

  results <- data.frame()

  # VAR-Lasso with CI
  message("Running VAR-Lasso forecast...")
  var_result <- tryCatch({
    VAR_LASSO_FORECAST_TWO_STEP_CI(
      train_data_ts = train,
      future_data_ts = future,
      future_real_ts = future_real,
      ROLL_WINDOW = roll_window,
      n_lags = n_lags,
      alpha = 0.05
    )
  }, error = function(e) {
    warning(sprintf("VAR-Lasso failed: %s", e$message))
    NULL
  })

  if (!is.null(var_result)) {
    var_df <- var_result$df_pred_results
    var_df <- var_df[, c("date_week_start", "Jurisdiction", "Predicted", "Lower_CI", "Upper_CI")]
    var_df$method <- "VAR_LASSO"
    names(var_df) <- c("date_week_start", "jurisdiction", "predicted", "lower_ci", "upper_ci", "method")
    results <- rbind(results, var_df)
  }

  # AR-Lasso with CI
  message("Running AR-Lasso forecast...")
  ar_result <- tryCatch({
    AR_model_CI(
      train_data_ts = train,
      future_data_ts = future,
      future_real_ts = future_real,
      list_jur = jurisdictions,
      n_step_ahead = 2,
      alpha = 0.05
    )
  }, error = function(e) {
    warning(sprintf("AR-Lasso failed: %s", e$message))
    NULL
  })

  if (!is.null(ar_result)) {
    ar_df <- ar_result[, c("date_week_start", "Jurisdiction", "Predicted", "Lower_CI", "Upper_CI")]
    ar_df$method <- "AR_LASSO"
    names(ar_df) <- c("date_week_start", "jurisdiction", "predicted", "lower_ci", "upper_ci", "method")
    results <- rbind(results, ar_df)
  }

  # Naive forecast (no CI)
  message("Running Naive forecast...")
  naive_result <- tryCatch({
    SIMPLE_NAIVE_FORECAST(
      train_data_ts = train,
      future_data_ts = future,
      future_real_ts = future_real,
      n_step_ahead = 2
    )
  }, error = function(e) {
    warning(sprintf("Naive forecast failed: %s", e$message))
    NULL
  })

  if (!is.null(naive_result)) {
    naive_df <- naive_result$df_pred_results
    naive_df <- naive_df[, c("date_week_start", "Jurisdiction", "Predicted")]
    naive_df$lower_ci <- NA_real_
    naive_df$upper_ci <- NA_real_
    naive_df$method <- "NAIVE"
    names(naive_df) <- c("date_week_start", "jurisdiction", "predicted", "lower_ci", "upper_ci", "method")
    results <- rbind(results, naive_df)
  }

  if (nrow(results) == 0) {
    stop("All forecast methods failed")
  }

  results$date_week_start <- as.Date(results$date_week_start)
  results
}
