#BOOTSTRAP_VAR_2STEP
 
VAR_LASSO_FORECAST_TWO_STEP_CI <- function(train_data_ts, future_data_ts, future_real_ts, ROLL_WINDOW,
                                           n_lags = 25, alpha = 0.05) {
  'Recursive 2-step-ahead VAR Lasso forecasts with propagated variance for t+2'
  
  print(paste0('N States: ', (ncol(train_data_ts) - 2)))
  print(paste0('Roll window: ', ROLL_WINDOW))
  print(paste0('N lags: ', n_lags))
  print(paste0('N step_ahead: ', 2))
  print(paste0('Alpha: ', alpha))
  
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts <- train_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_ts <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  juris_names <- colnames(train_data_ts)
  n_forecasts <- nrow(future_data_ts) - 1
  
  results_df <- data.frame()
  df_coeffs <- data.frame()
  train_week_t <- train_data_ts 
  
  for (week_t in 1:n_forecasts) { 
    cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
    
    # 1. Difference training data
    train_diff <- diff(as.matrix(train_week_t), differences = 1)
    
    # 2. Fit VAR Lasso model on differenced data
    model_tmp <- constructModel(train_diff, p = n_lags, struct = 'Basic',
                                gran = c(0.05), ownlambdas = TRUE, 
                                T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp <- cv.BigVAR(model_tmp)
    
    # 3. Save non-zero coefficients
    coefficients <- coef(cv_model_tmp)
    df_coeffs_t <- EXTRACT_NONZERO_LASSO_COEFFS(coefficients, juris_names)
    df_coeffs_t$Week_Number <- rep(week_number_forecast[week_t], nrow(df_coeffs_t))
    df_coeffs <- rbind(df_coeffs, df_coeffs_t)
    
    # 4. Step 1 forecast (t+1) with CI
    last_obs <- as.numeric(tail(train_week_t, 1))
    pred_diff_t1_ci <- predict(cv_model_tmp, n.ahead = 1, confint = TRUE)
    
    # Extract difference forecasts and bounds
    diff_t1_mean <- as.numeric(pred_diff_t1_ci$forecast)
    diff_t1_lower <- as.numeric(pred_diff_t1_ci$lower)
    diff_t1_upper <- as.numeric(pred_diff_t1_ci$upper)
    
    # Convert to levels
    level_t1_mean <- last_obs + diff_t1_mean
    level_t1_lower <- last_obs + diff_t1_lower
    level_t1_upper <- last_obs + diff_t1_upper
    
    # Step 1 variance in DIFFERENCES
    z <- qnorm(1 - alpha / 2)
    var_diff_t1 <- ((diff_t1_upper - diff_t1_lower) / (2 * z))^2
    
    # 5. Step 2 forecast (t+2) - use MEAN of t+1 for point forecast
    extended_series <- rbind(train_week_t, level_t1_mean)
    extended_diff <- diff(as.matrix(extended_series), differences = 1)
    
    model_tmp2 <- constructModel(extended_diff, p = n_lags, struct = 'Basic',
                                 gran = c(0.05), ownlambdas = TRUE, 
                                 T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp_2 <- cv.BigVAR(model_tmp2)
    pred_diff_t2_ci <- predict(cv_model_tmp_2, n.ahead = 1, confint = TRUE)
    
    # Extract difference forecasts
    diff_t2_mean <- as.numeric(pred_diff_t2_ci$forecast)
    diff_t2_lower <- as.numeric(pred_diff_t2_ci$lower)
    diff_t2_upper <- as.numeric(pred_diff_t2_ci$upper)
    
    # Step 2 variance in DIFFERENCES (conditional on t+1)
    var_diff_t2_cond <- ((diff_t2_upper - diff_t2_lower) / (2 * z))^2
    
    # 6. PROPAGATE VARIANCE
    # Total variance for cumulative difference (Δy_t+1 + Δy_t+2):
    # Assuming independence of forecast errors across time:
    # Var(Δy_t+1 + Δy_t+2) = Var(Δy_t+1) + Var(Δy_t+2|y_t+1)
    var_diff_cumulative <- var_diff_t1 + var_diff_t2_cond
    sd_diff_cumulative <- sqrt(var_diff_cumulative)
    
    # 7. Final t+2 forecast in LEVELS
    level_t2_mean <- last_obs + diff_t1_mean + diff_t2_mean
    
    # CI for cumulative difference
    diff_cumulative_lower <- (diff_t1_mean + diff_t2_mean) - z * sd_diff_cumulative
    diff_cumulative_upper <- (diff_t1_mean + diff_t2_mean) + z * sd_diff_cumulative
    
    # Convert to level CIs
    level_t2_lower <- last_obs + diff_cumulative_lower
    level_t2_upper <- last_obs + diff_cumulative_upper
    
    # 8. Actual values
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    
    # 9. Save results
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), length(juris_names)),
      Jurisdiction = juris_names,
      Predicted = pmax(level_t2_mean, 0),
      Lower_CI = pmax(level_t2_lower, 0),
      Upper_CI = pmax(level_t2_upper, 0),
      Actual = actual_values,
      bic_value = cv_model_tmp@BICMSFE,
      stringsAsFactors = FALSE
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # 10. Extend training set
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  # Wide-format
  df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
    arrange(Week_Number)
  df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
    arrange(Week_Number)
  
  return(list(
    df_pred_results = results_df,
    preds = df_pred,
    true = df_true,
    df_coeffs = df_coeffs,
    coeffs_final = coef(cv_model_tmp_2)
  ))
}


#*****************

AR_model_CI <- function(train_data_ts, 
                        future_data_ts, future_real_ts,
                        list_jur, n_step_ahead = 2, alpha = 0.05) {
  
  # ---- DATA PREPARATION ----
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  
  # Remove non-jurisdictional columns
  train_week_t <- train_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_data_clean <- future_data_ts %>% dplyr::select(-Week_Number, -date_week_start)
  future_real_clean <- future_real_ts %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Number of forecasts
  n_forecasts <- nrow(future_data_ts) - (n_step_ahead - 1)
  
  # Results storage
  results_list <- vector("list", n_forecasts)
  
  # ---- LOOP OVER WEEKS ----
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    
    # Force matrix
    Y <- as.matrix(train_week_t)
    
    # Ensure numeric
    if (!is.numeric(Y)) {
      Y <- apply(Y, 2, as.numeric)
    }
    
    if (is.null(colnames(Y))) {
      colnames(Y) <- list_jur
    }
    
    J <- ncol(Y)
    out_week <- vector("list", J)
    
    # ---- LOOP OVER JURISDICTIONS ----
    for (i in seq_len(J)) {
      
      jur <- colnames(Y)[i]
      
      # Extract column
      y <- Y[, i, drop = TRUE]
      
      # Check for NAs
      if (any(is.na(y))) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "contains NAs. Removing them."))
        y <- y[!is.na(y)]
      }
      
      # Check sufficient data
      if (length(y) < 3) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "has insufficient data. Skipping."))
        next
      }
      
      # ---- STEP 1: Forecast t+1 WITH CI ----
      last_obs <- tail(y, 1)
      dy <- diff(y)
      
      if (length(dy) < 2) {
        warning(paste("Week", week_t, "- Jurisdiction", jur, "has insufficient differenced data. Skipping."))
        next
      }
      
      # Fit AR(1) on differences
      fit1 <- tryCatch({
        arima(dy, order = c(1, 0, 0))
      }, error = function(e) {
        warning(paste("Week", week_t, "- ARIMA failed for", jur, ":", e$message))
        return(NULL)
      })
      
      if (is.null(fit1)) next
      
      # Forecast difference for t+1 with uncertainty
      fc1 <- predict(fit1, n.ahead = 1)
      z <- qnorm(1 - alpha / 2)
      
      diff_t1_mean <- fc1$pred[1]
      diff_t1_sd <- fc1$se[1]
      
      diff_t1_lower <- diff_t1_mean - z * diff_t1_sd
      diff_t1_upper <- diff_t1_mean + z * diff_t1_sd
      
      # Convert to levels
      level_t1_mean <- last_obs + diff_t1_mean
      level_t1_lower <- last_obs + diff_t1_lower
      level_t1_upper <- last_obs + diff_t1_upper
      
      # ---- STEP 2: Forecast t+2 WITH PROPAGATED UNCERTAINTY ----
      # Extend series with t+1 prediction (use mean)
      y_extended <- c(y, level_t1_mean)
      dy_extended <- diff(y_extended)
      
      # Fit AR(1) on extended differences
      fit2 <- tryCatch({
        arima(dy_extended, order = c(1, 0, 0))
      }, error = function(e) {
        warning(paste("Week", week_t, "- ARIMA step 2 failed for", jur, ":", e$message))
        return(NULL)
      })
      
      if (is.null(fit2)) next
      
      # Forecast difference for t+2
      # PROPER APPROACH: Use direct h=2 forecast from original model
      # This accounts for AR correlation structure
      fc_direct <- predict(fit1, n.ahead = 2)
      
      # The 2-step ahead forecast variance from ARIMA already accounts
      # for the correlation between forecast errors at different horizons
      diff_t1_mean <- fc_direct$pred[1]
      diff_t2_cumulative_mean <- sum(fc_direct$pred[1:2])
      diff_t2_cumulative_sd <- fc_direct$se[2]  # This is the 2-step SE
      
      # Alternative manual calculation (for AR(1) specifically):
      # If φ is the AR coefficient, then:
      # Var(ε_t+1 + φ*ε_t+2) = σ^2(1 + φ^2) for AR(1)
      # But predict() already does this correctly
      
      diff_t2_lower <- diff_t2_cumulative_mean - z * diff_t2_cumulative_sd
      diff_t2_upper <- diff_t2_cumulative_mean + z * diff_t2_cumulative_sd
      
      # Back to levels
      level_mean <- last_obs + diff_t2_cumulative_mean
      level_lower <- last_obs + diff_t2_lower
      level_upper <- last_obs + diff_t2_upper
      
      # Get actual value
      actual_value <- as.numeric(future_real_clean[week_t + 1, i])
      
      # Store results
      out_week[[i]] <- data.frame(
        Week_Number = week_number_forecast[week_t + 1],
        date_week_start = list_date_week_start[week_t + 1],
        Jurisdiction = jur,
        Predicted = pmax(as.numeric(level_mean), 0),
        Lower_CI = pmax(as.numeric(level_lower), 0),
        Upper_CI = pmax(as.numeric(level_upper), 0),
        Actual = actual_value,
        stringsAsFactors = FALSE
      )
    }
    
    # Combine jurisdictions for this week
    out_week <- out_week[!sapply(out_week, is.null)]
    
    if (length(out_week) > 0) {
      results_list[[week_t]] <- do.call(rbind, out_week)
    }
    
    # ---- EXTEND TRAINING SET ----
    train_week_t <- rbind(train_week_t, future_data_clean[week_t, ])
  }
  
  # ---- COMBINE ALL RESULTS ----
  results_list <- results_list[!sapply(results_list, is.null)]
  
  if (length(results_list) == 0) {
    stop("No valid forecasts produced. Check your data for NAs or non-numeric values.")
  }
  
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL
  
  # Add error column
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  
  return(results_df)
}

