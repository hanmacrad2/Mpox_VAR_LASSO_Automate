#*******************************************
#* VAR MODEL FUNCTIONS
#*****************************************

VAR_LASSO_FORECAST_TWO_STEP <- function(train_data_ts, future_data_ts, future_real_ts, ROLL_WINDOW,
                                        n_lags = 25) {
  'Recursive 2-step-ahead VAR Lasso forecasts — saving only the t+2 prediction as a dataframe'
  
  print(paste0('N States: ', (ncol(train_data_ts) - 2)))
  print(paste0('Roll window: ', ROLL_WINDOW))
  print(paste0('N lags: ', n_lags))
  print(paste0('N step_ahead: ', 2))
  
  week_number_forecast <- future_data_ts$Week_Number
  list_date_week_start <- as.Date(future_data_ts$date_week_start)
  train_data_ts <- train_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
  future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
  future_real_ts = future_real_ts  %>% dplyr::select(-Week_Number, - date_week_start)
  
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
                                gran = c(0.05), ownlambdas = TRUE, T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp <- cv.BigVAR(model_tmp)
    #residuals <- cv_model_tmp@resids   
    
    # 3. Save non-zero coefficients
    coefficients <- coef(cv_model_tmp)
    df_coeffs_t <- EXTRACT_NONZERO_LASSO_COEFFS(coefficients, juris_names)
    df_coeffs_t$Week_Number <- rep(week_number_forecast[week_t], nrow(df_coeffs_t))
    df_coeffs <- rbind(df_coeffs, df_coeffs_t)
    
    # 4. Step 1 forecast (t+1)
    last_obs <- as.numeric(tail(train_week_t, 1))
    pred_diff_t1 <- predict(cv_model_tmp, n.ahead = 1)
    #print(paste0('pred_diff_t1: ', pred_diff_t1))
    level_t1 <- last_obs + as.numeric(pred_diff_t1)
    
    # 5. Step 2 forecast (t+2)
    extended_series <- rbind(train_week_t, level_t1)
    extended_diff <- diff(as.matrix(extended_series), differences = 1)
    
    model_tmp2 <- constructModel(extended_diff, p = n_lags, struct = 'Basic',
                                 gran = c(0.05), ownlambdas = TRUE, T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
    
    cv_model_tmp_2 <- cv.BigVAR(model_tmp2)
    
    pred_diff_t2 <- predict(cv_model_tmp_2, n.ahead = 1)
    level_t2 <- level_t1 + as.numeric(pred_diff_t2)
    
    # 6. Get actual values
    actual_values <- as.numeric(future_real_ts[week_t + 1, ])
    
    # 7. Save results to long-format dataframe
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + 1], length(juris_names)),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), length(juris_names)),
      Jurisdiction = juris_names,
      Predicted = pmax(level_t2, 0),  # clip negatives
      Actual = actual_values,
      bic_value = cv_model_tmp@BICMSFE  
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # 8. Extend training set
    train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  print(summary(results_df$error))
  
  # Also create wide-format versions (optional)
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

#EXTRACT COEFFICIENTS
EXTRACT_NONZERO_LASSO_COEFFS <- function(coef_matrix, list_ordered_jur = NULL) {
  # Convert to data.frame if it's a matrix
  
  if (is.matrix(coef_matrix)) {
    coef_df <- as.data.frame(coef_matrix)
  } else {
    coef_df <- coef_matrix
  }
  
  # Add response as a column before melting
  coef_df$Response <- rownames(coef_df)
  
  # Reshape to long format
  coef_long <- reshape2::melt(coef_df, id.vars = "Response", 
                              variable.name = "Predictor", value.name = "Coefficient")
  
  # Filter to non-zero coefficients
  nonzero_coefs <- coef_long[coef_long$Coefficient != 0, ]
  
  # Rename Y1, Y2, etc. if list_ordered_jur is provided
  if (!is.null(list_ordered_jur)) {
    # Replace responses
    response_map <- setNames(list_ordered_jur, paste0("Y", seq_along(list_ordered_jur)))
    nonzero_coefs$Response <- response_map[nonzero_coefs$Response]
    
    # Replace predictors: detect Y[1-9]+ at start of string, e.g., Y3L2
    nonzero_coefs$Predictor <- sapply(nonzero_coefs$Predictor, function(pred) {
      if (grepl("^Y\\d+", pred)) {
        match <- regmatches(pred, regexpr("^Y\\d+", pred))
        index <- as.numeric(sub("Y", "", match))
        new_jur <- list_ordered_jur[index]
        sub("^Y\\d+", paste0(new_jur, "_"), pred)
      } else {
        pred
      }
    })
  }
  
  # Sort for readability
  nonzero_coefs <- nonzero_coefs[order(nonzero_coefs$Response, -abs(nonzero_coefs$Coefficient)), ]
  #nonzero_coefs <- nonzero_coefs %>% select(Predictor, Response, Coefficient)
  
  return(nonzero_coefs)
}

