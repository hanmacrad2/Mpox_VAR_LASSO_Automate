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


#VAR MODEL
# VAR_LASSO_FORECAST_TWO_STEP_2 <- function(train_data_ts, future_data_ts, future_real_ts, ROLL_WINDOW,
#                                         n_lags = 25) {
#   'Recursive 2-step-ahead VAR Lasso forecasts — saving only the t+2 prediction as a dataframe'
#   
#   print(paste0('N States: ', (ncol(train_data_ts) - 2)))
#   print(paste0('Roll window: ', ROLL_WINDOW))
#   print(paste0('N lags: ', n_lags))
#   print(paste0('N step_ahead: ', 2))
#   
#   week_number_forecast <- future_data_ts$Week_Number
#   list_date_week_start <- as.Date(future_data_ts$date_week_start)
#   train_data_ts <- train_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
#   future_data_ts <- future_data_ts %>% dplyr::select(-Week_Number, - date_week_start)
#   future_real_ts = future_real_ts  %>% dplyr::select(-Week_Number, - date_week_start)
#   
#   juris_names <- colnames(train_data_ts)
#   n_forecasts <- nrow(future_data_ts) - 1
#   
#   results_df <- data.frame()
#   df_coeffs <- data.frame()
#   train_week_t <- train_data_ts 
#   
#   for (week_t in 1:n_forecasts) { 
#     cat('Forecast', week_t, '=> predicting week:', week_number_forecast[week_t + 1], '\n')
#     
#     # 1. Difference training data
#     train_diff <- diff(as.matrix(train_week_t), differences = 1)
#     
#     # 2. Fit VAR Lasso model on differenced data
#     model_tmp <- constructModel(train_diff, p = n_lags, struct = 'Basic',
#                                 gran = c(0.05), ownlambdas = TRUE, T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
#     
#     cv_model_tmp <- cv.BigVAR(model_tmp)
#     #residuals <- cv_model_tmp@resids   
#     
#     # 3. Save non-zero coefficients
#     coefficients <- coef(cv_model_tmp)
#     df_coeffs_t <- EXTRACT_NONZERO_LASSO_COEFFS(coefficients, juris_names)
#     df_coeffs_t$Week_Number <- rep(week_number_forecast[week_t], nrow(df_coeffs_t))
#     df_coeffs <- rbind(df_coeffs, df_coeffs_t)
#     
#     # 4. Step 1 forecast (t+1)
#     last_obs <- as.numeric(tail(train_week_t, 1))
#     pred_diff <- predict(cv_model_tmp, n.ahead = 2)
#     #print(paste0('pred_diff_t1: ', pred_diff_t1))
#     level_t2 <- last_obs + as.numeric(pred_diff)
#     
#     # # 5. Step 2 forecast (t+2)
#     # extended_series <- rbind(train_week_t, level_t1)
#     # extended_diff <- diff(as.matrix(extended_series), differences = 1)
#     # 
#     # model_tmp2 <- constructModel(extended_diff, p = n_lags, struct = 'Basic',
#     #                              gran = c(0.05), ownlambdas = TRUE, T1 = floor(nrow(train_diff) * 0.65), verbose = FALSE)
#     # 
#     # cv_model_tmp_2 <- cv.BigVAR(model_tmp2)
#     # 
#     # pred_diff_t2 <- predict(cv_model_tmp_2, n.ahead = 1)
#     # level_t2 <- level_t1 + as.numeric(pred_diff_t2)
#     
#     # 6. Get actual values
#     actual_values <- as.numeric(future_real_ts[week_t + 1, ])
#     
#     # 7. Save results to long-format dataframe
#     df_t <- data.frame(
#       Week_Number = rep(week_number_forecast[week_t + 1], length(juris_names)),
#       date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), length(juris_names)),
#       Jurisdiction = juris_names,
#       Predicted = pmax(level_t2, 0),  # clip negatives
#       Actual = actual_values,
#       bic_value = cv_model_tmp@BICMSFE  
#     )
#     
#     results_df <- bind_rows(results_df, df_t)
#     
#     # 8. Extend training set
#     train_week_t <- rbind(train_data_ts, head(future_data_ts, week_t))
#   }
#   
#   results_df <- results_df %>%
#     mutate(error = Actual - Predicted)
#   print(summary(results_df$error))
#   
#   # Also create wide-format versions (optional)
#   df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
#     arrange(Week_Number)
#   df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
#     arrange(Week_Number)
#   
#   return(list(
#     df_pred_results = results_df,
#     preds = df_pred,
#     true = df_true,
#     df_coeffs = df_coeffs,
#     coeffs_final = coef(cv_model_tmp)
#   ))
# }


