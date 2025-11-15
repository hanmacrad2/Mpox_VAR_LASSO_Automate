#AR MODEL
#library(bigtime)

#*********************************
#* Sparse Auto-regression model
#* *******************************

AR_model_jurs <- function(train_data_ts, future_data_ts, future_real_ts, list_jur,
                     n_lags = 25, n_step_ahead = 2) {
  
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  df_results_ar = data.frame()
  
  for (jur_name in list_jur) {
    
    print(jur_name)
    train_data_ts_jur1 = train_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_data_ts_jur1 = future_data_ts[, c('date_week_start', 'Week_Number', jur_name)]
    future_real_ts_jur1 = future_real_ts[, c('date_week_start', 'Week_Number', jur_name)] # %>% filter(Jurisdiction == jur_name)
    df_result_jur = AR_model(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                         jur_name)
    df_results_ar = rbind(df_results_ar, df_result_jur)
    
  }
  
  return(df_results_ar)
}




AR_model <- function(train_data_ts_jur1, future_data_ts_jur1, future_real_ts_jur1,
                     jur_name, n_lags = 25, n_step_ahead = 2) {
  
  'Simple Naive estimate. Calculates 4 week average of weekly count data and projects n_step_ahead in weeks (2 weeks)'
  # Remove non-jurisdictional columns
  week_number_forecast <- future_data_ts_jur1$Week_Number
  list_date_week_start <- as.Date(future_data_ts_jur1$date_week_start)
  future_real_ts_jur1 <- future_real_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)
  
  # Remove Week_Number and date_week_start from train
  train_week_t <- train_data_ts_jur1 %>% dplyr::select(-Week_Number, -date_week_start)

  #jur_names <- colnames(train_data_ts_jur1)
  n_forecasts <- nrow(future_data_ts_jur1) - (n_step_ahead - 1)
  
  results_df <- data.frame()
  
  for (week_t in 1:n_forecasts) {
    
    print(paste0('week_t: ', week_t))
    
    train_diff <- diff(as.matrix(train_week_t), differences = 1)

    #Model fit
    train_diff_scaled = scale(train_diff)
    #train_diff_scaled = matrix(as.numeric(train_diff_scaled), nrow = nrow(train_diff_scaled))
    mu <- attr(train_diff_scaled, "scaled:center")
    sd_ <- attr(train_diff_scaled, "scaled:scale")
    
    #MODEL
    ar_lasso_model <- sparseVAR(Y= train_diff_scaled, p = n_lags, selection = "cv") #selection = "cv") #, VARpen = "L1",  selection = "cv")
    #browser()
    # 4. Step 1 forecast (t+1)
    last_obs <- as.numeric(tail(train_week_t, 1)) 
    pred_diff_t1_scaled <- directforecast(ar_lasso_model, h = 1)
    pred_diff_t1_unscaled = as.numeric(pred_diff_t1_scaled)*sd_ + mu
    
    level_t1 <- last_obs + as.numeric(pred_diff_t1_unscaled)
    
    # 5. Step 2 forecast (t+2)
    extended_series <- rbind(train_week_t, level_t1)
    
    extended_diff <- diff(as.matrix(extended_series), differences = 1)
    extended_diff_scaled = scale(extended_diff)
    mu <- attr(extended_diff_scaled, "scaled:center")
    sd_ <- attr(extended_diff_scaled, "scaled:scale")
    
    ar_lasso_model_2 = sparseVAR(Y= extended_diff_scaled, p = n_lags, selection = "cv") # VARpen = "L1",  
    pred_diff_t2_scaled <- directforecast(ar_lasso_model_2, h = 1)
    pred_diff_t2_unscaled = as.numeric(pred_diff_t2_scaled)*sd_ + mu
    
    level_t2 <- level_t1 + as.numeric(pred_diff_t2_unscaled)
    
    # Get last observed value at time t
    #last_obs <- as.numeric(tail(train_data_ts_jur1, 1))
    
    #STEP AHEAD: Actual values at t + n
    actual_values <- as.numeric(future_real_ts_jur1[week_t + 1, ])
    
    df_t <- data.frame(
      Week_Number = rep(week_number_forecast[week_t + 1], 1),
      date_week_start = rep(as.Date(list_date_week_start[week_t + 1]), 1),
      #Week_Number = rep(week_number_forecast[week_t + (n_step_ahead - 1)], 1),
      #date_week_start = rep(date_week_start_list[week_t + (n_step_ahead - 1)], 1),
      Jurisdiction = jur_name,
      Predicted = pmax(level_t2, 0),
      Actual = actual_values
    )
    
    results_df <- bind_rows(results_df, df_t)
    
    # Extend the training set forward by 1 week
    #train_data_ts_jur1 <- rbind(train_data_ts_jur1, future_data_ts_jur1[week_t, 1])
    #browser()
    train_week_t <- rbind(train_week_t, future_data_ts_jur1[week_t, 3])
  }
  
  results_df <- results_df %>%
    mutate(error = Actual - Predicted)
  
  # Reshape
  # df_pred <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Predicted) %>%
  #   arrange(Week_Number)
  # df_true <- pivot_wider(results_df, names_from = Jurisdiction, values_from = Actual) %>%
  #   arrange(Week_Number)
  
  return(results_df)
  
}
  
  # return(list(df_pred_results = results_df,
  #             preds = df_pred, true = df_true))


# #TEST
# periods <- 50
# k <- 1
# p <- 1
# burnin <- 100
# Xsim <- simVAR(periods+burnin+1, k, p, max_abs_eigval = 0.8, seed = 123)
# edist <- lag(Xsim$Y)[-1, ] + rnorm(periods + burnin, sd = 0.1)
# Ysim <- simVAR(periods, k , p, max_abs_eigval = 0.5, seed = 789,
#                e_dist = t(edist), burnin = burnin)
# plot(Ysim)
# 
# #MODEL
# x <- scale(Xsim$Y[-(1:(burnin+1))])
# y <- scale(Ysim$Y)
# 
# ARXfit <- sparseVAR(Y=y, selection = "cv")
# lagmatrix(fit=ARXfit)

#AR FIT 2
fit2 = sparseVAR(Y=y, p = n_lags, selection = 'cv') #, VARpen = "L1")

#mODELS
print("Class of Y before sparseVAR:")
print(class(y))
print("Dimensions of Y before sparseVAR (must have 2):")
print(dim(y))
print("Is Y a matrix? (Must be TRUE)")
print(is.matrix(y))
