#PERFORMANCE METRICS

GET_DF_WEIGHTED_SLOPE <- function(df_mpox_smooth, list_jur){
  
  df_slope = data.frame()
  
  for (jur in list_jur){
    
    df_smooth_jur = df_mpox_smooth %>% filter(Jurisdiction == jur) %>%
      arrange(date_week_start)
    
    #Slope
    delta_raw <- diff(df_smooth_jur$Cases)
    delta_yt = pmax(delta_raw, 0) #NB: pmax
    
    #Dated dataframe
    date_week_start <- df_smooth_jur$date_week_start[-1]
    
    df_slope_jur <- data.frame(
      Jurisdiction = jur,
      date_week_start = date_week_start,
      delta_yt = delta_yt
    )
    
    #Combine
    df_slope = rbind(df_slope, df_slope_jur)
  }
  
  
  return(df_slope)
}

GET_SLOPE_WEIGHTED_METRICS <- function(df_slope, df_preds) {
  
  df_preds_slope <- df_preds %>%
    left_join(df_slope, by = c('Jurisdiction', 'date_week_start')) %>%
    dplyr::select(Jurisdiction, date_week_start, Actual, Predicted, delta_yt)
  
  weights <- df_preds_slope$delta_yt
  
  #RMSE
  slope_weighted_rmse <- sqrt(sum(weights * (df_preds_slope$Actual - df_preds_slope$Predicted)^2, na.rm = TRUE) / sum(weights, na.rm = TRUE))
  slope_weighted_rmse = round(slope_weighted_rmse, 10) #3
  print(paste0('Slope weighted RMSE:', slope_weighted_rmse))
  
  #MAE
  slope_weighted_mae =  sum(weights * abs(df_preds_slope$Actual - df_preds_slope$Predicted), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_mae = round(slope_weighted_mae, 10) #3
  print(paste0('Slope weighted MAE:', slope_weighted_mae))
  
  #BIAS
  slope_weighted_bias = sum(weights * (df_preds_slope$Predicted - df_preds_slope$Actual), na.rm = TRUE) / sum(weights, na.rm = TRUE)
  slope_weighted_bias = round(slope_weighted_bias, 10) #3
  print(paste0('Slope weighted BIAS:', slope_weighted_bias))
  
  return(list(slope_weighted_rmse = slope_weighted_rmse, slope_weighted_mae = slope_weighted_mae, slope_weighted_bias = slope_weighted_bias))
}

GET_SLOPE_WEIGHTED_IMPROVEMENT <- function(df_slope, df_preds1, df_preds2) {
  
  # Get slope-weighted metrics for each prediction set
  list_metrics1 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds1)
  list_metrics2 <- GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds2)
  
  # Extract values
  var_w_slope_rmse <- list_metrics1$slope_weighted_rmse
  naive_w_slope_rmse <- list_metrics2$slope_weighted_rmse
  var_w_slope_mae <- list_metrics1$slope_weighted_mae
  naive_w_slope_mae <- list_metrics2$slope_weighted_mae
  var_w_slope_bias <- list_metrics1$slope_weighted_bias
  naive_w_slope_bias <- list_metrics2$slope_weighted_bias
  
  # Calculate percentage improvements
  var_rmse_pcent_improve <- 100 * (naive_w_slope_rmse - var_w_slope_rmse) / naive_w_slope_rmse
  var_mae_pcent_improve <- 100 * (naive_w_slope_mae - var_w_slope_mae) / naive_w_slope_mae
  var_bias_pcent_improve <- 100 * (naive_w_slope_bias - var_w_slope_bias) / naive_w_slope_bias
  
  # Return as one-row dataframe
  var_w_slope_rmse = round(var_w_slope_rmse, 3)
  naive_w_slope_rmse = round(naive_w_slope_rmse, 3)
  var_rmse_pcent_improve = round(var_rmse_pcent_improve, 1)
  var_w_slope_mae = round(var_w_slope_mae, 3)
  naive_w_slope_mae = round(naive_w_slope_mae, 3)
  var_mae_pcent_improve = round(var_mae_pcent_improve, 1)
  var_w_slope_bias = round(var_w_slope_bias, 3)
  naive_w_slope_bias = round(naive_w_slope_bias, 3)
  var_bias_pcent_improve = round(var_bias_pcent_improve, 1)
  
  df_out <- data.frame(
    Evaluation_Metric = c('Slope weighted RMSE', 'Slope weighted MAE', 'Slope weighted Bias'),
    VAR_result = c(var_w_slope_rmse, var_w_slope_mae, var_w_slope_bias),
    Naive_result = c(naive_w_slope_rmse, naive_w_slope_mae, naive_w_slope_bias),
    Percent_improve_var = c(var_rmse_pcent_improve, var_mae_pcent_improve, var_bias_pcent_improve)
  )
  
  return(df_out)
}

#ADDITIONAL FUNCTION
MATRIX_LASSO_RENAME_COLS <- function(mat, juris_names) {
  # Rename rows: Y1, Y2, ..., Yn → juris_names
  row_mapping <- setNames(juris_names, paste0("Y", seq_along(juris_names)))
  rownames(mat) <- row_mapping[rownames(mat)]
  
  # Rename columns: e.g., Y3L1 → Colorado_L1
  colnames(mat) <- sapply(colnames(mat), function(colname) {
    if (grepl("^Y\\d+", colname)) {
      match <- regmatches(colname, regexpr("^Y\\d+", colname))  # e.g., Y3
      index <- as.numeric(sub("Y", "", match))                  # e.g., 3
      new_name <- juris_names[index]                            # e.g., Colorado
      sub("^Y\\d+", paste0(new_name, "_"), colname)             # replace Y3 with Colorado_
    } else {
      colname  # leave "intercept" or any non-lag columns as-is
    }
  })
  
  return(mat)
}