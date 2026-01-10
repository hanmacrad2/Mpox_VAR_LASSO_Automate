#****************************************
#* 2. RUN MODEL ESTIMATES
#****************************************
'Get two week ahead forecasts'

#SEP
ROLL_WINDOW = 4 
n_lags = 25
print(paste0('TRAIN FINAL WEEK: ', TRAIN_WEEK))

#1. VAR LASSO SMOOTH  
forecasts_var_two_step = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                               ROLL_WINDOW, n_lags)

df_preds_var = forecasts_var_two_step$df_pred_results

df_preds_var_ci = VAR_LASSO_FORECAST_TWO_STEP_CI(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                                                         ROLL_WINDOW, n_lags)


#2. NAIVE ESTIMATE SMOOTH
naive_estimates_two_step <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                         future_data_ts, n_step_ahead = 2)

df_preds_naive = naive_estimates_two_step$df_pred_results


#3. NAIVE ESTIMATE SMOOTH
df_preds_ar <- AR_model_jurs(train_data_ts_smooth, future_data_ts_smooth,
                                                  future_data_ts, list_jur, n_step_ahead = 2)

df_preds_ar_ci = AR_model_CI(train_data_ts_smooth, future_data_ts_smooth, future_data_ts, list_jur)

saveRDS(df_preds_ar_ci, file = 'df_preds_ar_ci.rds')

#saveRDS(df_preds_var_conf_int, file = 'df_preds_var_conf_int.rds')
