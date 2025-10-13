#****************************************
#* 2. RUN MODEL ESTIMATES
#****************************************
'Get two week ahead forecasts'

#SEP
n_lags = 25
print(paste0('TRAIN FINAL WEEK: ', TRAIN_WEEK))

#1. VAR LASSO SMOOTH
forecasts_var_two_step = VAR_LASSO_FORECAST_TWO_STEP(train_data_ts_smooth, future_data_ts_smooth, future_data_ts,
                                               ROLL_WINDOW, n_lags)

df_preds_var = forecasts_var_two_step$df_pred_results

#2. NAIVE ESTIMATE SMOOTH
naive_estimates_two_step <- SIMPLE_NAIVE_FORECAST(train_data_ts_smooth, future_data_ts_smooth,
                                         future_data_ts, n_step_ahead = 2)

df_preds_naive = naive_estimates_two_step$df_pred_results