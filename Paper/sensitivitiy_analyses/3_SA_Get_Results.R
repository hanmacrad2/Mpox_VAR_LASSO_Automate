#3 GET SA RESULTS 

#*******************************************
#*GET RESULTS & PLOT 
#*******************************************

#1. GET FORECASTS
df_preds_var = forecasts_var_two_step$df_pred_results
df_preds_naive = naive_estimates_two_step$df_pred_results
df_preds_ar

#NAIVE VS VAR
df_slope = GET_DF_WEIGHTED_SLOPE(df_model_smooth, list_jur)
df_global_slope_window2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_naive)

#AR VS VAR
df_global_slope2_window2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_ar)

#***************
#NO SMOOTHING
#1. GET FORECASTS

#df_preds_var_ns, df_preds_naive_ns, df_preds_ar_ns

#NAIVE VS VAR
df_slope_ns = GET_DF_WEIGHTED_SLOPE(data_mpox_model, list_jur)

df_global_slope_window_ns = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope_ns, df_preds_var_ns, df_preds_naive_ns)

#AR VS VAR
df_global_slope_window_ns2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope_ns, df_preds_var_ns, df_preds_ar_ns)



#3. GET PERFORMANCE METRICS
# list_metrics_var = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_var)
# list_metrics_ar = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_ar)
# list_metrics_naive = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_naive)



# #2. PLOT ALL FORECASTS
# #title_plot = 'Top 8 jurisidiction forecasts, January-November 2024 using the VAR, AR & naive models'
# title_plot = 'VAR, AR & Naive two week-ahead forecasts for January-November 2024 - Top 8 jurisidictions'
# PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, df_preds_ar, df_preds_naive, list_jur, title_plot)
# 
# 
# #PLOT INDIVIDUAL
# title = 'VAR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
# PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, list_jur, title, n_col_plot = 3)
# 
# title = 'AR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
# PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, ar_preds_two_step, list_jur, title, n_col_plot = 3)
# 
# 
# 
# #ii. METRICS: JURISDICTION LEVEL
# df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_naive, df_slope, list_jur)
# df_jur_slope_results2 =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_ar, df_slope, list_jur)