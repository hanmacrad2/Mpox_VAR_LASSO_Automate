#*******************************************
#*GET RESULTS & PLOT 
#*******************************************

#1. GET FORECASTS
df_preds_var = forecasts_var_two_step$df_pred_results
df_preds_naive = naive_estimates_two_step$df_pred_results

#2. PLOT FORECASTS
#forecast_start_date = as.Date("2024-05-25") #start of forecast 
title = 'VAR two week-ahead Forecasts - Top 8 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, list_jur, title, n_col_plot = 3)

#3. GET PERFORMANCE METRICS
#i. METRIC: SLOPE-WEIGHTED AT OBSERVATIONAL/GLOBAL LEVEL
df_slope = GET_DF_WEIGHTED_SLOPE(df_mpox_smooth, list_jur)
list_metrics_var = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_var)
list_metrics_naive = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_naive)

#ii. METRICS: JURISDICTION LEVEL
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_naive, df_slope, list_jur)
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_naive)



