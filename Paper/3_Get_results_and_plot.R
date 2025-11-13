#*******************************************
#*GET RESULTS & PLOT 
#*******************************************

#1. GET FORECASTS
df_preds_var = forecasts_var_two_step$df_pred_results
df_preds_naive = naive_estimates_two_step$df_pred_results
df_preds_ar

#df_preds_var2 = forecasts_var_two_step2$df_pred_results

#2. PLOT FORECASTS
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, df_preds_ar, df_preds_naive, list_jur, title)

#forecast_start_date = as.Date("2024-05-25") #start of forecast 
title = 'VAR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, list_jur, title, n_col_plot = 3)

title = 'AR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, ar_preds_two_step, list_jur, title, n_col_plot = 3)


#3. GET PERFORMANCE METRICS
#i. METRIC: SLOPE-WEIGHTED AT OBSERVATIONAL/GLOBAL LEVEL
df_slope = GET_DF_WEIGHTED_SLOPE(df_model_smooth, list_jur)
list_metrics_var = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_var)
list_metrics_naive = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_naive)

#VAR1 VS VAR2
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var2, df_preds_var)

#NAIVE VS VAR
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_naive)

#AR VS VAR
df_global_slope2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, ar_preds_two_step)


#ii. METRICS: JURISDICTION LEVEL
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_naive, df_slope, list_jur)

df_jur_slope_results2 =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_ar, df_slope, list_jur)

#df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var2, df_preds_naive, df_slope, list_jur)

#FORECASTS - SAN DIEGO
title_plot = 'San Diego County Forecasts, January-November 2024'
df_preds_var_sd = df_preds_var %>% filter(Jurisdiction == 'SanDiego')
df_preds_ar_sd = df_preds_ar %>% filter(Jurisdiction == 'SanDiego')
df_preds_naive_sd = df_preds_naive %>% filter(Jurisdiction == 'SanDiego')

PLOT_DATES_TRUE_FORECAST_JUR(df_preds_var_sd, df_preds_ar_sd, df_preds_naive_sd, title_plot)
#PLOT_DATES_TRUE_FORECAST_JUR(df_preds_var_sd, title_plot)

