#*******************************************
#*GET RESULTS & PLOT 
#*******************************************

#1. GET FORECASTS
df_preds_var = forecasts_var_two_step$df_pred_results
df_preds_naive = naive_estimates_two_step$df_pred_results
df_preds_ar

#2. PLOT ALL FORECASTS
#title_plot = 'Top 8 jurisidiction forecasts, January-November 2024 using the VAR, AR & naive models'
title_plot = 'VAR, AR & Naive two week-ahead forecasts for January-November 2024 - Top 8 jurisidictions'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, df_preds_ar, df_preds_naive, list_jur, title_plot)


#PLOT INDIVIDUAL
title = 'VAR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, df_preds_var, list_jur, title, n_col_plot = 3)

title = 'AR two week-ahead Forecasts - Top 10 Jurisidictions by Case count in 2023 & 2024'
PLOT_DATES_TRUE_FORECAST(data_24_ts_forecast_start, ar_preds_two_step, list_jur, title, n_col_plot = 3)


#3. GET PERFORMANCE METRICS
df_slope = GET_DF_WEIGHTED_SLOPE(df_model_smooth, list_jur)
list_metrics_var = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_var)
list_metrics_ar = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_ar)
list_metrics_naive = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_naive)


#NAIVE VS VAR
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_naive)

#AR VS VAR
df_global_slope2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_ar)


#ii. METRICS: JURISDICTION LEVEL
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_naive, df_slope, list_jur)
df_jur_slope_results2 =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_ar, df_slope, list_jur)


#FORECASTS - SAN DIEGO
title_plot = 'San Diego County forecasts January - November 2024 using the VAR-Lasso, AR-Lasso & naive models'
df_preds_var_sd = df_preds_var %>% filter(Jurisdiction == 'SanDiego')
df_preds_ar_sd = df_preds_ar %>% filter(Jurisdiction == 'SanDiego')
df_preds_naive_sd = df_preds_naive %>% filter(Jurisdiction == 'SanDiego')

PLOT_DATES_TRUE_FORECAST_JUR(df_preds_var_sd, df_preds_ar_sd, df_preds_naive_sd, title_plot)
#PLOT_DATES_TRUE_FORECAST_JUR(df_preds_var_sd, title_plot)

#PLOT DATA
title_reported = 'Mpox Weekly Reported Cases. January 2023 - November 2024'
PLOT_REPORTED_CASES(data_ts_23_24, 
                    list_jur, 
                    title_reported)

#PLOT TOP 8
ymax = 17
PLOT_REPORTED_CASES_2x4(data_ts_23_24, list_jur, title_reported,  ymax, n_row_plot = 4,
                    n_col_plot = 2)

#PLOT ON ONE PLOT
PLOT_REPORTED_CASES_JUR(data_mpox, list_jur, 2023)

#ROUND NUMBERS
df_paper_jur_results1 = df_jur_slope_results
df_paper_jur_results1[] <- lapply(df_jur_slope_results, function(x) if(is.numeric(x)) round(x, 1) else x)

df_paper_jur_results2 = df_jur_slope_results2
df_paper_jur_results2[] <- lapply(df_paper_jur_results2, function(x) if(is.numeric(x)) round(x, 1) else x)
