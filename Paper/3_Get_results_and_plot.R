#*******************************************
#*GET RESULTS & PLOT 
#*******************************************

#1. GET FORECASTS
df_preds_var;df_preds_naive; df_preds_ar

#3. GET PERFORMANCE METRICS
df_slope = GET_DF_WEIGHTED_SLOPE(df_model_smooth, list_jur)

#NAIVE VS VAR
df_global_slope = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_naive)

#AR VS VAR
df_global_slope2 = GET_SLOPE_WEIGHTED_IMPROVEMENT(df_slope, df_preds_var, df_preds_ar)


#ii. METRICS: JURISDICTION LEVEL
df_jur_slope_results =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_naive, df_slope, list_jur)
df_jur_slope_results2 =  GET_JUR_METRICS_MODELS_COMPARED(df_preds_var, df_preds_ar, df_slope, list_jur)


#METRICS INDIVID
list_metrics_var = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_var)
list_metrics_ar = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_ar)
list_metrics_naive = GET_SLOPE_WEIGHTED_METRICS(df_slope, df_preds_naive)


#*********************
#PLOTS 

#2. PLOT ALL FORECASTS
title_plot = 'VAR-Lasso, AR-Lasso & Naive two week-ahead forecasts with Prediction intervals for January-November 2024 - Top 8 Jurisdictions'
PLOT_CASES_FORECASTS(data_24_ts_forecast_start, df_preds_var, df_preds_ar, df_preds_naive, list_jur, title_plot)
#ONE PLOT
PLOT_CASES_FORECASTS(data_24_ts_forecast_start, df_preds_var, df_preds_ar, df_preds_naive, list_jur, title_plot, three_figures = FALSE)


#CONFIDENCE INTERVALS WITH
title_ci_plot = 'VAR-Lasso, AR-Lasso & Naive two week-ahead forecasts for Jan-Nov 2024 with 95% Prediction intervals'
PLOT_CASES_FORECASTS_CI_LEGEND(data_24_ts_forecast_start, df_preds_var_conf_int, df_preds_ar_ci, df_preds_naive, list_jur, title_ci_plot)


#*************************
#FORECASTS - SAN DIEGO
title_plot = 'San Diego County forecasts, January - November 2024, using the VAR-Lasso, AR-Lasso & Naive models'

df_preds_var_sd = df_preds_var %>% filter(Jurisdiction == 'SanDiego')
df_preds_ar_sd = df_preds_ar %>% filter(Jurisdiction == 'SanDiego')
df_preds_naive_sd = df_preds_naive %>% filter(Jurisdiction == 'SanDiego')
data_ts_sd_24 =  data_ts_24[['SanDiego']]

PLOT_DATES_TRUE_FORECAST_JUR(df_preds_var_sd, df_preds_ar_sd, df_preds_naive_sd, title_plot)


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

