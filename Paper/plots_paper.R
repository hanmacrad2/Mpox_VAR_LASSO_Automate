#PLOTS: PAPER

FOLDER_FIGURES = "~/UCSD/PROJECTS/Project_2_Mpox/PAPER/PAPER_NATURE_COMMS/FIGURES/NATURE_FIGURES"
  
#1. PLOT_GLOBAL_IMPROVEMENTS (FIGURE 2; Paper)

#RESULTS
df_global_improve <- data.frame(
  Metric = c("Slope-weighted RMSE", "Slope-weighted MAE", "Slope-weighted Bias"),
  ImproveVA = c(12, 7, 66),
  ImproveVN = c(16, 13, 76)
)

#PLOT
PLOT_GLOBAL_IMPROVEMENTS(df_global_improve)

#SAVE
fig_name = '2_model_improve.png'
fig_width = 8; fig_height = 6
ggsave(paste0(FOLDER_FIGURES, fig_name), dpi = 300, width = fig_width, height = fig_height, units = "in")


#PLOT_GLOBAL_METRICS
PLOT_GLOBAL_METRICS(df_global_results)

#PLOT SENSE


#PLOT SENSITIVITY
df_st <- data.frame(
  Smoothing = c("No smoothing", "2", "3", "4", "5"),
  RMSE_VAR = c(3.0, 3.05, 3.0, 1.75, 1.95),
  RMSE_AR = c(2.9, 3.2, 3.1, 2.0, 2.3),
  RMSE_Naive = c(3.0, 3.1, 3.0, 2.11, 2.5),
  MAE_VAR = c(2.4, 2.5, 2.3, 1.4, 1.55),
  MAE_AR = c(2.3, 2.5, 2.3, 1.55, 1.8),
  MAE_Naive = c(2.50, 2.40, 2.3, 1.65, 1.87)
)



#PLOT SENSITIVITY
plots_sens <- PLOT_SMOOTHING_SENSITIVITY(df_st)

#RMSE
plots_sens$RMSE
#MAE
plots_sens$MAE


#PLOT X2
#PLOT_SMOOTHING_SENSITIVITY_x2(df_st)