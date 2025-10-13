__Forecasting weekly Mpox case counts in high-incidence U.S. jurisdictions using Lasso Vector Autoregression models with cross-jurisdictional lags__: ﻿New Repository for automation of forecasts. 

- This repo contains the code to implement the following;
  
 - We developed a Vector Autoregression model with Lasso regularization (VAR-Lasso) to generate rolling two-week-ahead forecasts of weekly Mpox cases for eight high-incidence U.S. jurisdictions using national surveillance data from the Centers for Disease Control and Prevention (CDC).
 - The VAR-Lasso approach simultaneously performs parameter estimation and variable selection, identifying significant auto-regressive and cross-jurisdictional lagged predictors.
 - Our performance evaluation focuses on positive-slope weighted error metrics, As the need for public health action is greatest when incidence is increasing
 - Forecast performance of the VAR-Lasso model is compared to a naïve moving-average estimate using slope-weighted Root Mean Squared Error (RMSE), slope-weighted Mean Absolute Error (MAE), and slope-weighted bias.
 - Across all observations, the VAR-Lasso model outperforms the naïve estimate.
 - The model identifies significant long-lag predictors that are consistent with independent phylogenetic analysis.
 - These findings highlight the potential of sparse multivariate time-series models that leverage cross-jurisdictional case data to improve early detection and forecasting of Mpox outbreaks.

Overview of Code:

## Installation

You can install the package in R from GitHub using:

```
install.packages("devtools")  # only needed once
library(devtools)
devtools::install_github("hanmacrad2/Mpox_VAR_LASSO_Automate/VARLASSOMpox")
library(VARLASSOMpox)

```

This tells devtools to; 

- Clone your GitHub repo,
- Look for the R package inside the VARLASSOMpox subdirectory,
- Build & install it locally to your - the user’s - system.

## Running the model code

To generate forecasts of Mpox using the VAR LASSO model;

Firstly load the libary (after installing it as above)

```
library(VARLASSOMpox)
```

1. Format and smooth the data using the file: 
```
1_get_and_format_mpox_data.R
```
2. Generate the forecasts using the file:
```
2_run_var_lasso_model.R
```

3.Plot the forecasts and determine the performance metrics using the file: 
```
3_get_results_and_plot.R
```



