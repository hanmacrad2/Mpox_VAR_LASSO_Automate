# Mpox VAR-LASSO Forecasting

__Forecasting weekly Mpox case counts in high-incidence U.S. jurisdictions using Lasso Vector Autoregression models with cross-jurisdictional lags__

- We developed a Vector Autoregression model with Lasso regularization (VAR-Lasso) to generate rolling two-week-ahead forecasts of weekly Mpox cases for eight high-incidence U.S. jurisdictions using national surveillance data from the Centers for Disease Control and Prevention (CDC).
- The VAR-Lasso approach simultaneously performs parameter estimation and variable selection, identifying significant auto-regressive and cross-jurisdictional lagged predictors.
- Our performance evaluation focuses on positive-slope weighted error metrics, As the need for public health action is greatest when incidence is increasing
- Forecast performance of the VAR-Lasso model is compared to a naïve moving-average estimate using slope-weighted Root Mean Squared Error (RMSE), slope-weighted Mean Absolute Error (MAE), and slope-weighted bias.
- Across all observations, the VAR-Lasso model outperforms the naïve estimate.
- The model identifies significant long-lag predictors that are consistent with independent phylogenetic analysis.
- These findings highlight the potential of sparse multivariate time-series models that leverage cross-jurisdictional case data to improve early detection and forecasting of Mpox outbreaks.

## Project Structure

```
main.R                  # Entry point for all execution modes
file_utils.R            # File path resolution and parameter loading
forecast_utils.R        # Data preparation and model execution
output_utils.R          # Output formatting (CSV and JSON)
libs_required.R         # Package dependency loading
VARLASSOMpox/           # Core R package (model implementations)
Paper/                  # Paper-specific analysis scripts and raw data
renv/                   # Reproducible R environment
renv.lock               # Dependency lock file
```

### Core Package (`VARLASSOMpox/R/`)

| File | Purpose |
|------|---------|
| `data_processing.R` | `GET_SMOOTH_DATA()`, `GET_TS_DATA()`, `get_train_test_data()` — data formatting and smoothing |
| `model_var.R` | `VAR_LASSO_FORECAST_TWO_STEP_CI()` — VAR-Lasso with confidence intervals |
| `AR_model.R` | `AR_model_CI()` — univariate sparse AR-Lasso per jurisdiction |
| `naive_estimate.R` | `SIMPLE_NAIVE_FORECAST()` — baseline rolling-average forecast |
| `performance_metrics.R` | Slope-weighted RMSE, MAE, MAPE, bias evaluation |
| `plot_functions.R` | ggplot2 visualizations for forecasts vs. actuals |

## Installation

Install the R package from GitHub:

```r
install.packages("devtools")  # only needed once
devtools::install_github("hanmacrad2/Mpox_VAR_LASSO_Automate/VARLASSOMpox")
```

Or from a local clone:

```r
devtools::install("VARLASSOMpox")
```

Then load dependencies:

```r
library(VARLASSOMpox)
source("libs_required.R")
```

## Running Forecasts

### Interactive (R Shell)

Run `main.R` directly from an R session or the command line. When ResilientSims environment variables are not set, it uses local defaults:

- **Input directory**: `.Rproj.user/input/`
- **Output directory**: `.Rproj.user/output/`
- **Parameters**: defaults defined in `file_utils.R`

```bash
Rscript main.R
```

### Via ResilientSims API

The simulator integrates with the ResilientSims platform. The platform invokes `main.R` with standardized environment variables:

| Environment Variable | Purpose |
|---------------------|---------|
| `SIMULATOR_CODE_DIR` | Directory containing the simulator code |
| `SIMULATOR_INPUT_DIR` | Directory containing input data files |
| `SIMULATOR_OUTPUT_DIR` | Directory where results must be written |
| `SIMULATOR_RUNTIME_PARAMS_FILE` | Path to runtime parameters JSON |

Example execution command (configured in the simulator registration):

```
cd $SIMULATOR_CODE_DIR && Rscript main.R --output_dir=$SIMULATOR_OUTPUT_DIR --params=$SIMULATOR_RUNTIME_PARAMS_FILE --input_dir=$SIMULATOR_INPUT_DIR
```

## Input

### Data Format

A CSV file (default: `mpox_combined_weekly.csv`) with columns:

| Column | Type | Description |
|--------|------|-------------|
| `Jurisdiction` | string | U.S. jurisdiction name |
| `date_week_start` | date (YYYY-MM-DD) | Start of the epi-week |
| `Cases` | integer | Weekly case count |

### Runtime Parameters

Parameters are provided as JSON (via `runtime_params.json` or defaults):

| Parameter | Default | Description |
|-----------|---------|-------------|
| `model_jurisdictions` | all in data | Jurisdictions used to train the VAR model |
| `portal_jurisdictions` | same as `model_jurisdictions` | Jurisdictions included in output |
| `last_data_date` | latest in data | Data cutoff date |
| `roll_window` | 4 | Smoothing window in weeks |
| `n_lags` | 25 | Maximum AR lag order |

## Output

All output is written to the output directory.

### `mpox_forecasts.csv`

Combined historical observations and forecasts:

| Column | Description |
|--------|-------------|
| `epiweek_start` | Start of the epi-week |
| `jurisdiction` | Jurisdiction name |
| `cases` | Observed case count (observations only) |
| `type` | `observation` or `forecast` |
| `method` | Model name (`VAR_LASSO`, `AR_LASSO`, `NAIVE`) |
| `predicted` | Predicted value |
| `lower_ci` | Lower confidence interval bound |
| `upper_ci` | Upper confidence interval bound |

### `results.json`

ResilientSims-compatible metadata:

```json
{
  "summary": {
    "forecast_origin_date": "2026-03-22",
    "forecast_date": "2026-04-05",
    "jurisdictions": ["SanDiego", "Texas"],
    "methods": ["VAR_LASSO", "AR_LASSO", "NAIVE"]
  },
  "plots": [],
  "output_manifest": ["mpox_forecasts.csv"],
  "history": []
}
```

## Pipeline Overview

`main.R` orchestrates the following steps:

1. **Resolve paths** — `file_utils.R` detects whether the simulator is running under ResilientSims (via environment variables) or locally, and resolves input/output directories accordingly.
2. **Load parameters** — Runtime parameters are read from JSON, with defaults applied for any missing values.
3. **Load and prepare data** — `forecast_utils.R` reads the input CSV, filters to the configured jurisdictions and date range, applies rolling-mean smoothing, and converts to wide format for VAR modeling.
4. **Run models** — Three forecasting methods are executed:
   - **VAR-Lasso**: Multivariate model capturing cross-jurisdictional lag structure
   - **AR-Lasso**: Univariate sparse autoregressive model per jurisdiction
   - **Naive**: 4-week rolling average baseline
5. **Write output** — `output_utils.R` formats results into the forecast CSV and `results.json`.

## Modeling Details

- Time series are **differenced** for stationarity before fitting
- AR models additionally **scale** differenced data per jurisdiction
- **Recursive forecasting**: the training window expands by one week per step
- Predictions are **clipped at 0**: negative forecasts are set to zero
- Confidence intervals are produced via bootstrap (VAR-Lasso) and model-based methods (AR-Lasso)

## Dependencies

Key R packages: `BigVAR`, `bigtime`, `forecast`, `vars`, `dplyr`, `tidyr`, `ggplot2`, `patchwork`, `zoo`, `readxl`, `jsonlite`

The `renv.lock` file pins all dependency versions for reproducibility. To restore the environment:

```r
renv::restore()
```
