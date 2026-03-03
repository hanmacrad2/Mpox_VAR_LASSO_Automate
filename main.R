#' Mpox VAR-Lasso Forecasting Pipeline
#'
#' Generates 2-week-ahead forecasts using VAR-Lasso, AR-Lasso, and Naive methods.
#'
#' @section Input Parameters (runtime_params.json):
#' \describe{
#'   \item{last_data_date}{Last date to include in training (default: last date in data)}
#'   \item{portal_jurisdiction}{Jurisdiction for output (default: "SanDiego" if in data, else first)}
#'   \item{model_jurisdictions}{Jurisdictions for VAR training (default: all in data)}
#'   \item{roll_window}{Rolling mean window for smoothing (default: 4)}
#'   \item{n_lags}{Maximum lag order for models (default: 25)}
#' }
#'
#' @section Outputs:
#' \describe{
#'   \item{mpox_forecasts.csv}{Historical observations + 2-week forecasts with CIs}
#'   \item{results.json}{ResilientSims-compatible summary and manifest}
#' }
#'
#' @section Manual Execution from RStudio:
#' After setting up the combined input file in the input directory (see merge_utils.R),
#' run from the top-level project directory:
#' \preformatted{
#' Sys.setenv(SIMULATOR_INPUT_DIR = "./.Rproj.user/input")
#' Sys.setenv(SIMULATOR_OUTPUT_DIR = "./.Rproj.user/output")
#' Sys.setenv(SIMULATOR_CODE_DIR = "./")
#' source("main.R")
#' }

# =============================================================================
# SETUP
# =============================================================================

library(VARLASSOMpox)

source("file_utils.R")
source("forecast_utils.R")
source("output_utils.R")

source(file.path(get_project_dir(), "libs_required.R"))

# =============================================================================
# 1. LOAD PARAMETERS AND DATA
# =============================================================================

message("=== Loading parameters and data ===")

params <- load_runtime_params(input_dir = get_input_dir())
data <- load_mpox_data(params)

# Apply data-dependent defaults
params <- apply_data_defaults(params, data)

# Filter to model_jurisdictions (in case user specified a subset)
data <- data[data$Jurisdiction %in% params$model_jurisdictions, ]

message(sprintf("Parameters: roll_window=%d, n_lags=%d",
                params$roll_window, params$n_lags))
message(sprintf("Model jurisdictions: %s", paste(params$model_jurisdictions, collapse = ", ")))
message(sprintf("Portal jurisdiction: %s", params$portal_jurisdiction))

# =============================================================================
# 2. PREPARE FORECAST DATA
# =============================================================================

message("=== Preparing forecast data ===")

forecast_data <- prepare_forecast_data(data, params$roll_window)

message(sprintf("Training data through: %s", forecast_data$last_data_date))
message(sprintf("Forecast date (2 weeks ahead): %s", forecast_data$forecast_date))

# =============================================================================
# 3. RUN FORECASTS
# =============================================================================

message("=== Running forecasts ===")

forecasts <- run_all_forecasts(forecast_data, params$roll_window, params$n_lags)

message(sprintf("Generated %d forecast rows", nrow(forecasts)))

# =============================================================================
# 4. WRITE OUTPUTS
# =============================================================================

message("=== Writing outputs ===")

write_outputs(
  data_long = data,
  forecasts = forecasts,
  forecast_data = forecast_data,
  portal_jurisdiction = params$portal_jurisdiction
)

message("=== Pipeline complete ===")
