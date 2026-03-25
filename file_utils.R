#' File and Parameter Utilities for ResilientSims Integration
#'
#' Functions for resolving file paths, loading parameters, and loading data.
#' Uses environment variables when running in ResilientSims containers,
#' with fallbacks for local development.

#' Get the project root directory
#'
#' @param default Default path for local development
#' @return Path to project root
get_project_dir <- function(default = NULL) {
  env_path <- Sys.getenv("SIMULATOR_CODE_DIR", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  if (!is.null(default)) {
    return(default)
  }
  return(getwd())
}


#' Get the input data directory
#'
#' @param default Default path for local development
#' @return Path to input data directory
get_input_dir <- function(default = NULL) {
  env_path <- Sys.getenv("SIMULATOR_INPUT_DIR", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  if (!is.null(default)) {
    return(default)
  }
  return(getwd())
}


#' Get the output directory
#'
#' @param default Default path for local development
#' @return Path to output directory
get_output_dir <- function(default = NULL) {
  env_path <- Sys.getenv("SIMULATOR_OUTPUT_DIR", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  if (!is.null(default)) {
    return(default)
  }
  return(getwd())
}


#' Get the runtime parameters file path
#'
#' @param default Default path for local development
#' @return Path to runtime parameters file, or NULL if not set
get_runtime_params_file <- function(default = NULL) {
  env_path <- Sys.getenv("SIMULATOR_RUNTIME_PARAMS_FILE", unset = "")
  if (nzchar(env_path)) {
    return(env_path)
  }
  return(default)
}


#' Load runtime parameters from JSON file
#'
#' Loads parameters from runtime_params.json if available, then applies
#' static defaults. Data-dependent defaults (model_jurisdictions,
#' portal_jurisdictions, last_data_date) are set later by apply_data_defaults().
#'
#' @param input_dir Input directory for local fallback
#' @param filename Name of the parameters file
#'
#' @return List of runtime parameters with static defaults applied:
#'   \itemize{
#'     \item \code{last_data_date} - Last date to include (default: NULL, uses all data)
#'     \item \code{portal_jurisdictions} - Jurisdictions for output (default: NULL, set from data)
#'     \item \code{model_jurisdictions} - Jurisdictions for VAR training (default: NULL, uses all)
#'     \item \code{roll_window} - Smoothing window in weeks (default: 4)
#'     \item \code{n_lags} - Maximum lag order for models (default: 25)
#'   }
load_runtime_params <- function(input_dir = NULL, filename = "runtime_params.json") {
  params_file <- get_runtime_params_file()

  if (is.null(params_file) && !is.null(input_dir)) {
    params_file <- file.path(input_dir, filename)
  }

  if (!is.null(params_file) && file.exists(params_file)) {
    params <- jsonlite::fromJSON(params_file)
    message(sprintf("Loaded runtime parameters from: %s", params_file))
  } else {
    params <- list()
    message("No runtime parameters file found, using defaults")
  }

  # Static defaults only - data-dependent defaults set by apply_data_defaults()
  params$roll_window <- params$roll_window %||% 4
  params$n_lags <- params$n_lags %||% 25

  return(params)
}


#' Apply data-dependent parameter defaults
#'
#' Sets defaults for parameters that depend on the loaded data:
#' model_jurisdictions, portal_jurisdictions, and last_data_date.
#'
#' @param params List of runtime parameters from load_runtime_params()
#' @param data Loaded data frame from load_mpox_data()
#'
#' @return Updated params list with data-dependent defaults applied
apply_data_defaults <- function(params, data) {
  jurisdictions_in_data <- unique(data$Jurisdiction)

  # model_jurisdictions: default to all jurisdictions in data

  if (is.null(params$model_jurisdictions)) {
    params$model_jurisdictions <- jurisdictions_in_data
  }

  # portal_jurisdictions: default to all model_jurisdictions
  if (is.null(params$portal_jurisdictions)) {
    params$portal_jurisdictions <- params$model_jurisdictions
  }

  # Validate portal_jurisdictions are in model_jurisdictions
  invalid <- setdiff(params$portal_jurisdictions, params$model_jurisdictions)
  if (length(invalid) > 0) {
    warning(sprintf(
      "portal_jurisdictions not in model_jurisdictions: %s (removing them)",
      paste(invalid, collapse = ", ")
    ))
    params$portal_jurisdictions <- intersect(params$portal_jurisdictions, params$model_jurisdictions)
  }

  if (length(params$portal_jurisdictions) == 0) {
    warning("No valid portal_jurisdictions, falling back to all model_jurisdictions")
    params$portal_jurisdictions <- params$model_jurisdictions
  }

  params
}


#' Load Mpox case data
#'
#' Loads weekly case data, applies filters, and computes sequential Week_Number.
#' Input file needs columns: Jurisdiction, date_week_start, Cases.
#'
#' @param params List of runtime parameters. Recognized fields:
#'   \itemize{
#'     \item \code{last_data_date} - Exclude dates after this
#'     \item \code{model_jurisdictions} - Include only these jurisdictions
#'   }
#' @param input_dir Directory containing the data file. Defaults to get_input_dir().
#' @param filename Data file name. Defaults to "mpox_combined_weekly.csv".
#'
#' @return Data frame with columns: Jurisdiction, date_week_start, Week_Number, Cases.
#'   Week_Number is sequential (1, 2, 3...) based on filtered dates.
load_mpox_data <- function(params, input_dir = NULL, filename = "mpox_combined_weekly.csv") {
  if (is.null(input_dir)) {
    input_dir <- get_input_dir()
  }

  filepath <- file.path(input_dir, filename)
  if (!file.exists(filepath)) {
    stop(sprintf("Data file not found: %s", filepath))
  }

  data <- read.csv(filepath, stringsAsFactors = FALSE)
  data$date_week_start <- as.Date(data$date_week_start)

  if (!is.null(params$last_data_date)) {
    cutoff <- as.Date(params$last_data_date)
    data <- data[data$date_week_start <= cutoff, ]
    message(sprintf("Filtered to dates <= %s", cutoff))
  }

  if (!is.null(params$model_jurisdictions)) {
    data <- data[data$Jurisdiction %in% params$model_jurisdictions, ]
    message(sprintf("Filtered to %d jurisdictions", length(params$model_jurisdictions)))
  }

  if (nrow(data) == 0) {
    stop("No data remaining after filtering")
  }

  unique_dates <- sort(unique(data$date_week_start))
  date_to_week <- setNames(seq_along(unique_dates), as.character(unique_dates))
  data$Week_Number <- date_to_week[as.character(data$date_week_start)]

  data <- data[order(data$Jurisdiction, data$date_week_start), ]

  message(sprintf("Loaded %d rows, %d jurisdictions, weeks 1-%d",
                  nrow(data), length(unique(data$Jurisdiction)), max(data$Week_Number)))

  return(data)
}
