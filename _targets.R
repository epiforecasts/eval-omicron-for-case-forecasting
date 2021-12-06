# load targets + parallel packages
library(targets)
library(tarchetypes)
library(future)
library(future.callr)
library(here)
plan(callr)

# should the whole pipeline be run or just the validation steps
validation <- TRUE
forecast <- TRUE

# datasets of interest
#sources <- list(source = c("Germany", "United Kingdom", "Belgium", "Italy"))
sources <- list(source = "Germany")

# load required packages and watch forecast.vocs for changes
tar_option_set(
  packages = c("forecast.vocs", "purrr", "data.table", "scoringutils",
               "ggplot2", "here", "stringr"),
  deployment = "worker",
  memory = "transient",
  workspace_on_error = TRUE,
  error = "continue",
  garbage_collection = TRUE
)

# load functions
functions <- list.files(here("R"), full.names = TRUE)
purrr::walk(functions, source)

# load target modules
targets <- list.files(here("_targets_r"), full.names = TRUE)
targets <- grep("*\\.R", targets, value = TRUE)
targets <- targets[!grepl("_targets_r/summarise_sources.R", targets)]
purrr::walk(targets, source)

# branch targets across data sources (see individual targets scripts in
# targets/ for further details of each step)
combined_targets <- tar_map(
  values = sources,
  c(
    obs_targets, # load source specific observations
    forecast_targets, # forecast
    summarise_forecast_targets, # summarise forecasts
    score_forecast_targets # score forecasts by source
  ),
  unlist = FALSE
)

# Load summary targets
source(here("_targets_r/summarise_sources.R"))

# Combine, evaluate, and summarise targets
targets_list <- list(
  meta_targets, # Inputs and control settings
  scenario_targets # Define scenarios to evaluate
)
if (validation) {
  # Prior and posterior checks across a range of scenarios
  targets_list <- c(targets_list, validation_targets)
}
if (forecast) {
  targets_list <- c(
    combined_targets, # Forecast all dates and scenarios
    summarise_source_targets # Summarise forecasts
  )
}
targets_list
