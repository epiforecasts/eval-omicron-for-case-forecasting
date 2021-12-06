meta_targets <- list(
  # Path to observations
  tar_target(
    observations_file,
    here("data", "observations", "covariants.csv"),
    format = "file", packages = c("here")
  ),
  tar_target(
    observations,
    fread(observations_file),
    packages = c("data.table", "here")
  ),
  # Compile models
  tar_target(
    single_model,
    forecast.vocs::fv_model(strains = 1),
    format = "file", deployment = "main",
  ),
  tar_target(
    two_model,
    forecast.vocs::fv_model(strains = 2),
    format = "file", deployment = "main",
  ),
  # Arguments that control fitting stan models
  tar_target(
    stan_args,
    list(
      adapt_delta = 0.99, max_treedepth = 15, parallel_chains = 1, chains = 2
    )
  ),
  # Arguments passed to `forecast()` to control forecasting
  tar_target(
    forecast_args,
    c(
      stan_args,
      list(
        voc_scale = c(0.5, 0.25), voc_label = "Omicron",
        horizon = 8, keep_fit = FALSE,
        probs = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
      )
    ),
    deployment = "main"
  ),
  # Data source to use for model validation
  tar_target(
    validation_source,
    "United Kingdom"
  ),
  # Forecast dates to use for model validation
  tar_target(
    validation_dates,
    as.Date(c("2021-06-12", "2021-6-26", "2021-07-10", "2021-07-24"))
  )
)
