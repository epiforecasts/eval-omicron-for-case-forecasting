# Targets producing forecasts for each week of observed data
fit_targets <- list(
  tar_target(
    single_fits,
    do.call(
      forecast,
      c(
        forecast_args,
        list(
          obs = avail_obs,
          strains = 1,
          overdispersion = overdispersion_scenarios,
          model = single_model
        )
      )
    ),
    cross(avail_obs, overdispersion_scenarios)
  ),
  tar_target(
    two_fits,
    do.call(
      forecast,
      c(
        forecast_args,
        list(
          obs = avail_obs,
          strains = 2,
          overdispersion = overdispersion_scenarios,
          variant_relationship = variant_relationship_scenarios,
          model = two_model
        )
      )
    ),
    cross(avail_obs, variant_relationship_scenarios, overdispersion_scenarios)
  )
)
