# Targets producing forecasts for each week of observed data
forecast_targets <- list(
  tar_target(
    single_retrospective_forecasts,
    do.call(
      forecast,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_obs,
          strains = 1,
          overdispersion = overdispersion_scenarios,
          model = single_model
        )
      )
    ),
    cross(retro_obs, overdispersion_scenarios)
  ),
  tar_target(
    two_retrospective_forecasts,
    do.call(
      forecast,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_obs,
          strains = 2,
          overdispersion = overdispersion_scenarios,
          variant_relationship = variant_relationship_scenarios,
          model = two_model
        )
      )
    ),
    cross(retro_obs, variant_relationship_scenarios, overdispersion_scenarios)
  )
)
