# Targets summarising forecasts
summarise_forecast_targets <- list(
  # Summarise the forecast models fit
  tar_target(
    fit_summary,
    rbindlist(
      map(list(
        single_fits,
        two_fits
      ), ~ .[, .(
        id, forecast_date, strains, overdispersion, variant_relationship,
        samples, max_rhat, divergent_transitions,
        per_divergent_transitions, max_treedepth, no_at_max_treedepth,
        per_at_max_treedepth
      )])
    ),
  ),
  # Combine forecasts into a single data frame
  tar_target(
    forecast_single,
    unnest_posterior(single_fits, target = "forecast"),
  ),
  tar_target(
    forecast_two,
    unnest_posterior(two_fits, target = "forecast"),
  ),
  # Combine all separate forecasts into a single data frame
  tar_target(
    forecasts,
    rbindlist(
      list(
        forecast_single,
        forecast_two
      )
    )[, location := source],
  ),
  # Extract forecasts for cases only and link to current observations
  tar_target(
    forecast_cases,
    merge(
      forecasts[value_type == "cases"][type %in% c("Overall", "Combined")][
        ,
        type := NULL
      ],
      current_obs[, .(date,
        true_value = cases,
        share_voc, seq_voc, seq_total
      )],
      all.x = TRUE, by = "date"
    ),
    deployment = "worker"
  )
)
