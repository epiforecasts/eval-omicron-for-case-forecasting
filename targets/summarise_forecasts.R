# Targets summarising forecasts
summarise_forecast_targets <- list(
  # Summarise the forecast models fit
  tar_target(
    fit_summary,
    rbindlist(
      map(list(
        single_retrospective_forecasts,
        two_retrospective_forecasts,
        two_scenario_forecasts
      ), ~ .[, .(
        id, forecast_date, strains, overdispersion, variant_relationship,
        samples, max_rhat, divergent_transitions,
        per_divergent_transitons, max_treedepth, no_at_max_treedepth,
        per_at_max_treedepth
      )])
    ),
  ),
  # Combine forecasts into a single data frame
  tar_target(
    forecast_single_retro,
    unnest_posterior(single_retrospective_forecasts, target = "forecast"),
  ),
  tar_target(
    forecast_two_retro,
    unnest_posterior(two_retrospective_forecasts, target = "forecast"),
  ),
  tar_target(
    forecast_two_scenario,
    unnest_posterior(two_scenario_forecasts, target = "forecast"),
  ),
  # Combine all separate forecasts into a single data frame
  tar_target(
    forecast,
    merge(
      rbindlist(
        list(
          forecast_single_retro,
          forecast_two_retro,
          forecast_two_scenario
        )
      )[, location := source][, voc_scale := NULL],
      data_availability_scenarios[
        ,
        voc_scale := map_chr(voc_scale, paste, collapse = ", ")
      ],
      by = "id", all.x = TRUE
    ),
  ),
  # Extract forecasts for cases only and link to current observations
  tar_target(
    forecast_cases,
    merge(
      forecast[value_type == "cases"][type %in% c("Overall", "Combined")][
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
