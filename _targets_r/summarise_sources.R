summarise_source_targets <- list(
  # gather summaries and forecasts
  tar_combine(
    fit_summary,
    combined_targets$fit_summary
  ),
  tar_combine(
    forecast_cases,
    combined_targets$forecast_cases
  ),
  tar_combine(
    forecast_scores,
    combined_targets$forecast_scores
  ),
  tar_combine(
    rwis,
    combined_targets$rwis
  )
)
