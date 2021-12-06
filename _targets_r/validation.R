validation_targets <- list(
  # validate both prior and posterior
  tar_target(
    validate_likelihood,
    c(TRUE, FALSE)
  ),
  # load data for validation data source
  tar_target(
    validation_obs,
    filter_obs(observations, validation_source)
  ),
  # extract the most up to date version of the validation data
  tar_target(
    current_validation_obs,
    latest_obs(validation_obs)
  ),
  # split data into that available in each forecast week
  tar_target(
    retro_validation_obs,
    filter_by_availability(validation_obs, date = validation_dates),
    map(validation_dates),
    iteration = "list"
  ),
  # plot prior predictive check
  # plot posterior predictive check
  # Targets producing forecasts for each week of observed data
  # prior and predictive checks for validation data on the single strain model
  tar_target(
    single_predictive_checks,
    do.call(
      forecast,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_validation_obs,
          strains = 1,
          model = single_model,
          likelihood = validate_likelihood,
          overdispersion = overdispersion_scenarios
        )
      )
    )[, likelihood := validate_likelihood],
    cross(retro_validation_obs, validate_likelihood, overdispersion_scenarios)
  ),
  # prior and predictive checks for validation data on the two strain model
  # stratified by modelled relationship between variants
  tar_target(
    two_predictive_checks,
    do.call(
      forecast,
      c(
        forecast_args,
        retro_args,
        list(
          obs = retro_validation_obs,
          strains = 2,
          variant_relationship = variant_relationship_scenarios,
          model = two_model,
          likelihood = validate_likelihood,
          overdispersion = overdispersion_scenarios
        )
      )
    )[, likelihood := validate_likelihood],
    cross(retro_validation_obs, variant_relationship_scenarios,
          overdispersion_scenarios, validate_likelihood)
  ),
  ## plot prior predictions for single model
  tar_target(
    plot_single_strain_prior,
    plot_single_strain_predictions(single_predictive_checks,
                                   current_validation_obs,
                                   likelihood = FALSE),
    format = "file"
  ),
  ## plot posterior predictions for single model
  tar_target(
    plot_single_strain_posterior,
    plot_single_strain_predictions(single_predictive_checks,
                                   current_validation_obs,
                                   likelihood = TRUE),
    format = "file"
  ),
  ## plot prior predictions for two strain model
  tar_target(
    plot_two_strain_prior_overdisp,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = FALSE, overdispersion = TRUE),
    format = "file"
  ),
  tar_target(
    plot_two_strain_prior,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = FALSE, overdispersion = FALSE),
    format = "file"
  ),
  tar_target(
    plot_two_strain_prior_overdisp_voc,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = FALSE, overdispersion = TRUE,
                                type = "voc"),
    format = "file"
  ),
  tar_target(
    plot_two_strain_prior_voc,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = FALSE, overdispersion = FALSE,
                                type = "voc"),
    format = "file"
  ),
  ## plot posterior predictions for two strain models
  tar_target(
    plot_two_strain_posterior_overdisp,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = TRUE, overdispersion = TRUE),
    format = "file"
  ),
  tar_target(
    plot_two_strain_posterior,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = TRUE, overdispersion = FALSE),
    format = "file"
  ),
  tar_target(
    plot_two_strain_posterior_overdisp_voc,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = TRUE, overdispersion = TRUE,
                                type = "voc"),
    format = "file"
  ),
  tar_target(
    plot_two_strain_posterior_voc,
    plot_two_strain_predictions(two_predictive_checks, current_validation_obs,
                                likelihood = TRUE, overdispersion = FALSE,
                                type = "voc"),
    format = "file"
  )
)
