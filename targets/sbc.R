sbc_targets <- list(
  tar_target(
    sbc_one_prior_simulations,
      do.call(
        generate_obs,
        c(
          retro_args,
          list(
            obs = retro_validation_obs,
            datasets = sbc_datasets,
            strains = 1,
            model = single_model,
            overdispersion = overdispersion_scenarios
          )
        )
      )[, `:=`(
        strains = 1,
        overdispersion = overdispersion_scenarios
      )
        ],
      cross(retro_validation_obs, overdispersion_scenarios)
  ),
  tar_target(
    sbc_two_prior_simulations,
      do.call(
        generate_obs,
        c(
          retro_args,
          list(
            obs = retro_validation_obs,
            datasets = sbc_datasets,
            strains = 2,
            model = two_model,
            overdispersion = overdispersion_scenarios,
            variant_relationship = variant_relationship_scenarios
          )
        )
      )[, `:=`(
        strains = 2,
        overdispersion = overdispersion_scenarios,
        variant_relationship = variant_relationship_scenarios
      )
        ],
      cross(retro_validation_obs, variant_relationship_scenarios,
            overdispersion_scenarios)
  ),
  tar_target(
    sbc_one_posteriors,
    sbc_one_prior_simulations[,
      forecast := list(do.call(
        forecast_data,
        c(
          stan_args,
          list(
            data = data[[1]],
            strains = 1,
            model = one_model
          )
        ),
      ))],
      map(sbc_one_prior_simulations)
  ),
  tar_target(
    sbc_two_posteriors,
    sbc_two_prior_simulations[,
      forecast := list(do.call(
        forecast_data,
        c(
          stan_args,
          list(
            data = data[[1]],
            strains = 2,
            model = two_model
          )
        ),
      ))],
      map(sbc_two_prior_simulations)
  ),
  # calculate coverage for one and two strain models across simulations
  tar_target(
    one_strain_coverage,
    sbc_coverage(sbc_one_posteriors, by = c("strains", "overdispersion"))
  ),
  tar_target(
    two_strain_coverage,
    sbc_coverage(sbc_two_posteriors, by = c("strains", "overdispersion",
                                            "variant_relationship"))
  )
)