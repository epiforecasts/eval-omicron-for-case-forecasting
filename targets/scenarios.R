# Targets defining the scenarios to evaluate
scenario_targets <- list(
  # define types of variant relationship to initial strain to tests
  tar_target(
    variant_relationship_scenarios,
    c("scaled", "pooled", "independent"),
    deployment = "main"
  ),
  # define overdispersion testing scenarios
  tar_target(
    overdispersion_scenarios,
    c(TRUE, FALSE),
    deployment = "main"
  ),
  #'  Define data availability scenarios for sequence data
  #' @param seq_lag The number of  weeks that sequences lag the date. Default is
  #' to test 0 to 3 weeks of lag.
  #' @param seq_samples Fraction of samples to include (deterministic scaling).
  #' The default is to test all samples down to 25% of samples by 25%
  #' increments.
  #' @param delta A list of mean and standard  deviations to use to inform
  #' the prior for additional transmissibility of the delta variant. The default
  #' a uninformed no prior knowledge prior (0, 0.5), a weak assumption of a
  #' transmissibility advantage (0.5, 0.25), and an estimate based on early UK
  #' travel adjusted growth (0.74, 0.1).
  tar_target(
    data_availability_scenarios,
    define_scenarios(
      seq_lag = 0:3,
      seq_samples = seq(1, by = -0.25, length.out = 4),
      voc_scale = list(c(0, 0.5), c(0.5, 0.25), c(0.74, 0.1))
    ),
    deployment = "main"
  )
)
