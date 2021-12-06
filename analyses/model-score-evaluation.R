library(brms)
library(data.table)
library(targets)
library(purrr)
library(here)

source(here("R", "model-score-evaluation.R"))

tar_load("rwis")
rwis_retro <- rwis[id == 0]
rwis_scenario <- rwis[id != 0]

rwis_retro <- process_rwis(rwis_retro)
rwis_scenario <- process_rwis(rwis_scenario)

summarise_rwis(rwis_retro)
summarise_rwis(rwis_scenario)

retro_fit <- brm(
  bf(
    log(rwis) ~ overdispersion + variant_relationship +
      s(horizon_minus_one, k = 4) + s(share_delta, k = 5)
  ),
  family = student(),
  data = rwis_retro,
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  adapt_delta = 0.99,
  max_treedepth = 15
)

scenario_fit <- brm(
  bf(
    log(rwis) ~ overdispersion + variant_relationship +
      s(horizon_minus_one, k = 4) + s(share_delta, k = 5, by = delta) +
      seq_samples + delta
  ),
  family = student(),
  data = rwis_scenario,
  backend = "cmdstanr",
  chains = 4,
  cores = 4,
  adapt_delta = 0.99,
  max_treedepth = 15
)

checks <- map(
  list("retro" = retro_fit, "scenario" = scenario_fit),
  fit_checks
)
checks <- rbindlist(checks, idcol = "model")
