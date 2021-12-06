# Targets converting observed data
obs_targets <- list(
  # load data from supplied source
  tar_target(
    obs,
    filter_obs(observations, source)
  ),
  # extract the most up to date version of the data
  tar_target(
    current_obs,
    latest_obs(obs)
  ),
  # define the list of dates to forecast at
  tar_target(
    forecast_dates,
    current_obs[!is.na(seq_available), ]$date[-c(1:2)]
  ),
  # split data into that available in each forecast week
  tar_target(
    avail_obs,
    filter_by_availability(obs, date = forecast_dates),
    map(forecast_dates),
    deployment = "worker",
    iteration = "list"
  )
)
