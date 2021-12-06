filter_obs <- function(observations, location) {
  if (missing(location)) {
    location <- NULL
  }
  obs <- data.table::copy(observations)

  if (!is.null(location)) {
    loc <- location
    obs <- obs[location_name %in% loc]
  }
  return(obs)
}

#' Save a plot and return the ppath for targets
#' @importFrom ggplot2 ggsave
save_plot <- function(plot, filename, ...) {
  ggplot2::ggsave(filename, plot, ...)
  return(filename)
}

#' Save a dataframe to a csv and return the path for targets
save_csv <- function(dt, path) {
  data.table::fwrite(dt, path)
  return(path)
}