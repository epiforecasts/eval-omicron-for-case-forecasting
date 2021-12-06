process_rwis <- function(rwis) {
  fit_data <- copy(rwis)
  fit_data <- fit_data[, log_baseline := log(baseline)][!is.na(share_delta)]
  fit_data[, forecast_date := as.factor(forecast_date)]
  fit_data[, date := as.factor(date)]
  fit_data[, horizon_minus_one := horizon - 1]
  fit_data[
    ,
    variant_relationship := factor(
      variant_relationship,
      levels = c("pooled", "scaled", "independent")
    )
  ]
  fit_data[
    ,
    overdispersion := factor(
      fcase(
        overdispersion == TRUE, "yes",
        overdispersion == FALSE, "no"
      ),
      levels = c("yes", "no")
    )
  ]
  return(fit_data[])
}

summarise_rwis <- function(rwis, by = c(
                             "overdispersion",
                             "variant_relationship",
                             "horizon"
                           )) {
  rwis[,
    as.list(summary(rwis)),
    by = by
  ][order(Median)]
}

fit_checks <- function(fit) {
  out <- data.table(
    effs = list(conditional_effects(fit)),
    smooths = list(conditional_smooths(fit)),
    pp_check = list(pp_check(fit))
  )

  out[, plot_effs := list(plot(effs[[1]], ask = FALSE))]
  out[, plot_smooths := list(plot(smooths[[1]], ask = FALSE))]
  return(out[])
}
