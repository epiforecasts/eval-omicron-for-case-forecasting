
plot_single_strain_predictions <- function(forecasts, obs, likelihood = TRUE) {
  sel_lik <- likelihood
  name <- ifelse(sel_lik, "posterior", "prior")
  p <- suppressWarnings(
    forecasts[likelihood == sel_lik] |>
      forecast.vocs::unnest_posterior() |>
      forecast.vocs::plot_cases(obs, log = TRUE) +
      ggplot2::facet_grid(
        ggplot2::vars(overdispersion),
        ggplot2::vars(forecast_date)
      ) +
      ggplot2::guides(col = "none", fill = "none")
  )
  file <- suppressWarnings(
    save_plot(
      p,
      here::here(
        "figures", "validation",
        paste0("single_", name, "_prediction.png")
      ),
      height = 9, width = 12
    )
  )
  return(file)
}

plot_two_strain_predictions <- function(forecasts, obs, likelihood = TRUE,
                                        overdispersion = TRUE, type = "cases") {
  sel_lik <- likelihood
  overdisp <- overdispersion
  name <- ifelse(sel_lik, "_posterior", "_prior")
  oname <- ifelse(overdisp, "_overdispersion", "")
  type <- match.arg(type, choices = c("cases", "voc"))
  plot <- ifelse(type %in% "cases",
    forecast.vocs::plot_cases,
    forecast.vocs::plot_voc
  )

  dtf <- forecasts[likelihood == sel_lik][overdispersion == overdisp]
  dtf <- forecast.vocs::unnest_posterior(dtf)
  dtf <- dtf[
    ,
    variant_relationship := stringr::str_to_title(variant_relationship)
  ]
  p <- suppressWarnings(
    dtf |>
      plot(obs) +
      ggplot2::facet_grid(
        ggplot2::vars(variant_relationship),
        ggplot2::vars(forecast_date)
      )
  )
  file <- suppressWarnings(
    save_plot(
      p,
      here::here(
        "figures", "validation",
        paste0("two_", type, name, oname, "_prediction.png")
      ),
      height = 9, width = 12
    )
  )
  return(file)
}
