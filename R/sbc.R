
forecast_data <- function(data, strains, model, ...) {
  inits <- forecast.vocs::fv_inits(data, strains = strains)

  fit <- forecast.vocs::fv_sample(data, init = inits, model = model, ...)

  posterior <- forecast.vocs::fv_posterior(fit)
  return(posterior)
}

coverage <- function(value, lower, upper) {
  mean(lower < value & value < upper)
}

sbc_coverage <- function(sbc, by = c("strains", "overdispersion",
                                     "variant_relationship")) {
  by_with_id <- c(by, "dataset")
  parameters <- sbc[, rbindlist(sbc$parameters), by = by_with_id]
  setnames(parameters, "parameter", "variable")
  posterior <- sbc[, rbindlist(forecast), by = by_with_id]
  sbc_unnest <- merge(posterior, parameters, by = c(by_with_id, "parameter"))
  sbc_unnest[,
      .(
          coverage_10 = coverage(sample, q45, q55),
          coverage_20 = coverage(sample, q40, q60),
          coverage_30 = coverage(sample, q35, q65),
          coverage_40 = coverage(sample, q30, q70),
          coverage_50 = coverage(sample, q25, q75),
          coverage_60 = coverage(sample, q20, q80),
          coverage_70 = coverage(sample, q15, q85),
          coverage_80 = coverage(sample, q10, q90),
          coverage_90 = coverage(sample, q5, q95),
          coverage_95 = coverage(sample, q2.5, q97.5),
      ),
      by = c("parameter", by)
  ]
  sbc_melt <- melt(
    sbc_unnest, measure.vars = patterns("coverage"),
    variable.name = "target", value.name = "actual"
  )
  sbc_melt[, target  := gsub("coverage_", "", target)]
  sbc_melt[,
    `:=`(target = as.numeric(target),
         actual = round(actual * 100, 1)
        )
  ]
  return(sbc_melt[])
}