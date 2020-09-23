load("data-raw/Forecasts_observations_Niamey_2016.rdata")

precip_Niamey_2016 <- tidyr::pivot_wider(
  forecasts,
  names_from = forecast,
  values_from = value
) %>%
  dplyr::full_join(obs, by = "date")

usethis::use_data(precip_Niamey_2016, overwrite = TRUE)
