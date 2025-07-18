#' Get weather forecast by coordinates
#' @param lat Latitude
#' @param lon Longitude
#' @param days Number of forecast days
#' @return An object of class forecast_result
#' @param ... Additional arguments (ignored).
#' @export
get_forecast <- function(lat, lon, days = 1) {
  base_url <- "https://api.open-meteo.com/v1/forecast"
  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      latitude = lat,
      longitude = lon,
      daily = c("temperature_2m_max", "temperature_2m_min", "precipitation_sum"),
      timezone = "auto",
      forecast_days = days,
      .multi = "comma"
    ) |>
    httr2::req_user_agent("OpenMeteoR Package")

  resp <- httr2::req_perform(req)
  content <- httr2::resp_body_json(resp)

  daily <- content$daily
  forecast_df <- data.frame(
    date = as.Date(unlist(daily$time)),
    temp_max = unlist(daily$temperature_2m_max),
    temp_min = unlist(daily$temperature_2m_min),
    precipitation = unlist(daily$precipitation_sum)
  )

  return(create_forecast_result(location = paste(lat, lon, sep = ","), forecast_data = forecast_df))
}

