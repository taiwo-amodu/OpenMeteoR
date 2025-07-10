#' Get Forecast by Latitude and Longitude
#'
#' @param lat Latitude (e.g., 27.7 for Kathmandu)
#' @param lon Longitude (e.g., 85.3 for Kathmandu)
#' @param days Number of forecast days (default = 1)
#'
#' @return A data.frame with daily forecast (temperature and precipitation)
#'
#' @importFrom httr2 request req_url_query req_user_agent req_perform resp_body_json
#' @export
get_forecast <- function(lat, lon, days = 1) {
  base_url <- "https://api.open-meteo.com/v1/forecast"

  req <- httr2::request(base_url) |>
    httr2::req_url_query(
      latitude = lat,
      longitude = lon,
      daily = c("temperature_2m_max", "temperature_2m_min", "precipitation_sum"),
      timezone = "auto",
      forecast_days = days
      .multi = "comma"
    ) |>
    httr2::req_user_agent("OpenMeteoR Package")

  resp <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  data.frame(
    date = as.Date(resp$daily$time),
    temp_max = resp$daily$temperature_2m_max,
    temp_min = resp$daily$temperature_2m_min,
    precipitation = resp$daily$precipitation_sum
  )
}
