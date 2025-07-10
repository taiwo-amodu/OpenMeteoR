#' Get Forecast Using a Place Name
#'
#' @param location A city or town name
#' @param days Number of forecast days
#' @return A data.frame with forecast data
#'
#' @importFrom httr2 request req_user_agent req_url_query req_perform resp_body_json
#' @export
get_forecast_by_location <- function(location, days = 1) {
  geo_url <- httr2::request("https://nominatim.openstreetmap.org/search") |>
    httr2::req_url_query(q = location, format = "json", limit = 1) |>
    httr2::req_user_agent("OpenMeteoR Package")

  geo_data <- httr2::req_perform(geo_url) |> httr2::resp_body_json()

  if (length(geo_data) == 0) stop("Location not found")

  lat <- as.numeric(geo_data[[1]]$lat)
  lon <- as.numeric(geo_data[[1]]$lon)

  return(get_forecast(lat, lon, days))
}
