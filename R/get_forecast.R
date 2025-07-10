#' Get Forecast Using a Place Name
#' @param location A city or town name
#' @param days Number of forecast days
#' @return A data.frame with forecast data
#' @export
get_forecast_by_location <- function(location, days = 1) {
  library(httr2)
  library(jsonlite)
  geo_url <- request("https://nominatim.openstreetmap.org/search") |>
    req_url_query(q = location, format = "json", limit = 1) |>
    req_user_agent("OpenMeteoR Package")
  geo_data <- req_perform(geo_url) |> resp_body_json()
  if (length(geo_data) == 0) stop("Location not found")
  lat <- as.numeric(geo_data[[1]]$lat)
  lon <- as.numeric(geo_data[[1]]$lon)
  return(get_forecast(lat, lon, days))
}
