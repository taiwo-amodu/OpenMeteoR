#' Get forecast by place name
#' @param location A city or town name
#' @param days Number of forecast days
#' @return An object of class forecast_result
#' @export
get_forecast_by_location <- function(location, days = 1) {
  geo_url <- httr2::request("https://nominatim.openstreetmap.org/search") |>
    httr2::req_url_query(q = location, format = "json", limit = 1) |>
    httr2::req_user_agent("OpenMeteoR Package")

  geo_data <- httr2::req_perform(geo_url) |> httr2::resp_body_json()
  if (length(geo_data) == 0) stop("Location not found")

  lat <- as.numeric(geo_data[[1]]$lat)
  lon <- as.numeric(geo_data[[1]]$lon)

  forecast <- get_forecast(lat, lon, days)
  forecast$location <- location  # Overwrite default lat,lon name
  return(forecast)
}

