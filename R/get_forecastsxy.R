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

  # Perform the request and store the parsed JSON
  resp <- httr2::req_perform(req) |> httr2::resp_body_json()

  # To Check that 'daily' exists
  if (!"daily" %in% names(resp)) stop("No daily forecast data found.")

  # Extract the daily forecast
  daily <- resp$daily

  # Convert nested lists to atomic vectors
  forecast_df <- data.frame(
    date = as.Date(unlist(daily$time)),
    temp_max = unlist(daily$temperature_2m_max),
    temp_min = unlist(daily$temperature_2m_min),
    precipitation = unlist(daily$precipitation_sum)
  )

  return(forecast_df)
}
