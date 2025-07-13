#' Create a forecast_result object
#' @param location Name of location (string)
#' @param forecast_data A data.frame with date, temp_max, temp_min, precipitation
#' @return An object of class forecast_result
#' @export
create_forecast_result <- function(location, forecast_data) {
  structure(
    list(
      location = location,
      data = forecast_data
    ),
    class = "forecast_result"
  )
}

#' Print method for forecast_result
#' @param x A forecast_result object
#' @param ... Further arguments
#' @export
print.forecast_result <- function(x, ...) {
  cat("Forecast for:", x$location, "\n")
  print(x$data)
}

#' Summary method for forecast_result
#' @param object A forecast_result object
#' @param ... Further arguments
#' @export
summary.forecast_result <- function(object, ...) {
  cat("Forecast Summary for:", object$location, "\n")
  cat("Date Range:", min(object$data$date), "to", max(object$data$date), "\n")
  cat("Max Temp Range:", range(object$data$temp_max), "\n")
  cat("Min Temp Range:", range(object$data$temp_min), "\n")
  cat("Precipitation (Total):", sum(object$data$precipitation), "mm\n")
}
