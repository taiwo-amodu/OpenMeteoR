#' Create a forecast_result object
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @importFrom utils head
#' @importFrom stats aggregate
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
#' @param ... Further arguments passed to or from other methods.
#' @export
print.forecast_result <- function(x, ...) {
  cat("Forecast for:", x$location, "\n")
  print(x$data)
}

#' Summary method for forecast_result
#' @param object A forecast_result object
#' @param ... Further arguments passed to or from other methods.
#' @export
summary.forecast_result <- function(object, ...) {
  cat("Forecast Summary for:", object$location, "\n")
  cat("Date Range:", min(object$data$date), "to", max(object$data$date), "\n")
  cat("Max Temp Range:", range(object$data$temp_max), "\n")
  cat("Min Temp Range:", range(object$data$temp_min), "\n")
  cat("Precipitation (Total):", sum(object$data$precipitation), "mm\n")
}

#' Plot method for forecast_result
#' @param x A forecast_result object
#' @param ... Further arguments passed to or from other methods.
#' @export
plot.forecast_result <- function(x, ...) {
  df <- x$data
  ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = temp_max), color = "tomato", size = 1.2) +
    ggplot2::geom_line(ggplot2::aes(y = temp_min), color = "steelblue", size = 1.2) +
    ggplot2::geom_col(ggplot2::aes(y = precipitation * 2), fill = "lightblue", alpha = 0.4) +
    ggplot2::labs(
      title = paste("Forecast for", x$location),
      x = "Date",
      y = "Temperature (\u00b0C) / Precipitation (x2 mm)"
    ) +
    ggplot2::theme_minimal()
}



#' Create a forecast_comparison object
#' @param locations A character vector of city names
#' @param days Number of forecast days (default is 3)
#' @return An object of class forecast_comparison
#' @export
compare_forecasts <- function(locations, days = 3) {
  results <- lapply(locations, function(city) {
    res <- get_forecast_by_location(city, days)
    df <- res$data
    df$location <- city
    df
  })

  combined <- do.call(rbind, results)
  structure(
    list(cities = locations, data = combined),
    class = "forecast_comparison"
  )
}

#' Print method for forecast_comparison
#' @param x A forecast_comparison object
#' @param ... Further arguments passed to or from other methods.
#' @export
print.forecast_comparison <- function(x, ...) {
  cat("Forecast comparison for cities:\n")
  cat(paste(" -", x$cities), sep = "\n")
  cat("\nFirst few rows of data:\n")
  print(head(x$data))
}

#' Summary method for forecast_comparison
#' @param object A forecast_comparison object
#' @param ... Further arguments passed to or from other methods.
#' @export
summary.forecast_comparison <- function(object, ...) {
  cat("Summary of forecast across cities:\n")
  summary_data <- aggregate(. ~ location, data = object$data[, c("location", "temp_max", "temp_min", "precipitation")], mean)
  print(round(summary_data, 2))
}

#' Plot method for forecast_comparison
#' @param x A forecast_comparison object
#' @param ... Further arguments passed to or from other methods.
#' @export
plot.forecast_comparison <- function(x, ...) {
  ggplot2::ggplot(x$data, ggplot2::aes(x = date, y = temp_max, color = location, group = location)) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::labs(
      title = "Max Temperature Forecast Comparison",
      x = "Date",
      y = "Max Temp (\u00b0C)"
    ) +
    ggplot2::theme_minimal()
}
