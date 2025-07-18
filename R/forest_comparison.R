#' Create forecast_comparison object
#'
#' @param locations A character vector of city names
#' @param days Number of forecast days
#' @return An object of class forecast_comparison
#' @param ... Additional arguments (ignored).
#' @export
compare_forecasts <- function(locations, days = 3) {
  results <- lapply(locations, function(city) {
    res <- get_forecast_by_location(city, days)
    data <- res$data
    data$location <- city
    return(data)
  })

  combined_data <- do.call(rbind, results)

  structure(
    list(cities = locations, data = combined_data),
    class = "forecast_comparison"
  )
}

#' Print method for forecast_comparison
#' @param x forecast_comparison object
#' @export
print.forecast_comparison <- function(x, ...) {
  cat("Forecast Comparison for:\n")
  cat(paste("- ", x$cities, collapse = "\n"), "\n\n")
  print(head(x$data))
}

#' Summary method for forecast_comparison
#' @param object forecast_comparison object
#' @export
summary.forecast_comparison <- function(object, ...) {
  cat("Summary of forecast comparison\n")
  print(aggregate(. ~ location, data = object$data[, c("location", "temp_max", "temp_min", "precipitation")], mean))
}

#' Plot method for forecast_comparison
#' @param x forecast_comparison object
#' @export
plot.forecast_comparison <- function(x, ...) {
  ggplot(x$data, aes(x = date, y = temp_max, color = location, group = location)) +
    geom_line(size = 1.2) +
    labs(title = "Max Temperature Forecast Comparison", y = "Temp Max (\u00b0C)", x = "Date") +
    theme_minimal()
}
