#' @export
plot.forecast_result <- function(x, ...) {
  df <- x$data
  ggplot2::ggplot(df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = temp_max, color = "Max Temp")) +
    ggplot2::geom_line(ggplot2::aes(y = temp_min, color = "Min Temp")) +
    ggplot2::geom_col(ggplot2::aes(y = precipitation * 2, fill = "Precipitation"), alpha = 0.3) +
    ggplot2::labs(
      title = paste("Weather Forecast for", x$location),
      x = "Date",
      y = "Temperature (°C) / Precipitation (x2 mm)"
    ) +
    ggplot2::theme_minimal()
}


