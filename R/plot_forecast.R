
#' Plot Forecast Data
#' @param forecast_df Data frame from get_forecast
#' @return A ggplot object
#' @importFrom ggplot2 ggplot aes geom_line geom_col labs theme_minimal
#' @importFrom rlang .data
#' @export
plot_forecast <- function(forecast_df) {
  library(ggplot2)
  ggplot(forecast_df, aes(x = as.Date(date))) +
    geom_line(aes(y = temp_max), color = "red") +
    geom_line(aes(y = temp_min), color = "blue") +
    geom_col(aes(y = precipitation * 2), fill = "lightblue", alpha = 0.3) +
    labs(
      title = "Weather Forecast",
      y = "Temp (°C) / Precipitation (x2 mm)", x = "Date"
    ) +
    theme_minimal()
}
