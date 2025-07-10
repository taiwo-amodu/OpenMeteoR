#' Plot Weather Forecast
#'
#' @param forecast_df A data frame returned from `get_forecast_by_location()` or `get_forecast()`
#'
#' @return A ggplot object showing temperature and precipitation
#' @importFrom ggplot2 ggplot aes geom_line geom_col labs theme_minimal
#' @export
plot_forecast <- function(forecast_df) {
  utils::globalVariables(c("temp_max", "temp_min", "precipitation"))

  ggplot2::ggplot(forecast_df, ggplot2::aes(x = date)) +
    ggplot2::geom_line(ggplot2::aes(y = temp_max, color = "Max Temp")) +
    ggplot2::geom_line(ggplot2::aes(y = temp_min, color = "Min Temp")) +
    ggplot2::geom_col(ggplot2::aes(y = precipitation * 2, fill = "Precipitation"), alpha = 0.3) +
    ggplot2::labs(
      title = "Weather Forecast",
      y = "Temp (\u00B0C) / Precipitation (x2 mm)",
      x = "Date",
      color = "Legend",
      fill = "Legend"
    ) +
    ggplot2::theme_minimal()
}

