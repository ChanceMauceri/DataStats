
#' Create an Automatic Scatterplot
#'
#' Creates a scatterplot using two numeric columns
#' from a dataset.
#'
#' @param data A data frame containing the variables to plot.
#' @param x The name of the column to use for the x-axis.
#' @param y The name of the column to use for the y-axis.
#' @param color Optional column name used to color the points.
#'
#' @return A ggplot object displaying a scatterplot.
#'
#' @examples
#' plot_scatter(mtcars, "wt", "mpg")
#' plot_scatter(iris, "Sepal.Length", "Sepal.Width", "Species")
#'
#' @export
plot_scatter <- function(data, x, y, color = NULL) {

  # Check that the input is a data frame
  if (!is.data.frame(data)) {
    stop("Please provide a data frame.")
  }

  # Check that the selected columns exist
  if (!x %in% names(data)) {
    stop("The x variable was not found in the dataset.")
  }

  if (!y %in% names(data)) {
    stop("The y variable was not found in the dataset.")
  }

  # Check that x and y are numeric
  if (!is.numeric(data[[x]]) || !is.numeric(data[[y]])) {
    stop("The x and y variables must be numeric.")
  }

  # Check the optional color column
  if (!is.null(color) && !color %in% names(data)) {
    stop("The color variable was not found in the dataset.")
  }

  # Create scatterplot
  if (is.null(color)) {

    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(x = .data[[x]], y = .data[[y]])
    )

  } else {

    plot <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data[[x]],
        y = .data[[y]],
        color = .data[[color]]
      )
    )
  }

  # Add points and labels
  plot +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggplot2::labs(
      title = "Scatterplot",
      x = x,
      y = y,
      color = color
    ) +
    ggplot2::theme_minimal()

}
