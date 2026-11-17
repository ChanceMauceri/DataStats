
#' Summarize Numeric Data
#'
#' Calculates descriptive statistics for a numeric vector,
#' including the mean, median, standard deviation,
#' minimum, maximum, and num of observations.
#'
#' @param x A numeric vector to summarize.
#' @param na.rm Logical. Should missing values (NA) be removed?
#' Defaults to TRUE.
#'
#' @return An object of class "data_summary" containing
#' descriptive statistics.
#'
#' @examples
#' summarize(mtcars$mpg)
#' summarize(c(10, 20, 30, 40, 50))
#'
#' @export
summarize <- function(x, na.rm = TRUE) {

  # Check that the input is numeric
  if (!is.numeric(x)) {
    stop("Please provide a numeric vector.")
  }

  # Check that na.rm is logical
  if (!is.logical(na.rm) || length(na.rm) != 1 || is.na(na.rm)) {
    stop("na.rm must be TRUE or FALSE.")
  }

  # Handle missing values
  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # Check for an empty vector
  if (length(x) == 0) {
    stop("No valid numeric observations available.")
  }

  if (anyNA(x)) {
    stop("Missing values found. Set na.rm = TRUE to remove them.")
  }

  # Calculate descriptive statistics
  results <- list(
    mean = mean(x),
    median = median(x),
    sd = stats::sd(x),
    min = min(x),
    max = max(x),
    n = length(x)
  )

  # Assign an S3 class
  class(results) <- "data_summary"

  return(results)
}


#' Print Data Summary
#'
#' Displays descriptive statistics from a data_summary object.
#'
#' @param x A data_summary object.
#' @param ... Additional arguments.
#'
#' @return The original object, invisibly.
#'
#' @export
print.data_summary <- function(x, ...) {

  cat("\nData Summary\n")
  cat("-------------------------\n")
  cat("Mean:   ", round(x$mean, 2), "\n")
  cat("Median: ", round(x$median, 2), "\n")
  cat("SD:     ", round(x$sd, 2), "\n")
  cat("Min:    ", round(x$min, 2), "\n")
  cat("Max:    ", round(x$max, 2), "\n")
  cat("Count:  ", x$n, "\n")
  cat("-------------------------\n")

  invisible(x)
}
