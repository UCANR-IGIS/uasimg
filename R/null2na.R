#' Convert NULL values to NA
#'
#' @param x A value
#'
#' @details This is an internal function used to convert NULL values to NA. This
#' is useful in if() statements when checking the value of an object which may possibly not exist.
#'
#' This function is *not* vectorized.
#'
#' @return The value

null2na <- function(x) {
  ifelse(is.null(x), NA, x)
}
