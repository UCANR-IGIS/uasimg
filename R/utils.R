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
#' @keywords internal

null2na <- function(x) {
  ifelse(is.null(x), NA, x)
}

#' Convert NA values to a number
#'
#' @param x A value to be tested
#' @param val The value to replace NAs with
#'
#' @details This is an internal function used to convert NA to something else. This
#' is useful in if() statements when checking the value of an object which may possibly not exist.
#'
#' This function is vectorized.
#'
#' @return The value
#' @keywords internal

na2val <- function(x, val = 0) {
  ifelse(is.na(x), val, x)
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL
