#' Converts an integer to a base36 string
#' @param int Integer input
#' @details Note that int \emph{must} be an integer. You can convert a
#' numeric object to integer with \code{as.integer}.
#'
#' Note this function is not vectorized. Wrap it in sapply if needed.

#' Thanks to Joshua Ulrich for providing the source:
#' https://stackoverflow.com/questions/36036404/convert-an-integer-to-base36

int2base36 <- function(int) {
  stopifnot(is.integer(int) || int < 0)

  base36 <- c(as.character(0:9),letters)
  result <- character(6)
  i <- 1L
  while (int > 0) {
    result[i] <- base36[int %% 36L + 1L]
    i <- i + 1L
    int <- int %/% 36L
  }
  return(paste(result, sep="", collapse=""))
}
