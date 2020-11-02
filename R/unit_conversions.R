#' @title Unit conversion functions
#'
#' @description Unit conversion functions
#'
#' @details These unit conversion functions are vectorized.
#'
#' @param x A numeric vector to convert
#' @return A vector of converted values
#' @export

m2ft <- function(x) return(x * 3.28084)

#' @describeIn m2ft Feet to meters

ft2m <- function(x) return(x * 0.3048)

#' @describeIn m2ft Centimeters to inches

cm2in <- function(x) return(x/2.54)

#' @describeIn m2ft Meters squared to acres

msq2acres <- function(x) return(x * 0.000247105)
