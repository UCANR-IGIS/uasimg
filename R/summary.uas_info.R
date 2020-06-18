#' Show a summary of a drone images metadata collection object
#'
#' Prints a summary of a drone images metadata collection object
#'
#' @param object A drone images metadata object
#' @param ... Other arguments (unused)
#'
#' @details Prints a summary of a drone images metadata collection object
#'
#' @seealso \link{uas_info}
#'
#' @export

summary.uas_info <- function(object, ...) {

    if (!inherits(object, "uas_info")) stop("x should be of class \"uas_info\"")

    for (x in names(object)) {
      cat(toupper(basename(x)), "\n")
      cat("  dir:", x, "\n")
      cat("  images:", nrow(object[[x]]$pts), "\n")
      cat("  area:", msq2acres(object[[x]]$area_m2), "acres\n")
    }
}

