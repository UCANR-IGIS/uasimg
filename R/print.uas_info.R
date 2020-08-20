#' Print a summary of a UAS Image Collection object
#'
#' Print a summary of a UAS Image Collection object
#'
#' @param x A UAS Image Collection object
#' @param metadata Show additional meta data info
#' @param ... Other arguments (unused)
#'
#' @details Prints a summary of a drone images metadata collection object
#'
#' @seealso \link{uas_info}
#'
#' @method print uas_info
#' @importFrom crayon yellow green bold
#' @export

print.uas_info <- function(x, metadata = TRUE, ...) {

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    for (i in 1:length(x)) {
      cat(green$bold("\n", x[[i]]$metadata$collection_name), "\n")
      cat(yellow(" dir:"), names(x)[i], "\n")
      cat(yellow(" images:"), nrow(x[[i]]$pts), "\n")
      cat(yellow(" camera:"), x[[i]]$camera_name, "\n")
      cat(yellow(" area:"), round(msq2acres(x[[i]]$area_m2), 2), "acres\n")
      cat(yellow(" size:"), format(x[[i]]$size_mb, big.mark = ","), "MB\n")
      cat(yellow(" date flown:"), x[[i]]$date_flown, "\n")
      if (metadata) {
        for (j in 1:length(x[[i]]$metadata)) {
          cat(yellow(" ", names(x[[i]]$metadata)[j], ":", sep=""),
              x[[i]]$metadata[[j]], "\n")
        }
      }

    }
}

