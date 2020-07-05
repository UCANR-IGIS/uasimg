#' Print a summary of a UAS Image Collection object
#'
#' Print a summary of a UAS Image Colletion object
#'
#' @param x A UAS image collection object
#' @param meta_extra Show additional meta data info
#' @param ... Other arguments (unused)
#'
#' @details Prints a summary of a drone images metadata collection object
#'
#' @seealso \link{uas_info}
#'
#' @method print uas_info
#' @import crayon
#' @export

print.uas_info <- function(x, meta_extra = TRUE, ...) {

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    for (i in 1:length(x)) {
      cat(crayon::green(x[[i]]$meta_extra$collection_name), "\n")
      cat(crayon::yellow(" dir:"), names(x)[i], "\n")
      cat(crayon::yellow(" images:"), nrow(x[[i]]$pts), "\n")
      cat(crayon::yellow(" camera:"), x[[i]]$camera_name, "\n")
      cat(crayon::yellow(" area:"), round(msq2acres(x[[i]]$area_m2), 2), "acres\n")
      cat(crayon::yellow(" size:"), format(x[[i]]$size_mb, big.mark = ","), "MB\n")
      cat(crayon::yellow(" date flown:"), x[[i]]$date_flown, "\n")
      if (meta_extra) {
        for (j in 1:length(x[[i]]$meta_extra)) {
          cat(crayon::yellow(" ", names(x[[i]]$meta_extra)[j], ":", sep=""),
              x[[i]]$meta_extra[[j]], "\n")
        }
      }

    }
}

