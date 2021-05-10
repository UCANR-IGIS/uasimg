#' Print a summary of UAS metadata
#'
#' Print a summary of metadata from a UAS image collection
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
      cat(yellow("flight:"), names(x)[i], "\n")
      cat(yellow("name_long:"), x[[i]]$metadata$name_long, "\n")
      cat(yellow("name_short:"), green$bold(x[[i]]$metadata$name_short), "\n")

      cameras_lst <- as.list(table(x[[i]]$pts$camera_name))
      cat(yellow("camera:"), paste(names(cameras_lst), collapse = ", "), "\n")
      cat(yellow("images:"), paste(unlist(cameras_lst), collapse = ", "), "\n")
      cat(yellow("area:"), round(msq2acres(x[[i]]$area_m2), 2), "acres\n")
      cat(yellow("size:"), format(x[[i]]$size_mb, big.mark = ","), "MB\n")
      cat(yellow("date flown:"), x[[i]]$date_flown, "\n")
      if (metadata) {
        for (j in 1:length(x[[i]]$metadata)) {
          if (!names(x[[i]]$metadata)[j] %in% c("collection_name", "name_short", "name_long")) {
            cat(yellow(names(x[[i]]$metadata)[j], ":", sep=""),
                x[[i]]$metadata[[j]], "\n")
          }
        }
      }
      cat("\n")

    }
}

