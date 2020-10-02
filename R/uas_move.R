#' Move UAS images into sub-directories by group
#'
#' Move UAS images into sub-directories by group
#'
#' @param x A list of class 'uas_info'
#' @param copymove Move or copy
#' @param basedir Base dir
#' @param quiet Suppress messages
#'
#' @details NOT YET SUPPORTED
#'
#' This will move groups of image files into subdirectories based on the group they fall into.
#'
#' @return A \link{uas_info} object
#'
#' @seealso \link{uas_info}
#'
#' @export

uas_move <- function(x, copymove="move", basedir=NULL, quiet=FALSE) {

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    ## Supported tokens in dirnames
    ## {start_time}
    ## {end_time}
    ## {flt_num}
    ## {date}
    ## {alt}

    ## Loop through the elements of x
    ## Compute the interval between images
    ## Find the median interval

    ## Return a new uas_info object
    return(invisible(NULL))
}

