#' Move UAS Images into Sub-Directories by Flight
#'
#' Move UAS Images into Sub-Directories by Flight
#'
#' @param x A list of class 'uas_info'
#' @param thresh_units Units (median sampling interval or seconds)
#' @param thresh_val Thresshold value
#' @param dirnames Directory names (template??)
#' @param init_flt Initial flight number
#' @param min_images Minimum number of images to consider a flight worth moving
#' @param preview Preview the change only
#' @param copymove Move or copy
#' @param basedir Base dir
#' @param alt Range of altitude to move
#' @param quiet Supress messages
#'
#' @details This will move the image files into subdirectories
#'
#' @return A \link{uas_info} object
#'
#' @seealso \link{uas_info}
#'
#' @export

uas_move <- function(x, thresh_units=c("msi", "secs")[1], thresh_val=10, dirnames="", init_flt=1, min_images=5, preview=FALSE, copymove="move", basedir=NULL, alt=NULL, quiet=FALSE) {

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

