#' Cameras
#'
#' Returns the names of known cameras
#'
#' @details
#' In order to estimate the GSD and image footprint, certain properties of the camera are required, including the physical dimension of the sensor. Properties of several commonly used cameras have been collected and are stored in a csv file. If your camera is not listed, you may edit the csv file yourself (look in the directory where your R packages are saved), or contact the package author to have it added.
#'
#' @return A character vector containing the names of cameras known in the uavimg package
#'
#' @export

uas_cameras <- function() {
  cameras_fn <- system.file("cameras/cameras.csv", package="uavimg")
  cameras_df <- utils::read.csv(cameras_fn, stringsAsFactors = FALSE)
  return(cameras_df$camera_name)
}
