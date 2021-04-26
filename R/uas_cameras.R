#' Cameras
#'
#' Returns the names of known cameras
#'
#' @param names_only Return only camera names
#'
#' @details
#' In order to estimate the GSD and image footprint, certain properties of the camera are required,
#' including the physical dimension of the sensor. Properties of several commonly used cameras have
#' been collected and are stored in a csv file.
#'
#' If your drone camera is not in the database, you'll get an error message when you try to run \code{\link{uas_info}}.
#' In this case, you can contact the package author by email, and/or start an
#' issue on \href{https://github.com/ucanr-igis/uasimg/issues}{GitHub}. Alternately, you can create your own
#' CSV file based on the one that comes with the package (to see where that lives, run
#' \code{system.file("cameras/cameras.csv", package = "uasimg")}), and pass the file name as the value of \code{cameras}
#' in \code{\link{uas_info}}.
#'
#'
#' @return A character vector containing the names of cameras known to the uasimg package
#'
#' @export

uas_cameras <- function(names_only = TRUE) {

  cameras_fn <- system.file("cameras/cameras.csv", package="uasimg")

  cameras_tbl <- uas_readcameras(cameras_fn)

  if (names_only) {
    sort(paste0(cameras_tbl$camera_name, " (", cameras_tbl$filetype, ")"))
  } else {
    cameras_tbl
  }
}

#'
#' @describeIn uas_cameras Read cameras csv file
#' @description Read a cameras.csv file
#' @param cameras_fn The file name (including path) of a cameras.csv file
#' @details This utility function will read a cameras.csv file. Note certain columns
#' are required. To see an example of a valid CSV file, run \code{uas_cameras(names_only = FALSE)}
#' @seealso \code{\link{uas_cameras}}
#' @importFrom readr read_csv col_character col_double col_integer cols_only
#' @export

uas_readcameras <- function(cameras_fn) {
  read_csv(cameras_fn,
            col_types = cols_only(
              make = col_character(),
              model = col_character(),
              filetype = col_character(),
              camera_name = col_character(),
              sensor_width = col_double(),
              sensor_height = col_double(),
              img_width_def = col_integer(),
              img_height_def = col_integer(),
              focal_length_def = col_double(),
              tag_yaw = col_character(),
              tag_elev_agl = col_character(),
              source = col_character()
            ))
}


