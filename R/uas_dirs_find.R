#' Search directories for images
#'
#' Find sub-directories which contain images
#'
#' @param x The base directory
#' @param ext A character vector of the file extensions to search for (excluding the '.')
#' @param min_images The minimum number of image files a directory must contain to be included in the results
#' @param exclude_tb Exclude folders called 'tb' (thumbnails)
#'
#' @details This function recurses through sub-directories of `x` and returns those that contain image files.
#' This can be used to help identify folders that need to be cataloged.
#'
#' Only directories that have at least \code{min_images} image files will be returned.
#' If \code{exclude_tb = TRUE}, directories named `tb` (which is where \code{\link{uas_thumbnails_make}} saves thumbnail
#' images) are excluded. Hidden directories and directories that start with `.` are excluded.
#'
#' @return A tibble with 3 columns: \code{path}, \code{ext}, and \code{num_files}.
#'
#' @importFrom dplyr filter group_by summarize select
#' @importFrom tools file_ext
#' @export

uas_dirs_find <- function(x, ext = c("jpg", "tif"), min_images = 3, exclude_tb = TRUE) {

  if (length(x) != 1) stop("x should the name of an existing directory")
  if (!file.exists(x)) stop(paste0("Directory not found: ", x))

  ## Get all the image files
  img_fn <- list.files(x, pattern = paste0(".", ext, "$", collapse = "|"),
                       full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

  ## Parse the file paths putting the results into a data frame
  img_all_tbl <- data.frame(path = normalizePath(dirname(img_fn)),
                            fn = basename(img_fn),
                            ext = file_ext(img_fn))

  ## If required, omit files in folders named 'tb'
  if (exclude_tb) {
    img_all_tbl <- img_all_tbl %>% filter(!grepl("/tb$|\\\\tb$", path))
  }

  if (nrow(img_all_tbl)) {
    ## Return a tibble with the number of images per directory, by extension
    img_all_tbl %>%
      group_by(path, ext) %>%
      summarize(num_files = n(), .groups = "drop") %>%
      filter(num_files >= min_images)
  } else {
    ## No rows in table --> no images found
    NULL
  }

}
