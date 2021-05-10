#' Convert file format
#'
#' Convert image file formats while preserving EXIF data
#'
#' @param x A vector of filenames (with path), or an object of class 'uas_info'
#' @param dir_out The output directory(s)
#' @param idx Row numbers of the images (optional)
#' @param format_out The output format
#' @param quality The quality level for jpg compression (0..100), larger numbers produce better image quality
#' @param copy_exif Whether to copy the original EXIF info to the new file, logical
#' @param overwrite Overwrite existing files, logical
#' @param quiet Suppress messages, logical
#'
#' @details This function converts image formats. Images are converted using the `magick` package
#' (which calls ImageMagick libraries), while EXIF data is copied using exiftool. Supported output formats include JPG and TIFF.
#'
#' \code{x} can be a vector of image file names (including the path), or a image metadata object (created by \code{\link{uas_info}}). If \code{idx}
#' (row numbers) is passed, it will be used to select images to convert.
#'
#' \code{dir_out} is the name of the directory where converted images will be output. If NULL, images will be placed in the same
#' directory as the input images. If \code{x }is an object of class `uas_info`, \code{dir_out} can be a vector of directories equal in length to the number of folders
#' indexed in \code{x} (in which case the converted images will be placed in the corresponding output directory).
#'
#' \code{quality} sets the compression ratio for jpg images. To preserve a high level of fidelity and improve the odds of successful stitching, it
#'  is recommended you set \code{quality} to 90 or higher. If \code{format_out = "tif"}, the output files will be saved as uncompressed tif files.
#'
#' @examples
#' \dontrun{
#' ## Make a list of DNG files to convert
#' files_dng <- list.files("D:/uas/mavic_pro/raw",
#'                         pattern = ".DNG$",
#'                         full.names = TRUE,
#'                         ignore.case = TRUE)
#'
## Convert them to JPG
#' uas_convert(files_dng,
#'             dir_out = "D:/uas/mavic_pro/jpg",
#'             format_out = "jpg")
#' }
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom magick image_read image_write
#' @importFrom tools file_path_sans_ext
#' @importFrom exiftoolr exif_call
#' @export

uas_convert <- function(x, dir_out = NULL, idx = NULL, format_out = c("jpg", "tif")[1], quality = 90, copy_exif = TRUE, overwrite = FALSE,
                        quiet = FALSE) {

  if (!is.null(dir_out)) {
    if (FALSE %in% file.exists(dir_out)) stop(paste0("Can't find output directory(s): ", paste0("`", dir_out[!file.exists(dir_out)], "`", collapse = ", ")))
  }

  ## Build up a list of input images
  if (inherits(x, "uas_info")) {

    if (is.null(dir_out)) {
      ## TODO: THIS NEEDS TO BE UPDATED, WHAT HAPPENS IF THE UAS_INFO OBJECT HAS FLIGHTS THAT
      ## SPAN MULTIPLE DIRECTORIES.
      dir_out_use <- names(x)

    } else {

      if (length(dir_out) == 1) {
        dir_out_use <- rep(dir_out, length(x))

      } else if (length(dir_out) != length(x)) {
        stop("length of dir_out and x not equal")

      } else {
        dir_out_use <- dir_out
      }

    }

    ## Create a blank list for the list of files

    in_files_lst <- list()

    for (i in 1:length(x)) {
      idx_use <- 1:nrow(x[[i]]$pts)
      if (!is.null(idx)) idx_use <- base::intersect(idx_use, idx)
      in_files_lst[[i]] <- x[[i]]$pts$img_fn[idx_use]
    }

  } else {
    ## x is a vector of files
    in_files_lst <- list(x)

    ## Get the out directory
    if (is.null(dir_out)) {
      dir_out_use <- dirname(x[1])
    } else {
      dir_out_use <- dir_out
    }

  }

  extension_lst <- list(
    jpg = ".jpg",
    jpeg = ".jpg",
    tif = ".tif"
  )

  ## Make list of format extensions
  if (!format_out %in% names(extension_lst)) stop(paste0("Unknown output format: ", format_out))

  files_processed <- 0
  if (!quiet) pb <- txtProgressBar(min = 0, max = length(unlist(in_files_lst)), style = 3)

  res <- NULL

  for (k in 1:length(in_files_lst)) {

    for (in_fn in in_files_lst[[k]]) {

      # Update the progress bar
      if (!quiet) {
        files_processed <- files_processed + 1
        setTxtProgressBar(pb, files_processed)
      }

      if (!file.exists(in_fn)) {
        warning(paste0(in_fn, " not found"))

      } else {

        ## Construct a name for the output file
        out_fn <- in_fn %>%
          basename() %>%
          file_path_sans_ext() %>%
          paste0(extension_lst[[format_out]]) %>%
          file.path(dir_out_use[k], .)

        if (!file.exists(out_fn) || overwrite) {

          try_read_image <- try({in_mag <- image_read(in_fn)}, silent = TRUE)

          if (is(try_read_image, "try-error")) {
            warning(paste0("Could not read: ", basename(in_fn)))

          } else {

            ## Write the output image
            if (format_out == "tif") {
              ## After a small bit of trial-and-error, I decided to make no compression
              ## the default. Zip compression gave a 10% ratio; LZW was bigger than uncompressed
              image_write(in_mag,
                          path = out_fn,
                          format = format_out,
                          compression = c("none", "LZW", "Zip")[1])
            } else {
              image_write(in_mag,
                          path = out_fn,
                          format = format_out,
                          quality = quality)
            }

            ## message(paste0(in_fn, " --> ", out_fn))

            if (copy_exif) {
              exif_call(args = c("-overwrite_original", "-tagsFromFile", in_fn), path = out_fn)
            }

            res <- c(res, out_fn)

          }

        }

      }

    }


  }


  if (!quiet) close(pb)

  invisible(res)

}


