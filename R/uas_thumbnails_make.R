#' Create image thumbnails
#'
#' Move UAS images into sub-directories by group
#'
#' @param x A list of class 'uas_info'
#' @param flt Flight(s) in x to process (character or numeric vector, default is all)
#' @param output_dir Output directory
#' @param tb_width Thumbnail width
#' @param rotate Rotate the thumbnails by the camera yaw, Logical
#' @param overwrite Overwrite existing files
#' @param use_magick Use functions from the magick package
#' @param stats Report the amount of time it takes to create each thumbnail, logical
#' @param quiet Suppress messages
#' @param flt_idx `r lifecycle::badge("deprecated")` Use `flt` instead
#'
#' @details
#' This will create thumbnail images for the drone images in \code{x}. The default output folder is a sub-directory of each image
#' folder called \emph{map/tb}, which will be created if needed. This location can be overridden with \code{output_dir}.
#' The dimensions of the thumbnails is determined by \code{tb_width}, from which the height is set automatically.
#'
#' \code{flt} allows you to specify a subset of image folders in \code{x} to process. You can pass a vector of flight names (use names(x)
#' to see what those are) or integers.
#'
#' \code{rotate} will rotate the thumbnails by the camera yaw. This can make it easier to match up ground features when viewing the
#' thumbnails in a flight report. Note when thumbnails are rotated the \code{tb_width} parameter sets the width of the image
#' \emph{before} the rotation. The width and height of the rotated thumbnail will vary according to the angle of rotation.
#' If your rotations look off feel free to contact the package author, as this feature is still experimental (i.e., some drones record
#' the yaw of the drone and the yaw of the gimbal camera separately).
#'
#' Thumbnail files will be given an 8-character suffix that looks random but is actually generated from the image contents.
#' This is to prevent clashes when thumnbail files from different flights are 'gathered' into a single folder attached to
#' a Table of Contents folder (see \code{\link{uas_toc}}).
#'
#' If \code{use_magick = TRUE}, it will use resizing functions from the \link[magick]{magick} package. This is slower than the equivalent
#' functions from the \code{imager} package (the default), but may be necessary if you are processing TIFs and don't have
#' \href{https://imagemagick.org}{ImageMagick} installed on your computer (which \code{imager} requires to read TIFs).
#'
#' @return A named list (one element for each directory processed) of thumbnail files created in the output directory
#'
#' @seealso \link{uas_report}
#'
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom digest digest
#' @importFrom crayon yellow green red bold
#' @importFrom imager load.image resize save.image resize_halfXY imrotate
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom dplyr select mutate pull
#' @importFrom sf st_drop_geometry
#' @export

# REMOVED (see additional comment below:
# If \code{use_magick = TRUE}, the function will create the thumbnail images using the ImageMagick command
# line tool (\emph{magick.exe}). This is a slightly faster way to generate thumbnails, which may help when you're
# creating thumbnails for 1000s of images. However you don't get the benefit of a progress bar. This option requires you to have \href{https://imagemagick.org/}{ImageMagick} installed.
# When installing the software, be sure to check the box that says 'Add application directory to your system path'.
# Note you'll have to restart RStudio after you install ImageMagick. You can test whether the command line tool is
# available to R by running \code{findonpath("magick.exe")} (MacOS users should omit the .exe extension).

uas_thumbnails_make <- function(x, flt = NULL, output_dir = NULL, tb_width = 400, rotate = FALSE,
                                overwrite = FALSE, use_magick = FALSE, stats = FALSE,
                                quiet = FALSE, flt_idx = deprecated()) {

  if (is_present(flt_idx)) {
    deprecate_warn("1.9.0", "uas_exp_kml(flt_idx)", "uas_exp_kml(flt)")
    flt <- flt_idx
  }

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  if (use_magick) {
    if (!requireNamespace("magick", quietly = TRUE)) stop("The `magick` package is required")
  }

  # if (use_magick) stop("use_magick has been suspended because testing shows that the command line maxes out at ~700 images. Need to do more tests before making it an option")
  # 5/3/21. use_magick is back in, however I don't use the CLI. Instead I use the magick package. This has
  # proven to be a lot slower than imager, however on systems that don't have ImageMagick installed (and perhaps
  # can't install it due to permissions) magick is the only option to rescale TIFs (which imager can't read)

  ## Verify that that value(s) in flt (if any) are valid
  if (is.null(flt)) {
    flt_idx_use <- 1:length(x)
  } else {
    if (is.numeric(flt)) {
      if (max(flt) > length(x)) stop("flt should not be larger than the number of flights saved in the uas image collection object")
      flt_idx_use <- flt
    } else if (is.character(flt)) {
      if (FALSE %in% (flt %in% names(x))) stop("flight name not found in the uas image collection object")
      flt_idx_use <- which(names(x) %in% flt)
    } else {
      stop("Invalid value for `flt`")
    }
  }

  ## if output_dir was passed, make sure it exists
  if (!is.null(output_dir)) {
    if (!file.exists(output_dir)) stop(paste0("Output directory '", output_dir, "' does not exist"))
  }

  res <- list()

  if (stats) {
    start_time <- Sys.time()
    num_tb_created <- 0
  }

  for (flt_idx in flt_idx_use) {

    ## Get the actual image directory(s)
    img_dir <- unique(dirname(x[[flt_idx]]$pts$img_fn))

    ## Get the output folder
    if (is.null(output_dir)) {

      if (length(img_dir) > 1) stop("When images for one flight live in multiple directories, you must specify `output_dir`")

      output_dir_use <- file.path(img_dir, "map", "tb")
      if (!file.exists(output_dir_use)) {
        dir.create(output_dir_use, recursive = TRUE)
        if (!file.exists(output_dir_use)) stop(paste0("Can't create output directory: ", output_dir_use))
      }

    } else {
      output_dir_use <- output_dir
    }

    ## Get the image filenames
    all_img_fn <- x[[flt_idx]]$pts$img_fn

    ## Compute the image rotation angles

    if (is.null(x[[flt_idx]]$pts$flt_yaw)) {
      all_img_yaw <- x[[flt_idx]]$pts$yaw

    } else {
      ## There could be two yaw values - flight yaw and camera yaw. Images should be rotated by 'net yaw' (difference)

      ## We compare the camera (or gimbal) yaw to the flight yaw.
      ## If they differ by more than 30 degrees, then the camera yaw is 180 degrees turned around.

      all_img_yaw <- x[[flt_idx]]$pts |>
        st_drop_geometry() |>
        select(yaw, flt_yaw) |>
        mutate(yaw_diff = abs(yaw - flt_yaw),
               offset = 180 * (yaw_diff >= 170 & yaw_diff <= 190),
               yaw_adjusted = (yaw + offset) %% 360) |>
        pull(yaw_adjusted)
    }

    ## Switch to Magick if there are TIFs in this list
    magick_force_use <- FALSE
    if (!use_magick) {
      if ((TRUE %in% grepl(".TIF$", all_img_fn, ignore.case = TRUE)) && !imager:::has.magick()) {
          if (!quiet) message(yellow(" - going to use the magick package to resize TIF files"))
          magick_force_use <- TRUE
          if (!requireNamespace("magick", quietly = TRUE)) stop("The `magick` package is required")
      }
    }

    ## Compute the flight name for messages
    if (is.na(null2na(x[[flt_idx]]$metadata$name_short))) {
        flight_name <- x[[flt_idx]]$id
    } else {
        flight_name <- x[[flt_idx]]$metadata$name_short
    }

    if (!quiet) message(yellow$bold(flight_name))

    ## In order to make the image thumbnails have unique filenames (so they can be gathered
    ## in one directory when uas_toc is called), we'll append a suffix.

    ## Unfortunately we can't just use file size which is identical for RAW TIFFs. Otherwise this would work:
    ## all_img_base36 <- sapply(as.integer(file.size(all_img_fn)), int2base36)

    ## Instead, we generate a cryptographic hash based on the first 2000 bytes of the file contents

    if (!quiet) message(yellow(" - computing thumbnail file names..."), appendLF = FALSE)

    all_img_suffixes <- as.character(sapply(all_img_fn, function(x)
        readBin(x, "raw", n = 2000) |> digest(algo = "crc32")))

    if (anyDuplicated(all_img_suffixes) != 0) {
      ## Process entire file contents
      if (!quiet) message(yellow(" - duplicates found, switching to slower method..."), appendLF = FALSE)
      all_img_suffixes <- as.character(sapply(all_img_fn, digest, algo = "xxhash64", serialize = FALSE, length = 10000, file = TRUE))
    }
    if (!quiet) message(yellow("Done."))

    ## Compute the file names of the thumbnail images. All thumbnails will be jpg, even if the orig is TIF
    tb_fn <- tolower(file.path(output_dir_use,
                               paste0(file_path_sans_ext(basename(all_img_fn)),
                                      "_tb", all_img_suffixes, ".jpg")))

    ## Save the base names (minus the path) of the thumbnail files to return
    res[[names(x)[flt_idx]]] <- basename(tb_fn)

    ## If *any* thumbnail needs to be created, go into a loop
    if (FALSE %in% file.exists(tb_fn) || overwrite) {

      if (!quiet) {
        message(yellow(paste0(" - generating thumbnails for ", flight_name)))
      }

      if (use_magick || magick_force_use) {

        # Setup progress bar
        if (!quiet) pb <- txtProgressBar(min = 0, max = length(all_img_fn), style = 3)

        ## Loop through the images
        for (j in 1:length(all_img_fn)) {
          # Update the progress bar
          if (!quiet) setTxtProgressBar(pb, j)

          if (!file.exists(tb_fn[j]) || overwrite) {
            magick::image_write(magick::image_scale(magick::image_read(all_img_fn[j], strip = TRUE), as.character(tb_width)), path = tb_fn[j], format = "jpeg", quality = 75)
            gc()
            if (stats) num_tb_created <- num_tb_created + 1
          }
        }
        if (!quiet) close(pb)

      } else {

        ## USE IMAGER FUNCTIONS
        ## Need to specify imager:: for save.image (same function exists in base R)

        ## Compute height

        first_img_dim <- dim(imager::load.image(all_img_fn[1]))
        height_new <- round(tb_width * first_img_dim[2] / first_img_dim[1], 0)

        ## Reducing resolution in half before using resize seems to slightly improve performance for large RGB images
        scale_factor <- floor(first_img_dim[1] / tb_width)
        halve_b4_resize <- (scale_factor > 3)

        # Setup progress bar
        if (!quiet) pb <- txtProgressBar(min = 0, max = length(all_img_fn), style = 3)

        ## Loop through the images
        for (j in 1:length(all_img_fn)) {

          # Update the progress bar
          if (!quiet) setTxtProgressBar(pb, j)

          if (!file.exists(tb_fn[j]) || overwrite) {

            if (halve_b4_resize) {

              if (rotate) {
                yaw_offset <- 0

                imager::load.image(all_img_fn[j]) |>
                  imager::resize_halfXY() |>
                  imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) |>
                  imager::imrotate(angle = all_img_yaw[j]) |>
                  imager::save.image(file = tb_fn[j])
              } else {
                imager::load.image(all_img_fn[j]) |>
                  imager::resize_halfXY() |>
                  imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) |>
                  imager::save.image(file = tb_fn[j])
              }

            } else {   ## do not half before resize
              if (rotate) {
                imager::load.image(all_img_fn[j]) |>
                  imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) |>
                  imager::imrotate(angle = all_img_yaw[j]) |>
                  imager::save.image(file = tb_fn[j])

              } else {
                imager::load.image(all_img_fn[j]) |>
                  imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) |>
                  imager::save.image(file = tb_fn[j])

              }
            }

            if (stats) num_tb_created <- num_tb_created + 1

          }
        }
        if (!quiet) close(pb)

      }

    } else {
      if (!quiet) message(yellow(" - thumbnails already exist"))
    }

  } ## Done with the loop

  if (stats) {
    time_taken = as.numeric(difftime(Sys.time(), start_time, units="secs"))
    message(yellow(paste0(" - Thumbnails created: ", num_tb_created)))
    if (num_tb_created > 0) {
        message(yellow(paste0(" - Avg time to create each image: ", round(time_taken / num_tb_created, 1), " seconds")))
    }

  }

  if (!quiet) message(green(" - Done."))

  ## Return a list of thumbnails image files created
  invisible(res)
}

