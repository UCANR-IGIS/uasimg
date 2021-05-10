#' Create image thumbnails
#'
#' Move UAS images into sub-directories by group
#'
#' @param x A list of class 'uas_info'
#' @param flt_idx Flight indices in x to process, integer
#' @param output_dir Output directory
#' @param tb_width Thumbnail width
#' @param overwrite Overwrite existing files
#' @param use_magick Use functions from the magick package
#' @param stats Report the amount of time it takes to create each thumbnail, logical
#' @param quiet Suppress messages
#'
#' @details
#' This will create thumbnail images for the drone images in \code{x}. \code{flt_idx} allows you to specify a subset of flights
#' in \code{x} to process. The default output folder is a sub-directory of each image
#' folder called \emph{map/tb}, which will be created if needed. This location can be overridden with \code{output_dir}.
#' The dimensions of the thumbnails is determined by \code{tb_width}, from which the height is set automatically.
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
#' @importFrom imager load.image resize save.image resize_halfXY
#' @importFrom magick image_write image_scale image_read
#' @importFrom tools file_ext file_path_sans_ext
#' @export

# REMOVED:
# If \code{use_magick = TRUE}, the function will create the thumbnail images using the ImageMagick command
# line tool (\emph{magick.exe}). This is a slightly faster way to generate thumbnails, which may help when you're
# creating thumbnails for 1000s of images. However you don't get the benefit of a progress bar. This option requires you to have \href{https://imagemagick.org/}{ImageMagick} installed.
# When installing the software, be sure to check the box that says 'Add application directory to your system path'.
# Note you'll have to restart RStudio after you install ImageMagick. You can test whether the command line tool is
# available to R by running \code{findonpath("magick.exe")} (MacOS users should omit the .exe extension).

uas_thumbnails_make <- function(x, flt_idx = NULL, output_dir = NULL, tb_width = 400,
                                overwrite = FALSE, use_magick = FALSE, stats = FALSE,
                                quiet = FALSE) {

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  # if (use_magick) stop("use_magick has been suspended because testing shows that the command line maxes out at ~700 images. Need to do more tests before making it an option")
  # 5/3/21. use_magick is back in, however I don't use the CLI. Instead I use the magick package. This has
  # proven to be a lot slower than imager, however on systems that don't have ImageMagick installed (and perhaps
  # can't install it due to permissions) magick is the only option to rescale TIFs (which imager can't read)

  ## Verify that that value(s) in flt_idx (if any) are valid
  if (is.null(flt_idx)) {
    flt_idx_use <- 1:length(x)
  } else {
    if (TRUE %in% (flt_idx > length(x))) stop("Invalid value for flt_idx")
    flt_idx_use <- flt_idx
  }

  # if (is.null(img_dir)) {
  #   dirs_use <- names(x)
  # } else {
  #   if (FALSE %in% (img_dir %in% names(x))) stop("Unknown value(s) in img_dir")
  #   dirs_use <- img_dir
  # }

  ## if output_dir was passed, make sure it exists
  if (!is.null(output_dir)) {
    if (!file.exists(output_dir)) stop(paste0("Output directory '", output_dir, "' does not exist"))
  }

  # NO LONGER USING THE IMAGE MAGICK CLI - proved to be buggy
  # if (use_magick) {
  #     if (.Platform$OS.type == "windows") {
  #         magick_exe <- "magick.exe"
  #     } else {
  #         magick_exe <- "magick"
  #     }
  #
  #     magick_fn <- findonpath(magick_exe, status = FALSE)
  #     if (is.null(magick_fn)) {
  #         stop("Cant find the magick command line tool. Please install ImageMagick and make sure its on the path. See help for details.")
  #     }
  # }

  res <- list()

  if (stats) {
    start_time <- Sys.time()
    num_tb_created <- 0
  }

  for (i in flt_idx_use) {

    ## Get the actual image directory(s)
    img_dir <- unique(dirname(x[[i]]$pts$img_fn))

    ## Get the output folder
    if (is.null(output_dir)) {

      if (length(img_dir) > 1) stop("When images for one flight live in multiple directories, you must specify output_dir")

      output_dir_use <- file.path(img_dir, "map", "tb")
      if (!file.exists(output_dir_use)) {
        dir.create(output_dir_use, recursive = TRUE)
        if (!file.exists(output_dir_use)) stop(paste0("Can't create output directory: ", output_dir_use))
      }

    } else {
      output_dir_use <- output_dir
    }

    ## Get the image filenames
    all_img_fn <- x[[i]]$pts$img_fn

    ## Switch to Magick if there are TIFs in this list and image magick app is not installed
    magick_force_use <- FALSE
    if (!use_magick) {
      if ((TRUE %in% grepl(".TIF$", all_img_fn, ignore.case = TRUE)) && !imager:::has.magick()) {
          if (!quiet) message(yellow(" - going to use the magick package to resize TIF files"))
          magick_force_use <- TRUE
      }
    }

    ## Compute the flight name for messages
    if (is.na(null2na(x[[i]]$metadata$name_short))) {
        flight_name <- x[[i]]$id
    } else {
        flight_name <- x[[i]]$metadata$name_short
    }

    if (!quiet) message(yellow$bold(flight_name))

    ## In order to make the image thumbnails have unique filenames (so they can be gathered
    ## in one directory when uas_toc is called), we'll append a suffix.

    ## Unfortunately we can't just use file size which is identical for RAW TIFFs
    ## all_img_base36 <- sapply(as.integer(file.size(all_img_fn)), int2base36)

    ## Instead, we generate a cryptographic hash based on the first 2000 bytes of the file contents

    if (!quiet) message(yellow(" - computing thumbnail file names..."), appendLF = FALSE)

    all_img_suffixes <- as.character(sapply(all_img_fn, function(x)
        readBin(x, "raw", n = 2000) %>%  digest(algo = "crc32")))

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
    res[[names(x)[i]]] <- basename(tb_fn)

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
            image_write(image_scale(image_read(all_img_fn[j], strip = TRUE), as.character(tb_width)), path = tb_fn[j], format = "jpeg", quality = 75)
            gc()
            if (stats) num_tb_created <- num_tb_created + 1
          }
        }
        if (!quiet) close(pb)

        ## Run the magick command line tool
        ## I NO LONGER USE THIS BECAUSE IT FAILS TO WORK IF YOU GIVE IT MORE THAN ~700 IMAGES.
        ## NEED TO READ MORE ABOUT CONVERT.EXE, MAYBE THERE'S AN UPPER LIMIT ON WHAT IT
        ## CAN HANDLE

        # input_file_ext <- file_ext(all_img_fn[1])
        # cmd_args <-paste0("convert \"",
        #                   idir, "/*.", input_file_ext, "\" -set filename:fn_sans_ext \"%t\" -thumbnail ",
        #                   tb_width, " -quality 75% \"",
        #                   output_dir_use, "/%[filename:fn_sans_ext]_tb.jpg\"")
        #
        # ## RUn the command
        # system2(magick_exe, args = cmd_args, stderr = FALSE)
        #
        # ## Rename the files created, adding the suffixes
        # tb_nosuffix_fn <- file.path(output_dir_use, paste0(file_path_sans_ext(basename(all_img_fn)), "_tb.jpg"))
        # for (j in 1:length(tb_nosuffix_fn)) {
        #     file.rename(tb_nosuffix_fn[j], tb_fn[j])
        # }

      } else {

        ## USE IMAGER FUNCTIONS
        ## Need to specify imager:: for save.image (same function exists in base R)

        ## Compute height
        first_img_dim <- dim(load.image(all_img_fn[1]))
        height_new <- round(tb_width * first_img_dim[2] / first_img_dim[1], 0)
        scale_factor <- floor(first_img_dim[1] / tb_width)

        ## Reducing resolution in half before using resize seems to slightly improve performance for large RGB images
        halve_b4_resize <- (scale_factor > 3)

        ## PERFORMANCE EXPERIMENTS

        #imresize(im,1/4) #Quarter size
        # rm(first_img)
        # j = 1

        # system.time(thumb_mthd1 <- imager::load.image(all_img_fn[j]) %>%
        #     imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3))
        #
        # thumb_mthd1 <- imager::load.image(all_img_fn[j]) %>%
        #     imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3)
        #
        # thumb_mthd2 <- imager::load.image(all_img_fn[j]) %>%
        #     imager::resize_halfXY() %>%
        #     imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3)

        #%>% imager::save.image(file = tb_fn[j])
        #imager::imresize(1 / scale_factor) %>%

        # system.time(thumb_mthd2 <- imager::load.image(all_img_fn[j]) %>%
        #     imager::resize_halfXY() %>%
        #     imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) )
        #
        # thumb_mthd2
        #
        # system.time(thumb2 <- imager::load.image(all_img_fn[j]) %>%
        #     imager::resize(size_x = tb_width, size_y = height_new, interpolation = 3) %>%
        #     imager::save.image(file = tb_fn[j]))


        # Setup progress bar
        if (!quiet) pb <- txtProgressBar(min = 0, max = length(all_img_fn), style = 3)

        ## Loop through the images
        for (j in 1:length(all_img_fn)) {

          # Update the progress bar
          if (!quiet) setTxtProgressBar(pb, j)

          if (!file.exists(tb_fn[j]) || overwrite) {

            if (halve_b4_resize) {
              load.image(all_img_fn[j]) %>%
                  resize_halfXY() %>%
                  resize(size_x = tb_width, size_y = height_new, interpolation = 3) %>%
                  imager::save.image(file = tb_fn[j])

            } else {
              load.image(all_img_fn[j]) %>%
                  resize(size_x = tb_width, size_y = height_new, interpolation = 3) %>%
                  imager::save.image(file = tb_fn[j])
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

