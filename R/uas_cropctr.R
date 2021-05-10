#' Crop out the center of images
#'
#' Crop out the center of images
#'
#' @param x A list of class 'uas_info'
#' @param crp_h Height of the crop box (meters)
#' @param crp_w Width of the crop box (meters)
#' @param dir_out The output directory for the cropped images
#' @param out_prefix A character object that will be pre-pended to output file names
#' @param out_suffix A character object that will be suffixed to output file names (before the extension)
#' @param overwrite Overwrite existing files, T/F
#' @param quiet Suppress messages and printing of the pandoc command line, T/F
#'
#' @details This function will crop around the center of a set of images. This can be used to create
#' a photomosaic. If the photos were taken at nadir (i.e., straight down), the center region of
#' each photo is often the least distorted. If the images have world files created
#' for them (see \code{\link{uas_worldfile}}), the cropped images will have the world files also.
#' If the images have been cropped to minimize the amount of overlap between then, and they have
#' world files, viewing them in GIS software should results in a basic photo mosaic (albeit without
#' any stitching or blending).
#'
#' This function requires the command line version of gdal to be installed. For instructions see
#' \url{http://gdal.org}. Windows users can download a gdal installation file (*.msi) from
#' \url{http://www.gisinternals.com/release.php}, install the software, add the installation
#' directory to the system path (using the Windows Control Panel >> System >> Advanced), and
#' restart RStudio. To see if it worked, run \code{Sys.which("gdal_translate")}.
#'
#' If the images were collected in a grid pattern, you can use the average distance between centers as the value
#' for `crp_h`, and the average distance between flight lines for `crp_w`. This will result in cropped images
#' where the edges are just touching. `crp_h` and `crp_w` should be expressed in meters.
#'
#' @return A vector of the image files created
#'
#' @seealso \code{\link{uas_worldfile}}
#'
#' @importFrom crayon green yellow
#' @importFrom tools file_path_sans_ext file_ext
#' @export

uas_cropctr <- function(x, crp_h = 10, crp_w = 10,
                        dir_out = ".", out_prefix = "", out_suffix = "_crp",
                        overwrite = FALSE, quiet = FALSE) {

  # right now we assume that height and width are going to be numeric constants
  #   - TODO: if height = "median", compute the median pt-to-pt overlap (add @importFrom stats median)

  # - crop images for a poor mans ortho
  # - height and width are expressed in map units
  # -

  ## TODO: there may be times when you need an dir_out for each directory in
  ## x. Could dir_out include {{img_fn}}?

  message(yellow("TODO: allow dir_out to support multiple dirs or a naming template with tokens"))

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  ## Verify that gdal_translate can be found
  if (Sys.which("gdal_translate") == "") {
    stop("Can not find gdal_translate. Please make sure this executable is installed and on the system path.")
  }

  files_gen <- NULL

  #for (img_dir in names(x)) {
  for (i in 1:length(x)) {

    ## Define the output directory for this folder of images
    ## (right now they're all the same, but later will support using a token basedir({{img_fn}})
    dir_out_use <- dir_out

    ## Check if dir_out exists (later, option to create the directory)
    if (!file.exists(dir_out_use)) stop("dir_out does not exist. Please create it and try again.")

    if (identical(x[[i]]$pts, NA)) {
      ## This should never happen
      warning(paste0("Centroids not found for flight ", i, ". Can not crop."))
      next
    }

    ## Present an info message
    if (!quiet) message(green("Saving cropped images in ", path.expand(dir_out_use)))

    ## Get the number of images
    num_imgs <- nrow(x[[i]]$pts)

    ## Create the progress bar
    if (!quiet) pb <- txtProgressBar(min = 0, max = num_imgs, style = 3)

    for (j in 1:num_imgs) {
      if (!quiet) setTxtProgressBar(pb, j)

      imgin_pathfn <- x[[i]]$pts[j, "img_fn", drop = TRUE]

      imgin_fn <- x[[i]]$pts[j, "file_name", drop = TRUE]
      imgin_ext <- file_ext(imgin_fn)
      imgin_base_no_ext <- file_path_sans_ext(imgin_fn)

      imgout_pathfn <- file.path(dir_out_use,
                                 paste0(out_prefix, imgin_base_no_ext,
                                        out_suffix, ".", imgin_ext))

      ## Get the gsd (in cm)
      gsd_cm <- x[[i]]$pts[j, "gsd", drop = TRUE]

      ## Compute the height & width of the crop box (in rows & columns)
      crop_h_pix <- ceiling(crp_h * 100 / gsd_cm)
      crop_w_pix <- ceiling(crp_w * 100 / gsd_cm)

      ## Get the number of rows and columns in the image
      img_height_px <- x[[i]]$pts[j, "img_height", drop = TRUE]
      img_width_px <- x[[i]]$pts[j, "img_width", drop = TRUE]

      ## Compute the starting coordinates for the crop
      xoff <- ceiling((img_width_px / 2) - (crop_w_pix / 2))
      yoff <- ceiling((img_height_px / 2) - (crop_h_pix / 2))

      ## Run gdal_translate

      gdal_res <- system2(command = "gdal_translate",
                          args = c(paste("-srcwin", xoff, yoff, crop_w_pix, crop_h_pix, sep = " "),
                                   "-co QUALITY=90",
                                   shQuote(imgin_pathfn),
                                   shQuote(imgout_pathfn)),
                          stderr = TRUE,
                          stdout = TRUE,
                          wait = TRUE)

      files_gen <- c(files_gen, imgout_pathfn)

    }
    if (!quiet) close(pb)

  }

  if (!quiet) message("Done")
  invisible(files_gen)

}
