#' Export individual images as a pseudo-georectified GeoTIFF
#'
#' Export individual images  as a pseudo-georectified GeoTIFF
#'
#' @param x A list of class 'uas_info'
#' @param flt Which flight(s) in x to process (default is all)
#' @param idx Which rows in x to process (default is all)
#' @param method Resampling method to use for the rotation
#' @param crs coordinate reference system of the output file
#' @param dir_out The output directory (default is the same folder as the source images)
#' @param overwrite Overwrite existing files
#' @param use_tmpdir Use the temp directory
#' @param quiet Show messages.
#'
#' @details
#' This function will export individual image(s) to a georectified GeoTIFF, using the estimated ground footprint modeled from the EXIF data.
#' It should be self-evident that footprints modeled from the recorded above-launch-point elevation, GPS location, and yaw, should
#' be considered approximate at best. Also note if your goal is simply to view individual images in their approximate location
#' in desktop GIS software, this function is probably overkill. Use \code{\link{uas_worldfile}} instead.
#'
#' Note that a prerequisite of this is that you compute footprints when you first create the image collection object (in other words,
#' be sure to pass pass \code{fp = TRUE} when you run \code{\link{uas_info}}).
#'
#' \code{method} is the name of a resampling method used to create the pseudo-georectified image. The default is \code{near} which is fastest
#' and should give good results because the pixel size isn't being altered. See also \code{\link[terra]{resample}}.
#'
#' The crs of the pseudo-georectified will be the crs of the image collection
#' object (UTM). You can override this with the \code{crs} argument. If
#' provided, \code{crs} should be provided as text as a <authority:number> code (ex. "epsg:4326") or WKT syntax.
#' For details see \code{\link[terra]{project}}.
#'
#' Note rectifying (unrotating) images can take a long time and result in much larger image files (because GeoTiffs are uncompressed). You can use the
#' \code{flt} and \code{idx} arguments to specify individual images within an image collection object to export.
#'
#' This function has been tested with JPG files from DJI cameras. It has not yet been fully tested for TIF files from
#' multispectral cameras, and may not work with those formats (contact the package author if you want to try).
#'
#' Un-rotating the images requires write permission for the directory where the images are saved (to write a
#' temporary worldfile). If you don't have write permission where the images reside, pass \code{use_tmpdir = TRUE}.
#' This will make a temporary copy of the image in the temp directory.
#'
#' @return A list of filenames generated.
#'
#' @seealso \code{\link{uas_info}}, \code{\link{uas_worldfile}}
#'
#' @importFrom sf st_crs
#' @importFrom crayon green silver yellow
#' @export

uas_exp_geotiff <- function(x, flt = NULL, idx = NULL,
                        method = c("near", "binlinear", "cubic")[1],
                        crs = NULL, dir_out = ".", overwrite = FALSE,
                        use_tmpdir = FALSE, quiet = FALSE) {

    if (!requireNamespace("terra")) stop("The terra package is needed for this function.")
    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")
    if (!method %in% c("near", "binlinear", "cubic")) stop("Invalid value for method")
    if (is.null(dir_out)) stop("`dir_out` is a required argument")

    ## TODO check if crs is valid, if passed

    ## Create an object to the item returned
    reslt <- list()

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
        stop("Invalid value for flt")
      }
    }

    ## Create an object to store temporary world files (if any) for auto-deletion
    tmpfiles_fn <- character(0)
    on.exit(lapply(tmpfiles_fn, unlink), add = TRUE)

    ## Loop through the selected flights
    for (flt_idx in flt_idx_use) {

      ## Loop through the selected images
      if (is.null(idx)) {
        idx_use <- 1:nrow(x[[flt_idx]]$pts)
      } else {
        if (max(idx) > nrow(x[[flt_idx]]$pts)) stop("idx should not be larger than the number of images in the flight")
        idx_use <- idx
      }

      ## Create an object to record the images exported from this flight
      files_gen <- NULL

      ## What is the CRS of the modeled footprints?
      fp_crs <- st_crs(x[[flt_idx]]$pts)

      ## Loop thru the selected images in the selected flight
      for (img_idx in idx_use) {

        ## Get the image full path
        img_fnfull <- x[[flt_idx]]$pts[img_idx, "img_fn", drop = TRUE]

        ## Compute the expected worldfile file name
        ## We are using aux.xml because it saves the CRS (where jpw does not, and/or isn't read by terra::rast())
        xml_fn <- paste0(img_fnfull, ".aux.xml")

        ## Generate the name of the output file
        tiff_out <- file.path(dir_out, gsub(".jpg$|.jpeg$", ".tif", basename(img_fnfull), ignore.case = TRUE))

        if (file.exists(tiff_out) && !overwrite) {
          if (!quiet) message(" - ", silver(tiff_out, "already exists. Skipping."))
        } else {

          ## See if we have to make a worldfile
          if (file.exists(xml_fn)) {
            if (!quiet) message(silver(" - going to use existing worldfile"))

          } else {
            ## Create the worldfile file if needed (temporarily). It must be in the same folder
            ## as the image for terra::rast() to recognize it

            ## If needed, copy the image to the temp directory
            if (use_tmpdir) {
              file.copy(from = img_fnfull, to = tempdir())
              imgtmp_fn <- file.path(tempdir(), basename(img_fnfull))
              img_fnfull <- imgtmp_fn
              tmpfiles_fn <- c(tmpfiles_fn, imgtmp_fn)
              x[[flt_idx]]$pts[img_idx, "img_fn"] <- imgtmp_fn
              xml_fn <- paste0(imgtmp_fn, ".aux.xml")
            }

            xml_fn_lst <- uas_worldfile(x, flt = flt_idx, idx = img_idx, aux.xml = TRUE, wld = FALSE, quiet = TRUE)
            if (!identical(xml_fn_lst[[1]], xml_fn)) stop("The worldfile created has an unexpected name")

            ## Add this to the queue of temporary world files for auto-deletion when the function exits
            tmpfiles_fn <- c(tmpfiles_fn, xml_fn)

          }

          ## Read in the image (with the worldfile) - Should be fast
          ## We suppress warnings so it doesn't give a warning that the image is rotated (which we already know)
          r1 <- suppressWarnings(terra::rast(img_fnfull))

          if (!is.null(crs)) {
            r1 <- terra::project(r1, crs)
          }

          ## TODO: Right here I need to verify this is rotated
          ## Best I could come up with is run gdalInfo and use regex to see if it contains a 'Geotransform =' section
          ## xx <- sf::gdal_utils(util = "info", source = "D:/Data/DroneData/mavicpro_jpg/DJI_0402.jpg")
          ## But see https://github.com/rspatial/terra/issues/1229

          ## Rectify the image (rotate it)
          if (!quiet) message(silver(" - georectifying", basename(img_fnfull), "(this can take a while)"))
          r1_rect <- try(terra::rectify(r1, method = method, filename = tiff_out), silent = TRUE)

          if (is(r1_rect, "try-error")) {
            if (!quiet) message(" - ", yellow(basename(img_fnfull), "was not rectified (perhaps because it isn't rotated)"))
            terra::writeRaster(r1, filename = tiff_out)
          } else {
            if (!quiet) message(silver(" -", tiff_out, "saved"))
            files_gen <- c(files_gen, tiff_out)
          }

          ## Shouldn't have to do this anymore, don't with rectify
          ## terra::writeRaster(r1_rect, filename = tiff_out)

        }


      } # for (img_idx in idx_use)

      ## Append this list of world files to the result
      reslt[[names(x)[flt_idx]]] <- files_gen

    } #for (flt_idx in flt_idx_use)

    if (!quiet) message(green(" - Done!"))

    invisible(reslt)
}
