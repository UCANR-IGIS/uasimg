#' Export spatial data from a UAS image collection
#'
#' Export spatial data from a UAS image collection
#'
#' @param x A list of class 'uas_info'
#' @param ctr Export the image centroids as a Shapefile, T/F or a filename
#' @param fp Export the image footprints as a Shapefile, T/F or a filename
#' @param mcp Export the minimum convex polygon of the image footprints as a Shapefile, T/F or a filename
#' @param shp_dir The directory where the Shapefiles should be saved. If NULL, they will be saved in a 'map' sub-directory of the image folder
#' @param create_dir Create the output directory if it doesn't exist, T/F
#' @param overwrite Overwrite existing files, T/F
#' @param quiet Supress messages and printing of the pandoc command line, T/F
#'
#' @details
#'
#' \code{ctr}, \code{fp}, and \code{mcp} can be logical (TRUE/FALSE) or a filename
#' (minus the shp extension). If logical values are passed, a Shapefile name will
#' be constructed based on the name of parent folder. You can specify which directory
#' the Shapefiles will be exported to using the \code{shp_dir} argument. The default
#' is to save them in a sub-directory of the images directory called 'map'.
#'
#' @return A vector of Shapefile filenames
#'
#' @seealso \link{uas_info}
#'
#' @import crayon
#' @importFrom sf st_write
#' @export

uas_exp <- function(x, ctr=FALSE, fp=FALSE, mcp=FALSE, shp_dir=NULL, create_dir=TRUE,
                       overwrite=FALSE, quiet=FALSE) {

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    ctrYN <- !identical(ctr, FALSE)
    fpYN <- !identical(fp, FALSE)
    mcpYN <- !identical(mcp, FALSE)

    if (!ctrYN && !fpYN && !mcpYN) {
      stop("Nothing to do! Please set ctr, fp, or mcp to TRUE or a filename.")
    }

    files_saved <- NULL

    for (img_dir in names(x)) {

      if (identical(x[[img_dir]]$pts, NA)) {
        warning(paste0("Centroids not found for ", img_dir, ". Skipping Shapefile export."))
        next
      }

      ## Get the output dir
      if (is.null(shp_dir)) {
        shp_dir_use <- file.path(img_dir, "map")
        if (!file.exists(shp_dir_use) && create_dir) {
          message("Creating ", shp_dir_use)
          dir.create(shp_dir_use)
        }
      } else {
        shp_dir_use <- shp_dir
      }
      if (!file.exists(shp_dir_use)) stop(paste0("Can't find ", shp_dir_use))

      ## Present an info message
      if (!quiet) message(crayon::green("Saving Shapefiles to ", path.expand(shp_dir_use)))

      ## Export centroids
      if (ctrYN) {

        ## Get the Shapefile name
        if (is.character(ctr)) {
          ctr_shp_fn <- ctr
          if (toupper(substr(ctr_shp_fn,nchar(ctr_shp_fn)-3,nchar(ctr_shp_fn)))==".SHP") {
            ctr_shp_fn <- substr(ctr_shp_fn, 0, nchar(ctr_shp_fn) - 4)
          }
        } else {
          ctr_shp_fn <- paste0(basename(img_dir), "_pts")
        }

        ## Compute the complete path and see if it already exists
        ctr_shp_pathfn <- file.path(path.expand(shp_dir_use), paste0(ctr_shp_fn, ".shp"))
        ctr_exists_yn <- file.exists(ctr_shp_pathfn)

        ## Export to Shapefile
        if (ctr_exists_yn && !overwrite) {
          warning(paste0(ctr_shp_fn, ".shp", " already exists. Skipping."))
        } else {
          sf::st_write(x[[img_dir]]$pts, dsn = ctr_shp_pathfn, delete_dsn = ctr_exists_yn, quiet = quiet)
          if (!quiet) message(crayon::green(ctr_shp_fn, ".shp saved", sep = ""))
          files_saved <- c(files_saved, ctr_shp_pathfn)
        }

      }

      ## Export footprints
      if (fpYN) {

        ## Get the Shapefile name
        if (is.character(fp)) {
          fp_shp_fn <- fp
          if (toupper(substr(fp_shp_fn,nchar(fp_shp_fn)-3,nchar(fp_shp_fn)))==".SHP") {
            fp_shp_fn <- substr(fp_shp_fn, 0, nchar(fp_shp_fn) - 4)
          }
        } else {
          fp_shp_fn <- paste0(basename(img_dir), "_fp")
        }

        ## Compute the complete path and see if it already exists
        fp_shp_pathfn <- file.path(path.expand(shp_dir_use), paste0(fp_shp_fn, ".shp"))
        fp_exists_yn <- file.exists(fp_shp_pathfn)

        ## Export to Shapefile
        if (fp_exists_yn && !overwrite) {
          warning(paste0(fp_shp_fn, ".shp", " already exists. Skipping."))
        } else {
          if (identical(x[[img_dir]]$fp, NA)) {
            warning("Footprints not found. Skipping.")
          } else {
            sf::st_write(x[[img_dir]]$fp, dsn = fp_shp_pathfn, delete_dsn = fp_exists_yn, quiet = quiet)
            if (!quiet) message(crayon::green(fp_shp_fn, ".shp saved", sep=""))
            files_saved <- c(files_saved, fp_shp_pathfn)
          }
        }
      }

      ## Export MCP
      if (mcpYN) {

        ## Get the Shapefile name
        if (is.character(mcp)) {
          mcp_shp_fn <- mcp
          if (toupper(substr(mcp_shp_fn,nchar(mcp_shp_fn)-3,nchar(mcp_shp_fn)))==".SHP") {
            mcp_shp_fn <- substr(mcp_shp_fn, 0, nchar(mcp_shp_fn) - 4)
          }
        } else {
          mcp_shp_fn <- paste0(basename(img_dir), "_mcp")
        }

        ## Compute the complete path and see if it already exists
        mcp_shp_pathfn <- file.path(path.expand(shp_dir_use), paste0(mcp_shp_fn, ".shp"))
        mcp_exists_yn <- file.exists(mcp_shp_pathfn)


        ## Export to Shapefile
        if (mcp_exists_yn && !overwrite) {
          warning(paste0(mcp_shp_fn, ".shp", " already exists. Skipping."))
        } else {
          sf::st_write(x[[img_dir]]$mcp, dsn = mcp_shp_pathfn, delete_dsn = mcp_exists_yn, quiet = quiet)
          if (!quiet) message(crayon::green(mcp_shp_fn, ".shp saved", sep=""))
          files_saved <- c(files_saved, mcp_shp_pathfn)
        }

      }
    }

    message("Done")
    invisible(files_saved)
}
