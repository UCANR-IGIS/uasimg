#' @describeIn uas_exp_kml Export flight info to Shapefile
#' @importFrom crayon green yellow red
#' @importFrom sf st_write
#' @importFrom dplyr mutate
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @export

uas_exp_shp <- function(x, flt = NULL, ctr = FALSE, fp = FALSE, mcp = FALSE,
                    combine_feats = FALSE, combine_fn = NULL,
                    output_dir = NULL, out_fnbase = NULL, create_dir = TRUE, overwrite = FALSE, quiet = FALSE,
                    flt_idx = deprecated()) {

    if (lifecycle::is_present(flt_idx)) {
      lifecycle::deprecate_warn("1.9.0", "uas_exp_kml(flt_idx)", "uas_exp_kml(flt)")
      flt <- flt_idx
    }

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    if (!ctr && !fp && !mcp) {
      stop("Nothing to do! At least one of `ctr`, `fp`, or `mcp` must be TRUE.")
    }

    ## Verify that that value(s) in flt_idx (if any) are valid
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

    if (combine_feats) {

      if (is.null(output_dir) || is.null(combine_fn)) {
        stop("To combine features into a single Shapefile or KML, you must pass `output_dir` and combine_fn`")

      } else {
        if (!quiet) message(green(" - combined features will be saved to:", path.expand(output_dir)))
        if (grepl("\\.", combine_fn)) warning("`combine_fn` should be the base of an output file name with no extension")

        ## Create NULL objects to hold 'combined' layers

        if (fp) fp_combined_sf <- NULL
        if (mcp) mcp_combined_sf <- NULL

        if (ctr) {
          ctr_combined_sf <- NULL
          ctr_combined_lst <- list() ## save placemarks
          icon_base <- "https://ucanr-igis.github.io/uasimg/kml_icons/"
          icon_png <- c("blu-circle-lv.png", "ylw-circle-lv.png", "red-circle-lv.png",
                        "magenta-rev-circle-lv.png", "red-rev-circle-lv.png", "grn-circle-lv.png", "blue-rev-circle-lv.png", "organe-circle-lv.png",
                        "purple-circle-lv.png", "tan-circle-lv.png", "bluegrn-circle-lv.png", "wht-circle-lv.png")
        }


      }
    }

    ## Create an object to store the files created
    files_saved <- NULL

    #for (img_dir in img_dir_use) {
    for (flt_idx in flt_idx_use) {

      ## Get the actual image directory(s)
      img_dir <- unique(dirname(x[[flt_idx]]$pts$img_fn))

      ## Get the output dir
      if (is.null(output_dir)) {

        if (length(img_dir) > 1) stop("When images for one flight live in multiple directories, you must specify output_dir")

        output_dir_use <- file.path(img_dir, "map")
        if (!file.exists(output_dir_use) && create_dir) {
          if (!quiet) message("- creating ", output_dir_use)
          if (!dir.create(output_dir_use, recursive = TRUE)) stop(paste0("Unable to create ", output_dir_use))
        }
      } else {
        output_dir_use <- output_dir
      }
      if (!file.exists(output_dir_use)) stop(paste0("Can't find ", output_dir_use))
      if (!quiet && !combine_feats) message(green(" - saving files to", path.expand(output_dir_use)))

      ## Define the base file name
      if (is.null(out_fnbase)) {

        if (!is.na(x[[flt_idx]]$metadata$name_short %>% null2na())) {
          fnbase <- x[[flt_idx]]$metadata$name_short
        } else {
          fnbase <- x[[flt_idx]]$id
        }
      } else {
        fnbase <- out_fnbase
      }

      ## Export centroids
      if (ctr) {

        if (combine_feats) {

          ctr_combined_sf <-
            rbind(ctr_combined_sf,
                  x[[flt_idx]]$pts %>%
                    select(file_name, date_time, gps_lat, gps_long, gps_alt, yaw, make, model) %>%
                    mutate(flight = fnbase))

        } else {
          ## Not combined features
          ctr_fn <- paste0(fnbase, "_ctr")

          ## Compute the complete path and see if it already exists
          ctr_shp_pathfn <- file.path(path.expand(output_dir_use), paste0(ctr_fn, ".shp"))

          ## Export to Shapefile
          if (file.exists(ctr_shp_pathfn) && !overwrite) {
            if (!quiet) message(yellow(paste0(" - ", ctr_fn, ".shp", " already exists. Skipping.")))

          } else {
            st_write(x[[flt_idx]]$pts %>%
                       select(file_name, date_time, gps_lat, gps_long, gps_alt, yaw, make, model),
                     dsn = ctr_shp_pathfn, delete_dsn = file.exists(ctr_shp_pathfn), quiet = quiet)
            if (!quiet) message(green(paste0(" - ", ctr_fn, ".shp saved")))
            files_saved <- c(files_saved, ctr_shp_pathfn)
          }

        }

      }

      ## Export footprints
      if (fp) {

        if (identical(x[[flt_idx]]$fp, NA)) {
          stop("Footprints not found. Try re-running `uas_info()` with `fp=TRUE`.")
        }

        if (combine_feats) {
          fp_combined_sf <- rbind(fp_combined_sf, x[[flt_idx]]$fp)

        } else {
          fp_fn <- paste0(fnbase, "_fp")

          ## Compute the complete path and see if it already exists
          fp_shp_pathfn <- file.path(path.expand(output_dir_use), paste0(fp_fn, ".shp"))

          ## Export to Shapefile
          if (file.exists(fp_shp_pathfn) && !overwrite) {

            if (!quiet) message(yellow(paste0(" - ", fp_fn, ".shp", " already exists. Skipping.")))

          } else {
            st_write(x[[flt_idx]]$fp, dsn = fp_shp_pathfn,
                     delete_dsn = file.exists(fp_shp_pathfn), quiet = quiet)
            if (!quiet) message(green(" - ", fp_fn, ".shp saved", sep=""))
            files_saved <- c(files_saved, fp_shp_pathfn)
          }

        }

      }

      ## Export MCP
      if (mcp) {

        if (combine_feats) {
          mcp_combined_sf <- rbind(mcp_combined_sf, x[[flt_idx]]$mcp)

        } else {
          mcp_fn <- paste0(fnbase, "_mcp")


          ## Compute the complete path and see if it already exists
          mcp_shp_pathfn <- file.path(path.expand(output_dir_use), paste0(mcp_fn, ".shp"))

          ## Export to Shapefile
          if (file.exists(mcp_shp_pathfn) && !overwrite) {
            if (!quiet) message(yellow(paste0(" - ", mcp_fn, ".shp", " already exists. Skipping.")))
            files_saved <- c(files_saved, mcp_shp_pathfn)

          } else {
            st_write(x[[flt_idx]]$mcp, dsn = mcp_shp_pathfn, delete_dsn = file.exists(mcp_shp_pathfn), quiet = quiet)
            if (!quiet) message(green(paste0(" - ", mcp_fn, ".shp saved")))
            files_saved <- c(files_saved, mcp_shp_pathfn)
          }

        }

      }

    }   # for i in flt_idx_use DONE WITH LOOP

    if (combine_feats) {

      if (ctr) {

        ## Compute the complete path and see if it already exists
        ctr_shp_pathfn <- file.path(path.expand(output_dir_use), paste0(combine_fn, "_ctr.shp"))

        ## Export to Shapefile
        if (file.exists(ctr_shp_pathfn) && !overwrite) {
          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_ctr.shp already exists. Skipping.")))
          files_saved <- c(files_saved, ctr_shp_pathfn)

        } else {
          st_write(ctr_combined_sf, dsn = ctr_shp_pathfn, delete_dsn = file.exists(ctr_shp_pathfn), quiet = quiet)
          if (!quiet) message(green(paste0(" - ", combine_fn, "_ctr.shp saved")))
          files_saved <- c(files_saved, ctr_shp_pathfn)
        }

      }

      if (fp) {
        message(red("Exporting combined footprints is not yet supported."))
      }

      if (mcp) {

        ## Compute the complete path and see if it already exists
        mcp_shp_pathfn <- file.path(path.expand(output_dir_use), paste0(combine_fn, "_mcp.shp"))

        ## Export to Shapefile
        if (file.exists(mcp_shp_pathfn) && !overwrite) {
          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_mcp.shp already exists. Skipping.")))
          files_saved <- c(files_saved, mcp_shp_pathfn)

        } else {
          st_write(mcp_combined_sf, dsn = mcp_shp_pathfn, delete_dsn = file.exists(mcp_shp_pathfn), quiet = quiet)
          if (!quiet) message(green(paste0(" - ", combine_fn, "_mcp.shp saved")))
          files_saved <- c(files_saved, mcp_shp_pathfn)
        }

      }

    }

    if (!quiet) message(green("Done"))
    invisible(files_saved)
}
