#' Flight summaries
#'
#' Creates image collection summaries for individual flights (folders)
#'
#' @param x A list of class 'uas_info'
#' @param flt Flight(s) in x to process (character or numeric vector, default is all)
#' @param group_img Group images within ~1m of each other into 1 point
#' @param thumbnails Display thumbnail images, logical
#' @param show_local_dir Show the local image directory, logical
#' @param units imperial or metric, character
#' @param report_title Title to appear at the top of the summary
#' @param attachments Supplementary files to create and link to the flight summary, see Details.
#' @param output_dir If NULL, then will be placed in a 'map' sub-directory of the images
#' @param create_dir Create the output directory if it doesn't exist
#' @param output_file	Name of the HTML file. If NULL a default based on the name of the input directory is chosen.
#' @param overwrite_html Overwrite existing HTML files without warning, logical
#' @param open_report Open the HTML file in a browser
#' @param self_contained Make the output HTML file self-contained
#' @param tbm_use Whether to create a PNG version of the map. May be logical, or dimensions of the output image in pixels (see Details)
#' @param tbm_overwrite Overwrite existing PNG files without warning, logical
#' @param tbm_size The size of a square flight thumbnail image in pixels, number
#' @param tbm_exp A proportion to expand the bounding box of the PNG map, see Details.
#' @param tbm_src The API service to use to get the flight thumbnail background image, see Details.
#' @param tbm_api_key API key for Google Static Maps or Stadia, see Details.
#' @param pts_col Color value(s) of the centroids and/or footprints
#' @param report_rmd Rmd template used to generate the HTML file. See Details.
#' @param header_html A HTML file name or URL to use as the header
#' @param footer_html A HTML file name or URL to use as the footer
#' @param use_tmpdir Use the temp dir for processing
#' @param quiet TRUE to supress printing of the pandoc command line
#' @param show_gps_coord `r lifecycle::badge("deprecated")` Does nothing
#' @param png_map `r lifecycle::badge("deprecated")` Use tbm_use
#' @param png_exp `r lifecycle::badge("deprecated")` Use tbm_exp
#' @param overwrite_png `r lifecycle::badge("deprecated")` Use tbm_overwrite
#' @param google_api `r lifecycle::badge("deprecated")` Use tbm_api_key
#' @param col `r lifecycle::badge("deprecated")` Use pts_col
#'
#'
#' @details This will generate HTML report(s) of the images in the UAS metadata object based.
#'
#' \code{group_img} determinies whether images at the same location are represented by a single point on the map. This is common with
#' multi-spectral sensors that take generate multiple images per location. 'Same location' is determined by looking at the 5th decimal
#' place of the x and y geographic coordinates (~1m).
#'
#' \code{units} defines whether the units for the flight area, above ground altitude, and GSD are reported in imperial or metric units.
#'
#' If no value for \code{output_dir} is passed, the report will be saved in a sub-directory of the image directory
#' called 'map'. This sub-directory will be created if \code{create_dir = TRUE}.
#'
#' \code{self_contained} determines whether the HTML file(s) created will have all the JavaScript and CSS files
#' embedded in the HTML file itself, or saved in a subdirectory called 'libs'. If saving several reports to one output directory,
#' If saving multiple HTML reports to the same output directory, passing \code{self_contained = FALSE} is more efficient
#'
#' The HTML report is generated from a RMarkdown file. If you know how to edit RMarkdown, you can modify the default template and pass the filename
#' of your preferred template using the \code{report_rmd} argument.
#'
#' \code{tbm_use} determines whether a thumbnail image of the flight will be downloaded from Google or StadiaMaps, and saved in \code{output_dir}.
#' Although this thumbnail image of the flight is not be displayed in the flight summary report, it is used when generated a Table-of-Contents for
#' a series of flights, and can be useful as a standalone quick glance of the flight in Windows Explorer, GitHub repos, etc.
#'
#' Note that both Google Maps and StadiaMaps require an API key (that you pass using \code{tbm_api_key}). If you don't have an API
#' key for one of these services, then you can't download a thumbnail image of the flight. To get an API key for the Google Static Maps service, see
#' \url{https://developers.google.com/maps/documentation/maps-static/} (there are also a number of tutorials available). Note that Google does require
#' a credit card or their APIs, but the monthly quota before you get charged should be more than enough if all you're doing is downloading thumbnail images
#' for your drone flights. To get an API key for StadiaMaps (no credit card required), start here. See the \href{https://github.com/dkahle/ggmap/}{ggmap package} for details.
#'
#' The flight thumbnail image will be created at the dimensions given by \code{tbm_size}. \code{tbm_exp} is a percentage of
#' the flight bounding box that will be used as a buffer around the background map. If the map seems too cropped, or you get a warning message about rows
#' removed, try increasing it. \code{pts_col} can be used to pass a single color or vector of custom colors for the image locations (the default is a rainbow color ramp).
#'
#' \code{attachment} specifies which supplementary files to create and link to the flight summary. Choices are
#' \code{ctr_kml} and \code{mcp_kml} for KML versions of the camera locations and MCP (minimum convex
#' polygon around all images). These KML files will be created in the same output directory as the flight
#' summary.
#'
#' @return The HTML file name(s) of the flight summaries generated
#'
#' @seealso \code{\link{uas_info}}, \code{\link{uas_exp_kml}},
#'
#' @importFrom crayon green yellow bold silver
#' @importFrom grDevices dev.off png rainbow
#' @importFrom utils browseURL packageVersion download.file
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom rmarkdown render
#' @importFrom lifecycle deprecated is_present deprecate_warn
#' @importFrom sf st_transform st_bbox st_drop_geometry
#' @importFrom methods is
#'
#' @export

uas_report <- function(x, flt = NULL, group_img = FALSE, thumbnails = FALSE,
                       show_local_dir = TRUE, units = c("imperial", "metric")[1], report_title = "Flight Summary",
                       attachments = c("mcp_kml", "ctr_kml")[0],
                       pts_col = NULL,
                       output_dir = NULL, create_dir = TRUE, output_file = NULL, overwrite_html = FALSE,
                       open_report = FALSE, self_contained = TRUE,
                       tbm_use = FALSE, tbm_overwrite = FALSE, tbm_size = 480, tbm_src = c("Google", "Stadia")[1], tbm_api_key = NULL, tbm_exp = 0.2,
                       report_rmd = NULL,
                       header_html = NULL, footer_html = NULL, use_tmpdir = FALSE, quiet = FALSE,
                       show_gps_coord = lifecycle::deprecated(),
                       png_map = lifecycle::deprecated(),
                       overwrite_png = lifecycle::deprecated(),
                       png_exp = lifecycle::deprecated(),
                       google_api = lifecycle::deprecated(),
                       col = lifecycle::deprecated()) {

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  if (lifecycle::is_present(show_gps_coord)) {
    lifecycle::deprecate_warn("1.6.8", "uas_report(show_gps_coord)", details = "No longer needed")
  }

  if (lifecycle::is_present(png_map)) {
    lifecycle::deprecate_warn("1.9.0", "uas_report(png_map)", "uas_report(tbm_use)")
  }

  if (lifecycle::is_present(png_exp)) {
    lifecycle::deprecate_warn("1.9.0", "uas_report(png_exp)", "uas_report(tbm_exp)")
  }

  if (lifecycle::is_present(google_api)) {
    lifecycle::deprecate_warn("1.9.0", "uas_report(google_api)", "uas_report(tbm_api_key)")
  }

  if (lifecycle::is_present(col)) {
    lifecycle::deprecate_warn("1.9.0", "uas_report(col)", "uas_report(pts_col)")
  }

  if (lifecycle::is_present(overwrite_png)) {
    lifecycle::deprecate_warn("1.9.0", "uas_report(overwrite_png)", "uas_report(tbm_overwrite)")
  }

  if (!units %in% c("imperial", "metric")) stop("Unknown value for `units`")
  if (!tbm_src %in% c("Google", "Stadia")) stop("Unknown value for `tbm_src`")

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

  ## Get the Rmd template
  if (is.null(report_rmd)) {
    report_rmd <- system.file("report/uas_report.Rmd", package="uasimg")
  }
  if (!file.exists(report_rmd)) stop("Can't find the report template")

  ## Validate value(s) of attachments argument
  if (is.null(attachments)) attachments <- character(0)
  if (length(attachments) > 0) {
    if (FALSE %in% (attachments %in% c("ctr_kml", "mcp_kml"))) stop("Unknown value(s) for `attachments`")
  }

  if (!missing("show_gps_coord")) {
    if (!quiet) message(silver(" - `show_gps_coord` has been deprecated and does nothing. Please remove it from your code."))
  }

  ## Define a variable for the temp directory if needed

  if (use_tmpdir) temp_dir <- tempdir()

  ## Set the size of the PNG map
  ## THIS IS NOW DONE BY TBM_SIZE
  # make_png <- FALSE
  # if (identical(png_map, TRUE)) {
  #   png_dim <- c(480,480)
  #   make_png <- TRUE
  # } else if (is.numeric(png_map)) {
  #   if (length(png_map)==1) png_map <- rep(png_map,2)
  #   if (length(png_map)!=2) stop("Invalid value for png_map")
  #   png_dim <- png_map
  #   make_png <- TRUE
  # }

  if (tbm_use) {
    if (!requireNamespace("ggmap", quietly = TRUE)) stop("Package ggmap required to make the thumbnail map")
    #if (!require("ggmap", quietly = TRUE)) stop("Package ggmap required to make the png map")   ### DONT WANT TO LOAD IT
    if (packageVersion("ggmap") < '3.0.0') stop("Please update ggmap package")
    png_dim <- rep(tbm_size, 2)
  }

  report_fn_vec <- NULL
  first_pass_yn <- TRUE

  ## Start the loop
  for (flt_idx in flt_idx_use) {

    if (identical(x[[flt_idx]]$pts, NA)) {
      warning(paste0("Centroids not found for ", flt_name, ". Skipping report."))
      next
    }

    ## Get the actual image directory(s)
    img_dir <- unique(dirname(x[[flt_idx]]$pts$img_fn))

    ## Verify that img_dir exists
    if (FALSE %in% file.exists(img_dir)) {
      stop(paste0("Can not find image directory: ", img_dir))
    }

    ## Get the output dir for the report
    if (is.null(output_dir)) {

      ## No output directory is specify
      if (length(img_dir) > 1) stop("When images for one flight live in multiple directories, you must specify output_dir")

      ## One input directory --> use 'map' folder in the image folder

      ## Test for write permission.
      if (file.access(img_dir, mode = 2) != 0) {
        stop(paste0("Sorry, you don't have write permissions for ", img_dir, ". Use `output_dir` to specify a directory can write to."))
      }

      output_dir_use <- file.path(img_dir, "map")
      if (!file.exists(output_dir_use) && create_dir) {
        if (!quiet) message(green("Creating", output_dir_use))
        if (!dir.create(output_dir_use, recursive = TRUE)) stop(paste0("Unable to create ", output_dir_use, ". Use `output_dir` to specify a directory can write to."))
      }

    } else {
      ## AN output directory was specified
      if (!file.exists(output_dir)) stop(paste0("Could not find output directory: ", output_dir))

      ## Test for write permission.
      if (file.access(output_dir, mode = 2) != 0) {
        stop(paste0("Sorry, you don't have write permissions for ", output_dir))
      }

      output_dir_use <- output_dir
    }

    ## Construct a base filename (to use for the HTML report, PNG, and KML files)
    if (is.na(x[[flt_idx]]$metadata$name_short %>% null2na())) {
      fnbase <- x[[flt_idx]]$id
    } else {
      fnbase <- x[[flt_idx]]$metadata$name_short
    }

    ## Construct a vector of colors for the points. These will be used both for
    ## the leaflet map as well as the thumbnail image

    if (is.null(pts_col)) {
      col_use <- rainbow(nrow(x[[flt_idx]]$pts), end=5/6)
    } else {
      col_use <- pts_col
    }

    ##################################################################
    ## Get the thumbnail map image
    if (tbm_use) {

      ## Construct the map.png filename
      if (is.null(output_file)) {
        map_fn <- paste0(fnbase, "_map.png")

      } else {
        map_fn <- paste0(file_path_sans_ext(basename(output_file)), ".png")
      }

      if (tbm_overwrite || !file.exists(file.path(output_dir_use, map_fn))) {

        ## Lets make a png map

        # Define the extent and center point of the flight area
        pts_ext <- x[[flt_idx]]$pts %>% st_transform(4326) %>% st_bbox() %>% as.numeric()
        lon_idx <- c(1, 3)
        lat_idx <- c(2, 4)
        ctr_ll <- c(mean(pts_ext[lon_idx]), mean(pts_ext[lat_idx]))

        ## Compute the Zoom level (use a minimum of 18)
        zoom_lev <- min(18, ggmap::calc_zoom(lon = range( pts_ext[lon_idx]),
                                             lat = range( pts_ext[lat_idx]),
                                             adjust=as.integer(-1)))

        if (tbm_src == "Google") {
          if (is.null(tbm_api_key)) stop("To download a thumbnail map of the flight area from Google, you must pass an API key for Google Static Maps ")
          ggmap::register_google(key = tbm_api_key)
          if (!quiet) message(yellow(" - downloading a thumbnail image of the flight area from Google"))
          m <- try(ggmap::get_googlemap(center=ctr_ll, zoom=zoom_lev, format="png8", maptype="satellite"))
          if (is(m, "try-error")) {
            warning("Failed to retrieve static map from Google Maps, check your API key.")
            m <- NULL
          }
        } else if (tbm_src == "Stadia") {
          ## Grab a Stadia map
          if (is.null(tbm_api_key)) stop("To download a thumbnail map of the flight area from Stadia, you must pass an API key for StadiaMaps.")
          ggmap::register_stadiamaps(key = tbm_api_key)
          if (!quiet) message(yellow(" - downloading a thumbnail image of the flight area from Stadia"))
          dx <- diff(pts_ext[lon_idx]) * tbm_exp
          dy <- diff(pts_ext[lat_idx]) * tbm_exp
          pts_ext <- pts_ext + c(-dx, -dy, dx, dy)
          m <- ggmap::get_stadiamap(bbox = pts_ext, zoom = zoom_lev, maptype="stamen_terrain")
        }

        if (!is.null(m)) {

          # Create the ggmap object and save to a variable
          pts_ggmap <- ggmap::ggmap(m) +
            geom_point(data = x[[flt_idx]]$pts %>% st_drop_geometry(),
                       aes(gps_long, gps_lat),
                       color = col_use,
                       show.legend = FALSE,
                       size = 3) +
            theme_void()

          ## Open the PNG driver
          png(filename = file.path(output_dir_use, map_fn), width = png_dim[1], height = png_dim[2])

          ## Print the map to the PNG drive
          print(pts_ggmap)

          ## Close the PNG driver
          dev.off()

        }

      } else {
        if (!quiet) message(yellow(map_fn, "already exists. Skipping."))
      }


    } else {
      map_fn <- ""
    } ## if tbm_use
    ###########################################


    ##################################################################
    ## Create image thumbnails if needed
    if (thumbnails) {

      ## Get the output folder
      tb_dir_use <- file.path(output_dir_use, "tb")
      if (!file.exists(tb_dir_use)) {
        if (!(dir.create(tb_dir_use))) {
          thumbails <- FALSE
          warning("Could not create thumbnail direcotry")
        }
      }

      if (thumbnails) {

        ## Call uas_thumbnails()
        tb_fn_lst <- uas_thumbnails_make(x, flt = flt_idx, output_dir = tb_dir_use,
                                         tb_width = 400, overwrite = FALSE)

        ## Save the base name (minus the path) of the thumbnail in the attribute table for the points, so it can be
        ## used in the leaflet map. This is only for passing the filenames to the Rmd file, not permanently saved
        x[[flt_idx]]$pts$tb_fn <- basename(tb_fn_lst[[names(x)[flt_idx]]])

      }

    }  ## if thumbnails = TRUE

    ## If thumbnails is (still) FALSE, fill the column with NA
    if (!thumbnails) x[[flt_idx]]$pts$tb_fn <- NA

    ## Get the HTML report output filename
    if (is.null(output_file)) {
      output_file_use <- paste0(fnbase, "_rpt.html")
    } else {
      output_file_use <- output_file
    }

    if (overwrite_html || !file.exists(file.path(output_dir_use, output_file_use))) {
      ## WE NEED TO GENERATE A HTML FILE

      ## Determine where we will render the HTML file
      if (use_tmpdir) {
        render_dir <- temp_dir
      } else {
        render_dir <- output_dir_use
      }

      ## In order to create HTML output which is *not* self-contained, we must
      ## manually copy the css file to the output dir. We must also
      ## temporarily copy the RMd file to the output dir, because knitr
      ## creates the lib_dir relative to the location of the Rmd file (with no
      ## other arguments or options)

      if (self_contained) {
        output_options <- list()
        report_rmd_use <- report_rmd

      } else {
        ## Copy the Rmd file to the output_dir (temporarily)
        ## If output_dir is specfied, only need to do this on the first pass
        if (is.null(output_dir) || first_pass_yn) {

          file.copy(from=report_rmd, to=render_dir, overwrite = TRUE)
          report_rmd_use <- file.path(render_dir, basename(report_rmd))

          ## Copy the CSS file to the output_dir (permanently)
          report_css <- system.file("report/uas_report.css", package="uasimg")
          file.copy(from=report_css, to=output_dir_use, overwrite = TRUE)

          ## Create a list of output options that specify not self contained
          output_options <- list(self_contained=FALSE, lib_dir="libs")
        }
      }

      ## Create a list of includes if header or footer are passed
      if (!is.null(footer_html) || !is.null(header_html)) {
        includes_lst <- list()

        if (!is.null(header_html)) {

          if (grepl("^http", header_html, ignore.case = TRUE)) {
            header_html_fn <- tempfile("uas_header_", fileext = ".html")
            download_successful <- download.file(url = header_html, destfile = header_html_fn)
            if (download_successful != 0) stop("Could not download the header URL")
            on.exit(unlink(header_html_fn))
          } else {
            header_html_fn <- header_html
          }

          if (file.exists(header_html_fn)) {
            if (use_tmpdir) {
              file.copy(from = header_html_fn, to = temp_dir, overwrite = ifelse(first_pass_yn, TRUE, FALSE))
              includes_lst[["before_body"]] <- file.path(temp_dir, basename(header_html_fn))
            } else {
              includes_lst[["before_body"]] <- header_html_fn
            }
          } else {
            stop(paste0("File not found: ", header_html_fn))
          }
        }

        if (!is.null(footer_html)) {

          if (grepl("^http", footer_html, ignore.case = TRUE)) {
            footer_html_fn <- tempfile("uas_footer_", fileext = ".html")
            download_successful <- download.file(url = footer_html, destfile = footer_html_fn)
            if (download_successful != 0) stop("Could not download the footer URL")
            on.exit(unlink(footer_html_fn))
          } else {
            footer_html_fn <- footer_html
          }

          if (file.exists(footer_html_fn)) {
            if (use_tmpdir) {
              file.copy(from = footer_html_fn, to = temp_dir, overwrite = ifelse(first_pass_yn, TRUE, FALSE))
              includes_lst[["after_body"]] <- file.path(temp_dir, basename(footer_html_fn))
            } else {
              includes_lst[["after_body"]] <- footer_html_fn
            }
          } else {
            stop(paste0("File not found: ", footer_html_fn))
          }
        }

        #output_options <- c(output_options, includes = includes_lst)
        output_options$includes = includes_lst

      }

      # ## Compute colors for the pts and fp
      ## THIS WAS ALREADY DONE HIGHER UP IN THE LOOP
      # if (is.null(col)) {
      #   # col_use <- rainbow(nrow(x[[img_dir]]$pts), end=5/6)
      #   ## We pass col = NA to render() because colors for the footprints and center
      #   ## could be different (e.g., there may be fewer footprints than centers)
      #   col_use <- NA
      # } else {
      #   col_use <- col
      # }

      ## Generate a KML file with the MCP
      if ("mcp_kml" %in% attachments) {
        kml_mcp_fn <- paste0(fnbase, "_mcp.kml")
        if (!file.exists(file.path(output_dir_use, kml_mcp_fn))) {
          uas_exp_kml(x, flt = i, mcp = TRUE, output_dir = output_dir_use, out_fnbase = fnbase)
        }
      } else {
        kml_mcp_fn <- NA
      }

      ## Generate a KML file with the centers
      if ("ctr_kml" %in% attachments) {
        kml_ctr_fn <- paste0(fnbase, "_ctr.kml")
        if (!file.exists(file.path(output_dir_use, kml_ctr_fn))) {
          uas_exp_kml(x, flt = i, ctr = TRUE, output_dir = output_dir_use, out_fnbase = fnbase)
        }
      } else {
        kml_ctr_fn <- NA
      }

      ## Render the HTML file
      report_fn <- render(input = report_rmd_use,
                          output_dir = render_dir, output_file = output_file_use,
                          output_options = output_options,
                          params = c(x[[flt_idx]], list(col = col_use,
                                                  img_dir = img_dir,
                                                  group_img = group_img,
                                                  show_local_dir = show_local_dir,
                                                  units = units,
                                                  map_fn = map_fn,
                                                  thumbnails = thumbnails,
                                                  kml_mcp_fn = kml_mcp_fn,
                                                  kml_ctr_fn = kml_ctr_fn,
                                                  report_title = report_title
                                                  )
                                              )
                                     )

      if (use_tmpdir) {

        ## Copy the HTML file from temp space to output_dir, then delete
        if (!quiet) message(yellow(" - Copying", output_file_use, "to output directory"))
        file.copy(from = report_fn, to = output_dir_use, overwrite = TRUE)
        unlink(report_fn)

        if (!self_contained) {
          ## Copy the libs directory to output_dir_use
          libs_dir <- file.path(render_dir, "libs")
          if (file.exists(libs_dir)) {
            file.copy(from = libs_dir, to = output_dir_use, overwrite = FALSE, recursive = TRUE)
            unlink(libs_dir, recursive=TRUE)
          }

          ## Copy the *_files  directory (if it exists)
          files_dir_pathfn <- file.path(render_dir,
                                        paste0(file_path_sans_ext(output_file_use), "_files"))
          if (file.exists(files_dir_pathfn)) {
            file.copy(from = files_dir_pathfn, to = output_dir_use, overwrite = FALSE, recursive = TRUE)
            unlink(files_dir_pathfn, recursive=TRUE)
          }

          ## The css file, kml attachments, and png file should already be in output_dir_use

        }

        report_fn_vec <- c(report_fn_vec,
                           normalizePath(file.path(output_dir_use, basename(report_fn))))

      } else {
        ## Add the filename to report_fn_vec which will eventually be returned
        report_fn_vec <- c(report_fn_vec, normalizePath(report_fn))
      }

      ## If not self-contained, delete the temporary copy of the Rmd file
      if (!self_contained) {
        if (is.null(output_dir) || i == length(x)) {
          file.remove(report_rmd_use)
        }

      }

    } else {
      message(yellow(output_file_use, "already exists. Skipping."))

      ## If the HTML file already exists (but wasn't overwritten), return it just the same
      if (file.exists(file.path(output_dir_use, output_file_use))) {
        report_fn_vec <- c(report_fn_vec, normalizePath(file.path(output_dir_use, output_file_use)))
      }


    }

    first_pass_yn <- FALSE

  }

  message(green("Done."))

  ## Open the file(s)
  if (open_report) {
    for (report_fn in report_fn_vec) {
      browseURL(report_fn)
    }
  }

  ## Return the filename(s) of the HTML file(s) (invisibly)
  invisible(report_fn_vec)
}

