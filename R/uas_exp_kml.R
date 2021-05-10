#' Export flight area and camera locations for GIS
#'
#' Export geometry(s) from a flight to KML and Shapefile
#'
#' @param x A list of class 'uas_info'
#' @param flt_idx Flight indices in x to process, integer
#' @param ctr Export the image centroids, Logical
#' @param fp Export the image footprints, Logical
#' @param mcp Export the minimum convex polygon of the image footprints, logical
#' @param combine_feats Combine features into a single Shapefile / KML, logical
#' @param combine_fn File name (minus path and extension) for the combined features layer
#' @param output_dir The output directory. If NULL, the layers will be saved in a 'map' sub-directory of the image folder
#' @param out_fnbase The base of output filenames
#' @param create_dir Create the output directory if it doesn't exist, logical
#' @param overwrite Overwrite existing files, Logical
#' @param quiet Suppress messages, Logical
#'
#' @details
#'
#' \code{flt_idx} allows you to specify a subset of image folders in \code{x} to process (use names(x) to see what those are).
#'
#' \code{ctr}, \code{fp}, and \code{mcp} (all TRUE/FALSE) specify which geometry(s) to export.

#' Output file names will be generated from the flight metadata saved in the uas_info object. Alternately,
#' you can pass the base of a file name using \code{out_fnbase}. You can specify the output directory
#' with \code{output_dir}. The default is to save them in a sub-directory of the images directory
#' called 'map', which will be created if needed.
#'
#' If \code{combine_feats = TRUE}, the geometries from the folders in x will be combined into a single KML or Shapefile.
#' For this to happen, you must also pass a value for \code{combine_fn} (a base for the file name of the combined features).
#'
#' @return A vector of filenames
#'
#' @seealso \code{\link{uas_info}}, \code{\link{uas_report}}
#'
#' @importFrom crayon green yellow red
#' @importFrom sf st_write st_coordinates st_transform st_bbox
#' @importFrom xml2 xml_new_root xml_add_child xml_add_sibling xml_parent xml_root read_xml write_xml xml_replace
#' @importFrom dplyr mutate pull slice
#' @importFrom tibble as_tibble
#' @export

uas_exp_kml <- function(x, flt_idx = NULL, ctr = FALSE, fp = FALSE, mcp = FALSE,
                        combine_feats = FALSE, combine_fn = NULL, output_dir = NULL, out_fnbase = NULL,
                        create_dir = TRUE, overwrite = FALSE, quiet = FALSE) {

    if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

    if (!ctr && !fp && !mcp) {
      stop("Nothing to do! At least one of `ctr`, `fp`, or `mcp` must be TRUE.")
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

    ## Verify that that value(s) in flt_idx (if any) are valid
    if (is.null(flt_idx)) {
      flt_idx_use <- 1:length(x)
    } else {
      if (TRUE %in% (flt_idx > length(x))) stop("Invalid value for flt_idx")
      flt_idx_use <- flt_idx
    }

    fnbase_all <- NULL   ## save these in the loop, used to name features when combine = TRUE
    files_saved <- NULL  ## gets returned at the end

    for (i in flt_idx_use) {

      ## Get the actual image directory(s)
      img_dir <- unique(dirname(x[[i]]$pts$img_fn))

      ## Get the output dir
      if (is.null(output_dir)) {

        if (length(img_dir) > 1) stop("When images for one flight live in multiple directories, you must specify output_dir")

        output_dir_use <- file.path(img_dir, "map")
        if (!file.exists(output_dir_use) && create_dir) {
          if (!quiet) message(yellow(" - Creating ", output_dir_use))
          if (!dir.create(output_dir_use, recursive = TRUE)) stop(paste0("Unable to create ", output_dir_use))
        }
      } else {
        output_dir_use <- output_dir
      }
      if (!file.exists(output_dir_use)) stop(paste0("Can't find ", output_dir_use))
      if (!quiet && !combine_feats) message(yellow(" - saving files to", path.expand(output_dir_use)))

      ## Define the base file name
      if (is.null(out_fnbase)) {

        if (!is.na(x[[i]]$metadata$name_short %>% null2na())) {
          fnbase <- x[[i]]$metadata$name_short
        } else {
          fnbase <- x[[i]]$id
        }
      } else {
        fnbase <- out_fnbase
      }
      fnbase_all <- c(fnbase_all, fnbase)


      ## Export centroids
      if (ctr) {

        ## Parse out the image date and time for the balloon text
        dt_date <- x[[i]]$pts %>%
          pull(date_time) %>%
          gsub(" .*$", "", .) %>%
          gsub(":", "-", .)

        dt_time <- x[[i]]$pts %>%
          pull(date_time) %>%
          gsub("^.* ", "", .)

        if (combine_feats) {

          ## Create a slab of XML text for the placemarks.
          ## First, paste the coordinates together
          coords_str <- x[[i]]$pts %>%
            st_transform(4326) %>%
            st_coordinates() %>%
            as_tibble() %>%
            mutate(coords_str = paste(X, Y,"0", sep = ",")) %>%
            pull(coords_str)

          ## Wrap the coordinates in tags and collapse
          placemarks_chr <- paste("<Placemark><name>",
                                  x[[i]]$pts %>% pull(file_name),
                                  "</name><description>",
                                  "<b>Date</b>: ", dt_date, "<br/>",
                                  "<b>Time</b>: ", dt_time,
                                  "</description><styleUrl>#",
                                  sprintf("%02d", (length(ctr_combined_lst) %% length(icon_png)) + 1),
                                  "</styleUrl><Point><coordinates>",
                                  coords_str, "</coordinates></Point></Placemark>",
                                  sep = "", collapse = "\n")

          ## Parse placemarks_chr into an xml node (fast)
          ctr_combined_lst[[fnbase]] <- read_xml(paste0("<Folder><name>",  fnbase, "</name>",
                                            placemarks_chr, "</Folder>"))


        } else {
          ## Not combined features
          ctr_fn <- paste0(fnbase, "_ctr")

          ## Compute the complete path and see if it already exists
          ctr_kml_pathfn <- file.path(path.expand(output_dir_use), paste0(ctr_fn, ".kml"))

          ## Export to KML
          if (file.exists(ctr_kml_pathfn) && !overwrite) {
            if (!quiet) message(yellow(paste0(" - ", ctr_fn, ".kml", " already exists. Skipping.")))
            files_saved <- c(files_saved, ctr_kml_pathfn)

          } else {

            ## Create a new XML document with a 'document' node
            nd_document <- xml_new_root("kml",
                                        xmlns = "http://www.opengis.net/kml/2.2",
                                        "xmlns:gx" = "http://www.google.com/kml/ext/2.2",
                                        "xmlns:kml" = "http://www.opengis.net/kml/2.2",
                                        "xmlns:atom" = "http://www.w3.org/2005/Atom") %>%
              xml_add_child("Document") %>%
              xml_add_child("name", ctr_fn) %>%
              xml_parent()

            #############################################################
            ## ADD STYLES

            balloon_style_text <- "<b>$[name]</b><br/><br/>$[description]"
            icon_url <- "http://maps.google.com/mapfiles/kml/paddle/red-circle-lv.png"

            ## Basic Style - no style map
            nd_style_rc <- nd_document %>%
              xml_add_child("Style", id = "rc")

            ## This BalloonStyle replaces the default balloon text
            nd_style_rc %>%
              xml_add_child("BalloonStyle") %>%
                xml_add_child("text", balloon_style_text)

            ## This LabelStyle hides the placemark name on the map
            nd_style_rc %>%
              xml_add_child("LabelStyle") %>%
              xml_add_child("scale", "0.0")

            nd_style_rc %>%
              xml_add_child("IconStyle") %>%
              xml_add_child("scale", "0.5") %>%
              xml_add_sibling("Icon") %>%
              xml_add_child("href", icon_url)

            #############################################################
            ## Add a folder node (really just a placeholder, will get swapped out below)
            nd_folder <- nd_document %>%
              xml_add_child("Folder")

            ## Create a slab of XML text for the placemarks.

            ## First, paste the coordinates together
            coords_str <- x[[i]]$pts %>%
              st_transform(4326) %>%
              st_coordinates() %>%
              as_tibble() %>%
              mutate(coords_str = paste(X, Y,"0", sep = ",")) %>%
              pull(coords_str)

            # ## Parse out the image date and time for the placeholder description
            # dt_date <- x[[i]]$pts %>%
            #   pull(date_time) %>%
            #   gsub(" .*$", "", .) %>%
            #   gsub(":", "-", .)
            #
            # dt_time <- x[[i]]$pts %>%
            #   pull(date_time) %>%
            #   gsub("^.* ", "", .)

            ## Wrap the coordinates and description in tags and collapse
            placemarks_chr <- paste("<Placemark><name>",
                                    x[[i]]$pts %>% pull(file_name),
                                    "</name>",
                                    "<description>",
                                    "<b>Date</b>: ", dt_date, "<br/>",
                                    "<b>Time</b>: ", dt_time,
                                    "</description>",
                                    "<styleUrl>#rc</styleUrl><Point><coordinates>",
                                    coords_str, "</coordinates></Point></Placemark>",
                                    sep = "", collapse = "\n")

            ## Define the look angle
            flight_ctr_xy <- x[[i]]$pts %>%
              st_transform(4326) %>%
              st_bbox() %>%
              matrix(ncol = 2) %>%
              apply(1, mean)

            look_at_str <- paste0("<LookAt>",
                                  "<longitude>", flight_ctr_xy[1], "</longitude>",
                                  "<latitude>", flight_ctr_xy[2], "</latitude>",
                                  "<altitude>0</altitude>",
                                  "<tilt>0</tilt>",
                                  "<range>2000</range>",
                                  "</LookAt>")

            ## Parse placemarks_chr into an xml node (fast)
            # placemarks_chr <- gsub("<", , x)
            nd_fldr_wth_pm <- read_xml(paste0("<Folder><name>",  ctr_fn, "</name>",
                                              look_at_str,
                                              placemarks_chr, "</Folder>"))

            # xx <- paste0("<Folder><name>",  ctr_fn, "</name>",
            #              look_at_str,
            #              placemarks_chr, "</Folder>")
            #
            # yy <- read_xml.character(xx)

            # placemarks_chr <- substr(placemarks_chr, 1, nchar(placemarks_chr) - 1)
            # writeClipboard(placemarks_chr)
            # nd_fldr_wth_pm <- read_xml(placemarks_chr, options = c("PEDANTIC"))



            ## Swap out the placeholder folder nd_folder with nd_fldr_wth_pm
            xml_replace(nd_folder, nd_fldr_wth_pm)

            write_xml(nd_document %>% xml_root(), file = ctr_kml_pathfn)

            if (!quiet) message(yellow(paste0(" - ", ctr_fn, ".kml", " saved")))
            files_saved <- c(files_saved, ctr_kml_pathfn)

          }
        }

      }

      ## Export footprints
      if (fp) {
        if (combine_feats) {
          fp_combined_sf <- rbind(fp_combined_sf, x[[i]]$fp)
        } else {
          fp_fn <- paste0(fnbase, "_fp")
          warning("Sorry, exporting footprints to KML is not yet supported")
        }
      }

      ## Export MCP
      if (mcp) {

        if (combine_feats) {
          mcp_combined_sf <- rbind(mcp_combined_sf, x[[i]]$mcp)

        } else {
          mcp_fn <- paste0(fnbase, "_mcp")

          ## Compute the complete path and see if it already exists
          mcp_kml_pathfn <- file.path(path.expand(output_dir_use), paste0(mcp_fn, ".kml"))

          ## Export to KML
          if (file.exists(mcp_kml_pathfn) && !overwrite) {
            if (!quiet) message(yellow(paste0(" - ", mcp_fn, ".kml", " already exists. Skipping.")))
            files_saved <- c(files_saved, mcp_kml_pathfn)

          } else {

            ## Create a new XML document with a 'document' node
            nd_document <- xml_new_root("kml",
                                        xmlns = "http://www.opengis.net/kml/2.2",
                                        "xmlns:gx" = "http://www.google.com/kml/ext/2.2",
                                        "xmlns:kml" = "http://www.opengis.net/kml/2.2",
                                        "xmlns:atom" = "http://www.w3.org/2005/Atom") %>%
              xml_add_child("Document") %>%
              xml_add_child("name", mcp_fn) %>%
              xml_parent()

            ## Add a style to the document node
            nd_style_hr <- nd_document %>% xml_add_child("Style", id = "hollow-cyan")

            ## Add PolyStyle
            nd_style_hr %>%
              xml_add_child("PolyStyle") %>%
              xml_add_child("fill", "0")

            ## Add Line Style
            nd_style_hr %>%
              xml_add_child("LineStyle") %>%
              xml_add_child("color", "ccffff55") %>%
              xml_add_sibling("width", "4")

            ## Create a new placemark node
            nd_placemark <- nd_document %>% xml_add_child("Placemark")

            ## Add a few base properties
            nd_placemark %>%
              xml_add_child("styleUrl", "#hollow-cyan") %>%
              xml_add_sibling("name", mcp_fn)

            ## Don't clamp to ground - LINE DISAPPEARS UNDER TREES
            ## xml_add_sibling("altitudeMode", "clampToGround") %>%

            ## Create the string of coordinates
            coords_chr <- x[[i]]$mcp %>%
              slice(1) %>%
              st_transform(4326) %>%
              st_coordinates() %>%
              as_tibble() %>%
              mutate(coords = paste(X, Y, "0", sep = ",")) %>%
              pull(coords) %>%
              paste(collapse = " ")

            ## Create the placemark child nodes
            nd_placemark %>%
              xml_add_child("Polygon") %>%
              xml_add_child("outerBoundaryIs") %>%
              xml_add_child("LinearRing") %>%
              xml_add_child("coordinates", coords_chr)

            write_xml(nd_document %>% xml_root(), file = mcp_kml_pathfn)

            if (!quiet) message(yellow(paste0(" - ", mcp_fn, ".kml", " saved")))
            files_saved <- c(files_saved, mcp_kml_pathfn)
          }

        }
      }

    }   # for i in idx_use. DONE WITH LOOP

    if (combine_feats) {

      if (ctr) {

        ## Compute the complete path and see if it already exists
        ctr_kml_pathfn <- file.path(path.expand(output_dir_use), paste0(combine_fn, "_ctr.kml"))

        ## Export to KML
        if (file.exists(ctr_kml_pathfn) && !overwrite) {
          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_ctr.kml already exists. Skipping.")))
          files_saved <- c(files_saved, ctr_kml_pathfn)

        } else {

          ## Create a new XML document with a 'document' node
          nd_document <- xml_new_root("kml",
                                      xmlns = "http://www.opengis.net/kml/2.2",
                                      "xmlns:gx" = "http://www.google.com/kml/ext/2.2",
                                      "xmlns:kml" = "http://www.opengis.net/kml/2.2",
                                      "xmlns:atom" = "http://www.w3.org/2005/Atom") %>%
            xml_add_child("Document") %>%
            xml_add_child("name", paste0(combine_fn, "_ctr")) %>%
            xml_parent()

          ## Add icon style(s) to the document node (up to length(icon_png))
          for (k in 1:min(length(icon_png), length(ctr_combined_lst))) {
            ## Add a style node
            nd_ctr_style <- nd_document %>%
              xml_add_child("Style", id = sprintf("%02d", k))

            ## Add the icon style node
            nd_ctr_style %>%
              xml_add_child("IconStyle") %>%
              xml_add_child("scale", "0.5") %>%
              xml_add_sibling("Icon") %>%
              xml_add_child("href", paste0(icon_base, icon_png[k]))

            ## Add a BalloonStyle node (replaces the default balloon text)
            balloon_style_text <- "<b>$[name]</b><br/><br/>$[description]"
            nd_ctr_style %>%
              xml_add_child("BalloonStyle") %>%
              xml_add_child("text", balloon_style_text)

            ## Add a LabelStyle (hides the placemark name on the map)
            nd_ctr_style %>%
              xml_add_child("LabelStyle") %>%
              xml_add_child("scale", "0.0")
          }

          ## Add each of the saved folder nodes for each of the flights
          for (fnbase in names(ctr_combined_lst)) {
            nd_document %>% xml_add_child(ctr_combined_lst[[fnbase]])
          }

          ## Save to disk
          write_xml(nd_document %>% xml_root(), file = ctr_kml_pathfn)

          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_ctr.kml saved")))
          files_saved <- c(files_saved, ctr_kml_pathfn)

        }


      }

      if (fp) {
        message(red("Exporting combined footprints is not yet supported."))
      }

      if (mcp) {

        ## Compute the complete path and see if it already exists
        mcp_kml_pathfn <- file.path(path.expand(output_dir_use), paste0(combine_fn, "_mcp.kml"))

        ## Export to KML
        if (file.exists(mcp_kml_pathfn) && !overwrite) {
          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_mcp.kml already exists. Skipping.")))
          files_saved <- c(files_saved, mcp_kml_pathfn)

        } else {

          ## Create a new XML document with a 'document' node
          nd_document <- xml_new_root("kml",
                                      xmlns = "http://www.opengis.net/kml/2.2",
                                      "xmlns:gx" = "http://www.google.com/kml/ext/2.2",
                                      "xmlns:kml" = "http://www.opengis.net/kml/2.2",
                                      "xmlns:atom" = "http://www.w3.org/2005/Atom") %>%
            xml_add_child("Document") %>%
            xml_add_child("name", paste0(combine_fn, "_mcp")) %>%
            xml_parent()

          ## Add a style to the document node
          nd_style_hc <- nd_document %>% xml_add_child("Style", id = "hollow-cyan")

          ## Add PolyStyle
          nd_style_hc %>%
            xml_add_child("PolyStyle") %>%
            xml_add_child("fill", "0")

          ## Add Line Style
          nd_style_hc %>%
            xml_add_child("LineStyle") %>%
            xml_add_child("color", "ffffffaa") %>%
            xml_add_sibling("width", "2")

          for (j in 1:nrow(mcp_combined_sf)) {

            ## Create a new placemark node
            nd_placemark <- nd_document %>% xml_add_child("Placemark")

            ## Add a few base properties
            nd_placemark %>%
              xml_add_child("styleUrl", "#hollow-cyan") %>%
              xml_add_sibling("name", fnbase_all[j])

            ## Don't clamp to ground - LINE DISAPPEARS UNDER TREES
            ## xml_add_sibling("altitudeMode", "clampToGround") %>%

            ## Create the string of coordinates
            coords_chr <- mcp_combined_sf %>%
              slice(j) %>%
              st_transform(4326) %>%
              st_coordinates() %>%
              as_tibble() %>%
              mutate(coords = paste(X, Y, "0", sep = ",")) %>%
              pull(coords) %>%
              paste(collapse = " ")

            ## Create the placemark child nodes
            nd_placemark %>%
              xml_add_child("Polygon") %>%
              xml_add_child("outerBoundaryIs") %>%
              xml_add_child("LinearRing") %>%
              xml_add_child("coordinates", coords_chr)

          }  ## for j in 1:nrow(mcp_combined_sf)

          ## Write to disk
          write_xml(nd_document %>% xml_root(), file = mcp_kml_pathfn)

          if (!quiet) message(yellow(paste0(" - ", combine_fn, "_mcp.kml saved")))
          files_saved <- c(files_saved, mcp_kml_pathfn)
        }

      }

    }

    if (!quiet) message(green("Done"))
    invisible(files_saved)
}
