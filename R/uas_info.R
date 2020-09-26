#' Extract info from UAS Images
#'
#' Extracts info from geotagged images taken from a drone
#'
#' @param img_dirs Directory(s) where the image files reside
#' @param exiftool The path to the exiftool command line tool (omit if on the OS path)
#' @param csv The file name of a new csv file where the exif data will be saved (omit to make a temp one)
#' @param alt_agl The elevation above ground level in meters (optional for images with the relevative altitude saved)
#' @param fp Compute image foot prints, T/F
#' @param fwd_overlap Whether or not to compute the amount of overlap between one image and the next, T/F
#' @param cameras Location of the cameras.csv file. Is NULL the package csv file will be used.
#' @param metadata A filename pattern for a metadata file, or a metadata list object (see Details)
#' @param cache Logical or a directory where EXIF data should be cached (see Details)
#' @param update_cache Whether to update the cache
#' @param quiet Don't show messages
#'
#' @details
#' This will read the EXIF header data from a directory of image files, and put out the centroids and image footprints on the ground. Mapping  image locations requires that the images have geostamps. In addition, mapping the image footprints requires that the camera parameters are known, and the flight elevation about ground level is either saved in the EXIF info, or provided in the \code{alt_agl} argument (in meters). If \code{alt_agl} is passed, it will override any elevation data in the EXIF info.
#'
#' Camera parameters are saved in a csv file called cameras.csv. To see where this is saved, type \code{system.file("cameras/cameras.csv", package="uasimg")}. If your camera is not detected, please contact the package author or create an issue on \href{https://github.com/UCANR-IGIS/uasimg/issues}{GitHub}.
#'
#' This function uses a free command line tool called EXIFtool to read the EXIF data, which can be downloaded
#' from \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}.  After you download it, rename the executable file,
#' 'exiftool(-k).exe' to 'exiftool.exe', and save it somewhere on your system's PATH (e.g., c:\\Windows).
#'
#' \code{metadata} is an optional argument to pass external metadata that can not be extracted from the
#' images (e.g., location name, pilot). It can be a regular expression for a metadata filename
#' (in YAML format, see below) that should be in the same folder as the images. Or it can be a named list containing
#' metadata fields. For supported field names, see \code{\link{uas_getflds}}.
#'
#' \code{metadata} cam also be a pattern for a filename (see \code{\link{list.files}}). If multiple files match the
#' pattern, they will all be read. Metadata files should be plain text in YAML format. Each line should contain a
#' key:value pair (with no quotes or delimiters). Lines starting with a hash or forward slash will be interpreted
#' as comments and ignored. For example:
#'
#' \code{collection_name: Hopland Research and Extension Center, Watershed II
#' data_url: https://ucdavis.box.com/s/dp0sdfssxxxxxsdf
#' description: These data were collected as part of a pre-restoration monitoring program}
#'
#' \code{cache} can be a logical value or the name of a directory where EXIF data gets cached.
#' If \code{cache = TRUE}, the default cache directory will be used (\code{~/.R}). Cached EXIF data is linked
#' to a directory of images based on the directory name and total size of all image files.
#' So if images are added or removed from the directory, the cache will be automatically rebuilt
#' the next time the function is run. \code{update_cache} is a logical value
#' which forces an update of cached data when TRUE.
#'
#' @return A UAS Image Collection object. This is a named list with elements for
#' image centroids (as a sf data frame), footprints, total area, minimum convex polygon,
#' total directory size, the data flown, and external metadata.
#'
#' @seealso \code{\link{uas_getcache}}, \code{\link{uas_report}}, \code{\link{uas_exp}}
#'
#' @import dplyr
#' @import sf
#' @importFrom grDevices chull
#' @importFrom digest digest
#' @importFrom tidyr replace_na
#' @importFrom utils read.csv
#' @importFrom crayon yellow green red magenta bold
#' @export

uas_info <- function(img_dirs, exiftool=NULL, csv=NULL, alt_agl=NULL,
                     fp=TRUE, fwd_overlap=fp, cameras=NULL, metadata = "metadata.*\\.txt",
                     cache=NULL, update_cache=FALSE, quiet=FALSE) {

  ## See if all directory(s) exist
  for (img_dir in img_dirs) {
    if (!file.exists(img_dir)) stop(paste0("Can't find ", img_dir))
  }

  if (is.null(cameras)) {
    cameras_fn <- system.file("cameras/cameras.csv", package="uasimg")
  } else {
    cameras_fn <- cameras
  }
  if (!file.exists(cameras_fn)) stop(paste0("Can't find cameras.csv file: ", cameras_fn))

  ## Create a list that we'll use later to shorten field names
  short_names <- list()
  short_names[["sourcefile"]] <- "img_fn"
  short_names[["filename"]] <- "file_name"
  short_names[["gpslatitude"]] <- "gps_lat"
  short_names[["gpslongitude"]] <- "gps_long"
  short_names[["datetimeoriginal"]] <- "date_time"
  short_names[["gpsdatestamp"]] <- "gps_date"
  short_names[["gpstimestamp"]] <- "gps_time"
  short_names[["gpsaltitude"]] <- "gps_alt"
  short_names[["make"]] <- "make"
  short_names[["model"]] <- "model"
  short_names[["focallength"]] <- "focal_len"
  short_names[["imagewidth"]] <- "img_width"
  short_names[["imageheight"]] <- "img_height"
  short_names[["relativealtitude"]] <- "alt_agl"
  ##short_names[[tolower(camera_tag_yaw)]] <- "yaw"   WILL DO THIS ONE BELOW AFTER WE GET THE CAMERA
  short_names[["sensor_width"]] <- "sens_wdth"
  short_names[["sensor_height"]] <- "sens_hght"
  short_names[["gsd"]] <- "gsd"
  short_names[["foot_w"]] <- "fp_width"
  short_names[["foot_h"]] <- "fp_height"
  short_names[["fwd_overlap"]] <- "fwd_ovrlap"

  ## See if exiftool is installed
  if (is.null(exiftool)) {
    if (.Platform$OS.type == "windows") {
      exiftool <- "exiftool.exe"
    } else {
      exiftool <- "exiftool"
    }
  }
  exiftool.exec <- findonpath(exiftool, status = FALSE)
  if (is.null(exiftool.exec)) {
    message(red("Cant find exiftool. Please make sure this file is downloaded and saved either in the working directory or a directory on the PATH environment variable (e.g., c:/windows). Download it from http://www.sno.phy.queensu.ca/~phil/exiftool/, then rename Rename 'exiftool(-k).exe' to 'exiftool.exe'."))
    return(invisible(NULL))
  }

  res <- list()
  for (img_dir in img_dirs) {

    if (!quiet) message(magenta$bold(img_dir))

    save_to_cache <- FALSE
    cache_loaded <- FALSE

    if (!is.null(cache)) {

      ## Get the cache directory
      cache_dir_use <- NA
      if (is.logical(cache)) {
        ## Cache is T/F
        if (cache) {
          cache_dir_use <- uas_getcache(quiet=TRUE, default=TRUE)
          if (!is.na(cache_dir_use)) save_to_cache <- TRUE
        } else {
          ## cache = FALSE. Take no action because
          ## cache_dir_use is already NA.
        }


      } else {
        ## String was passed, we presume this is a directory
        if (file.exists(cache)) {
          cache_dir_use <- cache
          save_to_cache <- TRUE
        } else {
          stop(paste0(cache, " does not exist. Please create and try again, or omit."))
        }
      }

      ## If a cache directory exists, look for a cached file for this folder
      if (!is.na(cache_dir_use)) {

        ## Construct the cache file name based on the image dir and total size
        ## Keep only image files for the purposes of computing the total file size
        dir_files <- list.files(img_dir, all.files = FALSE, full.names = TRUE)
        dir_files <- dir_files[grepl(".jpg$|.jpeg$|.tif$|.tiff$|.raw$", dir_files, ignore.case=TRUE)]
        #dir_files <- dir_files[!grepl(".txt$|.bak$", dir_files)]

        dir_size <- as.character(sum(file.size(dir_files)))
        cache_fn <- paste0("uas_", digest(paste0(img_dir, dir_size),
                                          algo='md5', serialize = FALSE), ".RData")

        ## If we're not doing auto-update, look for a file and load it if found
        if (!update_cache) {
          ## Look for a cache file
          if (file.exists(file.path(cache_dir_use, cache_fn))) {
            load(file.path(cache_dir_use, cache_fn))
            cache_loaded <- TRUE
            if (!quiet) message(yellow(" - Using cached data"))
          }
        }

      }

    }

    if (!cache_loaded) {

      ### Run EXIF tool on the first image to get the camera moodel
      ### (We assume all image files in the directory are from the same sensor, will not check)
      if (!quiet) message(yellow(" - Looking for image files"))

      first_fn <- list.files(path=img_dir, full.names=TRUE, pattern="jpg$|JPG$|jpeg$|JPEG$|tif$|TIF$")[1]
      if (is.na(first_fn)) stop(paste0("Couldn't find any jpg or tif files in ", img_dir))

      csv_first_fn <- tempfile(pattern="~map_uas_", fileext = ".csv")
      system2("exiftool", args=paste("-Make -Model -FileType -n -csv", shQuote(first_fn), sep=" "),
              stdout=csv_first_fn, stderr=FALSE)
      exif_first_df <- read.csv(csv_first_fn, stringsAsFactors = FALSE)
      file.remove(csv_first_fn)
      if (nrow(exif_first_df) == 0) stop("Couldn't find EXIF info in the first image file")

      camera_make <- exif_first_df[1, "Make"]
      camera_model <- exif_first_df[1, "Model"]
      camera_filetype <- exif_first_df[1, "FileType"]

      ## Import database of known sensors
      sensors_df <- utils::read.csv(cameras_fn, stringsAsFactors = FALSE)

      ## Search for this sensor
      sensor_this_df <- dplyr::filter(sensors_df, model==camera_model & filetype==camera_filetype)
      if (nrow(sensor_this_df)==0) stop(paste(camera_make, camera_model, camera_filetype, "not found in the database of known sensors"))

      ## Get the composite camera name from the sensor database
      camera_name <- sensor_this_df[1, "camera_name"]
      if (!quiet) message(yellow(" - Found", camera_name))

      ## Get the tag for yaw for this camera
      camera_tag_yaw <- sensor_this_df[1, "tag_yaw"]

      ## See if this camera stores elevation above ground level
      camera_agl_tag <- sensor_this_df[1, "tag_elev_agl"]
      camera_has_agl <- (camera_agl_tag != "none")

      if (is.null(alt_agl) && !camera_has_agl) {
        warning("The altitude above ground was not saved in the images, and no value for alt_agl was passed. Skipping gsd and footprints.")
        agl_avail <- FALSE
        # stop("alt_agl argument required (relative altitude not saved in image files)")
      } else {
        agl_avail <- TRUE
      }

      ## Still to come - incorporate non-nadir GimbalPitchDegree

      # Construct csv file name
      if (is.null(csv)) {
        csv_fn <- tempfile(pattern="map_uas_", fileext = ".csv")
      } else {
        csv_fn <- csv
      }

      # Identify EXIF tags to extract
      exif_tags <- c("FileName", "FileType", "FileSize#", "DateTimeOriginal", "GPSLatitude", "GPSLongitude",
                     "GPSAltitude", "Make", "Model", "FocalLength", "ImageWidth", "ImageHeight", camera_tag_yaw)
      if (camera_has_agl) exif_tags <- c(exif_tags, camera_agl_tag)

      ## NOTES
      ## Next version - check the model, grab appropriate tag
      ## Unused: "GPSLatitudeRef", "GPSLongitudeRef", "GPSAltitudeRef", (I think these are not needed, Exiftool uses to set sigh of decimal degrees
      ## FlightYawDegree
      ##
      ## "GPSDateStamp", "GPSTimeStamp" - these are the Sequoia only, replaced by DateTimeOriginal (which is an
      ## ExifID0 tag and saved in local time)
      ##
      ## DJI Drones (X5 and X3)
      ##   GimbalYawDegree is in degrees. 0 is north. 90 is east. -90 is west. 180 is south. -180 is south.
      ##
      ## Parrot Sequoia RGB and TIFF
      ##    no GimbalYawDegree, just'Yaw'
      ##    Make: Parrot
      ##    Model: Sequoia
      ##    Yaw (XMP-Camera)
      ##    Pitch (XMP-Camera)
      ##    Roll (XMP-Camera)
      ##    no tag for RelativeAltitude

      ## Construct args
      str_args <- paste("-", paste(exif_tags, collapse=" -"), " -n -csv ", shQuote(img_dir), sep="")

      # Run command
      if (!quiet) message(yellow(" - Running exiftool (this can take a while)..."), appendLF = FALSE)
      suppressWarnings(system2("exiftool", args=str_args, stdout=csv_fn, stderr=FALSE))
      if (!quiet) message(yellow("Done."))
      if (!file.exists(csv_fn)) {
        stop("exiftool could not create the csv file")
      }

      # Import EXIF CSV
      exif_df <- read.csv(csv_fn, stringsAsFactors=FALSE)
      if (is.null(csv)) file.remove(csv_fn)
      names(exif_df) <- tolower(names(exif_df))

      ## Right here we need to do some checks for tags

      ## Filter out images with incomplete EXIF info
      idx_incomplete <- which(is.na(exif_df$gpslatitude) |
                                is.na(exif_df$gpslongitude) |
                                is.na(exif_df$model) |
                                is.na(exif_df$filetype))
      if (length(idx_incomplete) > 0) exif_df <- exif_df[-idx_incomplete, ]


      ## Filter out images with 0 elevation
      if (agl_avail) {
        if (is.null(alt_agl)) {
          idx_onground <- which(exif_df[[camera_agl_tag]] <= 0)
          if (length(idx_onground) > 0) exif_df <- exif_df[-idx_onground, ]
        }
      }

      ## Get the total file size
      total_size_mb <- round(sum(exif_df$filesize) / 1048576)
      if (!quiet) message(yellow(paste0(" - Total file size: ", total_size_mb, " MB")))

      ## Get the date flown
      flight_date_dt <- as.Date(exif_df[1, "datetimeoriginal", drop=TRUE], format = "%Y:%m:%d %H:%M:%S")
      flight_date_str <- format(flight_date_dt, "%Y-%m-%d")

      ## Add the sensor dimensions to to exif_df
      sensor_info_df <- sensors_df %>% dplyr::select(model, filetype, sensor_width, sensor_height)
      exif_df <- exif_df %>% dplyr::left_join(sensor_info_df, by=c("model" = "model", "filetype" = "filetype"))

      ## Add image footprint gsd and dimensions
      ## Based on Pix4D GSD calculator. Assumptions:
      ##    input units: sensor_width and height - mm; focal length - mm, RelativeAltitude (flight height) - meters
      ##    output units: gsd - cm/pixel, Dw, Dh - meters
      ##    See https://support.pix4d.com/hc/en-us/articles/202560249-TOOLS-GSD-Calculator#gsc.tab=0

      ## Create the expression object for the gsd calculation
      if (agl_avail) {
        if (is.null(alt_agl)) {
          gsd_exprsn <- parse(text=paste("(sensor_width * ", tolower(camera_agl_tag),
                                         " * 100) / (focallength * imagewidth)", sep=""))
        } else {
          gsd_exprsn <- parse(text=paste("(sensor_width * ", alt_agl,
                                         " * 100) / (focallength * imagewidth)", sep=""))
        }
        exif_df <- dplyr::mutate(exif_df, gsd=eval(gsd_exprsn))
        exif_df <- dplyr::mutate(exif_df, foot_w=(gsd*imagewidth)/100, foot_h=(gsd*imageheight)/100)
      }

      #################################################################
      ## CREATE SPATIAL OBJECTS

      if (!("gpslongitude" %in% names(exif_df) && "gpslatitude" %in% names(exif_df))) {

        warning(paste0("gpslongitude and/or gpslatitude not found in the EXIF data for ",
                       img_dir, ". Skipping centroids and footprints."))

        imgs_ctr_utm_sf <- NA
        fp_utm_sf <- NA
        area_m2 <- NA
        mcp_sf <- NA

      } else {

        ## Create a sf dataframe for the image centroids
        imgs_ctr_ll_sf <- sf::st_as_sf(exif_df, coords = c("gpslongitude","gpslatitude"), remove = FALSE, crs = 4326)

        ## Convert to UTM
        utm_epsg <- geo2utm(exif_df[1,"gpslongitude"], exif_df[1,"gpslatitude"])
        imgs_ctr_utm_sf <- imgs_ctr_ll_sf %>% st_transform(utm_epsg)

        ## Compute footprints
        if (!fp || !agl_avail || camera_tag_yaw == "none") {
          fp_utm_sf <- NA
          if (!quiet) message(yellow(" - Skipping footprints"))
          nodes_all_mat <- imgs_ctr_utm_sf %>% st_coordinates()

        } else {
          short_names[[tolower(camera_tag_yaw)]] <- "yaw"

          if (!quiet) message(yellow(" - Creating footprints..."), appendLF = FALSE)
          corners_sign_mat <- matrix(data=c(-1,1,1,1,1,-1,-1,-1,-1,1), byrow=TRUE, ncol=2, nrow=5)

          ctr_utm <- sf::st_coordinates(imgs_ctr_utm_sf)

          ## Create an empty list to hold the individual st_polygons
          polys_sf_lst <- vector("list", nrow(imgs_ctr_utm_sf))

          ## Create an empty 2-column matrix to store the corners for all the footprints
          ## (in order to make the MCP)
          nodes_all_mat <- matrix(ncol=2, nrow=0)

          ## Loop through the centroids
          for (i in 1:nrow(imgs_ctr_utm_sf)) {
            dx <- imgs_ctr_utm_sf[i, "foot_w", drop = TRUE]
            dy <- imgs_ctr_utm_sf[i, "foot_h", drop = TRUE]

            if (dx>0 && dy>0) {

              ## Compute the nodes of the corners (centered around 0,0)
              corners_mat <- corners_sign_mat * matrix(data=c(dx/2, dy/2), byrow=TRUE, ncol=2, nrow=5)

              # Convert the gimbal yaw degree to radians, the rotate the rectangle to
              # align with the gimbal direction
              # DJI Gimbal directions
              #   0  = north (no rotation needed)
              #   90 = east (rotate 90 degrees clockwise)
              #  -90 = west (rotate 90 degress counter-clockwise)
              # -179, + 179 = south (rotate 180 degrees)

              # check if the Sequoia Yaw has the same alignment

              theta <- - pi * imgs_ctr_utm_sf[i,tolower(camera_tag_yaw), drop = TRUE] / 180
              rot_mat <- matrix(data=c(cos(theta),  -sin(theta), sin(theta), cos(theta)),
                                nrow=2, byrow=TRUE)
              corners_mat <- t(rot_mat %*% t(corners_mat))

              ## Grab the coordinates of this image centroid
              img_ctr_mat <- matrix(ctr_utm[i,], byrow=TRUE, ncol=2, nrow=5)
              fp_nodes_mat <- img_ctr_mat + corners_mat

              ## Create a polygon for this footprint and add it to the list
              polys_sf_lst[[i]] <- sf::st_polygon(list(fp_nodes_mat), dim = "XY")

              ## Add the nodes to the master matrix (for the purposes of computing the overall area)
              nodes_all_mat <- rbind(nodes_all_mat, fp_nodes_mat[1:4,])

            }
          }

          ## Create a sf data frame for the footprints, using the attributes from the centroids
          fp_utm_sf <- st_sf(imgs_ctr_utm_sf %>% sf::st_drop_geometry(),
                             geometry = sf::st_sfc(polys_sf_lst, crs = utm_epsg))

          if (!quiet) message(yellow("Done."))

          ## Compute the forward overlap
          if (fwd_overlap) {

            if (nrow(fp_utm_sf) == 1) {
              if (!quiet) message(yellow(" - Only 1 image, skipping forward overlap."))

            } else {
              if (!quiet) message(yellow(" - Computing forward overlap..."), appendLF = FALSE)

              idx_minus_one <- 1:(nrow(fp_utm_sf)-1)

              ## Compute areas of each footprint
              area_polys <- fp_utm_sf %>% slice(idx_minus_one) %>% st_area() %>% as.numeric()

              ## Compute the area of intersection of each footprint with the next one
              area_intersection_with_next <- sapply(idx_minus_one, function(i)
                st_intersection(fp_utm_sf[i, "geometry"], fp_utm_sf[i+1, "geometry"]) %>%
                  st_area()) %>% as.numeric() %>% replace_na(0)

              ## Add the precent overlap to the sf dataframe
              fp_utm_sf$fwd_overlap <- c(area_intersection_with_next / area_polys, NA)

              if (!quiet)message(yellow("Done."))

            }

          }

          ## Shorten field names in fp_utm_sf
          all_flds <- names(fp_utm_sf)
          for (i in 1:length(all_flds)) {
            fldname <- all_flds[i]
            if (fldname %in% names(short_names)) {
              names(fp_utm_sf)[i] <- short_names[[fldname]]
            }
          }

        }

        ## Create the MCP
        ## Compute area based on the convex hull around all the corners of all the footprints
        ## Find the indices of the combined set of corners that comprise the MCP nodes
        chull_idx <- chull(nodes_all_mat)
        chull_idx <- c(chull_idx, chull_idx[1])

        ## Turn this into a sf polygon
        mcp_polygon <- st_polygon(list(nodes_all_mat[chull_idx,]), dim = "XY")
        mcp_sfc <- st_sfc(mcp_polygon, crs = utm_epsg)

        ## Compute the area
        area_m2 <- mcp_sfc %>% st_area() %>% as.numeric()

        ## Generate a sf data frame for the MCP
        mcp_sf <- st_sf(data.frame(img_dir=img_dir, area_m2=area_m2), geometry = mcp_sfc)

        ## Shorten field names in imgs_ctr_utm_sf
        all_flds <- names(imgs_ctr_utm_sf)
        for (i in 1:length(all_flds)) {
          fldname <- all_flds[i]
          if (fldname %in% names(short_names)) {
            names(imgs_ctr_utm_sf)[i] <- short_names[[fldname]]
          }
        }

      }  #if (!("gpslongitude" %in% names(exif_df) && "gpslatitude" %in% names(exif_df)))

    }

    ## Cache results (if not already cached)
    if (save_to_cache) {
      if (update_cache || !cache_loaded) {
        ## Store the image folder name in case the cache data is used on its own
        img_folder <- img_dir
        save(img_folder, imgs_ctr_utm_sf, fp_utm_sf, area_m2, mcp_sf, total_size_mb,
             flight_date_str, camera_name, file = file.path(cache_dir_use, cache_fn))
        if (!quiet) message(yellow(" - Cache saved"))
      }
    }

    ## Load the additional meta data (which is not cached!)
    ## Get the extra metadata either by an argument or finding an metadata.txt file

    if (is.null(metadata)) {
      ## Create a blank metadata list
      flds_md <- uas_getflds()
      metadata_use <- as.list(rep(as.character(NA), length(flds_md)))
      names(metadata_use) <- flds_md

    } else if (is.list(metadata)) {
      metadata_use<- metadata

    } else if (is.character(metadata)) {
      ## Presume this is a file name pattern
      ## metadata_fn <- file.path(img_dir, "metadata.txt")
      metadata_fn <- list.files(img_dir, metadata, full.names = TRUE)

      if (length(metadata_fn) == 0) {
        if (!quiet) message(yellow(" - Metadata file not found"))
        flds_md <- uas_getflds()
        metadata_use <- as.list(rep(as.character(NA), length(flds_md)))
        names(metadata_use) <- flds_md

      } else {

        metadata_use <- list()

        for (md_fn in metadata_fn) {

          if (!quiet) message(yellow(" - Reading", basename(md_fn)))

          fcon <- file(md_fn, open = "r")
          while ( TRUE ) {
            one_line <- readLines(fcon, n = 1, warn = FALSE)
            if ( length(one_line) == 0 ) {
              ## You've gotten to the end
              break
            }

            first_char <- trimws(substr(one_line, 1, 1))
            if (first_char != "#" && first_char != "/") {
              colon_pos <- regexpr(":", one_line)
              if (colon_pos > 0) {

                ## Key (before colon)
                ln_key <- trimws(substring(one_line, 1, colon_pos - 1)[1])

                ##if (ln_key %in% names(metadata_use)) {  }
                metadata_use[[ln_key]] <- gsub("\"", "'",
                                               trimws(substring(one_line, colon_pos + 1)[1]))


              }

            }

          }  ## while TRUE

          close(fcon)

        }    ## for (md_fn in metadata_fn)

      }


    } else {
      warning("Unknown object for metadata")
      flds_md <- uas_getflds()
      metadata_use <- as.list(rep(as.character(NA), length(flds_md)))
      names(metadata_use) <- flds_md
    }

    ## Add to the result list
    res[[img_dir]] <- list(pts = imgs_ctr_utm_sf,
                           fp = fp_utm_sf,
                           area_m2 = area_m2,
                           mcp = mcp_sf,
                           size_mb = total_size_mb,
                           date_flown = flight_date_str,
                           camera_name = camera_name,
                           metadata = metadata_use)
  }


  if (!quiet) message(green("All done"))

  ## Return the results
  class(res) <- c("list", "uas_info")
  res

}

