#' Extract metadata info from UAS images
#'
#' Extracts info from geotagged images taken from a drone
#'
#' @param img_dirs Directory(s) where the image files reside
#' @param ext An optional file extension
#' @param alt_agl The elevation above ground level in meters (optional for images with the relevative altitude saved)
#' @param fp Compute image foot prints, T/F
#' @param fwd_overlap Whether or not to compute the amount of overlap between one image and the next, T/F
#' @param cameras Location of the cameras.csv file. Is NULL the package csv file will be used.
#' @param metadata A filename pattern for a metadata file, or a metadata list object (see Details)
#' @param path2name_fun A function to generate the default short name (see Details)
#' @param use_exiftoolr Whether to use the exiftoolr package, logical
#' @param exiftool The path to the exiftool command line tool (omit if on the OS path). Ignored if use_exiftoolr = TRUE.
#' @param exif_csv The file name of a new csv file where the exif data will be saved (omit to make a temp one)
#' @param cache Logical or a directory where EXIF data should be cached (see Details)
#' @param update_cache Whether to update the cache
#' @param quiet Don't show messages
#'
#' @details This will read the EXIF header data from a directory of image files, and extract
#' the centroids and estimated ground-footprints. Extracting the image locations requires that the images have
#' geostamps (common in most drone images but some drones require an extra processing step to geostamp the images).
#' To index a specific file type, you can pass a file extension to \code{ext} (not case sensitive).
#'
#' Estimating the ground-footprints (\code{fp = TRUE}) further requires that the camera parameters are known,
#' the flight elevation about ground level is either saved in the EXIF info or provided in
#' the \code{alt_agl} argument (in meters). If \code{alt_agl} is passed, it will override any elevation data
#' in the EXIF info. Ground-footprints are estimates only. They assume the camera was at nadir (common in
#' mapping work but there are exceptions) and only as accurate as the provided altitude (which is typically the least
#' accurate GPS coordinate).
#'
#' Camera parameters are saved in a csv file called \emph{cameras.csv}. The package ships with a CSV file containing
#' many popular drone cameras. If your drone camera is not in the database, you can create your own
#' \emph{cameras.csv} file (see \code{\link{uas_cameras}} for details) and pass the file name as the
#' \code{cameras} argument. Or contact the package maintainer to add your camera to the database.
#'
#' This function uses a free command line tool called EXIFtool to read the EXIF data. If you haven't already,
#' you can install this by running \code{\link[exiftoolr]{install_exiftool}}. Alternately you can download exiftool
#' from \url{http://www.sno.phy.queensu.ca/~phil/exiftool/}. After you download it, rename the executable file,
#' 'exiftool(-k).exe' to 'exiftool.exe', and save it somewhere on your system's PATH (e.g., c:\\Windows).
#'
#' \code{metadata} is an optional argument to pass supplemental flight metadata that can not be extracted from the
#' images (e.g., location name, pilot). For details, see the Vignette on Flight Metadata
#' \code{vignette("flight_metadata", package = "uasimg")}.
#'
#' \code{metadata} can also be a named list containing
#' metadata fields / values. For supported field names, see \code{vignette("flight_metadata", package = "uasimg")}.
#'
#' \code{metadata} can also be a filename regular expression (pattern) for a metadata text file
#' (for details on how to write a pattern expression, see \code{\link{list.files}}). This is the recommended
#' way to enter metadata, because the little text files move with the images.
#'
#' If multiple files match the pattern expression, they will all be read. This allows you for example to have a file called
#' \emph{metadata_org.txt} with organizational info (such as a contact person), and another called \emph{metadata.txt} with info
#' about that specific flight (e.g., the pilot or wind conditions).

#' Metadata text files should be plain text in YAML format (the easiest way to create new metadata text files
#' is using \code{\link{uas_metadata_make}}.) Each line should contain a key:value pair (with no quotes or delimiters).
#' Lines starting with a hash or forward slash will be ignored. Example:
#'
#' \preformatted{
#' name_short: hrec_wtrshd2_flt03
#'
#' name_long: Hopland Research and Extension Center, Watershed II Flight 3
#'
#' data_url: https://ucdavis.box.com/s/dp0sdfssxxxxxsdf
#'
#' pilot: Andy Lyons
#'
#' description: These data were collected as part of a restoration monitoring program.
#'
#' notes: We had to pause the mission about half way through as a hawk was getting close, hence there is a time lapse
#' of about 45 seconds. Pix4Dcapture was used as the mission planning software with an overlap of 75%.
#' }
#'
#' \code{path2name_fun} can be a function to generate a default short name for the flight. The function should
#' be written to accept one and only one argument - a directory path. This can be useful if the default
#' short names should be constructed from pieces of the image directory path. See also \code{\link{uas_path2name_fun}}.
#'
#' \code{cache} can be a logical value or the name of a directory where EXIF data gets cached.
#' If \code{cache = TRUE}, the default cache directory will be used (\code{~/.R}). The only information
#' that gets cached is image metadata. Flight metadata is never cached (see the Vignette on Flight Metadata
#' for a discussion of image and flight metadata). Cached EXIF data is linked
#' to a directory of images based on the directory name and total size of all image files.
#' So if images are added or removed from the directory, the cache will be automatically rebuilt
#' the next time the function is run. \code{update_cache} is a logical value
#' which forces an update of cached data when TRUE.
#'
#' @return A UAS Image Collection object. This is a named list with elements for
#' image centroids (as a sf data frame), footprints, total area, minimum convex polygon,
#' total directory size, the data flown, and external metadata.
#'
#' @seealso \code{\link{uas_getcache}}, \code{\link{uas_report}}, \code{\link{uas_path2name_fun}}
#'
#' @import dplyr
#' @import sf
#' @importFrom grDevices chull
#' @importFrom digest digest
#' @importFrom tidyr replace_na
#' @importFrom dplyr filter select left_join mutate
#' @importFrom magrittr extract2 %>%
#' @importFrom utils read.csv
#' @importFrom sf st_as_sf st_coordinates st_polygon st_drop_geometry st_sf st_sfc
#' @importFrom crayon yellow green red magenta bold
#' @importFrom exiftoolr configure_exiftoolr
#' @export

uas_info <- function(img_dirs, ext = NULL, alt_agl = NULL, fp = FALSE, fwd_overlap = fp,
                     cameras = NULL, metadata = "metadata.*\\.txt", path2name_fun = NULL,
                     use_exiftoolr = TRUE, exiftool = NULL, exif_csv = NULL,
                     cache = TRUE, update_cache = FALSE, quiet = FALSE) {


  ## Define the date for a cache to be considered valid (due to updates in the package that modify what gets saved)
  cache_valid_date <- ISOdatetime(2021, 5, 9, 0, 0, 0)

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
  short_names <- as.list(c("sourcefile" = "img_fn",
                           "filename" = "file_name",
                           "gpslatitude" = "gps_lat",
                           "gpslongitude" = "gps_long",
                           "datetimeoriginal" = "date_time",
                           "gpsdatestamp" = "gps_date",
                           "gpstimestamp" = "gps_time",
                           "gpsaltitude" = "gps_alt",
                           "make" = "make",
                           "model" = "model",
                           "focallength" = "focal_len",
                           "imagewidth" = "img_width",
                           "imageheight" = "img_height",
                           "relativealtitude" = "alt_agl",
                           "sensor_width" = "sens_wdth",
                           "sensor_height" = "sens_hght",
                           "gsd" = "gsd",
                           "foot_w" = "fp_width",
                           "foot_h" = "fp_height",
                           "fwd_overlap" = "fwd_ovrlap"))

  ## nomap will (eventually) be a vector of images that should be excluded
  ## from the flight summary map, mcp computation, area computation, etc.
  ## This is for things like calibration images
  nomap <- NA

  ## Look for the exiftool executable
  if (use_exiftoolr) {
    ## Note: system2() doesn't require you to quote an executable (only arguments with spaces)
    ## I can't suppress printing the version number, even sink() doesn''t work.
    ## Will have to live with it.
    exiftool_exec <- configure_exiftoolr(quiet = TRUE)

  } else {
    ## Not using exiftoolr. Look on the path for the correct filename.
    if (is.null(exiftool)) {
      if (.Platform$OS.type == "windows") {
        exiftool <- "exiftool.exe"
      } else {
        exiftool <- "exiftool"
      }
    }
    exiftool_exec <- findonpath(exiftool, status = FALSE)
    if (is.null(exiftool_exec)) {
      message(red("Cant find exiftool. Please make sure this file is downloaded and saved either in the working directory or a directory on the PATH environment variable (e.g., c:/windows). Download it from http://www.sno.phy.queensu.ca/~phil/exiftool/, then rename Rename 'exiftool(-k).exe' to 'exiftool.exe'."))
      return(invisible(NULL))
    } else {
      exiftool_exec <- exiftool_exec
    }
  }

  if (is.null(ext)) {
    grep_pattern_use <- ".jpg$|.jpeg$|.tif$|.tiff$|.raw$|.dng$"
    ext_use <- ""
    ext_msg <- " (all image files)"
  } else {
    grep_pattern_use <- paste0(".", ext, "$")
    ext_use <- paste0("-ext ", ext, " ")
    ext_msg <- paste0(" (", ext, " files)")
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
        ## String was passed for cache, we presume this is a directory
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
        dir_files <- dir_files[grepl(grep_pattern_use, dir_files, ignore.case=TRUE)]
        #dir_files <- dir_files[!grepl(".txt$|.bak$", dir_files)]

        dir_size <- as.character(sum(file.size(dir_files)))
        cache_fn <- paste0("uas_", digest(paste0(img_dir, dir_size),
                                          algo='md5', serialize = FALSE), ".RData")

        ## If we're not doing auto-update, look for a file and load it if found
        if (!update_cache) {
          ## Look for a cache file
          cache_pathfn <- file.path(cache_dir_use, cache_fn)
          if (file.exists(cache_pathfn)) {
            ## Make sure its newer than the release of version 1.7.0
            if (file.mtime(cache_pathfn) > cache_valid_date) {
              load(cache_pathfn)
              cache_loaded <- TRUE
              if (!quiet) message(yellow(" - Using cached data"))
            } else {
              if (!quiet) message(yellow(" - Updating cached data"))
            }
          }
        }

      }

    }

    if (!cache_loaded) {

      ### Run EXIF tool on the first image to get the camera model
      ### (We assume all image files in the directory are from the same sensor, will not check)
      if (!quiet) message(yellow(paste0(" - Looking for image files", ext_msg)))

      first_fn <- list.files(path=img_dir, full.names=TRUE, pattern=grep_pattern_use, ignore.case = TRUE)[1]

      if (is.na(first_fn)) stop(paste0("Couldn't find any image files that match the pattern in ", img_dir))

      csv_first_fn <- tempfile(pattern="~map_uas_", fileext = ".csv")

      system2(exiftool_exec, args=paste("-Make -Model -FileType -n -csv", shQuote(first_fn), sep=" "),
              stdout=csv_first_fn, stderr=FALSE)

      exif_first_df <- read.csv(csv_first_fn, stringsAsFactors = FALSE)

      file.remove(csv_first_fn)
      if (nrow(exif_first_df) == 0) stop("Couldn't find EXIF info in the first image file")

      camera_make <- exif_first_df[1, "Make"]
      camera_model <- exif_first_df[1, "Model"]
      camera_filetype <- exif_first_df[1, "FileType"]

      ## Import database of known sensors
      sensors_df <- uas_readcameras(cameras_fn)

      ## Search for this sensor
      sensor_this_df <- sensors_df %>%
        filter(model == camera_model & filetype == camera_filetype) %>%
        as.data.frame()

      if (nrow(sensor_this_df)==0) {

        ## UNKNOWN SENSOR!!!!

        ## right here I need to get **all** the fields, so I can see which one is the date time
        # system2(exiftool_exec, args=paste("-Make -Model -FileType -n -csv", shQuote(first_fn), sep=" "),
        #         stdout=csv_first_fn, stderr=FALSE)

        if (!quiet) message(yellow(paste0(" - Unknown sensor: ", camera_make, " ", camera_model)))
        if (!quiet) message(yellow(" - to add this camera to the database, please contact the package author via email, or create an issue on GitHub"))
        if (!quiet) message(yellow(" - using generic camera settings"))

        camera_name <- "unknown camera"
        camera_tag_yaw <- "none"
        camera_tag_dt <- "DateTimeOriginal" ## this could cause an error
        short_names[["none"]] <- "yaw"
        camera_agl_tag <- "none"
        camera_has_agl <- FALSE
        agl_avail <- FALSE

      } else {

        ## Get the human-friendly camera name from the sensor database
        camera_name <- sensor_this_df[1, "camera_name", drop = TRUE]

        ## TODO NOT SURE WHY ITS PUTTING A BLANK LINE AFTER THE FOLLOWING MESSAGE
        ## I'VE TRIED A LOT OF DIFFERENT THINGS!!
        if (!quiet) message(yellow(paste(" - Found", camera_name)))

        ## Get the tag for yaw for this camera
        camera_tag_yaw <- sensor_this_df[1, "tag_yaw"]
        short_names[[tolower(camera_tag_yaw)]] <- "yaw"

        ## Get the date_time field(s) for this camera. Usually this will be "DateTimeOriginal" but there are
        ## some cameras that don't have this EXIF tag, in which case we can use GPSDateStamp|GPSTimeStamp
        camera_tag_dt <- sensor_this_df[1, "date_time"]  %>% strsplit("\\|") %>% extract2(1)

        ## See if this camera stores elevation above ground level
        camera_agl_tag <- sensor_this_df[1, "tag_elev_agl"]
        camera_has_agl <- (tolower(camera_agl_tag) != "none")

        if (is.null(alt_agl) && !camera_has_agl) {
          agl_avail <- FALSE
          if (fp || fwd_overlap) {
            warning_msg <- "Can not estimate footprints - above ground altitude was not saved in the images, and no value for alt_agl was passed."
            if (quiet) {
              warning(warning_msg)
            } else {
              message(red(" -", warning_msg))
            }
          }
          # stop("alt_agl argument required (relative altitude not saved in image files)")
        } else {
          agl_avail <- TRUE
        }

      }

      ######################################
      ## Still to come - incorporate non-nadir GimbalPitchDegree

      # Construct exif_csv file name
      if (is.null(exif_csv)) {
        exif_csv_fn <- tempfile(pattern="uasimg_exifdata_", fileext = ".csv")
      } else {
        exif_csv_fn <- exif_csv
      }

      # Identify EXIF tags to extract. These are generic for all / most cameras
      exif_tags <- c("FileName", "FileType", "FileSize#", "GPSLatitude", "GPSLongitude",
                     "GPSAltitude", "Make", "Model", "FocalLength", "ImageWidth", "ImageHeight",
                     camera_tag_dt, camera_tag_yaw)

      if (camera_has_agl) exif_tags <- c(exif_tags, camera_agl_tag)

      ## Construct args
      str_args <- paste("-", paste(exif_tags, collapse=" -"), " -n -csv ", ext_use, shQuote(img_dir), sep="")

      # Run exiftool command
      if (!quiet) message(yellow(" - Running exiftool (this can take a while)..."), appendLF = FALSE)

      suppressWarnings(system2(exiftool_exec, args = str_args, stdout = exif_csv_fn, stderr = FALSE))
      if (!quiet) message(yellow("Done."))
      if (!file.exists(exif_csv_fn)) {
        stop("exiftool could not create the csv file")
      }

      # Import EXIF CSV

      exif_df <- read.csv(exif_csv_fn, stringsAsFactors=FALSE)
      if (is.null(exif_csv)) file.remove(exif_csv_fn)
      names(exif_df) <- tolower(names(exif_df))

      ## TODO Right here we need to do some checks for tags

      ## Filter out images with incomplete EXIF info
      idx_incomplete <- which(is.na(exif_df$gpslatitude) |
                                is.na(exif_df$gpslongitude) |
                                is.na(exif_df$model) |
                                is.na(exif_df$filetype))
      if (length(idx_incomplete) > 0) exif_df <- exif_df[-idx_incomplete, ]

      ## Filter out images with 0 elevation
      if (agl_avail) {
        if (is.null(alt_agl)) {
          idx_onground <- which(exif_df[[tolower(camera_agl_tag)]] <= 0)
          if (length(idx_onground) > 0) exif_df <- exif_df[-idx_onground, ]
        }
      }

      ## Get the total file size
      total_size_mb <- round(sum(exif_df$filesize) / 1048576)
      if (!quiet) message(yellow(paste0(" - Total file size: ", total_size_mb, " MB")))

      ## If datetimeoriginal doesn't exist, try to create it
      if (!"datetimeoriginal" %in% names(exif_df)) {
        exif_df[["datetimeoriginal"]] <- apply(exif_df[, tolower(camera_tag_dt), drop = FALSE], 1, paste, collapse = " ")

        ## see if it worked
        test_dt <- as.POSIXct(exif_df[1, "datetimeoriginal", drop=TRUE], format = "%Y:%m:%d %H:%M:%S")
        if (!inherits(test_dt, "POSIXct")) {
          warning_msg <- "Can't construct the timestamps"
          if (quiet) {
            warning(warning_msg)
          } else {
            message(red(" -", warning_msg))
          }
          exif_df[["datetimeoriginal"]] <- "1970-01-01 12:00:00"
        }
      }

      ## Get the date flown
      flight_date_dt <- as.Date(exif_df[1, "datetimeoriginal", drop=TRUE], format = "%Y:%m:%d %H:%M:%S")
      flight_date_str <- format(flight_date_dt, "%Y-%m-%d")

      ## Add the sensor dimensions to to exif_df
      if (camera_name == "unknown camera") {
        exif_df <- exif_df %>%
          mutate(camera_name = camera_name, camera_abbrev = "unknown", sensor_width = 0, sensor_height = 0)

      } else {
        sensor_info_df <- sensors_df %>%
          select(model, camera_name, camera_abbrev, filetype, sensor_width, sensor_height)

        exif_df <- exif_df %>% left_join(sensor_info_df, by=c("model" = "model", "filetype" = "filetype"))
      }

      ## no longer needed - these columns are now brought in a factors in readcameras()
      # mutate(make = as.factor(make)
      #      camera_name = as.factor(camera_name),
      #      camera_abbrev = as.factor(camera_abbrev)) %>%

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
        exif_df <- mutate(exif_df, gsd=eval(gsd_exprsn))
        exif_df <- mutate(exif_df, foot_w=(gsd*imagewidth)/100, foot_h=(gsd*imageheight)/100)
      }

      #################################################################
      ## CREATE SPATIAL OBJECTS

      if (!("gpslongitude" %in% names(exif_df) && "gpslatitude" %in% names(exif_df))) {

        warning_msg <- paste0("gpslongitude and/or gpslatitude not found in the EXIF data for ",
                            img_dir, ". Skipping centroids and footprints.")
        if (quiet) {
          warning(warning_msg)
        } else {
          message(red(" -", warning_msg))
        }

        imgs_ctr_utm_sf <- NA
        fp_utm_sf <- NA
        area_m2 <- NA
        mcp_sf <- NA

      } else {

        ## Create a sf dataframe for the image centroids
        imgs_ctr_ll_sf <- st_as_sf(exif_df, coords = c("gpslongitude","gpslatitude"), remove = FALSE, crs = 4326)

        ## Convert to UTM
        utm_epsg <- geo2utm(exif_df[1,"gpslongitude"], exif_df[1,"gpslatitude"])
        imgs_ctr_utm_sf <- imgs_ctr_ll_sf %>% st_transform(utm_epsg)

        ## Compute footprints
        if (!fp || !agl_avail || tolower(camera_tag_yaw) == "none" || sum(exif_df$sensor_width) == 0) {
          fp_utm_sf <- NA
            if (!quiet && sum(exif_df$sensor_width) == 0) message(yellow(" - Sensor size not available for this camera"))
          if (!quiet) message(yellow(" - Skipping footprints"))
          nodes_all_mat <- imgs_ctr_utm_sf %>% st_coordinates()

        } else {
          if (!quiet) message(yellow(" - Creating footprints..."), appendLF = FALSE)
          corners_sign_mat <- matrix(data=c(-1,1,1,1,1,-1,-1,-1,-1,1), byrow=TRUE, ncol=2, nrow=5)

          ctr_utm <- st_coordinates(imgs_ctr_utm_sf)

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

              # check if the Sequoia 'Yaw' has the same alignment

              theta <- - pi * imgs_ctr_utm_sf[i, tolower(camera_tag_yaw), drop = TRUE] / 180
              rot_mat <- matrix(data=c(cos(theta),  -sin(theta), sin(theta), cos(theta)),
                                nrow=2, byrow=TRUE)
              corners_mat <- t(rot_mat %*% t(corners_mat))

              ## Grab the coordinates of this image centroid
              img_ctr_mat <- matrix(ctr_utm[i,], byrow=TRUE, ncol=2, nrow=5)
              fp_nodes_mat <- img_ctr_mat + corners_mat

              ## Create a polygon for this footprint and add it to the list
              polys_sf_lst[[i]] <- st_polygon(list(fp_nodes_mat), dim = "XY")

              ## Add the nodes to the master matrix (for the purposes of computing the overall area)
              nodes_all_mat <- rbind(nodes_all_mat, fp_nodes_mat[1:4,])

            }

          }  ## for (i in 1:nrow(imgs_ctr_utm_sf)) {

          ## Create a sf data frame for the footprints, using the attributes from the centroids
          fp_utm_sf <- st_sf(imgs_ctr_utm_sf %>% st_drop_geometry(),
                             geometry = st_sfc(polys_sf_lst, crs = utm_epsg))

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

              ## Add the percent overlap to the sf dataframe
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

      ## CREATE AN ID STRING (WHICH WILL BE USED AS THE DEFAULT SHORT_NAME ALSO)
      if (is.null(path2name_fun)) {

        ## THIS BLOCK WAS CAUSING AN ERROR WHEN imgs_ctr_utm_sf WAS NULL
        ## BECAUSE gpslongitude WAS NOT FOUND ABOVE

            # if ("date_time" %in% names(imgs_ctr_utm_sf)) {
            #   dt_str <- paste0(gsub(" ", "_", gsub(":", "-", sort(imgs_ctr_utm_sf$date_time)[1])), "_")
            # } else {
            #   dt_str <- ""
            # }

        if ("datetimeoriginal" %in% names(exif_df)) {
          dt_str <- paste0(gsub(" ", "_", gsub(":", "-", sort(exif_df$datetimeoriginal)[1])), "_")
        } else {
          dt_str <- ""
        }

        # type_count <- imgs_ctr_utm_sf %>%    ## THIS WAS CAUSING AN ERROR WHEN imgs_ctr_utm_sf WAS NULL
        #   st_drop_geometry() %>%             ## BECAUSE gpslongitude WAS NOT FOUND ABOVE
        #   group_by(filetype) %>%
        #   count() %>%
        #   mutate(type_count = paste0(n, filetype, "s")) %>%
        #   pull(type_count) %>%
        #   paste(collapse = "_")

        type_count <- exif_df %>%
          group_by(filetype) %>%
          count() %>%
          mutate(type_count = paste0(n, filetype, "s")) %>%
          pull(type_count) %>%
          paste(collapse = "_")

        id_str <- paste0(dt_str, type_count)

      }  else {
        ## path2name_fun is NOT NULL (presume it's a function)
        id_str <- path2name_fun(img_dir)

      }


    } # if !cache_loaded

    ## Cache results (if not already cached)
    if (save_to_cache) {
      if (update_cache || !cache_loaded) {
        ## Store the image folder name in case the cache data is used on its own
        img_folder <- img_dir
        save(img_folder, imgs_ctr_utm_sf, fp_utm_sf, area_m2, mcp_sf, total_size_mb,
             flight_date_str, camera_name, id_str, file = file.path(cache_dir_use, cache_fn))
        if (!quiet) message(yellow(" - Cache saved"))
      }
    }

    ########################################################################################################
    ## Load the additional flight metadata (which is never cached!)
    ## Get the extra metadata either by an argument or finding an metadata.txt file

    if (is.null(metadata)) {
      ## Create a blank metadata list using the default fields
      flds_md <- uas_getflds()
      metadata_use <- as.list(rep(as.character(NA), length(flds_md)))
      names(metadata_use) <- flds_md
      if (!quiet) message(yellow(paste0(" - Metadata fields set to NA: ", paste(flds_md, collapse = ", "))))

    } else if (is.list(metadata)) {
      metadata_use <- metadata

    } else if (is.character(metadata)) {
      ## Presume this is a file name pattern
      ## metadata_fn <- file.path(img_dir, "metadata.txt")

      metadata_fn <- list.files(path = img_dir,
                                pattern = metadata,
                                full.names = TRUE)

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

    ## Create a default name_short if needed
    if (is.null(metadata_use$name_short)) {
      metadata_use$name_short <- id_str
    } else {
      if (is.na(metadata_use$name_short)) {
        metadata_use$name_short <- id_str
      } else if (metadata_use$name_short == "") {
        metadata_use$name_short <- id_str
      }
    }

    ## If there's a value for collection_name (a field which is now deprecated but still exists in some
    ## older metadata.txt files), rename it name_long
    if (is.null(metadata_use$name_long) && !is.null(metadata_use$collection_name)) {
      names(metadata_use)[names(metadata_use) == "collection_name"] <- "name_short"
      warning(yellow("Metadata field `collection_name` has been deprecated. Moving forward please use `name_long`."))
    }

    ## Add to the result list
    res[[img_dir]] <- list(pts = imgs_ctr_utm_sf,
                           fp = fp_utm_sf,
                           nomap = nomap,
                           area_m2 = area_m2,
                           mcp = mcp_sf,
                           size_mb = total_size_mb,
                           date_flown = flight_date_str,
                           # camera_name = camera_name,
                           id = id_str,
                           metadata = metadata_use)
  }


  if (!quiet) message(green("All done"))

  ## Return the results
  class(res) <- c("list", "uas_info")
  res

}

