#' Manage cache directory
#'
#' View and set the directory where extracted EXIF data are cached
#'
#' @param default Use a default cache directory if no other has been set
#' @param quiet Show messages, logical
#'
#' @details Extracting exif data from a large number of images can take awhile. To avoid having
#' to do this more than once for the same folder of images, the results can be saved (or cached) to
#' a local directory of your choice. When \code{cache = TRUE} in  \code{\link{uas_info}}, R will
#' first look to see if EXIF data has already been extracted for the image folder, and if
#' so use it instead of running exiftool.
#'
#' Cached results are saved as native R objects. Cache files store the EXIF data for a folder of
#' images, however if images are removed or added from a directory, any cached results will
#' be nullified and exiftool will run again. Cached EXIF data does not include supplemental flight metadata that you provide
#' with metadata text files (see \code{\link{uas_metadata_make}}, such as the pilot's name or
#' location name.
#'
#' \code{uas_getcache()} retrieves the current cache directory, and \code{uas_setcache()} sets it.
#' The default location is (\emph{~/.R/uasimg}). When \code{uas_setcache()} is run with
#' \code{write = TRUE}, the setting will be persistent across R sessions (generally recommended).
#' \code{uas_clearcache()} will delete cached EXIF data, which is sometimes called for after
#' a package update.
#'
#' @seealso \code{\link{uas_info}}
#'
#' @export

uas_getcache <- function(default=TRUE, quiet=FALSE) {

  cache_dir <- Sys.getenv("UASIMG_CACHE_DIR", unset = NA)

  if (is.na(cache_dir)) {

    ## No environment variable has been set (in .Renviron)
    if (default) {

      ## Use the default location (creating it if needed)
      default_dir <- "~/.R/uasimg"
      if (file.exists(default_dir)) {
        if (file.info(default_dir)$isdir) {
          ## We're done
          default_dir
        } else {
          ## Default dir exists, but is not a directory
          warning(paste0(default_dir, " exists but is not a directory"))
          NA
        }
      } else {
        ## Default dir doesn't exist, attempt to create it
        made_dir <- TRUE
        if (!file.exists("~/.R")) {
          made_dir <- made_dir && dir.create("~/.R")
        }
        if (made_dir && !file.exists("~/.R/uasimg")) {
          made_dir <- made_dir && dir.create("~/.R/uasimg")
        }

        ## Return the default dir or NA
        if (made_dir) {
          default_dir
        } else {
          NA
        }

      }

    } else {
      if (!quiet) {
        message("A cache directory for EXIF data has not been saved. Run uas_setcache.")
      }
      NA
    }


  } else {
    cache_dir
  }

}

#' @describeIn uas_getcache Set cache directory
#' @param dir The directory for  cached EXIF data (must exist)
#' @param write Write directory location to .Renviron
#' @param quiet Suppress messages
#' @export

uas_setcache <- function(dir = "~/.R/uasimg", write = FALSE, quiet = FALSE) {

  if (!file.exists(dir)) {
    ## If they passed the default, attempt to create it
    if (dir == "~/.R/uasimg") {
      made_dir <- TRUE
      if (!file.exists("~/.R")) {
        made_dir <- made_dir && dir.create("~/.R")
      }
      if (made_dir && !file.exists("~/.R/uasimg")) {
        made_dir <- made_dir && dir.create("~/.R/uasimg")
      }
      if (!made_dir) stop("Can not create default cache directory. Try passing a different location for dir.")
    }

    stop(paste0(dir, " does not exist. Please create it and try again."))
  }

  ## Save the cache directory as an environment variable
  Sys.setenv(UASIMG_CACHE_DIR = dir)

  if (write) {
    ## Grab the .Renviron file, creating it if needed
    environ_file <- file.path(Sys.getenv("HOME"), ".Renviron")
    if (!file.exists(environ_file)) {
      file.create(environ_file)
    }

    ## Read in .Renviron
    environ_lines <- readLines(environ_file)

    ## See if UASIMG_CACHE_DIR has already been saved
    cachedir_idx <- grep("^UASIMG_CACHE_DIR=", environ_lines)

    if (length(cachedir_idx) == 0) {
      if (!quiet) message(paste0("Adding directory to ", environ_file))
      environ_lines <- c(environ_lines, paste0("UASIMG_CACHE_DIR=", dir))
      writeLines(environ_lines, environ_file)

    } else {
      if (!quiet) message(paste0("Updating cache directory in ", environ_file))
      environ_lines[cachedir_idx] <- paste0("UASIMG_CACHE_DIR=", dir)
      writeLines(environ_lines, environ_file)
    }


  }

}


#' @describeIn uas_getcache Clear cache directory
#' @param quiet Suppress messages
#' @importFrom crayon green yellow
#' @export

uas_clearcache <- function(quiet = FALSE) {

  cache_fn <- list.files(path = uas_getcache(), pattern = "^uas_.*.RData$", full.names = TRUE)

  if (length(cache_fn) == 0) {
    if (!quiet) message(yellow("No EXIF cache files found"))

  } else {
    if (!quiet) {
      cont_yn <- readline(prompt = paste0("Delete EXIF cache files from ", uas_getcache(), "? [y/n] "))
      if (tolower(cont_yn) != "y") return(invisible(NULL))
    }

    unlink(cache_fn)
    if (!quiet) message(green(paste0(length(cache_fn), " cached EXIF files deleted")))
  }

}


