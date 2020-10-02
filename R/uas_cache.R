#' Manage cache directory
#'
#' View and set the directory where extracted EXIF data are cached
#'
#' @param default Use a default cache directory if no other has been set
#' @param quiet Show messages, logical
#'
#' @details Extracting exif data from a large number of images can take awhile. To avoid having
#' to do this more than once, the results can be saved or cached to a directory of your choice.
#' The next time you call \code{\link{uas_info}}, R will first look to see if EXIF data for that
#' image collection has already been generated, and if so use it instead of running
#' exiftool again.
#'
#' Cached results are saved as native R objects. File names encode the name of the
#' image directory as well as the total number of images and file size. Hence if images are
#' removed or added from a directory, any cached results will be nullified and exiftool will
#' run again. The cached data does not include supplemental metadata, such as the
#' collection name or data URI.
#'
#' If \code{default = TRUE}, a default directory for the cache (\emph{~/.R/uasimg}) will be used
#' if another one has not already been set.
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

