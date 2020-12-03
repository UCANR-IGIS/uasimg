#' Manage supplemental metadata fields
#'
#' Set and view a default set of supplemental metadata fields
#'
#' These functions allow you to define a default set of supplemental metadata fields that can be assigned
#' image collections either as a function argument, or a small text file placed
#' in the directory with the images. Note in this context 'supplemental metadata' refers to bits of information
#' that can not be automatically extracted from the images themselves (e.g., the name of the project
#' or pilot.)
#'
#' Creating a default set of supplemental metadata fields does not make them mandatory, nor does
#' it limit the metadata fields you can use. The default set serves as a template when
#' a new metadata.txt file is created.
#'
#' @seealso \code{\link{uas_getcache}}, \code{\link{uas_info}}
#' @describeIn uas_setflds Set default fields for image collection metadata.
#'
#' @param flds Character vector of metadata fields
#' @param reset Use a default set of metadata fields
#' @param quiet Suppress messages
#' @importFrom crayon yellow
#' @export

uas_setflds <- function(flds=NULL, reset=FALSE, quiet=FALSE) {

  ## If no fields were passed, use the default
  if (is.null(flds) || reset) {
    flds <- uas_flds_oem()
  }

  cache_dir <- uas_getcache(quiet = TRUE)

  ## If cache_dir is NA (never set), try to create it
  if (is.na(cache_dir)) {
    cache_dir <- uas_getcache(default = TRUE, quiet = TRUE)

    ## If cache_dir still doesn't exist, return an error
    if (is.na(cache_dir)) {
      stop("Can't find or create a cache directory. Use uas_setcache().")
    }

  }

  ## If cache_dir still doesn't exist, return an error
  if (!file.exists(cache_dir)) {
    stop("Can't find or create a cache directory. Use uas_setcache().")
  }

  save(flds, file = file.path(cache_dir, "flds_default.RData"))
  if (!quiet) message(yellow(" - Default metadata fields set to:", paste(flds, collapse = ", ")))
  invisible(flds)

}

#' @describeIn uas_setflds Get default supplemental metadata field names.
#' @export

uas_getflds <- function() {

  flds_oem <- uas_flds_oem()

  ## Get the cache directory
  cache_dir <- uas_getcache(quiet = TRUE)

  ## If cache_dir is NA (never set), try to create it
  if (is.na(cache_dir)) {
    cache_dir <- uas_getcache(default = TRUE, quiet = TRUE)
  }

  if (is.na(cache_dir)) {
    ## Unable to create a directory
    flds_oem

  } else {
    ## Look for a file called flds_default.RData in cache_dir
    metadata_fn <- file.path(cache_dir, "flds_default.RData")
    if (file.exists(metadata_fn)) {
      #cat("Found the meatafile file in cache dir \n")
      load(metadata_fn)
      flds

    } else {
      flds_oem
    }
  }

}

#' @describeIn uas_setflds Get a standard set of image collection metedata fields
#' @export

uas_flds_oem <- function() {
  c("name_short", "name_long", "description", "proj", "loc", "pilot", "contact", "uav", "data_url", "tags", "notes")
}
