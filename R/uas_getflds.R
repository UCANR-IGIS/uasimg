#' Manage Default Metadata fields
#'
#' View and set default metadata fields
#'
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

#' Set default metadata fields
#'
#' @describeIn uas_getflds
#' @param flds Name of metadata fields
#' @param reset Use orginal values
#' @param quiet Show messages, logical

uas_setflds <- function(flds=NULL, reset=FALSE) {

  ## If no fields were poassed, use the default
  if (is.null(flds)) {
    flds <- uas_flds_oem()
  }

  ## Get the cache directory
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


}

uas_flds_oem <- function() {
  c("proj", "loc", "collection_name", "description", "contact", "pilot", "data_url", "tags")
}
