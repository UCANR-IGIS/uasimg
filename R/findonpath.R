#' findonpath
#'
#' Searches for a file on the system environment path
#'
#' @param fn The filename to search for (without a path)
#' @param status Show messages
#' @param quote Enclose returned filename in quotes
#'
#' @note
#' This will return the first found occurence of file \code{fn}, searching 1) the current working directory, 2) the user's R 'home' directory,
#' then 3) the directories on the operating system environment 'path'
#'
#' @return The full path and name of the found file, or NULL if not found
#'
#' @export

findonpath <- function(fn, status=TRUE, quote=TRUE) {

  if (file.exists(fn)) {
    if (.Platform$OS.type == "windows") {
      return(fn)
    } else {
      return(file.path(dirname(fn), basename(fn)))
    }
  }

  path <- Sys.getenv("PATH")
  if (path == "") {
    warning("Path environment variable not found")
    return(NULL)
  }

  if (.Platform$OS.type == "windows") {
    path.delim <- ";"
  } else {
    path.delim <- ":"
  }

  ## Generate path.dirs
  path.dirs <- c(path.expand("~"), strsplit(path, path.delim, fixed = TRUE)[[1]])
  path.dirs <- gsub("\\\\$", "", path.dirs)    ## replace trailing back slash with ""
  path.dirs <- gsub("\\\\", "/", path.dirs )   ## replace all back slash with forward slash

  fn.with.path <- file.path(path.dirs, fn)
  fn.with.path.exists <- which(file.exists(fn.with.path))
  if (length(fn.with.path.exists)==0) {
    if (status) message(crayon::red(fn, "not found anywhere on the path"))
    return(NULL)
  }

  fnReturn <- normalizePath(fn.with.path[fn.with.path.exists[1]])

  if (length(fn.with.path.exists) > 1 && status) {
    message(crayon::yellow(length(fn.with.path.exists), "occurences of", fn, "found on path"))
    message(crayon::yellow("Returning", fnReturn))
  }

  if (quote) fnReturn <- shQuote(fnReturn)

  fnReturn

}

