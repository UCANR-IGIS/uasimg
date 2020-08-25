#' Create a blank metadata text file
#'
#' Create a blank metadata text file
#'
#' @param dirs A character vector of directories or a uas_info object
#' @param file Name of the file to create
#' @param suffix A suffix to append to the file names
#' @param flds A character vector of field names
#' @param overwrite Overwrite existing metadata files, logical
#' @param open Open the file for editing
#' @param quiet Suppress messages
#'
#' @details
#' This creates blank external metadata text file(s). One file will be placed in each
#' directory in \code{dirs} (which can be either a character vector or a
#' \code{\link{uas_info}} object. The files will be named based on the value of \code{file}
#' which will be repeated if needed, with an option to append a suffix (which can make it
#' easier to edit them in a text editor). If \code{suffix = 'dir'}, the base name of the
#' directory will be appended. Suffix can also be a literal string. Both \code{file}
#' and \code{suffix} should be of length 1 or equal to the number of \code{dirs}.
#'
#' @return A character vector of the external metadata text file(s) created.
#' @importFrom crayon red green bold yellow
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils file.edit

#' @export

# still to come - support a template with pre-populated values (maybe as a variable?)

uas_metadata_make <- function(dirs, file = "metadata.txt", suffix = c("", "dir")[1],
                              flds = uas_getflds(),
                              overwrite = FALSE, open = FALSE, quiet = FALSE) {

  if (inherits(dirs, "uas_info")) {
    dirs_use <- names(dirs)
  } else if (is.character(dirs)) {
    dirs_use <- dirs
  } else {
    stop("dirs should be a uas_info object or a character vector of directory paths.")
  }

  ## Repeat 'file' if needed
  if (length(file) != length(dirs_use)) {
    if (length(file) != 1) stop("file must be length 1 or match the length of dirs")
    files_use <- rep(file[1], length(dirs_use))
  } else {
    files_use <- file
  }

  ## Repeat 'suffix' if needed
  if (length(suffix) != length(dirs_use)) {
    if (length(suffix) != 1) stop("suffix must be length 1 or match the length of dirs")
    suffix_use <- rep(suffix[1], length(dirs_use))
  } else {
    suffix_use <- suffix
  }

  for (i in 1:length(dirs_use)) {

    md_fname <- files_use[i]

    if (suffix == "dir") {
      ##browser()
      message(red("suffix = 'dir' is not supported yet"))

    } else if (suffix_use[i] != "") {
      ## Assume that suffix is a literal string
      md_fname <- paste0(file_path_sans_ext(md_fname),
                         suffix_use[i], ".", file_ext(md_fname))
    }

    md_fn <- file.path(dirs_use[i], md_fname)

    make_file <- TRUE
    if (file.exists(md_fn) && !overwrite) {
      make_file <- FALSE
      message(red(paste0(basename(md_fn), " exists & overwrite = FALSE. Skipping.")))
    }

    if (make_file) {
      ## Generate the contents of the YAML file
      descript_line <- paste0("## ", md_fn, "\n\n")
      flds_yaml <- paste(flds, ": \n", sep="")

      ## Create and open the file
      cat(c(descript_line, flds_yaml), file = md_fn, sep="")
      if (open) file.edit(md_fn)

      if (!quiet) message(green(paste0(" - created ", md_fn)))
    }


  }


}
