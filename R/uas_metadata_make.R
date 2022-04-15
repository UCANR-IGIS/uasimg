#' Create flight metadata text files
#'
#' Create and/or edit flight metadata text files
#'
#' @param x A character vector of directories, or a uas_info object
#' @param md_file Name of the file to create
#' @param md_suffix A suffix to append to the file names
#' @param md_template A template metadata file
#' @param md_flds A character vector of field names
#' @param make_new Create new metadata file(s), Logical
#' @param read_uasinfo Incorporate existing flight metadata already saved in x (assuming x is a uas_info object)
#' @param overwrite Overwrite existing metadata files, Logical
#' @param open Open the file for editing
#' @param use_system_editor Whether to open metadata files in the operating system text editor (as opposed to RStudio), Logical
#' @param quiet Suppress messages
#'
#' @details
#' This creates, and/or opens for editing, supplemental metadata text file(s) in image folders.
#'
#' If \code{make_new = TRUE}, metadata files will be created in each directory of \code{x}. \code{x}
#' can be either a character vector or a \code{\link{uas_info}} object. The files will be named based on the value of \code{md_file},
#' with an option to append a suffix \code{md_suffix}. Both \code{md_file}
#' and \code{md_suffix} should be of length 1 or equal to the number of \code{x}.
#'
#' If \code{open = TRUE}, the text file(s) will be opened. To open existing metadata text files, let \code{open = TRUE}
#' and \code{make_new = FALSE}. By default, files will be opened with whichever text editor is in use by
#' R (see \code{file.edit}). To use the system text editor, let \code{use_system_editor = TRUE}.
#'
#' To customize the fields that are added to flight metadata files, run \code{\link{uas_setflds}} first. To pre-populate some of the fields, pass
#' the path to an existing metadata file as the value for \code{md_template}. \code{md_template} can either be a local file or
#' online (\href{https://gist.githubusercontent.com/ajlyons/d0826f4775413ba27a21c62aff619bc2/raw/a6f3dde133652a729283b0e03f8a923f54f73a67/hrec_riverfire2020_metadata}{example}).
#'
#' @return A character vector of the external metadata text file(s) created.
#'
#' @seealso \code{\link{uas_getflds}}, \code{\link{uas_setflds}}
#'
#' @importFrom crayon red green bold yellow
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils file.edit

#' @export

# still to come - support a template with pre-populated values (maybe as a variable?)
#
# If \code{suffix = 'dir'}, the base name of the directory will be appended. Suffix
# can also be a literal string.

uas_metadata_make <- function(x, md_file = "metadata.txt", md_suffix = NULL,
                              make_new = FALSE, overwrite = FALSE, open = TRUE,
                              md_flds = uas_getflds(), md_template = NULL,
                              read_uasinfo = FALSE,
                              use_system_editor = FALSE,
                              quiet = FALSE) {

  ## Get the directories
  if (inherits(x, "uas_info")) {

    ## Check to see if any of the flights in this object span multiple directories
    for (i in 1:length(x)) {
      if (length(unique(dirname(x[[i]]$pts$img_fn))) > 1) {
        stop("Sorry, can't create flight metadata text files for flights whose images span multiple directories")
      }
    }

    dirs_use <- sapply(x, function(y) unique(dirname(y$pts$img_fn)))

  } else if (is.character(x)) {
    dirs_use <- x

  } else {
    stop("x should be a uas_info object or a character vector of directory paths.")
  }

  ## Verify at least one action was requested
  if (!make_new && !open) stop("`make_new` and `open` are both FALSE. Nothing to do.")

  if (!is.null(md_template)) {
    if (!grepl("^http", md_template, ignore.case = TRUE)) {
      if (!file.exists(md_template)) stop(paste0("File not found: ", md_template))
    }
  }

  ## Repeat 'md_file' if needed
  if (length(md_file) != length(dirs_use)) {
    if (length(md_file) != 1) stop("md_file must be length 1 or match the length of x")
    md_files_use <- rep(md_file[1], length(dirs_use))
  } else {
    md_files_use <- md_file
  }

  ## Repeat 'md_suffix' if needed
  if (is.null(md_suffix)) {
    md_suffix_use <- rep("", length(dirs_use))

  } else if (length(md_suffix) != length(dirs_use)) {
    if (length(md_suffix) != 1) stop("md_suffix must be length 1 or match the length of x")
    md_suffix_use <- rep(md_suffix[1], length(dirs_use))

  } else {
    md_suffix_use <- md_suffix
  }

  ## Generate individualized suffixes (NOT YET SUPPORTED)
  # if (md_suffix_use[i] == "dir") {
  #   ## Idea here is to generate a suffix based on name of the image folder (no longer think this is a good idea)
  #   message(red("suffix = 'dir' is not supported yet"))

  ## Generate complete file names
  md_fnames <- file.path(dirs_use,
                         paste0(file_path_sans_ext(md_files_use),
                                md_suffix_use,
                                ".",
                                file_ext(md_files_use)))

  ## Generate a metadata list with the fields in md_flds
  if (is.null(md_flds)) {
    flds_lst <- list()

  } else if (identical(md_flds, NA) || identical(md_flds, "") )  {
    flds_lst <- list()

  } else if (is.list(md_flds)) {
    flds_lst <- md_flds

  } else if (is.character(md_flds)) {
    flds_lst <- as.list(rep(NA, length(md_flds)))
    names(flds_lst) <- md_flds

  } else {
    stop("Unknown value for md_flds")

  }

  ## Create a variable to store any comment lines found (to preserve them)
  md_template_comments <- character(0)

  ## Read md_template and update the values in flds_lst
  if (!is.null(md_template)) {
    fcon <- file(md_template, open = "r")
    while ( TRUE ) {
      one_line <- readLines(fcon, n = 1, warn = FALSE)
      if ( length(one_line) == 0 ) break

      ## Determine if this line is comment
      first_char <- trimws(substr(one_line, 1, 1))
      if (first_char == "#" || first_char == "/") {
        md_template_comments <- c(md_template_comments, one_line)

      } else {

        ## Look for a colon
        colon_pos <- regexpr(":", one_line)
        if (colon_pos > 0) {

          ## Get the key and value
          ln_key <- trimws(substring(one_line, 1, colon_pos - 1)[1])
          ln_val <- gsub("\"", "'", trimws(substring(one_line, colon_pos + 1)[1]))

          ## Update the value of this key in flds_lst
          flds_lst[[ln_key]] <- ln_val
        }

      }

    }  ## while TRUE
    close(fcon)

  }    ## if not is.null(md_template)

  ## In any comments lines were found, paste them together to write to the output file
  if (length(md_template_comments) == 0) {
    md_comments_all <- ""
  } else {
    md_comments_all <- paste0(paste(md_template_comments, collapse = "\n"), "\n\n")
  }

  for (i in 1:length(dirs_use)) {

    md_fn <- md_fnames[i]

    ## Make the file if called for
    if (make_new) {
      if (file.exists(md_fn) && !overwrite) {
        if (!quiet) message(yellow(paste0(md_fn, " already exists. Skipping.")))

      } else {

        ## Create the boilerplate intro:
        descript_line <- paste0("## FLIGHT METADATA FOR:\n## ", md_fn, "\n##\n",
                                "## Tips: \n",
                                "## Blank lines and comments (start with #) are ignored \n",
                                "## `name_short` is used to generate default file names. Keep it short and avoid special characters. \n",
                                "## `name_long` will be used as a title for the HTML summary page \n",
                                "## `proj` and 'loc' may be used to construct directory trees, so abbreviations are generally recommended \n\n")

        ## Make a copy of the generic list
        flst <- flds_lst

        ## If x is a uas_info object, Update any values in x[[ ]]$metadata (especially name_short)
        if (inherits(x, "uas_info")) {
          for (fldname in names(flst)) {
            if (fldname %in% names(x[[i]]$metadata)) {
              if (read_uasinfo || fldname == "name_short") {
                flst[[fldname]] <- x[[i]]$metadata[[fldname]]
              }
            }
          }
        }

        flds_yaml <- paste(sapply(1:length(flst), function(j) paste0(names(flst)[j], ": ", flst[[j]]) ), collapse = "\n\n")

        ## Write the file
        cat(c(descript_line, md_comments_all, flds_yaml), file = md_fn, sep="")

        if (!quiet) message(green(paste0(" - created ", md_fn)))

      }

    } else {
      if (!file.exists(md_fn)) {
        if (!quiet) message(red(paste0(" - Can not open ", md_fn, " because it doesn't exist. Try again with `make_new = TRUE`")))
      }
    }

    if (open && file.exists(md_fn)) {
      ## Open the metadata.txt file if needed

      if (use_system_editor) {
        ## Use the default text editor
        shell.exec(normalizePath(md_fn))
      } else {
        ## Use whichever text editor is defined by RStudio's getOption("editor")
        file.edit(normalizePath(md_fn))
      }
    }

  }

  md_fnames

}
