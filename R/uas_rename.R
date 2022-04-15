#' Rename UAS images
#'
#' Rename UAS images
#'
#' @param x A list of class 'uas_info'
#' @param flt_idx Indices of x to change file names, integer
#' @param name_template A template for generating the file names (see Details)
#' @param all_lower Make file names all lowercase, Logical
#' @param preview Preview the new names only
#' @param confirm Confirm continue before changing file names
#'
#' @details This function will rename image files on disk based on a naming template which can include
#' placeholders (tokens) from image and/or flight metadata. This can be useful when you want
#' to rename your image files based on some formula of date-time parts, AGL altitude, flight metadata
#' fields, camera type, etc. Renaming image files can be helpful when you're doing analysis of individual images, but
#' is generally not needed when you're doing photogrammetry whereby you're more likely to be using directory
#' names to help you identify groups of images for stitching.
#'
#' \strong{Caution is advised} when using this function, because it will actually \strong{rename your files!} Use \code{preview = TRUE}
#' to test your naming template. When \code{preview = TRUE}, the function will return a tibble with the 'old' and 'new' names, but not actually change
#' any file names.
#'
#' When you're ready, set \code{preview = FALSE}. Aafter renaming files, you'll need to rerun
#' \code{\link{uas_info}} on the directory(s) to update the info.
#'
#' @section File Name Template:
#'
#' \code{name_template} should be a pattern containing placeholders (or 'tokens') in curly brackets. When you run the function, the tokens will be swapped out
#' for actual values.
#'
#' For example, if a filename is \emph{DJI_0213.JPG} and the name template was
#' \code{"img_{Y}-{m}-{d}_{H}-{M}-{S}"}, the file would be renamed something like
#' \emph{img_2018-06-17_13-04-46.jpg}, where the numbers for date and time are extracted from the
#' image EXIF data. Supported tokens you can use include:
#'
#' \itemize{
#'   \item \code{\{i\}} an integer starting from 1 and going up, padded with enough leading zeros to accommodate all values
#'   \item \code{\{Y\}} year (4-digit)
#'   \item \code{\{y\}} year (2-digit)
#'   \item \code{\{m\}} month (2-digit)
#'   \item \code{\{d\}} day (2-digit)
#'   \item \code{\{j\}} Julian day
#'   \item \code{\{H\}} hour (2-digits)
#'   \item \code{\{M\}} minutes (2-digits)
#'   \item \code{\{S\}} seconds (2-digits)
#'   \item \code{\{camera_abbrev\}} an abbreviated name of the camera
#'   \item \code{\{alt_agl\}} altitude above the launch point (usually in meters). You can indicate rounding by
#' adding a comma and the number of decimal places. For example \code{\{alt_agl,0\}} would round the AGL value to the nearest whole number.
#' This option is only available for images where the relative altitude is saved in the EXIF data (which excludes most multi-spectral images).
#' }
#'
#' In addition, name templates can include any flight metadata field that has been defined. For example if the flight metadata information
#' includes fields for \code{proj} (project abbreviation) and \code{loc} (location), the name template could include  \code{\{proj\}} and  \code{\{loc\}}.
#'
#' When creating your name template, remember:
#'
#' 1) All file names must come out unique. An easy way to ensure this is to include \code{\{i\}} in your name template, which will be replaced with a sequence of integers.
#'
#' 2) File names should not include characters that aren't allowed for file names. These include \code{< > : `"` / \\ | ? *}. If any of these characters are found in name_template, it will be rejected.
#'
#' 3) \code{name_template} should not contain an extension
#'
#' @return A tibble showing the old and new names for each image.
#'
#' @seealso \code{\link{uas_info}}, \code{\link{uas_metadata_make}}
#'
#' @importFrom stringr str_replace_all str_extract
#' @importFrom tools file_ext
#' @importFrom dplyr tibble
#' @importFrom crayon red
#'
#' @export

uas_rename <- function(x, flt_idx = NULL,
                       name_template = "img{i}_{alt_agl}m_{camera_abbrev}_{Y}_{m}_{d}_{H}_{M}_{S}",
                       all_lower = TRUE, preview = TRUE, confirm = TRUE) {

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  if (length(name_template) != 1) stop("name_template should be a character object with length 1")

  ## Make sure name_template doesn't contain an extension
  if (grepl("\\..{3}$", name_template)) stop("The name template should not contain an extension")

  ## Look for invalid characters
  if (grepl("<|>|:|/|\\\\|\\||\\?|\\*", name_template)) stop("name_template has one or more characters that are not allowed in file names")

  ## Verify that the value(s) in flt_idx (if any) are valid
  if (is.null(flt_idx)) {
    flt_idx_use <- 1:length(x)
  } else {
    if (TRUE %in% (flt_idx > length(x))) stop("Invalid value for flt_idx")
    flt_idx_use <- flt_idx
  }

  if (!preview && confirm) {
    cat(crayon::red("This will change the names of files. It can not be undone.\n"))
    ans <- readline(prompt = "Continue? y/n ")
    if (ans != "y") return(invisible(NULL))
  }

  ## Initialize the result
  res <- NULL

  for (i in flt_idx_use) {

    ## Get the old and new file names
    fn_old <- x[[i]]$pts$file_name
    fn_new <- rep(name_template, length(fn_old))

    ## Tack on the extensions
    fn_new <- paste0(fn_new, ".", file_ext(fn_old))

    ## Do all the date-time replacements

    ## Convert the character date times to POSIXct
    flt_imgs_dt <- as.POSIXct(x[[i]]$pts$date_time, format="%Y:%m:%d %H:%M:%S")

    ## Replace the {i} token with an appropriate amount of leading 0s
    if (grepl("\\{i\\}", name_template)) {
      num_imgs <- length(fn_new)
      num_digits <- floor(log10(num_imgs)) + 1
      fn_new <- str_replace_all(fn_new,
                                "\\{i\\}",
                                sprintf(paste0("%0", num_digits, "d"), 1:num_imgs))
    }

    ## Loop through the supported date-time tokens. If found in name_template, apply them
    for (search_token in c("Y", "y", "m", "d", "j", "H", "M", "S")) {
      if (grepl(paste0("\\{", search_token, "\\}"), name_template)) {
        fn_new <- str_replace_all(fn_new,
                                  paste0("\\{", search_token, "\\}"),
                                  format(flt_imgs_dt, paste0("%", search_token)) )
      }
    }

    ## Swap out tokens from CHARACTER columns in the pts attribute table
    for (search_token in c("camera_abbrev")) {
      if (grepl(paste0("\\{", search_token, "\\}"), name_template)) {
        fn_new <- str_replace_all(fn_new,
                                  paste0("\\{", search_token, "\\}"),
                                  x[[i]]$pts[[search_token]])
      }
    }

    ## Swap out tokens from NUMERIC columns in the pts attribute table
    ## These ones are allowed to have a ",1" at the end to indicate rounding
    for (search_token in c("alt_agl")) {
      if (grepl(paste0("\\{", search_token, "(.*?)\\}"), name_template)) {

        if (grepl(paste0("\\{", search_token, ",(.*?)\\}"), name_template)) {
          ## PUll out the 'roundto' value using a lookbehind and lookahead pattern
          roundto <- as.numeric(str_extract(name_template, paste0("(?<=\\{", search_token, ",).*?(?=\\})")))
          replace_vals <- round(x[[i]]$pts[[search_token]], roundto)
        } else {
          replace_vals <- x[[i]]$pts[[search_token]]
        }

        fn_new <- str_replace_all(fn_new,
                                  paste0("\\{", search_token, "(.*?)\\}"),
                                  as.character(replace_vals))
      }
    }

    ## Go through the flight metadata fields. If any of them are found as tokens in name_template,
    ## swap them out
    for (search_token in names(x[[i]]$metadata)) {
      if (grepl(paste0("\\{", search_token, "\\}"), name_template)) {
        fn_new <- str_replace_all(fn_new,
                                  paste0("\\{", search_token, "\\}"),
                                  x[[i]]$metadata[[search_token]])
      }
    }

    if (all_lower) fn_new <- tolower(fn_new)

    ## Add the old and new names to the result
    df <- tibble(dir = names(x)[i], fn_old = fn_old, fn_new = fn_new)
    res <- rbind(res, df)

    if (!preview) {
      if (FALSE %in% file.exists(x[[i]]$pts$img_fn)) stop("One or more input files not found. Can not proceed.")
      if (anyDuplicated(fn_new) > 0 ) stop("Duplicate output file names detected. Try to modify your name_template to get unique file names.")

      file.rename(from = x[[i]]$pts$img_fn,
                  to = file.path(dirname(x[[i]]$pts$img_fn), fn_new))
    }

  } ##  for (i in flt_idx_use)

  if (preview) {
    ## Return the tibble
    res

  } else {
    message("Filenames changed. Remember to re-run uas_info() to reflect the changes")
    ## Return invisible
    invisible(res)
  }

}

