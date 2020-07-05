#' Generate a table of contents for image collection reports
#'
#' @param html_reports File names with path of HTML Image Collection summaries
#' @param output_dir Output directory
#' @param output_fn Output file name
#' @param gather Subdirectory where HTML files will be copied
#' @param toc_title Title to use for the Table of Contents
#' @param header_html HTML file name to use as a page header
#' @param footer_html HTML file name to use as a page footer
#' @param overwrite Overwrite existing file, logical
#' @param open_toc Open the table of contents in a browser when done, logical
#' @param quiet Suppress messages, logical
#'
#' @details This function generates a HTML master table of contents for a set
#' of HTML UAS Image Collection reports created by \code{\link{uas_report}}.
#'
#' \code{htmls_reports} should be a vector of HTML filenames, including the full path. Reports will
#' be appear in the Table of Contents in the same order. To link to the reports, R will attempt
#' to create relative paths from the \code{output_dir} to the locations
#' of the \code{htmls_reports}. At a minimum, this requires \code{output_dir} and
#' \code{htmls_reports} to be on the same volume. Preferably \code{output_dir} will be a parent
#' directory of \code{htmls_reports}. If your HTML reports are scattered across many directories,
#' consider using \code{gather} which will put them all in one place.
#'
#' \code{gather} specifies an optional sub-directory of \code{output_dir} where the HTML files
#' in \code{htmls_reports} (and any associated PNG files) will be copied. Links in the TOC
#' will then point to the copies of the HTML files in \code{gather}. If the \code{gather} sub-directory
#' does not already exist, R will attempt to create it.
#'
#' \code{header_html} and \code{footer_html} allow you to specify a page header and footer, i.e., to
#' add branding elements to the TOC.
#'
#' @seealso \code{\link{uas_report}}
#'
#' @importFrom crayon green
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @importFrom XML htmlTreeParse
#' @export

uas_toc <- function(html_reports, output_dir = ".", output_fn = "index.html",
                    gather = NULL, toc_title = "UAS Image Collections",
                    header_html = NULL, footer_html = NULL,
                    overwrite = FALSE, open_toc = FALSE, quiet = FALSE) {

  ## Get the HTML report output filename
  output_fn_use <- file.path(output_dir, output_fn)

  if (file.exists(output_fn_use) && !overwrite) stop(paste0(output_fn, " already exists. Use another output_fn, or set overwrite = TRUE. Quitting."))

  if (!is.null(footer_html)) {
    if (!file.exists(footer_html)) stop(paste0(footer_html, "not found"))
  }

  if (!is.null(header_html)) {
    if (!file.exists(header_html)) stop(paste0(header_html, "not found"))
  }

  if (!is.null(gather)) {
    gather_dir <- file.path(output_dir, gather)
    if (!file.exists(gather_dir)) {
      if (!dir.create(gather_dir)) stop(paste0("Can't create ", gather_dir, ". Specify a different output_dir or omit gather."))
    }

    html_gathered <- NULL
    for (fn in html_reports) {
      if (file.exists(fn)) {
        dest_fn <- file.path(gather_dir, basename(fn))
        if (file.copy(from = fn, to = dest_fn, overwrite = overwrite)) {
          html_gathered <- c(html_gathered, dest_fn)

          ## Next, copy the PNG file if found

          ## First, parse the HTML page
          html_tree <- htmlTreeParse(readLines(dest_fn), useInternalNodes = TRUE)

          ## Grab the map_fn which is encoded in a meta tag
          map_fn_meta <- html_tree[paste0("//meta[@name='map_fn']/@content")]
          map_fn <- trimws(as.character(unlist(map_fn_meta)))
          if (length(map_fn) == 1) {
            src_map_fn <- file.path(dirname(fn), map_fn)
            if (file.exists(src_map_fn)) {
              dest_map_fn <- file.path(gather_dir, map_fn)
              file.copy(from = src_map_fn, to = dest_map_fn, overwrite = overwrite)
            }
          }
        }

      }


    }

    if (!quiet) {
      message(green("Files gathered", "\n"))
      print(html_gathered)
    }
    html_reports <- html_gathered

  }

  ## Get the Rmd file
  toc_rmd <- system.file("report/uas_toc.Rmd", package="uasimg")

  ## Create a list of output options that specify not self contained
  if (!is.null(footer_html) || !is.null(header_html)) {
    includes_lst <- list(before_body = header_html, after_body = footer_html)
  } else {
    includes_lst <- NULL
  }

  output_options <- list(self_contained=FALSE, lib_dir = file.path(output_dir,"libs"),
                         includes = includes_lst)

  toc_fn <- render(input=toc_rmd, output_dir = output_dir, output_file = output_fn,
                   output_options = output_options,
                   params=list(html_reports = html_reports, output_dir = output_dir,
                               toc_title = toc_title))

  if (!quiet) message(green("Done."))

  if (open_toc) browseURL(toc_fn)

}
