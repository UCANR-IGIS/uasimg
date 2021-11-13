#' Generate a Table of Contents of flight summaries
#'
#' @param html_reports HTML file names of flight summaries
#' @param output_dir Output directory
#' @param output_fn Output file name
#' @param gather_dir Subdirectory of output_dir where HTML files will be copied
#' @param toc_title Title to show at the top of the Table of Contents
#' @param toc_desc A short description to appear under the title
#' @param fltmap_show Show a map of all flight areas, logical
#' @param fltmap_kml Create a KML of all flight areas, logical
#' @param fltmap_base A list object containing of background KML files and their symbology for the flight map, see Details.
#' @param header_html HTML file name or URL to use as a page header
#' @param footer_html HTML file name or URL to use as a page footer
#' @param overwrite_toc Overwrite existing file, logical
#' @param overwrite_gather Subdirectory of output_dir where HTML files will be copied
#' @param open_toc Open the table of contents in a browser when done, logical
#' @param quiet Suppress messages, logical
#'
#' @details This function generates a master Table of Contents HTML page for a set
#' of individual flight summaries created by \code{\link{uas_report}}.
#'
#' \code{htmls_reports} should be a vector of HTML filenames, including the full path. Reports will
#' be appear in the Table of Contents in the same order. To link to the reports, R will attempt
#' to create relative paths from the \code{output_dir} to the locations
#' of the \code{htmls_reports}. At a minimum, this requires \code{output_dir} and
#' \code{htmls_reports} to be on the same volume. Preferably \code{output_dir} will be a parent
#' directory of \code{htmls_reports}. If your HTML reports are scattered across many directories,
#' consider using \code{gather_dir} which will put them all in one place.
#'
#' \code{gather_dir} specifies an optional \emph{sub-directory} of \code{output_dir} where
#' the HTML files in \code{htmls_reports} (and any associated files such as thumbnails) will be copied.
#' \code{gather_dir} should be relative to \code{output_dir} (not an absolute path). To copy HTML files
#' in \code{output_dir} itself (i.e., not a sub-directory), set \code{gather_dir = '.'}
#' Links in the TOC will then point to the copies of the HTML files in \code{gather_dir}.
#' If \code{gather_dir} does not already exist, R will attempt to create it. To link to the
#' HTML files where they currently are (i.e., not make copies), let \code{gather_dir = NULL} (the default).
#'
#' If \code{fltmap_show = TRUE}, the Table of Contents will include an interactive map showing the flight
#' areas of all the flight summaries on the page. \code{fltmap_base} is an optional list
#' of lists you can use to include additional base layers. Currently only polygon and polyline KML files are
#' supported as additional base layers. Each list element should be a named list with three
#' elements: \emph{kml_fn} (a KML file name), \emph{color} (a named color or hex code for the outline),
#' and \emph{weight} (outline thickness in pixels).
#'
#' \code{header_html} and \code{footer_html} allow you to specify a page header and footer, i.e., to
#' add branding elements to the TOC.
#'
#' @seealso \code{\link{uas_report}}
#'
#' @importFrom crayon green
#' @importFrom rmarkdown render
#' @importFrom utils browseURL
#' @importFrom xml2 read_html xml_find_first xml_attr
#' @importFrom sf st_read
#' @importFrom tools file_path_sans_ext
#' @export

uas_toc <- function(html_reports, output_dir = ".", output_fn = "index.html",
                    gather_dir = NULL, toc_title = "UAS Flight Summaries",
                    toc_desc = NULL, fltmap_show = TRUE, fltmap_kml = FALSE, fltmap_base = NULL,
                    header_html = NULL, footer_html = NULL,
                    overwrite_toc = FALSE, overwrite_gather = FALSE, open_toc = FALSE, quiet = FALSE) {

  if (!file.exists(output_dir)) stop("output_dir does not exist")

  ## See if the HTML report output filename already exists
  if (file.exists(file.path(output_dir, output_fn)) && !overwrite_toc) stop(paste0(output_fn, " already exists. Use another output_fn, or set overwrite_toc = TRUE. Quitting."))

  # if (!is.null(footer_html)) {
  #   if (!file.exists(footer_html)) stop(paste0(footer_html, "not found"))
  # }
  #
  # if (!is.null(header_html)) {
  #   if (!file.exists(header_html)) stop(paste0(header_html, "not found"))
  # }

  if (FALSE %in% file.exists(html_reports)) {
    stop(paste0("File(s) not found: ", paste(html_reports[!file.exists(html_reports)], collapse = ", ")))
  }

  if (!is.null(gather_dir)) {
    ## If gather ends with '/', remove it
    if (substr(gather_dir, nchar(gather_dir), nchar(gather_dir)) == "/") {
      gather_dir <- substr(gather_dir, 1, nchar(gather_dir) - 1)
    }

    if (grepl("^(/|[A-Za-z]:|\\\\|~)", gather_dir)) {
      stop("gather_dir should be relative to output_dir, not an absolute path")
    }

    if (gather_dir == "." || gather_dir == "") {
      gather_dir <- output_dir
    } else {
      gather_dir <- file.path(output_dir, gather_dir)
    }

    if (!file.exists(gather_dir)) {
      if (!dir.create(gather_dir, recursive = TRUE)) stop(paste0("Can't create ", gather_dir, ". Specify a different output_dir or omit gather_dir."))
    }

    html_gathered_full <- NULL
    html_gathered_base <- NULL
    num_dups <- 0

    if (!quiet) message(yellow(paste0(" - copying files to ", gather_dir)))
    for (fn in html_reports) {

      if (file.exists(fn)) {

        if (!quiet) message(yellow(paste0(" - ", basename(fn))))

        ## Check for a duplicate HTML file name, and if found append a number to make it unique
        if (basename(fn) %in% html_gathered_base) {
          num_dups <- num_dups + 1
          dest_fn <- file.path(gather_dir,
                              paste0(file_path_sans_ext(basename(fn)),
                              "_", num_dups, ".", file_ext(fn)))
        } else {
          dest_fn <- file.path(gather_dir, basename(fn))
        }

        if (!file.exists(dest_fn) || overwrite_gather) {
          file.copy(from = fn, to = dest_fn, overwrite = overwrite_toc)
        }

        html_gathered_full <- c(html_gathered_full, dest_fn)
        html_gathered_base <- c(html_gathered_base, basename(dest_fn))

        ## Get the map_fn
        html_root <- read_html(dest_fn)

        ## Grab the PNG filename (map_fn) which is encoded in a meta tag
        map_fn <- html_root %>%
          xml_find_first("//meta[@name='map_fn']") %>%
          xml_attr("content") %>%
          trimws()

        ## If a filename was found, copy it to the gather directory
        if (!is.na(map_fn)) {
          src_map_fn <- file.path(dirname(fn), map_fn)
          if (file.exists(src_map_fn)) {
            dest_map_fn <- file.path(gather_dir, map_fn)
            if (!file.exists(dest_map_fn) || overwrite_gather) {
              file.copy(from = src_map_fn, to = dest_map_fn, overwrite = overwrite_gather)
            }
          }
        }

        ## In a similar manner, copy the MCP and ctr KML files
        kml_mcp_fn <- html_root %>%
          xml_find_first("//meta[@name='kml_mcp_fn']") %>%
          xml_attr("content") %>%
          trimws()

        if (!is.na(kml_mcp_fn)) {
          src_kml_mcp_fn <- file.path(dirname(fn), kml_mcp_fn)
          if (file.exists(src_kml_mcp_fn)) {
            dest_kml_mcp_fn <- file.path(gather_dir, kml_mcp_fn)
            if (!file.exists(dest_kml_mcp_fn) || overwrite_gather) {
              file.copy(from = src_kml_mcp_fn, to = dest_kml_mcp_fn, overwrite = overwrite_gather)
            }
          }
        }

        kml_ctr_fn <- html_root %>%
          xml_find_first("//meta[@name='kml_ctr_fn']") %>%
          xml_attr("content") %>%
          trimws()
        if (!is.na(kml_ctr_fn)) {
          src_kml_ctr_fn <- file.path(dirname(fn), kml_ctr_fn)
          if (file.exists(src_kml_ctr_fn)) {
            dest_kml_ctr_fn <- file.path(gather_dir, kml_ctr_fn)
            if (!file.exists(dest_kml_ctr_fn) || overwrite_gather) {
              file.copy(from = src_kml_ctr_fn, to = dest_kml_ctr_fn, overwrite = overwrite_gather)
            }
          }
        }

        ## Copy uas_report.css if found
        css_fn <- file.path(dirname(fn), "uas_report.css")
        if (file.exists(css_fn)) {
          file.copy(from = css_fn,
                    to = gather_dir,
                    overwrite = overwrite_gather)
        }

        ## Next, we need to copy the thumbnails folder
        tbsrc_dir <- file.path(dirname(fn), "tb")
        if (file.exists(tbsrc_dir)) {
          ## Get a list of thumbnail files
          tbsrc_fn <- list.files(path = tbsrc_dir, full.names = FALSE)
          ## Get the destination folder, create it if needed
          tbdest_dir <- file.path(gather_dir, "tb")
          if (!file.exists(tbdest_dir)) dir.create(tbdest_dir, recursive = TRUE)
          file.copy(from = file.path(tbsrc_dir, tbsrc_fn),
                    to = tbdest_dir,
                    overwrite = overwrite_gather)
        }

        ## Copy the xxxx_files directory if found
        files_dir <- paste0(file_path_sans_ext(fn), "_files")
        if (file.exists(files_dir)) {
          file.copy(from = files_dir,
                    to = gather_dir,
                    recursive = TRUE,
                    overwrite = overwrite_gather)
        }

        ## Finally, copy the libs directory if found
        libs_dir <- file.path(dirname(fn), "libs")
        if (file.exists(libs_dir)) {
          file.copy(from = libs_dir,
                    to = gather_dir,
                    recursive = TRUE,
                    overwrite = overwrite_gather)
        }

      } else {
        if (!quiet) message(red(paste0(" - ", basename(fn), " does not exist, skipping")))
      }

    }


    #if (!quiet) message(green("HTML files gathered:\n", paste("  ", html_gathered_full, sep = "", collapse = "\n"), "\n", sep = ""))

    html_reports <- html_gathered_full

  }

  bg_lst <- list()
  if (!is.null(fltmap_base)) {
    if (!is.list(fltmap_base)) stop("fltmap_base should be a list of lists")
    for (i in 1:length(fltmap_base)) {
      if (!is.list(fltmap_base[[i]])) stop("fltmap_base should be a list of lists")
      kml_fn <- fltmap_base[[i]]$kml_fn
      if (is.null(kml_fn)) stop("Each lists in fltmap_base should contain an element called `kml_fn`")
      if (!file.exists(kml_fn)) stop ("Background KML layer(s) not found")
      bg_lst[[i]] <- list(kml_sf = st_read(kml_fn),
                          color = ifelse(is.null(fltmap_base[[i]]$color), "red", fltmap_base[[i]]$color),
                          weight = ifelse(is.null(fltmap_base[[i]]$weight), 3, fltmap_base[[i]]$weight))
    }
  }

  ## Get the Rmd file
  toc_rmd <- system.file("report/uas_toc.Rmd", package="uasimg")

  ## Create a list of includes if header or footer are passed
  if (!is.null(footer_html) || !is.null(header_html)) {

    if (is.null(header_html)) {
      header_html_fn <- NULL
    } else {
      if (grepl("^http", header_html, ignore.case = TRUE)) {
        header_html_fn <- tempfile("uas_header_", fileext = ".html")
        download_successful <- download.file(url = header_html, destfile = header_html_fn)
        if (download_successful != 0) stop("Could not download the header URL")
        on.exit(unlink(header_html_fn))
      } else {
        if (!file.exists(header_html)) stop(paste0(header_html, "not found"))
        header_html_fn <- header_html
      }
    }

    if (is.null(footer_html)) {
      footer_html_fn <- NULL
    } else {
      if (grepl("^http", footer_html, ignore.case = TRUE)) {
        footer_html_fn <- tempfile("uas_footer_", fileext = ".html")
        download_successful <- download.file(url = footer_html, destfile = footer_html_fn)
        if (download_successful != 0) stop("Could not download the footer URL")
        on.exit(unlink(footer_html_fn))
      } else {
        if (!file.exists(footer_html)) stop(paste0(footer_html, "not found"))
        footer_html_fn <- footer_html
      }
    }

    includes_lst <- list(before_body = header_html_fn, after_body = footer_html_fn)
  } else {
    includes_lst <- NULL
  }

  ## Package up all the output options
  output_options <- list(self_contained = FALSE, lib_dir = file.path(normalizePath(output_dir),"libs"),
                         includes = includes_lst)

  toc_fn <- render(input = toc_rmd, output_dir = normalizePath(output_dir), output_file = output_fn,
                   output_options = output_options,
                   params=list(output_dir = output_dir,
                               output_fn = output_fn,
                               html_reports = html_reports,
                               toc_title = toc_title,
                               toc_desc = toc_desc,
                               fltmap_show = fltmap_show,
                               fltmap_kml = fltmap_kml,
                               bg_lst = bg_lst))

  if (!quiet) message(green("Done."))

  if (open_toc) browseURL(toc_fn)

  invisible(toc_fn)

}
