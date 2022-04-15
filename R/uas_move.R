#' Move UAS images into sub-directories
#'
#' Move UAS images into sub-directories
#'
#' @param x A list of class 'uas_info'
#' @param flt_idx Elements of x to move, integer
#' @param tree Directory tree template filename or character vector, see Details
#' @param outdir_base Output directory root
#' @param req_all_fltmdflds Require all flight metadata fields in the directory tree template to be defined
#' @param create_dirs Create the output directory tree
#' @param imgs_action The action to take with images
#' @param imgs_prepend_fn Whether th prepend image file names with datestamp (to ensure they'll be unique)
#' @param write_metadata Write a metadtata.txt file in the output image folder
#' @param preview_only Preview the directory tree only
#' @param tb_action The action to take with thumbnail images saved in the default location
#' @param map_action The action to take with the contents of the map folder
#' @param quiet Suppress messages
#'
#' @details
#' req_all_fltmdflds means don't move anything unless all uas_info objects have all tokens in the directory tree
#'
#' @return A vector of directory locations where images were copied / moved
#'
#' @seealso \code{\link{uas_info}}
#'
#' @importFrom stringr str_extract_all str_replace
#' @importFrom sf st_drop_geometry
#' @importFrom dplyr mutate select
#' @importFrom crayon yellow green red bold
#'
#' @export

uas_move <- function(x,
                     flt_idx = NULL,
                     tree,
                     outdir_base,
                     req_all_fltmdflds = TRUE,
                     create_dirs = "ask",
                     imgs_action = c("copy", "move", "none")[1],
                     imgs_prepend_fn = FALSE,
                     write_metadata = TRUE,
                     preview_only = FALSE,
                     tb_action = imgs_action,
                     map_action = c("copy", "move", "none")[3],
                     quiet = FALSE) {

  ## If there are multiple metadata files going to one place, that's an issue
  ## maybe give them different file names

  ## Do we really want to copy the map folder? I suppose so. Risk is that local_dir will be
  ## out of date.

  debug <- FALSE

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  if (!imgs_action %in% c("copy", "move", "none")) stop("Unknown value for imgs_action")

  ## Chop off any trailing slashes from outdir_base
  outdir_base_use <-  gsub("\\\\$|/$", "", outdir_base)
  if (!file.exists(outdir_base)) stop(paste0("Base directory not found: ", outdir_base))

  ## Verify that that value(s) in flt_idx (if any) are valid
  if (is.null(flt_idx)) {
    flt_idx_use <- 1:length(x)
  } else {
    if (TRUE %in% (flt_idx > length(x))) stop("Invalid value for flt_idx")
    flt_idx_use <- flt_idx
  }

  ## Load the tree template
  if (length(tree) == 1) {
    ## We assume tree is a file name
    if (!file.exists(tree)) stop(paste0("File not found: ", tree))
    tree_input <- readLines(tree, warn = FALSE)
  } else {
    # We assume tree is a character vector
    tree_input <- tree
  }

  ## Clean the tree template; make a list of tokens
  tree_use <- character(0)
  tokens <- character(0)
  #dir_fltimg_idx <- NULL
  #dir_nofltimg_idx <- NULL

  for (tree_ln in tree_input) {
    ## First trim any white space
    tree_ln_clean <- trimws(tree_ln)

    ## Next, make sure it isn't a comment
    if (!grepl("^#|^;", tree_ln_clean)) {

      ## Next, make sure it isn't a blank line
      if (tree_ln_clean != "") {

        ## Check if the line ends with **
        # put_fltimgs_here <- grepl("\\*\\*$", tree_ln_clean)
        # if (put_fltimgs_here) {
        #   tree_ln_clean <- trimws(gsub("\\*\\*$", "", tree_ln_clean))
        # }

        # ## Check if the line ends with --
        # put_nofltimgs_here <- grepl("--$", tree_ln_clean)
        # if (put_nofltimgs_here) {
        #     tree_ln_clean <- trimws(gsub("--$", "", tree_ln_clean))
        # }

        ## Extract all the tokens and add them to the master vector
        tokens <- unique(c(tokens,
                         str_extract_all(tree_ln_clean, "(?<=\\{).+?(?=\\})")[[1]]))

        ## Add this line to tree use
        tree_use <- c(tree_use, tree_ln_clean)

        ## Record the index if this is where flight images go
        #if (put_fltimgs_here) dir_fltimg_idx <- c(dir_fltimg_idx, length(tree_use))

        ## Record the index if this is where flight images go
        # if (put_nofltimgs_here) dir_nofltimg_idx <- c(dir_nofltimg_idx, length(tree_use))

      } # if (tree_ln_clean != "") {
    }
  }

  ## Check to see if one and only one directory was designated for flight images
  # if (length(dir_fltimg_idx) != 1) {
  #   stop("Error in tree: One and only one directory must be designated as the destination for flight images. See help.")
  # }

  ## if (length(dir_nofltimg_idx) != 1) stop("Error in tree: One and only one directory must be designated as the destination for non-flight images. See help.")

  ## Next, we need to see if all the flights have all the required metadata fields

  if (req_all_fltmdflds) {
    ## Verify that fligth metadata exists for all the required tokens (excluding those which will be generated
    ## automatically)

    tokens_check <- tokens[!tokens %in% c("flt_start", "flt_end", "flt_date", "flt_agl",
                                          "camera_name", "camera_abbrev",
                                          "filetype", "make", "model")]
    for (flt_name in names(x)) {
      if (FALSE %in% (tokens_check %in% names(ucm_flts_info[[flt_name]]$metadata))) {
          stop(paste0("Required metadata field(s) not found for `", flt_name, "`: ",
                      paste(tokens_check[!tokens_check %in% names(ucm_flts_info[[flt_name]]$metadata)],
                            collapse = ", " )))
      }
    }
  }

  img_tokens <- c("camera_name", "camera_abbrev", "filetype", "make", "model")
  img_token_present <- (TRUE %in% (img_tokens %in% tokens))

  if (debug) {
      cat("Tree Template: \n")
      cat(paste0("  ", tree_use, collapse = "\n"), "\n")
      #cat("Image dir: ", dir_fltimg_idx, "\n\n")
  }

  res <- NULL

  ## We've verified all tokens are available.

  for (i in flt_idx_use) {

    flt_tokens <- list()

    ## Gather all computed flight-tokens
    ## flt_start", "flt_end", "flt_date", "flt_agl""
    flt_imgs_dt <- as.POSIXct(x[[i]]$pts$date_time, format="%Y:%m:%d %H:%M:%S")
    if (NA %in% flt_imgs_dt) stop("Unable to parse image timestamps")
    flt_tokens[["flt_date"]] <- format(min(flt_imgs_dt), "%Y-%m-%d")
    flt_tokens[["flt_start"]] <- format(min(flt_imgs_dt), "%H%M")
    flt_tokens[["flt_end"]] <- format(max(flt_imgs_dt), "%H%M")

    ## Add all metadata flight token
    flt_tokens <- c(flt_tokens,
                    x[[i]]$metadata)

    ## Expand the directories needed for this flight
    flt_tree <- tree_use

    ## For every token in the tree, if we have a flight token
    ## with the same name, make the replacement
    for (my_token in tokens) {
      if (my_token %in% names(flt_tokens)) {
        flt_tree <- gsub(paste0("\\{", my_token, "\\}"), flt_tokens[[my_token]], flt_tree)
      }
    }

    ## Next, if any of the field names in $pts are tokens, make the replacements
    ## This could result in duplicate lines in the tree if the attribute column contains
    ## more than one unique value

    flt_img_tree <- NULL
    for (my_token in tokens) {
      if (my_token %in% names(x[[i]]$pts)) {
        for (replacement_val in unique(x[[i]]$pts[[my_token]])) {
          flt_img_tree <- c(flt_img_tree,
                            gsub(paste0("\\{", my_token, "\\}"), replacement_val, flt_tree))
        }
      }
    }

    ## Construct the final flight tree
    if (is.null(flt_img_tree)) {
      ## No substitutions were made
      flt_tree_final <- flt_tree
    } else {
      flt_tree_final <- unique(flt_img_tree)
    }

    # if (debug) {
    #   cat("\n", names(x)[i], ":\n", sep = "")
    #   cat("Flight tree final: \n")
    #   cat(paste0("  ", flt_tree_final, collapse = "\n"), "\n")
    # }

    ## Now we're ready to create the directories
    dirs_needed <- normalizePath(file.path(outdir_base, flt_tree_final), mustWork = FALSE)
    dir_missing <- !file.exists(dirs_needed)

    # if (debug) {
    #   cat("\n There are the dirs needed\n")
    #   cat(paste0("  ", dirs_needed, collapse = "\n"), "\n")
    # }

    #if (preview_only || identical(create_dirs, "ask")) {
    if (!quiet) {
      dir_asterik <- rep("", length(dirs_needed))
      dir_asterik[dir_missing] <- crayon::yellow(" **")
      message(crayon::yellow("\n", names(x)[i]), sep = "")
      message(" - output directory tree for this flight:")
      message(paste0("   - ", dirs_needed, dir_asterik, collapse = "\n"))
    }

    if (!preview_only && (TRUE %in% dir_missing)) {

      if (identical(create_dirs, TRUE)) {
        make_dir_yn <- TRUE
      } else if (identical(create_dirs, FALSE)) {
        make_dir_yn <- FALSE
      } else if (identical(create_dirs, "ask")) {
        message(crayon::yellow("    ** = new"))
        ans <- readline(prompt = "Create new directories now? y/n ")
        make_dir_yn <- (tolower(ans) == "y")
      }

      if (make_dir_yn) {
        message(" - creating directory tree")
        sapply(dirs_needed[dir_missing], dir.create, recursive = TRUE, showWarnings = FALSE)
      }

    }


    ## Move / copy images
    if (imgs_action != "none" && !preview_only) {
      ## See if the directories are

      if (FALSE %in% file.exists(dirs_needed)) {
        message(crayon::yellow(paste0(" - Directory not found. Can not ", imgs_action, " images")))

      } else {

        ans <- readline(prompt = paste0(imgs_action, " images now? y/n "))
        copymove_yn <- (tolower(ans) == "y")

        if (copymove_yn) {
          imgs_go_here <- normalizePath(file.path(outdir_base, flt_tree[1]), mustWork = FALSE)

          ## Append the 'images go here' path from the pts attribute table
          img_fromto_tbl <- x[[i]]$pts %>%
            st_drop_geometry() %>%
            mutate(dest_path = imgs_go_here)

          ## Loop through any image tokens in the attribute table and update the dest_path
          for (my_img_token in tokens[tokens %in% img_tokens]) {
            img_fromto_tbl <- img_fromto_tbl %>%
              mutate(dest_path = str_replace(dest_path,
                                             pattern = paste0("\\{", .env$my_img_token, "\\}"),
                                             replacement = !!as.name(my_img_token)))
          }

          ## Prepend a time stamp to file names (to make sure they're unique)
          if (imgs_prepend_fn) {
            img_fromto_tbl <- img_fromto_tbl %>%
              mutate(date_time_dt = as.POSIXct(date_time, format="%Y:%m:%d %H:%M:%S")) %>%
              mutate(file_name = paste0(format(date_time_dt, "%Y%m%d_%H%M%S_"), file_name))
          } else {
            ## TODO: check for duplicate file names right here
          }

          ## Keep only columns required for the move operation
          img_fromto_tbl <- img_fromto_tbl %>%
            select(img_fn, file_name, dest_path)

          ## Copy / move image files
          if (imgs_action == "copy") {
            file.copy(from = img_fromto_tbl$img_fn,
                      to = file.path(img_fromto_tbl$dest_path,
                                     img_fromto_tbl$file_name),
                      overwrite = FALSE)

            res <- c(res, unique(img_fromto_tbl$dest_path))

          } else if (imgs_action == "move") {
            file.rename(from = img_fromto_tbl$img_fn,
                        to = file.path(img_fromto_tbl$dest_path,
                                       img_fromto_tbl$file_name))
            res <- c(res, unique(img_fromto_tbl$dest_path))
          }

          ## Write a metadata.txt file the destination folder(s)
          if (write_metadata) {
            md_lst <- x[[i]]$metadata
            fltmd_yaml <- paste(sapply(1:length(md_lst), function(j) paste0(names(md_lst)[j], ": ", md_lst[[j]])), collapse = "\n\n")

            for (out_dir in unique(img_fromto_tbl$dest_path)) {
              md_pathfn <- file.path(out_dir, "metadata.txt")

              if (!file.exists(md_pathfn)) {
                descript_line <- paste0("## FLIGHT METADATA FOR:\n## ", out_dir, "\n##\n",
                                        "## Tips: \n",
                                        "## `name_short` is used to generate default file names. Keep it short and avoid special characters.\n",
                                        "## `name_long` will be used as a title for the HTML summary page\n\n")

                ## Write the file
                cat(c(descript_line, fltmd_yaml), file = md_pathfn, sep="")

                if (!quiet) message(" - metadata.txt written")
              }
            }

          }   ## if write_metadata = TRUE

          if (tb_action != "none") {

            tb_src <- file.path(unique(dirname(x[[i]]$pts$img_fn)), "map", "tb")

            for (my_tb_src in tb_src) {
              if (file.exists(my_tb_src)) {

                out_dirs <- unique(img_fromto_tbl$dest_path)

                if (length(out_dirs) > 1) {
                  if (!quiet) message(yellow(" - can not copy thumbnails (multiple output directories)"))

                } else {

                  tb_dest <- file.path(out_dirs, "map", "tb")
                  if (!file.exists(tb_dest)) {
                    dir.create(tb_dest, recursive = TRUE)
                  }

                  if (tb_action == "copy") {
                    if (!quiet) message(" - copying thumbnails")
                    file.copy(from = list.files(my_tb_src, full.names = TRUE),
                              to = tb_dest,
                              recursive = TRUE,
                              overwrite = FALSE)

                  } else if (tb_action == "move") {
                    if (!quiet) message(" - moving thumbnails")
                    file.rename(from = my_tb_src,
                                to = tb_dest)
                  }

                }

              }
            }

          }  # if tb_action != "none"

        }


      }  # if imgs_action != "none" && !preview_only


    }

  }

  ## Return a vector of image folders
  invisible(res)
}

