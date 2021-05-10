#' Parse an image collection into individual flights
#'
#' @param x A list of class 'uas_info'
#' @param interflt_val Time value between flights
#' @param interflt_units Time units between flights, see details "med_int" or "secs"
#' @param init_fltnum Initial flight number
#' @param min_images Minimum number of images to be considered a flight
#' @param cross_dirs Parse the images in directories collectively
#' @param options Options and overrides for creating groups
#' @param quiet Suppress messages
#'
#' @details
#'
#' cross_dirs means images in all directories will be examined when look for flights. This
#' would be appropriate if the images from one flight were spread across multiple folders.
#'
#'
#' @importFrom crayon green yellow
#' @importFrom stats median
#' @export

## STATUS AND TODO:
##  need to think about options 'one' (tree all images in all folders as one flight)
## and 'contiguous' (????)
##
##  need to build in 'options' argument (manual break, gap delete)
##  need to create the print.uas_grp() function (can they go in this file?)
##  how about a uas_grp_autoadjust() function (reduce_by = 1)


## STRUCTURE OF THE RESULT
##  List object with elements
##    $input   (one element only)
##       $dirs
##       $interflt_val
##       $interflt_units
##       $min_images
##       $cross_dirs
##       $options

##    $grps[[   ]] (one element for each group/flight)
##       $grpnum (corresponds to flight number, -99 for images that aren't part of a flight)
##       $imgs
##         $subdir
##            $idx

## what happens to the group numbers when each dir is processed individually.


##
## print.uas_group will print these out nicely

uas_grp_flt <- function(x, interflt_val = 10, interflt_units = c("med_int", "secs")[1],
                        init_fltnum = 1, min_images = 5,
                        cross_dirs = TRUE,
                        options = NULL,
                        quiet = FALSE) {

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  ## Initialize the result this function will return
  res <- list()
  res$input <- list(dirs = names(x),
                    grp_method = "flight",
                    interflt_val = interflt_val,
                    interflt_units = interflt_units,
                    min_images = min_images,
                    cross_dirs = cross_dirs,
                    options = options)
  res$grps <- list()
  class(res) <- c("uas_grp", "list")

  ## Step 1. Create a combined dataframe of dts
  imgdt_df <- do.call(rbind, lapply(1:length(x),
                             function(i) data.frame(subdir=factor(names(x)[i]),
                                                    img_idx = 1:nrow(x[[i]]$pts),
                                                    date_time = x[[i]]$pts$date_time)))

  ## Step 2. Create the processing group(s)
  imgdt_grps <- list()
  if (cross_dirs) {
    imgdt_grps[[1]] <- 1:nrow(imgdt_df)
  } else {
    for (i in 1:length(x)) {
      imgdt_grps[[i]] <- which(imgdt_df$subdir == names(x)[i])
    }
  }

  cur_flt_num <- init_fltnum

  ## Loop thru the processing groups
  for (i in 1:length(imgdt_grps)) {

    # imgdt_orig_str <- x[[i]]$pts$date_time
    imgdt_orig_str <- imgdt_df[imgdt_grps[[i]], "date_time"]
    imgdt_orig_dt <- as.POSIXct(imgdt_orig_str, format="%Y:%m:%d %H:%M:%S")
    imgdt_orig_ord <- order(imgdt_orig_dt)
    imgdt_sorted_dt <- imgdt_orig_dt[imgdt_orig_ord]

    ## Compute the time difference between images in seconds
    idx1 <- 1:(length(imgdt_sorted_dt)-1)
    idx2 <- 2:length(imgdt_sorted_dt)
    imgdt_sorted_diff <- difftime(time1 = imgdt_sorted_dt[idx2],
                                  time2 = imgdt_sorted_dt[idx1],
                                  units = "secs") %>% as.numeric()

    ## Compute the 'inter-flight threshold' in seconds
    if (interflt_units == "med_int") {
      ## IF YOU'VE GOT A MULTISPECTRAL DATASET, imgdt_sorted_diff WILL
      ## NEED TO BE FILTERED TO UNIQUE VALUES OF DT IF YOU WANT TO USE
      ## THE 'MEDIAN' METHOD, BECAUSE THERE WILL
      ## BE A LOT OF IMAGES WHERE THE TIME DIFFERENCE IS 0.
      interflt_thresh_secs <- interflt_val * median(imgdt_sorted_diff)
      message(green(" - still need to get rid of duplicates when computing median sampling interval"))
    } else if (interflt_units == "secs") {
      interflt_thresh_secs <- interflt_val
    } else {
      stop("unknown value for interflt_units")
    }

    if (!quiet) {
      message(yellow(" - using an inter-flight threshold of ", interflt_thresh_secs, " seconds", sep= ""))
    }

    ## Identify when the sampling interval exceeded the threshold (i.e., a new flight started)
    imgdt_sorted_fltjump <- as.numeric(imgdt_sorted_diff > interflt_thresh_secs)

    ## Compute the cumulative sum (so intervals below the threshold get the same flight number)
    imgdt_sorted_fltnum <- cumsum(imgdt_sorted_fltjump)

    ## Now we need to insert a flight number for the very first image.
    ## If the very first time diff (dt[2] - dt[1]) exceeds the absolute threshold, then the first image
    ## needs to have its own flight num. If it's 0, then element 1 should be the same as element 2.
    imgdt_incfirst_sorted_fltnum <- c(1, imgdt_sorted_fltnum + 1)

    ## Now each image has a flight number. Next we want to identify those that have > min_images
    # cat("Need to check this when i=1, getting a flight with zero idx \n")
    fltnums_minimages <- as.numeric(which(table(imgdt_incfirst_sorted_fltnum) >= min_images))

    ## 'Zero out' the flight number for images that don't belong to a valid flight
    imgdt_incfirst_sorted_fltnum[!imgdt_incfirst_sorted_fltnum %in% fltnums_minimages] <- -99

    ## Next we need to resample the flight numbers from 4, 7, 12, ... to 1, 2, 3, ...
    ## We do this by creating a list of indices in each flight
    flts_idx_lst <- lapply(fltnums_minimages, function(n) which(imgdt_incfirst_sorted_fltnum == n))

    ## Generate new flight numbers
    fltnums_use <- seq(from = cur_flt_num, length.out = length(fltnums_minimages))

    ## Reset cur_flt_num to be ready for the next loop
    cur_flt_num <- cur_flt_num + length(fltnums_minimages)

    ## Then use a loop to map this images to new values
    for (j in 1:length(flts_idx_lst)) {
      imgdt_incfirst_sorted_fltnum[flts_idx_lst[[j]]] <- fltnums_use[j]
    }

    ## Almost there. Only problem is that order of the flight numbers contained in
    ## imgdt_incfirst_sorted_fltnum is based on the sorted dt values, which may not
    ## be the original order
    fltnums_origord <- sapply(imgdt_orig_ord, function(j) imgdt_incfirst_sorted_fltnum[j])

    ## Double-check the flight numbers assigned to each image (looks good)
    #str(fltnums_origord)
    #table(fltnums_origord)
    ## writeClipboard(paste(uinfo_dt_str, fltnums_origord, sep = "\t"))

    ## Next, construct a list of the flights for this processing group
    grps_lst <- list()

    ## Loop thru fltnums_use and build up grps_lst
    for (j in c(fltnums_use, -99)) {

      ## Get the rows from imgdt_df for this flight
      thisflt_df <- imgdt_df[imgdt_grps[[i]][fltnums_origord == j], ]

      if (nrow(thisflt_df) > 0) {

        ## Create a group name
        if (j == -99) {
          grp_name <- "orphans"
        } else {
          grp_name <- paste0("flt", sprintf("%02d", j))
        }

        ## A list element to grps_lst
        grps_lst[[grp_name]] <- list(grpnum = j, imgs = list())

        ## Create a list of idx values by subdir
        subdirs_thisflt <- as.character(unique(thisflt_df$subdir))

        for (sdir in subdirs_thisflt) {
          grps_lst[[grp_name]]$imgs[[sdir ]] <- thisflt_df[thisflt_df$subdir == sdir,
                                                           "img_idx", drop = TRUE]
        }

      }


      # uv <- lapply(),
      #              function(sdir) idx = thisflt_df[thisflt_df$subdir == sdir, "img_idx"]   )
      ## Group these by subdir
      #imgdt_idx_per_flight_lst <- lapply(1:length(fltnums_minimages), function(j) imgdt_grps[[i]][which(fltnums_origord == j)])
      #imgdt_idx_per_flight_lst <- lapply(1:length(fltnums_minimages), function(j) imgdt_grps[[i]][fltnums_origord == j])
      #sapply(imgdt_idx_per_flight_lst, length)

    }

    ## Add grps_lst to the result
    res$grps <- c(res$grps, grps_lst)

    ##    $grps[[   ]] (one element for each group/flight)
    ##       $grpnum (corresponds to flight number, -99 for images not moved)
    ##       $imgs
    ##         $subdir = idx values (within that subdir)


    ## See how many rows were in this processing group
    ## imgdt_grps[[i]]
    ## str(imgdt_grps[[i]])

    ## Create a list object that has the indices of rows in this processing group for each flight
    ## pg_idx_per_flight_lst <- lapply(1:length(fltnums_minimages), function(j) which(fltnums_origord == j))

    ## These should add up to the number of rows in this processing group (minus any that
    ## aren't in any flight)
    # sapply(pg_idx_per_flight_lst, length)
    # length(imgdt_grps[[i]]) == sum(sapply(pg_idx_per_flight_lst, length))


    ## Lastly we need to grab the img_idx column from imgdt_df
    #k <- 1
    #imgdt_df[201   , "img_idx"]
    #    img_piece <- lapply(1:length(imgdt_idx_per_flight_lst), function(k) list( ))

    ## imgdt_df[imgdt_grps[[i]], "date_time"]

    ## Save result to list
    #outflt_lst <- list(list(tags = list(fltnum = -99), idx = which(imgdt_incfirst_sorted_fltnum == 0)))
    ## idx <- which(imgdt_incfirst_sorted_fltnum == 0)

    #outflt_lst <- list(grpnum = -99, idx = which(imgdt_incfirst_sorted_fltnum == 0))

    ## Pull out the rows from imgdt_df
    #head(imgdt_df); nrow(imgdt_df)



    ## These *should* add up to the number of rows in the processing group (minus any that not in a flight)
    ##sum(sapply(idx_per_flight_lst, length))


    # inflt_lst <- lapply(1:length(fltnums_minimages),
    #                     function(i) list(grpnum = i,
    #                                      idx = which(fltnums_origord == i)))


    # j2 <- inflt_lst[[2]]$idx
    # str(j2)
    # i <- 2
    # imgdt_df[imgdt_grps[[i]][i2], "date_time"]

    # res$groups[[names(x)[i]]] <-  c(outflt_lst, inflt_lst)

  }

  res

  ## Step 1. Construct groups of images for processing, which will vary somewhat
  ## based on the value of treat_dirs.

  # img_grps <- list()
  # if (treat_dirs == "separate") {
  #   for (i in 1:length(x)) {
  #     img_grps[[i]] <- data.frame(dir = names(x)[i], dt = x[[i]]$pts$date_time)
  #   }
  #
  # } else if (treat_dirs == "contiguous") {
  #   for (i in 1:length(x)) {
  #     img_grps[[i]] <- data.frame(dir = names(x)[i], dt = x[[i]]$pts$date_time)
  #   }
  #
  # } else if (treat_dirs == "one") {
  #
  # } else {
  #   stop(paste0("Unknown value for treat_dirs: ", treat_dirs))
  # }


  ##    $grps (one element for each group)
  ##       $grpnum (corresponds to flight number, -99 for images not moved)
  ##       $grpname - leave placeholder for now
  ##       $tags ?? OMIT
  ##       $imgs
  ##         $subdir
  ##            $idx


  ## Start the loop
  # for (i in 1:length(x)) {
  #
  #   imgdt_orig_str <- x[[i]]$pts$date_time
  #   imgdt_orig_dt <- as.POSIXct(imgdt_orig_str, format="%Y:%m:%d %H:%M:%S")
  #   imgdt_orig_ord <- order(imgdt_orig_dt)
  #   imgdt_sorted_dt <- imgdt_orig_dt[imgdt_orig_ord]
  #
  #   idx1 <- 1:(length(imgdt_sorted_dt)-1)
  #   idx2 <- 2:length(imgdt_sorted_dt)
  #
  #   ## Compute the time difference between images in seconds
  #   imgdt_sorted_diff <- difftime(time1 = imgdt_sorted_dt[idx2],
  #                                 time2 = imgdt_sorted_dt[idx1],
  #                                 units = "secs") %>% as.numeric()
  #
  #   ## Compute the 'inter-flight threshold' in seconds
  #   if (interflt_units == "med_int") {
  #     ## IF YOU'VE GOT A MULTISPECTRAL DATASET, imgdt_sorted_diff WILL
  #     ## NEED TO BE FILTERED TO UNIQUE VALUES OF DT IF YOU WANT TO USE
  #     ## THE 'MEDIAN' METHOD, BECAUSE THERE WILL
  #     ## BE A LOT OF IMAGES WHERE THE TIME DIFFERENCE IS 0.
  #     interflt_thresh_secs <- interflt_val * median(imgdt_sorted_diff)
  #   } else if (interflt_units == "secs") {
  #     interflt_thresh_secs <- interflt_val
  #   } else {
  #     stop("unknown value for interflt_units")
  #   }
  #
  #   if (!quiet) {
  #     cat("Using an inter-flight threshold of ", interflt_thresh_secs, " seconds \n", sep= "")
  #   }
  #
  #   ## Identify when the sampling interval exceeded the threshold (i.e., a new flight started)
  #   imgdt_sorted_fltjump <- as.numeric(imgdt_sorted_diff > interflt_thresh_secs)
  #
  #   ## Compute the cummulative sum (so intervals below the threshold get the same flight number)
  #   imgdt_sorted_fltnum <- cumsum(imgdt_sorted_fltjump)
  #
  #   ## Now we need to insert a flight number for the very first image.
  #   ## If the very first time diff (dt[2] - dt[1]) exceeds the absolute threshold, then the first image
  #   ## needs to have its own flight num. If it's 0, then element 1 should be the same as
  #   ## element 2.
  #   imgdt_incfirst_sorted_fltnum <- c(1, imgdt_sorted_fltnum + 1)
  #
  #   ## Now each image has a flight number. Next we want to identify those that have > min_images
  #   fltnums_minimages <- as.numeric(which(table(imgdt_incfirst_sorted_fltnum) >= min_images))
  #
  #   ## 'Zero out' the flight number for images that don't belong to a valid flight
  #   imgdt_incfirst_sorted_fltnum[!imgdt_incfirst_sorted_fltnum %in% fltnums_minimages] <- 0
  #
  #   ## Next we need to resample the flight numbers from 4, 7, 12, ... to 1, 2, 3, ...
  #   flts_idx_lst <- lapply(fltnums_minimages, function(n) which(imgdt_incfirst_sorted_fltnum == n))
  #   for (i in 1:length(flts_idx_lst)) imgdt_incfirst_sorted_fltnum[flts_idx_lst[[i]]] <- i
  #
  #   ## Almost there. Only problem is that order of the flight numbers contained in
  #   ## imgdt_incfirst_sorted_fltnum is based on the sorted dt values, which may not
  #   ## be the original order
  #   fltnums_origord <- sapply(imgdt_orig_ord, function(i) imgdt_incfirst_sorted_fltnum[i])
  #
  #   ## Double-check (looks good)
  #   ## writeClipboard(paste(uinfo_dt_str, fltnums_origord, sep = "\t"))
  #
  #   ## Save result to list
  #   outflt_lst <- list(list(tags = list(fltnum = -99), idx = which(imgdt_incfirst_sorted_fltnum == 0)))
  #
  #   inflt_lst <- lapply(1:length(fltnums_minimages),
  #          function(i) list(tags = list(fltnum = i),
  #                           idx = which(fltnums_origord == i)))
  #
  #   res$groups[[names(x)[i]]] <-  c(outflt_lst, inflt_lst)
  #
  #
  # }

}




