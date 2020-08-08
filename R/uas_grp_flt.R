#' Group a set of images into flights
#'
#' @param x A list of class 'uas_info'
#' @param interflt_val Time value between flights
#' @param interflt_units Time units between flights, see details "med_int" or "secs"
#' @param init_flt_num Initial flight number
#' @param min_images Minimum number of images to be considered a flight
#' @param quiet Suppress messages
#'
#' @export

uas_grp_flt <- function(x, interflt_val = 10, interflt_units = c("med_int", "secs")[1],
                        init_flt_num = 1, min_images = 5, quiet = FALSE) {

  if (!inherits(x, "uas_info")) stop("x should be of class \"uas_info\"")

  res <- list()

  ## Start the loop
  for (i in 1:length(x)) {

    imgdt_orig_str <- x[[i]]$pts$date_time
    imgdt_orig_dt <- as.POSIXct(imgdt_orig_str, format="%Y:%m:%d %H:%M:%S")
    imgdt_orig_ord <- order(imgdt_orig_dt)
    imgdt_sorted_dt <- imgdt_orig_dt[imgdt_orig_ord]

    idx1 <- 1:(length(imgdt_sorted_dt)-1)
    idx2 <- 2:length(imgdt_sorted_dt)

    ## Compute the time difference between images in seconds
    imgdt_sorted_diff <- difftime(time1 = imgdt_sorted_dt[idx2],
                                  time2 = imgdt_sorted_dt[idx1],
                                  units = "secs") %>% as.numeric()

    ## Compute the 'inter-flight threshold' in seconds
    if (interflt_units == "med_int") {
      interflt_thresh_secs <- interflt_val * median(uinfo_sorted_diff)
    } else if (interflt_units == "secs") {
      interflt_thresh_secs <- interflt_val
    } else {
      stop("unknown value for interflt_units")
    }

    if (!quiet) {
      cat("Using an inter-flight threshold of ", interflt_thresh_secs, " seconds \n", sep= "")
    }

    ## Identify when the sampling interval exceeded the threshold (i.e., a new flight started)
    imgdt_sorted_fltjump <- as.numeric(imgdt_sorted_diff > interflt_thresh_secs)

    ## Compute the cummulative sum (so intervals below the threshold get the same flight number)
    imgdt_sorted_fltnum <- cumsum(imgdt_sorted_fltjump)

    ## Now we need to insert a flight number for the very first image.
    ## If the very first time diff (dt[2] - dt[1]) exceeds the absolute threshold, then the first image
    ## needs to have its own flight num. If it's 0, then element 1 should be the same as
    ## element 2.
    imgdt_incfirst_sorted_fltnum <- c(1, imgdt_sorted_fltnum + 1)

    ## Now each image has a flight number. Next we want to identify those that have > min_images
    fltnums_minimages <- as.numeric(which(table(imgdt_incfirst_sorted_fltnum) >= min_images))

    ## 'Zero out' the flight number for images that don't belong to a valid flight
    imgdt_incfirst_sorted_fltnum[!imgdt_incfirst_sorted_fltnum %in% fltnums_minimages] <- 0

    ## Next we need to resample the flight numbers from 4, 7, 12, ... to 1, 2, 3, ...
    flts_idx_lst <- lapply(fltnums_minimages, function(n) which(imgdt_incfirst_sorted_fltnum == n))
    for (i in 1:length(flts_idx_lst)) imgdt_incfirst_sorted_fltnum[flts_idx_lst[[i]]] <- i

    ## Almost there. Only problem is that order of the flight numbers contained in
    ## imgdt_incfirst_sorted_fltnum is based on the sorted dt values, which may not
    ## be the original order
    fltnums_origord <- sapply(imgdt_orig_ord, function(i) imgdt_incfirst_sorted_fltnum[i])

    ## Double-check (looks good)
    ## writeClipboard(paste(uinfo_dt_str, fltnums_origord, sep = "\t"))

    ## Save result to list
    outflt_lst <- list(list(tags = list(fltnum = -99), idx = which(imgdt_incfirst_sorted_fltnum == 0)))

    inflt_lst <- lapply(1:length(fltnums_minimages),
           function(i) list(tags = list(fltnum = i),
                            idx = which(fltnums_origord == i)))

    res[[names(x)[i]]] <-  c(outflt_lst, inflt_lst)


  }

  class(res) <- c("uas_grp", "list")
  res

}


uas_grps2col <- function(x, colNoGrp = "dimgray") {

  if (!inherits(x, "uas_grp")) stop("x should be of class \"uas_grp\"")

  res <- list()

  ## Start the loop
  for (i in 1:length(x)) {

    #cols <- c(colNoGrp, rainbow(length(x[[i]]) - 1, end=5/6))

    cols <- sample(c("#89C5DA", "#DA5724", "#74D944", "#CE50CA", "#3F4921", "#C0717C", "#CBD588", "#5F7FC7",
    "#673770", "#D3D93E", "#38333E", "#508578", "#D7C1B1", "#689030", "#AD6F3B", "#CD9BCD",
    "#D14285", "#6DDE88", "#652926", "#7FDCC0", "#C84248", "#8569D5", "#5E738F", "#D1A33D",
    "#8A7C64", "#599861"), size = length(x[[i]]))


    idx_all <- unlist(sapply(x[[i]], function(y) y$idx))
    cols_this <- rep(NA, length(idx_all)) #instead of length() I could use max()

    for (j in 1:length(x[[i]])) cols_this[ x[[i]][[j]]$idx ] <- cols[j]

    res <- c(res, list(cols_this))

  }
  res

}



