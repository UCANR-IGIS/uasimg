#' Prints a UAS Image Collection Groups object
#' @param x A UAS Image Collection Groups object
#' @param ... unused
#'
#' @importFrom crayon yellow bold
#' @export

print.uas_grp <- function(x, ...) {
  if (!inherits(x, "uas_grp")) stop("x should be of class \"uas_grp\"")

  cat(yellow$bold("UAS Image Collection - Groups \n"))
  cat(yellow("INPUTS \n"))

  cat(yellow("  Group method: "), x$input$grp_method, "\n", sep="")

  if (x$input$grp_method == "flight") {
    cat(yellow("  Inter-flight units: "), x$input$interflt_units, "\n", sep="")
    cat(yellow("  Inter-flight threshold: "), x$input$interflt_val, "\n", sep="")
    cat(yellow("  Min# images: "), x$input$min_images, "\n", sep="")
    cat(yellow("  Combine dirs: "), x$input$combine_dirs, "\n", sep="")
  }

  cat(yellow("GROUPS \n"))

  for (i in 1:length(x$grps)) {
    #cat(i, "\n")
    if (!is.null(names(x$grps))) {
     cat("  ", names(x$grps)[i], "\n", sep="")
    }
    cat(yellow("   - group/flt #: "), x$grps[[i]]$grpnum, "\n", sep="")
    for (j in 1:length(x$grps[[i]]$imgs )) {
      cat(yellow("   - ", names(x$grps[[i]]$imgs)[j], ": ", sep=""), length(x$grps[[i]]$imgs[[j]]) , " images \n", sep = "")
    }
  }

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
