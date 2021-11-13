#' Creates a function that converts a directory path to a flight name
#'
#' Creates a function that converts a directory path to a flight name
#'
#' @param idx_from_last Indices of the directory path, see Details
#'
#' @details This will return a function that will parse a directory path into its constituent pieces and
#' construct a name from one or more of those pieces. This can be useful for automatically naming flights
#' based on the directory path in which the images are saved when running \code{\link{uas_info}}.
#'
#' \code{idx_from_last} are indices of the subdirectories in the path, starting from the end. Hence if you wanted to construct
#' names for a flight based on the very subdirctory (only), let \code{idx_from_last = 1}. If you want the names to be constructed
#' based on the 2nd to last subdirectory, let \code{idx_from_last = 2}, and so on. If more than one number is passed for
#' \code{idx_from_last}, the name returned will concatenate the pieces separated by underscores.
#'
#' Note that this function does not actually convert a directory path to a flight name, but returns a function
#' that will do the conversion.
#'
#' @return A function that returns a flight name
#' @seealso \code{\link{uas_info}}
#' @examples
#' \dontrun{
#' data_dir <- "D:/Drone_Data/HREC/PostRiverFire2020/imgs/ebX_Flt02_postproc/img/msp"
#' flight_name_fun <- uas_path2name_fun(c(5,3,1))
#' flight_name_fun(data_dir)
#' ebee_flt2_info <- uas_info(data_dir, fp = FALSE, path2name_fun = flight_name_fun)
#' }
#' @export

uas_path2name_fun <- function(idx_from_last = 1) {

  ## Construct a character object that defines the function, where idx_from_last is hard-coded
  foo_chr <- paste0("function(img_dir) {
    if (length(img_dir) != 1) stop(\"img_dir must be of length 1\")
    gsub(\"\\\\\\\\\", \"/\", img_dir) %>%
      gsub(\"/$\", \"\", .) %>%
      strsplit(\"/\") %>%
      extract2(1) %>% rev() %>%
      extract( c(", paste0(idx_from_last, collapse = ","), ") ) %>%
      paste0(collapse = \"_\")
  }")

  ## Return the evaluated expression
  eval(parse(text = foo_chr))

}

# Original:
# THIS WAS CONVERTED TO A FUNCTION THAT RETURNS A FUNCTION BECAUSE THE ARGUMENT IN
# uas_info() only accepts function, not arguments, and I don't want to add an additional
# argument to uas_info()

# uas_path2name_args <- function(img_dir, idx_from_last = 1) {
#   if (length(img_dir) != 1) stop("img_dir must be of length 1")
#   normalizePath(img_dir) %>% strsplit("\\\\") %>% extract2(1) %>% rev() %>% extract(idx_from_last) %>% paste0(collapse = "_")
# }
#



