#' Look up UTM zone
#'
#' Finds the UTM zone for a geographic coordinate
#'
#' @param x Longitude in decimal degrees. Can also be a numeric vector of length 2 containing longitude and latitude values.
#' @param lat Latitude in decimal degrees
#'
#' @details This will return a crs object (from the sf package) for the correct UTM zone.
#'
#' @return A \code{crs} object of the correct UTM zone
#' @export

geo2utm <- function(x, lat=NULL) {

  # Find UTM EPSG code from Latitude and Longitude coordinates (EPSG 4326 WGS84)
  # Source: https://geocompr.robinlovelace.net/reproj-geo-data.html
  # Source: https://gis.stackexchange.com/questions/13291/computing-utm-zone-from-lat-long-point

  len_err_msg <- "Please pass single numbers for lon and lat. This function is not vectorized."
  if(length(x) == 1) {
    lon <- x[1]
    if (is.null(lat)) stop("lat is a required argument")
    if (length(lat) != 1) stop(len_err_msg)
  } else if (length(x) == 2) {
    if (!is.null(lat)) stop("If you pass a 2-item numeric vector with a longitude and latitude values, you should not pass lat")
    lon <- x[1]
    lat <- x[2]
  } else {
    stop(len_err_msg)
  }

  #LatLonToUTMEPSGCode <- function(lat, lon) {

  zone_number <- (floor((lon + 180) / 6) %% 60) + 1

  # Special zones for Norway
  cond_32 <- lat >= 56.0 & lat < 64.0 & lon >= 3.0 & lon < 12.0
  zone_number[cond_32] <- 32

  # Special zones for Svalbard
  cond_lat <- lat >= 72.0 & lat < 84.0

  cond_31 <- cond_lat & lon >= 0.0 & lon <  9.0
  zone_number[cond_31] <- 31

  cond_33 <- cond_lat & lon >= 9.0 & lon < 21.0
  zone_number[cond_33] <- 33

  cond_35 <- cond_lat & lon >= 21.0 & lon < 33.0
  zone_number[cond_35] <- 35

  cond_37 <- cond_lat & lon >= 33.0 & lon < 42.0
  zone_number[cond_37] <- 37

  # EPSG code
  utm <- zone_number
  utm[lat > 0] <- utm[lat > 0] + 32600
  utm[lat <= 0] <- utm[lat <= 0] + 32700

  return(utm)

}

# geo2utm_old <- function(x, lat=NULL) {
# THIS FUNCTION HAS BEEN DEPRECATED IN FAVOR OF THE ONE ABOVE, BECAUSE WE'RE NO LONGER
# SUPPOSED TO USE PROJ4 STRINGS
#
#   len_err_msg <- "Please pass single numbers for lon and lat. This function is not vectorized."
#   if(length(x) == 1) {
#     lon <- x[1]
#     if (is.null(lat)) stop("lat is a required argument")
#     if (length(lat) != 1) stop(len_err_msg)
#   } else if (length(x) == 2) {
#       if (!is.null(lat)) stop("If you pass a 2-item numeric vector with a longitude and latitude values, you should not pass lat")
#       lon <- x[1]
#       lat <- x[2]
#   } else {
#     stop(len_err_msg)
#   }
#
#   ## Convert to UTM
#   utm_zone <- floor((lon + 180) / 6) + 1
#   utm_ns <- if (lat > 0) " +north" else " +south"
#
#   ## Return crs object
#   sf::st_crs(paste("+proj=utm +zone=", utm_zone, utm_ns, " +ellps=WGS84", sep=""))
# }



