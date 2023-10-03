#' Retrieve the assumed CRS
#' 
#' @export
#' @return CRS as character or numeric
eckhert4_crs = function(){
  return("+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
}

#' Convert [0,360] longitudes to [-180, 180]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being withing [0, 360] range
#' @return numeric vector
to_180 <- function(x) { ((x + 180) %% 360) - 180 }

#' Convert [-180,180] longitudes to [0, 360]
#'
#' @seealso \url{https://gis.stackexchange.com/questions/201789/verifying-formula-that-will-convert-longitude-0-360-to-180-to-180/201793}
#' @export
#' @param x numeric vector, no check is done for being within [0,3 60] range
#' @return numeric vector
to_360 <- function(x) {x %% 360}


#' Convert a bounding box to 0-360
#' 
#' @export
#' @param x four element named vector of 'xmin', 'ymin', 'xmax' and 'ymax'
#' @return a four element vector of 'xmin', 'ymin', 'xmax' and 'ymax'
to_360_bb = function(x = c(xmin = -72, ymin = 39, xmax = -63, ymax = 46)){
  x[["xmin"]] = to_360(x[["xmin"]])
  x[["xmax"]] = to_360(x[["xmax"]])
  x
}

#' Convert a bounding box to -180-180
#' 
#' @export
#' @param x four element named vector of 'xmin', 'ymin', 'xmax' and 'ymax'
#' @return a four element vector of 'xmin', 'ymin', 'xmax' and 'ymax'
to_180_bb = function(x = c(xmin = 288, ymin = 39, xmax = 297, ymax = 46)){
  x[["xmin"]] = to_180(x[["xmin"]])
  x[["xmax"]] = to_180(x[["xmax"]])
  x
}

#' Transform a stars or sf object from -180-180 range to 0-360
#' 
#' Caution is advised as it won't work in all cases (like date-line crossings etc)
#' 
#' @param x stars or sf object
st_to_360 = function(x){
  if (inherits(x, 'stars')){
    dd = stars::st_dimensions(x)
    dd$x$offset = to_360(dd$x$offset)
    stars::st_dimensions(x) <- dd
  } else {
    xy = sf::st_coordinates(x)
    xy[,1] = to_360(xy[,1])
    crs = sf::set_crs(x)
    x = sf::st_drop_coordinates(x) |>
      dplyr::bind_cols(dplyr::as_tibble(xy)) |>
      sf::st_as_sf(coords = c("X", "Y"), crs = crs)
  }
  x
}

#' Transform a stars or sf object from 0-360 range to -180-180
#' 
#' Caution is advised as it won't work in all cases (like date-line crossings etc)
#' 
#' @param x stars or sf object
st_to_180 = function(x){
  if (inherits(x, 'stars')){
    dd = stars::st_dimensions(x)
    dd$x$offset = to_180(dd$x$offset)
    stars::st_dimensions(x) <- dd
  } else {
    xy = sf::st_coordinates(x)
    xy[,1] = to_180(xy[,1])
    crs = sf::set_crs(x)
    x = sf::st_drop_coordinates(x) |>
      dplyr::bind_cols(dplyr::as_tibble(xy)) |>
      sf::st_as_sf(coords = c("X", "Y"), crs = crs)
  }
  x
}