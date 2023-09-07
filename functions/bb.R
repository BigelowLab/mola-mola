#' Retrieve a bounding box as a vector or sf bbox
#' 
#' @param x numeric 4 element vector in order of (xmin, ymin, xmax, ymax) in decimal degrees
#' @param form char, on of 'numeric' or 'bbox' (default) or 'polygon'
#' @param crs numeric or character, coordinate reference suitable for sf object 
#' @return either a 4 lement vector, sf bbox or sf polygon
get_bb = function(x = c(xmin = -76, ymin = 35, xmax = -58, ymax = 46),
                  form = c("numeric", "bbox", "polygon")[2],
                  crs = 4326){
  
  switch(tolower(form[1]),
         'bbox' = sf::st_bbox(x, crs = crs),
         'polygon' = sf::st_bbox(x, crs = crs) |>
           sf::st_as_sfc(),
         x)
}