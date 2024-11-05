#' A wrapper around [`tidysdm::predict_raster`] to enable multi-layer predictions.
#' 
#' @param object model or ensemble
#' @param raster the raster (must have the attributes looked for by \code{object}),
#'   and may have one or more layers.
#' @param ... see [`tidysdm::predict_raster`]
#' @return a stars object with one prediction for each input layer in `raster`
predict_rasters = function(object, raster, ...){
  
  dn = dimnames(raster)
  if (length(dn) == 2){
    r = tidysdm::predict_raster(object, raster, ...)
  } else if (length(dn) == 3){
    d3 = stars::st_get_dimension_values(raster, dn[3])
    dn3 = dn[3]
    along = list(d3) |>
      setNames(dn[3])
    d = dim(raster)
    r = lapply(seq_len(d[3]),
        function(i){
          tidysdm::predict_raster(object, slice(raster, {{ dn3 }}, i))
        }) |>
      bind_bands(along = along)
  } else {
    stop("raster must have either 2 or 3 dimensions")
  }
  r
}