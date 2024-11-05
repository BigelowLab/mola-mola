#' Write and read models and ensembles.
#' 
#' @param x a model or ensemble
#' @param filename chr, the name of the file to write to
#' @param ... extra keywords passed to [`readr::write_rds`] and [`readr::read_rds`].
#' @return a model or ensemble (invisibly for writing)
write_model = function(x, filename = "model.rds", ...){
  readr::write_rds(x, filename, ...)
}

#' @rdname write_model
read_model = function(filename, ...){
  readr::read_rds(filename, ...)
}

#' @rdname write_model
write_ensemble = function(x, filename = "ensemble.rds", ...){
  readr::write_rds(x, filename, ...)
}

#' @rdname write_model
read_ensemble = function(filename, ...){
  readr::read_rds(filename, ...)
}