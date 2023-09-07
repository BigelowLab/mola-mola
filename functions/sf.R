#' Write to a geopackage file by writing to a temp file first and then
#' copying to the specified destination.
#' 
#' @seealso \href{https://github.com/r-spatial/sf/issues/628#issuecomment-381690663}{sf issue 628}
#' @export
#' @param obj object to write
#' @param dsn character, the file to write to
#' @param overwrite, logical, overwrite existing file?
#' @param ... other arguments passed to \code{\link[sf]{write_sf}}
#' @return obj invisibly
write_gpkg <- function(obj, dsn, 
                       overwrite = TRUE,
                       ...){
  dsnpath <- dirname(dsn)
  if (!dir.exists(dsnpath)) stop("destination path doesn't exist: ", dsnpath)
  tmpfile <- tempfile(fileext = ".gpkg")
  obj <- try(sf::write_sf(obj, dsn=tmpfile, driver = "GPKG"))
  if (inherits(obj, "try-error")){
    print(obj)
  } else {
    ok <- file.copy(tmpfile, dsn, overwrite = overwrite)
  }
  invisible(obj)
}
