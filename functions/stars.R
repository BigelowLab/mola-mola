#' A convenience function for reading in predictor rasters
#' 
#' @param sst_db NULL or a database table
#' @param windspeed_db NULL or a database table
#' @param u_wind_db NULL or a database table
#' @param v_wind_db NULL or a database table
#' @param sst_path char, the path to sst data
#' @param wind_path char, the path to wind data
#' @return stars object with one or more attributes (variables)
read_predictors = function(
    sst_db = NULL, windspeed_db = NULL, u_wind_db = NULL, v_wind_db = NULL,
    sst_path = "data/oisst", wind_path = "data/nbs"){
  
  
  if (all(c(is.null(sst_db), is.null(windspeed_db), is.null(u_wind_db), is.null(v_wind_db)))){
    stop("at least one of the databases must be provided")
  }
  
  db = list(sst = sst_db, windspeed = windspeed_db, u_wind = u_wind_db, v_wind = v_wind_db)
  xx = sapply(names(db),
              function(name){
                if (is.null(db[[name]])) return(NULL)
                files = if(name == "sst"){
                  filenames = oisster::compose_filename(db[[name]], sst_path)
                } else {
                  filenames = nbs::compose_filename(db[[name]], wind_path)
                }
                stars::read_stars(filenames, along = list(time = db[[name]]$date)) |>
                  rlang::set_names(name)
              })
    
  # purge any NULLS
  ix = sapply(xx, is.null)
  xx = xx[!ix]
  
  # locate 'sst' in case we need to warp it
  ix = names(xx) == "sst"
  if (any(ix) && length(xx) > 2){
    iy = which(!ix)[1]
    xx[['sst']] = stars::st_warp(xx[['sst']], xx[[iy]])
  }
  
  if (length(xx) == 1){
    x = xx[[1]] |>
      rlang::set_names(names(xx))
  } else {
   x = do.call(c, append(xx, list(along = NA_integer_)))         
  }
  x
}