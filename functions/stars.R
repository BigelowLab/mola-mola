#' Given a list of simialr stars objects (geometry-wise), bind as attributes.
#'
#' @param x list of stars objects
#' @param .names chr, the name of the attributes
#' @return multi-attribute stars object
bind_attributes = function(x, .names = names(x)){
  do.call(c, append(x, list(along = NA_integer_))) |>
    setNames(.names)
}

#' Bind a list of \code{stars} objects by band
#'
#' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
#'
#' @export
#' @param x list of one or more \code{stars} objects
#' @param along list, see \code{c.stars}
#' @return \code{stars} objects
bind_bands <- function(x, along = list(time = seq_len(x))){
  do.call(c, append(x, list(along = names(along)))) |>
    stars::st_set_dimensions(names(along),  values = along[[1]])
             
}

#' Read a mask raster
#' 
#' @param name chr then name of the mask to read (default = "mask_factor")
#' @param path chr the path to the mask data
#' @return stars masking object
read_mask = function(name = "mask_factor", path = "data/mask"){
  filename = file.path(path, paste0(name[1], ".tif"))
  stars::read_stars(filename) |>
    setNames("mask")
}


#' Given a set of points and a target raster geometry, compute the density (count)
#' of points in each cell.
#' 
#' @param x sf POINT object
#' @param y stars or SpatRaster object that defines the geometry of the output
#' @param name chr, the name of the output variable
#' @param dilate num, the size of the square structuring element used for dilating the
#'   output counts (padding with zeroes).  Set to 0 to skip. 
#' @param dilate_value num, if dilation occurs, this is the value assigned to the padded cells
#' @param mask `SpatRaster` or `stars` object that defines masked areas where
#'   dilation does not occur with the value NA.  It must have the same spatial 
#'   geometry as the input \code{y}.
#' @return 
rasterize_point_density <- function(x, y, 
                                    name = "count",
                                    dilate = 0,
                                    dilate_value = 1,
                                    mask = y){
  
  if (inherits(y, "stars")){
    # if y has scrambled coords reorganize as x,y,z
    # then be sure we have just one variable and one band (ie simple 2d geometry)
    y = stars:::st_upfront(y)
    y = y[1]
    d = dim(y)
    if(length(d) > 2){
      y = dplyr::slice(y, names(d)[3], 1)  
    }
    
    # cast as point data
    v = sf::st_as_sf(y)
    # trim the points to just a "name" attribute
    x = dplyr::mutate(x, {{ name }} := 1) |>
      dplyr::select(dplyr::all_of(name))
    # aggregate by counting the instances of x in each element of y 
    # (or the polygonized-by-cell "v" version) and then cast back to 
    # the template raster
    r = aggregate(x, v, FUN = length) |>
      stars::st_rasterize(template = y, align = TRUE)
  } else {
    r = terra::rasterize(x, y, fun = "count")
  }
  
  
  if (dilate[1] > 0){
    
    if (inherits(r, 'stars')){
      raw = r[[1]][]
    } else {
      raw = as.matrix(r)
    }
   
    raw = (!is.na(raw)) * 1.0
    
    m = raw |> 
      imager::as.cimg() |>
      imager::dilate_square(dilate[1]) |>
      as.matrix()
    
    if (!is.null(mask)){
      if (inherits(mask, 'stars')){
          if (inherits(mask[[1]], "factor")) mask[[1]] <- as.numeric(mask[[1]])
          ix = is.na(mask[[1]][]) 
        } else {
          ix = is.na(as.matrix(mask))    
        }
      m[ix] <- NA
      # in case the edge cases produce unwanted zeroes (like over land)
      m[m <= 0] <- NA
    }
    
    # here we transfer the padded zeroes to the count data
    if (inherits(r, "stars")){
      s = r[[1]]
      ix <- (!is.na(s)) | (is.na(m))
      s[!ix] <- dilate_value
      r[[1]] <- s
    } else {
      s = r[[1]]
      ix <- (!is.na(s)) | (is.na(m))
      s[!ix] <- dilate_value
      r[[1]] <- s
    }
  }
  
  r
}

#' A convenience function for reading in predictor rasters
#' 
#' @param quick NULL or a character vector of variables to load.  Use this to
#'   have the functions laod the databases automatically.  To read all use
#'   \code{quick = c("sst", "windspeed", "u_wind", "v_wind")} or set quick to TRUE.
#'   Oddly enough, if you set quick to FALSE it will still read all four variable.
#' @param sst_db NULL or a database table
#' @param windspeed_db NULL or a database table
#' @param u_wind_db NULL or a database table
#' @param v_wind_db NULL or a database table
#' @param sst_path char, the path to sst data
#' @param nbs_path char, the path to wind data
#' @param nbs_shift numeric, days to shift nbs days by because nbs, natively,
#'   is time-stamped to the middle of each month.  Ignored if per in db
#'   is not "month" or "mon"
#' @return stars object with one or more attributes (variables)
read_predictors = function(
    quick = NULL, 
    sst_db = NULL, 
    windspeed_db = NULL, 
    u_wind_db = NULL, 
    v_wind_db = NULL,
    sst_path = "data/oisst", 
    nbs_path = "data/nbs",
    nbs_shift = -14){
  
  if (!is.null(quick)){
    if (is.logical(quick)){
      quick = c("sst", "windspeed", "u_wind", "v_wind")
    } 
    if ("sst" %in% quick){
      sst_db = oisster::read_database(sst_path) |>
        dplyr::arrange(date)
    }
    if(any(grepl("wind", quick, fixed = TRUE))){
      wind_db = nbs::read_database(nbs_path) |>
        dplyr::arrange(date)
    }
    if ("windspeed" %in% quick){
      windspeed_db = wind_db |>
        dplyr::filter(param == "windspeed")
    }
    if ("u_wind" %in% quick){
      u_wind_db = wind_db |>
        dplyr::filter(param == "u_wind")
    }
    if ("v_wind" %in% quick){
      v_wind_db = wind_db |>
        dplyr::filter(param == "v_wind")
    }
  }
  
  if (all(c(is.null(sst_db), is.null(windspeed_db), is.null(u_wind_db), is.null(v_wind_db)))){
    stop("at least one of the databases must be provided")
  }
  
  db = list(sst = sst_db, windspeed = windspeed_db, u_wind = u_wind_db, v_wind = v_wind_db)
  xx = sapply(names(db),
              function(name){
                if (is.null(db[[name]])) return(NULL)
                files = if(name == "sst"){
                  filenames = oisster::compose_filename(db[[name]], sst_path)
                  x = stars::read_stars(filenames, along = list(time = db[[name]]$date)) |>
                    rlang::set_names(name)
                } else {
                  filenames = nbs::compose_filename(db[[name]], nbs_path)
                  x = stars::read_stars(filenames, along = list(time = db[[name]]$date)) |>
                    rlang::set_names(name)
                  if (db[[name]]$per[1] %in% c("month", "mon")){
                    d = stars::st_dimensions(x)
                    d$time$values = d$time$values + nbs_shift
                    stars::st_dimensions(x) <- d
                  }
                }

                x
              }, simplify = FALSE)
    
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
  st_to_180(x)
}