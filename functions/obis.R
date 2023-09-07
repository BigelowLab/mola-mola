#' Fetch a species from OBIS
#' 
#' @export
#' @param scientificname character, the Latin name for a species
#' @param geometry WKT or NULL to limit search
#' @parma basis_of_record character, used to narrow the types of observations. To retrieve
#'   all types set to 'any' or 'all'.  By default 'HumanObservation'.
#' @param ... other arguments for \code{\link[robis]{occurrence}}
#' @param save_file NA or a path specification to save the file
#' @return sf tibble
fetch_obis <- function(scientificname = 'Mola mola', 
                       save_file = species_to_filename(scientificname, 
                                             path = here::here("data", "obis")),
                       geometry = get_bb(form = "polygon") |> sf::st_as_text(),
                       basis_of_record = "HumanObservation",
                       ...){
  
  
   x <- try(robis::occurrence(scientificname = scientificname[1], geometry = geometry,...)) 
  if (!inherits(x, 'try-error') && nrow(x) > 0){
    x <- dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(date = as.Date(substring(.data$eventDate, 1, nchar("YYYY-mm-dd")), 
                                               format = "%Y-%m-%d")) |>
      sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
      dplyr::select(occurrenceID, date, basisOfRecord, bathymetry, 
                    shoredistance, sst, sss) |>
      dplyr::mutate(bathymetry = as.numeric(bathymetry),
                    shoredistance = as.numeric(shoredistance),
                    sst = as.numeric(sst),
                    sss = as.numeric(sss))
    if (!any(basis_of_record %in% c("any", "all"))){
      x = dplyr::filter(x, basisOfRecord %in% basis_of_record)
    }
    
  }
  if (!inherits(x, 'try-error') && nrow(x) > 0 && !is.na(save_file)){
    x <- write_gpkg(x, save_file)
  }
  x
}


#' Convert from scientific name to file name
#' 
#' @param x chr, scientific name of species
#' @param ext chr, file extension to be added
#' @param sep chr, separator of file name (between genus and species)
#' @param path chr, path of which file is stored
#' @return character vector of file names
species_to_filename <- function(x = 'Mola mola', 
                      ext = '.gpkg', 
                      sep = "_", 
                      path = here::here("data", "obis")){
  
  x = gsub(" ", sep, x, fixed = TRUE)
  x = paste0(x, ext)
  x = file.path(path, x)
  x
}


#' Convert from file name to scientific_name
#' 
#' @export
#' @param x chr, name of file
#' @param ext chr, file extension to be removed
#' @param sep chr, separator of file name
#' @return character vector of scientific names
scientific_name <- function(x = 'Mola_mola.gpkg', 
                            ext = '.gpkg', 
                            sep = '_'){
  
  x = basename(x)
  x = sub(ext, '', x, fixed = TRUE)
  x = paste0(toupper(substring(x, 1,1)),substring(x, 2))
  x = gsub(sep," ", x, fixed = TRUE)
  x
}



#' Retrieve a listing of local files
#' 
#' @export
#' @param path the path to the datasets
#' @param full.names logical return basename or full path specs?
#' @param strip character or NA, if character then strip this pattern. Ignored
#'   if full.names is \code{TRUE}
#' @return character names (possibly filenames)
list_obis <- function(path = here::here("data", "obis"), 
                      full.names = FALSE,
                      strip = c(NA, ".gpgk")[2]){
  ff <- list.files(path, pattern = "^.*\\.gpkg$", full.names = full.names)
  if (!full.names && !is.na(strip[1])){
    ff <- sub(strip, "", ff, fixed = TRUE)
  }
  ff
}


#' function to read previously downlaoded species data file.
#' 
#' If file doesn't exist, first try to fetch it then save it to the cache.
#' 
#' @export
#' @param species character, scientific name of species
#' @param form character, if "table" then return a data frame  (tibble) otherwise
#'   return a spatially refeenced sf POINT object
#' @return sf tibble
read_obis = function(species = "Mola mola", 
                     form = c("dataframe", "sf")[2]){
  filename = species_to_filename(species[1])
  if (!file.exists(filename))  {
    x = fetch_obis(scientificname = species)
  } else {
    x = sf::read_sf(filename)
  }
  
  if (tolower(form[1] %in% c("table", "tibble", "dataframe"))){
    x = sf::st_drop_geometry(x)
  }
  
  return(x)
}


