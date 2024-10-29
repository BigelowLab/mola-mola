#' Read an observation dataset. 
#' 
#' @param name chr the name of the dataset to read. The choices are 
#'   `obis` to read the orginal OBIS data filter by date
#'   `thinned_obs` for the thinned observations
#'   `model_input` for the model input (presences and background)
#' @param path chr the data path
#' @return an sf object
read_obs = function(name = "obs_thinned", path = file.path("data/obs")){
  if (name == "obis") {
    obs = read_obis() |>
      dplyr::filter(date >= as.Date("2000-01-01"))
  } else {
    filename = file.path(path, paste0(name[1], ".gpkg"))
    obs = sf::read_sf(filename)
  }
  if (tolower(name[1]) == "model_input"){
    obs = dplyr::mutate(obs, class = factor(class)) |>
      sf::st_set_geometry("geometry")
  }
  
  obs
}
