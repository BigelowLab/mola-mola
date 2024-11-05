#' Install one or more packages from github
#' 
#' @param repos chr one or more package IDs in the form of 
#'   "package" = "namespace" such as c("charlier = BigelowLab")
#' @return invisible NULL
install_from_github = function(repos = c("charlier" = "BigelowLab",
                                         "maxnet" = "BigelowLab",
                                         "maxnetic" = "BigelowLab",
                                         "oisster" = "BigelowLab",
                                         "nbs" = "BigelowLab")){

  for (name in names(repos)) {
    remotes::install_github(sprintf("%s/%s", repos[name], name))
  }
invisible(NULL)
}

#' Alphabetize sessionInfo() packages
#' 
#' @param ... other arguments for sessionInfo
#' @return a sessionInfo object with packages alphabetized
session_info = function(...){
  si = sessionInfo(...)
  nm = names(si$otherPkgs)
  si$otherPkgs = si$otherPkgs[sort(nm)]
  nm = names(si$loadedOnly)
  si$loadedOnly = si$loadedOnly[sort(nm)]
  si
}