installed = rownames(installed.packages())

cran_packages = c("remotes", 
                  "fs",
                  "imager",
                  "rlang", 
                  "here", 
                  "maxnet", 
                  "sf", 
                  "stars",
                  "smoothr",
                  "ggOceanMaps", 
                  "robis",
                  "rnaturalearth", 
                  "rnaturalearthdata", 
                  "lubridate",
                  "ggplot2", 
                  "patchwork",
                  "tidyr", 
                  "readr", 
                  "tidysdm",
                  "ggplot2",
                  "dplyr")
ix = cran_packages %in% installed
for (package in cran_packages[!ix]) {
  install.packages(package)
}

github_packages = c(charlier = "BigelowLab/charlier",
                    maxnet = "BigelowLab/maxnet",
                    maxnetic = "BigelowLab/maxnetic",
                    oisster = "BigelowLab/oisster",
                    nbs = "BigelowLab/nbs")

ix = names(github_packages) %in% installed
for (package in names(github_packages[!ix])) {
  remotes::install_github(github_packages[[package]], upgrade = FALSE)
}

if (!("rnaturalearthhires" %in% installed)) {
  install.packages("rnaturalearthhires", 
                   repos = "http://packages.ropensci.org", 
                   type = "source")
}


suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
  for (package in names(github_packages)) library(package, character.only = TRUE)
  library(rnaturalearthhires)
})

for (f in list.files("functions", full.names = TRUE)) source(f, echo = FALSE)