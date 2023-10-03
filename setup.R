installed = rownames(installed.packages())

cran_packages = c("remotes", "fs",
                  "rlang", "here", 
                  "maxnet", 
                  "sf", "stars",
                  "smoothr",
                  "ggOceanMaps", "robis",
                  "rnaturalearth", "rnaturalearthdata", 
                  "ggplot2", "patchwork",
                  "tidyr", "readr", "dplyr")
ix = cran_packages %in% installed
for (package in cran_packages[!ix]) {
  install.packages(package)
}


github_packages = c("charlier" = "BigelowLab",
                    "maxnet" = "BigelowLab",
                    "maxnetic" = "BigelowLab",
                    "oisster" = "BigelowLab",
                    "nbs" = "BigelowLab")
ix = names(github_packages) %in% installed
for (package in names(github_packages[!ix])) {
  remotes::install_github(sprintf("%s/%s", github_packages[package], package))
}

if (!("rnaturalearthhires" %in% installed)) {
  install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
}


suppressPackageStartupMessages({
  for (package in cran_packages) library(package, character.only = TRUE)
  for (package in names(github_packages)) library(package, character.only = TRUE)
  library(rnaturalearthhires)
})

for (f in list.files("functions", full.names = TRUE)) source(f, echo = FALSE)