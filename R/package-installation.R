install.packages("remotes")
pkgs = c("stars", "sf", "tmap",
         "dplyr", "readr", "osfr")
remotes::install_cran(pkgs)
remotes::install_github("nowosad/motif")

