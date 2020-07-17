library(motif)
library(stars)
library(tmap)

landcover = read_stars(system.file("raster/landcover2015.tif", package = "motif"))

tm_ss1 = tm_shape(landcover) +
        tm_raster(drop.levels = TRUE, title = "Land cover:") +
        tm_layout(legend.position = c(0.05, 0))

tm_ss1

landcover_grid = lsp_add_sf(landcover, window = 200)

tm_ss2 = tm_shape(landcover) +
        tm_raster(drop.levels = TRUE, title = "Land cover:",
                  labels = c("agriculture", "forest", "grassland",
                             "settlement", "shrubland", "sparse vegetation", "water")) +
        tm_shape(landcover_grid) +
        tm_borders(col = "black") +
        tm_layout(legend.outside = TRUE, outer.margins = 0.01)

tm_ss2

coma_output = lsp_signature(landcover, type = "coma", window = 200)
coma_output

coma_output$signature[[1]]

ecoregions = read_sf(system.file("vector/ecoregions.gpkg", package = "motif"))
landcover_coma_e = lsp_signature(landcover, type = "coma", window = ecoregions["id"])
landcover_coma_e

tm_ss3 = tm_shape(landcover) +
        tm_raster(drop.levels = TRUE, title = "Land cover:",
                  labels = c("agriculture", "forest", "grassland",
                             "settlement", "shrubland", "sparse vegetation", "water")) +
        tm_shape(ecoregions) +
        tm_borders(col = "black") +
        tm_layout(legend.outside = TRUE, outer.margins = 0.01)

tm_ss3
