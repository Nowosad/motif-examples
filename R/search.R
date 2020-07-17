library(sf)
library(stars)
library(motif)
library(tmap)
library(grid)

gm = read_stars("data/geomorphons_pol.tif")
suw_lp = read_sf("data/suw_lp.gpkg")

gm_suw = gm[suw_lp]

tm_gm = tm_shape(gm) +
        tm_raster(title = "Geomorphons:") +
        tm_shape(suw_lp) +
        tm_symbols(col = "black", shape = 6) +
        tm_layout(legend.outside = TRUE, frame = FALSE)
tm_gm

tm_gm_suw = tm_shape(gm_suw) +
        tm_raster() +
        tm_shape(suw_lp) +
        tm_borders(col = "black") +
        tm_layout(legend.show = FALSE, frame = FALSE)
tm_gm_suw

tm_search1 = tmap_arrange(tm_gm_suw, tm_gm, nrow = 1)
tm_search1

# it takes a few minutes
gm_search = lsp_search(gm_suw, gm, type = "cove", dist = "jensen-shannon",
                       window = 100)

sel_ids = unique(gm_search$id[which(gm_search$dist < 0.00224)])
gm_search_sel = st_as_sf(gm_search) %>%
        subset(id %in% sel_ids)

gm_search_sel = gm_search_sel[order(gm_search_sel$dist, decreasing = FALSE), ]

tm_search2 = tm_shape(gm_search) +
        tm_raster("dist", style = "log10", palette = "BrBG", title = "Distance (JSD):", legend.is.portrait = FALSE) +
        tm_shape(gm_search_sel) +
        tm_symbols(shape = 2, col = "black") +
        tm_layout(legend.outside = TRUE,
                  legend.outside.position = "bottom",
                  legend.stack = "vertical")
tm_search2

gm_search_13874 = lsp_extract(gm, window = 100, id = 13874)
gm_search_7077 = lsp_extract(gm, window = 100, id = 7077)
gm_search_2239 = lsp_extract(gm, window = 100, id = 2239)
gm_search_12546 = lsp_extract(gm, window = 100, id = 12546)
gm_search_13586 = lsp_extract(gm, window = 100, id = 13586)
gm_search_11965 = lsp_extract(gm, window = 100, id = 11965)
gm_search_9955 = lsp_extract(gm, window = 100, id = 9955)
gm_search_16294 = lsp_extract(gm, window = 100, id = 16294)

tm2 = tm_shape(gm_search_13874) + tm_raster() + tm_layout(legend.show = FALSE)
tm3 = tm_shape(gm_search_7077) + tm_raster() + tm_layout(legend.show = FALSE)
tm4 = tm_shape(gm_search_2239) + tm_raster() + tm_layout(legend.show = FALSE)
tm5 = tm_shape(gm_search_12546) + tm_raster() + tm_layout(legend.show = FALSE)
tm6 = tm_shape(gm_search_13586) + tm_raster() + tm_layout(legend.show = FALSE)
tm7 = tm_shape(gm_search_11965) + tm_raster() + tm_layout(legend.show = FALSE)
tm8 = tm_shape(gm_search_9955) + tm_raster() + tm_layout(legend.show = FALSE)
tm9 = tm_shape(gm_search_16294) + tm_raster() + tm_layout(legend.show = FALSE)

tmap_arrange(tm2, tm3, tm4, tm5, tm6, tm7, tm8, tm9, ncol = 4)
