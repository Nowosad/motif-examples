library(stars)
library(motif)
library(tmap)
library(readr)

lc92 = read_stars("data/lc_am_1992.tif")
lc18 = read_stars("data/lc_am_2018.tif")

lc_palette_df = read.csv("data/lc_palette.csv")
names(lc_palette_df$color) = lc_palette_df$value

tm_compare1 = tm_shape(c(lc92, lc18)) +
        tm_raster(style = "cat",
                  palette = lc_palette_df$color,
                  labels = lc_palette_df$label,
                  title = "Land cover:") +
        tm_facets(nrow = 1) +
        tm_layout(legend.outside = TRUE,
                  panel.labels = c(1992, 2018))
tm_compare1

lc_am_compare = lsp_compare(lc92, lc18, type = "cove", window = 300,
                            dist_fun = "jensen-shannon")

tm_compare2 = tm_shape(lc_am_compare) +
        tm_raster("dist", palette = "viridis", style = "cont",
                  title = "Distance (JSD):", legend.is.portrait = FALSE) +
        tm_layout(legend.outside.position = "bottom",
                  legend.outside = TRUE)
tm_compare2

lc_am_compare_sel = st_as_sf(lc_am_compare) %>%
        subset(dist > 0.18)
lc_am_compare_sel = lc_am_compare_sel[order(lc_am_compare_sel$dist,
                                            decreasing = TRUE), ]
lc_am_compare_sel

lc_am_compare_1032 = lsp_extract(c(lc92, lc18), window = 300, id = 1032)
lc_am_compare_1120 = lsp_extract(c(lc92, lc18), window = 300, id = 1120)
lc_am_compare_827 = lsp_extract(c(lc92, lc18), window = 300, id = 827)
lc_am_compare_784 = lsp_extract(c(lc92, lc18), window = 300, id = 784)
lc_am_compare_1041 = lsp_extract(c(lc92, lc18), window = 300, id = 1041)
lc_am_compare_1121 = lsp_extract(c(lc92, lc18), window = 300, id = 1121)
lc_am_compare_826 = lsp_extract(c(lc92, lc18), window = 300, id = 826)

tm_plot = function(x){
        tm_shape(x) +
                tm_raster(style = "cat",
                          palette = lc_palette_df$color,
                          labels = lc_palette_df$label,
                          title = "Land cover:") +
                tm_facets(ncol = 2) +
                tm_layout(legend.show = FALSE,
                          panel.labels = c(1992, 2018))
}

tm1 = tm_plot(lc_am_compare_1032)
tm2 = tm_plot(lc_am_compare_1120)
tm3 = tm_plot(lc_am_compare_827)
tm4 = tm_plot(lc_am_compare_784)
tm5 = tm_plot(lc_am_compare_1041)
tm6 = tm_plot(lc_am_compare_1121)
tm7 = tm_plot(lc_am_compare_826)

tmap_arrange(tm1, tm2, tm3, tm4, tm5, tm6, tm7)
