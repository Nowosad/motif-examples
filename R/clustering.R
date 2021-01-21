library(sf)
library(stars)
library(motif)
library(tmap)
library(dplyr)
library(readr)

lc = read_stars("data/land_cover.tif")
lf = read_stars("data/landform.tif")

lc_palette_df = read_csv("data/lc_palette.csv")
lf_palette_df = read_csv("data/lf_palette.csv")
names(lc_palette_df$color) = lc_palette_df$value
names(lf_palette_df$color) = lf_palette_df$value

tm_lc = tm_shape(lc) +
        tm_raster(style = "cat",
                  palette = lc_palette_df$color,
                  labels = lc_palette_df$label,
                  title = "Land cover:") +
        tm_layout(legend.position = c("LEFT", "BOTTOM"),
                  legend.title.size = 0.8,
                  legend.text.size = 0.55,
                  inner.margins = c(0, 0, 0, 0),
                  legend.width = 1)

tm_lf = tm_shape(lf) +
        tm_raster(style = "cat",
                  palette = lf_palette_df$color,
                  labels = lf_palette_df$label,
                  title = "Landform:") +
        tm_layout(legend.position = c("LEFT", "BOTTOM"),
                  legend.text.size = 0.7,
                  # inner.margins = c(0.1, 0.1, 0, 0),
                  legend.width = 1)

tm_lf_no_legend = tm_lf + tm_layout(legend.show = FALSE)
tm_lf_legend = tm_lf + tm_layout(legend.only = TRUE)

tm_cluster1 = tmap_arrange(tm_lc, tm_lf_no_legend, tm_lf_legend, nrow = 1)
tm_cluster1

eco_data = c(lc, lf)

# it takes a few minutes
eco_signature = lsp_signature(eco_data,
                              type = "incove",
                              window = 300,
                              normalization = "pdf")

# it takes a few minutes
eco_dist = lsp_to_dist(eco_signature, dist_fun = "jensen-shannon")

eco_hclust = hclust(eco_dist, method = "ward.D2")
plot(eco_hclust)

clusters = cutree(eco_hclust, k = 8)

eco_grid_sf = lsp_add_clusters(eco_signature,
                               clusters)

eco_grid_sf2 = eco_grid_sf %>%
        dplyr::group_by(clust) %>%
        dplyr::summarize()

tm_clu = tm_shape(eco_grid_sf2) +
        tm_polygons("clust", style = "cat", palette = "Set2", title = "Cluster:") +
        tm_layout(legend.position = c("LEFT", "BOTTOM"))
tm_clu

tm_plot = function(x, title){
        tm_shape(x) +
                tm_raster(style = "cat", palette = list(lc_palette_df$color, lf_palette_df$color)) +
                tm_layout(legend.show = FALSE, title = title, title.position = c("LEFT", "TOP"))
}

# 1
eco_2723 = lsp_extract(eco_data, window = 300, id = 2723)

# 2
eco_2188 = lsp_extract(eco_data, window = 300, id = 2188)

# 3
eco_2599 = lsp_extract(eco_data, window = 300, id = 2599)

# 4
eco_1441 = lsp_extract(eco_data, window = 300, id = 1441)

# 5
eco_1514 = lsp_extract(eco_data, window = 300, id = 1514)

# 6
eco_3780 = lsp_extract(eco_data, window = 300, id = 3780)

# 7
eco_2782 = lsp_extract(eco_data, window = 300, id = 2782)

# 8
eco_3286 = lsp_extract(eco_data, window = 300, id = 3286)

tmap_arrange(
        tm_plot(eco_2723, "1"),
        tm_plot(eco_2188, "2"),
        tm_plot(eco_2599, "3"),
        tm_plot(eco_1441, "4"),
        tm_plot(eco_1514, "5"),
        tm_plot(eco_3780, "6"),
        tm_plot(eco_2782, "7"),
        tm_plot(eco_3286, "8")
)

eco_grid_sfq = lsp_add_quality(eco_grid_sf, eco_dist, type = "segmentation")
eco_grid_sfq2 = eco_grid_sfq %>%
        group_by(clust) %>%
        summarise(inhomogeneity = mean(inhomogeneity),
                  isolation = mean(isolation),
                  quality = mean(quality))

tm_inh = tm_shape(eco_grid_sfq2) +
        tm_polygons("inhomogeneity", style = "cont", palette = "magma")

tm_iso = tm_shape(eco_grid_sfq2) +
        tm_polygons("isolation", style = "cont", palette = "-inferno")

tm_qua = tm_shape(eco_grid_sfq2) +
        tm_polygons("quality", style = "cont", palette = "Greens")

tm_cluster3 = tmap_arrange(tm_clu, tm_qua, tm_inh, tm_iso, ncol = 2)
tm_cluster3
