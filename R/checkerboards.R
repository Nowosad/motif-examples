library(motif)
library(stars)

checkerboard1 = matrix(rep(c(1, 0), 961), nrow = 31, ncol = 31)
checkerboard1 = st_as_stars(checkerboard1)
plot(checkerboard1)

checkerboard2 = matrix(rep(c(0, 1), 961), nrow = 31, ncol = 31)
checkerboard2 = st_as_stars(checkerboard2)
plot(checkerboard2)

lsp_signature(checkerboard1, type = "coma")$signature[[1]]
lsp_signature(checkerboard2, type = "coma")$signature[[1]]
