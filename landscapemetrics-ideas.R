library(landscapemetrics)
library(landscapetools)
library(NLMR)


x <- nlm_mpd(ncol = 200, nrow = 200, roughness = 0.75, rand_dev = 0.5)
show_landscape(x)

