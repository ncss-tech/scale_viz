library(soilDB)
library(aqp)
library(sharpshootR)
library(terra)
library(purrr)




# example point, WGS84 coordinates
p <- vect('POINT(-90.42109 38.89624)', crs = 'epsg:4326')


# 500m buffer applied to point defined in geographic coordinates
b <- buffer(p, width = 8000)

# convert to BBOX
bb <- as.polygons(ext(b), crs = 'epsg:4326')

# result is a SpatVector
mu <- SDA_spatialQuery(bb, what = 'mupolygon', geomIntersection = TRUE)

mu <- project(mu, 'epsg:5070')


# simple map
plot(mu, axes = FALSE)
points(project(p, mu), pch = 16, col = 2, cex = 1)


## TODO: fix this, something isn't right, could be multi-part features

resolution <- function(p, D = 2) {
  
  # dump coordinates for all features
  .xy <- geom(p, df = TRUE) 
  .a <- expanse(p)
  
  # ignore holes and multi-part features for now
  .idx <- which(.xy$hole == 0 & .xy$part == 1)
  .xy <- .xy[.idx, ]
  .a <- .a[.idx]
  
  # iterate over features
  .xy <- split(.xy, .xy$geom)
  
  .res <- map_dbl(seq_along(.xy), .f = function(i) {
    
    
    # first/last point is repeated
    .pts <- nrow(.xy[[i]]) - 1
    
    # avg spatial resolution
    # Tobler, 1987
    (.a[i] / .pts) ^ (1/D)
  })
  
  
  
  return(.res)
}


mu$res <- log(resolution(mu))

par(mar = c(4.5, 4, 3, 1))
hist(mu$res, las = 1, xlab = 'Average Spatial Dimension', breaks = 30)


r <- rasterize(mu, rast(mu, res = 30), field = 'res')

plot(r, col = hcl.colors(100), axes = FALSE, breakby = 'cases')
lines(mu)

