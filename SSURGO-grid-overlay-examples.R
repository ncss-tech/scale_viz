library(soilDB)
library(sp)
library(sf)
library(mapview)

# new interface to leaflet 
# https://github.com/r-spatial/leafem
library(leafem)

source('local-functions.R')



## TODO
# sf methods are strange...
# make a shiny app that can generate nested grids at a given point and overlay on SSURGO data
# use a projected CRS for non-rotated grid cells
# generate clumps of grid cells at an idealized MMU
# generate nested grids by feature


coords <- c(-119.7936, 36.7426)
aea <- '+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs'

q <- sprintf("SELECT  mupolygongeo.STIntersection( geometry::STPointFromText('POINT(%f %f)', 4326).STBuffer(0.1).STEnvelope() ) AS geom, mukey
               FROM mupolygon
               WHERE mupolygongeo.STIntersects( geometry::STPointFromText('POINT(%f %f)', 4326).STBuffer(0.1) ) = 1
             ;", coords[1], coords[2], coords[1], coords[2])

# query / convert to SPDF
x <- SDA_query(q)
x <- processSDA_WKT(x)

# convert to sf class object and transform to CONUS AEA
x <- st_as_sf(x)
x <- st_transform(x, aea)

# convert points to sf and CONUS AEA
# coordinates from matrix -> sf
# that is annoying
p <- st_as_sf(data.frame(t(coords)), coords=c(1,2))
p <- st_set_crs(p, '+proj=longlat +datum=WGS84')
p <- st_transform(p, aea)

# # random points for vizualization
# s <- st_sample(x, size=50)

### ... why doesn't this work as expected?
# # get index to intersecting polygons
# idx <- which(lengths(st_intersects(x, s)) > 1)
# 
# # generate grid from these polygons
# # g.800 <- st_make_grid((x[idx,]), cellsize = 800)
# 
# makeGridByFeature <- function(i, size) {
#   st_make_grid(x[i, ], cellsize = size)
# }
# 
# g <- lapply(idx, makeGridByFeature, size=800)
# g.800 <- do.call('c', g)


## note: 30, 90, 270, 810 cell sizes can be nested



# ~ 113 ac. polygon near Fresno State
idx <- 26

# ~ 34 ac. polygon
idx <- 629
ng <- makeNestedGrids(x[idx, ])

# cols <- viridis::viridis(5)
# 
# par(bg=grey(0.65))
# plot(st_geometry(x[idx,]), border='white')
# plot(st_geometry(ng[['10m']]), add=TRUE, border=cols[1])
# plot(st_geometry(ng[['30m']]), add=TRUE, border=cols[2])
# plot(st_geometry(ng[['90m']]), add=TRUE, border=cols[3], lwd=1)
# plot(st_geometry(ng[['270m']]), add=TRUE, border=cols[4], lwd=3)
# plot(st_geometry(ng[['810m']]), add=TRUE, border=cols[5], lwd=3)



# ## TODO: pick a single delineation, or iterate over a selection of delineations
# # full grid from bbox of SSURGO
# # not very efficient for demonstrations
# g.30 <- st_make_grid(x, cellsize = 30)
# g.90 <- st_make_grid(x, cellsize = 90)
# g.270 <- st_make_grid(x, cellsize = 270)
# g.800 <- st_make_grid(x, cellsize = 810)
# 
# 
# ## indexing via spatial intersection, not very efficient nor intuitive
# idx.30 <- which(lengths(st_intersects(g.30, st_buffer(s, 50))) > 0)
# idx.90 <- which(lengths(st_intersects(g.90, st_buffer(s, 100))) > 0)
# idx.270 <- which(lengths(st_intersects(g.270, st_buffer(s, 100))) > 0)
# idx.800 <- which(lengths(st_intersects(g.800, st_buffer(s, 100))) > 0)
# 

# par(mar=c(1,1,1,1))
# 
# plot(st_geometry(x[idx, ]))
# plot(st_geometry(s), pch=16, col='firebrick', cex=0.5, add=TRUE)
# plot(st_geometry(g.800[idx.800]), col=NA, border='royalblue', lty=1, add=TRUE)
# plot(st_geometry(g.270[idx.270]), col=NA, border='orange', lty=1, add=TRUE)
# plot(st_geometry(g.90[idx.90]), col=NA, border='yellow', lty=1, add=TRUE)
# plot(st_geometry(g.30[idx.30]), col=NA, border='white', lty=1, add=TRUE)

# mapview(x)

mv <- mapview(x[idx, ], fill=FALSE, lwd=3, color='black', legend=FALSE, highlight=FALSE)
mv <- addFeatures(map = mv, data = st_transform(ng[['810m']], '+proj=longlat +datum=WGS84'), color='royalblue', fill=FALSE, weight=4)
mv <- addFeatures(map = mv, data = st_transform(ng[['270m']], '+proj=longlat +datum=WGS84'), color='firebrick', fill=FALSE, weight=3)
mv <- addFeatures(map = mv, data = st_transform(ng[['90m']], '+proj=longlat +datum=WGS84'), color='orange', fill=FALSE, weight=2)
mv <- addFeatures(map = mv, data = st_transform(ng[['30m']], '+proj=longlat +datum=WGS84'), color='yellow', fill=FALSE, weight=1)
mv <- addFeatures(map = mv, data = st_transform(ng[['10m']], '+proj=longlat +datum=WGS84'), color='white', fill=TRUE, weight=1)

mv
