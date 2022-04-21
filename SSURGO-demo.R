require(sp)
require(sf)
require(rgdal)
require(rgeos)
require(mapview)
require(leafem)
require(soilDB)

source('local-functions.R')

# scale is 1:24000
ssa.scale <- 24000

# an example point
p <- SpatialPoints(cbind(-91.3166, 38.4351), proj4string = CRS('+proj=longlat +datum=WGS84'))

# take a peek in SoilWeb
ZoomToSoilWebGmap(p)

# get MU data for this point and within 0.02 degree radius
context.search <- gBuffer(p, width=0.02)
s.context <- SDA_spatialQuery(context.search, what = 'geom')
s <- SDA_spatialQuery(p, what = 'geom')

# quick graphical check
par(mar=c(0,0,0,0))
plot(s.context)
plot(s, add=TRUE, lwd=2)
points(p, col='red', pch=16)
plot(context.search, border='royalblue', add=TRUE, col=NA, lwd=2)
box()


# convert to PCS for accurate buffering in meters
s <- spTransform(s, CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))

# compute area in acres
s$area_ac <- round(gArea(s) * 0.000247105)

# make example line width on the ground
s.buff <- MU_line_example(s, ssa.scale)

# make 3 MMU examples: 2, 5, 8 ac.
mmu.ex <- MMU_example(s, mmu = c(2,5,8), n=3)

# make nested grids
ng <- makeNestedGrids(st_as_sf(s))

## todo: better map styling
# https://environmentalinformatics-marburg.github.io/mapview/advanced/advanced.html

# interactive plot
mv <- mapview(s.context, fill=FALSE, lwd=1, color='grey', legend=FALSE)
mv <- addFeatures(map = mv, data = spTransform(s, CRS('+proj=longlat +datum=WGS84')), color='yellow', fill=FALSE, lwd=1)
mv <- addFeatures(map = mv, data = spTransform(s.buff, CRS('+proj=longlat +datum=WGS84')), color='black', fillColor='royalblue', weight=1, fillOpacity=0.2, labelOptions=list(interactive=TRUE), label=sprintf('example line width at 1:%s scale', ssa.scale))
mv <- addFeatures(map = mv, data = spTransform(mmu.ex, CRS('+proj=longlat +datum=WGS84')), color='black', fillColor='royalblue', weight=1, fillOpacity=0.5, labelOptions=list(interactive=TRUE), label=sprintf("example MMU\n\n%s ac.", mmu.ex$mmu_ac))

# mv

mv <- addFeatures(map = mv, data = st_transform(ng[['810m']], '+proj=longlat +datum=WGS84'), color='royalblue', fill=FALSE, weight=4)
mv <- addFeatures(map = mv, data = st_transform(ng[['270m']], '+proj=longlat +datum=WGS84'), color='firebrick', fill=FALSE, weight=3)
mv <- addFeatures(map = mv, data = st_transform(ng[['90m']], '+proj=longlat +datum=WGS84'), color='orange', fill=FALSE, weight=2)
mv <- addFeatures(map = mv, data = st_transform(ng[['30m']], '+proj=longlat +datum=WGS84'), color='yellow', fill=FALSE, weight=1)
mv <- addFeatures(map = mv, data = st_transform(ng[['10m']], '+proj=longlat +datum=WGS84'), color='white', fill=TRUE, weight=1)

mv

