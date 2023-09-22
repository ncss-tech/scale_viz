# load libraries
require(terra)
require(mapview)
require(soilDB)


source('local-functions.R')

scaleToLineWidth(c(12000, 24000, 65000, 250000, 5e6), units = 'f')
scaleToLineWidth(c(12000, 24000, 65000, 250000, 5e6), units = 'm')


## TODO: update to terra

# 
# # MLRA boundary polygons
# m <- readOGR(dsn="L:/NRCS/MLRAShared/Geodata/Boundaries/MLRAs/MLRAs_17_18_22A", layer = "MLRA_17_18_and_22A")
# 
# # mlra 18
# m <- m[1, ]
# 
# 
# # buffer to scale of MLRA line
# # 1mm line width = ~ 4.6mi @ 1:7.5 million scale
# # 4.6mi = 7500m
# b.outer <- gBuffer(m, width=7500/2, byid=TRUE)
# b.inner <- gBuffer(m, width=-(7500/2), byid=TRUE)
# 
# # compute the difference together
# b <- gDifference(b.outer, b.inner, byid=TRUE)
# 
# # upgrade to SPDF
# b <- SpatialPolygonsDataFrame(b, data=data.frame(ID=1), match.ID=FALSE)
# 
# # get some SSURGO for sense of scale
# q <- "SELECT G.MupolygonWktWgs84 as geom, '461845' as mukey from SDA_Get_MupolygonWktWgs84_from_Mukey('461845') as G"
# res <- SDA_query(q)
# s <- processSDA_WKT(res)
# 
# mv <- mapview(m, fill=FALSE, lwd=2)
# mv <- addFeatures(map = mv, data = spTransform(b, CRS('+proj=longlat +datum=WGS84')), color='black', fillColor='firebrick', weight=1, fillOpacity=0.1)
# addFeatures(map = mv, data = s, color='royalblue', fill=FALSE, weight=1)
# 
# # save as SHP
# writeOGR(b, dsn="L:/Geodata/Boundaries", layer="mlra_18_real_width", overwrite_layer=TRUE, driver='ESRI Shapefile')

