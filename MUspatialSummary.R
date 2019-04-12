library(soilDB)
library(mapview)
library(latticeExtra)
library(tactile)

## TODO: figure out how to compute fractal dimension: sql server syntax is stoopid
# ( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd

MUspatialSummary <- function(m, method='union') {
  
  # implicit vectorization via IN statement
  m.in.st <- format_SQL_in_statement(m)
  
  if( ! method %in% c('union', 'bbox')) {
    stop('method must be one of `union` or `bbox`', call. = FALSE)
  }
  
  
  # get union of polygon envelopes (BBOX)
  if(method == 'union') {
    q <- sprintf("SELECT 
  geometry::UnionAggregate(mupolygongeo.STEnvelope()).STAsText() AS geom, muname, P.mukey AS mukey
  -- , MuPolygonKey
  FROM mupolygon AS P
  INNER JOIN mapunit AS M ON P.mukey = M.mukey
  WHERE P.mukey IN %s 
  GROUP BY P.mukey, muname
  ;", m.in.st)
  }
  
  # get all polygon envelopes (BBOX)
  if(method == 'bbox') {
    q <- sprintf("SELECT 
geom, muname, mukind, mukey, MuPolygonKey, 
( 2.0 * LOG(0.25 * mupolygonGeography.STLength()) ) / LOG(mupolygonGeography.STArea() ) AS fd,
mupolygonGeography.STArea() * 0.000247105 as area_ac
FROM
(
  SELECT
  mupolygongeo.STEnvelope().STAsText() AS geom, 
  muname, mukind, P.mukey AS mukey, MuPolygonKey,
  GEOGRAPHY::STGeomFromWKB(
    (P.mupolygongeo.STUnion(mupolygongeo.STStartPoint()).STAsBinary()), 
    4326) as mupolygonGeography
  from mupolygon AS P 
  INNER JOIN mapunit AS M ON P.mukey = M.mukey 
  WHERE P.mukey IN %s
) AS subselect
;", m.in.st)
  }
  
  # get the results quietly
  res <- suppressMessages(SDA_query(q))
  
  # safely deal with NULL
  if(is.null(res))
    return(NULL)
  
  # convert to sp object
  s <- processSDA_WKT(res)
  
  return(s)
}


## CA630: 1:24,000
# table mtn
mukey <- '1865918'

## CA095: 1:24,000
# Yolo loam, 0 to 4 percent slopes, MLRA 17
mukey <- '456137'

## IA021: 1:16,000
# Canisteo clay loam, 0 to 2 percent slopes
mukey <- '403308'

## IN053: 1:15,840
# Blount silt loam, ground moraine, 0 to 2 percent slopes
mukey <- '162786'




# # extensive MU at land/water interface
# mukey <- '808530'
# # extensive WATER MU
# mukey <- '808535'


mu.union <- MUspatialSummary(mukey, method = 'union')
mu.bbox <- MUspatialSummary(mukey, method = 'bbox')


mapview(mu.union, fill=NA, color='firebrick', lwd=2, legend=FALSE)

mapview(mu.bbox, fill='royalblue', alpha.regions=0.25, color='black', lwd=0.5, legend=FALSE)

mapview(mu.bbox, zcol='fd', alpha.regions=0.25, color='black', lwd=0.5)



mu.summary <- MUspatialSummary(c('1865918', '456137', '162786', '403308'), method='bbox')

d <- mu.summary@data
d$muname_abbv <- gsub(', ', '\n', d$muname)
d$muname_abbv <- paste(d$muname_abbv, d$mukind, sep = '\n')

bwplot(muname_abbv ~ fd, data=d, 
       scales=list(alternating=3, y=list(cex=0.75), x=list(tick.number=10)), 
       xlab='Fractal Dimension (polygon complexity)', box.ratio=0.75,
       par.settings=tactile.theme,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })

bwplot(muname_abbv ~ area_ac, data=d, 
       scales=list(alternating=3, y=list(cex=0.75), x=list(log=10, tick.number=20)), 
       xscale.components=xscale.components.log10ticks, 
       xlab='Delineation Area (ac.)', box.ratio=0.75,
       par.settings=tactile.theme,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })


xyplot(fd ~ area_ac, groups=muname_abbv, data=d, 
       scales=list(y=list(cex=0.75, tick.number=10), x=list(log=10, tick.number=20)), 
       xscale.components=xscale.components.log10ticks, 
       ylab='Fractal Dimension (polygon complexity)', xlab='Delineation Area (ac.)', 
       par.settings=tactile.theme(superpose.symbol=list(col=brewer.pal(4, 'Set1'), pch=16, alpha=0.5, cex=0.5)),
       auto.key=list(columns=4, lines=FALSE, points=TRUE, cex=0.75), 
       panel=function(...) {
         panel.grid(-1, -1)
         panel.xyplot(...)
       })



## does this make any sense?


pr <- princomp(cbind(d$fd, log(d$area_ac)), cor = TRUE)
pc <- predict(pr)
d$pc1 <- pc[, 1]
d$pc2 <- pc[, 2]


xyplot(pc1 ~ pc2, groups=muname_abbv, data=d, 
       scales=list(y=list(cex=0.75, tick.number=10), x=list(tick.number=10)), 
       # xscale.components=xscale.components.log10ticks, 
       ylab='PC2', xlab='PC1', 
       par.settings=tactile.theme(superpose.symbol=list(col=brewer.pal(4, 'Set1'), pch=16, alpha=0.5, cex=0.5)),
       auto.key=list(columns=4, lines=FALSE, points=TRUE, cex=0.75), 
       panel=function(...) {
         panel.grid(-1, -1)
         panel.xyplot(...)
       })




densityplot(~ pc1, groups=muname_abbv, data=d, pch=NA, bw=0.3,
            scales=list(x=list(tick.number=10)), 
            par.settings=tactile.theme(superpose.line=list(col=brewer.pal(4, 'Set1'), lwd=2)),
            auto.key=list(columns=4, lines=TRUE, points=FALSE, cex=0.75), 
            panel=function(...) {
              panel.grid(-1, -1)
              panel.densityplot(...)
            })
