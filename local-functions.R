
## TODO: figure out how to compute fractal dimension: sql server syntax is stoopid
# ( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd

#' @param x sf object or SpatialPolygonsDataFrame, must be in PCS with units of meters
fractalDimension <- function(x) {
  
  # area and perimeter for fractal dimension
  p <- lwgeom::st_perimeter(x)
  a <- st_area(x)
  
  # fractal dimension
  # http://www.umass.edu/landeco/research/fragstats/documents/Metrics/Shape%20Metrics/Metrics/P9%20-%20FRAC.htm
  fd <- ( 2.0 * log(0.25 * p) ) / ( log(a) ) 
  
  # drop units
  return(as.vector(fd))
}


#' @param m vector of map unit keys
#' @param method grouping criteria
#' 
#' @return SpatialPolygonsDataFrame
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



#' @param z ideally a single delineation to keep grids small, must be in a PCS with units of meters
#' 
#' @return list of geometry sets represented nested grids
makeNestedGrids <- function(z) {
  g.10 <- st_make_grid(z, cellsize = 10)
  g.30 <- st_make_grid(z, cellsize = 30)
  g.90 <- st_make_grid(z, cellsize = 90)
  g.270 <- st_make_grid(z, cellsize = 270)
  g.810 <- st_make_grid(z, cellsize = 810)
  
  # generate a point for the grid "seed"
  s <- st_sample(z, size=1)
  
  # intersect grid "seed" at various search distances
  idx.10 <- which(lengths(st_intersects(g.10, st_buffer(s, 10))) > 0)
  idx.30 <- which(lengths(st_intersects(g.30, st_buffer(s, 25))) > 0)
  idx.90 <- which(lengths(st_intersects(g.90, st_buffer(s, 50))) > 0)
  idx.270 <- which(lengths(st_intersects(g.270, st_buffer(s, 100))) > 0)
  idx.810 <- which(lengths(st_intersects(g.810, st_buffer(s, 100))) > 0)
  
  # return intersecting, nested grid cells 
  # note: indexing records in an sf object without attributes is a little strange
  # g.XX are geometry sets vs. simple feature collections
  return(
    list(
      `10m`=g.10[idx.10],
      `30m`=g.30[idx.30],
      `90m`=g.90[idx.90], 
      `270m`=g.270[idx.270],
      `810m`=g.810[idx.810]
    )
  )
}

#' @param s map scale (e.g. 1:24,000 -> s=24000)
#' @param lw line width on the map in mm
#' 
#' @return real-world width of line on the ground in meters, based on an assumed 1mm line width on the map
scaleToLineWidth <- function(s, lw=1, units='m') {
  sf <- switch(units, m=0.001, f=0.003280839895, mi=6.2137e-7)
  # scale factor * line width * unit conversion factor
  res <- s * lw *  sf
  return(res)
}


#' @param mu map unit polygon to buffer according to real world width of line and map scale
#' @param s map scale (e.g. 1:24,000 -> s=24000)
#' @param ... arguments passed to scaleToLineWidth
#' 
#' @return buffered polygon
MU_line_example <- function(mu, s, ...) {
  # create real-world line width
  w <- scaleToLineWidth(s, ...)
  b.outer <- gBuffer(mu, width=w/2, byid=TRUE)
  b.inner <- gBuffer(mu, width=-(w/2), byid=TRUE)
  
  # compute the difference together
  b <- gDifference(b.outer, b.inner, byid=TRUE)
  
  # upgrade to SPDF
  b <- SpatialPolygonsDataFrame(b, data=data.frame(ID=1:length(b)), match.ID=FALSE)
  
  return(b)
}


#' @param p SpatialPoints object with point seends for MMU examples
#' @param mmu MMU size in acres
#' @param n number of randomly located MMU examples
#' @param ... arguments passed to spsample
#' 
#' @return SpatialPolgons object with area (sq. meters) that is very close to original MMU
MMU_example <- function(p, mmu, n=1, type='random', ...) {
  area_sqm <- mmu * 4046.8564224
  radius <- sqrt(area_sqm / pi)
  
  p.samples <- spsample(p, n=n, type=type, ...)
  res <- gBuffer(p.samples, byid=TRUE, width=radius, quadsegs = 32)
  
  res <- SpatialPolygonsDataFrame(res, data=data.frame(id=1:length(res), mmu_ac=mmu), match.ID = FALSE)
  return(res)
}


#' @geom an SP object with coordinates
ZoomToSoilWebGmap <- function(geom) {
  
  # sanity check, need GCS, WGS84
  if(is.projected(geom)) {
    p <- spTransform(geom, '+proj=longlat +datum=WGS84')
  }
  
  # some point within geom
  xy <- coordinates(gPointOnSurface(geom))
  
  # this will get you there
  url <- sprintf("https://casoilresource.lawr.ucdavis.edu/gmap/?loc=%s,%s", xy[, 2], xy[, 1])
  
  # init new browser window/tab
  browseURL(url)
}

