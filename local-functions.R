

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

