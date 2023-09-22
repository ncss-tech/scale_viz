library(tactile)
library(latticeExtra)
library(sf)
library(soilDB)

## TODO: 
## * update to sf/terra
## * use local copies, sourced via soilDB / sqlite 

# current CA792 linework
# already in PCS
CA792 <- read_sf(dsn='L:/NRCS/MLRAShared/CA792/ca792_spatial/FG_CA792_OFFICIAL.gdb', layer='ca792_a', encoding='OpenFileGDB', stringsAsFactors = FALSE)

# already in PCS
CA630 <- read_sf(dsn='L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb', layer='ca630_a', encoding='OpenFileGDB', stringsAsFactors = FALSE)

# GCS
CA790 <- readOGR(dsn='L:/NRCS/MLRAShared/CA790/SSURGO/CA790/spatial', layer='soilmu_a_ca790', stringsAsFactors = FALSE)
CA790 <- spTransform(CA790, CRS('+proj=utm +zone=11 +datum=NAD83'))

# GCS
MO071 <- read_sf(dsn='E:/gis_data/SSURGO-SSA/MO071/spatial', layer='soilmu_a_mo071', stringsAsFactors = FALSE)
MO071 <- spTransform(MO071,  CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0'))

# remove water
idx <- grep('w', CA792$MUSYM, ignore.case = TRUE, invert = TRUE)
CA792 <- CA792[idx, ]

# remove water and dams
idx <- grep('w|dam', CA630$MUSYM, ignore.case = TRUE, invert = TRUE)
CA630 <- CA630[idx, ]

# remove water and dams
idx <- grep('w|dam', CA790$MUSYM, ignore.case = TRUE, invert = TRUE)
CA790 <- CA790[idx, ]

# remove water and dams
idx <- grep('w|dam', MO071$MUSYM, ignore.case = TRUE, invert = TRUE)
MO071 <- MO071[idx, ]

# MO071 contains some other non-soil delineations
# this is rather slow, would be faster to query by areasymbol directly
# or, use the map unit table from file
is <- format_SQL_in_statement(MO071$MUKEY)
qry <- sprintf("SELECT mukey, muname from mapunit where mukey IN %s ;", is)
res <- SDA_query(qry)
names(res)[1] <- 'MUKEY'
MO071 <- merge(MO071, res, by='MUKEY')

# remove additional water, dumps, pits
idx <- grep('water|dumps|pits', MO071$muname, ignore.case = TRUE, invert = TRUE)
MO071 <- MO071[idx, ]


## TODO: simplfy all of this
# compute area
CA792.ac <- sapply(CA792@polygons, slot, 'area') * 0.000247105
CA630.ac <- sapply(CA630@polygons, slot, 'area') * 0.000247105
CA790.ac <- sapply(CA790@polygons, slot, 'area') * 0.000247105
MO071.ac <- sapply(MO071@polygons, slot, 'area') * 0.000247105

# fractal dimension
CA792.fd <- fractalDimension(st_as_sf(CA792))
CA630.fd <- fractalDimension(st_as_sf(CA630))
CA790.fd <- fractalDimension(st_as_sf(CA790))
MO071.fd <- fractalDimension(st_as_sf(MO071))

# stack
g <- make.groups(
  'Franklin County, Missouri (MO071)'=data.frame(area_ac=MO071.ac, fd=MO071.fd), 
  'Calaveras/Tuolumne (CA630)'=data.frame(area_ac=CA630.ac, fd=CA630.fd), 
  'Sequoia/Kings Canyon (CA792)'=data.frame(area_ac=CA792.ac, fd=CA792.fd),
  'Yosemite (CA790)'=data.frame(area_ac=CA790.ac, fd=CA790.fd)
)

png(file='SSA-comparison-ac.png', width=900, height=400, res = 90)

# there is a very tiny polygon in CA790.. sliver no doubt
bwplot(which ~ area_ac, data=g, 
       xlab='Area (acres)',
       subset=area_ac > 0.1,
       box.ratio=0.75,
       par.settings=tactile.theme, 
       scales=list(alternating=3, x=list(log=10)),
       xscale.components=xscale.components.log10.3,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
       )

dev.off()


png(file='SSA-comparison-fd.png', width=900, height=400, res = 90)

bwplot(which ~ fd, data=g, 
       xlab='Fractal Dimension (polygon complexity)',
       subset=area_ac > 0.1,
       box.ratio=0.75,
       par.settings=tactile.theme, 
       scales=list(alternating=3, y=list(cex=0.75), x=list(tick.number=10)), 
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
)

dev.off()


plyr::ldply(tapply(g$area_ac, g$which, quantile), round)



