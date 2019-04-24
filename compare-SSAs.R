library(tactile)
library(latticeExtra)
library(rgdal)


# current CA792 linework
CA792 <- readOGR(dsn='L:/NRCS/MLRAShared/CA792/ca792_spatial/FG_CA792_OFFICIAL.gdb', layer='ca792_a', encoding='OpenFileGDB', stringsAsFactors = FALSE)

CA630 <- readOGR(dsn='L:/NRCS/MLRAShared/CA630/Archived_OFFICIAL_DB/final/FG_CA630_GIS_2018.gdb', layer='ca630_a', encoding='OpenFileGDB', stringsAsFactors = FALSE)

CA790 <- readOGR(dsn='L:/NRCS/MLRAShared/CA790/SSURGO/CA790/spatial', layer='soilmu_a_ca790', stringsAsFactors = FALSE)
CA790 <- spTransform(CA790, CRS('+proj=utm +zone=11 +datum=NAD83'))


# remove water
idx <- grep('w', CA792$MUSYM, ignore.case = TRUE, invert = TRUE)
CA792 <- CA792[idx, ]

# remove water and dams
idx <- grep('w|dam', CA630$MUSYM, ignore.case = TRUE, invert = TRUE)
CA630 <- CA630[idx, ]

# remove water and dams
idx <- grep('w|dam', CA790$MUSYM, ignore.case = TRUE, invert = TRUE)
CA790 <- CA790[idx, ]



# compute area
CA792.ac <- sapply(CA792@polygons, slot, 'area') * 0.000247105
CA630.ac <- sapply(CA630@polygons, slot, 'area') * 0.000247105
CA790.ac <- sapply(CA790@polygons, slot, 'area') * 0.000247105

# stack
g <- make.groups(
  'Calaveras/Tuolumne (CA630)'=data.frame(area_ac=CA630.ac), 
  'Sequoia/Kings Canyon (CA792)'=data.frame(area_ac=CA792.ac),
  'Yosemite (CA790)'=data.frame(area_ac=CA790.ac))


# there is a very tiny polygon in CA790.. sliver no doubt
bwplot(which ~ area_ac, data=g, 
       xlab='Area (acres)',
       subset=area_ac > 0.1,
       par.settings=tactile.theme, 
       scales=list(alternating=3, x=list(log=10)),
       xscale.components=xscale.components.log10.3,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
       )




plyr::ldply(tapply(g$area_ac, g$which, quantile), round)



