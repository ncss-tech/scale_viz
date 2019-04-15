library(tactile)
library(latticeExtra)
library(rgdal)


# r.clump
# r.stats
x <- read.table(file='forms30-sizes.txt.gz', header=FALSE)
names(x) <- c('id', 'form', 'area_sqm')

# add levels / convert sq. meters -> ac.
x$form <- factor(x$form, levels = 1:10, labels = c('flat', 'summit', 'ridge', 'shoulder', 'spur', 'slope', 'hollow', 'footslope', 'valley', 'depression'))
x$area_ac <- with(x, area_sqm * 0.000247105)

# current CA792 linework
mu <- readOGR(dsn='L:/NRCS/MLRAShared/CA792/ca792_spatial/FG_CA792_OFFICIAL.gdb', layer='ca792_a', encoding='OpenFileGDB', stringsAsFactors = FALSE)

# remove water
idx <- grep('w', mu$MUSYM, ignore.case = TRUE, invert = TRUE)
mu <- mu[idx, ]

# compute area
a <- sapply(mu@polygons, slot, 'area')
mu_data <- data.frame(id=NA, form='CA792', area_sqm=NA, area_ac=a * 0.000247105)

# stack
g <- make.groups(geomorphons=x, CA792=mu_data)


bwplot(form ~ area_ac, data=x, 
       xlab='Area (acres)',
       par.settings=tactile.theme, 
       scales=list(alternating=3, x=list(log=10)),
       xscale.components=xscale.components.log10.3,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
       )


bwplot(form ~ area_ac, data=g, 
       xlab='Area (acres)',
       par.settings=tactile.theme, 
       scales=list(alternating=3, x=list(log=10)),
       xscale.components=xscale.components.log10.3,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
)



bwplot(which ~ area_ac, data=g, 
       xlab='Area (acres)',
       par.settings=tactile.theme, 
       scales=list(alternating=3, x=list(log=10)),
       xscale.components=xscale.components.log10.3,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       }
)


tapply(g$area_ac, g$which, quantile)



