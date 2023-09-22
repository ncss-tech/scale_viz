library(soilDB)
library(sf)
library(mapview)
library(latticeExtra)
library(tactile)
library(ragg)

source('local-functions.R')

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

## MO071
# Beemont very gravelly silt loam, 8 to 15 percent slopes, stony (73193)
mukey <- '2534946'


## very large / complex example
# Pewamo silty clay loam, 0 to 1 percent slopes
mukey <- '161428'


# # extensive MU at land/water interface
# mukey <- '808530'
# # extensive WATER MU
# mukey <- '808535'


mu.union <- MUspatialSummary(mukey, method = 'union')
mu.bbox <- MUspatialSummary(mukey, method = 'bbox')


mapview(mu.union, col.regions = NA, color = 'firebrick', lwd = 2, legend = FALSE)

mapview(mu.bbox, col.regions = 'royalblue', alpha.regions = 0.25, color = 'black', lwd = 0.5, legend = FALSE)

mapview(mu.bbox, zcol = 'fd', alpha.regions = 0.25, color = 'black', lwd = 0.5)


## get a collection for comparison
mu.summary <- MUspatialSummary(c('1865918', '456137', '162786', '403308', '2534946'), method='bbox')

## TODO: these aren't populated as often as we would like
# map unit / survey order

table(mu.summary$invesintens)
table(mu.summary$muname, mu.summary$invesintens)


# abbreviate map unit names
mu.summary$muname_abbv <- gsub(', ', '\n', mu.summary$muname)
mu.summary$muname_abbv <- paste(mu.summary$muname_abbv, mu.summary$mukind, sep = '\n')

agg_png(filename = 'mu-spatial-summary-fd.png', width = 1200, height = 550, scaling = 1.5)

bwplot(muname_abbv ~ fd, data = mu.summary, 
       scales = list(alternating = 3, y = list(cex = 0.75), x = list(tick.number = 10)), 
       xlab = 'Fractal Dimension (polygon complexity)', box.ratio = 0.75,
       par.settings = tactile.theme,
       panel = function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })

dev.off()

agg_png(filename = 'mu-spatial-summary-area.png', width = 1200, height = 550, scaling = 1.5)

bwplot(muname_abbv ~ area_ac, data = mu.summary, 
       scales = list(alternating = 3, y = list(cex = 0.75), x = list(log = 10, tick.number = 20)), 
       xscale.components = xscale.components.log10ticks, 
       xlab = 'Delineation Area (ac.)', box.ratio = 0.75,
       par.settings = tactile.theme,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })

dev.off()



bwplot(invesintens ~ fd, data = mu.summary, 
       scales = list(alternating = 3, y = list(cex = 0.75), x = list(tick.number = 10)), 
       xlab = 'Fractal Dimension (polygon complexity)', box.ratio = 0.75,
       par.settings = tactile.theme,
       panel = function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })


bwplot(invesintens ~ area_ac, data = mu.summary, 
       scales = list(alternating = 3, y = list(cex = 0.75), x = list(log = 10, tick.number = 20)), 
       xscale.components = xscale.components.log10ticks, 
       xlab = 'Delineation Area (ac.)', box.ratio = 0.75,
       par.settings = tactile.theme,
       panel=function(...) {
         panel.grid(-1, -1)
         panel.bwplot(...)
       })



xyplot(fd ~ area_ac, groups=muname_abbv, data = mu.summary, 
       type=c('p', 'r'),
       scales=list(y=list(cex=0.75, tick.number=10), x=list(log=10, tick.number=20)), 
       xscale.components=xscale.components.log10ticks, 
       ylab='Fractal Dimension (polygon complexity)', xlab='Delineation Area (ac.)', 
       par.settings=tactile.theme(superpose.symbol = list(alpha = 0.5)),
       auto.key=list(columns=3, lines=TRUE, points=FALSE, cex=0.75), 
       panel=function(...) {
         panel.grid(-1, -1)
         panel.xyplot(...)
       })



## does this make any sense?


pr <- princomp(cbind(mu.summary$fd, log(mu.summary$area_ac)), cor = TRUE)
pc <- predict(pr)
mu.summary$pc1 <- pc[, 1]
mu.summary$pc2 <- pc[, 2]


xyplot(pc1 ~ pc2, groups=muname_abbv, data=mu.summary, 
       scales=list(y=list(cex=0.75, tick.number=10), x=list(tick.number=10)), 
       # xscale.components=xscale.components.log10ticks, 
       ylab='PC2', xlab='PC1', 
       par.settings=tactile.theme(superpose.symbol=list(pch=16, alpha=0.5, cex=0.5)),
       auto.key=list(columns=3, lines=FALSE, points=TRUE, cex=0.75), 
       panel=function(...) {
         panel.grid(-1, -1)
         panel.xyplot(...)
       })




densityplot(~ pc1, groups=muname_abbv, data=mu.summary, pch=NA, bw=0.3,
            scales=list(x=list(tick.number=10)), 
            par.settings=tactile.theme(superpose.line=list(lwd=2)),
            auto.key=list(columns=3, lines=TRUE, points=FALSE, cex=0.75), 
            panel=function(...) {
              panel.grid(-1, -1)
              panel.densityplot(...)
            })
