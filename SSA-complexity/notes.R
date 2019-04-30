library(latticeExtra)
library(hexbin)
library(RColorBrewer)
library(viridis)

x <- read.csv('fractal-dimension-test.csv.gz')
head(x)
nrow(x)



table(x$areasymbol)

# bwplot(areasymbol ~ fd, data=x)
# 
# tapply(x$fd, x$areasymbol, summary)
# 
# plot(fd ~ log_sq_m, data=x)

summary(x$fd)
summary(x$log_sq_m)
summary(x$n_pts)

## TODO: convert area to acres


p.1 <- hexbinplot(fd ~ n_pts, data=x, xbins=50, main='FY2019 SSURGO\n100k Samples', xlab='Polygon Vertex Count', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2, scales=list(x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE)

p.2 <- hexbinplot(fd ~ log_sq_m, data=x, xbins=50, main='FY2019 SSURGO\n100k Samples', xlab='Log10 Polygon Area (sq.m)', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2, colorkey=FALSE)

p.3 <- hexbinplot(log_sq_m ~ n_pts, data=x, xbins=50, main='FY2019 SSURGO\n100k Samples', xlab='Polygon Vertex Count', ylab='Log10 Polygon Area (sq.m)', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2, scales=list(x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE)

print(p.1, split=c(1,1,3,1), more=TRUE)
print(p.2, split=c(2,1,3,1), more=TRUE)
print(p.3, split=c(3,1,3,1), more=FALSE)


hexbinplot(fd ~ n_pts | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main='FY2019 SSURGO\n100k Samples', xlab='Polygon Vertex Count', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3, x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))



# convert to percentiles
x$fd_pctile <- ecdf(x$fd)(x$fd)
x$n_pts_pctile <- ecdf(x$n_pts)(x$n_pts)

hexbinplot(fd_pctile ~ n_pts | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main='FY2019 SSURGO\n100k Samples', xlab='Polygon Vertex Count', ylab='Fractal Dimension Percentile', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3, x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))

# # what does this really mean?
# hexbinplot(fd_pctile ~ n_pts_pctile | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main='FY2019 SSURGO\n100k Samples', xlab='Polygon Vertex Count Percentile', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=viridis, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3), colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))


ecdf(x$fd)(1.3)
ecdf(x$n_pts)(1000)


# http://soilmap2-1.lawr.ucdavis.edu/dylan/soilweb/api/landPKS.php?q=spn&lon=-85.338196&lat=40.714611
# most complex in samples
ecdf(x$fd)(1.48329949700171)


ecdf(x$fd)(1.29691553046904)
