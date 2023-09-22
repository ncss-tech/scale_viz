library(latticeExtra)
library(hexbin)
library(tactile)
library(terra)
library(soilDB)

x <- read.csv('fractal-dimension-test.csv.gz')
head(x)
nrow(x)

# survey order by map unit
x$invesintens <- factor(x$invesintens, levels = c('missing', 'Order 1', 'Order 2', 'Order 3', 'Order 4', 'Order 5'))

x$projectscale <- factor(x$projectscale)

.fy <- '2023'

table(x$areasymbol)
table(x$invesintens)
table(x$projectscale)

# how does mu-level vs. ssa-level survey order align?
xtabs( ~ projectscale + invesintens, data = x)

#              invesintens
# projectscale missing Order 1 Order 2 Order 3 Order 4 Order 5
# 10000      11       0       2       0       0       0
# 12000    2309       6   11930       4       0       0
# 15000      13       0       0       0       0       0
# 15840    5955       0   19610      22       0       0
# 20000    9561       0   19575     364       0       0
# 24000    8298       0   19305    1641      72       3
# 25000      98       0     146      18       0       0
# 31680     311       0     189     307       5       0
# 42240       0       0      19       0       0       0
# 48000      57       0      10       0       0       0
# 62500       0       0       0      29       7       0
# 63000       9       0       0       0       0       0
# 63360      45       0       1      24      23       4


# bwplot(areasymbol ~ fd, data=x)
# 
# tapply(x$fd, x$areasymbol, summary)
# 
# plot(fd ~ log_sq_m, data=x)

summary(x$fd)
summary(x$log_sq_m)
summary(x$n_pts)

# whoa, what is up with the top FD map units?
# mukey = 2995023
# https://casoilresource.lawr.ucdavis.edu/soil_web/list_components.php?mukey=2995023
# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=30.6060197088614,-88.279394644734
knitr::kable(x[x$fd > 1.85, ], row.names = FALSE)


# investigate records with VERY high fractal dimension
# like errors
p <- vect('POINT(-88.279394644734 30.6060197088614)', crs = 'epsg:4326')
b <- buffer(p, 10)
mu <- SDA_spatialQuery(b, what = 'mupolygon', geomIntersection = TRUE)

# a sliver
plot(mu)
plot(p, add = TRUE, pch = 16, col = 2)


# filter for now
.thresh <- 1.9

# figure elements and style
.title <- sprintf('FY%s SSURGO\n100k Samples', .fy)
.cp <- hcl.colors(100, 'zissou1')
.cpf <- colorRampPalette(.cp)

## TODO: convert area to acres



p.1 <- hexbinplot(
  fd ~ n_pts, 
  data = x, 
  xbins = 50, 
  main = .title, 
  xlab = 'Polygon Vertex Count', 
  ylab = 'Fractal Dimension', 
  trans = log, 
  inv = exp, 
  asp = 1, 
  colramp = .cpf, 
  type = 'g', 
  subset = fd < .thresh, 
  scales = list(x = list(log = 10)), 
  xscale.components = xscale.components.log10ticks, 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)

p.2 <- hexbinplot(
  fd ~ log_sq_m, 
  data = x, 
  xbins = 50, 
  main = .title, 
  xlab = 'Log10 Polygon Area (sq.m)', 
  ylab = 'Fractal Dimension', 
  trans = log, 
  inv = exp, 
  asp = 1, 
  colramp = .cpf, 
  type = 'g', 
  subset = fd < .thresh, 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)

p.3 <- hexbinplot(
  log_sq_m ~ n_pts, 
  data = x, 
  xbins = 50, 
  main = .title, 
  xlab = 'Polygon Vertex Count', 
  ylab = 'Log10 Polygon Area (sq.m)', 
  trans = log, 
  inv = exp, 
  asp = 1, 
  colramp = .cpf, 
  type = 'g', 
  subset = fd < .thresh, 
  scales = list(x = list(log = 10)), 
  xscale.components = xscale.components.log10ticks, 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)

.file <- sprintf("FD-summary-FY%s.png", .fy)
ragg::agg_png(file = .file, width = 1250, height = 500, scaling = 1.5)

print(p.3, split = c(1, 1, 3, 1), more = TRUE)
print(p.1, split = c(2, 1, 3, 1), more = TRUE)
print(p.2, split = c(3, 1, 3, 1), more = FALSE)

dev.off()


## what about survey order at map unit level?

bwplot(
  invesintens ~ fd, 
  data = x, 
  main = .title, 
  xlab = 'Fractal Dimension', 
  ylab = '', 
  subset = fd < .thresh & invesintens %in% c('missing', 'Order 2', 'Order 3', 'Order 4'), 
  par.settings = tactile.theme()
)


bwplot(
  invesintens ~ n_pts, 
  data = x, 
  main = .title, 
  xlab = 'Polygon Vertex Count', 
  ylab = '', 
  subset = fd < .thresh & invesintens %in% c('missing', 'Order 2', 'Order 3', 'Order 4'), 
  scales = list(alternating = 3, x = list(log = 10)), 
  xscale.components = xscale.components.log10ticks, 
  par.settings = tactile.theme()
)

bwplot(
  invesintens ~ log_sq_m, 
  data = x, 
  main = .title, 
  xlab = 'Log10 Polygon Area (sq.m)', 
  ylab = '', 
  subset = fd < .thresh & invesintens %in% c('missing', 'Order 2', 'Order 3', 'Order 4'), 
  scales = list(alternating = 3, x = list(tick.number = 10)), 
  par.settings = tactile.theme()
)


## survey order at SSA level
bwplot(
  projectscale ~ fd, 
  data = x, 
  main = .title, 
  xlab = 'Fractal Dimension', 
  ylab = '', 
  subset = fd < .thresh, 
  par.settings = tactile.theme()
)


bwplot(
  projectscale ~ n_pts, 
  data = x, 
  main = .title, 
  xlab = 'Polygon Vertex Count', 
  ylab = '', 
  subset = fd < .thresh, 
  scales = list(alternating = 3, x = list(log = 10)), 
  xscale.components = xscale.components.log10ticks, 
  par.settings = tactile.theme()
)

bwplot(
  projectscale ~ log_sq_m, 
  data = x, 
  main = .title, 
  xlab = 'Log10 Polygon Area (sq.m)', 
  ylab = '', 
  subset = fd < .thresh, 
  scales = list(alternating = 3, x = list(tick.number = 10)), 
  par.settings = tactile.theme()
)





hexbinplot(fd ~ n_pts | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main=.title, xlab='Polygon Vertex Count', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=.cpf, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3, x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))



# convert to percentiles
x$fd_pctile <- ecdf(x$fd)(x$fd)
x$n_pts_pctile <- ecdf(x$n_pts)(x$n_pts)

hexbinplot(fd_pctile ~ n_pts | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main=.title, xlab='Polygon Vertex Count', ylab='Fractal Dimension Percentile', trans=log, inv=exp, asp=1, colramp=.cpf, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3, x=list(log=10)), xscale.components=xscale.components.log10ticks, colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))

# # what does this really mean?
hexbinplot(fd_pctile ~ n_pts_pctile | factor(toupper(substr(areasymbol, 1, 2))), data=x, xbins=30, main=.title, xlab='Polygon Vertex Count Percentile', ylab='Fractal Dimension', trans=log, inv=exp, asp=1, colramp=.cpf, type='g', subset=fd < 2 & grepl('ca|la|ne|wy|ak|ia', areasymbol), scales=list(alternating=3), colorkey=FALSE, as.table=TRUE, strip=strip.custom(bg=grey(0.85)))


ecdf(x$fd)(1.3)
ecdf(x$n_pts)(1000)


## worst offenders: see ssurgo-polygon-complexity.sql

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=40.714611,-85.338196

# most complex in samples
ecdf(x$fd)(1.48329949700171)

ecdf(x$fd)(1.29691553046904)





