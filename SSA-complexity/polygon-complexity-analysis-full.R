library(data.table)
library(lattice)
library(latticeExtra)
library(hexbin)
library(tactile)

# giant file
# no headers
x <- fread(file = 'mu-polygon-complexity.txt.gz', sep = '|', header = FALSE, showProgress = TRUE, stringsAsFactors = FALSE, verbose = TRUE)

# column names from inspection of original SQL query
head(x)
names(x) <- c('areasymbol', 'ogc_fid', 'mukey', 'fd', "log_sq_m", "n_pts", "invesintens", "coryear", "projectscale")


## TODO: develop better filters for figures
##  -> what are the extreme values all about?
##  -> very small polygons, very large FD, negative FD !
##  -> consider filtering on vertex count
summary(x)

quantile(x$fd, probs = c(0.01, 0.99))
quantile(x$n_pts, probs = c(0.01, 0.99))
quantile(x$log_sq_m, probs = c(0.01, 0.99))

x[x$fd > 1.2 & x$log_sq_m > 10, ]

x[x$fd > 1.6, ]

x[x$n_pts < 5, ]

z <- x[x$n_pts > 100000, ]

soilDB::format_SQL_in_statement(z$ogc_fid)


## flag wacky polygons

# 


## save links back to source data


## export as points / polygons / links




.fy <- '2025'



# figure elements and style
.title <- sprintf('FY%s SSURGO', .fy)
.cp <- hcl.colors(100, 'zissou1')
.cpf <- colorRampPalette(.cp)

## TODO: convert area to acres

options(scipen = 20)




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
  subset = log_sq_m > 0 & fd < 2, 
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
  subset = log_sq_m > 0 & fd < 2, 
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
  subset = log_sq_m > 0 & fd < 2, 
  scales = list(x = list(log = 10)), 
  xscale.components = xscale.components.log10ticks, 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)

.file <- sprintf("FD-summary-FY%s-full.png", .fy)
ragg::agg_png(file = .file, width = 1250, height = 500, scaling = 1.5)

print(p.3, split = c(1, 1, 3, 1), more = TRUE)
print(p.1, split = c(2, 1, 3, 1), more = TRUE)
print(p.2, split = c(3, 1, 3, 1), more = FALSE)

dev.off()
