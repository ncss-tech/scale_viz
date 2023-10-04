library(latticeExtra)
library(hexbin)
library(tactile)
library(terra)
library(soilDB)


x <- read.csv('fractal-dimension-test.csv.gz')
x$invesintens <- factor(x$invesintens, levels = c('missing', 'Order 1', 'Order 2', 'Order 3', 'Order 4', 'Order 5'))

y <- read.csv('entropy-by-mukey.csv.gz')

z <- merge(x, y[, c('mukey', 'mukind', 'entropy')], by = 'mukey', all.x = TRUE, sort = FALSE)
head(z)

z <- z[which(z$entropy > 0), ]


# figure elements and style
.fy <- '2024'
.title <- sprintf('FY%s SSURGO\n100k Samples', .fy)
.cp <- hcl.colors(100, 'zissou1')
.cpf <- colorRampPalette(.cp)

.file <- sprintf("FD-vs-H-FY%s.png", .fy)
ragg::agg_png(file = .file, width = 1200, height = 1200, scaling = 2)

hexbinplot(
  fd ~ entropy, 
  data = z, 
  xbins = 60, 
  main = .title, 
  xlab = 'Shannon Entropy [base 2] (component percentages)', 
  ylab = 'Fractal Dimension (geometry)', 
  trans = log, 
  inv = exp, 
  asp = 1, 
  colramp = .cpf, 
  type = 'g', 
  subset = fd < 1.9, 
  scales = list(tick.number = 8, alterntating = 1), 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)

dev.off()

