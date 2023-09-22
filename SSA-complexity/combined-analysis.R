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
.fy <- '2023'
.title <- sprintf('FY%s SSURGO\n100k Samples', .fy)
.cp <- hcl.colors(100, 'zissou1')
.cpf <- colorRampPalette(.cp)

hexbinplot(
  fd ~ entropy, 
  data = z, 
  xbins = 25, 
  main = .title, 
  xlab = 'Shannon Entropy (base 2)', 
  ylab = 'Fractal Dimension', 
  trans = log, 
  inv = exp, 
  asp = 1, 
  colramp = .cpf, 
  type = 'g', 
  subset = fd < 1.9, 
  scales = list(tick.number = 6, alterntating = 1), 
  colorkey = FALSE, 
  par.settings = tactile.theme()
)
