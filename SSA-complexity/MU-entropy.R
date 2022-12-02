library(aqp)
library(latticeExtra)
library(viridis)
library(tactile)
library(hexbin)


## TODO:

# 1. include misc. areas
# 2. 


equal.prob.H <- function(n, b = 2) {
  p <- rep(1, times = n) / n
  shannonEntropy(p, b = b)
}

epH <- Vectorize(equal.prob.H)


## double-check: YES
## H_2(p) / H_2_max(n) == H_n(p)

# p <- c(0.2, 0.2, 0.6)
# shannonEntropy(p, b = 2) / equal.prob.H(n = 3, b = 2)
# shannonEntropy(p, b = 3)

## question: does the distribution of H/H_max == distribution of quantile(H/H_max) ?

## how to incorporate area? H/area, by delineation?


x <- read.csv('entropy-by-mukey.csv.gz')
y <- read.csv('entropy-by-mukey-statsgo.csv.gz')


x$mukind <- sprintf("SSURGO\n%s", x$mukind)
y$mukind <- sprintf("STATSGO\n%s", y$mukind)

# stack
vars <- c('areasymbol', 'projectscale', 'mukind', 'mukey', 'entropy', 'n')
g <- make.groups(SSURGO = x[, vars], STATSGO = y[, vars])

row.names(g) <- NULL

head(g)

table(g$which, g$mukind, useNA = 'always')

summary(g$entropy)


# investigate these
# ~ 22% 0-entropy (~72k MU)
# some related to dropped misc. area components
prop.table(table(g$entropy == 0))

# single component MU
# misc. areas
# https://casoilresource.lawr.ucdavis.edu/soil_web/list_components.php?mukey=1425970
z <- g[g$entropy == 0, ]
head(z)


# remove 0-entropy for now
g <- subset(g, subset = entropy > 0)

(scale.tab <- sort(round(prop.table(table(g$projectscale)), 3)))


# equal-prob H
g$H.max <- epH(g$n)

# check: negative values suggests H > H_max
idx <- which(sign(g$H.max - g$entropy) == -1)
length(idx)

# these are all tiny numbers
g[idx, ]$entropy - g[idx, ]$H.max




tps <- tactile.theme(plot.symbol = list(cex = 0.5))


histogram( ~ entropy | which, data = g, par.settings = tps, breaks = 100, scales = list(alternating = 1, x = list(tick.number = 10), y = list(alternating = 3)), xlab = 'Shannon Entropy (base 2)')

histogram( ~ entropy / H.max | which, data = g, par.settings = tps, breaks = 100, scales = list(alternating = 1, x = list(tick.number = 10), y = list(alternating = 3)), xlab = 'Shannon Entropy / Equal-Probability Entropy')

bwplot(which ~ entropy / H.max, data = g, par.settings = tps, xlab = 'Shannon Entropy / Equal-Probability Entropy', main = 'All Map Unit Kinds', scales = list(x = list(tick.number = 10)), xlim = c(-0.1, 1.1))

bwplot(which ~ entropy, data = g, par.settings = tps, xlab = 'Shannon Entropy (base 2))', main = 'All Map Unit Kinds', scales = list(x = list(tick.number = 10)))



p1 <- bwplot(mukind ~ entropy, 
             data = g, 
             par.settings = tps, 
             xlab = 'Shannon Entropy (base 2)', 
             main = '', 
             scales = list(x = list(tick.number = 10)), 
             varwidth = FALSE, 
             panel = function(...) {
               panel.grid(h = 0, v = -1)
               panel.bwplot(...)
             }
)


p2 <- bwplot(mukind ~ entropy / H.max, 
             data = g, 
             par.settings = tps, 
             xlab = 'Shannon Entropy / Equal-Probability Entropy', 
             main = '', 
             scales = list(x = list(tick.number = 10)), 
             varwidth = FALSE, 
             panel = function(...) {
               panel.grid(h = 0, v = -1)
               panel.bwplot(...)
             }
)


ragg::agg_png(filename = 'mukind-entropy-example.png', width = 1000, height = 600, scaling = 1.5)

print(p1, split = c(1, 1, 1, 2), more = TRUE)
print(p2, split = c(1, 2, 1, 2), more = FALSE)

dev.off()


g.sub <- subset(g, subset = areasymbol %in% c('tx027', 'ca630', 'ca113', 'ca792', 'US'))
g.sub$areasymbol <- factor(g.sub$areasymbol, levels = c('tx027', 'ca630', 'ca113', 'ca792', 'US'))

bwplot(areasymbol ~ entropy / H.max, data = g.sub, par.settings = tps, xlab = 'Shannon Entropy / Equal-Probability Entropy', main = '',  scales = list(x = list(tick.number = 10)), varwidth = FALSE, xlim = c(-0.1, 1.1))

bwplot(areasymbol ~ entropy, data = g.sub, par.settings = tps, xlab = 'Shannon Entropy (base 2)', main = '', scales = list(x = list(tick.number = 10)), varwidth = FALSE)




bwplot(mukind ~ entropy, data = g, subset = projectscale %in% c(12000, 24000), par.settings = tps, xlab = 'Shannon Entropy (base 2)', main = '1:12,000 and 1:24,000 Surveys', scales = list(x = list(tick.number = 10)))


bwplot(mukind ~ entropy, data = g, subset = projectscale %in% c(12000, 24000), par.settings = tps, xlab = 'Shannon Entropy (base 2)', main = '1:12,000 and 1:24,000 Surveys', scales = list(x = list(tick.number = 10)), varwidth = TRUE)

bwplot(mukind ~ entropy / H.max, data = g, subset = projectscale %in% c(12000, 24000), par.settings = tps, xlab = 'Shannon Entropy / Equal-Probability Entropy', main = '1:12,000 and 1:24,000 Surveys', scales = list(x = list(tick.number = 10)), xlim = c(-0.1, 1.1))



# hard to interpret because not all scales are well-represented
bwplot(factor(projectscale) ~ entropy, data = g, subset = projectscale %in% c(12000, 24000, 250000), par.settings = tps, xlab = 'Shannon Entropy (base 2)', scales = list(x = list(tick.number = 10)))



##
library(ggplot2)
library(ggdist)

ggplot(g, aes(x = entropy, y = mukind)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal', size = 5) + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(size = 10, face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 3) +
  scale_color_brewer(palette = 'Blues') + 
  scale_x_continuous(n.breaks = 10) +
  xlab('Shannon Entropy (base 2)\nSingle Component and Misc. Area MU Removed') + ylab('') +
  labs(title = 'All Survey Areas FY23', color = 'Interval')



ggplot(g, aes(x = n, y = mukind)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal', size = 5) + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(size = 10, face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 3) +
  scale_color_brewer(palette = 'Blues') + 
  scale_x_continuous(n.breaks = 10) +
  xlab('Number of Components\nSingle Component and Misc. Area MU Removed') + ylab('') +
  labs(title = 'All Survey Areas FY23', color = 'Interval')




#
g.sub <- subset(g, subset = projectscale %in% c(12000, 24000))

ggplot(g.sub, aes(x = entropy, y = mukind)) +
  stat_interval(inherit.aes = TRUE, orientation = 'horizontal', size = 5) + 
  theme_minimal() +
  theme(legend.position = c(1, 1), legend.justification ='right', legend.direction	
        = 'horizontal', legend.background = element_rect(fill = 'white', color = NA), axis.text.y = element_text(size = 12, face = 'bold')) + 
  stat_summary(geom = 'point', fun = median, shape = 21, fill = 'black', col = 'white', cex = 3) +
  scale_color_brewer(palette = 'Blues') + 
  scale_x_continuous(n.breaks = 10) +
  xlab('Shannon Entropy (base 2)\nSingle Component and Misc. Area MU Removed') + ylab('') +
  labs(title = '1:12,000 and 1:24,000 Surveys FY23', color = 'Interval')




# # blah
# hexbinplot(entropy ~ projectscale, data = x, subset = projectscale < 100000, trans = log, inv = exp, colramp = viridis, colorkey = FALSE, asp = 1, scales = list(x = list(log = 10)), xscale.components = xscale.components.log10.3)
# 
# 
# ## nothing here
# hexbinplot(entropy ~ coryear, data = x, trans = log, inv = exp, colramp = viridis, colorkey = FALSE)
# 

