library(soilDB)
library(dplyr)
library(sf)

ssurgo <- read_sf("D:/geodata/soils/")

mlra    <- read_sf("D:/geodata/soils/MLRA_52.shp") |> st_transform(5070)
statsgo <- read_sf("D:/geodata/soils/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us_aea.shp")

mlra$m2     <- st_area(mlra)
statsgo$m2  <- st_area(statsgo)
statsgo$cm2 <- units::set_units(statsgo$m2, cm^2)


# SSURGO & STATSGO2 map scales
le <- get_legend_from_SDA(WHERE = "areasymbol LIKE '%'")
mu <- get_mapunit_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS_202210.gdb") 

sso <- mu %>%
  group_by(areasymbol, invesintens) %>%
  summarize(acres = sum(muacres, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(areasymbol, - acres) %>%
  filter(!duplicated(areasymbol))

le %>%
  left_join(sso, by = "areasymbol") %>%
  group_by(invesintens, projectscale) %>%
  tally(projectscale) %>%
  View()



# definitions ----

## map scale ----
ms <- 1 / 24000


# scale ratio ----
"1:24000"


## representative fraction ----
rf <- 1 / 24000


## minimum legible delineation ----
mld_cm2 <- units::set_units(c(0.01, 0.10, 0.25, 0.40, 0.60), cm^2)
mld_m2  <- units::set_units(mld_cm2, m^2)
mld_km2  <- units::set_units(mld_cm2, km^2)
mld_ha  <- units::set_units(mld_cm2, ha)
mld_ac  <- units::set_units(mld_cm2, acre)


## scale nummber ----
SN <- 24000
SN2 <- c(`SSURGO 1st` = 12000, `SSURGO 2nd` = 24000, STATSGO = 250000, LRU = 1000000, MLRA = 3500000, LRR = 7500000)

sn <- function(MLA, MLD) {

  # if (! is.null(MLA)) {
    sqrt(MLA / MLD)
  # }
  
  # if (!is.null(RF)) {
  #   ((1 / RF)^2) / 2.5*10^8
  # }
}

p <- c(0, 0.05, 0.1, 0.5, 0.95, 1)
sn(quantile(statsgo$m2, p), mld_m2[4])
sn(quantile(mlra$m2, p), mld_m2[4])


SN1 <- as.numeric(sn(MLA = c(5, 10, 30, 100, 1000)^2, mld_m2[1]))
names(SN1) <- c("5-meter", "10-meter", "30-meter", "100-meter", "1-kilometer")
SN2 <- c(SN1, SN2)
SN2



# function of delineation size ----

## minimum legible area ----
mla <- function(SN, MLD) {
  SN^2 * MLD
}

formatC(mla(SN2, mld_ac[4]), format)
mla(20000, mld_m2)
mla(24000, mld_m2)
mla(24000, mld_ac)

mla(24000, mld_ac)
mla(24000, mld_cm2[4])


## average size of delineation ----
asd <- function(area, n) {
  sum(area) /  n
}


## index of maximum reduction ----
imr <- function(asd, mld) {
  asd / mld
}


## effective scale number ----
esn = function(nsn, imr) {
  nsn * (imr / 2)
}


## shape complexity index ----
sci <- function(perimeter, area) {
  perimeter / (2 * (sqrt(area / pi)) * pi)
}


asd(statsgo$cm2 * 10000, nrow(statsgo))


# function of inspection density ----

## Forbes 1987 p31 ----
# 1 point / 50cm2
# points/cm2 * 10^10 * RF = points/km2
# conversion to km (1000 * 100) ^2 == 10^10
(1/50 * 10^10) * 1/20000^2
(1/50 * 10^10) /   20000^2
(10^10 / 50)   /   20000^2
n(50, 10^10, 20000)

format(signif((1/50 * 10^10) / SN2^2, 3), scientific = FALSE)

n(0.25, 10^10, 50000)


# SN = sqrt((MLA / N) / MLD)
20000 == sqrt(1/50 * 10^10/0.5)
20000 == sqrt(10^10/0.5 / 50)
20000 == sqrt(1e5^2/0.5 / 50)


# N  = (MLA / MLD * factor) / SN^2
0.5 == 1/50 * 10^10 / 20000^2
0.5 == 10^10/50 / 20000^2

n <- function(SN, MLA, MLD, factor = 1) {
  (MLA / (MLD * factor)) / SN^2
}
n(20000, 10^10, 50)


## Vink ----
# format(signif((1/0.25*10 * 10^10) / SN2^2, 3), scientific = FALSE)
# n(10^10, 0.25, 24000, 10)

## Rossiter 2003 p36 ----

# format(signif((1/0.4*4 * 10^10) / SN2^2, 3), scientific = FALSE)
# n(10^10, 0.4, 24000, 4)

n(25000, 10^10, mld_cm2[4], c(2, 10))
n(25000, 10^10, mld_cm2[4], c(2, 4, 10))
n(25000, 1, mld_km2[4], c(2, 4, 10))
n(25000, 1, mld_km2[4], c(2, 4, 10))
n(25000, 20234, mld_ha[4], c(2, 4, 10))
n(24000, 70000, mld_ac[4], c(2, 4, 10))

SN <- c(2500, 10000, 25000, 50000, 100000, 250000, 1000000, 1e6)
lapply(SN, function(x) n(x, 1, mld_km2[4], c(2, 4, 10)))


## Hengl 2006 ----
# SN = sqrt(4 * A/N) * 10^2 | sqrt(A/N) * 10^2
sqrt(c(1, 4) * 1000^2/10) * 10^2
signif(mean(sqrt(c(1, 4) * 1000^2/10) * 10^2), 1)

# N = (A * 10^2) / SN^2
# N  = (MLA / MLD * factor) / SN^2

round((1000^2 * 10^2^2) / 31622.78^2)

format(signif((1000^2 * 10^2^2) / SN2^2, 3), scientific = FALSE)


# table for printing ----
i1 <- rep(1, 5)
i2 <- rep(1, 6)
ac <- mla(SN2, c(mld_ac[1][i1], mld_ac[4][i2])) * c(i1 * 4, i2)
ha <- mla(SN2, c(mld_ha[1][i1], mld_ha[4][i2])) * c(i1 * 4, i2)
m2 <- mla(SN2, c(mld_m2[1][i1], mld_m2[4][i2])) * c(i1 * 4, i2)
m  <- signif(sqrt(as.numeric(m2)), 2) / 2
df <- data.frame(SN = signif(as.numeric(SN2), 2), `MLA ac` = ac, `MLA ha` = ha, `MLA m2` = m2, `MLA m` =  m)
df <- cbind(`Examples` = names(SN2), df)
df[3:6] <- lapply(df[3:6], function(x) format(signif(as.numeric(x), 2), scientific = FALSE, big.interval = ","))
row.names(df) <- NULL
df

test <- sapply(SN2, function(x) n(x, 50000, mld_ac[4], c(2, 4, 10)))
test <- t(test)
test <- data.frame(Example = row.names(test), test)
row.names(test) <- NULL
test[2:4] <- lapply(test[2:4], function(x) format(signif(x, 2), scientific = FALSE, big.interval = ","))
names(test)[2:4] <- paste0("MLD x ", c(2, 4, 10))
knitr::kable(test, caption = "Number of observations per 1km2 (i.e. 247 acres)", align = "r")
