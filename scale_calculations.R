library(sf)

ssurgo <- read_sf("D:/geodata/soils/")

mlra    <- read_sf("D:/geodata/soils/MLRA_52.shp") |> st_transform(5070)
statsgo <- read_sf("D:/geodata/soils/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us_aea.shp")

mlra$m2     <- st_area(mlra)
statsgo$m2  <- st_area(statsgo)
statsgo$cm2 <- units::set_units(statsgo$m2, cm^2)


# minimum legible delineation
mld_cm2 <- units::set_units(c(0.01, 0.10, 0.25, 0.40, 0.60), cm^2)
mld_m2  <- units::set_units(mld_cm2, m^2)
mld_ha  <- units::set_units(mld_cm2, ha)
mld_ac  <- units::set_units(mld_cm2, acre)

# definitions
## map scale
ms <- 1 / 24000

# scale ratio
"1:24000"

## representative fraction
rf <- 1 / 24000

# scale nummber
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


SN1 <- as.numeric(sn(MLA = c(10, 30, 100)^2, mld_m2[4]))
names(SN1) <- c("10-meter", "30-meter", "100-meter")
SN2 <- c(SN1, SN2)
SN2


# function of delineation size



# minimum legible area 
mla <- function(SN, MLD) {
  SN^2 * MLD
}

formatC(mla(SN2, mld_ac[4]), format)
mla(20000, mld_m2)
mla(24000, mld_m2)
mla(24000, mld_ac)


## average size of delineation
asd <- function(area, n) {
  sum(area) /  n
}


## index of maximum reduction
imr <- function(asd, mld) {
  asd / mld
}


## effective scale number
esn = function(nsn, imr) {
  nsn * (imr / 2)
}


# shape complexity index
sci <- function(perimeter, area) {
  perimeter / (2 * (sqrt(area / pi)) * pi)
}


asd(statsgo$cm2 * 10000, nrow(statsgo))


# table for printing

ac <- format(signif(as.numeric(mla(SN2, mld_ac[4])), 2), scientific = FALSE, big.interval = ",")
ha <- format(signif(as.numeric(mla(SN2, mld_ha[4])), 2), scientific = FALSE, big.interval = ",")
m2 <- format(signif(as.numeric(mla(SN2, mld_m2[4])), 2), scientific = FALSE, big.interval = ",")
m  <- signif(sqrt(as.numeric(m2)), 2)
df <- data.frame(SN = signif(as.numeric(SN2), 2), `MLD ac` = ac, `MLD ha` = ha, `MLD m2` = m2, `MLD m` =  m)
df <- cbind(`Soil Map` = names(SN2), df)
df

