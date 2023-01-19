library(sf)

ssurgo <- read_sf("D:/geodata/soils/")

mlra    <- read_sf("D:/geodata/soils/MLRA_52.shp") |> st_transform(5070)
statsgo <- read_sf("D:/geodata/soils/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us_aea.shp")

mlra$m2     <- st_area(mlra)
statsgo$m2  <- st_area(statsgo)
statsgo$cm2 <- units::set_units(statsgo$m2, cm^2)

# definitions
## map scale
ms <- 1 / 24000

# scale ratio
"1:24000"

## representative fraction
rf <- 1 / 24000

# scale nummber
sn <- 24000


sn <- function(MLA, MLD) {

  # if (! is.null(MLA)) {
    sqrt(MLA / MLD)
  # }
  
  # if (!is.null(RF)) {
  #   ((1 / RF)^2) / 2.5*10^8
  # }
}

p <- c(0, 0.05, 0.1)
sn(quantile(statsgo$m2, p), mld_m2[3])
sn(quantile(mlra$m2, p), mld_m2[3])


# function of delineation size

# minimum legible delination
mld_cm2 <- units::set_units(c(0.10, 0.25, 0.40), cm^2)
mld_m2  <- units::set_units(mld_cm2, m^2)


# minimum legible area 
mla <- function(SN, MLD) {
  SN^2 * MLD
}

mla(20000, mld_m2)



## average size of delination
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

