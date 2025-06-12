## TODO: use local cache / SDA 


library(soilDB)
library(dplyr)
library(sf)
library(units)

ssurgo <- read_sf("D:/geodata/soils/")
sapol <- read_sf("D:/geodata/soils/gNATSGO_CONUS_Oct2023/gNATSGO_CONUS.gdb", layer = "SAPOLYGON")
sapol <- within(sapol, {
  m2 = st_area(sapol)
  acres = units::set_units(m2, "acres")
})
sapol_tot <- sapol |> group_by(AREASYMBOL) |> summarize(acres = sum(acres, na.rm = TRUE))


mlra31    <- read_sf("D:/geodata/soils/MLRA/mlra_v31_l48/mlra_v31_l48.shp") |> st_transform(5070)
mlra52    <- read_sf("D:/geodata/soils/MLRA/MLRA_52.shp") |> st_transform(5070)
statsgo <- read_sf("D:/geodata/soils/STATSGO2/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us.shp") |>
  st_transform(crs = 5070)

mlra52$m2     <- st_area(mlra52)
mlra52$acres  <- units::set_units(mlra52$m2, acres)

statsgo$m2  <- st_area(statsgo)
statsgo$cm2 <- units::set_units(statsgo$m2, cm^2)
statsgo$acres <- units::set_units(statsgo$m2, acres)



# SSURGO & STATSGO2 map scales
le <- get_legend_from_SDA(WHERE = "areasymbol LIKE '%'")
mu <- get_mapunit_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS/gSSURGO_CONUS.gdb") 

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
mld_cm2 <- units::set_units(c(0.01, 0.1, 0.25, 0.40, 0.50, 1), cm^2)
mld_m2  <- units::set_units(mld_cm2, m^2)
mld_km2  <- units::set_units(mld_cm2, km^2)
mld_ha  <- units::set_units(mld_cm2, ha)
mld_ac  <- units::set_units(mld_cm2, acre)


## scale nummber ----
SN <- 24000


sn <- function(ground, map, units = "area") {

  # ground = MLA
  # map    = MLD
  
  # if (! is.null(MLA)) {
  if (units == "area") sn = sqrt(ground / map)
  if (units == "distance") sn = ground / map
  # }
  
  # if (!is.null(RF)) {
  #   ((1 / RF)^2) / 2.5*10^8
  # }
  return(sn)
}


# function of delineation size ----

## minimum legible area (MLA) ----
mla <- function(SN, MLD) {
  SN^2 * MLD
}

formatC(mla(SN2, mld_ac[4]), format)
mla(20000, mld_m2)
mla(24000, mld_m2)
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



# function of location accuracy ----
# FGDC-STD-007.3-1998 
# horizontal map accuracy (m)
# does corresponds to the vertex density standard of 15-meters in the NSSH 647?
12000 * units::set_units(1/30, inch) |> units::set_units(m)
12000 * 1/30 * 0.0254
12000 * 0.0008466667
15840 * units::set_units(1/30, inch) |> units::set_units(m)
15840 * 1/30 * 0.0254
24000 * units::set_units(1/50, inch) |> units::set_units(m)
24000 * 1/50 * 0.0254
24000 * 0.000508


# SN
1/(0.0008466667) 
1/(0.000508) 
1/(1/30 *0.0254)


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



# Compare ----
## SSM 1993 ----
# Table 2-1
SN_SSM_Table_2_1 <- data.frame(
  SN = c(
    order1 = 15840, 
    order2 = c(12000, 31680), 
    order3 = c(20000, 63360), 
    order4 = c(63360, 250000), 
    order5 = c(250000, 1000000)
    ))
SN_SSM_Table_2_1 |> 
  cbind(
    MLA = mla(SN_SSM_Table_2_1$SN, mld_ac[4]) |> signif(2) |> format(big.mark = ",", scientific = F)
  )
# these values are exactly what is printed in the text, except for 1:250,000


# Table 2-2
SN_SSM_Table_2_2 <- data.frame(
  SN = c(500, 2000, 5000, 7920, 10000, 12000, 15840, 20000, 24000, 31680, 62500, 63360, 100000, 125000, 250000)
)
SN_SSM_Table_2_2 |> 
  cbind(
    MLA = mla(SN_SSM_Table_2_2$SN, mld_ac[4]) |> signif(2) |> format(big.mark = ",", scientific = F)
  )
# these values are exactly what is printed in the text


## SSM 2017 p.272-275 ----
SN_SSM_Table_4_4 <- data.frame(
  SN = c(order1 = 15840, order2 = c(12000, 31680), order3 = c(20000, 63360), order4 = c(63360, 250000), order5 = c(250000, 1000000))
)
SN_SSM_Table_4_4 |>
  cbind(
    MLA = mla(SN_SSM_Table_4_4$SN, mld_ha[4]) |> signif(2) |> format(big.mark = ",", scientific = FALSE)
    )
# these values are exactly what is printed in the text, except for 1:250,000



## NSSH 648 1993 ----
NSSH_648_1993 <- data.frame(
  SN        = c(
    SSURGO   =       NA, 
    STATSGO  =   250000, 
    LRU      =   250000, 
    MLRA     =  7500000, 
    LLR      = 10000000),
  MLA_acres = c(
    SSURGO = NA, STATSGO = NA,
    LRU = NA,     MLRA = 1434803, LRR = NA)
)
NSSH_648_1993 |> 
  cbind(
    MLA_calc = mla(NSSH_648_1993$SN, mld_ac[6]) |> 
      format(big.mark = ",", scientific = FALSE)
    )
# these values are close to what is printed in the text, using the 1 cm^2 MLD


## NSSH 648 2017 ----
# this version has a typo, the 2024 corrected these numbers using 1 cm^2 except for the SSURGO dataset which should be 0.4 cm^2
NSSH_648_2017 <- data.frame(
  SN = c(
    SSURGO  = c(CONUS =   24000, AK =      NA),
    STATSGO = c(CONUS =  250000, AK =  500000),
    LRU     = c(CONUS = 1000000, AK = 5000000), 
    MLRA    = c(CONUS = 5000000, AK = 7500000), 
    LLR     = c(CONUS = 7500000, AK = 10000000)),
  MLA_acres = c(
    SSURGO  = c(       5, NA),
    STATSGO = c(     623, NA),
    LRU     = c(   10000, NA),     
    MLRA    = c(  250000, NA), 
    LRR     = c(  560000, NA)),
  MMA_acres = c(
    SSURGO  = c(      NA, NA),
    STATSGO = c(    1000, NA),
    LRU     = c(  100000, NA),
    MLRA    = c( 1000000, NA), 
    LRR     = c(20000000, NA))
)
NSSH_648_2017 |> 
  cbind(
    MLA_calc = mla(NSSH_648_2017$SN, mld_ac[4]) |> 
      signif(2) |> 
      format(big.makr = ", ", scientific = FALSE)
    )
# these values are close to what is printed in the text, using the 0.4 cm^2 MLD


NSSH_648_2017 |> 
  cbind(
    MLA_calc = mla(NSSH_648_2017$SN, mld_ac[6]) |> 
      signif(2) |> 
      format(big.makr = ", ", scientific = FALSE)
    )
# these values are NOT close to what is printed in the text, using the 1 cm^2 MLD
# these values are correct to what is printed in the 2024 text, using the 1 cm^2 MLD


## NSSH 648 2024 ----
# this version has a typo, the SSURGO dataset which should be 0.4 cm^2
NSSH_648_2024 <- data.frame(
  SN = c(
    SSURGO  = c(CONUS =   12000, AK =      NA),
    STATSGO = c(CONUS =  250000, AK =  500000),
    LRU     = c(CONUS = 1000000, AK = 5000000), 
    MLRA    = c(CONUS = 5000000, AK = 7500000), 
    LLR     = c(CONUS = 7500000, AK = 10000000)),
  MLA_acres = c(
    SSURGO  = c(      5, NA),
    STATSGO = c(   1545, NA),
    LRU     = c(  25000, NA),     
    MLRA    = c( 620000, NA), 
    LRR     = c(1400000, NA))
)
NSSH_648_2024 |> 
  cbind(
    MLA_ha = mla(NSSH_648_2024$SN, mld_ha[6]) |> 
      signif(2) |> 
      format(big.makr = ", ", scientific = FALSE),
    MLA_ac = mla(NSSH_648_2024$SN, mld_ac[6]) |> 
      signif(2) |> 
      format(big.makr = ", ", scientific = FALSE)
  )
# these values are close to what is printed in the text, using the 1 cm^2 MLD
# only the 

NSSH_648_2024 |> 
  cbind(
    MLA_calc = mla(NSSH_648_2017$SN, mld_ac[6]) |> 
      signif(2) |> 
      format(big.makr = ", ", scientific = FALSE)
  )
# these values are NOT close to what is printed in the text, using the 1 cm^2 MLD
# these values are correct to what is printed in the 2024 text, using the 1 cm^2 MLD


## Landing Page ----
SN_LP <- c(`SSURGO` = 12000, `SSURGO` = 24000, STATSGO = 250000, LRU = 1000000, MLRA = 3500000, LRR = 7500000)
mla(SN_LP, mld_ac[4]) |> signif(2) |> format(big.mark = ",", scientific = F)


## rasters ----
SN1 <- as.numeric(sn(c(5, 10, 30, 100, 1000)^2, mld_m2[1]))
names(SN1) <- c("5-meter", "10-meter", "30-meter", "100-meter", "1-kilometer")


## Polygons ----
p <- c(0, 0.05, 0.1, 0.5, 0.95, 1)


### MLRA ----
idx_conus_mlra <- st_intersects(mlra52, sapol) |> 
  lapply(function(x) length(x) > 1) |> 
  unlist()

sn(quantile(mlra$acres[idx_conus_mlra], p), mld_ac[4]) |> signif(2) |> format(big.mark = ",", scientific = FALSE)
# 0.4 cm^2 the 0th percentile is close to the 1:7,500,000 quoted in the 1993 NSSH Part 648

sn(quantile(mlra52$acres[idx_conus_mlra], p), mld_ac[6]) |> signif(2) |> format(big.mark = ",", scientific = FALSE)
# 0.4 cm^2 the 0th percentile is close to the 1:7,500,000 quoted in the 1993 NSSH Part 648


### STATSGO ----
idx_conus_statsgo <- st_intersects(statsgo, sapol) |> 
  lapply(function(x) length(x) > 1) |> 
  unlist()

sn(quantile(statsgo$acres[idx_conus_statsgo], p), mld_ac[4]) |> signif(2) |> format(big.mark = ",", scientific = FALSE)
sn(quantile(statsgo$acres[idx_conus_statsgo], p), mld_ac[6]) |> signif(2) |> format(big.mark = ",", scientific = FALSE)
# 1 cm^2 the 5th percentile is close to the 1:7,500,000 quoted in the 1993 NSSH Part 648
# the 0th percentile is close to the 1:5,000,000 quoted in the 2017 NSSH Part 648



# table for printing ----
SN1 <- as.numeric(sn(c(5, 10, 30, 100, 1000)^2, mld_m2[1]) * 2)
names(SN1) <- c("5-meter", "10-meter", "30-meter", "100-meter", "1-kilometer")

SN2 <- c(`SSURGO` = 12000, `SSURGO` = 24000, STATSGO = 250000, LRU = 1000000, MLRA = 5000000, LRR = 7500000)

SN2 <- c(SN1, SN2)
SN2


i1 <- rep(1, 5)
i2 <- rep(1, 2)
i3 <- rep(1, 4)
ac <- mla(SN2, c(mld_ac[1][i1], mld_ac[4][i2], mld_ac[6][i3])) * c(i1, i2, i3)
ha <- mla(SN2, c(mld_ha[1][i1], mld_ha[4][i2], mld_ac[6][i3])) * c(i1, i2, i3)
m2 <- mla(SN2, c(mld_m2[1][i1], mld_m2[4][i2], mld_ac[6][i3])) * c(i1, i2, i3)
m  <- signif(sqrt(as.numeric(m2)), 2) / (c(i1,  i2, i3) * 2)
df <- data.frame(
  SN = signif(as.numeric(SN2), 2), 
  `MLA ac` = ac, 
  `MLA ha` = ha, 
  `MLA m2` = m2, 
  `MLA m` =  m
  )
df <- cbind(`Examples` = names(SN2), df)
df[2:6] <- lapply(df[2:6], function(x) {
  x |> as.numeric() |> signif(2) |> format(big.mark = ",", scientific = FALSE)
})
row.names(df) <- NULL
df

test <- sapply(SN2, function(x) n(x, 50000, mld_ac[4], c(2, 4, 10)))
test <- t(test)
test <- data.frame(Example = row.names(test), test)
row.names(test) <- NULL
test[2:4] <- lapply(test[2:4], function(x) format(signif(x, 2), scientific = FALSE, big.interval = ","))
names(test)[2:4] <- paste0("MLD x ", c(2, 4, 10))
knitr::kable(test, caption = "Number of observations per 1km2 (i.e. 247 acres)", align = "r")
