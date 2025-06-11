

# load packages ----
library(soilDB)
library(dplyr)
library(sf)
library(units)
library(ggplot2)
library(pdqr)
library(USAboundaries)


# load data ----
fp <- "D:/geodata/soils"

data(state)
conus_idx <- !state.abb %in% c("AK", "HI")
conus_abb <- state.abb[conus_idx]
conus_sn  <- state.name[conus_idx]
# ? oconus <- c("AK", "HI", "AS", "FM", "GU", "PM", "MH", "MP", "MX", "VI")

# stpol <- read_sf("D:/geodata/government_units/GovernmentUnits_National_GPKG.gpkg", layer = "GU_CountyOrEquivalent")
# stpol_conus <- subset(stpol, STATE_NAME %in% conus_sn)
stpol <- us_states()
stpol <- subset(stpol, stusps %in% conus) |>
  st_transform(crs = 5070)
  

## NASIS ----
nasis_le <- get_legend_from_SDA(WHERE = "areasymbol LIKE '%'")
nasis_le_o1 <- soilDB::dbQueryNASIS(
  soilDB::dbConnectNASIS(),
  "SELECT areasymbol, muiid, musym, muname, dmuinvesintens
  
  FROM       datamapunit dmu
  INNER JOIN correlation co  ON co.dmuiidref = dmu.dmuiid
  INNER JOIN mapunit     mu  ON mu.muiid     = co.muiidref
  INNER JOIN lmapunit    lmu ON lmu.muiidref = mu.muiid
  INNER JOIN legend      l   ON l.liid       = lmu.liidref
  INNER JOIN area        a   ON a.areaiid    = l.areaiidref
  
  WHERE areasymbol IN ('NJ017', 'NV764', 'NY005', 'NY047', 'NY059', 'NY061', 'NY081', 'NY085', 'RI600', 'SOH061')
  ;"
)

nasis_mu_o1 <- get_mapunit_from_NASISWebReport(c("NJ017", "NV764", "NY005", "NY047", "NY059", "NY061", "NY081", "NY085", "RI600", "SOH061"))



## SSURGO ----
ssurgo_fp <- file.path(fp, "SSURGO/SSURGO_20241001_gpkg/ALL_SSURGO_10_01_2024.gpkg")
ssurgo_conn <- DBI::dbConnect(RSQLite::SQLite(), dbname = ssurgo_fp)

ssurgo_le <- DBI::dbReadTable(ssurgo_conn, name = "legend")  
ssurgo_mu <- DBI::dbReadTable(ssurgo_conn, name = "mapunit")  
ssurgo_co <- DBI::dbReadTable(ssurgo_conn, name = "component")  
ssurgo_h  <- DBI::dbReadTable(ssurgo_conn, name = "chorizon")  


# aggregate map units to legend
ssurgo_le2 <- ssurgo_mu |>
  group_by(lkey) |>
  summarize(
    n_mukey = length(mukey)
  ) |>
  ungroup() |>
  right_join(ssurgo_le, by = "lkey")


### aggregate components to legend ----
ssurgo_mu_avg <- ssurgo_co |>
  # aggregate map units
  group_by(mukey) |>
  summarize(
    n_components  = length(cokey),
    n_majcompflag = sum(majcompflag == "Yes", na.rm = TRUE)
    ) |>
  ungroup() |>
  inner_join(ssurgo_mu[c("lkey", "mukey", "muacres", "invesintens")], by = "mukey") |>
  # aggregate legends
  group_by(lkey, invesintens) |>
  summarize(
    muacres = sum(muacres),
    n_mukey = length(mukey),
    avg_comp = mean(n_components),
    avg_mcom = mean(n_majcompflag)
  ) |>
  ungroup() |>
  tidyr::pivot_wider(
    id_cols = lkey, 
    names_from = invesintens,
    values_from = c(muacres, n_mukey, avg_comp, avg_mcom)
  )
test <- ssurgo_mu_avg[grepl("muacres_Order", names(ssurgo_mu_avg))] |> as.matrix()
test[is.na(test)] <- 0
ssurgo_mu_avg$sso_dom <- colnames(test)[max.col(test)] |> gsub("muacres_", "", x = _)
ssurgo_le3 <- merge(ssurgo_le2, ssurgo_mu_avg, by = "lkey", all.x = TRUE, sort = FALSE)


sapol     <- read_sf(ssurgo_fp, layer = "SAPOLYGON") |>
  rmapshaper::ms_simplify()
sapol$m2  <- sapol |> st_transform(crs = "ESRI:54009") |> st_area()
sapol$acres <- sapol$m2 |> units::set_units(acres)
sapol_tot <- sapol |>
  st_drop_geometry() |>
  group_by(areasymbol) |>
  summarize(acres = sum(acres, na.rm = TRUE))



## MLRA ----
mlra_fp <- "D:/geodata/soils/MLRA"
mlra31  <- read_sf(file.path(mlra_fp, "mlra_v31_l48/mlra_v31_l48.shp"))
mlra52  <- read_sf(file.path(mlra_fp, "MLRA_52.shp"))

mlra52$m2     <- mlra52 |> st_transform(crs = "ESRI:54009") |> st_area()
mlra52$acres  <- units::set_units(mlra52$m2, acres)

mlra31 <- mlra31 |> st_transform(crs = "ESRI:102004")
mlra52 <- mlra52 |> st_transform(crs = "ESRI:102004")



## STATSGO ----
statsgo <- read_sf("D:/geodata/soils/STATSGO2/wss_gsmsoil_US_[2016-10-13]/spatial/gsmsoilmu_a_us.shp")
statsgo$m2  <- statsgo |> st_transform(crs = "ESRI:54009") |> st_area()
statsgo$cm2 <- units::set_units(statsgo$m2, cm^2)
statsgo$acres <- units::set_units(statsgo$m2, acres)



# SSURGO & STATSGO2 map scales
le <- get_legend_from_SDA(WHERE = "areasymbol LIKE '%'")
# mu <- get_mapunit_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS/gSSURGO_CONUS.gdb") 

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

# save ----
# save.image("C:/workspace2/prez_ncss_soil-information.RData")
load("C:/workspace2/prez_ncss_soil-information.RData")



# soil survey order ----
vars <- c("areasymbol", "muiid", "musym", "musym", "dmuinvesintens")
nasis_mu2 <- nasis_mu[vars]
names(nasis_mu2)[c(2,,5)] <- c("mukey", "invesintens")

vars <- c("areasymbol", "mukey", "musym", "musym", "invesintens")
sso <- ssurgo_mu |>
  left_join(ssurgo_le, by = "lkey") |>
  mutate(
    state = substr(areasymbol, 1, 2),
    Alaska = state == "AK"
    ) |>
  # cbind(.[vars], nasis_mu) |>
  filter(!is.na(invesintens)) # & invesintens != "Order 1")
sso |>
  ggplot(aes(x = invesintens, y = projectscale)) +
  geom_boxplot() +
  scale_y_log10(breaks = c(0, 12000, 24000, 100000, 250000))

test <- ssurgo_mu |>
  filter(!is.na(invesintens)) |>
  group_by(lkey, invesintens) |>
  summarize(sso_muacres = sum(muacres, na.rm = TRUE)) |>
  group_by(lkey) |>
  summarize(`Soil Survey` = invesintens[which.max(sso_muacres)]) |>
  ungroup()
test <- left_join(sapol, test, by = "lkey")
test |>
  filter(substr(areasymbol, 1, 2) %in% conus) |>
  subset(!is.na(`Soil Survey`)) |>
  # mutate(`Soil Survey` = droplevels(`Soil Survey`)) |>
  st_transform(crs = 5070) |>
  ggplot(aes(fill = `Soil Survey`)) +
  geom_sf() +
  scale_fill_viridis_d() +
  coord_sf(crs = 5070) +
  geom_sf(data = stpol, fill = NA, lwd = 0.1, col = "white") +
  theme(legend.position = "right", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank()) +
  ggtitle("Soil Survey Order (SSO)")
  


### survey area complexity ----
filter(ssurgo_le3, !is.na(sso)  & sso != "Order 1") |>
  mutate(sso = as.factor(sso)) |>
  ggplot(aes(y = n_mukey / areaacres, x = as.factor(projectscale/1000), fill = sso)) + 
  geom_boxplot() + 
  # facet_grid(~sso) +
  # ylim(0, 5) +
  xlab("projectscale/1000") + 
  scale_y_log10() +
  ggtitle("Legend Size by Map Scale and Survey Order")

lm(n_mukey ~ log(areaacres) + log(projectscale) + sso, data = ssurgo_le3) |> summary()


test <- merge(sapol, ssurgo_le3, by = c("lkey", "areasymbol"), all.x = TRUE)
test$n_mukey_aa <- test$n_mukey / test$areaacres * 10000
brks <- quantile(test$n_mukey_aa, probs = seq(0, 1, 0.2))
brks <- c(0, 1, 2, 4, 8, 16, 120)
test$`n mapunits / \n 10K acres` <- cut(test$n_mukey_aa, breaks = brks)

test |>
  filter(substr(areasymbol, 1, 2) %in% conus) |>
  ggplot() +
  geom_sf(aes(fill = `n mapunits / \n 10K acres`)) + #, lty = NA)) +
  scale_fill_viridis_d() +
  # scale_fill_viridis_c(transform = "log") +
  coord_sf(crs = st_crs(5070)) +
  ggtitle("Number of map units per 10,000 acres")
  


# sampling density ----
nssh_sd <- function(acres) {
  n <- NA
  n[acres <  2000]  <- 30
  n[acres >= 2000]  <- 40
  n[acres >= 6000]  <- 50
  n[acres >= 10000] <- 60
  
  return(n)
}
ssurgo_mu$n <- nssh_sd(ssurgo_mu$muacres)

# range of sample sizes
quantile(ssurgo_mu$n, probs = seq(0, 1, 0.1), na.rm = TRUE)


# sample size of SSURGO
sum(ssurgo_mu$n, na.rm = TRUE) |> formatC(big.mark = ",", format = "fg")




# sample size per survey
sso_n <- ssurgo_mu |>
  group_by(lkey) |>
  summarize(n = sum(n, na.rm = TRUE)) |>
  ungroup()
sso_n <- merge(sapol, sso_n, by = "lkey", all.y = TRUE) |>
  subset(substr(areasymbol, 1, 2) %in% conus) |>
  st_transform(crs = 5070)
brks <- quantile(sso_n$n, probs = seq(0, 1, 0.25))
brks <- c(0, 2000, 3000, 5000, 10000, 27000)
lbls  <- paste0(
  formatC(brks, big.mark = ",", format = "fg"), 
  " to ", 
  formatC(brks[-1]-1, big.mark = ",", format = "fg"
  )
)[-6]
sso_n$n <- cut(sso_n$n, breaks = brks, labels = lbls, right = FALSE)

ggplot(sso_n) +
  geom_sf(aes(fill = n)) +
  # scale_fill_viridis_c(transform = "log") +
  scale_fill_viridis_d() +
  coord_sf(crs = st_crs(5070)) +
  geom_sf(data = stpol, fill = NA, lwd = 0.25, col = "white") +
  theme(legend.position = "right", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank()) +
  ggtitle("Estimated sample size (n) required per soil survey area")



# cartographic scale ----
## ESRI File Geodatabase ----
dsn <- "D:/geodata/soils/SSURGO/SSURGO_CONUS_20231001/gSSURGO_CONUS_20231001.gdb"
sapol <- read_sf(dsn, layer = "SAPOLYGON") |>
  select(1:3) |>
  group_by(AREASYMBOL, SPATIALVER, LKEY) |>
  summarize() %>%
  ungroup() |>
  rmapshaper::ms_simplify()
sapol$acres <- st_area(sapol) |> units::set_units(acres)

le <- get_legend_from_GDB(dsn = dsn, stats = TRUE)
le$mla <- le$projectscale^2 * 0.4 |> units::set_units("cm^2") |> units::set_units("acres") |> as.numeric()

mu <- get_mapunit_from_GDB(dsn = dsn, stats = TRUE)
co <- get_component_from_GDB(dsn = dsn)

mupol_stats_gdb <- lapply(sort(sapol$AREASYMBOL), function(x) {
  
  cat("processing", x, as.character(Sys.time()), "\n")
  
  sapol2 <- sapol |>
    subset(AREASYMBOL == x) |> 
    st_cast("MULTILINESTRING")
  
  temp <- read_sf(
    dsn   = dsn, 
    query = paste0("SELECT * FROM MUPOLYGON WHERE AREASYMBOL = '", x, "'")
  )
  
  # change names to lowercase
  nm  <- names(temp)
  idx <- length(nm)
  names(temp)[-idx] <- tolower(nm[-idx])
  
  # calculate acres
  temp$acres <- st_area(temp) |> units::set_units(acres) |> as.numeric()
  
  # label border polygons
  idx <- st_intersects(temp, sapol2) |> sapply(function(x) length(x) > 0)
  temp$border <- idx
  
  temp <- st_drop_geometry(temp)
  return(temp)
})

mupol_stats_gdb <- data.table::rbindlist("rbind", mupol_stats_gdb)
fp <- file.path("D:/geodata/soils/SSURGO/SSURGO_CONUS_20231001")
# data.table::fwrite(mupol_stats_gdb, file.path(fp, "mupol_stats_gdb.csv"))
mupol_stats_gdb <- data.table::fread(file.path(fp, "mupol_stats_gdb.csv"))



# transform ----
pat <- "^water$|^water,|^water |^water-riverwash|^water-perennial" #^water-|-water-|-water "
mu2 <- mu |>
  mutate(mu_water = grepl(pat, tolower(mu$muname)))
table(mu2$muname[mu2$mu_water > 0 | grepl(pat, tolower(mu2$muname))]) |> 
  sort(decreasing = TRUE)


co_ma_mukey <- co |>
  mutate(
    compkind = tolower(compkind)
  ) |>
  arrange(mukey, -comppct_r) |>
  group_by(mukey) |>
  summarize(
    # domninant component
    compkind_dom   = compkind[1],
    compname_dom   = compname[1],
    compct_r_sum   = sum(comppct_r, na.rm = TRUE),
    
    # miscellaneous areas
    comppct_r_ma   = sum(compkind == 'miscellaneous area', na.rm = TRUE),
    majcompflag_ma = sum(compkind == 'miscellaneous area' & majcompflag == 'Yes', na.rm = TRUE),
    
    # water
    comppct_r_w   = sum(compkind == 'miscellaneous area' & grepl("water", compname), na.rm = TRUE),
    majcompflag_w = sum(compkind == 'miscellaneous area' & majcompflag == 'Yes' & grepl("water", compname), na.rm = TRUE)
  ) |>
  ungroup() |>
  inner_join(select(mu2, mukey, musym, muname, mukind, mu_water), by = "mukey") |>
  mutate(
    mukey    = as.integer(mukey),
    compkind_dom_mukind = paste0(compkind_dom, "-", tolower(mukind))
  )


mupol_stats_gdb2 <- mupol_stats_gdb |>
  inner_join(co_ma_mukey, by = c("mukey", "musym")) |>
  inner_join(
    select(mu, mukey, lkey) |> 
      mutate(mukey = as.integer(mukey)), 
    by = "mukey"
  ) |>
  inner_join(select(le, lkey, mla), by = "lkey")


test <- mupol_stats_gdb2 |>
  filter(border == FALSE) |>
  group_by(areasymbol) |>
  summarize(
    min_muaa = min(acres),
    avg_muaa = mean(acres),
    max_muaa = max(acres),
    n_mupolygonkey  = length(musym),
    
    n_mla      = sum(acres < round(mla, 2)),
    n_10_1     = sum(acres < 1),
    n_12_1p4   = sum(acres < 1.4),
    n_15_2p5   = sum(acres < 2.5),
    n_20_4     = sum(acres < 4),
    n_24_5p7   = sum(acres < 5.7),
    n_63_40    = sum(acres < 40),
    n_250_620  = sum(acres < 620),
    
    pct_mla     = sum(acres[acres < round(mla, 2)]) /
      sum(acres),
    pct_10_1    = sum(acres[acres < 1])   / sum(acres),
    pct_12_1p4  = sum(acres[acres < 1.4]) / sum(acres),
    pct_15_2p5  = sum(acres[acres < 2.5]) / sum(acres),
    pct_20_4    = sum(acres[acres < 4])   / sum(acres),
    pct_24_5p7  = sum(acres[acres < 5.7]) / sum(acres),
    pct_63_40   = sum(acres[acres < 40])  / sum(acres),
    pct_250_620 = sum(acres[acres < 620]) / sum(acres),
    min_muaa = min(acres),
    avg_muaa = mean(acres),
    max_muaa = max(acres),
    n_mupolygonkey  = length(musym),
    
    # miscellaneous areas
    n_mla_ma      = sum(acres < round(mla, 2) & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_10_1_ma     = sum(acres < 1             & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_12_1p4_ma   = sum(acres < 1.4           & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_15_2p5_ma   = sum(acres < 2.5           & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_20_4_ma     = sum(acres < 4             & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_24_5p7_ma   = sum(acres < 5.7           & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_63_40_ma    = sum(acres < 40            & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    n_250_620_ma  = sum(acres < 620           & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")),
    
    pct_mla_ma     = sum(acres[acres < round(mla, 2) & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) /
      sum(acres),
    pct_10_1_ma    = sum(acres[acres < 1   & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_12_1p4_ma  = sum(acres[acres < 1.4 & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_15_2p5_ma  = sum(acres[acres < 2.5 & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_20_4_ma    = sum(acres[acres < 4   & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_24_5p7_ma  = sum(acres[acres < 5.7 & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_63_40_ma   = sum(acres[acres < 40  & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    pct_250_620_ma = sum(acres[acres < 620 & compkind_dom_mukind %in% c("miscellaneous area-consociation", "miscellaneous area-NA")]) / sum(acres),
    
    # water
    n_mla_w      = sum(acres < round(mla, 2) & mu_water == TRUE),
    n_10_1_w     = sum(acres < 1             & mu_water == TRUE),
    n_12_1p4_w   = sum(acres < 1.4           & mu_water == TRUE),
    n_15_2p5_w   = sum(acres < 2.5           & mu_water == TRUE),
    n_20_4_w     = sum(acres < 4             & mu_water == TRUE),
    n_24_5p7_w   = sum(acres < 5.7           & mu_water == TRUE),
    n_63_40_w    = sum(acres < 40            & mu_water == TRUE),
    n_250_620_w  = sum(acres < 620           & mu_water == TRUE),
    
    pct_mla_w     = sum(acres[acres < round(mla, 2) & mu_water == TRUE]) /
      sum(acres),
    pct_10_1_w    = sum(acres[acres < 1   & mu_water == TRUE]) / sum(acres),
    pct_12_1p4_w  = sum(acres[acres < 1.4 & mu_water == TRUE]) / sum(acres),
    pct_15_2p5_w  = sum(acres[acres < 2.5 & mu_water == TRUE]) / sum(acres),
    pct_20_4_w    = sum(acres[acres < 4   & mu_water == TRUE]) / sum(acres),
    pct_24_5p7_w  = sum(acres[acres < 5.7 & mu_water == TRUE]) / sum(acres),
    pct_63_40_w   = sum(acres[acres < 40  & mu_water == TRUE]) / sum(acres),
    pct_250_620_w = sum(acres[acres < 620 & mu_water == TRUE]) / sum(acres)
  ) |>
  ungroup() |>
  st_drop_geometry() |>
  as.data.frame()


test |> 
  dplyr::summarize(
    lt_n       = sum(n_mla,       na.rm = TRUE) / sum(n_mupolygonkey),
    lt_ma      = sum(n_mla_ma,    na.rm = TRUE) / sum(n_mupolygonkey),
    lt_w       = sum(n_mla_w,     na.rm = TRUE) / sum(n_mupolygonkey),
    lt_acre    = mean(pct_mla,    na.rm = TRUE) / sum(n_mupolygonkey),
    lt_acre_ma = mean(pct_mla_ma, na.rm = TRUE) / sum(n_mupolygonkey),
    lt_acre_w  = mean(pct_mla_w,  na.rm = TRUE) / sum(n_mupolygonkey)
  )

fn <- function(x) {
  x2 = fivenum(x) 
  x2 = c(Min = x2[1], `1st Qu.` = x2[2], Median = x2[3], Mean = mean(x), `3rd Qu.` = x2[4], Max = x2[5]) 
  return(x2)}
test2 <- apply(test[grepl("pct_mla|n_mla", names(test))], 2, fn) |> as.data.frame()
idx <- grepl("n_", names(test2))
test2[idx] <- round(test2[idx])
idx <- grepl("pct_", names(test2))
test2[idx] <- round(test2[idx], 2)
test2 |> View()


test_agg <- aggregate(. ~ areasymbol, data = test[-c("areasymbol")], quantile)



vars <- names(test[grepl("n_|pct_", names(test))])
le2_lo <- reshape(
  test,
  direction = "long",
  timevar = "var", times = vars,
  v.names = "value", varying = vars
)
le2_lo <- merge(sapol, le2_lo, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)


# le2 <- le2_lo <- merge(sapol, mupol_stats, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)
# names(le2_lo) <- gsub("_muareaacres_lt", "", names(le2_lo))

## Number of polygons smaller than the minimum legible area ----
test2 <- subset(le2_lo, var %in% c("n_mla")) # , "n_12000_1p4", "n_24000_5p7"))
test2$`Count (N)` <- cut(
  test2$value, 
  breaks = c(0, 100, 1000, 5000, 70000),
  labels = c("0 to 99", "100 to 999" , "1,000 to 4,999", "5,000 to 70,000"),
  right = FALSE
  )

gg_n_mla <- test2 |>
  ggplot(aes(fill = `Count (N)`)) +
  geom_sf() +
  geom_sf(data = stpol, fill = NA, lwd = 0.25, col = "white") +
  scale_fill_viridis_d() +
  coord_sf(crs = st_crs(5070))  +
  theme(legend.position = "bottom", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
        )


## Percent of polygons smaller than the minimum legible area ----
test2 <- subset(le2_lo, var %in% c("pct_mla")) # , "n_12000_1p4", "n_24000_5p7"))
test2$`Percent (%)` <- cut(
  test2$value * 100, 
  breaks = c(0, 1, 5, 10, 20),
  labels = c("0 to 1", "1 to 5" , "5 to 10", "10 to 20"),
  right = FALSE
)

gg_pct_mla <- ggplot(test2, aes(fill = `Percent (%)`)) +
  geom_sf() +
  scale_fill_viridis_d() +
  geom_sf(data = stpol, fill = NA, lwd = 0.25, col = "white") +
  coord_sf(crs = st_crs(5070))  +
  theme(legend.position = "bottom", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
        )



png("test.png", width = 17, height = 7, units = "in", res = 300, pointsize = 40)
gridExtra::arrangeGrob(gg_n_mla, gg_pct_mla, nrow = 1, top = "Polygons smaller than the minimum legible area") |>
  plot()
dev.off()



# accuracy ----
ssurgo_mu_acc <- ssurgo_co |>
  group_by(mukey) |>
  summarize(
    ua = max(comppct_r, na.rm = TRUE),
    comppct_r_max = max(comppct_r, na.rm = TRUE)
    ) |>
  ungroup() |>
  right_join(ssurgo_mu, by = "mukey") |>
  mutate(
    comppct_r_max_muacres = muacres * comppct_r_max/100,
    )
ssurgo_le_acc <- ssurgo_mu_acc |>
  group_by(lkey) |>
  summarize(
    ua_avg = mean(ua, na.rm = TRUE)/100,
    ua_min = min(ua,  na.rm = TRUE)/100,
    ua_max = max(ua,  na.rm = TRUE)/100,
    oa = sum(comppct_r_max_muacres, na.rm = TRUE) / sum(muacres, na.rm = TRUE)
  ) |>
  ungroup()
ssurgo_le_acc <- merge(sapol, ssurgo_le_acc, by = "lkey", all.x = TRUE)


brks <- c(0, 0.5, 0.75, 0.9, 1)
idx <- length(brks)
lbls  <- paste0(brks[-idx], " to ", brks[-1]) 

ssurgo_le_acc |>
  subset(
    substr(areasymbol, 1, 2) %in% conus 
    ) |>
  transform(`Overall accuracy` = cut(oa, breaks = brks, labels = lbls, right = FALSE)) |>
  subset(!is.na(`Overall accuracy`)) |>
  mutate(`Overall accuracy` = droplevels(`Overall accuracy`)) |>
  st_transform(crs = 5070) |>
  ggplot() +
  geom_sf(aes(fill = `Overall accuracy`)) +
  scale_fill_viridis_d() +
  coord_sf(crs = st_crs(5070)) +
  geom_sf(data = stpol, fill = NA, lwd = 0.25, col = "white") +
  ggtitle("Internal estimate of overall accuracy of survey areas")  +
  theme(legend.position = "right", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)
  )


ssurgo_le_acc |>
  subset(substr(areasymbol, 1, 2) %in% conus) |>
  transform(`Average user accuracy` = cut(ua_avg, breaks = brks, labels = lbls)) |>
  ggplot() +
  geom_sf(aes(fill = `Average user accuracy`)) +
  # scale_fill_viridis_c(breaks = seq(0, 1, 0.2)) + #transform = "log") +
  scale_fill_viridis_d() +
  coord_sf(crs = st_crs(5070)) +
  ggtitle("Internal estimate of average user accuracy of survey areas")



# entropy ----

## survey area entropy ----
test <- ssurgo_le |>
  inner_join(ssurgo_mu, by = "lkey") |>
  mutate(muacres_pct = muacres / areaacres)

test2 <- test |>
  group_by(areasymbol, areaacres, projectscale) |>
  summarize(h = aqp::shannonEntropy(muacres_pct),
            n_mukeys = length(mukey)
            ) |>
  ungroup()
test3 <- test2 |> inner_join(ssurgo_le3[c("areasymbol", "sso_dom")], by = "areasymbol")

ggplot(test3, aes(x = as.factor(projectscale/1000), y = h/areaacres, fill = sso_dom)) +
  geom_boxplot() +
  scale_y_log10() +
  xlab("projectscale/1000") +
  ggtitle("Legend entropy by map scale and survey order")



# variance ----

nms  <- names(ssurgo_h)
pat  <- "_l$|_r$|_h$"
vars <- nms[grepl(pat, nms)] |> gsub(pat, "", x = _) |> unique()

ssurgo_h_vars <- lapply(vars, function(x) {
  
  cat(x, as.character(Sys.time()), "\n")
  
  idx <- which(grepl(x, nms))
  n_mis <- ssurgo_h[idx] |> is.na() |> rowSums()
  n_rv_mis <- ssurgo_h[idx[[2]]] |> is.na() |> rowSums()
  n_mis <- data.frame(n_rv_mis = n_rv_mis, n_mis = n_mis)
  names(n_mis) <- paste0(x, "_", names(n_mis))
  
  return(n_mis)
})
ssurgo_h_vars <- do.call("cbind", ssurgo_h_vars)

idx_lrh <- which(as.integer(ssurgo_h_vars$claytotal_n_mis) == 0)

# new distributions
samp_p <- function(x) unlist(x) |> new_p(type = "continuous", from = 0, to = 100) |>
  summ_quantile(probs = seq(0.01, 0.99, 0.01))
samp_var <- function(x) unlist(x) |> new_p(type = "continuous") |> summ_var()


x3 <- ssurgo_h[idx_lrh, paste0("claytotal", c("_l", "_r", "_h"))]

ssurgo_h_wi <- ssurgo_le[c("lkey", "areasymbol")] |>
  filter(substr(areasymbol, 1, 2) == "CO") |>
  inner_join(ssurgo_mu[c("mukey", "lkey", "muacres")],  by = "lkey") |>
  inner_join(ssurgo_co[c("cokey", "mukey", "comppct_r")], by = "mukey") |>
  inner_join(ssurgo_h, by = "cokey") |>
  aqp::hz_segment(intervals = c(0, 10), depthcols = c("hzdept_r", "hzdepb_r"))
ssurgo_h_wi$id <- row.names(ssurgo_h_wi)

vars <- paste0("claytotal", c("_l", "_r", "_h"))
idx_lrh <- ssurgo_h_wi[vars] |> is.na() |> rowSums()


library(future)
library(future.apply)

future::plan(future::multisession, workers = length(future::availableWorkers()) - 2)
test_wi <- future_apply(ssurgo_h_wi[idx_lrh < 1, vars],  1, samp_p, simplify = FALSE)
# test    <- future_apply(x3,         1, samp_p, simplify = FALSE)
plan(sequential)

# saveRDS(test, "ssurgo_h_samp-density.rds")
ssurgo_h_samp <- readRDS("ssurgo_h_samp-density.rds")

ssurgo_h_samp <- test_wi
ssurgo_h_samp <- do.call("rbind", ssurgo_h_samp)

apply(ssurgo_h_samp, 1, sd) |> summary()
ssurgo_h_samp_l <- ssurgo_h_samp |> t() |> as.data.frame() |> utils::stack()
ssurgo_h_wi_samp <- merge(ssurgo_h_wi, ssurgo_h_samp_l, by.x = "id", by.y = "ind", all.x = TRUE, sort = FALSE)

ssurgo_h_wi_samp$test <- ssurgo_h_wi_samp$value 
ssurgo_h_wi_samp <- ssurgo_h_wi_samp |>
  within({
    wt   = muacres * comppct_r * (hzdepb_r - hzdept_r)
    mukey = as.factor(mukey)
  })

as <- unique(ssurgo_h_wi_samp$areasymbol)

test_l <- lapply(as, function(x) {
  cat("fitting", x, as.character(Sys.time()), "\n")
  
  dat <- ssurgo_h_wi_samp[ssurgo_h_wi_samp$areasymbol == x, ]
  test_lm <- lm(test ~ mukey, data = dat, weights = wt, y = TRUE)
  
  pred <- test_lm$fitted.values
  obs  <- test_lm$y
  dat2 <- data.frame(obs, pred, wt = test_lm$weights)
  
  # des <- svydesign(id = ~ mukey, weights = ~wt, data = dat)
  # test_svylm <- svyglm(log10(test) ~ mukey, design = des)
  # obs  <- 10^test_svylm$fitted.values
  # pred <- 10^test_svylm$y
  
  # dat$pred <- predict(test_lm, newdata = dat, na.action = na.pass)^2
  R2   <- yardstick::rsq_trad(dat2, truth = obs, estimate = pred, case_weights = wt)
  RMSE <- yardstick::rmse(    dat2, truth = obs, estimate = pred, case_weights = wt)
  MAE  <- yardstick::mae(     dat2, truth = obs, estimate = pred, case_weights = wt)
  
  return(list(R2 = R2$.estimate, RMSE = RMSE$.estimate, MAE = MAE$.estimate))
})
# test_r2 <- sapply(test_l, function(x) x[[2]])
test_acc <- data.frame(
  R2   = sapply(test_l, function(x) x$R2),
  RMSE = sapply(test_l, function(x) x$RMSE),
  MAE  = sapply(test_l, function(x) x$MAE),
  areasymbol = as
  )
# test_r2 <- data.frame(r2 = unlist(test_l), areasymbol = as)
test <- inner_join(sapol, test_acc, by = "areasymbol") |>
  subset(!is.na(R2))
test_l <- tidyr::pivot_longer(test, cols = c("R2", "RMSE", "MAE"))


gg_R2 <- ggplot(test) +
  geom_sf(aes(fill = R2)) +
  scale_fill_viridis_c() +
  coord_sf(crs = st_crs(5070)) +
  theme(legend.position = "bottom", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
        )
  # ggtitle("Estimated R2 of clay % for the upper 10-cm")

gg_RMSE <- ggplot(test) +
  geom_sf(aes(fill = RMSE)) +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(crs = st_crs(5070))  +
  theme(legend.position = "bottom", 
        # panel.grid=element_blank(), 
        panel.background = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
        )
  # ggtitle("Estimated RMSE of clay % for the upper 10-cm")

gg_MAE <- ggplot(test) +
  geom_sf(aes(fill = MAE)) +
  scale_fill_viridis_c(direction = -1) +
  coord_sf(crs = st_crs(5070)) +
  theme(legend.position = "bottom", 
       # panel.grid=element_blank(), 
       panel.background = element_blank(),
       axis.text = element_blank(), 
       axis.ticks = element_blank(),
       legend.title = element_text(size = 16),
       legend.text = element_text(size = 16)
       )
  # ggtitle("Estimated MAE of clay % for the upper 10-cm")

png("test.png", width = 17, height = 7, units = "in", res = 300, pointsize = 40)
gridExtra::arrangeGrob(gg_R2, gg_RMSE, gg_MAE, nrow = 1, top = "Estimated soil survey area accuracy for clay % the upper 20-cm") |>
  plot()
dev.off()



h_ss <- function(x, var) {
  
  iv <- c("_l", "_r", "_h")
  x2 <- x[paste0(var, iv)]
  
 
  test_lrh <- apply(x2[idx_lrh, ], 1, function(y) {
    new_p(y, type = "continuous") |> summ_quantile(probs = seq(0.01, 0.99, 0.01))
  })
  
  x2[idx_lrh, ] |> dplyr::rowwise(data = _, function(x) new_p(x = x, type = "continuous") |> summ_quantile(probs = seq(0.01, 0.99, 0.01)))
}


h_sub <- aqp::hz_segment(ssurgo_h, c(0, 10), depthcols = c("hzdept_r", "hzdepb_r"))






col_idx <- which(gre)
