
library(soilDB)
library(sf)
library(rmapshaper)


# load data ----
## SAPOLYGON ----
sapol <- read_sf("D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", layer = "SAPOLYGON") |>
  select(1:3) |>
  group_by(AREASYMBOL, SPATIALVER, LKEY) |>
  summarize() %>%
  ungroup()
sapol$acres <- st_area(sapol) |> units::set_units(acres)


## legend ----
le <- get_legend_from_SDA(WHERE = "areasymbol LIKE '%'")
le$mla <- le$projectscale^2 * 0.4 |> units::set_units("cm^2") |> units::set_units("acres") |> as.numeric()
st <- substr(le$areasymbol, 1, 2) |> unique()

le <- get_legend_from_NASISWebReport(mlraoffice = "%", areasymbol = "%")
le$mla <- le$projectscale^2 * 0.4 |> units::set_units("cm^2") |> units::set_units("acres") |> as.numeric()
le <- within(le, {
  year   = format(cordate, "%Y") |> as.integer()
  decade = as.integer(year/10)*10
})
le <- le[-1689, ] # OR618


## mapunit ----
mu <- lapply(st, function(x) {
  cat("getting ", x, "\n")
  get_mapunit_from_SDA(WHERE = paste0("areasymbol LIKE '", x, "%'"))
})
mu <- do.call("rbind", mu)


mu <- get_mapunit_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", stats = TRUE)



## components ----
co <- get_component_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb")



# query data ----
## SDA ----
q <- function(x) paste0(
  "
  CREATE TABLE #MA1 (mukey INT, comppct_r_ma INT, majcompflag_ma INT, musym_w INT)
  
  INSERT INTO #MA1 (mukey, comppct_r_ma, majcompflag_ma, musym_w)
  
  SELECT
  co.mukey,
  SUM(CASE WHEN LOWER(compkind) = 'miscellaneous area' THEN comppct_r ELSE 0 END) comppct_r_ma,
  SUM(CASE WHEN LOWER(compkind) = 'miscellaneous area' AND majcompflag = 'Yes' THEN 1 ELSE 0 END) majcompflag_ma,
  SUM(CASE WHEN musym = 'W' OR musym = 'M-W' THEN 1 ELSE 0 END) musym_w
  
  FROM component co
  INNER JOIN mapunit mu ON mu.mukey = co.mukey
  INNER JOIN legend  le ON le.lkey  = mu.lkey
  
  
  WHERE areasymbol = '", x, "'
  
  GROUP BY co.mukey
  
  ----
  
  SELECT areasymbol, 
  
  MIN(muareaacres) min_muareaacres,
  AVG(muareaacres) avg_muareaacres,
  MAX(muareaacres) max_muareaacres,
  COUNT(mupolygonkey) n_mupolygonkey,
  
  ---- all areas
  SUM(CASE WHEN muareaacres < ", round(le$mla[le$areasymbol == x], 2)," THEN 1 ELSE 0 END) n_muareaacres_lt_mla, 
  SUM(CASE WHEN muareaacres < 1   THEN 1 ELSE 0 END) n_muareaacres_lt_10058_1, 
  SUM(CASE WHEN muareaacres < 1.4 THEN 1 ELSE 0 END) n_muareaacres_lt_12000_1p4, 
  SUM(CASE WHEN muareaacres < 2.5 THEN 1 ELSE 0 END) n_muareaacres_lt_15840_2p5, 
  SUM(CASE WHEN muareaacres < 4   THEN 1 ELSE 0 END) n_muareaacres_lt_20000_4, 
  SUM(CASE WHEN muareaacres < 5.7 THEN 1 ELSE 0 END) n_muareaacres_lt_24000_5p7, 
  SUM(CASE WHEN muareaacres < 40  THEN 1 ELSE 0 END) n_muareaacres_lt_63360_40, 
  SUM(CASE WHEN muareaacres < 620 THEN 1 ELSE 0 END) n_muareaacres_lt_250000_620, 
  
  SUM(CASE WHEN muareaacres < ", round(le$mla[le$areasymbol == x], 2)," THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_mla, 
  SUM(CASE WHEN muareaacres < 1   THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_10058_1, 
  SUM(CASE WHEN muareaacres < 1.4 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_12000_1p4, 
  SUM(CASE WHEN muareaacres < 2.5 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_15840_2p5, 
  SUM(CASE WHEN muareaacres < 4   THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_20000_4, 
  SUM(CASE WHEN muareaacres < 5.7 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_24000_5p7, 
  SUM(CASE WHEN muareaacres < 40  THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_63360_40, 
  SUM(CASE WHEN muareaacres < 620 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_250000_620,

  ---COUNT(mupolygonkey) OVER(PARTITION BY areasymbol) n_mupolygonkey,
  ---MIN(muareaacres) OVER(PARTITION BY areasymbol) min_muareaacres,
  ---AVG(muareaacres) OVER(PARTITION BY areasymbol) avg_muareaacres,
  ---MAX(muareaacres) OVER(PARTITION BY areasymbol) max_muareaacres,

  ---PERCENTILE_CONT(0.05) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct05_muareaacres,
  ---PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct50_muareaacres,
  ---PERCENTILE_CONT(0.95) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct95_muareaacres,
  
  
   ---- miscellaneous areas
   SUM(CASE WHEN muareaacres < ", round(le$mla[le$areasymbol == x], 2)," AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_mla_ma, 
  SUM(CASE WHEN muareaacres < 1   AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_10058_1_ma, 
  SUM(CASE WHEN muareaacres < 1.4 AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_12000_1p4_ma, 
  SUM(CASE WHEN muareaacres < 2.5 AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_15840_2p5_ma, 
  SUM(CASE WHEN muareaacres < 4   AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_20000_4_ma, 
  SUM(CASE WHEN muareaacres < 5.7 AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_24000_5p7_ma, 
  SUM(CASE WHEN muareaacres < 40  AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_63360_40_ma, 
  SUM(CASE WHEN muareaacres < 620 AND majcompflag_ma > 0 THEN 1 ELSE 0 END) n_muareaacres_lt_250000_620_ma, 

  
  SUM(CASE WHEN muareaacres < ", round(le$mla[le$areasymbol == x], 2)," AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_mla_ma, 
  SUM(CASE WHEN muareaacres < 1   AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_10058_1_ma, 
  SUM(CASE WHEN muareaacres < 1.4 AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_12000_1p4_ma, 
  SUM(CASE WHEN muareaacres < 2.5 AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_15840_2p5_ma, 
  SUM(CASE WHEN muareaacres < 4   AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_20000_4_ma, 
  SUM(CASE WHEN muareaacres < 5.7 AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_24000_5p7_ma, 
  SUM(CASE WHEN muareaacres < 40  AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_63360_40_ma, 
  SUM(CASE WHEN muareaacres < 620 AND majcompflag_ma > 0 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_250000_620_ma


  ---COUNT(mupolygonkey) OVER(PARTITION BY areasymbol) n_mupolygonkey,
  ---MIN(muareaacres) OVER(PARTITION BY areasymbol) min_muareaacres,
  ---AVG(muareaacres) OVER(PARTITION BY areasymbol) avg_muareaacres,
  ---MAX(muareaacres) OVER(PARTITION BY areasymbol) max_muareaacres,

  ---PERCENTILE_CONT(0.05) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct05_muareaacres,
  ---PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct50_muareaacres,
  ---PERCENTILE_CONT(0.95) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct95_muareaacres
  
  
  FROM mupolygon mupol
  INNER JOIN #MA1 mu ON mu.mukey = mupol.mukey

  WHERE areasymbol = '", x, "'
  
  GROUP BY areasymbol
  ")
mupol_stats <- lapply(sort(le$areasymbol), function(x) {
  cat(as.character(Sys.time()), x, "\n")
  
  tryCatch(
    SDA_query(q(paste0(x))), 
    error = function(e) {
      message(e)
      return(NULL)
    })
})
mupol_stats <- do.call("rbind", mupol_stats)
# write.csv(mupol_stats, "mupol_stats.csv", row.names = FALSE)
mupol_stats <- read.csv("mupol_stats.csv")

mupol_stats[-1] <- sapply(mupol_stats[-1], as.numeric)
idx <- apply(mupol_stats[-1], 1, function(x) all(is.na(x)))
sum(idx)
mupol_stats <- mupol_stats[- which(idx), ]

mupol_stats |> 
  dplyr::summarize(
    lt_n    = sum(n_muareaacres_lt_mla) / sum(n_mupolygonkey),
    lt_ma   = sum(n_muareaacres_lt_mla_ma) / sum(n_mupolygonkey),
    lt_acre = mean(pct_muareaacres_lt_mla) / sum(n_mupolygonkey),
    lt_acre_ma = mean(pct_muareaacres_lt_mla_ma) / sum(n_mupolygonkey)
)

mupol_stats |> with(
  n_muareaacres_lt_mla / n_mupolygonkey
) |>
  summary()
mupol_stats |> with(
    n_muareaacres_lt_mla_ma / n_mupolygonkey
  ) |>
  summary()
mupol_stats |> with(
  summary(pct_muareaacres_lt_mla)
)
mupol_stats |> with(
  summary(pct_muareaacres_lt_mla_ma)
)


# query miscellaneous areas
q <- function(x) {
  paste0(
  "
  CREATE TABLE #MA1 (mukey INT, comppct_r_ma INT, majcompflag_ma INT, musym_w INT)
  
  INSERT INTO #MA1 (mukey, comppct_r_ma, majcompflag_ma, musym_w)
  
  SELECT
  co.mukey,
  SUM(CASE WHEN LOWER(compkind) = 'miscellaneous area' THEN comppct_r ELSE 0 END) comppct_r_ma,
  SUM(CASE WHEN LOWER(compkind) = 'miscellaneous area' AND majcompflag = 'Yes' THEN 1 ELSE 0 END) majcompflag_ma,
  SUM(CASE WHEN musym = 'W' OR musym = 'M-W' THEN 1 ELSE 0 END) musym_w
  
  FROM component co
  INNER JOIN mapunit mu ON mu.mukey = co.mukey
  INNER JOIN legend  le ON le.lkey  = mu.lkey
  
  
  WHERE areasymbol LIKE '", x, "'
  
  GROUP BY co.mukey
  
  SELECT *
  
  FROM #MA1
  ")}

test <- lapply("IN", function(x) {
  cat(x, as.character(Sys.time()), "\n")
  SDA_query(q(paste0(x, "%")))
  })



## ESRI File Geodatabase ----
mupol_stats_gdb <- lapply(sort(sapol$AREASYMBOL), function(x) {
  
  cat("processing", x, as.character(Sys.time()), "\n")
  
  sapol2 <- sapol |>
    subset(AREASYMBOL == x) |> 
    st_cast("MULTILINESTRING")
  
  temp <- read_sf(
    dsn   = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", 
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
# data.table::fwrite(mupol_stats_gdb, "mupol_stats_gdb.csv")
mupol_stats_gdb <- data.table::fread("mupol_stats_gdb.csv")



# transform ----
pat <- "^water$|^water,|^water |^water-riverwash|^water-perennial" #^water-|-water-|-water "
mu2 <- mu |>
  mutate(mu_water = grepl(pat, tolower(mu$muname)))
table(mu2$muname[mu2$majcompflag_w > 0 | grepl(pat, tolower(mu2$muname))]) |> 
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
  mutate(compkind_dom_mukind = paste0(compkind_dom, "-", tolower(mukind)))


mupol_stats_gdb <- mupol_stats_gdb |>
  inner_join(co_ma_mukey, by = c("mukey", "musym"))


test <- mupol_stats_gdb |>
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

test_agg <- aggregate(. ~ areasymbol, data = test[-c("areasymbol")], quantile)



vars <- names(test[grepl("n_|pct_", names(test))])
le2_lo <- reshape(
  test,
  direction = "long",
  timevar = "var", times = vars,
  v.names = "value", varying = vars
  )
le2_lo <- merge(sapol, le2_lo, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)


le2 <- le2_lo <- merge(sapol, mupol_stats, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)
names(le2_lo) <- gsub("_muareaacres_lt", "", names(le2_lo))



# legend complexity and size
co2 <- co |>
  group_by(mukey) |>
  summarize(h = aqp::shannonEntropy(comppct_r/100)) |>
  ungroup()


mu2 <- mu |>
  group_by(areasymbol, invesintens) |>
  summarize(
    muacres = sum(muacres),
    n_mukey = length(mukey),
    avg_comp = mean(n_component),
    avg_mcom = mean(n_majcompflag)
  ) |>
  ungroup() |>
  tidyr::pivot_wider(
    id_cols = areasymbol, 
    names_from = invesintens,
    values_from = c(muacres, n_mukey, avg_comp, avg_mcom)
  )
test <- mu2[grepl("muacres_Order", names(mu2))] |> as.matrix()
test[is.na(test)] <- 0
mu2$sso <- colnames(test)[max.col(test)] |> gsub("muacres_", "", x = _)

le2 <- merge(le, mu2, by = "areasymbol", all.x = TRUE)

test <- merge(le, mu, by = "areasymbol", all.x = TRUE) |>
  merge(co2, by = "mukey", all.x = TRUE)



# model ----
# legend size
lm(n_mukey ~ log(areaacres) + log(projectscale) + sso, data = le2) |> summary()



# map ----
# survey area count < MLA
test2 <- subset(le2_lo, var %in% c("n_mla_ma")) # , "n_12000_1p4", "n_24000_5p7"))
n <- 10
tm_shape(test2) + 
  tm_fill(col = "value", n = n, style = "quantile", palette = viridis::cividis(n), title  = "Count (N)") + 
  # tm_facets("var", nrow = 1) +
  tm_layout(legend.outside = TRUE, main.title = "Number of Non-Soil Polygons Smaller than \n the Minimum Legible Area")


# survey area % < MLA
test2 <- subset(le2_lo, var %in% c("pct_mla_ma"))
tm_shape(test2) + 
  tm_fill(col = "value", breaks = c(0, 0.01, 0.05, 0.1, 0.15, 1), palette = viridis::cividis(4), title = "Percent (%)") + 
  tm_layout(legend.outside = TRUE, main.title = "Percent Non-Soil Acres Less Than the Minimum Legible Area")


brks <- classInt::classIntervals(le2$n_muareaacres_lt_mla, n = 10, style = "quantile")
brks <- quantile(le2$n_muareaacres_lt_mla, probs = seq(0, 1, 0.1), na.rm = TRUE)
le2$log <- cut(le2$n_muareaacres_lt_mla, brks)
ggplot() + 
  geom_sf(data = le2, aes(fill = log), lwd = NA, ) +
  scale_fill_viridis_d()
  

# survey area complexity
filter(le2, !is.na(sso)) |>
  mutate(sso = as.factor(sso)) |>
  ggplot(aes(y = n_mukey / areaacres, x = as.factor(projectscale/1000), fill = sso)) + 
  geom_boxplot() + 
  # facet_grid(~sso) +
  # ylim(0, 5) +
  xlab("projectscale/1000") + 
  scale_y_log10() +
  ggtitle("Legend Size by Map Scale and Survey Order")


# mapunit complexity
tb <- table(le$projectscale)
idx <- names(tb[tb > 100])
test |>
  filter(
    projectscale %in% idx & !is.na(projectscale) & !is.na(decade)) |>
  mutate(
    decade = as.factor(decade),
    invesintens = as.factor(invesintens),
    projectscale = paste0("1:", format(projectscale, big.mark = ","))
  ) |>
  ggplot(aes(y = h, x = invesintens, fill = decade)) + 
  geom_boxplot() +
  ylab("Shannon Entropy") + xlab("Soil Survey Order") +
  facet_wrap(~projectscale) +
  ggtitle("Map unit Entropy by Survey Order, Map Scale and Decade")


# survey area complexity
test <- le |>
  inner_join(mu, by = "areasymbol") |>
  group_by(decade, mukind) |>
  summarize(n = length(mukey)) |>
  ungroup()

ggplot(test, aes(y = n, x = decade, col = mukind)) + 
  geom_line(size = 2) +
  ggtitle("Trend in Map Unit Kind by Decade")


test <- le |>
  inner_join(mu, by = "areasymbol") |>
  group_by(decade, mukind, projectscale) |>
  summarize(n = length(mukey)) |>
  ungroup()

test_N <- test |>
  group_by(decade, projectscale) |>
  summarize(N = sum(n)) |>
  ungroup()

test2 <- test |>
  left_join(test_N, by = c("decade", "projectscale")) |>
  mutate(pct = round(n / N * 100))


test2 |>
  filter(projectscale %in% c(12000, 15840, 24000) & !is.na(mukind)) |>
  ggplot(aes(y = n, x = decade, col = mukind)) + 
  geom_line(size = 2) +
  facet_wrap(~projectscale) +
  ylab("Count (n)") +
  ggtitle("Trend in Map Unit Kind by Decade")


test2 |>
  filter(projectscale %in% c(12000, 15840, 24000) & !is.na(mukind)) |>
  ggplot(aes(y = pct, x = decade, col = mukind)) + 
  geom_line(size = 2) +
  facet_wrap(~projectscale) +
  ylab("percent (%)") +
  ggtitle("Trend in Map Unit Kind by Decade")


