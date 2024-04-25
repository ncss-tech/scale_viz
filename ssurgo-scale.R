
library(soilDB)
library(sf)
library(rmapshaper)


# load data ----
## SAPOLYGON ----
sapol <- read_sf("D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", layer = "SAPOLYGON")


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


## mapunit ----
mu <- lapply(st, function(x) {
  cat("getting ", x, "\n")
  get_mapunit_from_SDA(WHERE = paste0("areasymbol LIKE '", x, "%'"))
})
mu <- do.call("rbind", mu)

mu <- get_mapunit_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", stats = TRUE)


# components ----
co <- get_component_from_GDB(dsn = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb")



# query data ----
## SDA ----
mupol_stats <- lapply(le$areasymbol, function(x) {
  cat(as.character(Sys.time()), x, "\n")
  
  SDA_query(paste0(
  "
  SELECT areasymbol, 
  
  MIN(muareaacres) min_muareaacres,
  AVG(muareaacres) avg_muareaacres,
  MAX(muareaacres) max_muareaacres,
  COUNT(mupolygonkey) n_mupolygonkey,

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
  SUM(CASE WHEN muareaacres < 620 THEN muareaacres ELSE 0 END) /  SUM(muareaacres) pct_muareaacres_lt_250000_620

  ---COUNT(mupolygonkey) OVER(PARTITION BY areasymbol) n_mupolygonkey,
  ---MIN(muareaacres) OVER(PARTITION BY areasymbol) min_muareaacres,
  ---AVG(muareaacres) OVER(PARTITION BY areasymbol) avg_muareaacres,
  ---MAX(muareaacres) OVER(PARTITION BY areasymbol) max_muareaacres,

  ---PERCENTILE_CONT(0.05) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct05_muareaacres,
  ---PERCENTILE_CONT(0.5) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct50_muareaacres,
  ---PERCENTILE_CONT(0.95) WITHIN GROUP(ORDER BY muareaacres) OVER(PARTITION BY areasymbol) pct95_muareaacres

  FROM mupolygon
  WHERE areasymbol = '", x, "'
  GROUP BY areasymbol
  "))
})
mupol_stats <- do.call("rbind", mupol_stats)
# write.csv(mupol_stats, "mupol_stats.csv", row.names = FALSE)
mupol_stats <- read.csv("mupol_stats.csv")


mupol_stats |> with(
  sum(n_muareaacres_lt_mla) / sum(n_mupolygonkey)
)
mupol_stats |> with(
  n_muareaacres_lt_mla / n_mupolygonkey
) |>
  summary()
mupol_stats |> with(
  summary(pct_muareaacres_lt_mla)
)



## ESRI File Geodatabase ----
mupol_stats_gdb <- lapply(sort(sapol$AREASYMBOL), function(x) {
  
  cat("processing", x, as.character(Sys.time()), "\n")
  
  sapol2 <- subset(sapol, AREASYMBOL == x) |> st_cast("MULTILINESTRING")
  le2    <- subset(le,    areasymbol == x)
  
  temp <- read_sf(
    dsn   = "D:/geodata/soils/gSSURGO_CONUS_Oct2023/gSSURGO_CONUS.gdb", 
    query = paste0("SELECT * FROM MUPOLYGON WHERE AREASYMBOL = '", x, "'")
  )
  nm  <- names(temp)
  idx <- length(nm)
  names(temp)[-idx] <- tolower(nm[-idx])
  temp$acres <- st_area(temp) |> units::set_units(acres) |> as.numeric()
  
  idx <- st_intersects(temp, sapol2) |> sapply(function(x) length(x) > 0)
  idx <- idx & temp$acres < round(le2$mla[1], 2)
  
  temp[!idx, ] |>
    dplyr::group_by(areasymbol) |>
    dplyr::summarize(
      min_muareaacres = min(acres),
      avg_muareaacres = mean(acres),
      max_muareaacres = max(acres),
      n_mupolygonkey  = length(musym),
      
      n_muaa_mla      = sum(acres < round(le2$mla[1], 2)),
      n_muaa_10_1     = sum(acres < 1),
      n_muaa_12_1p4   = sum(acres < 1.4),
      n_muaa_15_2p5   = sum(acres < 2.5),
      n_muaa_20_4     = sum(acres < 4),
      n_muaa_24_5p7   = sum(acres < 5.7),
      n_muaa_63_40    = sum(acres < 40),
      n_muaa_250_620  = sum(acres < 620),
      
      pct_muaa_mla     = sum(acres[acres < round(le2$mla[1])], 2) / 
        sum(acres),
      pct_muaa_10_1    = sum(acres[acres < 1])   / sum(acres),
      pct_muaa_12_1p4  = sum(acres[acres < 1.4]) / sum(acres),
      pct_muaa_15_2p5  = sum(acres[acres < 2.5]) / sum(acres),
      pct_muaa_20_4    = sum(acres[acres < 4])   / sum(acres),
      pct_muaa_24_5p7  = sum(acres[acres < 5.7]) / sum(acres),
      pct_muaa_63_40   = sum(acres[acres < 40])  / sum(acres),
      pct_muaa_250_620 = sum(acres[acres < 620]) / sum(acres),
    ) |>
    st_drop_geometry() |>
    dplyr::ungroup() |>
    as.data.frame()
})

mupol_stats_gdb <- do.call("rbind", mupol_stats_gdb)
# write.csv(mupol_stats_gdb, "mupol_stats_gdb.csv", row.names = FALSE)
mupol_stats_gdb <- read.csv("mupol_stats_gdb.csv")



# transform ----
names(mupol_stats) <- gsub("_muareaacres_lt", "", names(mupol_stats))
vars <- names(mupol_stats[grepl("n_|pct_", names(mupol_stats))])
le2_lo <- reshape(
  mupol_stats,
  direction = "long",
  timevar = "var", times = vars,
  v.names = "value", varying = vars
  )
le2_lo <- merge(sapol, le2_lo, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)


le2 <- le2_lo <- merge(sapol, mupol_stats, by.x = "AREASYMBOL", by.y = "areasymbol", all.x = TRUE)
names(le2_lo) <- gsub("_muareaacres_lt", "", names(le2_lo))


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
test <- subset(le2_lo, var %in% c("n_mla")) # , "n_12000_1p4", "n_24000_5p7"))
n <- 10
tm_shape(test) + 
  tm_fill(col = "value", n = n, style = "quantile", palette = viridis::cividis(n), title  = "Count (N)") + 
  # tm_facets("var", nrow = 1) +
  tm_layout(legend.outside = TRUE, main.title = "Number of Polygons Smaller than the Minimum Legible Area")


# survey area % < MLA
test <- subset(le2_lo, var %in% c("pct_mla"))
tm_shape(test) + 
  tm_fill(col = "value", breaks = c(0, 0.01, 0.05, 0.1, 0.15, 1), palette = viridis::cividis(4), title = "Percent (%)") + 
  tm_layout(legend.outside = TRUE, main.title = "Percent Acres Less Than the Minimum Legible Area")


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

