library(soilDB)
library(terra)

# total number of map units
SDA_query("SELECT COUNT(mukey) as n FROM mapunit")

# exclude STATSGO
.sql <- "
SELECT CASE WHEN invesintens IS NULL THEN 'missing' ELSE invesintens END, 
CAST(
  COUNT(
    CASE WHEN invesintens IS NULL THEN 'missing' ELSE invesintens END
  ) 
  AS numeric
) / (SELECT COUNT(mukey) FROM mapunit) AS prop
FROM mapunit
INNER JOIN legend ON mapunit.lkey = legend.lkey
WHERE areasymbol != 'US'
GROUP BY invesintens 
ORDER BY prop DESC ;"

z <- SDA_query(.sql)


knitr::kable(z, digits = 3)

###

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=36.51233,-118.59492,z13

# make a bounding box and assign a CRS (4326: GCS, WGS84)
a <- vect('POLYGON((-118.7220 36.4500,-118.7220 36.5746,-118.4678 36.5746,-118.4678 36.4500,-118.7220 36.4500))', crs = 'epsg:4326')

# fetch gSSURGO map unit keys at native resolution (30m)
mu <- mukey.wcs(aoi = a, db = 'gssurgo')

# extract RAT for thematic mapping
rat <- cats(mu)[[1]]

.sql <- sprintf(
  "SELECT mukey, invesintens FROM mapunit WHERE mukey IN %s ;", 
  format_SQL_in_statement(as.integer(rat$mukey))
)

tab <- SDA_query(.sql)

rat <- merge(rat, tab, by = 'mukey', all.x = TRUE, sort = FALSE)

table(rat$invesintens)

# re-pack rat
levels(mu) <- rat

activeCat(mu) <- 'invesintens'
plot(mu, axes = FALSE, col = hcl.colors(n = 4, palette = 'spectral'))


