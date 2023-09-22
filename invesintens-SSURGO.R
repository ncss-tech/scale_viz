library(soilDB)


# total number of map units
SDA_query("SELECT COUNT(mukey) as n FROM mapunit")


.sql <- "
SELECT CASE WHEN invesintens IS NULL THEN 'missing' ELSE invesintens END, 
CAST(
  COUNT(
    CASE WHEN invesintens IS NULL THEN 'missing' ELSE invesintens END
  ) 
  AS numeric
) / (SELECT COUNT(mukey) FROM mapunit) AS prop
FROM mapunit
GROUP BY invesintens 
ORDER BY prop DESC ;"

z <- SDA_query(.sql)


knitr::kable(z, digits = 3)
