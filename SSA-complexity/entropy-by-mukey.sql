-- Compute Shanon Entropy 


--
-- Of course we don't need to use area... because we are talking about proportions
--
 
-- Notes:
-- * excluding all misc. areas --> misc. area map units with a single component have very low entropy... not a useful indicator (is this a good idea?)
-- * Dec 2022: attempting to leave in misc. areas, filtering single component MU in R
 
 
 
\timing

-- EXPLAIN


-- ~ 3 minutes

DROP TABLE IF EXISTS h;
CREATE TEMP TABLE h AS 

-- total component pct
WITH component_p AS (
-- areasymbol is not citext in most ssurgo.* tables, just lower-case
SELECT mapunit.areasymbol::citext AS areasymbol, 
mukind, mukey, comppct_r / 100.0 AS comppct
FROM ssurgo.mapunit 
JOIN ssurgo.component USING (mukey)
WHERE comppct_r IS NOT NULL
AND mukind != ''
-- AND compkind != 'Miscellaneous area'
AND compkind != ''
),
-- total by mukey
total_p AS (
SELECT mukey, SUM(comppct) AS sum_comppct
FROM component_p
GROUP BY mukey
),
-- proportions
proportions AS (
SELECT 
component_p.areasymbol, component_p.mukind, component_p.mukey, comppct / sum_comppct AS p
FROM component_p JOIN total_p USING (mukey)
)
-- entropy
-- also
SELECT
areasymbol, coryear, projectscale, mukind, mukey, 
-- note: log(0) --> error
-- either add fuzz or remove
ROUND(- SUM(p * LOG(2.0, p::numeric)), 6) AS entropy,
-- number of components
COUNT(p) AS n
FROM proportions
JOIN soilweb.ssurgo_status USING (areasymbol)
-- test this: retain only real proportions
WHERE p > 0
GROUP BY areasymbol, coryear, projectscale, mukind, mukey ;



--
-- statsgo
--
DROP TABLE IF EXISTS hs;
CREATE TEMP TABLE hs AS 


-- total component pct
WITH component_p AS (
SELECT mukind, mukey, comppct_r / 100.0 AS comppct
FROM statsgo.mapunit 
JOIN statsgo.component USING (mukey)
WHERE comppct_r IS NOT NULL
AND mukind != ''
-- AND compkind != 'Miscellaneous area'
AND compkind != ''
),
-- total by mukey
total_p AS (
SELECT mukey, SUM(comppct) AS sum_comppct
FROM component_p
GROUP BY mukey
),
-- proportions
proportions AS (
SELECT 
component_p.mukind, component_p.mukey, comppct / sum_comppct AS p
FROM component_p JOIN total_p USING (mukey)
)
-- entropy
-- also
SELECT
-- double-check / think about scale
'US' AS areasymbol, 250000::integer AS projectscale, mukind, mukey, 
-- note: log(0) --> error
-- either add fuzz or remove
ROUND(- SUM(p * LOG(2.0, p::numeric)), 6) AS entropy,
-- number of components
COUNT(p) AS n
FROM proportions
-- test this: retain only real proportions
WHERE p > 0
GROUP BY areasymbol, projectscale, mukind, mukey ;


\copy h TO 'entropy-by-mukey.csv' CSV HEADER
\copy hs TO 'entropy-by-mukey-statsgo.csv' CSV HEADER

--
-- compress
--

-- gzip -f entropy-by-mukey.csv
-- gzip -f entropy-by-mukey-statsgo.csv

--
-- checking
--


-- aqp::shannonEntropy(base = 2) --> 2.788091
-- computed above: ----------------> 2.788091
--
-- select mukey, cokey, ROUND(comppct_r / 100.0, 3) FROM ssurgo.component where mukey = '2766854';

-- s <- c(0.35, 0.1, 0.03, 0.07, 0.15, 0.15, 0.04, 0.05, 0.05, 0.01)
-- sum(s)
-- shannonEntropy(s)

--
-- entropy of 0:
--

-- single component, 85%
-- SELECT mukey, cokey, ROUND(comppct_r / 100.0, 3) FROM ssurgo.component where mukey = '1425970';

-- 100% misc. area
-- SELECT mukey, compkind, cokey, ROUND(comppct_r / 100.0, 3) FROM ssurgo.component where mukey = '618801';

-- single component, 100%
-- SELECT mukey, compkind, cokey, ROUND(comppct_r / 100.0, 3) FROM ssurgo.component where mukey = '54021';


