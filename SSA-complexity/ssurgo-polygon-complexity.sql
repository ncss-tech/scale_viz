-- http://www.umass.edu/landeco/research/fragstats/documents/Metrics/Shape%20Metrics/Metrics/P9%20-%20FRAC.htm

--
-- TODO: turn this into a parallel evaluation by areasymbol, save to new table
--

SELECT areasymbol, 
--
-- TODO: add percentiles of areasymbol
-- TODO: add polys / MU
--
PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) )
) AS fd_50,
--
PERCENTILE_CONT(0.95) WITHIN GROUP (ORDER BY 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) )
) AS fd_95,
--
PERCENTILE_CONT(0.99) WITHIN GROUP (ORDER BY 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) )
) AS fd_99 
--
FROM ssurgo.mapunit_poly 
WHERE areasymbol IN ('ks015', 'ca654', 'ca630', 'ca113', 'la057') 
GROUP BY areasymbol;


 areasymbol |    pctile_50     |    pctile_95     |    pctile_99
------------+------------------+------------------+------------------
 ca113      | 1.07073947410019 | 1.17588039731525 | 1.25376008004354
 ca630      | 1.07305256376983 | 1.17277349007584 | 1.22171482565627
 ca654      | 1.05828966182756 | 1.16940973138014 |  1.2363933083075
 ks015      | 1.07820870598141 | 1.18876876056659 | 1.24576752695496
 la057      | 1.08530703522537 | 1.21619921843597 |  1.2941185673511




--
-- get some example data
--


\timing

-- ~ 2 minutes

-- https://stackoverflow.com/questions/5297396/quick-random-row-selection-in-postgres/32023533
set search_path to ssurgo, public;
CREATE TEMP TABLE xxx AS 
SELECT areasymbol, ogc_fid,
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd,
-- log base 10
LOG(ST_Area(wkb_geometry::geography)::numeric) as log_sq_m,
-- test this: looks about right
ST_NumPoints(ST_ExteriorRing((ST_Dump(wkb_geometry)).geom)) as n_pts
FROM ssurgo.mapunit_poly
TABLESAMPLE SYSTEM_ROWS(100000);

-- save
\copy xxx TO 'fractal-dimension-test.csv' CSV HEADER


--
-- test specific polygons
--



SELECT ogc_fid, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-96.92222, 38.04056), 4326))
;


SELECT ogc_fid, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-85.338196, 40.714611), 4326))
;

-- maybe the most complex polygon in all of SSURGO
-- https://casoilresource.lawr.ucdavis.edu/gmap/?loc=29.3349,-90.3324
SELECT ogc_fid, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-90.3485, 29.3319), 4326))
;



-- worst offenders, > 45 minutes
SELECT areasymbol, mukey, muname 
FROM ssurgo.mapunit 
WHERE mukey IN ('1598653', '1598670', '162786', '1413444', '403308');


 areasymbol |  mukey  |                                     muname
------------+---------+--------------------------------------------------------------------------------
 la051      | 1413444 | Lafitte-Clovelly association, 0 to 0.2 percent slopes, very frequently flooded
 la075      | 1598653 | Bellpass muck, 0 to 0.2 percent slopes, very frequently flooded
 la075      | 1598670 | Timbalier muck, 0 to 0.2 percent slopes, tidal
 in053      | 162786  | Blount silt loam, ground moraine, 0 to 2 percent slopes
 ia021      | 403308  | Canisteo clay loam, 0 to 2 percent slopes

 
SELECT areasymbol, mukey, ogc_fid,
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd,
ST_X(ST_PointOnSurface(wkb_geometry)), ST_Y(ST_PointOnSurface(wkb_geometry))
FROM ssurgo.mapunit_poly 
WHERE mukey IN ('1598653', '1598670', '162786', '1413444', '403308') 
ORDER BY fd DESC;
 
 