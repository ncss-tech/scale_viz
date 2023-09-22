-- http://www.umass.edu/landeco/research/fragstats/documents/Metrics/Shape%20Metrics/Metrics/P9%20-%20FRAC.htm

--
-- TODO: turn this into a parallel evaluation by areasymbol, save to new table
--


SET search_path TO ssurgo, public;


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


 areasymbol |      fd_50       |      fd_95       |      fd_99
------------+------------------+------------------+------------------
 ca113      | 1.0705661303797  | 1.17595973778445 | 1.25385612288644
 ca630      | 1.07301681702903 | 1.17274607469945 | 1.22212023720076
 ca654      | 1.05826950679277 | 1.16930655330627 | 1.23639418371611
 ks015      | 1.07824576390685 | 1.18887782269612 | 1.24578674160347
 la057      | 1.08531015742667 |  1.2161166037306 | 1.29425076184811





--
-- get some example data
--


\timing
set search_path to ssurgo, public;

-- ~ 2 minutes

DROP TABLE IF EXISTS xxx;

CREATE TEMP TABLE xxx AS 
SELECT mapunit_poly.areasymbol, ogc_fid, mapunit_poly.mukey, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd,
-- log base 10
LOG(ST_Area(wkb_geometry::geography)::numeric) as log_sq_m,
-- test this: looks about right
ST_NumPoints(ST_ExteriorRing((ST_Dump(wkb_geometry)).geom)) as n_pts,
CASE WHEN invesintens IS NULL THEN 'missing' ELSE invesintens END AS invesintens, 
coryear, projectscale
FROM 
-- random sampling from a table
-- https://stackoverflow.com/questions/5297396/quick-random-row-selection-in-postgres/32023533
ssurgo.mapunit_poly TABLESAMPLE SYSTEM_ROWS(100000)
JOIN ssurgo.mapunit USING (mukey)
-- must make case insensitive
JOIN soilweb.ssurgo_status ON mapunit.areasymbol::citext = ssurgo_status.areasymbol ;

-- save
\copy xxx TO 'fractal-dimension-test.csv' CSV HEADER


-- gzip -f fractal-dimension-test.csv



--
-- test specific polygons
--


--
-- very high FD
-- POINT(-88.279394644734 30.6060197088614)
-- https://casoilresource.lawr.ucdavis.edu/gmap/?loc=30.6060197088614,-88.279394644734
--
-- likely a sliver
--
SELECT ST_asText(ST_PointOnSurface(wkb_geometry)) 
FROM ssurgo.mapunit_poly
WHERE ogc_fid = 828620 ;





--
-- https://casoilresource.lawr.ucdavis.edu/gmap/?loc=38.04056,-96.92222
-- FD: 1.30
--
SELECT ogc_fid, mukey, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-96.92222, 38.04056), 4326))
;

--
-- https://casoilresource.lawr.ucdavis.edu/gmap/?loc=40.714611,-85.338196
-- FD: 1.48
-- 
SELECT ogc_fid, mukey, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-85.338196, 40.714611), 4326))
;

-- maybe the most complex polygon in all of SSURGO
-- https://casoilresource.lawr.ucdavis.edu/gmap/?loc=29.33516,-90.33485,z16
-- FD: 1.40
-- 
SELECT ogc_fid, mukey, 
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd
FROM ssurgo.mapunit_poly
WHERE ST_Intersects(wkb_geometry, ST_SetSRID(ST_MakePoint(-90.33485, 29.33516), 4326))
;


--
-- worst offenders, > 45 minutes
--
SELECT areasymbol, mukey, muname 
FROM ssurgo.mapunit 
WHERE mukey IN ('1598653', '1598670', '162786', '1413444', '403308', '2605765', '161428', '808535');


 areasymbol |  mukey  |                                     muname
------------+---------+--------------------------------------------------------------------------------
 la051      | 1413444 | Lafitte-Clovelly association, 0 to 0.2 percent slopes, very frequently flooded
 la075      | 1598653 | Bellpass muck, 0 to 0.2 percent slopes, very frequently flooded
 la075      | 1598670 | Timbalier muck, 0 to 0.2 percent slopes, tidal
 in069      | 161428  | Pewamo silty clay loam, 0 to 1 percent slopes
 in053      | 162786  | Blount silt loam, ground moraine, 0 to 2 percent slopes
 ks015      | 2605765 | Irwin silty clay loam, 1 to 3 percent slopes
 ia021      | 403308  | Canisteo clay loam, 0 to 2 percent slopes
 la057      | 808535  | Water


 
SELECT areasymbol, mukey, ogc_fid,
( 2.0 * LN(0.25 * ST_Perimeter(wkb_geometry::geography)) ) / ( LN(ST_Area(wkb_geometry::geography)) ) AS fd,
ST_X(ST_PointOnSurface(wkb_geometry)), ST_Y(ST_PointOnSurface(wkb_geometry))
FROM ssurgo.mapunit_poly 
WHERE mukey IN ('1598653', '1598670', '162786', '1413444', '403308', '2605765', '161428', '808535') 
ORDER BY fd DESC;
 
 areasymbol |  mukey  | ogc_fid  |        fd         |       st_x        |       st_y
------------+---------+----------+-------------------+-------------------+------------------
 in069      | 161428  |  9937163 |  1.48328711710149 |  -85.416953776054 | 40.7499389755057
 in069      | 161428  |  9948437 |  1.46487512704564 |  -85.482530692046 | 40.7068578216985
 la075      | 1598653 | 12399549 |  1.46163071003991 | -89.9114833563087 | 29.4975338125364
 la075      | 1598653 | 12390850 |  1.45988530180611 | -89.4456791098703 | 29.1909038539091
 in069      | 161428  |  9947035 |  1.45958506507375 | -85.4776080088492 | 40.9512008459263
 la057      | 808535  | 12358142 |  1.44351990633671 | -90.9052395795618 | 29.7535805073645
 la075      | 1598653 | 12399094 |   1.4377956041352 | -89.4571619816923 | 29.1988396856297
 ia021      | 403308  |  4957979 |  1.43676407697544 | -95.0402802568294 | 42.7394000845724
 la057      | 808535  | 12357522 |  1.43548782699027 | -90.5702108833809 | 29.7445771122274
 in069      | 161428  |  9937364 |  1.42887746607053 |  -85.600613115432 | 40.9457825632656
 in069      | 161428  |  9936537 |  1.42302224162814 |  -85.640465571175 | 40.9778259842066
 la057      | 808535  | 12358490 |  1.42189171043039 | -90.5196409691703 | 29.6089347799457
 la057      | 808535  | 12358986 |  1.41932662856896 | -90.2435096155968 | 29.4243124877804
 la057      | 808535  | 12356857 |  1.40927152230042 | -90.5504866246769 | 29.7146190633603
 la075      | 1598653 | 12403712 |  1.40641834934647 |  -89.617586937944 | 29.3005355683554
 la057      | 808535  | 12357703 |  1.40373102348149 | -90.1078195510741 | 29.4591174179254
 la057      | 808535  | 12357687 |  1.40208424133049 | -90.5174479199878 | 29.7147878087805
 in069      | 161428  |  9938261 |  1.40206214445821 | -85.4262303400103 |   40.86742435906
 la057      | 808535  | 12355116 |  1.40001693367425 |  -90.596932159237 | 29.6824435958016
 la057      | 808535  | 12355102 |  1.39822321741683 | -90.6061649657523 | 29.6832350107081
 in069      | 161428  |  9943436 |  1.38400867153309 | -85.5537482314007 | 40.9908107561344
 ia021      | 403308  |  4943733 |  1.38287854140817 | -94.9996699969628 | 42.5963887381446
 ia021      | 403308  |  4985258 |  1.37926819033575 | -94.9739158816321 | 42.8676040813908
 la075      | 1598653 | 12392929 |  1.37800928892213 | -89.4243344856467 | 29.1409666717037
 la057      | 808535  | 12355927 |  1.37508132735342 | -90.5797425276571 | 29.6781094011394
 la075      | 1598653 | 12387689 |  1.37485349297922 | -89.7071767004589 | 29.3814761303123
 la057      | 808535  | 12354316 |   1.3719092152781 | -90.2592148749498 | 29.4431794637381
 la057      | 808535  | 12357894 |  1.37098613917991 | -90.2668347856119 | 29.4362974920748
 in069      | 161428  |  9937291 |  1.37040383504818 |    -85.5582776533 | 40.9062174894476
 la057      | 808535  | 12356230 |  1.36856604765946 | -90.4644098685822 | 29.5718737411316
 la057      | 808535  | 12357588 |  1.36782189835987 | -90.4221721934321 | 29.6679816241866
 in069      | 161428  |  9939200 |  1.36189563063162 | -85.6209811744206 | 40.9430019530877
 la057      | 808535  | 12356928 |  1.35751970291545 | -90.9678002096629 |   29.74024313623
 in069      | 161428  |  9937749 |  1.35289496620383 | -85.4912117783648 | 40.8353508566145
 la051      | 1413444 | 12345797 |  1.35162010647952 | -90.1522996215705 | 29.5672207450041
 in069      | 161428  |  9936883 |  1.35161809330301 | -85.5074115464124 | 40.9200371999367
 la075      | 1598653 | 12390446 |   1.3505508934247 | -89.7078367872153 | 29.6657777806247
 la075      | 1598653 | 12398872 |  1.34853986575574 | -89.7154567321214 | 29.4382825042961
 in069      | 161428  |  9941627 |  1.34839320185791 | -85.5339595244721 | 40.9909001099014
 la057      | 808535  | 12355733 |  1.34824929325214 | -90.2894599901661 | 29.5026745289379
 in069      | 161428  |  9936150 |  1.34499824236032 | -85.4780773012702 | 40.7338031345811
 in069      | 161428  |  9943394 |  1.34353857556078 | -85.3404893960683 | 40.9265815429331
 la057      | 808535  | 12355800 |  1.34166771415206 | -90.5248686017507 | 29.6051208867924
 la057      | 808535  | 12355769 |  1.34098031496372 | -90.5250723997578 | 29.6030465161942
 la051      | 1413444 | 12347226 |  1.33670939588206 | -90.0891806287276 | 29.6199418472623
 in069      | 161428  |  9940751 |  1.33625880863942 | -85.6292023795645 | 40.9251603424255
 la057      | 808535  | 12354390 |  1.33525463077489 | -90.2458085183358 |  29.440286474651
 in069      | 161428  |  9941045 |  1.33513895255764 | -85.5632861563958 | 40.7622842268404
 la057      | 808535  | 12357358 |  1.33198015759024 | -90.3913333927864 | 29.2729575932009
 in069      | 161428  |  9948386 |  1.32782308432321 | -85.3974280588568 | 40.9550885673713
 in069      | 161428  |  9949161 |  1.32681205247172 | -85.5041911715808 | 40.9959744619548
 in069      | 161428  |  9936436 |  1.32545800703881 | -85.5952824686905 | 40.9657594126796
 la075      | 1598653 | 12393849 |  1.32544565808693 |  -89.428202092228 | 29.1784225984066
 la051      | 1413444 | 12345616 |  1.32528689418158 | -90.0740614246168 | 29.6302457072079
 la051      | 1413444 | 12346629 |  1.32365666664288 | -90.0716427491015 | 29.6317959292541
 la057      | 808535  | 12355033 |  1.32307698833184 | -90.2770761844937 | 29.3958307743572
 in069      | 161428  |  9945162 |  1.32124139549428 | -85.3423539349313 | 40.8027595327971
 la075      | 1598653 | 12391419 |  1.32120992638606 | -89.7769974162972 | 29.3953811791682
 la057      | 808535  | 12359228 |  1.32091234710234 | -90.3544576803306 |  29.517539450842
 in069      | 161428  |  9936434 |  1.31764650095071 | -85.3842815969881 | 40.6622192743014
 la057      | 808535  | 12359042 |  1.31492531120493 | -90.6006474721135 | 29.5821172483422
 la075      | 1598670 | 12393247 |  1.31489374320777 | -89.8749546718819 | 29.4843328629019
 in069      | 161428  |  9940308 |  1.31059248383422 | -85.6392222138062 | 40.7355988343616
 ia021      | 403308  |  4953144 |  1.30971667476265 | -95.0304332899111 | 42.7157609514302
 in069      | 161428  |  9939038 |  1.30938555104118 | -85.5836039835344 | 40.9768121650041
 ia021      | 403308  |  4985059 |  1.30890534097908 | -95.0109166662954 | 42.8946955296661
 la051      | 1413444 | 12346991 |  1.30886413168357 | -90.0118768800692 | 29.6381949576827
 la075      | 1598653 | 12391751 |  1.30658553466144 | -89.7032856849138 | 29.3826119763685
 
 
-- ... 