High Seas Corporate Ownership Analysis
================

``` r
library(tidyverse)
library(naniar)
library(bigrquery)
library(DBI)
library(sf)
library(lubridate)
library(here)

knitr::opts_chunk$set(comment=NA, message = F)


DIR_GCS_data <- "~/gcs/spatial-datasets"
```

# Set BQ connection

``` r
BQ_connection <-  dbConnect(bigquery(), 
                            project = 'world-fishing-827',
                            billing = "world-fishing-827", 
                            use_legacy_sql = FALSE) 
```

# Get 2018 data from GFW

## Fishing vessels: activity and vessels characteristics

The following query obtains the identify, characteristics, and fishing
effort for those vessels that spent at least 5% of their fishing acitity
in areas beyond national jurisdiction in 2018

``` sql
WITH
  --------------------------------------------------------
  -- Use a slightly more agressive filter for good segments
  --------------------------------------------------------
  good_segments AS (
  SELECT
    seg_id
  FROM
    `world-fishing-827.gfw_research.pipe_production_v20190502_segs` 
  WHERE
    good_seg
    AND positions > 10
    AND NOT overlapping_and_short ),
  --------------------------------------------------------
  -- Likely gear
  -------------------------------------------------------
  likely_gear AS (
  SELECT
    ssvid
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_v20190430`
  WHERE 
   REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"(.*)([\\s]+[0-9]+%)$")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"[0-9].[0-9]V")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"[0-9]*\\ [0-9]V")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"(.*)[@]+([0-9]+V[0-9]?)$")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"BOUY")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"BUOY")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NET MARK")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NETMARK")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"^[0-9]*-[0-9]*$")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NET FISHING")
    OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NETFISHING")),
  ---------------------------------------------------
  -- nast_ssvid are ssvid that are likely offsetting because they have
  -- lots of positions more than 3000 km from the satellite
  ------------------------------------------------------
  nast_ssvid AS (
  SELECT
    ssvid,
    SUM( positions) positions
  FROM
    `world-fishing-827.gfw_research.pipe_production_v20190502_segs`
  WHERE
    ((dist_avg_pos_sat_vessel_km > 3000 AND sat_positions_known > 5) )
    AND first_timestamp < TIMESTAMP("2018-12-31")
    AND last_timestamp > TIMESTAMP("2018-01-01")
  GROUP BY
    ssvid
  HAVING
    positions > 50 ),
  --------------------------------------------------------
  -- good_ssvid here are ones that are definitely not spoofing
  -- and which have a at least 10 days of activity
  -------------------------------------------------------
  good_ssvid AS (
  SELECT
    ssvid,
    ais_identity.n_shipname_mostcommon.value AS shipname_norm,
    registry_info.best_known_imo imo,
    registry_info.best_known_callsign callsign,
    inferred.inferred_vessel_class vessel_class,
    best.best_flag flag,
    best.best_length_m length_m,
    best.best_tonnage_gt tonnage_gt,
    best.best_engine_power_kw engine_power
  FROM
    `gfw_research.vi_ssvid_byyear_v20190430`
  WHERE
    year = 2018
    AND on_fishing_list_best
    AND activity.overlap_hours_multinames = 0
    AND activity.overlap_hours < 24*3
    AND activity.active_hours > 24*10
    AND activity.offsetting IS FALSE
    AND ssvid NOT IN (SELECT ssvid FROM nast_ssvid)
    AND ssvid NOT IN (SELECT ssvid FROM likely_gear)
    AND CAST(ssvid AS int64) not in (SELECT ssvid FROM `gfw_research.bad_mmsi` CROSS JOIN UNNEST(ssvid) AS ssvid)),
  -----------------
  -- raw position data
  ----------------
  position_table AS (
  SELECT
    distinct ssvid, hours, timestamp, nnet_score,
    if(array_length(regions.eez)>0,true,false) in_eez
  FROM
    `gfw_research.pipe_production_v20190502_fishing`
  WHERE
    timestamp BETWEEN TIMESTAMP("2018-01-01")
    AND TIMESTAMP("2018-12-31")
    AND seg_id IN (SELECT seg_id FROM good_segments))
  --------------------------------------------------
  -- Group effort by ssvid
  -------------------------------------------------
SELECT
  ssvid,
  shipname_norm,
  imo,
  callsign,
  vessel_class,
  flag,
  length_m,
  tonnage_gt,
  engine_power,
  SUM(hours) hours,
  SUM(IF(nnet_score > .5, hours, 0)) fishing_hours,
  SUM(if(in_eez,0,hours)) hours_HS,
  SUM(IF( nnet_score > .5 and not in_eez, hours, 0)) fishing_hours_HS
FROM
  position_table
JOIN
  good_ssvid
USING
  (ssvid)
GROUP BY
  ssvid,
  shipname_norm,
  imo,
  callsign,
  vessel_class,
  flag,
  length_m,
  tonnage_gt,
  engine_power
having fishing_hours_HS > 0 and fishing_hours > 0 and fishing_hours_HS/fishing_hours >= 0.05
```

## Reefers and Bunkers

``` sql
WITH
  --------------------
  -- Get encounters
  --------------------
  encounters_table AS (
  SELECT
    event_id,
    vessel_id,
    event_start,
    event_end,
    lat_mean,
    lon_mean,
    JSON_EXTRACT(event_info, "$.median_distance_km") AS median_distance_km,
    JSON_EXTRACT(event_info, "$.median_speed_knots") AS median_speed_knots
  FROM
    `world-fishing-827.pipe_production_v20190502.published_events_encounters`
  WHERE
    event_start >= TIMESTAMP("2018-01-01")
    AND event_start <= TIMESTAMP("2018-12-31")),
  --------------------
  -- Add ssvid to encounters
  --------------------
  encounters_ssvid AS (
  SELECT
    (SPLIT(event_id,"."))[SAFE_ORDINAL(1)] AS event_id,
    ssvid,
    a.vessel_id AS vessel_id,
    event_start,
    event_end,
    lat_mean,
    lon_mean,
    median_distance_km,
    median_speed_knots
  FROM (
    SELECT
      *
    FROM
      encounters_table) a
  LEFT JOIN (
    SELECT
      ssvid,
      vessel_id,
      first_timestamp,
      last_timestamp
    FROM
      `pipe_production_v20190502.vessel_info`) b
  ON
    a.vessel_id = b.vessel_id
  WHERE
    a.event_start BETWEEN b.first_timestamp
    AND b.last_timestamp
    AND a.event_end BETWEEN b.first_timestamp
    AND b.last_timestamp ),
  --------------------
  --  Identify duplicate encounter event IDS
  --------------------
  duplicated_encounter_events AS (
  SELECT
    event_id
  FROM (
    SELECT
      event_id,
      COUNT(*) AS counts
    FROM (
      SELECT
        (SPLIT(event_id,"."))[SAFE_ORDINAL(1)] AS event_id,
        ssvid,
        event_start,
        event_end,
        lat_mean,
        lon_mean,
        median_distance_km,
        median_speed_knots
      FROM
        encounters_ssvid)
    GROUP BY
      1)
  WHERE
    counts > 2),
  --------------------
  --  Remove duplicate encounter event IDS
  --------------------
  encounter_duplicates_removed AS (
  SELECT
    *
  FROM
    encounters_ssvid
  WHERE
    event_id NOT IN (
    SELECT
      event_id
    FROM
      duplicated_encounter_events)),
  --------------------
  --  Get carrier vessels
  --------------------
  carrier_vessels AS (
  SELECT
    *
  FROM (
    SELECT
      CAST(identity.ssvid AS STRING) AS mmsi,
      first_timestamp,
      last_timestamp
    FROM
      `world-fishing-827.vessel_database.all_vessels_v20190801`,
      UNNEST(registry),
      UNNEST(activity)
    WHERE
      (is_carrier AND confidence = 3)
      OR identity.ssvid in (select cast(mmsi as STRING) mmsi from `world-fishing-827.gfw_research.bunker_vessels_v20190827`))),
  --------------------
  --  Get carrier encounters
  --------------------
  carrier_encounters AS(
  SELECT
    event_id,
    ssvid AS carrier_ssvid,
    event_start,
    EXTRACT(YEAR
    FROM
      event_start) AS event_year,
    event_end,
    lat_mean,
    lon_mean,
    median_distance_km,
    median_speed_knots
  FROM (
    SELECT
      *
    FROM
      encounter_duplicates_removed) a
  JOIN (
    SELECT
      mmsi,
      first_timestamp,
      last_timestamp
    FROM
      carrier_vessels) b
  ON
    a.ssvid = b.mmsi
  WHERE
    event_start BETWEEN first_timestamp
    AND last_timestamp
    AND event_end BETWEEN first_timestamp
    AND last_timestamp ),
  --------------------
  --  join to form full encounters
  --------------------
  encounters_joined AS (
  SELECT
    a.event_id AS event_id,
    carrier_ssvid AS carrier_ssvid,
    ssvid AS vessel2_ssvid,
    event_start,
  IF
    (event_year = 2019,
      2018,
      event_year) AS event_year,
    event_end,
    lat_mean,
    lon_mean,
    median_distance_km,
    median_speed_knots
  FROM (
    SELECT
      *
    FROM
      carrier_encounters ) a
  JOIN (
    SELECT
      ssvid,
      event_id
    FROM
      encounters_ssvid) b
  ON
    a.event_id = b.event_id
  WHERE
    a.carrier_ssvid != b.ssvid),
  --------------------
  --  Filter vessel2_ssvid to fishing vessels
  --------------------
  encounters_with_neighbor AS (
  SELECT
    event_id,
    carrier_ssvid,
    vessel2_ssvid AS neighbor_vessel_ssvid,
    event_start,
    event_end,
    lat_mean,
    lon_mean,
    median_distance_km,
    median_speed_knots
  FROM (
    SELECT
      *
    FROM
      encounters_joined) a
  INNER JOIN (
    SELECT
      ssvid,
      year
    FROM
      `gfw_research.vi_ssvid_byyear_v20190430`
    WHERE
      on_fishing_list_best
      AND year = 2018
      AND NOT ( REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"(.*)([\\s]+[0-9]+%)$")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"[0-9].[0-9]V")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"[0-9]*\\ [0-9]V")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"(.*)[@]+([0-9]+V[0-9]?)$")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"BOUY")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"BUOY")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NET MARK")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NETMARK")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"^[0-9]*-[0-9]*$")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NET FISHING")
        OR REGEXP_CONTAINS(ais_identity.shipname_mostcommon.value, r"NETFISHING"))) b
  ON
    a.vessel2_ssvid = b.ssvid
    AND a.event_year = b.year
  GROUP BY
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9),
  --------------------
  --  Get anchorages
  --------------------
  anchorages AS (
  SELECT
    *,
    ST_GEOGPOINT(lon,
      lat) AS anchorage_point
  FROM
    `world-fishing-827.gfw_research.named_anchorages` ),
  --------------------
  --  Get encounters in port
  --------------------
  encounters_near_anchorage AS (
  SELECT
    event_id,
    carrier_ssvid,
    neighbor_vessel_ssvid,
    event_start,
    event_end,
    lat_mean,
    lon_mean,
    median_distance_km,
    median_speed_knots
  FROM (
    SELECT
      *,
      ST_GEOGPOINT(lon_mean,
        lat_mean) AS encounter_point
    FROM
      encounters_with_neighbor ) a
  CROSS JOIN (
    SELECT
      *
    FROM
      anchorages) b
  WHERE
    ST_DISTANCE(encounter_point,
      anchorage_point) < 10000
  GROUP BY
    1,
    2,
    3,
    4,
    5,
    6,
    7,
    8,
    9),
  --------------------
  --  Remove encounters in port
  --------------------
  encounters_not_in_port AS (
  SELECT
    *,
    EXTRACT(YEAR
    FROM
      event_start) AS event_year
  FROM
    encounters_with_neighbor
  WHERE
    event_id NOT IN (
    SELECT
      event_id
    FROM
      encounters_near_anchorage))
SELECT
  *
FROM
  encounters_not_in_port
```

### Mask high seas encounters

``` r
eez_and_land <- sf::read_sf(file.path(DIR_GCS_data, 
                                      "marine_regions_eez_with_land", 
                                      "EEZ_land_v2_201410.shp"))

encounters_sf <- st_as_sf(encounters, coords = c("lon_mean", "lat_mean"), crs = st_crs(eez_and_land))

encounters_sf <- st_join(encounters_sf, eez_and_land)
```

    Warning in st_is_longlat(x): bounding box has potentially an invalid value range
    for longlat data

``` r
HS_reefers_and_bunkers <- encounters_sf %>% 
  sf::st_set_geometry(NULL) %>%
  group_by(carrier_ssvid) %>% 
  summarize(all_encounters = n(),
            HS_encounters = sum(is.na(Country))) %>% 
  filter(HS_encounters > 0) %>% 
  mutate(fraction_encounters_in_HS = round(HS_encounters/all_encounters, 2))
```

``` r
sfc_as_cols <- function(x, names = c("x","y")) {
  stopifnot(inherits(x,"sf") && inherits(sf::st_geometry(x),"sfc_POINT"))
  ret <- sf::st_coordinates(x)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}

high_seas_encounters <- encounters_sf %>% 
  sfc_as_cols(names = c("lon", "lat")) %>% 
  st_drop_geometry() %>% 
  filter(is.na(Country)) %>% 
  select(-Country, -OBJECTID,-ISO_3digit, -Changes,-Shape_Leng,-Shape_Area, -event_year)

write_csv(high_seas_encounters, 
          here("data", "01_raw","HS_encounters.csv"))
```

### Get carrier/bunker info

``` r
reefers_info_sql <- glue::glue_sql("SELECT
  CAST(identity.ssvid AS STRING) AS mmsi,
  identity.n_callsign AS callsign,
  imo,
  identity.n_shipname AS shipname_norm,
  flag,
  if(is_carrier, 'carrier', 'bunker') vessel_class ,
  length_m,
  tonnage_gt,
  engine_power_kw,
  crew
FROM
  `world-fishing-827.vessel_database.all_vessels_v20190901`,
  UNNEST(registry),
  UNNEST(activity)
WHERE
  identity.ssvid in ({ssvid_of_interest*})
  AND 2018 BETWEEN EXTRACT(YEAR FROM first_timestamp) AND EXTRACT(YEAR FROM last_timestamp)",
  ssvid_of_interest = pull(HS_reefers_and_bunkers, carrier_ssvid),
  .con = BQ_connection
)

HS_reefers_and_bunkers_info <- dbGetQuery(BQ_connection, reefers_info_sql)
```

    Warning in class(obj) <- c("scalar", class(obj)): Setting class(x) to multiple
    strings ("scalar", "SQL", ...); result will no longer be an S4 object

``` r
Mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

HS_reefers_and_bunkers_info <- HS_reefers_and_bunkers_info %>% 
  ungroup() %>% 
  group_by(mmsi) %>% 
  summarize_all(.funs = Mode)

HS_reefers_and_bunkers <- HS_reefers_and_bunkers %>% 
  left_join(HS_reefers_and_bunkers_info, 
            by = c("carrier_ssvid" = "mmsi"))
```

## Ownership data

``` r
ownership_sql <- glue::glue_sql(
  "
  SELECT
  identity.ssvid AS ssvid,
  owner AS owner,
  owner_address,
  owner_flag
FROM
  `world-fishing-827.vessel_database.all_vessels_v20190901`,
  UNNEST(registry),
  UNNEST(activity)
WHERE
  (owner IS NOT NULL OR owner_address IS NOT NULL)
  AND matched
  AND 2018 BETWEEN EXTRACT(YEAR FROM first_timestamp) AND EXTRACT(YEAR FROM last_timestamp)
  AND identity.ssvid in ({ssvid_of_interest*})",
  ssvid_of_interest = c(pull(HS_vessels, ssvid), pull(HS_reefers_and_bunkers, carrier_ssvid)),
  .con = BQ_connection
)

gfw_ownership_info <- dbGetQuery(BQ_connection, ownership_sql)
```

    Warning in class(obj) <- c("scalar", class(obj)): Setting class(x) to multiple
    strings ("scalar", "SQL", ...); result will no longer be an S4 object

``` r
gfw_ownership_info <- gfw_ownership_info %>% 
  filter(!(owner_address == "UNKNOWN." & is.na(owner))) 

gfw_ownership_info <- gfw_ownership_info %>% 
  ungroup() %>% 
  group_by(ssvid) %>% 
  summarize(gfw_owner = Mode(owner),
            gfw_owner_address = Mode(owner_address),
            gfw_owner_flag = Mode(owner_flag))
```

``` r
HS_vessels <- HS_vessels %>% 
  left_join(gfw_ownership_info) %>% 
  mutate_if(is.numeric, round, 2) 

HS_reefers_and_bunkers <- HS_reefers_and_bunkers %>% 
  rename(ssvid = carrier_ssvid) %>% 
  left_join(gfw_ownership_info) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  select(ssvid, shipname_norm, flag, imo, callsign, vessel_class, length_m, tonnage_gt, engine_power_kw,
         gfw_owner, gfw_owner_address, gfw_owner_flag, all_encounters, HS_encounters, fraction_encounters_in_HS)

write_csv(HS_reefers_and_bunkers, 
          here("data", "01_raw", "HS_reefers_and_bunkers.csv"))

write_csv(HS_vessels, 
          here("data", "01_raw","HS_fishing_vessels.csv"))
```
