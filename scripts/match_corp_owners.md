---
title: "Match with corporate ownership"
output: 
  html_document: 
    keep_md: yes
---


```r
library(tidyverse)
```

```
## ── Attaching packages ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.0     ✓ purrr   0.3.3
## ✓ tibble  2.1.3     ✓ dplyr   0.8.5
## ✓ tidyr   1.0.2     ✓ stringr 1.4.0
## ✓ readr   1.3.1     ✓ forcats 0.5.0
```

```
## ── Conflicts ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(naniar)
library(bigrquery)
library(DBI)
library(sf)
```

```
## Linking to GEOS 3.8.0, GDAL 2.4.2, PROJ 6.2.0
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(here)
```

```
## here() starts at /Users/juanmayorga/github/high-seas-corporations
```

```
## 
## Attaching package: 'here'
```

```
## The following object is masked from 'package:lubridate':
## 
##     here
```

```r
DIR_GCS_data <- "~/gcs/spatial-datasets"
```


```r
HS_vessels <- read_csv(here("data", "01_raw","HS_fishing_vessels.csv"))
```

```
## Parsed with column specification:
## cols(
##   ssvid = col_double(),
##   shipname_norm = col_character(),
##   imo = col_double(),
##   callsign = col_character(),
##   vessel_class = col_character(),
##   flag = col_character(),
##   length_m = col_double(),
##   tonnage_gt = col_double(),
##   engine_power = col_double(),
##   hours = col_double(),
##   fishing_hours = col_double(),
##   hours_HS = col_double(),
##   fishing_hours_HS = col_double(),
##   gfw_owner = col_character(),
##   gfw_owner_address = col_character(),
##   gfw_owner_flag = col_character()
## )
```

```r
HS_reefers_and_bunkers <- read_csv(here("data", "01_raw", "HS_reefers_and_bunkers.csv"))
```

```
## Parsed with column specification:
## cols(
##   ssvid = col_double(),
##   shipname_norm = col_character(),
##   flag = col_character(),
##   imo = col_double(),
##   callsign = col_character(),
##   vessel_class = col_character(),
##   length_m = col_double(),
##   tonnage_gt = col_double(),
##   engine_power_kw = col_double(),
##   gfw_owner = col_character(),
##   gfw_owner_address = col_character(),
##   gfw_owner_flag = col_character(),
##   all_encounters = col_double(),
##   HS_encounters = col_double(),
##   fraction_encounters_in_HS = col_double()
## )
```

```r
vessels_corp_data <- readxl::read_xlsx(here("data", "02_processed","corp_info_HS_fishing_vessels.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(ssvid = mmsi, owner, owner_source, corporate_actor, corporate_actor_source, main_company, main_company_flag, 
         subsidiaries_and_affiliates, main_company_and_subsidiaries_source, remarks)

reefers_corp_data <- readxl::read_xlsx(here("data", "02_processed", "corp_info_HS_reefers_and_bunkers.xlsx")) %>% 
  janitor::clean_names() %>% 
  select(ssvid, owner, owner_source, corporate_actor, corporate_actor_source, main_company, main_company_flag, 
         subsidiaries_and_affiliates, main_company_and_subsidiaries_source, remarks)
```

# Match with corporate actors info

## Fishing vessels


```r
HS_vessels <- HS_vessels %>% 
  left_join(vessels_corp_data ) %>% 
  replace_with_na_all(condition = ~.x == "n/a")
```

```
## Joining, by = "ssvid"
```

What % of fishing vessels can we assign corporate actors or ownership?


```r
HS_vessels %>% 
  summarize(n_vessels = n(),
            f_vessels_with_corp_actor = sum(!is.na(corporate_actor))/n_vessels,
            f_vessels_with_owner = sum(!is.na(corporate_actor) | !is.na(gfw_owner))/n_vessels,
            f_vessels_no_idea = sum(is.na(corporate_actor) & is.na(gfw_owner))/n_vessels) %>% 
  mutate_if(is.numeric, round, 2) %>% 
    knitr::kable()
```



 n_vessels   f_vessels_with_corp_actor   f_vessels_with_owner   f_vessels_no_idea
----------  --------------------------  ---------------------  ------------------
      3590                        0.72                   0.83                0.17


To what % of fishing effort can we assign corporate actors or ownership?


```r
HS_vessels %>% 
  summarize(HS_effort = sum(fishing_hours_HS),
            f_HS_effort_with_corp_actor = sum(fishing_hours_HS[!is.na(corporate_actor)])/HS_effort,
            f_HS_effort_with_owner = sum(fishing_hours_HS[!is.na(corporate_actor) | !is.na(gfw_owner)])/HS_effort,
            f_HS_effort_with_no_idea = sum(fishing_hours_HS[is.na(corporate_actor) & is.na(gfw_owner)])/HS_effort,
            ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
    knitr::kable()
```



 HS_effort   f_HS_effort_with_corp_actor   f_HS_effort_with_owner   f_HS_effort_with_no_idea
----------  ----------------------------  -----------------------  -------------------------
   6884670                          0.76                     0.89                       0.11

## Reefers and bunkers

What % of reefer and bunker can we assign corporate actors or ownership?



```r
HS_reefers_and_bunkers <- HS_reefers_and_bunkers %>% 
  left_join(reefers_corp_data ) %>% 
  replace_with_na_all(condition = ~.x == "n/a") 
```

```
## Joining, by = "ssvid"
```

```r
HS_reefers_and_bunkers %>% 
  summarize(n_vessels = n(),
            n_vessels_with_corp_actor = sum(!is.na(corporate_actor))/n_vessels,
            n_vessels_with_owner = sum(!is.na(corporate_actor) | !is.na(gfw_owner))/n_vessels,
            n_vessels_no_idea = sum(is.na(corporate_actor) & is.na(gfw_owner))/n_vessels) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  knitr::kable()
```



 n_vessels   n_vessels_with_corp_actor   n_vessels_with_owner   n_vessels_no_idea
----------  --------------------------  ---------------------  ------------------
       194                        0.63                   0.97                0.03

What % of encounters can we assign corporate actors or ownership?


```r
HS_reefers_and_bunkers %>% 
  summarize(HS_encounters = sum(HS_encounters),
            HS_encounters_with_corp_actor = sum(HS_encounters[!is.na(corporate_actor)])/HS_encounters,
            HS_encounters_with_owner = sum(HS_encounters[!is.na(corporate_actor) | !is.na(gfw_owner)])/HS_encounters,
            HS_encounters_with_no_idea = sum(HS_encounters[is.na(corporate_actor) & is.na(gfw_owner)])/HS_encounters) %>% 
  mutate_if(is.numeric, round, 2) %>%
  knitr::kable()
```



 HS_encounters   HS_encounters_with_corp_actor   HS_encounters_with_owner   HS_encounters_with_no_idea
--------------  ------------------------------  -------------------------  ---------------------------
          4752                              NA                         NA                           NA


```r
write_csv(HS_reefers_and_bunkers, 
          here("data", "03_output", "HS_reefers_and_bunker_with_corp_actors.csv"))

write_csv(HS_vessels, 
          here("data", "03_output", "HS_fishing_vessels_with_corp_actors.csv"))
```

