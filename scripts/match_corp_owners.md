Match with corporate ownership
================

``` r
library(tidyverse)
library(naniar)
library(bigrquery)
library(DBI)
library(sf)
library(lubridate)
library(here)

DIR_GCS_data <- "~/gcs/spatial-datasets"

knitr::opts_chunk$set(comment=NA, message = F)
```

``` r
HS_vessels <- read_csv(here("data", "01_raw","HS_fishing_vessels.csv"), col_types = cols())

HS_reefers_and_bunkers <- read_csv(here("data", "01_raw", "HS_reefers_and_bunkers.csv"), col_types = cols())

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

``` r
HS_vessels <- HS_vessels %>% 
  left_join(vessels_corp_data ) %>% 
  replace_with_na_all(condition = ~.x == "n/a")
```

What % of fishing vessels can we assign corporate actors or ownership?

``` r
HS_vessels %>% 
  summarize(n_vessels = n(),
            f_with_corp_actor = sum(!is.na(corporate_actor))/n_vessels,
            f_with_owner = sum(!is.na(corporate_actor) | !is.na(gfw_owner))/n_vessels,
            f_no_idea = sum(is.na(corporate_actor) & is.na(gfw_owner))/n_vessels) %>% 
  mutate_if(is.numeric, round, 2) %>% 
    knitr::kable()
```

| n\_vessels | f\_with\_corp\_actor | f\_with\_owner | f\_no\_idea |
| ---------: | -------------------: | -------------: | ----------: |
|       3590 |                 0.72 |           0.83 |        0.17 |

To what % of fishing effort can we assign corporate actors or ownership?

``` r
HS_vessels %>% 
  summarize(HS_effort_in_hrs = sum(fishing_hours_HS),
            f_with_corp_actor = sum(fishing_hours_HS[!is.na(corporate_actor)])/HS_effort_in_hrs,
            f_with_owner = sum(fishing_hours_HS[!is.na(corporate_actor) | !is.na(gfw_owner)])/HS_effort_in_hrs,
            f_no_idea = sum(fishing_hours_HS[is.na(corporate_actor) & is.na(gfw_owner)])/HS_effort_in_hrs,
            ) %>% 
  mutate_if(is.numeric, round, 2) %>% 
    knitr::kable()
```

| HS\_effort\_in\_hrs | f\_with\_corp\_actor | f\_with\_owner | f\_no\_idea |
| ------------------: | -------------------: | -------------: | ----------: |
|             6884670 |                 0.76 |           0.89 |        0.11 |

## Reefers and bunkers

What % of reefer and bunker can we assign corporate actors or ownership?

``` r
HS_reefers_and_bunkers <- HS_reefers_and_bunkers %>% 
  left_join(reefers_corp_data ) %>% 
  replace_with_na_all(condition = ~.x == "n/a") 

HS_reefers_and_bunkers %>% 
  summarize(n_vessels = n(),
            f_with_corp_actor = sum(!is.na(corporate_actor))/n_vessels,
            f_with_owner = sum(!is.na(corporate_actor) | !is.na(gfw_owner))/n_vessels,
            f_no_idea = sum(is.na(corporate_actor) & is.na(gfw_owner))/n_vessels) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  knitr::kable()
```

| n\_vessels | f\_with\_corp\_actor | f\_with\_owner | f\_no\_idea |
| ---------: | -------------------: | -------------: | ----------: |
|        194 |                 0.63 |           0.97 |        0.03 |

What % of encounters can we assign corporate actors or ownership?

``` r
HS_reefers_and_bunkers %>% 
  summarize(total_HS_encounters = sum(HS_encounters),
            f_with_corp_actor = sum(HS_encounters[!is.na(corporate_actor)])/total_HS_encounters,
            f_with_owner = sum(HS_encounters[!is.na(corporate_actor) | !is.na(gfw_owner)])/total_HS_encounters,
            f_no_idea = sum(HS_encounters[is.na(corporate_actor) & is.na(gfw_owner)])/total_HS_encounters) %>% 
  mutate_if(is.numeric, round, 2) %>%
  knitr::kable()
```

| total\_HS\_encounters | f\_with\_corp\_actor | f\_with\_owner | f\_no\_idea |
| --------------------: | -------------------: | -------------: | ----------: |
|                  4752 |                 0.77 |           0.99 |        0.01 |

``` r
write_csv(HS_reefers_and_bunkers, 
          here("data", "03_output", "HS_reefers_and_bunker_with_corp_actors.csv"))

write_csv(HS_vessels, 
          here("data", "03_output", "HS_fishing_vessels_with_corp_actors.csv"))
```
