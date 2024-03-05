# dfs = data frames

# 0. setup ----
library(vroom)
library(here)
library(dplyr)
library(tidyr)
library(janitor)
library(zipcodeR)
library(lubridate)

# load data
raw_ridb_2018 <- vroom::vroom(here::here("data/ridb/raw/reservations2018.csv"))
raw_ridb_2019 <- vroom::vroom(here::here("data/ridb/raw/reservations2019.csv"))
raw_ridb_2020 <- vroom::vroom(here::here("data/ridb/raw/FY20 Historical Reservations Full.csv"))
raw_ridb_2021 <- vroom::vroom(here::here("data/ridb/raw/FY21 Historical Reservations Full.csv"))

# load RIDB processing functions
source("scripts/ridb-functions.R")

# load spatial-processing.R to get `forestname_vec`
source("scripts/spatial-processing.R")

# create dfs as a list
all_raw_ridb <- list(
  ridb_2018 = raw_ridb_2018,
  ridb_2019 = raw_ridb_2019,
  ridb_2020 = raw_ridb_2020,
  ridb_2021 = raw_ridb_2021
)

# 1. RIDB Processing ----
combined_list <- list()

for (i in seq_along(all_raw_ridb)) {
  
  processed_ridb <- subset_ridb(raw_df = all_raw_ridb[[i]])
  processed_ridb <- clean_customer_zips(subset_df = processed_ridb)
  processed_ridb <- clean_forestname(customer_zips_df = processed_ridb,
                                     forestname_vec = forestname_vec)
  processed_ridb <- clean_park(forestname_df = processed_ridb)
  processed_ridb <- clean_facility_location(park_df = processed_ridb)
  processed_ridb <- calc_vars(facility_location_df = processed_ridb)
  processed_ridb <- clean_sitetype(calc_vars_df = processed_ridb)
  processed_ridb <- calc_dist_travel(sitetype_df = processed_ridb)
  processed_ridb <- add_states(dist_travel_df = processed_ridb)
  
  readr::write_csv(processed_ridb,
                   paste0(
                     here::here("data/ridb/clean/processed_"),
                     names(all_raw_ridb[i]),
                     ".csv"
                   ))
  
  readr::write_rds(processed_ridb,
                   paste0(
                     here::here("data/ridb/clean/processed_"),
                     # year
                     names(all_raw_ridb[i]),
                     ".rds"
                   ))
  
  combined_list <- append(combined_list, list(processed_ridb))
}

all_processed_ridb <- dplyr::bind_rows(combined_list)

readr::write_csv(all_processed_ridb,
                 here::here("data/ridb/clean/all_processed_ridb.csv"))

readr::write_rds(all_processed_ridb,
                 here::here("data/ridb/clean/all_processed_ridb.rds"))
