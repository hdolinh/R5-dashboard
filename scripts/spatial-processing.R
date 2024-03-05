# fp = file path
# df = data frame
# vec = vector
# shp = shapefile

# 0. setup ----
library(here)
library(sf)
library(tidyr)
library(dplyr)
library(janitor)
library(mapview)

# 1. load spatial data ----
fp <- here::here("data/spatial/raw/S_USA.AdministrativeForest/")
raw_df <- sf::read_sf(fp) %>% janitor::clean_names()

# 2. subset to R5 ----
r5_df <- raw_df %>% dplyr::filter(region == "05")

# To quickly view the spatial data as a map run:
# mapview::mapview(r5_df)

# 3. save R5 forestnames ----
# forestname_vec used in ridb_processing.R to filter out non-R5 forests
forestname_vec <- as.vector(unique(r5_df$forestname))

# 4. save spatial data as a shp ----
sf::write_sf(r5_df, here::here("data/spatial/clean/r5_bounds.shp"))
