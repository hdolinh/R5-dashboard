## NOTES HD ##
# fp = file path
# df = data frame
# vec = vector

# testing ----
# library(tidyr) # load to use ` %>% `
# spatial_data <- here::here("data/spatial/raw/S_USA.AdministrativeForest/")
# spatial_clean <- sf::read_sf(spatial_data) %>%
#   janitor::clean_names() %>%
#   dplyr::filter(region == "05")
# 
# forestname_vec <- as.vector(unique(spatial_clean$forestname))
# 
# mapview::mapview(spatial_clean)

# 1. get_spatial_data() ----
get_spatial_data <- function(fp, raw_df){
  
  df <- here::here(fp)
  raw_df <- sf::read_sf(df) %>% janitor::clean_names()
  
  return(raw_df)
}

# 2. r5_bounds() ----
r5_bounds <- function(raw_df, clean_df){
  
  clean_df <- raw_df %>% dplyr::filter(region == "05")
  
  return(clean_df)
}
# NOTEHD: To quickly view the spatial data as a map run:
# mapview::mapview(clean_df)

# 3. r5_forestnames() ----
r5_forestnames <- function(clean_df, forestname_vec){
  
  forestname_vec <- as.vector(unique(clean_df$forestname))
  
  # forestname_vec used in ridb_processing.R to filter out non-R5 forests
  return(forestname_vec)
}

# 4. r5_bounds_csv() ----
r5_bounds_csv <- function(clean_df, fp){
  
  sf::write_sf(clean_df, here(fp))
}
