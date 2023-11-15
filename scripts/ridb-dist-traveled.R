# setup ----
# 0. need a Google Geocoding API key, then run in the Console:
  # register_google(key = "your_api_key")
# 1. run spatial-processing to get r5_au_bounds
# 2. run ridb-processing to get usfs_ridb and packages

# processing data before running calc ----
# a. dist_traveled = diff of customer zip and facility zip
# b. facility zip is NA for some values ~130 parks
# c. fill in NA facility zip using lat / lon
# d. lat / lon coords are inconsistent across multiple yrs of data
# e. use google api to obtain lat / lon coords and to standarize
# f. testing now

## getting zip codes using ggmap revgeocode() + Geocoding API ##
# small test for Pyramid Lake Los Alamos Campground
# NOTEHD: returned multiple addresses so not sure if this will work well...
lon <- -118.8090801769054
lat <- 34.70535168726219
result <- revgeocode(c(lon, lat), output = "address")

test <- usfs_ridb %>% 
  filter(is.na(facilityzip) == TRUE) %>% 
  group_by(forestname,
           park,
           facilityzip,
           facilitylongitude,
           facilitylatitude
  ) %>% 
  summarize(n = n()) %>% 
  mutate(
    facilityzip = ggmap::revgeocode()
  )

unique_parks_usfs_ridb2020 <- unique_parks_usfs_ridb2020 %>% 
  mutate(facilityzip = as.character(facilityzip))

unique_parks_usfs_ridb2021 <- usfs_ridb %>% 
  group_by(forestname,
           park,
           facilityzip,
           facilitylongitude,
           facilitylatitude) %>% 
  summarize(n = n()) %>% 
  select(!n)n

all_unique_parks_usfs_ridb <- unique_parks_usfs_ridb2018 %>% 
  rbind(unique_parks_usfs_ridb2019, 
        unique_parks_usfs_ridb2020,
        unique_parks_usfs_ridb2021) %>% 
  mutate(
    park = case_when(
      forestname == "Angeles National Forest" & park == "Table Mountain" ~ "Table Mountain (Angeles)",
      forestname == "Inyo National Forest" & park == "Table Mountain" ~ "Table Mountain - Inyo",
      forestname == "Tahoe National Forest" & park == "Cal" ~ "Cal-Ida",
      forestname == "Sierra National Forest" & park == "Badger Flat Group" ~ "Badger Flats Group",
      forestname == "Eldorado National Forest" & park == "Black Oak " ~ "Black Oak",
      park == "Cottonwood Thousand Trails Management Services, Inc" ~ "Cottonwood",
      forestname == "Six Rivers National Forest" & park == "East Fork Campground – Six Rivers Nf" ~ "East Fork Campground – Six Rivers",
      forestname == "Los Padres National Forest" & park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
      forestname == "Tahoe National Forest" & park == "Sierra" ~ "Sierra Campground",
      forestname == "Sierra National Forest" & park == "Texas Flat" ~ "Texas Flats",
      forestname == "Stanislaus National Forest" & park %in% c("Lake Alpine", "Lake Alpine West Shore Campground") ~ "Lake Alpine Campground",
      forestname == "Sequoia National Forest" & park == "Big Meadow Campground" ~ "Big Meadow Campground (Sequoia)",
      forestname == "Stanislaus National Forest" & park == "Big Meadow Campground" ~ "Big Meadow Campground (Stanislaus)",
      TRUE ~ park
    )
  ) %>% 
  group_by(forestname,
           park
  ) %>% 
  summarize(n = n(), .groups = "keep") %>% 
  select(!n) %>%
  ggmap::mutate_geocode(park)

# save as csv when done
clean_unique_parks <- all_unique_parks_usfs_ridb %>% 
  mutate(
    # Angeles NF
    lat = case_when(park == "Table Mountain (Angeles)" ~ 34.38658365964618,
                    park == "Jackson Flats" ~ 34.38123385965565,
                    TRUE ~ lat),
    lon = case_when(park == "Table Mountain (Angeles)" ~ -117.68872557186754,
                    park == "Jackson Flats" ~ -117.73521959137213,
                    TRUE ~ lon),
    # Cleveland NF
    lat = case_when(park == "Falcon Group" ~ 33.65669074396994,
                    park == "Laguna" ~ 32.89029798325346,
                    park == "Oak Grove Campground" ~ 33.386697981603476,
                    TRUE ~ lat),
    lon = case_when(park == "Falcon Group" ~ -117.45040321894156,
                    park == "Laguna" ~  -116.44816235473544,
                    park == "Oak Grove Campground" ~ -116.79087830628274,
                    TRUE ~ lon),
    # Eldorado NF
    lat = case_when(),
    lon = case_when()
  )

park_test <- clean_unique_parks %>% 
  filter(forestname == "Eldorado National Forest") %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

bounds <- r5_au_bounds %>% filter(forestname == "Eldorado National Forest")

mapview(list(bounds, park_test))


# calculate distance traveled ----
usfs_ridb <- usfs_ridb %>% 
  mutate(dist_traveled = zipcodeR::zip_distance(facilityzip, customerzip))

# NOTEHD: Need to clean up, turn into function
# zipcodeR_db <- zipcodeR::zip_code_db %>% 
#   select(zipcode, major_city, county, state,
#          lat, lng)

# ridb_dist_traveled <- usfs_ridb %>% 
#   select(forestname, park, facilityzip, customerzip) %>% 
#   left_join(zipcodeR_db,
#             by = c("customerzip" = "zipcode")) %>% 
#   mutate(dist_traveled = zipcodeR::zip_distance(facilityzip, customerzip))

zipcode_a <- as.character(usfs_ridb$facilityzip)
zipcode_b <- as.character(usfs_ridb$customerzip)

# assemble zipcodes in dataframe
zip_data <- data.frame(zipcode_a, zipcode_b)

# create subset of zip_code_db with only zipcode, lat, and lng
zip_db_small <- zipcodeR::zip_code_db %>%
  dplyr::select(zipcode, lat, lng) %>%
  # NOTEHD: 21% of data is NA
  dplyr::filter(lat != "NA" & lng != "NA") 

test <- zipcodeR::zip_code_db %>% 
  filter(is.na(lat) == TRUE)

# join input data with zip_code_db
zip_data <- zip_data %>%
  dplyr::left_join(zip_db_small, by = c('zipcode_a' = 'zipcode')) %>%
  dplyr::left_join(zip_db_small, by = c('zipcode_b' = 'zipcode'), suffix = c('.a', '.b'))

# assemble matrices for distance calculation
points_a <- cbind(cbind(zip_data$lng.a, zip_data$lat.a))
points_b <- cbind(cbind(zip_data$lng.b, zip_data$lat.b))

# Calculate the distance matrix between both sets of points
distance <- raster::pointDistance(points_a, points_b, lonlat = TRUE)

# Convert the distance matrix from meters to miles
if (units == "miles") {
  distance <- distance * 0.000621371
}

# Round to 2 decimal places to match search_radius()
distance <- round(distance, digits = 2)

# Put together the results in a data.frame
result <- data.frame(zipcode_a, zipcode_b, distance)

result_na <- result %>% 
  filter(is.na(zipcode_a) == TRUE)