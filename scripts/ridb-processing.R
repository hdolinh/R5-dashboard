## TO DO ##
# clean facility zip (e.g. add missing zips)
# calculate distance traveled

# setup ----
library(vroom)
library(here)
library(tidyverse)
library(janitor)
library(zipcodeR)
library(tidycensus)
library(ggmap) # citation("ggmap")
library(lubridate)

# 1. get_ridb_data() ----
get_ridb_data <- function(fp, file, df_name, raw_df) {
  
  # df_name = ridbYYYY
  df_name <- here::here(fp, file)
  raw_df <- vroom::vroom(df_name)
  
  return(raw_df)
}

# 2018 ridb data
ridb2018 <- here("data/ridb/raw/reservations2018.csv")
raw_ridb2018 <- vroom(ridb2018)

# 2019 ridb data
ridb2019 <- here("data/ridb/raw/reservations2019.csv")
raw_ridb2019 <- vroom(ridb2019)

# 2020 ridb data
ridb2020 <- here("data/ridb/raw/FY20 Historical Reservations Full.csv")
raw_ridb2020 <- vroom(ridb2020)

# 2020 ridb data
ridb2021 <- here("data/ridb/raw/FY21 Historical Reservations Full.csv")
raw_ridb2021 <- vroom(ridb2021)

subse

## RIDB Processing ## ----

# 2. subset_ridb() ----
subset_ridb <- function(subset_df, raw_df){
  
  subset_df <- raw_df %>%
    # add sept_out to rm `_` to match column names
    janitor::clean_names(sep_out = "") %>% 
    
    ## subset data ##
    # 1. cols of interest
    dplyr::select(
      # historicalreservationid,
      agency,
      parentlocation,
      park,
      sitetype,
      facilityzip,
      facilitystate,
      facilitylongitude,
      facilitylatitude,
      customerzip,
      totalpaid,
      startdate,
      enddate,
      orderdate,
      numberofpeople
    ) %>% 
    # 2. filter for just USFS in CA
    # NOTEHD: `case_when()` is for 2018 data
    dplyr::mutate(facilitystate = dplyr::case_when(facilitystate == "CA" ~ "California",
                                                   TRUE ~ facilitystate)) %>% 
    dplyr::filter(agency == "USFS" & facilitystate == "California") %>% 
    # NOTEHD: rm'ing to keep df as small as possible
    dplyr::select(-c(agency,facilitystate)) %>% 
    # create year col; keep only 2018+ obs
    mutate(year = lubridate::year(startdate)) %>% 
    relocate(year, .before = parentlocation) %>% 
    filter(year != 2017)
  
  return(subset_df)
}

# 3. clean_customer_zips() ----
clean_customer_zips <- function(customer_zips_df, subset_df){
  
  customer_zips_df <- subset_df %>%
    ## clean customer zip codes ##
    # 1. zip codes must be class character
    dplyr::mutate(
      customerzip = as.character(customerzip),
      # 2. extract 5 digit customer zip codes
      customerzip = stringr::str_extract(string = customerzip,
                                         pattern = "[:digit:]{5}")
    ) %>%
    # 3. rm invalid customer zip codes
    dplyr::filter(!customerzip %in% c("00000", "99999")) %>%
    # 4. rm NA customer zip codes
    tidyr::drop_na(customerzip)
  
  return(customer_zips_df)
}

# 4. clean_forestname() ----
clean_forestname <- function(forestname_df, customer_zips_df, forestname_vec){
  
  forestname_df <- customer_zips_df %>% 
    ## clean forestname ##
    # 1. rename parentlocation to forestname
    dplyr::rename(forestname = parentlocation) %>% 
    dplyr::mutate(
      # 2. standardize "National Forest"
      forestname = stringr::str_replace(
        string = forestname,
        pattern = paste(c(
          "NF - FS", "NF -FS", "NF- FS",
          "NF-FS", "-FS", " - FS"
        ),
        collapse = "|"),
        replacement = "National Forest"
      ), 
      # 3. standardize to title case
      forestname = stringr::str_to_title(forestname),
      # 4. fix forestname typos based on spatial data
      forestname = dplyr::case_when(
        forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
        forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
        forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
        forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
        forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
        forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests", 
        # keep forestname if none of the previous conditions are TRUE
        TRUE ~ forestname)
    ) %>% 
    # 5. rm forests NOT in R5
    # `forestname_vec` comes from spatial-processing.R
    dplyr::filter(forestname %in% forestname_vec)
  
  return(forestname_df)
}

# 5. clean_park() ----
clean_park <- function(park_df, forestname_df){
  
  park_df <- forestname_df %>% 
    ## clean park ##
    dplyr::mutate(
      # 1. standardize case
      park = stringr::str_to_title(park), # 367 tot
      # 2. fix string errors
      park = stringr::str_remove(string = park,
                                 pattern = paste(
                                   c("\\(.*", " \\(.*",
                                     "---.*", " ---.*",
                                     "-.*", " -.*",
                                     ",.*"),
                                   collapse = "|"
                                 )), 
      park = stringr::str_replace(string = park,
                                  pattern = "@",
                                  replacement = "At"), 
      park = stringr::str_replace(string = park,
                                  pattern = "Cg",
                                  replacement = "Campground"), 
      park = stringr::str_replace(string = park,
                                  pattern = "&",
                                  replacement = "And"), 
      park = stringr::str_replace(string = park,
                                  pattern = paste(c("/", " / "), collapse = "|"),
                                  replacement = " "), 
      # 3. match park values for each NF
      park = dplyr::case_when(
        # Angeles NF
        park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
        # Eldorado NF
        park == "Bear Group Campground" ~ "Bear River Group Campground",
        park == "Black Oak " ~ "Black Oak",
        # Inyo NF
        forestname == "Inyo National Forest" & park == "East Fork California" ~ "East Fork Campground – Inyo National Forest",
        park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
        park == "Table Mountain Inyo" ~ "Table Mountain",
        # Los Padres NF
        park == "Arroyo Seco Campground" ~ "Arroyo Seco",
        park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
        park == "Los Prietos Campground" ~ "Los Prietos",
        # Plumas NF
        park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
        # San Bernardino NF
        park == "Black Mountain Group" ~ "Black Mountain",
        # Sierra NF
        park == "Badger Flat Group" ~ "Badger Flats Group",
        park == "Texas Flat" ~ "Texas Flats",
        # Sequoia NF
        park == "Camp 3 Campground" ~ "Camp Three Campground",
        park == "French Gulch Group" ~ "French Gulch",
        # Six Rivers NF
        park == "Boise Creek Campground" ~ "Boise Creek",
        forestname == "Six Rivers National Forest" & park == "East Fork Campground" ~ "East Fork Campground – Six Rivers Nf",
        park == "Oak Bottom" ~ "Oak Bottom Campground",
        # Stanislaus NF
        park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
        park == "Pioneer Trails" ~ "Pioneer Trail",
        park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
        park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
        # Tahoe NF
        park == "Boca Rest" ~ "Boca Rest Campground",
        park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
        park == "Cal" ~ "Cal-Ida",
        park == "Sierra Campground" ~ "Sierra",
        park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
        park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
        TRUE ~ park
      ),
      # 4. fill in missing park values (necessary for 2019-2021 data)
      # a. convert to char to reveal full coordinate
      facilitylongitude = as.character(facilitylongitude),
      facilitylatitude = as.character(facilitylatitude),
      # b. use lat/long to fill in missing park values
      park = dplyr::case_when(
        facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Group Campground",
        facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
        facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
        facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
        facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
        facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine Campground",
        facilitylatitude == "38.4163889" & facilitylongitude == "-120.105" ~ "Big Meadow",
        TRUE ~ park
      ),
      # c. convert lat/long back to numeric
      facilitylongitude = as.numeric(facilitylongitude),
      facilitylatitude = as.numeric(facilitylatitude)
    )
  
  return(park_df)
}

# 6. clean_facility_location() ----
park_location <- all_ridb %>%
  group_by(forestname,
           park,
           facilityzip,
           facilitylongitude,
           facilitylatitude) %>%
  summarize(count = n()) %>% 
  mutate(facilitylongitude = as.character(facilitylongitude),
         facilitylatitude = as.character(facilitylatitude))


park_location_na <- all_ridb %>%
  filter(is.na(facilityzip) == TRUE) %>%
  group_by(forestname,
           park,
           facilityzip,
           facilitylongitude,
           facilitylatitude) %>%
  summarize(count = n()) %>% 
  mutate(
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude)
    )
  

clean_facility_location <- function(facility_location_df, park_df){
  
  facility_location_df <- park_df %>% 
    ## clean facility zips & facility lat/lon ##
    dplyr::mutate(
      
      # convert to chr to see full coordinate
      facilitylongitude = as.character(facilitylongitude),
      facilitylatitude = as.character(facilitylatitude),
      # chose zip with highest count
      facilityzip = case_when(
        # Cleveland NF
        forestname == "Cleveland National Forest" & park == "El Prado Group" ~ "91948",
        forestname == "Cleveland National Forest" & park == "Falcon Group" ~ "92679",
        forestname == "Cleveland National Forest" & park == "Fry Creek Campground" ~ "92060",
        forestname == "Cleveland National Forest" & park == "Horse Heaven Group" ~ "92060",
        forestname == "Cleveland National Forest" & park == "Laguna" ~ "91948",
        # Eldorado NF
        forestname == "Eldorado National Forest" & park == "Black Oak" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Gerle Creek" ~ "95726",
        forestname == "Eldorado National Forest" & park == "Ice House" ~ "95735",
        # Inyo NF
        forestname == "Inyo National Forest" & park == "Aspen Group" ~ "93546",
        forestname == "Inyo National Forest" & park == "Big Pine Canyon" ~ "93513",
        forestname == "Inyo National Forest" & park == "Big Pine Creek Campground" ~ "93513",
        forestname == "Inyo National Forest" & park == "Bishop Park Group" ~ "93514",
        forestname == "Inyo National Forest" & park == "Coldwater Campground" ~ "93546",
        forestname == "Inyo National Forest" & park == "East Fork Campground – Inyo National Forest" ~ "93514",
        forestname == "Inyo National Forest" & park == "French Camp" ~ "93514",
        forestname == "Inyo National Forest" & park == "Grays Meadows" ~ "93526",
        forestname == "Inyo National Forest" & park == "June Lake" ~ "93529",
        # Lake Tahoe Basin Mgmt Unit
        forestname == "Lake Tahoe Basin Management Unit" & park == "Desolation Wilderness Permit" ~ "95735",
        # Lassen NF
        forestname == "Lassen National Forest" & park == "Almanor" ~ "96020",
        forestname == "Lassen National Forest" & park == "Gurnsey Creek" ~ "96061",
        forestname == "Lassen National Forest" & park == "Hat Creek" ~ "96071",
        # Plumas NF
        forestname == "Plumas National Forest" & park == "Hallsted" ~ "95984",
        forestname == "Plumas National Forest" & park == "Hutchins" ~ "95971",
        # San Bernardino NF
        forestname == "San Bernardino National Forest" & park == "Barton Flats" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Big Pine Equestrian Group Campground" ~ "92356",
        forestname == "San Bernardino National Forest" & park == "Bluff Mesa Group Camp" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Boulder Group Camp" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Buttercup Group Camp" ~ "92315",
        forestname == "San Bernardino National Forest" & park == "Crab Flats" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Deer Group Camp" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Dogwood" ~ "92352",
        forestname == "San Bernardino National Forest" & park == "Grays Peak Group Camp" ~ "92333",
        forestname == "San Bernardino National Forest" & park == "Green Spot Equestrian Group Camp" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Hanna Flat" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Heart Bar Campground" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Heart Bar Equestrian" ~ "92315",
        forestname == "San Bernardino National Forest" & park == "Ironwood Group Camp" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Juniper Springs Group Camp" ~ "92314",
        # Sequoia NF
        forestname == "Sequoia National Forest" & park == "Belknap" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Cove Group" ~ "93262",
        forestname == "Sequoia National Forest" & park == "Coy Flat" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Fir Group" ~ "93262",
        forestname == "Sequoia National Forest" & park == "Holey Meadow" ~ "93257",
        forestname == "Sequoia National Forest" & park == "Hospital Flat" ~ "93238",
        # Sierra NF
        forestname == "Sierra National Forest" & park == "Badger Flats Group" ~ "93634",
        forestname == "Sierra National Forest" & park == "Catavee" ~ "93634",
        forestname == "Sierra National Forest" & park == "Cedar Bluff" ~ "93669",
        forestname == "Sierra National Forest" & park == "Chilkoot" ~ "93604",
        forestname == "Sierra National Forest" & park == "College" ~ "93634",
        forestname == "Sierra National Forest" & park == "Deer Creek" ~ "93634",
        forestname == "Sierra National Forest" & park == "Dinkey Creek" ~ "93664",
        forestname == "Sierra National Forest" & park == "Dirt Flat" ~ "95318",
        forestname == "Sierra National Forest" & park == "Dorabelle Campground" ~ "93664",
        forestname == "Sierra National Forest" & park == "Dry Gulch" ~ "95318",
        forestname == "Sierra National Forest" & park == "Jackass Meadow" ~ "93634",
        forestname == "Sierra National Forest" & park == "Kelty Meadow" ~ "93644",
        forestname == "Sierra National Forest" & park == "Kinnikinnick" ~ "93634",
        # Six Rivers NF
        forestname == "Six Rivers National Forest" & park == "Dillon Creek Campground" ~ "96039",
        forestname == "Six Rivers National Forest" & park == "Fish Lake Campground" ~ "95556",
        # Stanislaus NF
        forestname == "Stanislaus National Forest" & park == "Big Meadow" ~ "95223",
        # Tahoe NF
        forestname == "Tahoe National Forest" & park == "East Meadow Campground" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Findley Campground" ~ "96161",
        forestname == "Tahoe National Forest" & park == "Fir Top Campground" ~ "96161",
        TRUE ~ facilityzip
      ),
      
      facilitylongitude = case_when(
        # Cleveland NF
        park == "El Prado Group" & facilityzip == "91948" ~ -116.4555556,
        park == "Falcon Group" & facilityzip == "92679" ~ -117.4602778,
        park == "Fry Creek Campground" & facilityzip == "92060" ~ -116.88,
        park == "Horse Heaven Group" & facilityzip == "91948" ~ -116.4408333,
        park == "Laguna" & facilityzip == "91948" ~ -116.4463889,
        # Eldorado NF
        park == "Black Oak" & facilityzip == "95735" ~ -120.5875,
        park == "Gerle Creek" & facilityzip == "95726" ~ -120.3916667,
        park == "Ice House" & facilityzip == "95735" ~ -120.3588889,
        # Inyo NF
        park == "Aspen Group" & facilityzip == "93514" ~ -118.711944444444,
        park == "Big Pine Canyon" & facilityzip == "93513" ~ -118.4222222,
        park == "Big Pine Creek Campground" & facilityzip == "93513" ~ -118.4325,
        park == "Bishop Park Group" & facilityzip == "93514" ~ -118.5933333,
        park == "Coldwater Campground" & facilityzip == "93546" ~ -118.9969444,
        park == "East Fork Campground – Inyo National Forest" & facilityzip == "93514" ~ -118.7175,
        park == "French Camp" & facilityzip == "93514" ~ -118.6791667,
        park == "Grays Meadows" & facilityzip == "93526" ~ -118.295,
        park == "June Lake" & facilityzip == "93529" ~ -119.0738889,
        # Lake Tahoe Basin Mgmt Unit
        park == "Lake Tahoe Basin Management Unit" & facilityzip == "95735" ~ -120.17,
        # Lassen NF
        park == "Almanor" & facilityzip == "96020" ~ -121.167777777778,
        park == "Gurnsey Creek" & facilityzip == "96061" ~ -121.4266667,
        park == "Hat Creek" & facilityzip == "96071" ~ -121.4455556,
        # Plumas NF
        park == "Hallsted" & facilityzip == "95984" ~ -121.0730556,
        park == "Hutchins" & facilityzip == "95971" ~ -121.1991667,
        # San Bernardino NF
        park == "Barton Flats" & facilityzip == "92305" ~ -116.8744444,
        park == "Big Pine Equestrian Group Campground" & facilityzip == "92356" ~ -117.00852475500508,
        park == "Bluff Mesa Group Camp" & facilityzip == "92305" ~ -116.9607795759009,
        park == "Boulder Group Camp" & facilityzip == "92305" ~ -116.94388005467727,
        park == "Buttercup Group Camp" & facilityzip == "92315" ~ -116.8802778,
        park == "Crab Flats" & facilityzip == "92314" ~ -117.0833333,
        park == "Deer Group Camp" & facilityzip == "92305" ~ -116.91664548351255,
        park == "Dogwood" & facilityzip == "92352" ~ -117.2091667,
        park == "Grays Peak Group Camp" & facilityzip == "92333" ~ -116.9705556,
        park == "Green Spot Equestrian Group Camp" & facilityzip == "92314" ~ -116.8061111,
        park == "Hanna Flat" & facilityzip == "92314" ~ -116.9744444,
        park == "Heart Bar Campground" & facilityzip == "92305" ~ -116.7858333,
        park == "Heart Bar Equestrian" & facilityzip == "92315" ~ -116.78123468792933,
        park == "Ironwood Group Camp" & facilityzip == "92314" ~ -117.0119444,
        park == "Juniper Springs Group Camp" & facilityzip == "92314" ~ -116.7163889,
        # Sequoia NF
        park == "Belknap" & facilityzip == "93208" ~ -118.5997222,
        park == "Cove Group" & facilityzip == "93262" ~ -118.8380556,
        park == "Coy Flat" & facilityzip == "93208" ~ -118.6180556,
        park == "Fir Group" & facilityzip == "93262" ~ -118.8416667,
        park == "Holey Meadow" & facilityzip == "93257" ~ -118.6180556,
        park == "Hospital Flat" & facilityzip == "93238" ~ -118.4577778,
        # Sierra NF
        park == "Badger Flats Group" & facilityzip == "93634" ~ -119.115,
        park == "Catavee" & facilityzip == "93634" ~ -119.1769444,
        park == "Cedar Bluff" & facilityzip == "93669" ~ -119.5441667,
        park == "Chilkoot" & facilityzip == "93604" ~ -119.5386111,
        park == "College" & facilityzip == "93634" ~ -119.1688889,
        park == "Deer Creek" & facilityzip == "93634" ~ -119.1769444,
        park == "Dinkey Creek" & facilityzip == "93664" ~ -119.1538889,
        park == "Dirt Flat" & facilityzip == "95318" ~ -119.8444444,
        park == "Dorabelle Campground" & facilityzip == "93664" ~ -119.3097222,
        park == "Dry Gulch" & facilityzip == "95318" ~ -119.8444444,
        park == "Jackass Meadow" & facilityzip == "93634" ~ -118.9636111,
        park == "Kelty Meadow" & facilityzip == "93644" ~ -119.5438889,
        park == "Kinnikinnick" & facilityzip == "93634" ~ -119.1777778,
        # Six Rivers NF
        park == "Dillon Creek Campground" & facilityzip == "96039" ~ -123.5430556,
        park == "Fish Lake Campground" & facilityzip == "95556" ~ -123.6844444,
        # Stanislaus NF
        park == "Big Meadow" & facilityzip == "95223" ~ -120.105,
        # Tahoe NF
        park == "East Meadow Campground" & facilityzip == "96126" ~ -120.5325,
        park == "Findley Campground" & facilityzip == "96161" ~ -120.5530556,
        park == "Fir Top Campground" & facilityzip == "96161" ~ -120.5502778,
        TRUE ~ facilitylongitude
      ),
      facilitylatitude = case_when(
        # Cleveland NF
        park == "El Prado Group" & facilityzip == "91948" ~ 32.8869444,
        park == "Falcon Group" & facilityzip == "92679" ~ 33.6558333,
        park == "Fry Creek Campground" & facilityzip == "92060" ~ 33.345,
        park == "Horse Heaven Group" & facilityzip == "91948" ~ 32.8875,
        park == "Laguna" & facilityzip == "91948" ~ 32.8872222,
        # Eldorado NF
        park == "Black Oak" & facilityzip == "95735" ~ 38.9041667,
        park == "Gerle Creek" & facilityzip == "95726" ~ 38.975,
        park == "Ice House" & facilityzip == "95735" ~ 38.8233333,
        # Inyo NF
        park == "Aspen Group" & facilityzip == "93514" ~ 37.5236111111111,
        park == "Big Pine Canyon" & facilityzip == "93513" ~ 37.1283333,
        park == "Big Pine Creek Campground" & facilityzip == "93513" ~ 37.1258333,
        park == "Bishop Park Group" & facilityzip == "93514" ~ 37.2438889,
        park == "Coldwater Campground" & facilityzip == "93546" ~ 37.5991667,
        park == "East Fork Campground – Inyo National Forest" & facilityzip == "93514" ~ 37.4836111,
        park == "French Camp" & facilityzip == "93514" ~ 37.5525,
        park == "Grays Meadows" & facilityzip == "93526" ~ 36.7688889,
        park == "June Lake" & facilityzip == "93529" ~ 37.7819444,
        # Lake Tahoe Basin Mgmt Unit
        park == "Lake Tahoe Basin Management Unit" & facilityzip == "95735" ~ 38.9197222,
        # Lassen NF
        park == "Almanor" & facilityzip == "96020" ~ 40.2169444444444,
        park == "Gurnsey Creek" & facilityzip == "96061" ~ 40.3088889,
        park == "Hat Creek" & facilityzip == "96071" ~ 40.6677778,
        # Plumas NF
        park == "Hallsted" & facilityzip == "95984" ~ 40.0175,
        park == "Hutchins" & facilityzip == "95971" ~ 39.8830556,
        # San Bernardino NF
        park == "Barton Flats" & facilityzip == "92305" ~ 34.1722222,
        park == "Big Pine Equestrian Group Campground" & facilityzip == "92356" ~ 34.316014961609305,
        park == "Bluff Mesa Group Camp" & facilityzip == "92305" ~ 34.22712525014481,
        park == "Boulder Group Camp" & facilityzip == "92305" ~ 34.22355039789103,
        park == "Buttercup Group Camp" & facilityzip == "92315" ~ 34.2352778,
        park == "Crab Flats" & facilityzip == "92314" ~ 34.2616667,
        park == "Deer Group Camp" & facilityzip == "92305" ~ 34.22210844156979,
        park == "Dogwood" & facilityzip == "92352" ~ 34.2352778,
        park == "Grays Peak Group Camp" & facilityzip == "92333" ~ 34.2733333,
        park == "Green Spot Equestrian Group Camp" & facilityzip == "92314" ~ 34.2233333,
        park == "Hanna Flat" & facilityzip == "92314" ~ 34.2877778,
        park == "Heart Bar Campground" & facilityzip == "92305" ~ 34.1586111,
        park == "Heart Bar Equestrian" & facilityzip == "92315" ~ 34.15572292102622,
        park == "Ironwood Group Camp" & facilityzip == "92314" ~ 34.3038889,
        park == "Juniper Springs Group Camp" & facilityzip == "92314" ~ 34.2197222,
        # Sequoia NF
        park == "Belknap" & facilityzip == "93208" ~ 36.1416667,
        park == "Cove Group" & facilityzip == "93262" ~ 36.665,
        park == "Coy Flat" & facilityzip == "93208" ~ 36.1291667,
        park == "Fir Group" & facilityzip == "93262" ~ 36.6641667,
        park == "Holey Meadow" & facilityzip == "93257" ~ 35.9541667,
        park == "Hospital Flat" & facilityzip == "93238" ~ 35.8286111,
        # Sierra NF
        park == "Badger Flats Group" & facilityzip == "93634" ~ 37.2694444444445,
        park == "Catavee" & facilityzip == ~ "93634" ~ 37.2525,
        park == "Cedar Bluff" & facilityzip == "93669" ~ 37.3077778,
        park == "Chilkoot" & facilityzip == "93604" ~ 37.3627778,
        park == "College" & facilityzip == "93634" ~ 37.2519444,
        park == "Deer Creek" & facilityzip == "93634" ~ 37.2519444,
        park == "Dinkey Creek" & facilityzip == "93664" ~ 37.0730556,
        park == "Dirt Flat" & facilityzip == "95318" ~ 37.6633333,
        park == "Dorabelle Campground" & facilityzip == "93664" ~ 37.1138889,
        park == "Dry Gulch" & facilityzip == "95318" ~ 37.6633333,
        park == "Jackass Meadow" & facilityzip == "93634" ~ 37.2797222,
        park == "Kelty Meadow" & facilityzip == "93644" ~ 37.4402778,
        park == "Kinnikinnick" & facilityzip == "93634" ~ 37.2527778,
        # Six Rivers NF
        park == "Dillon Creek Campground" & facilityzip == "96039" ~ 41.5733333,
        park == "Fish Lake Campground" & facilityzip == "95556" ~ 41.2641667,
        # Stanislaus NF
        park == "Big Meadow" & facilityzip == "95223" ~ 38.4163889,
        # Tahoe NF
        park == "East Meadow Campground" & facilityzip == "96126" ~ 39.5008333,
        park == "Findley Campground" & facilityzip == "96161" ~ 39.4847222,
        park == "Fir Top Campground" & facilityzip == "96161" ~ 39.4855556,
        TRUE ~ facilitylatitude
      ),
      # convert back to numeric
      facilitylongitude = as.numeric(facilitylongitude),
      facilitylatitude = as.numeric(facilitylatitude),
    )
}

# 7. clean_sitetype() ----
clean_sitetype <- function(sitetype_df, facility_zips_df){
  
  sitetype_df <- facility_zips_df %>% 
    ## clean sitetype ##
    dplyr::mutate(
      # 1. standardize to title case
      sitetype = stringr::str_to_title(sitetype),
      # 2. redefine "Management" sitetype for 2019-2021
      sitetype = dplyr::case_when(
        sitetype == "Management" & park == "Agnew Horse Camp" ~ "Equestrian",
        sitetype == "Management" & park %in% c("Almanor",
                                               "Boulder Creek",
                                               "Camp 9",
                                               "Dogwood",
                                               "Dorabelle Campground",
                                               "Fallen Leaf Campground",
                                               "Forks Campground",
                                               "French Meadows",
                                               "Giant Gap",
                                               "Hume Lake",
                                               "Laguna",
                                               "Lakeshore East",
                                               "Lewis At French Meadows",
                                               "Lodgepole Group",
                                               "Mcgill Campground And Group Campground",
                                               "Merrill Campground",
                                               "Mill Creek Campground",
                                               "New Shady Rest Campground",
                                               "Oh Ridge",
                                               "Onion Valley",
                                               "Salmon Creek",
                                               "Sandy Flat",
                                               "Sardine Lake",
                                               "Schoolhouse Campground",
                                               "Serrano",
                                               "Silver Valley Campground",
                                               "Silvertip Campground",
                                               "Summerdale Campground",
                                               "Sycamore Grove",
                                               "Tillie Creek",
                                               "Whitney Portal") ~ "RV or Tent",
        sitetype == "Management" & park %in% c("Aspen Grove Campground",
                                               "Faucherie",
                                               "Mono Creek") ~ "Tent Only",
        sitetype == "Management" & park == "Grouse Valley" ~ "Shelter",
        TRUE ~ sitetype
      ),
      # 3. aggregate sitetype
      sitetype = dplyr::case_when(
        # a. day use
        lengthofstay == 0 |
          sitetype == "Entry Point" &
          park == "Cedar Creek Falls" ~ "Day Use",
        # b. remote
        sitetype %in% c("Group Walk To",
                        "Walk To",
                        "Destination Zone") |
          sitetype == "Trailhead" & lengthofstay > 0 |
          sitetype %in% c("Trailhead",
                          "Entry Point") &
          park %in% c(
            "Sierra National Forest Wilderness Permits",
            "Inyo National Forest",
            "Mt. Whitney",
            "Desolation Wilderness Permit"
          ) ~ "Remote",
        # c. shelter
        sitetype %in% c(
          "Cabin Electric",
          "Cabin Nonelectric",
          "Group Shelter Nonelectric",
          "Shelter Nonelectric",
          "Yurt"
        ) ~ "Shelter",
        # d. water
        sitetype == "Boat In" |
          sitetype == "Entry Point" &
          park == "Tuolumne River Permits" ~ "Water",
        # e. equestrian
        sitetype %in% c("Equestrian Nonelectric",
                        "Group Equestrian") ~ "Equestrian",
        # f. rv only
        sitetype %in% c("Group Rv Area Nonelectric",
                        "Rv Electric",
                        "Rv Nonelectric") ~ "RV Only",
        # g. tent only
        sitetype %in% c("Group Tent Only Area Nonelectric",
                        "Tent Only Electric",
                        "Tent Only Nonelectric") |
          sitetype == "Entry Point" &
          lengthofstay > 0 ~ "Tent Only",
        # h. rv or tent
        sitetype %in% c(
          "Group Standard Area Nonelectric",
          "Group Standard Electric",
          "Group Standard Nonelectric",
          "Standard Nonelectric",
          "Standard Electric"
        ) ~ "RV or Tent",
        TRUE ~ sitetype
      )
    )
  
  return(sitetype_df)
}

# 8. calc_vars() ----
calc_vars <- function(calc_vars_df, sitetype_df){
  
  calc_vars_df <- sitetype_df %>% 
    ## calculate new variables ##
    dplyr::mutate(
      # 1. length of stay
      lengthofstay = as.numeric(difftime(enddate, startdate), units = "days"),
      # a. change neg length of stay to positive;
      # NOTEHD: previous conversations w/ R1S confirm neg start date is data error
      lengthofstay = dplyr::if_else(
        lengthofstay < 0,
        true = as.numeric(difftime(startdate, enddate), units = "days"),
        false = lengthofstay
      ),
      
      # 2. booking window
      bookingwindow = round(as.numeric(difftime(startdate, orderdate), units = "days"), 0), 
      
      # 3. daily cost
      dailycost = round(totalpaid / lengthofstay, 2),
      
      # 4. daily cost per visitor
      dailycostpervisitor = round(dailycost / numberofpeople, 2),
    )
  
  return(calc_vars_df)
}

# add state for each ZIP code ----
# create df of fips and full state names
# NOTEHD: df_states_fips is from outdoor equity code
# create only if we need fips and state full names
fips_vec <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
              "13", "15", "16", "17", "18", "19", "20", "21", "22", "23",
              "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
              "34", "35", "36", "37", "38", "39", "40", "41", "42", "44",
              "45", "46", "47", "48", "49", "50", "51", "53", "54", "55",
              "56", "72")

state_vec <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
               "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
               "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
               "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
               "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
               "WY", "PR")

states_full_names_vec <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                           "Colorado", "Connecticut", "Delaware", "District of Columbia",
                           "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                           "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                           "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                           "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                           "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                           "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                           "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                           "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")

df_states_fips <- as.data.frame(list(fips = fips_vec,
                                     state = state_vec,
                                     state_full = states_full_names_vec))

zipcode_db <- zipcodeR::zip_code_db %>% 
  select(zipcode,
         state,
         county
         # major_city,
         # lat,
         # lng
         ) %>% 
  left_join(df_states_fips, by = "state") %>% 
  relocate(fips, .before = state) %>% 
  relocate(state_full, .after = state) #%>% 
  # rename(customerlng = lng) %>% 
  # rename(customerlat = lat)

# df with customer state info
usfs_ridb <- usfs_ridb %>%
  left_join(zipcode_db, by = c("customerzip" = "zipcode"), multiple = "all")

# move columns + add FY col
# NOTEHD: rerun for each year, change year as needed
usfs_ridb2020 <- usfs_ridb %>% 
  relocate(fips, .after = customerzip) %>% 
  relocate(state, .after = fips) %>% 
  relocate(state_full, .after = state) %>% 
  mutate(fy = as.factor(2020)) %>% 
  relocate(fy, .before = historicalreservationid)
  
# calculate distance traveled ----
# see ridb-dist-traveled.R script

# combine data ----
# 2018 processed (300,809 obs)
# 2019 processed (228,956 obs)
# 2020 processed (387,564 obs)
# 2021 processed (469,376 obs)
# grand total 1,386,705 obs
usfs_ridb_all <- usfs_ridb2018 %>%
  rbind(usfs_ridb2019,
        usfs_ridb2020,
        usfs_ridb2021)

# save data ----
write_csv(usfs_ridb_all, here("data/ridb/clean/usfs_ridb_all.csv"))

# notes ----

## adding 'Yurt' to park leave it alone for now... ##
# note that all parks with 'Shelter' sitetype that doesn't say cabin in
# park name means it was most likely a 'Yurt' sitetype originally
# remote_test <- usfs_ridb %>% 
#   filter(sitetype == "Yurt") %>% 
#   mutate(
#     park = if_else(str_detect(sitetype, "Yurt") == TRUE,
#                    true = paste0(park, " Yurt"),
#                    false = park,
#                    missing = park),
#   ) %>% 
#   count(park)
# 
# remote_test2018 <- usfs_ridb2018 %>% 
#   filter(sitetype == "Yurt") %>% 
#   group_by(park, sitetype) %>% 
#   summarize(n = n()) %>% 
#   mutate(
#     # adding 'Yurt' to park
#     park = if_else(str_detect(sitetype, "Yurt") == TRUE,
#                    true = str_replace(park, park, paste0(park, " Yurt")),
#                    false = park,
#                    missing = park),
#   )

## neg length of stay ##
# 2018 tot 0
# 2019 tot 424
# 2020 tot 202
# 2021 tot 279
# lengthofstay_test <- usfs_ridb %>%
#   filter(lengthofstay < 0)

## customer zip code NA ##
# ~1.8% of 2018 data (tot 306,518)
# zipcode2018_test <- usfs_ridb2018 %>%
#   filter(is.na(customerzip) == TRUE)# %>% 
  # group_by(forestname) %>% 
  # summarize(n = n())

# ~37% of 2019 data (tot 363,714)
# ~26% of 2020 data (tot 520,568)
# ~23% of 2021 data (tot 617,333 obs; tot na 139,645) expect 477,688 obs

## test ##
ridb2021 <- raw_ridb2021 %>%
  # add sept_out to rm `_` to match column names
  janitor::clean_names(sep_out = "") %>% 
  
  ## subset data ##
  # 1. cols of interest
  dplyr::select(
    # historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # 2. filter for just USFS in CA
  # NOTEHD: `case_when()` is for 2018 data
  dplyr::mutate(facilitystate = dplyr::case_when(facilitystate == "CA" ~ "California",
                                                 TRUE ~ facilitystate)) %>% 
  dplyr::filter(agency == "USFS" & facilitystate == "California") %>% 
  # NOTEHD: rm'ing to keep df as small as possible
  dplyr::select(-c(agency,facilitystate)) %>% 
  # create year col; keep only 2018+ obs
  mutate(year = lubridate::year(startdate)) %>% 
  relocate(year, .before = parentlocation) %>% 
  filter(year != 2017) %>% 
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  dplyr::rename(forestname = parentlocation) %>% 
  dplyr::mutate(
    # 2. standardize "National Forest"
    forestname = stringr::str_replace(
      string = forestname,
      pattern = paste(c(
        "NF - FS", "NF -FS", "NF- FS",
        "NF-FS", "-FS", " - FS"
      ),
      collapse = "|"),
      replacement = "National Forest"
    ), 
    # 3. standardize to title case
    forestname = stringr::str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
    forestname = dplyr::case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests", 
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # 5. rm forests NOT in R5
  # `forestname_vec` comes from spatial-processing.R
  dplyr::filter(forestname %in% forestname_vec) %>% 
  ## clean park ##
  dplyr::mutate(
    # 1. standardize case
    park = stringr::str_to_title(park), # 367 tot
    # 2. fix string errors
    park = stringr::str_remove(string = park,
                               pattern = paste(
                                 c("\\(.*", " \\(.*",
                                   "---.*", " ---.*",
                                   "-.*", " -.*",
                                   ",.*"),
                                 collapse = "|"
                               )), 
    park = stringr::str_replace(string = park,
                                pattern = "@",
                                replacement = "At"), 
    park = stringr::str_replace(string = park,
                                pattern = "Cg",
                                replacement = "Campground"), 
    park = stringr::str_replace(string = park,
                                pattern = "&",
                                replacement = "And"), 
    park = stringr::str_replace(string = park,
                                pattern = paste(c("/", " / "), collapse = "|"),
                                replacement = " "), 
    # 3. match park values for each NF
    park = dplyr::case_when(
      # Angeles NF
      park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      park == "Black Oak " ~ "Black Oak",
      # Inyo NF
      forestname == "Inyo National Forest" & park == "East Fork California" ~ "East Fork Campground – Inyo National Forest",
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
      # San Bernardino NF
      park == "Black Mountain Group" ~ "Black Mountain",
      # Sierra NF
      park == "Badger Flat Group" ~ "Badger Flats Group",
      park == "Texas Flat" ~ "Texas Flats",
      # Sequoia NF
      park == "Camp 3 Campground" ~ "Camp Three Campground",
      park == "French Gulch Group" ~ "French Gulch",
      # Six Rivers NF
      park == "Boise Creek Campground" ~ "Boise Creek",
      forestname == "Six Rivers National Forest" & park == "East Fork Campground" ~ "East Fork Campground – Six Rivers Nf",
      park == "Oak Bottom" ~ "Oak Bottom Campground",
      # Stanislaus NF
      park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
      park == "Pioneer Trails" ~ "Pioneer Trail",
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Cal" ~ "Cal-Ida",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # 4. fill in missing park values (necessary for 2019-2021 data)
    # a. convert to char to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    # b. use lat/long to fill in missing park values
    park = dplyr::case_when(
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Group Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine Campground",
      facilitylatitude == "38.4163889" & facilitylongitude == "-120.105" ~ "Big Meadow",
      TRUE ~ park
    ),
    # c. convert lat/long back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude)
  ) #%>% 
  # filter(is.na(facilityzip) == TRUE) %>%
  # group_by(forestname, park) %>%
  # summarize(count = n())

ridb2020 <- raw_ridb2020 %>%
  # add sept_out to rm `_` to match column names
  janitor::clean_names(sep_out = "") %>% 
  
  ## subset data ##
  # 1. cols of interest
  dplyr::select(
    # historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # 2. filter for just USFS in CA
  # NOTEHD: `case_when()` is for 2018 data
  dplyr::mutate(facilitystate = dplyr::case_when(facilitystate == "CA" ~ "California",
                                                 TRUE ~ facilitystate)) %>% 
  dplyr::filter(agency == "USFS" & facilitystate == "California") %>% 
  # NOTEHD: rm'ing to keep df as small as possible
  dplyr::select(-c(agency,facilitystate)) %>% 
  # create year col; keep only 2018+ obs
  mutate(year = lubridate::year(startdate)) %>% 
  relocate(year, .before = parentlocation) %>% 
  filter(year != 2017) %>% 
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  dplyr::rename(forestname = parentlocation) %>% 
  dplyr::mutate(
    # 2. standardize "National Forest"
    forestname = stringr::str_replace(
      string = forestname,
      pattern = paste(c(
        "NF - FS", "NF -FS", "NF- FS",
        "NF-FS", "-FS", " - FS"
      ),
      collapse = "|"),
      replacement = "National Forest"
    ), 
    # 3. standardize to title case
    forestname = stringr::str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
    forestname = dplyr::case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests", 
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # 5. rm forests NOT in R5
  # `forestname_vec` comes from spatial-processing.R
  dplyr::filter(forestname %in% forestname_vec) %>% 
  ## clean park ##
  dplyr::mutate(
    # 1. standardize case
    park = stringr::str_to_title(park), # 367 tot
    # 2. fix string errors
    park = stringr::str_remove(string = park,
                               pattern = paste(
                                 c("\\(.*", " \\(.*",
                                   "---.*", " ---.*",
                                   "-.*", " -.*",
                                   ",.*"),
                                 collapse = "|"
                               )), 
    park = stringr::str_replace(string = park,
                                pattern = "@",
                                replacement = "At"), 
    park = stringr::str_replace(string = park,
                                pattern = "Cg",
                                replacement = "Campground"), 
    park = stringr::str_replace(string = park,
                                pattern = "&",
                                replacement = "And"), 
    park = stringr::str_replace(string = park,
                                pattern = paste(c("/", " / "), collapse = "|"),
                                replacement = " "), 
    # 3. match park values for each NF
    park = dplyr::case_when(
      # Angeles NF
      park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      park == "Black Oak " ~ "Black Oak",
      # Inyo NF
      forestname == "Inyo National Forest" & park == "East Fork California" ~ "East Fork Campground – Inyo National Forest",
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
      # San Bernardino NF
      park == "Black Mountain Group" ~ "Black Mountain",
      # Sierra NF
      park == "Badger Flat Group" ~ "Badger Flats Group",
      park == "Texas Flat" ~ "Texas Flats",
      # Sequoia NF
      park == "Camp 3 Campground" ~ "Camp Three Campground",
      park == "French Gulch Group" ~ "French Gulch",
      # Six Rivers NF
      park == "Boise Creek Campground" ~ "Boise Creek",
      forestname == "Six Rivers National Forest" & park == "East Fork Campground" ~ "East Fork Campground – Six Rivers Nf",
      park == "Oak Bottom" ~ "Oak Bottom Campground",
      # Stanislaus NF
      park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
      park == "Pioneer Trails" ~ "Pioneer Trail",
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Cal" ~ "Cal-Ida",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # 4. fill in missing park values (necessary for 2019-2021 data)
    # a. convert to char to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    # b. use lat/long to fill in missing park values
    park = dplyr::case_when(
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Group Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine Campground",
      facilitylatitude == "38.4163889" & facilitylongitude == "-120.105" ~ "Big Meadow",
      TRUE ~ park
    ),
    # c. convert lat/long back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude)
  )

ridb2019 <- raw_ridb2019 %>%
  # add sept_out to rm `_` to match column names
  janitor::clean_names(sep_out = "") %>% 
  
  ## subset data ##
  # 1. cols of interest
  dplyr::select(
    # historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # 2. filter for just USFS in CA
  # NOTEHD: `case_when()` is for 2018 data
  dplyr::mutate(facilitystate = dplyr::case_when(facilitystate == "CA" ~ "California",
                                                 TRUE ~ facilitystate)) %>% 
  dplyr::filter(agency == "USFS" & facilitystate == "California") %>% 
  # NOTEHD: rm'ing to keep df as small as possible
  dplyr::select(-c(agency,facilitystate)) %>% 
  # create year col; keep only 2018+ obs
  mutate(year = lubridate::year(startdate)) %>% 
  relocate(year, .before = parentlocation) %>% 
  filter(year != 2017) %>% 
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  dplyr::rename(forestname = parentlocation) %>% 
  dplyr::mutate(
    # 2. standardize "National Forest"
    forestname = stringr::str_replace(
      string = forestname,
      pattern = paste(c(
        "NF - FS", "NF -FS", "NF- FS",
        "NF-FS", "-FS", " - FS"
      ),
      collapse = "|"),
      replacement = "National Forest"
    ), 
    # 3. standardize to title case
    forestname = stringr::str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
    forestname = dplyr::case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests", 
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # 5. rm forests NOT in R5
  # `forestname_vec` comes from spatial-processing.R
  dplyr::filter(forestname %in% forestname_vec) %>% 
  ## clean park ##
  dplyr::mutate(
    # 1. standardize case
    park = stringr::str_to_title(park), # 367 tot
    # 2. fix string errors
    park = stringr::str_remove(string = park,
                               pattern = paste(
                                 c("\\(.*", " \\(.*",
                                   "---.*", " ---.*",
                                   "-.*", " -.*",
                                   ",.*"),
                                 collapse = "|"
                               )), 
    park = stringr::str_replace(string = park,
                                pattern = "@",
                                replacement = "At"), 
    park = stringr::str_replace(string = park,
                                pattern = "Cg",
                                replacement = "Campground"), 
    park = stringr::str_replace(string = park,
                                pattern = "&",
                                replacement = "And"), 
    park = stringr::str_replace(string = park,
                                pattern = paste(c("/", " / "), collapse = "|"),
                                replacement = " "), 
    # 3. match park values for each NF
    park = dplyr::case_when(
      # Angeles NF
      park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      park == "Black Oak " ~ "Black Oak",
      # Inyo NF
      forestname == "Inyo National Forest" & park == "East Fork California" ~ "East Fork Campground – Inyo National Forest",
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
      # San Bernardino NF
      park == "Black Mountain Group" ~ "Black Mountain",
      # Sierra NF
      park == "Badger Flat Group" ~ "Badger Flats Group",
      park == "Texas Flat" ~ "Texas Flats",
      # Sequoia NF
      park == "Camp 3 Campground" ~ "Camp Three Campground",
      park == "French Gulch Group" ~ "French Gulch",
      # Six Rivers NF
      park == "Boise Creek Campground" ~ "Boise Creek",
      forestname == "Six Rivers National Forest" & park == "East Fork Campground" ~ "East Fork Campground – Six Rivers Nf",
      park == "Oak Bottom" ~ "Oak Bottom Campground",
      # Stanislaus NF
      park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
      park == "Pioneer Trails" ~ "Pioneer Trail",
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Cal" ~ "Cal-Ida",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # 4. fill in missing park values (necessary for 2019-2021 data)
    # a. convert to char to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    # b. use lat/long to fill in missing park values
    park = dplyr::case_when(
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Group Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine Campground",
      facilitylatitude == "38.4163889" & facilitylongitude == "-120.105" ~ "Big Meadow",
      TRUE ~ park
    ),
    # c. convert lat/long back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude)
  )

ridb2018 <- raw_ridb2018 %>%
  # add sept_out to rm `_` to match column names
  janitor::clean_names(sep_out = "") %>% 
  
  ## subset data ##
  # 1. cols of interest
  dplyr::select(
    # historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    facilityzip,
    facilitystate,
    facilitylongitude,
    facilitylatitude,
    customerzip,
    totalpaid,
    startdate,
    enddate,
    orderdate,
    numberofpeople
  ) %>% 
  # 2. filter for just USFS in CA
  # NOTEHD: `case_when()` is for 2018 data
  dplyr::mutate(facilitystate = dplyr::case_when(facilitystate == "CA" ~ "California",
                                                 TRUE ~ facilitystate)) %>% 
  dplyr::filter(agency == "USFS" & facilitystate == "California") %>% 
  # NOTEHD: rm'ing to keep df as small as possible
  dplyr::select(-c(agency,facilitystate)) %>%
  # create year col; keep only 2018+ obs
  mutate(year = lubridate::year(startdate)) %>% 
  relocate(year, .before = parentlocation) %>% 
  filter(year != 2017) %>% 
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  dplyr::rename(forestname = parentlocation) %>% 
  dplyr::mutate(
    # 2. standardize "National Forest"
    forestname = stringr::str_replace(
      string = forestname,
      pattern = paste(c(
        "NF - FS", "NF -FS", "NF- FS",
        "NF-FS", "-FS", " - FS"
      ),
      collapse = "|"),
      replacement = "National Forest"
    ), 
    # 3. standardize to title case
    forestname = stringr::str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
    forestname = dplyr::case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests", 
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # 5. rm forests NOT in R5
  # `forestname_vec` comes from spatial-processing.R
  dplyr::filter(forestname %in% forestname_vec) %>% 
  ## clean park ##
  dplyr::mutate(
    # 1. standardize case
    park = stringr::str_to_title(park), # 367 tot
    # 2. fix string errors
    park = stringr::str_remove(string = park,
                               pattern = paste(
                                 c("\\(.*", " \\(.*",
                                   "---.*", " ---.*",
                                   "-.*", " -.*",
                                   ",.*"),
                                 collapse = "|"
                               )), 
    park = stringr::str_replace(string = park,
                                pattern = "@",
                                replacement = "At"), 
    park = stringr::str_replace(string = park,
                                pattern = "Cg",
                                replacement = "Campground"), 
    park = stringr::str_replace(string = park,
                                pattern = "&",
                                replacement = "And"), 
    park = stringr::str_replace(string = park,
                                pattern = paste(c("/", " / "), collapse = "|"),
                                replacement = " "), 
    # 3. match park values for each NF
    park = dplyr::case_when(
      # Angeles NF
      park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      park == "Black Oak " ~ "Black Oak",
      # Inyo NF
      forestname == "Inyo National Forest" & park == "East Fork California" ~ "East Fork Campground – Inyo National Forest",
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Mt. Figuroa Campground" ~ "Mt. Figueroa Campground",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
      # San Bernardino NF
      park == "Black Mountain Group" ~ "Black Mountain",
      # Sierra NF
      park == "Badger Flat Group" ~ "Badger Flats Group",
      park == "Texas Flat" ~ "Texas Flats",
      # Sequoia NF
      park == "Camp 3 Campground" ~ "Camp Three Campground",
      park == "French Gulch Group" ~ "French Gulch",
      # Six Rivers NF
      park == "Boise Creek Campground" ~ "Boise Creek",
      forestname == "Six Rivers National Forest" & park == "East Fork Campground" ~ "East Fork Campground – Six Rivers Nf",
      park == "Oak Bottom" ~ "Oak Bottom Campground",
      # Stanislaus NF
      park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
      park == "Pioneer Trails" ~ "Pioneer Trail",
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Cal" ~ "Cal-Ida",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # 4. fill in missing park values (necessary for 2019-2021 data)
    # a. convert to char to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    # b. use lat/long to fill in missing park values
    park = dplyr::case_when(
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Group Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine Campground",
      facilitylatitude == "38.4163889" & facilitylongitude == "-120.105" ~ "Big Meadow",
      TRUE ~ park
    ),
    # c. convert lat/long back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude)
  )

all_ridb_test <- all_ridb %>% 
  filter(is.na(facilityzip) == TRUE) %>% 
  group_by(forestname, park) %>% 
  summarize(count = n())


park_test <- all_ridb %>% 
  filter(park == "Big Meadow") %>% 
  # filter(facilityzip == "93634") %>% 
  group_by(facilityzip, facilitylongitude, facilitylatitude) %>%
  summarize(count = n())


test <- raw_ridb2021 %>% 
  filter(stringr::str_detect(parentlocation, "Tahoe"))



