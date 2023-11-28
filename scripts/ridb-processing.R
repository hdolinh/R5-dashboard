## TO DO ##
# calculate distance traveled - do we want this as a sep table or part of the df?
# turn into functions:
## add state names + zips (do we want county level data?)
## rbind dfs
## save data
# add functions to targets
# test ridb targets

# testing(setup) ----
usfs_ridb_test2018 <- vroom(here("data/ridb/raw/reservations2018.csv"))
usfs_ridb_test2019 <- vroom(here("data/ridb/raw/reservations2019.csv"))
usfs_ridb_test2020 <- vroom(here("data/ridb/raw/FY20 Historical Reservations Full.csv"))
usfs_ridb_test2021 <- vroom(here("data/ridb/raw/FY21 Historical Reservations Full.csv"))

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

# RIDB Processing ----

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
clean_forestname <- function(forestname_df, customer_zips_df, forestname_vec = forestname_vec){
  
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
clean_facility_location <- function(facility_location_df, park_df){
  
  facility_location_df <- park_df %>% 
    ## clean facility zips & facility lat/lon ##
    dplyr::mutate(
      # necessary for 2020 data
      facilityzip = as.character(facilityzip),
      # extract 5 digit customer zip codes
      facilityzip = stringr::str_extract(string = facilityzip,
                                         pattern = "[:digit:]{5}"),
      
      # NOTEHD: convert facilitylongitude & facilitylatitude to chr to see full coordinate
  
      # chose zip with highest number of visits
      facilityzip = case_when(
        # Angeles NF
        forestname == "Angeles National Forest" & park == "Pyramid Lake Los Alamos Campground" ~ "93243",
        # Cleveland NF
        forestname == "Cleveland National Forest" & park == "El Prado Group" ~ "91948",
        forestname == "Cleveland National Forest" & park == "Falcon Group" ~ "92679",
        forestname == "Cleveland National Forest" & park == "Fry Creek Campground" ~ "92060",
        forestname == "Cleveland National Forest" & park == "Horse Heaven Group" ~ "92060",
        forestname == "Cleveland National Forest" & park == "Laguna" ~ "91948",
        forestname == "Cleveland National Forest" & park == "Observatory Campground" ~ "92060",
        forestname == "Cleveland National Forest" & park == "Wooded Hill Group" ~ "91948",
        # Eldorado NF
        forestname == "Eldorado National Forest" & park == "Black Oak" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Gerle Creek" ~ "95726",
        forestname == "Eldorado National Forest" & park == "Ice House" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Loon Lake" ~ "95726",
        forestname == "Eldorado National Forest" & park == "Loon Lake Chalet" ~ "95726",
        forestname == "Eldorado National Forest" & park == "Pipi Campground" ~ "95684",
        forestname == "Eldorado National Forest" & park == "Ponderosa Cove Campground" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Red Fir" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Silver Lake East" ~ "96120",
        forestname == "Eldorado National Forest" & park == "South Shore Campground" ~ "95666",
        forestname == "Eldorado National Forest" & park == "Stumpy Meadows" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Wench Creek" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Wolf Creek California" ~ "95735",
        forestname == "Eldorado National Forest" & park == "Wrights Lake" ~ "95720",
        forestname == "Eldorado National Forest" & park == "Wrights Lake Equestrian Campground" ~ "95720",
        forestname == "Eldorado National Forest" & park == "Yellowjacket" ~ "95735",
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
        forestname == "Inyo National Forest" & park == "Mcgee Creek" ~ "93546",
        forestname == "Inyo National Forest" & park == "New Shady Rest Campground" ~ "93546",
        forestname == "Inyo National Forest" & park == "Onion Valley" ~ "93526",
        forestname == "Inyo National Forest" & park == "Pumice Flat Group Camp" ~ "93529",
        forestname == "Inyo National Forest" & park == "Reversed Creek Campground" ~ "93529",
        forestname == "Inyo National Forest" & park == "Rock Creek Lake Group Camp" ~ "93514",
        forestname == "Inyo National Forest" & park == "Table Mountain" ~ "93514",
        forestname == "Inyo National Forest" & park == "Trailhead Group" ~ "93514",
        forestname == "Inyo National Forest" & park == "Tuff Campground" ~ "93546",
        forestname == "Inyo National Forest" & park == "Twin Lakes Campground" ~ "93546",
        forestname == "Inyo National Forest" & park == "Upper Sage Flat" ~ "93513",
        forestname == "Inyo National Forest" & park == "Whitney Portal" ~ "93545",
        # Lake Tahoe Basin Mgmt Unit
        forestname == "Lake Tahoe Basin Management Unit" & park == "Desolation Wilderness Permit" ~ "95735",
        # Lassen NF
        forestname == "Lassen National Forest" & park == "Almanor" ~ "96020",
        forestname == "Lassen National Forest" & park == "Gurnsey Creek" ~ "96061",
        forestname == "Lassen National Forest" & park == "Hat Creek" ~ "96071",
        forestname == "Lassen National Forest" & park == "Mccarthy Point Lookout" ~ "95973",
        # Mendocino NF
        forestname == "Mendocino National Forest" & park == "Pine Mountain Lookout" ~ "95485",
        # Plumas NF
        forestname == "Plumas National Forest" & park == "Grasshopper Flat" ~ "96122",
        forestname == "Plumas National Forest" & park == "Grizzly" ~ "95983",
        forestname == "Plumas National Forest" & park == "Hallsted" ~ "95984",
        forestname == "Plumas National Forest" & park == "Hutchins" ~ "95971",
        forestname == "Plumas National Forest" & park == "Lakes Basin" ~ "96103",
        forestname == "Plumas National Forest" & park == "Lightning Tree" ~ "96122",
        forestname == "Plumas National Forest" & park == "Lone Rock" ~ "96114",
        forestname == "Plumas National Forest" & park == "Long Point" ~ "96114",
        forestname == "Plumas National Forest" & park == "Running Deer Campground" ~ "95981",
        forestname == "Plumas National Forest" & park == "Whitehorse Campground" ~ "95956",
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
        forestname == "San Bernardino National Forest" & park == "Oso Group" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Pineknot" ~ "92315",
        forestname == "San Bernardino National Forest" & park == "Serrano" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Skyline" ~ "92305",
        forestname == "San Bernardino National Forest" & park == "Tanglewood Group Camp" ~ "92314",
        forestname == "San Bernardino National Forest" & park == "Wild Horse Equestrian Family" ~ "92305",
        # Sequoia NF
        forestname == "Sequoia National Forest" & park == "Belknap" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Cove Group" ~ "93262",
        forestname == "Sequoia National Forest" & park == "Coy Flat" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Fir Group" ~ "93262",
        forestname == "Sequoia National Forest" & park == "Holey Meadow" ~ "93257",
        forestname == "Sequoia National Forest" & park == "Hospital Flat" ~ "93238",
        forestname == "Sequoia National Forest" & park == "Long Meadow Group" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Paradise Cove" ~ "93240",
        forestname == "Sequoia National Forest" & park == "Pioneer Point" ~ "93285",
        forestname == "Sequoia National Forest" & park == "Poso Guard Station Cabin" ~ "93260",
        forestname == "Sequoia National Forest" & park == "Princess" ~ "93628",
        forestname == "Sequoia National Forest" & park == "Quaking Aspen" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Quaking Aspen Cabin" ~ "93208",
        forestname == "Sequoia National Forest" & park == "Redwood Meadow" ~ "93257",
        forestname == "Sequoia National Forest" & park == "Sandy Flat" ~ "93518",
        forestname == "Sequoia National Forest" & park == "Stony Creek Sequoia" ~ "93262",
        forestname == "Sequoia National Forest" & park == "Tillie Creek" ~ "93285",
        forestname == "Sequoia National Forest" & park == "Upper Stony Creek Campground" ~ "93262",
        forestname == "Sequoia National Forest" & park == "White River" ~ "93260",
        forestname == "Sequoia National Forest" & park == "Wishon" ~ "93265",
        forestname == "Sequoia National Forest" & park == "Wishon Cabin" ~ "93265",
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
        forestname == "Sierra National Forest" & park == "Lower Billy Creek" ~ "93634",
        forestname == "Sierra National Forest" & park == "Lupine" ~ "93669",
        forestname == "Sierra National Forest" & park == "Mono Creek" ~ "93634",
        forestname == "Sierra National Forest" & park == "Mono Hot Springs" ~ "93634",
        forestname == "Sierra National Forest" & park == "Rancheria" ~ "93634",
        forestname == "Sierra National Forest" & park == "Recreation Point" ~ "93604",
        forestname == "Sierra National Forest" & park == "Soquel Campground" ~ "93604",
        forestname == "Sierra National Forest" & park == "Spring Cove" ~ "93644",
        forestname == "Sierra National Forest" & park == "Summerdale Campground" ~ "93623",
        forestname == "Sierra National Forest" & park == "Texas Flats" ~ "93644",
        forestname == "Sierra National Forest" & park == "Upper Billy Creek Campground" ~ "93634",
        forestname == "Sierra National Forest" & park == "Vermillion" ~ "93634",
        # Six Rivers NF
        forestname == "Six Rivers National Forest" & park == "Dillon Creek Campground" ~ "96039",
        forestname == "Six Rivers National Forest" & park == "Fish Lake Campground" ~ "95556",
        # Stanislaus NF
        forestname == "Stanislaus National Forest" & park == "Big Meadow" ~ "95223",
        forestname == "Stanislaus National Forest" & park == "Lodgepole Bear Valley" ~ "95223",
        forestname == "Stanislaus National Forest" & park == "Pioneer Trail" ~ "95364",
        # Tahoe NF
        forestname == "Tahoe National Forest" & park == "East Meadow Campground" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Findley Campground" ~ "96161",
        forestname == "Tahoe National Forest" & park == "Fir Top Campground" ~ "96161",
        forestname == "Tahoe National Forest" & park == "Little Lasier Meadows Campground" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Lower Little Truckee" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Pass Creek Campground" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Upper Little Truckee" ~ "96126",
        forestname == "Tahoe National Forest" & park == "Woodcamp Campground" ~ "96161",
        TRUE ~ facilityzip
      ),
      
      facilitylongitude = case_when(
        # Angeles NF
        park == "Pyramid Lake Los Alamos Campground" & facilityzip == "93243" ~ -118.7666667,
        # Cleveland NF
        park == "El Prado Group" & facilityzip == "91948" ~ -116.4555556,
        park == "Falcon Group" & facilityzip == "92679" ~ -117.4602778,
        park == "Fry Creek Campground" & facilityzip == "92060" ~ -116.88,
        park == "Horse Heaven Group" & facilityzip == "91948" ~ -116.4408333,
        park == "Laguna" & facilityzip == "91948" ~ -116.4463889,
        park == "Observatory Campground" & facilityzip == "92060" ~ -116.8786111,
        park == "Wooded Hill Group" & facilityzip == "91948" ~ -116.42,
        # Eldorado NF
        park == "Black Oak" & facilityzip == "95735" ~ -120.5875,
        park == "Gerle Creek" & facilityzip == "95726" ~ -120.3916667,
        park == "Ice House" & facilityzip == "95735" ~ -120.3588889,
        park == "Loon Lake" & facilityzip == "95726" ~ -120.32283751541158,
        park == "Loon Lake Chalet" & facilityzip == "95726" ~ -120.32781154451243,
        park == "Pipi Campground" & facilityzip == "95684" ~ -120.4375,
        park == "Ponderosa Cove Campground" & facilityzip == "95735" ~ -120.6027778,
        park == "Red Fir" & facilityzip == "95735" ~ -120.3125,
        park == "Silver Lake East" & facilityzip == "96120" ~ -119.8875,
        park == "South Shore Campground" & facilityzip == "95666" ~ -120.2347222,
        park == "Stumpy Meadows" & facilityzip == "95735" ~ -120.5916667,
        park == "Wench Creek" & facilityzip == "95735" ~ -120.3772222,
        park == "Wolf Creek California" & facilityzip == "95735" ~ -120.4,
        park == "Wrights Lake" & facilityzip == "95720" ~ -120.2311111,
        park == "Wrights Lake Equestrian Campground" & facilityzip == "95720" ~ -120.22887212588192,
        park == "Yellowjacket" & facilityzip == "95735" ~ -120.3916667,
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
        park == "Mcgee Creek" & facilityzip == "93546" ~ -118.7847222,
        park == "New Shady Rest Campground" & facilityzip == "93546" ~ -118.9591667,
        park == "Onion Valley" & facilityzip == "93526" ~ -118.3455556,
        park == "Pumice Flat Group Camp" & facilityzip == "93529" ~ -119.0736111,
        park == "Reversed Creek Campground" & facilityzip == "93529" ~ -119.083375,
        park == "Rock Creek Lake Group Camp" & facilityzip == "93514" ~ -118.7390139,
        park == "Table Mountain" & facilityzip == "93514" ~ -118.5683333,
        park == "Trailhead Group" & facilityzip == "93514" ~ -119.2724,
        park == "Tuff Campground" & facilityzip == "93546" ~ -118.6641667,
        park == "Twin Lakes Campground" & facilityzip == "93546" ~ -119.0069444,
        park == "Upper Sage Flat" & facilityzip == "93513" ~ -118.4338889,
        park == "Whitney Portal" & facilityzip == "93545" ~ -118.2297778,
        # Lake Tahoe Basin Mgmt Unit
        park == "Lake Tahoe Basin Management Unit" & facilityzip == "95735" ~ -120.17,
        # Lassen NF
        park == "Almanor" & facilityzip == "96020" ~ -121.167777777778,
        park == "Gurnsey Creek" & facilityzip == "96061" ~ -121.4266667,
        park == "Hat Creek" & facilityzip == "96071" ~ -121.4455556,
        park == "Mccarthy Point Lookout" & facilityzip == "95973" ~ -121.6833333,
        # Mendocino NF
        park == "Pine Mountain Lookout" & facilityzip == "95485" ~ -123.0205556,
        # Plumas NF
        park == "Grasshopper Flat" & facilityzip == "96122" ~ -120.47696586127579,
        park == "Grizzly" & facilityzip == "95983" ~ -120.47188845999149,
        park == "Hallsted" & facilityzip == "95984" ~ -121.0730556,
        park == "Hutchins" & facilityzip == "95971" ~ -121.1991667,
        park == "Lakes Basin" & facilityzip == "96103" ~ -120.62793180744488,
        park == "Lightning Tree" & facilityzip == "96122" ~ -120.48325777154785,
        park == "Lone Rock" & facilityzip == "96114" ~ -120.6172222,
        park == "Long Point" & facilityzip == "96114" ~ -120.5783333,
        park == "Running Deer Campground" & facilityzip == "95981" ~ -120.9530556,
        park == "Whitehorse Campground" & facilityzip == "95956" ~ -121.1411111, 
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
        park == "Oso Group" & facilityzip == "92305" ~ -116.8625,
        park == "Pineknot" & facilityzip == "92315" ~ -116.8830556,
        park == "Serrano" & facilityzip == "92314" ~ -116.9194444,
        park == "Skyline" & facilityzip == "92305" ~ -116.7830556,
        park == "Tanglewood Group Camp"  & facilityzip == "92314" ~ -116.8644444,
        park == "Wild Horse Equestrian Family" & facilityzip == "92305" ~ -116.7672222,
        # Sequoia NF
        park == "Belknap" & facilityzip == "93208" ~ -118.5997222,
        park == "Cove Group" & facilityzip == "93262" ~ -118.8380556,
        park == "Coy Flat" & facilityzip == "93208" ~ -118.6180556,
        park == "Fir Group" & facilityzip == "93262" ~ -118.8416667,
        park == "Holey Meadow" & facilityzip == "93257" ~ -118.6180556,
        park == "Hospital Flat" & facilityzip == "93238" ~ -118.4577778,
        park == "Long Meadow Group" & facilityzip == "93208" ~ -118.5805556,
        park == "Paradise Cove" & facilityzip == "93240" ~ -118.425,
        park == "Pioneer Point" & facilityzip == "93285" ~ -118.48640226657115,
        park == "Poso Guard Station Cabin" & facilityzip == "93260" ~ -118.6433333,
        park == "Princess" & facilityzip == "93628" ~ -118.9369444,
        park == "Quaking Aspen" & facilityzip == "93208" ~ -118.5472222,
        park == "Quaking Aspen Cabin" & facilityzip == "93208" ~ -118.5472222,
        park == "Redwood Meadow" & facilityzip == "93257" ~ -118.5916667,
        park == "Sandy Flat" & facilityzip == "93518" ~ -118.52464621136227,
        park == "Stony Creek Sequoia" & facilityzip == "93262" ~ -118.8316667,
        park == "Tillie Creek" & facilityzip == "93285" ~ -118.4544444,
        park == "Upper Stony Creek Campground" & facilityzip == "93262" ~ -118.8316667,
        park == "Wishon" & facilityzip == "93265" ~ -118.6625,
        park == "Wishon Cabin" & facilityzip == "93265" ~ -118.6625,
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
        park == "Lower Billy Creek" & facilityzip == "93634" ~ -119.2277778,
        park == "Lupine" & facilityzip == "93669" ~ -119.5441667,
        park == "Mono Creek" & facilityzip == "93634" ~ -118.9975,
        park == "Mono Hot Springs" & facilityzip == "93634" ~ -119.0177778,
        park == "Rancheria" & facilityzip == "93634" ~ -119.1611111,
        park == "Recreation Point" & facilityzip == "93604" ~ -119.5777778,
        park == "Soquel Campground" & facilityzip == "93604" ~ -119.5605556,
        park == "Spring Cove" & facilityzip == "93644" ~ -119.54171250982192,
        park == "Summerdale Campground" & facilityzip == "93623" ~ -119.6427333,
        park == "Texas Flats" & facilityzip == "93644" ~ -119.52196876893221,
        park == "Upper Billy Creek Campground" & facilityzip == "93634" ~ -119.2277778,
        park == "Vermillion" & facilityzip == "93634" ~ -119.0097222,
        # Six Rivers NF
        park == "Dillon Creek Campground" & facilityzip == "96039" ~ -123.5430556,
        park == "Fish Lake Campground" & facilityzip == "95556" ~ -123.6844444,
        # Stanislaus NF
        park == "Big Meadow" & facilityzip == "95223" ~ -120.105,
        park == "Lodgepole Bear Valley" & facilityzip == "95223" ~ -120.01369697895106,
        park == "Pioneer Trail" & facilityzip == "95364" ~ -119.9875,
        # Tahoe NF
        park == "East Meadow Campground" & facilityzip == "96126" ~ -120.5325,
        park == "Findley Campground" & facilityzip == "96161" ~ -120.5530556,
        park == "Fir Top Campground" & facilityzip == "96161" ~ -120.5502778,
        park == "Little Lasier Meadows Campground" & facilityzip == "96126" ~ -120.5155556,
        park == "Lower Little Truckee" & facilityzip == "96126" ~ -120.2363889,
        park == "Pass Creek Campground" & facilityzip == "96126" ~ -120.5343778,
        park == "Upper Little Truckee" & facilityzip == "96126" ~ -120.2438889,
        park == "Woodcamp Campground" & facilityzip == "96161" ~ -120.5477778,
        TRUE ~ facilitylongitude
      ),
      facilitylatitude = case_when(
        # Angeles NF
        park == "Pyramid Lake Los Alamos Campground" & facilityzip == "93243" ~ 34.65,
        # Cleveland NF
        park == "El Prado Group" & facilityzip == "91948" ~ 32.8869444,
        park == "Falcon Group" & facilityzip == "92679" ~ 33.6558333,
        park == "Fry Creek Campground" & facilityzip == "92060" ~ 33.345,
        park == "Horse Heaven Group" & facilityzip == "91948" ~ 32.8875,
        park == "Laguna" & facilityzip == "91948" ~ 32.8872222,
        park == "Observatory Campground" & facilityzip == "92060" ~ 33.3416667,
        park == "Wooded Hill Group" & facilityzip == "91948" ~ 32.8502778,
        # Eldorado NF
        park == "Black Oak" & facilityzip == "95735" ~ 38.9041667,
        park == "Gerle Creek" & facilityzip == "95726" ~ 38.975,
        park == "Ice House" & facilityzip == "95735" ~ 38.8233333,
        park == "Loon Lake" & facilityzip == "95726" ~ 38.979244612802034,
        park == "Loon Lake Chalet" & facilityzip == "95726" ~ 38.985269184778055,
        park == "Pipi Campground" & facilityzip == "95684" ~ 38.5736111,
        park == "Ponderosa Cove Campground" & facilityzip == "95735" ~ 38.8741667,
        park == "Red Fir" & facilityzip == "95735" ~ 39.0022222,
        park == "Silver Lake East" & facilityzip == "96120" ~ 38.675,
        park == "South Shore Campground" & facilityzip == "95666" ~ 38.5330556,
        park == "Stumpy Meadows" & facilityzip == "95735" ~ 38.9041667,
        park == "Wench Creek" & facilityzip == "95735" ~ 38.8902778,
        park == "Wolf Creek California" & facilityzip == "95735" ~ 38.8830556,
        park == "Wrights Lake" & facilityzip == "95720" ~ 38.8494444,
        park == "Wrights Lake Equestrian Campground" & facilityzip == "95720" ~ 38.84575599173599,
        park == "Yellowjacket" & facilityzip == "95735" ~ 38.8916667,
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
        park == "Mcgee Creek" & facilityzip == "93546" ~ 37.5644444,
        park == "New Shady Rest Campground" & facilityzip == "93546" ~ 37.65,
        park == "Onion Valley" & facilityzip == "93526" ~ 36.7747222,
        park == "Pumice Flat Group Camp" & facilityzip == "93529" ~ 37.6488889,
        park == "Reversed Creek Campground" & facilityzip == "93529" ~ 37.771241,
        park == "Rock Creek Lake Group Camp" & facilityzip == "93514" ~ 37.4538222,
        park == "Table Mountain" & facilityzip == "93514" ~ 37.2080556,
        park == "Trailhead Group" & facilityzip == "93514" ~ 37.9642,
        park == "Tuff Campground" & facilityzip == "93546" ~ 37.5625,
        park == "Twin Lakes Campground" & facilityzip == "93546" ~ 37.6158333,
        park == "Upper Sage Flat" & facilityzip == "93513" ~ 37.1258333,
        park == "Whitney Portal" & facilityzip == "93545" ~ 36.5898611,
        # Lake Tahoe Basin Mgmt Unit
        park == "Lake Tahoe Basin Management Unit" & facilityzip == "95735" ~ 38.9197222,
        # Lassen NF
        park == "Almanor" & facilityzip == "96020" ~ 40.2169444444444,
        park == "Gurnsey Creek" & facilityzip == "96061" ~ 40.3088889,
        park == "Hat Creek" & facilityzip == "96071" ~ 40.6677778,
        park == "Mccarthy Point Lookout" & facilityzip == "95973" ~ 40.1877778,
        # Mendocino NF
        park == "Pine Mountain Lookout" & facilityzip == "95485" ~ 39.3669444,
        # Plumas NF
        park == "Grasshopper Flat" & facilityzip == "96122" ~ 39.89070374599337,
        park == "Grizzly" & facilityzip == "95983" ~ 39.887370353199195,
        park == "Hallsted" & facilityzip == "95984" ~ 40.0175,
        park == "Hutchins" & facilityzip == "95971" ~ 39.8830556,
        park == "Lakes Basin" & facilityzip == "96103" ~ 39.712369911694275,
        park == "Lightning Tree" & facilityzip == "96122" ~ 39.934727498748224,
        park == "Lone Rock" & facilityzip == "96114" ~ 40.1952778,
        park == "Long Point" & facilityzip == "96114" ~ 40.1783333,
        park == "Running Deer Campground" & facilityzip == "95981" ~ 39.7352778,
        park == "Whitehorse Campground" & facilityzip == "95956" ~ 39.8880556,
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
        park == "Oso Group" & facilityzip == "92305" ~ 34.1755556,
        park == "Pineknot" & facilityzip == "92315" ~ 34.2352778,
        park == "Serrano" & facilityzip == "92314" ~ 34.2613889,
        park == "Skyline" & facilityzip == "92305" ~ 34.1555556,
        park == "Tanglewood Group Camp"  & facilityzip == "92314" ~ 34.2922222,
        park == "Wild Horse Equestrian Family" & facilityzip == "92305" ~ 34.2019444,
        # Sequoia NF
        park == "Belknap" & facilityzip == "93208" ~ 36.1416667,
        park == "Cove Group" & facilityzip == "93262" ~ 36.665,
        park == "Coy Flat" & facilityzip == "93208" ~ 36.1291667,
        park == "Fir Group" & facilityzip == "93262" ~ 36.6641667,
        park == "Holey Meadow" & facilityzip == "93257" ~ 35.9541667,
        park == "Hospital Flat" & facilityzip == "93238" ~ 35.8286111,
        park == "Long Meadow Group" & facilityzip == "93208" ~ 35.9791667,
        park == "Paradise Cove" & facilityzip == "93240" ~ 35.6491667,
        park == "Pioneer Point" & facilityzip == "93285" ~ 35.65053527642188,
        park == "Poso Guard Station Cabin" & facilityzip == "93260" ~ 35.8075,
        park == "Princess" & facilityzip == "93628" ~ 36.8027778,
        park == "Quaking Aspen" & facilityzip == "93208" ~ 36.1208333,
        park == "Quaking Aspen Cabin" & facilityzip == "93208" ~ 36.1208333,
        park == "Redwood Meadow" & facilityzip == "93257" ~ 35.9777778,
        park == "Sandy Flat" & facilityzip == "93518" ~ 35.58215941873024,
        park == "Stony Creek Sequoia" & facilityzip == "93262" ~ 36.6647222,
        park == "Tillie Creek" & facilityzip == "93285" ~ 35.7013889,
        park == "Upper Stony Creek Campground" & facilityzip == "93262" ~ 36.6647222,
        park == "Wishon" & facilityzip == "93265" ~ 36.1875,
        park == "Wishon Cabin" & facilityzip == "93265" ~ 36.1875,
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
        park == "Lower Billy Creek" & facilityzip == "93634" ~ 37.2380556,
        park == "Lupine" & facilityzip == "93669" ~ 37.3077778,
        park == "Mono Creek" & facilityzip == "93634" ~ 37.3586111,
        park == "Mono Hot Springs" & facilityzip == "93634" ~ 37.3263889,
        park == "Rancheria" & facilityzip == "93634" ~ 37.2477778,
        park == "Recreation Point" & facilityzip == "93604" ~ 37.3286111,
        park == "Soquel Campground" & facilityzip == "93604" ~ 37.405,
        park == "Spring Cove" & facilityzip == "93644" ~ 37.30172738982204,
        park == "Summerdale Campground" & facilityzip == "93623" ~ 37.4706472,
        park == "Texas Flats" & facilityzip == "93644" ~ 37.43244295679876,
        park == "Upper Billy Creek Campground" & facilityzip == "93634" ~ 37.2380556,
        park == "Vermillion" & facilityzip == "93634" ~ 37.3791667,
        # Six Rivers NF
        park == "Dillon Creek Campground" & facilityzip == "96039" ~ 41.5733333,
        park == "Fish Lake Campground" & facilityzip == "95556" ~ 41.2641667,
        # Stanislaus NF
        park == "Big Meadow" & facilityzip == "95223" ~ 38.4163889,
        park == "Lodgepole Bear Valley" & facilityzip == "95223" ~ 38.47903511408578,
        park == "Pioneer Trail" & facilityzip == "95364" ~ 38.1875,
        # Tahoe NF
        park == "East Meadow Campground" & facilityzip == "96126" ~ 39.5008333,
        park == "Findley Campground" & facilityzip == "96161" ~ 39.4847222,
        park == "Fir Top Campground" & facilityzip == "96161" ~ 39.4855556,
        park == "Little Lasier Meadows Campground" & facilityzip == "96126" ~ 39.4905556,
        park == "Lower Little Truckee" & facilityzip == "96126" ~ 39.4855556,
        park == "Pass Creek Campground" & facilityzip == "96126" ~ 39.5040667,
        park == "Upper Little Truckee" & facilityzip == "96126" ~ 39.4908333,
        park == "Woodcamp Campground" & facilityzip == "96161" ~ 39.4855556,
        TRUE ~ facilitylatitude
      )
      # convert back to numeric
      # facilitylongitude = as.numeric(facilitylongitude),
      # facilitylatitude = as.numeric(facilitylatitude),
    )
  
  return(facility_location_df)
}

# 7. calc_vars() ----
# NOTEHD: need to calculate length of stay BEFORE cleaning site type
# site type cleaning relies on length of stay var
# NOTEHD: still have negative booking windows...
calc_vars <- function(calc_vars_df, facility_location_df){
  
  calc_vars_df <- facility_location_df %>% 
    ## calculate new variables ##
    dplyr::mutate(
      # 1. length of stay
      lengthofstay = as.numeric(difftime(enddate, startdate), units = "days"),
      # 1a. change neg length of stay to positive;
      # NOTEHD: previous conversations w/ R1S confirm neg start date is data error
      lengthofstay = dplyr::if_else(
        lengthofstay < 0,
        true = as.numeric(difftime(startdate, enddate), units = "days"),
        false = lengthofstay
      ),
      
      # 2. booking window
      bookingwindow = round(as.numeric(difftime(startdate, orderdate), units = "days"), 0),
      # 2a. change neg booking window to positive; see note in 1a
      bookingwindow = dplyr::if_else(
        bookingwindow == -1,
        true = 0,
        false = bookingwindow
      ),
      bookingwindow = dplyr::if_else(
        bookingwindow < -1,
        true = round(as.numeric(difftime(orderdate, startdate), units = "days"), 0),
        false = bookingwindow
      ),
      
      # 3. daily cost
      dailycost = round(totalpaid / lengthofstay, 2),
      
      # 4. daily cost per visitor
      dailycostpervisitor = round(dailycost / numberofpeople, 2),
    )
  
  return(calc_vars_df)
}

# 8. clean_sitetype() ----
clean_sitetype <- function(sitetype_df, calc_vars_df){
  
  sitetype_df <- calc_vars_df %>% 
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

usfs_ridb2018_dist <- zipcodeR::zip_distance(usfs_ridb2018$facilityzip, usfs_ridb2018$customerzip) %>% 
  rename(facilityzip = zipcode_a) %>% 
  rename(customerzip = zipcode_b) %>% 
  rename(distance_mi = distance)

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

# testing(functions) ----
# 2018 #
usfs_ridb2018 <- subset_ridb(raw_df = usfs_ridb_test2018)
usfs_ridb2018 <- clean_customer_zips(subset_df = usfs_ridb2018)
usfs_ridb2018 <- clean_forestname(customer_zips_df = usfs_ridb2018,
                                  forestname_vec = forestname_vec)
usfs_ridb2018 <- clean_park(forestname_df = usfs_ridb2018)
usfs_ridb2018 <- clean_facility_location(park_df = usfs_ridb2018)
usfs_ridb2018 <- calc_vars(facility_location_df = usfs_ridb2018)
usfs_ridb2018 <- clean_sitetype(calc_vars_df = usfs_ridb2018)

# 2019 #
usfs_ridb2019 <- subset_ridb(raw_df = usfs_ridb_test2019)
usfs_ridb2019 <- clean_customer_zips(subset_df = usfs_ridb2019)
usfs_ridb2019 <- clean_forestname(customer_zips_df = usfs_ridb2019,
                                  forestname_vec = forestname_vec)
usfs_ridb2019 <- clean_park(forestname_df = usfs_ridb2019)
usfs_ridb2019 <- clean_facility_location(park_df = usfs_ridb2019)
usfs_ridb2019 <- calc_vars(facility_location_df = usfs_ridb2019)
usfs_ridb2019 <- clean_sitetype(calc_vars_df = usfs_ridb2019)

# 2020 #
usfs_ridb2020 <- subset_ridb(raw_df = usfs_ridb_test2020)
usfs_ridb2020 <- clean_customer_zips(subset_df = usfs_ridb2020)
usfs_ridb2020 <- clean_forestname(customer_zips_df = usfs_ridb2020,
                                  forestname_vec = forestname_vec)
usfs_ridb2020 <- clean_park(forestname_df = usfs_ridb2020)
usfs_ridb2020 <- clean_facility_location(park_df = usfs_ridb2020)
usfs_ridb2020 <- calc_vars(facility_location_df = usfs_ridb2020)
usfs_ridb2020 <- clean_sitetype(calc_vars_df = usfs_ridb2020)

# 2021 #
usfs_ridb2021 <- subset_ridb(raw_df = usfs_ridb_test2021)
usfs_ridb2021 <- clean_customer_zips(subset_df = usfs_ridb2021)
usfs_ridb2021 <- clean_forestname(customer_zips_df = usfs_ridb2021,
                                  forestname_vec = forestname_vec)
usfs_ridb2021 <- clean_park(forestname_df = usfs_ridb2021)
usfs_ridb2021 <- clean_facility_location(park_df = usfs_ridb2021)
usfs_ridb2021 <- calc_vars(facility_location_df = usfs_ridb2021)
usfs_ridb2021 <- clean_sitetype(calc_vars_df = usfs_ridb2021)

write_csv(usfs_ridb2021, here("data/ridb/clean/usfs_ridb2021_no_dist_travel.csv"))

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

# 1,788,339
all_ridb <- rbind(ridb2018, # 296,725
                  ridb2019, # 363,244
                  ridb2020, # 520,590
                  ridb2021) # 607,780

all_ridb_2019up <- rbind(ridb2019, # 363,244
                         ridb2020, # 520,590
                         ridb2021) # 607,780

park_test <- all_ridb %>%
  # filter(facilityzip == "93634") %>% 
  group_by(forestname, park, facilityzip, facilitylongitude, facilitylatitude) %>%
  summarize(count = n())

write_csv(all_ridb_2019up,)

usfs_ridb <- read_csv(here::here("data/ridb/clean/usfs_ridb_all.csv"))




