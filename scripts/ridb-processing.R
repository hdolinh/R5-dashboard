# to do ----
# aggregate sitetype
# calculate distance traveled
# add customer state

# setup ----
library(vroom)
library(here)
library(tidyverse)
library(janitor)

# read in data ----
# 2018 ridb data
fp_ridb2018 <- here("data/ridb/raw/reservations2018.csv")
raw_ridb2018 <- vroom(fp_ridb2018)

# 2019 ridb data
fp_ridb2019 <- here("data/ridb/raw/reservations2019.csv")
raw_ridb2019 <- vroom(fp_ridb2019)

# 2020 ridb data
fp_ridb2020 <- here("data/ridb/raw/FY20 Historical Reservations Full.csv")
raw_ridb2020 <- vroom(fp_ridb2020)

# 2020 ridb data
fp_ridb2021 <- here("data/ridb/raw/FY21 Historical Reservations Full.csv")
raw_ridb2021 <- vroom(fp_ridb2021)

# 2018 clean and subset ridb ----
usfs_ridb2018 <- raw_ridb2018 %>% 
  # add sept_out to rm `_` to match other column names
  janitor::clean_names(sep_out = "") %>% 
  # cols of interest
  select(
    historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    usetype,
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
  # extract 5 digit zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # rm invalid zip codes
  filter(!customerzip %in% c("00000", "99999")) %>% 
  # filter for just USFS in CA, OR, and WA
  filter(agency == "USFS" & facilitystate %in% c("CA", "OR", "WA")) %>% 
  # rename parent_location to forestname
  rename(forestname = parentlocation) %>% 
  # clean forestname #
  mutate(
    # standardize "National Forest"
    forestname = str_replace(string = forestname,
                             pattern = paste(c(
                               "NF - FS", "NF -FS", "NF- FS",
                               "NF-FS", "-FS", " - FS"),
                               collapse = "|"),
                             replacement = "National Forest"),
    # standardize to title case
    forestname = str_to_title(forestname),
    # fix forestname typos based on spatial data
    forestname = case_when(
      forestname == "Columbia R Gorge Nsanational Forest" ~ "Columbia River Gorge National Scenic Area",
      forestname == "Fremont Winema National Forest" ~ "Fremont-Winema National Forest",
      forestname == "Lake Tahoe Basin National Forest" ~ "Lake Tahoe Basin Management Unit",
      forestname == "Mt. Baker-Snoqu National Forest" ~ "Mt. Baker-Snoqualmie National Forest",
      forestname == "Okanogan - Wenatchee National Forest" ~ "Okanogan-Wenatchee National Forest",
      forestname == "Rogue River - Siskiyou National Forest" ~ "Rogue River-Siskiyou National Forests",
      # keep forestname if none of the previous conditions are TRUE
      TRUE ~ forestname)
  ) %>% 
  # rm forests not in R5; forestname var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  # clean park #
  mutate(
    park = str_to_title(park), # 367 tot
    # string errors
    park = str_remove(string = park,
                      pattern = paste(c("\\(.*", " \\(.*",
                                        "---.*", " ---.*",
                                        ",.*"), collapse = "|")),
    park = str_replace(string = park,
                       pattern = "@",
                       replacement = "At"),
    park = str_replace(string = park,
                       pattern = "Cg",
                       replacement = "Campground"),
    park = str_replace(string = park,
                       pattern = paste(c("/", " / "), collapse = "|"),
                       replacement = " "),
    # match park with 2019+ data
    park = case_when(
      # Angeles NF
      park == "Pyramid Lake / Los Alamos Campground" ~ "Pyramid Lake - Los Alamos Campground",
      park == "Table Mountain - Angeles Nf" ~ "Table Mountain",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      park == "Silver Lake East" ~ "Silver Lake East- Eldorado",
      park == "South Fork Group" ~ "South Fork Group (Ca)",
      # Inyo NF
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood\nThousand Trails Management Services, Inc" ~ "Cottonwood",
      # San Bernardino NF
      park == "Black Mountain Group" ~ "Black Mountain",
      # Sequoia NF
      park == "Camp 3 Campground" ~ "Camp Three Campground",
      park == "French Gulch Group" ~ "French Gulch",
      # Six Rivers NF
      park == "Boise Creek Campground" ~ "Boise Creek",
      park == "Oak Bottom" ~ "Oak Bottom Campground",
      # Stanislaus NF
      park == "Lodgepole/Bear Valley" ~ "Lodgepole Group",
      park == "Pioneer Trails" ~ "Pioneer Trail",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # calculate new variables
    lengthofstay = as.numeric(difftime(enddate, startdate), units = "days"), 
    bookingwindow = round(as.numeric(difftime(startdate, orderdate), units = "days"), 0),
    dailycost = round(totalpaid / lengthofstay, 2),
    dailycostpervisitor = round(dailycost / numberofpeople, 2),
    # convert sitetype to title case
    sitetype = str_to_title(sitetype)
  )

test2018 <- usfs_ridb2018 %>% 
  filter(sitetype == "Yurt") %>% 
  count(park)

  
# 2019-2021 clean and subset ridb ----
usfs_ridb <- raw_ridb2021 %>% 
  # match colnames
  janitor::clean_names(sep_out = "") %>% 
  # cols of interest
  select(
    historicalreservationid,
    agency,
    parentlocation,
    park,
    sitetype,
    usetype,
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
  # extract 5 digit zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # rm invalid zip codes
  filter(!customerzip %in% c("00000", "99999")) %>% 
  # filter for just USFS in CA, OR, and WA
  filter(agency == "USFS" & facilitystate %in% c("California",
                                                 "Oregon",
                                                 "Washington")) %>% 
  # rename parent_location to forestname
  rename(forestname = parentlocation) %>% 
  # clean forestname
  mutate(
    forestname = case_when(forestname == "SHASTA-TRINITY NATIONAL FOREST" ~ "Shasta-Trinity National Forest",
                           TRUE ~ forestname)
  ) %>%
  # rm forests not in R5; forestname_R5 var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  # clean park #
  mutate(
    park = str_to_title(park), # 381 tot (2019) 385 tot (2020) 402 tot (2021)
    # string errors
    park = str_remove(string = park,
                      pattern = paste(c("\\(.*", " \\(.*", 
                                        "-.*", " -.*"), 
                                      collapse = "|")),
    park = str_replace(string = park,
                       pattern = "Cg",
                       replacement = "Campground"),
    park = str_replace(string = park,
                       pattern = "@",
                       replacement = "At"),
    park = str_replace(string = park,
                       pattern = "&",
                       replacement = "And"),
    park = str_replace(string = park,
                       pattern = paste(c("/", " / "), collapse = "|"),
                       replacement = " "),
    # match park with 2018 data
    park = case_when(
      # Stanislaus NF
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # convert to character to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    park = case_when(
      # fill in missing data based on lat/long
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Overflow Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine West Shore Campground",
      TRUE ~ park
    ),
    # convert back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude),
    # calculate new variables #
    # length of stay
    lengthofstay = as.numeric(difftime(enddate, startdate), units = "days"),
    # change neg length of stay to positive;
    # NOTEHD: previous conversations w/ R1S confirm neg start date is data error
    lengthofstay = if_else(lengthofstay < 0,
                           true = as.numeric(difftime(startdate, enddate), units = "days"),
                           false = lengthofstay),
    # booking window
    bookingwindow = round(as.numeric(difftime(startdate, orderdate), units = "days"), 0),
    # daily cost
    dailycost = round(totalpaid / lengthofstay, 2),
    # daily cost per visitor
    dailycostpervisitor = round(dailycost / numberofpeople, 2),
    # convert sitetype to title case
    sitetype = str_to_title(sitetype),
    # redefine "Management" sitetype for 2019-2021
    sitetype = case_when(
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
                                             "Whitney Portal") ~ "RV or Tent Only",
      sitetype == "Management" & park %in% c("Aspen Grove Campground",
                                             "Faucherie",
                                             "Mono Creek") ~ "Tent Only",
      sitetype == "Management" & park == "Grouse Valley" ~ "Shelter",
      TRUE ~ sitetype
    ),
    # aggregate sitetype
    sitetype = case_when(
      # day use
      lengthofstay == 0 |
        sitetype == "Entry Point" & park == "Cedar Creek Falls" ~ "Day Use",
      # remote
      sitetype %in% c("Group Walk To", 
                      "Walk To", 
                      "Destination Zone") |
        sitetype == "Trailhead" & lengthofstay > 0 |
        sitetype %in% c("Trailhead", 
                        "Entry Point") & park %in% c("Sierra National Forest Wilderness Permits",
                                                     "Inyo National Forest",
                                                     "Mt. Whitney") ~ "Remote",
      # shelter
      sitetype %in% c("Cabin Electric",
                      "Cabin Nonelectric",
                      "Group Shelter Nonelectric",
                      "Shelter Nonelectric",
                      "Yurt") ~ "Shelter",
      # water
      sitetype == "Boat In" |
        sitetype == "Entry Point" & park == "Tuolumne River Permits" ~ "Water",
      # equestrian
      sitetype %in% c("Equestrian Nonelectric", 
                      "Group Equestrian") ~ "Equestrian",
      # rv only
      sitetype %in% c("Group Rv Area Nonelectric", 
                      "Rv Electric",
                      "Rv Nonelectric") ~ "RV Only",
      # tent only
      # sitetype %in% c("Group Tent Only Area Nonelectric",
      #                 "Tent Only Electric",
      #                 "Tent Only Nonelectric") |
      #   sitetype == "Entry Point" & lengthofstay > 0 ~ "Tent Only",
      # rv or tent
      sitetype %in% c("Group Standard Area Nonelectric",
                      "Group Standard Electric",
                      "Group Standard Nonelectric",
                      "RV or Tent Only",
                      "Standard Nonelectric",
                      "Standard Electric") ~ "RV or Tent",
      TRUE ~ sitetype
    )
  )

test <- usfs_ridb %>% 
  filter(sitetype == "Entry Point")

# notes ----
## adding 'Yurt' to park leave it alone for now... ##
# note that all parks with 'Shelter' sitetype that doesn't say cabin in
# park name means it was most likely a 'Yurt' sitetype originally
remote_test <- usfs_ridb %>% 
  filter(sitetype == "Yurt") %>% 
  mutate(
    park = if_else(str_detect(sitetype, "Yurt") == TRUE,
                   true = paste0(park, " Yurt"),
                   false = park,
                   missing = park),
  ) %>% 
  count(park)

remote_test2018 <- usfs_ridb2018 %>% 
  filter(sitetype == "Yurt") %>% 
  group_by(park, sitetype) %>% 
  summarize(n = n()) %>% 
  mutate(
    # adding 'Yurt' to park
    park = if_else(str_detect(sitetype, "Yurt") == TRUE,
                   true = str_replace(park, park, paste0(park, " Yurt")),
                   false = park,
                   missing = park),
  )

## neg length of stay ##
# 2018 tot 0
# 2019 tot 424
# 2020 tot 202
# 2021 tot 279
lengthofstay_test <- usfs_ridb %>%
  filter(lengthofstay < 0)

## customer zip code NA ##
# ~1.8% of 2018 data (tot 306,518)
zipcode2018_test <- usfs_ridb2018 %>% 
  filter(is.na(customerzip) == TRUE)# %>% 
  # group_by(forestname) %>% 
  # summarize(n = n())

# ~37% of 2019 data (tot 363,714)
zipcode2019_test <- usfs_ridb %>% 
  filter(is.na(customerzip) == TRUE)

# ~26% of 2020 data (tot 520,568)
zipcode2020_test <- usfs_ridb %>% 
  filter(is.na(customerzip) == TRUE)# %>% 
  # group_by(forestname) %>% 
  # summarize(n = n())

# ~23% of 2021 data (tot 607,766)
zipcode2021_test <- usfs_ridb %>% 
  filter(is.na(customerzip) == TRUE)

