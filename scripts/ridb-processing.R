# to do ----
# calculate distance traveled
# add customer state

# setup ----
library(vroom)
library(here)
library(tidyverse)
library(janitor)
library(zipcodeR)

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

# combined processing ----
usfs_ridb <- raw_ridb2021 %>% 
  # add sept_out to rm `_` to match column names
  janitor::clean_names(sep_out = "") %>% 
  
  ## subset data ##
  # 1. cols of interest
  select(
    historicalreservationid,
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
  mutate(facilitystate = case_when(facilitystate == "CA" ~ "California",
                                   TRUE ~ facilitystate)) %>% 
  filter(agency == "USFS" & facilitystate == "California") %>% 
  
  ## clean customer zip codes ##
  # 1. extract 5 digit customer zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # 2. rm invalid customer zip codes
  filter(!customerzip %in% c("00000", "99999")) %>%
  # 3. rm NA customer zip codes
  drop_na(customerzip) %>% 
  
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  rename(forestname = parentlocation) %>% 
  mutate(
    # 2. standardize "National Forest"
    forestname = str_replace(string = forestname,
                             pattern = paste(c(
                               "NF - FS", "NF -FS", "NF- FS",
                               "NF-FS", "-FS", " - FS"),
                               collapse = "|"),
                             replacement = "National Forest"),
    # 3. standardize to title case
    forestname = str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
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
  # 5. rm forests NOT in R5
  # NOTEHD: forestname var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  
  ## clean park ##
  mutate(
    # 1. standardize case
    park = str_to_title(park), # 367 tot
    # 2. fix string errors
    park = str_remove(string = park,
                      pattern = paste(c("\\(.*", " \\(.*",
                                        "---.*", " ---.*",
                                        "-.*", " -.*",
                                        ",.*"), collapse = "|")),
    park = str_replace(string = park,
                       pattern = "@",
                       replacement = "At"),
    park = str_replace(string = park,
                       pattern = "Cg",
                       replacement = "Campground"),
    park = str_replace(string = park,
                       pattern = "&",
                       replacement = "And"),
    park = str_replace(string = park,
                       pattern = paste(c("/", " / "), collapse = "|"),
                       replacement = " "),
    # 3. match park values for each NF
    park = case_when(
      # Angeles NF
      park == "Pyramid Lake" ~ "Pyramid Lake Los Alamos Campground",
      # Eldorado NF
      park == "Bear Group Campground" ~ "Bear River Group Campground",
      # Inyo NF
      park == "Silver Lake Campground" ~ "Silver Lake Campground June Lake",
      park == "Table Mountain Inyo" ~ "Table Mountain",
      # Los Padres NF
      park == "Arroyo Seco Campground" ~ "Arroyo Seco",
      park == "Los Prietos Campground" ~ "Los Prietos",
      # Plumas NF
      park == "Cottonwood Thousand Trails Management Services, Inc" ~ "Cottonwood",
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
      park == "Big Meadow Stanislaus Natl Fs" ~ "Big Meadow",
      park == "Lake Alpine - Lodgepole Group" ~ "Lodgepole Group",
      # Tahoe NF
      park == "Boca Rest" ~ "Boca Rest Campground",
      park == "Calpine Lookout Cabin" ~ "Calpine Lookout",
      park == "Sierra Campground" ~ "Sierra",
      park == "Tunnel Mills Il" ~ "Tunnel Mills Group",
      park == "Tunnel Mills Ii" ~ "Tunnel Mills Group",
      TRUE ~ park
    ),
    # 4. fill in missing park values 
    # a. convert to char to reveal full coordinate
    facilitylongitude = as.character(facilitylongitude),
    facilitylatitude = as.character(facilitylatitude),
    # b. use lat/long to fill in missing park values
    park = case_when(
      facilitylatitude == "38.477308" & facilitylongitude == "-120.024175" ~ "Lodgepole Overflow Campground",
      facilitylatitude == "38.4811" & facilitylongitude == "-120.017" ~ "Silvertip Campground",
      facilitylatitude == "38.480752" & facilitylongitude == "-119.988643" ~ "Pine Marten Campground", # 2021 data
      facilitylatitude == "38.4815" & facilitylongitude == "-119.989" ~ "Pine Marten Campground", # 2019 data
      facilitylatitude == "38.4802" & facilitylongitude == "-119.985" ~ "Silver Valley Campground",
      facilitylatitude == "38.477333" & facilitylongitude == "-120.008045" ~ "Lake Alpine West Shore Campground",
      TRUE ~ park
    ),
    # c. convert lat/long back to numeric
    facilitylongitude = as.numeric(facilitylongitude),
    facilitylatitude = as.numeric(facilitylatitude)
    ) %>% 
  
  ## calculate new variables ##
  mutate(
    # 1. length of stay
    lengthofstay = as.numeric(difftime(enddate, startdate), units = "days"), 
    # a. change neg length of stay to positive;
    # NOTEHD: previous conversations w/ R1S confirm neg start date is data error
    lengthofstay = if_else(lengthofstay < 0,
                           true = as.numeric(difftime(startdate, enddate), units = "days"),
                           false = lengthofstay),
    
    # 2. booking window
    bookingwindow = round(as.numeric(difftime(startdate, orderdate), units = "days"), 0),
    
    # 3. daily cost
    dailycost = round(totalpaid / lengthofstay, 2),
    
    # 4. daily cost per visitor
    dailycostpervisitor = round(dailycost / numberofpeople, 2),
  ) %>% 
  
  ## clean sitetype ##
  mutate(
    # 1. standardize to title case
    sitetype = str_to_title(sitetype),
    # 2. redefine "Management" sitetype for 2019-2021
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
                                             "Whitney Portal") ~ "RV or Tent",
      sitetype == "Management" & park %in% c("Aspen Grove Campground",
                                             "Faucherie",
                                             "Mono Creek") ~ "Tent Only",
      sitetype == "Management" & park == "Grouse Valley" ~ "Shelter",
      TRUE ~ sitetype
    ),
    # 3. aggregate sitetype
    sitetype = case_when(
      # a. day use
      lengthofstay == 0 |
        sitetype == "Entry Point" & park == "Cedar Creek Falls" ~ "Day Use",
      # b. remote
      sitetype %in% c("Group Walk To", 
                      "Walk To", 
                      "Destination Zone") |
        sitetype == "Trailhead" & lengthofstay > 0 |
        sitetype %in% c("Trailhead", 
                        "Entry Point") & park %in% c("Sierra National Forest Wilderness Permits",
                                                     "Inyo National Forest",
                                                     "Mt. Whitney",
                                                     "Desolation Wilderness Permit") ~ "Remote",
      # c. shelter
      sitetype %in% c("Cabin Electric",
                      "Cabin Nonelectric",
                      "Group Shelter Nonelectric",
                      "Shelter Nonelectric",
                      "Yurt") ~ "Shelter",
      # d. water
      sitetype == "Boat In" |
        sitetype == "Entry Point" & park == "Tuolumne River Permits" ~ "Water",
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
        sitetype == "Entry Point" & lengthofstay > 0 ~ "Tent Only",
      # h. rv or tent
      sitetype %in% c("Group Standard Area Nonelectric",
                      "Group Standard Electric",
                      "Group Standard Nonelectric",
                      "Standard Nonelectric",
                      "Standard Electric") ~ "RV or Tent",
      TRUE ~ sitetype
    )
  )


# add state for each ZIP code ----
# create df of fips and full state names
fips_list <- c("01", "02", "04", "05", "06", "08", "09", "10", "11", "12", 
               "13", "15", "16", "17", "18", "19", "20", "21", "22", "23", 
               "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", 
               "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", 
               "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", 
               "56", "72")

state_list <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL",
                    "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
                    "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
                    "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
                    "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                    "WY", "PR")

states_full_names_list <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                            "Colorado", "Connecticut", "Delaware", "District of Columbia",
                            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
                            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
                            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
                            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
                            "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
                            "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
                            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
                            "Washington", "West Virginia", "Wisconsin", "Wyoming", "Puerto Rico")

df_states_fips <- as.data.frame(list(fips = fips_list,
                                     state = state_list,
                                     state_full = states_names_list))


# loop through state df to get all ZIP codes w/in state
df_states_zip_codes <- data.frame()

for (i in seq_along(fips_list)){
  state <- zipcodeR::search_fips(state_fips = fips_list[[i]]) %>% 
    select(zipcode, state)
  
  df_states_zip_codes <- rbind(df_states_zip_codes, state)
}

# add full state name and fips code to list of all ZIP codes for each state
df_states_fips_zip_codes <- 
  left_join(x = df_states_zip_codes,
            y = df_states_fips,
            by = "state") %>% 
  select(-fips) %>% 
  rename(customerstate = state,
         customerstatefull = state_full,
         customerzip = zipcode)

# join RIDB data with state data
ridb_state <- left_join(
  usfs_ridb, df_states_fips_zip_codes, by = "customerzip"
) %>% 
  relocate(customerstate, .after = customerzip) %>% 
  relocate(customerstatefull, .after = customerstate)

# calculate distance traveled ----
# bootstrap geometries and reproject to NAD 83
df_geometries <- usfs_ridb %>% 
  st_as_sf(coords = c("facilitylongitude", "facilitylatitude"),
           crs = 4326) %>% 
  st_transform(crs = 4269) # using NAD83 because measured in meters

# get centroid of geometries for all US ZIP codes 
df_zip_centroids_us <- get_acs(geography = "zcta", year = 2018, geometry = TRUE, 
                               summary_var = "B01001_001",
                               survey = "acs5",
                               variables = c(male = "B01001_002")) %>% 
  select(NAME, geometry) %>% 
  mutate(zip_code = str_sub(NAME, start = -5, end = -1)) %>% 
  select(zip_code, geometry) %>% 
  st_centroid()

# join data and calculate `distance_traveled` variable
df_joined_geometries <- 
  left_join(x = df_geometries %>% as.data.frame(),
            y = df_zip_centroids_us %>% as.data.frame(), 
            by = c("customerzip" = "zip_code")) %>%
  st_sf(sf_column_name = 'geometry.x') %>% 
  mutate(distance_traveled_m = st_distance(x = geometry.x, 
                                           y = geometry.y,
                                           by_element = TRUE),
         distance_traveled_m = as.numeric(distance_traveled_m))

# convert back to data.frame (from sf data.frame), remove geometries
df_joined <- df_joined_geometries %>% 
  as.data.frame() %>% 
  extract(col = geometry.x, 
          into = c('facility_latitude', 'facility_longitude'), 
          regex = '\\((.*), (.*)\\)', 
          convert = TRUE, remove = TRUE) %>% 
  select(-geometry.y)

# combine data ----

# save data ----
write_csv(usfs_ridb, here("data/ridb/clean/usfs_ridb.csv"))

# testing ----
test_park2018 <- raw_ridb2018 %>% 
  # add sept_out to rm `_` to match other column names
  janitor::clean_names(sep_out = "") %>% 
  # cols of interest
  select(
    historicalreservationid,
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
  # filter for just USFS in CA
  # NOTEHD: `case_when()` is for 2018 data
  mutate(facilitystate = case_when(facilitystate == "CA" ~ "California",
                                   TRUE ~ facilitystate)) %>% 
  filter(agency == "USFS" & facilitystate == "California") %>% 
  ## clean customer zip codes ##
  # 1. extract 5 digit customer zip codes 
  mutate(customerzip = str_extract(string = customerzip,
                                   pattern = "[:digit:]{5}")) %>% 
  # 2. rm invalid customer zip codes
  filter(!customerzip %in% c("00000", "99999")) %>%
  # 3. rm NA customer zip codes
  drop_na(customerzip) %>% 
  ## clean forestname ##
  # 1. rename parentlocation to forestname
  rename(forestname = parentlocation) %>% 
  mutate(
    # 2. standardize "National Forest"
    forestname = str_replace(string = forestname,
                             pattern = paste(c(
                               "NF - FS", "NF -FS", "NF- FS",
                               "NF-FS", "-FS", " - FS"),
                               collapse = "|"),
                             replacement = "National Forest"),
    # 3. standardize to title case
    forestname = str_to_title(forestname),
    # 4. fix forestname typos based on spatial data
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
  # 5. rm forests NOT in R5
  # NOTEHD: forestname var comes from spatial data script
  filter(forestname %in% forestname_R5) %>% 
  mutate(park = str_to_title(park),
         # 2. fix string errors
         park = str_remove(string = park,
                           pattern = paste(c("\\(.*", " \\(.*",
                                             "---.*", " ---.*",
                                             "-.*", " -.*",
                                             ",.*"), collapse = "|")),
         park = str_replace(string = park,
                            pattern = "@",
                            replacement = "At"),
         park = str_replace(string = park,
                            pattern = "Cg",
                            replacement = "Campground"),
         park = str_replace(string = park,
                            pattern = "&",
                            replacement = "And"),
         park = str_replace(string = park,
                            pattern = paste(c("/", " / "), collapse = "|"),
                            replacement = " ")) %>% 
  group_by(forestname, park) %>% 
  summarize(n = n())


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
# ~26% of 2020 data (tot 520,568)
# ~23% of 2021 data (tot 617,333 obs; tot na 139,645) expect 477,688 obs

