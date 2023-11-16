# setup ----
# load libraries
library(tidyverse)
library(here)
library(tidycensus)

# read in/load data ----
census_api_key("insert_api_key", install = TRUE, overwrite = TRUE)

# race acs processing ----
function(geography = "zcta", year, state){
  # read in raw data
  df_race <- 
    get_acs(geography = "zcta",
            year = 2018,
            state = "California",
            survey = "acs5",
            summary_var = "B03002_001", #Estimate!!Total: 
            variables = c(
              white = "B03002_003", #Estimate!!Total!!Not Hispanic or Latino!!White alone
              black = "B03002_004", # Estimate!!Total:!!Not Hispanic or Latino!!Black or African American alone
              native_american = "B03002_005", # Estimate!!Total:!!Not Hispanic or Latino!!American Indian and Alaska Native alone
              asian = "B03002_006", # Estimate!!Total:!!Not Hispanic or Latino!!Asian alone
              pacific_islander = "B03002_007", # Estimate!!Total:!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone
              other = "B03002_008", # Estimate!!Total:!!Not Hispanic or Latino!!Some other race alone
              multiracial = "B03002_009", # Estimate!!Total:!!Not Hispanic or Latino!!Two or more races
              hispanic_latinx = "B03002_012" # Estimate!!Total!!Hispanic or Latino
            )) %>% 
    clean_names() %>% 
    rename(race = variable) %>% 
    # create column of 5 digit ZIP code
    mutate(zip_code = str_sub(name, start = -5, end = -1))
  # calculate percentage
  df_percent <- 
    df_race %>% 
    group_by(zip_code, race) %>% 
    summarise(estimate = sum(estimate),
              moe = sum(moe),
              summary_est = unique(summary_est),
              summary_moe = unique(summary_moe),
              percent = estimate / summary_est)
  # create column for each percentage for each group (pivot wider)
  # necessary to be able to left_join() with RIDB data
  df_percent_wider <- 
    df_percent %>% 
    select(zip_code, summary_est, race, percent) %>% 
    rename(zip_code_population = summary_est) %>% 
    pivot_wider(names_from = "race",
                values_from = "percent")
  
  # create df
  if (is.null(state)) {
    assign(paste0("data_acs_", year, "_race_percent"), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  } else {
    assign(paste0("data_acs_", year, "_race_percent_", state), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  }
}

# education acs processing ----
function(geography = "zcta", year, state){
  # read in raw data
  df_edu_2018 <- 
    get_acs(geography = "zcta",
            year = 2018,
            state = "California",
            survey = "acs5",
            summary_var = "B15003_001", # Estimate!!Total:
            variables = c(
              # Estimate!!Total:!!GED or alternative credential or below (including above)
              hs_GED_or_below = "B15003_002", 
              hs_GED_or_below = "B15003_003",  
              hs_GED_or_below = "B15003_004", 
              hs_GED_or_below = "B15003_005", 
              hs_GED_or_below = "B15003_006", 
              hs_GED_or_below = "B15003_007", 
              hs_GED_or_below = "B15003_008", 
              hs_GED_or_below = "B15003_009",
              hs_GED_or_below = "B15003_010", 
              hs_GED_or_below = "B15003_011", 
              hs_GED_or_below = "B15003_012", 
              hs_GED_or_below = "B15003_013",
              hs_GED_or_below = "B15003_014", 
              hs_GED_or_below = "B15003_015", 
              hs_GED_or_below = "B15003_016", 
              hs_GED_or_below = "B15003_017",
              hs_GED_or_below = "B15003_018", 
              # Estimate!!Total:!!Some college
              some_college = "B15003_019", 
              some_college = "B15003_020", 
              # Estimate!!Total:!!Associates or bachelors degree
              college = "B15003_021", 
              college = "B15003_022", 
              # Estimate!!Total:!!Masters degree and above
              master_or_above = "B15003_023", 
              master_or_above = "B15003_024", 
              master_or_above = "B15003_025" 
            )) %>% 
    clean_names() %>% 
    rename(education = variable) %>% 
    # create ZIP column with just 5 digit numbers
    mutate(zip_code = str_sub(name, start = -5, end = -1))
  # calculate percentage
  df_percent <- 
    df_edu %>% 
    group_by(zip_code, education) %>% 
    summarize(estimate = sum(estimate),
              moe = sum(moe),
              summary_est = unique(summary_est),
              summary_moe = unique(summary_moe),
              percent = estimate / summary_est)
  # create column for each percentage for each group (pivot wider)
  # necessary to be able to left_join() with RIDB data
  df_percent_wider <- 
    df_percent %>% 
    select(zip_code, summary_est, education, percent) %>% 
    rename(zip_code_population = summary_est) %>% 
    pivot_wider(names_from = education,
                values_from = percent)
  
  # create df
  if (is.null(state)) {
    assign(paste0("data_acs_", year, "_education_percent"), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  } else {
    assign(paste0("data_acs_", year, "_education_percent_", state), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  }
}

# language acs processing ----
function(geography = "zcta", state){
  # read in raw data
  df_lang <- 
    get_acs(geography = "zcta",
            year = 2018, # closest year to 2018 that doesn't pull all NA values
            state = "California",
            survey = "acs5",
            summary_var = "B16001_001", # Estimate!!Total:
            variables = c(
              english_only = "B16001_002" # Estimate!!Total!!Speak only English
            )) %>% 
    clean_names() %>% 
    rename(language = variable) %>% 
    # create ZIP column with just 5 digit numbers
    mutate(zip_code = str_sub(name, start = -5, end = -1))
  # calculate percentage
  df_percent <- 
    df_lang %>% 
    group_by(zip_code, language) %>% 
    summarize(estimate = sum(estimate),
              moe = sum(moe),
              summary_est = unique(summary_est),
              summary_moe = unique(summary_moe),
              percent = estimate / summary_est)
  # create column for each percentage for each group (pivot wider)
  # necessary to be able to left_join() with RIDB data
  df_percent_wider <- 
    df_percent %>% 
    select(zip_code, summary_est, language, percent) %>% 
    rename(zip_code_population = summary_est) %>% 
    pivot_wider(names_from = "language",
                values_from = "percent") %>% 
    mutate(not_english_only = 1 - english_only)
  
  # create df
  if (is.null(state)) {
    assign(paste0("data_acs_", "2020", "_language_percent"), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  } else {
    assign(paste0("data_acs_", "2020", "_language_percent_", state), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  }
}

# median income acs processing ----
function(geography = "zcta", year, state){
  df_median_income <- 
    get_acs(geography = "zcta",
            year = 2018,
            state = "California",
            survey = "acs5",
            variables = c(
              median_income = "B19013_001" # Estimate!!Median household income in the past 12 months (in 2019 inflation-adjusted dollars)
            )) %>% 
    clean_names() %>% 
    rename(median_income = estimate) %>% 
    mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
    select(median_income, zip_code)
  
  # create df
  if (is.null(state)) {
    assign(x = paste0("data_acs_", year, "_median_income"), 
           data.frame(df), envir = .GlobalEnv)
  } else {
    assign(x = paste0("data_acs_", year, "_median_income_", state), 
           data.frame(df), envir = .GlobalEnv)
  }
}

# median age acs processing ----
function(geography = "zcta", year, state){
  df_median_age <- 
    get_acs(geography = "zcta",
            year = 2018,
            state = "California",
            survey = "acs5",
            variables = c(
              median_income = "B01002_001" # Estimate!!Median age
            )) %>% 
    clean_names() %>% 
    rename(median_age = estimate) %>% 
    mutate(zip_code = str_sub(name, start = -5, end = -1)) %>% 
    select(median_age, zip_code)
  
  # create df
  if (is.null(state)) {
    assign(x = paste0("data_acs_", year, "_median_age"), 
           data.frame(df), envir = .GlobalEnv)
  } else {
    assign(x = paste0("data_acs_", year, "_median_age_", state), 
           data.frame(df), envir = .GlobalEnv)
  }
}

# computer in households acs processing ----
function(geography = "zcta", year, state){
  # read in raw data
  df_computers <- 
    get_acs(geography = "zcta",
            year = 2018,
            state = "California",
            survey = "acs5",
            summary_var = "B28003_001", #Estimate!!Total: 
            variables = c(
              has_a_computer = "B28003_002", #Estimate!!Total!!Has one or more types of computing devices
              no_computer = "B28003_006" # Estimate!!Total:!!No computer
            )) %>% 
    clean_names() %>% 
    rename(access_to_computer = variable) %>% 
    # create column of 5 digit ZIP code
    mutate(zip_code = str_sub(name, start = -5, end = -1))
  # calculate percentage
  df_percent <- 
    df_computers %>% 
    group_by(zip_code, access_to_computer) %>% 
    summarise(estimate = sum(estimate),
              moe = sum(moe),
              summary_est = unique(summary_est),
              summary_moe = unique(summary_moe),
              percent = estimate / summary_est)
  
  # create df
  if (is.null(state)) {
    assign(paste0("data_acs_", year, "_race_percent"), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  } else {
    assign(paste0("data_acs_", year, "_race_percent_", state), 
           data.frame(df_percent_wider), envir = .GlobalEnv)
  }
}

# combine data into one data frame ----
# use join() or rbind()


# save data as a csv file ----
# use write_csv()