# need an api key to access data, this key is located in the google drive
# key should not be public or shared
# file path for saving acs csv files: root-of-project/data/census/csv
# file path for saving acs rds files: root-of-project/data/census/rds

# df = data frame

# 0. setup ----
library(zipcodeR)
library(dplyr)
library(tidyr)
library(readr)
library(tidycensus)
library(here)

## load data using Census API Key ----
# Only need to run once - key is saved to your RStudio environment after
# run Sys.getenv("CENSUS_API_KEY") in the Console to check
# census_api_key("insert_api_key", install = TRUE, overwrite = TRUE)

# 1. state information for ZIPS ----
zips <- zipcodeR::zip_code_db %>%
  dplyr::select(zipcode,
                state)

# 2. race acs processing ----
raw_race_df <- tidycensus::get_acs(
  # accessing data from 2017-2021 5-year ACS
  geography = "zcta",
  survey = "acs5",
  summary_var = "B03002_001",
  #Estimate!!Total:
  variables = c(
    white = "B03002_003",
    #Estimate!!Total!!Not Hispanic or Latino!!White alone
    black = "B03002_004",
    # Estimate!!Total:!!Not Hispanic or Latino!!Black or African American alone
    native_american = "B03002_005",
    # Estimate!!Total:!!Not Hispanic or Latino!!American Indian and Alaska Native alone
    asian = "B03002_006",
    # Estimate!!Total:!!Not Hispanic or Latino!!Asian alone
    pacific_islander = "B03002_007",
    # Estimate!!Total:!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone
    other = "B03002_008",
    # Estimate!!Total:!!Not Hispanic or Latino!!Some other race alone
    multiracial = "B03002_009",
    # Estimate!!Total:!!Not Hispanic or Latino!!Two or more races
    hispanic_latinx = "B03002_012" # Estimate!!Total!!Hispanic or Latino
  )
)

clean_race_df <- raw_race_df %>%
  janitor::clean_names() %>%
  dplyr::rename(race = variable) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_race_df <-
  dplyr::full_join(clean_race_df, zips, by = c("zip_code" = "zipcode"))

clean_race_df <- join_race_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code) %>%
  # add state info for ZIPS not listed in zipcodeR::zip_code_db
  dplyr::mutate(
    state = dplyr::case_when(
      zip_code == "72405" ~ "AR",
      zip_code == "72713" ~ "AR",
      zip_code == "75036" ~ "TX",
      zip_code == "75072" ~ "TX",
      zip_code == "89437" ~ "NV",
      zip_code == "97003" ~ "OR",
      TRUE ~ state
    )
  )

# calculate percentages of each racial group by zip code
calc_race_df <- clean_race_df %>%
  dplyr::group_by(zip_code, state, race) %>%
  dplyr::summarize(
    estimate = sum(estimate),
    moe = sum(moe),
    summary_est = unique(summary_est),
    summary_moe = unique(summary_moe),
    percent = estimate / summary_est
  )

## just CA calculated percentages
ca_calc_race_df <- calc_race_df %>%
  dplyr::filter(state == "CA")

# necessary to be able to `left_join()` with RIDB data
calc_race_wider_df <- calc_race_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                race,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "race",
                     values_from = "percent")

## just CA ZIPs
ca_calc_race_wider_df <- ca_calc_race_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                race,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "race",
                     values_from = "percent")

# save data as csv
readr::write_csv(calc_race_wider_df,
                 here::here("data/census/csv/acs_race_all_zips.csv"))
readr::write_csv(ca_calc_race_wider_df,
                 here::here("data/census/csv/acs_race_ca_zips.csv"))

# save data as rds
readr::write_rds(calc_race_wider_df,
                 here::here("data/census/rds/acs_race_all_zips.rds"))
readr::write_rds(ca_calc_race_wider_df,
                 here::here("data/census/rds/acs_race_ca_zips.rds"))

# 3. education acs processing ----
raw_edu_df <- tidycensus::get_acs(
  # accessing data from 2017-2021 5-year ACS
  geography = "zcta",
  survey = "acs5",
  summary_var = "B15003_001",
  # Estimate!!Total:
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
  )
)

clean_edu_df <- raw_edu_df %>%
  janitor::clean_names() %>%
  dplyr::rename(education = variable) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_edu_df <-
  dplyr::full_join(clean_edu_df, zips, by = c("zip_code" = "zipcode"))

clean_edu_df <- join_edu_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code) %>%
  # add state info for ZIPS not listed in zipcodeR::zip_code_db
  dplyr::mutate(
    state = dplyr::case_when(
      zip_code == "72405" ~ "AR",
      zip_code == "72713" ~ "AR",
      zip_code == "75036" ~ "TX",
      zip_code == "75072" ~ "TX",
      zip_code == "89437" ~ "NV",
      zip_code == "97003" ~ "OR",
      TRUE ~ state
    )
  )

# calculate percentages of each edu group by zip code
calc_edu_df <- clean_edu_df %>%
  dplyr::group_by(zip_code, state, education) %>%
  dplyr::summarize(
    estimate = sum(estimate),
    moe = sum(moe),
    summary_est = unique(summary_est),
    summary_moe = unique(summary_moe),
    percent = estimate / summary_est
  )

## just CA calculated percentages
ca_calc_edu_df <- calc_edu_df %>%
  dplyr::filter(state == "CA")

# necessary to be able to `left_join()` with RIDB data
calc_edu_wider_df <- calc_edu_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                education,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "education",
                     values_from = "percent")

## just CA ZIPs
ca_calc_edu_wider_df <- ca_calc_edu_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                education,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "education",
                     values_from = "percent")

# save data as csv
readr::write_csv(calc_edu_wider_df,
                 here::here("data/census/csv/acs_edu_all_zips.csv"))
readr::write_csv(ca_calc_edu_wider_df,
                 here::here("data/census/csv/acs_edu_ca_zips.csv"))

# save data as rds
readr::write_rds(calc_edu_wider_df,
                 here::here("data/census/rds/acs_edu_all_zips.rds"))
readr::write_rds(ca_calc_edu_wider_df,
                 here::here("data/census/rds/acs_edu_ca_zips.rds"))

# 4. language acs processing ----
raw_lang_df <- tidycensus::get_acs(
  # accessing data from the 2011-2015 5-year ACS
  geography = "zcta",
  # closest year to 2018 that doesn't pull all NA values
  year = 2015,
  survey = "acs5",
  # Estimate!!Total:
  summary_var = "B16001_001",
  # Estimate!!Total!!Speak only English
  variables = c(english_only = "B16001_002")
)

clean_lang_df <- raw_lang_df %>%
  janitor::clean_names() %>%
  dplyr::rename(language = variable) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_lang_df <-
  dplyr::full_join(clean_lang_df, zips, by = c("zip_code" = "zipcode"))

clean_lang_df <- join_lang_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code)

# calculate percentages of each lang group by zip code
calc_lang_df <- clean_lang_df %>%
  dplyr::group_by(zip_code, state, language) %>%
  dplyr::summarize(
    estimate = sum(estimate),
    moe = sum(moe),
    summary_est = unique(summary_est),
    summary_moe = unique(summary_moe),
    percent = estimate / summary_est
  )

## just CA calculated percentages
ca_calc_lang_df <- calc_lang_df %>%
  dplyr::filter(state == "CA")

# necessary to be able to `left_join()` with RIDB data
calc_lang_wider_df <- calc_lang_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                language,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "language",
                     values_from = "percent") %>%
  dplyr::mutate(not_english_only = 1 - english_only)

## just CA ZIPs
ca_calc_lang_wider_df <- ca_calc_lang_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                language,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "language",
                     values_from = "percent") %>%
  dplyr::mutate(not_english_only = 1 - english_only)

# save data as csv
readr::write_csv(calc_lang_wider_df,
                 here::here("data/census/csv/acs_lang_all_zips.csv"))
readr::write_csv(ca_calc_lang_wider_df,
                 here::here("data/census/csv/acs_lang_ca_zips.csv"))

# save data as rds
readr::write_rds(calc_lang_wider_df,
                 here::here("data/census/rds/acs_lang_all_zips.rds"))
readr::write_rds(ca_calc_lang_wider_df,
                 here::here("data/census/rds/acs_lang_ca_zips.rds"))

# 5. median income acs processing ----
raw_med_inc_df <- tidycensus::get_acs(
  # accessing data from the 2017-2021 5-year ACS
  geography = "zcta",
  survey = "acs5",
  # Estimate!!Median household income in the past 12 months
  variables = c(median_income = "B19013_001")
)

clean_med_inc_df <- raw_med_inc_df %>%
  janitor::clean_names() %>%
  dplyr::rename(median_income = estimate) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_med_inc_df <-
  dplyr::full_join(clean_med_inc_df, zips, by = c("zip_code" = "zipcode"))

# note: some median incomes are NA
clean_med_inc_df <- join_med_inc_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code) %>%
  # add state info for ZIPS not listed in zipcodeR::zip_code_db
  dplyr::mutate(
    state = dplyr::case_when(
      zip_code == "72405" ~ "AR",
      zip_code == "72713" ~ "AR",
      zip_code == "75036" ~ "TX",
      zip_code == "75072" ~ "TX",
      zip_code == "89437" ~ "NV",
      zip_code == "97003" ~ "OR",
      TRUE ~ state
    )
  ) %>%
  dplyr::select(-geoid,-variable,-moe)

ca_med_inc_df <- clean_med_inc_df %>%
  dplyr::filter(state == "CA")

# save data as csv
readr::write_csv(clean_med_inc_df,
                 here::here("data/census/csv/acs_med_inc_all_zips.csv"))
readr::write_csv(ca_med_inc_df,
                 here::here("data/census/csv/acs_med_inc_ca_zips.csv"))

# save data as rds
readr::write_rds(clean_med_inc_df,
                 here::here("data/census/rds/acs_med_inc_all_zips.rds"))
readr::write_rds(ca_med_inc_df,
                 here::here("data/census/rds/acs_med_inc_ca_zips.rds"))


# 6. median age acs processing ----
raw_med_age_df <- tidycensus::get_acs(
  # accessing data from the 2017-2021 5-year ACS
  geography = "zcta",
  survey = "acs5",
  # Estimate!!Median age
  variables = c(median_age = "B01002_001")
)

clean_med_age_df <- raw_med_age_df %>%
  janitor::clean_names() %>%
  dplyr::rename(median_age = estimate) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_med_age_df <-
  dplyr::full_join(clean_med_age_df, zips, by = c("zip_code" = "zipcode"))

# note: some median ages are NA
clean_med_age_df <- join_med_age_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code) %>%
  # add state info for ZIPS not listed in zipcodeR::zip_code_db
  dplyr::mutate(
    state = dplyr::case_when(
      zip_code == "72405" ~ "AR",
      zip_code == "72713" ~ "AR",
      zip_code == "75036" ~ "TX",
      zip_code == "75072" ~ "TX",
      zip_code == "89437" ~ "NV",
      zip_code == "97003" ~ "OR",
      TRUE ~ state
    )
  ) %>%
  dplyr::select(-geoid,-variable,-moe)

ca_med_age_df <- clean_med_age_df %>%
  dplyr::filter(state == "CA")

# save data as csv
readr::write_csv(clean_med_age_df,
                 here::here("data/census/csv/acs_med_age_all_zips.csv"))
readr::write_csv(ca_med_age_df,
                 here::here("data/census/csv/acs_med_age_ca_zips.csv"))

# save data as rds
readr::write_rds(clean_med_age_df,
                 here::here("data/census/rds/acs_med_age_all_zips.rds"))
readr::write_rds(ca_med_age_df,
                 here::here("data/census/rds/acs_med_age_ca_zips.rds"))


# 7. computer in household acs processing ----
raw_comp_df <- tidycensus::get_acs(
  # accessing data from the 2017-2021 5-year ACS
  geography = "zcta",
  survey = "acs5",
  summary_var = "B28003_001",
  #Estimate!!Total:
  variables = c(# Estimate!!Total!!Has one or more types of computing devices
    has_a_computer = "B28003_002",
    # Estimate!!Total:!!No computer
    no_computer = "B28003_006")
)

clean_comp_df <- raw_comp_df %>%
  janitor::clean_names() %>%
  dplyr::rename(computer = variable) %>%
  dplyr::rename(zip_code = name) %>%
  # extract 5 digit ZIP code
  dplyr::mutate(zip_code = str_sub(zip_code, start = -5, end = -1))

# combine acs and state data
join_comp_df <-
  dplyr::full_join(clean_comp_df, zips, by = c("zip_code" = "zipcode"))

clean_comp_df <- join_comp_df %>%
  # rm rows added from zipcodeR::zip_code_db
  dplyr::filter(is.na(geoid) != TRUE) %>%
  dplyr::relocate(state, .after = zip_code) %>%
  # add state info for ZIPS not listed in zipcodeR::zip_code_db
  dplyr::mutate(
    state = dplyr::case_when(
      zip_code == "72405" ~ "AR",
      zip_code == "72713" ~ "AR",
      zip_code == "75036" ~ "TX",
      zip_code == "75072" ~ "TX",
      zip_code == "89437" ~ "NV",
      zip_code == "97003" ~ "OR",
      TRUE ~ state
    )
  )

# calculate percentages of each computer group by zip code
calc_comp_df <- clean_comp_df %>%
  dplyr::group_by(zip_code, state, computer) %>%
  dplyr::summarize(
    estimate = sum(estimate),
    moe = sum(moe),
    summary_est = unique(summary_est),
    summary_moe = unique(summary_moe),
    percent = estimate / summary_est
  )

## just CA calculated percentages
ca_calc_comp_df <- calc_comp_df %>%
  dplyr::filter(state == "CA")

# necessary to be able to `left_join()` with RIDB data
calc_comp_wider_df <- calc_comp_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                computer,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "computer",
                     values_from = "percent")

## just CA ZIPs
ca_calc_comp_wider_df <- ca_calc_comp_df %>%
  dplyr::select(zip_code,
                state,
                summary_est,
                computer,
                percent) %>%
  dplyr::rename(zip_code_population = summary_est) %>%
  tidyr::pivot_wider(names_from = "computer",
                     values_from = "percent")

# save data as csv
readr::write_csv(calc_comp_wider_df,
                 here::here("data/census/csv/acs_comp_all_zips.csv"))
readr::write_csv(ca_calc_comp_wider_df,
                 here::here("data/census/csv/acs_comp_ca_zips.csv"))

# save data as rds
readr::write_rds(calc_comp_wider_df,
                 here::here("data/census/rds/acs_comp_all_zips.rds"))
readr::write_rds(ca_calc_comp_wider_df,
                 here::here("data/census/rds/acs_comp_ca_zips.rds"))



# 8. combine all census data ----
## all ZIPs ##
# need to rm zip_code_population bc estimates are not the same for all vars
acs_dfs <- list(calc_race_wider_df,
                calc_edu_wider_df,
                calc_lang_wider_df,
                calc_comp_wider_df)

for (i in 1:length(acs_dfs)) {
  
  acs_dfs[[i]] <- acs_dfs[[i]] %>% dplyr::select(-zip_code_population)
  
}

# join race + edu
combined_acs_df <- dplyr::full_join(acs_dfs[[1]], acs_dfs[[2]], by = c("zip_code", "state"))
# join race/edu + lang
combined_acs_df  <- dplyr::full_join(combined_acs_df , acs_dfs[[3]], by = c("zip_code", "state"))
# join race/edu/lang + med inc
combined_acs_df  <- dplyr::full_join(combined_acs_df , clean_med_inc_df, by = c("zip_code", "state"))
# join race/edu/lang/med_inc + med_age
combined_acs_df  <- dplyr::full_join(combined_acs_df , clean_med_age_df, by = c("zip_code", "state"))
# join race/edu/lang/med_inc/med_age + comp
combined_acs_df  <- dplyr::full_join(combined_acs_df , acs_dfs[[4]], by = c("zip_code", "state"))

## just CA ZIPs ## 
# need to rm zip_code_population bc estimates are not the same for all vars
acs_ca_dfs <- list(
  ca_calc_race_wider_df,
  ca_calc_edu_wider_df,
  ca_calc_lang_wider_df,
  ca_calc_comp_wider_df
)

for (i in 1:length(acs_ca_dfs)) {
  
  acs_ca_dfs[[i]] <- acs_ca_dfs[[i]] %>% dplyr::select(-zip_code_population)
}

# join race + edu
combined_ca_acs_df <- dplyr::full_join(acs_ca_dfs[[1]], acs_ca_dfs[[2]], by = c("zip_code", "state"))
# join race/edu + lang
combined_ca_acs_df  <- dplyr::full_join(combined_ca_acs_df , acs_ca_dfs[[3]], by = c("zip_code", "state"))
# join race/edu/lang + med inc
combined_ca_acs_df  <- dplyr::full_join(combined_ca_acs_df , ca_med_inc_df, by = c("zip_code", "state"))
# join race/edu/lang/med_inc + med_age
combined_ca_acs_df  <- dplyr::full_join(combined_ca_acs_df , ca_med_age_df, by = c("zip_code", "state"))
# join race/edu/lang/med_inc/med_age + comp
combined_ca_acs_df  <- dplyr::full_join(combined_ca_acs_df , acs_ca_dfs[[4]], by = c("zip_code", "state"))

# save as csv
readr::write_csv(combined_acs_df,
                 here::here("data/census/csv/combined_acs_all_zips.csv"))
readr::write_csv(combined_ca_acs_df,
                 here::here("data/census/csv/combined_acs_ca_zips.csv"))

# save data as rds
readr::write_rds(combined_acs_df,
                 here::here("data/census/rds/combined_acs_all_zips.rds"))
readr::write_rds(combined_ca_acs_df,
                 here::here("data/census/rds/combined_acs_ca_zips.rds"))

