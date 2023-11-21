# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  # packages that your targets need to run
  packages = c("here",
               "vroom",
               "sf",
               "janitor",
               "lubridate",
               "dplyr",
               "tidyr",
               "stringr")
)

# Run the R scripts in the scripts/ folder with your custom functions:
tar_source(files = "scripts")
# source("other_functions.R") # Source other scripts as needed.

# Replace the target list below with your own: 
list(
  tar_target(
    ## --- spatial data: R5 boundaries --- ##
    name = file_spatial,
    command = get_spatial_data(fp = "data/spatial/raw/S_USA.AdministrativeForest/",
                               df_name = usfs_regions,
                               raw_df = raw_usfs_regions,
                               format = "file")
  ),
  tar_target(
    name = filter_r5,
    command = r5_bounds(raw_df = file_spatial,
                        clean_df = r5_bounds)
  ),
  # create `forestname_vec` use in RIDB processing
  tar_target(
    name = get_forestnames,
    command = r5_forestnames(clean_df = filter_r5,
                             forestname_vec = forestname_vec)
  ),
  tar_target(
    name = save_spatial,
    command = r5_bounds_csv(clean_df = filter_r5, 
                            fp = "data/spatial/clean/r5_boundaries.shp")
  ),
  
  ## --- RIDB data --- ##
  tar_target(
    name = file_ridb,
    command = get_ridb_data(fp = "data/ridb/raw/",
                            file = "reservations2018.csv",
                            df_name = ridb,
                            raw_df = raw_ridb)
  ),
  tar_target(
    name = get_r5_ridb,
    command = subset_ridb(subset_df = subset_df, 
                          raw_df = file_ridb)
  ),
  tar_target(
    name = process_customer_zips,
    command = clean_customer_zips(customer_zips_df = customer_zips_df,
                                  subset_df = get_r5_ridb)
  ),
  tar_target(
    name = process_forestname,
    command = clean_forestname(forestname_df = forestname_df,
                               customer_zips_df = process_customer_zips)
  ),
  tar_target(
    name = process_park,
    command = clean_park(park_df = park_df,
                         forestname_df = process_forestname,
                         # `forestname_vec` from spatial bounds processing
                         forestname_vec = get_forestnames)
  ),
  tar_target(
    name = process_facility_location,
    command = clean_facility_location(facility_location_df = facility_location_df,
                                      park_df = process_park)
  ),
  tar_target(
    name = process_sitetype,
    command = clean_sitetype(sitetype_df = sitetype_df,
                             facility_location_df = process_facility_location)
  ),
  tar_target(
    # length of stay, booking window, daily cost, daily cost per visitor
    name = create_vars,
    command = calc_vars(calc_vars_df = calc_vars_df,
                        sitetype_df = process_sitetype)
  )
  
  
)
