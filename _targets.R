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
               "dplyr")
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
  )
)
