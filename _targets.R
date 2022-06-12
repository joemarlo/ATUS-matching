# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes)
options(mc.cores = min(parallel::detectCores(), 6))

# Set target options:
tar_option_set(
  packages = c("dplyr", "lubridate", "tidyr", "stringr", "purrr", 'tibble', "ggplot2", "rvest",
               "fuzzyjoin", "tidytext", "viridis", "viridisLite"), # packages that your targets need to run
  format = "rds" # default storage format
)

# tar_make_clustermq()
options(clustermq.scheduler = "multicore")

# tar_make_future() 

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)


# useful commands
# tar_visnetwork()
# tar_make()
# tar_watch()
# tar_meta(fields = warnings)


# pipeline ----------------------------------------------------------------

pipeline <- list()

# data pipelines
pipeline$data$download <- list(
  tar_target(
    name = download_ATUS_files,
    command = download_ATUS()
  )
)
pipeline$data$essential_industries <- list(
  tar_target(
    name = path_NAICS,
    command = 'inputs/2017_NAICS_Index_File.xlsx',
    format = "file"
  ),
  tar_target(
    name = NAICS,
    command = readxl::read_xlsx(path_NAICS)
  ),
  tar_target(
    name = path_crosswalk,
    command = 'inputs/crosswalk_2017_Census_to_2012_Census.xlsx',
    format = "file"
  ),
  tar_target(
    name = crosswalk,
    command = readxl::read_xlsx(path_crosswalk)
  ),
  tar_target(
    name = essential_industries,
    command = create_industries('https://www.cdc.gov/vaccines/covid-19/categories-essential-workers.html', NAICS, crosswalk)
  ),
  tar_target(
    name = path_essential_industries,
    command = 'data/essential_industries.csv',
    format = "file"
  ),
  tar_target(
    name = write_out_essential_industries,
    command = readr::write_csv(essential_industries, path_essential_industries)
  )
)
pipeline$data$FIPS <- list(
  tar_target(
    name = FIPS,
    command = create_fips('https://www.nrcs.usda.gov/wps/portal/nrcs/detail/?cid=nrcs143_013696')
  ),
  tar_target(
    name = write_out_FIPS,
    command = readr::write_csv(FIPS, 'data/FIPS.csv')
  )
)
pipeline$data$ATUS <- list(
  tar_target(
    name = paths_ATUS,
    command = list.files(file.path('inputs', 'ATUS-2003-2020'), pattern = '*.dat')
  ),
  tar_target(
    name = data_ATUS,
    command = read_atus(paths_ATUS)
  ),
  tar_target(
    name = write_atussum_0320,
    command = readr::write_tsv(data_ATUS$atussum_0320, 'data/atussum_0320.tsv')
  ),
  tar_target(
    name = clean_ATUS,
    command = clean_atus(data_ATUS$atusact_0320)
  ),
  tar_target(
    name = write_ATUS_1,
    command = readr::write_tsv(clean_ATUS$ATUS_1, 'data/atus_SSC.tsv')
  ),
  tar_target(
    name = write_ATUS_30,
    command = readr::write_tsv(clean_ATUS$ATUS_30, 'data/atus_30min_SSC.tsv')
  )
)
pipeline$data$demographics <- list(
  tar_target(
    name = data_demographics,
    command = clean_demographics(
      clean_ATUS$ATUS_30,
      data_ATUS$atusrost_0320,
      data_ATUS$atuscps_0320,
      data_ATUS$atussum_0320,
      data_ATUS$atusresp_0320,
      essential_industries,
      FIPS
    )
  ),
  tar_target(
    name = write_demographics,
    command = readr::write_tsv(data_demographics, 'data/demographic.tsv')
  )
)


# analysis pipelines
pipeline$analysis$timeseries <- list(
  
)
pipeline$analysis$matching <- list(
  
)
pipeline$analysis$SA <- list(
  
)


# full pipeline
list(
  pipeline$data$download,
  pipeline$data$essential_industries,
  pipeline$data$FIPS,
  pipeline$data$ATUS,
  pipeline$data$demographics,
  pipeline$analysis$timeseries,
  pipeline$analysis$matching,
  pipeline$analysis$SA
)
