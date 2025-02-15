  ########################################################## Load data from a list of DOIs ########################################################## 
  require(dplyr)
  require(zen4R)
  require(arrow)
  require(futile.logger)
  require(here)
  here::i_am("create_or_load_default_dataset.R")
  source(here::here('install.R'))
  flog.info("Loading libraries")
  source(here::here('R/download_data.R'))
  source(here::here('R/load_data.R'))
  source(here::here('R/load_spatial_data.R'))
  source(here::here('R/load_default_dataset.R'))
  source(here::here('R/load_grouped_data.R'))
  source(here::here('R/load_filters_combinations.R'))
  source(here::here('R/update_current_filters.R'))
  source(here::here('R/list_areas_within_wkt.R'))
  source(here::here('R/verify_filesize.R'))
  source(here::here('R/apply_filters.R'))
  flog.info("Loading libraries")
  flog.info("All libraries loaded successfully.")
  spatial_processing_mode <- "sf" # "QGIS"
  sf::sf_use_s2(FALSE)
  mode="DOI"
  here::i_am("create_or_load_default_dataset.R")
  if(!file.exists(here::here("data/list_dataframes.qs"))){
  list_DOIs <-"data/DOI.csv"
  DOIs <- readr::read_csv(list_DOIs) %>% dplyr::mutate(identifier="",title="")
  list_dataframes <- load_data(mode=mode)
  qs::qsave(list_dataframes,here::here("data/list_dataframes.qs"))
  } else {
    list_dataframes <- qs::qread(here::here("data/list_dataframes.qs"))
      }