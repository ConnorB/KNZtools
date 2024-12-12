#' Load Konza Prairie Precipitation Data
#'
#' Fetches data from the dataset: APT01 Daily precipitation amounts measured at multiple sites across Konza Prairie.
#'
#' @return A tibble containing daily precipitation data.
#' @importFrom EDIutils list_data_package_revisions read_data_entity_names read_data_entity
#' @importFrom readr read_csv
#' @export
load_knz_ppt <- function() {
  # Retrieve the latest package revision
  revision <- EDIutils::list_data_package_revisions(scope = 'knb-lter-knz', identifier = "4", filter = "newest")
  packageid <- paste0('knb-lter-knz.4.', revision)

  # Fetch data entity and read data
  res <- EDIutils::read_data_entity_names(packageid)
  raw <- EDIutils::read_data_entity(packageId = packageid, entityId = res$entityId[1])
  data <- readr::read_csv(file = raw)

  return(data)
}

#' Load Konza Prairie Weather Data
#'
#' Fetches data from the dataset: AWE01 Meteorological data from the Konza Prairie headquarters weather station.
#'
#' @return A tibble containing meteorological data.
#' @importFrom EDIutils list_data_package_revisions read_data_entity_names read_data_entity
#' @importFrom readr read_csv
#' @export
load_knz_weather <- function() {
  # Retrieve the latest package revision
  revision <- EDIutils::list_data_package_revisions(scope = 'knb-lter-knz', identifier = "14", filter = "newest")
  packageid <- paste0('knb-lter-knz.14.', revision)

  # Fetch data entity and read data
  res <- EDIutils::read_data_entity_names(packageid)
  raw <- EDIutils::read_data_entity(packageId = packageid, entityId = res$entityId[1])
  data <- readr::read_csv(file = raw)

  return(data)
}

#' Load Konza Prairie Weir Data
#'
#' Fetches stream discharge data from multiple datasets:
#' - ASD02 Stream discharge measured at the flumes on watershed N04D at Konza Prairie.
#' - ASD04 Stream discharge measured at the flumes on watershed N20B at Konza Prairie.
#' - ASD05 Stream discharge measured at the flumes on watershed N01B at Konza Prairie.
#' - ASD06 Stream discharge measured at the flumes on watershed N02B at Konza Prairie.
#'
#' @return A tibble containing combined stream discharge data from multiple watersheds.
#' @importFrom EDIutils list_data_package_revisions read_data_entity_names read_data_entity
#' @importFrom readr read_csv
#' @export
load_knz_weirs <- function() {
  # Define package IDs and dataset metadata
  datasets <- list(
    n04d = list(scope = "knb-lter-knz", id = "7"),
    n20b = list(scope = "knb-lter-knz", id = "8"),
    n01b = list(scope = "knb-lter-knz", id = "9"),
    n02b = list(scope = "knb-lter-knz", id = "10")
  )

  # Define column names for the data
  weir_col_names <- c(
    "DataCode", "RecType", "RecYear", "RecMonth", "RecDay", "Watershed",
    "DayofYear", "RecHour", "Discharge", "Sheight", "CorrectedSheight",
    "Height", "LogFlag", "QualFlag"
  )

  # Function to retrieve and read data for a given dataset
  get_dataset_data <- function(scope, id) {
    revision <- EDIutils::list_data_package_revisions(scope = scope, identifier = id, filter = "newest")
    package_id <- paste0(scope, ".", id, ".", revision)
    entity <- EDIutils::read_data_entity_names(package_id)
    raw_data <- EDIutils::read_data_entity(packageId = package_id, entityId = entity$entityId[1])
    readr::read_csv(file = raw_data,
                    skip = 1,
                    col_names = weir_col_names,
                    na = c(" ", ".", "NA", ""),
                    show_col_types = FALSE)
  }

  # Retrieve and combine data from all datasets
  data <- lapply(datasets, function(dataset) {
    get_dataset_data(dataset$scope, dataset$id)
  })

  combined_data <- do.call(rbind, data)

  return(combined_data)
}
