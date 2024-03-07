# prepare_input_data -----------------------------------------------------------

#' Prepare Input Data: Rename, Add Columns
#'
#' Rename columns from ABIMO 3.2 original names to ABIMO 3.3 internal names
#'
#' @param data data frame with columns CODE, REGENJA, REGENSO, NUTZUNG,
#'   TYP, BEZIRK, FLGES, STR_FLGES, PROBAU, PROVGU, VGSTRASSE, KAN_BEB, BELAG1,
#'   BELAG2, BELAG3, BELAG4, KAN_VGU, STR_BELAG1, STR_BELAG2, STR_BELAG3,
#'   STR_BELAG4, KAN_STR, FLUR, FELD_30, FELD_150
#' @param config configuration object (list) as returned by the function
#'   \code{abimo_config_to_config()} used on \code{kwb.abimo::read_config()}
#' @return \code{data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, (main) usage, yield, irrigation)
prepare_input_data <- function(data, config)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #data <- kwb.abimo::abimo_input_2019
  #config <- abimo_config_to_config(kwb.abimo::read_config())

  #
  # See inst/extdata/test-rabimo.R for test data assignments
  #

  # 1. Rename columns from ABIMO 3.2 names to ABIMO new* internal names
  # 2. Select only the columns that are required
  data <- rename_columns(data, renamings = get_column_renamings())

  # Create column accessor function
  fetch <- create_accessor(data)
  fetch_config <- create_accessor(config)

  # correct precipitation with correction factor from the config file
  data[["prec_yr"]] <- fetch("prec_yr") *
    fetch_config("precipitation_correction_factor")

  # If area fractions are missing (NA) set them to 0
  data <- set_columns_to_zero_where_na(
    data = data,
    columns = grep("roof|pvd|srf", names(data), value = TRUE)
  )

  # Calculate total area
  data[["total_area"]] <- fetch("area_main") + fetch("area_rd")

  # Convert percentages to fractions
  data <- calculate_fractions(data)

  data[["sealed"]] <- with(data, roof + pvd)

  # Get (usage, yield, irrigation) tuples based on Berlin-specific codes
  usage_types <- fetch(c("berlin_usage", "berlin_type"))

  usages <- get_usage_tuple(
    usage = usage_types[[1L]],
    type = usage_types[[2L]]
  )

  # Calculate potential evaporation for all areas
  pot_evaporation <- get_potential_evaporation(
    is_waterbody = land_type_is_waterbody(usages[["land_type"]]),
    district = fetch("district"),
    lookup = fetch_config("potential_evaporation")
  )

  # Column-bind everything together
  data <- cbind(data, usages, pot_evaporation)

  # Add a text column describing the type of block (usage)
  data[["block_type"]] <- get_block_type(usage_types)

  # Set roof area that are NAs to 0 for water bodies
  data$roof[land_type_is_waterbody(data$land_type) & is.na(data$roof)] <- 0

  # Set order of columns as defined in "column-names.csv"
  select_columns(data, get_column_selection())
}

# get_column_renamings ---------------------------------------------------------
get_column_renamings <- function()
{
  read_column_info() %>%
    dplyr::filter(nzchar(.data[["abimo_berlin"]])) %>%
    to_lookup_list(data = select_columns(., c("abimo_berlin", "rabimo_berlin")))
}

# read_column_info -------------------------------------------------------------
read_column_info <- function()
{
  "extdata/column-names.csv" %>%
    system.file(package = "kwb.rabimo") %>%
    utils::read.csv()
}

# calculate_fractions ----------------------------------------------------------
calculate_fractions <- function(data)
{
  # Column accessor
  fetch <- create_accessor(data)

  total_area <- fetch("total_area")

  # Transform percentage to fractions
  data[["main_fraction"]] <- fetch("area_main") / total_area
  data[["road_fraction"]] <- fetch("area_rd") / total_area

  # Determine names of columns that need to be divided by 100
  columns <- read_column_info() %>%
    dplyr::filter(.data[["by_100"]] == "x") %>%
    select_columns("rabimo_berlin")

  for (column in columns) {
    data[[column]] <- fetch(column) / 100
  }

  data
}

# get_block_type ---------------------------------------------------------------
get_block_type <- function(usage_types)
{
  merge_metadata <- function(data, name, by) {
    dplyr::left_join(
      x = data,
      y = utils::read.table(
        file = kwb.abimo::extdata_file(paste0(name, ".csv")),
        sep = ";",
        header = TRUE,
        fileEncoding = "WINDOWS-1252"
      ),
      by = by
    )
  }

  usage_types %>%
    merge_metadata("nutzungstypen_berlin", c(berlin_usage = "Typ_Nutzung")) %>%
    merge_metadata("strukturtypen_berlin", c(berlin_type = "Typ")) %>%
    paste_columns(sep = ": ") %>%
    subst_special_chars()
}

# get_column_selection ---------------------------------------------------------
get_column_selection <- function()
{
  read_column_info() %>%
    select_columns("rabimo_berlin") %>%
    setdiff(c("berlin_usage", "berlin_type")) %>%
    c("block_type")
}
