# prepare_input_data -----------------------------------------------------------

#' Prepare Input Data: Rename, Add Columns
#'
#' Rename columns from ABIMO 3.2 original names to ABIMO 3.3 internal names
#'
#' @param input_data data frame with columns CODE, REGENJA, REGENSO, NUTZUNG,
#'   TYP, BEZIRK, FLGES, STR_FLGES, PROBAU, PROVGU, VGSTRASSE, KAN_BEB, BELAG1,
#'   BELAG2, BELAG3, BELAG4, KAN_VGU, STR_BELAG1, STR_BELAG2, STR_BELAG3,
#'   STR_BELAG4, KAN_STR, FLUR, FELD_30, FELD_150
#' @param config configuration object (list) as returned by the function
#'   \code{abimo_config_to_config()} used on \code{kwb.abimo::read_config()}
#' @return \code{input_data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, (main) usage, yield, irrigation)
#' @export
prepare_input_data <- function(input_data, config)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  #
  # See inst/extdata/test-rabimo.R for test data assignments
  #

  # 1. Rename columns from ABIMO 3.2 names to ABIMO new* internal names
  # 2. Select only the columns that are required
  input <- rename_columns(input_data, renamings = get_column_renamings())

  # Create column accessor function
  fetch <- create_accessor(input)
  fetch_config <- create_accessor(config)

  # correct precipitation with correction factor from the config file
  input[["prec_yr"]] <- fetch("prec_yr") *
    fetch_config("precipitation_correction_factor")

  # If area fractions are missing (NA) set them to 0
  input <- set_columns_to_zero_where_na(
    data = input,
    columns = grep("roof|pvd|srf", names(input), value = TRUE)
  )

  # Calculate total area
  input[["total_area"]] <- fetch("area_main") + fetch("area_rd")

  # Convert percentages to fractions
  input <- calculate_fractions(input)

  input[["sealed"]] <- with(input, roof + pvd + 1e-14) # do we still need this?

  # Get (usage, yield, irrigation) tuples based on Berlin-specific codes
  usages <- get_usage_tuple(
    usage = fetch("berlin_usage"),
    type = fetch("berlin_type")
  )

  # Calculate potential evaporation for all areas
  pot_evaporation <- get_potential_evaporation(
    is_waterbody = usage_is_waterbody(usages[["land_type"]]),
    district = fetch("district"),
    lookup = fetch_config("potential_evaporation")
  )

  as.data.frame(names(input))

  # Column-bind everything together
  input <- cbind(input, usages, pot_evaporation)

  # Set roof area that are NAs to 0 for water bodies
  selected <- usage_is_waterbody(input$land_type) & is.na(input$roof)
  input$roof[selected] <- 0

  # Set order of columns as defined in "column-names.csv"
  select_columns(input, get_column_selection())
}

# get_column_renamings ---------------------------------------------------------
get_column_renamings <- function()
{
  read_column_info() %>%
    dplyr::filter(nzchar(.data[["abimo_berlin"]])) %>%
    to_lookup_list(data = rev(.)) # Revert column order
}

# read_column_info -------------------------------------------------------------
read_column_info <- function()
{
  "extdata/column-names.csv" %>%
    system.file(package = "kwb.rabimo") %>%
    utils::read.csv()
}

# calculate_fractions ----------------------------------------------------------
calculate_fractions <- function(input)
{
  # Column accessor
  fetch <- create_accessor(input)

  # Helper function to select column and divide by 100
  by_100 <- function(x) fetch(x) / 100

  total_area <- fetch("total_area")

  # Transform percentage to fractions
  input[["main_fraction"]] <- fetch("area_main") / total_area
  input[["road_fraction"]] <- fetch("area_rd") / total_area
  input[["roof"]] = by_100("roof")
  input[["pvd"]] = by_100("pvd")
  input[["pvd_rd"]] = by_100("pvd_rd")
  input[["swg_roof"]] = by_100("swg_roof")
  input[["swg_pvd"]] = by_100("swg_pvd")
  input[["srf1_pvd"]] = by_100("srf1_pvd")
  input[["srf2_pvd"]] = by_100("srf2_pvd")
  input[["srf3_pvd"]] = by_100("srf3_pvd")
  input[["srf4_pvd"]] = by_100("srf4_pvd")
  input[["swg_pvd_rd"]] = by_100("swg_pvd_rd")
  input[["srf1_pvd_rd"]] = by_100("srf1_pvd_rd")
  input[["srf2_pvd_rd"]] = by_100("srf2_pvd_rd")
  input[["srf3_pvd_rd"]] = by_100("srf3_pvd_rd")
  input[["srf4_pvd_rd"]] = by_100("srf4_pvd_rd")

  input
}

# get_column_selection ---------------------------------------------------------
get_column_selection <- function()
{
  read_column_info() %>%
    select_columns("rabimo_berlin") %>%
    setdiff(c("berlin_usage", "berlin_type"))
}
