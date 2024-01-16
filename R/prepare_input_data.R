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
#'   \code{abimo_config_to_config()} used on \code{kwb.abimo:::read_config()}
#' @return \code{input_data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, (main) usage, yield, irrigation)
#' @export
prepare_input_data <- function(input_data, config)
{
  # PARAMETERS FOR TESTING
  # kwb.utils::assignPackageObjects("kwb.rabimo");simulate_abimo = TRUE
  # input_data <- kwb.abimo::abimo_input_2019
  # input_data <- berlin_2020_data
  # config <- abimo_config_to_config(kwb.abimo:::read_config())

  # 1. Rename columns from ABIMO 3.2 names to ABIMO new* internal names
  # 2. Select only the columns that are required
  input <- rename_and_select(input_data, INPUT_COLUMN_RENAMINGS)

  # Create column accessor function
  fetch <- create_accessor(input)
  fetch_config <- create_accessor(config)

  # Calculate total area
  input[["total_area"]] <- fetch("main_area") + fetch("rd_area")

  # Convert percentages to fractions
  input <- calculate_fractions(input)

  input[["sealed_area"]] <- calculate_main_fraction_sealed(
    fetch("roof_area"),
    fetch("paved_area")
  )

  # Get (usage, yield, irrigation) tuples based on Berlin-specific codes
  usages <- get_usage_tuple(
    usage = fetch("berlin_usage"),
    type = fetch("berlin_type")
  )

  # Calculate potential evaporation for all areas
  pot_evaporation <- get_potential_evaporation(
    is_waterbody = usage_is_waterbody(usages[["usage"]]),
    district = fetch("district"),
    lookup = fetch_config("potential_evaporation")
  )

  select_columns(input, INPUT_COLUMNS_NEEDED)

  as.data.frame(names(input))

  # Column-bind everything together
  input <- cbind(input, usages, pot_evaporation)
}


# INPUT_COLUMN_RENAMINGS -------------------------------------------------------
INPUT_COLUMN_RENAMINGS_OLD <- list(
  CODE = "code",
  REGENJA = "precipitationYear",
  REGENSO = "precipitationSummer",
  NUTZUNG = "berlin_usage",
  TYP = "berlin_type",
  BEZIRK = "district",
  FLGES = "mainArea",
  STR_FLGES = "roadArea",
  PROBAU = "mainPercentageBuiltSealed",
  PROVGU = "mainPercentageUnbuiltSealed",
  VGSTRASSE = "roadPercentageSealed",
  KAN_BEB = "builtSealedPercentageConnected",
  BELAG1 = "unbuiltSealedPercentageSurface1",
  BELAG2 = "unbuiltSealedPercentageSurface2",
  BELAG3 = "unbuiltSealedPercentageSurface3",
  BELAG4 = "unbuiltSealedPercentageSurface4",
  KAN_VGU = "unbuiltSealedPercentageConnected",
  STR_BELAG1 = "roadSealedPercentageSurface1",
  STR_BELAG2 = "roadSealedPercentageSurface2",
  STR_BELAG3 = "roadSealedPercentageSurface3",
  STR_BELAG4 = "roadSealedPercentageSurface4",
  KAN_STR = "roadSealedPercentageConnected",
  FLUR = "depthToWaterTable",
  FELD_30 = "fieldCapacity_30",
  FELD_150 = "fieldCapacity_150"
)

INPUT_COLUMN_RENAMINGS <- list(
  CODE = "code",
  REGENJA = "prec_yr",
  REGENSO = "prec_s",
  NUTZUNG = "berlin_usage",
  TYP = "berlin_type",
  BEZIRK = "district",
  FLGES = "main_area",
  STR_FLGES = "rd_area",
  PROBAU = "roof_area",
  PROVGU = "paved_area",
  VGSTRASSE = "rd_pvd_area",
  KAN_BEB = "swg_roof_area",
  BELAG1 = "pvm_1",
  BELAG2 = "pvm_2",
  BELAG3 = "pvm_3",
  BELAG4 = "pvm_4",
  KAN_VGU = "swg_pvd_area",
  STR_BELAG1 = "rd_pvm_1",
  STR_BELAG2 = "rd_pvm_2",
  STR_BELAG3 = "rd_pvm_3",
  STR_BELAG4 = "rd_pvm_4",
  KAN_STR = "swg_rd_area",
  FLUR = "gw_dist",
  FELD_30 = "ufc_30",
  FELD_150 = "ufc_150"
)

# INPUT_COLUMNS_NEEDED ---------------------------------------------------------
INPUT_COLUMNS_NEEDED <- c("code", "total_area", "prec_yr", "prec_s",
                             "epot_yr", "epot_s", "district", "main_area",
                             "rd_area", "roof_area", "paved_area",
                             "rd_pvd_area", "swg_roof_area", "pvm_1", "pvm_2",
                             "pvm_3", "pvm_4", "swg_pvd_area", "rd_pvm_1",
                             "rd_pvm_2", "rd_pvm_3", "rd_pvm_4", "swg_rd_area",
                             "sealed_area", "gw_dist", "ufc_30", "ufc_150",
                             "usage", "yield", "irrigation"
)

# INPUT_COLUMNS_NOT_NEEDED -----------------------------------------------------
INPUT_COLUMNS_NOT_NEEDED <- setdiff(
  names(kwb.abimo::abimo_input_2019),
  INPUT_COLUMNS_NEEDED
)

# calculate_fractions ----------------------------------------------------------
calculate_fractions <- function(input)
{
  # Column accessor
  fetch <- create_accessor(input)

  # Helper function to select column and divide by 100
  by_100 <- function(x) fetch(x) / 100

  total_area <- fetch("total_area")

  # Transform percentage to fractions
  input[["main_area"]] <- fetch("main_area") / total_area
  input[["rd_area"]] <- fetch("rd_area") / total_area
  input[["roof_area"]] = by_100("roof_area")
  input[["paved_area"]] = by_100("paved_area")
  input[["rd_pvd_area"]] = by_100("rd_pvd_area")
  input[["swg_roof_area"]] = by_100("swg_roof_area")
  input[["swg_pvd_area"]] = by_100("swg_pvd_area")
  input[["pvm_1"]] = by_100("pvm_1")
  input[["pvm_2"]] = by_100("pvm_2")
  input[["pvm_3"]] = by_100("pvm_3")
  input[["pvm_4"]] = by_100("pvm_4")
  input[["swg_rd_area"]] = by_100("swg_rd_area")
  input[["rd_pvm_1"]] = by_100("rd_pvm_1")
  input[["rd_pvm_2"]] = by_100("rd_pvm_2")
  input[["rd_pvm_3"]] = by_100("rd_pvm_3")
  input[["rd_pvm_4"]] = by_100("rd_pvm_4")

  input
}

# calculate_main_fraction_sealed -----------------------------------------------
calculate_main_fraction_sealed <- function(
    roof_area,
    paved_area
)
{
  # Calculate the percentage of built and unbuilt sealed areas. Add a small
  # value to round .5 "up" not "down":
  # round(98.5) -> 98
  # round(98.5 + 1e-12) -> 99

  # === Code in C++:
  # vgd = (dbReader.getRecord(k, "PROBAU")).toFloat() / 100.0F; // Dachflaechen
  # vgb = (dbReader.getRecord(k, "PROVGU")).toFloat() / 100.0F; // Hofflaechen
  # ptrDA.VER = (int)round((vgd * 100) + (vgb * 100));

  roof_area + paved_area + 1e-12

}
