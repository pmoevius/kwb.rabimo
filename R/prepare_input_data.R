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
  # PARAMETERS FOR TESTING (to delete later)
  if(FALSE){
    kwb.utils::assignPackageObjects("kwb.rabimo");simulate_abimo = TRUE

    #paths
    path_amarex_ap4 <- "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4/"
    path_data_2020 <- paste0(
      path_amarex_ap4,
      "ABIMO_Daten/ISU5_2020_datengrundlage/isu5_2020_berlin/cleaned/"
    )

    file_berlin_2020 <- paste0(path_data_2020, "isu5_2020_abimo_cleaned.dbf")

    berlin_2020_data <- foreign::read.dbf(file_berlin_2020)
    berlin_2019_data <- kwb.abimo::abimo_input_2019
    input_data <- berlin_2020_data
    config <- abimo_config_to_config(kwb.abimo:::read_config())

    }

  # 1. Rename columns from ABIMO 3.2 names to ABIMO new* internal names
  # 2. Select only the columns that are required
  input <- rename_and_select(input_data, INPUT_COLUMN_RENAMINGS)

  # Create column accessor function
  fetch <- create_accessor(input)
  fetch_config <- create_accessor(config)

  # Calculate total area
  input[["total_area"]] <- fetch("area_main") + fetch("area_rd")

  # Convert percentages to fractions
  input <- calculate_fractions(input)

  input[["sealed_area"]] <- calculate_main_fraction_sealed(
    fetch("roof"),
    fetch("pvd")
  )

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

  # Set order of columns
  select_columns(input, INPUT_COLUMNS_NEEDED)

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
  FLGES = "area_main", #m2
  STR_FLGES = "area_rd", #m2
  PROBAU = "roof",
  PROVGU = "pvd",
  VGSTRASSE = "pvd_rd",
  KAN_BEB = "swg_roof",
  BELAG1 = "srf1_pvd",
  BELAG2 = "srf2_pvd",
  BELAG3 = "srf3_pvd",
  BELAG4 = "srf4_pvd",
  KAN_VGU = "swg_pvd",
  STR_BELAG1 = "srf1_pvd_rd",
  STR_BELAG2 = "srf2_pvd_rd",
  STR_BELAG3 = "srf3_pvd_rd",
  STR_BELAG4 = "srf4_pvd_rd",
  KAN_STR = "swg_pvd_rd",
  FLUR = "gw_dist",
  FELD_30 = "ufc30",
  FELD_150 = "ufc150"
)

# INPUT_COLUMNS_NEEDED ---------------------------------------------------------
INPUT_COLUMNS_NEEDED <- c("code", "total_area", "prec_yr", "prec_s",
                          "epot_yr", "epot_s", "district", "area_main",
                          "area_rd", "roof", "pvd",
                          "pvd_rd", "swg_roof", "srf1_pvd", "srf2_pvd",
                          "srf3_pvd", "srf4_pvd", "swg_pvd", "srf1_pvd_rd",
                          "srf2_pvd_rd", "srf3_pvd_rd", "srf4_pvd_rd", "swg_pvd_rd",
                          "sealed_area", "gw_dist", "ufc30", "ufc150",
                          "land_type", "veg_class", "irrigation"
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
  input[["area_main"]] <- fetch("area_main") / total_area
  input[["area_rd"]] <- fetch("area_rd") / total_area
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

# calculate_main_fraction_sealed -----------------------------------------------
calculate_main_fraction_sealed <- function(
    roof,
    pvd
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

  roof + pvd + 1e-12

}
