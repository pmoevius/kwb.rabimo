# prepare_input_data -----------------------------------------------------------

#' Prepare Input Data: Rename, Add Columns
#'
#' Rename columns from ABIMO 3.2 original names to ABIMO 3.3 internal names
#'
#' @param input_data data frame with columns REGENJA, REGENSO, NUTZUNG, TYP,
#'   BEZIRK, FLGES, STR_FLGES, PROBAU, PROVGU, VGSTRASSE, KAN_BEB, BELAG1,
#'   BELAG2, BELAG3, BELAG4, KAN_VGU, STR_BELAG1, STR_BELAG2, STR_BELAG3,
#'   STR_BELAG4, KAN_STR, FLUR, FELD_30, FELD_150
#' @return \code{input_data} with columns renamed and additional columns
#'  (e.g. ratios calculated from percentages, (main) usage, yield, irrigation)
#' @export
prepare_input_data <- function(input_data)
{
  # Rename columns from ABIMO 3.2 names to ABIMO 3.3 internal names
  input <- rename_columns(input_data, list(
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
  ))

  # Calculate the percentage of built and unbuild sealed areas. Add a small
  # value to round .5 "up" not "down":
  # round(98.5) -> 98
  # round(98.5 + 1e-12) -> 99

  # === Code in C++:
  # vgd = (dbReader.getRecord(k, "PROBAU")).toFloat() / 100.0F; // Dachflaechen
  # vgb = (dbReader.getRecord(k, "PROVGU")).toFloat() / 100.0F; // Hofflaechen
  # ptrDA.VER = (int)round((vgd * 100) + (vgb * 100));

  input[["mainPercentageSealed"]] <- as.integer(round(
    select_columns(input, "mainPercentageBuiltSealed") +
      select_columns(input, "mainPercentageUnbuiltSealed") +
      1e-12
  ))

  # Helper function to select column and divide by 100
  by_100 <- function(x) select_columns(input, x) / 100

  # Calculate additional columns (e.g. percentage to fraction)
  main_area <- select_columns(input, "mainArea")
  road_area <- select_columns(input, "roadArea")
  total_area <-  main_area + road_area

  input[["totalArea"]] <- total_area
  input[["areaFractionMain"]] <- select_columns(input, "mainArea") / total_area
  input[["areaFractionRoad"]] <- select_columns(input, "roadArea") / total_area

  input[["mainFractionBuiltSealed"]] <- by_100("mainPercentageBuiltSealed")
  input[["mainFractionUnbuiltSealed"]] <- by_100("mainPercentageUnbuiltSealed")
  input[["mainFractionSealed"]] <- by_100("mainPercentageSealed")
  input[["roadFractionRoadSealed"]] <- by_100("roadPercentageSealed")
  input[["builtSealedFractionConnected"]] <- by_100("builtSealedPercentageConnected")
  input[["unbuiltSealedFractionSurface1"]] <- by_100("unbuiltSealedPercentageSurface1")
  input[["unbuiltSealedFractionSurface2"]] <- by_100("unbuiltSealedPercentageSurface2")
  input[["unbuiltSealedFractionSurface3"]] <- by_100("unbuiltSealedPercentageSurface3")
  input[["unbuiltSealedFractionSurface4"]] <- by_100("unbuiltSealedPercentageSurface4")
  input[["unbuiltSealedFractionConnected"]] <- by_100("unbuiltSealedPercentageConnected")
  input[["roadSealedFractionSurface1"]] <- by_100("roadSealedPercentageSurface1")
  input[["roadSealedFractionSurface2"]] <- by_100("roadSealedPercentageSurface2")
  input[["roadSealedFractionSurface3"]] <- by_100("roadSealedPercentageSurface3")
  input[["roadSealedFractionSurface4"]] <- by_100("roadSealedPercentageSurface4")
  input[["roadSealedFractionConnected"]] <- by_100("roadSealedPercentageConnected")

  # Add the "usage tuple" as columns
  cbind(input, get_usage_tuple(
    usage = select_columns(input, "berlin_usage"),
    type = select_columns(input, "berlin_type")
  ))
}
