# prepareInputData -------------------------------------------------------------

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
prepareInputData <- function(input_data)
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

  # Short helper functions for safe column access
  quotient <- function(a, b) select_columns(input, a) / select_columns(input, b)
  fraction_of_total <- function(x) quotient(x, "totalArea")
  by_100 <- function(x) select_columns(input, x) / 100

  # Calculate addtional columns (e.g. percentage to fraction)
  input[["totalArea"]] <- quotient("mainArea", "roadArea")
  input[["areaFractionMain"]] <- fraction_of_total("mainArea")
  input[["areaFractionRoad"]] <- fraction_of_total("roadArea")
  input[["mainFractionBuiltSealed"]] <- by_100("mainPercentageBuiltSealed")
  input[["mainFractionUnbuiltSealed"]] <- by_100("mainPercentageUnbuiltSealed")
  input[["roadFractionSealed"]] <- by_100("roadPercentageSealed")
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
  cbind(input, getUsageTuple(
    usage = select_columns(input, "berlin_usage"),
    type = select_columns(input, "berlin_type")
  ))
}
