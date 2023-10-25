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
#'  (e.g. ratios calculated from percentages)
#' @export
prepareInputData <- function(input_data)
{
  # Define column renamings from ABIMO 3.2 names to ABIMO 3.3 internal names
  renamings <- list(
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

  # Rename columns
  input <- rename_columns(input_data, renamings)

  # Calculate addtional columns (e.g. percentage to fraction)
  input$totalArea <- input$mainArea + input$roadArea
  input$areaFractionMain <- input$mainArea/input$totalArea
  input$areaFractionRoad <- input$roadArea/input$totalArea
  input$mainFractionBuiltSealed <- input$mainPercentageBuiltSealed / 100
  input$mainFractionUnbuiltSealed <- input$mainPercentageUnbuiltSealed / 100
  input$roadFractionSealed <- input$roadPercentageSealed / 100
  input$builtSealedFractionConnected <- input$builtSealedPercentageConnected / 100
  input$unbuiltSealedFractionSurface1 <- input$unbuiltSealedPercentageSurface1 / 100
  input$unbuiltSealedFractionSurface2 <- input$unbuiltSealedPercentageSurface2 / 100
  input$unbuiltSealedFractionSurface3 <- input$unbuiltSealedPercentageSurface3 / 100
  input$unbuiltSealedFractionSurface4 <- input$unbuiltSealedPercentageSurface4 / 100
  input$unbuiltSealedFractionConnected <- input$unbuiltSealedPercentageConnected / 100
  input$roadSealedFractionSurface1 <- input$roadSealedPercentageSurface1 / 100
  input$roadSealedFractionSurface2 <- input$roadSealedPercentageSurface2 / 100
  input$roadSealedFractionSurface3 <- input$roadSealedPercentageSurface3 / 100
  input$roadSealedFractionSurface4 <- input$roadSealedPercentageSurface4 / 100
  input$roadSealedFractionConnected <- input$roadSealedPercentageConnected / 100

  # Return the modified input data
  input
}
