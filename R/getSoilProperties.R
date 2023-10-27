# getSoilProperties ------------------------------------------------------------

#' Calculate Soil Properties
#'
#' @param usage usage string, one of "vegetationless_D", "waterbody_G",
#'   "horticultural_K", "agricultural_L", "forested_W"
#' @param yield yield class
#' @param depthToWaterTable depth to water table
#' @param fieldCapacity_30 field capacity in 30 cm depth
#' @param fieldCapacity_150 field capacity in 150 cm depth
#' @export
getSoilProperties <- function(
    usage,
    yield,
    depthToWaterTable,
    fieldCapacity_30,
    fieldCapacity_150
)
{
  #Initialise variables that are relevant to calculate evaporation
  result <- list()

  result$depthToWaterTable = depthToWaterTable

  # Nothing to do for waterbodies
  if (usage == "waterbody_G") {
    return(result)
  }

  # Feldkapazitaet
  result$usableFieldCapacity = estimateWaterHoldingCapacity(
    f30 = fieldCapacity_30,
    f150 = fieldCapacity_150,
    isForest = (usage == "forested_W")
  )

  # pot. Aufstiegshoehe TAS = FLUR - mittl. Durchwurzelungstiefe TWS
  # potentielle Aufstiegshoehe
  result$potentialCapillaryRise_TAS <- result$depthToWaterTable -
    getRootingDepth(usage, yield)

  # mittlere pot. kapillare Aufstiegsrate kr (mm/d) des Sommerhalbjahres
  # Kapillarer Aufstieg pro Jahr ID_KR neu, old: KR
  result$meanPotentialCapillaryRiseRate <-
    getMeanPotentialCapillaryRiseRate(
      result$potentialCapillaryRise_TAS,
      result$usableFieldCapacity,
      usage,
      yield
    )

  result
}

# estimateWaterHoldingCapacity -------------------------------------------------
estimateWaterHoldingCapacity <- function(f30, f150, isForest)
{
  if (min(f30, f150) < 1) {
    return(13.0)
  }

  if (abs(f30 - f150) < min(f30, f150)) { # unwesentliche Abweichung
    return(as.double(ifelse(isForest, f150, f30)))
  }

  0.75 * as.double(ifelse(isForest, f150, f30)) +
    0.25 * as.double(ifelse(isForest, f30, f150))
}

# getRootingDepth --------------------------------------------------------------
getRootingDepth <- function(usage, yield)
{
  if (usage == "agricultural_L") {
    return(ifelse(yield <= 50, 0.6, 0.7))
  }

  if (usage == "vegetationless_D") {
    return(0.2)
  }

  if (usage == "horticultural_K") {
    return(0.7)
  }

  if (usage == "forested_W") {
    return(1.0)
  }

  # in any other case
  0.2
}

# getMeanPotentialCapillaryRiseRate --------------------------------------------
getMeanPotentialCapillaryRiseRate <- function(
    potentialCapillaryRise,
    usableFieldCapacity,
    usage,
    yieldPower
)
{
  # potentialCapillaryRise <- 0.39
  # usableFieldCapacity <- 8.2

  M <- MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX

  kr <- ifelse(
    potentialCapillaryRise <= 0.0,
    7.0,
    M[
      helpers_index(usableFieldCapacity, attr(M, "row_values")) + 1L,
      helpers_index(potentialCapillaryRise, attr(M, "col_values")) + 1L
    ]
  )

  days_of_growth <- estimateDaysOfGrowth(usage, yieldPower)

  as.integer(round(days_of_growth * kr))
}

# MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX ----------------------------
MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX <- local({

  row_values <- c(
    8.0, 9.0, 14.0, 14.5, 15.5, 17.0, 20.5
  )

  col_values <- c(
    0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.2, 1.4, 1.7, 2.0, 2.3
  )

  result <- matrix(
    data = c(
      7.0, 6.0, 5.0, 1.5, 0.5, 0.2, 0.1, 0.0, 0.0, 0.0 , 0.0 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 5.0, 3.0, 1.2, 0.5, 0.2, 0.1, 0.0 , 0.0 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 3.0, 1.5, 0.7, 0.3, 0.15, 0.1 , 0.0, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 3.0, 2.0, 1.0, 0.7, 0.4 , 0.15, 0.1, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 4.5, 2.5, 1.5, 0.7, 0.4 , 0.15, 0.1, 0.0 , 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 5.0, 5.0, 3.5, 2.0, 1.5, 0.8 , 0.3 , 0.1, 0.05, 0.0, 0.0,
      7.0, 7.0, 6.0, 6.0, 6.0, 5.0, 5.0, 5.0, 3.0, 2.0 , 1.0 , 0.5, 0.15, 0.0, 0.0
    ),
    ncol = length(col_values),
    byrow = TRUE,
    dimnames = list(
      usable_field_capacity = as.character(row_values),
      potential_rate_of_ascent = as.character(col_values)
    )
  )

  structure(
    result,
    row_values = row_values,
    col_values = col_values
  )
})

# estimateDaysOfGrowth ---------------------------------------------------------
estimateDaysOfGrowth <- function(usage, yield, default = 50)
{
  # Special case for agricultural use
  if (usage == "agricultural_L") {
    return(ifelse(yield <= 50, 60, 75))
  }

  # Constant estimates for other uses
  days_of_growth <- list(
    vegetationless_D = 50,
    horticultural_K = 100,
    forested_W = 90
  )

  # Lookup constant estimate. Return default if use is not in list
  default_if_null(days_of_growth[[usage]], default)
}
