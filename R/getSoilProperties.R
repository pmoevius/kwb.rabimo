# getSoilProperties ------------------------------------------------------------

#' Calculate Soil Properties
#'
#' @param usage usage string, one of "vegetationless_D", "waterbody_G",
#'   "horticultural_K", "agricultural_L", "forested_W"
#' @param yield yield class
#' @param depthToWaterTable depth to water table
#' @param fieldCapacity_30 field capacity in 30 cm depth
#' @param fieldCapacity_150 field capacity in 150 cm depth
#' @param defaultForWaterbodies value to be used for waterbodies. Default: NA
#' @export
getSoilProperties <- function(
    usage,
    yield,
    depthToWaterTable,
    fieldCapacity_30,
    fieldCapacity_150,
    defaultForWaterbodies = NA
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  if (FALSE) {
    data <- kwb.rabimo::prepareInputData(kwb.abimo::abimo_input_2019[1:10,])
    usage <- data$usage
    yield <- data$yield
    depthToWaterTable <- data$depthToWaterTable
    fieldCapacity_30 <- data$fieldCapacity_30
    fieldCapacity_150 <- data$fieldCapacity_150
  }

  #Initialise variables that are relevant to calculate evaporation
  result <- list()

  result$depthToWaterTable <- depthToWaterTable

  # Nothing to do for waterbodies
  isWaterbody <- usage == "waterbody_G"

  # Feldkapazitaet
  result$usableFieldCapacity <- ifelse(
    test = isWaterbody,
    yes = defaultForWaterbodies,
    no = estimateWaterHoldingCapacity(
      f30 = fieldCapacity_30,
      f150 = fieldCapacity_150,
      isForest = (usage == "forested_W")
    )
  )

  # pot. Aufstiegshoehe TAS = FLUR - mittl. Durchwurzelungstiefe TWS
  # potentielle Aufstiegshoehe
  result$potentialCapillaryRise_TAS <- ifelse(
    test = isWaterbody,
    yes = defaultForWaterbodies,
    no = result$depthToWaterTable - getRootingDepth(usage, yield)
  )

  # mittlere pot. kapillare Aufstiegsrate kr (mm/d) des Sommerhalbjahres
  # Kapillarer Aufstieg pro Jahr ID_KR neu, old: KR
  result$meanPotentialCapillaryRiseRate <- ifelse(
    test = isWaterbody,
    yes = defaultForWaterbodies,
    no = getMeanPotentialCapillaryRiseRate(
      result$potentialCapillaryRise_TAS,
      result$usableFieldCapacity,
      daysOfGrowth = estimateDaysOfGrowth(usage, yield)
    )
  )

  result
}

# estimateWaterHoldingCapacity -------------------------------------------------
estimateWaterHoldingCapacity <- function(f30, f150, isForest)
{
  n <- length(f30)

  stopifnot(length(f150) == n)
  stopifnot(length(isForest) == n)

  # Initialise result vector with the default result
  y <- numeric(n)

  # Smaller value of f30, f150 at each index
  min_capacity <- pmin(f30, f150)

  # Special case 1: smaller value below 1
  todo <- min_capacity < 1
  y[todo] <- 13.0

  # Special case 2: minor difference
  todo <- !todo & abs(f30 - f150) < min_capacity
  y[todo] <- as.double(ifelse(isForest[todo], f150[todo], f30[todo]))

  # Default result for the non-special cases
  todo <- is.na(y)

  y[todo] <- ifelse(isForest[todo],
    0.25 * f30[todo] + 0.75 * f150[todo],
    0.75 * f30[todo] + 0.25 * f150[todo]
  )

  y
}

# getRootingDepth --------------------------------------------------------------
getRootingDepth <- function(usage, yield)
{
  n <- length(usage)
  stopifnot(length(yield) == n)

  y <- rep(NA_real_, n)

  todo <- usage == "agricultural_L"
  y[todo] <- ifelse(yield[todo] <= 50, 0.6, 0.7)

  y[usage == "vegetationless_D"] <- 0.2
  y[usage == "horticultural_K"] <- 0.7
  y[usage == "forested_W"] <- 1.0

  # in any other case
  y[is.na(y)] <- 0.2

  y
}

# getRootingDepth_1 --------------------------------------------------------------
getRootingDepth_1 <- function(usage, yield)
{
  stopifnot(length(usage) == 1L)
  stopifnot(length(yield) == 1L)

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
    daysOfGrowth
)
{
  # potentialCapillaryRise <- 0.39
  # usableFieldCapacity <- 8.2

  n <- length(potentialCapillaryRise)
  stopifnot(length(usableFieldCapacity) == n)
  stopifnot(length(daysOfGrowth) == n)

  M <- MEAN_POTENTIAL_CAPILLARY_RISE_RATES_SUMMER_MATRIX

  indices <- cbind(
    helpers_index(usableFieldCapacity, attr(M, "row_values")) + 1L,
    helpers_index(potentialCapillaryRise, attr(M, "col_values")) + 1L
  )

  kr <- ifelse(potentialCapillaryRise <= 0.0, 7.0, M[indices])

  as.integer(round(daysOfGrowth * kr))
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
estimateDaysOfGrowth <- function(usage, yield, default = 50L)
{
  n <- length(usage)

  stopifnot(length(yield) == n)

  # Initialise result vector
  y <- rep(NA_integer_, n)

  # Special case for agricultural use
  isAgricultural <- usage == "agricultural_L"
  y[isAgricultural] <- ifelse(yield[isAgricultural] <= 50, 60L, 75L)

  # Constant estimates for other uses
  y[usage == "vegetationless_D"] <- 50L
  y[usage == "horticultural_K"] <- 100L
  y[usage == "forested_W"] <- 90L

  # Return default for any other use
  y[is.na(y)] <- default

  y
}

# estimateDaysOfGrowth_1 -------------------------------------------------------
estimateDaysOfGrowth_1 <- function(usage, yield, default = 50)
{
  stopifnot(length(usage) == 1L)
  stopifnot(length(yield) == 1L)

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
