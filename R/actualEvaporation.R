# actualEvaporation ------------------------------------------------------------

#' Calculate Actual Evapotranspiration for Impervious Areas
#'
#' @param usageTuple list as returned by \code{\link{getUsageTuple}}
#' @param potentialEvaporation potential evaporation in mm
#' @param soilProperties list as returned by \code{\link{getSoilProperties}}
#' @param precipitation precipitation in mm
#' @param log logical indicating whether or not to show debug messages
#' @export
actualEvaporation <- function(
    usageTuple,
    potentialEvaporation,
    soilProperties,
    precipitation,
    log = TRUE
)
{
  epYear <- potentialEvaporation$perYearFloat

  # Initialise result vector
  y <- numeric(length = length(epYear))

  # for water bodies return the potential evaporation
  # ??? in this test version not implemented ???
  # TODO: Check with Francesco
  isWater <- (usageTuple$usage == "waterbody_G")

  y[isWater] <- epYear

  # indices of entries related to any other usage
  i <- which(!isWater)

  # otherwise calculate the real evapotranspiration
  stopifnot(all(epYear[i] > 0)) # ???

  # determine the BAGROV parameter for unsealed surfaces
  bagrovParameter <- getBagrovParameterUnsealed(
    g02 = lookupG02(soilProperties$usableFieldCapacity[i]),
    usage = usageTuple$usage[i],
    yield = usageTuple$yield[i],
    irrigation = usageTuple$irrigation[i],
    precipitationSummer = precipitation$inSummerInteger[i],
    potentialEvaporationSummer = potentialEvaporation$inSummerInteger[i],
    meanPotentialCapillaryRiseRate =
      soilProperties$meanPotentialCapillaryRiseRate[i]
  )

  cat_if(log, "calculated n-value(s): ", bagrovParameter[i], "\n\n")

  y[i] <- realEvapoTranspiration(
    potentialEvaporation = epYear[i],
    xRatio = (
      precipitation$perYearCorrectedFloat[i] +
        soilProperties$meanPotentialCapillaryRiseRate[i] +
        usageTuple$irrigation[i]
    ) / epYear[i],
    bagrovParameter = bagrovParameter
  )

  rises <- soilProperties$potentialCapillaryRise_TAS
  depths <- soilProperties$depthToWaterTable

  # indices of entries related to non-water usage and capillary rises < 0
  i <- which(!isWater & rises < 0)

  y[i] <- y[i] + (epYear[i] - y[i]) * exp(depths[i] / rises[i])

  y
}

# getBagrovParameterUnsealed (C++ name: getEffectivityParameter) ---------------
getBagrovParameterUnsealed <- function(
    g02,
    usage,
    yield,
    irrigation,
    precipitationSummer,
    potentialEvaporationSummer,
    meanPotentialCapillaryRiseRate
)
{
  # Initialise result vector
  y <- numeric(length = length(g02))

  isForest <- (usage == "forested_W")
  noForest <- !isForest

  y[isForest] <- lookupBagrovForest(g02[isForest])

  y[noForest] <- lookupBagrovUnsealed(g02[noForest], yield[noForest]) * ifelse(
    test = irrigation[noForest] > 0 & isDrySummer(
      precipitationSummer[noForest],
      potentialEvaporationSummer[noForest]
    ),
    yes = irrigationInDrySummerCorrectionFactor(irrigation[noForest]),
    no = 1
  )

  # in case of a "wet" summer, correct the BAGROV parameter with a factor
  y * ifelse(
    test = isWetSummer(precipitationSummer, potentialEvaporationSummer),
    yes = wetSummerCorrectionFactor(
      waterAvailability =
        precipitationSummer +
        irrigation +
        meanPotentialCapillaryRiseRate,
      potentialEvaporationSummer = potentialEvaporationSummer
    ),
    no = 1
  )
}

# isDrySummer ------------------------------------------------------------------
# TODO: Remove redundancy. Variables are (almost!) one another's opposite!
isDrySummer <- function(precipitationSummer, potentialEvaporationSummer)
{
  precipitationSummer <= 0 & potentialEvaporationSummer <= 0
}

# isWetSummer ------------------------------------------------------------------
# TODO: Remove redundancy. Variables are (almost!) one another's opposite!
isWetSummer <- function(precipitationSummer, potentialEvaporationSummer)
{
  precipitationSummer > 0 & potentialEvaporationSummer > 0
}

# wetSummerCorrectionFactor ----------------------------------------------------
wetSummerCorrectionFactor <- function(
    waterAvailability, potentialEvaporationSummer
)
{
  stats::approx(
    x = SUMMER_CORRECTION_MATRIX[, "water_availability"],
    y = SUMMER_CORRECTION_MATRIX[, "correction_factor"],
    xout = waterAvailability / potentialEvaporationSummer,
    rule = 2L
  )$y
}

# lookupG02 --------------------------------------------------------------------
lookupG02 <- function(usableFieldCapacity)
{
  index <- as.integer(usableFieldCapacity + 0.5) + 1L

  stopifnot(all(index %in% seq_along(LOOKUP_G02)))

  LOOKUP_G02[index]
}

# lookup table for the G02 value
LOOKUP_G02 <- c(
  0.0,   0.0,  0.0,  0.0,  0.3,  0.8,  1.4,  2.4,  3.7,  5.0,
  6.3,   7.7,  9.3, 11.0, 12.4, 14.7, 17.4, 21.0, 26.0, 32.0,
  39.4, 44.7, 48.0, 50.7, 52.7, 54.0, 55.0, 55.0, 55.0, 55.0,
  55.0
)

# lookupBagrovForest -----------------------------------------------------------
lookupBagrovForest <- function(g02)
{
  n <- length(g02)

  if (n == 0L) {
    return(numeric(0))
  }

  breaks <- c(-Inf, 10.0, 25.0, Inf)
  values <- c(3.0,  4.0,  8.0)

  index <- if (n > 1L) {
    findInterval(g02, breaks, left.open = TRUE)
  } else if (g02 <= breaks[2L]) {
    1L
  } else if (g02 <= breaks[3L]) {
    2L
  } else {
    3L
  }

  values[index]
}

# irrigationInDrySummerCorrectionFactor ----------------------------------------
irrigationInDrySummerCorrectionFactor <- function(irrigation)
{
  0.9985 + 0.00284 * irrigation - 0.00000379762 * irrigation^2
}

# lookupBagrovUnsealed ---------------------------------------------------------
lookupBagrovUnsealed <- function(g02, yield, do_correction = TRUE)
{
  # Calculate the k index (integer)
  k <- yield_to_k_index(yield)

  # Calculate result based on the k index
  y <-
    BAGROV_COEFFICIENTS[k] +
    BAGROV_COEFFICIENTS[k + 1L] * g02 +
    BAGROV_COEFFICIENTS[k + 2L] * g02^2

  # Return y if no correction is required
  if (!do_correction) {
    return(y)
  }

  # Apply correction where needed
  i <- which(
    (y >= 2.0 & yield < 60) |
      (g02 >= 20.0 & yield >= 60)
  )

  y[i] <-
    BAGROV_COEFFICIENTS[k[i] - 2L] * g02[i] +
    BAGROV_COEFFICIENTS[k[i] - 1L]

  y
}

# yield_to_k_index -------------------------------------------------------------
yield_to_k_index <- function(yield)
{
  k <- as.integer(ifelse(yield < 50, yield / 5, yield / 10 + 5))

  # make sure that k is at least 1
  k <- pmax(1L, k)

  # if k is at least 4, reduce it by one
  selected <- k >= 4L
  k[selected] <- k[selected] - 1L

  5L * pmin(k, 13L) - 2L
}

# BAGROV_COEFFICIENTS ----------------------------------------------------------

# Define lookup table for effectivity (n or Bagrov-Value)
BAGROV_COEFFICIENTS <- c(
  0.04176, -0.647 , 0.218  ,  0.01472, 0.0002089,
  0.04594, -0.314 , 0.417  ,  0.02463, 0.0001143,
  0.05177, -0.010 , 0.596  ,  0.02656, 0.0002786,
  0.05693,  0.033 , 0.676  ,  0.0279 , 0.00035  ,
  0.06162,  0.176 , 0.773  ,  0.02809, 0.0004695,
  0.06962,  0.24  , 0.904  ,  0.02562, 0.0007149,
  0.0796 ,  0.31  , 1.039  ,  0.0288 , 0.0008696,
  0.07998,  0.7603, 1.2    ,  0.0471 , 0.000293 ,
  0.08762,  1.019 , 1.373  ,  0.04099, 0.0014141,
  0.11833,  1.1334, 1.95   ,  0.0525 , 0.00125  ,
  0.155  ,  1.5   , 2.64999,  0.0725 , 0.001249 ,
  0.20041,  2.0918, 3.69999,  0.08   , 0.001999 ,
  0.33895,  3.721 , 6.69999, -0.07   , 0.013
)

# SUMMER_CORRECTION_MATRIX -----------------------------------------------------

# lookup table for the summer correction factor
SUMMER_CORRECTION_MATRIX <- matrix(
  ncol = 2L,
  byrow = TRUE,
  dimnames = list(
    NULL,
    c("water_availability", "correction_factor")
  ),
  data = c(
    0.45, 0.65,
    0.50, 0.75,
    0.55, 0.82,
    0.60, 0.90,
    0.65, 1.00,
    0.70, 1.06,
    0.75, 1.15,
    0.80, 1.22,
    0.85, 1.30,
    0.90, 1.38,
    0.95, 1.47,
    1.00, 1.55,
    1.05, 1.63,
    1.10, 1.70
  )
)
