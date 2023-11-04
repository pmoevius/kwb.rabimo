# actualEvaporationWaterbodyOrPervious -----------------------------------------

#' Calculate Actual Evapotranspiration for Waterbodies or Pervious Areas
#'
#' @param usageTuple list as returned by \code{\link{getUsageTuple}}
#' @param potentialEvaporation potential evaporation in mm
#' @param soilProperties list as returned by \code{\link{getSoilProperties}}
#' @param precipitation precipitation in mm
#' @param dbg logical indicating whether or not to show debug messages
#' @param \dots further arguments passed to \code{\link{realEvapoTranspiration}}
#'   such as \code{runParallel}, \code{blocksize}
#' @param digits optional. If given, the BAGROV parameter values are rounded to
#'   this number of digits. This reduces the number of BAGROV curves that need
#'   to be calculated and thus improves the performance (by reducing the
#'   precision of the output)
#' @export
actualEvaporationWaterbodyOrPervious <- function(
    usageTuple,
    potentialEvaporation,
    soilProperties,
    precipitation,
    dbg = TRUE,
    ...,
    digits = NULL
)
{
  epYear <- potentialEvaporation$perYearFloat

  # Initialise result vector
  y <- numeric(length = length(epYear))

  # for water bodies return the potential evaporation
  # ??? in this test version not implemented ???
  # TODO: Check with Francesco
  isWater <- (usageTuple$usage == "waterbody_G")

  y[isWater] <- epYear[isWater]

  # if all block areas are waterbodies, return
  if (all(isWater)) {
    return(y)
  }

  # indices of entries related to any other usage
  i <- which(!isWater)

  # otherwise calculate the real evapotranspiration
  stopifnot(all(epYear[i] > 0)) # ???

  # determine the BAGROV parameter(s) for unsealed surfaces
  bagrovParameter <- getBagrovParameterUnsealed(
    g02 = soilProperties$g02[i],
    usage = usageTuple$usage[i],
    yield = usageTuple$yield[i],
    irrigation = usageTuple$irrigation[i],
    precipitationSummer = precipitation$inSummerInteger[i],
    potentialEvaporationSummer = potentialEvaporation$inSummerInteger[i],
    meanPotentialCapillaryRiseRate =
      soilProperties$meanPotentialCapillaryRiseRate[i]
  )

  if (!is.null(digits)) {
    bagrovParameter <- cat_and_run(
      sprintf("Rounding BAGROV parameters to %d digits", digits),
      round(bagrovParameter, digits)
    )
  }

  cat_if(dbg, sprintf(
    "Range of calculated %sn-value(s): %s\n",
    ifelse(is.null(digits), "", "and rounded "),
    paste(range(bagrovParameter), collapse = " - ")
  ))

  y[i] <- realEvapoTranspiration(
    potentialEvaporation = epYear[i],
    xRatio = (
      precipitation$perYearCorrectedFloat[i] +
        soilProperties$meanPotentialCapillaryRiseRate[i] +
        usageTuple$irrigation[i]
    ) / epYear[i],
    bagrovParameter = bagrovParameter,
    ...
  )

  rises <- soilProperties$potentialCapillaryRise_TAS
  depths <- soilProperties$depthToWaterTable

  # indices of entries related to non-water usage and capillary rises < 0
  j <- which(!isWater & rises < 0)

  y[j] <- y[j] + (epYear[j] - y[j]) * exp(depths[j] / rises[j])

  naVector <- rep(NA_real_, length(y))

  structure(y, bagrovUnsealed = data.frame(
    bagrovEff = `[<-`(naVector, i, bagrovParameter),
    factorDry = `[<-`(naVector, i, get_attribute(bagrovParameter, "factorDry")),
    factorWet = `[<-`(naVector, i, get_attribute(bagrovParameter, "factorWet"))
  ))
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

  factorDry <- ifelse(
    test = irrigation > 0 & isDrySummer(
      precipitationSummer,
      potentialEvaporationSummer
    ),
    yes = irrigationInDrySummerCorrectionFactor(irrigation[noForest]),
    no = 1
  )

  y[noForest] <- lookupBagrovUnsealed(g02[noForest], yield[noForest]) *
    factorDry[noForest]

  # in case of a "wet" summer, correct the BAGROV parameter with a factor
  factorWet <- ifelse(
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

  structure(
    y * factorWet,
    factorDry = factorDry,
    factorWet = factorWet
  )
}

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

# Coefficients for linear or squared equations used to calculate the BAGROV
# parameters
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

# isDrySummer ------------------------------------------------------------------
# TODO: Remove redundancy with isWetSummer.
# Variables are (almost!) one another's opposite!
isDrySummer <- function(precipitationSummer, potentialEvaporationSummer)
{
  precipitationSummer <= 0 & potentialEvaporationSummer <= 0
}

# irrigationInDrySummerCorrectionFactor ----------------------------------------
irrigationInDrySummerCorrectionFactor <- function(irrigation)
{
  0.9985 + 0.00284 * irrigation - 0.00000379762 * irrigation^2
}

# isWetSummer ------------------------------------------------------------------
# TODO: Remove redundancy with isDrySummer.
# Variables are (almost!) one another's opposite!
isWetSummer <- function(precipitationSummer, potentialEvaporationSummer)
{
  precipitationSummer > 0 & potentialEvaporationSummer > 0
}

# wetSummerCorrectionFactor ----------------------------------------------------
wetSummerCorrectionFactor <- function(
    waterAvailability, potentialEvaporationSummer, useAbimoApprox = TRUE
)
{
  xout <- waterAvailability / potentialEvaporationSummer
  x <- WET_SUMMER_CORRECTION_MATRIX[, "water_availability"]
  y <- WET_SUMMER_CORRECTION_MATRIX[, "correction_factor"]

  if (useAbimoApprox) {
    stats::approx(x = x, y = y, xout = xout, rule = 2L)$y
  } else {
    interpolate(x = x, y = y, xout = xout)
  }
}

# WET_SUMMER_CORRECTION_MATRIX -------------------------------------------------
WET_SUMMER_CORRECTION_MATRIX <- matrix(
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
