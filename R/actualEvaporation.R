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
  epot_per_year <- potentialEvaporation$perYearFloat

  # for water bodies return the potential evaporation
  # ??? in this test version not implemented ???
  # TODO: Check with Francesco
  if (usageTuple$usage == "waterbody_G") {
    return(epot_per_year)
  }

  # otherwise calculate the real evapotranspiration
  stopifnot(epot_per_year > 0)

  # determine the BAGROV parameter for unsealed surfaces
  bagrovParameter <- determineBagrovParameter(
    usageTuple,
    usableFieldCapacity = soilProperties$usableFieldCapacity,
    precipitationSummer = precipitation$inSummerInteger,
    potentialEvaporationSummer = potentialEvaporation$inSummerInteger,
    meanPotentialCapillaryRiseRate = soilProperties$meanPotentialCapillaryRiseRate
  )

  cat_if(log, "calculated n-value: ", bagrovParameter, "\n\n")

  xRatio <- (
    precipitation$perYearCorrectedFloat +
      soilProperties$meanPotentialCapillaryRiseRate +
      usageTuple$irrigation
  ) / epot_per_year

  result <- realEvapoTranspiration(
    potentialEvaporation = epot_per_year,
    xRatio = xRatio,
    bagrovParameter = bagrovParameter
  )

  tas <- soilProperties$potentialCapillaryRise_TAS

  if (tas < 0) {
    factor <- exp(soilProperties$depthToWaterTable / tas)
    result <- result + (epot_per_year - result) * factor;
  }

  result
}

# determineBagrovParameter (C++ name: getEffectivityParameter) -----------------
determineBagrovParameter <- function(
    usageTuple,
    usableFieldCapacity,
    precipitationSummer,
    potentialEvaporationSummer,
    meanPotentialCapillaryRiseRate
)
{
  # variable is_forest
  is_forest <- usageTuple$usage == "forested_W"

  # summer variables (they are not necessary one another's opposite)
  is_summer <- precipitationSummer > 0.0 && potentialEvaporationSummer > 0
  is_not_summer <- !(precipitationSummer > 0.0) && potentialEvaporationSummer == 0

  # g02 value
  index <- as.integer(usableFieldCapacity + 0.5) + 1L

  stopifnot(in_range(index, 1L, length(LOOKUP_G002)))

  g02 <- LOOKUP_G002[index]

  result <- if (is_forest) {
    bag0_forest(g02)
  } else {
    bag0_default(
      g02,
      usageTuple$yield,
      usageTuple$irrigation,
      is_not_summer
    )
  }

  if (is_summer) {

    # calculation of the water availability
    height <- precipitationSummer +
      usageTuple$irrigation +
      meanPotentialCapillaryRiseRate

    summer_correction_factor <- stats::approx(
      x = SUMMER_CORRECTION_MATRIX[, "water_availability"],
      y = SUMMER_CORRECTION_MATRIX[, "correction_factor"],
      xout = height/potentialEvaporationSummer,
      rule = 2L
    )$y

    result <- result * summer_correction_factor
  }

  result
}

# lookup table for the G02 value
LOOKUP_G002 <- c(
  0.0,   0.0,  0.0,  0.0,  0.3,  0.8,  1.4,  2.4,  3.7,  5.0,
  6.3,   7.7,  9.3, 11.0, 12.4, 14.7, 17.4, 21.0, 26.0, 32.0,
  39.4, 44.7, 48.0, 50.7, 52.7, 54.0, 55.0, 55.0, 55.0, 55.0,
  55.0
)

# bag0_forest ------------------------------------------------------------------
bag0_forest <- function(g02)
{
  breaks <- c(-Inf, 10.0, 25.0, Inf)
  values <- c(3.0,  4.0,  8.0)

  index <- if (length(g02) > 1L) {
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

# bag0_default -----------------------------------------------------------------
bag0_default <- function(g02, yield, irrigation, isNotSummer){

  # yield <- usageTuple$yield
  # irrigation <- usageTuple$irrigation

  result <- tableLookup_parameter(g02, yield)

  if(irrigation > 0 && isNotSummer) {
    nonSummerCorrectionFactor <- 0.9985 + 0.00284 * irrigation -
      0.00000379762 * irrigation^2
    result <- result * nonSummerCorrectionFactor
  }

  return(result)
}

# tableLookup_parameter --------------------------------------------------------
tableLookup_parameter <- function(g02, yield, do_correction = TRUE)
{
  # Calculate the k index (integer)
  k <- yield_to_k_index(yield)

  # Calculate result based on the k index
  result <- EFFECTIVITY_COEFFICIENTS[k] +
    EFFECTIVITY_COEFFICIENTS[k + 1L] * g02 +
    EFFECTIVITY_COEFFICIENTS[k + 2L] * g02^2

  # Return the result if no correction is required
  if (!do_correction) {
    return(result)
  }

  # Apply correction if needed
  if ((result >= 2.0 && yield < 60) || (g02 >= 20.0 && yield >= 60)) {
    result <- EFFECTIVITY_COEFFICIENTS[k - 2L] * g02 +
      EFFECTIVITY_COEFFICIENTS[k - 1L]
  }

  # C++ code
  #
  # float result =
  #   EFFECTIVENESS_PARAMETER_VALUES.at(k - 1) +
  #   EFFECTIVENESS_PARAMETER_VALUES.at(k) * g02 +
  #   EFFECTIVENESS_PARAMETER_VALUES.at(k + 1) * g02 * g02;
  #
  # if ((result >= 2.0 && yield < 60) || (g02 >= 20.0 && yield >= 60)) {
  #   result =
  #     EFFECTIVENESS_PARAMETER_VALUES.at(k - 3) * g02 +
  #     EFFECTIVENESS_PARAMETER_VALUES.at(k - 2);
  # }

  result
}

# yield_to_k_index -------------------------------------------------------------
yield_to_k_index <- function(yield)
{
  k <- as.integer(ifelse(yield < 50, yield / 5, yield / 10 + 5))

  # make sure that k is at least 1
  k <- max(1L, k)

  # if k is at least 4, reduce it by one
  if (k >= 4L){
    k <- k - 1L
  }

  5 * min(k, 13L) - 2L
}

# Define lookup table for effectivity (n or Bagrov-Value)
EFFECTIVITY_COEFFICIENTS <- c(
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
