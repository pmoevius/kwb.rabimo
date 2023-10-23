# getDefaultConfiguration ------------------------------------------------------

#' Get ABIMO Default Configuration
#'
#' @param precipitationCorrectionFactor precipitation correction factor
#' @return list with elements
#'   precipitationCorrectionFactor,
#'   bagrovValues,
#'   runoffFactors,
#'   potentialEvaporation
#' @export
getDefaultConfiguration <- function(precipitationCorrectionFactor)
{
  list(

    precipitationCorrectionFactor = precipitationCorrectionFactor,

    bagrovValues = c(
      Dachflaechen = 0.05,
      Belaglsklasse1 = 0.11,
      Belaglsklasse2 = 0.11,
      Belaglsklasse3 = 0.25,
      Belaglsklasse4 = 0.40
    ),

    runoffFactors = 1 - c(
      Dachflaechen   = 0.00,
      Belaglsklasse1 = 0.10,
      Belaglsklasse2 = 0.30,
      Belaglsklasse3 = 0.60,
      Belaglsklasse4 = 0.90
    ),

    potentialEvaporation = list(
      district_1 = list(etp = 660, etps = 530),
      district_2 = list(etp = 650, etps = 520),
      district_3 = list(etp = 650, etps = 520),
      district_4 = list(etp = 650, etps = 520),
      district_5 = list(etp = 660, etps = 530),
      district_6 = list(etp = 660, etps = 530),
      district_7 = list(etp = 660, etps = 530),
      district_8 = list(etp = 630, etps = 505),
      district_9 = list(etp = 630, etps = 505),
      district_10 = list(etp = 630, etps = 505),
      district_11 = list(etp = 660, etps = 530),
      district_12 = list(etp = 630, etps = 505),
      district_13 = list(etp = 640, etps = 515),
      district_14 = list(etp = 640, etps = 515),
      district_15 = list(etp = 620, etps = 500),
      district_16 = list(etp = 620, etps = 500),
      district_17 = list(etp = 640, etps = 515),
      district_18 = list(etp = 620, etps = 500),
      district_19 = list(etp = 620, etps = 500),
      district_20 = list(etp = 620, etps = 500)
    )
  )
}
