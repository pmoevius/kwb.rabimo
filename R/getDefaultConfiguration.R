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
    )
  )
}
