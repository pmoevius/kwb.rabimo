# getPrecipitation -------------------------------------------------------------

#' Provide Information on Precipitation
#'
#' @param precipitationYear precipitation per year in mm
#' @param precipitationSummer precipitation within summer period in mm
#' @param correctionFactor correction factor
#' @return list with elements \code{perYearInteger},
#'   \code{perYearCorrectedFloat}, \code{inSummerFloat}
#' @export
#' @examples
#' getPrecipitation(600, 300, 0.8)
getPrecipitation <- function(
    precipitationYear,
    precipitationSummer,
    correctionFactor
)
{
  list(

    # Set integer fields (originally from input dbf)
    perYearInteger = precipitationYear,

    # Set float fields:
    # - Correct the (non-summer) precipitation (at ground level)
    # - No correction for summer precipitation!
    perYearCorrectedFloat = as.double(precipitationYear * correctionFactor),
    inSummerFloat = as.double(precipitationSummer)
  )
}
