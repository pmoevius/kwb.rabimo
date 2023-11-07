# getPrecipitation -------------------------------------------------------------

#' Provide Information on Precipitation
#'
#' @param precipitationYear precipitation per year in mm
#' @param precipitationSummer precipitation within summer period in mm
#' @param correctionFactor correction factor
#' @return list with elements \code{perYear}, \code{inSummer}
#' @export
#' @examples
#' getPrecipitation(600, 300, 0.8)
getPrecipitation <- function(
    precipitationYear,
    precipitationSummer,
    correctionFactor
)
{
  # - Correct the (non-summer) precipitation (at ground level)
  # - No correction for summer precipitation!

  list(
    perYear = precipitationYear * correctionFactor,
    inSummer = precipitationSummer
  )
}
