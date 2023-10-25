# getPotentialEvaporation -----------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param isWaterbody (vector of) logical indicating whether a block area is
#'   of type (from the type/yield/irrigation tuple) "waterbody"
#' @param district (vector of) integer indicating the district number of the
#'   plot area (from the original input column "BEZIRK")
#' @param config list structure as returned by
#'   \code{\link{abimo_config_to_config}}
#' @export
#' @examples
#' \dontrun{
#' config <- abimo_config_to_config(kwb.abimo:::read_config())
#' getPotentialEvaporation(
#'   is_waterbody = TRUE,
#'   district = 1,
#'   config = config
#' )
#' }
#'
getPotentialEvaporation <- function(isWaterbody, district, config)
{
  #`%>%` <- magrittr::`%>%`
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #abimo_config <- kwb.abimo:::read_config()

  #data <- data.frame(isWaterbody = FALSE, district = 10L)
  #data <- data.frame(isWaterbody = c(FALSE, TRUE, FALSE), district = 22:24)

  # Prepare input data for multi_column_lookup
  data <- data.frame(
    isWaterbody = isWaterbody,
    district = district
  )

  # Create lookup table from abimo configuration object
  lookup <- select_elements(config, "potentialEvaporation")

  result <- c(perYearInteger = "etp", inSummerInteger = "etps") %>%
    lapply(function(column) {
      multi_column_lookup(data, select_columns(lookup, c(names(data), column)))
    })

  result[["perYearFloat"]] <- as.double(result[["perYearInteger"]])


  if (all(lengths(result) == 1L)) {
    return(result)
  }

  as.data.frame(result)
}
