# getUsageTuple ----------------------------------------------------------------

#' Get Usage Tuple (Usage, Yield, Irrigation) from NUTZUNG and TYP
#'
#' @param usage value of column NUTZUNG in input data frame
#' @param type value of column TYP in input data frame
#' @return list with elements \code{usage}, \code{yield}, \code{irrigation}
#' @export
#' @examples
#' getUsageTuple(10, 10)
getUsageTuple <- function(usage, type)
{
  #usage = 10L; type = 10L
  #usage = 10L; type = 333L
  data <- data.frame(
    berlin_usage = usage,
    berlin_type = type
  )

  result <- merge(BERLIN_TYPES_TO_USAGE_YIELD_IRRIGATION, data)

  # If there was no match, try to find a default for the given berlin_usage
  if (nrow(result) != 1L) {
    data$berlin_type <- -1L
    result <- merge(BERLIN_TYPES_TO_USAGE_YIELD_IRRIGATION, data)
  }

  if (nrow(result) != 1L) {
    kwb.utils::stopFormatted(
      paste(
        "Could not find a (usage, yield, irrigation) tuple for",
        "NUTZUNG = %d, TYP = %d"
      ),
      usage,
      type
    )
  }

  as.list(kwb.utils::removeColumns(result, c("berlin_usage", "berlin_type")))
}
