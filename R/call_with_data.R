# call_with_data ---------------------------------------------------------------

#' Call a Function with Argument Combinations from a Data Frame
#'
#' @param FUN function to be called
#' @param data data frame with one column per argument of \code{FUN}
#' @param \dots further arguments passed to \code{\link{mapply}}
#' @param threshold if the ratio of unique value combinations in the relevant
#'   columns in data to all value combinations in these columns is below this
#'   threshold value then FUN will be called only with the unique value
#'   combinations. This should increase performance.
#' @return vector of length \code{nrow(data)} with the result values returned by
#'   \code{FUN}
#' @importFrom methods formalArgs
#' @export
#' @examples
#' combis <- expand.grid(x = 1:2, y = c(10, 20, 30))
#' combis
#'
#' call_with_data(`+`, combis)
call_with_data <- function(
    FUN,
    data,
    ...,
    threshold = 0.5
)
{
  # What arguments does FUN have?
  all_args <- methods::formalArgs(FUN)

  # Select the columns from data that are arguments of FUN
  arg_data <- select_columns(data, intersect(all_args, names(data)))

  # Split arg_data into sets of identical rows
  sets <- split_into_identical_rows(arg_data)

  # Number of all value combinations
  n_all <- nrow(arg_data)

  # Number of unique value combinations
  n_unique <- length(sets)

  # Should we run FUN only for the unique value combinations?
  run_unique <- (n_unique / n_all < threshold)

  # Name of the function to be called
  fun_name <- deparse(substitute(FUN))

  # Run FUN for each row of run_data
  results <- cat_and_run(
    messageText = if (run_unique) {
      sprintf(
        "-> Calling %s() for %d unique value combinations",
        fun_name, n_unique
      )
    } else {
      sprintf(
        "-> Calling %s() for all %d value combinations",
        fun_name, n_all
      )
    },
    expr = do.call(
      what = mapply,
      args = c(
        list(FUN = FUN),
        if (run_unique) {
          remove_columns(rbind_first_rows(sets), "row.")
        } else {
          arg_data
        },
        list(...)
      )
    )
  )

  if (!run_unique) {
    return(results)
  }

  cat_and_run(
    "-> Expanding the results to the extent of the input",
    expand_to_vector(
      x = results,
      indices = lapply(sets, select_columns, "row.")
    )
  )
}
