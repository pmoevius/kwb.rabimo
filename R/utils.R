# call_with_data ---------------------------------------------------------------

#' Call a Function with Argument Combinations from a Data Frame
#'
#' @param FUN function to be called
#' @param data data frame with one column per argument of \code{FUN}
#' @param \dots further arguments passed to \code{\link{mapply}} via
#'   \code{MoreArgs}
#' @param simplify passed to \code{\link{mapply}}
#' @return vector of length \code{nrow(data)} with the result values returned by
#'   \code{FUN}
#' @importFrom methods formalArgs
#' @export
#' @examples
#' combis <- expand.grid(x = 1:2, y = c(10, 20, 30))
#' combis
#'
#' call_with_data(`+`, combis)
call_with_data <- function(FUN, data, ..., simplify = TRUE)
{
  more_args <- list(...)

  given_columns <- setdiff(methods::formalArgs(FUN), names(more_args))

  #sapply(seq_len(nrow(data)), function(i) do.call(FUN, arg_data[i, ]))
  do.call(
    mapply,
    args = c(
      list(
        FUN = FUN,
        MoreArgs = more_args,
        SIMPLIFY = simplify
      ),
      kwb.utils::selectColumns(data, given_columns)
    )
  )
}

# helpers_index ----------------------------------------------------------------
helpers_index <- function(x, values)
{
  which.min(abs(x - values)) - 1L
}

# range_to_seq -----------------------------------------------------------------

#' Sequence of Values Between the Range of Values in a Given Vector
#'
#' @param x vector of values from which to take the range
#' @param by increment of seqence
#' @return sequence of values between \code{min(x)} and \code{max(x)} with
#'   increment \code{by}
range_to_seq <- function(x, by = 1)
{
  do.call(seq, c(as.list(range(x)), list(by = by)))
}
