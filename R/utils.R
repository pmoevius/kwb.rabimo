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

# list_to_data_frame_with_integer_keys -----------------------------------------

#' Convert List of Similar Flat Sublists to a Data Frame
#'
#' @param x list of similar flat lists, i.e. lists that have list elements with
#'   the same names and list elements that all have length one
#' @param key_name name of column in the returned data frame that will contain
#'   the integer values that are constructed from the element names in \code{x}
#' @param key_pattern regular expression matching all element names in \code{x}.
#'   The expression must contain one pair of parentheses enclosing the part that
#'   can be converted to integer, e.g. \code{"element_([0-9]+)"}
#' @return data frame with keys in a column named according to \code{key_name}
#'   and value columns according to the list elements in the sublists of
#'   \code{x}
#' @export
#' @examples
#' list_to_data_frame_with_integer_keys(
#'   x = list(
#'     element_1 = list(a = 100, b = 10),
#'     element_2 = list(a = 200, b = 20)
#'   ),
#'   key_name = "element",
#'   key_pattern = "element_([0-9]+)"
#' )
list_to_data_frame_with_integer_keys <- function(x, key_name, key_pattern)
{
  stopifnot(is.list(x), all(grepl(key_pattern, names(x))))

  result <- kwb.utils::rbindAll(lapply(x, as.data.frame), key_name)

  result[[key_name]] <- as.integer(gsub(key_pattern, "\\1", result[[key_name]]))

  result
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
