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
      select_columns(data, given_columns)
    )
  )
}

# cat_if -----------------------------------------------------------------------
#' @importFrom kwb.utils catIf
cat_if <- kwb.utils::catIf

# helpers_index ----------------------------------------------------------------
helpers_index <- function(x, values)
{
  which.min(abs(x - values)) - 1L
}

# index_string_to_integers -----------------------------------------------------
#' Convert String of Integer Ranges to Vector of Integer
#'
#' Convert e.g. "1,4-6,10-11,20" to c(1L, 4L, 5L, 6L, 10L, 11L, 20L)
#'
#' @param x vector of character of length one representing a string
#'   of integer ranges, e.g. \code{"1,4-6,10-11,20"}
#' @param splits characters at which to 1. split \code{x} into range strings, 2.
#'  split the range strings into begin and end values of the ranges. Default:
#'  \code{c(",", "-")}
#' @return vector of integer
#' @export
#' @examples
#' index_string_to_integers("1,4-6,10-11,20")
#'
index_string_to_integers <- function(x, splits = c(",", "-"))
{
  do.call(c, lapply(strsplit(x, splits[1L])[[1L]], function(range_string) {
    from_to <- strsplit(range_string, splits[2L])[[1L]]
    seq.int(
      as.integer(from_to[1L]),
      as.integer(from_to[1L + (length(from_to) > 1L)]),
      by = 1L
    )
  }))
}

# list_to_data_frame_with_keys -------------------------------------------------

#' Convert List of Similar Flat Sublists to a Data Frame
#'
#' @param x list of similar flat lists, i.e. lists that have list elements with
#'   the same names and list elements that all have length one
#' @param key_name name of column in the returned data frame that will contain
#'   the integer values that are constructed from the element names in \code{x}
#' @param key_pattern regular expression matching all element names in \code{x}.
#'   The expression must contain one pair of parentheses enclosing the part that
#'   is to be used as key, e.g. \code{"element_([0-9]+)"}
#' @param convert function to be applied to the (character) key. Set e.g.
#'   \code{convert = as.integer} to generate integer keys. Default:
#'   \code{\link{identity}}
#' @return data frame with keys in a column named according to \code{key_name}
#'   and value columns according to the list elements in the sublists of
#'   \code{x}
#' @export
#' @examples
#' list_to_data_frame_with_keys(
#'   x = list(
#'     element_1 = list(a = 100, b = 10),
#'     element_2 = list(a = 200, b = 20)
#'   ),
#'   key_name = "element",
#'   key_pattern = "element_([0-9]+)",
#'   convert = as.integer
#' )
list_to_data_frame_with_keys <- function(
    x, key_name, key_pattern, convert = identity
)
{
  elements <- names(x)

  stopifnot(is.list(x), all(grepl(key_pattern, elements)))

  result <- safe_row_bind_all(lapply(x, as.data.frame))

  result[[key_name]] <- convert(gsub(key_pattern, "\\1", elements))

  move_columns_to_front(result, key_name)
}

# move_columns_to_front --------------------------------------------------------
#' @importFrom kwb.utils moveColumnsToFront
move_columns_to_front <- kwb.utils::moveColumnsToFront

# multi_column_lookup ----------------------------------------------------------
#' @importFrom kwb.utils multiColumnLookup
multi_column_lookup <- kwb.utils::multiColumnLookup

# print_if ---------------------------------------------------------------------
#' @importFrom kwb.utils printIf
print_if <- kwb.utils::printIf

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

# remove_columns ---------------------------------------------------------------
#' @importFrom kwb.utils removeColumns
remove_columns <- kwb.utils::removeColumns

# rename_columns ---------------------------------------------------------------
#' @importFrom kwb.utils renameColumns
rename_columns <- kwb.utils::renameColumns

# safe_row_bind_all ------------------------------------------------------------
#' @importFrom kwb.utils safeRowBindAll
safe_row_bind_all <- kwb.utils::safeRowBindAll

# select_columns ---------------------------------------------------------------
#' @importFrom kwb.utils selectColumns
select_columns <- kwb.utils::selectColumns

# select_elements --------------------------------------------------------------
#' @importFrom kwb.utils selectElements
select_elements <- kwb.utils::selectElements

# stop_formatted ---------------------------------------------------------------
#' @importFrom kwb.utils stopFormatted
stop_formatted <- kwb.utils::stopFormatted
