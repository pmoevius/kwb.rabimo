# expand_ranges_in_column ------------------------------------------------------
expand_ranges_in_column <- function(x, range_column, index_column)
{
  # Split data frame into a list of rows
  data_rows <- unname(split(x, seq_len(nrow(x))))

  # Expand each row with a range string in column according to the ranges
  do.call(rbind, unname(lapply(data_rows, function(data_row) {

    range_string <- select_columns(data_row, range_column)

    # Determine the indices that are described by the range string
    indices <- if (is.na(range_string)) {
      NA
    } else {
      index_string_to_integers(range_string)
    }

    # Repeat the current row for each of these indices
    result <- data_row[rep.int(1L, length(indices)), ]

    # Append a column with these indices
    result[[index_column]] <- indices

    # Return the data frame
    result
  })))
}
