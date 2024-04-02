#' Validate requried columns
#'
#' @description Function that checks if all required columns are present in a df and optionally 
#' creates any that are missing. Optionally, you can also use a named list for the column 
#' specifications to indicate which ones are date, numeric, string, or other.
#'
#' @param df `dataframe` Dataframe to be validated.
#' @param cols `list` List of columns that must be present. If `cols` is named, any columns in the
#'   items 'date', 'numeric', and 'character' will have their type validated. In the case of a 
#'   named vector, anything that is not meant to be 'date', 'numeric', or 'character' can be placed
#'   in an element named 'other' (or something else, the name is arbitrary).
#' @param create_missing `bool` If TRUE, missing columns will be created.
#' @param select `bool` If TRUE, only the columns specified in `cols` will be returned. All columns
#'   will be returned otherwise.
#'
#' @examples
#' df <- data.frame(date = c('2022-01-01', '2022-02-01'), cases = c('152', '296'), deaths = c(1, 4))
#' cols <- list(date = 'date',  numeric = c('cases', 'deaths'), other = c('test_result'))
#' validate_columns(df, cols)
#'
#' @importFrom dplyr %>% mutate
#' @importFrom rlang .data
#' @export
validate_columns <- function(df, cols, create_missing = TRUE, select = TRUE) {

  # create columns that don't exist
  col_values <- unname(unlist(cols))

  if (create_missing) {
    df[col_values[!col_values %in% names(df)]] <- NA
  }

  # if column type informtaion is available, makes sure all columns correspond correctly
  if (!is.null(names(cols))) {

    # TODO: add intersect check to make sure each column has only one type characterization

    # i don't know why sapply isn't working here...
    if ('date' %in% names(cols)) {
      #df[ , cols$date] <- sapply(df[ , cols$date], as.Date)
      for (col in cols$date) {
        #df <- dplyr::mutate(df, rlang::.data[[col]] = as.Date(rlang::.data[[col]]))
        df <- df %>%
          mutate('{col}' := as.Date(.data[[col]]))
      }
    }

    if ('numeric' %in% names(cols)) {
      df[ , cols$numeric] <- sapply(df[ , cols$numeric], as.numeric)
    }

    if ('character' %in% names(cols)) {
      df[ , cols$character] <- sapply(df[ , cols$character], as.character)
    }
  }

  if (select) {
    df <- df[ , col_values]
  }

  return(df)
}
