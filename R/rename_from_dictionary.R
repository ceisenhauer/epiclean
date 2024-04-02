#' Rename columns of a dataframe using a dictionary
#'
#' @description Uses a dictionary either from a file or named list to rename the relevant columns of
#' a dataframe.
#'
#' @param df `dataframe` Dataframe to be renamed
#' @param dictionary `char` Named character vector where the names are the new column names and the 
#'   values are the old ones. 
#' @param dictionary_fn `char` String indicating the location of a file containing the renaming
#'   dictionary as a dataframe (for exmaple in an csv or xlsx). By default, the function will
#'   look for columns specified be `col_old` and `col_new`, if these are not found in the file
#'   provided, it will use the first and second columns respectively. If `dictionary` is provided,
#'   this argument will be ignored.
#' @param col_old `chr` If importing a dictionary from `dictionary_fn`, `col_old` specifies which
#'   column contains the old (original) column names. Default is 'original_name'. If this column
#'   is not found in `dictionary_fn`, `rename_from_dicitonary()` will assume the first column
#'   contains the old names.
#' @param col_new `chr` If importing a dictionary from `dictionary_fn`, `col_new` specifies which
#'   column contains the new column names to be used. Default is 'new_name'. If this column
#'   is not found in `dictionary_fn`, `rename_from_dicitonary()` will assume the second column
#'   contains the new names.
#' @param ... Additional arguments to be passed to [rio::import()] when loading `dictionary_fn`. 
#'   This could be useful, for example  if you have stored the renaming dictionaries for multiple
#'   datasets in a single xlsx where each sheet coresponds to a separate dictionary.
#'
#' @examples
#' df <- data.frame(fizz = c(1, 2, 3),
#'                  buzz = c('one', 'two', 'three'))
#' 
#' dictionary <- c(foo = 'fizz',
#'                 bar = 'buzz')
#' 
#' rename_from_dictionary(df, dictionary)
#' 
#' @importFrom rio import
#' @importFrom dplyr rename
#' @export
rename_from_dictionary <- function(df, dictionary = NULL, dictionary_fn = NULL, 
                                   col_old = 'original_name', col_new = 'new_name', ...) {
  if (is.null(dictionary) & is.null(dictionary_fn)) {
    stop('you must provide either a list or filename with the renaming dictionary to be used...')
  }

  if (!is.null(dictionary) & !is.null(dictionary_fn)) {
    warning('both `dictionary` and `dictionary_fn` provided...using `dictionary`.')
    dictionary_fn <- NULL
  }

  if (!is.null(dictionary_fn)) {
    dictionary_df <- rio::import(dictionary_fn,
                                 ...)

    if (!is.data.frame(dictionary_df)) {
      stop(paste0('`names_fn` must contain a dataframe but i found a ', class(dictionary_df),
                  'instead...'))
    }

    cols <- names(dictionary_df)
    if (!(col_old %in% cols & col_new %in% cols)) {
      cols[1] <- col_old
      cols[2] <- col_new
    }

    dictionary_df <- dictionary_df[ , c(col_new, col_old)]

    dictionary <- dictionary_df$original_name
    names(dictionary) <- dictionary_df$new_name
  }

  out <- dplyr::rename(df, !!dictionary)

  return(out)
}
    

