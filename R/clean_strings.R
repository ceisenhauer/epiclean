#' Clean Strings
#'
#' @description Cleans strings to squish unwanted spaces and translate non-ascii characters. 
#' TODO: Optionally translate to lower, upper, or title case. Optionally replace spaces and hyphens
#' with underscores.
#' 
#' @param x `chr vct` Character vector to be cleaned.
#' @param transliterate `chr vct` String or character vector of transliterations; passed directly
#'   to shtringi stri_trans_general.
#' @param to_lower `bool` If TRUE, strings will be converted to lower case. Default is FALSE.
#' @param replace_spaces `chr` String that will replace spaces (unless NULL). Default is NULL, ie:
#'   spcaes will not be replaced.
#' @param replace_hyphens `chr` String that will replace hyphens (unless NULL). Default is NULL, ie:
#'   hyphens will not be replaced.
#'
#' @examples
#'  x <- c(' too    many spaces  ',
#'         '\u00c5ccents ar\u00e9 the w\u00f4rst')
#'
#' @importFrom stringi stri_trans_general
#' @importFrom stringr str_replace str_squish
#' @export
clean_strings <- function(x, transliterate = 'Latin-ASCII', to_lower = FALSE,
                          replace_spaces = NULL, replace_hyphens = NULL) {
  x <- stringr::str_squish(x)

  if (!is.null(transliterate)) {
    x <- stringi::stri_trans_general(x, transliterate)
  }

  if (to_lower) {
    x <- tolower(x)
  }

  if (!is.null(replace_spaces)) {
    if (!is.character(replace_spaces)) {
      stop('replace_spaces must be a string')
    }

    x <- x %>% 
      stringr::str_replace_all(' ', replace_spaces)
  }

  if (!is.null(replace_hyphens)) {
    if (!is.character(replace_hyphens)) {
      stop('replace_hyphens must be a string')
    }

    x <- x %>% 
      stringr::str_replace_all('-', replace_hyphens)
  }

  return(x)
}
