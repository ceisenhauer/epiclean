#' Clean Names
#'
#' @description Convenience wrapper for a pipeline of name cleaning methods using the package 
#' [hatch](https://epicentre-msf.github.io/hmatch/index.html) for hierarchical fuzzy matching. 
#' If hmatch is not installed, `clean_adm_names()` can still take a manual fixes file and apply
#' thosecorrections to the dataset. 
#' todo: convert to method style to preserve var type between data.frame, tibble, and tidytable
#'
#' @param df `dataframe` Dataframe to be checked.
#' @param by `chr vct` Column name(s) in `df` to be checked against `ref`. Default is to look for 
#'   columns called `adm1` and `adm2`.
#' @param code_col `chr` Column in `ref` to be used as the code column, for example for matching to 
#'   a `fixes` dataset. Default is `pcode`.
#' @param ref `dataframe` Dataframe of reference data. **NOTE**, at least one of `ref` or `fn_ref`
#'   *must* be provided. If both are provided only `ref` is considered.
#' @param fn_ref `chr` Path to a set of reference data. **NOTE**, at least one of `ref` or `fn_ref`
#'   *must* be provided. If both are provided only `ref` is considered.
#' @param fixes `dataframe` Dataframe of manual fixes. This dataframe *must* contain the a code 
#'   column with the appropriate name (as specified by `code_col`).
#' @param fn_fixes `chr` Path to set of manual fixes data. This data *must* contain the a code 
#'   column with the appropriate name (as specified by `code_col`).
#' @param save_unmatched `bool` If TRUE, unmatched values will be saved to `fn_fixes` for the user
#'   to provide manual fixes. Default behavior is for this to happen whenever `fn_fixes` is not NULL
#'   and not otherwise.
#' @param require_all `bool` If TRUE, `clean_names()` will return an error if it fails to find a
#'   match for all values. Default is TRUE.
#' @param keep_raw `bool` If TRUE, the original unmatched data will be retained in the returned
#'   output. The relevant columns will have "_raw" appended to their names. Default is FALSE.
#'
#' @importFrom rio import export
#' @importFrom stringr str_replace
#' @importFrom dplyr all_of anti_join any_of bind_rows distinct inner_join mutate rename_with select
#' @export
clean_names <- function(df,
                        by = c('adm1', 'adm2'),
                        code_col = 'pcode',
                        ref = NULL,
                        fn_ref = NULL,
                        fixes = NULL,
                        fn_fixes = NULL,
                        save_unmatched = ifelse(is.null(fn_fixes), FALSE, TRUE),
                        require_all = TRUE,
                        keep_raw = FALSE) {

  tmp <- df |>
    dplyr::select(dplyr::all_of(by)) |>
    dplyr::distinct()


  # WRANGLE REFERENCE AND FIXES DATA ---------------------------------------------------------------
  # get reference data, prioritize ref over fn_ref
  if (is.null(ref) && is.null(fn_ref)) {
    stop('you must provide reference data via either a dataframe (`ref`) or a file (`fn_ref`)')

  } else if (is.null(ref)) {
    ref <- rio::import(fn_ref) |>
      dplyr::select(dplyr::all_of(c(by, code_col))) |>
      dplyr::distinct()
  }


  # load data in fn_fixes (if there is any) and then row bind the data in fixes (if there is any)
  if (!is.null(fn_fixes) && file.exists(fn_fixes)) {
    fixes <- rio::import(fn_fixes) |>
      dplyr::bind_rows(fixes)
  }


  # RUN WITHOUT HTMATCH (IF NOT INSTALLED) ---------------------------------------------------------
  if (!requireNamespace('hmatch', quietly = TRUE)) {
    warning(paste0('this function is designed to use {hmatch}, consider installing it using ',
                   '`devtools::install_github("epicentre-msf/hmatch")`. without {hmatch}, ',
                   '`clean_names()` will consider manual fix data only.'))

    matched <- dplyr::inner_join(tmp, ref)
    remaining <- dplyr::anti_join(tmp, ref)


  # RUN MATCHING WITH HMATCH (IF INSTALLED) --------------------------------------------------------
  # preform core matching using hmatch composite
  } else {
    matched <- hmatch::hmatch_composite(raw = tmp,
                                        ref = ref,
                                        man = fixes,
                                        code_col = code_col,
                                        fuzzy = TRUE,
                                        type = 'resolve_inner') |>
      dplyr::select(-any_of('match_type'))

    remaining <- hmatch::hmatch_composite(raw = tmp, 
                                          ref = ref,
                                          man = fixes,
                                          code_col = code_col,
                                          fuzzy = TRUE,
                                          type = 'resolve_anti')


    # if there are still unmated values, try tokens
    if (nrow(remaining != 0)) {
      matched <- remaining |> 
        hmatch::hmatch_tokens(ref = ref,
                              type = 'resolve_inner') |>
        dplyr::bind_rows(matched)

      remaining <- remaining |> 
        hmatch::hmatch_tokens(ref = ref,
                              type = 'resolve_anti') 
    }
  }


  # REPORT AND (OPTIONALLY SAVE) UNMATCHED VALUES --------------------------------------------------
  # if there are STILL unmatched values, tell user to update fixes
  # if save_unmatched is true, the user can update fn_fixes, otherwise fixes must be updated inline
  if (nrow(remaining != 0)) {
    remaining <- remaining |>
      dplyr::select(dplyr::all_of(by)) |>
      dplyr::mutate(code_col := '')

    writeLines(paste0('oh no, unable to find matches for the following values:'))
    print(remaining)

    if (save_unmatched) {
      writeLines(paste0('unmated values added to fixes file; please add your manual fixes to: ',
                        fn_fixes))

      remaining |>
        dplyr::bind_rows(fixes) |>
        rio::export(fn_fixes)
    }

    if (require_all) {
      stop('not all values matched, please add manual fixes and try again.')
    }

  } else {
    writeLines('all names matched! go get some coffee =)')
  }


  # RETURN DF WITH MATCH RESULTS -------------------------------------------------------------------
  out <- df |>
    dplyr::left_join(matched) |>
    dplyr::rename_with(~ paste0(.x,
                                '_raw',
                                recycle0 = TRUE),
                       dplyr::all_of(by)) |>
    dplyr::rename_with(~stringr::str_replace(.x, 'ref_', ''),
                       dplyr::all_of(paste0('ref_', by)))

  if (!keep_raw) {
    out <- out |>
      dplyr::select(!dplyr::any_of(paste0(by, '_raw')))
  }

  if (nrow(out) != nrow(df)) {
    stop('oh no, matching resulted in duplicates. check for contradictory fixes and try again.')
  }
    
  return(out)
}
