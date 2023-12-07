#' Clean Names
#'
#' @description Convenience wrapper for a pipeline of name cleaning methods using the package 
#' [hatch](https://epicentre-msf.github.io/hmatch/index.html) for hierarchical fuzzy matching. 
#' If hmatch is not installed, `clean_names()` can still take a manual fixes file and apply those
#' corrections to the dataset. **Warning:** at present, this function will necessarily return a 
#' [tidytable].
#' todo: convert to method style to preserve var type between data.frame, tibble, and tidytable
#' todo: add option to keep old names
#' todo: replace need to spec by
#' todo: make sure that files are not directories
#' two versions of 'fixes' behavior, either you give a fixes df XOR you give an external file, 
#' preference is given to the former
#' todo: let user decide to make file or not
#'
#' importFrom
clean_names <- function(df,
                        by = c('adm1', 'adm2'),
                        code_col = 'pcode',
                        #pattern = '^adm',
                        ref = NULL,
                        fn_ref = NULL, #here::here('data', 'reference', 'geo_dictionary.csv'),
                        #fixes = NULL,
                        fn_fixes = NULL,# here::here('data', 'reference', 'geo_fixes.csv'),
                        save_fixes = TRUE,
                        #create_fixes_file = FALSE,
                        keep_raw = FALSE,
                        verbose = FALSE) {

  if (is.null(ref) & (is.null(fn_ref) | !file.exists(fn_ref))) {
    stop('no reference data provided and no reference file found')
  } else if (is.null(ref)) {
    ref <- rio::import(fn_ref)
  }

  if (save_fixes) {
    if (dir.exists(fn_fixes)) {
      stop('fn_fixes must be a file, not a directory')
    }

    if (!file.exists(fn_fixes)) {
      #make_fixes_file <- menu(c('Yes, please', 'Yes, but I want to use a different filename',
                                #'No (this means your fixes will not be saved to a file)'),
                              #title = paste0(fn_fixes, 'does not seem to exist. create it ?'))

      #if (make_fixes_file == 3) {
        #break
      #}
    }


    writeLines(paste0('couldn\'nt find a a fixes file...making a new one at ', fn_fixes, '\n\n'))
    cat(paste(c(by, code_col)),
              sep = ',',
              file = fn_fixes)

    fixes <- NULL
  }

  # if no fixes file exists, then create one
  if (is.null(fixes) & (is.null(fn_fixes) | !file.exists(fn_fixes))) {
  #if (!file.exists(fn_fixes) & create_fixes_file) {
    writeLines(paste0('couldn\'nt find a a fixes file...making a new one at ', fn_fixes, '\n\n'))
    cat(paste(c(by, code_col)),
              sep = ',',
              file = fn_fixes)

    fixes <- NULL

  } else if (file.exists(fn_fixes)) {
    fixes <- rio::import(fn_fixes)

  } else {
    fixes <- NULL
  }


  tmp <- df %>%
    tidytable::select(all_of(by)) %>%
    tidytable::distinct()

  # base
  matched <- hmatch::hmatch_composite(raw = tmp,
                                      ref = ref,
                                      man = fixes,
                                      code_col = code_col,
                                      fuzzy = TRUE,
                                      type = 'resolve_inner') %>%
    tidytable::select(-match_type)

  remaining <- hmatch::hmatch_composite(raw = tmp, 
                                        ref = ref,
                                        man = fixes,
                                        code_col = code_col,
                                        fuzzy = TRUE,
                                        type = 'resolve_anti')


  # tokens
  if (nrow(remaining != 0)) {
    matched <- remaining %>% 
      hmatch::hmatch_tokens(ref = ref,
                            type = 'resolve_inner') %>%
      tidytable::bind_rows(matched)

    remaining <- remaining %>% 
      hmatch::hmatch_tokens(ref = ref,
                            type = 'resolve_anti') 

  remaining <- remaining %>%
    tidytable::select(by) %>%
    tidytable::mutate(code_col = '')

  remaining %>%
    tidytable::bind_rows(fixes) %>%
    rio::export(fn_fixes)
  }



  if (nrow(remaining != 0)) {
    stop(paste0('oh no, you need to do some fixes by hand... please update ', fn_fixes))

  } else {
    out <- df %>%
      tidytable::left_join(matched) %>%
      tidytable::rename_with(~ paste0(.x,
                                      '_raw',
                                      recycle0 = TRUE),
                             by) %>%
      tidytable::rename_with(~stringr::str_replace(.x, 'ref_', ''),
                             paste0('ref_', by))

    if (!keep_raw) {
      out <- out %>%
        tidytable::select(!any_of(paste(by, '_raw')))
    }

    if (nrow(out) != nrow(df)) {
      warning('uh oh, cleaning has resulted in duplicates...')
    }
      
    if (verbose) {
      writeLines('all names matched! go get some coffee =)')
    }

    return(out)
  }
}
