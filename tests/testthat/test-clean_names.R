test_that('reference data is required', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  expect_error(clean_names(df))

})


test_that('you need to specify the right columns', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct()

  expect_error(clean_names(df))

})


test_that('basic functionality with reference df (no fixes)', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  ref <- rio::import(testthat::test_path('data_clean_names', 'reference.RDS'))

  actual <- clean_names(df,
                        ref = ref,
                        keep_raw = TRUE,
                        require_all = FALSE)

  expected <- rio::import(testthat::test_path('data_clean_names', 'expected_no_fixes.RDS'))

  expect_equal(actual,
               expected)
})


test_that('basic functionality with reference file (no fixes)', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  actual <- clean_names(df,
                        fn_ref = testthat::test_path('data_clean_names', 'reference.RDS'),
                        keep_raw = TRUE,
                        require_all = FALSE)

  expected <- rio::import(testthat::test_path('data_clean_names', 'expected_no_fixes.RDS'))

  expect_equal(actual,
               expected)
})


test_that('works with fixes df', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  fixes <- rio::import(testthat::test_path('data_clean_names', 'fixes.RDS'))

  actual <- clean_names(df,
                        fn_ref = testthat::test_path('data_clean_names', 'reference.RDS'),
                        fixes = fixes,
                        keep_raw = TRUE,
                        require_all = FALSE)

  expected <- rio::import(testthat::test_path('data_clean_names', 'expected_with_raw.RDS'))

  expect_equal(actual,
               expected)
})


test_that('works with fixes file', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  actual <- clean_names(df,
                        fn_ref = testthat::test_path('data_clean_names', 'reference.RDS'),
                        fn_fixes = testthat::test_path('data_clean_names', 'fixes.RDS'),
                        keep_raw = TRUE,
                        require_all = FALSE)

  expected <- rio::import(testthat::test_path('data_clean_names', 'expected_with_raw.RDS'))

  expect_equal(actual,
               expected)
})


test_that('raw columns removed by default', {

  df <- drc %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  actual <- clean_names(df,
                        fn_ref = testthat::test_path('data_clean_names', 'reference.RDS'),
                        fn_fixes = testthat::test_path('data_clean_names', 'fixes.RDS'),
                        require_all = FALSE)

  expected <- rio::import(testthat::test_path('data_clean_names', 'expected_no_raw.RDS'))

  expect_equal(actual,
               expected)
})

