test_that('drc core works', {

  df <- drc %>%
    tidytable::filter(MALADIE == 'PALUDISME SUSP') %>%
    tidytable::select(prov, zs) %>%
    tidytable::distinct() %>%
    tidytable::rename(adm1 = prov,
                      adm2 = zs)

  actual <- clean_names(df)

  expect_equal(actual,
               expected)
})

