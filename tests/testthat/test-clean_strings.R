test_that('removes extra spaces and accents', {
  x <- c(' too    many spaces  ',
         '\u00c5ccents ar\u00e9 the w\u00f4rst')

  actual <- clean_strings(x)
  
  expected <- c('too many spaces',
                'Accents are the worst')

  expect_equal(actual,
               expected)
})
