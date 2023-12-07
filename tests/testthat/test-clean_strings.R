test_that('removes extra spaces and accents', {
  x <- c(' too    many spaces  ',
         '\u00c5ccents ar\u00e9 the w\u00f4rst')

  actual <- clean_strings(x)
  
  expected <- c('too many spaces',
                'Accents are the worst')

  expect_equal(actual,
               expected)
})

test_that('removes extra spaces and accents and goes to lower (if option is TRUE)', {
  x <- c(' too    many spaces  ',
         '\u00c5ccents ar\u00e9 the w\u00f4rst')

  actual <- clean_strings(x,
                          to_lower = TRUE)
  
  expected <- c('too many spaces',
                'accents are the worst')

  expect_equal(actual,
               expected)
})


test_that('accents left when transliterate is NULL', {
  x <- c(' too    many spaces  ',
         '\u00c5ccents ar\u00e9 the w\u00f4rst')

  actual <- clean_strings(x,
                          transliterate = NULL,
                          to_lower = TRUE)
  
  expected <- c('too many spaces',
                 '\u00e5ccents ar\u00e9 the w\u00f4rst')

  expect_equal(actual,
               expected)
})


test_that('correctly replaces spaces and hyphens with specified strings', {
  x <- c('this string had spaces',
         'this-string-had-hyphens')

  actual <- clean_strings(x,
                          replace_spaces = '_',
                          replace_hyphens = '#')

  expected <- c('this_string_had_spaces',
                'this#string#had#hyphens')

  expect_equal(actual,
               expected)
})


test_that('doesn\'t replace spaces and hyphens by default', {
  x <- c('this string had spaces',
         'this-string-had-hyphens')

  actual <- clean_strings(x)

  expect_equal(actual,
               x)
})


test_that('only accepts string for replace_spaces argument', {
  x <- c('this string had spaces',
         'this-string-had-hyphens')

  expect_error(clean_strings(x,
                             replace_spaces = TRUE,
                             replace_hyphens = '_'),
               'replace_spaces must be a string',
               fixed = TRUE)
})


test_that('only accepts string for replace_hyphens argument', {
  x <- c('this string had spaces',
         'this-string-had-hyphens')

  expect_error(clean_strings(x,
                             replace_spaces = '_',
                             replace_hyphens = 2),
               'replace_hyphens must be a string',
               fixed = TRUE)
})


