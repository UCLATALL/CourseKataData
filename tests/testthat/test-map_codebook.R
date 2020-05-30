test_responses <- sample(codebook_1.6[, 'lrn_question_reference'])
test_responses[['release']] <- '1.6'

test_that('the function returns a tibble with `var_name`', {
  test_responses <- codebook_1.6[, 'lrn_question_reference']
  actual <- map_codebook(test_responses, release = '1.6')
  expect_true(tibble::is_tibble(actual))
  expect_vector(actual$var_name, ptype = character(), nrow(test_responses))
})

test_that('`var_name` maps item references to variable names', {
  actual <- map_codebook(test_responses, release = '1.6')
  expect_identical(actual$var_name, codebook_1.6$var_name)
})

test_that('the release will be guessed from `release` if not specified', {
  actual <- map_codebook(test_responses)
  expect_identical(actual$var_name, codebook_1.6$var_name)
})

test_that('a error is thrown if the release cannot be guessed', {
  test_responses <- test_responses[, names(test_responses) != 'release']
  expect_error(map_codebook(test_responses), "Unable to guess which codebook to use.")
})

test_that('an error is thrown if the relevant codebook cannot be found', {
  expect_error(map_codebook(test_responses, 'does-not-exist'))
})

test_that('mapping is associated with a message to inspect the codebook', {
  # Take a look at the codebook(s) for more information about the survey
  # items. Codebooks for these releases were used for these data:
  # - 1.6
  # Type codebook_<release> to view the relevant codebook, e.g. "codebook_1.6"
  # expect_message(map_codebook(test_responses), NA)
  expect_message(map_codebook(test_responses))
})

test_that('quiet mode suppresses errors and messages for automatic actions', {
  expect_message(map_codebook(test_responses, quiet = TRUE), NA)

  test_responses <- test_responses[, names(test_responses) != 'release']
  expect_error(map_codebook(test_responses, quiet = TRUE), NA)
})
