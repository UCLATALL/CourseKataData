test_that("it converts JSON strings without emitting errors if there were problems", {
  input <- c('{"a": 1}', '{')
  expect_identical(safe_convert_json(input), list(list(a = 1L), list()))
})

test_that("it can parse a character vector as a numeric vector", {
  input <- c("1", "-1", "1.1")
  expect_identical(parse_integer(input), c(1L, -1L, 1L))
  expect_identical(parse_double(input), c(1.0, -1, 1.1))
})

test_that("it drops non-numeric characters when parsing as a number", {
  input <- "a-1.1"
  expect_identical(parse_double(input), -1.1)
})

test_that("it will warn about NAs when parsing non-numeric things that look like numbers", {
  input <- "1.1.1"
  parse_double(input) %>%
    expect_identical(NA_real_) %>%
    expect_warning("NAs introduced by coercion")
})

test_that("it parses date-times with the format YYYY-MM-DD hh:mm:ss.sss", {
  input <- "2019-09-25 22:21:31.000"
  expected <- as.POSIXct(input, "UTC")
  expect_identical(parse_datetime(input), expected)
})

test_that("it converts to the desired time zone", {
  input <- "2019-09-25 22:21:31.000"
  expected <- as.POSIXct(input, "UTC")
  actual <- parse_datetime(input, "America/Los_Angeles")
  expect_identical(actual, expected, ignore_attr = TRUE)
  expect_snapshot(actual)
})
