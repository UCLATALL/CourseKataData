context("Processing media views")

library(dplyr)
library(fs)
library(zip)
library(vctrs)

filename <- "media_views.csv"
filepath <- data_file(filename)
unprocessed <- read.csv(filepath, stringsAsFactors = FALSE)
processed <- process_media_views(read.csv(filepath, stringsAsFactors = FALSE))

# single in temp zip
fs::file_copy(filepath, path_temp(filename))
zip_file <- file_temp(ext = ".zip")
zipr(zip_file, path_temp(filename))

# multiple in temp dir
top_dir <- path_temp("data_download")
class_dirs <- dir_create(path(top_dir, paste0("class_", 1:2)))
file_copy(rep(filepath, 2), class_dirs)


# Tests: Type conversion --------------------------------------------------

test_that("the object is a tibble", {
  expect_is(process_media_views(data.frame()), "tbl_df")
})

test_that("numeric columns are appropriately typed if they exist", {
  # this mock should have all expected numeric columns
  mock_response <- data.frame(
    proportion_video = "1",
    proportion_time = "1"
  )

  expect_vectors_in_df(
    process_media_views(mock_response),
    names(mock_response),
    numeric()
  )
})

test_that("datetime columns are appropriately typed if they exist", {
  # this mock should have all expected datetime columns
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date()),
    dt_last_event = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_media_views(mock_response),
    names(mock_response),
    new_datetime(tzone = "UTC")
  )
})

test_that("datetime columns can be read-in as a specific time zone", {
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_media_views(mock_response, time_zone = Sys.timezone()),
    names(mock_response),
    new_datetime(tzone = Sys.timezone())
  )
})

test_that("list columns are appropriately typed if they exist", {
  # this mock should have all expected list columns
  mock_response <- data.frame(
    log_json = c("{}", "", ";")
  )

  expect_vectors_in_df(
    process_media_views(mock_response),
    names(mock_response),
    list()
  )
})

test_that("all non-explicitly-typed columns are converted to character", {
  mock_response <- data.frame(
    some_variable = factor(1)
  )

  expect_vectors_in_df(
    process_media_views(mock_response),
    names(mock_response),
    character()
  )
})


# Tests: Reading from file ------------------------------------------------

test_that("data can be read from a specific file", {
  expect_identical(process_media_views(filepath), processed)
})

test_that("data can be loaded from a course zip file", {
  expect_identical(process_media_views(zip_file), processed)
})

test_that("data can be loaded from a directory", {
  expect_identical(
    process_media_views(top_dir),
    bind_rows(processed, processed)
  )
})

test_that("data can be processed from a specific class in a directory", {
  expect_identical(
    process_media_views(top_dir, class_id = "class_1"),
    processed
  )
})
