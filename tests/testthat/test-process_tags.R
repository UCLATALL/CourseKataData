context("Processing tags")

library(dplyr)
library(fs)
library(zip)
library(vctrs)

filename <- "tags.csv"
filepath <- data_file(filename)
unprocessed <- read.csv(filepath, stringsAsFactors = FALSE)
processed <- process_tags(read.csv(filepath, stringsAsFactors = FALSE))

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
  expect_is(process_tags(data.frame()), "tbl_df")
})

test_that("all non-explicitly-typed columns are converted to character", {
  mock_response <- data.frame(
    some_variable = factor(1)
  )

  expect_vectors_in_df(
    process_tags(mock_response),
    names(mock_response),
    character()
  )
})


# Tests: Reading from file ------------------------------------------------

test_that("data can be read from a specific file", {
  expect_identical(process_tags(filepath), processed)
})

test_that("data can be loaded from a course zip file", {
  expect_identical(process_tags(zip_file), processed)
})

test_that("data can be loaded from a directory", {
  expect_identical(
    process_tags(top_dir),
    bind_rows(processed, processed)
  )
})

test_that("data can be processed from a specific class in a directory", {
  expect_identical(
    process_tags(top_dir, class_id = "class_1"),
    processed
  )
})
