context("Processing full data download")

library(fs)
library(zip)

ex_dir <- path_temp("some_path")
zip_file <- data_file("zipped.zip")

test_that("it will load all of the data and create six data frames", {
  zip::unzip(data_file("zipped.zip"), exdir = ex_dir)

  suppressWarnings(process_data(ex_dir))

  expect_identical(classes, process_classes(ex_dir))
  expect_identical(responses, suppressWarnings(process_responses(ex_dir)))
  expect_identical(page_views, process_page_views(ex_dir))
  expect_identical(media_views, process_media_views(ex_dir))
  expect_identical(items, process_items(ex_dir))
  expect_identical(tags, process_tags(ex_dir))

  rm(classes, responses, page_views, media_views, items, tags, envir = .GlobalEnv)
})

test_that("it can load from a zip file and create six data frames", {
  suppressWarnings(process_data(zip_file))

  expect_identical(classes, process_classes(zip_file))
  expect_identical(responses, suppressWarnings(process_responses(zip_file)))
  expect_identical(page_views, process_page_views(zip_file))
  expect_identical(media_views, process_media_views(zip_file))
  expect_identical(items, process_items(zip_file))
  expect_identical(tags, process_tags(zip_file))

  rm(classes, responses, page_views, media_views, items, tags, envir = .GlobalEnv)
})
