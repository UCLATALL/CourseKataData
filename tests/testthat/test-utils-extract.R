test_that("is_zip_file is vectorized over path", {
  paths <- c(data_dir("zipped.zip"), data_dir("zipped.zip"))
  expected <- c(TRUE, TRUE) %>% rlang::set_names(paths)
  expect_identical(is_zip_file(paths), expected)
})

test_that("dir_to_files returns all files (not directories) in nested directories", {
  nested_files <- c("items", "media_views", "page_views", "responses", "tags")
  paths <- c(
    fs::path("./data/unzipped/classes/class_1", nested_files, ext = "csv"),
    "./data/unzipped/classes.csv"
  )
  expect_identical(as.character(dir_to_files("./data/unzipped")), paths)
})
