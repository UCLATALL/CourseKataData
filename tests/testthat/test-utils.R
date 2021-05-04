test_that("is_zip_file is vectorized over path", {
  paths <- c(data_dir("zipped.zip"), data_dir("zipped.zip"))
  expected <- c(TRUE, TRUE) %>% rlang::set_names(paths)
  expect_equal(is_zip_file(paths), expected)
})
