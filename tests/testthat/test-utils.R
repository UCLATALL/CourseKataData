test_that("is_zip_file is vectorized over path", {
  paths <- c(data_file("zipped.zip"), data_file("zipped.zip"))
  expected <- c(TRUE, TRUE) %>% rlang::set_names(paths)
  expect_equal(is_zip_file(paths), expected)
})

test_that("%notin% is the inverse of %in%", {
  expect_identical(c("a", "A") %notin% letters, !(c("a", "A") %in% letters))
})
