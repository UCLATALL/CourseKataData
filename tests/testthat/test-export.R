
test_that("list-columns can be converted to their original JSON form", {
  df <- tibble::tibble(x = list(list(a = "named"), list(1:3)))
  df_json <- tibble::tibble(x = c('{\"a\":[\"named\"]}', "[[1,2,3]]"))

  actual <- convert_lists(df)
  expect_identical(actual, df_json)
})

test_that("the converter can swapped out for something custom", {
  df <- tibble::tibble(x = list(list(1, 2, 3), list(4, 5, 6)))
  df_commas <- tibble::tibble(x = c("1,2,3", "4,5,6"))
  to_comma <- function(x) {
    paste0(x, collapse = ",")
  }

  actual <- convert_lists(df, to_comma)
  expect_identical(actual, df_commas)
})

test_that("list columns are converted to JSON for CSV output", {
  on.exit(try(unlink(test_file, recursive = TRUE, force = TRUE), silent = TRUE))
  test_file <- fs::file_temp(pattern = "ckd-write-csv-", tmp_dir = tempdir(check = TRUE))

  df <- tibble::tibble(x = list(list(a = "named"), list(1:3)))
  write.csv(df, test_file)
  contents <- readLines(test_file)
  expect_identical(contents, c('"x"', '"{""a"":[""named""]}"', '"[[1,2,3]]"'))
})
