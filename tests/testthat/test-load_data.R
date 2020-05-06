
test_df <- data.frame(x = factor('a'))
test_dfs <- function(n = length(test_files)) {
  vctrs::vec_recycle(test_df, n)
}

test_dir <- fs::path_temp('coursekatadata')
test_class_dirs <- fs::dir_create(fs::path(test_dir, 1:2), recurse = TRUE)
test_files <- fs::file_temp(
  tmp_dir = c(test_class_dirs[[1]], test_class_dirs),
  ext = 'csv'
)
purrr::walk2(test_df, test_files, write.csv, row.names = FALSE)

test_zip <- fs::file_temp(ext = 'zip')
zip::zipr(test_zip, test_dir)

# from a data frame
test_that('loading a data frame converts it to a normalized tibble', {
  object <- load_data(test_df)
  expect_true(tibble::is_tibble(object))
  expect_vector(object$x, character(), 1)
})

# from a file path
test_that('data can be loaded from a single file', {
  object <- load_data(test_files[[1]])
  expected <- load_data(test_df)
  expect_identical(object, expected)
})

test_that('data can be loaded from a file vector', {
  object <- load_data(test_files)
  expected <- load_data(test_dfs())
  expect_identical(object, expected)
})

# filtering arguments
test_that('an error is thrown if no files match filter arguments', {
  message <- 'No files were found matching the regexp/class_id combination given.'
  expect_error(load_data(test_files, regexp = 'does not exist'), message)
  expect_error(load_data(test_files, class_id = 'does not exist'), message)
})

test_that('a file vector can be filtered by regexp when loading', {
  regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
  object <- load_data(test_files, regexp = regexp)
  expected <- load_data(test_df)
  expect_identical(object, expected)
})

# test_that('a file vector can be filtered by class_id when loading', {
#   object <- load_data(test_files, class_id = 1)
#   expected <- load_data(test_dfs(2))
#   expect_identical(object, expected)
# })

test_that('a file vector can be filtered by multiple class_ids when loading', {
  object <- load_data(test_files, class_id = 1:2)
  expected <- load_data(test_dfs(3))
  expect_identical(object, expected)
})

test_that('a file vector can be filtered by class_id and regexp when loading', {
  regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
  object <- load_data(test_files, regexp = regexp, class_id = 1)
  expected <- load_data(test_df)
  expect_identical(object, expected)
})


# from a directory path
test_that('directories are loaded like file vectors', {
  object <- load_data(test_dir)
  expected <- load_data(test_dfs())
  expect_identical(object, expected)
})

test_that('directories are filtered like file vectors', {
  regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
  expected <- load_data(test_dfs(1))
  expect_identical(load_data(test_dir, regexp = regexp), expected)
  expect_identical(load_data(test_dir, class_id = 2), expected)
})

test_that('directory vectors are loaded like file vectors', {
  object <- load_data(test_class_dirs)
  expected <- load_data(test_dfs())
  expect_identical(object, expected)
})

# from a zip file
test_that('zips are extracted and loaded like file vectors', {
  object <- load_data(test_zip)
  expected <- load_data(test_dfs())
  expect_identical(object, expected)
})

# test_that('zips are filtered like file vectors', {
#   regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
#   expected <- load_data(test_dfs(1))
#   expect_identical(load_data(test_zip, regexp = regexp), expected)
#   expect_identical(load_data(test_zip, class_id = 2), expected)
# })

test_that('zip vectors are extracted and loaded like file vectors', {
  object <- load_data(c(test_zip, test_zip))
  expected <- load_data(vctrs::vec_c(test_dfs(), test_dfs()))
  expect_identical(object, expected)
})


# from a mixed list of object types
test_that('it can load a mixed list of directories, files, and zips', {
  object <- load_data(c(test_files, test_dir, test_zip))
  expected <- load_data(vctrs::vec_c(test_dfs(), test_dfs(), test_dfs()))
  expect_identical(object, expected)
})
