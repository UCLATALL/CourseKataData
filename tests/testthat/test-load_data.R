
df <- function() {
  data.frame(x = factor('a'))
}

make_test_dir <- function(dir_in_temp, envir = parent.frame()) {
  tempdir(check = TRUE)
  make_dir <- function(temp_dir, envir) {
    withr::defer(
      try(unlink(temp_dir, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir
    )

    fs::dir_create(temp_dir)
    temp_dir
  }

  temp_dir <- fs::path(tempdir(check = TRUE), as.character(dir_in_temp))
  tryCatch(
    make_dir(temp_dir, envir),
    error = function(e) {
      try(unlink(temp_dir, recursive = TRUE, force = TRUE), silent = TRUE)
      errors <- paste0('\n* ', e$message, collapse = '')
      skip(paste0("cannot create test directory", errors))
    }
  )
}

make_test_file <- function(ext, make_in = NULL, envir = parent.frame()) {
  tempdir(check = TRUE)
  make_file <- function(temp_file, envir) {
    withr::defer(
      try(unlink(temp_file, recursive = TRUE, force = TRUE), silent = TRUE),
      envir = envir
    )
    fs::file_create(temp_file)
    temp_file
  }

  # determine where the file should be created
  make_in <-
    if (is.null(make_in)) {
      tempdir(check = TRUE)
    } else if (fs::is_dir(as.character(make_in))) {
      make_in
    } else {
      # not null, but doesn't exist
      make_test_dir(make_in, envir)
    }

  test_file <- fs::file_temp(pattern = 'ckd-', tmp_dir = make_in, ext = ext)
  tryCatch(
    make_file(test_file, envir),
    error = function(e) {
      try(unlink(test_file, recursive = TRUE), silent = TRUE)
      errors <- paste0('\n* ', e$message, collapse = '')
      skip(paste0("cannot create test file", errors))
    }
  )
}

make_test_csv <- function(data, make_in = NULL, envir = parent.frame()) {
  tempdir(check = TRUE)
  test_file <- make_test_file('csv', make_in, envir)
  utils::write.csv(data, test_file, row.names = FALSE)
  test_file
}

make_test_csvs <- function(data, n, make_in = tempdir(check = TRUE), envir = parent.frame()) {
  tempdir(check = TRUE)
  purrr::map2_chr(seq_len(n), make_in, function(.x, make_in) {
    make_test_csv(data, make_in = make_in, envir = envir)
  })
}

make_test_zip <- function(dir_in_temp, files_data, files_n, files_make_in,
                          envir = parent.frame()) {
  tempdir(check = TRUE)
  make_zip <- function(dir, files_data, files_n, files_make_in, envir) {
    make_in <- fs::dir_create(fs::path(test_dir, as.character(files_make_in)))
    test_files <- make_test_csvs(files_data, files_n, make_in, envir)

    test_zip <- make_test_file('zip', envir = envir)
    zip::zipr(test_zip, test_dir, recurse = TRUE, include_directories = TRUE)
    test_zip
  }

  test_dir <- make_test_dir(dir_in_temp, envir)
  tryCatch(
    make_zip(test_dir, files_data, files_n, files_make_in, envir),
    error = function(e) {
      try(unlink(test_dir, recursive = TRUE), silent = TRUE)
      errors <- paste0('\n* ', e$message, collapse = '')
      skip(paste0("cannot create test zip", errors))
    }
  )
}



# Basics --------------------------------------------------------------------------------------

# responses.csv has 53 rows and 41 columns

test_that('data can be loaded from a vector of files to a tibble', {
  files <- class_dir(c("responses.csv", "responses.csv"))
  actual <- load_data(files)
  expect_identical(ncol(actual), 41L)
  expect_identical(nrow(actual), 53L * 2L)
})

test_that("data can be loaded from a directory with a regexp to indicate files", {
  actual <- load_data(class_dir(), "responses.csv")
  expect_identical(actual, load_data(class_dir("responses.csv")))
})

test_that("an error is thrown if the requested file cannot be found", {
  message <- 'No files were found matching the regexp/class_id combination given.'
  expect_error(load_data(data_dir('does not exist')), "does not exist or is non-readable")
  expect_error(load_data(data_dir(), regexp = 'does not exist'), message)
  expect_error(load_data(data_dir(), class_id = 'does not exist'), message)
})


# Advanced filtering --------------------------------------------------------------------------
# The file vector filtering isn't particularly useful, but the way that directories are filtered is
# by listing their files then filtering the file vector.

test_that('a file vector can be filtered by regexp when loading', {
  files <- class_dir(c("responses.csv", "classes.csv"))
  expect_identical(load_data(files, "responses"), load_data(class_dir("responses.csv")))
})

test_that('a file vector can be filtered by class_id when loading', {
  test_df <- df()
  add_prefix <- function(x) paste0('filter-id-', x)
  test_files <- make_test_csvs(test_df, 3, make_in = add_prefix(c(1, 1, 2)))

  object <- load_data(test_files, class_id = add_prefix(1))
  expected <- load_data(vctrs::vec_rep(test_df, 2))
  expect_identical(object, expected)
})

test_that('a file vector can be filtered by multiple class_ids when loading', {
  test_df <- df()
  add_prefix <- function(x) paste0('file-filter-multiple-ids-', x)
  test_files <- make_test_csvs(test_df, 3, make_in = add_prefix(c(1, 1, 2)))

  object <- load_data(test_files, class_id = add_prefix(1:2))
  expected <- load_data(vctrs::vec_rep(test_df, 3))
  expect_identical(object, expected)
})

test_that('a file vector can be filtered by class_id and regexp when loading', {
  test_df <- df()
  add_prefix <- function(x) paste0('file-filter-id-and-regexp-', x)
  test_files <- make_test_csvs(test_df, 3, add_prefix(c(1, 1, 2)))

  regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
  object <- load_data(test_files, regexp = regexp, class_id = add_prefix(1))
  expected <- load_data(test_df)
  expect_identical(object, expected)
})


# from a directory path
test_that('directories are loaded like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('dir-all-', x)
  test_file <- make_test_csv(test_df, make_in = add_prefix(1))
  test_dir <- fs::path_dir(test_file)

  object <- load_data(test_dir)
  expected <- load_data(test_df)
  expect_identical(object, expected)
})

test_that('directories are filtered like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('dir-filter-id-and-regexp-', x)
  test_files <- make_test_csvs(test_df, 3, make_in = add_prefix(c(1, 1, 2)))
  test_dir <- fs::path_dir(fs::path_dir(test_files[[1]]))

  regexp <- sprintf('.*%s$', fs::path_file(test_files[[1]]))
  expected <- load_data(test_df)
  expect_identical(load_data(test_dir, regexp = regexp), expected)
  expect_identical(load_data(test_dir, class_id = add_prefix(2)), expected)
})

test_that('directory vectors are loaded like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('dir-vector-', x)
  test_files <- make_test_csvs(test_df, 3, make_in = add_prefix(c(1, 1, 2)))
  test_dirs <- unique(fs::path_dir(test_files))

  object <- load_data(test_dirs)
  expected <- load_data(vctrs::vec_rep(test_df, 3))
  expect_identical(object, expected)
})

# from a zip file
test_that('zips are extracted and loaded like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('zip-all-', x)
  test_zip <- make_test_zip('ckd-zip-all-', test_df, 3, add_prefix(c(1, 1, 2)))

  object <- load_data(test_zip)
  expected <- load_data(vctrs::vec_rep(test_df, 3))
  expect_identical(object, expected)
})

test_that('zips are filtered like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('zip-filter-', x)
  test_zip <- make_test_zip('ckd-zip-filter-', test_df, 3, add_prefix(c(1, 1, 2)))
  zip_files <- zip::zip_list(test_zip)$filename
  first_csv <- zip_files[grepl('.*[.]csv$', zip_files)][[1]]

  regexp <- sprintf('.*%s$', fs::path_file(first_csv))
  expected <- load_data(test_df)
  expect_identical(load_data(test_zip, regexp = regexp), expected)
  expect_identical(load_data(test_zip, class_id = add_prefix(2)), expected)
})

test_that('zip vectors are extracted and loaded like file vectors', {
  test_df <- df()
  add_prefix <- function(x) paste0('zip-vector-', x)
  test_zip <- make_test_zip('ckd-zip-vector-', test_df, 1, add_prefix(1))

  object <- load_data(c(test_zip, test_zip))
  expected <- load_data(vctrs::vec_rep(test_df, 2))
  expect_identical(object, expected)
})


# from a mixed list of object types
test_that('it can load a mixed list of directories, files, and zips', {
  test_df <- df()
  add_prefix <- function(x) paste0('zip-mixed-', x)
  test_file <- make_test_csv(test_df, add_prefix(1))
  test_dir <- fs::path_dir(test_file)
  test_zip <- make_test_zip('ckd-zip-mixed-', test_df, 1, add_prefix(1))

  object <- load_data(c(test_file, test_dir, test_zip))
  expected <- load_data(vctrs::vec_rep(test_df, 3))
  expect_identical(object, expected)
})
