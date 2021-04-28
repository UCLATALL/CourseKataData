# uses vctrs

test_that('loaded class data is in a tidy table with appropriate types', {
  classes <- process_classes(data_file('classes.csv'))
  expect_is(classes, 'data.frame')
  expect_vector(classes$class_id, character(), 2)
  expect_vector(classes$course_name, character(), 2)
  expect_vector(classes$release, character(), 2)
  expect_vector(classes$teacher_id, character(), 2)
  expect_vector(classes$lms, character(), 2)
  expect_vector(classes$setup_yaml, character(), 2)

  classes <- process_classes(data_file('classes.csv'), convert_json = TRUE)
  expect_vector(classes$setup_yaml, list(), 2)
})


# PAGE_VIEWS specific
test_that("(page_views) datetime columns are appropriately typed", {
  # this mock should have all expected datetime columns
  mock_response <- data.frame(
    dt_accessed = as.character(Sys.Date())
  )

  actual <- process_page_views(mock_response)
  expect_vector(actual$dt_accessed, vctrs::new_datetime(tzone = "UTC"))
})

test_that("(page_views) time zone can be specified for datetime columns", {
  mock_response <- data.frame(
    dt_accessed = as.character(Sys.Date())
  )

  actual <- process_page_views(mock_response, time_zone = Sys.timezone())
  expect_vector(actual$dt_accessed, vctrs::new_datetime(tzone = Sys.timezone()))
})

# MEDIA_VIEWS specific
test_that("(media_views) list columns are appropriately typed if requested", {
  # this mock should have all expected list columns
  mock_response <- data.frame(
    log_json = c("{}", "", ";")
  )

  actual <- process_media_views(mock_response)
  expect_vector(actual$log_json, character())

  actual <- process_media_views(mock_response, convert_json = TRUE)
  expect_vector(actual$log_json, list())
})

test_that("(media_views) datetime columns are appropriately typed", {
  # this mock should have all expected datetime columns
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date())
  )

  actual <- process_media_views(mock_response)
  expect_vector(actual$dt_started, vctrs::new_datetime(tzone = "UTC"))
})

test_that("(media_views) time zone can be specified for datetime columns", {
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date())
  )

  actual <- process_media_views(mock_response, time_zone = Sys.timezone())
  expect_vector(actual$dt_started, vctrs::new_datetime(tzone = Sys.timezone()))
})

test_that("numeric columns are appropriately typed if they exist", {
  # this mock should have all expected numeric columns
  mock_response <- data.frame(
    proportion_video = "1",
    proportion_time = "1"
  )

  actual <- process_media_views(mock_response)
  expect_vector(actual$proportion_video, numeric())
})


# ITEMS specific
test_that("integer columns are appropriately typed if they exist", {
  # this mock should have all expected integer columns
  mock_response <- data.frame(
    lrn_question_position = "1"
  )

  actual <- process_items(mock_response)
  expect_vector(actual$lrn_question_position, integer())
})

test_that("list columns are appropriately typed if requested", {
  # this mock should have all expected list columns
  mock_response <- data.frame(
    lrn_question_data = c("{}", "", ";")
  )

  actual <- process_items(mock_response, convert_json = TRUE)
  expect_vector(actual$lrn_question_data, list())

  actual <- process_items(mock_response)
  expect_vector(actual$lrn_question_data, character())
})

