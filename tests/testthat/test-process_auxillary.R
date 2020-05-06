# uses vctrs

test_that('loaded class data is in a tidy table with appropriate types', {
  classes <- process_classes(data_file('classes.csv'))
  expect_is(classes, 'data.frame')
  expect_vectors_in_df(
    classes,
    c('class_id', 'course_name', 'release', 'teacher_id', 'lms'),
    character(), '2'
  )
  expect_vectors_in_df(classes, 'setup_yaml', list(), '2')
})


# PAGE_VIEWS specific
test_that("(page_views) datetime columns are appropriately typed", {
  # this mock should have all expected datetime columns
  mock_response <- data.frame(
    dt_accessed = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_page_views(mock_response),
    names(mock_response),
    vctrs::new_datetime(tzone = "UTC")
  )
})

test_that("(page_views) time zone can be specified for datetime columns", {
  mock_response <- data.frame(
    dt_accessed = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_page_views(mock_response, time_zone = Sys.timezone()),
    names(mock_response),
    vctrs::new_datetime(tzone = Sys.timezone())
  )
})

# MEDIA_VIEWS specific
test_that("(media_views) list columns are appropriately typed if they exist", {
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

test_that("(media_views) datetime columns are appropriately typed", {
  # this mock should have all expected datetime columns
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_media_views(mock_response),
    names(mock_response),
    vctrs::new_datetime(tzone = "UTC")
  )
})

test_that("(media_views) time zone can be specified for datetime columns", {
  mock_response <- data.frame(
    dt_started = as.character(Sys.Date())
  )

  expect_vectors_in_df(
    process_media_views(mock_response, time_zone = Sys.timezone()),
    names(mock_response),
    vctrs::new_datetime(tzone = Sys.timezone())
  )
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


# ITEMS specific
test_that("integer columns are appropriately typed if they exist", {
  # this mock should have all expected integer columns
  mock_response <- data.frame(
    lrn_question_position = "1"
  )

  expect_vectors_in_df(
    process_items(mock_response),
    names(mock_response),
    integer()
  )
})

test_that("list columns are appropriately typed if they exist", {
  # this mock should have all expected list columns
  mock_response <- data.frame(
    lrn_question_data = c("{}", "", ";")
  )

  expect_vectors_in_df(
    process_items(mock_response),
    names(mock_response),
    list()
  )
})

