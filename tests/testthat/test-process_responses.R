mock_response_looktbl <- data.frame(
  student_id = 1,
  prompt = "text",
  lrn_question_reference = c(1, 2, 1, 3),
  lrn_type = c("mcq", "plaintext", "mcq", "mcq"),
  response = c('["1"]', '["2"]', '["0", "1"]', "[]"),
  lrn_option_0 = c("Yes", 1, "Yes", "50"),
  lrn_option_1 = c("No", 2, "No", "60"),
  lrn_option_2 = c(NA, "Three", NA, "70")
)

mock_responses_integration <- data.frame(
  class_id = 1,
  student_id = 1,
  prompt = 1,
  response = 1,
  lrn_type = 1,
  lrn_question_reference = 1
)

top_dir <- fs::path(tempdir(check = TRUE), "data_download")
class_dir <- fs::path(top_dir, "classes", c("class_1", "class_2")) %>% fs::dir_create()

resp_file_1 <- fs::path(class_dir[[1]], "responses", ext = "csv")
resp_file_2 <- fs::path(class_dir[[2]], "responses", ext = "csv")
utils::write.csv(mock_responses_integration, resp_file_1, row.names = FALSE)
utils::write.csv(mock_responses_integration, resp_file_2, row.names = FALSE)

zip_file <- fs::file_temp(pattern = "ckd-responses-", ext = ".zip")
zip::zipr(zip_file, top_dir)


# Tests: Type conversion --------------------------------------------------

test_that("response structure is appropriately typed", {
  ptype <- tibble::tibble(
    attempt = integer(),
    lrn_question_position = integer(),
    points_possible = numeric(),
    points_earned = numeric(),
    dt_submitted = new_datetime(tzone = "UTC"),
    lrn_dt_started = new_datetime(tzone = "UTC"),
    lrn_dt_saved = new_datetime(tzone = "UTC")
  )

  actual <- load_data(class_dir("responses.csv")) %>%
    convert_types_in_responses()

  typed <- actual[, names(ptype)]
  char_only <- actual[, !vctrs::vec_in(names(actual), names(ptype))]

  expect_vector(typed, ptype)
  purrr::map(char_only, expect_vector, ptype = character())
})

test_that("datetime columns can be read-in as a specific time zone", {
  data <- load_data(class_dir("responses.csv"))
  actual <- convert_types_in_responses(data, time_zone = Sys.timezone())
  expect_vector(actual$dt_submitted, new_datetime(tzone = Sys.timezone()))
})

test_that("list columns are appropriately converted from JSON if requested", {
  data <- load_data(class_dir("responses.csv"))

  actual <- convert_types_in_responses(data, convert_json = TRUE)
  expect_vector(actual$lrn_response_json, list())

  actual <- convert_types_in_responses(data, convert_json = FALSE)
  expect_vector(actual$lrn_response_json, character())
})


# Tests: Required columns -------------------------------------------------

test_that("response tables missing required columns throw informative errors", {
  expect_error(
    ensure_data_in_responses(data.frame(student_id = 1, prompt = 1)),
    "Response table missing required column: class_id"
  )
  expect_error(
    ensure_data_in_responses(data.frame(class_id = 1, prompt = 1)),
    "Response table missing required column: student_id"
  )
  expect_error(
    ensure_data_in_responses(data.frame(class_id = 1, student_id = 1)),
    "Response table missing required column: prompt"
  )
  expect_error(
    ensure_data_in_responses(data.frame(class_id = 1)),
    "Response table missing required columns: student_id, prompt"
  )
})

test_that("responses with a missing class_id are dropped with message", {
  mock_response <- data.frame(
    class_id = NA,
    student_id = 1,
    prompt = 1
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 1 row missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(0)
})

test_that("responses with a missing student_id are dropped with message", {
  mock_response <- data.frame(
    class_id = 1,
    student_id = NA,
    prompt = 1
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 1 row missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(0)
})

test_that("responses with a missing prompt are dropped with message", {
  mock_response <- data.frame(
    class_id = 1,
    student_id = 1,
    prompt = NA
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 1 row missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(0)
})

test_that("responses with multiple missing values have comprehensive message", {
  mock_response <- data.frame(
    class_id = NA,
    student_id = NA,
    prompt = NA
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 1 row missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(0)
})

test_that("empty strings are treated like NA when ensuring required columns", {
  mock_response <- data.frame(
    class_id = "",
    student_id = "",
    prompt = ""
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 1 row missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(0)
})

test_that("multiple dropped responses have a comprehensive message", {
  mock_response <- data.frame(
    class_id = c(NA, NA, 1, 1),
    student_id = c(NA, 1, NA, 1),
    prompt = c(1, 1, NA, 1)
  )

  ensure_data_in_responses(mock_response) %>%
    expect_message("Dropped 3 rows missing data at either class_id, student_id, or prompt") %>%
    expect_nrow(1)
})


# Tests: Mapping multiple-choice responses --------------------------------

test_that("cannot map responses without any responses", {
  expect_error(map_response_options(data.frame()))
})

test_that("mapping MC responses adds the lookup table as an attribute", {
  map_response_options(mock_response_looktbl) %>%
    attr("option_value_table") %>%
    expect_vector(tibble::tibble(
      lrn_question_reference = character(),
      lrn_option_0 = character(),
      lrn_option_1 = character(),
      lrn_option_2 = character()
    ))
})

test_that("lookup table cannot be created without type and ref.", {
  expect_warning(
    map_response_options(data.frame(response = 1, lrn_question_reference = 1)),
    "missing required column: lrn_type"
  )

  expect_warning(
    map_response_options(data.frame(response = 1, lrn_type = 1)),
    "missing required column: lrn_question_reference"
  )

  expect_warning(
    map_response_options(data.frame(response = 1)),
    "missing required columns: lrn_type, lrn_question_reference"
  )
})

test_that("the lookup table only includes multiple choice questions", {
  mcq_pos <- with(mock_response_looktbl, lrn_type == "mcq")
  mcq_item_ids <- mock_response_looktbl[["lrn_question_reference"]][mcq_pos]

  actual <- map_response_options(mock_response_looktbl) %>%
    attr("option_value_table")

  expect_true(all(actual[["lrn_question_reference"]] %in% mcq_item_ids))
})

test_that("the lookup table only has unique entries", {
  mcq_pos <- with(mock_response_looktbl, lrn_type == "mcq")
  expected <- mock_response_looktbl[["lrn_question_reference"]][mcq_pos] %>%
    unique() %>%
    length()

  map_response_options(mock_response_looktbl) %>%
    attr("option_value_table") %>%
    expect_nrow(expected)
})

test_that("mapping a non-lookupable item does not change response", {
  map_response_options(mock_response_looktbl[2, ])$response %>%
    expect_identical('["2"]')
})

test_that("mapping an empty response array yields missing value", {
  map_response_options(mock_response_looktbl[4, ])$response %>%
    expect_identical(NA_character_)
})

test_that("mapping a 1 option response yields a length 1 string with value", {
  map_response_options(mock_response_looktbl[1, ])$response %>%
    expect_identical("No")
})

test_that("mapping a 2 option response yields a length 1 delimited string", {
  map_response_options(mock_response_looktbl[3, ])$response %>%
    expect_identical("Yes; No")
})

test_that("mapping responses works with multiple responses in a data.frame", {
  map_response_options(mock_response_looktbl)$response %>%
    expect_identical(c("No", '["2"]', "Yes; No", NA_character_))
})


# Tests: Integration of sub-processes -------------------------------------

test_that("response processing methods do not need to be called in order", {
  order_1 <- mock_responses_integration %>%
    convert_types_in_responses() %>%
    ensure_data_in_responses() %>%
    map_response_options()

  order_2 <- mock_responses_integration %>%
    convert_types_in_responses() %>%
    map_response_options() %>%
    ensure_data_in_responses()

  order_3 <- mock_responses_integration %>%
    ensure_data_in_responses() %>%
    convert_types_in_responses() %>%
    map_response_options()

  order_4 <- mock_responses_integration %>%
    ensure_data_in_responses() %>%
    map_response_options() %>%
    convert_types_in_responses()

  expect_identical(order_1, order_2)
  expect_identical(order_1, order_3)
  expect_identical(order_3, order_4)
})

test_that("general response processing returns a tibble", {
  expect_s3_class(process_responses(mock_responses_integration), "tbl_df")
})

test_that("general response processing method is the sum of its parts", {
  expect_identical(
    mock_responses_integration %>%
      process_responses(),
    mock_responses_integration %>%
      tibble::as_tibble() %>%
      ensure_data_in_responses() %>%
      convert_types_in_responses() %>%
      map_response_options()
  )
})

test_that("general response processing allows setting the time zone", {
  expect_identical(
    mock_responses_integration %>%
      process_responses(time_zone = Sys.timezone()),
    mock_responses_integration %>%
      tibble::as_tibble() %>%
      ensure_data_in_responses() %>%
      convert_types_in_responses(time_zone = Sys.timezone()) %>%
      map_response_options()
  )
})


# Tests: Real data --------------------------------------------------------

test_that("processing responses shows no errors with a subset of real data", {
  test_resp <- read.csv(class_dir("responses.csv"))
  expect_error(process_responses(test_resp) %>% suppressMessages(), NA)
})
