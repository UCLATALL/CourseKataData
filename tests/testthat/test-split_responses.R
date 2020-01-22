context("Splitting responses")

library(dplyr)

mock_responses <- tibble(
  item_id = c(
    "Presurvey_101_other_text", "Postsurvey_101_other_text",
    "Ch1_Practice_Quiz", "Ch6_Practice_Quiz_2",
    "literally", "anything", "else"
  )
)

split <- split_responses(mock_responses)

test_that("responses are split into named tibbles", {
  split <- split_responses(mock_responses)

  expect_is(split, "list")
  expect_is(split$surveys, "tbl_df")
  expect_is(split$in_text, "tbl_df")
  expect_is(split$quizzes, "tbl_df")
})

test_that("response tables missing required columns throw informative errors", {
  expect_error(
    split_responses(data.frame()),
    "Cannot split responses. Response table missing required column: item_id"
  )
})

test_that("pre- and post- survey items end up in surveys", {
  expect_identical(split$survey, mock_responses[1:2, ])
})

test_that("practice quiz items end up in quizzes", {
  expect_identical(split$quizzes, mock_responses[3:4, ])
})

test_that("all other items end up in in_text", {
  expect_identical(split$in_text, mock_responses[5:7, ])
})

test_that("all responses are accounted for", {
  mock_responses <- mock_responses %>%
    dplyr::mutate(rownames = rownames(.))
  split <- split_responses(mock_responses)

  expect_identical(
    bind_rows(split) %>% arrange(rownames),
    mock_responses %>% arrange(rownames)
  )
})
