mock_responses <- tibble::tibble(
  item_id = c(
    "Presurvey_0719_Student_Background", "Postsurvey_0719_Attitudes", "Embedded_0719_Value",
    "Ch1_Practice_Quiz", "Ch6_Practice_Quiz_2",
    "literally", "anything", "else"
  )
)

split <- split_responses(mock_responses)

test_that("responses are split into named tibbles", {
  expect_vector(split, list())
  expect_s3_class(split$surveys, "tbl_df")
  expect_s3_class(split$in_text, "tbl_df")
  expect_s3_class(split$quizzes, "tbl_df")
})

test_that("response tables missing required columns throw informative errors", {
  expect_error(
    split_responses(data.frame()),
    "Cannot split responses. Response table missing required column: item_id"
  )
})

test_that("survey items end up in surveys", {
  expect_identical(split$surveys, mock_responses[1:3, ], ignore_attr = TRUE)
})

test_that("practice quiz items end up in quizzes", {
  expect_identical(split$quizzes, mock_responses[4:5, ], ignore_attr = TRUE)
})

test_that("all other items end up in in_text", {
  expect_identical(split$in_text, mock_responses[6:8, ], ignore_attr = TRUE)
})

test_that("all responses are accounted for", {
  rownames <- as.integer(rownames(mock_responses))
  mock_responses[["rownames"]] <- rownames
  split <- split_responses(mock_responses)
  spliced <- purrr::reduce(split, rbind) %>%
    .[match(rownames, .[["rownames"]]), ]

  expect_identical(spliced, mock_responses, ignore_attr = TRUE)
})
