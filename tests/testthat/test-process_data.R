ex_dir <- fs::path(tempdir(check = TRUE), "coursekdatadata_process_data")
zip_file <- data_dir("zipped.zip")
zip::unzip(data_dir("zipped.zip"), exdir = ex_dir)

test_env <- rlang::env()

test_that("it will load all of the data and create six data frames", {
  process_data(ex_dir, env = test_env) %>%
    suppressMessages()

  rlang::with_env(test_env, {
    expect_identical(classes, process_classes(ex_dir))
    expect_identical(responses, process_responses(ex_dir) %>% suppressMessages())
    expect_identical(page_views, process_page_views(ex_dir))
    expect_identical(media_views, process_media_views(ex_dir))
    expect_identical(items, process_items(ex_dir))
    expect_identical(tags, process_tags(ex_dir))
  })
})

test_that("it can load from a zip file and create six data frames", {
  process_data(zip_file, env = test_env) %>%
    suppressMessages()

  rlang::with_env(test_env, {
    expect_identical(classes, process_classes(zip_file))
    expect_identical(responses, process_responses(zip_file) %>% suppressMessages())
    expect_identical(page_views, process_page_views(zip_file))
    expect_identical(media_views, process_media_views(zip_file))
    expect_identical(items, process_items(zip_file))
    expect_identical(tags, process_tags(zip_file))
  })
})

test_that("it will optionally split responses to globals", {
  process_data(zip_file, split_responses = TRUE, env = test_env) %>%
    suppressMessages()

  rlang::with_env(test_env, {
    expected <- process_responses(zip_file) %>%
      split_responses() %>%
      suppressMessages()

    expect_false(exists("responses"))
    expect_identical(surveys, expected$surveys)
    expect_identical(quizzes, expected$quizzes)
    expect_identical(in_text, expected$in_text)
  })
})

test_that("you can specify a different time zone", {
  process_data(zip_file, time_zone = Sys.timezone(), env = test_env) %>%
    suppressMessages()

  rlang::with_env(test_env, suppressMessages({
    expect_identical(page_views, process_page_views(zip_file, time_zone = Sys.timezone()))
    expect_identical(media_views, process_media_views(zip_file, time_zone = Sys.timezone()))
    expect_identical(responses, process_responses(zip_file, time_zone = Sys.timezone()))
  }))
})
