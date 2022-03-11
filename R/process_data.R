#' Process the data from a CourseKata data download.
#'
#' This function loads and processes data from a CourseKata end-of-class downloads. To use it, just
#' point it to the zip file or folder that you downloaded from CourseKata and run it. It will load
#' all of the data (showing progress bars along the way) and output it into several data frames:
#' `classes`, `responses`, `items`, `page_views`, `media_views`, and `tags`. If any of those
#' variables already exist, you will be prompted to either allow overwriting or abort the process
#' (if not being run interactively, a message is emitted noting that the variables were
#' overwritten).
#'
#' @inheritParams process_auxiliary
#' @param path The path to a CourseKata data download zip file or the path to an extracted data
#'   download directory.
#' @param split_responses If `TRUE`, split the responses out into three tables (`surveys`,
#'   `quizzes`, `in_text`) via [`split_responses`].
#' @param env The [environment] to create the variables in (defaults to the global environment). In
#'   most cases, the function will be used interactively and you will want to create the variables
#'   in the global environment, so that is the default behavior. In testing or programmatic
#'   applications it is useful to control where the variables are created, so an environment can be
#'   passed to capture them, e.g. by setting `env = rlang::env()`.
#'
#' @return This function returns the environment that the data frames were created in. Usually you
#' can ignore this return value and just use the variables as you need. If you are an advanced user
#' and you supplied an environment to `env`, that environment is returned.
#'
#' @export
process_data <- function(path,
                         time_zone = "UTC",
                         split_responses = FALSE,
                         convert_json = FALSE,
                         env = rlang::global_env()) {
  tbls <- c(
    if (split_responses) c("quizzes", "in_text", "surveys") else "responses",
    "classes", "page_views", "media_views", "items", "tags"
  )

  existing <- purrr::keep(tbls, exists)

  if (length(existing) > 0) {
    var_string <- paste(existing, collapse = ", ")

    if (interactive()) {
      choice <- utils::menu(
        title = paste("These variables will be overwritten:", var_string),
        choices = c("Abort.", "Overwrite them.", "Do not overwrite.")
      )

      if (choice != 2) {
        rlang::abort("Data processing aborted by user.\n")
      }
    }
  }

  started <- Sys.time()
  rlang::inform("\nProcessing data (this can take a few minutes, and bigger files will take longer)")
  rlang::env_bind(env, classes = process_classes(path))
  rlang::env_bind(env, page_views = process_page_views(path, time_zone))
  rlang::env_bind(env, media_views = process_media_views(path, time_zone))
  rlang::env_bind(env, items = process_items(path))
  rlang::env_bind(env, tags = process_tags(path))
  rlang::env_bind(env, responses = process_responses(path, time_zone))

  if (split_responses) {
    responses <- split_responses(env$responses)
    rlang::env_unbind(env, "responses")
    rlang::env_bind(env, surveys = responses[["surveys"]])
    rlang::env_bind(env, quizzes = responses[["quizzes"]])
    rlang::env_bind(env, in_text = responses[["in_text"]])
  }

  rlang::inform(paste("\nData processed in", format(Sys.time() - started, digits = 3)))
  var_string <- paste(tbls, collapse = ", ")
  rlang::inform(paste("\nYou can now use these variables:\n", var_string))

  invisible(env)
}
