#' Process the data from a CourseKata data download.
#'
#' This function loads and processes data from a CourseKata end-of-class downloads. It functions by
#' passing the path to each of the processing functions in this package. The output data frames are
#' all created in the user's active environment as `classes`, `responses`, `items`, `page_views`,
#' `media_views`, and `tags`. If any of the variables already exist, the user is prompted to either
#' allow overwriting or abort the process (if not being run interactively, a message is emitted
#' noting that the variables were overwritten).
#'
#' @inheritParams process_auxillary
#' @param path The path to a CourseKata data download zip file or the path to an extracted data
#'   download directory.
#' @param split_responses If `TRUE`, split the responses out into three tables (`surveys`,
#'   `quizzes`, `in_text`) via [`split_responses`].
#' @param env The environment to create the variables in (defaults to the global environment). In
#'   most cases, the function will be used interactively and you will want to create the variables
#'   in the global environment, so that is the default behavior. In testing or programmatic
#'   applications it is useful to control where the variables are created, so an environment can be
#'   passed to capture them.
#'
#' @return This function returns `TRUE` if it completes successfully, and `FALSE` if it is aborted.
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
        rlang::abort("Data processing aborted by user.")
      }
    }

    rlang::inform(paste("Overwriting variables:\n", var_string))
  }


  rlang::env_bind(env, classes = process_classes(path))
  rlang::env_bind(env, page_views = process_page_views(path, time_zone))
  rlang::env_bind(env, media_views = process_media_views(path, time_zone))
  rlang::env_bind(env, responses = process_responses(path, time_zone))
  rlang::env_bind(env, items = process_items(path))
  rlang::env_bind(env, tags = process_tags(path))

  if (split_responses) {
    responses <- split_responses(env$responses)
    rlang::env_unbind(env, "responses")
    rlang::env_bind(env, surveys = responses[["surveys"]])
    rlang::env_bind(env, quizzes = responses[["quizzes"]])
    rlang::env_bind(env, in_text = responses[["in_text"]])
  }

  var_string <- paste(tbls, collapse = ", ")
  rlang::inform(paste("You can now use these variables:\n", var_string))

  return(env)
}
