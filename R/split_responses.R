#' Split a responses table into semantically distinct parts.
#'
#' The resulting responses table can be quite large and is filled with many
#' different types of responses. Pass the responses table to `split_responses()`
#' to break the data into three distinct components: pre- and post-course
#' surveys, end of chapter quizzes, and in-text questions.
#'
#' @param responses A responses data frame (processed or not) from a CourseKata
#'   class.
#'
#' @return A list of \code{\link{tibble}} objects with three elements:
#'   \code{surveys}, for all responses from the course Pre- and Post-Surveys;
#'   \code{quizzes}, for all responses from practice quizzes in the course; and
#'   \code{in_text}, for all other items that appear in the textbook.
#'
#' @export
split_responses <- function(responses) {
  ensure_columns(responses, "item_id", stop, "Cannot split responses. ")
  surveys <- survey_item_map(responses)
  quizzes <- quiz_item_map(responses)
  groups <- ifelse(surveys, "surveys", ifelse(quizzes, "quizzes", "in_text"))
  split(tibble::as_tibble(responses), groups)
}


survey_item_map <- function(responses) {
  item_map_lower <- stringr::str_to_lower(codebook[["item_id"]])
  stringr::str_to_lower(responses[["item_id"]]) %in% item_map_lower
}


quiz_item_map <- function(responses) {
  stringr::str_detect(responses[["item_id"]], ".*_Practice_Quiz.*")
}
