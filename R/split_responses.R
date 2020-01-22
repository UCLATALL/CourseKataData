#' Split a responses table into semantically distinct parts.
#'
#' @param responses A responses data frame (processed or not) from a CourseKata
#'   class.
#'
#' @return A list of \code{\link{tibble}} objects with three elements:
#'   \code{surveys}, for all responses from the course Pre- and Post-Surveys;
#'   \code{quizzes}, for all responses from practice quizzes in the course; and
#'   \code{in_text}, for all other items that appear in the textbook.
#' @export
split_responses <- function(responses) {
  ensure_columns(responses, "item_id", stop, "Cannot split responses. ")
  groups <- dplyr::case_when(
    stringr::str_detect(responses$item_id, "^(?:Pre|Post)survey_") ~ "surveys",
    stringr::str_detect(responses$item_id, ".*_Practice_Quiz.*") ~ "quizzes",
    TRUE ~ "in_text"
  )
  dplyr::as_tibble(responses) %>%
    split(groups)
}
