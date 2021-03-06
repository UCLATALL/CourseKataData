#' Processing functions for all auxillary (non-response) data.
#'
#' These functions are for individually processing the auxillary components of a
#' CourseKata data download. These functions assume that you are working files
#' downloaded from CourseKata, as these files have a predictable structure.
#'
#' with a zip archive as downloaded from CourseKata, a directory extracted from
#' said zip archive, the path to the specif
#'
#' @inheritParams load_data
#' @param time_zone The time zone to use when parsing date-time objects (see
#'   \code{\link{timezones}}).
#'
#' @return These functions will always return a \code{\link{tibble}} of the data
#'   you are processing. Each row of the resulting `tibble`, describes the type
#'   of data named by the function, e.g. `process_classes()` returns a `tibble`
#'   with rows describing *classes*. For descriptions of each of variables in
#'   each of the tables, see the README for this package (included in the data
#'   download or online at <https://github.com/UCLATALL/CourseKataData>).
#'
#' @name process_auxillary
#' @family processing functions
NULL

#' @rdname process_auxillary
#' @export
process_classes <- function(object, class_id = NULL) {
  object <- load_data(object, "classes[.]csv$", class_id = class_id)
  purrr::modify_at(object, "setup_yaml", safe_convert_json)
}

#' @rdname process_auxillary
#' @export
process_page_views <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())
  load_data(object, "page_views[.]csv$", class_id = class_id) %>%
    purrr::modify_at("dt_accessed", parse_datetime, tzone = time_zone)
}

#' @rdname process_auxillary
#' @export
process_media_views <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())

  datetimes <- c("dt_started", "dt_last_event")
  doubles <- c("proportion_video", "proportion_time")

  # parsers located at process_function_helpers
  load_data(object, "media_views[.]csv$", class_id = class_id) %>%
    purrr::modify_at(datetimes, parse_datetime, tzone = time_zone) %>%
    purrr::modify_at(doubles, parse_double) %>%
    purrr::modify_at("log_json", safe_convert_json)
}

#' @rdname process_auxillary
#' @export
process_items <- function(object, class_id = NULL) {
  load_data(object, "items[.]csv$", class_id = class_id) %>%
    purrr::modify_at("lrn_question_position", parse_integer) %>%
    purrr::modify_at("lrn_question_data", safe_convert_json)
}

#' @rdname process_auxillary
#' @export
process_tags <- function(object, class_id = NULL) {
  load_data(object, "tags[.]csv$", class_id = class_id)
}
