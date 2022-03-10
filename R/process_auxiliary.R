#' Processing functions for all auxiliary (non-response) data.
#'
#' These functions are for individually processing the auxiliary components of a CourseKata data
#' download. These functions assume that you are working files downloaded from CourseKata, as these
#' files have a predictable structure.
#'
#' with a zip archive as downloaded from CourseKata, a directory extracted from said zip archive,
#' the path to the specif
#'
#' @inheritParams load_data
#' @inheritParams parse_datetime
#' @param convert_json For speed of processing, columns containing JSON are just read in as strings.
#'   If you are going to use these data, it may be useful to convert them to lists by specifying
#'   this argument as `TRUE`.
#'
#' @return These functions will always return a [`tibble`] of the data you are processing. Each row
#'   of the resulting `tibble`, describes the type of data named by the function, e.g.
#'   `process_classes()` returns a `tibble` with rows describing *classes*. For descriptions of each
#'   of variables in each of the tables, see the README for this package (included in the data
#'   download or online at <https://github.com/UCLATALL/CourseKataData>).
#'
#' @name process_auxiliary
#' @family processing functions
NULL

#' @rdname process_auxiliary
#' @export
process_classes <- function(object, class_id = NULL, convert_json = FALSE) {
  msg <- "Processing classes..."
  object <- load_data(object, "classes[.]csv$", class_id = class_id, progress_message = msg)

  if (convert_json) {
    object <- object %>% purrr::modify_at("setup_yaml", safe_convert_json)
  }

  object
}

#' @rdname process_auxiliary
#' @export
process_page_views <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())
  msg <- "Processing page views..."
  load_data(object, "page_views[.]csv$", class_id = class_id, progress_message = msg) %>%
    purrr::modify_at("dt_accessed", parse_datetime, time_zone = time_zone)
}

#' @rdname process_auxiliary
#' @export
process_media_views <- function(object, time_zone = "UTC", class_id = NULL, convert_json = FALSE) {
  rlang::arg_match(time_zone, OlsonNames())

  datetimes <- c("dt_started", "dt_last_event")
  doubles <- c("proportion_video", "proportion_time")

  # parsers located at process_function_helpers
  msg <- "Processing media views..."
  out <- load_data(object, "media_views[.]csv$", class_id = class_id, progress_message = msg) %>%
    purrr::modify_at(datetimes, parse_datetime, time_zone = time_zone) %>%
    purrr::modify_at(doubles, parse_double)

  if (convert_json) {
    out <- purrr::modify_at(out, "log_json", safe_convert_json)
  }

  out
}

#' @rdname process_auxiliary
#' @export
process_items <- function(object, class_id = NULL, convert_json = FALSE) {
  msg <- "Processing item info..."
  out <- load_data(object, "items[.]csv$", class_id = class_id, progress_message = msg) %>%
    purrr::modify_at("lrn_question_position", parse_integer)

  if (convert_json) {
    out <- purrr::modify_at(out, "lrn_question_data", safe_convert_json)
  }

  out
}

#' @rdname process_auxiliary
#' @export
process_tags <- function(object, class_id = NULL) {
  msg <- "Processing tags..."
  load_data(object, "tags[.]csv$", class_id = class_id, progress_message = msg)
}
