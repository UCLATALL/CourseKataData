#' Processing functions for all auxillary (non-response) data.
#'
#' These functions are for individually processing the auxillary components of a
#' CourseKata data download. These functions assume that you are working files
#' downloaded from CourseKata, as these files have a predictable structure.
#'
#' with a zip archive as downloaded from CourseKata, a directory extracted from
#' said zip archive, the path to the specif
#'
#' @param object The object to process the data from. This can be one of a
#'   variety of options:
#'   - the path to a CourseKata data download zip file
#'   - the path to an extracted data download directory
#'   - the path to a specific CSV file (e.g. `classes.csv`)
#'   - the `data.frame` loaded from a CSV file
#' @param time_zone The time zone to use when parsing datetime objects (see
#'   \code{\link{timezones}}).
#' @param class_id (Optional) A character vector of the IDs of specific classes
#'   you want to extract data for. These values should correspond to the names
#'   of the directories where the desired CSV files can be found in the original
#'   data download.
#'
#' @return These functions will always return a \code{\link{tibble}} of the
#'   data you are processing, where each row describes the data named in the
#'   function's name, e.g. `process_classes()` returns a `tibble` with rows
#'   describing classes. For descriptions of each of variables in each of the
#'   tables, see the README for this package (included in the data download or
#'   online at <https://github.com/UCLATALL/CourseKataData>).
#'
#' @name process_auxillary
#' @family processing functions
NULL

#' @rdname process_auxillary
#' @export
process_classes <- function(object) {
  object <- load_object(object, "classes", limit_search = 1)
  purrr::modify_at(object, "setup_yaml", ~ purrr::map(.x, jsonlite::parse_json))
}

#' @rdname process_auxillary
#' @export
process_page_views <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())
  purrr::modify_at(
    load_object(object, "page_views", class_id),
    "dt_accessed",
    readr::parse_datetime,
    locale = readr::locale(tz = time_zone)
  )
}

#' @rdname process_auxillary
#' @export
process_media_views <- function(object, time_zone = "UTC", class_id = NULL) {
  rlang::arg_match(time_zone, OlsonNames())
  load_object(object, "media_views", class_id) %>%
    purrr::modify_at(
      c("dt_started", "dt_last_event"),
      readr::parse_datetime,
      locale = readr::locale(tz = time_zone)
    ) %>%
    purrr::modify_at(
      c("proportion_video", "proportion_time"),
      readr::parse_double
    ) %>%
    purrr::modify_at(
      "log_json",
      safe_convert_json
    )
}

#' @rdname process_auxillary
#' @export
process_items <- function(object, class_id = NULL) {
  load_object(object, "items", class_id) %>%
    purrr::modify_at("lrn_question_position", readr::parse_integer) %>%
    purrr::modify_at("lrn_question_data", safe_convert_json)
}

#' @rdname process_auxillary
#' @export
process_tags <- function(object, class_id = NULL) {
  load_object(object, "tags", class_id)
}