#' Convert character vectors to numeric vectors.
#'
#' Given a character vector, these function will strip non-numeric characters from each item and
#' then attempt to convert the result to either a double using [`as.double()`] or an integer using
#' [`as.integer()`].
#'
#' @param x The character vector to convert.
#'
#' @return For `parse_double` and `parse_integer` a vector only containing doubles or integers. The
#'   helper function `strip_alpha` removes all non-numeric values leaving only numbers, dots (`.`
#'   for decimals), and hyphens (`-` for negative values). The parsers will fail if `strip_alpha`
#'   yields something non-numeric looking, like "1.1.1".
#' @keywords internal
#' @rdname number_parsers
strip_alpha <- function(x) {
  stringi::stri_replace_all_regex(x, "[^0-9.-]", "")
}

#' @keywords internal
#' @rdname number_parsers
parse_double <- function(x) {
  if (is.character(x)) as.double(strip_alpha(x)) else as.double(x)
}

#' @keywords internal
#' @rdname number_parsers
parse_integer <- function(x) {
  as.integer(parse_double(x))
}

#' Convert a JSON-encoded vector to a list, ignoring all errors.
#'
#' Note that you should really understand what is in that vector, because this function will discard
#' all errors and replace them with an empty list. The successfully converted vector will be a
#' vector of lists. Each list will have keys corresponding to the JSON keys. If you pass this a JSON
#' string with no keys (e.g. an array or number) it will probably work, but it wasn't built to do
#' that.
#'
#' @inheritParams parse_double
#'
#' @return A vector of lists, where each list has keys corresponding to keys in the JSON. It will
#'   use [`jsonlite::parse_json()`] and if it encounters an error, the error is discarded and
#'   replaced with an empty list.
#' @keywords internal
safe_convert_json <- function(json_col) {
  possibly_parse <- purrr::possibly(jsonlite::parse_json, otherwise = list())
  purrr::map(json_col, possibly_parse)
}

#' Parse a vector of date-time strings and convert to the desired time-zone.
#'
#' This function assumes that it is being given date-time strings in the UTC/GMT time-zone with a
#' specific format. The format is similar to ISO-8601, but it omits the "T" dividing the date and
#' time: `YYYY-MM-DD hh:mm:ss.sss`. For example, the date-time May 1, 1983 at 3:33PM and 44.444
#' seconds, would be represented as `1983-05-01 15:33.444`. After parsing those strings as
#' [date-times][DateTimeClasses], the time-zone is converted to the desired zone.
#'
#' @inheritParams parse_double
#' @param time_zone The time zone to use when parsing date-time objects (see [`timezones`]).
#'
#' @return A vector of date-times, where the date-time has been converted to the desired time-zone.
#' @keywords internal
parse_datetime <- function(x, time_zone = "UTC") {
  dt <- lubridate::ymd_hms(x, tz = "UTC")
  lubridate::with_tz(dt, time_zone)
}
