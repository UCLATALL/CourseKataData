
# Capture and discard errors when converting JSON to list
safe_convert_json <- function(json_col) {
  possibly_parse <- purrr::possibly(jsonlite::parse_json, otherwise = list())
  purrr::map(json_col, possibly_parse)
}

# Ensure that a data frame has the required columns or signal with a message
ensure_columns <- function(data, required_columns, signal, message_prefix = "") {
  missing_required_cols <- !required_columns %in% names(data)
  if (any(missing_required_cols)) {
    signal(sprintf(
      "%sResponse table missing required %s: %s",
      message_prefix,
      ngettext(sum(missing_required_cols), "column", "columns"),
      paste0(required_columns[missing_required_cols], collapse = ", ")
    ))
  }

  invisible(!any(missing_required_cols))
}

# Simple parsers
strip_alpha <- function(x) stringr::str_remove_all(x, '[^0-9.-]')
parse_double <- function(x) vctrs::vec_cast(strip_alpha(x), double())
parse_integer <- function(x) vctrs::vec_cast(parse_double(x), integer())
parse_datetime <- function(x, tzone = 'UTC') {
  new <- vctrs::vec_cast(x, vctrs::new_datetime(tzone = 'UTC'))
  structure(new, tzone = tzone)
}
